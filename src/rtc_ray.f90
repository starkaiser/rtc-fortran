!
! Copyright (C) 2025-2026 Sorin Cătălin Păștiță, sorincatalinpastita@gmail.com
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module rtc_ray
    
    use rtc_tuple,  only: tuple, tuple_add, tuple_multiply, tuple_subtract, tuple_dot, make_point
    use rtc_sphere, only: sphere, make_sphere
    use rtc_matrix, only: matrix_mul_tuple, matrix_inverse
    
    implicit none
    
    private
    public :: ray, intersection
    public :: ray_position, ray_intersects, ray_hit, ray_transform
    
    type :: ray
        type(tuple) :: orig, dir
    end type ray
    
    type :: intersection
        real :: t, object
    end type intersection
    
contains

    pure function ray_position(r, t) result(p)
        type(ray), intent(in) :: r
        real, intent(in) :: t
        type(tuple) :: p
        p = tuple_add(r%orig, tuple_multiply(r%dir, t))
    end function ray_position
    
    function ray_intersects(s, r) result(xs)
        type(sphere), intent(in) :: s
        type(ray), intent(in) :: r
        type(ray) :: r2
        real :: a, b, c, discriminant
        type(intersection) :: xs(2)
        type(tuple) :: sphere_to_ray
        
        r2 = ray_transform(r, matrix_inverse(s%transform))
        
        sphere_to_ray = tuple_subtract(r2%orig, make_point(0.0,0.0,0.0))
        a = tuple_dot(r2%dir, r2%dir)
        b = 2 * tuple_dot(r2%dir, sphere_to_ray)
        c = tuple_dot(sphere_to_ray, sphere_to_ray) - 1.0
        discriminant = b*b - 4.0 * a * c
        
        if (discriminant < 0) then
            xs(1)%t = 0.0
            xs(1)%object = s%id
            xs(2)%t = 0.0
            xs(2)%object = s%id
        else
            xs(1)%t = (-b - sqrt(discriminant)) / (2.0*a)
            xs(1)%object = s%id
            xs(2)%t = (-b + sqrt(discriminant)) / (2.0*a)
            xs(2)%object = s%id
        end if       
    end function ray_intersects
    
    ! needs improvement when all intersections are negative
    pure function ray_hit(xs) result(i)
        type(intersection), intent(in) :: xs(:)
        type(intersection) :: i
        integer :: j, x
        i = intersection(huge(x), 0.0) ! caveman hack
        do j = 1, size(xs)
            if (xs(j)%t <= 0.0) then
                cycle
            end if
            if (xs(j)%t < i%t) then
                i = xs(j)
            end if
        end do
    end function ray_hit
    
    pure function ray_transform(old_ray, matrix) result(new_ray)
        type(ray), intent(in) :: old_ray
        real, intent(in) :: matrix(4,4)
        type(ray) :: new_ray
        new_ray = ray(matrix_mul_tuple(matrix, old_ray%orig), &
                      matrix_mul_tuple(matrix, old_ray%dir))        
    end function ray_transform

end module rtc_ray
