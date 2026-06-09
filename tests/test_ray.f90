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

module test_ray

    use rtc_utils, only: is_equal
    use rtc_tuple
    use rtc_ray
    use rtc_sphere
    use rtc_trans
    use rtc_matrix, only: matrix_print, matrix_equal, matrix_identity
    
    implicit none
    
    private
    public :: run_test_ray
    
contains

    subroutine run_test_ray()
    
        type(ray) :: r, new_ray
        type(sphere) :: s
        type(tuple) :: origin, direction
        type(intersection) :: xs(2), i0, i1, i2, i3, i4, xss(4)
        real :: trans(4,4)
        
        ! ========== ray ==========
        r = ray(make_point(1.0, 2.0, 3.0), make_vector(4.0, 5.0, 6.0))

        if (tuple_equal(r%orig, make_point(1.0, 2.0, 3.0)) .and. &
            tuple_equal(r%dir, make_vector(4.0, 5.0, 6.0))) then
            print *, "Test: ray - passed"
        else
            print *, "Test: ray - failed"
        end if
        
        ! ========== ray_position ==========
        r = ray(make_point(2.0, 3.0, 4.0), make_vector(1.0, 0.0, 0.0))
        
        if (tuple_equal(ray_position(r, 0.0), make_point(2.0, 3.0, 4.0)) .and. &
            tuple_equal(ray_position(r, 1.0), make_point(3.0, 3.0, 4.0)) .and. &
            tuple_equal(ray_position(r, -1.0), make_point(1.0, 3.0, 4.0)) .and. &
            tuple_equal(ray_position(r, 2.5), make_point(4.5, 3.0, 4.0))) then
            print *, "Test: ray_position - passed"
        else
            print *, "Test: ray_position - failed"
        end if
        
        ! ========== ray_intersects ==========
        r = ray(make_point(0.0, 0.0, -5.0), make_vector(0.0, 0.0, 1.0))
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        xs = ray_intersects(s, r)
        if (is_equal(xs(1)%t, 4.0) .and. &
            is_equal(xs(2)%t, 6.0)) then
            print *, "Test: ray_intersects - passed"
        else
            print *, "Test: ray_intersects - failed"
        end if
        
        r = ray(make_point(0.0, 1.0, -5.0), make_vector(0.0, 0.0, 1.0))
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        xs = ray_intersects(s, r)
        if (is_equal(xs(1)%t, 5.0) .and. &
            is_equal(xs(2)%t, 5.0)) then
            print *, "Test: ray_intersects 2 - passed"
        else
            print *, "Test: ray_intersects 2 - failed"
        end if
        
        r = ray(make_point(0.0, 2.0, -5.0), make_vector(0.0, 0.0, 1.0))
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        xs = ray_intersects(s, r)
        if (is_equal(xs(1)%t, 0.0) .and. &
            is_equal(xs(2)%t, 0.0)) then
            print *, "Test: ray_intersects 3 - passed"
        else
            print *, "Test: ray_intersects 3 - failed"
        end if
        
        r = ray(make_point(0.0, 0.0, 0.0), make_vector(0.0, 0.0, 1.0))
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        xs = ray_intersects(s, r)
        if (is_equal(xs(1)%t, -1.0) .and. &
            is_equal(xs(2)%t, 1.0)) then
            print *, "Test: ray_intersects 4 - passed"
        else
            print *, "Test: ray_intersects 4 - failed"
        end if
        
        r = ray(make_point(0.0, 0.0, 5.0), make_vector(0.0, 0.0, 1.0))
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        xs = ray_intersects(s, r)
        if (is_equal(xs(1)%t, -6.0) .and. &
            is_equal(xs(2)%t, -4.0)) then
            print *, "Test: ray_intersects 5 - passed"
        else
            print *, "Test: ray_intersects 5 - failed"
        end if
        
        if (is_equal(xs(1)%object, s%id) .and. &
            is_equal(xs(2)%object, s%id)) then
            print *, "Test: ray_intersects 6 - passed"
        else
            print *, "Test: ray_intersects 6 - failed"
        end if
        
        ! ========== ray_hit ==========
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        i1 = intersection(1.0, s%id)
        i2 = intersection(2.0, s%id)
        xs(1) = i1
        xs(2) = i2
        i0 = ray_hit(xs)
        if (is_equal(i0%t, i1%t) .and. &
            is_equal(i0%object, i1%object)) then
            print *, "Test: ray_hit 1 - passed"
        else
            print *, "Test: ray_hit 1 - failed"
        end if
        
        i1 = intersection(-1.0, s%id)
        i2 = intersection(1.0, s%id)
        xs(1) = i1
        xs(2) = i2
        i0 = ray_hit(xs)
        if (is_equal(i0%t, i2%t) .and. &
            is_equal(i0%object, i2%object)) then
            print *, "Test: ray_hit 2 - passed"
        else
            print *, "Test: ray_hit 2 - failed"
        end if
        
        i1 = intersection(-2.0, s%id)
        i2 = intersection(-1.0, s%id)
        xs(1) = i1
        xs(2) = i2
        i0 = ray_hit(xs)
        if (is_equal(i0%object, 0.0)) then
            print *, "Test: ray_hit 3 - passed"
        else
            print *, "Test: ray_hit 3 - failed"
        end if
        
        i1 = intersection(5.0, s%id)
        i2 = intersection(7.0, s%id)
        i3 = intersection(-3.0, s%id)
        i4 = intersection(2.0, s%id)
        xss(1) = i1
        xss(2) = i2
        xss(3) = i3
        xss(4) = i4
        i0 = ray_hit(xss)
        if (is_equal(i0%t, i4%t) .and. &
            is_equal(i0%object, i4%object)) then
            print *, "Test: ray_hit 4 - passed"
        else
            print *, "Test: ray_hit 4 - failed"
        end if
        
        ! ========== ray_transform ==========
        r = ray(make_point(1.0, 2.0, 3.0), make_vector(0.0, 1.0, 0.0))
        trans = trans_translation(3.0, 4.0, 5.0)
        new_ray = ray_transform(r, trans)
        origin = make_point(4.0, 6.0, 8.0)
        direction = make_vector(0.0, 1.0, 0.0)
        if (tuple_equal(new_ray%orig, origin) .and. &
            tuple_equal(new_ray%dir, direction)) then
            print *, "Test: ray_transform translation - passed"
        else
            print *, "Test: ray_transform translation - failed"
        end if
        
        trans = trans_scaling(2.0, 3.0, 4.0)
        new_ray = ray_transform(r, trans)
        origin = make_point(2.0, 6.0, 12.0)
        direction = make_vector(0.0, 3.0, 0.0)
        if (tuple_equal(new_ray%orig, origin) .and. &
            tuple_equal(new_ray%dir, direction)) then
            print *, "Test: ray_transform scaling - passed"
        else
            print *, "Test: ray_transform scaling - failed"
        end if
        
        ! ========== sphere transformation ==========
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        if (matrix_equal(s%transform, matrix_identity())) then
            print *, "Test: sphere default transformation - passed"
        else
            print *, "Test: sphere default transformation - failed"
        end if
        
        s%transform = trans_translation(2.0, 3.0, 4.0)
        if (matrix_equal(s%transform, trans_translation(2.0, 3.0, 4.0))) then
            print *, "Test: sphere transformation change - passed"
        else
            print *, "Test: sphere transformation change - failed"
        end if
        
        ! ========== intersecting a scaled sphere with a ray ==========
        r = ray(make_point(0.0, 0.0, -5.0), make_vector(0.0, 0.0, 1.0))
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        s%transform = trans_scaling(2.0, 2.0, 2.0)
        xs = ray_intersects(s, r)
        if (is_equal(xs(1)%t, 3.0) .and. &
            is_equal(xs(2)%t, 7.0)) then
            print *, "Test: intersecting a scaled sphere with a ray - passed"
        else
            print *, "Test: intersecting a scaled sphere with a ray - failed"
        end if
        
        ! ========== intersecting a translated sphere with a ray ==========
        r = ray(make_point(0.0, 0.0, -5.0), make_vector(0.0, 0.0, 1.0))
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        s%transform = trans_translation(5.0, 0.0, 0.0)
        xs = ray_intersects(s, r)
        if (is_equal(xs(1)%t, 0.0) .and. &
            is_equal(xs(2)%t, 0.0)) then
            print *, "Test: intersecting a translated sphere with a ray - passed"
        else
            print *, "Test: intersecting a translated sphere with a ray - failed"
        end if

    end subroutine run_test_ray
    
end module test_ray
