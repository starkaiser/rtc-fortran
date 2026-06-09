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

module rtc_tuple
    
    use rtc_utils, only: is_equal
    
    implicit none

    public :: tuple, make_point, make_vector
    public :: tuple_add, tuple_subtract, tuple_negate, tuple_multiply, tuple_divide
    public :: tuple_magnitude, tuple_normalize, tuple_dot, tuple_cross, tuple_equal, tuple_to_matrix
    
    type :: tuple
        real :: x, y, z, w
    end type tuple
    
contains
    
    pure function make_point(x, y, z) result(p)
        real, intent(in) :: x, y, z
        type(tuple) :: p
        p = tuple(x, y, z, 1.0)
    end function make_point
    
    pure function make_vector(x, y, z) result(v)
        real, intent(in) :: x, y, z
        type(tuple) :: v
        v = tuple(x, y, z, 0.0)
    end function make_vector
    
    pure function tuple_add(a, b) result(t)
        type(tuple), intent(in) :: a, b
        type(tuple) :: t
        t = tuple(a%x + b%x, a%y + b%y, a%z + b%z, a%w + b%w)
    end function tuple_add
    
    pure function tuple_subtract(a, b) result(t)
        type(tuple), intent (in) :: a, b
        type(tuple) :: t
        t = tuple(a%x - b%x, a%y - b%y, a%z - b%z, a%w - b%w)
    end function tuple_subtract
    
    pure function tuple_negate(a) result(t)
        type(tuple), intent(in) :: a
        type(tuple) :: t
        t = tuple(-a%x, -a%y, -a%z, -a%w)
    end function tuple_negate
    
    pure function tuple_multiply(a, scalar) result(t)
        type(tuple), intent(in) :: a
        real, intent(in) :: scalar
        type(tuple) :: t
        t = tuple(scalar * a%x, scalar * a%y, scalar * a%z, scalar * a%w)
    end function tuple_multiply
    
    pure function tuple_divide(a, scalar) result(t)
        type(tuple), intent(in) :: a
        real, intent(in) :: scalar
        type(tuple) :: t
        t = tuple(a%x / scalar, a%y / scalar, a%z / scalar, a%w / scalar)
    end function tuple_divide
    
    pure function tuple_magnitude(v) result(m)
        type(tuple), intent(in) :: v
        real :: m
        m = sqrt(v%x**2 + v%y**2 + v%z**2 + v%w**2)
    end function tuple_magnitude
    
    pure function tuple_normalize(v) result(t)
        type(tuple), intent(in) :: v
        type(tuple) :: t
        real :: m
        m = tuple_magnitude(v)
        t = tuple(v%x/m, v%y/m, v%z/m, v%w/m)
    end function tuple_normalize
    
    pure function tuple_dot(a, b) result(r)
        type(tuple), intent(in) :: a, b
        real :: r
        r = a%x*b%x + a%y*b%y + a%z*b%z + a%w*b%w
    end function tuple_dot
    
    pure function tuple_cross(a, b) result(t)
        type(tuple), intent(in) :: a, b
        type(tuple) :: t
        t = make_vector(a%y*b%z - a%z*b%y, &
                        a%z*b%x - a%x*b%z, &
                        a%x*b%y - a%y*b%x)
    end function tuple_cross
    
    pure function tuple_equal(a, b) result(r)
        type(tuple), intent(in) :: a, b
        logical :: r
        if (is_equal(a%x,b%x) .and. &
            is_equal(a%y,b%y) .and. &
            is_equal(a%z,b%z) .and. &
            is_equal(a%w,b%w)) then
            r = .true.
        else
            r = .false.
        end if
    end function tuple_equal
    
    pure function tuple_to_matrix(t) result(r)
        type(tuple), intent(in) :: t
        real :: r(4)
        r = [t%x, t%y, t%z, t%w]
    end function tuple_to_matrix
    
end module rtc_tuple
