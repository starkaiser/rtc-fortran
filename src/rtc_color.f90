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

module rtc_color
    
    use rtc_utils, only: is_equal
    
    implicit none
    
    private
    
    public :: color, color_add, color_subtract, color_multiply, color_product, color_equal
    
    type :: color
        real :: r, g, b
    end type color
    
contains
    
    pure function color_add(a, b) result(r)
        type(color), intent(in) :: a, b
        type(color) :: r
        r = color(a%r + b%r, a%g + b%g, a%b + b%b)
    end function color_add
    
    pure function color_subtract(a, b) result(r)
        type(color), intent(in) :: a, b
        type(color) :: r
        r = color(a%r - b%r, a%g - b%g, a%b - b%b)
    end function color_subtract
    
    pure function color_multiply(a, scalar) result(r)
        type(color), intent(in) :: a
        real, intent(in) :: scalar
        type(color) :: r
        r = color(scalar * a%r, scalar * a%g, scalar * a%b)
    end function color_multiply
    
    pure function color_product(a, b) result(r)
        type(color), intent(in) :: a, b
        type(color) :: r
        r = color(a%r*b%r, a%g*b%g, a%b*b%b)
    end function color_product
    
    pure elemental function color_equal(a, b) result(r)
        type(color), intent(in) :: a, b
        logical :: r
        if (is_equal(a%r,b%r) .and. &
            is_equal(a%g,b%g) .and. &
            is_equal(a%b,b%b)) then
            r = .true.
        else
            r = .false.
        end if
    end function color_equal
    
end module rtc_color
