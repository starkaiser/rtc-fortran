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

module test_color

    use rtc_utils, only: is_equal
    use rtc_color
    
    implicit none
    
    private
    public :: run_test_color
    
    type(color) :: c1, c2, c_result
    real :: scalar
    
contains

    subroutine run_test_color()
    
        c1 = color(0.9, 0.6, 0.75)
        c2 = color(0.7, 0.1, 0.25)
        
        ! ========== color_equal ==========
        if (color_equal(c1, color(0.9, 0.6, 0.75))) then
            print *, "Test: color_equal - passed"
        else
            print *, "Test: color_equal - failed"
        end if
        
        ! ========== color_add ==========
        c_result = color_add(c1, c2)
        if (color_equal(c_result, color(1.6, 0.7, 1.0))) then
            print *, "Test: color_add - passed"
        else
            print *, "Test: color_add - failed"
        end if
        
        ! ========== color_subtract ==========
        c_result = color_subtract(c1, c2)
        if (color_equal(c_result, color(0.2, 0.5, 0.5))) then
            print *, "Test: color_subtract - passed"
        else
            print *, "Test: color_subtract - failed"
        end if
        
        ! ========== color_multiply ==========
        c1 = color(0.2, 0.3, 0.4)
        scalar = 2.0
        c_result = color_multiply(c1, scalar)
        if (color_equal(c_result, color(0.4, 0.6, 0.8))) then
            print *, "Test: color_multiply - passed"
        else
            print *, "Test: color_multiply - failed"
        end if
        
        ! ========== color_product ==========
        c1 = color(1.0, 0.2, 0.4)
        c2 = color(0.9, 1.0, 0.1)
        c_result = color_product(c1, c2)
        if (color_equal(c_result, color(0.9, 0.2, 0.04))) then
            print *, "Test: color_product - passed"
        else
            print *, "Test: color_product - failed"
        end if
    
    end subroutine run_test_color

end module test_color
