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

module test_canvas

    use rtc_utils, only: is_equal
    use rtc_color, only: color, color_equal
    use rtc_canvas

    implicit none
    
    private
    public :: run_test_canvas
    
    type(canvas) :: c
    logical :: exists
    
contains
    
    subroutine run_test_canvas()
        
        ! ========== canvas_create ==========
        c = make_canvas(10, 20)
        if (c%width == 10 .and. &
            c%height == 20 .and. &
            all(color_equal(c%pixels, color(0.0, 0.0, 0.0)))) then
            print *, "Test: canvas_create - passed"
        else
            print *, "Test: canvas_create - failed"
        end if
        
        ! ========== canvas_write_pixel ==========
        call canvas_write_pixel(c, 2, 3, color(1.0, 0.0, 0.0))
        if (color_equal(c%pixels(2,3), color(1.0, 0.0, 0.0))) then
            print *, "Test: canvas_write_pixel - passed"
        else
            print *, "Test: canvas_write_pixel - failed"
        end if
        
        ! ========== canvas_pixel_at ==========
        if (color_equal(canvas_pixel_at(c, 2,3), color(1.0, 0.0, 0.0))) then
            print *, "Test: canvas_pixel_at - passed"
        else
            print *, "Test: canvas_pixel_at - failed"
        end if
        
        ! ========== canvas_write_ppm ==========
        call canvas_write_ppm(c,"/home/starkaiser/Coding/rtc-fortran/output/canvas.ppm")
        inquire(file="/home/starkaiser/Coding/rtc-fortran/output/canvas.ppm", exist=exists)
        if (exists) then
            print *, "Test: canvas_write_ppm - passed"
        else
            print *, "Test: canvas_write_ppm - passed"
        end if
        
    end subroutine run_test_canvas
    
end module test_canvas
