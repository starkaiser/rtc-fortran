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

module rtc_canvas

    use rtc_color, only: color
    
    implicit none
    
    private
    public :: canvas, make_canvas, canvas_write_pixel, canvas_pixel_at, canvas_write_ppm
    
    type :: canvas
        integer :: width, height
        type(color), allocatable :: pixels(:,:)
    end type canvas
    
contains
    
    ! ==================================================
    ! Private procedures
    ! ==================================================
    
    pure function int_to_string(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=20) :: tmp
        write(tmp, '(I0)') i
        s = trim(tmp)
    end function int_to_string
    
    pure function scale_color_component(c) result(r)
        real, intent(in) :: c
        integer :: r
        real :: clamped
        clamped = max(0.0, min(1.0, c))
        r = nint(clamped * 255.0)
    end function scale_color_component
    
    pure function canvas_to_ppm(c) result(ppm)
        type(canvas), intent(in) :: c
        character(:), allocatable :: ppm
        character(len=:), allocatable :: line
        integer :: x, y
        character(len=12) :: pixel_str
        character(len=:), allocatable :: body
        integer :: r, g, b
        
        ppm = "P3" // new_line('a') // &
              trim(adjustl(int_to_string(c%width))) // " " // &
              trim(adjustl(int_to_string(c%height))) // new_line('a') // &
              "255" // new_line('a')
        body = ""
        
        do y = c%height, 1, -1
            line = ""
            do x = 1, c%width
                r = scale_color_component(c%pixels(x,y)%r)
                g = scale_color_component(c%pixels(x,y)%g)
                b = scale_color_component(c%pixels(x,y)%b)
                
                write(pixel_str, '(1X,I0,1X,I0,1X,I0,1X)') r, g, b
                
                if (len_trim(line) + len_trim(pixel_str) > 70) then
                    body = body // trim(line) // new_line('a')
                    line = trim(pixel_str)
                else
                    if (len_trim(line) > 0) line = trim(line) // " "
                    line = trim(line) // trim(pixel_str)
                end if
            end do
            body = body // trim(line) // new_line('a')
        end do
        
        ppm = ppm // body
    end function canvas_to_ppm
    
    ! ==================================================
    ! Public procedures
    ! ==================================================
    
    pure function make_canvas(width, height) result(c)
        integer, intent(in) :: width, height
        type(canvas) :: c
        allocate(c%pixels(width, height))
        c%width = width
        c%height = height
        c%pixels = color(0.0, 0.0, 0.0)
    end function make_canvas
    
    subroutine canvas_write_pixel(c, x, y, col)
        type(canvas), intent(inout) :: c
        integer, intent(in) :: x, y
        type(color), intent(in) :: col
        if (x >= 1 .and. x <= c%width .and. y >= 1 .and. y <= c%height) then
            c%pixels(x, y) = col
        end if
    end subroutine canvas_write_pixel
    
    pure function canvas_pixel_at(c, x, y) result(col)
        type(canvas), intent(in) :: c
        integer, intent(in) :: x, y
        type(color) :: col
        if (x >= 1 .and. x <= c%width .and. y >= 1 .and. y <= c%height) then
            col = c%pixels(x, y)
        else
            col = color(0.0, 0.0, 0.0)
        end if
    end function canvas_pixel_at
    
    subroutine canvas_write_ppm(c, file_path)
        type(canvas), intent(in) :: c
        character(len=*), intent(in) :: file_path
        character(:), allocatable :: ppm
        
        ppm = canvas_to_ppm(c)
        
        open(unit=10, file=file_path, status='replace')
        write(10, '(A)') ppm
        close(10)
    end subroutine canvas_write_ppm
    
end module rtc_canvas
