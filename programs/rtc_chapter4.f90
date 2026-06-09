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

program rtc_chapter4

    use rtc_utils, only: pi
    use rtc_tuple
    use rtc_color
    use rtc_canvas
    use rtc_matrix
    use rtc_trans
    
    implicit none
    
    type(tuple) :: origin, p12, p
    type(canvas) :: c
    
    real :: r, rot(4,4)
    integer :: height, width, i
    height = 600
    width = 600
    r = height / 4.0
    
    origin = make_point(0.0, 0.0, 0.0)
    p12 = make_point(0.0, 0.0, 1.0)
    c = make_canvas(height, width)
    
    do i = 1, 12
        rot = trans_rotation_y(i*pi/6.0)
        p = matrix_mul_tuple(rot, p12)
        call canvas_write_pixel(c, &
                                int(p%x*r+width/2.0), &
                                int(p%z*r+height/2.0), &
                                color(1.0, 0.5, 0.1))
    end do
    
    call canvas_write_ppm(c,"../output/chapter4.ppm")
    
end program rtc_chapter4
