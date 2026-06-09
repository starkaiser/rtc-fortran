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

program rtc_chapter2

    use rtc_tuple
    use rtc_color
    use rtc_canvas
    
    implicit none
    
    type(tuple) :: proj_position, proj_velocity, env_gavity, env_wind
    type(canvas) :: c
    integer :: tick
    
    proj_position = make_point(0.0, 1.0, 0.0)
    proj_velocity = make_vector(10.0, 25.0, 0.0)
    env_gavity = make_vector(0.0, -0.8, 0.0)
    env_wind = make_vector(-0.01, 0.0, 0.0)
    tick = 0
    c = make_canvas(620, 450)
    
    do
        proj_position = tuple_add(proj_position, proj_velocity)
        proj_velocity = tuple_add(proj_velocity, tuple_add(env_gavity, env_wind))
        tick = tick +1
        print *, "Projectile position is ", proj_position
        call canvas_write_pixel(c, &
                                int(proj_position%x), &
                                int(proj_position%y), &
                                color(1.0, 0.5, 0.1))
        if (proj_position%y <= 0.0) then
            print *, "Number of ticks is ", tick
            exit
        end if
    end do
    
    call canvas_write_ppm(c,"../output/chapter2.ppm")
    
end program rtc_chapter2
