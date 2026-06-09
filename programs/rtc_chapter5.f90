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

program rtc_chapter5

    use rtc_tuple
    use rtc_color
    use rtc_canvas
    use rtc_ray
    use rtc_sphere
    
    implicit none
    
    type(tuple) :: ray_orig, p
    integer :: canvas_pixels
    type(color) :: c
    real :: pixel_size, wall_z, wall_size, half
    real :: world_x, world_y
    type(canvas) :: canv
    type(sphere) :: myshape
    type(ray) :: r
    type(intersection) :: xs(2)
    
    integer :: x, y
    
    ray_orig = make_point(0.0, 0.0, -5.0)
    wall_z = 10.0
    c = color(1.0, 0.0, 0.0)
    wall_size = 7.0
    canvas_pixels = 100
    pixel_size = wall_size / canvas_pixels
    canv = make_canvas(canvas_pixels, canvas_pixels)
    half = wall_size / 2.0
    myshape = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
    
    do y = 1, canvas_pixels
        world_y = half - pixel_size * y
        do x = 1, canvas_pixels
            world_x = -half + pixel_size * x
            p = make_point(world_x, world_y, wall_z)
            r = ray(ray_orig, tuple_normalize(tuple_subtract(p, ray_orig)))
            xs = ray_intersects(myshape, r)
            if (xs(1)%t /= 0.0 .or. xs(2)%t /= 0.0) then
                call canvas_write_pixel(canv, x, y, c)
            end if
        end do
    end do
    
    call canvas_write_ppm(canv,"../output/chapter5.ppm")
    
end program rtc_chapter5
