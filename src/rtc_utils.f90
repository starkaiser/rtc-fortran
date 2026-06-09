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

module rtc_utils
    
    implicit none
    
    private :: eps
    real, parameter :: eps = 0.00001
    
    public :: is_equal, pi
    real, parameter :: pi = 3.14159
    
contains
    
    pure elemental logical function is_equal(a, b)
        real, intent(in) :: a, b
        is_equal = abs(a - b) < eps
    end function is_equal
    
    pure function deg_to_rad(deg) result(rad)
        real, intent(in) :: deg
        real :: rad
        rad = (deg*pi) / 180.0
    end function deg_to_rad
    
    pure function rad_to_deg(rad) result(deg)
        real, intent(in) :: rad
        real :: deg
        deg = (rad*180.0) / pi
    end function rad_to_deg
    
end module rtc_utils
