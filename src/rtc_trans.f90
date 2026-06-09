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

module rtc_trans
    
    use rtc_matrix, only: matrix_make, matrix_identity
    
    implicit none
    
    private
    public :: trans_translation, trans_scaling
    public :: trans_reflection_x, trans_reflection_y, trans_reflection_z
    public :: trans_rotation_x, trans_rotation_y, trans_rotation_z, trans_shearing
    
contains

    pure function trans_translation(x, y, z) result(a)
        real, intent(in) :: x, y, z
        real :: a(4,4)
        a = matrix_identity()
        a(1,4) = x
        a(2,4) = y
        a(3,4) = z
    end function trans_translation
    
    pure function trans_scaling(x, y, z) result(a)
        real, intent(in) :: x, y, z
        real :: a(4,4)
        a = matrix_identity()
        a(1,1) = x
        a(2,2) = y
        a(3,3) = z
    end function trans_scaling
    
    pure function trans_reflection_x() result(a)
        real :: a(4,4)
        a = trans_scaling(-1.0, 1.0, 1.0)
    end function trans_reflection_x
    
    pure function trans_reflection_y() result(a)
        real :: a(4,4)
        a = trans_scaling(1.0, -1.0, 1.0)
    end function trans_reflection_y
    
    pure function trans_reflection_z() result(a)
        real :: a(4,4)
        a = trans_scaling(1.0, 1.0, -1.0)
    end function trans_reflection_z
    
    pure function trans_rotation_x(rad) result(a)
        real, intent(in) :: rad
        real :: a(4,4)
        a = matrix_identity()
        a(2,2) = cos(rad)
        a(2,3) = -sin(rad)
        a(3,2) = sin(rad)
        a(3,3) = cos(rad)
    end function trans_rotation_x
    
    pure function trans_rotation_y(rad) result(a)
        real, intent(in) :: rad
        real :: a(4,4)
        a = matrix_identity()
        a(1,1) = cos(rad)
        a(1,3) = sin(rad)
        a(3,1) = -sin(rad)
        a(3,3) = cos(rad)
    end function trans_rotation_y
    
    pure function trans_rotation_z(rad) result(a)
        real, intent(in) :: rad
        real :: a(4,4)
        a = matrix_identity()
        a(1,1) = cos(rad)
        a(1,2) = -sin(rad)
        a(2,1) = sin(rad)
        a(2,2) = cos(rad)
    end function trans_rotation_z
    
    pure function trans_shearing(xy,xz,yx,yz,zx,zy) result(a)
        real, intent(in) :: xy, xz, yx, yz, zx, zy
        real :: a(4,4)
        a = matrix_identity()
        a(1,2) = xy
        a(1,3) = xz
        a(2,1) = yx
        a(2,3) = yz
        a(3,1) = zx
        a(3,2) = zy
    end function trans_shearing

end module rtc_trans
