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

module rtc_sphere
    
    use rtc_tuple
    use rtc_matrix, only: matrix_identity

    implicit none
    
    private
    public :: sphere, make_sphere, normal_at
    
    type :: sphere
        real :: id
        real :: r
        type(tuple) :: orig
        real :: transform(4,4)
    end type sphere
    
contains

    function make_sphere(r, o) result(s)
        real, intent(in) :: r
        type(tuple), intent(in) :: o
        type(sphere) :: s
        s = sphere(0.0, r, o, matrix_identity())
        call random_number(s%id)
    end function make_sphere
    
    pure function normal_at(s, p) result(n)
        type(sphere), intent(in) :: s
        type(tuple), intent(in) :: p
        type(tuple) :: n
        if (tuple_equal(p, make_point(1.0, 0.0, 0.0))) then
            n = make_vector(1.0, 0.0, 0.0)
        else if (tuple_equal(p, make_point(0.0, 1.0, 0.0))) then
            n = make_vector(0.0, 1.0, 0.0)
        else if (tuple_equal(p, make_point(0.0, 0.0, 1.0))) then
            n = make_vector(0.0, 0.0, 1.0)
        else
            n = tuple_normalize(make_vector(p%x, p%y, p%z))
        end if
    end function normal_at

end module rtc_sphere
