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

module test_sphere

    use rtc_utils, only: is_equal
    use rtc_tuple
    use rtc_sphere

    implicit none
    
    private
    public :: run_test_sphere
    
contains

    subroutine run_test_sphere()
        type(sphere) :: s
        type(tuple) :: p, n
        
        s = make_sphere(1.0, make_point(0.0, 0.0, 0.0))
        p = make_point(1.0, 0.0, 0.0)
        
        n = normal_at(s, p)
        
        if (tuple_equal(n, make_vector(1.0, 0.0, 0.0))) then
            print *, "Test: sphere normal_at 1 - passed"
        else
            print *, "Test: sphere normal_at  1 - failed"
        end if
        
        p = make_point(0.0, 1.0, 0.0)
        n = normal_at(s, p)
        if (tuple_equal(n, make_vector(0.0, 1.0, 0.0))) then
            print *, "Test: sphere normal_at  2 - passed"
        else
            print *, "Test: sphere normal_at  2 - failed"
        end if
        
        p = make_point(0.0, 0.0, 1.0)
        n = normal_at(s, p)
        if (tuple_equal(n, make_vector(0.0, 0.0, 1.0))) then
            print *, "Test: sphere normal_at  3 - passed"
        else
            print *, "Test: sphere normal_at  3 - failed"
        end if
        
        p = make_point(sqrt(3.0)/3.0, sqrt(3.0)/3.0, sqrt(3.0)/3.0)
        n = normal_at(s, p)
        if (tuple_equal(n, make_vector(sqrt(3.0)/3.0, sqrt(3.0)/3.0, sqrt(3.0)/3.0))) then
            print *, "Test: sphere normal_at  4 - passed"
        else
            print *, "Test: sphere normal_at  4 - failed"
        end if
    end subroutine run_test_sphere
    
end module test_sphere
