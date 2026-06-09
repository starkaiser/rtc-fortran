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

module test_tuple

    use rtc_utils, only: is_equal
    use rtc_tuple
    
    implicit none
    
    private
    public :: run_test_tuple
    
    type(tuple) :: point, vector, tuple1, tuple2, tuple_result
    real :: scalar, r

contains
    
    subroutine run_test_tuple()
    
        point = make_point(4.3, -4.2, 3.1)
        vector = make_vector(4.3, -4.2, 3.1)
        
        ! ========== make_point ==========
        if (is_equal(point%w, 1.0)) then
            print *, "Test: make_point - passed"
        else
            print *, "Test: make_point - failed"
        end if
        
        ! ========== make_vector ==========
        if (is_equal(vector%w, 0.0)) then
            print *, "Test: make_vector - passed"
        else
            print *, "Test: make_vector - failed"
        end if
        
        ! ========== tuple_equal ========== 
        tuple1 = tuple(3.0, -2.0, 5.0, 1.0)
        tuple2 = tuple(3.0, -2.0, 5.0, 1.0)
        if (tuple_equal(tuple1, tuple2)) then
            print *, "Test: tuple_equal - passed"
        else
            print *, "Test: tuple_equal - failed"
        end if
        
        ! ========== tuple_add ==========
        tuple2 = tuple(-2.0, 3.0, 1.0, 0.0)
        tuple_result = tuple_add(tuple1, tuple2)
        if (tuple_equal(tuple_result, tuple(1.0, 1.0, 6.0, 1.0))) then
            print *, "Test: tuple_add - passed"
        else
            print *, "Test: tuple_add - failed"
        end if
        
        ! ========== tuple_subtract>point-point ==========
        tuple1 = make_point(3.0, 2.0, 1.0)
        tuple2 = make_point(5.0, 6.0, 7.0)
        tuple_result = tuple_subtract(tuple1, tuple2)
        
        if (tuple_equal(tuple_result, tuple(-2.0, -4.0, -6.0, 0.0))) then
            print *, "Test: tuple_subtract>point-point - passed"
        else
            print *, "Test: tuple_subtract>point-point - failed"
        end if
        
        ! ========== tuple_subtract>point-vector ==========
        tuple2 = make_vector(5.0, 6.0, 7.0)
        tuple_result = tuple_subtract(tuple1, tuple2)
        if (tuple_equal(tuple_result, tuple(-2.0, -4.0, -6.0, 1.0))) then
            print *, "Test: tuple_subtract>point-vector - passed"
        else
            print *, "Test: tuple_subtract>point-vector - failed"
        end if
        
        ! ========== tuple_subtract>vector-vector ==========
        tuple1 = make_vector(3.0, 2.0, 1.0)
        tuple2 = make_vector(5.0, 6.0, 7.0)
        tuple_result = tuple_subtract(tuple1, tuple2)
        if (tuple_equal(tuple_result, tuple(-2.0, -4.0, -6.0, 0.0))) then
            print *, "Test: tuple_subtract>vector-vector - passed"
        else
            print *, "Test: tuple_subtract>vector-vector - failed"
        end if
        
        ! ========== tuple_negate ==========
        tuple1 = tuple(1.0, -2.0, 3.0, -4.0)
        tuple_result = tuple_negate(tuple1)
        if (tuple_equal(tuple_result, tuple(-1.0, 2.0, -3.0, 4.0))) then
            print *, "Test: tuple_negate - passed"
        else
            print *, "Test: tuple_negate - failed"
        end if
        
        ! ========== tuple_multiply ==========
        tuple1 = tuple(1.0, -2.0, 3.0, -4.0)
        scalar = 0.5
        tuple_result = tuple_multiply(tuple1, scalar)
        if (tuple_equal(tuple_result, tuple(0.5, -1.0, 1.5, -2.0))) then
            print *, "Test: tuple_multiply - passed"
        else
            print *, "Test: tuple_multiply - failed"
        end if
        
        ! ========== tuple_divide ==========
        tuple1 = tuple(1.0, -2.0, 3.0, -4.0)
        scalar = 2
        tuple_result = tuple_divide(tuple1, scalar)
        if (tuple_equal(tuple_result, tuple(0.5, -1.0, 1.5, -2.0))) then
            print *, "Test: tuple_divide - passed"
        else
            print *, "Test: tuple_divide - failed"
        end if
        
        ! ========== tuple_magnitude ==========
        tuple1 = make_vector(0.0, 0.0, 1.0)
        r = tuple_magnitude(tuple1)
        if (is_equal(r, 1.0)) then
            print *, "Test: tuple_magnitude - passed"
        else
            print *, "Test: tuple_magnitude - failed"
        end if
        
        ! ========== tuple_normalize ==========
        tuple1 = make_vector(1.0, 2.0, 3.0)
        tuple_result = tuple_normalize(tuple1)
        if (tuple_equal(tuple_result, tuple(0.26726, 0.53452, 0.80178, 0.0))) then
            print *, "Test: tuple_normalize - passed"
        else
            print *, "Test: tuple_normalize - failed"
        end if
        
        ! ========== tuple_dot ==========
        tuple1 = make_vector(1.0, 2.0, 3.0)
        tuple2 = make_vector(2.0, 3.0, 4.0)
        r = tuple_dot(tuple1, tuple2)
        if (is_equal(r, 20.0)) then
            print *, "Test: tuple_dot - passed"
        else
            print *, "Test: tuple_dot - failed"
        end if
        
        ! ========== tuple_cross ==========
        tuple1 = make_vector(1.0, 2.0, 3.0)
        tuple2 = make_vector(2.0, 3.0, 4.0)
        if (tuple_equal(tuple_cross(tuple1, tuple2), tuple(-1.0, 2.0, -1.0, 0.0)) .and. &
           tuple_equal(tuple_cross(tuple2, tuple1), tuple(1.0, -2.0, 1.0, 0.0))) then
            print *, "Test: tuple_cross - passed"
        else
            print *, "Test: tuple_cross - failed"
        end if

    end subroutine run_test_tuple

end module test_tuple
