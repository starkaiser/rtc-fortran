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

module test_trans

    use rtc_utils,  only: pi
    use rtc_trans
    use rtc_tuple,  only: tuple, tuple_equal, make_point, make_vector
    use rtc_matrix, only: matrix_mul_tuple, matrix_multiply, matrix_inverse

    implicit none
    
    private
    public :: run_test_trans
    
contains

    subroutine run_test_trans()
        real :: translation(4,4), inv(4,4), trans(4,4), rot(4,4)
        type(tuple) :: p, v
        
        ! ========== trans_translation ==========
        trans = trans_translation(5.0, -3.0, 2.0)
        p = make_point(-3.0, 4.0, 5.0)
        
        if (tuple_equal(matrix_mul_tuple(trans, p), make_point(2.0, 1.0, 7.0))) then
            print *, "Test: trans_translation - passed"
        else
            print *, "Test: trans_translation - failed"
        end if
        
        translation = trans_translation(5.0, -3.0, 2.0)
        p = make_point(-3.0, 4.0, 5.0)
        inv = matrix_inverse(translation)
        p = make_point(-3.0, 4.0, 5.0)
        
        if (tuple_equal(matrix_mul_tuple(inv, p), make_point(-8.0, 7.0, 3.0))) then
            print *, "Test: trans_translation 2 - passed"
        else
            print *, "Test: trans_translation 2 - failed"
        end if
        
        v = make_vector(-3.0, 4.0, 5.0)
        
        if (tuple_equal(matrix_mul_tuple(trans, v), v)) then
            print *, "Test: trans_translation 3 - passed"
        else
            print *, "Test: trans_translation 3 - failed"
        end if
        
        ! ========== trans_scaling ==========
        trans = trans_scaling(2.0, 3.0, 4.0)
        p = make_point(-4.0, 6.0, 8.0)
        
        if (tuple_equal(matrix_mul_tuple(trans, p), make_point(-8.0, 18.0, 32.0))) then
            print *, "Test: trans_scaling - passed"
        else
            print *, "Test: trans_scaling - failed"
        end if
        
        v = make_vector(-4.0, 6.0, 8.0)
        
        if (tuple_equal(matrix_mul_tuple(trans, v), make_vector(-8.0, 18.0, 32.0))) then
            print *, "Test: trans_scaling 2 - passed"
        else
            print *, "Test: trans_scaling 2 - failed"
        end if
        
        inv = matrix_inverse(trans)
        
        if (tuple_equal(matrix_mul_tuple(inv, v), make_vector(-2.0, 2.0, 2.0))) then
            print *, "Test: trans_translation 3 - passed"
        else
            print *, "Test: trans_translation 3 - failed"
        end if
        
        ! ========== trans_reflection_x ==========
        trans = trans_reflection_x()
        p = make_point(2.0, 3.0, 4.0)
        
        if (tuple_equal(matrix_mul_tuple(trans, p), make_point(-2.0, 3.0, 4.0))) then
            print *, "Test: trans_reflection_x - passed"
        else
            print *, "Test: trans_reflection_x - failed"
        end if
        
        ! ========== trans_reflection_y ==========
        trans = trans_reflection_y()
        p = make_point(2.0, 3.0, 4.0)
        
        if (tuple_equal(matrix_mul_tuple(trans, p), make_point(2.0, -3.0, 4.0))) then
            print *, "Test: trans_reflection_y - passed"
        else
            print *, "Test: trans_reflection_y - failed"
        end if
        
        ! ========== trans_reflection_z ==========
        trans = trans_reflection_z()
        p = make_point(2.0, 3.0, 4.0)
        
        if (tuple_equal(matrix_mul_tuple(trans, p), make_point(2.0, 3.0, -4.0))) then
            print *, "Test: trans_reflection_z - passed"
        else
            print *, "Test: trans_reflection_z - failed"
        end if
        
        ! ========== trans_rotation_x ==========
        p = make_point(0.0, 1.0, 0.0)
        rot = trans_rotation_x(pi/4.0)
        inv = matrix_inverse(rot)
        
        if (tuple_equal(matrix_mul_tuple(inv, p), &
                        make_point(0.0, sqrt(2.0)/2.0, -sqrt(2.0)/2.0))) then
            print *, "Test: trans_rotation_x - passed"
        else
            print *, "Test: trans_rotation_x - failed"
        end if
        
        ! ========== trans_rotation_y ==========
        p = make_point(0.0, 0.0, 1.0)
        rot = trans_rotation_y(pi/4.0)
        
        if (tuple_equal(matrix_mul_tuple(rot, p), &
                        make_point(sqrt(2.0)/2.0, 0.0, sqrt(2.0)/2.0))) then
            print *, "Test: trans_rotation_y - passed"
        else
            print *, "Test: trans_rotation_y - failed"
        end if
        
        ! ========== trans_rotation_z ==========
        p = make_point(0.0, 1.0, 0.0)
        rot = trans_rotation_z(pi/4.0)
        
        if (tuple_equal(matrix_mul_tuple(rot, p), &
                        make_point(-sqrt(2.0)/2.0, sqrt(2.0)/2.0, 0.0))) then
            print *, "Test: trans_rotation_z - passed"
        else
            print *, "Test: trans_rotation_z - failed"
        end if
        
        ! ========== trans_shearing ==========
        trans = trans_shearing(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        p = make_point(2.0, 3.0, 4.0)
        
        if (tuple_equal(matrix_mul_tuple(trans, p), make_point(5.0, 3.0, 4.0))) then
            print *, "Test: trans_shearing - passed"
        else
            print *, "Test: trans_shearing - failed"
        end if
        
    end subroutine run_test_trans

end module test_trans
