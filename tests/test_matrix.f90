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

module test_matrix

    use rtc_matrix
    use rtc_tuple, only: tuple, tuple_equal

    implicit none
    
contains
    
    subroutine run_test_matrix()
    
        real :: a(4,4), b(4,4), r(4,4), i(4,4), i_identity(4,4), c(2,2), d(3,3)
        type(tuple) :: t
        
        ! ========== matrix_make ==========
        a = matrix_make([1.0, 2.0, 3.0, 4.0, &
                         5.0, 6.0, 7.0, 8.0, &
                         9.0, 10.0, 11.0, 12.0, &
                         13.0, 14.0, 15.0, 16.0])
        b = matrix_make([1.0, 2.0, 3.0, 4.0, &
                         5.0, 6.0, 7.0, 8.0, &
                         9.0, 10.0, 11.0, 12.0, &
                         13.0, 14.0, 15.0, 16.0])
        print *, "Test: matrix_make - passed"
        
        ! ========== matrix_print ==========
        !call matrix_print(a)
        print *, "Test: matrix_print - passed"
        
        ! ========== matrix_equal ==========
        if (matrix_equal(a, b)) then
            print *, "Test: matrix_equal - passed"
        else
            print *, "Test: matrix_equal - failed"
        end if
        
        ! ========== matrix_identity ==========
        i = matrix_make([1.0,0.0,0.0,0.0, &
                         0.0,1.0,0.0,0.0, &
                         0.0,0.0,1.0,0.0, &
                         0.0,0.0,0.0,1.0])
        i_identity = matrix_identity()
        
        if (matrix_equal(i, i_identity)) then
            print *, "Test: matrix_identity - passed"
        else
            print *, "Test: matrix_identity - failed"
        end if
        
        ! ========== matrix_multiply ==========
        a = matrix_make([1.0,2.0,3.0,4.0, &
                         5.0,6.0,7.0,8.0, &
                         9.0,8.0,7.0,6.0, &
                         5.0,4.0,3.0,2.0])
        b = matrix_make([-2.0,1.0,2.0,3.0, &
                          3.0,2.0,1.0,-1.0, &
                          4.0,3.0,6.0,5.0, &
                          1.0,2.0,7.0,8.0])
        r = matrix_make([20.0,22.0,50.0,48.0, &
                         44.0,54.0,114.0,108.0, &
                         40.0,58.0,110.0,102.0, &
                         16.0,26.0,46.0,42.0])
        
        if (matrix_equal(matrix_multiply(a, b), r)) then
            print *, "Test: matrix_multiply - passed"
        else
            print *, "Test: matrix_multiply - failed"
        end if
        
        ! ========== matrix_mul_tuple ==========
        t = tuple(1.0, 2.0, 3.0, 1.0)
        a = matrix_make([1.0,2.0,3.0,4.0, &
                         2.0,4.0,4.0,2.0, &
                         8.0,6.0,4.0,1.0, &
                         0.0,0.0,0.0,1.0])
        
        if (tuple_equal(matrix_mul_tuple(a, t), tuple(18.0, 24.0, 33.0, 1.0))) then
            print *, "Test: matrix_mul_tuple - passed"
        else
            print *, "Test: matrix_mul_tuple - failed"
        end if
        
        ! ========== matrix_transpose ==========
        a = matrix_make([0.0,9.0,3.0,0.0, &
                         9.0,8.0,0.0,8.0, &
                         1.0,8.0,5.0,3.0, &
                         0.0,0.0,5.0,8.0])
        b = reshape([0.0,9.0,3.0,0.0, &
                         9.0,8.0,0.0,8.0, &
                         1.0,8.0,5.0,3.0, &
                         0.0,0.0,5.0,8.0], [4,4])
        if (matrix_equal(matrix_transpose(a), b)) then
            print *, "Test: matrix_transpose - passed"
        else
            print *, "Test: matrix_transpose - failed"
        end if
        
        ! ========== matrix_submatrix ==========
        d = matrix_make([1.0,5.0,0.0, &
                        -3.0,2.0,7.0, &
                         0.0,6.0,-3.0])
        c = matrix_make([-3.0, 2.0, &
                          0.0, 6.0])
        if (matrix_equal(matrix_submatrix(d, 1, 3), c)) then
            print *, "Test: matrix_submatrix - passed"
        else
            print *, "Test: matrix_submatrix - failed"
        end if
        
        ! ========== matrix_inverse ==========
        a = matrix_make([8.0,-5.0,9.0,2.0, &
                         7.0,5.0,6.0,1.0, &
                         -6.0,0.0,9.0,6.0, &
                         -3.0,0.0,-9.0,-4.0])
        b = matrix_make([-0.15385,-0.15385,-0.28205,-0.53846, &
                          -0.07692,0.12308,0.02564,0.03077, &
                          0.35897,0.35897,0.43590,0.92308, &
                          -0.69231,-0.69231,-0.76923,-1.92308])
        r = matrix_inverse(a)
        !call matrix_print(r)
        if (matrix_equal(r, b)) then
            print *, "Test: matrix_inverse - passed"
        else
            print *, "Test: matrix_inverse - failed"
        end if
        
        a = matrix_make([3.0,-9.0,7.0,3.0, &
                         3.0,-8.0,2.0,-9.0, &
                         -4.0,4.0,4.0,1.0, &
                         -6.0,5.0,-1.0,1.0])
        b = matrix_make([8.0,2.0,2.0,2.0, &
                         3.0,-1.0,7.0,0.0, &
                         7.0,0.0,5.0,4.0, &
                         6.0,-2.0,0.0,5.0])
        r = matrix_multiply(a, b)
        if (matrix_equal(matrix_multiply(r,matrix_inverse(b)), a)) then
            print *, "Test: matrix_inverse 2 - passed"
        else
            print *, "Test: matrix_inverse 2 - failed"
        end if
        
    end subroutine run_test_matrix

end module test_matrix
