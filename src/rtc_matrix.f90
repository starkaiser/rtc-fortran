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

module rtc_matrix

    use rtc_utils, only: is_equal
    use rtc_tuple, only: tuple, tuple_to_matrix

    implicit none
    
    private
    public :: matrix_make, matrix_print, matrix_equal, matrix_identity
    public :: matrix_multiply, matrix_mul_tuple, matrix_transpose, matrix_submatrix, matrix_inverse
    
contains

    ! ==================================================
    ! Private procedures
    ! ==================================================
    
    pure function matrix_cofactor_2x2(a, row, col) result(r)
        real, intent(in) :: a(2,2)
        integer, intent(in) :: row, col
        real :: m(1,1), r
        
        m = matrix_submatrix(a, row, col)
        if (mod(row+col, 2) == 0) then
            r = m(1,1)
        else
            m = -m(1,1)
        end if
    end function matrix_cofactor_2x2
    
    pure function matrix_det_2x2(a) result(det)
        real, intent(in) :: a(2,2)
        real :: det
        det = a(1,1)*a(2,2) - a(1,2)*a(2,1)
    end function matrix_det_2x2
    
    pure function matrix_minor_3x3(a, row, col) result(m)
        real, intent(in) :: a(3,3)
        integer, intent(in) :: row, col
        real :: m
        m = matrix_det_2x2(matrix_submatrix(a, row, col))
    end function matrix_minor_3x3
    
    pure function matrix_cofactor_3x3(a, row, col) result(m)
        real, intent(in) :: a(3,3)
        integer, intent(in) :: row, col
        real :: m
        if (mod(row+col, 2) == 0) then
            m = matrix_minor_3x3(a, row, col)
        else
            m = -matrix_minor_3x3(a, row, col)
        end if
    end function matrix_cofactor_3x3
    
    pure function matrix_det_3x3(a) result(m)
        real, intent(in) :: a(3,3)
        real :: m
        integer :: i
        m = 0.0
        do i = 1, 3
            m = m + a(1,i) * matrix_cofactor_3x3(a, 1, i)
        end do
    end function matrix_det_3x3
    
    pure function matrix_minor_4x4(a, row, col) result(m)
        real, intent(in) :: a(4,4)
        integer, intent(in) :: row, col
        real :: m
        m = matrix_det_3x3(matrix_submatrix(a, row, col))
    end function matrix_minor_4x4
    
    pure function matrix_cofactor_4x4(a, row, col) result(m)
        real, intent(in) :: a(4,4)
        integer, intent(in) :: row, col
        real :: m
        if (mod(row+col, 2) == 0) then
            m = matrix_minor_4x4(a, row, col)
        else
            m = -matrix_minor_4x4(a, row, col)
        end if
    end function matrix_cofactor_4x4
    
    pure function matrix_det_4x4(a) result(m)
        real, intent(in) :: a(4,4)
        real :: m
        integer :: i
        m = 0.0
        do i = 1, 4
            m = m + a(1,i) * matrix_cofactor_4x4(a, 1, i)
        end do
    end function matrix_det_4x4
    
    ! ==================================================
    ! Public procedures
    ! ==================================================
    
    pure function matrix_make(v) result(m)
        real, intent(in) :: v(:)
        integer :: i
        real, allocatable :: m(:,:)
        i = int(sqrt(real(size(v))))
        
        allocate(m(i,i))
        m = transpose(reshape(v, [i,i]))
    end function matrix_make
    
    subroutine matrix_print(m)
        real, intent(in) :: m(:,:)
        integer :: i, j
        do i = 1, size(m, 1)
            do j = 1, size(m, 2)
                if (j /= size(m,2)) then ! statement used to not have | at the end of a line
                    write(*, '(F10.5, A)', advance='no') m(i,j), " | "
                else
                    write(*, '(F10.5)', advance='no') m(i,j)
                end if
            end do
            write(*, '(A)') ""
        end do
    end subroutine matrix_print
    
    pure logical function matrix_equal(a, b)
        real, intent(in) :: a(:,:), b(:,:)
        matrix_equal = all(is_equal(a,b))
    end function matrix_equal
    
    pure function matrix_identity() result(m)
        real :: m(4,4)
        m = 0.0
        m(1,1) = 1.0
        m(2,2) = 1.0
        m(3,3) = 1.0
        m(4,4) = 1.0
    end function matrix_identity

    pure function matrix_multiply(a, b) result(m)
        real, intent(in) :: a(4,4), b(4,4)
        real :: m(4,4)
        m = matmul(a, b)
    end function matrix_multiply
    
    pure function matrix_mul_tuple(a, t) result(r)
        real, intent(in) :: a(4,4)
        type(tuple), intent(in) :: t
        real :: m(4), temp(4)
        type(tuple) :: r
        temp = tuple_to_matrix(t)
        m = matmul(a, temp)
        r = tuple(m(1), m(2), m(3), m(4))
    end function matrix_mul_tuple
    
    pure function matrix_transpose(a) result(b)
        real, intent(in) :: a(:,:)
        real, allocatable :: b(:,:)
        allocate(b(size(a,2), size(a,1)))
        b = transpose(a)
    end function matrix_transpose
    
    pure function matrix_submatrix(a, row, col) result(b)
        real, intent(in) :: a(:,:)
        integer, intent(in) :: row, col
        real, allocatable :: b(:,:)
        integer :: i,j,ii,jj, n, m
        n = size(a,1)
        m = size(a,2)
        allocate(b(n-1, m-1))
        
        ii = 0
        do i = 1, n
            if (i == row) cycle
            ii = ii + 1
            jj = 0
            do j = 1, m
                if (j == col) cycle
                jj = jj + 1
                b(ii,jj) = a(i,j)
            end do
        end do
    end function matrix_submatrix
    
    pure function matrix_det(a) result(det)
        real, intent(in) :: a(:,:)
        real :: det
        if (size(a,1) == 2) then
            det = matrix_det_2x2(a)
        else if (size(a,1) == 3) then
            det = matrix_det_3x3(a)
        else if (size(a,1) == 4) then
            det = matrix_det_4x4(a)
        end if
    end function matrix_det
    
    pure function matrix_cofactor(a, row, col) result(c)
        real, intent(in) :: a(:,:)
        integer, intent(in) :: row, col
        real :: c
        if (size(a,1) == 2) then
            c = matrix_cofactor_2x2(a, row, col)
        else if (size(a,1) == 3) then
            c = matrix_cofactor_3x3(a, row, col)
        else if (size(a, 1) == 4) then
            c = matrix_cofactor_4x4(a, row, col)
        end if
    end function matrix_cofactor
    
    function matrix_inverse(a) result(b)
        real, intent(in) :: a(:,:)
        real :: det, c
        real, allocatable :: b(:,:)
        integer :: i, j
        
        det = matrix_det(a)
        if (det == 0) then
            error stop "Matrix is not invertible"
        end if
        
        allocate(b(size(a,1), size(a,2)))
        
        do i = 1, size(a,1)
            do j = 1, size(a, 2)
                c = matrix_cofactor(a, i, j)
                b(j,i) = c / matrix_det(a)
            end do
        end do
    end function matrix_inverse
    
end module rtc_matrix
