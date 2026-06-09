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

program test_program

    use test_tuple,  only: run_test_tuple
    use test_color,  only: run_test_color
    use test_canvas, only: run_test_canvas
    use test_matrix, only: run_test_matrix
    use test_trans,  only: run_test_trans
    use test_ray,    only: run_test_ray
    use test_ray,    only: run_test_sphere

    implicit none

!    print *, "========== Tuple =========="
!    call run_test_tuple()
!    print *, ""
!    print *, "========== Color =========="
!    call run_test_color()
!    print *, ""
!    print *, "========== Canvas =========="
!    call run_test_canvas()
!    print *, ""
!    print *, "========== Matrix =========="
!    call run_test_matrix()
!    print *, ""
!    print *, "========== Transformations =========="
!    call run_test_trans()
!    print *, ""
!    print *, "========== Ray =========="
!    call run_test_ray()
!    print *, ""
    print *, "========== Sphere =========="
    call run_test_sphere()

end program test_program
