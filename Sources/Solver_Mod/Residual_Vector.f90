!==============================================================================!
  subroutine Solver_Mod_Residual_Vector(ni, r, b, a, x)
!------------------------------------------------------------------------------!
!   Calculates residual vector {r} = {b} - [A]{x}                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: ni
  type(Vector_Type) :: r  ! this might be only for inner cells
  type(Vector_Type) :: b  ! this might be only for inner cells
  type(Matrix_Type) :: a
  type(Vector_Type) :: x  ! this may incude buffer cells
!-----------------------------------[Locals]-----------------------------------!
  integer  :: i, j, k
!==============================================================================!

  !----------------!
  !   r = b - Ax   !
  !----------------!
  do i = 0, ni-1
    r % val(i) = b % val(i)
    do j = a % row(i), a % row(i+1) - 1
      k = a % col(j)
      r % val(i) = r % val(i) - a % val(j) * x % val(k)
    end do
  end do

  end subroutine
