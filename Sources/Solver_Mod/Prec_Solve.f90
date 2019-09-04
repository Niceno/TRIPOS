!==============================================================================!
  subroutine Solver_Mod_Prec_Solve(ni, a, d, x, b)
!------------------------------------------------------------------------------!
!   Solves the preconditioning system [d]{x}={b}                               !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: ni
  type(Matrix_Type) :: a
  type(Matrix_Type) :: d
  type(Vector_Type) :: x
  type(Vector_Type) :: b
!-----------------------------------[Locals]-----------------------------------!
  integer  :: i, j, k
  real(RP) :: sum1
!==============================================================================!

  ! Forward substitutionn
  do i = 0, ni-1
    sum1 = b % val(i)
    do j = a % row(i),a % dia(i)-1     ! only the lower triangular
      k = a % col(j)
      sum1 = sum1 - a % val(j) * x % val(k)
    end do
    x % val(i) = sum1 * d % val(d % dia(i))
  end do

  do i = 0, ni-1
    x % val(i) = x % val(i) / (d % val(d % dia(i)) + SMALL)
  end do

  ! Backward substitution
  do i = ni-1, 0, -1
    sum1 = x % val(i)
    do j = a % dia(i)+1, a % row(i+1)-1                  ! upper triangular
      k = a % col(j)
      if(k <= ni) sum1 = sum1 - a % val(j) * x % val(k)
    end do
    x % val(i) = sum1* d % val(d % dia(i))
  end do

  end subroutine
