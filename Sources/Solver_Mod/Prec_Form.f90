!==============================================================================!
  subroutine Prec_Form(ni, a, d)
!------------------------------------------------------------------------------!
!   Forms preconditioning matrix "d" from provided matrix "a".                 !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: ni
  type(Matrix_Type) :: a
  type(Matrix_Type) :: d
!-----------------------------------[Locals]-----------------------------------!
  real(RP) :: sum1
  integer  :: i, j, k
!==============================================================================!

  do i = 0, ni-1
    sum1 = a % val(a % dia(i))       ! take diaginal entry
    do j = a % row(i), a % dia(i)-1  ! only lower traingular
      k = a % col(j)
      sum1 = sum1 - d % val(d % dia(k)) * a % val(j) * a % val(j)  
    end do
    d % val(d % dia(i)) = 1.0 / sum1
  end do

  end subroutine
