!==============================================================================!
  real(RP) function Solver_Mod_Root_Mean_Square(ni, r)
!------------------------------------------------------------------------------!
!   Calculates root means square of vector r without normalization.            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: ni
  type(Vector_Type) :: r
!-----------------------------------[Locals]-----------------------------------!
  real(RP) :: rms
  integer  :: i
!==============================================================================!

  ! Compute rms normalizing it with main diagonal in the system matrix
  rms = 0.0
  do i = 0, ni-1
    rms = rms + r % val(i)**2
  end do
  rms = sqrt(rms)

  Solver_Mod_Root_Mean_Square = rms

  end function
