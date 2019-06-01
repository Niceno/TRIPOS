!==============================================================================!
  subroutine File_Mod_Eps_Circle(lw, x1, y1, rad, r, g, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer  :: lw
  real(RP) :: x1, y1, rad
  real(RP) :: r, g, b
!==============================================================================!

  write(FU, "(i2,     a)", advance="no")  lw, " slw "
  write(FU, "(3f6.3,  a)", advance="no")  r, g, b, " srgb "
  write(FU, "(5f12.5, a)", advance="no")  x1, y1, rad, 0.0, 360.0, " arc "
  write(FU, "(a)")                        " s "

  end subroutine
