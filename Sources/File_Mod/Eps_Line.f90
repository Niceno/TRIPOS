!==============================================================================!
  subroutine File_Mod_Eps_Line(lw, x1, y1, x2, y2, r, g, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer  :: lw
  real(RP) :: x1, y1, x2, y2
  real(RP) :: r, g, b
!==============================================================================!

  write(FU, "(i2,     a)", advance="no")  lw, " slw "
  write(FU, "(3f6.3,  a)", advance="no")  r, g, b, " srgb "
  write(FU, "(2f12.5, a)", advance="no")  x1, y1, " m "
  write(FU, "(2f12.5, a)", advance="no")  x2, y2, " l "
  write(FU, "(a)")                        " s "

  end subroutine
