!==============================================================================!
  subroutine File_Mod_Eps_Solid(x1, y1, x2, y2, x3, y3, r, g, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real(RP) :: x1, y1, x2, y2, x3, y3
  real(RP) :: r, g, b
!==============================================================================!

  ! Create solid
  write(FU, "(i2,     a)", advance="no")  1,     " slw "  ! line width = 1
  write(FU, "(2f12.5, a)", advance="no")  x1, y1, " m "
  write(FU, "(2f12.5, a)", advance="no")  x2, y2, " l "
  write(FU, "(2f12.5, a)", advance="no")  x3, y3, " l cp "

  ! Set fill color
  write(FU, "(3f6.3, a)", advance="no")  r, g, b, " srgb "

  ! Fill and stroke
  write(FU, "(a)")  " f s "

  end subroutine
