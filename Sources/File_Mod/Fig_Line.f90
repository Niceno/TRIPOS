!==============================================================================!
  subroutine File_Mod_Fig_Line(x1, y1, x2, y2, style, width, color, dash_length)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer  :: x1, y1, x2, y2, style, width, color
  real(RP) :: dash_length
!==============================================================================!

  write(FU, "(a,3i2,a,f5.3,a)")  " 2 1",               &
                                 style, width, color,  &
                                 " 7 0 0 -1 ",         &
                                 dash_length,          &
                                 " 0 0 -1 0 0 2"

  write(FU, "(a,4i12)")  "       ", x1, y1, x2, y2

  end subroutine
