!==============================================================================!
  subroutine File_Mod_Fig_Circle(xc, yc, rad, style, width,  &
                                 line_color, fill_color)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer  :: xc, yc, rad, style, width, line_color, fill_color
!==============================================================================!

  write(FU, "(a,4i2,a,8i12)")  " 1 3",                                &
                               style, width, line_color, fill_color,  &
                               " 25 -1 20 0.000 1 0.0000",            &
                               xc, yc, rad,    rad,                   &
                               xc, yc, rad+xc, yc

  end subroutine
