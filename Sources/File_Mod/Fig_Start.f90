!==============================================================================!
  subroutine File_Mod_Fig_Start
!------------------------------------------------------------------------------!
!   Subroutine to start a .fig file.                                           !
!==============================================================================!

  open(unit=FU, file=fig_name)

  write(FU, "(a)")  "#FIG 3.1"
  write(FU, "(a)")  "Landscape"
  write(FU, "(a)")  "Center"
  write(FU, "(a)")  "Metric"
  write(FU, "(a)")  "1200 2"

  end subroutine
