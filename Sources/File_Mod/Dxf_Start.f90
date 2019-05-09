!==============================================================================!
  subroutine File_Mod_Dxf_Start
!------------------------------------------------------------------------------!
!   Subroutine to start a .dxf file.                                           !
!==============================================================================!

  open(unit=FU, file=dxf_name)

  write(FU, "(a)")  "0"
  write(FU, "(a)")  "SECTION"
  write(FU, "(a)")  "2"
  write(FU, "(a)")  "ENTITIES"

  end subroutine
