!==============================================================================!
  subroutine File_Mod_Dxf_Line(x1, y1, z1, x2, y2, z2, layer)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real(RP)         :: x1, y1, z1, x2, y2, z2
  character(len=*) :: layer
!==============================================================================!

  write(FU, "(a)")        "0"
  write(FU, "(a)")        "LINE"
  write(FU, "(a)")        "8"
  write(FU, "(a)")        trim(layer)
  write(FU, "(a)")        "10"
  write(FU, "(es15.5e3)") x1
  write(FU, "(a)")        "20"
  write(FU, "(es15.5e3)") y1
  write(FU, "(a)")        "30"
  write(FU, "(es15.5e3)") z1
  write(FU, "(a)")        "11"
  write(FU, "(es15.5e3)") x2
  write(FU, "(a)")        "21"
  write(FU, "(es15.5e3)") y2
  write(FU, "(a)")        "31"
  write(FU, "(es15.5e3)") z2

  end subroutine
