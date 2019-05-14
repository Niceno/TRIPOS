!==============================================================================!
  subroutine File_Mod_Dxf_Solid(x1, y1, x2, y2, x3, y3, layer)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real(RP)            :: x1, y1, x2, y2, x3, y3
  character(len=*)    :: layer
  real(RP), parameter :: zero = 0.0
!==============================================================================!

  write(FU, "(a)")        "0"
  write(FU, "(a)")        "SOLID"
  write(FU, "(a)")        "8"
  write(FU, "(a)")        trim(layer)
  write(FU, "(a)")        "10"
  write(FU, "(es15.5e3)") x1
  write(FU, "(a)")        "20"
  write(FU, "(es15.5e3)") y1
  write(FU, "(a)")        "30"
  write(FU, "(es15.5e3)") zero
  write(FU, "(a)")        "11"
  write(FU, "(es15.5e3)") x2
  write(FU, "(a)")        "21"
  write(FU, "(es15.5e3)") y2
  write(FU, "(a)")        "31"
  write(FU, "(es15.5e3)") zero
  write(FU, "(a)")        "12"
  write(FU, "(es15.5e3)") x3
  write(FU, "(a)")        "22"
  write(FU, "(es15.5e3)") y3
  write(FU, "(a)")        "32"
  write(FU, "(es15.5e3)") zero
  write(FU, "(a)")        "13"
  write(FU, "(es15.5e3)") x3
  write(FU, "(a)")        "23"
  write(FU, "(es15.5e3)") y3
  write(FU, "(a)")        "33"
  write(FU, "(es15.5e3)") zero

  end subroutine
