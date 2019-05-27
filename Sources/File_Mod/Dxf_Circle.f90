!==============================================================================!
  subroutine File_Mod_Dxf_Circle(xc, yc, rad, layer)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real(RP)            :: xc, yc, rad
  character(len=*)    :: layer
  real(RP), parameter :: zero = 0.0
!==============================================================================!

  write(FU, "(a)")        "0"
  write(FU, "(a)")        "CIRCLE"
  write(FU, "(a)")        "8"
  write(FU, "(a)")        trim(layer)
  write(FU, "(a)")        "10"
  write(FU, "(es15.5e3)") xc
  write(FU, "(a)")        "20"
  write(FU, "(es15.5e3)") yc
  write(FU, "(a)")        "30"
  write(FU, "(es15.5e3)") zero
  write(FU, "(a)")        "40"
  write(FU, "(es15.5e3)") rad

  end subroutine
