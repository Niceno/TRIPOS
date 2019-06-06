!==============================================================================!
  subroutine File_Mod_Name_Layer(layer_name, lev, val)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  character(len=CL) :: layer_name  ! string to be operated on
  integer           :: lev         ! level
  real(RP)          :: val         ! value
!==============================================================================!

  ! Set default layer name
  layer_name(1:13) = "00_p0_00Ep000"

  ! Name layer (could be a function)
  write(layer_name(1: 2), "(i2.2)")     lev+1
  write(layer_name(4:13), "(es10.2e3)") val
  if(val >= 0.0) layer_name(4:4) = "p"
  if(val <  0.0) layer_name(4:4) = "m"
  layer_name(6:6)                = "_"
  layer_name(9:9)                = "e"
  if(layer_name(10:10) .eq. "+") layer_name(10:10) = "p"
  if(layer_name(10:10) .eq. "-") layer_name(10:10) = "m"

  end subroutine
