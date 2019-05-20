!==============================================================================!
  subroutine File_Mod_Fig_Start(comm)
!------------------------------------------------------------------------------!
!   Subroutine to start a .fig file.                                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Comm_Type) :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer           :: len
  character(len=CL) :: fig_mesh_name = ""
!==============================================================================!

  if(comm % messages .eq. ON) print *, "Plotting the mesh in fig format"

  len = len_trim(comm % problem_name)
  fig_mesh_name(    1:len-2) = comm % problem_name(1:len-2)
  fig_mesh_name(len-1:len+2) = ".fig"

  open(unit=FU, file=fig_mesh_name)

  write(FU, "(a)")  "#FIG 3.1"
  write(FU, "(a)")  "Landscape"
  write(FU, "(a)")  "Center"
  write(FU, "(a)")  "Metric"
  write(FU, "(a)")  "1200 2"

  end subroutine
