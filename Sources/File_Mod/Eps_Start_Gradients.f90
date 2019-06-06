!==============================================================================!
  subroutine File_Mod_Eps_Start_Gradients(comm)
!------------------------------------------------------------------------------!
!   Subroutine to start an .eps file for plotting gradients.                   !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Comm_Type) :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer           :: len
  character(len=CL) :: eps_gradients_name = ""
!==============================================================================!

  if(comm % messages .eq. ON) print *, "Plotting the gradients in eps format"

  ! Form file name ...
  len = len_trim(comm % problem_name)
  eps_gradients_name(    1:len  ) = comm % problem_name(1:len)
  eps_gradients_name(len+1:len+6) = ".g.eps"

  ! ... and open it
  open(unit=FU, file=eps_gradients_name)

  !------------!
  !   Header   !
  !------------!
  write(FU, "(a)")     "%!PS-Adobe-2.0 EPSF-1.2"
  write(FU, "(a)")     "%%Created by Tripos"
  write(FU, "(a, a)")  "%%File: ", trim(eps_gradients_name)
  write(FU, "(a)")     "%%BoundingBox: (atend)"
  write(FU, "(a)")     "save"
  write(FU, "(a)")     "countdictstack"
  write(FU, "(a)")     "mark"
  write(FU, "(a)")     "newpath"

  !--------------!
  !   Commands   !
  !--------------!
  write(FU, "(a)")  "/cp   {closepath}    bind def"
  write(FU, "(a)")  "/f    {fill}         bind def"
  write(FU, "(a)")  "/l    {lineto}       bind def"
  write(FU, "(a)")  "/m    {moveto}       bind def"
  write(FU, "(a)")  "/s    {stroke}       bind def"
  write(FU, "(a)")  "/srgb {setrgbcolor}  bind def"
  write(FU, "(a)")  "/slw  {setlinewidth} bind def"
  write(FU, "(a)")  "/sds  {setdash}      bind def"

  end subroutine