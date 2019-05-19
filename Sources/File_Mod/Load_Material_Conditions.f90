!==============================================================================!
  subroutine File_Mod_Load_Material_Conditions(mesh, comm)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  type(Comm_Type), target :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n, len
  character(len=CL) :: line
  character(len=CL) :: dumc
  character(len=CL) :: mat_cond_name = ""
!==============================================================================!

  ! Form file name
  len = len_trim(comm % problem_name)
  mat_cond_name(  1:len) = comm % problem_name(1:len)
  mat_cond_name(len:len) = "m"

  ! Open the file ...
  open(FU, file=mat_cond_name)

  ! ... and read it
  call File_Mod_Get_Useful_Line(FU, line)
  read(line, *) mesh % n_mater

  do n = 1, mesh % n_mater
    call File_Mod_Get_Useful_Line(FU, line)
    read(line, *) dumc,                     &
                  mesh % mater(n) % mu,     &
                  mesh % mater(n) % j
  end do

  close(FU)

  end subroutine
