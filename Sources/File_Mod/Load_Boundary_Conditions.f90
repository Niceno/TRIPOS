!==============================================================================!
  subroutine File_Mod_Load_Boundary_Conditions(mesh, comm)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  type(Comm_Type), target :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n, len
  character(len=CL) :: line
  character(len=CL) :: dumc
  character(len=CL) :: bnd_cond_name = ""
!==============================================================================!

  len = len_trim(comm % problem_name)
  bnd_cond_name(  1:len) = comm % problem_name(1:len)
  bnd_cond_name(len:len) = "b"

  open(FU, file=bnd_cond_name)

  call File_Mod_Get_Useful_Line(FU, line)
  read(line, *) mesh % n_bound

  do n = 1, mesh % n_bound
    call File_Mod_Get_Useful_Line(FU, line)
    read(line, *) dumc,                     &
                  mesh % bound(n) % type,   &
                  mesh % bound(n) % value
  end do

  close(FU)

  end subroutine
