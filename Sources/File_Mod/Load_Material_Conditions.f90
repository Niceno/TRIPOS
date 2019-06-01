!==============================================================================!
  subroutine File_Mod_Load_Material_Conditions(mesh, comm)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  type(Comm_Type), target :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n, len
  logical           :: file_exists
  character(len=CL) :: line
  character(len=CL) :: dumc
  character(len=CL) :: mat_file_name = ""
!==============================================================================!

  ! Form file name
  len = len_trim(comm % problem_name)
  mat_file_name(    1:len  ) = comm % problem_name(1:len)
  mat_file_name(len+1:len+2) = ".m"

  ! Check if it exists
  inquire(file=mat_file_name, exist=file_exists)
  if(.not. file_exists) then
    print *, "File: ", trim(mat_file_name), " does not exist!"
    print *, "It is needed to specify material conditions."
    print *, "Exiting solving the Poisson's equation!"
    stop
  end if

  if(comm % messages .eq. ON) then
    write(*, "(a,a)") " Loading material conditions from ", trim(mat_file_name)
  end if

  ! Open the file ...
  open(FU, file=mat_file_name)

  ! ... and read it
  call File_Mod_Get_Useful_Line(FU, line)
  read(line, *) mesh % n_mater

  if(comm % messages .eq. ON)  &
    write(*, "(i6,a)", advance="no") mesh % n_mater, " material conditions "

  do n = 1, mesh % n_mater
    call File_Mod_Get_Useful_Line(FU, line)
    read(line, *) dumc,                     &
                  mesh % mater(n) % mu,     &
                  mesh % mater(n) % j
    if(comm % messages .eq. ON) write(*, "(a,a)", advance="no") trim(dumc), " "
  end do

  if(comm % messages .eq. ON) write(*, *) " "

  close(FU)

  end subroutine
