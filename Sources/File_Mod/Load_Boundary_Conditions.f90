!==============================================================================!
  subroutine File_Mod_Load_Boundary_Conditions(mesh, opts)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type),    target :: mesh
  type(Options_Type), target :: opts
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n, len
  logical           :: file_exists
  character(len=CL) :: line
  character(len=CL) :: dumc
  character(len=CL) :: bnd_file_name = ""
!==============================================================================!

  ! Form file name
  len = len_trim(opts % problem_name)
  bnd_file_name(    1:len  ) = opts % problem_name(1:len)
  bnd_file_name(len+1:len+2) = ".b"

  ! Check if it exists
  inquire(file=bnd_file_name, exist=file_exists)
  if(.not. file_exists) then
    print *, "File: ", trim(bnd_file_name), " does not exist!"
    print *, "It is needed to specify boundary conditions."
    print *, "Exiting solving the Poisson's equation!"
    stop
  end if

  if(opts % messages .eq. ON) then
    write(*, "(a,a)") " Loading boundary conditions from ", trim(bnd_file_name)
  end if

  ! Open the file ...
  open(FU, file=bnd_file_name)

  ! ... and read it
  call File_Mod_Get_Useful_Line(FU, line)
  read(line, *) mesh % n_bound

  if(opts % messages .eq. ON)  &
    write(*, "(i6,a)", advance="no") mesh % n_bound, " boundary conditions "

  do n = 1, mesh % n_bound
    call File_Mod_Get_Useful_Line(FU, line)
    read(line, *) dumc,                     &
                  mesh % bound(n) % type,   &
                  mesh % bound(n) % value
    if(opts % messages .eq. ON) write(*, "(a,a)", advance="no") trim(dumc), " "
  end do

  if(opts % messages .eq. ON) write(*, *) " "

  close(FU)

  end subroutine
