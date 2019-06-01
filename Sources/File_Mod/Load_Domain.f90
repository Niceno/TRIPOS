!==============================================================================!
  subroutine File_Mod_Load_Domain(mesh, comm)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  type(Comm_Type), target :: comm
!-----------------------------------[Locals]-----------------------------------!
  character(len=CL)        :: line
  character(len=CL)        :: dumc
  character(len=CL)        :: dom_file_name = ""
  logical                  :: file_exists
  integer                  :: n, len, s
  integer,         pointer :: np, ns
  type(Node_Type), pointer :: point(:)
  type(Chai_Type), pointer :: chain(:)
  type(Segm_Type), pointer :: segment(:)
!==============================================================================!

  ! Form file name
  len = len_trim(comm % problem_name)
  dom_file_name(    1:len  ) = comm % problem_name(1:len)
  dom_file_name(len+1:len+2) = ".d"

  ! Check if it exists
  inquire(file=dom_file_name, exist=file_exists)
  if(.not. file_exists) then
    print *, "File: ", trim(dom_file_name), " does not exist!"
    print *, "Exiting without generating a mesh!"
    stop
  end if

  ! Open the file
  open(FU, file=dom_file_name)

  ! Take aliases
  np      => mesh % n_point
  ns      => mesh % n_segment
  point   => mesh % point
  chain   => mesh % chain
  segment => mesh % segment

  !-----------------!
  !   Read points   !
  !-----------------!
  call File_Mod_Get_Useful_Line(FU, line)
  read(line, *) np

  do n = 0, np-1
    call File_Mod_Get_Useful_Line(FU, line)
    read(line, *) dumc,          &
                  point(n) % x,  &
                  point(n) % y,  &
                  point(n) % f,  &
                  point(n) % mark

    ! Initialize number of times it was inserted
    point(n) % inserted = 0
  end do

  !-------------------!
  !   Read segments   !
  !-------------------!
  call File_Mod_Get_Useful_Line(FU, line)
  read(line, *) ns

  do s = 0, ns-1
    call File_Mod_Get_Useful_Line(FU, line)
    read(line, *) dumc,              &
                  segment(s) % n0,   &
                  segment(s) % n1,   &
                  segment(s) % mark
  end do

  close(FU)

  !--------------------------------------!
  !   Check how the input was numbered   !
  !--------------------------------------!
  num_from = MAX_NODES
  do s = 0, ns-1
    num_from = min(num_from, segment(s) % n0)
    num_from = min(num_from, segment(s) % n1)
  end do
  if(num_from .eq. 1) then
    do s = 0, ns-1
      segment(s) % n0 = segment(s) % n0 - 1
      segment(s) % n1 = segment(s) % n1 - 1
    end do
  end if

  end subroutine
