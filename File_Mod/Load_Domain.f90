!==============================================================================!
  subroutine File_Mod_Load_Domain
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  character(len=CL) :: line
  character(len=CL) :: dumc
  integer           :: n, s
!==============================================================================!

  open(FU, file = name)

  !-----------------!
  !   Read points   !
  !-----------------!
  call File_Mod_Get_Useful_Line(FU, line)
  read(line, *) n_point

  do n = 0, n_point-1
    call File_Mod_Get_Useful_Line(FU, line)
    read(line, *) dumc,          &
                  point(n) % x,  &
                  point(n) % y,  &
                  point(n) % f,  &
                  point(n) % mark

    point(n) % inserted = 0
  end do

  !-------------------!
  !   Read segments   !
  !-------------------!
  call File_Mod_Get_Useful_Line(FU, line)
  read(line, *) n_segment

  ! Allocate memory for segments and chains
  allocate(segment(0:n_segment+1))  ! why n_seg+1?
  allocate(chain  (0:n_segment+1))  ! approximation

  do s = 0, n_segment-1
    call File_Mod_Get_Useful_Line(FU, line)
    read(line, *) dumc,              &
                  segment(s) % n0,   &
                  segment(s) % n1,   &
                  segment(s) % mark
  end do

  close(FU)

  end subroutine
