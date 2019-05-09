!==============================================================================!
  subroutine Cpu_Timer_Mod_Statistics
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer           :: max_l, f
  character(len= 4) :: max_l_s
  real(RP)          :: total_time
  character(len=CL) :: f_name
!==============================================================================!

  ! Find max function name length
  max_l = 0
  do f=1, n_funct
    max_l = max(max_l, len_trim(funct_name(f)))
  end do
  write(max_l_s, "(i4.4)"), max_l+4


  total_time = 0.0
  do f=1, n_funct
    total_time = total_time + funct_time(f)
  end do

  do f=1, n_funct
    f_name(1:CL) = ""
    f_name(2:2)  = "-"
    f_name(4:len_trim(funct_name(f))+3) = funct_name(f)
    write(*,"(a" // max_l_s //", a1, f8.3, a4, f8.1, a2)")  &
             f_name(1:max_l+4),                  ":",       &
             funct_time(f),                      " [s]",    &
             funct_time(f) / total_time * 100.0, " %"
  end do

  end subroutine
