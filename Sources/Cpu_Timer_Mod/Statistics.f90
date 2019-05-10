!==============================================================================!
  subroutine Cpu_Timer_Mod_Statistics
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer           :: max_l, f
  character(len= 4) :: max_l_s
  real(RP)          :: total_time
  character(len=CL) :: line
!==============================================================================!

  ! Find max function name length
  max_l = 0
  do f=1, n_funct
    max_l = max(max_l, len_trim(funct_name(f)))
  end do
  write(max_l_s, "(i4.4)"), max_l+4
  max_l = 40

  total_time = 0.0
  do f=1, n_funct
    total_time = total_time + funct_time(f)
  end do

  print *, "#=======================================================#"
  print *, "#                 CPU usage statistics                  #"
  print *, "#-------------------------------------------------------#"
  line( 1:CL) = " "
  line( 1:29) = "#             Total CPU time: "
  write(line(30:39), "(f9.3)") total_time
  line(40:42) = "[s]"
  line(57:57) = "#"
  print *, trim(line)
  print *, "#-------------------------------------------------------#"
  print *, "# Function:                      Time:                  #"
  print *, "#-------------------------------------------------------#"

  do f=1, n_funct
    line( 1:CL) = " "
    line( 1: 1) = "#"
    line(57:57) = "#"
    line( 3: 3+len_trim(funct_name(f))) = funct_name(f)
    line(29:29) = ":"
    write(line(31:38), "(f8.3)") funct_time(f)
    line(40:42) = "[s]"
    write(line(47:50), "(f4.1)") funct_time(f) / total_time * 100.0
    line(52:52) = "%"
    print *, trim(line)
  end do

  print *, "#=======================================================#"
  print *, ""

  end subroutine
