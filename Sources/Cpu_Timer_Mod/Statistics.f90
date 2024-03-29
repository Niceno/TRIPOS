!==============================================================================!
  subroutine Cpu_Timer_Mod_Statistics
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer            :: f
  real(RP)           :: total_time, t_temp
  character(len=CL)  :: line, n_temp
  integer, parameter :: I=8  ! indent
  logical            :: swap
!==============================================================================!

  ! Perform bubble sort on times
  do
    swap = .false.
    do f=1, n_funct-1
      if(funct_time(f+1) > funct_time(f)) then
        t_temp = funct_time(f)
        n_temp = funct_name(f)
        funct_time(f)   = funct_time(f+1)
        funct_name(f)   = funct_name(f+1)
        funct_time(f+1) = t_temp
        funct_name(f+1) = n_temp
        swap = .true.
      end if
    end do
    if(.not. swap) goto 1
  end do
1 continue

  ! Estimate total time
  total_time = 0.0
  do f=1, n_funct
    total_time = total_time + funct_time(f)
  end do

  line( 1:CL) = " "
  line( 1+I:57+I) = "#=======================================================#"
  print *, trim(line)
  line( 1+I:57+I) = "#                 CPU usage statistics                  #"
  print *, trim(line)
  line( 1+I:57+I) = "#-------------------------------------------------------#"
  print *, trim(line)
  line( 1:CL) = " "
  line( 1+I:29+I) = "#             Total CPU time: "
  write(line(30+I:39+I), "(f9.3)") total_time
  line(40+I:42+I) = "[s]"
  line(57+I:57+I) = "#"
  print *, trim(line)
  line( 1+I:57+I) = "#-------------------------------------------------------#"
  print *, trim(line)
  line( 1+I:57+I) = "# Function:                      Time:                  #"
  print *, trim(line)
  line( 1+I:57+I) = "#-------------------------------------------------------#"
  print *, trim(line)

  do f=1, n_funct
    line( 1:CL) = " "
    line( 1+I: 1+I) = "#"
    line(57+I:57+I) = "#"
    line( 3+I: 3+I+len_trim(funct_name(f))) = funct_name(f)
    line(29+I:29+I) = ":"
    write(line(31+I:38+I), "(f8.3)") funct_time(f)
    line(40+I:42+I) = "[s]"
    write(line(45+I:51+I), "(f6.3)") funct_time(f) / total_time * 100.0
    line(53+I:53+I) = "%"
    print *, trim(line)
  end do

  line( 1+I:57+I) = "#-------------------------------------------------------#"
  print *, trim(line)
  print *, ""

  end subroutine
