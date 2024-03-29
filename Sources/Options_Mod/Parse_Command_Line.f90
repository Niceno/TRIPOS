!==============================================================================!
  subroutine Options_Mod_Parse_Command_Line(comm)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Options_Type) :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer           :: len
  integer           :: arg
  character(len=CL) :: argv  ! command line argument
!==============================================================================!

  ! Chack if first argument is "+example" ...
  call get_command_argument(1, argv)

  ! ... and if so, create example file and beat it
  if(argv .eq. "+example") then
    comm % example = ON
    return
  end if

  ! First argument wasn't "+example", meaning it was domain file name
  comm % problem_name = argv
  len = len_trim(comm % problem_name)

  ! Parse command line options
  do arg = 2, command_argument_count()
    call get_command_argument(arg, argv)

    if(argv .eq. "+a") then          ! levels of aggressivity
      call get_command_argument(arg+1, argv)
      if(argv .eq. "0")  comm % r_tol = 0.7
      if(argv .eq. "1")  comm % r_tol = 0.75
      if(argv .eq. "2")  comm % r_tol = 0.8
      if(argv .eq. "3")  comm % r_tol = 0.85
      if(argv .eq. "4")  comm % r_tol = 0.9
      if(argv .eq. "5")  comm % r_tol = 0.95
      if(argv .eq. "6")  comm % r_tol = 1.0
    end if
    if(argv .eq. "-d") then          ! don't triangulate the domain
      comm % tri    = OFF
      comm % relax  = OFF
      comm % smooth = OFF
    end if
    if(argv .eq. "+dxf") comm % dxf = ON    ! create dxf
    if(argv .eq. "+eps") comm % eps = ON    ! create dxf
    if(argv .eq. "+dxf" .or.  &
       argv .eq. "+eps") then
      call get_command_argument(arg+1, argv)
      if(argv .eq. "D") then
        comm % delaunay = ON
        comm % voronoi  = OFF
      else if(argv .eq. "V") then
        comm % delaunay = OFF
        comm % voronoi  = ON
      else if(argv .eq. "H") then
        comm % head = ON
      end if
      call get_command_argument(arg+2, argv)
      if(argv .eq. "D") then
        comm % delaunay = ON
        comm % voronoi  = OFF
      else if(argv .eq. "V") then
        comm % delaunay = OFF
        comm % voronoi  = ON
      else if(argv .eq. "H") then
        comm % head = ON
      end if
    end if
    if(argv .eq. "-r")  comm % relax    = OFF
    if(argv .eq. "-s")  comm % smooth   = OFF
    if(argv .eq. "-m")  comm % messages = OFF
    if(argv .eq. "+solve") comm % solve = ON    ! solve Poisoon's equation
  end do

  end subroutine
