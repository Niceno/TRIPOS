!==============================================================================!
  program Easymesh
!------------------------------------------------------------------------------!
!           _____________________._____________________    _________           !
!           \__    ___/\______   \   \______   \_____  \  /   _____/           !
!             |    |    |       _/   ||     ___//   |   \ \_____  \            !
!             |    |    |    |   \   ||    |   /    |    \/        \           !
!             |____|    |____|_  /___||____|   \_______  /_______  /           !
!                              \/                      \/        \/            !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod
  use Comm_Mod
  use File_Mod
  use Vector_Mod
  use Matrix_Mod
  use Solver_Mod
  use Cpu_Timer_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  type(Comm_Type)   :: comm
  type(Mesh_Type)   :: mesh

  ! Variables relevant for solvers
  integer           :: max_it, act_it, i
  real(RP)          :: dt = 0.1, f_res, t_res = 1.0e-6
  type(Matrix_Type) :: a
  type(Vector_Type) :: x, x_o
  type(Vector_Type) :: b
  type(Solver_Type) :: krylov
!==============================================================================!

  call Cpu_Timer_Mod_Start('Easymesh_Main')

  !----------------------------------!
  !                                  !
  !   Parse command line arguments   !
  !                                  !
  !----------------------------------!

  ! No command line arguments were specified
  if(command_argument_count() .eq. 0) then
    call File_Mod_Splash
    stop

  ! Some command line arguments were specified; parse them!
  else
    call Parse_Command_Line(comm)
  end if  ! some command arguments were passed

  ! If user wanted to create an example, do it an exit
  if(comm % example .eq. ON) then
    call File_Mod_Example
    stop
  end if

  !-----------------------------------------!
  !                                         !
  !   Finished processing user arguments,   !
  !      mesh generation can now start      !
  !                                         !
  !-----------------------------------------!
  if(comm % messages .eq. ON) call File_Mod_Logo
  call File_Mod_Load_Domain(mesh, comm)   ! load the domain file
  call Setup_Chains(mesh)                 ! set the domain up

  !-----------------------!
  !   Generate the mesh   !
  !-----------------------!
  call Mesh_Mod_Generate(mesh, comm)

  !-----------------------------------------!
  !   Save mesh in native Easymesh format   !
  !-----------------------------------------!
  call File_Mod_Save_Mesh(mesh, comm)

  !--------------------------------!
  !   Plot dxf file if requested   !
  !--------------------------------!
  if(comm % dxf .eq. ON) then
    call File_Mod_Dxf_Start_Mesh(mesh, comm)
    call File_Mod_Dxf_Draw_Mesh(mesh, comm % delaunay, comm % voronoi)
    call File_Mod_Dxf_End
  end if

  !--------------------------------!
  !   Plot eps file if requested   !
  !--------------------------------!
  if(comm % eps .eq. ON) then
    call File_Mod_Eps_Start_Mesh(comm)
    call File_Mod_Eps_Draw_Mesh(mesh, comm % delaunay, comm % voronoi)
    call File_Mod_Eps_End
  end if

  if(comm % messages .eq. ON) print *, "Done!"
  if(comm % messages .eq. ON) print *, ""

  if(comm % messages .eq. ON) call File_Mod_Mesh_Statistics(mesh)

  !-------------------------------!
  !                               !
  !   Numerical mesh generated,   !
  !    solution can now start     !
  !                               !
  !-------------------------------!
  if(comm % solve .eq. ON) then

    if(comm % messages .eq. ON) then
      print *, "Solving conservation equations.  Please wait!"
    end if

    !--------------------------!
    !   Create linear solver   !
    !--------------------------!
    call Solver_Mod_Allocate(krylov, mesh)
    call Matrix_Mod_Allocate(a,   mesh)
    call Vector_Mod_Allocate(x,   mesh)
    call Vector_Mod_Allocate(x_o, mesh)
    call Vector_Mod_Allocate(b,   mesh)

    !---------------------------------------!
    !   Discretize conservation equations   !
    !---------------------------------------!
    call File_Mod_Load_Boundary_Conditions(mesh, comm)
    call File_Mod_Load_Material_Conditions(mesh, comm)

    !------------------------!
    !   Time stepping loop   !
    !------------------------!
    max_it = mesh % n_node  ! set maximum number of iterations

    do i = 1, 12000

      if(mod(i, 20) .eq. 0 .and. comm % messages .eq. ON) then
        print "(a,i5,es12.4)", " Current time step: ", i, dt
      end if

      ! Actual value becomes old
      x_o % val(:) = x % val(:)

      ! Discretize linear system and update r.h.s.
      call Discretize(a, x_o, b, dt)

      ! Cal linear solver
      ! call Solver_Mod_Bicg(krylov, a, x, b, max_it, act_it, t_res, f_res)
      call Solver_Mod_Cg(krylov, a, x, b, max_it, act_it, t_res, f_res)

      ! Gradually increase time step, you want steady solution anyway
      dt = dt * 1.2

      if(act_it .eq. 0) then
        goto 1
      end if

      if(mod(i, 20) .eq. 0 .and. comm % messages .eq. ON) then
        print "(a,i5,es12.4)", " Solver iterations: ", act_it, f_res
      end if
    end do
1   continue

    !-------------------------------------!
    !   Plot results in DXF file format   !
    !-------------------------------------!
    if(comm % dxf .eq. ON) then
      call File_Mod_Dxf_Start_Results(x, comm)
      call File_Mod_Dxf_Draw_Results(x)
      call File_Mod_Dxf_End
    end if

    !-------------------------------------!
    !   Plot results in EPS file format   !
    !-------------------------------------!
    if(comm % eps .eq. ON) then
      call File_Mod_Eps_Start_Results(comm)
      call File_Mod_Eps_Draw_Results(x)
      call File_Mod_Eps_End
    end if
  end if

  if(comm % messages .eq. ON) call Cpu_Timer_Mod_Stop('Easymesh_Main')
  if(comm % messages .eq. ON) call Cpu_Timer_Mod_Statistics()

  end program
