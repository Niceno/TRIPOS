!==============================================================================!
  program Easymesh
!------------------------------------------------------------------------------!
!       ___________                       _____                .__             !
!       \_   _____/____    _________.__. /     \   ____   _____|  |__          !
!        |    __)_\__  \  /  ___<   |  |/  \ /  \_/ __ \ /  ___/  |  \         !
!        |        \/ __ \_\___ \ \___  /    Y    \  ___/ \___ \|   Y  \        !
!       /_______  (____  /____  >/ ____\____|__  /\___  >____  >___|  /        !
!               \/     \/     \/ \/            \/     \/     \/     \/         !
!                                                                              !
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
  !   Parse command line arguments   !
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
  if(comm % exa .eq. ON) then
    call File_Mod_Example
    stop
  end if

  !-----------------------------------------!
  !   Finished processing user arguments,   !
  !      mesh generation can now start      !
  !-----------------------------------------!
  if(comm % mes .eq. ON) call File_Mod_Logo
  call File_Mod_Load_Domain(mesh, comm)   ! load the domain file
  call Setup_Chains(mesh)                 ! set the domain up

  !-----------------------!
  !   Generate the mesh   !
  !-----------------------!
  call Mesh_Mod_Generate(mesh, comm)

  !-----------------------------------------!
  !   Save mesh in native Easymesh format   !
  !-----------------------------------------!
  if(comm % mes .eq. ON) print *, "Saving the mesh"
  call File_Mod_Save_Mesh(mesh, comm)

  !--------------------------------!
  !   Plot fig file if requested   !
  !--------------------------------!
  if(comm % fig .eq. ON) then
    if(comm % mes .eq. ON) print *, "Plotting the mesh in fig format"
    call File_Mod_Fig_Start(comm)
    call File_Mod_Fig_Draw_Mesh(mesh, comm % del, comm % vor)
    call File_Mod_Fig_End
  end if

  !--------------------------------!
  !   Plot dxf file if requested   !
  !--------------------------------!
  if(comm % dxf .eq. ON) then
    if(comm % mes .eq. ON) print *, "Plotting the mesh in dfx format"
    call File_Mod_Dxf_Start_Mesh(mesh, comm)
    call File_Mod_Dxf_Draw_Mesh(mesh, comm % del, comm % vor)
    call File_Mod_Dxf_End
  end if

  if(comm % mes .eq. ON) print *, "Done!"
  if(comm % mes .eq. ON) print *, ""

  if(comm % mes .eq. ON) call File_Mod_Mesh_Statistics(mesh)

  !-------------------------------!
  !                               !
  !   Numerical mesh generated,   !
  !    solution can now start     !
  !                               !
  !-------------------------------!
  if(comm % mes .eq. ON) print *, "Solving conservation equations.  Please wait!"

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

    if(mod(i, 20) .eq. 0) print "(a,i5,es12.4)", " Current time step: ", i, dt

    ! Actual value becomes old
    x_o % val(:) = x % val(:)

    ! Discretize linear system and update r.h.s.
    call Discretize(a, x_o, b, dt)

    ! Cal linear solver
    ! call Solver_Mod_Bicg(krylov, a, x, b, max_it, act_it, t_res, f_res)
    call Solver_Mod_Cg(krylov, a, x, b, max_it, act_it, t_res, f_res)

    dt = dt * 1.2

    if(act_it .eq. 0) then
      goto 1
    end if

    if( mod(i, 20) .eq. 0 ) then
      if(comm % mes .eq. ON) then
        print "(a,i5,es12.4)", " Solver iterations: ", act_it, f_res
      end if
    end if
  end do
1 continue

  !-------------------------------------!
  !   Plot results in DXF file format   !
  !-------------------------------------!
  call File_Mod_Dxf_Start_Results(x, comm)
  call File_Mod_Dxf_Draw_Results(x)
  call File_Mod_Dxf_End

  if(comm % mes .eq. ON) call Cpu_Timer_Mod_Stop('Easymesh_Main')
  if(comm % mes .eq. ON) call Cpu_Timer_Mod_Statistics()

  end program
