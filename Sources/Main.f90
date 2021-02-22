!==============================================================================!
  program Tripos
!------------------------------------------------------------------------------!
!           _____________________._____________________    _________           !
!           \__    ___/\______   \   \______   \_____  \  /   _____/           !
!             |    |    |       _/   ||     ___//   |   \ \_____  \            !
!             |    |    |    |   \   ||    |   /    |    \/        \           !
!             |____|    |____|_  /___||____|   \_______  /_______  /           !
!                              \/                      \/        \/            !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use File_Mod
  use Solver_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  type(Options_Type)      :: opts
  type(Mesh_Type)         :: mesh
  integer                 :: max_it, act_it, j
  character(3), parameter :: I = '   '  ! indent for solver output
  real(RP)                :: dt = 1.0   ! false time step
  real(RP)                :: f_sol, t_sol = 1.0e-8
  real(RP)                :: c_out, t_out = 1.0e-6
  type(Matrix_Type)       :: a
  type(Vector_Type)       :: phi_n, phi_o
  type(Vector_Type)       :: b
  type(Solver_Type)       :: krylov
!==============================================================================!

  call Cpu_Timer_Mod_Start('Tripos_Main')

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
    call Options_Mod_Parse_Command_Line(opts)
  end if  ! some command arguments were passed

  ! If user wanted to create an example, do it an exit
  if(opts % example .eq. ON) then
    call File_Mod_Example
    stop
  end if

  !-----------------------------------------!
  !                                         !
  !   Finished processing user arguments,   !
  !      mesh generation can now start      !
  !                                         !
  !-----------------------------------------!
  if(opts % messages .eq. ON) call File_Mod_Logo
  call File_Mod_Load_Domain(mesh, opts)   ! load the domain file
  call Mesh_Mod_Setup_Chains(mesh)        ! set the domain up

  !-----------------------!
  !   Generate the mesh   !
  !-----------------------!
  call Mesh_Mod_Generate(mesh, opts)

  !---------------------------------------!
  !   Save mesh in native Tripos format   !
  !---------------------------------------!
  call File_Mod_Save_Mesh(mesh, opts)

  !--------------------------------!
  !   Plot dxf file if requested   !
  !--------------------------------!
  if(opts % dxf .eq. ON) then
    call File_Mod_Dxf_Start_Mesh(mesh, opts)
    call File_Mod_Dxf_Draw_Mesh(mesh, opts % delaunay, opts % voronoi)
    call File_Mod_Dxf_End
  end if

  !--------------------------------!
  !   Plot eps file if requested   !
  !--------------------------------!
  if(opts % eps .eq. ON) then
    call File_Mod_Eps_Start_Mesh(opts)
    call File_Mod_Eps_Draw_Mesh(mesh, opts % delaunay, opts % voronoi)
    call File_Mod_Eps_End
  end if

  if(opts % messages .eq. ON) print *, "Done!"

  if(opts % messages .eq. ON) print *, ""
  if(opts % messages .eq. ON) call File_Mod_Mesh_Statistics(mesh)

  !-------------------------------!
  !                               !
  !   Numerical mesh generated,   !
  !    solution can now start     !
  !                               !
  !-------------------------------!
  if(opts % solve .eq. ON) then

    if(opts % messages .eq. ON) then
      print *, "Solving conservation equations.  Please wait!"
    end if

    !--------------------------!
    !   Create linear solver   !
    !--------------------------!
    call Solver_Mod_Allocate(krylov, mesh)
    call Matrix_Mod_Allocate(a,   mesh)
    call Vector_Mod_Allocate(phi_n, mesh)
    call Vector_Mod_Allocate(phi_o, mesh)
    call Vector_Mod_Allocate(b,   mesh)

    !---------------------------------------!
    !   Discretize conservation equations   !
    !---------------------------------------!
    call File_Mod_Load_Boundary_Conditions(mesh, opts)
    call File_Mod_Load_Material_Conditions(mesh, opts)

    !------------------------!
    !   Time stepping loop   !
    !------------------------!
    max_it = mesh % n_node  ! set maximum number of iterations

    ! Discretize linear system for the first time
    call Discretize(a, phi_o, b, dt)

    print "(a)", ""
    print "(a,a)", I, "#=================================" // &
                      "==================================#"
    print "(a,a,es10.3,a)", I, "# Starting iterative procedure " // &
                               "with target residual: ",  t_out, "     #"
    print "(a,a)", I, "#---------------------------------" // &
                      "----------------------------------#"
    do j = 1, 100000

      if(mod(j, 20) .eq. 0 .and. opts % messages .eq. ON) then
        print "(a,a,i5,a,2es10.3,a)", I, "# Time step:", j,  &
              "; solver and outer residuals: ", f_sol, c_out, " #"
      end if

      ! Actual value becomes old
      phi_o % val(:) = phi_n % val(:)

      ! Discretize linear system and update r.h.s.
      call Discretize(a, phi_o, b, dt)

      ! Cal linear solver
      ! call Solver_Mod_Bicg(krylov, a, phi_n, b, max_it, act_it, t_sol, f_sol)
      call Solver_Mod_Cg(krylov, a, phi_n, b, max_it, act_it, t_sol, f_sol)

      ! Find current residual for outer iterations
      c_out = norm2((phi_n % val - phi_o % val))

      if(act_it .eq. 0) then
        print "(a,a)", I, "#---------------------------------" // &
                          "----------------------------------#"
        print "(a,a,i5,a)", I, "# Solver can't converge " // &
                               "any further at time step: ",  &
                               j, "             #"
        goto 1
      end if

      if(c_out < t_out) then
        print "(a,a)", I, "#---------------------------------" // &
                          "----------------------------------#"
        print "(a, a,i5,a,es10.3,a)", I, "# Convergence reached at time step ",&
                                      j, " with residual   ", c_out, " #"
        goto 1
      end if

      if(mod(j, 20) .eq. 0 .and. opts % messages .eq. ON) then
      end if

      ! Gradually increase time step, you want steady solution anyway
      dt = dt * 1.2

    end do
1   continue
    print "(a,a)", I, "#=================================" // &
                      "==================================#"

    !-------------------------------------!
    !   Plot results in DXF file format   !
    !-------------------------------------!
    if(opts % dxf .eq. ON) then
      call File_Mod_Dxf_Start_Results(phi_n, opts)
      call File_Mod_Dxf_Draw_Results(phi_n)
      call File_Mod_Dxf_End

      call File_Mod_Dxf_Start_Gradients(phi_n, opts)
      call File_Mod_Dxf_Draw_Gradients(phi_n, opts)
      call File_Mod_Dxf_End
    end if

    !-------------------------------------!
    !   Plot results in EPS file format   !
    !-------------------------------------!
    if(opts % eps .eq. ON) then
      call File_Mod_Eps_Start_Results(opts)
      call File_Mod_Eps_Draw_Results(phi_n)
      call File_Mod_Eps_End

      call File_Mod_Eps_Start_Gradients(opts)
      call File_Mod_Eps_Draw_Gradients(phi_n, opts)
      call File_Mod_Eps_End
    end if
  end if

  if(opts % messages .eq. ON) call Cpu_Timer_Mod_Stop('Tripos_Main')

  if(opts % messages .eq. ON) print *, ""
  if(opts % messages .eq. ON) call Cpu_Timer_Mod_Statistics()

  end program
