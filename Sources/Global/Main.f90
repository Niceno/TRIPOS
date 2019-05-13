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
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod
  use File_Mod
  use Cpu_Timer_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  character(len=CL) :: argv                          ! command line argument
  integer           :: arg, n_node_old
  integer           :: del, rel, smo, dxf, fig, mes  ! control paramaters
!==============================================================================!

  call Cpu_Timer_Mod_Start('Easymesh_Main')

  ! Initialize control parameters
  del = ON   ! perform Delaunay triangulization
  rel = ON   ! perform relaxation
  smo = ON   ! perform smoothing
  dxf = OFF  ! don't create DXF file
  fig = OFF  ! don't create FIG file
  mes = ON   ! print messages

  !----------------------------------------------!
  !   No command line arguments were specified   !
  !----------------------------------------------!
  if(command_argument_count() .eq. 0) then
    call File_Mod_Splash
    return

  !------------------------------------------------!
  !   Some command line arguments were specified   !
  !------------------------------------------------!
  else

    ! Chack if first argument is "+example" ...
    call get_command_argument(1, argv)

    ! ... and if so, create example file and beat it
    if(argv .eq. "+example") then
      call File_Mod_Example
      return
    end if

    ! First argument wasn't "+example", meaning it was domain file name
    name = argv
    len = len_trim(name)

    ! Form file names
    if(name(len-1:len-1) .eq. ".") then
      if(name(len:len) .eq. "d" .or.  &
         name(len:len) .eq. "D") then
        dxf_name(1:len-2) = name(1:len-2);  dxf_name(len-1:len+2) = ".dxf"
        fig_name(1:len-2) = name(1:len-2);  fig_name(len-1:len+2) = ".fig"
      else
        print *, "Critical error: Domain file name must have extension .d!"
        stop
      end if
    else  ! extension was longer than one character
      print *, "Critical error: Domain file name must have extension .d!"
      stop
    end if

    ! Parse command line options
    do arg = 2, command_argument_count()
      call get_command_argument(arg, argv)

      if(argv .eq. "+a") then          ! levels of aggressivity
        call get_command_argument(arg+1, argv)
        if(argv .eq. "0")  r_tol = 0.7
        if(argv .eq. "1")  r_tol = 0.75
        if(argv .eq. "2")  r_tol = 0.8
        if(argv .eq. "3")  r_tol = 0.85
        if(argv .eq. "4")  r_tol = 0.9
        if(argv .eq. "5")  r_tol = 0.95
        if(argv .eq. "6")  r_tol = 1.0
      end if
      if(argv .eq. "-d") then          ! don't triangulate the domain
        del =OFF
        rel =OFF
        smo =OFF
      end if
      if(argv .eq. "+dxf") dxf = ON    ! create dxf
      if(argv .eq. "+fig") fig = ON    ! create fig
      if(argv .eq. "-r")   rel = OFF   ! no relaxation
      if(argv .eq. "-s")   smo = OFF   ! no smoothing
      if(argv .eq. "-m")   mes = OFF   ! no messages
    end do
  end if  ! some command arguments were passed

  !-----------------------------------------!
  !   Finished processing user arguments,   !
  !     normal operation can now start      !
  !-----------------------------------------!
  if(mes .eq. ON) call File_Mod_Logo
  call File_Mod_Load_Domain    ! load the domain file

  call Easymesh_Setup_Domain   ! set the domain up
  call Easymesh_Insert_Domain  ! insert points which define domain

  call Mesh_Mod_Erase
  call Mesh_Mod_Classify

  !-----------------------!
  !   Main algorithm to   !
  !    generate a mesh    !
  !-----------------------!
  if(mes .eq. ON) print *, "Working.  Please wait!"

  if(del .eq. ON) then
    do while(ugly .ne. OFF)
      n_node_old = n_node
      call Mesh_Mod_New_Node
      call Mesh_Mod_Classify

      if(mes .eq. ON .and.  &
         mod(n_node, 500) .eq. 0) print "(i6,a)", n_node, " nodes"

      ! Exit because maximum number of nodes has been reached
      if(n_node .eq. MAX_NODES-1) then
        if(mes .eq. on) then
          print "(a,i6,a)", " Maximum number of nodes (",  &
                            MAX_NODES, ") has been reached." 
          print "(a)",      " The mesh might suffer from poor quality"
        end if
        exit
      end if

      ! No new nodes added to domain, exit the loop
      if(n_node .eq. n_node_old) exit
    end do
  end if

  call Mesh_Mod_Neighbours

  !-----------------------------------------------------!
  !   Improve mesh quality by relaxation and smooting   !
  !-----------------------------------------------------!
  if(rel .eq. ON .or. smo .eq. ON) then
    if(mes .eq. ON) print *, "Improving the mesh quality"

    if(rel .eq. ON)                  call Mesh_Mod_Relax
    if(rel .eq. ON .or. smo .eq. ON) call Mesh_Mod_Smooth
  end if

  !--------------------------------!
  !   Renumber all mesh entities   !
  !--------------------------------!
  if(mes .eq. ON) print *, "Renumerating nodes, elements and sides"
  call Mesh_Mod_Renumber

  !----------------------------!
  !   Process material marks   !
  !----------------------------!
  if(mes .eq. ON) print *, "Processing material marks"
  call Mesh_Mod_Materials

  !-----------------------------------------!
  !   Save mesh in native Easymesh format   !
  !-----------------------------------------!
  if(mes .eq. ON) print *, "Saving the mesh"
  call File_Mod_Save_Mesh

  !--------------------------------!
  !   Plot fig file if requested   !
  !--------------------------------!
  if(fig .eq. ON) then
    if(mes .eq. ON) print *, "Plotting the mesh in fig format"
    call File_Mod_Fig_Start
    call File_Mod_Fig_Draw
    call File_Mod_Fig_End
  end if

  !--------------------------------!
  !   Plot dxf file if requested   !
  !--------------------------------!
  if(dxf .eq. ON) then
    if(mes .eq. ON) print *, "Plotting the mesh in dfx format"
    call File_Mod_Dxf_Start
    call File_Mod_Dxf_Draw
    call File_Mod_Dxf_End
  end if

  if(mes .eq. ON) print *, "Done!"
  if(mes .eq. ON) print *, ""

  if(mes .eq. ON) call File_Mod_Mesh_Statistics

  if(mes .eq. ON) call Cpu_Timer_Mod_Stop('Easymesh_Main')
  if(mes .eq. ON) call Cpu_Timer_Mod_Statistics()

  end program
