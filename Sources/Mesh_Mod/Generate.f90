!==============================================================================!
  subroutine Mesh_Mod_Generate(mesh, comm)
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n_node_old
  type(Comm_Type)   :: comm
  type(Mesh_Type)   :: mesh
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Generate')

  call Mesh_Mod_Insert_Chains(mesh)    ! insert points which define domain
  call Mesh_Mod_Erase(mesh)            ! erase excessive elements
  call Mesh_Mod_Classify(mesh, comm % r_tol)

  !-----------------------!
  !   Main algorithm to   !
  !    generate a mesh    !
  !-----------------------!
  if(comm % mes .eq. ON) print *, "Generating mesh.  Please wait!"

  if(comm % tri .eq. ON) then
    do while(ugly .ne. OFF)
      n_node_old = mesh % n_node
      call Mesh_Mod_New_Node(mesh)
      call Mesh_Mod_Classify(mesh, comm % r_tol)

      if(comm % mes .eq. ON .and.  &
         mod(mesh % n_node, 500) .eq. 0) print "(i6,a)", mesh % n_node, " nodes"

      ! Exit because maximum number of nodes has been reached
      if(mesh % n_node .eq. MAX_NODES-1) then
        if(comm % mes .eq. on) then
          print "(a,i6,a)", " Maximum number of nodes (",  &
                            MAX_NODES, ") has been reached." 
          print "(a)",      " The mesh might suffer from poor quality"
        end if
        exit
      end if

      ! No new nodes added to domain, exit the loop
      if(mesh % n_node .eq. n_node_old) exit
    end do
  end if

  call Mesh_Mod_Neighbours(mesh)

  !-----------------------------------------------------!
  !   Improve mesh quality by relaxation and smooting   !
  !-----------------------------------------------------!
  if(comm % relax .eq. ON .or. comm % smooth .eq. ON) then
    if(comm % mes .eq. ON) print *, "Improving the mesh quality"

    if(comm % relax .eq. ON)   call Mesh_Mod_Relax(mesh)
    if(comm % relax .eq. ON .or.  &
       comm % smooth .eq. ON)  call Mesh_Mod_Smooth(mesh)
  end if

  !--------------------------------!
  !   Renumber all mesh entities   !
  !--------------------------------!
  if(comm % mes .eq. ON) print *, "Renumerating nodes, elements and sides"
  call Mesh_Mod_Renumber(mesh)
  call Mesh_Mod_Compress(mesh)

  !----------------------------!
  !   Process material marks   !
  !----------------------------!
  if(comm % mes .eq. ON) print *, "Processing material marks"
  call Mesh_Mod_Materials(mesh)

  if(comm % mes .eq. ON) call Cpu_Timer_Mod_Stop('Mesh_Mod_Generate')

  end subroutine
