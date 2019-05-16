!==============================================================================!
  subroutine Mesh_Mod_Relax(mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: s, t, e
  integer,         pointer :: ns
  type(Node_Type), pointer :: node(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Relax')

  ! Take aliases
  ns   => mesh % n_side
  node => mesh % node
  side => mesh % side

  do t = 6, 3, -1
    do s = 0, ns-1

      if(side(s) % mark .eq. 0) then
        if( (node(side(s) % a) % mark .eq. 0)  .and.  &
            (node(side(s) % b) % mark .eq. 0)  .and.  &
            (node(side(s) % c) % mark .eq. 0)  .and.  &
            (node(side(s) % d) % mark .eq. 0) ) then
          e =   node(side(s) % c) % nne + node(side(s) % d) % nne  &
              - node(side(s) % a) % nne - node(side(s) % b) % nne

          if(e .eq. t) then
            node(side(s) % a) % nne = node(side(s) % a) % nne + 1
            node(side(s) % b) % nne = node(side(s) % b) % nne + 1
            node(side(s) % c) % nne = node(side(s) % c) % nne - 1
            node(side(s) % d) % nne = node(side(s) % d) % nne - 1
            call Mesh_Mod_Swap_Side(mesh, s)
          end if
        end if
      end if

    end do
  end do

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Relax')

  end subroutine
