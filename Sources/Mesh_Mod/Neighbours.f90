!==============================================================================!
  subroutine Mesh_Mod_Neighbours(mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: s
  integer,         pointer :: ns
  type(Node_Type), pointer :: node(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  ! Take aliases
  ns   => mesh % n_side
  node => mesh % node
  side => mesh % side

  do s = 0, ns-1

    if(side(s) % mark .eq. 0) then

      if(node(side(s) % c) % mark .eq. 0) then
        node(side(s) % c) % nne = node(side(s) % c) % nne + 1
      end if

      if(node(side(s) % d) % mark .eq. 0) then
        node(side(s) % d) % nne = node(side(s) % d) % nne + 1
      end if

    end if

  end do

  end subroutine
