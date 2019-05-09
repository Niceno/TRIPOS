!==============================================================================!
  subroutine Mesh_Mod_Neighbours
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: s
!==============================================================================!

  do s = 0, n_side-1

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
