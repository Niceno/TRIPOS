!==============================================================================!
  subroutine Mesh_Mod_Smooth
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: it, s, n, e
!==============================================================================!

  do it = 0, 9
    do s = 0, n_side-1
      if(side(s) % mark .eq. 0) then
        if(node(side(s) % c) % mark .eq. 0) then
          node(side(s) % c) % sumx =  &
          node(side(s) % c) % sumx + node(side(s) % d) % x
          node(side(s) % c) % sumy =  &
          node(side(s) % c) % sumy + node(side(s) % d) % y
        end if

        if(node(side(s) % d) % mark .eq. 0) then
          node(side(s) % d) % sumx =  &
          node(side(s) % d) % sumx + node(side(s) % c) % x
          node(side(s) % d) % sumy =  &
          node(side(s) % d) % sumy + node(side(s) % c) % y
        end if
      end if
    end do

    do n = 0, n_node-1
      if(node(n) % mark .eq. 0) then
        node(n) % x = node(n) % sumx / node(n) % nne;  node(n) % sumx = 0.0
        node(n) % y = node(n) % sumy / node(n) % nne;  node(n) % sumy = 0.0
      end if
    end do
  end do

  do e = 0, n_elem-1
    if(elem(e) % mark .ne. OFF) then
      call Mesh_Mod_Circles(e)
    end if
  end do

  end subroutine
