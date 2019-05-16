!==============================================================================!
  subroutine Mesh_Mod_Smooth(mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: it, s, n, e
  integer,         pointer :: nn, ne, ns
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Smooth')

  ! Take aliases
  nn   => mesh % n_node
  ne   => mesh % n_elem
  ns   => mesh % n_side
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

  do it = 0, 9
    do s = 0, ns-1
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

    do n = 0, nn-1
      if(node(n) % mark .eq. 0) then
        node(n) % x = node(n) % sumx / node(n) % nne;  node(n) % sumx = 0.0
        node(n) % y = node(n) % sumy / node(n) % nne;  node(n) % sumy = 0.0
      end if
    end do
  end do

  do e = 0, ne-1
    if(elem(e) % mark .ne. OFF) then
      call Mesh_Mod_Circles(mesh, e)
    end if
  end do

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Smooth')

  end subroutine
