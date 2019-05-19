!==============================================================================!
  subroutine Mesh_Mod_Erase(mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: s, n, e, a, b, c
  integer,         pointer :: ne, ns
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
  type(Chai_Type), pointer :: chain(:)
!==============================================================================!

  ! Take aliases
  ne    => mesh % n_elem
  ns    => mesh % n_side
  node  => mesh % node
  elem  => mesh % elem
  side  => mesh % side
  chain => mesh % chain

  !-----------------------------!
  !   Negative area check for   !
  !   elimination of elements   !
  !-----------------------------!
  do e = 0, ne-1
    if( (node(elem(e) % i) % chain .eq. node(elem(e) % j) % chain) .and.  &
        (node(elem(e) % j) % chain .eq. node(elem(e) % k) % chain) .and.  &
        (chain(node(elem(e) % i) % chain) % type .eq. CLOSED) ) then
      a = min( min(elem(e) % i, elem(e) % j), elem(e) % k )
      c = max( max(elem(e) % i, elem(e) % j), elem(e) % k )
      b = elem(e) % i+elem(e) % j+elem(e) % k - a - c

      if(a .lt. 3) then
        elem(e) % mark = OFF
      else if(Mesh_Mod_Area(node(a), node(b), node(c)) .lt. 0.0) then
        elem(e) % mark = OFF
      end if

    end if
  end do

  do e = 0, ne-1
    if(elem(elem(e) % ei) % mark .eq. OFF) elem(e) % ei = OFF
    if(elem(elem(e) % ej) % mark .eq. OFF) elem(e) % ej = OFF
    if(elem(elem(e) % ek) % mark .eq. OFF) elem(e) % ek = OFF
  end do

  !--------------------------!
  !   Elimination of sides   !
  !--------------------------!
  do s = 0, 2
    side(s) % mark = OFF
  end do

  do s = 3, ns-1
    if( (elem(side(s) % ea) % mark .eq. OFF)  .and.  &
        (elem(side(s) % eb) % mark .eq. OFF) ) then
      side(s) % mark = OFF;
    end if
  end do

  do s = 3, ns-1
    if(side(s) % mark .ne. OFF) then
      if(elem(side(s) % ea) % mark .eq. OFF) then
        side(s) % ea = OFF; side(s) % a = OFF
      end if
      if(elem(side(s) % eb) % mark .eq. OFF) then
        side(s) % eb = OFF; side(s) % b = OFF
      end if
    end if
  end do

  ! Correct negative ea
  do s = 3, ns-1
    if(side(s) % mark .ne. OFF) then
      if(side(s) % ea .eq. OFF) then
        call Swap_Int(side(s) % a,  side(s) % b)
        call Swap_Int(side(s) % c,  side(s) % d)
        call Swap_Int(side(s) % ea, side(s) % eb)
      end if
    end if
  end do

  !--------------------------!
  !   Elimination of nodes   !
  !--------------------------!
  do n = 0, 2
    node(n) % mark = OFF
  end do

  end subroutine
