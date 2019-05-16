!==============================================================================!
  subroutine Mesh_Mod_Insert_Node(mesh,         &
                                  x,            &
                                  y,            &
                                  spac,         &
                                  prev_n,       &
                                  prev_s_mark,  &
                                  mark,         &
                                  next_s_mark,  &
                                  next_n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  real(RP)                :: x
  real(RP)                :: y
  integer                 :: spac
  integer                 :: prev_n
  integer                 :: prev_s_mark
  integer                 :: mark
  integer                 :: next_s_mark
  integer                 :: next_n
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: i, j, k, e, ei, ej, ek, s, si, sj, sk
  real(RP)                 :: sx, sy
  integer,         pointer :: nn, ne, ns
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Insert_Node')

  ! Take aliases
  nn    => mesh % n_node
  ne    => mesh % n_elem
  ns    => mesh % n_side
  node  => mesh % node
  elem  => mesh % elem
  side  => mesh % side

  nn = nn + 1          ! one new node

  node(nn-1) % x    = x
  node(nn-1) % y    = y
  node(nn-1) % mark = mark

  ! Find the element which contains new node
  e = Mesh_Mod_In_Elem(mesh, node(nn-1))

  ! Calculate the spacing function in the new node
  if(spac .eq. ON)  call Mesh_Mod_Spacing(mesh, e, nn-1)

  i  = elem(e) % i;   j  = elem(e) % j;   k  = elem(e) % k
  ei = elem(e) % ei;  ej = elem(e) % ej;  ek = elem(e) % ek
  si = elem(e) % si;  sj = elem(e) % sj;  sk = elem(e) % sk

  ne = ne + 2
  ns = ns + 3

  !------------------!
  !   New elements   !
  !------------------!
  elem(ne-2) % i = nn-1
  elem(ne-2) % j = k
  elem(ne-2) % k = i

  elem(ne-1) % i = nn-1
  elem(ne-1) % j = i
  elem(ne-1) % k = j

  elem(ne-2) % ei = ej
  elem(ne-2) % ej = ne-1
  elem(ne-2) % ek = e

  elem(ne-1) % ei = ek
  elem(ne-1) % ej = e
  elem(ne-1) % ek = ne-2

  elem(ne-2) % si = sj
  elem(ne-2) % sj = ns-2
  elem(ne-2) % sk = ns-3

  elem(ne-1) % si = sk
  elem(ne-1) % sj = ns-1
  elem(ne-1) % sk = ns-2

  !---------------!
  !   New sides   !
  !---------------!
  side(ns-3) % c  = k
  side(ns-3) % d  = nn-1   ! c-d
  side(ns-3) % a  = j
  side(ns-3) % b  = i      ! a-b
  side(ns-3) % ea = e
  side(ns-3) % eb = ne-2

  side(ns-2) % c  = i
  side(ns-2) % d  = nn-1   ! c-d
  side(ns-2) % a  = k
  side(ns-2) % b  = j      ! a-b
  side(ns-2) % ea = ne-2
  side(ns-2) % eb = ne-1

  side(ns-1) % c  = j
  side(ns-1) % d  = nn-1   ! c-d
  side(ns-1) % a  = i
  side(ns-1) % b  = k      ! a-b
  side(ns-1) % ea = ne-1
  side(ns-1) % eb = e

  do s = 1, 3
    sx = node(side(ns-s) % c) % x - node(side(ns-s) % d) % x
    sy = node(side(ns-s) % c) % y - node(side(ns-s) % d) % y
    side(ns-s) % s = sqrt(sx*sx+sy*sy)
  end do

  elem(e) % i  = nn - 1
  elem(e) % ej = ne - 2
  elem(e) % ek = ne - 1
  elem(e) % sj = ns - 3
  elem(e) % sk = ns-  1

  if(side(si) % a .eq. i) then
    side(si) % a  = nn - 1
    side(si) % ea = e
  end if
  if(side(si) % b .eq. i) then
    side(si) % b  = nn-1
    side(si) % eb = e
  end if

  if(side(sj) % a .eq. j) then
    side(sj) % a  = nn - 1
    side(sj) % ea = ne - 2
  end if
  if(side(sj) % b .eq. j) then
    side(sj) % b  = nn - 1
    side(sj) % eb = ne - 2
  end if

  if(side(sk) % a .eq. k) then
    side(sk) % a  = nn - 1
    side(sk) % ea = ne - 1
  end if
  if(side(sk) % b .eq. k) then
    side(sk) % b  = nn - 1
    side(sk) % eb = ne - 1
  end if

  if(ej .ne. -1) then
    if(elem(ej) % ei .eq. e) elem(ej) % ei = ne - 2
    if(elem(ej) % ej .eq. e) elem(ej) % ej = ne - 2
    if(elem(ej) % ek .eq. e) elem(ej) % ek = ne - 2
  end if

  if(ek .ne. -1) then
    if(elem(ek) % ei .eq. e) elem(ek) % ei = ne - 1
    if(elem(ek) % ej .eq. e) elem(ek) % ej = ne - 1
    if(elem(ek) % ek .eq. e) elem(ek) % ek = ne - 1
  end if

  ! Find circumenters for two new elements, 
  ! and for the one who's segment has changed
  call Mesh_Mod_Circles(mesh, e)
  call Mesh_Mod_Circles(mesh, ne-2)
  call Mesh_Mod_Circles(mesh, ne-1)

  call Mesh_Mod_Bowyer(mesh, nn-1)

  !----------------------------------------------------!
  !   NEW ! Insert boundary conditions for the sides   !
  !----------------------------------------------------!
  do s = 3, ns-1
    if(side(s) % c .eq. prev_n  .and. side(s) % d .eq. nn-1)  &
      side(s) % mark = prev_s_mark
    if(side(s) % d .eq. prev_n  .and. side(s) % c .eq. nn-1)  &
      side(s) % mark = prev_s_mark
    if(side(s) % c .eq. next_n  .and. side(s) % d .eq. nn-1)  &
      side(s) % mark = next_s_mark
    if(side(s) % d .eq. next_n  .and. side(s) % c .eq. nn-1)  &
      side(s) % mark = next_s_mark
  end do 

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Insert_Node')

  end subroutine
