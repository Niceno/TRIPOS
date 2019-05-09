!==============================================================================!
  subroutine Mesh_Mod_Insert_Node(x,            &
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
  real(RP) :: x
  real(RP) :: y
  integer  :: spac
  integer  :: prev_n
  integer  :: prev_s_mark
  integer  :: mark
  integer  :: next_s_mark
  integer  :: next_n
!-----------------------------------[Locals]-----------------------------------!
  integer  :: i, j, k, e, ei, ej, ek, s, si, sj, sk
  real(RP) :: sx, sy
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Insert_Node')

  n_node = n_node + 1          ! one new node

  node(n_node-1) % x    = x
  node(n_node-1) % y    = y
  node(n_node-1) % mark = mark

  ! Find the element which contains new node
  e = Mesh_Mod_In_Elem(node(n_node-1))

  ! Calculate the spacing function in the new node
  if(spac .eq. ON)  &
    call Mesh_Mod_Spacing(e, n_node-1)

  i  = elem(e) % i;   j  = elem(e) % j;   k  = elem(e) % k
  ei = elem(e) % ei;  ej = elem(e) % ej;  ek = elem(e) % ek
  si = elem(e) % si;  sj = elem(e) % sj;  sk = elem(e) % sk

  n_elem = n_elem + 2
  n_side = n_side + 3

  !------------------!
  !   New elements   !
  !------------------!
  elem(n_elem-2) % i = n_node-1
  elem(n_elem-2) % j = k
  elem(n_elem-2) % k = i

  elem(n_elem-1) % i = n_node-1
  elem(n_elem-1) % j = i
  elem(n_elem-1) % k = j

  elem(n_elem-2) % ei = ej
  elem(n_elem-2) % ej = n_elem-1
  elem(n_elem-2) % ek = e

  elem(n_elem-1) % ei = ek
  elem(n_elem-1) % ej = e
  elem(n_elem-1) % ek = n_elem-2

  elem(n_elem-2) % si = sj
  elem(n_elem-2) % sj = n_side-2
  elem(n_elem-2) % sk = n_side-3

  elem(n_elem-1) % si = sk
  elem(n_elem-1) % sj = n_side-1
  elem(n_elem-1) % sk = n_side-2

  !---------------!
  !   New sides   !
  !---------------!
  side(n_side-3) % c  = k;         side(n_side-3) % d  = n_node-1   ! c-d
  side(n_side-3) % a  = j;         side(n_side-3) % b  = i          ! a-b
  side(n_side-3) % ea = e;         side(n_side-3) % eb = n_elem-2

  side(n_side-2) % c  = i;         side(n_side-2) % d  = n_node-1   ! c-d
  side(n_side-2) % a  = k;         side(n_side-2) % b  = j          ! a-b
  side(n_side-2) % ea = n_elem-2;  side(n_side-2) % eb = n_elem-1

  side(n_side-1) % c  = j;         side(n_side-1) % d  = n_node-1   ! c-d
  side(n_side-1) % a  = i;         side(n_side-1) % b  = k          ! a-b
  side(n_side-1) % ea = n_elem-1;  side(n_side-1) % eb = e

  do s = 1, 3
    sx = node(side(n_side-s) % c) % x - node(side(n_side-s) % d) % x
    sy = node(side(n_side-s) % c) % y - node(side(n_side-s) % d) % y
    side(n_side-s) % s = sqrt(sx*sx+sy*sy)
  end do

  elem(e) % i  = n_node - 1
  elem(e) % ej = n_elem - 2
  elem(e) % ek = n_elem - 1
  elem(e) % sj = n_side - 3
  elem(e) % sk = n_side-  1

  if(side(si) % a .eq. i) then
    side(si) % a  = n_node - 1
    side(si) % ea = e
  end if
  if(side(si) % b .eq. i) then
    side(si) % b  = n_node-1
    side(si) % eb = e
  end if

  if(side(sj) % a .eq. j) then
    side(sj) % a  = n_node - 1
    side(sj) % ea = n_elem - 2
  end if
  if(side(sj) % b .eq. j) then
    side(sj) % b  = n_node - 1
    side(sj) % eb = n_elem - 2
  end if

  if(side(sk) % a .eq. k) then
    side(sk) % a  = n_node - 1
    side(sk) % ea = n_elem - 1
  end if
  if(side(sk) % b .eq. k) then
    side(sk) % b  = n_node - 1
    side(sk) % eb = n_elem - 1
  end if

  if(ej .ne. -1) then
    if(elem(ej) % ei .eq. e) elem(ej) % ei = n_elem - 2
    if(elem(ej) % ej .eq. e) elem(ej) % ej = n_elem - 2
    if(elem(ej) % ek .eq. e) elem(ej) % ek = n_elem - 2
  end if

  if(ek .ne. -1) then
    if(elem(ek) % ei .eq. e) elem(ek) % ei = n_elem - 1
    if(elem(ek) % ej .eq. e) elem(ek) % ej = n_elem - 1
    if(elem(ek) % ek .eq. e) elem(ek) % ek = n_elem - 1
  end if

  ! Find circumenters for two new elements, 
  ! and for the one who's segment has changed
  call Mesh_Mod_Circles(e)
  call Mesh_Mod_Circles(n_elem-2)
  call Mesh_Mod_Circles(n_elem-1)

  call Mesh_Mod_Bowyer(n_node-1)

  !----------------------------------------------------!
  !   NEW ! Insert boundary conditions for the sides   !
  !----------------------------------------------------!
  do s = 3, n_side-1
    if(side(s) % c .eq. prev_n  .and. side(s) % d .eq. n_node-1)  &
      side(s) % mark = prev_s_mark
    if(side(s) % d .eq. prev_n  .and. side(s) % c .eq. n_node-1)  &
      side(s) % mark = prev_s_mark
    if(side(s) % c .eq. next_n  .and. side(s) % d .eq. n_node-1)  &
      side(s) % mark = next_s_mark
    if(side(s) % d .eq. next_n  .and. side(s) % c .eq. n_node-1)  &
      side(s) % mark = next_s_mark
  end do 

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Insert_Node')

  end subroutine
