!==============================================================================!
  subroutine Mesh_Mod_Compress
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer                      :: e, s, n, r_n_node, r_n_side, r_n_elem
  type(Node_Type), allocatable :: r_node(:)
  type(Elem_Type), allocatable :: r_elem(:)
  type(Side_Type), allocatable :: r_side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Compress')

  ! Allocate memory for local structures
  allocate(r_node(0:n_node-1))
  allocate(r_elem(0:n_elem-1))
  allocate(r_side(0:n_side-1))

  !--------------------!
  !   Renumber nodes   !
  !--------------------!

  ! Renumber
  r_n_node = 0
  do n=0, n_node-1
    if(node(n) % mark .ne. OFF .and. node(n) % new_numb .ne. OFF) then
      r_n_node = r_n_node + 1
      r_node(node(n) % new_numb) % x    = node(n) % x
      r_node(node(n) % new_numb) % y    = node(n) % y
      r_node(node(n) % new_numb) % mark = node(n) % mark
    end if
  end do

  ! Compress nodes
  n_node = r_n_node
  node(0:n_node-1) % x    = r_node(0:n_node-1) % x
  node(0:n_node-1) % y    = r_node(0:n_node-1) % y
  node(0:n_node-1) % mark = r_node(0:n_node-1) % mark

  ! Change elements and sides who contain node data
  do e=0, n_elem-1
    elem(e) % i = node(elem(e) % i) % new_numb
    elem(e) % j = node(elem(e) % j) % new_numb
    elem(e) % k = node(elem(e) % k) % new_numb
  end do
  do s=0, n_side-1
    if(side(s) % a .ne. OFF)  &
      side(s) % a = node(side(s) % a) % new_numb
    if(side(s) % b .ne. OFF)  &
      side(s) % b = node(side(s) % b) % new_numb
    side(s) % c = node(side(s) % c) % new_numb
    side(s) % d = node(side(s) % d) % new_numb
  end do

  !-----------------------!
  !   Renumber elements   !
  !-----------------------!

  ! Renumber
  r_n_elem = 0
  do e=0, n_elem-1
    if(elem(e) % mark .ne. OFF .and. elem(e) % new_numb .ne. OFF) then
      r_n_elem = r_n_elem + 1
      r_elem(elem(e) % new_numb) % i  = elem(e) % i
      r_elem(elem(e) % new_numb) % j  = elem(e) % j
      r_elem(elem(e) % new_numb) % k  = elem(e) % k
      r_elem(elem(e) % new_numb) % si = side(elem(e) % si) % new_numb
      r_elem(elem(e) % new_numb) % sj = side(elem(e) % sj) % new_numb
      r_elem(elem(e) % new_numb) % sk = side(elem(e) % sk) % new_numb
      r_elem(elem(e) % new_numb) % xv = elem(e) % xv;
      r_elem(elem(e) % new_numb) % yv = elem(e) % yv;
      r_elem(elem(e) % new_numb) % material = elem(e) % material

      if(elem(e) % ei .ne. OFF) then
        r_elem(elem(e) % new_numb) % ei = elem(elem(e) % ei) % new_numb
      else
        r_elem(elem(e) % new_numb) % ei = OFF;
      end if

      if(elem(e) % ej .ne. OFF) then
        r_elem(elem(e) % new_numb) % ej = elem(elem(e) % ej) % new_numb
      else
        r_elem(elem(e) % new_numb) % ej = OFF;
      end if

      if(elem(e) % ek .ne. OFF) then
        r_elem(elem(e) % new_numb) % ek = elem(elem(e) % ek) % new_numb
      else
        r_elem(elem(e) % new_numb) % ek = OFF;
      end if
    end if
  end do

  ! Compress elements
  n_elem = r_n_elem
  elem(0:n_elem-1) % i        = r_elem(0:n_elem-1) % i
  elem(0:n_elem-1) % j        = r_elem(0:n_elem-1) % j
  elem(0:n_elem-1) % k        = r_elem(0:n_elem-1) % k
  elem(0:n_elem-1) % ei       = r_elem(0:n_elem-1) % ei
  elem(0:n_elem-1) % ej       = r_elem(0:n_elem-1) % ej
  elem(0:n_elem-1) % ek       = r_elem(0:n_elem-1) % ek
  elem(0:n_elem-1) % si       = r_elem(0:n_elem-1) % si
  elem(0:n_elem-1) % sj       = r_elem(0:n_elem-1) % sj
  elem(0:n_elem-1) % sk       = r_elem(0:n_elem-1) % sk
  elem(0:n_elem-1) % xv       = r_elem(0:n_elem-1) % xv
  elem(0:n_elem-1) % yv       = r_elem(0:n_elem-1) % yv
  elem(0:n_elem-1) % mark     = r_elem(0:n_elem-1) % mark
  elem(0:n_elem-1) % material = r_elem(0:n_elem-1) % material

  ! Change side data which contains elements
  do s=0, n_side-1
    if(side(s) % ea .ne. OFF)  side(s) % ea = elem(side(s) % ea) % new_numb
    if(side(s) % eb .ne. OFF)  side(s) % eb = elem(side(s) % eb) % new_numb
  end do

  !--------------------!
  !   Renumber sides   !
  !--------------------!

  ! Renumber
  r_n_side = 0
  do s=0, n_side-1
    if(side(s) % mark .ne. OFF .and. side(s) % new_numb .ne. OFF) then
      r_n_side = r_n_side + 1
      r_side(side(s) % new_numb) % a  = side(s) % a
      r_side(side(s) % new_numb) % b  = side(s) % b
      r_side(side(s) % new_numb) % c  = side(s) % c
      r_side(side(s) % new_numb) % d  = side(s) % d
      r_side(side(s) % new_numb) % ea = side(s) % ea
      r_side(side(s) % new_numb) % eb = side(s) % eb
      r_side(side(s) % new_numb) % mark = side(s) % mark;
    end if
  end do

  ! Compress sides
  n_side = r_n_side
  side(0:n_side-1) % a    = r_side(0:n_side-1) % a
  side(0:n_side-1) % b    = r_side(0:n_side-1) % b
  side(0:n_side-1) % c    = r_side(0:n_side-1) % c
  side(0:n_side-1) % d    = r_side(0:n_side-1) % d
  side(0:n_side-1) % ea   = r_side(0:n_side-1) % ea
  side(0:n_side-1) % eb   = r_side(0:n_side-1) % eb
  side(0:n_side-1) % mark = r_side(0:n_side-1) % mark

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Compress')

  end subroutine
