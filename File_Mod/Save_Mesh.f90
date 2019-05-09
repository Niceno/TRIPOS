!==============================================================================!
  subroutine File_Mod_Save_Mesh
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer                      :: e, s, n, r_n_node, r_n_side, r_n_elem
  type(Node_Type), allocatable :: r_node(:)
  type(Elem_Type), allocatable :: r_elem(:)
  type(Side_Type), allocatable :: r_side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Save_Mesh')

  ! Initialize renumbered numbers of nodes, sides and elements
  r_n_node = 0
  r_n_side = 0
  r_n_elem = 0
  len = len_trim(name)

  ! Allocate memory for local structures
  allocate(r_node(0:n_node-1))
  allocate(r_elem(0:n_elem-1))
  allocate(r_side(0:n_side-1))

  !---------------------------------!
  !   Renumber and save node data   !
  !---------------------------------!

  ! Renumber
  do n=0, n_node-1
    if(node(n) % mark .ne. OFF .and. node(n) % new_numb .ne. OFF) then
      r_n_node = r_n_node + 1
      r_node(node(n) % new_numb) % x    = node(n) % x
      r_node(node(n) % new_numb) % y    = node(n) % y
      r_node(node(n) % new_numb) % mark = node(n) % mark
    end if
  end do

  ! Form file name and open it
  name(len:len) = "n"
  open(unit=FU, file=name)

  ! Save
  write(FU, *) r_n_node
  do n=0, r_n_node-1
    write(FU, "(i4, a1, 2es22.13e3, i4)")  &
                       n, ":", r_node(n) % x, r_node(n) % y, r_node(n) % mark
  end do
  write(FU, "(a)") "--------------------------------------------------------"
  write(FU, "(a)") "   n:  x                     y                      mark"

  close(FU)

  !------------------------------------!
  !   Renumber and save element data   !
  !------------------------------------!

  ! Renumber
  do e=0, n_elem-1
    if(elem(e) % mark .ne. OFF .and. elem(e) % new_numb .ne. OFF) then
      r_n_elem = r_n_elem + 1
      r_elem(elem(e) % new_numb) % i  = node(elem(e) % i) % new_numb
      r_elem(elem(e) % new_numb) % j  = node(elem(e) % j) % new_numb
      r_elem(elem(e) % new_numb) % k  = node(elem(e) % k) % new_numb
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

  ! Form file name and open it
  name(len:len) = "e"
  open(unit=FU, file=name)

  ! Save
  write(FU, *) r_n_elem
  do e=0, r_n_elem-1
    write(FU, "(i4, a1, 9i5, 2es22.13e3, i4)")                     &
          e, ":", r_elem(e) % i,  r_elem(e) % j,  r_elem(e) % k,   &
                  r_elem(e) % ei, r_elem(e) % ej, r_elem(e) % ek,  &
                  r_elem(e) % si, r_elem(e) % sj, r_elem(e) % sk,  &
                  r_elem(e) % xv, r_elem(e) % yv,                  &
                  r_elem(e) % material
  end do
  write(FU, "(a)") "---------------------------------------------------" // &
                   "--------------------------------------------------"
  write(FU, "(a)") "   e:   i,   j,   k,  ei,  ej,  ek,  si,  sj,  sk" // &
                   "   xv,                   yv,                    sign"
  close(FU)

  !---------------------------------!
  !   Renumber and save side data   !
  !---------------------------------!

  ! Renumber
  do s=0, n_side-1
    if(side(s) % mark .ne. OFF .and. side(s) % new_numb .ne. OFF) then
      r_n_side = r_n_side + 1
      r_side(side(s) % new_numb) % c    = node(side(s) % c) % new_numb
      r_side(side(s) % new_numb) % d    = node(side(s) % d) % new_numb
      r_side(side(s) % new_numb) % mark = side(s) % mark;

      if(side(s) % a .ne. OFF) then
        r_side(side(s) % new_numb) % a  = node(side(s) % a) % new_numb
        r_side(side(s) % new_numb) % ea = elem(side(s) % ea) % new_numb
      else
        r_side(side(s) % new_numb) % a  = OFF
        r_side(side(s) % new_numb) % ea = OFF
      end if

      if(side(s) % b .ne. OFF) then
        r_side(side(s) % new_numb) % b  = node(side(s) % b) % new_numb
        r_side(side(s) % new_numb) % eb = elem(side(s) % eb) % new_numb
      else
        r_side(side(s) % new_numb) % b  = OFF
        r_side(side(s) % new_numb) % eb = OFF
      end if
    end if
  end do

  ! Form file name and open it
  name(len:len) = "s"
  open(unit=FU, file=name)

  ! Save
  write(FU, *) r_n_side
  do s=0, r_n_side-1
    write(FU, "(i4, a1, 5i4)")                            &
                 s, ":", r_side(s) % c,  r_side(s) % d,   &
                         r_side(s) % ea, r_side(s) % eb,  &
                         r_side(s) % mark
  end do
  write(FU, "(a)") "--------------------------------"
  write(FU, "(a)") "   s:    c    d   ea   eb   mark"

  close(FU)

  call Cpu_Timer_Mod_Stop('File_Mod_Save_Mesh')

  end subroutine
