!==============================================================================!
  subroutine Mesh_Mod_Renumber
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: n, s, e, c, d, i, j, k;
  integer :: new_elem, new_node, new_side, next_e, next_s, lowest
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Renumber')

  ! Initialize new_numbs
  do n = 0, n_node-1
    node(n) % new_numb = OFF
  end do
  do e = 0, n_elem-1
    elem(e) % new_numb = OFF
  end do
  do s = 0, n_side-1
    side(s) % new_numb = OFF
  end do

  new_elem = 0
  new_node = 0
  new_side = 0

  ! Search for the first element which is ON
  do e = 0, n_elem-1
    if(elem(e) % mark .ne. OFF) goto 1
  end do
1 continue

  ! Assign numbers 0 and 1 to the nodes of first element
  node(elem(e) % i) % new_numb = new_node; new_node = new_node + 1
  node(elem(e) % j) % new_numb = new_node; new_node = new_node + 1

  !---------------------------!
  !   Renumeration of nodes   !
  !---------------------------!
  do
    lowest = n_node * 2
    next_e = OFF

    do e = 0, n_elem-1
      if(elem(e) % mark .ne. OFF .and. elem(e) % new_numb .eq. OFF) then
        i = node(elem(e) % i) % new_numb
        j = node(elem(e) % j) % new_numb
        k = node(elem(e) % k) % new_numb
 
        if( i+j+k+2 .eq. abs(i) + abs(j) + abs(k) ) then
          if( (i .eq. OFF) .and. (j+k) < lowest) then
            next_e = e;  lowest = j+k
          end if
          if( (j .eq. OFF) .and. (i+k) < lowest) then
            next_e = e;  lowest = i+k
          end if
          if( (k .eq. OFF) .and. (i+j) < lowest) then
            next_e = e;  lowest = i+j
          end if
        end if
      end if
    end do
 
    if(next_e .ne. OFF) then
      i = node(elem(next_e) % i) % new_numb
      j = node(elem(next_e) % j) % new_numb
      k = node(elem(next_e) % k) % new_numb
 
      ! Assign a new number to the node
      if(i .eq. OFF) then
        node(elem(next_e) % i) % new_numb = new_node
        new_node = new_node + 1
      end if
      if(j .eq. OFF) then
        node(elem(next_e) % j) % new_numb = new_node
        new_node = new_node + 1
      end if
      if(k .eq. OFF) then
        node(elem(next_e) % k) % new_numb = new_node
        new_node = new_node + 1
      end if
    end if

    if(next_e .eq. OFF) goto 2  ! effectivelly: do ... while(next_e .ne. OFF)
  end do
2 continue

  !------------------------------!
  !   Renumeration of elements   !
  !------------------------------!
  do
    lowest = n_node * 3
    next_e = OFF;

    do e = 0, n_elem-1
      if(elem(e) % mark .ne. OFF .and. elem(e) % new_numb .eq. OFF) then
        i = node(elem(e) % i) % new_numb
        j = node(elem(e) % j) % new_numb
        k = node(elem(e) % k) % new_numb

        if( (i+j+k) < lowest ) then
          next_e = e;
          lowest = i+j+k
        end if
      end if
    end do

    if(next_e .ne. OFF) then
     elem(next_e) % new_numb = new_elem; new_elem = new_elem + 1
    end if

    if(next_e .eq. OFF) goto 3  ! effectivelly: do ... while(next_e .ne. OFF)
  end do
3 continue

  !---------------------------!
  !   Renumeration of sides   !
  !---------------------------!
  do
    lowest = n_node * 2
    next_s = OFF

    do s = 0, n_side-1
      if(side(s) % mark .ne. OFF .and. side(s) % new_numb .eq. OFF) then
        c = node(side(s) % c) % new_numb
        d = node(side(s) % d) % new_numb

        if( (c+d) < lowest) then
          lowest = c + d;  next_s = s
        end if
      end if
    end do

    if(next_s .ne. OFF) then
      side(next_s) % new_numb = new_side
      new_side = new_side + 1
    end if

    if(next_s .eq. OFF) goto 4  ! effectivelly: do ... while(next_s .ne. OFF)
  end do
4 continue

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Renumber')

  end subroutine
