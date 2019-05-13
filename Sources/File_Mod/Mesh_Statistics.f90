!==============================================================================!
  subroutine File_Mod_Mesh_Statistics
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer            :: c_node, c_elem, c_side, n, e, s
  integer            :: i, j, k, max_width
  real(RP)           :: area, max_area, min_area, avg_area
  real(RP)           :: max_r_rat, min_r_rat, avg_r_rat
  character(len=CL)  :: line
  integer, parameter :: L = 10
!==============================================================================!

  line( 1:CL) = " "
  line( 1+L:57+L) = "#=======================================================#"
  print *, trim(line)
  line( 1+L:57+L) = "#                   Mesh statistics                     #"
  print *, trim(line)
  line( 1+L:57+L) = "#-------------------------------------------------------#"
  print *, trim(line)

  !------------------------!
  !   Number of entities   !
  !------------------------!
  c_node = 0
  c_elem = 0
  c_side = 0
  do n = 0, n_node-1
    if(node(n) % mark .ne. OFF) c_node = c_node + 1
  end do
  do e = 0, n_elem-1
    if(elem(e) % mark .ne. OFF) c_elem = c_elem + 1
  end do
  do s = 0, n_side-1
    if(side(s) % mark .ne. OFF) c_side = c_side + 1
  end do

  line( 1:CL) = " ";  
  line(57+L:57+L) = "#"
  write(line( 1+L:23+L), "(a)")  "# Number of nodes    : "
  write(line(24+L:29+L), "(i6)") c_node
  print *, trim(line)
  write(line( 1+L:23+L), "(a)")  "# Number of elements : "
  write(line(24+L:29+L), "(i6)") c_elem
  print *, trim(line)
  write(line( 1+L:23+L), "(a)")  "# Number of sides    : "
  write(line(24+L:29+L), "(i6)") c_side
  print *, trim(line)
  line( 1+L:57+L) = "#-------------------------------------------------------#"
  print *, trim(line)

  !-------------------!
  !   Element areas   !
  !-------------------!
  avg_area =  0.0
  max_area = -GREAT
  min_area = +GREAT
  do e = 0, n_elem-1
    if(elem(e) % mark .ne. OFF) then
      i = elem(e) % i
      j = elem(e) % j
      k = elem(e) % k
      area = Mesh_Mod_Area(node(i), node(j), node(k))
      max_area = max(max_area, area)
      min_area = min(min_area, area)
      avg_area = avg_area + area
    end if
  end do
  avg_area = avg_area / n_elem

  line( 1:CL) = " ";  
  line(57+L:57+L) = "#"
  write(line( 1+L:26+L), "(a)")  "# Largest element area  : "
  write(line(27+L:36+L), "(es10.4)") max_area
  print *, trim(line)
  write(line( 1+L:26+L), "(a)")  "# Smallest element area : "
  write(line(27+L:36+L), "(es10.4)") min_area
  print *, trim(line)
  write(line( 1+L:26+L), "(a)")  "# Average element area  : "
  write(line(27+L:36+L), "(es10.4)") avg_area
  print *, trim(line)
  line( 1+L:57+L) = "#-------------------------------------------------------#"
  print *, trim(line)

  !--------------!
  !   Uglyness   !
  !--------------!
  max_r_rat = -GREAT
  min_r_rat = +GREAT
  avg_r_rat =  0.0
  do e = 0, n_elem-1
    if(elem(e) % mark .ne. OFF) then
      max_r_rat = max(max_r_rat, elem(e) % r_rat)
      min_r_rat = min(min_r_rat, elem(e) % r_rat)
      avg_r_rat = avg_r_rat + elem(e) % r_rat
    end if
  end do
  avg_r_rat = avg_r_rat / n_elem

  line( 1:CL) = " ";  
  line(57+L:57+L) = "#"
  write(line( 1+L:31+L), "(a)")  "# Maximum ex/in radius ratio : "
  write(line(32+L:41+L), "(es10.4)") max_r_rat
  print *, trim(line)
  write(line( 1+L:31+L), "(a)")  "# Minimum ex/in radius ratio : "
  write(line(32+L:41+L), "(es10.4)") min_r_rat
  print *, trim(line)
  write(line( 1+L:31+L), "(a)")  "# Average ex/in radius ratio : "
  write(line(32+L:41+L), "(es10.4)") avg_r_rat
  print *, trim(line)
  line( 1+L:57+L) = "#-------------------------------------------------------#"
  print *, trim(line)

  !----------------------!
  !   Matrix bandwidth   !
  !----------------------!
  max_width = 0
  do e = 0, n_elem-1
    if(elem(e) % mark .ne. OFF) then
      i = elem(e) % i
      j = elem(e) % j
      k = elem(e) % k
      i = node(i) % new_numb
      j = node(j) % new_numb
      k = node(k) % new_numb
      max_width = max(max_width, abs(i-j))
      max_width = max(max_width, abs(j-k))
      max_width = max(max_width, abs(k-i))
    end if
  end do

  line( 1:CL) = " ";  
  line(57+L:57+L) = "#"
  write(line( 1+L:29+L), "(a)")  "# Maximum matrix bandwidth : "
  write(line(30+L:33+L), "(i4)") max_width
  print *, trim(line)
  line( 1+L:57+L) = "#-------------------------------------------------------#"
  print *, trim(line)
  print *, ""

  end subroutine
