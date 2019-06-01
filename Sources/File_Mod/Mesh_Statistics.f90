!==============================================================================!
  subroutine File_Mod_Mesh_Statistics(mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e
  integer                  :: i, j, k, max_width
  real(RP)                 :: area, max_area, min_area, avg_area, tot_area
  real(RP)                 :: max_r_rat, min_r_rat, avg_r_rat
  character(len=CL)        :: line
  integer, parameter       :: L = 8
  integer,         pointer :: nn, ne, ns
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  ! Take aliases
  nn   => mesh % n_node
  ne   => mesh % n_elem
  ns   => mesh % n_side
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

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
  line( 1:CL) = " ";  
  line(57+L:57+L) = "#"
  write(line( 1+L:23+L), "(a)")  "# Number of nodes    : "
  write(line(24+L:29+L), "(i6)") nn
  print *, trim(line)
  write(line( 1+L:23+L), "(a)")  "# Number of elements : "
  write(line(24+L:29+L), "(i6)") ne
  print *, trim(line)
  write(line( 1+L:23+L), "(a)")  "# Number of sides    : "
  write(line(24+L:29+L), "(i6)") ns
  print *, trim(line)
  line( 1+L:57+L) = "#-------------------------------------------------------#"
  print *, trim(line)

  !-------------------!
  !   Element areas   !
  !-------------------!
  tot_area =  0.0
  max_area = -GREAT
  min_area = +GREAT
  do e = 0, ne-1
    if(elem(e) % mark .ne. OFF) then
      i = elem(e) % i
      j = elem(e) % j
      k = elem(e) % k
      area = Mesh_Mod_Area(node(i), node(j), node(k))
      max_area = max(max_area, area)
      min_area = min(min_area, area)
      tot_area = tot_area + area
    end if
  end do
  avg_area = tot_area / ne

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
  write(line( 1+L:26+L), "(a)")  "# Total domain area     : "
  write(line(27+L:36+L), "(es10.4)") tot_area
  print *, trim(line)
  line( 1+L:57+L) = "#-------------------------------------------------------#"
  print *, trim(line)

  !--------------!
  !   Uglyness   !
  !--------------!
  max_r_rat = -GREAT
  min_r_rat = +GREAT
  avg_r_rat =  0.0
  do e = 0, ne-1
    if(elem(e) % mark .ne. OFF) then
      max_r_rat = max(max_r_rat, elem(e) % r_rat)
      min_r_rat = min(min_r_rat, elem(e) % r_rat)
      avg_r_rat = avg_r_rat + elem(e) % r_rat
    end if
  end do
  avg_r_rat = avg_r_rat / ne

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
  do e = 0, ne-1
    if(elem(e) % mark .ne. OFF) then
      i = elem(e) % i
      j = elem(e) % j
      k = elem(e) % k
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
