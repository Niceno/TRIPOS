!==============================================================================!
  subroutine File_Mod_Save_Mesh(mesh, comm)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  type(Comm_Type)         :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e, s, n
  integer                  :: nf, len
  integer,         pointer :: nn, ne, ns
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Save_Mesh')

  ! Take aliases
  nn   => mesh % n_node
  ne   => mesh % n_elem
  ns   => mesh % n_side
  node => mesh % node
  elem => mesh % elem
  side => mesh % side
  nf   =  num_from      ! take alias of "num_from"; this is zero or one

  !---------------!
  !   Node data   !
  !---------------!

  ! Form file name and open it (bad practice to fiddle comm variable here)
  len = len_trim(comm % problem_name)
  comm % problem_name(len:len) = "n"
  open(unit=FU, file = comm % problem_name)

  ! Save
  write(FU, *) nn
  do n=0, nn-1
    write(FU, "(i6, a1, 2es22.13e3, i4)")  &
                     n+nf, ":", node(n) % x, node(n) % y, node(n) % mark
  end do
  write(FU, "(a)") "----------------------------------------------------------"
  write(FU, "(a)") "     n:  x                     y                      mark"

  close(FU)

  !------------------!
  !   Element data   !
  !------------------!

  ! Form file name and open it
  comm % problem_name(len:len) = "e"
  open(unit=FU, file = comm % problem_name)

  ! Save
  write(FU, *) ne
  do e=0, ne-1
    write(FU, "(i6, a1, 9i6, 2es22.13e3, i4)")                         &
        e+nf, ":", elem(e) % i +nf, elem(e) % j +nf, elem(e) % k +nf,  &
                   elem(e) % ei+nf, elem(e) % ej+nf, elem(e) % ek+nf,  &
                   elem(e) % si+nf, elem(e) % sj+nf, elem(e) % sk+nf,  &
                   elem(e) % xv,    elem(e) % yv,                      &
                   elem(e) % material
  end do
  write(FU, "(a)") "-----------------------------------------------------" // &
                   "-----------------------------------------------------" // &
                   "------"
  write(FU, "(a)") "     e:  i,    j,    k,   ei,   ej,   ek,"  // &
                   "   si,   sj,   sk"                          // &
                   "     xv,                   yv,                    sign"
  close(FU)

  !---------------!
  !   Side data   !
  !---------------!

  ! Form file name and open it
  comm % problem_name(len:len) = "s"
  open(unit=FU, file = comm % problem_name)

  ! Save
  write(FU, *) ns
  do s=0, ns-1
    write(FU, "(i6, a1, 5i6)")                            &
                 s+nf, ":", side(s) % c+nf,  side(s) % d+nf,   &
                            side(s) % ea+nf, side(s) % eb+nf,  &
                            side(s) % mark
  end do
  write(FU, "(a)") "-------------------------------------"
  write(FU, "(a)") "     s:   c     d    ea    eb    mark"

  close(FU)

  call Cpu_Timer_Mod_Stop('File_Mod_Save_Mesh')

  end subroutine
