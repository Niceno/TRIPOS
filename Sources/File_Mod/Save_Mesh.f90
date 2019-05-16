!==============================================================================!
  subroutine File_Mod_Save_Mesh
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer                      :: e, s, n
  integer                      :: nf
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Save_Mesh')

  ! Take alias of "num_from"; this is zero or one
  nf = num_from

  len = len_trim(name)

  !---------------!
  !   Node data   !
  !---------------!

  ! Form file name and open it
  name(len:len) = "n"
  open(unit=FU, file=name)

  ! Save
  write(FU, *) n_node
  do n=0, n_node-1
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
  name(len:len) = "e"
  open(unit=FU, file=name)

  ! Save
  write(FU, *) n_elem
  do e=0, n_elem-1
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
  name(len:len) = "s"
  open(unit=FU, file=name)

  ! Save
  write(FU, *) n_side
  do s=0, n_side-1
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
