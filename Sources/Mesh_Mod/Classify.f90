!==============================================================================!
  subroutine Mesh_Mod_Classify(mesh, r_tol)
!*-----------------------------------------------------------------------------!
!  Find the ugliest element                                                    !
!                                                                              !
!  This function searches through all elements every time to find the ugliest. !
!  Some optimisation should definitely bee attampted.  But it also must me     !
!  noted, that this function defines the strategy for insertion of new nodes.  !
!                                                                              !
!  It's MUCH MUCH better when the ugliest element is found as one with         !
!  highest ratio of R/r !!! (before it was element with greater R)             !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  real(RP)                :: r_tol
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e, ei, ej, ek, si, sj, sk
  real(RP)                 :: max_ratio, f
  logical, save            :: done_boundary = .false.
  integer,         pointer :: ne
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Classify')

  ! Take aliases
  ne   => mesh % n_elem
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

  ! Initialize variables
  max_ratio = -GREAT
  ugly      =  OFF

  !----------------------------------------------!
  !   Browse throuhg all elements to see which   !
  !      are e waiting and which are done        !
  !----------------------------------------------!
  do e = 0, ne-1
    if(elem(e) % mark .ne. OFF) then
      if(elem(e) % state .ne. DONE) then
        ei = elem(e) % ei
        ej = elem(e) % ej
        ek = elem(e) % ek

        f = (  node(elem(e) % i) % f  &
             + node(elem(e) % j) % f  &
             + node(elem(e) % k) % f) / 3.0

        elem(e) % state = WAITING

        ! Ideal triangle would have r_tol equal to 0.577
        ! Version 1.5d introduced variable r_tol which 
        ! can be set by command line option.  Before,
        ! it was constant equal to 0.7
        if(elem(e) % r_ex < r_tol*f) then
          elem(e) % state = DONE
        end if

        ! Even this is possible
        if(ei .ne. OFF .and.  &
           ej .ne. OFF .and.  &
           ek .ne. OFF) then
          if(elem(ei) % state .eq. DONE .and.  &
             elem(ej) % state .eq. DONE .and.  &
             elem(ek) % state .eq. DONE) then
            elem(e) % state = DONE
          end if
        end if
      end if
    end if
  end do

  !-----------------------------------------!
  !   Diamond check. Is it so important ?   !
  !-----------------------------------------!
  call Mesh_Mod_Diamond(mesh)

  !---------------------------------------------------!
  !   First part of the trick:                        !
  !     search through the elements on the boundary   !
  !---------------------------------------------------!
  if(.not. done_boundary) then
    do e = 0, ne-1
      if(elem(e) % mark .ne. OFF .and.  &
         elem(e) % state .ne. DONE) then
        si = elem(e) % si
        sj = elem(e) % sj
        sk = elem(e) % sk

        if(side(si) % mark .ne. 0)  elem(e) % state = ACTIVE
        if(side(sj) % mark .ne. 0)  elem(e) % state = ACTIVE
        if(side(sk) % mark .ne. 0)  elem(e) % state = ACTIVE

        if(elem(e) % state .eq. ACTIVE .and.  &
           elem(e) % r_rat > max_ratio) then
          max_ratio = max(max_ratio,   elem(e) % r_rat)
          ugly = e
        end if
      end if
    end do
  end if

  if(ugly .eq. OFF) done_boundary = .true.

  !----------------------------------------------------!
  !   Second part of the trick:                        !
  !     if non-acceptable element on the boundary is   !
  !     found, ignore the elements inside the domain   !
  !----------------------------------------------------!
  if(done_boundary) then
    do e = 0, ne-1
      if(elem(e) % mark .ne. OFF) then
        if(elem(e) % state .ne. DONE) then
          ei = elem(e) % ei
          ej = elem(e) % ej
          ek = elem(e) % ek

          if(ei .ne. OFF) then
            if(elem(ei) % state .eq. DONE) then
              elem(e) % state = ACTIVE
            end if
          end if

          if(ej .ne. OFF) then
            if(elem(ej) % state .eq. DONE) then
              elem(e) % state = ACTIVE
            end if
          end if

          if(ek .ne. OFF) then
            if(elem(ek) % state .eq. DONE) then
              elem(e) % state = ACTIVE
            end if
          end if

          if(elem(e) % state .eq. ACTIVE .and.  &
             elem(e) % r_rat > max_ratio) then
            max_ratio = max(max_ratio,   elem(e) % r_rat)
            ugly = e
          end if
        end if
      end if
    end do
  end if

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Classify')

  end subroutine
