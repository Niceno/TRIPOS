!==============================================================================!
  subroutine Mesh_Mod_Materials(mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e, c, mater, over
  integer                  :: ei, ej, ek, si, sj, sk
  integer,         pointer :: ne, np
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
  type(Node_Type), pointer :: point(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Materials')

  ! Take aliases
  ne    => mesh % n_elem
  np    => mesh % n_point
  elem  => mesh % elem
  side  => mesh % side
  point => mesh % point

  do e = 0, ne-1
    if(elem(e) % mark .ne. OFF) then
      elem(e) % material = OFF
    end if
  end do

  !----------------------------------------!
  !   Assume materials are not specified   !
  !----------------------------------------!
  mater = OFF

  !-------------------------------------------------------------------!
  !   Browse throuhg all points, and pick those which have not been   !
  !   inserted, meaning which do not belong to boundary chains        !
  !-------------------------------------------------------------------!
  do c = 0, np-1
   if(point(c) % inserted .eq. 0) then  ! =--> this is BAD, ghost number

     ! Store material marker ...
     elem(Mesh_Mod_In_Elem(mesh, point(c))) % material = point(c) % mark

     ! ... and tell program that material markers are specified
     mater = ON
   end if
  end do

  !-------------------------------------------------!
  !   Material markers are specified; spread them   !
  !   over using a kind of a flood-fill algorithm   !
  !-------------------------------------------------!
  if(mater .eq. ON) then
    do
      over = ON

      do e = 0, ne-1
        if(elem(e) % mark .ne. OFF .and. elem(e) % material .eq. OFF) then
          ei = elem(e) % ei
          ej = elem(e) % ej
          ek = elem(e) % ek

          si = elem(e) % si
          sj = elem(e) % sj
          sk = elem(e) % sk

          if(ei .ne. OFF) then
            if(elem(ei) % material .ne. OFF .and.  &
               side(si) % mark .eq. 0) then           ! =--> BAD, ghost number
              elem(e) % material=elem(ei) % material
              over=OFF;
            end if
          end if

          if(ej .ne. OFF) then
            if(elem(ej) % material .ne. OFF .and.  &
               side(sj) % mark .eq. 0) then           ! =--> BAD, ghost number
             elem(e) % material=elem(ej) % material
             over=OFF
            end if
          end if

          if(ek .ne. OFF) then
            if(elem(ek) % material .ne. OFF .and.  &
               side(sk) % mark .eq. 0) then           ! =--> BAD, ghost number
              elem(e) % material=elem(ek) % material
              over=OFF
            end if
          end if

        end if
      end do

      if(over .eq. ON) exit

    end do

  !-----------------------------------------------------------!
  !   Material markers are not specified, set them all to 1   !
  !-----------------------------------------------------------!
  else
    do e = 0, ne-1
      if(elem(e) % mark .ne. OFF) then
        elem(e) % material = 1
      end if
    end do
  end if  ! mater .eq. ON

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Materials')

  end subroutine
