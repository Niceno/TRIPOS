!==============================================================================!
  subroutine Mesh_Mod_Diamond
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: ea, eb, eac, ead, ebc, ebd, s
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Diamond')

  do s = 0, n_side-1

    if(side(s) % mark .ne. OFF) then
      ea = side(s) % ea;
      eb = side(s) % eb;

      ! If both are off, doesn't make sense to continue
      if(ea .ne. OFF .and. eb .ne. OFF) then

        ! Handle ea
        if(elem(ea) % ei .eq. eb) then
          ead = elem(ea) % ej
          eac = elem(ea) % ek
        end if
        if(elem(ea) % ej .eq. eb) then
          ead = elem(ea) % ek
          eac = elem(ea) % ei
        end if
        if(elem(ea) % ek .eq. eb) then
          ead = elem(ea) % ei
          eac = elem(ea) % ej
        end if

        ! Handle eb
        if(elem(eb) % ei .eq. ea) then
          ebc = elem(eb) % ej
          ebd = elem(eb) % ek
        end if
        if(elem(eb) % ej .eq. ea) then
          ebc = elem(eb) % ek
          ebd = elem(eb) % ei
        end if
        if(elem(eb) % ek .eq. ea) then
          ebc = elem(eb) % ei
          ebd = elem(eb) % ej
        end if

        if( (eac .eq. OFF .or. elem(eac) % state .eq. DONE)  .and.  &
            (ebc .eq. OFF .or. elem(ebc) % state .eq. DONE)  .and.  &
            (ead .eq. OFF .or. elem(ead) % state .eq. DONE)  .and.  &
            (ebd .eq. OFF .or. elem(ebd) % state .eq. DONE) ) then
          elem(ea) % state = DONE
          elem(eb) % state = DONE
        end if

      end if  ! ea and eb are not off

    end if  ! side is not off

  end do

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Diamond')

  end subroutine
