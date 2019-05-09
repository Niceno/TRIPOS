!==============================================================================!
  subroutine Mesh_Mod_Bowyer(n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: n
!-----------------------------------[Locals]-----------------------------------!
  integer         :: e, s, swap
  type(Node_Type) :: vor
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Bowyer')

  do
    swap = 0

    do s = 0, n_side-1

      if(side(s) % mark .eq. 0) then
        if(side(s) % a .eq. n) then
          e = side(s) % eb
          if(e .ne. OFF) then
            vor % x = elem(e) % xv
            vor % y = elem(e) % yv
            if( Mesh_Mod_Dist(vor, node(n)) < elem(e) % r_out ) then
              call Mesh_Mod_Swap_Side(s)
              swap = 1
            end if
          end if
        else if(side(s) % b .eq. n) then
          e = side(s) % ea
          if(e .ne. OFF) then
            vor % x = elem(e) % xv
            vor % y = elem(e) % yv
            if( Mesh_Mod_Dist(vor, node(n)) < elem(e) % r_out ) then
              call Mesh_Mod_Swap_Side(s)
              swap = 1
            end if
          end if
        end if
      end if

    end do

    ! If no more swappings done, get out
    if(swap .eq. 0) exit

  end do

  call Cpu_Timer_Mod_Stop('Mesh_Mod_Bowyer')

  end subroutine
