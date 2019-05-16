!==============================================================================!
  subroutine Mesh_Mod_Bowyer(mesh, n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  integer                 :: n
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e, s, swap
  type(Node_Type)          :: vor
  integer,         pointer :: ns
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_Bowyer')

  ! Take aliases
  ns   => mesh % n_side
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

  do
    swap = 0

    do s = 0, ns-1

      if(side(s) % mark .eq. 0) then
        if(side(s) % a .eq. n) then
          e = side(s) % eb
          if(e .ne. OFF) then
            vor % x = elem(e) % xv
            vor % y = elem(e) % yv
            if( Mesh_Mod_Dist(vor, node(n)) < elem(e) % r_ex ) then
              call Mesh_Mod_Swap_Side(mesh, s)
              swap = 1
            end if
          end if
        else if(side(s) % b .eq. n) then
          e = side(s) % ea
          if(e .ne. OFF) then
            vor % x = elem(e) % xv
            vor % y = elem(e) % yv
            if( Mesh_Mod_Dist(vor, node(n)) < elem(e) % r_ex ) then
              call Mesh_Mod_Swap_Side(mesh, s)
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
