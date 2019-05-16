!==============================================================================!
  integer function Mesh_Mod_In_Elem(mesh, n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  type(Node_Type)         :: n
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e
  integer,         pointer :: ne
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_In_Elem')

  ! Take aliases
  ne   => mesh % n_elem
  node => mesh % node
  elem => mesh % elem

  do e = 0, ne-1  ! does it really have to search through all elements?
    if(   Mesh_Mod_Area(n, node(elem(e) % i), node(elem(e) % j)) >= 0.0    &
    .and. Mesh_Mod_Area(n, node(elem(e) % j), node(elem(e) % k)) >= 0.0    &
    .and. Mesh_Mod_Area(n, node(elem(e) % k), node(elem(e) % i)) >= 0.0 ) then
      Mesh_Mod_In_Elem = e
      return
    end if
  end do

  call Cpu_Timer_Mod_Stop('Mesh_Mod_In_Elem')

  end function
