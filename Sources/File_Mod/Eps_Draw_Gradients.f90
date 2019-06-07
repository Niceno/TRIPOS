!==============================================================================!
  subroutine File_Mod_Eps_Draw_Gradients(phi, comm)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Vector_Type), target :: phi
  type(Comm_Type)           :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: n, e, s, l
  real(RP)                 :: xc, yc, xd, yd, xv, yv, dx, dy
  real(RP)                 :: max_mag, rad, mag
  real(RP)                 :: r, g, b
  integer,         pointer :: ne, ns, nn
  type(Mesh_Type), pointer :: mesh
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
  real(RP), allocatable    :: phi_x(:), phi_y(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Eps_Draw_Gradients')

  ! Take aliases
  mesh => phi % pnt_mesh
  nn   => mesh % n_node
  ne   => mesh % n_elem
  ns   => mesh % n_side
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

  ! Compute gradients
  allocate(phi_x(0:ne-1))  ! dphi / dx
  allocate(phi_y(0:ne-1))  ! dphi / dy
  call File_Mod_Gradients(mesh, phi, phi_x(0:ne-1), phi_y(0:ne-1))

  max_mag = 0.0
  do e = 0, ne-1
    max_mag = max(max_mag, sqrt(phi_x(e)**2 + phi_y(e)**2))
  end do

  !-------------------!
  !   Draw boundary   !
  !-------------------!
  do s = 0, ns-1
    if(side(s) % mark .gt. 0) then  ! it means, side is on the boundary
      xc = node(side(s) % c) % x * scl
      yc = node(side(s) % c) % y * scl
      xd = node(side(s) % d) % x * scl
      yd = node(side(s) % d) % y * scl
      r = 0.3
      g = 0.3
      b = 0.3
      call File_Mod_Eps_Line(1, xc, yc, xd, yd, r, g, b)
    end if
  end do

  ! Find mesh size on the boundary
  rad = -GREAT
  do n = 0, nn-1
    if(node(n) % mark .gt. 0) then  ! it means, node is on the boundary
      rad = max(rad, node(n) % f)
    end if
  end do
  rad = rad * scl * 0.166667

  !------------------!
  !   Draw vectors   !
  !------------------!
  do e = 0, ne-1
    xv = elem(e) % xv
    yv = elem(e) % yv
    mag = sqrt(phi_x(e)**2 + phi_y(e)**2)
    l = ceiling(17 * mag / max_mag) - 1
    call File_Mod_Get_Rainbow_Color(l, r, g, b)

    dx = +rad * phi_y(e) / mag
    dy = -rad * phi_x(e) / mag

    if(comm % head .eq. ON) then
      call File_Mod_Eps_Line(0, xv*scl-dx,              &
                                yv*scl-dy,              &
                                xv*scl+0.5*dx,          &
                                yv*scl+0.5*dy,             r, g, b)
      call File_Mod_Eps_Line(0, xv*scl+0.5*dx,          &
                                yv*scl+0.5*dy,          &
                                xv*scl+0.5*dx+0.25*dy,  &
                                yv*scl+0.5*dy-0.25*dx,     r, g, b)
      call File_Mod_Eps_Line(0, xv*scl+0.5*dx,          &
                                yv*scl+0.5*dy,          &
                                xv*scl+0.5*dx-0.25*dy,  &
                                yv*scl+0.5*dy+0.25*dx,     r, g, b)
      call File_Mod_Eps_Line(0, xv*scl+dx,              &
                                yv*scl+dy,              &
                                xv*scl+0.5*dx+0.25*dy,  &
                                yv*scl+0.5*dy-0.25*dx,     r, g, b)
      call File_Mod_Eps_Line(0, xv*scl+dx,              &
                                yv*scl+dy,              &
                                xv*scl+0.5*dx-0.25*dy,  &
                                yv*scl+0.5*dy+0.25*dx,     r, g, b)
    else
      call File_Mod_Eps_Line(0, xv*scl-dx,              &
                                yv*scl-dy,              &
                                xv*scl+dx,              &
                                yv*scl+dy,                 r, g, b)
    end if
  end do

  call Cpu_Timer_Mod_Stop('File_Mod_Eps_Draw_Gradients')

  end subroutine
