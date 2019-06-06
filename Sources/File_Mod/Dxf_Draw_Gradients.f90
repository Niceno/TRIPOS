!==============================================================================!
  subroutine File_Mod_Dxf_Draw_Gradients(phi, comm)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Vector_Type), target :: phi
  type(Comm_Type)           :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e, s, l, n
  real(RP)                 :: xc, yc, xd, yd
  real(RP)                 :: xv, yv, dx, dy
  real(RP)                 :: val, max_mag, del_mag, mag, rad
  character(len=CL)        :: il
  integer,         pointer :: nn, ne, ns
  type(Mesh_Type), pointer :: mesh
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
  real(RP), allocatable    :: phi_x(:), phi_y(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Dxf_Draw_Gradients')

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
  del_mag = max_mag / N_ISOLINE

  !------------------------!
  !   Draw Delaunay mesh   !
  !------------------------!
  do s = 0, ns-1
    if(side(s) % mark .eq. 0) then  ! it means: side is in the domain */
      xc = node(side(s) % c) % x
      yc = node(side(s) % c) % y
      xd = node(side(s) % d) % x
      yd = node(side(s) % d) % y
      call File_Mod_Dxf_Line(xc, yc, xd, yd, "mesh")
    end if
  end do

  !-------------------!
  !   Draw boundary   !
  !-------------------!
  do s = 0, ns-1
    if(side(s) % mark .gt. 0) then  ! it means, side is on the boundary */
      xc = node(side(s) % c) % x
      yc = node(side(s) % c) % y
      xd = node(side(s) % d) % x
      yd = node(side(s) % d) % y
      call File_Mod_Dxf_Line(xc, yc, xd, yd, "boundary")
    end if
  end do

  ! Find mesh size on the boundary
  rad = -GREAT
  do n = 0, nn-1
    if(node(n) % mark .gt. 0) then  ! it means, node is on the boundary
      rad = max(rad, node(n) % f)
    end if
  end do
  rad = rad * 0.166667

  !------------------!
  !   Draw vectors   !
  !------------------!
  do e = 0, ne-1
    xv = elem(e) % xv
    yv = elem(e) % yv
    mag = sqrt(phi_x(e)**2 + phi_y(e)**2)
    l   = ceiling(17 * mag / max_mag) - 1
    val = del_mag * 0.5 + del_mag * l

    ! Name layer
    call File_Mod_Name_Layer(il, l, val)

    dx = +rad * phi_y(e) / mag
    dy = -rad * phi_x(e) / mag

    ! Draw line
    if(comm % head .eq. ON) then
      call File_Mod_Dxf_Line(xv-dx,              &
                             yv-dy,              &
                             xv+0.5*dx,          &
                             yv+0.5*dy,             il(1:13))
      call File_Mod_Dxf_Line(xv+0.5*dx,          &
                             yv+0.5*dy,          &
                             xv+0.5*dx+0.25*dy,  &
                             yv+0.5*dy-0.25*dx,     il(1:13))
      call File_Mod_Dxf_Line(xv+0.5*dx,          &
                             yv+0.5*dy,          &
                             xv+0.5*dx-0.25*dy,  &
                             yv+0.5*dy+0.25*dx,     il(1:13))
      call File_Mod_Dxf_Line(xv+dx,              &
                             yv+dy,              &
                             xv+0.5*dx+0.25*dy,  &
                             yv+0.5*dy-0.25*dx,     il(1:13))
      call File_Mod_Dxf_Line(xv+dx,              &
                             yv+dy,              &
                             xv+0.5*dx-0.25*dy,  &
                             yv+0.5*dy+0.25*dx,     il(1:13))
    else
      call File_Mod_Dxf_Line(xv-dx,              &
                             yv-dy,              &
                             xv+dx,              &
                             yv+dy,                 il(1:13))
    end if
  end do

  call Cpu_Timer_Mod_Stop('File_Mod_Dxf_Draw_Gradients')

  end subroutine
