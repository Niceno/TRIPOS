!==============================================================================!
  subroutine File_Mod_Dxf_Draw_Mesh(mesh, delaunay, voronoi)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  integer                 :: delaunay, voronoi
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: n, e, s, ea, eb
  real(RP)                 :: xc, yc, xd, yd, xa, ya, xb, yb
  real(RP)                 :: xi, yi, xj, yj, xk, yk, xn, yn, rad
  character(len=CL)        :: bnd_layer, mat_layer
  integer,         pointer :: ne, ns, nn
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Dxf_Draw_Mesh')

  ! Take aliases
  ne   => mesh % n_elem
  ns   => mesh % n_side
  nn   => mesh % n_node
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

  !-------------------!
  !   Draw elements   !
  !-------------------!
  do e = 0, ne-1
    if(elem(e) % mark .ne. OFF) then  ! it means: side is in the domain */
      xi = node(elem(e) % i) % x
      yi = node(elem(e) % i) % y
      xj = node(elem(e) % j) % x
      yj = node(elem(e) % j) % y
      xk = node(elem(e) % k) % x
      yk = node(elem(e) % k) % y
      mat_layer(1:11) = "material-00"
      write(mat_layer(10:11), "(i2.2)") elem(e) % material
      call File_Mod_Dxf_Solid(xi, yi, xj, yj, xk, yk, mat_layer(1:11))
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
      bnd_layer(1:11) = "boundary-00"
      write(bnd_layer(10:11), "(i2.2)") side(s) % mark
      call File_Mod_Dxf_Line(xc, yc, xd, yd, bnd_layer(1:11))
    end if
  end do

  !-------------------!
  !   Draw Delaunay   !
  !-------------------!
  if(delaunay .eq. ON) then
    do s = 0, ns-1
      if(side(s) % mark .eq. 0) then  ! it means: side is in the domain */
        xc = node(side(s) % c) % x
        yc = node(side(s) % c) % y
        xd = node(side(s) % d) % x
        yd = node(side(s) % d) % y
        call File_Mod_Dxf_Line(xc, yc, xd, yd, "delaunay")
      end if
    end do
  end if

  !------------------!
  !   Draw Voronoi   !
  !------------------!
  if(voronoi .eq. ON) then
    do s = 0, ns-1
      if(side(s) % mark .ne. OFF) then

        ea = side(s) % ea
        if(ea .ne. OFF) then
          xa = elem(ea) % xv
          ya = elem(ea) % yv
        else
          xa = 0.5 * (node(side(s) % c) % x + node(side(s) % d) % x)
          ya = 0.5 * (node(side(s) % c) % y + node(side(s) % d) % y)
        end if

        eb = side(s) % eb
        if(eb .ne. OFF) then
          xb = elem(eb) % xv
          yb = elem(eb) % yv
        else
          xb = 0.5*(node(side(s) % c) % x + node(side(s) % d) % x)
          yb = 0.5*(node(side(s) % c) % y + node(side(s) % d) % y)
        end if

        call File_Mod_Dxf_Line(xa, ya, xb, yb, "voronoi")

      end if
    end do
  end if

  !-------------------------!
  !   Draw boundary nodes   !
  !-------------------------!

  ! Find mesh size on the boundary
  rad = GREAT
  do n = 0, nn-1
    if(node(n) % mark .gt. 0) then  ! it means, node is on the boundary */
      rad = min(rad, node(n) % f)
    end if
  end do

  do n = 0, nn-1
    if(node(n) % mark .gt. 0) then  ! it means, node is on the boundary */
      xn  = node(n) % x
      yn  = node(n) % y
      write(bnd_layer(10:11), "(i2.2)") node(n) % mark
      call File_Mod_Dxf_Circle(xn, yn, rad*0.4, bnd_layer(1:11))
      call File_Mod_Dxf_Circle(xn, yn, rad*0.3, bnd_layer(1:11))
      call File_Mod_Dxf_Circle(xn, yn, rad*0.2, bnd_layer(1:11))
      call File_Mod_Dxf_Circle(xn, yn, rad*0.1, bnd_layer(1:11))
    end if
  end do

  call Cpu_Timer_Mod_Stop('File_Mod_Dxf_Draw_Mesh')

  end subroutine
