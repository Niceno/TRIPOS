!==============================================================================!
  subroutine File_Mod_Eps_Draw_Mesh(mesh, delaunay, voronoi)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  integer                 :: delaunay, voronoi
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: n, e, s, ea, eb
  real(RP)                 :: xc, yc, xd, yd, xa, ya, xb, yb
  real(RP)                 :: xi, yi, xj, yj, xk, yk, xn, yn, rad
  real(RP)                 :: r, g, b
  integer,         pointer :: ne, ns, nn
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Eps_Draw_Mesh')

  ! Take aliases
  ne   => mesh % n_elem
  ns   => mesh % n_side
  nn   => mesh % n_node
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

  xmax = -GREAT
  xmin = +GREAT
  ymax = -GREAT
  ymin = +GREAT
  do n = 0, nn-1
    if(node(n) % mark .ne. OFF) then
      xmin = min(xmin, node(n) % x)
      ymin = min(ymin, node(n) % y)
      xmax = max(xmax, node(n) % x)
      ymax = max(ymax, node(n) % y)
    end if
  end do
  scl = min(1120.0/(ymax-ymin+SMALL), 800.0/(xmax-xmin+SMALL) )

  !-------------------!
  !   Draw elements   !
  !-------------------!
  do e = 0, ne-1
    if(elem(e) % mark .ne. OFF) then  ! it means: side is in the domain */
      xi = node(elem(e) % i) % x * scl
      yi = node(elem(e) % i) % y * scl
      xj = node(elem(e) % j) % x * scl
      yj = node(elem(e) % j) % y * scl
      xk = node(elem(e) % k) % x * scl
      yk = node(elem(e) % k) % y * scl
      call File_Mod_Get_Cool_Color(elem(e) % material, r, g, b)
      call File_Mod_Eps_Solid(xi, yi, xj, yj, xk, yk, r, g, b)
    end if
  end do

  !------------------!
  !   Draw Voronoi   !
  !------------------!
  if(voronoi .eq. ON) then
    do s = 0, ns-1
      if(side(s) % mark .ne. OFF) then

        ea = side(s) % ea
        if(ea .ne. OFF) then
          xa = elem(ea) % xv * scl
          ya = elem(ea) % yv * scl
        else
          xa = 0.5 * (node(side(s) % c) % x + node(side(s) % d) % x) * scl
          ya = 0.5 * (node(side(s) % c) % y + node(side(s) % d) % y) * scl
        end if

        eb = side(s) % eb
        if(eb .ne. OFF) then
          xb = elem(eb) % xv * scl
          yb = elem(eb) % yv * scl
        else
          xb = 0.5*(node(side(s) % c) % x + node(side(s) % d) % x) * scl
          yb = 0.5*(node(side(s) % c) % y + node(side(s) % d) % y) * scl
        end if

        r = 1.0
        g = 0.0
        b = 1.0
        call File_Mod_Eps_Line(0, xa, ya, xb, yb, r, g, b)  ! magenta

      end if
    end do
  end if

  !-------------------!
  !   Draw Delaunay   !
  !-------------------!
  if(delaunay .eq. ON) then
    do s = 0, ns-1
      if(side(s) % mark .eq. 0) then  ! it means: side is in the domain */
        xc = node(side(s) % c) % x * scl
        yc = node(side(s) % c) % y * scl
        xd = node(side(s) % d) % x * scl
        yd = node(side(s) % d) % y * scl
        r = 0.0
        g = 0.0
        b = 1.0
        call File_Mod_Eps_Line(0, xc, yc, xd, yd, r, g, b)  ! blue
      end if
    end do
  end if

  !-------------------!
  !   Draw boundary   !
  !-------------------!
  do s = 0, ns-1
    if(side(s) % mark .gt. 0) then  ! it means, side is on the boundary
      xc = node(side(s) % c) % x * scl
      yc = node(side(s) % c) % y * scl
      xd = node(side(s) % d) % x * scl
      yd = node(side(s) % d) % y * scl
      call File_Mod_Get_Warm_Color(side(s) % mark, r, g, b)
      call File_Mod_Eps_Line(1, xc, yc, xd, yd, r, g, b)
    end if
  end do

  !-------------------------!
  !   Draw boundary nodes   !
  !-------------------------!

  ! Find mesh size on the boundary
  rad = GREAT
  do n = 0, nn-1
    if(node(n) % mark .gt. 0) then  ! it means, node is on the boundary
      rad = min(rad, node(n) % f)
    end if
  end do

  rad = rad * scl

  do n = 0, nn-1
    if(node(n) % mark .gt. 0) then  ! it means, node is on the boundary
      xn  = node(n) % x * scl
      yn  = node(n) % y * scl
      call File_Mod_Get_Warm_Color(node(n) % mark, r, g, b)
      call File_Mod_Eps_Circle(0, xn, yn, rad*0.4, r, g, b)
      call File_Mod_Eps_Circle(0, xn, yn, rad*0.3, r, g, b)
      call File_Mod_Eps_Circle(0, xn, yn, rad*0.2, r, g, b)
      call File_Mod_Eps_Circle(0, xn, yn, rad*0.1, r, g, b)
    end if
  end do

  call Cpu_Timer_Mod_Stop('File_Mod_Eps_Draw_Mesh')

  end subroutine
