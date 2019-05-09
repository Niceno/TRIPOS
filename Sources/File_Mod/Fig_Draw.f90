!==============================================================================!
  subroutine File_Mod_Fig_Draw
!------------------------------------------------------------------------------!
!   Let's say that drawing area is 20 x 20 cm. One cm in xfig is 450 poins.    !
!   It means that drawing area is 9000 x 9000 points.                          !
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer             :: n, s, ea, eb
  real(RP)            :: xc, yc, xd, yd, xa, ya, xb, yb
  real(RP)            :: xmax, xmin, ymax, ymin, scl
  real(RP), parameter :: zero = 0.0
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Fig_Draw')

  xmax = -GREAT
  xmin = +GREAT
  ymax = -GREAT
  ymin = +GREAT
  do n = 0, n_node-1
    if(node(n) % mark .ne. OFF) then
      xmin = min(xmin, node(n) % x)
      ymin = min(ymin, node(n) % y)
      xmax = max(xmax, node(n) % x)
      ymax = max(ymax, node(n) % y)
    end if
  end do
  scl = min( 9000.0/(ymax-ymin+SMALL), 9000.0/(xmax-xmin+SMALL) )

  !-------------------!
  !   Draw boundary   !
  !-------------------!
  do s = 0, n_side-1
    if(side(s) % mark>0) then  ! it means, side is on the boundary */
      xc = node(side(s) % c) % x
      yc = node(side(s) % c) % y
      xd = node(side(s) % d) % x
      yd = node(side(s) % d) % y
      call File_Mod_Fig_Line (450+floor(scl*xc),  &
                              450+floor(scl*yc),  &
                              450+floor(scl*xd),  &
                              450+floor(scl*yd),  &
                              0, 3, 0, zero)
    end if
  end do

  !-------------------!
  !   Draw Delaunay   !
  !-------------------!
  do s = 0, n_side-1
    if(side(s) % mark==0) then  ! it means: side is in the domain */
      xc = node(side(s) % c) % x
      yc = node(side(s) % c) % y
      xd = node(side(s) % d) % x
      yd = node(side(s) % d) % y
      call File_Mod_Fig_Line(450+floor(scl*xc),  &
                             450+floor(scl*yc),  &
                             450+floor(scl*xd),  &
                             450+floor(scl*yd),  &
                             0, 1, 1, zero);
    end if
  end do

  !------------------!
  !   Draw Voronoi   !
  !------------------!
  do s = 0, n_side-1
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

      call File_Mod_Fig_Line(450+floor(scl*xa),  &
                             450+floor(scl*ya),  &
                             450+floor(scl*xb),  &
                             450+floor(scl*yb),  &
                             0, 1, 4, zero)
    end if
  end do

  call Cpu_Timer_Mod_Stop('File_Mod_Fig_Draw')

  end subroutine
