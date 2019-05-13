!==============================================================================!
  subroutine File_Mod_Dxf_Draw
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer             :: s, ea, eb
  real(RP)            :: xc, yc, xd, yd, xa, ya, xb, yb
  real(RP), parameter :: zero = 0.0
  character(len=CL)   :: bnd_layer
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Dxf_Draw')

  !-------------------!
  !   Draw boundary   !
  !-------------------!
  do s = 0, n_side-1
    if(side(s) % mark .gt. 0) then  ! it means, side is on the boundary */
      xc = node(side(s) % c) % x
      yc = node(side(s) % c) % y
      xd = node(side(s) % d) % x
      yd = node(side(s) % d) % y
      bnd_layer(1:11) = "boundary-00"
      write(bnd_layer(10:11), "(i2.2)") side(s) % mark
      call File_Mod_Dxf_Line(xc, yc, zero, xd, yd, zero, bnd_layer(1:11))
    end if
  end do

  !-------------------!
  !   Draw Delaunay   !
  !-------------------!
  do s = 0, n_side-1
    if(side(s) % mark .eq. 0) then  ! it means: side is in the domain */
      xc = node(side(s) % c) % x
      yc = node(side(s) % c) % y
      xd = node(side(s) % d) % x
      yd = node(side(s) % d) % y
      call File_Mod_Dxf_Line(xc, yc, zero, xd, yd, zero, "delaunay")
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

      call File_Mod_Dxf_Line(xa, ya, zero, xb, yb, zero, "voronoi")

    end if
  end do

  call Cpu_Timer_Mod_Stop('File_Mod_Dxf_Draw')

  end subroutine
