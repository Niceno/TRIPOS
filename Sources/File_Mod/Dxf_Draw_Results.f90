!==============================================================================!
  subroutine File_Mod_Dxf_Draw_Results(solution)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Vector_Type), target :: solution
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e, s, i, j, k, n_hits, l
  real(RP)                 :: xc, yc, xd, yd
  real(RP)                 :: xi, yi, xj, yj, xk, yk, vi, vj, vk
  real(RP)                 :: x(3), y(3)
  real(RP)                 :: val, max_val, min_val
  character(len=CL)        :: iso_layer
  integer,         pointer :: ne, ns
  type(Mesh_Type), pointer :: mesh
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  call Cpu_Timer_Mod_Start('File_Mod_Dxf_Draw_Solution')

  ! Take aliases
  mesh => solution % pnt_mesh
  ne   => mesh % n_elem
  ns   => mesh % n_side
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

  max_val = maxval(solution % val(:))
  min_val = minval(solution % val(:))

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

  !-------------------!
  !   Draw solution   !
  !-------------------!
  iso_layer(1:13) = "00:0.000E+000"
  do l = 0, N_ISOLINE - 1
    val = (min_val+SMALL) + l * ((max_val-min_val)-SMALL)   &
        / (N_ISOLINE-1)
    write(iso_layer(1: 2), "(i2.2)")     l+1
    write(iso_layer(4:13), "(es10.3e3)") val
    do e = 0, ne-1
      i  = elem(e) % i
      j  = elem(e) % j
      k  = elem(e) % k

      xi = node(i) % x
      yi = node(i) % y
      xj = node(j) % x
      yj = node(j) % y
      xk = node(k) % x
      yk = node(k) % y

      vi = solution % val(i)
      vj = solution % val(j)
      vk = solution % val(k)

      n_hits = 0

      ! Check i and j
      if(vi .ne. vj) then
        if( (val .ge. vi) .and. (val .le. vj)  .or.  &
            (val .le. vi) .and. (val .ge. vj) ) then
          n_hits = n_hits + 1
          x(n_hits) = xi * abs(val - vj) + xj * abs(val - vi)
          x(n_hits) = x(n_hits) / abs(vi - vj)
          y(n_hits) = yi * abs(val - vj) + yj * abs(val - vi)
          y(n_hits) = y(n_hits) / abs(vi - vj)
        end if
      end if

      ! Check i and j
      if(vi .ne. vk) then
        if( (val .ge. vi) .and. (val .le. vk)  .or.  &
            (val .le. vi) .and. (val .ge. vk) ) then
          n_hits = n_hits + 1
          x(n_hits) = xi * abs(val - vk) + xk * abs(val - vi)
          x(n_hits) = x(n_hits) / abs(vi - vk)
          y(n_hits) = yi * abs(val - vk) + yk * abs(val - vi)
          y(n_hits) = y(n_hits) / abs(vi - vk)
        end if
      end if

      ! Check j and k
      if(vj .ne. vk) then
        if( (val .ge. vj) .and. (val .le. vk)  .or.  &
            (val .le. vj) .and. (val .ge. vk) ) then
          n_hits = n_hits + 1
          x(n_hits) = xj * abs(val - vk) + xk * abs(val - vj)
          x(n_hits) = x(n_hits) / abs(vj - vk)
          y(n_hits) = yj * abs(val - vk) + yk * abs(val - vj)
          y(n_hits) = y(n_hits) / abs(vj - vk)
        end if
      end if

      if(n_hits .eq. 2) then
        call File_Mod_Dxf_Line(x(1), y(1), x(2), y(2), iso_layer(1:13))
      end if
    end do
  end do

  call Cpu_Timer_Mod_Stop('File_Mod_Dxf_Draw_Solution')

  end subroutine
