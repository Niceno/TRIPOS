!==============================================================================!
  subroutine File_Mod_Eps_Draw_Results(solution)
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
  real(RP)                 :: r, g, b
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
      xc = node(side(s) % c) % x * scl
      yc = node(side(s) % c) % y * scl
      xd = node(side(s) % d) % x * scl
      yd = node(side(s) % d) % y * scl
      r = 0.7
      g = 0.7
      b = 0.7
      call File_Mod_Eps_Line(0, xc, yc, xd, yd, r, g, b)  ! blue
    end if
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

  !-------------------!
  !   Draw solution   !
  !-------------------!
  do l = 0, N_ISOLINE - 1
    val = (min_val+SMALL) + l * ((max_val-min_val)-SMALL)   &
        / (N_ISOLINE-1)
    do e = 0, ne-1
      i  = elem(e) % i
      j  = elem(e) % j
      k  = elem(e) % k

      xi = node(i) % x * scl
      yi = node(i) % y * scl
      xj = node(j) % x * scl
      yj = node(j) % y * scl
      xk = node(k) % x * scl
      yk = node(k) % y * scl

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
        call File_Mod_Get_Rainbow_Color(l, r, g, b)
        call File_Mod_Eps_Line(2, x(1), y(1), x(2), y(2), r, g, b)
      end if
    end do
  end do

  call Cpu_Timer_Mod_Stop('File_Mod_Dxf_Draw_Solution')

  end subroutine
