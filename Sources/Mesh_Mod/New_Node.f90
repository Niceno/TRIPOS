!==============================================================================!
  subroutine Mesh_Mod_New_Node
!------------------------------------------------------------------------------+
!   This function is very important since it determines the position of the    !
!   newly inserted node.                                                       !
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer         :: s, n
  real(RP)        :: xm, ym, p, q, qx, qy, rhom, rho_m, d
  type(Node_Type) :: ca
!==============================================================================!

  call Cpu_Timer_Mod_Start('Mesh_Mod_New_Node')

  s = OFF

  !----------------------------------------------------------------------------!
  !   It's obvious that elements which are near the boundary, will come into   !
  !   play first.                                                              !
  !                                                                            !
  !   However, some attention has to be payed for the case when two accepted   !
  !   elements surround the ugly one                                           !
  !                                                                            !
  !   What if new points falls outside the domain                              !
  !----------------------------------------------------------------------------!
  ! V1.4c: if(elem(elem(ugly) % ei) % state .eq. DONE) then
  ! V1.4c:   s = elem(ugly) % si
  ! V1.4c:   n = elem(ugly) % i
  ! V1.4c: end if
  ! V1.4c: if(elem(elem(ugly) % ej) % state .eq. DONE) then
  ! V1.4c:   s = elem(ugly) % sj
  ! V1.4c:   n = elem(ugly) % j
  ! V1.4c: end if
  ! V1.4c: if(elem(elem(ugly) % ek) % state .eq. DONE) then
  ! V1.4c:   s = elem(ugly) % sk
  ! V1.4c:   n = elem(ugly) % k
  ! V1.4c: end if

  ! Version 1.5d introduced the additional checks around each element
  if(elem(ugly) % ei .ne. OFF) then
    if(elem(elem(ugly) % ei) % state .eq. DONE) then
      s = elem(ugly) % si
      n = elem(ugly) % i
    end if
  end if
  if(elem(ugly) % ej .ne. OFF) then
    if(elem(elem(ugly) % ej) % state .eq. DONE) then
      s = elem(ugly) % sj
      n = elem(ugly) % j
    end if
  end if
  if(elem(ugly) % ek .ne. OFF) then
    if(elem(elem(ugly) % ek) % state .eq. DONE) then
      s = elem(ugly) % sk
      n = elem(ugly) % k
    end if
  end if

  if(side(elem(ugly) % si) % mark .gt. 0) then
    s = elem(ugly) % si
    n = elem(ugly) % i
  end if
  if(side(elem(ugly) % sj) % mark .gt. 0) then
    s = elem(ugly) % sj
    n = elem(ugly) % j
  end if
  if(side(elem(ugly) % sk) % mark .gt. 0) then
    s = elem(ugly) % sk
    n = elem(ugly) % k
  end if

  if(s .eq. OFF) then
    call Cpu_Timer_Mod_Stop('Mesh_Mod_New_Node')
    return
  end if

  xm = 0.5 * (node(side(s) % c) % x + node(side(s) % d) % x)
  ym = 0.5 * (node(side(s) % c) % y + node(side(s) % d) % y)

  ca % x = elem(ugly) % xv
  ca % y = elem(ugly) % yv

  p = 0.5 * side(s) % s;    ! not checked

  qx = ca % x - xm
  qy = ca % y - ym
  q  = sqrt(qx**2 + qy**2)

  rhom = 0.577 * 0.5*(node(side(s) % c) % f + node(side(s) % d) % f)

  rho_m = min( max(rhom, p), 0.5*(p**2 + q**2) / q )

  if(rho_m < p) then
    d = rho_m
  else
    d = rho_m + sqrt(rho_m**2 - p**2)
  end if

  !-------------------------------------------------------------------------!
  !   The following line check can the new point fall outside the domain.   !
  !   However, I can't remember how it works, but I believe that it is      !
  !   still a weak point of the code, particulary when there are lines      !
  !   inside the domain                                                     !
  !-------------------------------------------------------------------------!
  if(  Mesh_Mod_Area(node(side(s) % c),  &
                     node(side(s) % d),  &
                     ca)                 &
     * Mesh_Mod_Area(node(side(s) % c),  &
                     node(side(s) % d),  &
                     node(n)) .gt. 0.0 ) then
    call Mesh_Mod_Insert_Node(xm + d*qx/q,  &
                              ym + d*qy/q,  &
                              ON,           &
                              OFF,          &
                              0,            &
                              0,            &
                              0,            &
                              OFF)
  end if

! else
!   node(n) % x = xm - d * qx/q
!   node(n) % y = ym - d * qy/q
!   node(n) % mark = 6
!   do e = 0, n_elem-1
!     if(elem(e) % i .eq. n || elem(e) % j .eq. n || elem(e) % k .eq. n) then
!       circles(e)
!     end if
!   end do
! end if

  call Cpu_Timer_Mod_Stop('Mesh_Mod_New_Node')

  end subroutine
