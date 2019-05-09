!==============================================================================!
  subroutine Mesh_Mod_Circles(e)
!*-----------------------------------------------------------------------------!
!   This function calculates radii of inscribed and circumscribed circle       !
!   for a given element (int e)                                                !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: e
!-----------------------------------[Locals]-----------------------------------!
  real(RP) :: x, y, xi, yi, xj, yj, xk, yk, xij, yij, xjk, yjk, num, den
  real(RP) :: si, sj, sk, o
!==============================================================================!

  xi = node(elem(e) % i) % x;  yi = node(elem(e) % i) % y
  xj = node(elem(e) % j) % x;  yj = node(elem(e) % j) % y
  xk = node(elem(e) % k) % x;  yk = node(elem(e) % k) % y

  xij = 0.5*(xi+xj);  yij = 0.5*(yi+yj)
  xjk = 0.5*(xj+xk);  yjk = 0.5*(yj+yk)

  num = (xij-xjk)*(xj-xi) + (yij-yjk)*(yj-yi)
  den = (xj -xi) *(yk-yj) - (xk -xj) *(yj-yi)

  if(den > 0) then
    x = xjk + num / den * (yk-yj)
    y = yjk - num / den * (xk-xj)
    elem(e) % xv = x
    elem(e) % yv = y

    elem(e) % r_out = sqrt( (xi-x)**2 + (yi-y)**2 )
  end if

  si = side(elem(e) % si) % s
  sj = side(elem(e) % sj) % s
  sk = side(elem(e) % sk) % s
  o  = si + sj + sk
  elem(e) % det = xi*(yj-yk) - xj*(yi-yk) + xk*(yi-yj)

  elem(e) % xin = (xi*si + xj*sj + xk*sk) / o
  elem(e) % yin = (yi*si + yj*sj + yk*sk) / o

  elem(e) % r_in = elem(e) % det / o

  end subroutine
