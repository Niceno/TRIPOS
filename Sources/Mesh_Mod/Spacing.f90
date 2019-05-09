!==============================================================================!
  subroutine Mesh_Mod_Spacing(e, n)
!------------------------------------------------------------------------------+
!   This function calculates the value of the spacing function in a new node   !
!   'n' which is inserted in element 'e' by a linear approximation from the    !
!   values of the spacing function in the elements nodes.                      !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: e, n
!-----------------------------------[Locals]-----------------------------------!
  real(RP) :: dxji, dxki, dyji, dyki, dx_i, dy_i, det, a, b
!==============================================================================!

  dxji = node(elem(e) % j) % x - node(elem(e) % i) % x
  dyji = node(elem(e) % j) % y - node(elem(e) % i) % y
  dxki = node(elem(e) % k) % x - node(elem(e) % i) % x
  dyki = node(elem(e) % k) % y - node(elem(e) % i) % y
  dx_i = node(n) % x - node(elem(e) % i) % x
  dy_i = node(n) % y - node(elem(e) % i) % y

  det = dxji*dyki - dxki*dyji

  a = (+ dyki*dx_i - dxki*dy_i) / det
  b = (- dyji*dx_i + dxji*dy_i) / det

  node(n) % f = node(elem(e) % i) % f                                &
              + a * (node(elem(e) % j) % f - node(elem(e) % i) % f)  &
              + b * (node(elem(e) % k) % f - node(elem(e) % i) % f)

  end subroutine
