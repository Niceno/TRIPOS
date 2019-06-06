!==============================================================================!
  subroutine File_Mod_Gradients(mesh, phi, phi_x, phi_y)
!------------------------------------------------------------------------------!
!   Computes gradients of dependent variable.                                  !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Vector_Mod,    only: Vector_Type
  use Mesh_Mod,      only: Mesh_Type
  use Cpu_Timer_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type),   target :: mesh
  type(Vector_Type), target :: phi
  real(RP)                  :: phi_x(0:mesh % n_elem-1),  &
                               phi_y(0:mesh % n_elem-1)
!-----------------------------------[Locals]-----------------------------------!
  integer  :: i, j, k, e
  real(RP) :: xi, yi, xj, yj, xk, yk
  real(RP) :: det_inv, mat(3,3), mat_inv(3,3)
  real(RP) :: a, b, c
!==============================================================================!

  call Cpu_Timer_Mod_Start('Gradients')

  do e = 0, mesh % n_elem - 1

    ! Extract element nodes
    i = mesh % elem(e) % i
    j = mesh % elem(e) % j
    k = mesh % elem(e) % k

    xi = mesh % node(i) % x
    xj = mesh % node(j) % x
    xk = mesh % node(k) % x

    yi = mesh % node(i) % y
    yj = mesh % node(j) % y
    yk = mesh % node(k) % y

    ! Find the matrix and its inverse
    mat(1,1) = 1.0;  mat(1,2) = xi;  mat(1,3) = yi
    mat(2,1) = 1.0;  mat(2,2) = xj;  mat(2,3) = yj
    mat(3,1) = 1.0;  mat(3,2) = xk;  mat(3,3) = yk

    ! Calculate the inverse determinant of the matrix
    det_inv = 1.0 / (mat(1,1)*mat(2,2)*mat(3,3) - mat(1,1)*mat(2,3)*mat(3,2)  &
                   - mat(1,2)*mat(2,1)*mat(3,3) + mat(1,2)*mat(2,3)*mat(3,1)  &
                   + mat(1,3)*mat(2,1)*mat(3,2) - mat(1,3)*mat(2,2)*mat(3,1))

    ! Calculate the inverse of the matrix
    mat_inv(1,1) = +det_inv * (mat(2,2)*mat(3,3) - mat(2,3)*mat(3,2))
    mat_inv(2,1) = -det_inv * (mat(2,1)*mat(3,3) - mat(2,3)*mat(3,1))
    mat_inv(3,1) = +det_inv * (mat(2,1)*mat(3,2) - mat(2,2)*mat(3,1))
    mat_inv(1,2) = -det_inv * (mat(1,2)*mat(3,3) - mat(1,3)*mat(3,2))
    mat_inv(2,2) = +det_inv * (mat(1,1)*mat(3,3) - mat(1,3)*mat(3,1))
    mat_inv(3,2) = -det_inv * (mat(1,1)*mat(3,2) - mat(1,2)*mat(3,1))
    mat_inv(1,3) = +det_inv * (mat(1,2)*mat(2,3) - mat(1,3)*mat(2,2))
    mat_inv(2,3) = -det_inv * (mat(1,1)*mat(2,3) - mat(1,3)*mat(2,1))
    mat_inv(3,3) = +det_inv * (mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1))

    ! Find coeffiecients a, b and c
    a = mat_inv(1,1) * phi % val(i)  &
      + mat_inv(1,2) * phi % val(j)  &
      + mat_inv(1,3) * phi % val(k)

    b = mat_inv(2,1) * phi % val(i)  &
      + mat_inv(2,2) * phi % val(j)  &
      + mat_inv(2,3) * phi % val(k)

    c = mat_inv(3,1) * phi % val(i)  &
      + mat_inv(3,2) * phi % val(j)  &
      + mat_inv(3,3) * phi % val(k)

    phi_x(e) = b
    phi_y(e) = c
  end do

  call Cpu_Timer_Mod_Stop('Gradients')

  end subroutine
