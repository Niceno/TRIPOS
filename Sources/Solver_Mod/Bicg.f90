!==============================================================================!
  subroutine Solver_Mod_Bicg(sol, a, x, b, miter, niter, tol, res)
!------------------------------------------------------------------------------!
!   Solves the linear systems of equations by a precond. BiCG Method.          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Solver_Type), target :: sol
  type(Matrix_Type), target :: a
  type(Vector_Type), target :: x
  type(Vector_Type), target :: b
  integer                   :: miter             ! max and actual ...
  integer                   :: niter             ! ... num. iterations
  real(RP)                  :: tol               ! tolerance
  real(RP)                  :: res               ! residual
!-----------------------------------[Locals]-----------------------------------!
  type(Matrix_Type), pointer :: d
  type(Vector_Type), pointer :: p1
  type(Vector_Type), pointer :: p2
  type(Vector_Type), pointer :: q1
  type(Vector_Type), pointer :: q2
  type(Vector_Type), pointer :: r1
  type(Vector_Type), pointer :: r2
  integer                    :: ni
  real(RP)                   :: alfa, beta, rho, rho_old, bnrm2
  integer                    :: i, j, k, iter
!==============================================================================!

  call Cpu_Timer_Mod_Start('Solver_Mod_Bicg')

  ! Take some aliases
  d  => sol % d
  p1 => sol % p1
  p2 => sol % p2
  q1 => sol % q1
  q2 => sol % q2
  r1 => sol % r1
  r2 => sol % r2
  ni =  sol % pnt_mesh % n_node

  res = 0.0

  !---------------------!
  !   Preconditioning   !
  !---------------------!
  call Prec_Form(ni, a, d)

  !-----------------------------------!
  !    This is quite tricky point.    !
  !   What if bnrm2 is very small ?   !
  !-----------------------------------!
  bnrm2 = Root_Mean_Square(ni, b)

  if(bnrm2 < tol) then
    iter = 0
    goto 1
  end if

  !----------------!
  !   r = b - Ax   !
  !----------------!
  call Residual_Vector(ni, r1, b, a, x)

  !--------------------------------!
  !   Calculate initial residual   !
  !--------------------------------!
  res = Root_Mean_Square(ni, r1)

  if(res < tol) then
    iter = 0
    goto 1
  end if

  !-----------------------!
  !   Choose initial r~   !
  !-----------------------!
  r2 % val(0:ni-1) = r1 % val(0:ni-1)

  !---------------!
  !               !
  !   Main loop   !
  !               !
  !---------------!
  do iter = 0, miter-1

    !------------------------!
    !    solve M   z  = r    !
    !    solve M^T z~ = r~   !  don't have M^T!!!
    !    (q instead of z)    !
    !------------------------!
    call Prec_Solve(ni, a, d, q1, r1)
    call Prec_Solve(ni, a, d, q2, r2)

    !------------------!
    !   rho = (z,r~)   !
    !------------------!
    rho = dot_product(q1 % val(0:ni-1), r2 % val(0:ni-1))

    if(iter .eq. 0) then
      p1 % val(0:ni-1) = q1 % val(0:ni-1)
      p2 % val(0:ni-1) = q2 % val(0:ni-1)
    else
      beta = rho / rho_old
      p1 % val(0:ni-1) = q1 % val(0:ni-1) + beta * p1 % val(0:ni-1)
      p2 % val(0:ni-1) = q2 % val(0:ni-1) + beta * p2 % val(0:ni-1)
    end if

    !---------------!
    !   q = A   p   !
    !   q~= A^T p~  !  don't have A^T
    !---------------!
    do i = 0, ni-1
      q1 % val(i) = 0.0
      q2 % val(i) = 0.0
      do j = a % row(i), a % row(i+1)-1
        k = a % col(j)
        q1 % val(i) = q1 % val(i) + a % val(j) * p1 % val(k)
        q2 % val(i) = q2 % val(i) + a % val(j) * p2 % val(k)
      end do
    end do

    !----------------------!
    !   alfa = rho/(p,q)   !
    !----------------------!
    alfa = 0.0
    alfa = alfa + dot_product(p2 % val(0:ni-1), q1 % val(0:ni-1))
    alfa = rho / alfa

    !--------------------!
    !   x = x + alfa p   !
    !   r = r - alfa q   !
    !--------------------!
    x  % val(0:ni-1) = x  % val(0:ni-1) + alfa * p1 % val(0:ni-1)
    r1 % val(0:ni-1) = r1 % val(0:ni-1) - alfa * q1 % val(0:ni-1)
    r2 % val(0:ni-1) = r2 % val(0:ni-1) - alfa * q2 % val(0:ni-1)


    !-----------------------!
    !   Check convergence   !
    !-----------------------!
    res = Root_Mean_Square(ni, r1)

    if(res < tol) goto 1

    rho_old=rho

  end do     ! iter

  !----------------------------------!
  !                                  !
  !   Convergence has been reached   !
  !                                  !
  !----------------------------------!
1 continue
  niter = iter

  call Cpu_Timer_Mod_Stop('Solver_Mod_Bicg')

  end subroutine
