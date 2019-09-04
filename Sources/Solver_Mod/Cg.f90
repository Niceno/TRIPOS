!==============================================================================!
  subroutine Solver_Mod_Cg(sol, a, x, b, miter, niter, tol, res)
!------------------------------------------------------------------------------!
!   Solves the linear systems of equations by a precond. CG method.            !
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
  type(Vector_Type), pointer :: q1
  type(Vector_Type), pointer :: r1
  integer                    :: ni
  real(RP)                   :: alfa, beta, rho, rho_old, bnrm2
  integer                    :: i, j, k, iter
!==============================================================================!

  call Cpu_Timer_Mod_Start('Solver_Mod_Cg')

  ! Take some aliases
  d  => sol % d
  p1 => sol % p1
  q1 => sol % q1
  r1 => sol % r1
  ni =  sol % pnt_mesh % n_node

  res = 0.0

  !---------------------!
  !   Preconditioning   !
  !---------------------!
  call Solver_Mod_Prec_Form(ni, a, d)

  !-----------------------------------!
  !    This is quite tricky point.    !
  !   What if bnrm2 is very small ?   !
  !-----------------------------------!
  bnrm2 = Solver_Mod_Root_Mean_Square(ni, b)

  if(bnrm2 < tol) then
    iter = 0
    goto 1
  end if

  !----------------!
  !   r = b - Ax   !
  !----------------!
  call Solver_Mod_Residual_Vector(ni, r1, b, a, x)

  !--------------------------------!
  !   Calculate initial residual   !
  !--------------------------------!
  res = Solver_Mod_Root_Mean_Square(ni, r1)

  if(res < tol) then
    iter = 0
    goto 1
  end if

  !-----------!
  !   p = r   !
  !-----------!
  p1 % val(0:ni-1) = r1 % val(0:ni-1)

  !---------------!
  !               !
  !   Main loop   !
  !               !
  !---------------!
  do iter = 0, miter-1

    !----------------------!
    !     solve Mz = r     !
    !   (q instead of z)   !
    !----------------------!
    call Solver_Mod_Prec_Solve(ni, a, d, q1, r1)

    !-----------------!
    !   rho = (r,z)   !
    !-----------------!
    rho = dot_product(r1 % val(0:ni-1), q1 % val(0:ni-1))

    if(iter .eq. 0) then
      p1 % val(0:ni-1) = q1 % val(0:ni-1)
    else
      beta = rho / rho_old
      p1 % val(0:ni-1) = q1 % val(0:ni-1) + beta * p1 % val(0:ni-1)
    end if

    !------------!
    !   q = Ap   !
    !------------!
    do i = 0, ni-1
      q1 % val(i) = 0.0
      do j = a % row(i), a % row(i+1)-1
        k = a % col(j)
        q1 % val(i) = q1 % val(i) + a % val(j) * p1 % val(k)
      end do
    end do

    !------------------------!
    !   alfa = (r,z)/(p,q)   !
    !------------------------!
    alfa = dot_product(p1 % val(0:ni-1), q1 % val(0:ni-1))
    alfa = rho / alfa

    !---------------------!
    !   x = x + alfa p    !
    !   r = r - alfa Ap   !
    !---------------------!
    x  % val(0:ni-1) = x  % val(0:ni-1) + alfa * p1 % val(0:ni-1)
    r1 % val(0:ni-1) = r1 % val(0:ni-1) - alfa * q1 % val(0:ni-1)

    !-----------------------!
    !   Check convergence   !
    !-----------------------!
    res = Solver_Mod_Root_Mean_Square(ni, r1)

    if(res < tol) goto 1

    rho_old = rho

  end do ! iter

  !----------------------------------!
  !                                  !
  !   Convergence has been reached   !
  !                                  !
  !----------------------------------!
1 continue
  niter = iter

  call Cpu_Timer_Mod_Stop('Solver_Mod_Cg')

  end subroutine
