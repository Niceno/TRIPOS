!==============================================================================!
  module Solver_Mod
!------------------------------------------------------------------------------!
!   Module used for native linear solvers.                                     !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod
  use Mesh_Mod
  use Matrix_Mod
  use Vector_Mod
  use Cpu_Timer_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !------------------!
  !   Solvers type   !
  !------------------!
  type Solver_Type

    type(Mesh_Type), pointer :: pnt_mesh

    ! Preconditioning "matrix" for single grid methods
    type(Matrix_Type) :: d     ! preconditioning "matrix"

    type(Vector_Type) :: p1
    type(Vector_Type) :: p2
    type(Vector_Type) :: q1
    type(Vector_Type) :: q2
    type(Vector_Type) :: r1
    type(Vector_Type) :: r2
  end type

  contains

  include 'Solver_Mod/Cg.f90'                   ! cg solver
  include 'Solver_Mod/Bicg.f90'                 ! bicg solver
  include 'Solver_Mod/Allocate.f90'             ! memory all. and creation
  include 'Solver_Mod/Root_Mean_Square.f90'
  include 'Solver_Mod/Prec_Form.f90'
  include 'Solver_Mod/Prec_Solve.f90'
  include 'Solver_Mod/Residual_Vector.f90'

  end module 
