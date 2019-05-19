!==============================================================================!
  subroutine Solver_Mod_Allocate(sol, mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Solver_Type)        :: sol
  type(Mesh_Type),  target :: mesh
!==============================================================================!

  sol % pnt_mesh => mesh

  ! Allocate memory for system matrix, preconditioning
  ! matrix and right hand side vector
  call Matrix_Mod_Allocate(sol % d, mesh)

  ! Memory for helping arrays
  call Vector_Mod_Allocate(sol % p1, mesh)
  call Vector_Mod_Allocate(sol % p2, mesh)
  call Vector_Mod_Allocate(sol % q1, mesh)
  call Vector_Mod_Allocate(sol % q2, mesh)
  call Vector_Mod_Allocate(sol % r1, mesh)
  call Vector_Mod_Allocate(sol % r2, mesh)

  end subroutine
