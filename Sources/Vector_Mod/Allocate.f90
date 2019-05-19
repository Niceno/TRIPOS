!==============================================================================!
  subroutine Vector_Mod_Allocate(vector, mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Vector_Type)        :: vector
  type(Mesh_Type),  target :: mesh
!==============================================================================!

  ! Store pointer to the mesh
  vector % pnt_mesh => mesh

  ! Allocate memory for vector
  allocate (vector % val(0:mesh % n_node-1));  vector % val = 0.0

  end subroutine
