!==============================================================================!
  module Vector_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod, only: RP
  use Mesh_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------!
  !   Vector type   !
  !-----------------!
  type Vector_Type

    type(Mesh_Type), pointer :: pnt_mesh

    real(RP), allocatable :: val(:)    ! value
  end type

  contains

  include 'Vector_Mod/Allocate.f90'

  end module 