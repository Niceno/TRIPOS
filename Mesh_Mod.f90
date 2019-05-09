!==============================================================================!
  module Mesh_Mod
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod,     only: RP, MAX_NODES, ON, OFF, GREAT, CLOSED,  &
                           ACTIVE, WAITING, DONE
  use Cpu_Timer_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  integer  :: ugly         ! does ugly have to be here?
  real(RP) :: r_tol = 0.7  ! adjusts level of aggresivity

  !------------------!
  !   Element type   !
  !------------------!
  type Elem_Type
    integer :: i,  j,  k   ! element's nodes
    integer :: ei, ej, ek  ! element's neighbours
    integer :: si, sj, sk  ! element's sides
    integer :: mark        ! is it off (ON or OFF)
    integer :: state       ! is it DONE, ACTIVE or WAITING
    integer :: material    ! material marker
    integer :: new_numb;   ! used for renumeration
    real(RP) :: xv, yv, xin, yin, r_out, r_in, det;
  end type

  !---------------!
  !   Node type   !
  !---------------!
  type Node_Type
    integer :: nne        ! number of neighboring elements
    integer :: mark       ! is it off?
    integer :: next       ! next node in the boundary chain
    integer :: chain      ! to which chains the node belongs
    integer :: inserted
    integer :: new_numb   ! used for renumeration
    real(RP) :: x, y, f
    real(RP) :: sumx, sumy
  end type

  !---------------!
  !   Side type   !
  !---------------!
  type Side_Type
    integer :: ea, eb      ! left and right element
    integer :: a, b, c, d  ! left, right, start and end point
    integer :: mark        ! is it off, is on the boundary
    integer :: new_numb    ! used for renumeration
    real(RP) :: s
  end type

  !------------------!
  !   Segment type   !
  !------------------!
  type Segm_Type
    integer :: n, n0, n1
    integer :: chain
    integer :: bound
    integer :: mark
  end type

  !----------------!
  !   Chain type   !
  !----------------!
  type Chai_Type
    integer :: s0, s1
    integer :: type
  end type

  !----------------------------------!
  !   Mesh is a composition of all   !
  !    those types defined above     !
  !----------------------------------!
  integer                      :: n_elem, n_node, n_side
  integer                      :: n_point, n_chain, n_segment
  type(Elem_Type)              :: elem(0:MAX_NODES*2)
  type(Node_Type)              :: node(0:MAX_NODES)
  type(Node_Type)              :: point(0:MAX_NODES/2)
  type(Side_Type)              :: side(0:MAX_NODES*3)
  type(Segm_Type), allocatable :: segment(:)
  type(Chai_Type), allocatable :: chain(:)

  contains

  include 'Mesh_Mod/Area.f90'
  include 'Mesh_Mod/Dist.f90'
  include 'Mesh_Mod/Erase.f90'
  include 'Mesh_Mod/Relax.f90'
  include 'Mesh_Mod/Bowyer.f90'
  include 'Mesh_Mod/Smooth.f90'
  include 'Mesh_Mod/Circles.f90'
  include 'Mesh_Mod/Diamond.f90'
  include 'Mesh_Mod/In_Elem.f90'
  include 'Mesh_Mod/Spacing.f90'
  include 'Mesh_Mod/Classify.f90'
  include 'Mesh_Mod/New_Node.f90'
  include 'Mesh_Mod/Renumber.f90'
  include 'Mesh_Mod/Materials.f90'
  include 'Mesh_Mod/Swap_Side.f90'
  include 'Mesh_Mod/Neighbours.f90'
  include 'Mesh_Mod/Insert_Node.f90'

  end module
