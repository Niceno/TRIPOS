!==============================================================================!
  module Mesh_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod,     only: RP, MAX_NODES, ON, OFF, GREAT, OPEN, CLOSED,  &
                           ACTIVE, WAITING, DONE
  use Cpu_Timer_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  integer :: num_from     ! one or zero, depends how is user numbering
  integer :: ugly         ! does ugly have to be here?

  !------------------!
  !   Element type   !
  !------------------!
  type Elem_Type
    integer  :: i,  j,  k    ! element's nodes
    integer  :: ei, ej, ek   ! element's neighbours
    integer  :: si, sj, sk   ! element's sides
    integer  :: mark         ! is it off (ON or OFF)
    integer  :: state        ! is it DONE, ACTIVE or WAITING
    integer  :: material     ! material marker
    integer  :: new_numb;    ! used for renumeration
    real(RP) :: xv, yv       ! center of Voronoi polygon
    real(RP) :: xin, yin     ! center of inscribed circle
    real(RP) :: r_ex, r_in, r_rat
    real(RP) :: area_i, area_j, area_k
  end type

  !---------------!
  !   Node type   !
  !---------------!
  type Node_Type
    integer :: nne        ! number of neighboring elements
    integer :: mark       ! is it off?
    integer :: next       ! next node in the boundary chain
    integer :: chain      ! to which chains the node belongs
    integer :: inserted   ! how many times has it been inserted?
    integer :: new_numb   ! used for renumeration
    real(RP) :: x, y, f
    real(RP) :: sumx, sumy
  end type

  !---------------!
  !   Side type   !
  !---------------!
  type Side_Type
    integer  :: ea, eb      ! left and right element
    integer  :: a, b, c, d  ! left, right, start and end point
    integer  :: mark        ! is it off, is on the boundary
    integer  :: new_numb    ! used for renumeration
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

  !-----------------------------!
  !   Boundary condition type   !
  !-----------------------------!
  type Boun_Type
    character :: type    ! can be D/d (Dirchlet) or N/n (Neumann)
    real(RP)  :: value   ! for Dirichlet conditions
  end type

  !-------------------!
  !   Material type   !
  !-------------------!
  type Mate_Type
    real(RP) :: mu  ! diffusion constant
    real(RP) :: j   ! source
  end type

  !----------------------------------!
  !   Mesh is a composition of all   !
  !    those types defined above     !
  !----------------------------------!
  type Mesh_Type
    integer         :: n_elem, n_node, n_side
    integer         :: n_point, n_chain, n_segment
    integer         :: n_bound, n_mater
    type(Elem_Type) :: elem(0:MAX_NODES*2)
    type(Node_Type) :: node(0:MAX_NODES)
    type(Node_Type) :: point(0:MAX_NODES/2)
    type(Side_Type) :: side(0:MAX_NODES*3)
    type(Segm_Type) :: segment(0:MAX_NODES/16)
    type(Chai_Type) :: chain(0:MAX_NODES/32)
    type(Boun_Type) :: bound(0:MAX_NODES/32)
    type(Mate_Type) :: mater(0:MAX_NODES/32)
  end type

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
  include 'Mesh_Mod/Compress.f90'
  include 'Mesh_Mod/New_Node.f90'
  include 'Mesh_Mod/Renumber.f90'
  include 'Mesh_Mod/Materials.f90'
  include 'Mesh_Mod/Swap_Side.f90'
  include 'Mesh_Mod/Neighbours.f90'
  include 'Mesh_Mod/Insert_Node.f90'
  include 'Mesh_Mod/Insert_Chains.f90'

  end module
