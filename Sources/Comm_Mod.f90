!==============================================================================!
  module Comm_Mod
!------------------------------------------------------------------------------!
!  For command line options to the program                                     !
!------------------------------------------------------------------------------!
  use Const_Mod, only: RP, CL, ON, OFF
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  type Comm_Type

    ! Problem name
    character(len=CL) :: problem_name

    ! Command line arguments
    integer  :: messages  = ON     ! print messages
    integer  :: example   = OFF    ! create example domain
    integer  :: tri       = ON     ! perform Delaunay triangulization
    integer  :: relax     = ON     ! perform relaxation
    integer  :: smooth    = ON     ! perform smoothing
    integer  :: dxf       = OFF    ! don't create DXF file
    integer  :: eps       = OFF    ! don't create EPS file
    integer  :: delaunay  = OFF    ! don't draw Delaunay mesh in plots
    integer  :: voronoi   = OFF    ! don't draw Voronoi mesh in plots
    integer  :: solve     = OFF    ! solve Poisson equation
    real(RP) :: r_tol     = 0.7    ! adjusts level of aggresivity
  end type

  end module
