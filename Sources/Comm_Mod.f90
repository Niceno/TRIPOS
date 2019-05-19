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
    integer  :: exa    = OFF    ! create example domain
    integer  :: tri    = ON     ! perform Delaunay triangulization
    integer  :: relax  = ON     ! perform relaxation
    integer  :: smooth = ON     ! perform smoothing
    integer  :: dxf    = OFF    ! don't create DXF file
    integer  :: fig    = OFF    ! don't create FIG file
    integer  :: del    = OFF    ! don't create DXF file
    integer  :: vor    = OFF    ! don't create FIG file
    integer  :: mes    = ON     ! print messages
    real(RP) :: r_tol  = 0.7    ! adjusts level of aggresivity
  end type

  end module