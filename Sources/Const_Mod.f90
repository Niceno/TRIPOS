!==============================================================================!
  module Const_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  ! Constants defining precision and length of certain variables
  integer, parameter  :: RP =   8   ! real numbers' precision
  integer, parameter  :: FU =   9   ! default file unit number
  integer, parameter  :: CL = 128   ! default string length

  ! A few handy constants
  real(RP), parameter :: PI    = 3.14159265359
  real(RP), parameter :: SMALL = 1.0e-30
  real(RP), parameter :: GREAT = 1.0e+30

  !------------------------------!
  !   Various elements' states   !
  !------------------------------!
  integer, parameter :: OFF     = -1  ! element is switched off
  integer, parameter :: ON      =  0
  integer, parameter :: ACTIVE  =  3
  integer, parameter :: DONE    =  4
  integer, parameter :: WAITING =  5

  integer, parameter :: MAX_NODES = 32768

  !-------------------------------!
  !   Definitions for the chains  !
  !-------------------------------!
  integer, parameter :: CLOSED = 0
  integer, parameter :: OPEN   = 1
  integer, parameter :: INSIDE = 2

  end module
