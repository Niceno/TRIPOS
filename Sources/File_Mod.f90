!==============================================================================!
  module File_Mod
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod, only: CL, FU, GREAT, SMALL
  use Mesh_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  character(len=CL) :: name
  character(len=CL) :: dxf_name
  character(len=CL) :: fig_name
  integer           :: len

  contains

  include 'File_Mod/Logo.f90'
  include 'File_Mod/Splash.f90'
  include 'File_Mod/Example.f90'
  include 'File_Mod/Dxf_End.f90'
  include 'File_Mod/Dxf_Line.f90'
  include 'File_Mod/Dxf_Draw.f90'
  include 'File_Mod/Dxf_Start.f90'
  include 'File_Mod/Fig_End.f90'
  include 'File_Mod/Fig_Line.f90'
  include 'File_Mod/Fig_Draw.f90'
  include 'File_Mod/Fig_Start.f90'
  include 'File_Mod/Save_Mesh.f90'
  include 'File_Mod/Load_Domain.f90'
  include 'File_Mod/Get_Useful_Line.f90'

  end module

