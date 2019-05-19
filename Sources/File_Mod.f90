!==============================================================================!
  module File_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod, only: CL, FU, GREAT, SMALL
  use Comm_Mod
  use Vector_Mod
  use Mesh_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  integer, parameter :: N_ISOLINE = 17  ! seem to work well with DXF colors

! character(len=CL) :: fig_results_name = ""

  contains

  include 'File_Mod/Logo.f90'
  include 'File_Mod/Splash.f90'
  include 'File_Mod/Example.f90'
  include 'File_Mod/Dxf_End.f90'
  include 'File_Mod/Dxf_Line.f90'
  include 'File_Mod/Dxf_Draw_Mesh.f90'
  include 'File_Mod/Dxf_Draw_Results.f90'
  include 'File_Mod/Dxf_Solid.f90'
  include 'File_Mod/Dxf_Start_Mesh.f90'
  include 'File_Mod/Dxf_Start_Results.f90'
  include 'File_Mod/Fig_End.f90'
  include 'File_Mod/Fig_Line.f90'
  include 'File_Mod/Fig_Draw_Mesh.f90'
  include 'File_Mod/Fig_Solid.f90'
  include 'File_Mod/Fig_Start.f90'
  include 'File_Mod/Save_Mesh.f90'
  include 'File_Mod/Load_Domain.f90'
  include 'File_Mod/Get_Useful_Line.f90'
  include 'File_Mod/Mesh_Statistics.f90'
  include 'File_Mod/Load_Boundary_Conditions.f90'
  include 'File_Mod/Load_Material_Conditions.f90'

  end module

