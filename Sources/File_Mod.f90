!==============================================================================!
  module File_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod, only: CL, FU, GREAT, SMALL, ON
  use Comm_Mod
  use Vector_Mod
  use Mesh_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  integer, parameter :: N_ISOLINE = 17  ! seem to work well with DXF colors
  real(RP)           :: xmax, xmin, ymax, ymin, scl

  contains

  include 'File_Mod/Logo.f90'
  include 'File_Mod/Splash.f90'
  include 'File_Mod/Example.f90'
  include 'File_Mod/Dxf_End.f90'
  include 'File_Mod/Dxf_Line.f90'
  include 'File_Mod/Dxf_Circle.f90'
  include 'File_Mod/Dxf_Draw_Mesh.f90'
  include 'File_Mod/Dxf_Draw_Results.f90'
  include 'File_Mod/Dxf_Draw_Gradients.f90'
  include 'File_Mod/Dxf_Solid.f90'
  include 'File_Mod/Dxf_Start_Mesh.f90'
  include 'File_Mod/Dxf_Start_Results.f90'
  include 'File_Mod/Dxf_Start_Gradients.f90'
  include 'File_Mod/Eps_End.f90'
  include 'File_Mod/Eps_Line.f90'
  include 'File_Mod/Eps_Circle.f90'
  include 'File_Mod/Eps_Draw_Mesh.f90'
  include 'File_Mod/Eps_Draw_Results.f90'
  include 'File_Mod/Eps_Draw_Gradients.f90'
  include 'File_Mod/Eps_Solid.f90'
  include 'File_Mod/Eps_Start_Mesh.f90'
  include 'File_Mod/Eps_Start_Results.f90'
  include 'File_Mod/Eps_Start_Gradients.f90'
  include 'File_Mod/Save_Mesh.f90'
  include 'File_Mod/Load_Domain.f90'
  include 'File_Mod/Get_Useful_Line.f90'
  include 'File_Mod/Mesh_Statistics.f90'
  include 'File_Mod/Load_Boundary_Conditions.f90'
  include 'File_Mod/Load_Material_Conditions.f90'
  include 'File_Mod/Get_Cool_Color.f90'
  include 'File_Mod/Get_Warm_Color.f90'
  include 'File_Mod/Get_Rainbow_Color.f90'
  include 'File_Mod/Name_Layer.f90'
  include 'File_Mod/Gradients.f90'

  end module

