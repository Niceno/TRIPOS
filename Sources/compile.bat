ifort -c Const_Mod.f90 
ifort -c Cpu_Timer_Mod.f90 
ifort -c Mesh_Mod.f90 
ifort -c File_Mod.f90 
ifort -o Easymesh_Fortran.exe *.obj Global\*.f90
