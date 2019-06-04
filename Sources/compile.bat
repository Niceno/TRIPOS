ifort -c Const_Mod.f90 
ifort -c Cpu_Timer_Mod.f90 
ifort -c Comm_Mod.f90 
ifort -c Mesh_Mod.f90 
ifort -c Sort_Mod.f90 
ifort -c Vector_Mod.f90
ifort -c File_Mod.f90 
ifort -c Matrix_Mod.f90
ifort -c Solver_Mod.f90
ifort -o Tripos.exe *.obj Global\*.f90 Main.f90
