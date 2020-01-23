#===============================================================================
#
#   Tripos makefile
#
#-------------------------------------------------------------------------------

#--------------------------
#   Variable definitions
#--------------------------

# Fortran compiler ("gnu", "intel" or "portland")
FC = gfortran

# Directories for objects and modules. (No need to change.)
DIR_MOD = .Modules
DIR_OBJ = .Objects

OPT_COMP = -J $(DIR_MOD) -g -Wall
OPT_LINK = $(OPT_COMP)

# Program name (This should hardly change)
PROGRAM_FILE = Tripos

VPATH = Global

#-------------
#   Modules
#-------------

# Modules in the local directory
SRC_MOD += Const_Mod.f90	\
           Options_Mod.f90	\
           Sort_Mod.f90		\
           Cpu_Timer_Mod.f90	\
           Mesh_Mod.f90		\
           Vector_Mod.f90	\
           Matrix_Mod.f90	\
           Solver_Mod.f90	\
           File_Mod.f90

SRC_FUN = Swap_Int.f90			\
          Setup_Chains.f90		\
          Discretize.f90		\
          Parse_Command_Line.f90	\
          Main.f90

#--------------------------------------------------------
#   List of objects generated from the list of modules
#--------------------------------------------------------
OBJ_MOD = $(SRC_MOD:%.f90=$(DIR_OBJ)/%.o)
OBJ_FUN = $(SRC_FUN:%.f90=$(DIR_OBJ)/%.o)
OBJ = $(OBJ_MOD) $(OBJ_FUN)

#---------------------------------------------------------
#   Default rule to build Fortran modules and functions
#---------------------------------------------------------

# Modules
$(DIR_OBJ)/%.o: %.f90 %/*.f90
	@echo FC $<
	@$(FC) $(OPT_COMP) -c -o $@ $<

# Functions
$(DIR_OBJ)/%.o: %.f90
	@echo FC $<
	@$(FC) $(OPT_COMP) -c -o $@ $<

#-----------------------------------
#   Rule to build main program
#-----------------------------------
#   Note: Should not be modified.
#-----------------------------------
$(PROGRAM_FILE): $(OBJ) 
	@echo Linking "\033[0;32m $(PROGRAM_FILE) \033[0m"
	@$(FC) $(OPT_LINK) -o $(PROGRAM_FILE) $(OBJ)

#--------------------------------------------------------------------
#   Explicit dependencies for modules
#--------------------------------------------------------------------

# Dependecy Level Cpu_Timer
DL_CPUT = Const_Mod.f90		\
          Cpu_Timer_Mod.f90	\
          Cpu_Timer_Mod/*.f90
$(DIR_OBJ)/Cpu_Timer_Mod.o: $(DL_CPUT)

# Dependecy Level Mesh
DL_MESH = Mesh_Mod.f90		\
          Mesh_Mod/*.f90
$(DIR_OBJ)/Mesh_Mod.o: $(DL_CPUT) $(DL_MESH)

# Dependecy Level Vector
DL_VECT = Vector_Mod.f90	\
          Vector_Mod/*.f90
$(DIR_OBJ)/Vector_Mod.o: $(DL_CPUT) $(DL_MESH) $(DL_VECT)

# Dependecy Level Matrix
DL_MATR = Sort_Mod.f90		\
          Matrix_Mod.f90	\
          Matrix_Mod/*.f90
$(DIR_OBJ)/Matrix_Mod.o: $(DL_CPUT) $(DL_MESH) $(DL_VECT) $(DL_MATR)

# Dependecy Level Solver
DL_SOLV = Solver_Mod.f90	\
          Solver_Mod/*.f90
$(DIR_OBJ)/Solver_Mod.o: $(DL_CPUT) $(DL_MESH) $(DL_VECT) $(DL_MATR) $(DL_SOLV)

# Dependecy Level File
DL_FILE = File_Mod.f90		\
          File_Mod/*.f90
$(DIR_OBJ)/File_Mod.o: $(DL_CPUT) $(DL_MESH) $(DL_VECT) $(DL_FILE)

$(OBJ_FUN): $(DL_CPUT) $(DL_MESH) $(DL_VECT) $(DL_MATR) $(DL_SOLV) $(DL_FILE)

#---------------------
#   Explicit target.
#---------------------
clean:
	rm -f $(DIR_OBJ)/*.o $(DIR_MOD)/*.mod $(PROGRAM_FILE)
