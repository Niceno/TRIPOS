#===============================================================================
#
#   Generate Makefile
#
#-------------------------------------------------------------------------------

#--------------------------
#   Variable definitions
#--------------------------

# Fortran compiler ("gnu", "intel" or "portland")
FC = gfortran

# Directories for objects and modules. (No need to change.)
DIR_MODULE = .Modules
DIR_OBJECT = .Objects

OPT_COMP = -J $(DIR_MODULE) -g -Wall
OPT_LINK = $(OPT_COMP)

# Program name (This should hardly change)
PROGRAM_FILE = Easymesh_F

VPATH = Global

#-------------
#   Modules
#-------------

# Modules in the local directory
SRC_MOD += Const_Mod.f90	\
           Cpu_Timer_Mod.f90	\
           Mesh_Mod.f90		\
           File_Mod.f90


SRC_FUN = Setup_Domain.f90	\
          Insert_Domain.f90	\
          Main.f90

#--------------------------------------------------------
#   List of objects generated from the list of modules
#--------------------------------------------------------
OBJ_MOD = $(SRC_MOD:%.f90=$(DIR_OBJECT)/%.o)
OBJ_FUN = $(SRC_FUN:%.f90=$(DIR_OBJECT)/%.o)
OBJ = $(OBJ_MOD) $(OBJ_FUN)

#---------------------------------------------------------
#   Default rule to build Fortran modules and functions
#---------------------------------------------------------

# Modules
$(DIR_OBJECT)/%.o: %.f90 %/*.f90
	@echo FC $<
	@$(FC) $(OPT_COMP) -c -o $@ $<

# Functions
$(DIR_OBJECT)/%.o: %.f90
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
DEP_LEVEL = Const_Mod.f90	\
            Cpu_Timer_Mod.f90	\
            Cpu_Timer_Mod/*.f90
$(DIR_OBJECT)/Cpu_Timer_Mod.o: $(DEP_LEVEL)

DEP_LEVEL += Mesh_Mod.f90	\
             Mesh_Mod/*.f90
$(DIR_OBJECT)/Mesh_Mod.o: $(DEP_LEVEL)

DEP_LEVEL += File_Mod.f90	\
             File_Mod/*.f90
$(DIR_OBJECT)/File_Mod.o: $(DEP_LEVEL)

$(OBJ_FUN): $(DEP_LEVEL)

#---------------------
#   Explicit target.
#---------------------
clean:
	rm -f $(DIR_OBJECT)/*.o $(DIR_MODULE)/*.mod $(PROGRAM_FILE)
