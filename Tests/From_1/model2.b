#--------------#
#              #
#   model2.b   #
#              #
#--------------#

#------------------------------------------------#
# This is input file for Easymesh program        #
# for specification of boundary conditions       #
#                                                #
# Everything enclosed within cashes is a comment #
#------------------------------------------------#

# The file starts by specifying number of boundary conditions #
#                                                             #
# This number should be the same to number of different       #
# boundary conditions specified in name.d file                #

3   # number of boundary conditions #

# For each boundary condition we have one line with following entries:  #
# - one word describing the boundary condition                          #
# - letter "D" or "N" for Dirichlet or neuman boundary condition type   #
# - value for the boundary condition                                    #
#  word    type    val                                                  #
  bnd-1:     D     0.0
  bnd-2:     N     0.0
  bnd-3:     N     0.0
