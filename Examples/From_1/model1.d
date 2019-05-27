#--------------#
#              #
#   model1.d   #
#              #
#--------------#

#------------------------------------------------#
# This is input file for Easymesh program        #
# for specification of problem domain shape      #
#                                                #
# Everything enclosed within cashes is a comment #
# The file has two main sections:                #
#                                                #
# 1) list of points defining the problem domain  #
# 2) list of segments connecting points          #
#------------------------------------------------#

#---------------------------#
#   First section: points   #
#---------------------------#

# The section starts with one line specifying number of points #
7

# For each point we have one line with following entries (columns):      #
# - one word describing the point                                        #
# - point's x coordinate                                                 #
# - point's y coordinate                                                 #
# - desired mesh spacing (elemet size) at that point                     #
# - a marker which can be 1 or 0, points which shouldn't move during     #
#   the mesh genration point are 1, the others are 0                     #

# We start with ten points which shouldn't move.  #
# The points define problem boundary              #
#  word      x     y    spc   marker              #
    1:      0.0   0.0   0.5     1
    2:     20.0   0.0   0.25    2
    3:     20.0   5.0   0.25    2
    4:     25.0   5.0   0.25    2
    5:     25.0  25.0   0.5     1
    6:      0.0  25.0   0.5     1

# One point for which doesn't belong to any chain   #
# but is inserted only to control mesh spacing      #
#  word      x     y    spc   marker                #
    7:     10.0  15.0   2.5     0

#------------------------------#
#   Second section: segments   #
#------------------------------#

# The section starts with one line specifying number of points #
7

# For each segment we have one line with following entries (columns):  #
# - one word describing the segment                                    #
# - number of first point (from the above list)                        #
# - number of second point (from the above list)                       #
# - boundary condition marker (to be defined in file name.b)           #

# Six segmentns defining the contours of the domain              #
#  word     pnt-1  pnt-2  marker                                 #
    1:       1      2       3
    2:       2      3       2
    3:       3      4       2
    4:       4      5       3
    5:       5      6       1
    6:       6      1       1

# Segment to accomodate point for coarsening            #
#                                                       #
# This segment has marker 0, meaning it is not static.  #
# It moves during the mesh generation process, but      #
# gives desired element spacing in its surroundings.    #
#  word     pnt-1  pnt-2  marker                        #
    7:       7      7       0
