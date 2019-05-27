#--------------#
#              #
#   model2.d   #
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
14   # in this line there is the number of points #

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
  pnt-01:   0.0   0.0   1.0     1
  pnt-02:   5.0   0.0   1.0     1
  pnt-03:  80.0   0.0   6.0     1
  pnt-04:  90.0  35.0   6.0     1
  pnt-05:  60.0  50.0   1.0     2
  pnt-06:   0.0  80.0   2.0     2
  pnt-07:  10.0  50.0   1.0     3
  pnt-08:  20.0  50.0   1.0     3
  pnt-09:  20.0  30.0   1.0     3
  pnt-10:  60.0  30.0   1.0     3

# One point for which doesn't belong to any chain   #
# but is inserted only to control mesh spacing      #
#  word      x     y    spc   marker                #
  pnt-11:  40.0  40.0   3.0     0

# Three poins which will be inserted to denote materials, in this order:  #
#  1: iron                                                                #
#  2: wire                                                                #
#  3: air                                                                 #
#  word      x     y    spc   marker                                      #
  pnt-12:  40.0  10.0   2.0     1
  pnt-13:  40.0  40.0   2.0     2
  pnt-14:  40.0  55.0   2.0     3

#------------------------------#
#   Second section: segments   #
#------------------------------#

# The section starts with one line specifying number of segments #
13   # this line contains number of segments #

# For each segment we have one line with following entries (columns):  #
# - one word describing the segment                                    #
# - number of first point (from the above list)                        #
# - number of second point (from the above list)                       #
# - boundary condition marker (to be defined in file name.b)           #

# Six segmentns defining the contours of the domain              #
#  word     pnt-1  pnt-2  marker                                 #
  seg-01:     1      2      1    # 1 will be Dirichlet in name.b #
  seg-02:     2      3      1    # 1 will be Dirichlet in name.b #
  seg-03:     3      4      1    # 1 will be Dirichlet in name.b #
  seg-04:     4      5      2    # 2 will be Neumann in name.b   #
  seg-05:     5      6      2    # 2 will be Neumann in name.b   #
  seg-06:     6      1      2    # 2 will be Neumann in name.b   #

# Separation between iron and wire #
#  word     pnt-1  pnt-2  marker   #
  seg-07:     5      8      3
  seg-08:     8      9      3
  seg-09:     9     10      3
  seg-10:    10      5      3

# Separation between iron and air #
#  word     pnt-1  pnt-2  marker  #
  seg-11:     2      7      3
  seg-12:     7      8      3

# "Fake" segment for coarsening                         #
# This segment has marker 0, meaning it is not static.  #
# It moves during the mesh generation process, but      #
# gives desired element spacing in its surroundings.    #
#  word     pnt-1  pnt-2  marker  #
  seg-13:    11     11      0
