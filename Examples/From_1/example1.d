#-----------#
# Example 1 #
#-----------#

#=========
| POINTS |
=========#
9 # number of points #

# Nodes which define the boundary #
pnt-1:   0.0   0.0   0.25   1
pnt-2:   5.0   0.0   0.25   1
pnt-3:   5.0   2.0   0.25   2
pnt-4:   4.0   3.0   0.25   3
pnt-5:   0.0   3.0   0.25   3

# Nodes which define the hole #
pnt-6:   1.0   1.0   0.1    4
pnt-7:   1.0   2.0   0.1    4
pnt-8:   2.0   2.0   0.1    4
pnt-9:   2.0   1.0   0.1    4

#===========
| SEGMENTS |
===========#
9 # Number of segments #

# Boundary segments #
seg-1:   1   2   1
seg-2:   2   3   2
seg-3:   3   4   2
seg-4:   4   5   3
seg-5:   5   1   3

# Hole segments #
seg-6:   6   7   4
seg-7:   7   8   4
seg-8:   8   9   4
seg-9:   9   6   4
