#--------------#
# Model 1 Full #
#--------------#

#=========
| POINTS |
=========#
5 # number of points #

# Points which define the boundary #
1:   0.0   0.0   0.5   1
2:  20.0   0.0   0.5   1
3:  20.0   5.0   0.5   1
4:  10.0  15.0   3.0   1
5:   0.0  25.0   0.5   1

#===========
| SEGMENTS |
===========#
5 # Number of segments #

# Boundary segments 

  Boundary conditions are defined as following:
  1: A = 0
  2: A = 20
  3: dA/dn = 0
#

1:   1   2   3
2:   2   3   2
3:   3   4   3
4:   4   5   3
5:   5   1   1

