#-----------#
# Example 1 #
#-----------#

#=========
| POINTS |
=========#
14 # number of points #

# Nodes which define the boundary #
pnt-01:   0.0   0.0   1.5   1
pnt-02:   5.0   0.0   1.5   1
pnt-03:  80.0   0.0   9.0   1
pnt-04:  90.0  35.0   9.0   1
pnt-05:  60.0  50.0   1.5   1
pnt-06:   0.0  80.0   3.0   1
pnt-07:  10.0  50.0   1.5   1
pnt-08:  20.0  50.0   1.5   1
pnt-09:  20.0  30.0   1.5   1
pnt-10:  60.0  30.0   1.5   1

# One point for coarsening #
pnt-11:  40.0  40.0   5.0   0

# Points for material markers:

  1: iron
  2: wire
  3: air
#
pnt-12:  40.0  10.0   3.0   1
pnt-13:  40.0  40.0   3.0   2
pnt-14:  40.0  55.0   3.0   3

#===========
| SEGMENTS |
===========#
13 # Number of segments #


# Boundary conditions are defined as following:
  1) A = 0       (boundary-01 in dxf)
  2) dA/dn = 0   (boundary-02 in dxf) 
  3) iron-wire   (boundary-03 in dxf)
  4) iron-air    (boundary-04 in dxf)

# The whole domain (iron, air and wire) #
01:     1    2    2
02:     2    3    2
03:     3    4    1
04:     4    5    2
05:     5    6    2
06:     6    1    2

# Separation between iron and wire #
07:     5    8    3
08:     8    9    3
09:     9   10    3
10:    10    5    3

# Separation between iron and air #
11:     2    7    4
12:     7    8    4

# Fake segment for coarsening #
13:    11   11    0
