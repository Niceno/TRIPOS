#-----------#
# Example 1 #
#-----------#

#=========
| POINTS |
=========#
11 # number of points #

# Nodes which define the boundary #
pnt-01:   0.0   0.0   5.0   1
pnt-02:   5.0   0.0   5.0   1
pnt-03:  80.0   0.0   5.0   1
pnt-04:  90.0  35.0   5.0   1
pnt-05:  60.0  50.0   1.5   1
pnt-06:   0.0  80.0   5.0   1
pnt-07:  10.0  50.0   1.5   1
pnt-08:  20.0  50.0   1.5   1
pnt-09:  20.0  30.0   1.5   1
pnt-10:  60.0  30.0   1.5   1

# one point for coarsening #
pnt-11:  40.0  40.0   5.0   0

#===========
| SEGMENTS |
===========#
13 # Number of segments #

# The whole domain (iron, air and wire) #
01:     1    2    1
02:     2    3    1
03:     3    4    1
04:     4    5    1
05:     5    6    1
06:     6    1    1

# Separation between iron and wire #
07:     5    8    1
08:     8    9    1
09:     9   10    1
10:    10    5    1

# Separation between iron and air #
11:     2    7    1
12:     7    8    1

13:    11   11    0
