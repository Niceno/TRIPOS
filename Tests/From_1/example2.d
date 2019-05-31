#-----------#
# Example 2 #
#-----------#

#=========
| POINTS |
=========#
22  # number of points #

#  Nodes which define the boundary #
 1:   0.0   0.0   0.2    3
 2:   5.0   0.0   0.2    3
 3:   5.0   3.0   0.2    3
 4:   3.0   3.0   0.2    3
 5:   3.0   2.0   0.2    3
 6:   0.0   2.0   0.2    3

# circular hole #
 7:   4.00  0.50   10.0   2
 8:   3.75  0.567  10.0   2
 9:   3.567 0.75   10.0   2
10:   3.50  1.00   10.0   2
11:   3.567 1.25   10.0   2
12:   3.75  1.433  10.0   2
13:   4.00  1.50   10.0   2
14:   4.25  1.433  10.0   2
15:   4.433 1.25   10.0   2
16:   4.50  1.00   10.0   2
17:   4.433 0.75   10.0   2
18:   4.25  0.567  10.0   2

#  Nodes which define the virtual hole #
19:   0.75  0.75  0.1    0
20:   2.25  0.75  0.1    0
21:   2.25  1.25  0.1    0
22:   0.75  1.25  0.1    0


#===========
| SEGMENTS |
===========#
22 # Number of segments #

# Boundary segments #
 1:   1   2   3
 2:   2   3   3
 3:   3   4   3
 4:   4   5   3 
 5:   5   6   3
 6:   6   1   3

# Hole segments #
 7:  7   8   2
 8:  8   9   2
 9:  9  10   2
10: 10  11   2
11: 11  12   2
12: 12  13   2
13: 13  14   2
14: 14  15   2
15: 15  16   2
16: 16  17   2
17: 17  18   2
18: 18   7   2

# Refinement section #
18:  19  20  0
19:  20  21  0
20:  21  22  0
21:  22  19  0