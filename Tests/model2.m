#--------------#
#              #
#   model2.m   #
#              #
#--------------#

#------------------------------------------------#
# This is input file for TRIPOS program          #
# for specification of material properties       #
#                                                #
# Everything enclosed within cashes is a comment #
#------------------------------------------------#

# The file starts by specifying number of materials #
#                                                   #
# This number should be the same to number of       #
# different materials specified in name.d file      #
# (For this particular example, materials are       #
#  listed in lines 57-59 of file model2.d)          #

3 # number of materials

#  For each material we have one line, defining:  #
#  - one word with descrition                     #
#  - mu                                           #
#  - j                                            #
#  word      mu     j                             #
  1-iron:  0.005   0.0
  2-wire:  1.0     4.0
  3-air:   1.0     0.0

