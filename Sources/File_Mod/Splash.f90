!==============================================================================!
  subroutine File_Mod_Splash
!------------------------------------------------------------------------------!
!   Splashes the screen with command line options                              !
!==============================================================================!

  call File_Mod_Logo  ! prints 12 lines

  print *, ""
  print *, "Usage:  EasyMesh  <name>  [<options>]"
  print *, ""
  print *, "* Options"
  print *, ""
  print *, "   +a [0..6] bee more agressive when meshing"
  print *, "   -d        don't triangulate domain"
  print *, "   -m        without messages"
  print *, "   -r        without relaxation"
  print *, "   -s        without Laplacian smoothing"
  print *, "   +dxf      create drawing in DXF format"
  print *, "   +fig      create drawing in fig format"
  print *, "   +example  create example input file"

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""
  print *, "Note: After +a option, you should specify the agressivity"
  print *, "      level which is an integer value from 0 to 6. The more"
  print *, "      agressive you are, the chances of getting the final"
  print *, "      mesh are better, but the mesh quality is poorer."
  print *, ""
  print *, "* Input"
  print *, ""
  print *, "Input file (name.d) has the following format"
  print *, "  first line:              <n_bnd_p>"
  print *, "  following n_bnd_p lines: <point:> <x> <y> <spacing> <marker>"
  print *, "  one line:                <n_bnd_s>"
  print *, "  following n_bnd_s lines: <segment:> <start_point> " // & 
                                                 "<end_point> <marker>"
  print *, ""
  print *, "  where:"
  print *, "    n_bnd_p is the number of points defining the boundary"
  print *, "    n_bnd_s is the number of sides defining the boundary"
  print *, "    marker  is the boundary condition marker"
  print *, ""
  print *, "Note: Input file has to end with the extension: .d !"
  print *, ""

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""
  print *, "* Output"
  print *, ""
  print *, "EasyMesh produces the following three output files:"
  print *, "  name.n"
  print *, "  name.e"
  print *, "  name.s"
  print *, ""
  print *, "Node file (name.n) has the following format:"
  print *, "  first line:         <n_node>"
  print *, "  following Nn lines: <node:> <x> <y> <marker> "
  print *, "  last two lines:     comments inserted by the program "
  print *, ""
  print *, "  where:"
  print *, "  n_node  is the number of nodes"
  print *, "  x, y    are the node coordinates"
  print *, "  marker  is the node boundary marker"
  print *, ""
  print *, "Element file (name.e) has the following format:"
  print *, "  first line          <n_elem> "
  print *, "  following Ne lines: <element:> <i> <j> <k> <ei> <ej> <ek> " // & 
                                     "<si> <sj> <sk> <xv> <yv> <marker> "
  print *, "  last two lines:     comments inserted by the program "
  print *, ""

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""
  print *, "  where:"
  print *, "    n_elem      is the number of elements"
  print *, "    i,   j,  k  are the nodes belonging to the element, "
  print *, "    ei, ej, ek  are the neighbouring elements,"
  print *, "    si, sj, sk  are the element sides. "
  print *, "    xv, yv      are the coordinates of the element circumcenter"
  print *, "    marker      is the side boundary marker"
  print *, ""
  print *, "Side file (name.s) has the following format:"
  print *, "  first line:             <n_side> "
  print *, "  following n_side lines: <side:> <c> <d> <ea> <eb> <marker> " 
  print *, "  last two lines:         comments inserted by the program "
  print *, ""
  print *, "  where:"
  print *, "    n_side  is the number of sides"
  print *, "    c,  d   are the starting and ending node of the side,"
  print *, "    ea, eb  are the elements left and right from the side."
  print *, ""
  print *, "Note: If eb equals to -1, it means that the right element "
  print *, "      doesn''t exists, i.e. the side is on the boundary !"
  print *, ""

  end subroutine
