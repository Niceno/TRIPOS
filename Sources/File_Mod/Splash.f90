!==============================================================================!
  subroutine File_Mod_Splash
!------------------------------------------------------------------------------!
!   Splashes the screen with command line options                              !
!==============================================================================!

  call File_Mod_Logo  ! prints 12 lines

  print *, ""                                                             ! 13
  print *, "Usage:  EasyMesh  <name.d>  [<options>]"                      ! 14
  print *, ""                                                             ! 15
  print *, "* Options"                                                    ! 16
  print *, ""                                                             ! 17
  print *, "   +a [0..6]   bee more agressive when meshing"               ! 18
  print *, "   -d          don't triangulate domain"                      ! 19
  print *, "   -m          without messages"                              ! 20
  print *, "   -r          without relaxation"                            ! 21
  print *, "   -s          without Laplacian smoothing"                   ! 22
  print *, "   +dxf [D,V]  create drawing in DXF format"                  ! 23
  print *, "   +fig [D,V]  create drawing in fig format"                  ! 24
  print *, "   +example    create example input file"                     ! 25

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""                                                             !  1
  print *, "* Notes"                                                      !  2
  print *, ""                                                             !  3
  print *, "  - After +a option, you should specify the agressivity"      !  4
  print *, "    level which is an integer value from 0 to 6. The more"    !  5
  print *, "    agressive you are, the chances of getting the final"      !  6
  print *, "    mesh are better, but the mesh quality is poorer."         !  7
  print *, ""                                                             !  8
  print *, "  - If you want eps output with Delaunay or Voronoi"          !  9
  print *, "    mesh only, specify D or V after +eps option."             ! 10
  print *, "    If you don't specify anything after +eps option,"         ! 11
  print *, "    EasyMesh will draw both meshes."                          ! 12
  print *, ""                                                             ! 13
  print *, "* Input"                                                      ! 14
  print *, ""                                                             ! 15
  print *, "Input file <name.d> has the following format"                 ! 16
  print *, "  first line:              <n_bnd_p>"                         ! 17
  print *, "  following n_bnd_p lines: <point:> <x> <y> <spacing> <marker>"
  print *, "  one line:                <n_bnd_s>"                         ! 19
  print *, "  following n_bnd_s lines: <segment:> <start_point> " // &    ! 20
                                                 "<end_point> <marker>"
  print *, ""                                                             ! 21
  print *, "  where:"                                                     ! 22
  print *, "    n_bnd_p is the number of points defining the boundary"    ! 23
  print *, "    n_bnd_s is the number of sides defining the boundary"     ! 24
  print *, "    marker  is the boundary condition marker"                 ! 25

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""                                                             !  1
  print *, "* Output"                                                     !  2
  print *, ""                                                             !  3
  print *, "EasyMesh produces the following three output files:"          !  4
  print *, "  name.n"                                                     !  5
  print *, "  name.e"                                                     !  6
  print *, "  name.s"                                                     !  7
  print *, ""                                                             !  8
  print *, "Node file (name.n) has the following format:"                 !  9
  print *, "  first line:         <n_node>"                               ! 10
  print *, "  following Nn lines: <node:> <x> <y> <marker> "              ! 11
  print *, "  last two lines:     comments inserted by the program "      ! 12
  print *, ""                                                             ! 13
  print *, "  where:"                                                     ! 14
  print *, "  n_node  is the number of nodes"                             ! 15
  print *, "  x, y    are the node coordinates"                           ! 16
  print *, "  marker  is the node boundary marker"                        ! 17
  print *, ""                                                             ! 18
  print *, "Element file (name.e) has the following format:"              ! 19
  print *, "  first line          <n_elem> "                              ! 20
  print *, "  following Ne lines: <element:> <i> <j> <k> <ei> <ej> <ek> " // & 
                                     "<si> <sj> <sk> <xv> <yv> <marker> " ! 21
  print *, "  last two lines:     comments inserted by the program "      ! 22
  print *, ""                                                             ! 23

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""                                                             !  1
  print *, "  where:"                                                     !  2
  print *, "    n_elem      is the number of elements"                    !  3
  print *, "    i,   j,  k  are the nodes belonging to the element, "     !  4
  print *, "    ei, ej, ek  are the neighbouring elements,"               !  5
  print *, "    si, sj, sk  are the element sides. "                      !  6
  print *, "    xv, yv      are the coordinates of the element circumcenter"
  print *, "    marker      is the side boundary marker"                  !  8
  print *, ""                                                             !  9
  print *, "Side file (name.s) has the following format:"                 ! 10
  print *, "  first line:             <n_side> "                          ! 11
  print *, "  following n_side lines: <side:> <c> <d> <ea> <eb> <marker> "! 12
  print *, "  last two lines:         comments inserted by the program "  ! 13
  print *, ""                                                             ! 14
  print *, "  where:"                                                     ! 15
  print *, "    n_side  is the number of sides"                           ! 16
  print *, "    c,  d   are the starting and ending node of the side,"    ! 17
  print *, "    ea, eb  are the elements left and right from the side."   ! 18
  print *, ""                                                             ! 19
  print *, "Note: If eb equals to -1, it means that the right element "   ! 20
  print *, "      doesn''t exists, i.e. the side is on the boundary !"    ! 21
  print *, ""

  end subroutine
