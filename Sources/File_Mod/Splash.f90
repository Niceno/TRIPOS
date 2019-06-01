!==============================================================================!
  subroutine File_Mod_Splash
!------------------------------------------------------------------------------!
!   Splashes the screen with command line options                              !
!==============================================================================!

  call File_Mod_Logo  ! prints 12 lines

  print *, ""                                                             ! 13
  print *, "Usage:  Tripos  <name>  [<options>]"                          ! 14
  print *, ""                                                             ! 15
  print *, "where <name> is a problem name specified without extesions."  ! 16
  print *, ""                                                             ! 17
  print *, "* The program can be used in two modes: as a triangular mesh" ! 18
  print *, "generator only, or as a triangular mesh generator and solver" ! 19
  print *, "for Poisson's equation defined on the generated mesh."        ! 20
  print *, ""                                                             ! 21
  print *, "* If Tripos will be used as a mesh generator only, the file"  ! 22
  print *, "<name>.d, which describes the problem domain, has to reside"  ! 23
  print *, "in the working directory.  Its format is explained below."    ! 24
  print *, ""                                                             ! 25

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""                                                             !  1
  print *, "* If Tripos will be used to solve Poisson's equation after"   !  2
  print *, "generating the triangular mesh, two more files are needed:"   !  3
  print *, "<name>.b, which describes boundary conditions and"            !  4
  print *, "<name>.m, which describes material properties and sources."   !  5
  print *, "The description of these files is given below."               !  6
  print *, ""                                                             !  7
  print *, "* Valid options for the progam are:"                          !  8
  print *, ""                                                             !  9
  print *, "   +a [0..6]   bee more agressive when meshing"               ! 10
  print *, "   -d          don't triangulate domain"                      ! 11
  print *, "   -m          without messages"                              ! 12
  print *, "   -r          without relaxation"                            ! 13
  print *, "   -s          without Laplacian smoothing"                   ! 14
  print *, "   +dxf [D,V]  create drawing in DXF format"                  ! 15
  print *, "   +eps [D,V]  create drawing in EPS format"                  ! 16
  print *, "   +example    create example input file"                     ! 17
  print *, "   +solve      run solve for Poisson equation"                ! 18
  print *, ""                                                             ! 19
  print *, "* Notes:"                                                     ! 20
  print *, ""                                                             ! 21
  print *, "  - After +a option, you should specify the agressivity"      ! 22
  print *, "    level which is an integer value from 0 to 6. The more"    ! 23
  print *, "    agressive you are, the chances of getting the final"      ! 24
  print *, "    mesh are better, but the mesh quality is poorer."         ! 25

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""                                                             !  1
  print *, "  - If you want eps output with Delaunay or Voronoi"          !  2
  print *, "    mesh only, specify D or V after +eps/+dxf option."        !  3
  print *, "    If you don't specify anything after +eps/+dxf option,"    !  4
  print *, "    Tripos will draw both meshes."                            !  5
  print *, ""                                                             !  6
  print *, "* Input for mesh generator (mandatory):"                      !  7
  print *, ""                                                             !  8
  print *, "Input file <name.d> has the following format"                 !  9
  print *, "  first line:              <n_bnd_p>"                         ! 10
  print *, "  following n_bnd_p lines: <point:> <x> <y> <spacing> <marker>"
  print *, "  one line:                <n_bnd_s>"                         ! 11
  print *, "  following n_bnd_s lines: <segment:> <start_point> " // &    ! 12
                                                 "<end_point> <marker>"
  print *, ""                                                             ! 13
  print *, "  where:"                                                     ! 14
  print *, "    n_bnd_p is the number of points defining the boundary"    ! 15
  print *, "    n_bnd_s is the number of sides defining the boundary"     ! 16
  print *, "    marker  is the boundary condition marker"                 ! 17
  print *, ""                                                             ! 18
  print *, "* Input for solver (only with option +solve)"                 ! 19
  print *, ""                                                             ! 20
  print *, "Input file <name.b> has the following format:"                ! 21
  print *, "  first line:              <n_bnd_c>"                         ! 22
  print *, "  following n_bnd_c lines: <cond:> <D/N> <value>"             ! 23
  print *, ""                                                             ! 24

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""                                                             !  1
  print *, "  where:"                                                     !  2
  print *, "    n_bnd_c is the number of bundary conditions"              !  3
  print *, "    D/N     specifies if condition is Dirichlet or Neumann"   !  4
  print *, "    value   is boundary condition value or flux (if D or N)"  !  5
  print *, ""                                                             !  6
  print *, "Input file <name.m> has the following format:"                !  7
  print *, "  first line:              <n_mat>"                           !  8
  print *, "  following n_mat lines:   <mat:>  <diff> <source>"           !  9
  print *, ""                                                             ! 10
  print *, "  where:"                                                     ! 11
  print *, "    n_mat   is number of materials"                           ! 12
  print *, "    diff    diffusion coefficient in this material"           ! 13
  print *, "    source  source in this material"                          ! 14
  print *, ""                                                             ! 15
  print *, "* Output"                                                     ! 16
  print *, ""                                                             ! 17
  print *, "Tripos produces the following three output files:"            ! 18
  print *, "  name.n"                                                     ! 19
  print *, "  name.e"                                                     ! 20
  print *, "  name.s"                                                     ! 21
  print *, ""                                                             ! 22
  print *, "Node file (name.n) has the following format:"                 ! 23
  print *, "  first line:         <n_node>"                               ! 24
  print *, ""                                                             ! 25

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""                                                             !  1
  print *, "  following Nn lines: <node:> <x> <y> <marker> "              !  2
  print *, "  last two lines:     comments inserted by the program "      !  3
  print *, ""                                                             !  4
  print *, "  where:"                                                     !  5
  print *, "  n_node  is the number of nodes"                             !  6
  print *, "  x, y    are the node coordinates"                           !  7
  print *, "  marker  is the node boundary marker"                        !  8
  print *, ""                                                             !  9
  print *, "Element file (name.e) has the following format:"              ! 10
  print *, "  first line              <n_elem> "                          ! 11
  print *, "  following n_elem lines: <element:> <i> <j> <k> " // & 
                                     "<ei> <ej> <ek> "         // &
                                     "<si> <sj> <sk> <xv> <yv> <marker> " ! 12
  print *, "  last two lines:     comments inserted by the program "      ! 13
  print *, ""                                                             ! 14
  print *, "  where:"                                                     ! 15
  print *, "    n_elem      is the number of elements"                    ! 16
  print *, "    i,   j,  k  are the nodes belonging to the element, "     ! 17
  print *, "    ei, ej, ek  are the neighbouring elements,"               ! 18
  print *, "    si, sj, sk  are the element sides. "                      ! 19
  print *, "    xv, yv      are the coordinates of the element circumcenter"
  print *, "    marker      is the side boundary marker"                  ! 20
  print *, ""                                                             ! 21
  print *, "Side file (name.s) has the following format:"                 ! 22
  print *, ""                                                             ! 23
  print *, "  first line:             <n_side> "                          ! 24
  print *, "  following n_side lines: <side:> <c> <d> <ea> <eb> <marker> "! 25

  ! Page break
  write(*, "(a)", advance = "no")  "Press ENTER to continue!";  read *

  print *, ""                                                             !  1
  print *, "  last two lines:         comments inserted by the program "  !  2
  print *, ""                                                             !  3
  print *, "  where:"                                                     !  4
  print *, "    n_side  is the number of sides"                           !  5
  print *, "    c,  d   are the starting and ending node of the side,"    !  6
  print *, "    ea, eb  are the elements left and right from the side."   !  7
  print *, ""                                                             !  8
  print *, "Note: If eb equals to -1, it means that the right element "   !  9
  print *, "      doesn''t exists, i.e. the side is on the boundary !"    ! 10
  print *, ""

  end subroutine
