!==============================================================================!
  subroutine File_Mod_Dxf_Start_Mesh(mesh, comm)
!------------------------------------------------------------------------------!
!   Subroutine to start a .dxf file.                                           !
!   Note: DXF color codes are described here: http://gohtx.com/acadcolors.php  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  type(Comm_Type)         :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: e, l, s
  integer                  :: max_bnd, max_mat, len
  character(len=CL)        :: bnd_layer, mat_layer
  character(len=CL)        :: dxf_mesh_name = ""
  integer,         pointer :: ne, ns
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  ! Take aliases
  ne   => mesh % n_elem
  ns   => mesh % n_side
  elem => mesh % elem
  side => mesh % side

  !-------------------------------!
  !   Count boundary conditions   !
  !-------------------------------!
  max_bnd  = 0
  do s = 0, ns-1
    max_bnd = max(max_bnd, side(s) % mark)
  end do

  !---------------------!
  !   Count materials   !
  !---------------------!
  max_mat = 0
  do e = 0, ne-1
    max_mat = max(max_mat, elem(e) % material)
  end do

  ! Form file name ...
  len = len_trim(comm % problem_name)
  dxf_mesh_name(    1:len-2) = comm % problem_name(1:len-2)
  dxf_mesh_name(len-1:len+2) = ".dxf"

  ! ... and open it
  open(unit=FU, file=dxf_mesh_name)

  !-------------------!
  !   Define layers   !
  !-------------------!
  write(FU, "(a)")  "0"        ! beginning of new definition
  write(FU, "(a)")  "SECTION"  ! section word

  write(FU, "(a)") "2"         ! what kind of section?
  write(FU, "(a)") "TABLES"    ! tables

  write(FU, "(a)") "0"         ! beginning of new definition
  write(FU, "(a)") "TABLE"     ! we begin to define a table

  write(FU, "(a)") "2"         ! what is in this section?
  write(FU, "(a)") "LAYER"     ! layer(s)

  write(FU, "(a)") "70"        ! layer type flags to be defined
  write(FU, "(a)") "6"         ! and they are 2 + 4 (frozen + locked)

  ! Delaunay and Voronoi
  do l = 1, 2
    write(FU, "(a)") "0"                       ! new defintion
    write(FU, "(a)") "LAYER"                   ! definition of a layer
    write(FU, "(a)") "2"                       ! layer name will follow
    if(l .eq. 1) write(FU, "(a)") "delaunay"   ! layer name
    if(l .eq. 2) write(FU, "(a)") "voronoi"    ! layer name
    write(FU, "(a)") "70"                      ! flags
    write(FU, "(a)") "64"                      ! 64 means it is referenced later
    write(FU, "(a)") "62"                      ! color definition
    if(l .eq. 1) write(FU, "(a)") "5"          ! layer name
    if(l .eq. 2) write(FU, "(a)") "6"          ! layer name
    write(FU, "(a)") "6"                       ! line type (educated guess)
    write(FU, "(a)") "CONTINUOUS"              ! line is continuous
  end do

  ! Materials
  mat_layer(1:11) = "material-00"
  do l = 1, max_mat
    write(mat_layer(10:11), "(i2.2)") l
    write(FU, "(a)") "0"                     ! new defintion
    write(FU, "(a)") "LAYER"                 ! definition of a layer
    write(FU, "(a)") "2"                     ! layer name will follow
    write(FU, "(a)") mat_layer(1:11)         ! layer name
    write(FU, "(a)") "70"                    ! flags
    write(FU, "(a)") "64"                    ! 64 means it is referenced later
    write(FU, "(a)") "62"                    ! color definition
    write(FU, "(i4)") l*20 + 101             ! color
    write(FU, "(a)") "6"                     ! line type (educated guess)
    write(FU, "(a)") "CONTINUOUS"            ! line is continuous
  end do

  ! Boundary conditions
  bnd_layer(1:11) = "boundary-00"
  do l = 1, max_bnd
    write(bnd_layer(10:11), "(i2.2)") l
    write(FU, "(a)") "0"                     ! new defintion
    write(FU, "(a)") "LAYER"                 ! definition of a layer
    write(FU, "(a)") "2"                     ! layer name will follow
    write(FU, "(a)") bnd_layer(1:11)         ! layer name
    write(FU, "(a)") "70"                    ! flags
    write(FU, "(a)") "64"                    ! 64 means it is referenced later
    write(FU, "(a)") "62"                    ! color definition
    write(FU, "(i4)") l*20                   ! color
    write(FU, "(a)") "6"                     ! line type (educated guess)
    write(FU, "(a)") "CONTINUOUS"            ! line is continuous
  end do

  write(FU, "(a)") "0"           ! new defintion
  write(FU, "(a)") "ENDTAB"      ! end of table
  write(FU, "(a)") "0"           ! new definition
  write(FU, "(a)") "ENDSEC"      ! end of section

  ! Start entities
  write(FU, "(a)")  "0"
  write(FU, "(a)")  "SECTION"
  write(FU, "(a)")  "2"
  write(FU, "(a)")  "ENTITIES"

  end subroutine
