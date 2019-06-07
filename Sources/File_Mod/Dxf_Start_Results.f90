!==============================================================================!
  subroutine File_Mod_Dxf_Start_Results(solution, comm)
!------------------------------------------------------------------------------!
!   Subroutine to start a .dxf file.                                           !
!   Note: DXF color codes are described here: http://gohtx.com/acadcolors.php  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Vector_Type), target :: solution
  type(Comm_Type)           :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer           :: l, len
  character(len=CL) :: iso_layer
  character(len=CL) :: dxf_results_name = ""
  real(RP)          :: val, max_val, min_val
!==============================================================================!

  max_val = maxval(solution % val(:))
  min_val = minval(solution % val(:))

  ! Form file name
  len = len_trim(comm % problem_name)
  dxf_results_name(    1:len  ) = comm % problem_name(1:len)
  dxf_results_name(len+1:len+6) = ".r.dxf"

  ! ... and open it
  open(unit=FU, file=dxf_results_name)

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

  ! Mesh (Delaunay)
  write(FU, "(a)") "0"                       ! new defintion
  write(FU, "(a)") "LAYER"                   ! definition of a layer
  write(FU, "(a)") "2"                       ! layer name will follow
  write(FU, "(a)") "mesh"                    ! layer name
  write(FU, "(a)") "70"                      ! flags
  write(FU, "(a)") "64"                      ! 64 means it is referenced later
  write(FU, "(a)") "62"                      ! color definition
  write(FU, "(a)") "8"                       ! color (8 is dark gray; 9 is gray)
  write(FU, "(a)") "6"                       ! line type (educated guess)
  write(FU, "(a)") "CONTINUOUS"              ! line is continuous

  ! Boundary
  write(FU, "(a)") "0"                       ! new defintion
  write(FU, "(a)") "LAYER"                   ! definition of a layer
  write(FU, "(a)") "2"                       ! layer name will follow
  write(FU, "(a)") "boundary"                ! layer name
  write(FU, "(a)") "70"                      ! flags
  write(FU, "(a)") "64"                      ! 64 means it is referenced later
  write(FU, "(a)") "62"                      ! color definition
  write(FU, "(a)") "7"                       ! blaack/white
  write(FU, "(a)") "6"                       ! line type (educated guess)
  write(FU, "(a)") "CONTINUOUS"              ! line is continuous

  ! Isolines (one could thing of naming them after values)
  do l = 0, N_ISOLINE - 1
    val = (min_val+SMALL) + l * ((max_val-min_val)-SMALL)   &
        / (N_ISOLINE-1)

    ! Name layer
    call File_Mod_Name_Layer(iso_layer, l, val)

    write(FU, "(a)") "0"                     ! new defintion
    write(FU, "(a)") "LAYER"                 ! definition of a layer
    write(FU, "(a)") "2"                     ! layer name will follow
    write(FU, "(a)") iso_layer(1:13)         ! layer name
    write(FU, "(a)") "70"                    ! flags
    write(FU, "(a)") "64"                    ! 64 means it is referenced later
    write(FU, "(a)") "62"                    ! color definition
    write(FU, "(i4)") 170 - l*10             ! color
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
