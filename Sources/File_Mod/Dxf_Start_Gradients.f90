!==============================================================================!
  subroutine File_Mod_Dxf_Start_Gradients(phi, comm)
!------------------------------------------------------------------------------!
!   Subroutine to start a .dxf file for plotting gradients.                    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Vector_Type) :: phi
  type(Comm_Type)   :: comm
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: l, e, len
  character(len=CL)        :: iso_layer
  character(len=CL)        :: dxf_gradients_name = ""
  real(RP)                 :: val, max_mag, del_mag
  real(RP), allocatable    :: phi_x(:), phi_y(:)
  type(Mesh_Type), pointer :: mesh
!==============================================================================!

  ! Take aliases
  mesh => phi % pnt_mesh

  ! Compute gradients
  allocate(phi_x(0:mesh % n_elem-1))  ! dphi / dx
  allocate(phi_y(0:mesh % n_elem-1))  ! dphi / dy
  call File_Mod_Gradients(mesh,                      &
                          phi,                       &
                          phi_x(0:mesh % n_elem-1),  &
                          phi_y(0:mesh % n_elem-1))

  ! Find max vector magnitude
  max_mag = 0.0
  do e = 0, mesh % n_elem-1
    max_mag = max(max_mag, sqrt(phi_x(e)**2 + phi_y(e)**2))
  end do
  del_mag = max_mag / N_ISOLINE

  if(comm % messages .eq. ON) print *, "Plotting the gradients in dxf format"

  ! Form file name
  len = len_trim(comm % problem_name)
  dxf_gradients_name(    1:len  ) = comm % problem_name(1:len)
  dxf_gradients_name(len+1:len+6) = ".g.dxf"

  ! ... and open it
  open(unit=FU, file=dxf_gradients_name)

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
    val = del_mag * 0.5 + del_mag * l

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
