!==============================================================================!
  subroutine Discretize(matrix, sol, rhs, dt)
!------------------------------------------------------------------------------!
!   Determines the topology of the system matrix.                              !
!   It relies only on SideC structure. Try to keep it that way.                !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod,     only: RP, OFF
  use Matrix_Mod,    only: Matrix_Type
  use Vector_Mod,    only: Vector_Type
  use Mesh_Mod,      only: Mesh_Type, Node_Type, Mesh_Mod_Area
  use Cpu_Timer_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Matrix_Type) :: matrix
  type(Vector_Type) :: sol
  type(Vector_Type) :: rhs
  real(RP)          :: dt
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: s, i, j, k, e
  integer                  :: c, d    ! nodes c and d
  integer                  :: ea, eb  ! elements
  real(RP)                 :: xa, ya, xb, yb, xc, yc, xd, yd, xs, ys
  real(RP)                 :: delta, la, lb, a12, a21, mu_a, mu_b, tot_area
  type(Mesh_Type), pointer :: mesh
  type(Node_Type)          :: p_ij, p_jk, p_ki, p_vr
  logical, save            :: first_entry = .true.
!==============================================================================!

  call Cpu_Timer_Mod_Start('Discretize')

  ! Get pointer to the mesh
  mesh => matrix % pnt_mesh

  !-------------------------------------------------------------------!
  !                                                                   !
  !   Calculate nodal areas (these will be used for unsteady terms)   !
  !                                                                   !
  !-------------------------------------------------------------------!
  if(first_entry) then
    do e = 0, mesh % n_elem - 1

      ! Extract element nodes
      i = mesh % elem(e) % i
      j = mesh % elem(e) % j
      k = mesh % elem(e) % k

      ! Point in the element center
      p_vr % x = mesh % elem(e) % xv
      p_vr % y = mesh % elem(e) % yv

      ! Work out points at side centers
      p_ij % x = (mesh % node(i) % x + mesh % node(j) % x) * 0.5
      p_ij % y = (mesh % node(i) % y + mesh % node(j) % y) * 0.5
      p_jk % x = (mesh % node(j) % x + mesh % node(k) % x) * 0.5
      p_jk % y = (mesh % node(j) % y + mesh % node(k) % y) * 0.5
      p_ki % x = (mesh % node(k) % x + mesh % node(i) % x) * 0.5
      p_ki % y = (mesh % node(k) % y + mesh % node(i) % y) * 0.5

      mesh % elem(e) % area_i = Mesh_Mod_Area(p_vr, p_ki, mesh % node(i))  &
                              + Mesh_Mod_Area(p_vr, mesh % node(i), p_ij)

      mesh % elem(e) % area_j = Mesh_Mod_Area(p_vr, p_ij, mesh % node(j))  &
                              + Mesh_Mod_Area(p_vr, mesh % node(j), p_jk)

      mesh % elem(e) % area_k = Mesh_Mod_Area(p_vr, p_jk, mesh % node(k))  &
                              + Mesh_Mod_Area(p_vr, mesh % node(k), p_ki)
    end do

    tot_area = 0.0
    do e = 0, mesh % n_elem - 1
      tot_area = tot_area + mesh % elem(e) % area_i    &
                          + mesh % elem(e) % area_j    &
                          + mesh % elem(e) % area_k
    end do
    print *, "Total area = ", tot_area
  end if

  !-----------------------------------------!
  !                                         !
  !   Create system matrix on first entry   !
  !                                         !
  !-----------------------------------------!

  !------------------------------------------------------!
  !   Compute diffusion coefficients for system matrix   !
  !------------------------------------------------------!
  matrix % val(:) = 0.0
  do s = 0, mesh % n_side-1

    ! Distance between two nodes
    c  = mesh % side(s) % c
    d  = mesh % side(s) % d
    xc = mesh % node(c) % x
    yc = mesh % node(c) % y
    xd = mesh % node(d) % x
    yd = mesh % node(d) % y
    delta = sqrt( (xc-xd)**2 + (yc-yd)**2 )

    ! Side mid-point
    xs = 0.5 * (xc + xd)
    ys = 0.5 * (yc + yd)

    ! Voronoi point
    ea = mesh % side(s) % ea
    eb = mesh % side(s) % eb

    if(ea .eq. OFF) then
      print *, "This shouldn't have happened"
      stop
    end if

    xa = mesh % elem(ea) % xv
    ya = mesh % elem(ea) % yv
    la = sqrt( (xs-xa)**2 + (ys-ya)**2 )
    mu_a = mesh % mater(mesh % elem(ea) % material) % mu

    if(eb .ne. OFF) then
      xb = mesh % elem(eb) % xv
      yb = mesh % elem(eb) % yv
      lb = sqrt( (xs-xb)**2 + (ys-yb)**2 )
      mu_b = mesh % mater(mesh % elem(eb) % material) % mu
    else
      lb   = 0.0
      mu_b = 0.0
    end if

    ! Bare bone coefficient
    matrix % fc(s) = mu_a * la / delta  &
                   + mu_a * lb / delta

    ! Should be multiplied with physical properties somehow
    a12 = matrix % fc(s)
    a21 = matrix % fc(s)

    ! Insert into matrix
    matrix % val(matrix % dia(c))  = matrix % val(matrix % dia(c))  + a12
    matrix % val(matrix % dia(d))  = matrix % val(matrix % dia(d))  + a21
    matrix % val(matrix % pos(1,s)) = matrix % val(matrix % pos(1,s)) - a12
    matrix % val(matrix % pos(2,s)) = matrix % val(matrix % pos(2,s)) - a21
  end do

  !----------------------------------------!
  !   Unsteady term in the system matrix   !
  !----------------------------------------!
  do e = 0, mesh % n_elem - 1

    ! Extract element nodes
    i = mesh % elem(e) % i
    j = mesh % elem(e) % j
    k = mesh % elem(e) % k

    matrix % val(matrix % dia(i)) = matrix % val(matrix % dia(i))  &
                                  + mesh % elem(e) % area_i / dt

    matrix % val(matrix % dia(j)) = matrix % val(matrix % dia(j))  &
                                  + mesh % elem(e) % area_j / dt

    matrix % val(matrix % dia(k)) = matrix % val(matrix % dia(k))  &
                                  + mesh % elem(e) % area_k / dt
  end do

  !-----------------------------!
  !                             !
  !   Fill up right hand side   !
  !                             !
  !-----------------------------!

  !------------------------------------------!
  !   Unsteady term in the right hand side   !
  !------------------------------------------!
  rhs % val(:) = 0.0
  do e = 0, mesh % n_elem - 1

    ! Extract element nodes
    i = mesh % elem(e) % i
    j = mesh % elem(e) % j
    k = mesh % elem(e) % k

    rhs % val(i) = rhs % val(i) + sol % val(i) * mesh % elem(e) % area_i / dt
    rhs % val(j) = rhs % val(j) + sol % val(j) * mesh % elem(e) % area_j / dt
    rhs % val(k) = rhs % val(k) + sol % val(k) * mesh % elem(e) % area_k / dt
  end do

  !-----------------------------------------!
  !   Sourcey term in the right hand side   !
  !-----------------------------------------!
  do e = 0, mesh % n_elem - 1

    ! Extract element nodes
    i = mesh % elem(e) % i
    j = mesh % elem(e) % j
    k = mesh % elem(e) % k

    rhs % val(i) = rhs % val(i)                                    &
                 + mesh % mater(mesh % elem(e) % material) % j  &
                 * mesh % elem(e) % area_i / dt

    rhs % val(j) = rhs % val(j)                                    &
                 + mesh % mater(mesh % elem(e) % material) % j  &
                 * mesh % elem(e) % area_j / dt

    rhs % val(k) = rhs % val(k)                                    &
                 + mesh % mater(mesh % elem(e) % material) % j  &
                 * mesh % elem(e) % area_k / dt
  end do

  !-----------------------------!
  !   Set boundary conditions   !
  !-----------------------------!
  do s = 0, mesh % n_side-1

    c  = mesh % side(s) % c
    d  = mesh % side(s) % d

    if(mesh % side(s) % mark .ne. 0) then

      ! Insert Dirichlet boundary conditions (for the time being)
      if(mesh % bound(mesh % side(s) % mark) % type .eq. "D" .or.  &
         mesh % bound(mesh % side(s) % mark) % type .eq. "d") then

        do j = matrix % row(c), matrix % row(c + 1) - 1
          matrix % val(j) = 0.0 
        end do
        do j = matrix % row(d), matrix % row(d + 1) - 1
          matrix % val(j) = 0.0 
        end do
        matrix % val(matrix % dia(c)) = 1.0
        matrix % val(matrix % dia(d)) = 1.0

        rhs % val(c) = mesh % bound(mesh % side(s) % mark) % value
        rhs % val(d) = mesh % bound(mesh % side(s) % mark) % value
      end if

    end if

  end do

  first_entry = .false.

  call Cpu_Timer_Mod_Stop('Discretize')

  end subroutine
