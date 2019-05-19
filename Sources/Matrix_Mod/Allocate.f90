!==============================================================================!
  subroutine Matrix_Mod_Allocate(matrix, mesh)
!------------------------------------------------------------------------------!
!   Determines the topology of the system matrix.                              !
!   It relies only on SideC structure. Try to keep it that way.                !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Matrix_Type)       :: matrix
  type(Mesh_Type), target :: mesh
!-----------------------------------[Locals]-----------------------------------!
  integer              :: n, s, pos, pos1, pos2, nz
  integer              :: c, d    ! nodes c and d
  integer              :: i, j    ! neighbour 1 and 2
  integer, allocatable :: stencw(:)
!==============================================================================!

  ! Store pointer to the mesh
  matrix % pnt_mesh => mesh

  ! Allocate memory for matrix
  allocate(matrix % row(   0:mesh % n_node  ));  matrix % row = 0
  allocate(matrix % dia(   0:mesh % n_node-1));  matrix % dia = 0
  allocate(matrix % fc (   0:mesh % n_side-1));  matrix % fc  = 0.0
  allocate(matrix % pos(2, 0:mesh % n_side-1));  matrix % pos = 0

  ! Allocate memory for local array
  allocate(stencw(0:mesh % n_node-1)); stencw = 1

  ! Initialize number of nonzeros
  matrix % nonzeros = 0

  ! Compute stencils widths
  do s = 0, mesh % n_side-1
    c = mesh % side(s) % c
    d = mesh % side(s) % d
    stencw(c) = stencw(c) + 1
    stencw(d) = stencw(d) + 1
  end do

  ! Count the nonzero entries and allocate the memory for the array 
  nz = 0
  do n = 0, mesh % n_node-1
    nz = nz + stencw(n)
  end do

  matrix % nonzeros = nz + 1
  allocate(matrix % val(0:nz)); matrix % val = 0. ! it reffers to matrix % row+1
  allocate(matrix % col(0:nz)); matrix % col = 0  ! it reffers to matrix % row+1
  allocate(matrix % mir(0:nz)); matrix % mir = 0  ! it reffers to matrix % row+1

  ! Form matrix % row and diagonal only formation of matrix % col
  matrix % row(0) = 0
  do n = 0, mesh % n_node-1
    matrix % row(n + 1) = matrix % row(n) + stencw(n)
    matrix % col(matrix % row(n)) = n   ! it is first to its own self
    stencw(n) = 1                       ! reset stencw
  end do 

  ! Extend matrix % col entries with off-diagonal terms
  do s = 0, mesh % n_side-1
    c = mesh % side(s) % c
    d = mesh % side(s) % d
    matrix % col(matrix % row(c) + stencw(c)) = d
    matrix % col(matrix % row(d) + stencw(d)) = c
    stencw(c) = stencw(c) + 1
    stencw(d) = stencw(d) + 1
  end do

  ! Sort matrix % col to make them nice and neat
  ! and also locate the position of diagonal
  do n = 0, mesh % n_node-1
    call Sort_Mod_Int(matrix % col(matrix % row(n) :  &
                                   matrix % row(n) + stencw(n) - 1))
    do pos = matrix % row(n),matrix % row(n+1)-1
      if(matrix % col(pos) .eq. n) matrix % dia(n) = pos
    end do
  end do 

  ! Find mirror positions
  do c = 0, mesh % n_node-1
    do pos1 = matrix % row(c), matrix % row(c + 1) - 1
      i = matrix % col(pos1)  ! at this point you have c and i

      ! Inner loop (it might probably go from 1 to c-1
      d = i
      do pos2 = matrix % row(d), matrix % row(d + 1) - 1
        j = matrix % col(pos2)  ! at this point you have d and j

        if(j .eq. c) then
          matrix % mir(pos1) = pos2
          matrix % mir(pos2) = pos1
          goto 2  ! done with the inner loop, get out
        end if
      end do

2     continue
    end do
  end do

  ! Connect faces with matrix entries
  do s = 0, mesh % n_side-1
    c = mesh % side(s) % c
    d = mesh % side(s) % d

    ! Where is matrix(c,d) and ...
    do n = matrix % row(c), matrix % row(c+1) - 1
      if(matrix % col(n) .eq. d) matrix % pos(1, s) = n
    end do

    ! Where is matrix(d,c) and ...
    do n = matrix % row(d),matrix % row(d+1) - 1
      if(matrix % col(n) .eq. c) matrix % pos(2, s) = n
    end do
  end do

  deallocate(stencw)

  end subroutine
