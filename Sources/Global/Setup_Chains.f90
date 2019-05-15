!==============================================================================!
  subroutine Easymesh_Setup_Chains
!----------------------------------[Modules]-----------------------------------!
  use Const_Mod
  use Mesh_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer  :: n, s
  real(RP) :: xo, xn, yo, yn, l, dlm
!==============================================================================!

  !----------------------!
  !   Count the chains   !
  !----------------------!
  n_chain = 0
  chain(n_chain) % s0 = 0

  do s = 0, n_segment-1

    ! Increase the inserted count for points
    point(segment(s) % n0) % inserted = point(segment(s) % n0) % inserted + 1
    point(segment(s) % n1) % inserted = point(segment(s) % n1) % inserted + 1

    ! Store each segment's chain number
    segment(s) % chain = n_chain

    ! If there are more than one chains, this will work
    if( segment(s) % n1 .ne. segment(s+1) % n0) then
      chain(n_chain) % s1 = s
      n_chain = n_chain + 1
      chain(n_chain) % s0 = s + 1
    end if

  end do

  ! If there is one chain only, the above will not work
  if(n_chain .eq. 0) then
    chain(n_chain) % s0 = 0
    chain(n_chain) % s1 = n_segment-1
    n_chain = 1
  end if

  !-------------------------------------!
  !   Correct the spacing on segments   !
  !-------------------------------------!
  do s = 0, n_segment-1

    xo = point(segment(s) % n0) % x;  yo = point(segment(s) % n0) % y
    xn = point(segment(s) % n1) % x;  yn = point(segment(s) % n1) % y

    l = sqrt((xn-xo)**2 + (yn-yo)**2)

    if(                                                                  &
        ((point(segment(s) % n0) % f + point(segment(s) % n1) % f) > l)  &
        .and.                                                            &
        ( segment(s) % n0 .ne. segment(s) % n1 )                         &
      ) then
      point(segment(s) % n0) % f = min(point(segment(s) % n0) % f, l)
      point(segment(s) % n1) % f = min(point(segment(s) % n1) % f, l)
    end if

  end do

  !----------------------------------!
  !   Count nodes on each segments   !
  !----------------------------------!
  do s = 0, n_segment-1

    xo = point(segment(s) % n0) % x;  yo = point(segment(s) % n0) % y
    xn = point(segment(s) % n1) % x;  yn = point(segment(s) % n1) % y

    l = sqrt((xn-xo)**2 + (yn-yo)**2)

    if(                                                                   &
        ((point(segment(s) % n0) % f + point(segment(s) % n1) % f) <= l)  &
      ) then
      dlm  =0.5 * (  point(segment(s) % n0) % f  &
                   + point(segment(s) % n1) % f)
      segment(s) % n = ceiling(l/dlm)
    else
      segment(s) % n = 1;
    end if

  end do

  !---------------------------!
  !   Determine chain types   !
  !---------------------------!
  do n = 0, n_chain-1
    if( segment(chain(n) % s0) % n0 .eq. segment(chain(n) % s1) % n1 )  &
      chain(n) % type = CLOSED

    if( segment(chain(n) % s0) % n0 .ne. segment(chain(n) % s1) % n1 )  &
      chain(n) % type = OPEN

    if( (point(segment(chain(n) % s0) % n0) % inserted .eq. 1) .and.  &
        (point(segment(chain(n) % s1) % n1) % inserted .eq. 1) )      &
      chain(n) % type = INSIDE

  end do

  end subroutine
