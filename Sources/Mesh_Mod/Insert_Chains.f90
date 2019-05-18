!==============================================================================!
  subroutine Mesh_Mod_Insert_Chains(mesh)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: c, m, mc, n, s
  real(RP)                 :: xmax, xmin, ymax, ymin
  real(RP)                 :: xo, xn, yo, yn, xt, yt, gab
  real(RP)                 :: l, lx, ly, l_tot, ddl, dlm
  integer,         pointer :: nn, np, ns, ne, nc
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
  type(Node_Type), pointer :: point(:)
  type(Chai_Type), pointer :: chain(:)
  type(Segm_Type), pointer :: segment(:)
!==============================================================================!

  ! Take aliases
  nn      => mesh % n_node
  ne      => mesh % n_elem
  np      => mesh % n_point
  ns      => mesh % n_side
  nc      => mesh % n_chain
  node    => mesh % node
  elem    => mesh % elem
  side    => mesh % side
  point   => mesh % point
  chain   => mesh % chain
  segment => mesh % segment

  !-------------------------------------!
  !   Work out xmax, xmin, ymax, ymin   !
  !-------------------------------------!
  xmax = -GREAT;  ymax = -GREAT
  xmin = +GREAT;  ymin = +GREAT
  do n = 1, np-1
    xmax = max(xmax, point(n) % x); ymax = max(ymax, point(n) % y);
    xmin = min(xmin, point(n) % x); ymin = min(ymin, point(n) % y);
  end do

  xt = 0.5 * (xmax + xmin)
  yt = 0.5 * (ymax + ymin)
  gab = max((xmax-xmin), (ymax-ymin))

  !-----------------------------!
  !   Start with big triangle   !
  !-----------------------------!

  ! First three nodes
  nn = 3
  node(2) % x = xt;              node(2) % y = yt + 2.8 * gab
  node(0) % x = xt - 2.0 * gab;  node(0) % y = yt - 1.4 * gab
  node(1) % x = xt + 2.0 * gab;  node(1) % y = yt - 1.4 * gab
  node(2) % inserted = 2
  node(1) % inserted = 2
  node(0) % inserted = 2
  node(2) % next = 1
  node(1) % next = 0
  node(0) % next = 2

  ! First three elements
  ne = 1
  elem(0) % i   =0;  elem(0) % j =  1;  elem(0) % k =  2
  elem(0) % ei=OFF;  elem(0) % ej=OFF;  elem(0) % ek=OFF
  elem(0) % si=  1;  elem(0) % sj=  2;  elem(0) % sk=  0

  ! First three sides
  ns = 3
  side(0) % c = 0;  side(0) % d = 1;  side(0) % a = 2;  side(0) % b = OFF
  side(1) % c = 1;  side(1) % d = 2;  side(1) % a = 0;  side(1) % b = OFF
  side(2) % c = 2;  side(2) % d = 0;  side(2) % a = 1;  side(2) % b = OFF
  side(0) % ea = 0;  side(0) % eb = OFF
  side(1) % ea = 0;  side(1) % eb = OFF
  side(2) % ea = 0;  side(2) % eb = OFF

  ! Initialize new numbers
  do n = 0, np-1
    point(n) % new_numb = OFF
  end do

  !-------------------------------!
  !   Browse through all chains   !
  !      and their segments       !
  !-------------------------------!
  do c = 0, nc-1
    do s = chain(c) % s0, chain(c) % s1

      xo = point(segment(s) % n0) % x;  yo = point(segment(s) % n0) % y
      xn = point(segment(s) % n1) % x;  yn = point(segment(s) % n1) % y

      ! First point
      if( point(segment(s) % n0) % new_numb .eq. OFF ) then

        if(s .eq. chain(c) % s0) then  ! first segment in the chain
          call Mesh_Mod_Insert_Node(mesh,                           &
                                    xo,                             &
                                    yo,                             &
                                    OFF,                            &
                                    OFF,                            &
                                    OFF,                            &
                                    point(segment(s) % n0) % mark,  &
                                    OFF,                            &
                                    OFF);
        else if(s .eq. chain(c) % s1 .and. segment(s) % n .eq. 1) then
          call Mesh_Mod_Insert_Node(mesh,                           &
                                    xo,                             &
                                    yo,                             &
                                    OFF,                            &
                                    nn-1,                           &
                                    segment(s-1) % mark,            &
                                    point(segment(s) % n0) % mark,  &
                                    segment(s) % mark,              &
                                    point(segment(chain(c)%s0)%n0) % new_numb)
        else
          call Mesh_Mod_Insert_Node(mesh,                           &
                                    xo,                             &
                                    yo,                             &
                                    OFF,                            &
                                    nn-1,                           &
                                    segment(s-1) % mark,            &
                                    point(segment(s) % n0) % mark,  &
                                    OFF,                            &
                                    OFF)
        end if

        node(nn-1) % next  = nn       ! n_node-1 is index
        node(nn-1) % chain = segment(s) % chain  ! of inserted node
        node(nn-1) % f     = point(segment(s) % n0) % f

        point(segment(s) % n0) % new_numb = nn - 1

      end if

      lx  = xn - xo
      ly  = yn - yo
      l   = sqrt((xn-xo)**2 + (yn-yo)**2)
      dlm = l/segment(s) % n

      if(point(segment(s) % n0) % f + point(segment(s) % n1) % f <= l) then
        if(point(segment(s) % n0) % f > point(segment(s) % n1) % f) then
          m   = -segment(s) % n / 2
          ddl = (point(segment(s) % n1) % f - dlm) / m
        else
          m   = +segment(s) % n / 2
          ddl = (dlm - point(segment(s) % n0) % f) / m
        end if
      end if

      ! Middle points
      l_tot = 0.0

      if(  (  point(segment(s) % n0) % f               &
            + point(segment(s) % n1) % f ) <= l) then
        do mc = 1, abs(segment(s) % n)-1

          l_tot = l_tot + (dlm - m * ddl)

          if(  point(segment(s) % n0) % f  &
             > point(segment(s) % n1) % f) then
            m = m + 1
            if(m .eq. 0 .and. mod(segment(s) % n, 2) .eq. 0) m = m + 1
          else
            m = m - 1
            if(m .eq. 0 .and. mod(segment(s) % n, 2) .eq. 0) m = m - 1
          end if

          if(s .eq. chain(c) % s1 .and.  &
             mc .eq. (abs(segment(s) % n)-1)) then

            call Mesh_Mod_Insert_Node(mesh,                               &
                                      xo + lx/l*l_tot,                    &
                                      yo + ly/l*l_tot,                    &
                                      OFF,                                &
                                      nn-1,                               &
                                      segment(s) % mark,                  &
                                      segment(s) % mark,                  &
                                      segment(s) % mark,                  &
                                      point(segment(s) % n1) % new_numb)
            node(nn-1) % next = nn

          else if(mc .eq. 1) then
            call Mesh_Mod_Insert_Node(mesh,                               &
                                      xo+lx/l*l_tot,                      &
                                      yo+ly/l*l_tot,                      &
                                      OFF,                                &
                                      point(segment(s) % n0) % new_numb,  &
                                      segment(s) % mark,                  &
                                      segment(s) % mark,                  &
                                      OFF,                                &
                                      OFF)
            node(nn-1) % next = nn

          else
            call Mesh_Mod_Insert_Node(mesh,               &
                                      xo+lx/l*l_tot,      &
                                      yo+ly/l*l_tot,      &
                                      OFF,                &
                                      nn-1,               &
                                      segment(s) % mark,  &
                                      segment(s) % mark,  &
                                      OFF,                &
                                      OFF)
            node(nn-1) % next = nn

          end if

          node(nn-1) % chain  &
                             = segment(s) % chain;
          node(nn-1) % f      &
                             = 0.5 * (node(nn-2) % f + (dlm-m*ddl))

        end do  ! mc
      end if

      ! Last point
      if( (point(segment(s) % n1) % new_numb .eq. OFF) .and.  &
          (s .eq. chain(c) % s1) ) then
        call Mesh_Mod_Insert_Node(mesh,                           &
                                  xN,                             &
                                  yN,                             &
                                  OFF,                            &
                                  nn-1,                           &
                                  segment(s) % mark,              &
                                  point(segment(s) % n1) % mark,  &
                                  OFF,                            &
                                  OFF)
        node(nn-1) % next  = OFF
        node(nn-1) % chain = segment(s) % chain
        node(nn-1) % f     = point(segment(s) % n1) % f
      end if

      if( chain(c) % type .eq. CLOSED .and. s .eq. chain(c) % s1)  &
        node(nn-1) % next = point(segment(chain(c) % s0) % n0) % new_numb

      if( chain(c) % type .eq. OPEN .and. s .eq. chain(c) % s1)  &
        node(nn-1) % next = OFF

    end do  ! through segments
  end do    ! though chains

  end subroutine
