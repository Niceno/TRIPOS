!==============================================================================!
  subroutine Mesh_Mod_Swap_Side(mesh, s)
!*-----------------------------------------------------------------------------!
!  This function calculates radii of inscribed and circumscribed circle        !
!  for a given element (int e)                                                 !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type), target :: mesh
  integer                 :: s
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: a, b, c, d, ea, eb
  integer                  :: eac, ead, ebc, ebd
  integer                  :: sad, sac, sbc, sbd
  real(RP)                 :: sx, sy
  type(Node_Type), pointer :: node(:)
  type(Elem_Type), pointer :: elem(:)
  type(Side_Type), pointer :: side(:)
!==============================================================================!

  ! Take aliases
  node => mesh % node
  elem => mesh % elem
  side => mesh % side

  ea = side(s) % ea
  eb = side(s) % eb
  a  = side(s) % a
  b  = side(s) % b
  c  = side(s) % c
  d  = side(s) % d

  if(elem(ea) % ei .eq. eb) then
    ead = elem(ea) % ej;  eac = elem(ea) % ek
    sad = elem(ea) % sj;  sac = elem(ea) % sk
  end if
  if(elem(ea) % ej .eq. eb) then
    ead = elem(ea) % ek;  eac = elem(ea) % ei
    sad = elem(ea) % sk;  sac = elem(ea) % si
  end if   
  if(elem(ea) % ek .eq. eb) then
    ead = elem(ea) % ei;  eac = elem(ea) % ej
    sad = elem(ea) % si;  sac = elem(ea) % sj
  end if

  if(elem(eb) % ei .eq. ea) then
    ebc = elem(eb) % ej;  ebd = elem(eb) % ek
    sbc = elem(eb) % sj;  sbd = elem(eb) % sk
  end if
  if(elem(eb) % ej .eq. ea) then
    ebc = elem(eb) % ek;  ebd = elem(eb) % ei
    sbc = elem(eb) % sk;  sbd = elem(eb) % si
  end if
  if(elem(eb) % ek .eq. ea) then
    ebc = elem(eb) % ei;  ebd = elem(eb) % ej
    sbc = elem(eb) % si;  sbd = elem(eb) % sj
  end if

  elem(ea) % i  = a;    elem(ea) % j  = b;    elem(ea) % k  = d
  elem(ea) % ei = ebd;  elem(ea) % ej = ead;  elem(ea) % ek = eb
  elem(ea) % si = sbd;  elem(ea) % sj = sad;  elem(ea) % sk = s

  elem(eb) % i  = a;    elem(eb) % j  = c;    elem(eb) % k  = b
  elem(eb) % ei = ebc;  elem(eb) % ej = ea;   elem(eb) % ek = eac
  elem(eb) % si = sbc;  elem(eb) % sj = s;    elem(eb) % sk = sac

  if(eac .ne. -1) then
    if(elem(eac) % ei .eq. ea) elem(eac) % ei = eb
    if(elem(eac) % ej .eq. ea) elem(eac) % ej = eb
    if(elem(eac) % ek .eq. ea) elem(eac) % ek = eb
  end if

  if(ebd .ne. -1) then
    if(elem(ebd) % ei .eq. eb) elem(ebd) % ei = ea
    if(elem(ebd) % ej .eq. eb) elem(ebd) % ej = ea
    if(elem(ebd) % ek .eq. eb) elem(ebd) % ek = ea
  end if

  if(side(sad) % ea .eq. ea) side(sad) % a = b
  if(side(sad) % eb .eq. ea) side(sad) % b = b

  if(side(sbc) % ea .eq. eb) side(sbc) % a = a
  if(side(sbc) % eb .eq. eb) side(sbc) % b = a

  if(side(sbd) % ea .eq. eb) then
    side(sbd) % ea = ea
    side(sbd) % a  = a
  end if
  if(side(sbd) % eb .eq. eb) then
    side(sbd) % eb = ea
    side(sbd) % b  = a
  end if
 
  if(a<b) then
    side(s) % c  = a;   side(s) % d  = b
    side(s) % a  = d;   side(s) % b  = c
    side(s) % ea = ea;  side(s) % eb = eb
  else
    side(s) % c  = b;   side(s) % d  = a
    side(s) % a  = c;   side(s) % b  = d
    side(s) % ea = eb;  side(s) % eb = ea
  end if

  sx = node(side(s) % c) % x - node(side(s) % d) % x
  sy = node(side(s) % c) % y - node(side(s) % d) % y
  side(s) % s = sqrt(sx**2 + sy**2)

  if(side(sac) % ea .eq. ea) then
    side(sac) % ea = eb
    side(sac) % a  = b
  end if
  if(side(sac) % eb .eq. ea) then
    side(sac) % eb = eb
    side(sac) % b  = b
  end if
 
  if(side(sad) % ea .eq. ea)  side(sad) % a = b
  if(side(sad) % eb .eq. ea)  side(sad) % b = b

  if(side(sbc) % ea .eq. eb)  side(sbc) % a = a
  if(side(sbc) % eb .eq. eb)  side(sbc) % b = a

  if(side(sbd) % ea .eq. eb) then
    side(sbd) % ea = ea
    side(sbd) % a  = a
  end if
  if(side(sbd) % eb .eq. eb) then
    side(sbd) % eb = ea
    side(sbd) % b  = a
  end if

  call Mesh_Mod_Circles(mesh, ea)
  call Mesh_Mod_Circles(mesh, eb)

  end subroutine
