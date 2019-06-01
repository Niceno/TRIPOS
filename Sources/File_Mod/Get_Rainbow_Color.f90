!==============================================================================!
  subroutine File_Mod_Get_Rainbow_Color(level, r, g, b)
!------------------------------------------------------------------------------!
!   Colors are cool shades of red to orane for boundary markers                !
!   Check them here: http://gohtx.com/acadcolors.php                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer  :: level
  real(RP) :: r, g, b
!==============================================================================!

  ! Set r, g and b vallues   170 - l*10
  select case(level)
    case(0)  ! acad 170
                         r =   0.0;  g =   0.0;  b = 255.0
    case(1)  ! acad 160
                         r =   0.0;  g =  63.0;  b = 255.0
    case(2)  ! acad 150
                         r =   0.0;  g = 127.0;  b = 255.0
    case(3)  ! acad 140
                         r =   0.0;  g = 192.0;  b = 255.0
    case(4)  ! acad 130
                         r =   0.0;  g = 255.0;  b = 255.0
    case(5)  ! acad 120
                         r =   0.0;  g = 255.0;  b = 191.0
    case(6)  ! acad 110
                         r =   0.0;  g = 255.0;  b = 127.0
    case(7)  ! acad 100
                         r =   0.0;  g = 255.0;  b =  63.0
    case(8)  ! acad  90
                         r =   0.0;  g = 255.0;  b =   0.0
    case(9)  ! acad  80
                         r =  63.0;  g = 255.0;  b =   0.0
    case(10) ! acad  70
                         r = 127.0;  g = 255.0;  b =   0.0
    case(11) ! acad  60
                         r = 192.0;  g = 255.0;  b =   0.0
    case(12) ! acad  50
                         r = 255.0;  g = 255.0;  b =   0.0
    case(13) ! acad  40
                         r = 255.0;  g = 191.0;  b =   0.0
    case(14) ! acad  30
                         r = 255.0;  g = 127.0;  b =   0.0
    case(15) ! acad  20
                         r = 255.0;  g =  63.0;  b =   0.0
    case(16) ! acad  10
                         r = 255.0;  g =   0.0;  b =   0.0
  end select

  ! Normalize them
  r = r / 255.0
  g = g / 255.0
  b = b / 255.0

  end subroutine
