!==============================================================================!
  subroutine File_Mod_Get_Warm_Color(color, r, g, b)
!------------------------------------------------------------------------------!
!   Colors are cool shades of red to orane for boundary markers                !
!   Check them here: http://gohtx.com/acadcolors.php                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer  :: color
  real(RP) :: r, g, b
!==============================================================================!

  ! Set r, g and b vallues
  select case(color)
    case(0)  ! acad   0
                         r =   0.0;  g =   0.0;  b =   0.0
    case(1)  ! acad  20
                         r = 255.0;  g =  63.0;  b =   0.0
    case(2)  ! acad  40
                         r = 255.0;  g = 191.0;  b =   0.0
    case(3)  ! acad  60
                         r = 191.0;  g = 255.0;  b =   0.0
    case(4)  ! acad  80
                         r =  63.0;  g = 255.0;  b =   0.0
    case(5)  ! acad 100
                         r =   0.0;  g = 255.0;  b =  63.0
    case(6)  ! acad 120
                         r =   0.0;  g = 255.0;  b = 191.0
    case default
  end select

  ! Normalize them
  r = r / 255.0
  g = g / 255.0
  b = b / 255.0

  end subroutine
