!==============================================================================!
  subroutine File_Mod_Get_Cool_Color(color, r, g, b)
!------------------------------------------------------------------------------!
!   Colors are cool shades of blue for materials                               !
!   Check them here: http://gohtx.com/acadcolors.php                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer  :: color
  real(RP) :: r, g, b
!==============================================================================!

  ! Set r, g and b vallues
  select case(color)
    case(0)  ! acad 101
                         r = 170.0;  g = 255.0;  b = 191.0
    case(1)  ! acad 121
                         r = 170.0;  g = 255.0;  b = 234.0
    case(2)  ! acad 141
                         r = 170.0;  g = 234.0;  b = 255.0
    case(3)  ! acad 161
                         r = 170.0;  g = 191.0;  b = 255.0
    case(4)  ! acad 181
                         r = 191.0;  g = 170.0;  b = 255.0
    case(5)  ! acad 201
                         r = 234.0;  g = 170.0;  b = 255.0
    case(6)  ! acad 221
                         r = 255.0;  g = 170.0;  b = 234.0
    case default
  end select

  ! Normalize them
  r = r / 255.0
  g = g / 255.0
  b = b / 255.0

  end subroutine
