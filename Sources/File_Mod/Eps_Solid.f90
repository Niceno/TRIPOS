!==============================================================================!
  subroutine File_Mod_Eps_Solid(x1, y1, x2, y2, x3, y3, color)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real(RP) :: x1, y1, x2, y2, x3, y3
  integer  :: color
!==============================================================================!

  write(FU, "(i2,     a)", advance="no")  1,     " slw "  ! line width = 1
  write(FU, "(2f12.5, a)", advance="no")  x1, y1, " m "
  write(FU, "(2f12.5, a)", advance="no")  x2, y2, " l "
  write(FU, "(2f12.5, a)", advance="no")  x3, y3, " l cp "

  ! Colors are cool shades of blue for materials
  ! Check them here: http://gohtx.com/acadcolors.php
  select case(color)
    case(0)
      write(FU, "(a)", advance="no")  " 0.667 1.000 0.749 srgb "  ! acad 101
    case(1)
      write(FU, "(a)", advance="no")  " 0.667 1.000 0.918 srgb "  ! acad 121
    case(2)
      write(FU, "(a)", advance="no")  " 0.667 0.918 1.000 srgb "  ! acad 141
    case(3)
      write(FU, "(a)", advance="no")  " 0.667 0.749 1.000 srgb "  ! acad 161
    case(4)
      write(FU, "(a)", advance="no")  " 0.749 0.667 1.000 srgb "  ! acad 181
    case(5)
      write(FU, "(a)", advance="no")  " 0.918 0.667 1.000 srgb "  ! acad 201
    case(6)
      write(FU, "(a)", advance="no")  " 1.000 0.667 0.918 srgb "  ! acad 221
    case default
      write(FU, "(a)", advance="no")  " 1.000 1.000 1.000 srgb "  ! white
  end select

  ! Fill and stroke
  write(FU, "(a)")  " f s "

  end subroutine
