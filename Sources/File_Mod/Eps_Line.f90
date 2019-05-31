!==============================================================================!
  subroutine File_Mod_Eps_Line(x1, y1, x2, y2, color)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real(RP) :: x1, y1, x2, y2
  integer  :: color
!==============================================================================!

  write(FU, "(i2, a)", advance="no")  1, " slw "  ! line width = 1

  select case(color)
    case(0)
      write(FU, "(a)", advance="no")  " 0.00  0.00  0.00  srgb "  ! black
    case(1)
      write(FU, "(a)", advance="no")  " 0.00  0.00  1.00  srgb "  ! blue
    case(2)
      write(FU, "(a)", advance="no")  " 0.00  1.00  0.00  srgb "  ! green
    case(3)
      write(FU, "(a)", advance="no")  " 1.00  0.00  0.00  srgb "  ! red
    case(4)
      write(FU, "(a)", advance="no")  " 0.00  1.00  1.00  srgb "  ! cyan
    case(5)
      write(FU, "(a)", advance="no")  " 1.00  1.00  0.00  srgb "  ! yellow
    case(6)
      write(FU, "(a)", advance="no")  " 1.00  0.00  1.00  srgb "  ! magenta
    case default
      write(FU, "(a)", advance="no")  " 1.00  1.00  1.00  srgb "  ! white
  end select

  write(FU, "(2f12.5, a)", advance="no")  x1, y1, " m "
  write(FU, "(2f12.5, a)", advance="no")  x2, y2, " l "

  ! Stroke
  write(FU, "(a)")  " s "

  end subroutine
