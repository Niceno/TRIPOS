!==============================================================================!
  subroutine File_Mod_Eps_End
!------------------------------------------------------------------------------!
!   Subroutine to end an .eps file.                                            !
!==============================================================================!

  write(FU, "(a)")          "%%Trailer"
  write(FU, "(a, 4f12.5)")  "%%BoundingBox: ",   &
                            xmin * scl - 5,      &
                            ymin * scl - 5,      &
                            xmax * scl + 5,      &
                            ymax * scl + 5
  write(FU, "(a)")          "%%EOF"

  close(FU)

  end subroutine
