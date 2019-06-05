!==============================================================================!
  subroutine File_Mod_Eps_End
!------------------------------------------------------------------------------!
!   Subroutine to end an .eps file.                                            !
!==============================================================================!

  write(FU, "(a)")       "%%Trailer"
  write(FU, "(a, 4i7)")  "%%BoundingBox: ",        &
                         floor  (xmin * scl - 5),  &
                         floor  (ymin * scl - 5),  &
                         ceiling(xmax * scl + 5),  &
                         ceiling(ymax * scl + 5)
  write(FU, "(a)")       "%%EOF"

  close(FU)

  end subroutine
