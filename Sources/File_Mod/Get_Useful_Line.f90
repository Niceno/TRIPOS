!==============================================================================!
  subroutine File_Mod_Get_Useful_Line(u, line)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: u
  character(len=CL) :: line
!-----------------------------------[Locals]-----------------------------------!
  character :: sc, ec
  integer   :: len
!==============================================================================!

  do
    read(u, "(a)", end=1), line
    line = adjustl(line)
    len = len_trim(line)

    ! Only handle non-empty lines
    if(len .gt. 0) then

      sc = line(1:1)      ! starting character
      ec = line(len:len)  ! ending character

      ! Skip comment lines, and blocks of comments
      if(sc .eq. "#") then
        do while(ec .ne. "#")
          read(u, "(a)", end=1), line
          line = adjustl(line)
          len = len_trim(line)
          if(len .gt. 0) then
            ec = line(len:len)    ! ending character
          end if
        end do

      ! If you came to here, you have a useful line
      !  (Yet, you might think of analyzing if ...
      !   ...there are further embedded comments)
      else

        return
      end if  ! if beginning of comment

    end if  ! line longer than zero
  end do

1 continue

  end subroutine
