subroutine skipcomment(ifile)
integer         :: ifile
character(1024) :: line
do
  read(ifile, '(A)') line
  line = adjustl(line)
  if (line(1:1) /= '#' )  exit
end do
backspace(ifile)
end subroutine