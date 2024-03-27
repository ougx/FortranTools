program ghb_combine

character(1024) :: file1,  file2,  file3,  line
character(1024), allocatable :: line1(:), line2(:)
integer         :: ifile1, ifile2, ifile3
integer         :: i, istp, i1, i2, n1, n2, nstp


call get_command_argument(1, line)
read(line, *) nstp

call get_command_argument(2, file1)
call get_command_argument(3, file2)
call get_command_argument(4, file3)

print*, 'Combine '//trim(file1)//' and '//trim(file2)//' to '//trim(file3)

open(newunit=ifile1, file=trim(file1), status='old')
call skipcomment(ifile1)
read(ifile1, *) n1
allocate(line1(n1))

open(newunit=ifile2, file=trim(file2), status='old')
call skipcomment(ifile2)
read(ifile2, *) n2
allocate(line2(n2))

open(newunit=ifile3, file=trim(file3), status='replace')
write(ifile3, *) n1+n2, 54, 'NOPRINT'

do istp=1, nstp
  read(ifile1, *) i1
  read(ifile2, *) i2
  if (i1>=0) n1 = i1
  if (i2>=0) n2 = i2

  write(ifile3, *) n1+n2

  do i =1, i1
    read(ifile1, '(A)') line1(i)
  end do
  do i =1, n1
    write(ifile3, '(A)') trim(line1(i))
  end do

  do i =1, i2
    read(ifile2, '(A)') line2(i)
  end do
  do i =1, n2
    write(ifile3, '(A)') trim(line2(i))
  end do
end do
close(ifile1)
close(ifile2)
close(ifile3)
end program
