program ghb_combine

character(1024), allocatable  :: files(:)
integer, allocatable          :: ifile(:)
integer, allocatable          :: maxpkg(:)
integer, allocatable          :: itmp(:)

character(1024)               :: line
character(256) , allocatable  :: lines(:,:)

integer, allocatable          :: ifileold(:)
integer                       :: nfile

integer                       :: i, istp, i1, i2, n1, n2, nstp, maxnew

nfile = command_argument_count() - 2
allocate(files(0:nfile)) ! 0 for the new well file
allocate(ifile(0:nfile)) ! 0 for the new well file
allocate(maxpkg(nfile))  ! maximum boundary conditions in each file
allocate(itmp(nfile))    ! ITMP

call get_command_argument(1, line); read(line, *) nstp
call get_command_argument(2, files(0))
open(newunit=ifile(0), file=trim(files(0)), status='replace')

line = 'Combine '
do i = 1, nfile
  call get_command_argument(i+2, files(i))
  open(newunit=ifile(i-2), file=trim(files(i)), status='old')
  call skipcomment(ifile(i))
  read(ifile(i), *) maxpkg(i)

  if (i==3) then
    line = trim(line)//trim(files(i-2))
  else
    line = trim(line)//' and '//trim(files(i-2))
end do
line = trim(line)//' to '//trim(files(0))
print*, trim(line)



open(newunit=ifile(0), file=trim(file3), status='replace')
write(ifile(0), *) n1+n2, 54, 'NOPRINT'

do istp=1, nstp
  read(ifile1, *) i1
  read(ifile2, *) i2
  if (i1>=0) n1 = i1
  if (i2>=0) n2 = i2

  write(ifile(0), *) n1+n2

  do i =1, i1
    read(ifile1, '(A)') line1(i)
  end do
  do i =1, n1
    write(ifile(0), '(A)') trim(line1(i))
  end do

  do i =1, i2
    read(ifile2, '(A)') line2(i)
  end do
  do i =1, n2
    write(ifile(0), '(A)') trim(line2(i))
  end do
end do
close(ifile1)
close(ifile2)
close(ifile(0))
end program
