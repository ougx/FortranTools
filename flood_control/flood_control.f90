program flood_control
integer             :: iper1, iper2, kper, kstp, ilay, ifile, ierr, ifileout, ncount
integer             :: nrow, ncol, floodcount
real                :: pertim, totim, floodsum, floodmax, r1,r2,r3
character(len=16  ) :: period1, period2, text, how
character(len=1024) :: hdsfile, topfile
real           , allocatable, dimension(:,:) :: head, top, diff
integer        , allocatable, dimension(:,:) :: ones

period1 = ''
period2 = ''
iper1 = 0
iper2 = 99999
!call get_command_argument(1, how)
call get_command_argument(1, hdsfile)
call get_command_argument(2, topfile)
call get_command_argument(3, period1)
call get_command_argument(4, period2)

if (period1 /= '' .and. period2 /= '') then
  read(period1, *) iper1
  read(period2, *) iper2
end if

floodsum   = 0.0d0
floodcount = 0
floodmax   = 0.d0
! check the dimension of the array
open(newunit=ifile, file=trim(hdsfile), status='old', access='stream', form='unformatted')
read(ifile) kstp,kper,pertim,totim,text,ncol,nrow,ilay
print*, 'Identify model Grid', ncol, nrow
allocate(head(ncol,nrow), top(ncol,nrow), diff(ncol,nrow), ones(ncol,nrow))
close(ifile)
ones = 1

open(newunit=ifile, file=trim(topfile), status='old')
read(ifile, *) top
close(ifile)

open(newunit=ifileout, file='Output.floodcells', status='replace')
write(ifileout, '(A)') '        SP FloodCellCount FloodCellMax FloodCellSum'

open(newunit=ifile,    file=trim(hdsfile), status='old', access='stream', form='unformatted')
ierr = 0
do while (ierr==0)
  read(ifile, iostat=ierr) kstp,kper,pertim,totim,text,ncol,nrow,ilay
  if (ierr /= 0) exit
  read(ifile, iostat=ierr) head
  if (ierr /= 0) exit
  if (ilay>1) cycle
  if (kper<iper1) cycle
  if (kper>iper2) cycle
  diff = head - top

  where ( diff>0.d0 .and. abs(head)<1e10 )
    head = diff
  elsewhere
    head = 0.d0
  end where
  ncount = count(head>0.d0)
  r1 = maxval(head)
  r2 = sum(head)
  write(ifileout, '(I10,I15,2F13.1)')  kper, ncount, r1, r2
  floodcount = floodcount + ncount
  floodmax   = max(floodmax, r1)
  floodsum   = floodsum   + r2
end do
write(ifileout, '(A10,I15,2F13.1)')  'Total', floodcount, floodmax, floodsum
end program
