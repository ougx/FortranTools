program ghb_interp

integer               :: nctlpnt
integer               :: nghbcell
integer               :: nlay, nrow, ncol, nper

character(2014)       :: filedis, filectl, filecell, fileout
integer               :: ifiledis, ifilectl, ifileghb, ifileout
integer               :: i, itmp, ilay
character(2014)               :: line
character(2014),  allocatable :: ghblines(:)
double precision, allocatable :: href(:),dhtr(:),cond(:),hghb(:)
double precision, allocatable :: frac1(:),botm(:, :)
double precision              :: hh,cc
integer         , allocatable :: row(:),column(:),ipoint1(:)


call get_command_argument(1, line); read(line, *) nctlpnt
call get_command_argument(2, line); read(line, *) nghbcell
call get_command_argument(3, filedis)
call get_command_argument(4, filectl)
call get_command_argument(5, filecell)
call get_command_argument(6, fileout)

allocate(href(nctlpnt),dhtr(nctlpnt),cond(nctlpnt),hghb(nctlpnt))
allocate(frac1(nghbcell),botm(nghbcell, nlay))
allocate(row(nghbcell),column(nghbcell),ipoint1(nghbcell))

! read dimemsion from dis file
open(newunit=ifiledis, file=trim(filedis),   status='old')
call skipcomment(ifiledis)
read(ifiledis, *) nlay, nrow, ncol, nper
print "(A,4I5)", 'NLAY,NROW,NCOL,NPER:', nlay, nrow, ncol, nper
close(ifiledis)

open(newunit=ifilectl, file=filectl, status='old')
read(ifilectl, '(A)') line
do i = 1, nctlpnt
  read(ifilectl, *) href(i), dhtr(i), cond(i)
end do
close(ifilectl)

open(newunit=ifileghb, file=filecell, status='old')
read(ifileghb, '(A)') line
do i = 1, nghbcell
  read(ifileghb, *) row(i),column(i),ipoint1(i),frac1(i),botm(i, 1:nlay)
end do
close(ifileghb)

! write GHB for SS model
print*, 'Creating GHB'

hghb = href + dhtr
open(newunit=ifileout, file=fileout, status='replace')
write(ifileout, *) nghbcell*nlay, 54, 'NOPRINT'                ! GHB assgined for the first two layers
itmp = 0
do i = 1, nghbcell
  hh = frac1(i) * hghb(ipoint1(i)) + (1.d0 - frac1(i)) * hghb(ipoint1(i)+1)
  cc = frac1(i) * cond(ipoint1(i)) + (1.d0 - frac1(i)) * cond(ipoint1(i)+1)
  do ilay = 1, nlay
    if (hh>=botm(i, ilay)) then
      itmp = itmp + 1
      write(ghblines(itmp), '(3I10, F10.4, es10.3)') ilay, row(i),column(i),hh,cc
    end if
  end do
end do
write(ifileout, *) itmp
do i=1, itmp
  write(ifileout, *) trim(ghblines(i))
end do
do i = 1, nper
  write(ifileout, *) -1
end do

close(ifileout)
end program
