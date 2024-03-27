program ghb_inactivate
! deactivate specified GHB cells by setting conductance to ZERO
character(1024)              :: disfile, deactfile, oldghb, newghb
character(1024)              :: line
integer                      :: idisfile, ideact, ioldghb, inewghb
integer                      :: i, nlay, nrow, ncol, nper, lay, row, col, itmp, iper, cell
integer, allocatable         :: deactcells(:)
double precision             :: cond, head


call get_command_argument(1, disfile)
call get_command_argument(2, deactfile)
call get_command_argument(3, oldghb)
call get_command_argument(4, newghb)

open(newunit=idisfile, file=trim(disfile),   status='old')
open(newunit=ideact,   file=trim(deactfile), status='old')
open(newunit=ioldghb,  file=trim(oldghb),    status='old')
open(newunit=inewghb,  file=trim(newghb),    status='replace')

! read dimemsion from dis file
call skipcomment(idisfile)
read(idisfile, *) nlay, nrow, ncol, nper
print "(A,4I5)", 'NLAY,NROW,NCOL,NPER:', nlay, nrow, ncol, nper
close(idisfile)


! read deactivate cells
call skipcomment(ideact)
read(ideact, *) maxdeact
allocate(deactcells(maxdeact))
read(ideact, *) deactcells   ! cell = (lay - 1) * nrow * ncol + (row - 1) * ncol + col
close(ideact)

! transfer header
read(ioldghb, '(A)') line
line = trim(adjustl(line))
if (line(1:9) == 'PARAMETER' .or. line(1:9) == 'PARAMETER') then 
  print*, 'PARAMETER IS NOT SUPPORTED'
  stop 1
end if 
write(inewghb, '(A)') trim(line)


do iper=1, nper
  read (ioldghb, '(A)') line
  write(inewghb, '(A)') trim(line)
  read(line, *) itmp
  do i = 1, itmp
    read (ioldghb, *) lay, row, col, head, cond
    cell = (lay - 1) * nrow * ncol + (row - 1) * ncol + col
    if (sum(findloc(deactcells, cell)) > 0) cond = 0.d0
    write(inewghb,'(3I10, F10.3, es10.3)') lay, row, col, head, cond
  end do
end do
close(ioldghb)
close(inewghb)
end program
