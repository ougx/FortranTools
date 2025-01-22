module io

contains
  subroutine readdata(file, nrow, ncol, res, idx)
    character(1024)             :: file
    integer                     :: nn
    integer, optional, target   :: idx(nrow)
    real                        :: res(ncol,nrow)
    ! local
    integer                     :: ifile, ii
    integer, pointer            :: iidx(:)

    if (present(idx)) then
      iidx => idx
    else
      allocate(iidx(nrow))
    end if
    open(newunit=ifile, file=trim(file), status='old')
    read(ifile,*, iostat=ioerr)   ! header
    do ii = 1, nrow
        read(ifile,*, iostat=ioerr) iidx(ii), res(:, ii)
    end do
    close(ifile)
  end subroutine readdata

  function linecount(file)
    integer         :: linecount
    character(1024) :: file
    open(newunit=ifile, file=trim(file), status='old')
    linecount = 0
    do
      read(ifile,*, iostat=ioerr)
      if (ioerr/=0) EXIT
      linecount = linecount + 1
    end do
    close(ifile)
  end function
end module io
