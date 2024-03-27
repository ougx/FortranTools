subroutine write_int(iout, line)
  integer            :: iout
  double precision   :: dtmp
  character(len=999) :: line, tag, val1

  read(line, *) tag, dtmp
  write(val1, *) int(dtmp)
  write(iout, '(A,A)') '  '//tag(:30)//'  ', trim(adjustl(val1))
end subroutine write_int

subroutine write_opt(iout, line, options)
  integer            :: iout
  double precision   :: dtmp
  character(len=999) :: line, tag
  character(len=20)  :: options(*)

  read(line, *) tag, dtmp
  write(iout, '(A,A)') '  '//tag(:30)//'  ', trim(adjustl(options(int(dtmp))))
end subroutine write_opt

subroutine write_verbose(iout, line)
  integer            :: iout
  double precision   :: dtmp
  character(len=999) :: line, tag, val1

  read(line, *) tag, val1
  write(iout, '(A,A)') '  '//tag(:30)//'  ',  trim(adjustl(val1))
end subroutine write_verbose

program cleanIMS

  double precision   :: line11(7)
  character(len=10)  :: spec
  double precision   :: line12(8)
  double precision   :: line2(10)
  character(len=999) :: infile, outfile, line, tmp, tag, iom
  integer            :: ierr, iblock


  character(len=20), parameter :: under_relaxation(4)    = (/'NONE','SIMPLE','COOLEY','DBD'/)
  character(len=20), parameter :: linear_acceleration(2) = (/'CG', 'BICGSTAB'/)
  character(len=20), parameter :: scaling_method(3)      = (/'NONE','DIAGONAL','L2NORM'/)
  character(len=20), parameter :: reordering_method(3)   = (/'NONE','RCM','MD'/)
  character(len=20), parameter :: blocks(3)              = (/'OPTIONS','NONLINEAR','LINEAR'/)

  call get_command_argument(1, infile)
  call get_command_argument(2, outfile)

  open(unit=101, file=trim(infile),  status='old')
  open(unit=102, file=trim(outfile), status='replace')
  call skipcomment(101)

  iblock = 0
  ims: do

    read(101, '(A)', iostat=ierr, iomsg=iom) tmp
    if (ierr/=0) then
      print '(A)', trim(iom)
      exit ims
    end if
    if (tmp=='') cycle

    line = trim(adjustl(tmp))
    call to_upper(line)
    print '(A)', trim(line)

    if (line(1:5) == 'BEGIN' .and. abs(iblock)<3) then
      read(line(6:), *) tag
      do i = 1, 3
        if (trim(tag) == blocks(i)) then
          iblock = abs(iblock) + 1
          write(102, '(A)') 'BEGIN '//trim(blocks(i))
        end if
      end do
      cycle
    end if

    ! option block
    if (line(1:4)=='END ' .and. iblock>0 .and. iblock<4) then
      write(102, '(A)') 'END '//trim(blocks(iblock))
      write(102, '(A)') ''
      iblock = -iblock
      cycle
    end if

    if (iblock == 1) then
      write(102, '(A)') '  '//trim(line)
    elseif (iblock == 2) then
      read(line, *) tag
      if (trim(tag)=='OUTER_MAXIMUM'   )          call write_int(102, line)
      if (trim(tag)=='OUTER_DVCLOSE'   )          call write_verbose(102, line)
      if (trim(tag)=='UNDER_RELAXATION')          call write_opt(102, line, under_relaxation)
      if (trim(tag)=='UNDER_RELAXATION_GAMMA')    call write_verbose(102, line)
      if (trim(tag)=='UNDER_RELAXATION_THETA')    call write_verbose(102, line)
      if (trim(tag)=='UNDER_RELAXATION_KAPPA')    call write_verbose(102, line)
      if (trim(tag)=='UNDER_RELAXATION_MOMENTUM') call write_verbose(102, line)

      if (trim(tag)=='BACKTRACKING_NUMBER'   )        call write_int(102, line)
      if (trim(tag)=='BACKTRACKING_TOLERANCE')        call write_verbose(102, line)
      if (trim(tag)=='BACKTRACKING_REDUCTION_FACTOR') call write_verbose(102, line)
      if (trim(tag)=='BACKTRACKING_RESIDUAL_LIMIT')   call write_verbose(102, line)
      ! linear block
    elseif (iblock == 3) then
      read(line, *) tag
      if (trim(tag)=='INNER_MAXIMUM'   )              call write_int(102, line)
      if (trim(tag)=='INNER_DVCLOSE'   )              call write_verbose(102, line)
      if (trim(tag)=='INNER_RCLOSE')                  call write_verbose(102, line)

      if (trim(tag)=='LINEAR_ACCELERATION')           call write_opt(102, line, linear_acceleration)
      if (trim(tag)=='RELAXATION_FACTOR')             call write_verbose(102, line)
      if (trim(tag)=='PRECONDITIONER_LEVELS')         call write_int(102, line)
      if (trim(tag)=='PRECONDITIONER_DROP_TOLERANCE') call write_verbose(102, line)

      if (trim(tag)=='NUMBER_ORTHOGONALIZATIONS'   )  call write_int(102, line)
      if (trim(tag)=='SCALING_METHOD        ')        call write_opt(102, line, scaling_method)
      if (trim(tag)=='REORDERING_METHOD')             call write_opt(102, line, reordering_method)
    end if

  end do ims
  write(102, '(A)') ''
  write(102, '(A)') '# Parsed from '//trim(infile)

end program
