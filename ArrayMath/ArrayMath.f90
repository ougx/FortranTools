! Created by Michael Ou

program arraymath
  use f90getopt
  use m_mrgref
  use iso_fortran_env, only: input_unit, error_unit, output_unit
  implicit none

  real, parameter :: verysmall = tiny(1.0) * 1e5
  real, parameter :: verylarge = huge(1.0) * 1e-3

  type(option_s), allocatable  :: opts(:)

  character(1024) :: arrfile, multfile, outfile, fomt
  character(len=2)  :: opt

  integer         :: ifile, irow, nskiprow, nskipcol, icol
  real            :: rscale, roffset, power, vmin, vmax
  real, allocatable :: results(:,:), array(:,:), multi(:,:), trans(:,:)
  character(256)  :: stemp(9999)
  character(10)   :: func
  character(30), allocatable   :: rownames(:), colnames(:), tmpnames(:)
  logical         :: verbose

  integer         :: nrow, ncol, itemp, mdim, hasRowName, hasColName, maxRowNameWidth, ntmp
  integer, allocatable:: rowindex(:)

  allocate(opts, source=(/&
    option_s("dim"      ,  "d", 2, "shape of the array, i.e. nrow ncol; this must be defined as the first argument."), &
    option_s("add"      ,  "a", 3, "adding a new array, follwed by three arguments: arrayfile multiplyfile scale; if arrayfile is '-', it reads stdin; if multiplyfile is '-', no multiarray is used."), &
    option_s("multi"    ,  "m", 1, "multiply with a number or a new array reading from a file; follwed by one argument. if the argument starts with '*', it multiplies the number after '*'."), &
    option_s("dot"      , "dt", 1, "Calculate dot product of the array and another array; follwed by one argument: multiplyfile; if multiplyfile is '-', it reads stdin."), &
    option_s("help"     ,  "h", 0, "show this message."), &
    option_s("fmt"      , "fm", 1, "fortran format to write results such as '(10F10.3)'; default is '(*(G0.8,x))'."), &
    option_s("offset"   ,  "o", 1, "adding offset to the final result, default is zero."), &
    option_s("duplicate", "dr", 1, "duplicate the rows for a number of times."), &
    option_s("skiprow"  , "sr", 1, "number of rows to skip when reading the input array file; must define before reading the file; default is zero; it can be defined multiple times."), &
    option_s("skipcol"  , "sc", 1, "number of columns to skip when reading the input array file; must define before reading the file; default is zero; it can be defined multiple times."), &
    option_s("func"     ,  "x", 1, "apply a Fortran intrinsic mathematical function to the final result"), &
    option_s("groupby"  ,  "g", 2, "split and apply a function."), &
    option_s("stack"    , "st", 0, "stack the 2D array into one column."), &
    option_s("power"    ,  "p", 1, "apply a power function to the final result"), &
    option_s("clip",       "c", 2, "clip the array to the interval between vmin and vmax. if `-c 0 1` is specified, values smaller than 0 become 0, and values larger than 1 become 1."), &
    option_s("subset"   ,  "s", 1, "subset the array. -s r1:10 to subset the first 10 rows; -s c1,3,5 to subset the columns 1,3 and 5; -s r2:2 or -s c5:5 to subset one row or column."), &
    option_s("filter"   ,  "f", 1, "filter array by column names, e.g. --filter time>=3; --filter layer==1"), &
    option_s("verbose"  ,  "v", 0, "print running logs to screen."), &
    option_s("transpose",  "t", 0, "transposes the array."), &
    option_s("rowname"  , "rn", 0, "enable/disable reading row names from the first column after skipping columns. default is off."), &
    option_s("colname"  , "cn", 0, "enable/disable reading column names from the first row after skipping rows. default is off."), &
    option_s("mfbin"    , "mb", 0, "TODO: read modflow binary file"), &
    option_s("mfdbin"   , "md", 0, "TODO: read modflow binary file in double precision")  &
  /))


  ! set up the initial values
  fomt='(*(G0.8,x))'
  nrow=0
  nskiprow=0
  nskipcol=0
  roffset = 0.
  func = ''
  power = 1.0
  verbose = .false.
  outfile = '~'
  hasRowName = 0
  hasColName = 0

  do
    opt = getopt(opts)
    select case(trim(opt))

      case(char(0)) ! When all options are processed
        if (len_trim(optarg)>0) outfile = trim(optarg)
        exit

      case("d")
        read(optarg, *) nrow, ncol
        mdim = nrow*ncol
        allocate(results(ncol, nrow))
        allocate(rownames(0:mdim), colnames(0:mdim), tmpnames(0:mdim), rowindex(mdim))
        results = 0.
        rownames = ''
        colnames = ''

      case("rn")
        hasRowName = 1

      case("cn")
        hasColName = 1

      case("s")
        call subset()

      case("st")
        call stack()

      case("f")
        if (hasColName==0) call perror('filter option can only applied to named columns')
        call filter()

      case("t")
        call rtranspose()

      case("p")
        read(optarg, *) power
        results = results ** power

      case("dr")
        read(optarg, *) ntmp
        call duplicate_rows(ntmp)

      case("c")
        read(optarg, *) vmin, vmax
        results = merge(results, vmin, results>vmin)
        results = merge(results, vmax, results<vmax)

      case("x")
        call applyfunc()

      case("g")
        if (hasColName==0) call perror('groupby option can only applied to named columns')
        call groupby()

      case("m")
        read(optarg, *) multfile
        if (multfile(1:1)=="*") then
          read(multfile(2:), *) rscale
          results = results * rscale
        else
          allocate(multi, source=results)
          if (trim(multfile) == '-') then
            !TODO:
            read(input_unit, *) multi
          else
            call readdata(multfile, multi)
          end if
          if (verbose) print*, "Multiplying array from "//trim(multfile)
          results = results * multi
          deallocate(multi)
        end if

      case("dt")
        read(optarg, *) multfile
        allocate(array, source=results)
        allocate(multi, source=results)
        if (trim(multfile) == '-') then
          !TODO:
          read(input_unit, *) multi
        else
          call readdata(multfile, multi)
        end if
        call SGEMM('N','N',nrow,ncol,ncol,1.0,results,ncol,multi,ncol,0.0,array,ncol)
        results = array
        deallocate(array)
        deallocate(multi)

      case("a")
        read(optarg, *) arrfile, multfile, rscale
        if (verbose) print*, "Adding array from "//trim(arrfile)

        allocate(array, source=results)
        allocate(multi, source=results)
        if (trim(arrfile) == '-') then
          !TODO:
          read(input_unit, *) array
        else
          call readdata(arrfile, array)
        end if

        if (trim(multfile) /= '' .and. trim(multfile) /= '-') then
          call readdata(multfile, multi)
          array = array * multi
        end if

        results = results + array * rscale
        deallocate(array)
        deallocate(multi)

      case("sr")
        read(optarg, *) nskiprow

      case("sc")
        read(optarg, *) nskipcol

      case("o")
        read(optarg, *) roffset
        if (abs(roffset) > verysmall) results = results + roffset

      case("fm")
        fomt=adjustl(trim(optarg))

      case("h")
        call showhelp

      case("v")
        verbose = .true.

      case default
        !call perror('Unknown option: '//trim(optarg))
        stop
    end select
    if (nrow==0) exit
  end do

  if (nrow==0) then
    call perror("  Error: Dimension needs to be defined in the first argument")
  end if

  where(abs(results) < verysmall)
    results = 0.
  end where

  !where(abs(results) > verylarge)
  !  results = 0.
  !end where
  if (trim(outfile)=='~') then
    ifile = output_unit ! print to IO
  else
    open(newunit=ifile, file=outfile, status='replace')
  end if

  if (hasRowName==1) then
    do irow=1, nrow
      maxRowNameWidth = max(maxRowNameWidth, len_trim(rownames(irow)))
    end do
  else
    maxRowNameWidth =0
  end if
  if (hasColName==1) write(ifile, '(A,x,*(A,x))') colnames(0)(:maxRowNameWidth), (colnames(icol)(1:max(9,len_trim(colnames(icol)))),icol=1,ncol)

  do irow=1, nrow
    write(ifile, "(A,x,"//fomt(2:)) rownames(irow)(:maxRowNameWidth), results(1:ncol, irow)
  end do
  close(ifile)
  if (verbose) print*, 'Results have been written to "'//trim(outfile)//'" successfully'


  contains

  subroutine readdata(afile, arr)
  character(1024) :: afile
  real            :: arr(:,:)

  open(newunit=ifile, file=trim(afile), status='old')

  do irow=1, nskiprow
    read(ifile, *)
  end do

  if (hasColName==1) then
    if (nskipcol>0) then
      read(ifile, *) stemp(1:nskipcol), colnames((1-hasRowName):ncol)
    else
      read(ifile, *) colnames((1-hasRowName):ncol)
    end if
    rownames(0) = colnames(0)
  end if

  if (nskipcol==0) then
    if (hasRowName==1) then
      do irow=1, nrow
        read(ifile, *) rownames(irow), arr(:, irow)
      end do
    else
      read(ifile, *) arr
    end if
  else
    if (hasRowName==1) then
      do irow=1, nrow
        read(ifile, *) stemp(1:nskipcol), rownames(irow), arr(:, irow)
      end do
    else
      do irow=1, nrow
        read(ifile, *) stemp(1:nskipcol), arr(:, irow)
      end do
    end if
  end if
  close(ifile)
  end subroutine

  subroutine filter()
    integer              :: idx0,idx1,idx2,idxc
    real                 :: rv
    character(30)        :: cname
    integer, allocatable :: idxs(:)

    idx0=index(optarg, '<')
    idx1=index(optarg, '>')
    idx2=index(optarg, '=')

    if (idx0==0 .and. idx1==0 .and. idx2==0) call perror('Incorrect filter clause: '//trim(optarg))
    allocate(idxs(nrow))
    do idxc=1, nrow
      idxs(idxc)=idxc
    end do

    if(idx0>0) then
      read(optarg(:idx0-1), *) cname
      idxc = get_idxcol(cname)
      if (idx2==0) then
        read(optarg(idx0+1:), *) rv
        call redefine_array(source=results(:,pack(idxs,results(idxc,:)<rv)))
      else
        if (idx2 /= idx0 + 1) call perror('Incorrect filter clause: '//trim(optarg))
        read(optarg(idx0+2:), *) rv
        call redefine_array(source=results(:,pack(idxs,results(idxc,:)<=rv)))
      end if
    elseif (idx1>0) then
      idx0 = idx1
      read(optarg(:idx0-1), *) cname
      idxc = get_idxcol(cname)
      if (idx2==0) then
        read(optarg(idx0+1:), *) rv
        call redefine_array(source=results(:,pack(idxs,results(idxc,:)>rv)))
      else
        if (idx2 /= idx0 + 1) call perror('Incorrect filter clause: '//trim(optarg))
        read(optarg(idx0+2:), *) rv
        call redefine_array(source=results(:,pack(idxs,results(idxc,:)>=rv)))
      end if
    else
      idx0 = idx2
      if (optarg(idx0-1:idx0-1)=='/') then
        read(optarg(:idx0-2), *) cname
        read(optarg(idx0+1:), *) rv
        idxc = get_idxcol(cname)
        call redefine_array(source=results(:,pack(idxs,results(idxc,:)/=rv)))
      else if (optarg(idx0+1:idx0+1)=='=') then
        read(optarg(:idx0-1), *) cname
        read(optarg(idx0+2:), *) rv
        idxc = get_idxcol(cname)
        call redefine_array(source=results(:,pack(idxs,results(idxc,:)==rv)))
      else
        call perror('Incorrect filter clause: '//trim(optarg))
      end if
    end if
    if (nrow==0) call perror('Filter clause remove all data: '//trim(optarg))
  end subroutine

  integer function get_idxcol(cname)
    character(*)        :: cname
    integer              :: idxc
    get_idxcol = 0
    do idxc=0, ncol
      if (trim(colnames(idxc))==trim(cname)) then
        get_idxcol = idxc
        return
      end if
    end do
    call perror('Could not identify column name: '//trim(cname))
  end function


  subroutine stack()
    call redefine_array(source=reshape(results, [1,nrow*ncol]))
  end subroutine


  subroutine subset()
    integer              :: idx0,idx1,idx2,nidx
    integer              :: idxs(9999)

    optarg=adjustl(optarg)
    idx0 = index(optarg, ':')
    idx1 = index(optarg, ',')
    if (optarg(1:1)/='c' .and. optarg(1:1)/='r') then
      call perror('The first character of the subsetting augument must be "r" or "c" to subset rows or columns, respectively.')
    end if

    if ((idx0>0 .and. idx1>0)) then
      call perror('Subsetting must be performed by slicing using ":" or by one or a list of rows or colums (seperated by ",").')
    end if

    if (idx0 > 0) then
      read(optarg(2:idx0-1),*) idx1
      read(optarg(idx0+1: ),*) idx2
      if (optarg(1:1)=='c') then
        call redefine_array(source=results(idx1:idx2,:))
        colnames(1:ncol)=colnames(idx1:idx2)
      else
        call redefine_array(source=results(:,idx1:idx2))
        rownames(1:nrow)=rownames(idx1:idx2)
      end if
    else
      idx2 = 1
      nidx = 0
      do
        idx1 = idx2+1
        idx2 = index(optarg(idx1:), ",")
        if (idx2==0) exit
        idx2 = idx1+idx2-1
        nidx = nidx + 1
        read(optarg(idx1:idx2-1),*) idxs(nidx)
      end do
      nidx = nidx + 1
      read(optarg(idx1:),*) idxs(nidx)
      if (optarg(1:1)=='c') then
        call redefine_array(source=results(idxs(:nidx),:))
        colnames(1:ncol)=colnames(idxs(:nidx))
      else
        call redefine_array(source=results(:,idxs(:nidx)))
        rownames(1:nrow)=rownames(idxs(:nidx))
      end if
    end if

  end subroutine


  subroutine rtranspose()
    integer :: hasname
    allocate(trans(nrow, ncol))
    do irow=1, nrow
      do icol=1, ncol
        trans(irow, icol) = results(icol, irow)
      end do
    end do
    deallocate(results)
    allocate(results(nrow, ncol))
    results = trans
    deallocate(trans)
    ntmp = ncol
    ncol = nrow
    nrow = ntmp
    tmpnames = rownames
    rownames = colnames
    colnames = tmpnames
    hasname = hasRowName
    hasRowName = hasColName
    hasColName = hasname
  end subroutine


  subroutine duplicate_rows(duplicate)
    integer :: duplicate

    allocate(trans, source=results)
    deallocate(results)
    allocate(results(ncol, nrow*duplicate))
    do irow = 1, duplicate
      results(:, (nrow*(irow-1)+1):nrow*irow) = trans
    end do
    deallocate(trans)
    nrow = nrow * duplicate
  end subroutine


  subroutine groupby()
    integer  :: ngroup, ig1, igroup(nrow)
    character(30)   :: colname

    read(optarg, *) colname, func
    icol = get_idxcol(colname)
    if (icol==0) then
      call mrgref(rownames(1:nrow), rowindex(1:nrow))
    else
      call mrgref(results(icol,:), rowindex(1:nrow))
    end if
    results=results(:,rowindex(1:nrow))
    rownames(1:nrow)=rownames(rowindex(1:nrow))
    ! find group number for each row
    igroup(1) = 1
    do irow = 2, nrow
      igroup(irow) = igroup(irow-1)
      if (icol==0) then
        if (rownames(irow)/=rownames(irow-1)) igroup(irow) = igroup(irow) + 1
      else
        if (results(icol,irow) /= results(icol,irow-1)) igroup(irow) = igroup(irow) + 1
      end if
      if (igroup(irow) /= igroup(irow-1)) rownames(igroup(irow)) = rownames(irow)
    end do
    ngroup = igroup(nrow)
    allocate(trans(ncol, ngroup))
    if (trim(func) == "sum" .or. trim(func) == "mean") trans=0.0
    if (trim(func) == "max") trans = -huge(0.0)
    if (trim(func) == "min") trans = huge(0.0)

    do irow = 1, nrow
      select case (trim(func))
      case ('sum')
        trans(:, igroup(irow)) = trans(:, igroup(irow)) + results(:,irow)
      case ('mean')
        trans(:, igroup(irow)) = trans(:, igroup(irow)) + results(:,irow)
      case ('max')
        trans(:, igroup(irow)) = max(trans(:, igroup(irow)), results(:,irow))
      case ('min')
        trans(:, igroup(irow)) = min(trans(:, igroup(irow)), results(:,irow))
      end select
    end do
    if (trim(func) == "mean") then
      do ig1 = 1, ngroup
        trans(:, ig1) = trans(:, ig1) / count(igroup==ig1)
      end do
    end if
    call redefine_array(trans)
    deallocate(trans)
  end subroutine

  subroutine applyfunc()
  integer itmp
  read(optarg, *) func
  call to_lower(func)
  if (trim(func)=="sort" .or. trim(func)=="sortrow") func="sortr"
  if (trim(func)=="sortcolumn" ) func="sortc"
  select case(adjustl(trim(func)))
  case ('abs'      ); results = abs     (results)
  case ('exp'      ); results = exp     (results)
  case ('log10'    ); results = log10   (results)
  case ('log'      ); results = log     (results)
  case ('sqrt'     ); results = sqrt    (results)
  case ('sinh'     ); results = sinh    (results)
  case ('cosh'     ); results = cosh    (results)
  case ('tanh'     ); results = tanh    (results)
  case ('sin'      ); results = sin     (results)
  case ('cos'      ); results = cos     (results)
  case ('tan'      ); results = tan     (results)
  case ('asin'     ); results = asin    (results)
  case ('acos'     ); results = acos    (results)
  case ('atan'     ); results = atan    (results)
  case ('int'      ); results = int     (results)
  case ('nint'     ); results = nint    (results)
  case ('floor'    ); results = floor   (results)
  case ('inverse'  ); results = 1.0    / results
  case ('matinv'   ); call matinv(results)
  case ('fraction' ); results = fraction(results)
  case ('diff1'    ); results(:,2:nrow) = add2dand1d(results(:,2:nrow), -results(:,1)); results(:,1)=0
  case ('diff'     ); results(:,2:nrow) = results(:,2:nrow)-results(:,1:nrow-1)
  case ('cbrt'     ); results = sign(abs(results)**(1.0/3.0), results)
  case ('max'      ); nrow=1; results(:, 1)=maxval(results, 2); if (hasRowName==1) rownames(1)="max"
  case ('min'      ); nrow=1; results(:, 1)=minval(results, 2); if (hasRowName==1) rownames(1)="min"
  case ('sum'      ); nrow=1; results(:, 1)=sum   (results, 2); if (hasRowName==1) rownames(1)="sum"
  case ('mean'     ); nrow=1; results(:, 1)=sum(results, 2)/size(results, 2); if (hasRowName==1) rownames(1)="avg"
  case ('cumsum'   ); call cumsum()
  case ('sortr'    ); call mrgref(rownames(1:nrow), rowindex(1:nrow)); rownames(1:nrow)=rownames(rowindex(1:nrow)); results=results(:,rowindex(1:nrow))
  case ('sortc'    ); call mrgref(colnames(1:nrow), rowindex(1:ncol)); colnames(1:ncol)=colnames(rowindex(1:ncol)); results=results(rowindex(1:ncol),:)
  case ('transpose'); call rtranspose()
  case default
    call perror('Unknown function: '//trim(func))
  end select
  end subroutine

  subroutine cumsum()
    integer :: i
    do i = 2, nrow
      results(:,i) = results(:,i-1) + results(:,i)
    end do
  end subroutine

  function add2dand1d(a2d, b1d, dim) result(c)
    real, intent(in) :: a2d(:,:), b1d(:)
    real, allocatable :: c(:,:)
    integer, intent(in), optional :: dim
    integer :: shape_a(2), i, idim
    shape_a = shape(a2d)
    idim = 2
    if (present(dim)) idim = dim
    c = a2d + spread(b1d, idim, shape_a(idim))
  end function add2dand1d

  subroutine redefine_array(source)
    real  :: source(:,:)
    ncol = ubound(source, dim=1)
    nrow = ubound(source, dim=2)
    deallocate(results)
    allocate(results, source=source)
  end subroutine

  subroutine matinv(matA)
    INTEGER              :: INFO, LWORK
    integer, external    :: ILAENV
    INTEGER, allocatable :: IPIV(:)
    REAL, allocatable    :: WORK(:)
    real, contiguous     :: matA(:,:)
    allocate(IPIV(nrow))

    call SGETRF( nrow, ncol, matA, ncol, IPIV, INFO )
    if (INFO /= 0) then
      call perror('Matrix factorization failed.')
    end if

    LWORK = nrow*ILAENV( 1, 'SGETRI', ' ', nrow, -1, -1, -1 )
    allocate(WORK(LWORK))

    call SGETRI( nrow, matA, ncol, IPIV, WORK, LWORK, INFO )
    if (INFO /= 0) then
      call perror('Matrix inverse failed.')
    end if
  end subroutine

  subroutine perror(msg)
    character(*) :: msg
    write(error_unit, '(A)') msg
    stop
  end subroutine

  subroutine showhelp()
    print "(A)", ''
    print "(A)", ''
    print "(A)", ' ArrayMath -d nrow ncol [options] -a arrayfile multiplyfile scale [options] [output]'
    print "(A)", ''
    print "(A)", '   ArrayMath calculates the sum of an offset and the product of one or more arrays, optional scaling arrays and scalers.'
    print "(A)", '   result = func(sum(array_i * multiarray_i * scale_i)**power + offset), `offset` can be applied after `func`.'
    print "(A)", ' '
    print "(A)", '   Arguments:'
    print "(A)", '      -d   or  --dim       shape of the array, i.e. nrow ncol; this needs to be defined in the first argument.'
    print "(A)", '      -a   or  --add       adding a new array, follwed by three arguments, if multiplyfile == "-", no multiarray is used. it can be defined multiple times'
    print "(A)", ' '
    print "(A)", '   Optional arguments:'
    do itemp=3, size(opts)
      print "(A)", '      -'//opts(itemp)%short//'  or  --'//opts(itemp)%name(:10)//trim(opts(itemp)%description)
    end do
    print "(A)", '      output               output file name; must be the final argument. If output=="~" or omitted, output will print to screen.'
    print "(A)", ' '
    print "(A)", '   Functions for -x or --func:'
    print "(A)", '     abs        computes the absolute values of the array'
    print "(A)", '     exp        computes the base e exponential of the array.'
    print "(A)", '     log10      computes the base 10 logarithm of the array.'
    print "(A)", '     log        computes the base e logarithm of the array.'
    print "(A)", '     sqrt       computes the square root of the array.'
    print "(A)", '     sinh       computes the inverse hyperbolic sine of the array.'
    print "(A)", '     cosh       computes the hyperbolic cosine of the array.'
    print "(A)", '     tanh       computes the hyperbolic tangent of the array.'
    print "(A)", '     sin        computes the sine of the array.'
    print "(A)", '     cos        computes the cosine of the array.'
    print "(A)", '     tan        computes the tangent of the array.'
    print "(A)", '     asin       computes the arcsine of its the array (inverse of SIN(X))'
    print "(A)", '     acos       computes the arccosine of the array (inverse of COS(X))'
    print "(A)", '     atan       computes the arctangent of the array(inverse of TAN(X)).'
    print "(A)", '     int        converts the array to integer type.'
    print "(A)", '     nint       rounds the array to the nearest whole number.'
    print "(A)", '     floor      returns the greatest integer less than or equal to the array.'
    print "(A)", '     inverse    computes the reciprocal of each element in the array.'
    print "(A)", '     matinv     computes the matrix inverse of the array, using the LU factorization.'
    print "(A)", '     fraction   returns the fractional part of the model representation of the array.'
    print "(A)", '     diff1      subtract the first row from the array.'
    print "(A)", '     diff       calculates the difference of each row and its previous row  row_i - row_(i-1).'
    print "(A)", '     cbrt       calculates the cubic root of the array. The new results have the same sign as the original array.'
    print "(A)", '     max        calculates the maximum values for each columns.'
    print "(A)", '     min        calculates the minimum values for each columns.'
    print "(A)", '     sum        adds the elements along each columns.'
    print "(A)", '     cumsum     computes the cumulatice sum along each columns.'
    print "(A)", '     mean       calculates the average for each columns.'
    print "(A)", '     transpose  transposes the array.'
    print "(A)", '     sortr      sorts array by row names.'
    print "(A)", '     sortc      sorts array by column names.'
    stop
    end subroutine
  end program
