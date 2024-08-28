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

  integer         :: nrow, ncol, itemp, mdim, hasRowName, hasColName, maxRowNameWidth
  integer, allocatable:: rowindex(:)

  allocate(opts, source=(/&
    option_s("dim"    ,  "d", 2, "shape of the array, i.e. nrow ncol; this must be defined as the first argument."), &
    option_s("add"    ,  "a", 3, "adding a new array, follwed by three arguments: arrayfile multiplyfile scale; if arrayfile is '-', it reads stdin; if multiplyfile is '-', no multiarray is used."), &
    option_s("multi"  ,  "m", 1, "multiply with a new array reading from a file; follwed by one argument: multiplyfile; if multiplyfile is '-', it reads stdin."), &
    option_s("dot"  ,   "dt", 1, "Calculate dot product of the array and another array; follwed by one argument: multiplyfile; if multiplyfile is '-', it reads stdin."), &
    option_s("help"   ,  "h", 0, "show this message."), &
    option_s("fmt"    , "fm", 1, "fortran format to write results such as '(10F10.3)'; default is '(*(G0.8,x))'."), &
    option_s("offset" ,  "o", 1, "adding offset to the final result, default is zero."), &
    option_s("skiprow", "sr", 1, "number of rows to skip when reading the input array file; must define before reading the file; default is zero; it can be defined multiple times."), &
    option_s("skipcol", "sc", 1, "number of columns to skip when reading the input array file; must define before reading the file; default is zero; it can be defined multiple times."), &
    option_s("func"   ,  "x", 1, "apply a Fortran intrinsic mathematical function to the final result"), &
    option_s("groupby",  "g", 2, "split and apply a function."), &
    option_s("power"  ,  "p", 1, "apply a power function to the final result"), &
    option_s("clip",     "c", 2, "clip the array to the interval between vmin and vmax. if `-c 0 1` is specified, values smaller than 0 become 0, and values larger than 1 become 1."), &
    option_s("subset" ,  "s", 1, "subset the array. -s r1:10 to subset the first 10 rows; -s c1,3,5 to subset the columns 1,3 and 5; -s r2:2 or -s c5:5 to subset one row or column."), &
    option_s("filter" ,  "f", 1, "filter array by column names, e.g. --filter time>=3; --filter layer==1"), &
    option_s("verbose",  "v", 0, "print running logs to screen."), &
    option_s("rowname", "rn", 0, "enable/disable reading row names from the first column after skipping columns. default is off."), &
    option_s("colname", "cn", 0, "enable/disable reading column names from the first row after skipping rows. default is off."), &
    option_s("mfbin" ,  "mb", 0, "TODO: read modflow binary file"), &
    option_s("mfdbin",  "md", 0, "TODO: read modflow binary file in double precision")  &
  /))


  ! set up the initial values
  fomt='(A,*(x,G0.8))'
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
        mdim = max(nrow,ncol)
        allocate(results(ncol, nrow))
        allocate(rownames(0:mdim), colnames(0:mdim), tmpnames(0:mdim), rowindex(mdim))
        results = 0.
        rownames = ''
        colnames = ''
      array = 0.

      case("rn")
        hasRowName = 1

      case("cn")
        hasColName = 1

      case("s")
        call subset()

      case("f")
        if (hasColName==0) call perror('filter option can only applied to named columns')
        call filter()

      case("p")
        read(optarg, *) power
        results = results ** power

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
        fomt=trim(optarg)

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
  if (hasColName==1) write(ifile, '(*(A,x))') colnames(0)(:maxRowNameWidth), (colnames(icol)(1:max(9,len_trim(colnames(icol)))),icol=1,ncol)

  do irow=1, nrow
    write(ifile, fomt) rownames(irow)(:maxRowNameWidth), results(1:ncol, irow)
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
    do idxc=1, ncol
      if (trim(colnames(idxc))==trim(cname)) then
        get_idxcol = idxc
        return
      end if
    end do
    call perror('Could not identify column name: '//trim(cname))
  end function

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

  subroutine groupby()
    integer  :: ngroup, ig1
    character(30)   :: colname

    read(optarg, *) colname, func
    icol = get_idxcol(colname)
    call mrgref(results(icol,:), rowindex(1:nrow))
    results=results(:,rowindex(1:nrow))
    allocate(trans, source=results)
    ig1 = 1
    ngroup = 0
    do irow = 2, nrow
      if (results(icol,irow) /= results(icol,irow-1)) then
        ngroup = ngroup + 1
        select case (trim(func))
        case ('sum')
          trans(:, ngroup) = sum(results(:,ig1:irow-1), 2)
          trans(icol, ngroup) = results(icol,ig1)
        case ('mean')
          trans(:, ngroup) = sum(results(:,ig1:irow-1), 2) / (irow - ig1)
        case ('max')
          trans(:, ngroup) = maxval(results(:,ig1:irow-1), 2)
        case ('min')
          trans(:, ngroup) = minval(results(:,ig1:irow-1), 2)
        end select
        trans(icol, ngroup) = results(icol,ig1)
        ig1 = irow
      end if
    end do
    ngroup = ngroup + 1
    select case (trim(func))
    case ('sum')
      trans(:, ngroup) = sum(results(:,ig1:irow-1), 2)
      trans(icol, ngroup) = results(icol,ig1)
    case ('mean')
      trans(:, ngroup) = sum(results(:,ig1:irow-1), 2) / (irow - ig1)
    case ('max')
      trans(:, ngroup) = maxval(results(:,ig1:irow-1), 2)
    case ('min')
      trans(:, ngroup) = minval(results(:,ig1:irow-1), 2)
    end select
    trans(icol, ngroup) = results(icol,ig1)

    call redefine_array(trans(:, :ngroup))
    deallocate(trans)
  end subroutine

  subroutine applyfunc()
  integer itmp
  read(optarg, *) func
  call to_lower(func)
  if (trim(func)=='abs'  )    then; results = abs     (results); else &
  if (trim(func)=='exp'  )    then; results = exp     (results); else &
  if (trim(func)=='log10')    then; results = log10   (results); else &
  if (trim(func)=='log'  )    then; results = log     (results); else &
  if (trim(func)=='sqrt' )    then; results = sqrt    (results); else &
  if (trim(func)=='sinh' )    then; results = sinh    (results); else &
  if (trim(func)=='cosh' )    then; results = cosh    (results); else &
  if (trim(func)=='tanh' )    then; results = tanh    (results); else &
  if (trim(func)=='sin'  )    then; results = sin     (results); else &
  if (trim(func)=='cos'  )    then; results = cos     (results); else &
  if (trim(func)=='tan'  )    then; results = tan     (results); else &
  if (trim(func)=='asin' )    then; results = asin    (results); else &
  if (trim(func)=='acos' )    then; results = acos    (results); else &
  if (trim(func)=='atan' )    then; results = atan    (results); else &
  if (trim(func)=='int'  )    then; results = int     (results); else &
  if (trim(func)=='nint' )    then; results = nint    (results); else &
  if (trim(func)=='floor')    then; results = floor   (results); else &
  if (trim(func)=='inverse')  then; results = 1.0    / results ; else &
  if (trim(func)=='matinv')   then; call matinv(results)       ; else &
  if (trim(func)=='fraction') then; results = fraction(results); else &
  if (trim(func)=='diff1')    then; do itemp=2, nrow; results(:,itemp) = results(:,itemp)-results(:,1); end do; results(:,1)=0; else &
  if (trim(func)=='diff')     then; results(:,2:nrow) = results(:,2:nrow)-results(:,1:nrow-1); else &
  if (trim(func)=='cbrt' )    then; results = sign(abs(results)**(1.0/3.0), results); else &
  if (trim(func)=='max'  )    then; nrow=1; results(:, 1)=maxval(results, 2); else &
  if (trim(func)=='min'  )    then; nrow=1; results(:, 1)=minval(results, 2); else &
  if (trim(func)=='sum'  )    then; nrow=1; results(:, 1)=sum  (results, 2); else &
  if (trim(func)=='cumsum')   then; call cumsum()              ; else &
  if (trim(func)=='mean' )    then; nrow=1; results(:, 1)=sum(results, 2)/size(results, 2); else &
  if (trim(func)=='sort' .or. trim(func)=='sortr' .or. trim(func)=='sortrow')    then
    call mrgref(rownames(1:nrow), rowindex(1:nrow))
    rownames(1:nrow)=rownames(rowindex(1:nrow))
    results=results(:,rowindex(1:nrow))
  else &
  if (trim(func)=='sortc' .or. trim(func)=='sortcolumn')    then
    call mrgref(colnames(1:nrow), rowindex(1:ncol))
    colnames(1:ncol)=colnames(rowindex(1:ncol))
    results=results(rowindex(1:ncol),:)
  else &
  if (trim(func)=='transpose')then
    call redefine_array(source=transpose(results))
    tmpnames=rownames
    rownames=colnames
    colnames=tmpnames
    itemp = hasColName
    hasColName = hasRowName
    hasRowName = itemp
  else
    call perror('Unknown function: '//trim(func))
  end if
  end subroutine

  subroutine cumsum()
    integer :: i
    do i = 2, nrow
      results(:,i) = results(:,i-1) + results(:,i)
    end do
  end subroutine

  subroutine redefine_array(source)
    real  :: source(:,:)
    real, allocatable :: rt1(:,:)
    allocate(rt1, source=source)
    deallocate(results)
    allocate(results, source=rt1)
    deallocate(rt1)
    ncol = ubound(results, dim=1)
    nrow = ubound(results, dim=2)
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
    print "(A)", '     abs:       computes the absolute values of the array'
    print "(A)", '     exp:       computes the base e exponential of the array.'
    print "(A)", '     log10:     computes the base 10 logarithm of the array.'
    print "(A)", '     log:       computes the base e logarithm of the array.'
    print "(A)", '     sqrt:      computes the square root of the array.'
    print "(A)", '     sinh:      computes the inverse hyperbolic sine of the array.'
    print "(A)", '     cosh:      computes the hyperbolic cosine of the array.'
    print "(A)", '     tanh:      computes the hyperbolic tangent of the array.'
    print "(A)", '     sin:       computes the sine of the array.'
    print "(A)", '     cos:       computes the cosine of the array.'
    print "(A)", '     tan:       computes the tangent of the array.'
    print "(A)", '     asin:      computes the arcsine of its the array (inverse of SIN(X))'
    print "(A)", '     acos:      computes the arccosine of the array (inverse of COS(X))'
    print "(A)", '     atan:      computes the arctangent of the array(inverse of TAN(X)).'
    print "(A)", '     int:       converts the array to integer type.'
    print "(A)", '     nint:      rounds the array to the nearest whole number.'
    print "(A)", '     floor:     returns the greatest integer less than or equal to the array.'
    print "(A)", '     inverse:   computes the reciprocal of each element in the array.'
    print "(A)", '     matinv:    computes the matrix inverse of the array, using the LU factorization.'
    print "(A)", '     fraction:  returns the fractional part of the model representation of the array.'
    print "(A)", '     diff1:     subtract the first row from the array.'
    print "(A)", '     diff:      calculates the difference of each row and its previous row: row_i - row_(i-1).'
    print "(A)", '     cbrt:      calculates the cubic root of the array. The new results have the same sign as the original array.'
    print "(A)", '     max:       calculates the maximum values for each columns.'
    print "(A)", '     min:       calculates the minimum values for each columns.'
    print "(A)", '     sum:       adds the elements along each columns.'
    print "(A)", '     cumsum:    computes the cumulatice sum along each columns.'
    print "(A)", '     mean:      calculates the average for each columns.'
    print "(A)", '     transpose: transposes the array.'
    print "(A)", '     sort:      sorts array by row names.'
    print "(A)", '     sortc:     sorts array by column names.'
    stop
    end subroutine
  end program
