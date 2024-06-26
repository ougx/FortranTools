! Created by Michael Ou
! sequential indicator kriging
program ppsis
  use f90getopt
  !use m_mrgref
  use m_inssor
  use rotation
  use normal_dist
  use variogram
  use ieee_arithmetic
  use iso_fortran_env, only: input_unit, error_unit, output_unit
  implicit none

  double precision, parameter :: verysmall = tiny(1.0d0) * 1000
  double precision, parameter :: verylarge = huge(1.0d0) * 1e-3
  double precision, parameter :: zero = 0.0d0

  type(option_s), allocatable  :: opts(:)

  character(1024) :: obsfile, obsfile2, gridfile, facfile, outfile, randpath, samfile, fomt
  character(len=2)  :: opt
  ! inputs
  integer         :: ndim, nind, nobs, ngrid, ndrift, nmax, seed, unbias, nobs2, nmax2, nsim
  double precision            :: maxdist, vmin, vmax, std, avg
  integer, allocatable :: irandpath(:), iobs(:), igrid(:), iobs2(:), mobs(:)
  double precision, allocatable :: obs(:,:,:), obs2(:,:), grid(:,:), obsdrift(:,:,:), griddrift(:,:) ! coordinates and values, variance
  double precision, allocatable :: samples(:)
  type(variog), allocatable(:)     :: vario(:), varioc(:), vario2

  ! local
  integer         :: matsize, npp, npp1, npp1o, npp1g, npp2, nppd, nppdu, mgrid
  integer         :: ifile, ioerr, ifilefac, i, ii, jj, kk, ig, isim, ivario, ivarioc, iobs1, iind
  double precision, allocatable :: oodist(:,:), ogdist(:,:), ggdist(:,:), oodist2(:,:), ogdist2(:,:), coodist(:,:), tmpdist(:)
  double precision, allocatable :: weights(:), matA(:,:), rhsB(:), obsval(:), cov0(:,:)
  integer, allocatable :: inear(:), inear2(:)
  logical         :: correct_weight, verbose


  allocate(opts, source=(/&
    option_s("dim"    ,  "d", 6, "space dimension; number of indicator; number of observations for each indicator, split by comma; number of grid; number of covariate observation. number of drifts."), &
    option_s("obsfile",  "o", 1, "observation data file. the columns should be x,(y,z),obsvalue in that file. drift value columns can be added after obsvalue if drift is used. This should be repeated for number of indicators."), &
    option_s("facfile",  "f", 1, "interpolation factor file. the columns should be gindex,nobs,iobs1,iobs2... in that file."), &
    option_s("gridfile", "g", 1, "grid file. the columns should be x,(y,z) in that file. drift value columns can be added if drift is used."), &
    option_s("randpath", "r", 1, "a file contain the indices of the random path, if not defined, the random path will be generated"), &
    option_s("samfile",  "s", 1, "a file contain samples from standard normal distribution."), &
    option_s("obsfile2","o2", 1, "secondary observation data file for the covariate. the columns should be x,(y,z),obsvalue in that file."), &
    option_s("seed",    "sd", 1, "seed number to generate random path and variance."), &
    option_s("nsim",    "ns", 1, "number of simulation. default is 1. Kriging is used when it is 0."), &
    option_s("nmax",    "nm", 1, "maximum number of primary variable used for Kriging. default is 200."), &
    option_s("nmax2",   "m2", 1, "maximum number of co-variate used for Kriging. default is 200."), &
    option_s("unbias",   "u", 0, "whether to include unbias term; default is .false. for Simple Kriging; switch on for Ordinary Kriging;."), &
    option_s("ang1",    "a1", 1, "set azimuth angle for principal direction; default is 0."), &
    option_s("ang2",    "a2", 1, "set dip angle for principal direction; default is 0."), &
    option_s("ang3",    "a3", 1, "set third rotation angle; default is 0."), &
    option_s("anis1",   "s1", 1, "set first anisotropy ratio; default is 1."), &
    option_s("anis2",   "s2", 1, "set second anisotropy ratio; default is 1."), &
    option_s("vario",    "v", 4, "set variogram, following type, range, sill, nugget; type must be sph, exp, gau, pow, cir, hol or lin. This should be repeated for the number of indicators."), &
    option_s("vario2",  "v2", 4, "set variogram for the secondary covariate."), &
    option_s("varioc",  "vc", 4, "set cross-variogram between the primary and secondary variables. This should be repeated for the number of indicators."), &
    option_s("bounds",  "bs", 2, "set the lower and upper bounds, if not set, the simulation is unbounded."), &
    option_s("maxdist", "md", 1, "set the maximum distance for search, this is the rotated/anisotropic distance if there is rotation/anisotropy."), &
    option_s("correct",  "c", 0, "apply weight correction. default is no weight correction."), &
    option_s("fmt"    , "fm", 1, "fortran format to write results such as '(10F10.3)'; default is '(G0.12)'."), &
    option_s("verbose", "vb", 0, "print running logs to screen."), &
    option_s("help"   ,  "h", 0, "show this message.") &
  /))


  ! set up the initial/default values
  fomt = '(10(G0.12,:x))'
  outfile = '~'
  obsfile = ''
  obsfile2 = ''
  gridfile = ''
  facfile = ''
  randpath = ''
  samfile = ''
  maxdist = -1.0
  nmax = 200
  nmax2 = 0
  ndrift = -1
  unbias = 0
  seed = 0
  ang1   = 0
  ang2   = 0
  ang3   = 0
  anis1  = 1
  anis2  = 1
  nsim = 1
  correct_weight = .false.
  ndim = 2
  nobs = 0
  nobs2 = 0
  ngrid = 0
  vmin = -verylarge
  vmax =  verylarge
  verbose = .false.
  ivario = 0
  ivarioc =0
  iobs1 = 0
  do
    opt = getopt(opts)
    select case(trim(opt))
      ! When all options are processed
      case(char(0))
        if (len_trim(optarg)>0) outfile = trim(optarg)
        exit

      case( "d")
        read(optarg, *) ndim, nind, nobs, ngrid, nobs2, ndrift
        allocate(irandpath(ngrid))
        allocate(obs(ndim+1, nobs, nind))     ! coordinates plus values
        allocate(iobs(nobs))
        allocate(grid(ndim+1, ngrid))   ! coordinates plus values
        allocate(igrid(ngrid))
        allocate(samples(ngrid))
        allocate(vario(nind))
        allocate(cov0(3,nind))
        allocate(mobs(nind))            ! number of observations for each indicator; nobs is the maximum number
        if (nobs2>0) then
          allocate(obs2(ndim+1, nobs2)) ! coordinates plus values
          allocate(iobs2(nobs2))
          allocate(varioc(nind), vario2)
        end if
        if (ndrift>0) then
          allocate(griddrift(ndrift, ngrid))
          allocate(obsdrift (ndrift, nobs, nind))
        end if
        mobs = nobs

      case( "o"); iobs1=iobs1+1; read(optarg, *)  obsfile; call readobs(1)

      case("o2"); read(optarg, *) obsfile2; if (nobs2>0) call readobs(2)

      case( "g"); read(optarg, *) gridfile; call readgrid()

      case( "u"); unbias = 1

      case( "c"); correct_weight = .true.

      case( "f"); read(optarg, *) facfile

      case( "r"); read(optarg, *) randpath

      case( "s"); read(optarg, *) samfile

      case("sd"); read(optarg, *) seed

      case("ns"); read(optarg, *) nsim

      case("nm"); read(optarg, *) nmax
      case("m2"); read(optarg, *) nmax2

      case("a1"); read(optarg, *) ang1
      case("a2"); read(optarg, *) ang2
      case("a3"); read(optarg, *) ang3
      case("s1"); read(optarg, *) anis1
      case("s2"); read(optarg, *) anis2
      case("bs"); read(optarg, *) vmin, vmax
      case("md"); read(optarg, *) maxdist

      case( "v"); ivario =ivario+1 ; read(optarg, *) vario(ivario) ; cov0(1:1, ivario) = covfuc(vario , [ zero ])
      case("vc"); ivarioc=ivarioc+1; read(optarg, *) varioc(ivario); cov0(2:2, ivarioc) = covfuc(varioc, [ zero ])
      case("v2"); read(optarg, *) vario2; cov0(3:3,:) = covfuc(vario2, [ zero ])(0)

      case("fm"); read(optarg, *) fomt

      case("vb"); verbose = .true.

      case("h"); call showhelp

      case default; stop
    end select
    if (ndrift==-1) exit
  end do
  if (ndrift==-1) call perr("  Error: Dimension needs to be defined as the first argument")

  ! set up random seed
  if (nsim>0) then
    call random_seed_initialize(seed)
    call set_samples()
  end if

  if (nobs2>0) then
    if (nmax2 == 0) nmax2 = nmax
    if (nmax2 > nobs2) nmax2 = nobs2
  end if
  if (nmax>nobs+ngrid-1) nmax=nobs+ngrid-1

  matsize = nmax+unbias+ndrift+nmax2
  allocate(weights(matsize))

  allocate(inear(nobs+ngrid))
  if (nobs2>0) then
    allocate(inear2(nobs2))
    inear2 = [(ii,ii=1,nobs2)]
    npp2 = nmax2
  else
    npp2 = 0
  end if

  allocate(matA(matsize, matsize), rhsB(matsize), tmpdist(nobs+ngrid))

  call setrot()
  if (nsim>0) then
    call set_randpath()
    grid = grid(:, irandpath)
  else
    irandpath=[(ig, ig=1, ngrid)]
  end if

  ! calculate distance
  call setdist()

  if (verbose) print*, 'Factors written in "'//trim(facfile)//'"'
  open(newunit=ifilefac, file=trim(facfile), status='replace')
  write(ifilefac, '(A,*(:" index",I0," weight",I0))') 'igrid nobs1 ngrid nobs2 std', (ii,ii,ii=1,nmax+nmax2)

  if (verbose) open (unit=6, carriagecontrol='fortran')

  grid: do ig = 1, ngrid
    ! search for the nearest obs
    if (verbose) call progress(real(ig)/real(ngrid))
    ! already evaluated grid cells
    mgrid = ig-1 ! minus 1 to exclude the cell itself
    if (nsim==0) mgrid=0   ! if not a simulation, grid cell will be excluded as obs
    ! evaluate probability at this cell for each indicator
    indicator: do iind=1, nind
      nobs = mobs(iind)
      if (nobs+mgrid>nmax) then
        tmpdist(1:nobs) = ogdist(:, ig)
        if (mgrid > 0) tmpdist(nobs+1:nobs+mgrid) = ggdist(1:mgrid, ig)
        inear(1:nmax) = search_nearest(tmpdist(1:nobs+mgrid), nobs+mgrid, nmax)
        npp1 = nmax
        npp1o = count(inear(1:npp1)<=nobs)
        npp1g = npp1 - npp1o
      else
        ! use all points
        npp1o = nobs
        npp1g = mgrid
        npp1 = nobs + mgrid
        inear(1:npp1) = [(ii,ii=1,npp1)]
      end if

      ! search for the nearest covariate
      if (nobs2>0 .and. nmax2<nobs2) then
        inear2(1:nmax2) = search_nearest(ogdist2(:, ig), nobs2, nmax2)
      end if

      if (maxdist>0) then
        npp   = count(tmpdist(inear(1:npp1))<=maxdist)
        if (npp /= npp1) then
          npp1 = npp
          npp1o = count(inear(1:npp1)<=nobs)
          npp1g = npp1 - npp1o
        end if
        npp   = count(ogdist2(inear2(1:npp2), ig)<=maxdist)
        if (npp /= npp2) then
          npp2 = npp
        end if
      end if

      call inssor(inear(1:npp1))

      npp = npp1 + npp2       ! total obs points
      nppd  = npp + ndrift    ! obs points + drifts
      nppdu = nppd + unbias   ! obs points + drifts + unbias

      ! construct linear system
      matA = 0.0
      rhsB = 0.0

      ! obs1
      do ii=1, npp1o
        kk = inear(ii)
        matA(ii, ii)         = cov0(1)                                                                   ! diagonal
        matA(ii+1:npp1o, ii) = covfuc(vario, oodist(inear(ii+1:npp1o), kk))                              ! obs ~ obs
        if (npp1g  > 0) matA(npp1o+1:npp1, ii) = covfuc(vario , ogdist (kk, inear(npp1o+1:npp1)-nobs ))  ! obs ~ grid
        if (npp2   > 0) matA(npp1 +1:npp , ii) = covfuc(varioc, coodist(inear2(1:npp2)         , kk  ))  ! obs ~ obs2
        if (ndrift > 0) matA(npp  +1:nppd, ii) = obsdrift(:,kk)                                          ! obsdrift
        if (unbias > 0) matA(nppdu, ii) = 1                                                              ! unbias
        rhsB(ii:ii) = covfuc(vario, ogdist(kk:kk, ig))                                                   ! right hand side
      end do

      ! grids
      do ii=npp1o+1, npp1
        kk = inear(ii)-nobs
        matA(ii, ii)         = cov0(1)                                                                   ! diagonal
        if (npp1g  > 0) matA(npp1o+1:npp1, ii) = covfuc(vario , ggdist (inear(npp1o+1:npp1)-nobs, kk))   ! grid ~ grid
        if (npp2   > 0) matA(npp1 +1:npp , ii) = covfuc(varioc, ogdist2(inear2(1:npp2),           kk))   ! grid ~ obs2
        if (ndrift > 0) matA(npp  +1:nppd, ii) = griddrift(:,kk)                                         ! griddrift
        if (unbias > 0) matA(nppdu, ii) = 1                                                              ! unbias
        rhsB(ii:ii) = covfuc(vario, ggdist(kk:kk, ig))                                                   ! right hand side
      end do

      ! if (verbose) print *, '  Kriging Cell2',ig
      ! obs2
      do ii=npp1+1, npp
        kk = inear2(ii-npp1)
        matA(ii, ii)          = cov0(3)                                                                  ! diagonal
        matA(npp1+1: npp, ii) = covfuc(vario2, oodist2(inear2(1:npp2), kk))                              ! obs2 ~ obs2
        if (unbias > 0) matA(nppdu, ii) = 1                                                              ! unbias
        rhsB(ii:ii) = covfuc(varioc, ogdist2(kk:kk, ig))                                                 ! right hand side
      end do

      ! if (verbose) print *, '  Kriging Cell3',ig
      ! drift
      if (ndrift > 0) rhsB(npp+1:nppd) = griddrift(:,ig)

      ! unbias
      if (unbias > 0) rhsB(nppdu)      = 1

      call krige()
      std = sqrt(cov0(1) - sum(rhsB(1:nppdu) * weights(1:nppdu)))
      write(ifilefac, '(4(I0,x),G0.12,*(:x,I0,x,F0.10))') irandpath(ig), npp1o, npp1g, npp2, std, &
        (inear(ii)     , weights(ii),     ii=      1,npp1o), &
        (inear(ii)-nobs, weights(ii),     ii=npp1o+1,npp1), &
        (inear2(ii)    , weights(npp1+ii),ii=      1,npp2)
    end do indicator ! indicator
  end do grid ! grid
  close(6)
  close(ifilefac)

  contains

  function linecount(afile)
    integer         :: linecount
    character(1024) :: afile
    open(newunit=ifile, file=trim(afile), status='old')
    linecount = 0
    do
      read(ifile,*, iostat=ioerr)
      if (ioerr/=0) EXIT
      linecount = linecount + 1
    end do
    close(ifile)
  end function

  !@(#) M_random::scramble(3f): return integer array of random values 1 to N.
  ! https://fortranwiki.org/fortran/show/scramble
  function scramble( number_of_values ) result(array)

    integer,intent(in)    :: number_of_values
    integer,allocatable   :: array(:)
    integer               :: i, j, k, m, n
    integer               :: temp
    real                  :: u

    array=[(i,i=1,number_of_values)]

    ! The intrinsic RANDOM_NUMBER(3f) returns a real number (or an array
    ! of such) from the uniform distribution over the interval [0,1). (ie.
    ! it includes 0 but not 1.).
    !
    ! To have a discrete uniform distribution on
    ! the integers {n, n+1, ..., m-1, m} carve the continuous distribution
    ! up into m+1-n equal sized chunks, mapping each chunk to an integer.
    !
    ! One way is:
    !   call random_number(u)
    !   j = n + FLOOR((m+1-n)*u)  ! choose one from m-n+1 integers

    n=1
    m=number_of_values
    do k=1,2
        do i=1,m
          call random_number(u)
          j = n + FLOOR((m+1-n)*u)
          ! switch values
          temp=array(j)
          array(j)=array(i)
          array(i)=temp
        enddo
    enddo
  end function scramble

  ! search for the k smallest values in an unsorted array, return the index of these numbers
  function search_nearest(dist, n, k) result(idx_nearest)
    integer                     :: n, k
    double precision            :: dist(n)
    integer                     :: idx_nearest(k)
    ! local
    double precision            :: vm
    integer                     :: imax, ii
    idx_nearest = [(ii,ii=1, k)]
    imax = maxloc(dist(:k), dim=1)
    vm = dist(imax)
    do ii = k+1, n
      if(dist(ii)<vm) then
        idx_nearest(imax) = ii
        imax = maxloc(dist(idx_nearest), dim=1)
        vm = dist(idx_nearest(imax))
      end if
    end do
  end function

  subroutine readdata(afile, nn, idx, arr, arr1)
    character(1024)             :: afile
    integer                     :: nn
    integer                     :: idx(:)
    double precision            :: arr(:,:)
    double precision, optional  :: arr1(:,:)
    open(newunit=ifile, file=trim(afile), status='old')
    read(ifile,*, iostat=ioerr)
    do ii = 1, nn
      if (present(arr1)) then
        read(ifile,*, iostat=ioerr) idx(ii), arr(:, ii), arr1(:, ii)
      else
        read(ifile,*, iostat=ioerr) idx(ii), arr(:, ii)
      end if
      if (ioerr<0) exit ! end of file
    end do
    nn = ii
    close(ifile)
  end subroutine readdata

  subroutine readdata_1cold(afile, nn, arr)
    character(1024) :: afile
    integer         :: nn
    double precision:: arr(nn)
    open(newunit=ifile, file=trim(afile), status='old')
    read(ifile,*, iostat=ioerr)
    read(ifile,*, iostat=ioerr) arr
    close(ifile)
  end subroutine readdata_1cold

  subroutine readgrid()
    ! reading grid file
    if (verbose) print*, 'Reading GRID in "'//trim(gridfile)//'"'
    if (ngrid==0) ngrid = linecount(gridfile) - 1
    if (ndrift>0) then
      call readdata(gridfile, ngrid, igrid, grid(1:ndim,:), griddrift)
    else
      call readdata(gridfile, ngrid, igrid, grid(1:ndim,:))
    end if
  end subroutine readgrid

  subroutine readobs(ivar)
    ! reading OBS file
    integer     :: ivar
    if (ivar==1) then
      if (verbose) print*, 'Reading OBS in "'//trim(obsfile)//'"'
      if (nobs==0) nobs = linecount(obsfile) - 1
      if (ndrift>0) then
        call readdata(obsfile, mobs(iobs1), iobs, obs(:,:,iobs1), obsdrift(:,:,iobs1))
      else
        call readdata(obsfile, mobs(iobs1), iobs, obs)
      end if
    else
      if (verbose) print*, 'Reading covariate OBS in "'//trim(obsfile2)//'"'
      if (nobs2==0) nobs2 = linecount(obsfile) - 1
      call readdata(obsfile2, nobs2, iobs2, obs2)
    end if
  end subroutine readobs

  subroutine set_randpath()
    if (randpath == '') then
      if (verbose) print*, 'Generating random path'
      irandpath = scramble(ngrid)
      open(newunit=ifile, file=trim(facfile)//'_path', status='replace')
      write(ifile, '(A)') 'randpath'
      do ii=1, ngrid; write(ifile, '(I0)') irandpath(ii); end do
      close(ifile)
    else
      if (verbose) print*, 'Reading random path in "'//trim(randpath)//'"'
      call readdata_1cold(randpath, ngrid, samples); irandpath = int(samples)
    end if
  end subroutine set_randpath

  subroutine set_samples()
    if (samfile == '') then
      if(nsim==0) then
        if (verbose) print*, 'No simulation. All samples are set to 0.'
        samples = 0.0d0
      else
        if (verbose) print*, 'Generating samples (uniform distribution)'
        do ii=1, ngrid;  call random_number(samples(ii)); end do
      end if
    else
      if (verbose) print*, 'Reading samples in "'//trim(samfile)//'"'
      call readdata_1cold(samfile, ngrid, samples)
    end if
  end subroutine set_samples

  ! calculate distance between obs and grid
  function cdist(coord1, coord2)
    integer                       :: n1, n2
    double precision              :: coord1(:,:), coord2(:,:)
    double precision, allocatable :: cdist(:,:)
    n1 = size(coord1, dim=2)
    n2 = size(coord2, dim=2)
    allocate(cdist(n1,n2))
    do ii = 1, n1
      do jj = 1, n2
        cdist(ii, jj) = rotated_dist(ndim, coord1(1:ndim, ii), coord2(1:ndim, jj))
      end do
    end do
  end function cdist

  ! calculate distance between obs (upper triangular matrix)
  function pdist(coord1)
    integer                       :: n
    double precision              :: coord1(:,:)
    double precision, allocatable :: pdist(:,:)

    n = size(coord1, dim=2)
    allocate(pdist(n, n))
    pdist = 0.0
    do ii = 1, n
      do jj = ii+1, n
        pdist(jj, ii) = rotated_dist(ndim, coord1(1:ndim, ii), coord1(1:ndim, jj))
        pdist(ii, jj) = pdist(jj, ii)
      end do
    end do
    !print*, pdist
  end function pdist

  ! read factor file
  subroutine readfactor()
    if (verbose) print*, 'Read factors in "'//trim(facfile)//'"'
    open(newunit=ifile, file=trim(facfile), status='old')
    close(ifile)
  end subroutine readfactor

  subroutine perr(msg)
    character(*) :: msg
    write(error_unit, '(A)') msg
    stop
  end subroutine perr

  subroutine setdist()
    if (verbose) print*, 'Calculating distances ...'
    oodist = pdist(obs(1:ndim, :))                          ! distance of primary variate ~ primary variate
    ggdist = pdist(grid(1:ndim, :))                         ! distance of grid ~ grid
    ! print*, 'ggdist'
    ! print '(4(G0.12,x))', ggdist(1:4,1:4)
    ogdist = cdist(obs(1:ndim, :), grid(1:ndim, :))         ! distance of grid ~ primary variate
    ! print*, 'ogdist'
    ! print '(4(G0.12,x))', ogdist(1:4,1:4)
    if (nobs2>0) then
      oodist2 = pdist(obs2(1:ndim, :))                      ! distance of covariate ~ covariate
      ! print*, 'oodist2'
      ! print '(4(G0.12,x))', oodist2(1:4,1:4)
      ogdist2 = cdist(obs2(1:ndim, :), grid(1:ndim, :))     ! distance of grid ~ covariate
      ! print*, 'ogdist2'
      ! print '(4(G0.12,x))', ogdist2(1:4,1:4)
      coodist = cdist(obs2(1:ndim, :), obs(1:ndim, :))      ! distance of variate ~ covariate
      ! print*, 'coodist'
      ! print '(4(G0.12,x))', coodist(1:4,1:4)
    end if
  end subroutine setdist

  subroutine krige()
    real, allocatable       ::  AP(:), B(:)
    integer, allocatable    ::  IPIV(:)
    integer                 ::  INFO
    ! nppdu is the mat size ~ obs + drift + unbias
    allocate(AP(nppdu*(nppdu+1)/2), B(nppdu), IPIV(nppdu))
    ! store the matrix to column compact form
    kk = 1
    do ii=1, nppdu
      AP(kk:kk+ii-1) = real(matA(ii, 1:ii))
      kk = kk + ii
    end do
    B = real(rhsB(1:nppdu))
    call SSPSV( 'U', nppdu, 1, AP, IPIV, B, nppdu, INFO )
    if (INFO /= 0) call perr('Failed to find solution of the linear system.')

    if (correct_weight) then
      weights(1:nppdu) = merge(B(1:nppdu), 0.0, B(1:nppdu)>0)
      weights(1:nppdu) = weights(1:nppdu) / sum(weights(1:nppdu))
    else
      weights(1:nppdu) = B(1:nppdu)
    end if
  end subroutine krige

  subroutine write_output(arr)
    double precision    :: arr(:)
    if (trim(outfile)=='~') then
      ifile = output_unit ! print to stdout
    else
      open(newunit=ifile, file=outfile, status='replace')
    end if

    write(ifile, fomt) arr

    if (ifile /= output_unit) close(ifile)
    if (verbose) print*, 'Results have been written successfully'
  end subroutine write_output

  subroutine showhelp()
    print "(A)", ''
    print "(A)", ''
    print "(A)", ' ppsgs -d ndim nobs ngrid nobs2 ndrift -o obsfile [-g gridfile] [-f facfile] [-o2 obsfile2] [options] [output]'
    print "(A)", ''
    print "(A)", '   Generate sequential gaussian simulation.'
    print "(A)", ' '
    print "(A)", '   Arguments:'
    do ii=1, 3
      print "(A)", '      -'//opts(ii)%short//'  or  --'//opts(ii)%name(:10)//trim(opts(ii)%description)
    end do
    print "(A)", ' '
    print "(A)", '   Optional arguments:'
    do ii=4, size(opts)
      print "(A)", '      -'//opts(ii)%short//'  or  --'//opts(ii)%name(:10)//trim(opts(ii)%description)
    end do
    print "(A)", '      output               output file name. If output=="~" or omitted, output will print to screen.'
    print "(A)", ' '
    stop
    end subroutine showhelp
  end program
