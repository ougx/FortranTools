! Created by Michael Ou
! TODO: drifts
program ppsgs
  use f90getopt
  !use m_mrgref
  use m_inssor
  use rotation
  use normal_dist
  use variogram
  use kdtree2_module
  !use ieee_arithmetic
  use iso_fortran_env, only: input_unit, error_unit, output_unit
  implicit none

  real, parameter :: verysmall = tiny(1.0e0) * 1000
  real, parameter :: verylarge = huge(1.0e0) * 1e-3
  real, parameter :: zero = 0.0e0

  type(kdtree2),  pointer           :: obstree, obstree2
  type(option_s), allocatable       :: opts(:)
  type(kdtree2_result), allocatable :: kdnearest(:),kdnearest2(:)

  character(2048) :: obsfile, obsfile2, gridfile, facfile, outfile, randpath, samfile, fomt
  character(len=2)  :: opt
  ! inputs
  integer         :: ndim, nobs, ngrid, ndrift, nmax, seed, unbias, nobs2, nmax2, nsim
  real            :: maxdist, vmin, vmax, std, avg, avg2
  integer, allocatable :: irandpath(:), iobs(:), igrid(:), iobs2(:)
  logical, allocatable :: kdmask(:)
  real, allocatable ::  obs(:,:),  obs2(:,:),  grid(:,:), obsdrift(:,:), griddrift(:,:) ! coordinates and values, variance
  real, allocatable :: robs(:,:), rob2(:,:)                                ! rotated, coordinates for search and variogram
  real, allocatable :: samples(:)
  type(variog)     :: vario, varioc, vario2
  ! local
  integer         :: matsize, npp, npp1, npp1o, npp1g, npp2, nppd, nppdu, mgrid
  integer         :: ifile, ioerr, ifilefac, i, ii, jj, kk, ig, isim
  real            :: cov0(3)
  real, allocatable :: tmpdist(:), tmpdist2(:), xcell(:)
  real, allocatable :: weights(:), matA(:,:), rhsB(:), obsval(:)
  integer, allocatable :: inear(:), inear2(:)
  logical         :: correct_weight, writexy, verbose


  allocate(opts, source=(/&
    option_s("dim"    ,  "d", 5, "space dimension; number of observation; number of grid; number of covariate observation. number of drifts."), &
    option_s("obsfile",  "o", 1, "observation data file. the columns should be x,(y,z),obsvalue in that file. drift value columns can be added after obsvalue if drift is used."), &
    option_s("facfile",  "f", 1, "interpolation factor file. the columns should be gindex,nobs,iobs1,iobs2... in that file. This file will be generated using this tool when grid file is defined."), &
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
    option_s("vario",    "v", 4, "set variogram, following type, range, sill, nugget; type must be sph, exp, gau, pow, cir, hol or lin."), &
    option_s("vario2",  "v2", 4, "set variogram for the secondary covariate."), &
    option_s("varioc",  "vc", 4, "set cross-variogram between the primary and secondary variables."), &
    option_s("bounds",  "bs", 2, "set the lower and upper bounds, if not set, the simulation is unbounded."), &
    option_s("maxdist", "md", 1, "set the maximum distance for search, this is the rotated/anisotropic distance if there is rotation/anisotropy."), &
    option_s("correct",  "c", 0, "apply weight correction by removing negative weights. default is no weight correction."), &
    option_s("fmt"    , "fm", 1, "fortran format to write results such as '(10F10.3)'; default is '(G0.12)'."), &
    option_s("writexy", "xy", 0, "write coordinates in the output; default only estimates are written."), &
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
  ang1   = 0.0
  ang2   = 0.0
  ang3   = 0.0
  anis1  = 1.0
  anis2  = 1.0
  nsim = 1
  correct_weight = .false.
  ndim = 2
  nobs = 0
  nobs2 = 0
  ngrid = 0
  vmin = -verylarge
  vmax =  verylarge
  writexy = .false.
  verbose = .false.
  ! check if verbose is set
  do
    opt = getopt(opts)
    if (trim(trim(opt)) == 'vb') then
      verbose=.true.
      exit
    end if
    if (trim(opt) == char(0)) exit
  end do
  call reset_opt()
  do
    opt = getopt(opts)
    select case(trim(opt))
      ! When all options are processed
      case(char(0))
        if (len_trim(optarg)>0) outfile = adjustl(trim(optarg))
        exit

      case( "d")
        read(optarg, *) ndim, nobs, ngrid, nobs2, ndrift
        allocate(irandpath(ngrid))
        allocate(obs(ndim+1, nobs))     ! coordinates plus values
        allocate(iobs(nobs))
        allocate(grid(ndim+2, ngrid))   ! coordinates plus values
        allocate(igrid(ngrid))
        allocate(samples(ngrid))
        if (nobs2>0) then
          allocate(obs2(ndim+1, nobs2)) ! coordinates plus values
          allocate(iobs2(nobs2))
        end if
        if (ndrift>0) then
          allocate(griddrift(ndrift, ngrid))
          allocate(obsdrift (ndrift, nobs))
        end if

      case( "o"); obsfile =adjustl(optarg); call readobs(1)

      case("o2"); obsfile2=adjustl(optarg); if (nobs2>0) call readobs(2)

      case( "g"); gridfile=adjustl(optarg); call readgrid()

      case( "u"); unbias = 1

      case( "c"); correct_weight = .true.

      case( "f"); facfile=adjustl(optarg)

      case( "r"); randpath=adjustl(optarg)

      case( "s"); samfile=adjustl(optarg)

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

      case( "v"); read(optarg, *) vario ; cov0(1) = vario %sill + vario %nugget
      case("vc"); read(optarg, *) varioc; cov0(2) = varioc%sill + varioc%nugget
      case("v2"); read(optarg, *) vario2; cov0(3) = vario2%sill + vario2%nugget

      case("fm"); read(optarg, *) fomt

      case("xy"); writexy = .true.
      case("vb"); verbose = .true.

      case("h"); call showhelp

      case default; stop
    end select
    if (ndrift==-1) exit
  end do
  if (ndrift==-1) call perr("  Error: Dimension needs to be defined as the first argument")

  ! set up random seed
  call random_seed_initialize(seed)
  call set_samples()
  if (nobs2>0) then
    if (nmax2 == 0) nmax2 = nmax
    if (nmax2 > nobs2) nmax2 = nobs2
  else
    nmax2 = 0
  end if
  if (nmax>nobs+ngrid-1) nmax=nobs+ngrid-1
  call setrot()

  if (gridfile == '') then
    ! read the factors from file
    if (verbose) print*, 'Estimating values using factors in "'//trim(facfile)//'"'
    writexy = .false.
    allocate(inear (nobs+ngrid-1))
    allocate(obsval(nobs+ngrid-1))
    allocate(weights(nobs+ngrid+nobs2-1))
    if (nobs2>0) then
      allocate(inear2(nobs2-1))
    end if
    open(newunit=ifilefac, file=trim(facfile), status='old')
    read(ifilefac, *)
    do ig = 1, ngrid
      read(ifilefac, *) irandpath(ig), npp1o, npp1g, npp2, std,           &
        (inear(ii)     , weights(ii),            ii=      1,npp1o      ), &
        (inear(ii)     , weights(ii),            ii=npp1o+1,npp1o+npp1g), &
        (inear2(ii)    , weights(npp1o+npp1g+ii),ii=      1,npp2)
      npp1 = npp1o + npp1g
      npp  = npp1 + npp2

      obsval(      1:npp1o)  = obs (ndim+1, inear (      1:npp1o))
      obsval(npp1o+1:npp1 )  = grid(ndim+1, irandpath(inear (npp1o+1:npp1 )))
      obsval(npp1 +1:npp  )  = obs2(ndim+1, inear2(      1:npp2 ))
      avg = sum(obsval(1:npp1)) / npp1      ! the average of obs1 and grid
      ! if (unbias==0) obsval(1:npp1) = obsval(1:npp1) - avg
      if (npp2>0) then
        avg2 = sum(obsval(npp1+1:npp)) / npp2
        obsval(npp1+1:npp) = obsval(npp1+1:npp) + avg - avg2  ! ISAAKS and SRIVASTAVA, An Introduction to Applied Geostatistics, pp410
      end if
      kk = irandpath(ig)
      grid(ndim+1, kk) = sum(weights(1:npp) * obsval(1:npp))
      ! if (unbias==0) then
      !   grid(ndim+1, kk) = (1.0 - sum(weights(1:npp1))) * avg
      ! end if
      grid(ndim+1, kk) = grid(ndim+1, kk) + samples(ig) * std
      if (grid(ndim+1, kk) < vmin) grid(ndim+1, kk) = vmin
      if (grid(ndim+1, kk) > vmax) grid(ndim+1, kk) = vmax
    end do
    !if (nsim==0) then
    call write_output()
    !else
    !  ! put back in correct order
    !  call mrgref(irandpath, inear(1:ngrid))
    !  call write_output(grid(ndim+1, inear(1:ngrid)))
    !end if
  else
    ! Krige the factors
    matsize = nmax+unbias+ndrift+nmax2
    allocate(weights(matsize))
    allocate(matA(matsize, matsize), rhsB(matsize), kdnearest(nmax))
    if (nobs2>0) allocate(kdnearest2(nmax2))

    if (nsim>0) then
      call set_randpath()
      grid = grid(:, irandpath)
      igrid = igrid(irandpath)
    else
      irandpath=[(ig, ig=1, ngrid)]
    end if

    ! calculate distance
    call setdist()

    if (facfile/='') then
      if (verbose) print*, 'Factors written in "'//trim(facfile)//'"'
      open(newunit=ifilefac, file=trim(facfile), status='replace')
      write(ifilefac, '(A,*(:" index",I0," weight",I0))') 'igrid nobs1 ngrid nobs2 std', (ii,ii,ii=1,nmax+nmax2)
    else
      ifilefac = 0
    end if

#ifdef __INTEL_COMPILER
    if (verbose) open (unit=6, carriagecontrol='fortran')
#endif

    do ig = 1, ngrid

      ! search for the nearest obs
      if (verbose) call progress(real(ig)/real(ngrid))
      ! already evaluated grid cells
      mgrid = ig-1 ! minus 1 to exclude the cell itself
      if (nsim==0) then
        mgrid=0   ! if not a simulation, grid cell will not be included as obs
      else
        kdmask(nobs+mgrid) = .true.
      end if

      xcell = robs(:ndim,nobs+ig)
      if (nobs+mgrid>nmax) then
        ! print*, ig, 'search for neighbor1'
        call kdtree2_n_nearest(obstree,xcell,nmax,kdnearest,kdmask)
        inear = kdnearest%idx
        npp1 = nmax
        npp1o = count(inear(1:npp1)<=nobs)
        npp1g = npp1 - npp1o
        call inssor(inear(1:npp1))
      else
        ! use all points
        npp1o = nobs
        npp1g = mgrid
        npp1 = nobs + mgrid
        inear = [(ii,ii=1,npp1)]
      end if

      if (nobs2>0) then
        if (nmax2<nobs2) then
          ! print*, ig, 'search for neighbor2'
          npp2 = nmax2
          call kdtree2_n_nearest(obstree2,xcell,nmax2,kdnearest2)
          inear2 = kdnearest2%idx
        else
          npp2 = nobs2
          inear2 = [(ii,ii=1,npp2)]
        end if
      else
        npp2 = 0
      end if

      tmpdist               = sdistn1(robs(:,inear ) ,robs(:,nobs+ig))
      if (nobs2>0) tmpdist2 = sdistn1(rob2(:,inear2) ,robs(:,nobs+ig))

      ! calculate the distance from the unknown grid cell to all data points
      if (maxdist>0) then
        npp   = count(tmpdist<=maxdist)
        if (npp /= npp1) then
          inear   = pack(inear, tmpdist<=maxdist)
          tmpdist = pack(tmpdist, tmpdist<=maxdist)
          npp1  = npp
          npp1o = count(inear<=nobs)
          npp1g = npp1 - npp1o
        end if
        if (nobs2>0) then
          npp   = count(tmpdist2<=maxdist)
          if (npp /= npp2) then
            inear2 = pack(inear2, tmpdist2<=maxdist)
            tmpdist2 = pack(tmpdist2, tmpdist2<=maxdist)
            npp2 = npp
          end if
        end if
      end if

      npp = npp1 + npp2       ! total obs points
      nppd  = npp + ndrift    ! obs points + drifts
      nppdu = nppd + unbias   ! obs points + drifts + unbias

      ! construct linear system
      matA = 0.0
      rhsB = 0.0
      ! print "(99I9)", nobs, nobs2, ngrid, nmax2, npp2, npp1o, npp2, ndrift, unbias
      ! print*, 'start to build matrix', sdistn1(robs(:,inear (1+1:npp1o)  ), robs(:,1))
      ! obs1
      do ii=1, npp1o
        kk = inear(ii)
                        matA(ii          , ii) = cov0(1)                                                            ! print*, ii, "diagonal"
        if (npp1o  >ii) matA(ii+1:npp1o  , ii) = covfuc(vario , sdistn1(robs(:,inear (ii+1:npp1o)  ), robs(:,kk)))  ! print*, ii, "obs1 ~ obs1"
        if (npp1g  > 0) matA(npp1o+1:npp1, ii) = covfuc(vario , sdistn1(robs(:,inear (npp1o+1:npp1)), robs(:,kk)))  ! print*, ii, "obs1 ~ grid"
        if (npp2   > 0) matA(npp1 +1:npp , ii) = covfuc(varioc, sdistn1(rob2(:,inear2(1:npp2      )), robs(:,kk)))  ! print*, ii, "obs1 ~ obs2"
        if (ndrift > 0) matA(npp  +1:nppd, ii) = obsdrift(:,kk)                                                     ! print*, ii, "obsdrift"
        if (unbias > 0) matA(nppdu       , ii) = 1.0                                                                ! print*, ii, "unbias"
                        rhsB(ii          : ii) = covfuc(vario , tmpdist(ii:ii))                                     ! print*, ii, "right hand side"
      end do

      ! grids
      do ii=npp1o+1, npp1
        kk = inear(ii)
                        matA(ii          , ii) = cov0(1)                                                            ! print*, ii, "diagonal"
        if (npp1   >ii) matA(ii   +1:npp1, ii) = covfuc(vario , sdistn1(robs(:,inear (ii+1:npp1)   ), robs(:,kk)))  ! print*, ii, "grid ~ grid"
        if (npp2   > 0) matA(npp1 +1:npp , ii) = covfuc(varioc, sdistn1(rob2(:,inear2(1:npp2      )), robs(:,kk)))  ! print*, ii, "grid ~ obs2"
        if (ndrift > 0) matA(npp  +1:nppd, ii) = griddrift(:,kk)                                                    ! print*, ii, "griddrift"
        if (unbias > 0) matA(nppdu       , ii) = 1.0                                                                ! print*, ii, "unbias"
                        rhsB(ii          : ii) = covfuc(vario,  tmpdist(ii:ii))                                     ! print*, ii, "right hand side"
      end do

      ! obs2
      do ii=1, npp2
        kk = inear2(ii)
                        matA(npp1+ii      , npp1+ii) = cov0(3)                                                        ! print*, ii, "diagonal"
        if (npp2   >ii) matA(npp1+ii+1:npp, npp1+ii) = covfuc(vario2, sdistn1(rob2(:,inear2(ii+1:npp2)),rob2(:,kk)))  ! print*, ii, "obs2 ~ obs2"
        if (unbias > 0) matA(nppdu        , npp1+ii) = 1.0                                                            ! print*, ii, "unbias"
                        rhsB(npp1+ii      : npp1+ii) = covfuc(varioc, tmpdist2(ii:ii))                                ! print*, ii, "right hand side"
      end do

      ! drift
      if (ndrift > 0) rhsB(npp+1:nppd) = griddrift(:,ig)

      ! unbias
      if (unbias > 0) rhsB(nppdu)      = 1

      call krige()
      if (correct_weight) then
        where(weights(1:npp)<0.0) weights(1:npp)=0.0
        weights(1:npp) = weights(1:npp) / sum(weights(1:npp))
      end if
      if (ifilefac==0) then
        ! save result to grid for exporting
        obsval = [obs(ndim+1,inear(:npp1o)), grid(ndim+1, inear(npp1o+1:npp1)-nobs), obs2(ndim+1, inear2(1:npp2))]
        if (npp2>0) then
          avg2 = sum(obsval(npp1+1:npp)) / npp2
          obsval(npp1+1:npp) = obsval(npp1+1:npp) + avg - avg2  ! ISAAKS and SRIVASTAVA, An Introduction to Applied Geostatistics, pp410
        end if
        grid(ndim+1, ig) = sum(weights(1:npp) * obsval(1:npp))
        grid(ndim+2, ig) = cov0(1) - sum(rhsB(1:nppdu) * weights(1:nppdu))
        if (nsim>0) grid(ndim+1, ig) = grid(ndim+1, ig) + samples(ig) * grid(ndim+2, ig) ** 0.5
        if (grid(ndim+1, ig) < vmin) grid(ndim+1, ig) = vmin
        if (grid(ndim+1, ig) > vmax) grid(ndim+1, ig) = vmax
      else
        ! save results to factor file
        std = sqrt(cov0(1) - sum(rhsB(1:nppdu) * weights(1:nppdu)))
        write(ifilefac, '(4(I0,x),G0.12,*(:x,I0,x,F0.10))') irandpath(ig), npp1o, npp1g, npp2, std, &
          (inear(ii)     , weights(ii),     ii=      1,npp1o), &
          (inear(ii)-nobs, weights(ii),     ii=npp1o+1,npp1), &
          (inear2(ii)    , weights(npp1+ii),ii=      1,npp2)
      end if
    end do
#ifdef __INTEL_COMPILER
    if (verbose) close(6)
#else
    if (verbose) print *, "" ! start a new line below the progress bar
#endif
    if (ifilefac==0) then
      call write_output()
    else
      close(ifilefac)
    end if
  end if
  if (verbose) print*, "PPSGS exited peacefully."
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
    integer               :: l, j, k, m, n
    integer               :: temp
    real                  :: u

    array=[(l,l=1,number_of_values)]

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
        do l=1,m
          call random_number(u)
          j = n + FLOOR((m+1-n)*u)
          ! switch values
          temp=array(j)
          array(j)=array(l)
          array(l)=temp
        enddo
    enddo
  end function scramble

  ! search for the k smallest values in an unsorted array, return the index of these numbers
  function search_smallest(vals, n, nk) result(idx_nearest)
    integer         :: n, nk
    real            :: vals(n)
    integer         :: idx_nearest(nk)
    ! local
    real            :: valmax
    integer         :: imax, k

    idx_nearest = [(k,k=1, nk)]

    imax = maxloc(vals(:nk), dim=1)
    valmax = vals(imax)

    do k = nk+1, n
      if(vals(k)<valmax) then
        ! print "(I10,12F10.1)", k,vals(k),valmax,vals(idx_nearest)
        idx_nearest(imax) = k
        imax = maxloc(vals(idx_nearest), dim=1)
        valmax = vals(idx_nearest(imax))
      end if
    end do
  end function

  subroutine readdata(afile, nn, idx, arr, arr1)
    character(1024)             :: afile
    integer                     :: nn
    integer                     :: idx(:)
    real            :: arr(:,:)
    real, optional  :: arr1(:,:)
    open(newunit=ifile, file=trim(afile), status='old')
    read(ifile,*, iostat=ioerr)
    do ii = 1, nn
      if (present(arr1)) then
        read(ifile,*, iostat=ioerr) idx(ii), arr(:, ii), arr1(:, ii)
      else
        read(ifile,*, iostat=ioerr) idx(ii), arr(:, ii)
      end if
    end do
    close(ifile)
  end subroutine readdata

  subroutine readdata_1cold(afile, nn, arr)
    character(1024) :: afile
    integer         :: nn
    real:: arr(nn)
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
    !do ii =1, 5; print*, grid(:,ii); end do
  end subroutine readgrid

  subroutine readobs(ivar)
    ! reading OBS file
    integer     :: ivar
    if (ivar==1) then
      if (verbose) print*, 'Reading OBS in "'//trim(obsfile)//'"'
      if (nobs==0) nobs = linecount(obsfile) - 1
      if (ndrift>0) then
        call readdata(obsfile, nobs, iobs, obs, obsdrift)
      else
        call readdata(obsfile, nobs, iobs, obs)
      end if
      !do ii =1, 5; print*,obs(:,ii); end do
    else
      if (verbose) print*, 'Reading covariate OBS in "'//trim(obsfile2)//'"'
      if (nobs2==0) nobs2 = linecount(obsfile2) - 1
      call readdata(obsfile2, nobs2, iobs2, obs2)
      !do ii =1, 5; print*,obs2(:,ii); end do
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
        samples = 0.0e0
      else
        if (verbose) print*, 'Generating samples'
        call r8vec_normal_01 ( ngrid, samples )
        open(newunit=ifile, file=trim(facfile)//'_sample', status='replace')
        write(ifile, '(A)') 'sample'
        do ii=1, ngrid; write(ifile, '(G0.10)') samples(ii); end do
        close(ifile)
      end if
    else
      if (verbose) print*, 'Reading samples in "'//trim(samfile)//'"'
      call readdata_1cold(samfile, ngrid, samples)
    end if
  end subroutine set_samples

  ! calculate distance between multiple points to one point
  function sdist1(coord1, coord2) result(res)
    real              :: coord1(:), coord2(:)
    real, allocatable :: res

    res = sqrt(sum((coord1(:ndim) - coord2(:ndim)) ** 2))
  end function

  function sdistn1(coords, coord2) result(res)
    real              :: coords(:,:), coord2(:)
    real, allocatable :: res(:)
    ! local
    integer                       :: n1
    n1 = size(coords, dim=2)

    res = [(sdist1(coords(:, ii), coord2), ii = 1, n1)]
  end function

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
    real, allocatable :: centerloc(:)
    if (verbose) print*, 'Building distance kdtree ...'

    ! rotating/scaling
    centerloc = sum(grid(1:ndim, :), dim=2)/ngrid
    robs = rotate(ndim, nobs+ngrid, reshape([obs(1:ndim, :), grid(1:ndim, :)], [ndim, nobs+ngrid]), centerloc)
    if (nsim>0) then
      allocate(kdmask(nobs+ngrid))
      kdmask(:nobs) = .true.
      kdmask(nobs:) = .false.
      ! print*, 'Building distance kdtree 1'
      obstree => kdtree2_create(robs         , sort=.false., rearrange=.false.)
    else
      ! print*, 'Building distance kdtree 1a'
      ! print "(3F10.2)", centerloc
      ! print "(3F10.2)", robs(:,nobs-10:nobs)
      obstree => kdtree2_create(robs(:,:nobs), sort=.false., rearrange=.false.)
    end if
    ! print*, 'Building distance kdtree 1z'

    if (nobs2>0) then
      rob2 = rotate(ndim, nobs2, obs2(1:ndim, :), centerloc)
      obstree2 => kdtree2_create(rob2, sort=.false., rearrange=.false.)
    end if
    ! print*, 'Building distance kdtree 2'
  end subroutine setdist

  subroutine krige()
    real, allocatable       ::  AP(:), B(:)
    integer, allocatable    ::  IPIV(:)
    integer                 ::  INFO
    character*256           ::  sig
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
    if (INFO /= 0) then
      open(newunit=ifile, file='matA.dat', status='replace')
      do ii =1, nppdu
        write(ifile, "(*(ES12.4))") matA(:nppdu, ii)
      end do
      close(ifile)
      open(newunit=ifile, file='rhsB.dat', status='replace')
      do ii =1, nppdu
        write(ifile, "(*(ES12.4))") rhsB(ii)
      end do
      close(ifile)
      open(newunit=ifile, file='index.dat', status='replace')
      do ii =1, npp1
        write(ifile, "(I0)") inear(ii)
      end do
      close(ifile)
      if (npp2>0) then
        open(newunit=ifile, file='index2.dat', status='replace')
        do ii =1, npp2
          write(ifile, "(I0)") inear2(ii)
        end do
      end if
      close(ifile)
      write(sig, "(I0)") ig
      call perr(new_line("")//'Failed to find solution of the linear system at cell '//trim(sig))
    end if

    if (correct_weight) then
      weights(1:nppdu) = merge(B(1:nppdu), 0.0, B(1:nppdu)>0)
      weights(1:nppdu) = weights(1:nppdu) / sum(weights(1:nppdu))
    else
      weights(1:nppdu) = B(1:nppdu)
    end if
  end subroutine krige

  subroutine write_output()
    character :: cname(3)=['x', 'y', 'z']
    if (trim(outfile)=='~') then
      ifile = output_unit ! print to stdout
    else
      open(newunit=ifile, file=trim(outfile), status='replace')
    end if
    if (writexy) then
      write(ifile, '(99(A,:,","))') 'igrid', cname(1:ndim), 'estimate', 'variance'
      do ig=1, ngrid
        write(ifile, "(I0,*(:',',G0.12))") igrid(ig), grid(:ndim+2, ig)
      end do
    else
      write(ifile, fomt) grid(ndim+1, :)
    end if

    if (ifile /= output_unit) close(ifile)
    if (verbose) print*, 'Results have been written successfully'
  end subroutine write_output

  subroutine showhelp()
    integer    :: Mandatory = 2
    print "(A)", ''
    print "(A)", ''
    print "(A)", ' ppsgs -d ndim nobs ngrid nobs2 ndrift -o obsfile [-g gridfile] [-f facfile] [-o2 obsfile2] [options] [output]'
    print "(A)", ''
    print "(A)", '   Generate sequential gaussian simulation.'
    print "(A)", ' '
    print "(A)", '   Arguments:'
    do ii=1, Mandatory
      print "(A)", '      -'//opts(ii)%short//'  or  --'//opts(ii)%name(:10)//trim(opts(ii)%description)
    end do
    print "(A)", ' '
    print "(A)", '   Optional arguments:'
    do ii=Mandatory+1, size(opts)
      print "(A)", '      -'//opts(ii)%short//'  or  --'//opts(ii)%name(:10)//trim(opts(ii)%description)
    end do
    print "(A)", '      output               output file name. If output=="~" or omitted, output will print to screen.'
    print "(A)", ' '
    stop
    end subroutine showhelp
  end program
