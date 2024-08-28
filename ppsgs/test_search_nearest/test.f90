program test
use rotation
integer               :: ii, ndim, nobs, ik, nrand, state_size
integer, allocatable  :: iiobs(:)
INTEGER               :: clock0, clock1, clockrate
real                  :: r
integer, allocatable, dimension(:) :: state
call system_clock(clock0, clockrate)
call setrot()

print*, huge(ii)

!call test_search_smallest
!stop


call random_seed( size=state_size )
write(*,*) '-- state size is: ', state_size
allocate(state(state_size))

state = 20180815
call random_seed( put=state )

nrand = 5000
allocate(iiobs(nrand))
do ii=1, nrand
  call random_number(r)
  iiobs(ii) = int(r*60000)
end do

call system_clock(clock0)
call test_search_exhaustive()
call system_clock(clock1)
print *, "test_search_exhaustive elapsed time: ", real(clock1 - clock0) / real(clockrate)

!call system_clock(clock0)
!call test_search_spiral()
!call system_clock(clock1)
!print *, "test_search_spiral elapsed time: ", real(clock1 - clock0) / real(clockrate)

call system_clock(clock0)
call test_search_kdtree()
call system_clock(clock1)
print *, "test_search_kdtree elapsed time: ", real(clock1 - clock0) / real(clockrate)

call system_clock(clock0)
call test_search_kdtree_mask()
call system_clock(clock1)
print *, "test_search_kdtree_mask elapsed time: ", real(clock1 - clock0) / real(clockrate)

call system_clock(clock0)
call test_search_kdtree_reang()
call system_clock(clock1)
print *, "test_search_kdtree_reang elapsed time: ", real(clock1 - clock0) / real(clockrate)
!======================================================================================
contains

subroutine test_search_smallest()

integer, allocatable  :: ismall(:)
real                  :: a(10)
real                  :: dist(66339)
a = [1.1, 1.3, 2.3, 0.1, 5.4, 0.9, 4.5, 3.4, 2.5, 1.1]
print '(10F5.2)', a

ismall = search_smallest(a, 10, 4)
print '(10I5)', ismall          !         6       0.9       2.3       1.1       1.3       2.3       0.1
print '(10F5.2)', a(ismall)     !        10       1.1       1.3       1.1       1.3       0.9       0.1


ismall = search_smallest(a, 10, 5)
print '(10I5)', ismall          !         6       0.9       5.4       1.1       1.3       2.3       0.1       5.4
print '(10F5.2)', a(ismall)     !        10       1.1       2.3       1.1       1.3       2.3       0.1       0.9

open(unit=11, file='dist.bin', form='unformatted', access='stream', status='old')
read(11) dist
close(11)


ismall = search_smallest(dist, 66339, 10)
print '(10I10)', search_smallest(dist, 66339, 10)
print '(10I10)', ismall           !       9078      9076     10171      9075      9079     10170     10169     10167     10168      9077
print '(10F10.2)', dist(ismall)   !     203.95    190.45    481.85    368.41    421.89    309.68    233.28    436.28    301.32      0.00

end subroutine

subroutine test_search_spiral()

integer, allocatable  :: ismall(:,:)
real, allocatable     :: obsloc(:,:), newdist(:,:)
integer               :: nmax, iobs
ndim = 3
nobs = 66339
allocate(obsloc(ndim, nobs))
open(unit=10, file='../test_data/aem3d_clay.csv', status='old')
read(10, *)
do ii=1, nobs
  read(10, *) ik, obsloc(:, ii)
end do
close(10)

nmax = 10
allocate(ismall(nmax, nrand))
allocate(newdist(nmax, nrand))
do iobs = 1, size(iiobs)
  call search_spiral(obsloc, obsloc(:,iiobs(iobs)), nobs, nmax, ismall(:,iobs), newdist(:,iobs))
end do
iobs = int(nrand/2)
print "(A,2I10,10I10)"  , 'spiral    ', iobs, iiobs(iobs), ismall(:, iobs)
print "(A,2I10,10F10.1)", 'spiral    ', iobs, iiobs(iobs), newdist(:, iobs)
end subroutine

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

! search for nearest data points based on their rotated distance
subroutine search_spiral(obsloc, targetloc, n, k, inearest, distnearest)
  integer                     :: n, k
  real                        :: obsloc(ndim,n), targetloc(ndim), distnearest(k)
  integer                     :: inearest(k)
  ! local
  real                        :: rngdist(ndim), dloc(ndim), zeroloc(ndim), dx(ndim), maxdx(ndim)
  integer                     :: idim, nk
  logical                     :: inside(ndim, n)
  real, allocatable           :: obsdist(:)
  integer, allocatable        :: icontain(:)

  ! Step 1. calculate block size along each dimension
  ! print*, 'Step 1'
  maxdx = [((maxval(obsloc(idim,:)) - minval(obsloc(idim,:))), idim=1, ndim)]

  zeroloc = 0.0
  rngdist = 1.0
  dx = 1.0
  do idim=1, ndim
    dloc=0.0
    dloc(idim)=1.0
    rngdist(idim) = rotated_dist(ndim, dloc, zeroloc)
    if (rngdist(idim)>0) dx = dx * rngdist(idim)
  end do
  do idim=1, ndim
    if (rngdist(idim)>0) then
      dx(idim) = dx(idim) / rngdist(idim)
    end if
  end do
  ! print*, 'Step 1a ', 'dist vector=', rngdist
  dx = dx / max(anis1, anis2) * sum(maxdx**2)**0.5 * 0.25

  ! Step 2. define the block that contains at least 2k data points
  ! print*, 'Step 2 ', 'initial_bloack=', dx
  inside = .true.
  do
    do idim = 1, ndim
      if (rngdist(idim)>0) then
        inside(idim,:) = obsloc(idim,:)>=targetloc(idim)-dx(idim) .and. &
                         obsloc(idim,:)<=targetloc(idim)+dx(idim)
      end if
    end do
    nk = count(all(inside, dim=1))
    ! print*, 'Decreasing block', dx, nk
    if (nk<=3*k) then
      exit
    elseif (nk<=4*k) then
      dx = dx * 0.8
    else
      dx = sqrt(3.0*k/nk) * dx
    end if
  end do

  do while(nk<2.0*k)
    dx = dx * 1.1
    do idim = 1, ndim
      if (rngdist(idim)>0) then
        inside(idim,:) = obsloc(idim,:)>=targetloc(idim)-dx(idim) .and. &
                         obsloc(idim,:)<=targetloc(idim)+dx(idim)
      end if
    end do
    nk = count(all(inside, dim=1))
  end do

  ! Step 3 calculate the distance from the target location to data points within the block
  ! print*, 'Step 3', ' final block', dx, nk

  ! dx = rotated_dist(maxdx, zeroloc)
  allocate(obsdist(nk))
  icontain = pack([(ii, ii=1, n)], all(inside, dim=1))
  ! print*, 'Step 3a', icontain
  obsdist = [(sum((obsloc(:,icontain(ii))-targetloc)**2), ii = 1, nk)]
  ! print*, 'Step 3b', obsdist
  ! do ii = 1, nk
  !   obsdist(ii) = rotated_dist(ndim, obsloc(:,ii), targetloc)
  !     icontain(ii)=ii
  !     print*, count(icontain(:ii)>0), obsloc(:,ii), obsdist(ii)
  !   end if
  ! end do

  ! Step 4 assign index and distance array
  ! print*, 'Step 4', nk, 'points'

  inearest = search_smallest(obsdist, nk, k)
  ! print*, 'Step 4a', inearest
  ! print*, 'Step 4b', obsdist(inearest)
  distnearest = obsdist(inearest)**0.5
  inearest    = icontain(inearest)
end subroutine

subroutine test_search_exhaustive()
  integer                     :: nobs
  real, allocatable           :: obsloc(:,:)
  real, allocatable           :: distnearest(:,:)
  integer, allocatable        :: inearest(:,:)
  ! local
  real, allocatable           :: mdist(:)
  integer                     :: jj, iobs

  ndim = 3
  nobs = 66339
  allocate(obsloc(ndim, nobs), mdist(nobs))
  open(unit=10, file='../test_data/aem3d_clay.csv', status='old')
  read(10, *)
  do ii=1, nobs
    read(10, *) ik, obsloc(:, ii)
  end do
  close(10)

  nmax = 10
  allocate(inearest(nmax, nrand), distnearest(nmax, nrand))


  do iobs=1, nrand
    do jj=1, nobs
      mdist(jj) = sum((obsloc(:,iiobs(iobs))-obsloc(:,jj))**2)
    end do
    inearest(:, iobs) = search_smallest(mdist, nobs, nmax)
    ! print*, iobs, iiobs(iobs), inearest(:, iobs)
    distnearest(:, iobs) = mdist(inearest(:, iobs))**0.5
    !if (iobs==int(nrand/2)) then
    !  open(unit=11, file='dist.bin', form='unformatted', access='stream', status='replace')
    !  write(11) mdist**0.5
    !end if
  end do
  iobs = int(nrand/2)
  print "(A,2I10,10I10)"  , 'exhaustive', iobs, iiobs(iobs), inearest(:, iobs)
  print "(A,2I10,10F10.1)", 'exhaustive', iobs, iiobs(iobs), distnearest(:, iobs)


end subroutine

subroutine test_search_kdtree()
  use kdtree2_module
  integer                     :: nobs
  real, allocatable           :: obsloc(:,:)
  real, allocatable           :: distnearest(:,:)
  integer, allocatable        :: inearest(:,:)
  ! local
  real, allocatable           :: mdist(:)
  integer                     :: jj, iobs

  type(kdtree2), pointer :: tree
  type(kdtree2_result), allocatable   :: results(:)

  ndim = 3
  nobs = 66339
  allocate(obsloc(ndim, nobs), mdist(nobs))
  open(unit=10, file='../test_data/aem3d_clay.csv', status='old')
  read(10, *)
  do ii=1, nobs
    read(10, *) ik, obsloc(:, ii)
  end do
  close(10)


  tree => kdtree2_create(obsloc,sort=.false.,rearrange=.false.)  ! this is how you create a tree.
  nmax = 10
  allocate(inearest(nmax, nrand), distnearest(nmax, nrand), results(nmax))


  do iobs=1, nrand
    call kdtree2_n_nearest(tree,obsloc(:,iiobs(iobs)),nmax,results)
    inearest(:, iobs) = [(results(ii)%idx, ii=1, nmax)]
    ! print*, iobs, iiobs(iobs), inearest(:, iobs)
    distnearest(:, iobs) = [(results(ii)%dis**0.5, ii=1, nmax)]
    if (iobs==int(nrand/2)) print*, obsloc(:,7381), sum((obsloc(:,7381)-obsloc(:,iiobs(iobs)))**2)**0.5
  end do
  iobs = int(nrand/2)
  print "(A,2I10,10I10)"  , 'kdtree    ', iobs, iiobs(iobs), inearest(:, iobs)
  print "(A,2I10,10F10.1)", 'kdtree    ', iobs, iiobs(iobs), distnearest(:, iobs)
end subroutine


subroutine test_search_kdtree_mask()
  use kdtree2_module
  integer                     :: nobs
  real, allocatable           :: obsloc(:,:)
  real, allocatable           :: distnearest(:,:)
  integer, allocatable        :: inearest(:,:)
  ! local
  real, allocatable           :: mdist(:)
  logical, allocatable        :: mask(:)
  integer                     :: jj, iobs

  type(kdtree2), pointer :: tree
  type(kdtree2_result), allocatable   :: results(:)

  ndim = 3
  nobs = 66339
  allocate(obsloc(ndim, nobs), mdist(nobs))
  open(unit=10, file='../test_data/aem3d_clay.csv', status='old')
  read(10, *)
  do ii=1, nobs
    read(10, *) ik, obsloc(:, ii)
  end do
  close(10)


  tree => kdtree2_create(obsloc,sort=.false.,rearrange=.false.)  ! this is how you create a tree.
  nmax = 8
  allocate(inearest(nmax, nrand), distnearest(nmax, nrand), results(nmax), mask(nobs))

  mask = .true.
  mask(899) = .false.
  mask(900) = .false.
  do iobs=1, nrand
    call kdtree2_n_nearest(tree,obsloc(:,iiobs(iobs)),nmax,results,mask)
    inearest(:, iobs) = [(results(ii)%idx, ii=1, nmax)]
    ! print*, iobs, iiobs(iobs), inearest(:, iobs)
    distnearest(:, iobs) = [(results(ii)%dis**0.5, ii=1, nmax)]
  end do
  iobs = int(nrand/2)
  print "(A,2I10,10I10)"  , 'kdtreemask', iobs, iiobs(iobs), inearest(:, iobs)
  print "(A,2I10,10F10.1)", 'kdtreemask', iobs, iiobs(iobs), distnearest(:, iobs)
end subroutine

subroutine test_search_kdtree_reang()
  use kdtree2_module
  integer                     :: nobs
  real, allocatable           :: obsloc(:,:)
  real, allocatable           :: distnearest(:,:)
  integer, allocatable        :: inearest(:,:)
  ! local
  real, allocatable           :: mdist(:)
  logical, allocatable        :: mask(:)
  integer                     :: jj, iobs

  type(kdtree2), pointer :: tree
  type(kdtree2_result), allocatable   :: results(:)

  ndim = 3
  nobs = 66339
  allocate(obsloc(ndim, nobs), mdist(nobs))
  open(unit=10, file='../test_data/aem3d_clay.csv', status='old')
  read(10, *)
  do ii=1, nobs
    read(10, *) ik, obsloc(:, ii)
  end do
  close(10)

  ! tree => kdtree2_create(reshape(obsloc, [ndim, nobs]), sort=.false., rearrange=.true.)  ! this is how you create a tree.

  tree => kdtree2_create(obsloc, sort=.false., rearrange=.true.)  ! this is how you create a tree.
  nmax = 8
  allocate(inearest(nmax, nrand), distnearest(nmax, nrand), results(nmax), mask(nobs))

  mask = .true.
  mask(899) = .false.
  mask(900) = .false.
  do iobs=1, nrand
    call kdtree2_n_nearest(tree,obsloc(:,iiobs(iobs)),nmax,results,mask)
    inearest(:, iobs) = [(results(ii)%idx, ii=1, nmax)]
    ! print*, iobs, iiobs(iobs), inearest(:, iobs)
    distnearest(:, iobs) = [(results(ii)%dis**0.5, ii=1, nmax)]
  end do
  iobs = int(nrand/2)
  print "(A,2I10,10I10)"  , 'kdtreerean', iobs, iiobs(iobs), inearest(:, iobs)
  print "(A,2I10,10F10.1)", 'kdtreerean', iobs, iiobs(iobs), distnearest(:, iobs)
end subroutine

end program test