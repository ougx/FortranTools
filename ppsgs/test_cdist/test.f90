program test
use rotation
double precision, allocatable :: a(:,:),b(:,:),c(:,:)
double precision, allocatable :: results(:,:)
integer                       :: ndim=2, ii, jj
a = reshape([1,2], [2,1])
b = reshape([1,2,3,4], [2,2])
c = reshape([3,4,5,6,7,8], [2,3])
call setrot()

print*, a(1:ndim, 1), b(1:ndim, 1), rotated_dist(ndim, a(1:ndim, 1), b(1:ndim, 2))
print*, b(1:ndim, 1), c(1:ndim, 3), rotated_dist(ndim, b(1:ndim, 1), c(1:ndim, 3))
results = cdist(a, b)
print*, 'results1', results
results = cdist(b, c)
print*, 'results1', results
contains
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

end program