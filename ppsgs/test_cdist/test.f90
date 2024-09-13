program test
use rotation

real, allocatable :: a(:,:),b(:,:),c(:,:)
real, allocatable :: results(:,:)

integer                       :: ndim=3, ii, jj

anis2 = 0.1
call setrot()

a = reshape([0,0,0], [ndim,1])
b = reshape([0,0,1,1,0,0], [ndim,2])
c = reshape([3,4,5,6,7,8,1,1,1], [ndim,3])

a = rotate(ndim, 1, a)
b = rotate(ndim, 2, b)
c = rotate(ndim, 3, c)

print "(10F10.2)", sdistn1(b, a(:,1))
print "(10F10.2)", sdistn1(c, a(:,1))
print "(10F10.2)", (sum(([3,4,50])**2))**0.5,(sum(([6,7,80])**2))**0.5,(sum(([1,1,10])**2))**0.5

contains

  ! calculate distance between multiple points to one point
  function sdist1(coord1, coord2) result(res)
    real              :: coord1(:), coord2(:)
    real              :: res
    res = sqrt(sum((coord1(1:ndim) - coord2(1:ndim)) ** 2))
  end function

  function sdistn1(coords, coord2) result(res)
    real              :: coords(:,:), coord2(:)
    real, allocatable :: res(:)
    ! local
    integer                       :: n1
    n1 = size(coords, dim=2)
    res = [(sdist1(coords(:, ii), coord2), ii = 1, n1)]
  end function

end program