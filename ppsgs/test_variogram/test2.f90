program test
use variogram
type(variog)           :: v1
integer                :: i, j, n, m
real, allocatable      :: res(:)

v1%vtype = 'sph'
v1%range = 100.0
v1%sill = 1.0
v1%nugget = 0.0

n = 100000
m = 10000

do j=1, m
    res = covfuc(v1, [(i/real(n)*1000.0, i=1, n)])
end do
print "(10F10.4)", res(:200)
end program