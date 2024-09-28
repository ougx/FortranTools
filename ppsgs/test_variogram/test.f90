program test
use variogram
class(variog), pointer :: v1
integer                :: i, j, n, m
real, allocatable      :: res(:)

v1 => get_vgm('sph', 100.0, 1.0, 0.0)

n = 100000
m = 10000

do j=1, m
    res = v1%covfuc([(i/real(n)*1000.0, i=1, n)])
end do
print "(10F10.4)", res(:200)
end program