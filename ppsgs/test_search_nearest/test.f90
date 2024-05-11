program test
double precision      :: a(10)
integer, allocatable  :: ismall(:)
a = [1.1, 1.3, 2.3, 0.1, 5.4, 0.9, 4.5, 3.4, 2.5, 1.1]

ismall = search_nearest(a, 10, 4)
print*, a(ismall)


ismall = search_nearest(a, 10, 5)
print*, a(ismall)

contains
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
end program test