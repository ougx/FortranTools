module variogram
  type variog
    character(3) :: vtype
    double precision         :: range
    double precision         :: sill
    double precision         :: nugget
  end type

  contains

  function covfuc(va, dist)
    double precision, allocatable :: covfuc(:)
    double precision              :: dist(:)
    type(variog)                  :: va
    double precision, parameter   :: pi = 3.141592653589793d0
    integer                       :: i, n
    double precision, allocatable :: hr(:)

    n = size(dist)
    allocate(covfuc(n))
    covfuc = 0.0
    hr = dist / va%range

    select case (va%vtype)
    case('sph'); do i = 1, n; if (dist(i)<va%range) covfuc(i) = 1.0 - 1.5 * hr(i) + 0.5 * hr(i) ** 3; end do
    case('exp'); do i = 1, n; covfuc(i) = exp(-3.0 * hr(i)); end do
    case('hol'); do i = 1, n; covfuc(i) = cos(pi * hr(i)); end do
    case('gau'); do i = 1, n; covfuc(i) = exp(-49.0 / 16.0 * hr(i) ** 2.0); end do
    case('pow'); do i = 1, n; covfuc(i) = dist(i) ** va%range; end do
    case('cir'); do i = 1, n; if (dist(i)<va%range) covfuc(i) = (2 * hr(i) * sqrt(1.0 - hr(i)**2) + 2 * asin(hr(i))) / pi; end do
    case('lin'); do i = 1, n; covfuc(i) = va%sill - hr(i); end do
    case default; print*, 'Unknown variogram model.'; stop
    end select

    if (va%vtype /= 'lin') covfuc = va%sill * covfuc
    if (va%nugget > 0) then
        where (dist==0) covfuc = covfuc + va%nugget
    end if
  end function


end module