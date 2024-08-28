module variogram
  type variog
    character(3) :: vtype
    real         :: range
    real         :: sill
    real         :: nugget
  end type

  contains

  function covfuc(va, dist)
    real, allocatable :: covfuc(:)
    real              :: dist(:)
    type(variog)                  :: va
    real, parameter   :: pi = 3.141592653589793e0
    integer                       :: i, n
    real, allocatable :: hr(:)

    n = size(dist)
    allocate(covfuc(n))
    covfuc = 0.0
    hr = dist / va%range

    select case (va%vtype)
    case('sph'); do i = 1, n; if (hr(i)==0.) then; covfuc(i) = 1.0; elseif (dist(i)<va%range) then; covfuc(i) = 1.0 - 1.5 * hr(i) + 0.5 * hr(i) ** 3; end if; end do
    case('exp'); do i = 1, n; if (hr(i)==0.) then; covfuc(i) = 1.0; else; covfuc(i) = exp(-3.0 * hr(i)); end if; end do
    case('hol'); do i = 1, n; if (hr(i)==0.) then; covfuc(i) = 1.0; else; covfuc(i) = cos(pi * hr(i)); end if; end do
    case('gau'); do i = 1, n; if (hr(i)==0.) then; covfuc(i) = 1.0; else; covfuc(i) = exp(-49.0 / 16.0 * hr(i) ** 2.0); end if; end do
    case('pow'); do i = 1, n; if (hr(i)==0.) then; covfuc(i) = 1.0; else; covfuc(i) = 1 - hr(i) ** 2.0; end if; end do
    case('cir'); do i = 1, n; if (hr(i)==0.) then; covfuc(i) = 1.0; elseif (dist(i)<va%range) then; covfuc(i) = (2 * hr(i) * sqrt(1.0 - hr(i)**2) + 2 * asin(hr(i))) / pi; end if; end do
    case('lin'); do i = 1, n; if (hr(i)==0.) then; covfuc(i) = 1.0; elseif (dist(i)<va%range) then; covfuc(i) = 1 - hr(i); else; covfuc(i) = 0; end if; end do
    case default; print*, 'Unknown variogram model.'; stop
    end select

    covfuc = covfuc * va%sill
    if (va%nugget > 0) then
        where (dist==0) covfuc = covfuc + va%nugget
    end if
  end function


end module