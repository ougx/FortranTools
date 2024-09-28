module variogram

  real, parameter   :: pi = 3.141592653589793e0

  !abstract interface
  !  function corefunc_if(rdist) result(res)
  !    real, intent(in) :: rdist
  !    real             :: res
  !  end function corefunc_if
  !end interface

  type, abstract :: variog
    !character(3) :: vtype
    real         :: range
    real         :: sill
    real         :: nugget
  contains
    procedure           :: initialize
    procedure, NOPASS   :: corefunc
    procedure           :: covfuc
  end type

  type, extends(variog) ::  variog_sph
  contains
    procedure, NOPASS   :: corefunc => corefunc_sph
  end type

  type, extends(variog) ::  variog_exp
  contains
    procedure, NOPASS   :: corefunc => corefunc_exp
  end type

  type, extends(variog) ::  variog_hol
  contains
    procedure, NOPASS   :: corefunc => corefunc_hol
  end type

  type, extends(variog) ::  variog_gau
  contains
    procedure, NOPASS   :: corefunc => corefunc_gau
  end type

  type, extends(variog) ::  variog_pow
  contains
    procedure, NOPASS   :: corefunc => corefunc_pow
  end type

  type, extends(variog) ::  variog_cir
  contains
    procedure, NOPASS   :: corefunc => corefunc_cir
  end type

  type, extends(variog) ::  variog_lin
  contains
    procedure, NOPASS   :: corefunc => corefunc_lin
  end type

  contains

  function get_vgm(vtype, range, sill, nugget) result(res)
    character(3), intent(in) :: vtype
    real        , intent(in) :: sill, range, nugget
    class(variog), pointer   :: res

    select case (vtype)
    case('sph'); allocate(res, source=variog_sph(range, sill, nugget))
    case('exp'); allocate(res, source=variog_exp(range, sill, nugget))
    case('hol'); allocate(res, source=variog_hol(range, sill, nugget))
    case('gau'); allocate(res, source=variog_gau(range, sill, nugget))
    case('pow'); allocate(res, source=variog_pow(range, sill, nugget))
    case('cir'); allocate(res, source=variog_cir(range, sill, nugget))
    case('lin'); allocate(res, source=variog_lin(range, sill, nugget))
    case default; print*, 'Unknown variogram model.'; stop
    end select
  end function

  subroutine initialize(this, range, sill, nugget)
    class(variog)            :: this
    real        , intent(in) :: sill, range, nugget

    this%sill = sill
    this%range = range
    this%nugget = nugget

  end subroutine

  elemental function covfuc(this, dist) result(res)
    class(variog), intent(in)     :: this
    real, intent(in)              :: dist
    real                          :: res
    ! local
    real, parameter               :: verysmall = tiny(1.0e0) * 1e3
    if (dist > verysmall) then
      res = this%sill * this%corefunc(dist / this%range)
    else
      res = this%sill + this%nugget
    end if
  end function

  elemental function corefunc(rdist) result(res)
    real, intent(in) :: rdist
    real             :: res
    res = 0.0
  end function corefunc

  elemental function corefunc_sph(rdist) result(res)
    real, intent(in) :: rdist
    real             :: res
    if (rdist<1.0) then
      res = 1.0 - 1.5 * rdist + 0.5 * rdist ** 3
    else
      res = 0.0
    end if
  end function corefunc_sph

  elemental function corefunc_exp(rdist) result(res)
    real, intent(in) :: rdist
    real             :: res
    res = exp(-3.0 * rdist)
  end function corefunc_exp

  elemental function corefunc_hol(rdist) result(res)
    real, intent(in) :: rdist
    real             :: res
    res = cos(pi * rdist)
  end function corefunc_hol

  elemental function corefunc_gau(rdist) result(res)
    real, intent(in) :: rdist
    real             :: res
    res = exp(-49.0 / 16.0 * rdist ** 2.0)
  end function corefunc_gau

  elemental function corefunc_pow(rdist) result(res)
    real, intent(in) :: rdist
    real             :: res
    if (rdist<1.0) then
      res = 1 - rdist ** 2.0
    else
      res = 0.0
    end if
  end function corefunc_pow

  elemental function corefunc_cir(rdist) result(res)
    real, intent(in) :: rdist
    real             :: res
    if (rdist<1.0) then
      res = 1.0 - (2 * rdist * sqrt(1.0 - rdist**2) + 2 * asin(rdist)) / pi
    else
      res = 0.0
    end if
  end function corefunc_cir

  elemental function corefunc_lin(rdist) result(res)
    real, intent(in) :: rdist
    real             :: res
    if (rdist<1.0) then
      res = 1- rdist
    else
      res = 0.0
    end if
  end function corefunc_lin

end module