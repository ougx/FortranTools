module common
  logical           :: verbose=.false.
  real, parameter   :: pi = 4.0*atan(1.0)
  real, parameter   :: verysmall = tiny(1.0e0) * 1000
  real, parameter   :: verylarge = huge(1.0e0) * 1e-3
  real, parameter   :: zero = 0.0e0
  real, parameter   :: one  = 1.0e0
  real, parameter   :: DEG2RAD = pi/180.0, EPSLON = 1.e-10
end module common