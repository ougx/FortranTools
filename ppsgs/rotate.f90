module rotation
real      :: rotmat(3,3)
real      :: ang1=0e0,ang2=0e0,ang3=0e0,anis1=1e0,anis2=1e0
contains
subroutine setrot()

!
!              Sets up an Anisotropic Rotation Matrix
!              **************************************
!
! Sets up the matrix to transform cartesian coordinates to coordinates
! accounting for angles and anisotropy (see manual for a detailed
! definition):
!
!
! INPUT PARAMETERS: (moved to module definition)
!
!   ang1             Azimuth angle for principal direction
!   ang2             Dip angle for principal direction
!   ang3             Third rotation angle
!   anis1            First anisotropy ratio
!   anis2            Second anisotropy ratio
!   rotmat           The rotation matrices
!
!
!
! Author: C. Deutsch                                Date: September 1989
!-----------------------------------------------------------------------
real, parameter         :: DEG2RAD = 3.141592653589793e0/180.0e0, EPSLON = 1.e-10
real                    :: alpha, beta, theta, &
                                sina, sinb, sint, cosa, cosb, cost, &
                                afac1, afac2
!
! Converts the input angles to three angles which make more
!  mathematical sense:
!
!         alpha   angle between the major axis of anisotropy and the
!                 E-W axis. Note: Counter clockwise is positive.
!         beta    angle between major axis and the horizontal plane.
!                 (The dip of the ellipsoid measured positive down)
!         theta   Angle of rotation of minor axis about the major axis
!                 of the ellipsoid.
!
if(ang1.ge.0.0.and.ang1.lt.270.0) then
    alpha = (90.0   - ang1) * DEG2RAD
else
    alpha = (450.0  - ang1) * DEG2RAD
endif
beta  = -1.0 * ang2 * DEG2RAD
theta =          ang3 * DEG2RAD
!
! Get the required sines and cosines:
!
sina  = sin(alpha)
sinb  = sin(beta)
sint  = sin(theta)
cosa  = cos(alpha)
cosb  = cos(beta)
cost  = cos(theta)
!
! Construct the rotation matrix in the required memory:
!
afac1 = 1.0 / max(anis1,EPSLON)
afac2 = 1.0 / max(anis2,EPSLON)
rotmat(1,1) =       (cosb * cosa)
rotmat(2,1) =       (cosb * sina)
rotmat(3,1) =       (-sinb)
rotmat(1,2) = afac1*(-cost*sina + sint*sinb*cosa)
rotmat(2,2) = afac1*(cost*cosa + sint*sinb*sina)
rotmat(3,2) = afac1*( sint * cosb)
rotmat(1,3) = afac2*(sint*sina + cost*sinb*cosa)
rotmat(2,3) = afac2*(-sint*cosa + cost*sinb*sina)
rotmat(3,3) = afac2*(cost * cosb)

end subroutine setrot

function rotated_dist(ndim, coord1, coord2)

!
!    Squared Anisotropic Distance Calculation Given Matrix Indicator
!    ***************************************************************
!
! This routine calculates the anisotropic distance between two points
!  given the coordinates of each point and a definition of the
!  anisotropy.
!
! INPUT VARIABLES:
!   coord1           Coordinates of first point
!   coord2           Coordinates of second point
!   rotmat           The rotation matrices
!
!
! OUTPUT VARIABLES:
!   dist           The distance accounting for the anisotropy
!                      and the rotation of coordinates (if any).
!
!
! Author: C. Deutsch                                Date: September 1989
integer                 :: ndim
real        :: coord1(ndim), coord2(ndim), rotated_dist
integer                 :: i
real        :: dd(ndim)


dd = coord1 - coord2

rotated_dist = 0.0
do i=1, ndim
    rotated_dist = rotated_dist + sum((rotmat(1:ndim,i) * dd))**2
end do
rotated_dist = rotated_dist ** 0.5

end function rotated_dist
end module