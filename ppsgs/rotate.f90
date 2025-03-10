module rotation
use common, only: DEG2RAD, EPSLON
real      :: rotmat(3,3)
real      :: ang1=0e0,ang2=0e0,ang3=0e0,anis1=1e0,anis2=1e0
logical   :: scaling = .false.
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
!   ang1             Azimuth angle for principal direction (clockwise)
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
scaling = abs(anis1-1.0e0)>EPSLON .or. abs(anis2-1.0e0)>EPSLON
if(.not. scaling) then
    ! print*, "no scaling is needed"
    rotmat(:,1) = [1.0, 0.0, 0.0]
    rotmat(:,2) = [0.0, 1.0, 0.0]
    rotmat(:,3) = [0.0, 0.0, 1.0]
    return
end if
! if(ang1.ge.0.0.and.ang1.lt.270.0) then
!     alpha = (90.0   - ang1) * DEG2RAD
! else
!     alpha = (450.0  - ang1) * DEG2RAD
! endif
alpha = -ang1 * DEG2RAD
beta  = -ang2 * DEG2RAD
theta =  ang3 * DEG2RAD
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
rotmat(1,1) = afac1*(cosb * cosa)
rotmat(1,2) = afac1*(cosb * sina)
rotmat(1,3) = afac1*(-sinb)
rotmat(2,1) =       (-cost*sina + sint*sinb*cosa)
rotmat(2,2) =       ( cost*cosa + sint*sinb*sina)
rotmat(2,3) =       ( sint * cosb)
rotmat(3,1) = afac2*( sint*sina + cost*sinb*cosa)
rotmat(3,2) = afac2*(-sint*cosa + cost*sinb*sina)
rotmat(3,3) = afac2*(cost * cosb)

end subroutine setrot

subroutine print_rotmat(iout)
integer, optional       :: iout
integer                 :: iiout
if (present(iout)) then
    iiout=iout
else
    iiout=6
end if
write(iiout, *) ''
write(iiout, *) 'Rotation matrix:'
write(iiout, '(A,5F10.3)') ' Parameters:', ang1,ang2,ang3,anis1,anis2
write(iiout, '(3F15.10)') rotmat
write(iiout, *) ''
end subroutine

function rotate(ndim, npnt, coord1, origin) result(coord2)

integer                 :: npnt, ndim
real                    :: coord1(ndim, npnt)
real                    :: coord2(ndim, npnt)
real, optional          :: origin(ndim)
! local
integer                 :: idim


if (present(origin)) then
  do idim=1, ndim
    coord2(idim,:) = coord1(idim,:) - origin(idim)
  end do
else
  coord2 = coord1
end if

! print "(2F10.2)", coord2
if (scaling) then
    coord2 = matmul(rotmat(1:ndim,1:ndim), coord2(1:ndim,:))
    !do idim=1, ndim
    !    coord2(idim,1:npnt) = [(sum(rotmat(idim, 1:ndim)*coords(1:ndim,ipnt)), ipnt=1, npnt)]
    !end do
end if
end function

function rotated_dist(ndim, coord1, coord2) result(res)

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
    real                    :: coord1(ndim), coord2(ndim), res

    res = sqrt(sum(rotate(ndim, 1, coord1, coord2) ** 2))
end function

real function sqdist(x1,y1,z1,x2,y2,z2)
!-----------------------------------------------------------------------
!
!    Squared Anisotropic Distance Calculation Given Matrix Indicator
!    ***************************************************************
!
! This routine calculates the anisotropic distance between two points
!  given the coordinates of each point and a definition of the
!  anisotropy.
!
!
! INPUT VARIABLES:
!
!   x1,y1,z1         Coordinates of first point
!   x2,y2,z2         Coordinates of second point
!   ind              The rotation matrix to use
!   MAXROT           The maximum number of rotation matrices dimensioned
!   rotmat           The rotation matrices
!
!
!
! OUTPUT VARIABLES:
!
!   sqdist           The squared distance accounting for the anisotropy
!                      and the rotation of coordinates (if any).
!
!
! NO EXTERNAL REFERENCES
!
!
!-----------------------------------------------------------------------
     real cont,dx,dy,dz
!
! Compute component distance vectors and the squared distance:
!
    dx = (x1 - x2)
    dy = (y1 - y2)
    dz = (z1 - z2)
    sqdist = 0.0
    do i=1,3
        cont   = rotmat(i,1) * dx &
                + rotmat(i,2) * dy &
                + rotmat(i,3) * dz
        sqdist = sqdist + cont * cont
    end do
    return
end

end module