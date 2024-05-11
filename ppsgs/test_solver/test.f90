program test
integer, parameter :: nppdu = 6
real matA(nppdu,nppdu)
real rhsB(nppdu), weights(nppdu)
real cov10
integer ii, ifile

cov10 = 0.0
open(newunit=ifile, file='matA.txt', status='old'); read(ifile, *) matA; close(ifile)
open(newunit=ifile, file='rhsB.txt', status='old'); read(ifile, *) rhsB; close(ifile)
print *, 'matA'
print '(6(F4.2,x))', matA
print *, 'rhsB'
print '(6(F4.2,x))', rhsB

call krige1()

print *, 'Weights'
print '(*(F0.8,x))', weights
std = cov10 - sum(rhsB(1:nppdu) * weights(1:nppdu))
print *, 'Variance'
print*, std
contains

subroutine perr(msg)
  character(*) :: msg
  print '(A)', msg
  stop
end subroutine perr

subroutine krige()
real, allocatable       ::  AP(:), B(:)
integer, allocatable    ::  IPIV(:)
integer                 ::  INFO
allocate(AP(nppdu*(nppdu+1)/2), B(nppdu), IPIV(nppdu))
kk = 1
do ii=1, nppdu
  AP(kk:kk+ii-1) = matA(ii, 1:ii)
  kk = kk + ii
end do

print *, 'AP'
print '(6(F4.2,x))', AP

B = rhsB(1:nppdu)
print *, 'B'
print '(6(F4.2,x))', B

call SSPSV( 'U', nppdu, 1, AP, IPIV, B, nppdu, INFO )
if (INFO /= 0) call perr('Failed to find solution of the linear system.')
print*, 'info', INFO
!print*, B
weights(1:nppdu) = B(1:nppdu)
end subroutine krige

subroutine krige1()
  real, allocatable       ::  AP(:), B(:)
  integer, allocatable    ::  IPIV(:)
  integer                 ::  INFO
  allocate(AP(nppdu*(nppdu+1)/2), B(nppdu), IPIV(nppdu))

  kk = 1
  do ii=1, nppdu
    AP(kk:kk+ii-1) = matA(ii, 1:ii)
    kk = kk + ii
  end do

  B = rhsB(1:nppdu)
  call SSPSV( 'U', nppdu, 1, AP, IPIV, B, nppdu, INFO )
  if (INFO /= 0) call perr('Failed to find solution of the linear system.')
  weights(1:nppdu) = B(1:nppdu)
end subroutine krige1

end program