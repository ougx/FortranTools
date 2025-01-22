program test
use variogram
type(vgm_struct)       :: vario1
type(variog_exp)       :: varioa
type(variog_gau)       :: variob
integer                :: i, j, n, m, ifile
real                   :: dh = 10, maxh=50000.0

varioa = variog_exp("exp",1000,500,500)
variob = variog_gau("gau",30000,25,0)

call vario1%define(varioa%tostr())
call vario1%define(variob%tostr())

print*, "nstruct", vario1%nstruct
do i=1, vario1%nstruct
  print "(A)", vario1%vgms(i)%vgm%tostr()
end do
print "(A)", vario1%tostr()

open(newunit=ifile, file="test3.csv", status="replace")
write(ifile, "(A)") "h,exp,gau,sum"
m = int(maxh / dh + 1)
do j=0, m
  write(ifile, "(G0.8,',',G0.8,',',G0.8,',',G0.8)") &
    j * dh, varioa%covfuc(j * dh), variob%covfuc(j * dh), vario1%covfuc(j * dh)
  if (j ==0) write(ifile, "(G0.8,',',G0.8,',',G0.8,',',G0.8)") &
    1e-5, varioa%covfuc(1e-5), variob%covfuc(1e-5), vario1%covfuc(1e-5)
end do
end program