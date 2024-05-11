
subroutine progress(r)
  implicit none
  real           :: r
  integer        ::j,k
  character(len=17)::bar="???% |          |"
  j = int(r * 100)
  write(unit=bar(1:3),fmt="(i3)") j
  do k=1, int(j/10)
    bar(6+k:6+k)="*"
  enddo
  ! print the progress bar.
  write(unit=6,fmt="(a1,a1,a17)") '+',char(13), bar
  return
end subroutine progress