program test
use rotation

! call test_rotate0
call test_rotate1
! call test_0
! call test_1
! call test_xy_ani
! call test3d_z_anisotropy

contains


subroutine test_rotate0()
print*,''
ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 1.0
anis2 = 1.0
call setrot()

print*, "No scaling"
print "(A,3F10.2)", 'test_rotate0_1: correct answer = 1,0,0; test result =', rotate(3, 1, [1.0, 0.0, 0.0])
print "(A,3F10.2)", 'test_rotate0_2: correct answer = 0,1,0; test result =', rotate(3, 1, [0.0, 1.0, 0.0])
print "(A,3F10.2)", 'test_rotate0_3: correct answer = 0,0,1; test result =', rotate(3, 1, [0.0, 0.0, 1.0])

print*, "Scaling"
anis1 = 0.9999
anis2 = 0.9999
call setrot()
call print_rotmat()
print "(A,3F10.2)", 'test_rotate0_1: correct answer = 1,0,0; test result =', rotate(3, 1, [1.0, 0.0, 0.0])
print "(A,3F10.2)", 'test_rotate0_2: correct answer = 0,1,0; test result =', rotate(3, 1, [0.0, 1.0, 0.0])
print "(A,3F10.2)", 'test_rotate0_3: correct answer = 0,0,1; test result =', rotate(3, 1, [0.0, 0.0, 1.0])
!
!lock
!eal a
!=0.5**0.5
!rint*, 'test_rotate0_4: correct answer = 1; test result =',rotated_dist(3, [1.0, 0.0, 0.0], [1.0, a  , a  ])
!rint*, 'test_rotate0_5: correct answer = 1; test result =',rotated_dist(3, [0.0, 1.0, 0.0], [a  , 1.0, a  ])
!rint*, 'test_rotate0_6: correct answer = 1; test result =',rotated_dist(3, [0.0, 0.0, 1.0], [a  , a  , 1.0])
!nd block
end subroutine


subroutine test_0()
print*,''
ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 1.0
anis2 = 1.0
call setrot()

print*, 'test_0_1: correct answer = 1; test result =',rotated_dist(3, [1.0, 0.0, 0.0], [0.0, 0.0, 0.0])
print*, 'test_0_2: correct answer = 1; test result =',rotated_dist(3, [0.0, 1.0, 0.0], [0.0, 0.0, 0.0])
print*, 'test_0_3: correct answer = 1; test result =',rotated_dist(3, [0.0, 0.0, 1.0], [0.0, 0.0, 0.0])

block
real a
a=0.5**0.5
print*, 'test_0_4: correct answer = 1; test result =',rotated_dist(3, [1.0, 0.0, 0.0], [1.0, a  , a  ])
print*, 'test_0_5: correct answer = 1; test result =',rotated_dist(3, [0.0, 1.0, 0.0], [a  , 1.0, a  ])
print*, 'test_0_6: correct answer = 1; test result =',rotated_dist(3, [0.0, 0.0, 1.0], [a  , a  , 1.0])
end block
end subroutine

subroutine test_1()
print*,''
ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 1.0
anis2 = 1.0
call setrot()
print*, 'test_1_1: correct answer = 5; test result =',rotated_dist(2, [3.0, 0.0], [0.0, 4.0])
print*, 'test_1_2: correct answer = 5; test result =',rotated_dist(3, [3.0, 0.0, 0.0], [0.0, 4.0, 0.0])
print*, 'test_1_3: correct answer = 5; test result =',rotated_dist(3, [3.0, 0.0, 7.0**0.5], [0.0, 3.0, 0.0])

print*, 'test_1_4: correct answer = sqrt(3); test result =',rotated_dist(3, [1.0, 1.0, 1.0], [0.0, 0.0, 0.0])
print*, 'test_1_5: correct answer = sqrt(2); test result =',rotated_dist(2, [1.0, 1.0], [0.0, 0.0])

ang1 = 45.0
call setrot()
print*, 'test_1_6: correct answer = 2; test result =',rotated_dist(2, [1.0, 0.0], [1.0, 2.0])
print*, 'test_1_7: correct answer = 4; test result =',rotated_dist(2, [1.0, 0.0], [1.0, 4.0])
print*, 'test_1_8: correct answer = 6; test result =',rotated_dist(2, [1.0, 0.0], [1.0, 6.0])
print*, 'test_1_9: correct answer = 8; test result =',rotated_dist(2, [1.0, 0.0], [1.0, 8.0])
end subroutine


subroutine test_xy_ani()
real, allocatable :: rxyz0(:,:), rxyz1(:,:)

print*, "========== 0 degree =========="
ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 0.5
anis2 = 1.0
call setrot()
call print_rotmat
print*, 'test_xy_ani_1: correct answer = 4; test result =',rotated_dist(2, [2.0, 1.0], [0.0, 1.0])
print*, 'test_xy_ani_2: correct answer = 8; test result =',rotated_dist(2, [4.0, 1.0], [0.0, 1.0])
print*, 'test_xy_ani_3: correct answer =12; test result =',rotated_dist(2, [6.0, 1.0], [0.0, 1.0])
print*, 'test_xy_ani_4: correct answer =16; test result =',rotated_dist(2, [8.0, 1.0], [0.0, 1.0])

print*, "========== 45 degree =========="
ang1 = 45.0
call setrot()
call print_rotmat
rxyz0 = rotate(3, 1, [[1.0, 0.0, 0.0]])
rxyz1 = rotate(3, 4, [[1.0, 2.0, 0.0],[1.0, 4.0, 0.0],[1.0, 6.0, 0.0],[1.0, 8.0, 0.0]])
print*, "0_45",rxyz0
print*, "0_45",rxyz1
print*, "1_45",sum((rxyz1(:, 1) - rxyz0(:, 1))**2)**0.5
print*, "2_45",sum((rxyz1(:, 2) - rxyz0(:, 1))**2)**0.5
print*, "3_45",sum((rxyz1(:, 3) - rxyz0(:, 1))**2)**0.5
print*, "4_45",sum((rxyz1(:, 4) - rxyz0(:, 1))**2)**0.5

print*, "========== 90 degree =========="
ang1 = 90
call setrot()
call print_rotmat
rxyz0 = rotate(3, 1, [[1.0, 0.0, 0.0]])
rxyz1 = rotate(3, 4, [[1.0, 2.0, 0.0],[1.0, 4.0, 0.0],[1.0, 6.0, 0.0],[1.0, 8.0, 0.0]])
print*, "0_90",rxyz0(:2,:)
print*, "0_90",rxyz1(:2,:)
print*, "1_90",sum((rxyz1(:, 1) - rxyz0(:, 1))**2)**0.5,rotated_dist(2, [1.0, 2.0, 0.0], [1.0, 0.0, 0.0])
print*, "2_90",sum((rxyz1(:, 2) - rxyz0(:, 1))**2)**0.5,rotated_dist(2, [1.0, 4.0, 0.0], [1.0, 0.0, 0.0])
print*, "3_90",sum((rxyz1(:, 3) - rxyz0(:, 1))**2)**0.5,rotated_dist(2, [1.0, 6.0, 0.0], [1.0, 0.0, 0.0])
print*, "4_90",sum((rxyz1(:, 4) - rxyz0(:, 1))**2)**0.5,rotated_dist(2, [1.0, 8.0, 0.0], [1.0, 0.0, 0.0])
end subroutine


subroutine test_rotate1()
real :: rxyz0(3), rxyz1(3), a1
integer :: i, n
print*,'test_rotate1 alpha1'
ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 0.5
anis2 = 1.0

n = 20
do i =0, n
    a1 = i * 360.0 / n
    ang1 = a1
    call setrot()
    print "(I3,10F8.3)", i, a1, rotate(3,1,[1.0, 0.0, 0.0]), rotated_dist(3, [0.0, 0.0, 0.0], [1.0, 0.0, 0.0]), sqdist(0.0,0.0,0.0,1.0,0.0,0.0)**0.5
end do

print*, "test_rotate1 alpha2"
anis1 = 1.0
anis2 = 0.5
n = 20
do i =0, n
    a1 = i * 360.0 / n
    ang1 = 0.0
    ang2 = a1
    call setrot()
    print "(I3,10F8.3)", i, a1, rotate(3,1,[1.0, 0.0, 0.0]), rotated_dist(3, [0.0, 0.0, 0.0], [1.0, 0.0, 0.0]), sqdist(0.0,0.0,0.0,1.0,0.0,0.0)**0.5
end do
end subroutine test_rotate1

subroutine test3d_z_anisotropy()
print*,''
ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 1.0
anis2 = 1.0
call setrot()
! call print_rotmat()
print*, 'test3d_z_anisotropy_1: correct answer = 1 ; test result =', rotated_dist(3, [0.0, 0.0, 1.0], [0.0, 0.0, 0.0])

anis1 = 1.0
anis2 = 0.1
call setrot()
! call print_rotmat()
print*, 'test3d_z_anisotropy_2: correct answer = 10; test result =', rotated_dist(3, [0.0, 0.0,         1.0], [0.0, 0.0, 0.0])
print*, 'test3d_z_anisotropy_3: correct answer = 20; test result =', rotated_dist(3, [0.0, sqrt(300.0), 1.0], [0.0, 0.0, 0.0])

anis1 = 0.1
anis2 = 1.0
call setrot()
! call print_rotmat()
print*, rotate(2, 1, [0.0, 1.0])
print*, 'test3d_z_anisotropy_4: correct answer = 10; test result =', rotated_dist(3, [1.0, 0.0, 0.0        ], [0.0, 0.0, 0.0])
print*, 'test3d_z_anisotropy_5: correct answer = 20; test result =', rotated_dist(3, [1.0, 0.0, sqrt(300.0)], [0.0, 0.0, 0.0])


anis1 = 0.1
anis2 = 0.1
call setrot()
! call print_rotmat()
print*, "ratate (sqrt(3),0,1) into ", rotate(3, 1, [sqrt(3.0), 0.0, 1.0])
print*, 'test3d_z_anisotropy_6: correct answer = 20; test result =', rotated_dist(3, [sqrt(3.0), 0.0, 1.0], [0.0, 0.0, 0.0])

end subroutine
end program