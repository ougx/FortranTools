program test
use rotation

call test_rotate0
call test_0
call test_1
call test3d_z_anisotropy

contains

subroutine test_rotate0()
print*,''
ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 1.0
anis2 = 1.0
call setrot()

print "(A,3F10.2)", 'test_rotate0_1: correct answer = 0,1,0; test result =', rotate(3, 1, [1.0, 0.0, 0.0])
print "(A,3F10.2)", 'test_rotate0_2: correct answer =-1,0,0; test result =', rotate(3, 1, [0.0, 1.0, 0.0])
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

ang1 = 45.0
anis1 = 0.5
call setrot()
print*, 'test_1_6: correct answer = 4; test result =',rotated_dist(2, [1.0, 0.0], [1.0, 2.0])
print*, 'test_1_7: correct answer = 8; test result =',rotated_dist(2, [1.0, 0.0], [1.0, 4.0])
print*, 'test_1_8: correct answer =12; test result =',rotated_dist(2, [1.0, 0.0], [1.0, 6.0])
print*, 'test_1_9: correct answer =16; test result =',rotated_dist(2, [1.0, 0.0], [1.0, 8.0])
end subroutine


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
print*, 'test3d_z_anisotropy_4: correct answer = 10; test result =', rotated_dist(3, [0.0, 1.0, 0.0        ], [0.0, 0.0, 0.0])
print*, 'test3d_z_anisotropy_5: correct answer = 20; test result =', rotated_dist(3, [0.0, 1.0, sqrt(300.0)], [0.0, 0.0, 0.0])


anis1 = 0.1
anis2 = 0.1
call setrot()
! call print_rotmat()
print*, 'test3d_z_anisotropy_6: correct answer = 20; test result =', rotated_dist(3, [0.0, sqrt(3.0),   1.0], [0.0, 0.0, 0.0])

end subroutine
end program