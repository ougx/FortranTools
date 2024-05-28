program test
use rotation

ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 1.0
anis2 = 1.0

call setrot()

print '(3F5.1)', rotmat

call test_0
!call test_1
call test3d_z_anisotropy

contains

subroutine test_1()
print*, rotated_dist(2, [3.0, 0.0], [0.0, 4.0])
print*, rotated_dist(3, [3.0, 0.0, 0.0], [0.0, 4.0, 0.0])

print*, rotated_dist(3, [3.0, 0.0, 7.0**0.5], [0.0, 3.0, 0.0])

print*, rotated_dist(3, [1.0, 1.0, 1.0], [0.0, 0.0, 0.0])
print*, rotated_dist(2, [1.0, 1.0], [0.0, 0.0])

ang1 = 45.0
anis1 = 0.5
call setrot()
print '(3F5.1)', rotmat
print*, rotated_dist(2, [1.0, 10.0], [1.0, 2.0])
print*, rotated_dist(2, [1.0, 10.0], [3.0, 4.0])
print*, rotated_dist(2, [1.0, 10.0], [5.0, 6.0])
print*, rotated_dist(2, [1.0, 10.0], [7.0, 8.0])
end subroutine


subroutine test_0()

print*, 'test_0: correct answer = 1; test result =',rotated_dist(3, [1.0, 0.0, 0.0], [0.0, 0.0, 0.0])
print*, 'test_0: correct answer = 1; test result =',rotated_dist(3, [0.0, 1.0, 0.0], [0.0, 0.0, 0.0])
print*, 'test_0: correct answer = 1; test result =',rotated_dist(3, [0.0, 0.0, 1.0], [0.0, 0.0, 0.0])

block
real a
a=0.5**0.5
print*, 'test_0: correct answer = 1; test result =',rotated_dist(3, [1.0, 0.0, 0.0], [1.0, a  , a  ])
print*, 'test_0: correct answer = 1; test result =',rotated_dist(3, [0.0, 1.0, 0.0], [a  , 1.0, a  ])
print*, 'test_0: correct answer = 1; test result =',rotated_dist(3, [0.0, 0.0, 1.0], [a  , a  , 1.0])
end block
end subroutine


subroutine test3d_z_anisotropy()

ang1 = 0.0
ang2 = 0.0
ang3 = 0.0
anis1 = 1.0
anis2 = 1.0
call setrot()
print*, 'test3d_z_anisotropy: correct answer = 1; test result =', rotated_dist(3, [0.0, 0.0, 1.0], [0.0, 0.0, 0.0])


anis2 = 0.1
call setrot()
print*, 'test3d_z_anisotropy: correct answer = 10; test result =', rotated_dist(3, [0.0, 0.0, 1.0], [0.0, 0.0, 0.0])

end subroutine
end program