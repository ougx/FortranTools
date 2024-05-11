program test
use rotation

ang1 = 0.0
ang2 = 0.0 
ang3 = 0.0 
anis1 = 1.0
anis2 = 1.0

call setrot()

print '(3F5.1)', rotmat

print*, rotated_dist(2, [3.0d0, 0.0d0], [0.0d0, 4.0d0])
print*, rotated_dist(3, [3.0d0, 0.0d0, 0.0d0], [0.0d0, 4.0d0, 0.0d0])

print*, rotated_dist(3, [3.0d0, 0.0d0, 7.0d0**0.5], [0.0d0, 3.0d0, 0.0d0])

print*, rotated_dist(3, [1.0d0, 1.0d0, 1.0d0], [0.0d0, 0.0d0, 0.0d0])
print*, rotated_dist(2, [1.0d0, 1.0d0], [0.0d0, 0.0d0])


ang1 = 45.0
call setrot()
print '(3F5.1)', rotmat
print*, rotated_dist(2, [1.0d0, 1.0d0], [0.0d0, 0.0d0])
print*, rotated_dist(2, [0.5d0**0.5d0, 0.5d0**0.5d0], [0.0d0, 0.0d0])


ang1 = 45.0
anis1 = 0.5
call setrot()
print '(3F5.1)', rotmat
print*, rotated_dist(2, [1.0d0, 10.0d0], [1.0d0, 2.0d0])
print*, rotated_dist(2, [1.0d0, 10.0d0], [3.0d0, 4.0d0])
print*, rotated_dist(2, [1.0d0, 10.0d0], [5.0d0, 6.0d0])
print*, rotated_dist(2, [1.0d0, 10.0d0], [7.0d0, 8.0d0])

end program