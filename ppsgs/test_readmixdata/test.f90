program aa
integer :: i, j, n
integer :: a(10)

a = 1
open(unit=101, file='data.txt', status='old')

read(101, *) i, (a(j),j=i,i+i-1)
read(101, *) n, (a(j),j=n,n+n-1)

print*, a

print*, 'a', a(9:7)
print*, 'as', sum(a(9:7)*a(3:1))
end