rm fac.dat*

..\ppsgs.exe -d 3 1206 4800 66339 0 ^
	-o  "..\test_data\obs3d_clay.csv" ^
	-o2 "..\test_data\aem3d_clay.csv" ^
	-g  "..\test_data\gridxyz.csv" ^
	-f  fac.dat ^
	-v  sph 5000.0 0.12 0.0 ^
	-vc sph 5000.0 0.04 0.05 ^
	-v2 sph 5000.0 0.068 0.0  ^
	-u --anis2 0.1^
	--nmax 300 --nmax2 300 ^
	-ns 0 ^
	-vb

..\ppsgs.exe -d 3 1206 4800 66339 0  -o  "..\test_data\obs3d_clay.csv"  -o2 "..\test_data\aem3d_clay.csv" -f  fac.dat -ns 0 -bs 0 1 -vb grid.csv

