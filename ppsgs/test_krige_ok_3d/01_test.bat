
..\ppsgs.exe -d 3 1206 4800 0 0 ^
	-of "..\test_data\obs3d_clay.csv" ^
	-gf "..\test_data\gridxyz.csv" ^
	-v1 sph 5000.0 0.12 0.0 ^
	-u -bd 0.0 1.0^
	--nmax 30 ^
	-v -xy grid.csv

..\ppsgs.exe -d 3 1206 4800 0 0 ^
	-of "..\test_data\obs3d_clay.csv" ^
	-gf "..\test_data\gridxyz.csv" ^
	-v1 sph 5000.0 0.12 0.0 ^
	-u --anis2 0.5 -bd 0.0 1.0^
	--nmax 30 ^
	-v -xy grid_ani.csv

pause