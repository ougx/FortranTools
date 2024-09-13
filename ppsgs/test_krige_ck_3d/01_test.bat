rm fac.dat*

..\ppsgs.exe -d 3 1206 4800 66339 0 ^
	-of "..\test_data\obs3d_clay.csv" ^
	-o2 "..\test_data\aem3d_clay.csv" ^
	-gf "..\test_data\gridxyz.csv" ^
	-v1 sph 5000.0 0.12 0.0 ^
	-vc sph 5000.0 0.04 0.05 ^
	-v2 sph 5000.0 0.068 0.0  ^
	-u --anis2 0.5 -bd 0.0 1.0^
	--nmax 30 --nmax2 30 ^
	-v -xy grid.csv

rem 	-ff fac.dat ^
rem ..\ppsgs.exe -d 3 1206 4800 66339 0  -of  "..\test_data\obs3d_clay.csv"  -o2 "..\test_data\aem3d_clay.csv" -ff  fac.dat -bs 0 1 -v grid.csv

