rm fac.dat*
../ppsgs.exe -d 2 62 4800 15320 0 \
	-o  '../test_data/pc2d.csv' \
	-g  '../test_data/grid2d.csv' \
	-o2 '../test_data/aem2d.csv' \
	-f fac.dat \
	-v  sph 5000.0 0.12 0.0 \
	-vc sph 5000.0 0.04 0.05 \
	-v2 sph 5000.0 0.068 0.0  \
	-u --nmax 300 --nmax2 300 -vb -ns 0

../ppsgs.exe -d 2 62 4800 15320 0 -o '../test_data/pc2d.csv' -o2 '../test_data/aem2d.csv' -f fac.dat -ns 0 -bs 0.0 1.0 -vb -xy grid.csv

