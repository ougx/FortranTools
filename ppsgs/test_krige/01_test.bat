

..\ppsgs.exe -d 2 5 1 0 0 -o obs.csv -g grid01.csv -f fac01.dat -u -v lin 1.0 10.0 0.0 -vb
..\ppsgs.exe -d 2 5 1 0 0 -o obs.csv -f fac01.dat -s sample.csv -u -v lin 1.0 10.0 0.0 -vb
echo Correct result: 0.7796277

..\ppsgs.exe -d 2 5 1 0 0 -o obs.csv -g grid02.csv -f fac02.dat -u -v lin 1.0 10.0 0.0 -vb
..\ppsgs.exe -d 2 5 1 0 0 -o obs.csv -f fac02.dat -s sample.csv -u -v lin 1.0 10.0 0.0 -vb
echo Correct result: 1.1827978

..\ppsgs.exe -d 2 5 1 0 0 -o obs.csv -g grid03.csv -f fac03.dat -u -v lin 1.0 10.0 0.0 -vb
..\ppsgs.exe -d 2 5 1 0 0 -o obs.csv -f fac03.dat -s sample.csv -u -v lin 1.0 10.0 0.0 -vb
echo Correct result: 1.5879789