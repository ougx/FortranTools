call ..\compiler_setting.bat

del f90getopt_sample.exe
del f90getopt_sample.obj
del f90getopt.mod 
del f90getopt.obj

%ff% f90getopt.F90 f90getopt_sample.f90 -o f90getopt_sample