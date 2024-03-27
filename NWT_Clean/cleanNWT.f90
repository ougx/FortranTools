program cleanNWT

double precision   :: line11(7)
character(len=10)  :: spec
character(len=1024):: stmp
double precision   :: line12(8)
double precision   :: line2(10)

call get_command_argument(1, stmp)
open(unit=101, file=stmp, status='old')
call skipcomment(101)
read(101, *) line11, spec, line12
if (int(line11(5)) == 2) then
  read(101, *) line2
else
  read(101, *) line2(1:5)
end if
close(101)

call get_command_argument(2, stmp)
open(unit=102, file=stmp, status='replace')
write(102, "(A)") '#  HEADTOL   FLUXTOL MAXITEROUT THICKFACT  LINMETH    IPRNWT    IBOTAV   OPTIONS  DBDTHETA  DBDKAPPA  DBDGAMMA   MOMFACT  BACKFLAG MAXBACKITER BACKTOL BACKREDUCE'
write(102, "(2es10.3,I10,es10.3,3I10,x,A9,4es10.3,2I10,2es10.3)") &
 line11(1:2), int(line11(3)),line11(4),int(line11(5:7)),spec,line12(1:4),int(line12(5:6)),line12(7:8)
if (int(line11(5)) == 2) then
  write(102, "(5I10,es10.3,I10,2es10.3,I10)") int(line2(1:5)),line2(6),int(line2(7)),line2(8),line2(9),int(line2(10))
  write(102, "(A)") '#     IACL    NORDER     LEVEL     NORTH   IREDSYS   RRCTOLS  IDROPTOL     EPSRN HCLOSEXMD MXITERXMD'
else
  write(102, "(3I10,2es10.3,I10,2es10.3,I10)") int(line2(1:3)),line2(4:5)
  write(102, "(A)") '#MAXITINNER ILUMETHOD  LEVFILL   STOPTOL      MSDR'
end if
end program