subroutine input1(matname,JDATE,JTIME,TSTEP, &
                  time,nhrs,pfile,nch,ns,nsamp)

use common_puf
use common_met
use multcomp_inc
use files_inc
use error_inc
!use host_inc
use M3UTILIO

implicit none

character*(*) pfile
character*80  rfile
character*4   matname
character*80  line
real time
integer nhrs
integer nch,nblank
integer nchr, ns, nsamp, ios
real    xs, ys, zs
      
INTEGER         STDATE    !  model starting date,    format YYYYDDD
INTEGER         STTIME    !  model starting time,    format HHMMSS
INTEGER         TSTEP     !  output time step, format HHMMSS
INTEGER         JDATE     !  current model date, coded YYYYDDD
INTEGER         JTIME     !  current model time, coded HHMMSS
INTEGER         JEDATE    !  ending date, coded YYYYDDD
INTEGER         JETIME    !  ending time, coded HHMMSS

REAL    TIMHRS            ! hours since start of integration 

! ... External Functions (not already declared by IODECL3.EXT):
!     INTEGER, EXTERNAL :: SECSDIFF

CHARACTER( 16 ) :: PROGNAME = 'input1'

!------ Puff  file

write(6,*)' '
write(6,*)'SCICHEM Program - Sampler values from Puff data'
write(6,*)' '

call get_file('Puff',pfile,nch)

write(6,100,advance = 'no')
100   format(' Simulation start date (YYYYDDD) : ')
read(5,*) STDATE

write(6,110,advance = 'no')
110   format(' Simulation start time (HHMMSS) : ')
read(5,*) STTIME

write(6,120,advance = 'no')
120   format(' Merge start date (YYYYDDD) : ')
read(5,*) JDATE

write(6,130,advance = 'no')
130   format(' Merge start time (HHMMSS) : ')
read(5,*) JTIME

write(6,140,advance = 'no')
140   format(' Merge end date (YYYYDDD) : ')
read(5,*) JEDATE

write(6,150,advance = 'no')
150   format(' Merge end time (HHMMSS) : ')
read(5,*) JETIME

write(6,160,advance = 'no')
160   format(' Output time step (HHMMSS) : ')
read(5,*) TSTEP

write(6,*) 'Simulation start',STDATE, STTIME
write(6,*) 'Merge start', JDATE, JTIME
write(6,*) 'Merge end', JEDATE, JETIME,TSTEP

TIMHRS = FLOAT ( SECSDIFF(STDATE, STTIME, JDATE, JTIME) ) / 3600.
time = timhrs

write(*,*)'time: ',time

NHRS = SECSDIFF(JDATE, JTIME, JEDATE, JETIME) / 3600 + 1

write(*,*)'no. of hours to extract: ',nhrs

matname = 'TRAC'

!------ Read Project File

nch = nblank(pfile)
lun_prj = 1
file_prjr = pfile(1:nch-3)//'prj'

!debug
write(*,*)'project file: ',file_prjr
write(*,*)'calling read_prj'
call flush(6)
!debug

call read_prj

!debug
write(*,*)'finished read_prj'
call flush(6)
!debug

call init_mcp !initialize multicomponents

call setgrid(JDATE,JTIME,TSTEP)

call initmap(JDATE,JTIME,TSTEP)

lamb3d = .true.
call init_amb_3d

lun_dgn = 56
file_dgn = 'tmp.dgn'
lun_dmp = 57
file_dmp = 'tmp.dmp'
call init_diagnostics

!------ Get sampler locations

call get_file('sampler',rfile,nchr)

if (rfile == ' ') then
  write(*,*)'Abort: sampler file not specified'
  stop
else
  nsamp = 10
  open(unit=nsamp,file=rfile,iostat=ios)
  if (ios /= 0) then
    write(6,*)'Abort: Error opening Sampler file'
    stop
  else
    read(nsamp,*,iostat=ios)
    if (ios > 0) then
      write(6,*)'Abort: Error reading Sampler file header'
      stop
    end if
    ns = 0   
    do while (.true.)
      read(nsamp,*,iostat=ios) xs,ys,zs
      if (ios < 0) exit
      if (ios > 0) then
        write(6,*)'Abort: Error reading Sampler locations'
        stop
      end if
      ns = ns + 1
      if (ns == 1) write(*,*)'read sampler ',ns,' located at (',xs,ys,zs,')'
    end do
    write(6,*)'Read',ns,' sampler locations from ',TRIM(rfile)
    write(6,*)'sampler ',ns,' located at (',xs,ys,zs,')'
    rewind(nsamp,iostat=ios)
    read(nsamp,*,iostat=ios)line
    if (ios /= 0) then
      write(6,*)'Abort: Error rereading Sampler file header'
      stop
    end if
  end if

end if

return

end
subroutine input2(xs,ns,nsamp)

use common_puf
use files_inc

implicit none

real,dimension(3,ns) :: xs

integer ns,nsamp
integer ios

character*256 line

real    xsmp, ysmp, hx, hy ,hz
integer i,j

do j = 1,ns
  read(nsamp,*,iostat=ios) (xs(i,j),i=1,3)
  if (ios /= 0) then
    write(6,*) 'Error reading sampler locations for sampler ',j
    stop
  end if
  xsmp = xs(1,j)
  ysmp = xs(2,j)

! Add terrain height to sampler height
  if (lter) then
    call get_topog(xsmp,ysmp,hz,hx,hy)
  else
    hz = 0.
  end if
  xs(3,j) = xs(3,j) + hz
end do

if(nsamp /= 5)close(unit=nsamp)

return

end
