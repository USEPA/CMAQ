      subroutine input1(matname,JDATE,JTIME,TSTEP,time,nhrs,dfile,wfile,
     &                  nch)

      use common_puf
      use common_met
      use multcomp_inc
      use files_inc
      use error_inc
!     use host_inc
      use M3UTILIO

      implicit none

      character*(*) dfile, wfile
      character*4   matname
      real time
      integer nhrs
      integer nch,nblank

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

C------ Deposition file

      write(6,*)' '
      write(6,*)'SCICHEM Program - Read deposition data'
      write(6,*)' '

      call get_file('Dry Deposition',dfile,nch)
      call get_file('Wet Deposition',wfile,nch)

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
      TIMHRS = FLOAT ( SECSDIFF(STDATE, STTIME, JDATE, JTIME) ) /
     &                  3600.

      time = timhrs

      write(*,*)'time: ',time
      NHRS = SECSDIFF(JDATE, JTIME, JEDATE, JETIME) / 3600 + 1

      write(*,*)'no. of hours to extract: ',nhrs

      matname = 'TRAC'

C------ Read Project File

      nch = nblank(dfile)
      lun_prj = 1
      file_prjr = dfile(1:nch-3)//'prj'

      call read_prj

      call setgrid(JDATE,JTIME,TSTEP)

      call initmap(JDATE,JTIME,TSTEP)

      return
      end
