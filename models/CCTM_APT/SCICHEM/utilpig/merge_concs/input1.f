      subroutine input1(matname,JDATE,JTIME,TSTEP,
     &                  time,nhrs,pfile,nch)

      use common_puf
      use common_met
      use multcomp_inc
      use files_inc
      use error_inc
!     use host_inc
      use M3UTILIO

      implicit none

      character*(*) pfile
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

C------ Puff  file

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
      TIMHRS = FLOAT ( SECSDIFF(STDATE, STTIME, JDATE, JTIME) ) /
     &                  3600.

      time = timhrs

      write(*,*)'time: ',time
      NHRS = SECSDIFF(JDATE, JTIME, JEDATE, JETIME) / 3600 + 1

      write(*,*)'no. of hours to extract: ',nhrs

      matname = 'TRAC'

C------ Read Project File

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

      return
      end
