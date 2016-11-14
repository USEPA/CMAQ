C**************************************************************
C     Program: NLDN_2D
C     
C     02-DEC-2011
C     Rob Pinder
C     
C     This program generates an IOAPI file of monthly total
C     NLDN strikes 
C
C     Environment variables:
C     NLDNFILE: file with NLDN data     
C     DESCFILE: ioapi input file to set the domain
C     OUTFILE: ioapi output file with monthly total strikes 
C              per grid cell
C
C     
C     
C     
C     
C***************************************************************


      USE M3UTILIO
      IMPLICIT NONE

C     Include Files

C     external functions
C      integer TIME2SEC
C      integer SEC2TIME
C      integer SECSDIFF
C      LOGICAL SETLAM

C     local variables      
      integer numSpec
      parameter (numSpec = 1)

      integer ifin, status
      character *16 pname

      logical setSfirst

      character*120 omFile
      character*120 nldnFile
      character*120 paramsFile
      character*120 iccgFile
      character*120 sfString
      character*1000 omString

      real STRIKE_FACTOR
      real sum_change

      integer col, row, layer
      integer c,r, k, i, c2, r2, prevc, prevr
      real x,y
      integer maxR, maxC ! limits of IC2CG climatology dataset

      integer startdate, starttime, enddate, endtime
      integer lastdate, lasttime, runlen

      integer match
      integer miss
      integer NLDNcount, LTNGcount

      real cg2ic
      real KMSQUARE ! square kilometers in a grid cell

      character*9 date 
      character*13 time
      real lat, lon, val
      real current
      character*11 dump
      character*34 dump2
      integer flag
      integer month,day,year, hour
      integer nday, prevday

      integer curday            ! day of previous iteration
      integer curhour           ! hour of previous iteration
      integer curmonth          ! month of previous iteration
      integer iodate,iotime     ! date and time for writing and reading to file
      integer jday              ! julian date
      
      data pname /'NLDN_2D'/

      integer logdev

      Real, Allocatable :: NLDNstrikes(:,:,:) ! number of strikes from NLDN
      
      logdev = init3()
      match =0
      miss = 0
      LTNGcount = 0
      NLDNcount = 0


C...  get NDLN dataset filename and open
      Call NAMEVAL( 'NLDNFILE', nldnFile )
      open(unit=71, file=nldnFile)

C         read(71,FMT="A9 A13 F8.4 F10.4 F8.1 A13") 
C    &        date, time, lat, lon, current, dump
C         read(date,FMT="I2A1I2A1I2A1")
C     &        month, dump, day, dump, year, dump
C         read(time,FMT="I2A11")
C     &        hour, dump
         
C         write(*,*) date, time, lat, lon, current, month, hour

      

C     open met file
      if( .not. open3('DESCFILE',fsread3,pname)) then
         call m3err(pname, 0, 0,
     &        'Could not open DESCFILE', .TRUE.)
      endif

C     load met file description to set NCOLS3D, NROWS3D, etc.
      if( .not. desc3('DESCFILE')) then
         call m3err (pname, 0, 0,
     &        'Could not load file description from DESCFILE', .TRUE.)
      endif


C...  set Lambert projection
      IF( .NOT. SETLAM( SNGL( P_ALP3D ),
     &     SNGL( P_BET3D ),  
     &     SNGL( P_GAM3D ),               
     &     SNGL( XCENT3D ),               
     &     SNGL( YCENT3D ) ) ) THEN              
         CALL M3EXIT( 'CELL_LL', 0, 0, 
     &        'Lambert projection setup error', 2 )                
      ENDIF  
      write(*,*) P_BET3D, P_GAM3D, XCENT3D, YCENT3D, P_ALP3D

      KMSQUARE = (XCELL3D * 1.0e-3) * (YCELL3D * 1.0e-3)

      Allocate( NLDNstrikes( NCOLS3D , NROWS3D, 1) )


C     write(*,*) P_BET3D, P_GAM3D, XCENT3D, YCENT3D, P_ALP3D
      do c = 1, NCOLS3D
         do r = 1, NROWS3D
               NLDNstrikes(c,r,1) = 0.0
         enddo
      enddo

C     initialize
      setSfirst = .TRUE.
      curhour = -1
      curday = 0


c     loop over all lines in lightning data NLDN file
c format of NLDN file is like so
C08/15/03 00:00:00.083  35.0574 -108.2710   -13.1   1.0  0.4  G
C08/05/03 00:00:00.043  42.9549 -120.8220   -52.2   0.8  0.4  G

C07/01/04 00:00:00.078  49.398 -119.091   -10.0 kA  1  0.4  0.4  1.0 141   0.3  4
C07/01/04 00:00:00.092  49.295 -119.448    -7.2 kA  1  0.4  0.4  1.0 131   2.2  5
C07/01/04 00:00:00.116  43.491 -102.562   -47.0 kA  3  0.9  0.4  2.2  91   1.1  4
C07/01/04 00:00:00.122  34.549  -74.842   +19.0 kA  1  1.9  0.5  3.8 124   0.2  4
C07/01/04 00:00:00.211  32.108  -93.179   -34.6 kA  2  0.5  0.4  1.2  38   0.4  8
C07/01/04 00:00:02.685  49.214 -112.605   -35.9 kA  1  0.7  0.4  1.7 177   0.5  7
C07/01/04 00:00:06.269  41.630 -120.958   -26.6 kA  1  1.2  0.4  3.0  37   1.0  3
C07/01/04 00:00:07.044  31.154  -82.476   -81.8 kA  1  0.4  0.4  1.0  56   0.9 14
C07/01/04 00:00:07.121  29.958  -81.912   -35.4 kA  4  0.8  0.4  2.0  54   0.2  6
C07/01/04 00:00:07.508  31.047  -82.524    -4.2 kA  1 10.5  0.4 26.2 109   0.4  2


      nday = 0
      prevday = 0

      Do

         read(71,FMT="(A9,A13,F7.3,F9.3,F8.1,A34)",end = 400) 
     &        date, time, lat, lon, current, dump2
C         write(*,*) date, time, lat, lon, current, dump2
         read(date,FMT="(I2,A1,I2,A1,I2,A1)")
     &        month, dump, day, dump, year, dump
         read(time,FMT="(I2,A11)")
     &        hour, dump


         if (day.ne.prevday) then
            write(*,*) prevday, day, nday
            nday = nday + 1
            prevday = day
         endif
         

C     screen out spurious flashes
C     if the current is between 0 and 20, likely a intercloud flash 
C     rather than cloud to ground

         if ((current.lt.0).or.(current.gt.15)) then
            

C     calculate the grid cell

C            Call ll2lam(lon,lat,x,y)
	    IF ( .NOT. ll2lam(lon,lat,x,y) ) THEN
	       write(*,*) 'Error converting coordinates'
C     write(*,*) x,y
            ELSE 
               c = INT(x - XORIG3D) / XCELL3D + 0.5
               r = INT(y - YORIG3D) / YCELL3D + 0.5
            ENDIF
c     check to make sure in range
            if ((c.gt.0).and.(c.lt.NCOLS3D).and.
     &           (r.gt.0).and.(r.lt.NROWS3D)) then
               
               NLDNcount = NLDNcount + 1

c     strike is in domain, increment NLDNstrike array
               NLDNstrikes(c,r,1) = NLDNstrikes(c,r,1) + 1

            endif

         endif                  ! screen out spurious flashes

      enddo                     ! loop to next line in NLDN 

 400  continue

c     set up output file

      TSTEP3D = 0  ! create as a time independent dataset
      NVARS3D = 1

      vname3d(1) = "NLDNstrk"
      vdesc3d(1) = "NLDN strikes from NLDN data source"
      Units3d(1) = "km-2 day-1"
      Vtype3d(1) =  M3REAL

C     set ioapi date and time to start of month
      day = 1                   ! set to first day in month
      if (month.eq.1)  jday = day
      if (month.eq.2)  jday = day + 31
      if (month.eq.3)  jday = day + 59
      if (month.eq.4)  jday = day + 90
      if (month.eq.5)  jday = day + 120
      if (month.eq.6)  jday = day + 151
      if (month.eq.7)  jday = day + 181
      if (month.eq.8)  jday = day + 212
      if (month.eq.9)  jday = day + 243
      if (month.eq.10) jday = day + 273
      if (month.eq.11) jday = day + 304
      if (month.eq.12) jday = day + 334

      if ((mod((year+2000),4).eq.0).and.(month.gt.2))
     &     jday = jday + 1

C      iotime = curhour*10000
      iotime = 0
      iodate = (year+2000)*1000 + jday

      write(*,*) "end: ", iodate, iotime

      SDATE3D = iodate
      STIME3D = iotime
      

C     open output file
      if( .not. open3('OUTFILE',5,pname)) then
         call m3err(pname, 0, 0,
     &        'Could not open OUTFILE', .TRUE.)
      endif


C     convert NLDNstrk to strikes per day per square km

      NLDNstrikes = NLDNstrikes / nday / KMSQUARE

      IF( .NOT. WRITE3('OUTFILE','NLDNstrk',iodate,iotime,NLDNstrikes))
     &     go to 600


C     close all files 
      Status = SHUT3 ()

      stop
      
 500  Call m3err (pname, iodate, iotime,
     &     'Input variables not defined for time step', .TRUE.)
 600  Call m3err (pname, iodate, iotime,
     &     'Cannot write output variables for time step', .TRUE.)
      

 920  FORMAT (I11,16E11.3)
 930  FORMAT (A30, E12.3, E12.3, A10, A14, I4, I4)
      END
