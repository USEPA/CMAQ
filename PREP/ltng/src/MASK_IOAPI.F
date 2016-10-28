C**************************************************************
C     Program: LTNG_2D_DEFN
C     
C     02-NOV-2010
C     Rob Pinder
C     
C     This program generates an IOAPI file of a file that 
C     specifies a mask.
C
C     Assumes dimensions of mask input equals description file
C
C     Environment variables:
C     DESCFILE: met input file with layer height and temperature
C     MASKFILE: comma separated file containing mask values
C     OUTFILE: file to put output 
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

      character*9 date 
      character*13 time
      real lat, lon, val
      real current
      character*11 dump
      integer flag
      integer month,day,year, hour

      integer curday            ! day of previous iteration
      integer curhour           ! hour of previous iteration
      integer curmonth          ! month of previous iteration
      integer iodate,iotime     ! date and time for writing and reading to file
      integer jday              ! julian date
      
      data pname /'LTNG_2D_DATA'/

      integer logdev

      Real, Allocatable :: MASK(:,:) ! 0 if continental, 1 if off-shore

      
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



      Allocate( MASK ( NCOLS3D , NROWS3D ) )



C     read in ocean mask, generated using ocean_mask.R
C     is 1 if lightning is off-shore
C     and is 0 if lightning is near-continental (within 300 km)

C...  get ocean mask dataset filename and open
      Call NAMEVAL( 'MASKFILE', omFile )
      write(*,*) omFile
      open(unit=73, file=omFile)
c      write(*,*) NCOLS3D, NROWS3D
      do r=1,NROWS3D
         read(73,*) (MASK(c,r),c=1,NCOLS3D)
      enddo
      close(73)


c     set up output file

      TSTEP3D = 0  ! create as a time independent dataset
      NVARS3D = 1

      vname3d(1) = "MASK"
      vdesc3d(1) = "mask: 1 or 0"
      Units3d(1) = "unitless"
      Vtype3d(1) =  M3REAL


C     open output file
      if( .not. open3('OUTFILE',5,pname)) then
         call m3err(pname, 0, 0,
     &        'Could not open OUTFILE', .TRUE.)
      endif

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


C     write LTratio as first time in first day of month
      
      IF( .NOT. WRITE3('OUTFILE','MASK',iodate,iotime,MASK))
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
