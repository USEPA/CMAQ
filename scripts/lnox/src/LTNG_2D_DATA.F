C**************************************************************
C     Program: LTNG_2D_DEFN
C     
C     02-NOV-2010
C     Rob Pinder
C     
C     This program generates an IOAPI file of scaling factors
C     that alter the inline calculated lightning strikes to 
C     match the NLDN and correct IC:CG ratio.
C
C     True lightning is LTNG calculated by CMAQ * this ratio
C     Assumes LTNG = RC * STRIKE_FACTOR
C     LTNG: lightning strikes
C     RC: convective precip
C     STRIKE_FACTOR: relates convective precip to lightning strikes,
C                    originally calculated using NLDN over longer
C                    time periods
C     
c     CMAQ will read in OUTFILE and multiple LTNG * LTratio to 
C     get true lightning strike total
C
C     Environment variables:
C     NLDNFILE: file with NLDN data     
C     METFILE: met input file with layer height and temperature
C     CLOUDFILE: cloud input file with cloud information
C     CG2IC: ratio of cloud to ground strikes and intercloud lightning
C     PARAMS: file with paramaters to drive calculations
C     EMISFILE: file to put output NO lightning emissions
C     
C     Update: Jan 23, 2012
C     Altered to work with monthly National Lightning Detection 
C     Network strike density (strikes km^-2 day^-1)
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
      real MOLSNCGv
      real MOLSNICv
      real sum_change
      real KMSQUARE ! square kilometers in a grid cell

      integer col, row, layer
      integer c,r, k, i, c2, r2, prevc, prevr
      real x,y
      integer maxR, maxC ! limits of IC2CG climatology dataset

      integer startdate, starttime, enddate, endtime
      integer lastdate, lasttime, runlen

      integer match
      integer miss
      integer NLDNcount, LTNGcount
      real count
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
      integer curdate,curtime   ! date and time for reading file
      
      data pname /'LTNG_2D_DATA'/

      integer logdev

      Real, Allocatable :: RC(:,:,:) ! convective precip in cm from METCRO2D
      Real, Allocatable :: CLDT(:,:,:) ! cloud top in meters precip from METCRO2D
      Real, Allocatable :: OCEAN_MASK(:,:) ! 0 if continental, 1 if off-shore
      Real, Allocatable :: STRKCNT(:,:) ! strikes per unit precip
      Real, Allocatable :: MOLSN(:,:) ! moles N per flash
      Real, Allocatable :: MOLSNIC(:,:) ! moles N per flash
      Real, Allocatable :: iccg(:,:) ! number of intercloud strikes to one ground strike
      Real, Allocatable :: NLDNstrikes(:,:,:) ! number of strikes from NLDN
      Real, Allocatable :: LTNGstrikes(:,:,:) ! number of calculated strikes
      Real, Allocatable :: LTNGratio(:,:,:) ! ratio of NLDN to calculated strikes      

      Real, Allocatable :: IC2CG(:,:)

      
      logdev = init3()
      match =0
      miss = 0
      LTNGcount = 0
      NLDNcount = 0


c     read in factor: RC*strikeFactor = LTNGstrikes
      Call NAMEVAL('STRIKE_FACTOR',SFstring)  
      read(sfString,*) STRIKE_FACTOR

c     read in moles N per CG flash
      Call NAMEVAL('MOLES_N_CG',SFstring)  
      read(sfString,*) MOLSNCGv

c     read in moles N per CG flash
      Call NAMEVAL('MOLES_N_IC',SFstring)  
      read(sfString,*) MOLSNICv
      

C     open NLDN file
      if( .not. open3('NLDNFILE',fsread3,pname)) then
         call m3err(pname, 0, 0,
     &        'Could not open NLDNFILE', .TRUE.)
      endif
      

C     open met file
      if( .not. open3('METFILE',fsread3,pname)) then
         call m3err(pname, 0, 0,
     &        'Could not open METFILE', .TRUE.)
      endif

C     load met file description to set NCOLS3D, NROWS3D, etc.
      if( .not. desc3('METFILE')) then
         call m3err (pname, 0, 0,
     &        'Could not load file description from METFILE', .TRUE.)
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

      Allocate( RC ( NCOLS3D , NROWS3D, 1) )
      Allocate( CLDT ( NCOLS3D , NROWS3D, 1) )
      Allocate( OCEAN_MASK ( NCOLS3D , NROWS3D ) )
      Allocate( MOLSN ( NCOLS3D , NROWS3D ) )
      Allocate( MOLSNIC ( NCOLS3D , NROWS3D ) )
      Allocate( STRKCNT ( NCOLS3D , NROWS3D ) )
      Allocate( iccg ( NCOLS3D , NROWS3D ) )
      Allocate( NLDNstrikes( NCOLS3D , NROWS3D, 1) )
      Allocate( LTNGstrikes( NCOLS3D , NROWS3D, 1) )
      Allocate( LTNGratio ( NCOLS3D , NROWS3D , 1 ) )




C     set moles N per flash
      do c = 1,NCOLS3D
         do r=1,NROWS3D
            MOLSN(c,r) = MOLSNCGv ! set to same moles N per flash everywhere
            MOLSNIC(c,r) = MOLSNICv ! set to same moles N per IC flash everywhere
            STRKCNT(c,r) = STRIKE_FACTOR ! strikes per unit precip
         enddo
      enddo


C     read in ocean mask, generated using ocean_mask.R
C     is 1 if lightning is off-shore
C     and is 0 if lightning is near-continental (within 300 km)

C...  get ocean mask dataset filename and open
      Call NAMEVAL( 'OCEANMASKFILE', omFile )
      write(*,*) omFile
      open(unit=73, file=omFile)
c      write(*,*) NCOLS3D, NROWS3D
      do r=1,NROWS3D
         read(73,*) (OCEAN_MASK(c,r),c=1,NCOLS3D)
c         write(*,*) (OCEAN_MASK(c,r),c=138,148)
c         write(*,*) r
      enddo
      close(73)



C...  get number of intercloud per ground strike
      Call NAMEVAL( 'ICCG', iccgFile)
      write(*,*) iccgFile

      open(unit=73, file=iccgFile)
      do r=1,NROWS3D
         read(73,*) (iccg(c,r),c=1,NCOLS3D)
C         write(*,*) (iccg(c,r),c=138,148)
C         write(*,*) r

      enddo
      close(73)


C     write(*,*) P_BET3D, P_GAM3D, XCENT3D, YCENT3D, P_ALP3D
      do c = 1, NCOLS3D
         do r = 1, NROWS3D
               LTNGstrikes(c,r,1) = 0.0
               LTNGratio(c,r,1) = 0.0
         enddo
      enddo


C  Compute last date and time to process                                                                                                                                                                          

      startDate = SDATE3D
      startTime = STIME3D

C     read in NLDNstrikes
         IF( .NOT. READ3('NLDNFILE','NLDNstrk',ALLAYS3,
     &     startDate,startTime,NLDNstrikes))
     &        go to 500


      write(*,*) startDate
      write(*,*) startTime

      curdate = startDate
      curtime = startTime


      RUNLEN = SEC2TIME( (MXREC3D-1) * TIME2SEC( TSTEP3D ) )
      endDate = SDATE3D
      endTime = STIME3D
      CALL NEXTIME ( endDate, endTime, runlen )

c loop over all time periods in METFILE
      
      count = 0.0


      Do
         write(*,'('' Processing Variables at time:'',i7,'':'',i6)' )
     &    curdate, curtime, count
         write(*,*) count
         count = count + 1

         
         IF( .NOT. READ3('METFILE','RC',ALLAYS3,curdate,curtime,RC))
     &        go to 500
         
         IF( .NOT. READ3('METFILE','CLDT',ALLAYS3,curdate,curtime,CLDT))
     &        go to 500

!     loop over all rows and columns, adding lightning strikes
!     calculated as convective rainfall * strike factor
!     only include lightning on cloud tops greater than 5500 meters

         do c = 1, NCOLS3D
            do r = 1, NROWS3D

               NLDNcount = NLDNcount + NLDNstrikes(c,r,1) * KMSQUARE
               if (CLDT(c,r,1) > 5500.0) then ! check cloud top > 5500 meters

                  LTNGstrikes(c,r,1) = LTNGstrikes(c,r,1) + 
     &                 RC(c,r,1) * STRIKE_FACTOR
c     &                    ((1 + iccg(c,r)) * OCEAN_MASK(c,r))

                  LTNGcount = LTNGcount + 
     &                 INT(RC(c,r,1) * STRIKE_FACTOR)
                  
               endif            ! end if for adding lighting strikes
            enddo
         enddo


C     advance to next step                                                                                                                                                                                        
         call NEXTIME ( curdate, curtime, TSTEP3D )
         if( SECSDIFF(curdate, curtime, endDate, endTime) .lt. 0 )
     &        exit

      enddo



C##################################################################
c     calculate ratio

 400  continue

      do c = 1, NCOLS3D
         do r = 1, NROWS3D

C     convert to strike km^-2 day^-1
            LTNGstrikes(c,r,1) = LTNGstrikes(c,r,1) / KMSQUARE 
     &           * 24.0 / count

            if ((LTNGstrikes(c,r,1) .gt. 0.0) .and. 
     &           (NLDNstrikes(c,r,1) .gt. 0.0)) then

C              calculate ratio of total NLDN for that grid cell and 
C              what is calculated from strike estimate
C              set upper bound on this ratio to 50, to avoid blowing up
               LTNGratio(c,r,1) = 
     &              MIN(NLDNstrikes(c,r,1) / LTNGstrikes(c,r,1),
     &                  50.0)

               match = match + 1

            else

c     if zero NLDN strikes, set ratio to 1.0 in order to include lightning 
c     in places where detection is low (offshore)

               LTNGratio(c,r,1) = 1.0
               if (NLDNstrikes(c,r,1).gt.0.0) miss = miss + 1
            endif
         enddo
      enddo

c     set up output file

      TSTEP3D = 0  ! create as a time independent dataset
      NVARS3D = 8
      vname3d(1) = "LTratio"
      vdesc3d(1) = "NLDN correction factor"
      Units3d(1) = "unitless"
      Vtype3d(1) =  M3REAL

      vname3d(2) = "LTstrk"
      vdesc3d(2) = "LTNG strikes as calculated RC * strike_count"
      Units3d(2) = "km-2 day-1"
      Vtype3d(2) =  M3REAL

      vname3d(3) = "NLDNstrk"
      vdesc3d(3) = "NLDN strikes from NLDN data source"
      Units3d(3) = "km-2 day-1"
      Vtype3d(3) =  M3REAL

      vname3d(4) = "ICCG"
      vdesc3d(4) = "number of intercloud strikes per ground strike"
      Units3d(4) = "unitless"
      Vtype3d(4) =  M3REAL

      vname3d(5) = "OCNMASK"
      vdesc3d(5) = "ocean mask: offshore, less lightning is produced"
      Units3d(5) = "unitless"
      Vtype3d(5) =  M3REAL

      vname3d(6) = "MOLSN"
      vdesc3d(6) = "moles of N per CG flash"
      Units3d(6) = "molN"
      Vtype3d(6) =  M3REAL

      vname3d(7) = "MOLSNIC"
      vdesc3d(7) = "moles of N per IC flash"
      Units3d(7) = "molN"
      Vtype3d(7) =  M3REAL

      vname3d(8) = "STRKCNT"
      vdesc3d(8) = "strike count"
      Units3d(8) = "unitless"
      Vtype3d(8) =  M3REAL



C     open output file
      if( .not. open3('OUTFILE',5,pname)) then
         call m3err(pname, 0, 0,
     &        'Could not open OUTFILE', .TRUE.)
      endif

      write(*,*) "end: ", startdate, starttime


C     write LTratio as first time in first day of month
      
      IF( .NOT. WRITE3('OUTFILE','LTratio',startdate,starttime,LTNGratio))
     &     go to 600

      IF( .NOT. WRITE3('OUTFILE','LTstrk',startdate,starttime,LTNGstrikes))
     &     go to 600

      IF( .NOT. WRITE3('OUTFILE','NLDNstrk',startdate,starttime,NLDNstrikes))
     &     go to 600

      IF( .NOT. WRITE3('OUTFILE','ICCG',startdate,starttime,iccg))
     &     go to 600

      IF( .NOT. WRITE3('OUTFILE','OCNMASK',startdate,starttime,OCEAN_MASK))
     &     go to 600

      IF( .NOT. WRITE3('OUTFILE','MOLSN',startdate,starttime,MOLSN))
     &     go to 600

      IF( .NOT. WRITE3('OUTFILE','STRKCNT',startdate,starttime,STRKCNT))
     &     go to 600

C     close all files 
      Status = SHUT3 ()

C     do some quality checks

C     match = locations with both NLDN and LTNG from convective precip
C     miss = locations with no convective precip but strikes from NLDN
      write(*,*) "Match: ", match, " Miss: ", miss, " Ratio: ",
     &     REAL(match) / REAL((miss + match))

      write(*,*) "NLDN count: ", NLDNcount
      write(*,*) "LTNG count: ", LTNGcount

      sum_change = 0.0
      do c = 1, NCOLS3D
         do r = 1, NROWS3D
            sum_change = sum_change + 
     &           LTNGstrikes(c,r,1)*LTNGratio(c,r,1)*OCEAN_MASK(c,r) 
         enddo
      enddo
      write(*,*) "sum change: ", sum_change

C     LTNGcount = 0
C     do c = 1, NCOLS3D
C     do r = 1, NROWS3D
C     LTNGcount = LTNGcount + 
C     &           INT(LTNGratio(c,r,1) * LTNGstrikes(c,r,1))
C     enddo
C     enddo

c     check to see if the post-LTNG ratio adjusted is equal to NLDN
c     it should be a little off, because the count is assumed to be
c     an integer, so we lose fractions of a lightning strike
C     write(*,*) "LTNG ratio adjusted: ", LTNGcount


      stop
      
 500  Call m3err (pname, curdate, curtime,
     &     'Input variables not defined for time step', .TRUE.)
 600  Call m3err (pname, startdate, starttime,
     &     'Cannot write output variables for time step', .TRUE.)
      

 920  FORMAT (I11,16E11.3)
 930  FORMAT (A30, E12.3, E12.3, A10, A14, I4, I4)
      END
