!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/TOOLS/src/combine/module_file.F,v 1.1.1.1 2005/07/27 12:55:20 sjr Exp $

C***********************************************************************
C
C  MODULE:  sets up file data
C             
C***********************************************************************
      MODULE M3FILES

      USE M3UTILIO

      INTEGER, Public :: N_M3FILES                   ! No. of input Models-3 files

      INTEGER, PARAMETER, Public  :: MXM3FLS = MXFILE3 - 1   ! Max no. of input files - use IOAPI parameter 
                                                             ! MXFILE3 - 1 to also allow one output file
 
      CHARACTER*16, Public :: M3FILENAME( MXM3FLS )  ! filenames
      CHARACTER*10, Public :: FILETYPE( MXM3FLS )    ! filetypes
      Integer, Public      :: WRFid( MXM3FLS )       ! NCIDs for WRF files

      Logical, Public :: convert(MXM3FLS)            ! convert flags

      INTEGER, Public :: startDate, startTime
      INTEGER, Public :: endDate, endTime
      INTEGER, Public :: TSTEP
      INTEGER, Public :: NROWS
      INTEGER, Public :: NCOLS
      INTEGER, Public :: NLAYS
      REAL*8,  Public :: XCELL
      REAL*8,  Public :: YCELL
      REAL*8,  Public :: XORIG
      REAL*8,  Public :: YORIG 
      REAL*8,  Public :: YCENT 

      REAL*8,  Public, PARAMETER :: smallnum = 0.001d0

      Public :: canConvert, OPENFILE,    getDESC, WRF_DESC, OPEN_FILES, ReadValues,
     &          Rd_ioapi,     Rd_wrf, SetMapProj,   ToProj,       ToLL
      
      Private 

      Interface            
         Subroutine getFld( record, delimiter, nth, del, field, exception )
            CHARACTER*(*), Intent( In  ) :: record
            CHARACTER*(*), Intent( In  ) :: delimiter
            CHARACTER,     Intent( Out ) :: del
            Integer,       Intent( In  ) :: nth
            CHARACTER*(*), Intent( Out ) :: field
            CHARACTER*(*), Optional, Intent( In ) :: exception
         End Subroutine getFld
         INTEGER FUNCTION getFldCount(record, delimiter, exception) Result(nfields)
            CHARACTER*(*), Intent( In ) :: record
            CHARACTER*(*), Intent( In ) :: delimiter
            CHARACTER*(*), Optional, Intent( In ) :: exception
         End FUNCTION getFldCount
         Subroutine LeftTrim( STRING )
            CHARACTER*(*), INTENT( INOUT ) :: STRING
         End Subroutine LeftTrim
         Subroutine RightTrim( STRING )
            CHARACTER*(*), INTENT( INOUT ) :: STRING
         End Subroutine RightTrim
         SUBROUTINE UCASE ( STR )
            CHARACTER, INTENT( INOUT ) :: STR*( * )
         END SUBROUTINE UCASE
         Subroutine replace( string, old, new )
            Character*(*), Intent( InOut ) :: string
            Character*(1), Intent( In    ) :: old    
            Character*(1), Intent( In    ) :: new    
         End Subroutine replace 
         SUBROUTINE Remove_WhiteSpaces (text)
            CHARACTER*(*), Intent( InOut ) :: text
         END SUBROUTINE Remove_WhiteSpaces
      End Interface
      
      CONTAINS

C***********************************************************************
C   open input ioapi files
C***********************************************************************
         SUBROUTINE OPEN_FILES

         USE M3UTILIO
        
         IMPLICIT NONE 

         ! LOCAL VARIABLES:
         INTEGER   n, m               ! Loop index
         INTEGER   status             ! Status code
         INTEGER   EDATE, ETIME, runlen
         Character*(256) fname

         LOGICAL valid
         LOGICAL function canConvert


         !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ! Determine the number of input CTM conc files that need to be read
         !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         N_M3FILES = 0
         if( .NOT. OPENFILE( 1 ) ) then
           Write(*,'(''**Error** Could not open input file'',
     &               '' for "INFILE1"'')')
           return
           endif

         if( .NOT. getDESC( 1 ) ) then
           Write( *, '(''**Error** While running getDESC on '',
     &                 A)' ) M3FILENAME(1)
           return
           endif

         N_M3FILES = 1

         ! save file parameters
         TSTEP = TSTEP3D
         NROWS = NROWS3D
         NCOLS = NCOLS3D
         NLAYS = NLAYS3D
         XCELL = XCELL3D
         YCELL = YCELL3D
         XORIG = XORIG3D
         YORIG = YORIG3D
         YCENT = YCENT3D
    
         ! set startDate and startTime from first file         
         startDate = SDATE3D
         startTime = STIME3D

         ! compute ending time of file 
         endDate = SDATE3D
         endTime = STIME3D
         DO m = 1, MXREC3D-1
           Call Nextime(endDate, endTime, TSTEP3D)
           enddo

         ! try to open files (2-MXM3FLS) 
         DO n = 2, MXM3FLS

           if( .NOT. OPENFILE( n ) ) exit

           if( .NOT. getDESC( n ) ) then
             Write( *, '(''**Error** While running getDESC on '',A)' )
     &             M3FILENAME(n)
             N_M3FILES = 0
             return 
             endif
               
           valid = .true.

           !! if file is time dependent, adjust start and end dates
           if( TSTEP3D.gt. 0 ) then

             if( TSTEP.lt.TSTEP3D ) valid = .false.  ! time step

             ! reset startDate and startTime if file starts after
             if( SECSDIFF (startDate, startTime, SDATE3D, STIME3D) .gt. 0 ) then
               startDate = SDATE3D
               startTime = STIME3D
               endif
 
             ! compute ending time of file
             EDATE = SDATE3D
             ETIME = STIME3D
             DO m = 1, MXREC3D-1
               Call Nextime(EDATE, ETIME, TSTEP3D)
               enddo

             ! reset endDate and endTime if file ends before
             if( SECSDIFF (endDate, endTime, EDATE, ETIME) .lt. 0 ) then
               endDate = EDATE
               endTime = ETIME
               endif
             endif   ! time independent file

           ! verify domain parameters

           if( DABS(XCELL-XCELL3D) > smallnum ) valid = .false. ! delta-X
           if( DABS(YCELL-YCELL3D) > smallnum ) valid = .false. ! delta-Y
           if( .NOT.valid ) then
             Write( *, '(''**Error** Inconsistent file domain for ''
     &             ,A)' ) M3FILENAME(n)
             N_M3FILES = 0
             return
             endif 

           ! check if file needs to be converted
           convert(N) = .false.
           if( NROWS.ne.NROWS3D ) convert(N) = .true.
           if( NCOLS.ne.NCOLS3D ) convert(N) = .true.
           if( DABS(XORIG-XORIG3D) > smallnum ) convert(N) = .true.
           if( DABS(YORIG-YORIG3D) > smallnum ) convert(N) = .true.

           ! check if file can be converted
           if( convert(N) .and. (.NOT.canConvert()) ) then
             Write( *, '(''**Error** Inconsistent file domain for ''
     &             ,A)' ) M3FILENAME(n)
             write(*,'(''     NROWS='',2i8)') NROWS, NROWS3D
             write(*,'(''     NCOLS='',2i8)') NCOLS, NCOLS3D
             write(*,'(''     XORIG='',2f12.2)') REAL(XORIG), REAL(XORIG3D)
             write(*,'(''     YORIG='',2f12.2)') REAL(YORIG), REAL(YORIG3D)
             N_M3FILES = 0
             return
             endif
         
           N_M3FILES = N_M3FILES +1
           enddo   !! end file open loop
 
         ! get file description for M3FILENAME(1)
         if( .NOT. getDESC( 1 ) ) then
           Write( *, '(''**Error** While running getDESC on '',A)' )
     &            M3FILENAME(1)
           N_M3FILES = 0
           return
           endif
             
         return

         END SUBROUTINE OPEN_FILES


C***********************************************************************
C   Determine file type and open file for given type
C***********************************************************************
         Logical Function OPENFILE(fileNo) result( rstatus )

         USE M3UTILIO

         IMPLICIT NONE

         !! Include netcdf header file
         INCLUDE 'netcdf.inc'

         !! arguments
         Integer fileNo

         !! local variables
         Character*(256) fname
         Integer status
         Logical valid
         Integer ncid
         Integer dimid
         Integer attlen

   
         !! check for invalid file number 
         if( fileNo.le.0 .or. fileNo.gt.MXM3FLS ) then
           Write( *, '(''**Error** Invalid file number in OPENFILE routine'')' )
           rstatus = .false.
           return
           endif


         !! build envName for file
         if( fileNo.lt.10 ) then
          write(M3FILENAME(fileNo), '( ''INFILE'', I1 )' ) fileNo
         elseif(fileNo.lt.100) then
          write(M3FILENAME(fileNo), '( ''INFILE'', I2 )' ) fileNo
         else
          write(M3FILENAME(fileNo), '( ''INFILE'', I3 )' ) fileNo
         endif

         !! get filename from ENV variable
         Call NAMEVAL( M3FILENAME(fileNo), fname )

         !! check if file exist
         INQUIRE(file=fname, exist=valid)
         if( .not.valid ) then
           write(*,'(''**WARNING** File does not exist:'',a)') TRIM(fname)
           rstatus = .false.
           return
           endif

         !! open file as a netcdf file and determine type
         status = NF_OPEN(fname, NF_NOWRITE, ncid)    
         if( status.ne.0 ) then
           write(*,'(''**ERROR** Cannot open input file:'',a)') TRIM(fname)
           rstatus = .false.
           return
           endif

         !! check for IOAPI file, verify attribute "IOAPI_VERSION"  
         status = NF_INQ_ATTLEN(ncid, NF_GLOBAL, 'IOAPI_VERSION', attlen)
         if( status.eq.0 ) then
           filetype(fileNo) = 'IOAPI'
           status = NF_CLOSE(ncid)
           rstatus = OPEN3( M3FILENAME(fileNo), 1, 'combine') 
           return
           endif

         !! check for WRF file, verify dimension "west_east"  
         status = NF_INQ_DIMID(ncid, 'west_east', dimid)
         if( status.eq.0 ) then
           filetype(fileNo) = 'WRF'
           WRFid(fileNo) = ncid

           if( .NOT.WRF_DESC( fileNo ) ) then
             rstatus = .false.
             return
             endif

           !! write file description
           write(*,'(/,5x,''"'',a,''" opened as OLD:READ-ONLY'')') TRIM(M3FILENAME(fileNo))
           write(*,'(5x,''File name "'',a,''"'')') TRIM(fname) 
           write(*,'(5x,''File type netcdf'')') 
           write(*,'(5x,''Execution ID "'',a,''"'')') TRIM(EXECN3D) 
           write(*,'(5x,''Dimension:'',i5,'' rows, '',i5,'' cols, '',i5,'' lays, '',i5,'' vbles'')')
     &               NROWS3D, NCOLS3D, NLAYS3D, NVARS3D
           write(*,'(5x,''NetCDF ID:'',i10)') ncid 
           write(*,'(5x,''Starting date and time '',i8,'':'',i6.6,'' ('',a,'')'')')
     &               SDATE3D, STIME3D, DT2STR(SDATE3D, STIME3D)
           write(*,'(5x,''Time step    '',i8,'' ('',a,'' hh:mm:ss)'' )') TSTEP3D, TRIM(HHMMSS(TSTEP3D))
           write(*,'(5x,''Maximun current record number'',i10,/)')  MXREC3D

           rstatus = .true. 
           return
           endif

         !! Unknown file type, return error status
         write(*,'(''**ERROR** Unknown file type:'',a)') TRIM(fname)
         rstatus = .false.

         return 
         end Function OPENFILE


C*****************************************************************************
C   set the file variables defining map projection, grid, and time parameters
C*****************************************************************************
         Logical Function getDESC(fileNo) result( rstatus )

         USE M3UTILIO

         IMPLICIT NONE

         !! arguments
         Integer fileNo

         !! local variables
         Character*(256) fname
         Integer status
         Logical valid

         !! OUTPUT file type
         if( fileNo .eq. 0 ) then
           rstatus = DESC3( 'OUTFILE' )
           return
           endif 

         !! IOAPI file type
         if( FILETYPE( fileNo ) .eq. 'IOAPI' ) then
           rstatus = DESC3( M3FILENAME(fileNo) )
           return
           endif 

         !! WRF file type
         if( FILETYPE( fileNo ) .eq. 'WRF' ) then
           rstatus = WRF_DESC( fileNo )
           return
           endif 

         return
         end Function getDESC



C***********************************************************************
C   check if domain grid is a subgrid of current file description
C***********************************************************************
         Logical Function canConvert() result( pass )

         USE M3UTILIO

         IMPLICIT NONE 

         Real xdiff, ydiff
         Integer xoffset, yoffset 


         pass = .false.
         
         ! find origin different
         xdiff = XORIG - XORIG3D
         ydiff = YORIG - YORIG3D
         
         ! check lower limits
         if( xdiff.lt.0.0 ) return 
         if( ydiff.lt.0.0 ) return

         ! check upper limits
         if( (XORIG + NCOLS*XCELL) .gt. (XORIG3D + NCOLS3D*XCELL3D) ) return
         if( (YORIG + NROWS*YCELL) .gt. (YORIG3D + NROWS3D*YCELL3D) ) return

         ! compute row and column offsets that grid lies on grid line
         xoffset = xdiff / XCELL3D
         yoffset = ydiff / YCELL3D
         
         ! check that offsets lies on grid line
         if( int(xoffset*XCELL3D) .ne. int(xdiff) ) return
         if( int(yoffset*YCELL3D) .ne. int(ydiff) ) return

         pass = .true.
         return       

         END FUNCTION canConvert 


C***********************************************************************
C   routine to read species values from file
C***********************************************************************
         SUBROUTINE ReadValues( fileNo, specName, ilayer, idate, itime,
     &                          isize, specValue, status)

         USE M3UTILIO

         IMPLICIT NONE

         ! argument variables
         Integer  fileNo
         Character*(*) specName
         Integer ilayer, idate, itime, isize, status
         Real specValue(isize)

         status = 0

         !! check to read OUTPUT file
         if( fileNo .eq. 0 ) then
           if(.NOT.SYNC3( 'OUTFILE' )) Write(*,'(''**ERROR** on SYNC3 call'')')
            
           if(.NOT.READ3( 'OUTFILE', specName, ilayer, idate,
     &                  itime, specValue)) status = -1
 
           return
           endif

         !! check file type and call read it's read routine
         if( FILETYPE(fileNo) .eq. 'IOAPI' ) then
           Call Rd_ioapi( M3FILENAME(fileNo), specName, ilayer, idate, itime,
     &                    isize, specValue, status)
           return
           endif

         if( FILETYPE(fileNo) .eq. 'WRF' ) then
           Call Rd_wrf( fileNo, specName, ilayer, idate, itime,
     &                  isize, specValue, status)
           return
           endif

         Write(*,'(/,''**ERROR** Unknown file type for file number:'',i3)') fileNo
         stop
         
         end SUBROUTINE ReadValues  



C***********************************************************************
C   routine to read species values from IOAPI file
C***********************************************************************
         SUBROUTINE Rd_ioapi( fileName, specName, ilayer, idate, itime,
     &                          isize, specValue, status)

         USE M3UTILIO

         IMPLICIT NONE 

         ! argument variables
         Character*(*) fileName
         Character*(*) specName
         Integer ilayer, idate, itime, isize, status
         Real specValue(isize)

         ! local variables
         Integer fileNo
         Integer jdate, jtime
         Integer colOffset, rowOffset
         Real, Allocatable :: values(:,:,:)
         Integer lay1, lay2
         Integer k, c, r, l

         ! read file number from fileName
         read(fileName,'(6x,i2)', iostat=status) fileNo

         ! get file description
         if( .NOT.getDESC( fileNo ) ) then
           status = -1
           return
           endif

         ! set time and date to read, if TSTEP3D==0, then set date/time to SDATE3D/STIME3D
         jdate = idate
         jtime = itime
         if( TSTEP3D.eq.0 ) then
           jdate = SDATE3D
           jtime = STIME3D
           endif

         ! if no conversion needed, read the values directly
         if( .NOT.convert(fileNo) ) then

           if(.NOT.READ3( fileName, specName, ilayer, jdate,
     &                  jtime, specValue)) status = -1 
           return
           endif


         !!!! read values from file and convert to specValue 
        
         ! determine number of layers to read 
         lay1 = NLAYS3D
         if(ilayer.gt.0) lay1 = 1

         ! allocate values array 
         Allocate ( values(NCOLS3D, NROWS3D, lay1) )
               
         ! read values from super file
         if(.NOT.READ3( fileName, specName, ilayer, jdate,
     &                 jtime, values)) then 
           status = -1
           return
           endif 

         ! compute column and row offsets
         colOffset = (XORIG - XORIG3D) / XCELL3D
         rowOffset = (YORIG - YORIG3D) / YCELL3D

         ! compute starting and ending layers to copy
         lay1 = 1
         lay2 = NLAYS
         if( ilayer.gt.0 ) then
           lay1 = 1
           lay2 = 1
           endif  

         ! copy values array to specValue array
         k = 0
         do l=lay1,lay2
           do r=1,NROWS
             do c=1,NCOLS
               k = k+1
               specValue(k) = values( c+colOffset, r+rowOffset, l)
               enddo
             enddo
           enddo

         Deallocate( values )
         Return 

         END SUBROUTINE Rd_ioapi


C***********************************************************************
C   routine to read species values from WRF file
C***********************************************************************
         SUBROUTINE Rd_wrf( fileNo, specName, ilayer, idate, itime,
     &                          isize, specValue, status)

         USE M3UTILIO

         IMPLICIT NONE

         !! Include netcdf header file
         INCLUDE 'netcdf.inc'

         ! argument variables
         Integer fileNo   
         Character*(*) specName
         Integer ilayer, idate, itime, isize, status
         Real specValue(isize)

         ! local variables
         Integer varid
         Integer curDate, curTime, n, step
         Integer dimids( NF_MAX_VAR_DIMS )
         Character*64 name
         Integer xtype, ndims, natts
         Integer start(4), count(4)
         Integer size2d       
         Integer colOffset, rowOffset
         Integer l, i, j

         ! get file description
         if( .NOT.getDESC( fileNo ) ) then
           status = -1
           return
           endif

         !! get varid for variable
         status =          NF_INQ_VARID( WRFid(fileNo), specName, varid)
         status = status + NF_INQ_VAR( WRFid(fileNo), varid, name, xtype, ndims, dimids, natts )
         if( status.ne. 0 ) then
           write(*,'(''**ERROR** Reading species ['',a,'']'')') TRIM(specName)
           return           
           endif

         !! determine time step to read
         curDate = SDATE3D
         curTime = STIME3D
         step = -1
         do n = 1, MXREC3D
           if( SECSDIFF (curDate, curTime, idate, itime) .eq. 0 ) then
             step = n
             EXIT
             endif
           Call NEXTIME(curDate, curTime, TSTEP3D)
           enddo

         !! check for date/time out of range
         if( step.le.0 ) then
           status = -1
           return
           endif

         !! read 2-D 
         if( ndims.eq.3 ) then

           !! check if conversion needed
           if( convert(fileNo) ) then 
             colOffset = (XORIG - XORIG3D) / XCELL3D
             rowOffset = (YORIG - YORIG3D) / YCELL3D
             start = (/ 1+colOffset, 1+rowOffset, step, 0 /)
             count = (/     NCOLS,     NROWS,    1, 0 /)
           else 
             start = (/       1,       1, step, 0 /)
             count = (/ NCOLS3D, NROWS3D,    1, 0 /)
             endif
     
           !! read variable 
           status = NF_GET_VARA_REAL( WRFid(fileNo), varid, start, count, specValue)
           if( status.ne. 0 ) then
             write(*,'(''**ERROR** Reading species ['',a,'']'')') TRIM(specName)
             return           
             endif

           !! if more then 1 layer, duplicate layer 1 to rest of layers
           if(ilayer.lt.0) then
             size2d = NCOLS * NROWS

             do l = 2, NLAYS
               do i = 1, size2d
                 j = size2d * (l-1) + i
                 specValue(j) = specValue(i) 
                 enddo
               enddo

             endif 

          return
          endif   !! read 2-D

         !! read 3-D
         if( ndims.eq.4 ) then

           !! check if conversion needed
           if( convert(fileNo) ) then
             colOffset = (XORIG - XORIG3D) / XCELL3D
             rowOffset = (YORIG - YORIG3D) / YCELL3D
             start = (/ 1+colOffset, 1+rowOffset,    1,  step /)
             count = (/     NCOLS,     NROWS,    1,     1 /)
           else
             start = (/       1,       1,    1,  step /)
             count = (/ NCOLS3D, NROWS3D,    1,     1 /)
             endif

           !! determine which layer(s) to read
           if(ilayer.gt.0) then
             start(3) = ilayer
             count(3) = 1
           else
             start(3) = 1
             count(3) = NLAYS
             endif

           !! read variable
           status = NF_GET_VARA_REAL( WRFid(fileNo), varid, start, count, specValue)
           if( status.ne. 0 ) then
             write(*,'(''**ERROR** Reading species ['',a,'']'')') TRIM(specName)
             return
             endif
          
           return
           endif  !! 3-D read

         Return

         END SUBROUTINE Rd_wrf   



     
C******************************************************************************
C set the WRF file variables defining map projection, grid, and time parameters
C******************************************************************************
         Logical Function WRF_DESC(fileNo) result( rstatus )

         USE M3UTILIO

         IMPLICIT NONE

         !! Include netcdf header file
         INCLUDE 'netcdf.inc'

         !! arguments
         Integer fileNo

         !! local variables
         Integer status
         Logical valid
         Integer dimid
         Integer varid
         Integer map_proj
         Integer year, month, day, jday
         Integer hour, minute, second
         Integer jdate1,jtime1,jdate2,jtime2
         Real    tstep
         Real    cen_lat, cen_lon
         Real    truelat1, truelat2, stand_lon, moad_cen_lat
         Real    dx, dy
         Real    x, y
         Real    Xoffset, Yoffset
         Real    xtemp, ytemp
         Integer start(4), count(4)
         Integer nvars, n, i
         Integer dimids( NF_MAX_VAR_DIMS )
         Integer NCHARDATE
         Character*64 name
         Character*24 dateStr
         Integer xtype, ndims, natts
         Logical match
         Integer ncdif, nrdif, mycelloff
         Real    col_cent_cmaq, row_cent_cmaq, diflat, diflon
         Real, Parameter :: centol = 0.0001
         Real*8  xe_cent_cmaq, yn_cent_cmaq
         Real    cen_lat_cmaq, cen_lon_cmaq

         Character*16 :: reqDimNa3D(4)=(/'west_east  ', 
     &                                   'south_north',
     &                                   'bottom_top ',
     &                                   'Time       ' /)
         Integer      :: reqDimid3D(4)
         Character*16 :: reqDimNa2D(3)=(/'west_east  ', 
     &                                   'south_north',
     &                                   'Time       ' /)
         Integer      :: reqDimid2D(3)
         
         character*19, allocatable :: timestamp(:)

         !! Check for very fine grid spacing. Tolerance and testing is
         !! not set up to trap error conditions as grid spacing becomes
         !! very small.

         IF ( xcell3d <= 444.0 .OR. ycell3d <= 444.0 ) THEN
                 WRITE(*,'(''**ERROR** Error traps not tested for '',
     &                     '' domains with smaller DX than 444-km'')')
           rstatus = .false.
           RETURN
         ENDIF


         !! set FTYPE3D to gridded
         FTYPE3D = 1

         status = NF_GET_ATT_TEXT( WRFid(fileNo), NF_GLOBAL, 'TITLE', EXECN3D )
         if( status.ne. 0 .or. ICHAR(EXECN3D(1:1)).eq.0 ) then
           EXECN3D = 'WRF V3.3'
           endif

         !! set NCOLS3D, NROWS3D, and NLAYS3D variables
         status =          NF_INQ_DIMID( WRFid(fileNo), 'west_east', dimid )
         status = status + NF_INQ_DIMLEN( WRFid(fileNo), dimid, NCOLS3D )

         status = status + NF_INQ_DIMID( WRFid(fileNo), 'south_north', dimid )
         status = status + NF_INQ_DIMLEN( WRFid(fileNo), dimid, NROWS3D )

         status = status + NF_INQ_DIMID( WRFid(fileNo), 'bottom_top', dimid )
         status = status + NF_INQ_DIMLEN( WRFid(fileNo), dimid, NLAYS3D )

         status = status + NF_INQ_DIMID( WRFid(fileNo), 'Time', dimid )
         status = status + NF_INQ_DIMLEN( WRFid(fileNo), dimid, MXREC3D )


         status = status + NF_INQ_DIMID( WRFid(fileNo), 'DateStrLen', dimid )
         status = status + NF_INQ_DIMLEN( WRFid(fileNo), dimid, NCHARDATE )

         if( status.ne. 0 ) then
           write(*,'(''**ERROR** Reading required WRF dimensions'')')
           rstatus = .false.
           return
           endif

         !! set map projection variables
         status =          NF_GET_ATT_INT( WRFid(fileNo), NF_GLOBAL, 'MAP_PROJ', map_proj)
         status = status + NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'TRUELAT1', truelat1)
         status = status + NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'TRUELAT2', truelat2)
         status = status + NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'STAND_LON', stand_lon)
         status = status + NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'MOAD_CEN_LAT', moad_cen_lat)
         status = status + NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'CEN_LAT', cen_lat)
         status = status + NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'CEN_LON', cen_lon)
         if( status.ne. 0 ) then
           write(*,'(''**ERROR** Reading required map projection variables'')')
           rstatus = .false.
           return
           endif
         
         !! set horizontal grid variables
         NTHIK3D = 1
         write(GDNAM3D, '(''WRF_'',i3.3,''X'',I3.3)') NCOLS3D, NROWS3D

         status =          NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'DX', dx)
         status = status + NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'DY', dy)
         if( status.ne. 0 ) then
           write(*,'(''**ERROR** Reading required map projection variables'')')
           rstatus = .false.
           return
           endif

         if( map_proj .eq. 1 ) then   !! Lambert
 
           GDTYP3D = 2   !! lambert
           P_ALP3D = DBLE(min(truelat1,truelat2))
           P_BET3D = DBLE(max(truelat1,truelat2))
           P_GAM3D = DBLE(stand_lon)
           XCENT3D = DBLE(stand_lon)   
           YCENT3D = YCENT  ! assign reference latitude from first IOAPI file
           
           XCELL3D = DBLE(dx)
           YCELL3D = DBLE(dy)

         else if( map_proj .eq. 2 ) then   !! Polste
 
           GDTYP3D = 6   !! polste
           P_ALP3D = SIGN(1.0, cen_lat)
           P_BET3D = truelat1
           P_GAM3D = stand_lon
           XCENT3D = stand_lon    

           YCENT3D = DBLE( moad_cen_lat)
           
           XCELL3D = DBLE(dx)
           YCELL3D = DBLE(dy)

         else
           write(*,*) map_proj
           write(*,'(''**ERROR** Unsupported map projection type for wrfout'')')
           rstatus = .false.
           return
         endif


         ! Find latitude and longitude coordinates of center of the
         ! CMAQ domain. Use these for comparison against the WRF
         ! domain. It is expected that these coordinates are the same
         ! as in the WRF domain because it is expected that the CMAQ
         ! domain is a subset of the WRF domain with a symmetrical
         ! perimeter removed. The latitude and longitude of the center
         ! of the CMAQ domain are in CEN_LAT_CMAQ and CEN_LON_CMAQ.

         col_cent_cmaq = FLOAT(ncols+1) * 0.5
         row_cent_cmaq = FLOAT(nrows+1) * 0.5

         xe_cent_cmaq = xorig + DBLE(col_cent_cmaq - 0.5) * xcell
         yn_cent_cmaq = yorig + DBLE(row_cent_cmaq - 0.5) * ycell

         Call SetMapProj(GDTYP3D, real(P_ALP3D), real(P_BET3D), 
     &                   real(P_GAM3D), real(XCENT3D), real(YCENT3D))

         Call ToLL(GDTYP3D, real(xe_cent_cmaq), real(yn_cent_cmaq),
     &             cen_lon_cmaq, cen_lat_cmaq)


         diflat = ABS(cen_lat - cen_lat_cmaq)
         diflon = ABS(cen_lon - cen_lon_cmaq)

         ! For polar stereographic grids that are centered at a pole,
         ! the center longitude is irrelevant for this comparison.

         IF ( ( gdtyp3d == 6 ) .AND. 
     &        ( ( diflat == 0 ) .AND. ( ABS(NINT(cen_lat)) == 90 ) ) ) THEN
            diflon = 0.0
         ENDIF


         ! Assume that the only time we are coming into this section
         ! of code is for WRF files generated by the two-way model
         ! when MCIP files are not handy for processing by Combine. In
         ! that case, the "trim" perimeter for WRF should be
         ! symmetrical.

         ncdif = ncols3d - ncols
         nrdif = nrows3d - nrows
         IF ( ( ncdif == nrdif  ) .AND. ( MOD (nrdif, 2) == 0 ) .AND.
     &        ( nrdif  >  0     ) .AND. 
     &        ( diflat < centol ) .AND. ( diflon < centol ) ) THEN
                 mycelloff = nrdif / 2
         ELSE
                 write(*,'(''**ERROR** Expecting centered WRF and ''
     &                 ''CMAQ domains'')')
                 rstatus = .false.
                 return
         ENDIF

         xorig3d = xorig - mycelloff * xcell3d
         yorig3d = yorig - mycelloff * ycell3d



         !! set vertical grid variables
         VGTYP3D = 7

         !! read V_TOP variable for VGTOP3D
         start = (/ 1, 0, 0, 0 /)
         count = (/ 1, 0, 0, 0 /)
         status =          NF_INQ_VARID( WRFid(fileNo), 'P_TOP', varid)
         status = status + NF_GET_VARA_REAL( WRFid(fileNo), varid, start, count, VGTOP3D)
         if( status.ne. 0 ) then
           write(*,'(''**ERROR** Reading VGTOP3D value from P_TOP variable'')')
           rstatus = .false.
           return
           endif


         !! read ZNW values for VGLVS3D values
         start = (/ 1, 1, 1, 1 /)
         count = (/ NLAYS3D+1, 1, 0, 0 /) 
         status =          NF_INQ_VARID( WRFid(fileNo), 'ZNW', varid)
         status = status + NF_GET_VARA_REAL( WRFid(fileNo), varid, start, count, VGLVS3D)
         if( status.ne. 0 ) then
           write(*,'(''**ERROR** Reading vertical sigma values from ZNW variable'')')
           rstatus = .false.
           return
           endif

         !! set time and date variables
         status = NF_GET_ATT_TEXT( WRFid(fileNo), NF_GLOBAL, 'START_DATE', dateStr )
         status = status + NF_GET_ATT_REAL( WRFid(fileNo), NF_GLOBAL, 'DT', tstep)
         if( status.ne. 0 ) then
           write(*,'(''**ERROR** Reading required map projection variables'')')
           rstatus = .false.
           return
           endif
       
         read(dateStr,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)',iostat=status) year,month,day,hour,minute,second
         if( status.ne.0 ) then
           write(*,'(''**ERROR** Reading starting date string:'',a)') TRIM(dateStr)
           rstatus = .false.
           return
           endif
         
         if (MXREC3D.eq.1)  then !only one time step, set TSTEP3D to a nominal value of 10000

          TSTEP3D = 10000
          jday = JULIAN(year, month, day)
          SDATE3D = 1000*year + jday
          STIME3D = 10000*hour + 100*minute + second
          
         else !determine TSTEP3D from the first two values of the "Times" character variable


          status =          NF_INQ_VARID( WRFid(fileNo), 'Times', varid)
          if( status.ne. 0 ) then
           write(*,'(a)') '**ERROR** Reading variable Times'
           rstatus = .false.
           return           
          endif

          start = (/         1,       1,    0, 0 /)
          count = (/ NCHARDATE, MXREC3D,    0, 0 /)

          allocate(timestamp(MXREC3D))
     
          status = NF_GET_VARA_TEXT( WRFid(fileNo), varid, start, count, timestamp)
          if( status.ne. 0 ) then
           write(*,'(a)') '**ERROR** Reading variable Times'
           deallocate(timestamp)
           rstatus = .false.
           return           
          endif

          read(timestamp(1),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)',iostat=status) year,month,day,hour,minute,second
          if( status.ne.0 ) then
           write(*,'(''**ERROR** Reading first timestamp string:'',a)') TRIM(timestamp(1))
           rstatus = .false.
           return
          endif

          jday = JULIAN(year, month, day)
          jdate1 = 1000*year + jday
          jtime1 = 10000*hour + 100*minute + second

          SDATE3D = jdate1 !use the first time step in the file, rather than the simulation start stored in START_DATE
          STIME3D = jtime1

          read(timestamp(2),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)',iostat=status) year,month,day,hour,minute,second
          if( status.ne.0 ) then
           write(*,'(''**ERROR** Reading second timestamp string:'',a)') TRIM(timestamp(2))
           rstatus = .false.
           return
          endif

          jday = JULIAN(year, month, day)
          jdate2 = 1000*year + jday
          jtime2 = 10000*hour + 100*minute + second
          
          TSTEP3D = SEC2TIME( INT(SECSDIFF(jdate1,jtime1,jdate2,jtime2)) )
          
          deallocate(timestamp)
          
         endif

         !! set array of variables
         NVARS3D = 0
         VNAME3D = ' '
         UNITS3D = ' '
         VDESC3D = ' '

         ! build array of required dimensions for variable match
         do n = 1, SIZE(reqDimNa3D)
           status = status + NF_INQ_DIMID( WRFid(fileNo), reqDimNa3D(n), reqDimid3D(n) )
           enddo
         do n = 1, SIZE(reqDimNa2D)
           status = status + NF_INQ_DIMID( WRFid(fileNo), reqDimNa2D(n), reqDimid2D(n) )
           enddo

         if( status.ne. 0 ) then
           write(*,'(''**ERROR** Reading WRF required dimensions'')')
           rstatus = .false.
           return
           endif


         ! get number of variables in WRF file
         status = NF_INQ_NVARS( WRFid(fileNo), nvars )

         !! loop thru variables and record variables with dimensions
         !!  (Time, bottom_top, south_north, west_east)
         do n = 1, nvars
           status = NF_INQ_VAR( WRFid(fileNo), n, name, xtype, ndims, dimids, natts )

           match = .false.

           !! check 3D variables for match
           if( ndims.eq.SIZE(reqDimid3D) ) then 
             match = .true.
             do i = 1, SIZE(reqDimid3D)
               if( reqDimId3D(i) .ne. dimids(i) ) match = .false.
               enddo
             endif

           !! check 2D variables for match
           if( ndims.eq.SIZE(reqDimid2D) ) then 
             match = .true.
             do i = 1, SIZE(reqDimid2D)
               if( reqDimId2D(i) .ne. dimids(i) ) match = .false.
               enddo
             endif

           !! if match add variable to list
           if( match ) then
             NVARS3D = NVARS3D + 1
             if( NVARS3D .gt. MXVARS3 ) then
               write(*,'(''**ERROR** Number of variables exceed maximum'')')
               rstatus = .false.
               return
               endif

             VNAME3D( NVARS3D ) = name
             VTYPE3D( NVARS3D ) = M3REAL
             status = status + NF_GET_ATT_TEXT( WRFid(fileNo), n, 'units', UNITS3D(NVARS3D) )
             status = status + NF_GET_ATT_TEXT( WRFid(fileNo), n, 'description', VDESC3D(NVARS3D) )

             !! replace null with spaces
             if( ICHAR(UNITS3D(NVARS3D)(1:1)) .eq. 0 ) UNITS3D(NVARS3D) = ' '
             if( ICHAR(VDESC3D(NVARS3D)(1:1)) .eq. 0 ) VDESC3D(NVARS3D) = ' '

             endif

           if( status.ne. 0 ) then
             write(*,'(''**ERROR** Reading WRF variable units and descriptions'')')
             rstatus = .false.
             return
             endif

           enddo  ! variables loop

         rstatus = .true.  
         
         return
         end Function WRF_DESC


 

      !****************************************************************************
      !  routine to set map projection
      !****************************************************************************
      Subroutine SetMapProj(gdtype, alpha, beta, gamma, xcent, ycent)
      
      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real alpha, beta, gamma, xcent, ycent


      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if( .NOT. SETLAM( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('combine', 0, 0, 'Lambert projection setup error', xstat2)
          endif
        return
        endif

      !  check for polste projection
      if( gdtype .eq. 6 ) then
        if( .NOT. SETPOL( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('combine', 0, 0, 'polar stereographic projection setup error', xstat2)
          endif
        return
        endif

      !  check for equatorial secant mercator projection
      if( gdtype .eq. 7 ) then
        if( .NOT. SETEQM( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('combine', 0, 0, 'equatorial mercator projection setup error', xstat2)
          endif
        return
        endif

      Call m3exit ('combine', 0, 0, 'Map projection setup error', xstat2)

      end Subroutine SetMapProj


      !****************************************************************************
      !  routine to compute map projection from LAT/LON
      !****************************************************************************
      Subroutine ToProj(gdtype, longitude, latitude, x, y)

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real longitude, latitude, x, y

      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        x = longitude
        y = latitude
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if(.NOT.LL2LAM(longitude, latitude, x, y) ) then
          Call m3exit('combine', 0, 0, 'Lat/Lon to Lambert error', xstat2)
          endif
        return
        endif

      !  check for polste projection
      if( gdtype .eq. 6 ) then
        if(.NOT.LL2POL(longitude, latitude, x, y) ) then
          Call m3exit('combine', 0, 0, 'Lat/Lon to polar stereographic error', xstat2)
          endif
        return
        endif

      !  check for equatorial mercator projection
      if( gdtype .eq. 7 ) then
        if(.NOT.LL2EQM(longitude, latitude, x, y) ) then
          Call m3exit('combine', 0, 0, 'Lat/Lon to equatorial mercator error', xstat2)
          endif
        return
        endif

      Call m3exit ('combine', 0, 0, 'Map projection setup error', xstat2)

      end Subroutine ToProj



      !****************************************************************************
      !  routine to compute LAT/LON from map projection
      !****************************************************************************
      Subroutine ToLL(gdtype, x, y, longitude, latitude)

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real longitude, latitude, x, y

      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        longitude = x
        latitude = y
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if(.NOT.LAM2LL(x, y, longitude, latitude) ) then
          Call m3exit('combine', 0, 0, 'Lat/Lon to Lambert error', xstat2)
          endif
        return
        endif

      !  check for polste projection
      if( gdtype .eq. 6 ) then
        if(.NOT.POL2LL(x, y, longitude, latitude) ) then
          Call m3exit('combine', 0, 0, 'Lat/Lon to polar stereographic error', xstat2)
          endif
        return
        endif

      !  check for equatorial mercator projection
      if( gdtype .eq. 7 ) then
        if(.NOT.EQM2LL(x, y, longitude, latitude) ) then
          Call m3exit('combine', 0, 0, 'Lat/Lon to equatorial mercator error', xstat2)
          endif
        return
        endif

      Call m3exit ('combine', 0, 0, 'Map projection setup error', xstat2)

      end Subroutine ToLL

      END MODULE M3FILES
