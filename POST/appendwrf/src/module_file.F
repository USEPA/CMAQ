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

C***********************************************************************
C
C  MODULE:  sets up file data
C             
C***********************************************************************
      MODULE WRF_FILES

      Integer N_FILES                     ! No. of WRF input files

      Integer, Parameter :: MXFILES = 15  ! Max no. of input

      Character*256 FILENAME( MXFILES )   ! filenames

      Integer NCID( MXFILES )             ! NETCDF IDs

      Integer, Allocatable :: TINDEX(:,:) ! indexes to output time step  

      Integer STARTDATE                   ! Starting date of data set (YYYYJJJ)
      Integer STARTTIME                   ! Starting time of data set (hhmmss)
      Integer TSTEP                       ! time step in seconds 

      CONTAINS

C***********************************************************************
C   open input WRF files
C***********************************************************************
         SUBROUTINE OPEN_FILES

         IMPLICIT NONE

         ! Include netcdf header file
         Include 'netcdf.inc'

         ! functions
         Integer SECSDIFF

         ! local variables
         Character*(10)   fname
         Integer  status
         Integer  n
         Character*(64) name
         Integer  unlimdimid
         Integer  timelen
         Integer  maxtimelen
         Integer  hour, minute

         fname = 'INFILE_1'
         N_FILES = 1

         ! get name of first input file
         Call GETENV(fname, filename(1))
         if( filename(1).eq.' ' ) Call abort('System variable INFILE_1 not defined')

         ! open netCdf input file
         status = NF_OPEN(filename(1), 0, NCID(1))
         if( status.ne.0 ) Call abort('Cannot open input file:' // TRIM(filename(1)))
         Write(*,'(a,'' ['',a,'']  opened'')') TRIM(fname),TRIM(filename(1))

         ! try to open files (2-MXFILES) 
         do n = 2, MXFILES
           if( n.le.9 ) write(fname, '( ''INFILE_'', I1 )' ) n
           if( n.ge.10) write(fname, '( ''INFILE_'', I2 )' ) n

           Call GETENV(fname,filename(n))
           if( filename(n).eq. ' ' ) EXIT

           ! open netCdf input file n
           status = NF_OPEN(filename(n), 0, NCID(n))
           if( status.ne.0 ) Call abort('Cannot open input file:' // TRIM(filename(n)))
           Write(*,'(a,'' ['',a,'']  opened'')') TRIM(fname),TRIM(filename(n))

           N_FILES = n
           enddo  ! file loop 
      
         ! loop thru each input file and save maximum time length, use this to allocate
         ! TINDEX array
         maxtimelen = 0
         do n = 1, N_FILES

           status = NF_INQ_UNLIMDIM( NCID(n), unlimdimid )
           status = NF_INQ_DIM( NCID(n), unlimdimid, name, timelen )
           if( timelen .gt. maxtimelen ) maxtimelen = timelen
           enddo

         Allocate( TINDEX(N_FILES, maxtimelen) )


         ! read array of Times and compute starting date and time step
         Call getStartDate( NCID(1), STARTDATE, STARTTIME, TSTEP )


         ! build INDEX array
         Call bldIndex()
         
         return

         END SUBROUTINE OPEN_FILES


C***********************************************************************
C   close input WRF files
C***********************************************************************
         SUBROUTINE CLOSE_FILES

         IMPLICIT NONE

         ! Include netcdf header file
         Include 'netcdf.inc'

         ! local variables
         Integer status
         Integer n
    
         do n = 1, N_FILES
           status = NF_CLOSE(NCID(n))
           enddo
 
         return

         END SUBROUTINE CLOSE_FILES


C***********************************************************************
C   copy header from file1 to file2
C***********************************************************************
         SUBROUTINE COPY_HEADER( NCID1, NCID2 )

         IMPLICIT NONE

         ! Include netcdf header file
         Include 'netcdf.inc'

         ! arguments
         Integer NCID1
         Integer NCID2

         ! local variables
         Integer status
         Integer n
         Integer i
         Integer ndims
         Integer unlimdimid
         Integer nvars
         Integer ngatts
         Character*64 name
         Integer length
         Integer dimid
         Integer varid
         Integer natts
         Integer xtype
         Integer dimids( NF_MAX_VAR_DIMS ) 

         ! copy dimensions
         status = NF_INQ_NDIMS( NCID1, ndims )
         status = NF_INQ_UNLIMDIM( NCID1, unlimdimid )
   
         do n = 1, ndims
           status = NF_INQ_DIM( NCID1, n, name, length )
           if( n .eq. unlimdimid ) length = NF_UNLIMITED
           status = NF_DEF_DIM( NCID2, name, length, dimid )
           enddo  ! dimension loop

         ! copy global attributes
         status = NF_INQ_NATTS( NCID1, ngatts )

         do n = 1, ngatts
           status = NF_INQ_ATTNAME( NCID1, NF_GLOBAL, n, name )
           status = NF_COPY_ATT( NCID1, NF_GLOBAL, name, NCID2, NF_GLOBAL )  
           enddo  ! global attributes loop

         ! copy variables
         status = NF_INQ_NVARS( NCID1, nvars )

         do n = 1, nvars 
           status = NF_INQ_VAR( NCID1, n, name, xtype, ndims, dimids, natts )
           status = NF_DEF_VAR( NCID2, name, xtype, ndims, dimids, varid )

           ! copy attributes for variable
           do i = 1, natts
             status = NF_INQ_ATTNAME( NCID1, n, i, name )
             status = NF_COPY_ATT( NCID1, n, name, NCID2, n )  
             enddo  ! variable attributes loop

           enddo  ! variables loop

         ! exit definition mode
         status = NF_ENDDEF( NCID2 )

         return
 
         END SUBROUTINE COPY_HEADER


C***********************************************************************
C   compute date and time from string  (YYYY-MM-DD_hh:mm:ss)
C***********************************************************************
         SUBROUTINE str2date( string, idate, itime )

         IMPLICIT NONE

         ! functions
         Integer JULIAN

         ! arguments
         Character(*) string
         Integer idate
         Integer itime

         ! local variables
         Character(32)  field 
         Character(32)  dfield 
         Character(32)  tfield 

         Integer year, month, day, jday
         Integer hour, minute, second

         !  parse date and time fields from string 
         Call getField( string, '_', 1, dfield )
         Call getField( string, '_', 2, tfield )

         !  parse year, month, and day
         Call getField( dfield, '-', 1, field )
         read(field,'(i4)') year

         Call getField( dfield, '-', 2, field )
         read(field,'(i2)') month

         Call getField( dfield, '-', 3, field )
         read(field,'(i2)') day 

         !  parse hour, minute, and second
         Call getField( tfield, '-', 1, field )
         read(field,'(i2)') hour
         
         Call getField( tfield, '-', 2, field )
         read(field,'(i2)') minute

         Call getField( tfield, '-', 3, field )
         read(field,'(i2)') second

         ! build idate
         jday = JULIAN( year, month, day )
         idate = 1000*year + jday 

         ! build itime
         itime = 10000*hour + 100*minute + second

         return
         END SUBROUTINE str2date


C***********************************************************************
C   routine to get starting date from Times array            
C***********************************************************************
         SUBROUTINE getStartDate( fid, sdate, stime, step )

         IMPLICIT NONE

         ! Include netcdf header file
         Include 'netcdf.inc'

         ! functions
         Integer SECSDIFF

         ! arguments
         Integer fid
         Integer sdate, stime, step

         ! local variables
         Integer status
         Integer date2, time2
         Character*64 vname
         Character*64 dname
         Integer timesId
         Integer xtype
         Integer ndims
         Integer dimids( NF_MAX_VAR_DIMS )
         Integer natts
         Integer length

         Character, Allocatable :: times(:,:)
         Integer start(4)
         Integer count(4)
         Character(100) timestr1, timestr2

         status = NF_INQ_VARID( fid, 'Times', timesId )
         if( status.ne.0 ) Call abort('Cannot find Times variable in file')

         status = NF_INQ_VAR( fid, timesId, vname, xtype, ndims, dimids, natts )
         status = NF_INQ_DIM( fid, dimids(1), dname, length )

         start = 1
         count = 1
         count(1) = length
         count(2) = 2

         Allocate( times( count(1), count(2) ) )

         status = NF_GET_VARA_TEXT( fid, timesId, start, count, times )
         if( status.ne.0 ) Call abort('Cannot read Times variable from file')

         write(timestr1, '(100a)') times(:,1)
         write(timestr2, '(100a)') times(:,2)

         call str2date( timestr1, sDate, sTime )         
         call str2date( timestr2, date2, time2 )           
         step = SECSDIFF( sDate, sTime, date2, time2 )                   

         Deallocate( times )
         return

         End SUBROUTINE getStartDate


C***********************************************************************
C   routine to build index array from starting date and times
C***********************************************************************
         SUBROUTINE bldIndex()

         IMPLICIT NONE

         ! Include netcdf header file
         Include 'netcdf.inc'

         ! functions
         Integer SECSDIFF
         Integer TIME2SEC

         ! local variables
         Integer status
         Integer n, i
         Integer unlimdimid 
         Integer nsteps
         Character*64 name
         Integer sdate, stime 
         Integer step, idx
         Integer secs


         do n = 1, N_FILES

           ! find number of steps in file
           status = NF_INQ_UNLIMDIM( NCID(n), unlimdimid )
           status = NF_INQ_DIM( NCID(n), unlimdimid, name, nsteps )

           call getStartDate( NCID(n), sdate, stime, step )

           if( step .ne. TSTEP ) then
             Write(*,'(''first TSTEP ='',i10)') TSTEP
             Write(*,'(''Time step of file:'',a,'' ='',i10)') TRIM(filename(n)),step

             Call abort('Time steps between files do not match')
             endif

           ! compute index of file's starting time
           secs = SECSDIFF( STARTDATE, STARTTIME, sdate, stime )
           if( secs .lt. 0 ) Call abort('Time steps prior to Start Date')
          
           idx = secs / TSTEP   
           do i = 1, nsteps
             TINDEX( n, i ) = idx + i
            
             enddo   ! index loop 

           enddo   ! file loop

         return
         END SUBROUTINE bldIndex


C***********************************************************************
C   routine to copy variable data to output file
C***********************************************************************
         SUBROUTINE COPY_DATA( NCIDout, v)

         IMPLICIT NONE

         ! Include netcdf header file
         Include 'netcdf.inc'

         ! arguments
         Integer NCIDout
         Integer v

         ! local variables
         Integer status
         Integer f
         Integer xtype, ndims, natts
         Integer varid
         Integer n
         Integer length
         Integer nsteps
         Integer unlimdimid
         Integer dimids( NF_MAX_VAR_DIMS )
         Character*64 vname
         Character*64 dname

         Integer start(4)
         Integer count(4)

         Integer, Allocatable         :: IDATA(:,:,:,:)
         Real, Allocatable            :: RDATA(:,:,:,:)
         Character, Allocatable       :: CDATA(:,:,:,:)

         status = NF_INQ_VAR( NCIDout, v, vname, xtype, ndims, dimids, natts )
         Write(*,'(''Copy variable '',a)') TRIM(vname)

         !  check for unsupported data types
         if( xtype.ne.2 .and. xtype.ne.4 .and. xtype.ne.5 ) then
           Write(*,'(''**ERROR** Variable data type'',i5,'' not supported'')') xtype
           stop
           endif
       
         start = 1
         count = 1

         ! set count to length of each dimension
         do n = 1, ndims-1
           status = NF_INQ_DIM( NCIDout, dimids(n), dname, length )
           count(n) = length
           enddo

         ! allocate array
         if( xtype .eq. 2 ) Allocate( CDATA( count(1), count(2), count(3), count(4) ) )
         if( xtype .eq. 4 ) Allocate( IDATA( count(1), count(2), count(3), count(4) ) )
         if( xtype .eq. 5 ) Allocate( RDATA( count(1), count(2), count(3), count(4) ) )
        
         ! file loop
         do f = 1, N_FILES

	  Write(*,'(''File  '',i0,'' of '',i0)') f,N_FILES
           ! find number of steps in file
           status = NF_INQ_UNLIMDIM( NCID(f), unlimdimid )
           status = NF_INQ_DIM( NCID(f), unlimdimid, dname, nsteps )

           ! get variable id of vname in file f
           status = NF_INQ_VARID( NCID(f), vname, varid )
           if( status .ne. 0 ) then
             write(*,'(''**ERROR** Variable '',a,'' not found in file '',a)') TRIM(vname),
     &           TRIM(filename(f))
             stop
             endif
          
           do n = 1, nsteps

             ! read from record n (NCID(f))
             start(ndims) = n

             if( xtype .eq. 2 ) status = NF_GET_VARA_TEXT( NCID(f), varid, start, count, CDATA )
             if( xtype .eq. 4 ) status = NF_GET_VARA_REAL( NCID(f), varid, start, count, IDATA )
             if( xtype .eq. 5 ) status = NF_GET_VARA_REAL( NCID(f), varid, start, count, RDATA )
             if( status.ne.0 ) then
               write(*,'(//,''**ERROR** Problem reading variable ['',a,''] from file:'',
     &               a,'' at step'',i5)') TRIM(vname), TRIM(filename(f)), n
               Stop
               endif

	     Write(*,'(''time index  '',i0,'' of '',i0,2x,i0)') n,nsteps,TINDEX(f,n)

             ! write to record TINDEX(f,n) (NCIDout)
             start(ndims) = TINDEX(f,n)

             if( xtype .eq. 2 ) status = NF_PUT_VARA_TEXT( NCIDout, v, start, count, CDATA )
             if( xtype .eq. 4 ) status = NF_PUT_VARA_REAL( NCIDout, v, start, count, IDATA )
             if( xtype .eq. 5 ) status = NF_PUT_VARA_REAL( NCIDout, v, start, count, RDATA )
             if( status.ne.0 ) then
               write(*,'(//,''**ERROR** Problem writing variable ['',a,''] at step'',i5)')
     &               TRIM(vname), TINDEX(f,n)
               Stop
               endif

             enddo  ! record loop

           enddo   ! file loop

         ! deallocate array
         if( xtype .eq. 2 ) Deallocate( CDATA )
         if( xtype .eq. 4 ) Deallocate( IDATA )
         if( xtype .eq. 5 ) Deallocate( RDATA )

         return

         END SUBROUTINE COPY_DATA


C***********************************************************************
C   routine to write error message and exit program
C***********************************************************************
         SUBROUTINE ABORT( message )

         IMPLICIT NONE

         ! arguments
         Character(*)   message

         Write(*,'(//''**ERROR** Program abortted'',/,5x,a)') TRIM(message)
         Stop

         End SUBROUTINE ABORT


      END MODULE WRF_FILES
