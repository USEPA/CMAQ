
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

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/yoj/arc/CCTM/src/phot/phot_inline/o3totcol.f,v 1.2 2011/10/21 16:11:28 yoj Exp $ 

      subroutine viz_o3totcol ( jdate )

!----------------------------------------------------------------------
! Function:
!    This subroutine returns total column ozone [DU] for any lat, lon, and date,
!    by interpolating spacially and temporally between OMI/TOMS data in file
!    OMI.  The interpolation of ozone between lat, lon, and time is linear.
!    If missing data are encourntered (designated as -1.0 in OMI), the 
!    interpolation proceeds with available contiguous and conterminous data.
!
! Revision history:
!     Aug 11 J.Streicher: initial version
!     Dec 2013 S.Roselle: time-records adjusted to input file provided;
!                         improved logfile reporting
!     Jun 2015 J.Young: maintain code stnds
!----------------------------------------------------------------------

      USE m3utilio

      implicit none

! arguments

      integer, intent( in ) :: jdate      ! Julian day of the year (yyyyddd)


! parameters

      integer, parameter :: nlat = 17 ! or 19
      integer, parameter :: nlon = 17
!     integer, parameter :: nt   = 570 ! or 1000
      integer, save :: nt

! local variables

      character( 16 ), save :: tmfile   = 'OMI_current.dat'
      character( 16 ), save :: ncf_file = 'omi_14t16_v5.ncf'
      character( 16 ), save :: pname    = 'O3TOTCOL'

      character( 96 ) :: xmsg = ' '
      character( 96 ) :: xmsgs( 3 )

      integer :: allocstat
      integer :: ilat
      integer :: ilon
      integer :: i
      integer :: icount
      integer :: ios
      integer :: nrecs
      integer :: jyear
      integer :: days_per_year

      integer, save :: jdate_prev = 0
      integer, save :: jtdate_temp
      integer, save :: tmunit    = -1
      integer, save :: logdev           ! output log unit number
      integer, save :: it

      integer, save :: st_year       ! initial year of interpolated date
      integer, save :: end_year      ! final year of interpolated date
      integer, save :: jstdate       ! initial date of interpolated date YYYYDDD
      integer, save :: jenddate      ! final date of interpolated date  YYYYDDD
      integer, save :: days_st_year  ! number of days in initial year
      integer, save :: days_end_year ! number of days in final year

      real :: latitude ( nlat )   ! viz file latitude point
      real :: longitude( nlon ) ! viz file longitude point
      real :: flag( 8 )
      real :: x2
      real :: x3
      real :: np_oz
      real :: sp_oz
      real :: total
      real :: latitudem
      real :: tdate_temp, tdate
 
      real, save :: stdate   ! initial date of interpolated date dicemal
      real, save :: enddate  ! final date of interpolated date  dicemal
      real, save :: x1
      real, save :: lat( nlat )
      real, save :: lon( nlon )
      real, save :: oz( nlat, nlon, 2 )   ! two timesteps for interpolation
      real, save :: ozone( nlon-1, nlat ) ! total column ozone [DU] for viz file

      real, allocatable, save :: t( : )

      logical   :: Leap_Year
      logical   :: EXISTS
 
      logical, save :: firsttime = .true.

      interface
        SUBROUTINE CREATE_CMAQ_OMI ( FILE_NAME, JDATE, LAT, LON )
          CHARACTER( 16 ), INTENT( IN ) :: FILE_NAME  ! name of file 
          INTEGER,         INTENT( IN ) :: JDATE      ! Start date of file, YYYYDDD
          REAL,            INTENT( IN ) :: LAT( : )   ! center latitudes of file cells
          REAL,            INTENT( IN ) :: LON( : )   ! center longtudes of file cells
        END SUBROUTINE CREATE_CMAQ_OMI
      end interface

!----------------------------------------------------------------------

      if ( firsttime ) then
      
        firsttime = .false.
        logdev = 6 ! init3()
        
! Assign values to array of longitudes: lon

        do ilon = 1, nlon
          lon( ilon ) = -180.0 + 22.5 * real( ilon - 1 )
        end do

        x2 = 360.0 / real( nlon - 1 )
        do ilon = 1, nlon
          longitude( ilon ) = -180.0 + x2 * real( ilon-1 )
        end do

        x2 = 160.0 / real( nlat - 1 )
        do ilat = 1, nlat
          latitude( ilat ) =   90.0 - x2 * real( ilat - 1  ) - 10.0
        end do

! create ioapi for visualization
        call CREATE_CMAQ_OMI ( ncf_file, jdate, latitude, longitude )


        inquire( file = tmfile, EXIST = EXISTS )
        IF( EXISTS )THEN
           open( file = tmfile, status = 'unknown', iostat = ios, newunit = tmunit )
        else
          xmsg = trim( pname ) // ' Cannot find ' // tmfile
     &        // ' at JDATE = '
          write(logdev,'(a,i8)') xmsg, jdate
          stop
        end if

        if ( ios .gt. 0 ) then
          xmsg = trim( pname ) // ' Error opening ' // tmfile
     &        // ' at JDATE = '
          write(logdev,'(a,i8)') xmsg, jdate
          stop
        end if

        nrecs = 0
        read( tmunit, * ) ! skip header record
        do
          read( tmunit, *, iostat=ios )
          if ( ios .ne. 0 ) exit
          nrecs = nrecs + 1
        end do
        if ( nrecs .gt. 0 ) nt = nrecs / nlat

        allocate ( t( nt ), stat = allocstat )
        if ( allocstat .ne. 0 ) then
          xmsg = trim( pname ) 
     &        // 'Failure allocating t array at JDATE = '
          write(logdev,'(a,i8)') xmsg, jdate
          stop
        end if

        rewind( tmunit )
        read( tmunit, * )

        t   = 0.0
        lat = 0.0

! When adding x lines of data to OMI.dat, increase upper limit by x
! Note: ilat(1) => 80N = + 80 deg ; ilat(nlat) => 80S = -80 deg
! Note: ilon(1) = international dateline = ilon(nlon); ilon(2)=> 157.5W ; ilon(9)= ZULU

! Read in array of dates (format: yyyy.yyy)
        do it = 1, nt
          do ilat = 1, nlat
            read( tmunit,* ) t( it ), lat( ilat )
            latitude( nlat - ilat + 1 ) = lat( ilat )
          end do
        end do

! get initial and final dates of data and convert to YYYYDDD format
        stdate  = minval( t )
        st_year = int( stdate ) 
        Leap_Year = ( ( mod(st_year,4) .eq. 0 )
     &              .And. ( mod(st_year,100) .ne. 0 )
     &                 .Or. ( mod(st_year,400) .eq. 0  ))
        if( Leap_Year )then
           days_st_year = 366.0
        else
           days_st_year = 365.0
        end if
        jstdate = st_year * 1000
     &          + int( days_st_year * amod(stdate, 1.0))

        enddate = maxval( t )
        end_year = int( enddate ) 
        Leap_Year = ( ( mod(end_year,4) .eq. 0 )
     &              .And. ( mod(end_year,100) .ne. 0 )
     &                 .Or. ( mod(end_year,400) .eq. 0  ))
        if( Leap_Year )then
            days_end_year = 366.0
        else
            days_end_year = 365.0
        end if
        jenddate = end_year * 1000
     &           + int( days_end_year * amod(enddate, 1.0))

        print*,'jstdate, jstdate = ',jstdate, jenddate 


      end if ! firsttime

      if ( jdate .ne. jdate_prev ) then

! Initialize

        jdate_prev = jdate
        oz  = 0.0

!        rewind( tmunit )
!        read( tmunit, * )

! When adding x lines of data to OMI.dat, increase upper limit by x
! Note: ilat(1) => 80N = + 80 deg ; ilat(nlat) => 80S = -80 deg
! Note: ilon(1) = international dateline = ilon(nlon); ilon(2)=> 157.5W ; ilon(9)= ZULU

! Read in array of dates (format: yyyy.yyy)

!        do it = 1, nt
!          do ilat = 1, nlat
!            read( tmunit,* ) t( it ), lat( ilat )
!            latitude( nlat - ilat + 1 ) = lat( ilat )
!          end do
!       end do


! Use a temporary dummy variable jdate_temp so as not to overwrite jdate

        jyear = jdate / 1000
        Leap_Year = ( ( mod(jyear,4) .eq. 0 )
     &              .And. ( mod(jyear,100) .ne. 0 )
     &                 .Or. ( mod(jyear,400) .eq. 0  ))
        if( Leap_Year )then
            days_per_year = 366.0
        else
            days_per_year = 365.0
        end if
 
        tdate = real( jyear ) + real( jdate - jyear * 1000 ) / days_per_year
        tdate_temp = tdate

!        print*,'tdate = ', tdate
! Determine if the ozone database includes the requested jdate

! Submitted date is outside of ozone database range.
!     Total column ozone will be estimated from the corresponding Julian Day
!     of the prior year

        if ( jdate .ge. jenddate ) then
          
          tdate_temp = real( end_year ) + ( tdate - real( int( tdate ) ) )
          if ( tdate_temp .gt. enddate ) then
            tdate_temp = tdate_temp - 1.0
          end if
          jtdate_temp = int( tdate_temp ) * 1000
     &                + nint( days_end_year * amod( tdate_temp, 1.0 ) )
          write(xmsg,'(a,i8,a,i8)') trim( pname)
     &    // ' WARNING:Requested date, ',jdate,', is beyond available data on OMI file:  <', 
     &        jenddate
          write(logdev,'(a)')xmsg

          xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day '
          write(xmsgs( 2 ),'(a,i8)')'of the last available year on the '
     &    // 'OMI input file:', jtdate_temp
          xmsgs( 3 ) = ' '
          do i = 1, 3
           write(logdev,'(a)')xmsgs(i)
          end do
! Submitted date is outside of ozone database range.
!     Total column ozone will be estimated from the corresponding Julian Day of
!     the subsequent year

        else if ( jdate .le. jstdate ) then
          tdate_temp = real( st_year ) + ( tdate - real( int( tdate ) ) )
          if ( tdate_temp .lt. stdate ) then
            tdate_temp = tdate_temp + 1.0
          end if
          jtdate_temp = int( tdate_temp ) * 1000
     &                + nint( days_end_year * amod( tdate_temp, 1.0 ) )

          write(xmsg,'(a,i8,a,i8)') trim( pname)
     &    // ' WARNING:Requested date, ',jdate,', preceeds available data on OMI file:  >',
     &        jstdate
          write(logdev,'(a)')xmsg
          xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day'
          write(xmsgs( 2 ),'(a,i8)')'of the next available year on the OMI input file:',
     &                                jtdate_temp
          xmsgs( 3 ) = ' '
          do i = 1, 3
           write(logdev,'(a)')xmsgs(i)
          end do
! Submitted date falls within the satellite data measurement gap beginning
!     on 24 Nov 1994 and ending on 22 Jul 1996.

        else if ( ( tdate .ge. 1994.899 ) .and. ( tdate .le. 1996.557 ) ) then
        
          if ( tdate .le. 1995.738 ) then
            tdate_temp = tdate - 1.0  ! use previous year
          else
            tdate_temp = tdate + 1.0  ! use subsequent year
          end if
          jyear = tdate_temp / 1000
          if( jyear .eq. 1996 )then
            days_per_year = 366.0
          else
            days_per_year = 365.0
          end if
          jtdate_temp = jyear * 1000
     &                + nint( days_per_year * amod( tdate_temp, 1.0 ) )
          write(xmsg,'(a,i8,a,i8)') trim( pname)
     &    // ' WARNING:Requested date, ',jdate,',falls within satellite data'
     &    // ' measurement gap: 24 Nov 1994 - 22 Jul 1996'
          write(logdev,'(a)')xmsg
          xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day'
          write(xmsgs( 2 ),'(a,i8)')'of the closest available year on the OMI input file:',
     &                               jtdate_temp
          xmsgs( 3 ) = ' '
          do i = 1, 3
           write(logdev,'(a)')xmsgs(i)
          end do
        else
          write(xmsgs( 1 ),'(a,i8)')'Total column ozone will be interpolated to day ',
     &                  jdate
          xmsgs( 2 ) = 'from data available on the OMI input file'
          xmsgs( 3 ) = ' '
          do i = 1, 3
           write(logdev,'(a)')xmsgs(i)
          end do
        end if

!        stop

! When adding x lines of data to OMI.dat, increase upper limit by x
! and increase the dimension of t as needed

! Determine the specific dates in the database that bound the requested jdate
! i.e.  (it) < (jdate_temp) < (it+1)
! where it is the index var for the database
! and determine the interpolation factor ?x1? between the bounding dates

        x1 = 0.0
        x1loop: do it = 1, nt-1
          if ( ( tdate_temp .ge. t( it ) ) .and. 
     &         ( tdate_temp .le. t( it+1 ) ) ) then
            x1 = ( tdate_temp - t( it ) ) / ( t( it+1) - t( it ) )
            exit x1loop
          end if
        end do x1loop


! Determine the corresponding bounding ozone values for all lats and lons

        rewind( tmunit )
        read( tmunit,* )
   
        do i = 1, it-1
          do ilat = 1, nlat
            read( tmunit,* )
          end do
        end do
   
        do ilat = 1, nlat
          read( tmunit,* ) t( it ), lat( ilat ), ( oz( ilat, ilon, 1 ), ilon=1,nlon-1 )
          oz( ilat, nlon, 1 ) = oz( ilat, 1, 1 )
        end do
   
        do ilat = 1, nlat
          read( tmunit,* ) t( it+1 ), lat( ilat ), ( oz( ilat, ilon, 2 ), ilon=1,nlon-1 )
          oz( ilat, nlon, 2 ) = oz( ilat, 1, 2 )
        end do
   
      end if   ! jdate .ne. jdate_prev

      ozone = -1.0
      latitudem = 0.0
      flag = 1.0
      x2   = 0.0
      x3   = 0.0

!     for not set x1 = 0.0 to show individual time steps of data
      x1 = 0.0
    
      x2loop: do ilat = 1, nlat
         x3loop: do ilon = 1, nlon-1

!             flag( 1:8 ) = 0.0
!             if ( oz( ilat, ilon, 2 ) .le. 0.0 ) then
!                oz( ilat, ilon, 2 ) = 0.0
!                flag( 2 ) = 0.0
!             else
!                flag( 2 ) = x1
!             end if

!             if ( oz( ilat, ilon, 1 ) .le. 0.0 ) then
!                  oz( ilat, ilon, 1 ) = 0.0
!                  flag( 6 ) = 0.0
!             else
!                  flag( 6 ) = ( 1.0 - x1 ) 
!             end if

!             total = sum( flag )
!             if ( total .le. 0.0 )cycle


! No,simple linear interpolation of input data to j tdate_temp
             ozone(ilon,nlat-ilat+1) = (     x1 ) * oz( ilat  , ilon  , 2 )
     &                               + ( 1.0-x1 ) * oz( ilat  , ilon  , 1 )


!             ozone(ilon,nlat-ilat+1) = ozone(ilon,nlat - ilat + 1 )
!     &                               / total

         end do x3loop
      end do x2loop

      IF ( .not. write3( ncf_file, 'OZONE_COLUMN', jdate, 0,
     &                     ozone ) ) THEN
             xmsg = 'Error writing variable OZONE_COLUMN'
             call m3exit ( pname, jdate, 0, xmsg, xstat1 )
      END IF

      return

      end subroutine viz_o3totcol
