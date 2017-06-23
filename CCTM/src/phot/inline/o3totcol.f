
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

      subroutine o3totcol ( latitude, longitude, jdate, ozone )

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

      use utilio_defn

      implicit none

! arguments

      integer, intent( in ) :: jdate      ! Julian day of the year (yyyyddd)

      real, intent( in )    :: latitude   ! latitude of point on earth's surface
      real, intent( in )    :: longitude  ! longitude of point on earth's surface
      real, intent( inout ) :: ozone      ! total column ozone [DU]

! parameters

      integer, parameter :: nlat = 17 ! or 19
      integer, parameter :: nlon = 17

! local variables

      character( 16 ), save :: tmfile = 'OMI'
      character( 16 ), save :: pname = 'O3TOTCOL'
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

      integer, save :: logdev           ! output log unit number
      integer, save :: nt
      integer, save :: it
      integer, save :: tmunit
      integer, save :: jdate_prev = 0
      integer, save :: jstdate, jenddate, jtdate_temp

      real :: flag( 8 )
      real :: x2
      real :: x3
      real :: np_oz
      real :: sp_oz
      real :: total
      real :: latitudem
      real :: tdate_temp, tdate

      real, save :: x1
      real, save :: stdate, enddate

      real, allocatable, save :: t( : )
      real, allocatable, save :: lat( : )
      real, allocatable, save :: lon( : )
      real, allocatable, save :: oz( :, :, : ) ! two timesteps for interpolation

      logical, save :: firsttime = .true.

!----------------------------------------------------------------------

      if ( firsttime ) then
      
        firsttime = .false.
        logdev = init3()

        allocate ( lat( nlat ), stat = allocstat )
        if ( allocstat .ne. 0 ) then
          xmsg = 'Failure allocating lat'
          call m3exit ( pname, jdate, 0, xmsg, xstat1 )
        end if

        allocate ( lon( nlon ), stat = allocstat )
        if ( allocstat .ne. 0 ) then
          xmsg = 'Failure allocating lon'
          call m3exit ( pname, jdate, 0, xmsg, xstat1 )
        end if
        
! Assign values to array of longitudes: lon
        x2 = 360.0 / real( nlon - 1 )
        do ilon = 1, nlon
          lon( ilon ) = -180.0 + x2 * real( ilon - 1 )
        end do

        tmunit = getefile( tmfile, .true., .true., pname )

        if ( tmunit .lt. 0 ) then
          xmsg = 'Error opening ' // tmfile
          call m3exit ( pname, jdate, 0, xmsg, xstat1 )
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
          xmsg = 'Failure allocating T'
          call m3exit ( pname, jdate, 0, xmsg, xstat1 )
        end if

        allocate ( oz( nlat, nlon, 2 ), stat = allocstat )
        if ( allocstat .ne. 0 ) then
          xmsg = 'Failure allocating oz '
          call m3exit ( pname, jdate, 0, xmsg, xstat1 )
        end if

        rewind( tmunit )
        read( tmunit, * )

! When adding x lines of data to OMI.dat, increase upper limit by x
! Note: ilat(1) => North to South in degrees
! Note: ilon(1) = International Dateline (ID) = ilon(nlon); ilon(2)=> West of ID 

! Read in array of dates (format: yyyy.yyy)

        do it = 1, nt
          do ilat = 1, nlat
            read( tmunit,* ) t( it ), lat( ilat )
          end do
        end do

        stdate  = minval( t )
        enddate = maxval( t )

      end if ! firsttime

      if ( jdate .ne. jdate_prev ) then
! reset oz and jdate_prev
        jdate_prev = jdate
        oz = 0.0

! Use a temporary dummy variable jdate_temp so as not to overwrite jdate

        jyear = jdate / 1000
        tdate = real( jyear ) + real( jdate - jyear * 1000 ) * yr2day( jyear )
        tdate_temp = tdate

! Determine if the ozone database includes the requested jdate

! Submitted date is outside of ozone database range.
!     Total column ozone will be estimated from the corresponding Julian Day
!     of the prior year

        if ( tdate .ge. enddate ) then
          tdate_temp = aint( enddate ) + ( tdate - aint( tdate ) )
          if ( tdate_temp .gt. enddate ) then
            tdate_temp = tdate_temp - 1.0
          end if
          jenddate = int( enddate ) * 1000
     &             + int( ( 1.0 / yr2day( int( enddate ) ) )
     &                  * ( enddate - aint( enddate ) ) )
          jtdate_temp = int( tdate_temp ) * 1000
     &                + nint( ( 1.0 / yr2day( int( tdate_temp ) ) )
     &                 * ( tdate_temp - aint( tdate_temp ) ) )
          xmsg = 'Requested date is beyond available data on OMI file:  <' 
     &           // dt2str( jenddate, 0 )
          call m3warn ( pname, jdate, 0, xmsg )
          xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day '
          xmsgs( 2 ) = 'of the last available year on the '
     &               // 'OMI input file:' // dt2str( jtdate_temp, 0 ) // '<<---<<'
          xmsgs( 3 ) = ' '
          call m3parag ( 3, xmsgs )

! Submitted date is outside of ozone database range.
!     Total column ozone will be estimated from the corresponding Julian Day of
!     the subsequent year

        else if ( tdate .le. stdate ) then
          tdate_temp = real( int( stdate ) ) + ( tdate - real( int( tdate ) ) )
          if ( tdate_temp .lt. stdate ) then
            tdate_temp = tdate_temp + 1.0
          end if
          jstdate = int( stdate ) * 1000
     &            + int( ( 1.0 / yr2day( int( stdate ) ) )
     &               * ( stdate - aint( stdate ) ) )
          jtdate_temp = int( tdate_temp ) * 1000
     &                + nint( ( 1.0 / yr2day( int( tdate_temp ) ) )
     &                 * ( tdate_temp - aint( tdate_temp ) ) )
          xmsg = 'Requested date preceeds available data on OMI file:  >' 
     &           // dt2str( jstdate, 0 )
          call m3warn ( pname, jdate, 0, xmsg )
          xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day'
          xmsgs( 2 ) = 'of the next available year on the OMI input file:'
     &               // dt2str( jtdate_temp, 0 ) // '<<---<<'
          xmsgs( 3 ) = ' '
          call m3parag ( 3, xmsgs )

! Submitted date falls within the satellite data measurement gap beginning
!     on 24 Nov 1994 and ending on 22 Jul 1996.

        else if ( ( tdate .ge. 1994.899 ) .and. 
     &            ( tdate .le. 1996.557 ) ) then
        
          if ( tdate .le. 1995.738 ) then
            tdate_temp = tdate - 1.0  ! use previous year
          else
            tdate_temp = tdate + 1.0  ! use subsequent year
          end if
          jtdate_temp = int( tdate_temp ) * 1000
     &                + nint( ( 1.0 / yr2day( int( tdate_temp ) ) )
     &                 * ( tdate_temp - aint( tdate_temp ) ) )
          xmsg = 'Requested date falls within satellite data'
     &           // ' measurement gap: 24 Nov 1994 - 22 Jul 1996'
          call m3warn ( pname, jdate, 0, xmsg )
          xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day'
          xmsgs( 2 ) = 'of the closest available year on the OMI input file:'
     &               // dt2str( jtdate_temp, 0 ) // '<<---<<'
          xmsgs( 3 ) = ' '
          call m3parag ( 3, xmsgs )
        
        else
          xmsgs( 1 ) = 'Total column ozone will be interpolated to day '
     &               // dt2str( jdate, 0 )
          xmsgs( 2 ) = 'from data available on the OMI input file'
          xmsgs( 3 ) = ' '
          call m3parag ( 3, xmsgs )
        end if

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
          read( tmunit,* ) t( it ), lat( ilat ), ( oz( ilat, ilon, 1 ), ilon=1,(nlon-1) )
          oz( ilat, nlon, 1 ) = oz( ilat, 1, 1 )
        end do
   
        do ilat = 1, nlat
          read( tmunit,* ) t( it+1 ), lat( ilat ), ( oz( ilat, ilon, 2 ), ilon=1,(nlon-1) )
          oz( ilat, nlon, 2 ) = oz( ilat, 1, 2 )
        end do
   
      end if   ! jdate .ne. jdate_prev

      flag  = 0.0
      ozone = 0.0
      latitudem = 0.0
      x2 = 0.0
      x3 = 0.0

! Handle the special case of abs(lat) > 80.
! use a dummy latitude variable latitudem so as to prevent overwriting latitude

      if ( latitude .gt. 80.0 ) then
        latitudem = 80.0
      else if ( latitude .lt. -80.0 ) then
        latitudem = -80.0
      else
        latitudem = latitude
      end if

! Identify the database latitudes that bound the requested latitude
! Determine the proportionality x2

      x2loop: do ilat = 1, nlat-1
        if ( ( latitudem .le. lat( ilat ) ) .and. 
     &       ( latitudem .ge. lat( ilat+1 ) ) ) then
          x2 = ( latitudem - lat( ilat+1 ) ) / ( lat( ilat ) - lat( ilat+1 ) )
          exit x2loop
        end if
      end do x2loop

! Analogously determine the proportionality x3 fot longitude

      x3loop: do ilon = 1, nlon-1
        if ( ( longitude .ge. lon( ilon ) ) .and. 
     &       ( longitude .le. lon( ilon+1 ) ) ) then
          x3 = ( longitude - lon( ilon ) ) / ( lon( ilon+1 ) - lon( ilon ) )
          exit x3loop
        end if
      end do x3loop

      ozone = 0.0

! Determine the interpolated ozone, with consideration that some of the 8 ozone values
! of the data cube may be missing.  Construct the estimate from those values that are     ! available

      flag = 1.0

      if ( oz( ilat, ilon+1, 2 ) .le. 0.0 ) then
        oz( ilat, ilon+1, 2 ) = 0.0
        flag( 1 ) = 0.0
      else
        flag( 1 ) = x1 * x2 * x3
      end if

      if ( oz( ilat, ilon, 2 ) .le. 0.0 ) then
        oz( ilat, ilon, 2 ) = 0.0
        flag( 2 ) = 0.0
      else
        flag( 2 ) = x1 * x2 * ( 1.0 - x3 )
      end if

      if ( oz( ilat+1, ilon+1, 2 ) .le. 0.0 ) then
        oz( ilat+1, ilon+1, 2 ) = 0.0
        flag( 3 ) = 0.0
      else
        flag( 3 ) = x1 * ( 1.0 - x2 ) * x3
      end if

      if ( oz( ilat+1, ilon, 2 ) .le. 0.0 ) then
        oz( ilat+1, ilon, 2 ) = 0.0
        flag( 4 ) = 0.0
      else
        flag( 4 ) = x1 * ( 1.0 - x2 ) * ( 1.0 - x3 )
      end if

      if ( oz( ilat, ilon+1, 1 ) .le. 0.0 ) then
        oz( ilat, ilon+1, 1 ) = 0.0
        flag( 5 ) = 0.0
      else
        flag( 5 ) = ( 1.0 - x1 ) * x2 * x3
      end if

      if ( oz( ilat, ilon, 1 ) .le. 0.0 ) then
        oz( ilat, ilon, 1 ) = 0.0
        flag( 6 ) = 0.0
      else
        flag( 6 ) = ( 1.0 - x1 ) * x2 * ( 1.0 - x3 )
      end if

      if ( oz( ilat+1, ilon+1, 1 ) .le. 0.0 ) then
        oz( ilat+1, ilon+1, 1 ) = 0.0
        flag( 7 ) = 0.0
      else
        flag( 7 ) = ( 1.0 - x1 ) * ( 1.0 - x2 ) * x3
      end if

      if ( oz( ilat+1, ilon, 1 ) .le. 0.0 ) then
        oz( ilat+1, ilon, 1 ) = 0.0
        flag( 8 ) = 0.0
      else
        flag( 8 ) = ( 1.0 - x1 ) * ( 1.0 - x2 ) * ( 1.0 - x3 )
      end if

      ozone = (     x1 ) * (     x2 ) * (     x3 ) * oz( ilat  , ilon+1, 2 )
     &       +(     x1 ) * (     x2 ) * ( 1.0-x3 ) * oz( ilat  , ilon  , 2 )
     &       +(     x1 ) * ( 1.0-x2 ) * (     x3 ) * oz( ilat+1, ilon+1, 2 )
     &       +(     x1 ) * ( 1.0-x2 ) * ( 1.0-x3 ) * oz( ilat+1, ilon  , 2 )
     &       +( 1.0-x1 ) * (     x2 ) * (     x3 ) * oz( ilat  , ilon+1, 1 )
     &       +( 1.0-x1 ) * (     x2 ) * ( 1.0-x3 ) * oz( ilat  , ilon  , 1 )
     &       +( 1.0-x1 ) * ( 1.0-x2 ) * (     x3 ) * oz( ilat+1, ilon+1, 1 )
     &       +( 1.0-x1 ) * ( 1.0-x2 ) * ( 1.0-x3 ) * oz( ilat+1, ilon  , 1 )

      total = sum( flag )

! Special case of abs(lat) > 80

      if ( latitude .ge. 80.0 ) then

        np_oz = 0.0
        icount = 0

        do ilon = 1, nlon
          if ( ( ( oz( 1, ilon, 1 ) ) .le. 0.0 ) .or. 
     &         ( ( oz( 1, ilon, 2 ) ) .le. 0.0 ) ) then
            cycle
          else
            icount = icount + 1
            np_oz = np_oz + ( ( 1.0 - x1 ) * oz( 1, ilon, 1 )
     &                      +         x1   * oz( 1, ilon, 2 ) )
          end if
        end do

        if ( icount .eq. 0 ) then
          go to 899
        end if

        np_oz = np_oz / real( icount )

      else if ( latitude .le. -80.0 ) then

        sp_oz = 0.0
        icount = 0

        do ilon = 1, nlon
          if ( ( ( oz( nlat, ilon, 1 ) ) .le. 0.0 ) .or. 
     &         ( ( oz( nlat, ilon, 2 ) ) .le. 0.0 ) ) then
            cycle
          else
            icount = icount + 1
            sp_oz = sp_oz + ( ( 1.0 - x1 ) * oz( nlat, ilon, 1 )
     &                    +           x1   * oz( nlat, ilon, 2 ) )
          end if
        end do

        if ( icount .eq. 0 ) then
          go to 899
        end if

        sp_oz = sp_oz / real( icount )

      end if    ! Special case of abs(lat) > 80 is complete

! When no contiguous and conterminous data are available, default to 300 DU:

      if ( total .le. 0.0 ) then
        ozone = 300.0
      else
        if ( latitude .ge. 80.0 ) then
          np_oz = np_oz / total
          ozone = ( ( latitude - 80.0 ) * 0.1 ) * np_oz
     &          + ( 1.0 - ( ( ( latitude - 80.0 ) * 0.1 ) ) ) * ozone / total
        
        else if ( latitude .le. -80.0 ) then
          sp_oz = sp_oz / total
          ozone = ( ( latitude + 80.0 ) * 0.1 ) * sp_oz
     &          + ( 1.0 - ( ( ( latitude + 80.0 ) * 0.1 ) ) ) * ozone / total
        
        else
          ozone = ozone / total
        end if
      end if

899   if ( ozone .lt. 100.0 ) then
        ozone = 100.0
      else if ( ozone .gt. 600.0 ) then
        ozone = 600.0
      end if

      return

      end subroutine o3totcol
