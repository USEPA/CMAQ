
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

      subroutine o3totcol ( latitude, longitude, jdate, jtime, ozone )

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

      use runtime_vars
      use utilio_defn

      implicit none

! arguments

      integer, intent( in ) :: jdate      ! Julian day of the year (yyyyddd)
      integer, intent( in ) :: jtime      ! time (hhmmss)

      real, intent( in )    :: latitude   ! latitude of point on earth's surface
      real, intent( in )    :: longitude  ! longitude of point on earth's surface
      real, intent( inout ) :: ozone      ! total column ozone [DU]

! parameters

       real,    parameter :: sec2day = 1.0 / 8.64E+4
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
      integer :: time

      integer, save :: nlat ! = 17 ! or 19
      integer, save :: nlon ! = 17
      integer, save :: nt
      integer, save :: it
      integer, save :: icolumn_prev = 1 
      integer, save :: icolumn_next = 2
      integer, save :: tmunit
      integer, save :: jdate_prev = 0
      integer, save :: jtime_prev = 0
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
      real, save :: strdate, enddate
      real, save :: max_lat, min_lat

      real, allocatable, save :: t( : )
      real, allocatable, save :: lat( : )
      real, allocatable, save :: lon( : )
      real, allocatable, save :: oz( :, :, : ) ! two timesteps for interpolation

      logical, save :: firsttime = .true.

!----------------------------------------------------------------------

      if ( firsttime ) then
      
        firsttime = .false.

        tmunit = getefile( tmfile, .true., .true., pname )

        if ( tmunit .lt. 0 ) then
          xmsg = 'Error opening ' // tmfile
          call m3exit ( pname, jdate, 0, xmsg, xstat1 )
        end if

        read( tmunit, '(5x,i7)') nlat
        read( tmunit, '(5x,i7)') nlon

        write(logdev,'(a,i7,a,i7)')'OMI Ozone column data has Lat by Lon Resolution: ',
     &  nlat,'X',nlon

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
        read( tmunit, * )
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

        max_lat = maxval( lat )
        min_lat = minval( lat )
        strdate  = minval( t )
        enddate = maxval( t )

      end if ! firsttime

      if ( jdate .ne. jdate_prev .or. jtime .ne. jtime_prev ) then
! Use a temporary dummy variable jdate_temp so as not to overwrite jdate
        jtime_prev = jtime

        jyear = jdate / 1000
        time  = mod(jtime, 100) + 60*mod(jtime/100, 100)+ 3600*(jtime/10000)

        tdate = real( jyear ) 
     &        + ( real( jdate - jyear * 1000 ) + real( time ) * sec2day ) * yr2day( jyear )

        tdate_temp = tdate

! Determine if the ozone database includes the requested jdate
           if ( tdate .ge. enddate ) then
! Submitted date is outside of ozone database range.
!     Total column ozone will be estimated from the corresponding Julian Day
!     of the prior year
             tdate_temp = aint( enddate ) + ( tdate - aint( tdate ) )
             if ( tdate_temp .gt. enddate ) then
               tdate_temp = tdate_temp - 1.0
             end if
             jenddate = int( enddate ) * 1000
     &                + int( ( 1.0 / yr2day( int( enddate ) ) )
     &                  * ( enddate - aint( enddate ) ) )
             jtdate_temp = int( tdate_temp ) * 1000
     &                   + nint( ( 1.0 / yr2day( int( tdate_temp ) ) )
     &                   * ( tdate_temp - aint( tdate_temp ) ) )
             if( jdate_prev .ne. jdate )then ! write message to log
                xmsg = 'Requested date is beyond available data on OMI file:  <' 
     &              // dt2str( jenddate, 0 )
                call m3warn ( pname, jdate, 0, xmsg )
                xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day '
                xmsgs( 2 ) = 'of the last available year on the '
     &                    // 'OMI input file:' // dt2str( jtdate_temp, 0 ) // '<<---<<'
                write(xmsgs( 3 ),'(A,F14.8)')'Exact date: ',tdate_temp
                call m3parag ( 3, xmsgs )
             end if
           else if ( tdate .le. strdate ) then
! Submitted date is outside of ozone database range.
!     Total column ozone will be estimated from the corresponding Julian Day of
!     the subsequent year
             tdate_temp = real( int( strdate ) ) + ( tdate - real( int( tdate ) ) )
             if ( tdate_temp .lt. strdate ) then
               tdate_temp = tdate_temp + 1.0
             end if
             jstdate = int( strdate ) * 1000
     &               + int( ( 1.0 / yr2day( int( strdate ) ) )
     &               * ( strdate - aint( strdate ) ) )
             jtdate_temp = int( tdate_temp ) * 1000
     &                   + nint( ( 1.0 / yr2day( int( tdate_temp ) ) )
     &                   * ( tdate_temp - aint( tdate_temp ) ) )
             if( jdate_prev .ne. jdate )then ! write message to log
                xmsg = 'Requested date preceeds available data on OMI file:  >' 
     &              // dt2str( jstdate, 0 )
                call m3warn ( pname, jdate, 0, xmsg )
                xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day'
                xmsgs( 2 ) = 'of the next available year on the OMI input file:'
     &                 // dt2str( jtdate_temp, 0 ) // '<<---<<'
                xmsgs( 3 ) = ' '
                call m3parag ( 3, xmsgs )
             end if

! Submitted date falls within the satellite data measurement gap beginning
!     on 24 Nov 1994 and ending on 22 Jul 1996.

           else if ( ( tdate .ge. 1994.899 ) .and. 
     &               ( tdate .le. 1996.557 ) ) then
        
             if ( tdate .le. 1995.738 ) then
               tdate_temp = tdate - 1.0  ! use previous year
             else
               tdate_temp = tdate + 1.0  ! use subsequent year
             end if
             jtdate_temp = int( tdate_temp ) * 1000
     &                + nint( ( 1.0 / yr2day( int( tdate_temp ) ) )
     &                 * ( tdate_temp - aint( tdate_temp ) ) )
             if( jdate_prev .ne. jdate )then ! write message to log
                xmsg = 'Requested date falls within satellite data'
     &              // ' measurement gap: 24 Nov 1994 - 22 Jul 1996'
                call m3warn ( pname, jdate, 0, xmsg )
                xmsgs( 1 ) = 'Total column ozone will be estimated from the corresponding Julian Day'
                xmsgs( 2 ) = 'of the closest available year on the OMI input file:'
     &                     // dt2str( jtdate_temp, 0 ) // '<<---<<'
                xmsgs( 3 ) = ' '
                call m3parag ( 3, xmsgs )
             end if
           else
             if( jdate_prev .ne. jdate )then ! write message to log
               xmsgs( 1 ) = 'Total column ozone will be interpolated to day '
     &                    // dt2str( jdate, 0 )
               xmsgs( 2 ) = 'from data available on the OMI input file'
               xmsgs( 3 ) = ' '
               call m3parag ( 3, xmsgs )
             end if  
           end if

        if( jdate_prev .ne. jdate )then ! need to update day interpolation points
           jdate_prev = jdate
           oz = 0.0

! When adding x lines of data to OMI.dat, increase upper limit by x
! and increase the dimension of t as needed

! Determine the specific dates in the database that bound the requested jdate
! i.e.  (it) < (jdate_temp) < (it+1)
! where it is the index var for the database
! and determine the interpolation factor ?x1? between the bounding dates
! reset oz and jdate_prev

           x1 = 0.0
           x1loop: do it = 1, nt-1
             if ( ( tdate_temp .ge. t( it ) ) .and. 
     &              ( tdate_temp .le. t( it+1 ) ) ) then
                icolumn_prev = it
                icolumn_next = it + 1
                exit x1loop
              end if
            end do x1loop
! Determine the corresponding bounding ozone values for all lats and lons
           rewind( tmunit )
           read( tmunit,* )
           read( tmunit,* )
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
        end if 

        x1 = ( tdate_temp - t( icolumn_prev ) ) / ( t( icolumn_next ) - t( icolumn_prev ) )
   
      end if   ! jdate .ne. jdate_prev and jtime .ne. jday

      flag  = 0.0
      ozone = 0.0
      latitudem = 0.0
      x2 = 0.0
      x3 = 0.0

! Handle the special case of lat > max_lat or lat < min_lat.
! use a dummy latitude variable latitudem so as to prevent overwriting latitude

      if ( latitude .gt. max_lat ) then
        latitudem = max_lat
      else if ( latitude .lt. min_lat ) then
        latitudem = min_lat
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

! Special case of min_lat > lat or lat > max_lat

      if ( latitude .ge. max_lat ) then

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

      else if ( latitude .le. min_lat ) then

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
        if ( latitude .ge. max_lat ) then
          np_oz = np_oz / total
          ozone = ( ( latitude - max_lat ) * 0.1 ) * np_oz
     &          + ( 1.0 - ( ( ( latitude - max_lat ) * 0.1 ) ) ) * ozone / total
        
        else if ( latitude .le. min_lat ) then
          sp_oz = sp_oz / total
          ozone = ( ( latitude - min_lat ) * 0.1 ) * sp_oz
     &          + ( 1.0 - ( ( ( latitude - min_lat ) * 0.1 ) ) ) * ozone / total
        
        else
          ozone = ozone / total
        end if
      end if

899   if ( ozone .lt. 100.0 ) then
        ozone = 100.0
!        xmsg = 'interpolated ozone column below 100 DU'
!        write(logdev,'(A,20(F10.4,1X))')'For time:',tdate_temp
!        write(logdev,'(A,20(F10.4,1X))')'At lat,lon:',latitude,longitude
!        write(logdev,'(A,20(F10.4,1X))')'Intepolated data'
!        write(logdev,'(A,20(F10.4,1X))')'Time Point 1',
!     &       t( icolumn_prev ),lat( ilat ),lat( ilat+1 ),lon( ilon),lon( ilon+1 ),
!     &       oz( ilat,   ilon+1, 1 ), oz( ilat  , ilon  , 1 ), 
!     &       oz( ilat+1, ilon+1, 1 ), oz( ilat+1, ilon  , 1 )
!        write(logdev,'(A,20(F10.4,1X))')'Time Point 2',
!     &       t( icolumn_next ),lat( ilat ),lat( ilat+1 ),lon( ilon),lon( ilon+1 ),
!     &       oz( ilat,   ilon+1, 2 ), oz( ilat  , ilon  , 2 ), 
!     &       oz( ilat+1, ilon+1, 2 ), oz( ilat+1, ilon  , 2 )
!        write(logdev,'(A,20(F10.4,1X))')'Weights, x1, x2,x3: ',x1, x2,x3
!        CALL M3EXIT( 'o3totcol', JDATE, JTIME, XMSG, XSTAT1 )
      else if ( ozone .gt. 800.0 ) then
!        xmsg = 'interpolated ozone column above 800 DU'
!        write(logdev,'(A,20(F10.4,1X))')'For time:',tdate_temp
!        write(logdev,'(A,20(F10.4,1X))')'At lat,lon:',latitude,longitude
!        write(logdev,'(A,20(F10.4,1X))')'Intepolated data'
!        write(logdev,'(A,20(F10.4,1X))')'Time Point 1',
!     &       t( icolumn_prev ),lat( ilat ),lat( ilat+1 ),lon( ilon),lon( ilon+1 ),
!     &       oz( ilat,   ilon+1, 1 ), oz( ilat  , ilon  , 1 ), 
!     &       oz( ilat+1, ilon+1, 1 ), oz( ilat+1, ilon  , 1 )
!        write(logdev,'(A,20(F10.4,1X))')'Time Point 2',
!     &       t( icolumn_next ),lat( ilat ),lat( ilat+1 ),lon( ilon),lon( ilon+1 ),
!     &       oz( ilat,   ilon+1, 2 ), oz( ilat  , ilon  , 2 ), 
!     &       oz( ilat+1, ilon+1, 2 ), oz( ilat+1, ilon  , 2 )
!        write(logdev,'(A,20(F10.4,1X))')'Weights, x1, x2,x3: ',x1, x2,x3
!        CALL M3EXIT( 'o3totcol', JDATE, JTIME, XMSG, XSTAT1 )
        ozone = 800.0
      end if

      return

      end subroutine o3totcol
