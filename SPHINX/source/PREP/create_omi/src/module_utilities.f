      Module utilities_module

         Implicit None

      Contains

!***********************************************************************      
      subroutine get_OMI_listsize(rawfilename,nfiles)
      implicit none
!arguments:
      character*(*), intent( in  ) :: rawfilename
      integer,       intent( out ) :: nfiles
!local:
      character(1) a(256)
      character(8) date(10000)


      a(:)='a'
      write(6,'(a)')'OMI File List = ' // Trim( rawfilename )
      open(7,file=rawfilename,status='old')

      do 90 nfiles = 1,10000
       read(7,150,end=100)a(1:1)
90    continue  
100   nfiles = nfiles-1
      print*,'Number of Raw data to be processed = ', nfiles
      if(nfiles .lt. 1 )STOP

!      rewind(7)
!      do 190 j = 1,nfiles         
!       read(7,150)(a(i),i=1,18)      
!       write(9,150)(a(i),i=11,18)
150    format(18a1)
!190   continue
      close(7)
      return
      end subroutine get_OMI_listsize
!***********************************************************************
!***********************************************************************
      subroutine julian_date (year,month,day,julday,yrfrac)

c     input year, month, day
c     returns julian_date day (julday), year fraction (yrfrac)

      implicit none
      integer, intent( in  ) :: year, day, month
      integer, intent( out ) :: julday
      real(8), intent( out ) :: yrfrac

      integer   yrlength,leap,m4,m100,m400,
     &          i

      integer daytab(13,2)
      data daytab / 0,31,28,31,30,31,30,31,31,30,31,30,31,
     +              0,31,29,31,30,31,30,31,31,30,31,30,31 /

      real(8)   pi

      pi   = dacos(-1.d0)
      leap = 1
      yrlength = 365
      m4       = mod(year,4  )
      m100     = mod(year,100)
      m400     = mod(year,400)
      if(((m4.eq.0).and.(m100.ne.0)).or.(m400.eq.0))then
       leap = 2
       yrlength = 366
      endif
      
      julday = day
      do 190 i=1,month
       julday = julday + daytab(i,leap)
190   continue

!      yrfrac = (julday - 1.d0)/(real(yrlength,8))
       yrfrac = (real(julday, 8)-0.5d0)/(real(yrlength,8))
      return
      end subroutine julian_date
      Subroutine Julian_Plus_One( Jdate )
! increments date by one day
        Implicit None
        Integer, Intent( InOut ) :: Jdate ! YYYYDDD
        Integer :: day
        Integer :: year
        Integer :: days_per_year
     
        Jdate = Jdate + 1
        year  = Jdate / 1000
        day = mod(jdate, 1000 )
        If( ( mod(year,4) .eq. 0 )
     &               .And. ( mod(year,100) .ne. 0 )
     &                   .Or. ( mod(year,400) .eq. 0  ) )Then ! leap year
           days_per_year = 366
        Else
           days_per_year = 365
        Endif
        If( day .gt. days_per_year )Then
            Jdate = Jdate + 1000 - day + 1
        End If
      End Subroutine Julian_Plus_One
      Integer Function Delta_Julian( JDate1, Jdate2 )
! returns JDate1 minus Jdate2      
        Implicit None
        Integer, Intent( In ) :: Jdate1
        Integer, Intent( In ) :: Jdate2
!local
        Integer :: ldate1, ldate2 
        Integer :: year1, year2
        Integer :: day1, day2
        Integer :: iyear
        
        Delta_Julian = 0
        If( Jdate1 .Eq. Jdate2 )Return

        ldate2 = jdate1
        ldate1 = jdate2

        year1 = ldate1 / 1000
        year2 = ldate2 / 1000
        day1  = ldate1 - year1 * 1000
        day2  = ldate2 - year2 * 1000

        If( year1 .Eq. year2 )Then
            Delta_Julian = day2 - day1
            Return
        End If
        If( ( mod(year1,4) .eq. 0 )
     &               .And. ( mod(year1,100) .ne. 0 )
     &                   .Or. ( mod(year1,400) .eq. 0  ) )Then ! leap year
           Delta_Julian = 366 - day1 + day2
        Else
           Delta_Julian = 365 - day1 + day2
        Endif

        If( year1 + 1 .Eq. year2 )Return
        Do iyear = (year1 + 1), (year2 - 1)
           If( ( mod(iyear,4) .eq. 0 )
     &                  .And. ( mod(iyear,100) .ne. 0 )
     &                      .Or. ( mod(iyear,400) .eq. 0  ) )Then ! leap year
              Delta_Julian = Delta_Julian + 366
           Else
              Delta_Julian = Delta_Julian + 365
           Endif
        End Do
        
      End Function Delta_Julian
!***********************************************************************
      logical function is_leap_year( year )
! Function: Determines whether function is a leap year
         Implicit None
         
         Integer, Intent( In ) :: year
         
         is_leap_year = ( ( mod(year,4) .eq. 0 )
     &               .And. ( mod(year,100) .ne. 0 )
     &                   .Or. ( mod(year,400) .eq. 0  ) )
         
      end function is_leap_year
      subroutine extract_o3_cmaq ( jdate, date, latitude, longitude, ozone_omi )

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
      USE ENV_VARS

      implicit none

! arguments
      integer, intent( in ) :: jdate             ! julian date yyyyddd
      real(8), intent( in ) :: date              ! date year plus fraction of year elapsed
      real,    intent( in ) :: latitude ( : )    ! latitude of point on earth's surface
      real,    intent( in ) :: longitude( : )    ! longitude of point on earth's surface
      real(8), intent( in ) :: ozone_omi( :, : ) ! omi total column ozone [DU]

! parameters

      integer, save :: nlat     = 179 ! 161 ! 179 ! 17 
      integer, save :: nlon     = 361 ! 361 ! 17
      real,    parameter :: pi       = 3.14159265
      real,    parameter :: pi_180   = pi / 180.0
      real,    save :: lat_window = 1.0 ! 10.0

! local variables

!      character( 23 ), save :: tmfile   = 'OMI_CMAQ_DAT
      character( 16 ), save :: OMI_CMAQ_NCF = 'OMI_CMAQ_NCF'
      character( 16 ), save :: pname    = 'O3TOT_CMAQ'
      character( 96 ) :: xmsg = ' '
      character( 96 ) :: xmsgs( 3 )
      logical, save :: firsttime = .true.

      integer :: allocstat
      integer :: nlat_omi
      integer :: nlon_omi
      integer :: i, j
      integer :: icount
      integer :: ios
      integer :: nrecs
      integer :: jyear
!      integer :: delta_julian
      integer :: delta_date

      real :: flag( 4 )
      real :: Q11, Q21
      real :: Q12, Q22
      real :: x2
      real :: x3
      real :: np_oz
      real :: sp_oz
      real :: total
      real :: latitudem
      real :: max_lat_omi, min_lat_omi
      real :: tdate_temp, tdate
      real :: stdate, enddate

      integer, save :: jdate_expect = 0
      integer, save :: it
      integer, save :: io_unit
      integer, save :: ilat 
      integer, save :: ilon 

      integer, allocatable, save :: jlat( : ) ! ( nlat )
      integer, allocatable, save :: jlon( : ) ! ( nlon )

      real, save :: dx2
      real, save :: dy2

      real, allocatable, save :: lat( : ) ! ( nlat )
      real, allocatable, save :: lon( : ) ! ( nlon )
      real, allocatable, save :: lon_out( : ) ! ( nlon )
      real, allocatable, save :: ozone( :,: ) ! ( nlat, nlon )    = 0.0  ! ascii file interpolated total column ozone [DU]
      real, allocatable, save :: ozone_viz( :,: ) ! ( nlon-1, nlat )  = 0.0  ! ioapi file interpolated total column ozone [DU]
      real, allocatable, save :: viz_prev ( :,: ) ! ( nlon-1, nlat )  = 0.0  ! previous value of ozone_viz
      real, allocatable, save :: viz_adjust( :,: ) ! ( nlon-1, nlat ) = 0.0  ! correction to viz_prev if 
      real, allocatable, save :: dx1( : ) ! ( nlon )
      real, allocatable, save :: dy1( : ) ! ( nlat )    
      real, allocatable, save :: lon_transformed( : )

      character(34) :: output_format 
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

! set output dimenion based on environment settings
        
        nlat       = npoints_lat
        nlon       = npoints_lon
        lat_window = lat_border
        
! allocate save arrays

        Allocate( jlat(nlat), jlon(nlon), stat=allocstat )
        If ( allocstat  .ne. 0 ) Then
          xmsg = 'error allocating jlat,jlon'
          write(6,'(a)')xmsg
          Stop 
        End If
        
        Allocate( ozone(nlat,nlon), stat=allocstat )
        If ( allocstat  .ne. 0 ) Then
          xmsg = 'error allocating ozone'
          write(6,'(a)')xmsg
          Stop 
        End If
        ozone = 0.0
        
        Allocate( ozone_viz ( nlon-1, nlat ), 
     &            viz_prev  ( nlon-1, nlat ), 
     &            viz_adjust( nlon-1, nlat ), stat=allocstat )
        If ( allocstat  .ne. 0 ) Then
          xmsg = 'error allocating ozone_viz, viz_prev, viz_adjust '
          write(6,'(a)')xmsg
          Stop 
        End If
        ozone_viz  = 0.0
        viz_prev   = 0.0
        viz_adjust = 0.0

        Allocate( lat(nlat), lon(nlon), 
     &            dy1(nlat), dx1(nlon), 
     &            lon_out(nlon), stat=allocstat )
        If ( allocstat  .ne. 0 ) Then
          xmsg = 'error allocating lat,lon,lon_out,dx1,dy1'
          write(6,'(a)')xmsg
          Stop 
        End If
 
! Assign values to array of longitudes: lon

        dx2 = 360.0 / real( nlon - 1 )
        do ilon = 1, nlon
          lon_out( ilon ) = -180.0 + dx2 * real( ilon-1 )
        end do

        lon = lon_out 
        do j = 1, nlon
           if( lon( j ) .lt. 0.0 )then
               lon( j ) = lon( j ) + 360.0
           end if
!           print*,'j,lon( j ) = ',j,lon( j )
        end do

        dy2 = ( 180.0 - 2.0*lat_window )/ real( nlat - 1 )
        do ilat = 1, nlat
          lat( ilat ) =   90.0 - dy2 * real( ilat - 1  ) - lat_window
        end do

        open( file = OMI_CMAQ_DAT, status = 'unknown', newunit = io_unit )
        write(io_unit,549)'nlat',nlat
        write(io_unit,549)'nlon',nlon
        write(output_format,'(a,i8,a)')'(2(a,tr1),',(nlon+1),'(f7.2,tr1))'
        write(io_unit,output_format)'yeardate','latitude',(lon_out(j),j=1,nlon) 

        nlat_omi = size( latitude )
        nlon_omi = size( longitude )
      if( .Not. allocated( lon_transformed ) )then
!          print*," allocating lon_transformed: nlon_omi = ",size( longitude ) 
          allocate( lon_transformed ( nlon_omi ) )
      end if

      lon_transformed = longitude
      do j = 1, nlon_omi
         if( lon_transformed( j ) .lt. 0.0 )then
             lon_transformed( j ) = lon_transformed( j ) + 360.0
         end if
!         print*,'j,lon_transformed( j ) = ',j,lon_transformed( j )
      end do
! find the nearest longitude point
        do j = 1, nlon
           dx1( j )  = lon( j ) - lon_transformed( 1 )
           jlon( j ) = 1
           x3loop: do ilon = 2,nlon_omi
             dx2 = lon( j ) - lon_transformed( ilon )
             if( abs( dx2  ) .lt. abs( dx1( j ) ) )then
                 dx1( j )  = dx2
                 jlon( j ) = ilon
             end if
           end do x3loop
!           print*,'lon_transformed( jlon ),lon( j ) = ', lon_transformed( jlon( j ) ),lon( j )
!           print*,'longitude( jlon ),lon( j       ) = ', longitude( jlon( j ) ),lon_out( j )
        end do
! find the nearest latitude point
        do i = 1, nlat
           dy1( i )  = lat( i ) - latitude( 1 )
           jlat( i ) = 1
           x2loop: do ilat = 2, nlat_omi
             dy2 = lat( i ) - latitude( ilat )
             if( abs( dy2  ) .lt. abs( dy1( i ) ) )then
                 dy1( i ) = dy2
                 jlat(i)  = ilat
             end if
           end do x2loop
!           print*,'latitude( jlat ),lat( i ) = ', latitude( jlat( i ) ),lat( i )
        end do 
! create ioapi for visualization
         call CREATE_CMAQ_OMI ( OMI_CMAQ_NCF, jdate, lat, lon_out )
         jdate_expect = jdate
      else
        call Julian_plus_One( jdate_expect )
        open( file = OMI_CMAQ_DAT, status = 'unknown', position = 'append', newunit = io_unit )

      end if ! firsttime


549   Format(a4,1x,i7)
     


! Initialize


      flag  = 0.0
      ozone = 0.0
      latitudem = 0.0
      x2 = 0.0
      x3 = 0.0
      ozone = -1.0
!      max_lat_omi = maxval( latitude )
!      min_lat_omi = minval( latitude )



          
      do j = 1, nlon
         do i = 1, nlat
            ozone( i , j) = real( ozone_omi( jlat( i )  , jlon( j ) ) ) 
            if( j .gt. nlon-1 )cycle
            ozone_viz( j, nlat-i+1 ) = ozone( i , j)
         end do
      end do

      If( jdate_expect .ne. jdate )Then
! write interpolated values up to current date
         delta_date = Delta_julian( jdate, jdate_expect )
         viz_adjust = ( ozone_viz - viz_prev )/real(delta_date + 1)

         Do j = 1, delta_date
            viz_prev = viz_prev + viz_adjust
            If ( .not. write3( OMI_CMAQ_NCF, 'OZONE_COLUMN', jdate_expect, 0,
     &                           viz_prev ) ) THEN
                   xmsg = 'Error writing variable OZONE_COLUMN'
                  call m3exit ( pname, jdate_expect, 0, xmsg, xstat1 )
            Else
               write(6,*)'observation missing on ', jdate_expect
               write(6,*)'writing to netcdf file inpolation between observations'
            End If
            call Julian_plus_One( jdate_expect )
         End Do
      End If

      If ( .not. write3( OMI_CMAQ_NCF, 'OZONE_COLUMN', jdate, 0,
     &                        ozone_viz ) ) THEN
             xmsg = 'Error writing variable OZONE_COLUMN'
             call m3exit ( pname, jdate, 0, xmsg, xstat1 )
      End If

      write(output_format,'(a,i8,a)')'(f9.4,tr1,f7.1,',(nlon+1),'(i7,tr1))'
      do i = 1,nlat          
        write(io_unit,output_format)date,lat(i),(nint( ozone(i,j) ),j=1,nlon)       
590   end do
     
      jdate_expect = jdate
      viz_prev     = ozone_viz

      close( io_unit )
      return

      end subroutine extract_o3_cmaq 
      Subroutine fill( lat, lon, values, limit)
        Implicit None
! arguments:        
        real( 8 ), intent( in )    :: lat( : )      ! latitude, radians
        real( 8 ), intent( in )    :: lon( : )      ! longitude (0 to 2PI), radians
        real( 8 ), intent( inout ) :: values( :,: ) ! quantity in lat, lon dimensions
        real( 8 ), intent( in )    :: limit         ! limit on weight for computing average
! local:        
        integer :: nlat
        integer :: nlon
        integer :: i, j, k, m, n
        integer :: icount
        
        real( 8 ) :: sum_weighted
        real( 8 ) :: sum_weights
        real( 8 ) :: weight
        real( 8 ) :: p5_delta_lat
        real( 8 ) :: p5_delta_lon
        real( 8 ) :: sin2_delta_lat
        real( 8 ) :: sin2_delta_lon
        real( 8 ) :: cos_dot_lat
        real( 8 ) :: arc_length

        real( 8 ) :: cut_off
        
          nlat = size( lat )
          nlon = size( lon )
          
          cut_off = sin( 0.5d0*limit ) * sin( 0.5d0*limit )

          do i = 1, nlat
             do j = 1, nlon
                if( values( i, j ) .gt. 0.0d0 )then
!                    print*,'invalid at i,j = ',i,j
                    cycle
                else
!                    print*,'invalid at i,j = ',i,j
                end if
!    attempt to replace with average based on nearest neighbors                
                sum_weights  = 0.0d0
                sum_weighted = 0.0d0
                icount       = 0
                do m = 1, nlat
                   p5_delta_lat   = abs(lat(i)-lat(m))
!                   write(6,'(a,10(es12.4,1x))')'fill: p5_delta_lat = ', p5_delta_lat
                   if( p5_delta_lat .gt. limit )cycle
                   p5_delta_lat   = 0.5d0*p5_delta_lat
                   sin2_delta_lat = sin(p5_delta_lat)*sin(p5_delta_lat)
                   cos_dot_lat    = cos(lat(i))*cos(lat(m))
!                   write(6,'(a,10(es12.4,1x))')'fill: p5_delta_lat,sin2_delta_lat,sin2_delta_lon,cos_dot_lat = ',
!     &                     p5_delta_lat,sin2_delta_lat,cos_dot_lat
                   do n = 1, nlon
                      if( i .eq. m .and. j .eq. n )cycle
                      if( values( m,n ) .le. 0.0d0 )cycle
!                     set up factors for geodesic arc length
                      p5_delta_lon   = 0.5d0*abs(lon(j)-lon(n))
                      sin2_delta_lon = sin(p5_delta_lon)*sin(p5_delta_lon)
                      arc_length     = sin2_delta_lat+cos_dot_lat*sin2_delta_lon
                      if( arc_length .gt. cut_off )cycle
                      arc_length     = 2.0d0 * asin( sqrt(arc_length) )
                      icount = icount + 1
!                      write(6,'(a,14(es12.4,1x))')'fill: p5_delta_lon,sin2_delta_lon,arc_length = ',
!     &                lat(i),lon(j),lat(m),lon(n),p5_delta_lon,sin2_delta_lon,arc_length
                      weight       = exp( -arc_length*arc_length )
                      sum_weights  = sum_weights + weight
                      sum_weighted = sum_weighted + weight*values(m,n)
                   end do
               end do
               if( sum_weights .gt. 0.0d0 )then
                   values(i,j) = sum_weighted / sum_weights
               end if
             end do
          end do
          return        
      End Subroutine fill

      End Module utilities_module
