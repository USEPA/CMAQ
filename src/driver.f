      program omi

      USE m3utilio
      USE ENV_VARS

      implicit none
      
      character(18)  :: rowheader
      character(256), allocatable :: OMI_filename( : )
      character(256) :: file_name
      character(256) :: file_line
!      character(32)  :: OMI_FILE_LIST  = 'OMI_FILE_LIST'
      character(16)  :: OMI_FILE_NCF   = 'OMI_FULL_NCF'
!      character(16)  :: OMI_FULL_DAT   = 'OMI_FULL_DAT'
      character(16)  :: EXTEN_FILE_NCF = 'OMI_EXPAND_NCF'
      CHARACTER(80)  :: XMSG = ' '

      integer       nlatitude   
      integer       nlongitude
      integer       year,month,day, julday
      integer       icount, jcount
      integer       i,j,k
      integer       i_, j_
      integer       ip1, kp1
      integer       i_max, j_max
      integer       nfiles
      integer       unit_expand
      integer       ipass
      integer       io_files
      integer       io_file_init
      integer       io_full_dat
      integer       jdate_init
      integer       jdate_prev
      integer       jdate_next
      integer       ldate
      integer       stat_allocate
      integer       delta_julian
      integer       delta_date

      integer, allocatable :: jdate( : ) 
      integer, allocatable :: idate( : )
      integer, allocatable :: oz_toms( : )

      Character(3), allocatable :: oz_string( : )
         
      real,    parameter :: pi    = 3.14159
      real,    parameter :: pi180 = pi / 180.0
      real(8), parameter :: fill_limit = 3.14159d0 /36.0d0

      real(8), allocatable :: oz ( :,: )
      real(8), allocatable :: oz_( :,: )
      real(8), allocatable :: oz_mean( :,: ) 
      real(8), allocatable :: oz_prev( :,: )
      real(8), allocatable :: oz_expand( :,: )
      real(8), allocatable :: gregdate( : ),yrfrac_( : )

      real,    allocatable :: lat_omi( :), lon_omi( : ), lat_ioapi( :,: ) 
      real(8), allocatable :: phi_omi( : ), theta_omi( : ) ! lat, lon [radians]
      real(8), allocatable :: lat_( : ),lon_( : )
      real(8), allocatable :: oz_extend( :,: )

      real,    allocatable :: ioapi_buff( :,: )
      real,    allocatable :: ioapi_prev( :,: )
      real,    allocatable :: oz_adjust( :,: )
      real,    allocatable :: oz_ioapi( :,: )
      real,    allocatable :: cloud_fraction( :,: )
      real,    allocatable :: o3_missing( :,: )
      real,    allocatable :: lat_expand(:),lon_expand(:) 

      real(8) :: yrfrac
      real    :: latstepsize,lonstepsize
      real    :: lat,lon
      real    :: init_lat,init_lon
      real(8) :: w(2),v(4)


      logical, save      :: First_time    = .True.
      logical, parameter :: near_neighbor = .False. ! replace missing using fill subroutine
      logical            :: read_clouds   = .False.
      logical            :: TOMS_FORMAT   = .False.

      interface
        subroutine get_OMI_listsize(rawfilename,nfiles)
           character*(*), intent( in  ) :: rawfilename
           integer,       intent( out ) :: nfiles
        end subroutine get_OMI_listsize
        subroutine o3tot_cmaq ( date, latitude, longitude, ozone_omi )
           real(8), intent( in ) :: date              ! date year plus fraction of year elapsed
           real,    intent( in ) :: latitude ( : )    ! latitude of point on earth's surface
           real,    intent( in ) :: longitude( : )    ! longitude of point on earth's surface
           real(8), intent( in ) :: ozone_omi( :, : ) ! omi total column ozone [DU]
        end subroutine o3tot_cmaq
        subroutine extract_o3_cmaq ( jdate, date, latitude, longitude, ozone_omi )
           integer, intent( in ) :: jdate             ! julian date yyyyddd
           real(8), intent( in ) :: date              ! date year plus fraction of year elapsed
           real,    intent( in ) :: latitude ( : )    ! latitude of point on earth's surface
           real,    intent( in ) :: longitude( : )    ! longitude of point on earth's surface
           real(8), intent( in ) :: ozone_omi( :, : ) ! omi total column ozone [DU]
        end subroutine extract_o3_cmaq
        subroutine julian_date (year,month,day,julday,yrfrac)
          implicit none
          integer, intent( in  ) :: year, day, month
          integer, intent( out ) :: julday
          real(8), intent( out ) :: yrfrac
        end subroutine julian_date
        SUBROUTINE CREATE_IOAPI_OMI ( FILE_NAME, JDATE, NLAT, NLON )
          CHARACTER( 16 ), INTENT( IN ) :: FILE_NAME  ! name of file 
          INTEGER,         INTENT( IN ) :: JDATE      ! Start date of file, YYYYDDD
          INTEGER,         INTENT( IN ) :: NLAT       ! # of latitude points
          INTEGER,         INTENT( IN ) :: NLON       ! # of Longitude points
        END SUBROUTINE CREATE_IOAPI_OMI
        SUBROUTINE CREATE_EXTEND_OMI ( FILE_NAME, JDATE )
          CHARACTER( 16 ), INTENT( IN ) :: FILE_NAME  ! name of file 
          INTEGER,         INTENT( IN ) :: JDATE      ! Start date of file, YYYYDDD
        END SUBROUTINE CREATE_EXTEND_OMI
!       subroutine viz_o3totcol ( jdate )
!         integer, intent( in ) :: jdate      ! Julian day of the year (yyyyddd)
!       end subroutine viz_o3totcol
        Subroutine fill( lat, lon, values, limit )
          real( 8 ), intent( in )    :: lat( : )      ! latitude, radians
          real( 8 ), intent( in )    :: lon( : )      ! longitude (0 to 2PI), radians
          real( 8 ), intent( inout ) :: values( :,: ) ! quantity in lat, lon dimensions
          real( 8 ), intent( in )    :: limit         ! limit on weight for computing average
        end subroutine fill
        Subroutine Julian_Plus_One( Jdate )
           Integer, Intent( InOut ) :: Jdate ! YYYYDDD
           Integer :: day
           Integer :: year
           Integer :: days_per_year
        End Subroutine Julian_Plus_One
      end interface


      Call GET_ENVS()

      nlatitude  = 180
      nlongitude = 360

      if( CREATE_FULL_FILES )open(file=OMI_FULL_DAT,newunit=io_full_dat)

      file_name ='a'      
      
      call get_OMI_listsize(OMI_FILE_LIST,nfiles)

      Allocate( OMI_filename( nfiles ), stat=stat_allocate )
      If ( stat_allocate  .ne. 0 ) Then
          xmsg = 'error allocating OMI_filename'
          write(6,'(a)')xmsg
          Stop 
       End If
       Allocate( jdate( nfiles ), idate(nfiles), stat=stat_allocate )
       If ( stat_allocate  .ne. 0 ) Then
          xmsg = 'error allocating jdate,idate'
          write(6,'(a)')xmsg
          Stop 
       End If
       Allocate( gregdate( nfiles ),yrfrac_(nfiles), stat=stat_allocate )
       If ( stat_allocate  .ne. 0 ) Then
         xmsg = 'error allocating gregdate,yrfrac'
         write(6,'(a)')xmsg
         Stop 
       End If
     
      Open(file=OMI_FILE_LIST,status='old',newunit=io_files)
      Do j = 1,nfiles         
          read(io_files,'(a)')file_name
! get starting position of date in file name
          k = index(file_name,'OMI.ozone.', back=.true. ) + 10
          If( k .eq. 10 )Then
              k = index(file_name,'OMI.full.', back=.true. ) + 9
          End If
          If( k .eq. 9 )Then
              k = index(file_name,'L3e_ozone_omi_', back=.true. ) + 14
              TOMS_FORMAT = .True.
          End If
          read(file_name(k:k+7),*)idate(j)
      End Do

      If( TOMS_FORMAT )Then ! reset nlatitude and nlongitude
        Open(file=file_name,status='old',newunit=io_file_init)
        Read(io_file_init,'(a)')file_line
        Read(io_file_init,'(12x,i6)')nlongitude
        Read(io_file_init,'(12x,i6)')nlatitude
        Allocate( oz_string( nlongitude ), stat=stat_allocate )
        Allocate( oz_toms( nlongitude ), stat=stat_allocate )
        If ( stat_allocate  .ne. 0 ) Then
           xmsg = 'error allocating jdate,idate'
           write(6,'(a)')xmsg
           Stop 
        End If
        Close( io_file_init )
      End If

      Call Init_Arrays()  

      do j = 1,nfiles

        gregdate(j) = real(idate(j),8)
       
        year  = int( gregdate(j)                            /10000.d0)
        month = int((gregdate(j) - (real(year,8))*10000.d0)/  100.d0)
        day   = int((gregdate(j) - (real(year,8))*10000.d0 
     &                           - (real(month,8))*100.d0  )/    1.d0)

        call julian_date (year,month,day,julday,yrfrac)

        jdate( j ) = 1000 * year + julday
        If( j .Gt. 1 )Then ! check for continuous and ascending dates
           If( ( jdate( j )-jdate( j-1 ) ) .Ne. 1 )Then
              Print*,'Data gap from ',jdate( j-1 ),' to ', jdate( j ) 
           End If
        End If
        yrfrac_(j) = yrfrac+real(year,8)
      End Do

!set jdate_init
       if( mod(jdate( 1 ),1000) .gt. 1 )then
           jdate_init = jdate( 1 ) - 1
       else 
           year  = idate(1)/10000 - 1
           day   = 31
           month = 12
           call julian_date (year,month,day,julday,yrfrac)
           jdate_init = 1000 * year + julday
       end if

      latstepsize = 180.0/real(nlatitude)
      lonstepsize = 360.0/real(nlongitude)
     
      init_lat = 0.5*real(latstepsize)
      init_lon = 0.5*real(lonstepsize)

      close(io_files)
      open(file=OMI_FILE_LIST,newunit=io_files)
       do i = 1, (nlatitude/2)
        lat_omi( i ) = ( 90.0 + init_lat ) - latstepsize*real(i)
        phi_omi( i ) = real(pi180*lat_omi(i), 8 )
        lat_omi( nlatitude - i + 1 ) = - lat_omi( i )
        phi_omi( nlatitude - i + 1 )  = - phi_omi( i  )
       end do


       do i = 1, nlatitude
          print*,'lat_omi( i ), phi_omi( i ) = ',lat_omi( i ), phi_omi( i ) 
       end do 
          print*,'lat_omi( 1 ), phi_omi( 1 ) = ',lat_omi( 1 ), phi_omi( 1 ) 



       do i = 1, nlongitude
        lon_omi( i ) = -180.0 - init_lon + lonstepsize*real(i)
       end do
       do i = 1, (nlongitude/2)
        theta_omi( i ) = real(pi180*(lon_omi( i ) + 360.0), 8)
        k = i + (nlongitude/2)
        theta_omi( k ) = real(pi180*lon_omi( k ), 8)
       end do
       
       do i = 1, nlongitude
          print*,'lon_omi( i ), theta_omi( i ) = ',lon_omi( i ), theta_omi( i ), lonstepsize
       end do 
         print*,'lon_omi( 1 ), theta_omi( 1 ) = ',lon_omi( 1 ), theta_omi( 1 ), lonstepsize
       
      Do i = 1, nlatitude
         Do j = 1, nlongitude
            lat_ioapi( j,nlatitude-i+1) = lat_omi( i )
         End Do
      End Do

       call expand_init()
!       open( file = 'OMI_expand_14t16.dat', status = 'unknown', newunit = unit_expand )
!       write(unit_expand,'(a19,720f7.1)')'  yeardate    lat      ',(lon_expand(j),j=1,720) 


      call get_mean()
      
      do i = 1, nlatitude
         do k = 1, nlongitude
            if( oz_mean( i,k ) .ne. oz_mean( i,k ) )stop
            oz_ioapi( k, nlatitude - i + 1 ) = real( oz_mean( i,k ), 4 )
         end do
      end do

        print*,'oz_ioapi:oz_mean max/min: ',maxval(oz_ioapi),'/',maxval(oz_mean),minval(oz_ioapi),
     &                               '/',minval(oz_mean)

      if( CREATE_FULL_FILES )then
         call CREATE_IOAPI_OMI( OMI_FILE_NCF, jdate_init, nlatitude, nlongitude )
!          call CREATE_EXTEND_OMI( EXTEN_FILE_NCF, jdate( 1 ) )
         IF ( .NOT. WRITE3( OMI_FILE_NCF, 'OZONE_COLUMN', JDATE_INIT, 0,
     &                      OZ_IOAPI ) ) THEN
             XMSG = 'Error writing variable OZONE_COLUMN'
             CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
         END IF

         cloud_fraction = -1.0
         IF ( .NOT. WRITE3( OMI_FILE_NCF, 'CLOUD_FRACT', JDATE_INIT, 0,
     &                      CLOUD_FRACTION ) ) THEN
             XMSG = 'Error writing variable CLOUD_FRACT'
             CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
         END IF
   
          o3_missing = -1.0
         IF ( .NOT. WRITE3( OMI_FILE_NCF, 'O3_MISSING', JDATE_INIT, 0,
     &                      O3_MISSING ) ) THEN
             XMSG = 'Error writing variable O3_MISSING'
             CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( OMI_FILE_NCF, 'LATITUDE', JDATE_INIT, 0,
     &                   LAT_IOAPI ) ) THEN
             XMSG = 'Error writing variable LATITUDE'
             CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
         END IF
      end if

! set initial previous values to mean from all files      
      oz_prev    = oz_mean
      jdate_next = jdate_init
      Loop_Omi_Files: do j = 1,nfiles
              
         read(io_files,'(a)')OMI_filename(j)
         write(6,*)OMI_filename(j)
! determine whether to read cloud fraction
         if( index(OMI_filename(j),'OMI.full.', back=.true. ) .gt. 0 )then
             read_clouds = .True.
         else
             read_clouds = .False.
         end if 

         open(file=OMI_filename(j),status = 'old', newunit=io_file_init)
         oz = 0.d0
         oz_ = 0.d0
         cloud_fraction = -1.0
         If( TOMS_FORMAT )Then
            Do i = 1, 3
               read(io_file_init,'(a)')file_line
            End Do
            Do i = nlatitude, 1, -1
               lat = init_lat*real( i ) - ( 90.0 + init_lat ) 
!               read(io_file_init,'(1x,25a3)')(oz_string(k),k = 1, nlongitude)
!                Do k = 1, nlongitude
!                    If( oz_string(k) .Eq. '***' )Then
!                       oz_toms(k) = 0.0
!                    Else
!                       read(oz_string(k),'(i3)')oz_toms(k)
!                    End If
!                End Do
               read(io_file_init,'(1x,25i3)')(oz_toms(k),k = 1, nlongitude)
               oz(i,1:nlongitude) = real( oz_toms(1:nlongitude),8)
!               write(6,'(25(i3,1x))')(int(oz(i,k)),k = 1, nlongitude)
!          if(i .ge. nlatitude -1 )write(6,555)yrfrac_(j),lat_omi(i),(( oz_toms(k) ),k=1,nlongitude)       
            End Do
         Else
            read(io_file_init,*)
            do i = nlatitude,1,-1
               lat = 90.0 + init_lat - init_lat*real( i )
               read(io_file_init,*)rowheader,(oz(i,k),k=1,nlongitude)
            end do
            if( read_clouds )then
               do i = 1,nlatitude
                  read(io_file_init,*)rowheader,(cloud_fraction(k,i),k=1,nlongitude)
               end do
               where( cloud_fraction .lt. -1.0 ) cloud_fraction = -1.0
               where( cloud_fraction .gt.  1.0 ) cloud_fraction =  1.0
            end if
        End If
        Close( io_file_init )
!        pause
        where( oz .lt. 1.0d-3 ) oz = -1.0d0


! fill in missing values with nearest neighbors
        if( LUSE_NEIGHBORS ) then           
           call fill(phi_omi, theta_omi, oz, fill_limit)
        end if

! replace values still missing with previous values
        do i = 1, nlatitude
           do k = 1, nlongitude
              if( oz(i,k) .le. 0.0d0 )then
                  if( LUSE_PREV_DATE )oz(i,k) = oz_prev(i,k)
                  o3_missing( k, nlatitude - i + 1 ) = 1.0
              else 
                  o3_missing( k, nlatitude - i + 1 ) = -1.0
              end if
           end do
        end do

        do i = 1, nlatitude
           do k = 1, nlongitude
              oz_ioapi( k, nlatitude - i + 1 ) = real( oz( i,k ), 4 )
           end do
        end do

        write(6,'(5(a,f6.2,1x))') 'oz_ioapi:oz max:oz_ioapi:oz min/min ',
     &  maxval(oz_ioapi),'/',maxval(oz),':',minval(oz_ioapi),'/',minval(oz)

! Test whether Observation's date matches expected date
        Call Julian_plus_One( jdate_next )
        If( jdate_next .ne. JDATE( J ) )Then ! corrected expected date
              delta_date = Delta_julian( jdate_next, JDATE( J ) )
              OZ_ADJUST = ( OZ_IOAPI - IOAPI_PREV )/REAL( delta_date  + 1 )
              If( CREATE_FULL_FILES )Then ! write out previous values 
                 Do ldate = 1, delta_date
                    IOAPI_PREV = OZ_ADJUST + IOAPI_PREV
                    IF ( .NOT. WRITE3( OMI_FILE_NCF, 'OZONE_COLUMN', jdate_next, 0,
     &                                 IOAPI_PREV ) ) THEN
                         XMSG = 'Error writing variable OZONE_COLUMN'
                         CALL M3EXIT ( 'RO3', JDATE( J ), 0, XMSG, XSTAT1 )
                    END IF
                    IF ( .NOT. WRITE3( OMI_FILE_NCF, 'CLOUD_FRACT', jdate_next, 0,
     &                                 IOAPI_BUFF ) ) THEN
                         XMSG = 'Error writing variable CLOUD_FRACT'
                         CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
                    END IF
                    IF ( .NOT. WRITE3( OMI_FILE_NCF, 'O3_MISSING', jdate_next, 0,
     &                                 IOAPI_BUFF ) ) THEN
                        XMSG = 'Error writing variable O3_MISSING'
                        CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
                     END IF
                     IF ( .NOT. WRITE3( OMI_FILE_NCF, 'LATITUDE', jdate_next, 0,
     &                                  LAT_IOAPI ) ) THEN
                         XMSG = 'Error writing variable LATITUDE'
                         CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
                     END IF
                     Call Julian_plus_One( jdate_next )
                 End Do
              Else
                 Do ldate = 1, delta_date
                     Call Julian_plus_One( jdate_next )
                 End Do
              End If
        End If

        If( CREATE_FULL_FILES )Then
           IF ( .NOT. WRITE3( OMI_FILE_NCF, 'OZONE_COLUMN', JDATE( J ), 0,
     &                        OZ_IOAPI ) ) THEN
                 XMSG = 'Error writing variable OZONE_COLUMN'
                 CALL M3EXIT ( 'RO3', JDATE( J ), 0, XMSG, XSTAT1 )
           END IF
           IF ( .NOT. WRITE3( OMI_FILE_NCF, 'CLOUD_FRACT', JDATE( J ), 0,
     &                        CLOUD_FRACTION ) ) THEN
                 XMSG = 'Error writing variable CLOUD_FRACT'
                 CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
           END IF
           IF ( .NOT. WRITE3( OMI_FILE_NCF, 'O3_MISSING', JDATE( J ), 0,
     &                        O3_MISSING ) ) THEN
                 XMSG = 'Error writing variable O3_MISSING'
                 CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
           END IF
           IF ( .NOT. WRITE3( OMI_FILE_NCF, 'LATITUDE', JDATE( J ), 0,
     &                        LAT_IOAPI ) ) THEN
                XMSG = 'Error writing variable LATITUDE'
                CALL M3EXIT ( 'RO3', JDATE_INIT, 0, XMSG, XSTAT1 )
           END IF
        End If

        IOAPI_PREV = OZ_IOAPI
        jdate_prev = jdate( j )

        i_ = 0
        lat_ = 0.0d0
        lon_ = 0.0d0
        oz_  = 0.0d0
        do 490 i = 1,nlatitude
           i_ = i_ + 1
           lat_(i_) = real( lat_omi( i ),8 ) ! lat                 
           j_ = 0
          do 470 k = 1,nlongitude
             j_ = j_ + 1
             lon_(j_)   = real( lon_omi( k ),8 )  ! lon
             oz_(i_,j_) = max( -1.0d0, oz(i,k) )
470       continue
490     continue
        i_max = i_
        j_max = j_

        call expand_grid

        do i = 1, 2*nlatitude-1
           do k = 1, 2*nlongitude
              oz_extend( k, 2*nlatitude - i  ) = oz_expand( i,k )
           end do
        end do

!        IF ( .NOT. WRITE3( EXTEN_FILE_NCF, 'OZONE_COLUMN', JDATE( J ), 0,
!     &                     OZ_EXTEND ) ) THEN
!             XMSG = 'Error writing variable OZONE_COLUMN'
!             CALL M3EXIT ( 'RO3', JDATE( J ), 0, XMSG, XSTAT1 )
!       END IF


        If( CREATE_FULL_FILES )Then
           do i_ = 1, i_max
              if((j.eq.1).and.(i_.eq.1))then
                write(io_full_dat,545)latstepsize,lonstepsize
                write(io_full_dat,550)'  yeardate    lat      ',((lon_(j_)),j_=1,j_max) 
              endif
              write(io_full_dat,555)yrfrac_(j),lat_(i_),(idnint( oz_(i_,j_) ),j_=1,j_max)       
           end do
        End If

!        call o3tot_cmaq ( yrfrac_(j), lat_omi, lon_omi, oz_ )                  

!        do i = 1,359
!           write(unit_expand,555)yrfrac_(j),lat_expand(i),
!     &                          (idnint( oz_expand(i,k) ),k=1,720)       
!       end do

        call extract_o3_cmaq ( jdate(j), yrfrac_(j), lat_expand, lon_expand, oz_expand )
!       call viz_o3totcol ( jdate(j) )

        oz_prev  = oz

890   End Do Loop_Omi_Files 



545   format(2(f7.3,1x))
!550   format(7x,360f7.1)
550   format(a19,2880f9.3)       
!555   format(f6.1,1x,360f7.0) 
555   format(f10.4,f9.3,2880i7)
!      write(12,*)date(j)
     
      close(io_files)       
      if( CREATE_FULL_FILES )close(io_full_dat)
!      close(unit_expand)
999   stop
      CONTAINS
         Subroutine Init_Arrays()
            Implicit None


            Allocate( oz(nlatitude,nlongitude) ,oz_(nlatitude,nlongitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating oz,oz_'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( oz_mean(nlatitude,nlongitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating oz_mean'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( oz_prev(nlatitude,nlongitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating oz_prev'
              write(6,'(a)')xmsg
              Stop 
            End If 
            Allocate( oz_expand(2*nlatitude-1,2*nlongitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
               xmsg = 'error allocating oz_expand'
               write(6,'(a)')xmsg
               Stop 
            End If
            Allocate( lat_omi(nlatitude),lon_omi(nlongitude) , stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating lat_omi,lon_omi'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( phi_omi(nlatitude),theta_omi(nlongitude) , stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating phi_omi,theta_omi'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( lat_(nlatitude),lon_(nlongitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating lat_,lon_'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( oz_extend(2*nlongitude,2*nlatitude-1), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating oz_extend'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( oz_ioapi(nlongitude,nlatitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating oz_ioapi'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( ioapi_prev(nlongitude,nlatitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating ioapi_prev'
              write(6,'(a)')xmsg
              Stop 
            End If
            ioapi_prev = 0.0
            Allocate( ioapi_buff(nlongitude,nlatitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating ioapi_buff'
              write(6,'(a)')xmsg
              Stop 
            End If
            ioapi_buff = -1.0
            Allocate( oz_adjust(nlongitude,nlatitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating oz_adjust'
              write(6,'(a)')xmsg
              Stop 
            End If
            oz_adjust = 0.0
            Allocate( cloud_fraction(nlongitude,nlatitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating cloud_fraction'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( o3_missing(nlongitude,nlatitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating oz_missing'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( lat_ioapi(nlongitude,nlatitude), stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating lat_ioapi'
              write(6,'(a)')xmsg
              Stop 
            End If
            Allocate( lat_expand(2*nlatitude),lon_expand(2*nlongitude) , stat=stat_allocate )
            If ( stat_allocate  .ne. 0 ) Then
              xmsg = 'error allocating lat_expand,lon_expand'
              write(6,'(a)')xmsg
              Stop 
            End If

         End Subroutine Init_Arrays
         subroutine get_mean()
            Implicit None
            
            real(8), parameter :: zero_limit = 3.1416d0

            real(8), allocatable  :: weigth(:,:)
            
! local:        
            integer :: nlat
            integer :: nlon
            integer :: n,iread
            integer :: iozone
            integer :: line_number
            integer :: ilon

            integer :: mod_read
            integer :: rem_read

            real( 8 ) :: oz_min

            nlat = size(lat_omi)
            nlon = size(lon_omi)

            allocate( weigth(nlat,nlon) )
            
            oz_mean = 0.0d0
            mod_read = nlongitude/25
            rem_read = mod(nlongitude, 25)
            oz_toms = 0

            rewind(io_files)
            do n = 1,nfiles
              read(io_files,'(a)')OMI_filename(n)
              write(6,*)OMI_filename(n)
              open(file=OMI_filename(n),newunit = iozone)
              line_number = 0
              If( TOMS_FORMAT )Then
                 Do i = 1, 3
                    line_number = line_number + 1
                    read(iozone,'(a)',end=9503)file_line
                 End Do
                 Do i = nlat,1,-1
                    line_number = line_number + 1
                    ilon = 0
                    do iread = 1, mod_read
                       read(iozone,'(a)',err=9501,end=9503,advance='yes')file_line
                       read(file_line,'(1x,25i3)',err=9502)(oz_toms(ilon+k),k = 1, 25)
                       ilon = ilon + 25
                    end do
                    read(iozone,'(a)',err=9501,end=9503,advance='yes')file_line
                    read(file_line,'(1x,25i3)',err=9502)(oz_toms(ilon+k),k = 1, rem_read)
                    ilon = ilon + rem_read
                    oz(i,1:nlongitude) = real( oz_toms(1:nlongitude),8)
                 End Do
              Else
                 read(iozone,*)
                 oz = 0.d0
                 do i = nlat,1,-1
                    read(iozone,*)rowheader,(oz(i,k),k=1,nlon)
                 end do
              End If
             
             close(iozone)
             print*,'maxval(oz) = ', maxval(oz)
             do i = 1, nlat
               do j = 1, nlon
                  if( oz(i,j) .le. 1.0d-3 )cycle
                  weigth(i,j)  = 1.0d0 + weigth(i,j)
                  oz_mean(i,j) = oz(i,j) + oz_mean(i,j)
               end do
             end do
             
           end do
           oz_min = 1.0d8
           do i = 1, nlat
             do j = 1, nlon
                if( weigth(i,j) .le. 0.0d0 )cycle
                oz_mean(i,j) = oz_mean(i,j) / weigth(i,j)
                if( oz_mean(i,j) .Gt. 1 .And. oz_mean(i,j) .Lt. oz_min )oz_min = oz_mean(i,j)
             end do
          end do

          where( oz_mean .lt. oz_min ) oz_mean = oz_min

          do i = 1, nlat
             write(6,'(25(i3,1x))')(int(oz_mean(i,j)),j = 1, nlon)
          end do
          print*,'For mean, sum(weigth):maxval(weigth) = ',sum(weigth),":",maxval(weigth)
          rewind(io_files)
! fill in missing values with nearest neighbors
          if( near_neighbor ) then                      
            call fill(phi_omi, theta_omi, oz_mean, fill_limit)
          end if

          deallocate( weigth )
          return
9501      write(6,'(2a)')'Error reading file: ',Trim( OMI_filename(n) )
          write(6,'(a,i7)')'at line number: ',line_number
          Stop
9502      write(6,'(2a)')'Error reading file: ',Trim( OMI_filename(n) )
          write(6,'(a,i7)')'Cannot data at line number:',line_number
          write(6,'(a)')Trim(file_line)
          Stop
9503      write(6,'(2a)')'Premature File End in ',Trim( OMI_filename(n) )
          write(6,'(a,i7)')'at line number: ',line_number-1
          print*,'Last line read: ',Trim(file_line)
          write(6,'(a,i7)')'Expected number of lines: ',
     &    nlatitude*int(nlongitude/25)+nlatitude+3
          Stop
         end subroutine get_mean
         subroutine expand_init()
            Implicit None

            lat_expand = 0.0
            icount     = 0
            do i = 1, ( (size( lat_expand ) + 1)/2 - 1 ) ! 179
               icount = icount + 1
               lat_expand( icount ) = lat_omi( i )  
               icount = icount + 1
               lat_expand( icount ) = 0.5*(lat_omi( i )+ lat_omi( i+1 ))
            end do
            icount = icount + 1
            lat_expand( icount ) = lat_omi( i )  
            lon_expand = 0.0
            icount     = 0
            do i = 1, ( size( lon_expand )/2 - 1 ) ! 359
               icount = icount + 1
               lon_expand( icount ) = lon_omi( i )  
               icount = icount + 1
               lon_expand( icount ) = 0.5*(lon_omi( i )+ lon_omi( i+1 ))
            end do
            icount = icount + 1
            lon_expand( icount ) = lon_omi( nlongitude )  
            icount = icount + 1
            lon_expand( icount ) = 0.5*(lon_omi( nlongitude )+lon_omi( 1 ))+180.0

         end subroutine expand_init
         subroutine expand_grid()
            Implicit None
            icount = 0
            oz_expand = -1.0d0
            ipass = 1
            do i = 1,nlatitude
               ip1 = i + 1
               icount = icount + 1  
               jcount = 0
               do k = 1,nlongitude
                  jcount  = jcount + 1
                  kp1 = max(mod(k+1,nlongitude), 1)
                  oz_expand(icount,jcount)   = oz(i,k)
                  w = 0.5d0
                  if( oz(i,k)   .lt. 0.0d0 ) w(1) = 0.0d0
                  if( oz(i,kp1) .lt. 0.0d0 ) w(2) = 0.0d0
                  if( sum( w ) .gt. 1.0d-4 )then
                     oz_expand(icount,jcount+1) = (w(1)*oz(i,k)+w(2)*oz(i,kp1))
     &                                          / sum( w )
                  end if
                  if( ip1 .gt. nlatitude )cycle
                  w = 0.5d0
                  if( oz(ip1,k) .lt. 0.0d0 )w(1) = 0.0d0
                  if( oz(i,k)   .lt. 0.0d0 )w(2) = 0.0d0
                  if( sum( w ) .gt. 1.0d-4 )then
                      oz_expand(icount+1,jcount) = (w(1)*oz(ip1,k)+w(2)*oz(i,k))
     &                                           / sum( w )
                  end if 
                  v = 0.25d0
                  if( oz(i  ,k  ) .lt. 0.0d0 ) v(1) = 0.0d0
                  if( oz(ip1,k  ) .lt. 0.0d0 ) v(2) = 0.0d0
                  if( oz(i  ,kp1) .lt. 0.0d0 ) v(3) = 0.0d0
                  if( oz(i+1,kp1) .lt. 0.0d0 ) v(4) = 0.0d0
                  if( sum( v ) .gt. 1.0d-4 )then
                      oz_expand(icount+1,jcount+1) = (v(1)*oz(i,k)+v(2)*oz(ip1,k)
     &                                             +  v(3)*oz(i,kp1)+v(4)*oz(ip1,kp1))
     &                                             / sum( v )
                  end if 
                  jcount = jcount + 1
                end do
              icount = icount + 1  
           end do


555        format(f10.4,f7.1,720i7)
650        format(a19,720f7.1)       
         end subroutine expand_grid
      End Program Omi       
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
      print*,'OMI File List = ',rawfilename
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
      end        
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

        ldate2 = max( jdate2, jdate1 )
        ldate1 = min( jdate2, jdate1 )

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
  
      subroutine o3tot_cmaq ( date, latitude, longitude, ozone_omi )

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


      implicit none

! arguments
      real(8), intent( in ) :: date              ! date year plus fraction of year elapsed
      real,    intent( in ) :: latitude ( : )    ! latitude of point on earth's surface
      real,    intent( in ) :: longitude( : )    ! longitude of point on earth's surface
      real(8), intent( in ) :: ozone_omi( :, : ) ! omi total column ozone [DU]

! parameters

      integer, parameter :: nlat     = 17 
      integer, parameter :: nlon     = 17
      real,    parameter :: pi       = 3.14159265
      real,    parameter :: pi_180   = pi / 180.0

! local variables

      character( 32 ), save :: tmfile = 'OMI_CMAQ_DAT'
      character( 16 ), save :: pname = 'O3TOT_CMAQ'
      character( 96 ) :: xmsg = ' '
      character( 96 ) :: xmsgs( 3 )
      logical, save :: firsttime = .true.

      integer :: allocstat
      integer :: ilat
      integer :: ilon, jlon
      integer :: nlat_omi
      integer :: nlon_omi
      integer :: i, j
      integer :: icount
      integer :: ios
      integer :: nrecs
      integer :: jyear
      integer, save :: it
      integer, save :: io_unit
      integer, save :: logdev           ! output log unit number

      real, save :: lat( nlat )
      real, save :: lon( nlon )
      real, save :: lon_out( nlon )
      real       :: ozone( nlat, nlon ) ! interpolated total column ozone [DU]

      real :: flag( 4 )
      real :: dx1, dx2
      real :: dy1, dy2
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

      real, allocatable, save :: lon_transformed( : )


!----------------------------------------------------------------------

      if ( firsttime ) then
      
        firsttime = .false.

! Assign values to array of longitudes: lon

        do ilon = 1, nlon
          lon( ilon )     =  180.0 + 22.5 * real( ilon-1 )
          lon_out( ilon ) = -180.0 + 22.5 * real( ilon-1 )
        end do

        where( lon .GE. 360.0 ) lon = lon - 180.0

        do ilat = 1, nlat
          lat( ilat ) =   90.0 - 10.0 * real( ilat )
        end do

        open( file = tmfile, status = 'unknown', newunit = io_unit )
        write(io_unit,550)'  yeardate    lat      ',(lon_out(j),j=1,nlon) 

      else

        open( file = tmfile, status = 'unknown', position = 'append', newunit = io_unit )
!        endfile( io_unit )

      end if ! firsttime

545   format(2i7)
550   format(a19,360f7.1)       
555   format(f10.4,f7.1,360i7)
     

! Initialize


      flag  = 0.0
      ozone = 0.0
      latitudem = 0.0
      x2 = 0.0
      x3 = 0.0
      ozone = -1.0
      nlat_omi = size( latitude )
      nlon_omi = size( longitude )
      max_lat_omi = maxval( latitude )
      min_lat_omi = minval( latitude )


      if( .Not. allocated( lon_transformed ) )then
          allocate( lon_transformed ( nlon_omi ) )
      end if

      lon_transformed = lon
      where( lon_transformed .lt. 0.0 )lon_transformed = lon_transformed + 360.0
          
      do j = 1, nlon
! Analogously determine the proportionality x3 fot longitude

         x3loop: do ilon = 1,nlon_omi
           if( ilon .lt. nlon_omi ) then
              jlon = ilon + 1
           else 
              jlon = 1
           end if
           if( lon_transformed( jlon ) .lt. lon_transformed( ilon ) )then
               if ( ( lon( j ) .gt. lon_transformed( ilon ) ) .and.  (lon( j ) .lt. 360.0) )then
                  x3 = ( lon( j ) - lon_transformed( ilon ) )
     &               / ( (lon_transformed( jlon )+360.0) - lon_transformed( ilon ) )
                  dx1 = lon_transformed( ilon )
                  dx2 = lon_transformed( jlon )+360.0
                  exit x3loop
               else if( (lon( j ) .ge. 0.0 ) .and. ( lon( j ) .lt. lon_transformed( jlon )) )then
                  x3 = ( (lon( j )+360.0) - lon_transformed( ilon ) )
     &               / ( (lon_transformed( jlon )+360.0) - lon_transformed( ilon ) )
                  dx1 = lon_transformed( ilon )
                  dx2 = lon_transformed( jlon )+360.0
                  exit x3loop
               end if
           else
               if ( ( lon( j ) .gt. lon_transformed( ilon ) ) .and. 
     &              ( lon( j ) .le. lon_transformed( jlon ) ) ) then
                  x3 = ( lon( j ) - lon_transformed( ilon ) ) 
     &               / ( lon_transformed( jlon ) - lon_transformed( ilon ) )
                  exit x3loop
                  dx1 = lon_transformed( ilon )
                  dx2 = lon_transformed( jlon )
               end if
           endif
         end do x3loop

         do i = 1, nlat
! Handle case  where lat < min_lat_omi or lat > max_lat_omi by setting to missing
! value
            if ( lat( i ) .gt. max_lat_omi )then
!              latitudem = max_lat_omi
              cycle
            else if ( lat( i )  .lt. min_lat_omi ) then
!              latitudem = min_lat_omi
              cycle
            else
! use a dummy latitude variable latitudem so as to prevent overwriting latitude
              latitudem = lat( i )
             end if

! Identify the database latitudes that bound the requested latitude
! Determine the proportionality x2

            x2loop: do ilat = 1, nlat_omi-1
              if ( ( latitudem .le. latitude( ilat ) ) .and. 
     &             ( latitudem .ge. latitude( ilat+1 ) ) ) then
                x2 = ( latitudem - latitude( ilat+1 ) ) / (latitude( ilat ) - latitude( ilat+1 ) )
                exit x2loop
              end if
            end do x2loop
!               print*,'ilon, ilon+1, ozone_omi( 1, ilon+1 ) = ',ilon, ilon+1, lon(j)
!               print*,'ilon, ilon+1, ozone_omi( 1, ilon+1 ) = ',ilon, ilon+1, ozone_omi( 1, ilon+1 )
! Determine the interpolated ozone, with consideration that some of the 8 ozone values
! of the data cube may be missing.  Construct the estimate from those values that are     ! available
            if ( ozone_omi( ilat, jlon ) .le. 0.0d0 ) then
!               ozone_omi( ilat, ilon+1 ) = 0.0
              flag( 1 ) = 0.0
            else
              flag( 1 ) = 1.0 ! x2 * x3
            end if

            if ( ozone_omi( ilat, ilon ) .le. 0.0d0 ) then
!              ozone_omi( ilat, ilon ) = 0.0
              flag( 2 ) = 0.0
            else
              flag( 2 ) = x2 * ( 1.0 - x3 )
            end if

            if ( ozone_omi( ilat+1, jlon ) .le. 0.0d0 ) then
!              ozone_omi( ilat+1, ilon+1 ) = 0.0
              flag( 3 ) = 0.0
            else
              flag( 3 ) = ( 1.0 - x2 ) * x3 !* cos( latitudem )
            end if

            if ( ozone_omi( ilat+1, ilon ) .le. 0.0d0 ) then
!              ozone_omi( ilat+1, ilon ) = 0.0
              flag( 4 ) = 0.0
            else
              flag( 4 ) = ( 1.0 - x2 ) * ( 1.0 - x3 ) ! * cos( latitudem )
            end if

            ozone( i , j) = flag( 1 ) * real( ozone_omi( ilat  , jlon ) )
     &                    + flag( 2 ) * real( ozone_omi( ilat  , ilon   ) )
     &                    + flag( 3 ) * real( ozone_omi( ilat+1, jlon ) )
     &                    + flag( 4 ) * real( ozone_omi( ilat+1, ilon   ) )

            total = sum( flag ) 

! When no contiguous and conterminous data are available, default to -1 or missing value:

            if ( total .le. 0.0 ) then
              ozone( i,j ) =  -1
            else
              ozone( i,j ) = ozone( i,j ) / total
            end if

899         if ( ozone( i,j ) .lt. 0.0 ) then
              ozone( i,j ) = -1
            else if ( ozone( i,j ) .gt. 600.0 ) then
              ozone( i,j ) = 600.0
            end if
      
         end do
      end do

      do 590 i = 1,nlat          
       write(io_unit,555)date,lat(i),(nint( ozone(i,j) ),j=1,nlon)       
590   continue

      close( io_unit )
      return

      end subroutine o3tot_cmaq
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
      integer :: delta_julian
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
           print*,'j,lon( j ) = ',j,lon( j )
        end do

        dy2 = ( 180.0 - 2.0*lat_window )/ real( nlat - 1 )
        do ilat = 1, nlat
          lat( ilat ) =   90.0 - dy2 * real( ilat - 1  ) - lat_window
        end do

        open( file = OMI_CMAQ_DAT, status = 'unknown', newunit = io_unit )
        write(io_unit,549)'nlat',nlat
        write(io_unit,549)'nlon',nlon
        write(io_unit,550)'yeardate','latitude',(lon_out(j),j=1,nlon) 

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
         print*,'j,lon_transformed( j ) = ',j,lon_transformed( j )
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
           print*,'lon_transformed( jlon ),lon( j ) = ', lon_transformed( jlon( j ) ),lon( j )
           print*,'longitude( jlon ),lon( j       ) = ', longitude( jlon( j ) ),lon_out( j )
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
           print*,'latitude( jlat ),lat( i ) = ', latitude( jlat( i ) ),lat( i )
        end do 
! create ioapi for visualization
         call CREATE_CMAQ_OMI ( OMI_CMAQ_NCF, jdate, lat, lon_out )
         jdate_expect = jdate
      else
        call Julian_plus_One( jdate_expect )
        open( file = OMI_CMAQ_DAT, status = 'unknown', position = 'append', newunit = io_unit )

      end if ! firsttime


545   Format(2i7)
549   Format(a4,1x,i7)
550   Format(2(a,tr1),361(f7.1,tr1))       
555   Format(f9.4,tr1,f7.1,tr1,361(i7,tr1))
     

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
         delta_date = Delta_julian( jdate_expect, jdate )
         viz_adjust = ( ozone_viz - viz_prev )/real(delta_date + 1)

         Do j = 1, delta_date
            viz_prev = viz_prev + viz_adjust
            If ( .not. write3( OMI_CMAQ_NCF, 'OZONE_COLUMN', jdate_expect, 0,
     &                           viz_prev ) ) THEN
                   xmsg = 'Error writing variable OZONE_COLUMN'
                  call m3exit ( pname, jdate_expect, 0, xmsg, xstat1 )
            End If
            call Julian_plus_One( jdate_expect )
         End Do
      End If

      If ( .not. write3( OMI_CMAQ_NCF, 'OZONE_COLUMN', jdate, 0,
     &                        ozone_viz ) ) THEN
             xmsg = 'Error writing variable OZONE_COLUMN'
             call m3exit ( pname, jdate, 0, xmsg, xstat1 )
      End If

      do i = 1,nlat          
        write(io_unit,555)date,lat(i),(nint( ozone(i,j) ),j=1,nlon)       
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
                    print*,'invalid at i,j = ',i,j
                    cycle
                else
                    print*,'invalid at i,j = ',i,j
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
