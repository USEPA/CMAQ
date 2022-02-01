      program omi

      USE m3utilio
      USE ENV_VARS
      USE utilities_module

      implicit none
      
      character(18)  :: rowheader
      character(256), allocatable :: OMI_filename( : )
      character(256) :: file_name
      character(256) :: file_line
      character(16)  :: OMI_FILE_NCF   = 'OMI_FULL_NCF'
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
!      integer       delta_julian
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

      logical            :: eflag         = .False.
      logical, save      :: First_time    = .True.
      logical, parameter :: near_neighbor = .False. ! replace missing using fill subroutine
      logical            :: read_clouds   = .False.
      logical            :: TOMS_FORMAT   = .False.

      interface
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
           delta_date =  Delta_Julian( jdate( j ),jdate( j-1 ) )
           If( delta_date .gt. 1 )Then
              Print*,'Data gap from ',jdate( j-1 ),' to ', jdate( j ) 
           Else If( delta_date .eq. 0 )Then
              write(6,'(a,2(i1000,1x))')
     &       'Input file list has files with equal dates betweeen lines:', j-1,j
              eflag = .true.
           Else If( delta_date .lt. 0 )Then
              write(6,'(a,2(i1000,1x))')
     &       'Input file list has files with decreasing dates betweeen lines:', j-1,j
              eflag = .true.
           End If
        End If
        yrfrac_(j) = yrfrac+real(year,8)
      End Do
      
      If( eflag )Then
         write(6,'(a)')'Above errors found in OMI_FILE_LIST'
         Stop
      End If 

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


!       do i = 1, nlatitude
!          print*,'lat_omi( i ), phi_omi( i ) = ',lat_omi( i ), phi_omi( i ) 
!       end do 
!          print*,'lat_omi( 1 ), phi_omi( 1 ) = ',lat_omi( 1 ), phi_omi( 1 ) 



       do i = 1, nlongitude
        lon_omi( i ) = -180.0 - init_lon + lonstepsize*real(i)
       end do
       do i = 1, (nlongitude/2)
        theta_omi( i ) = real(pi180*(lon_omi( i ) + 360.0), 8)
        k = i + (nlongitude/2)
        theta_omi( k ) = real(pi180*lon_omi( k ), 8)
       end do
       
!       do i = 1, nlongitude
!          print*,'lon_omi( i ), theta_omi( i ) = ',lon_omi( i ), theta_omi( i ), lonstepsize
!       end do 
!         print*,'lon_omi( 1 ), theta_omi( 1 ) = ',lon_omi( 1 ), theta_omi( 1 ), lonstepsize
       
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

!        print*,'oz_ioapi:oz_mean max/min: ',maxval(oz_ioapi),'/',maxval(oz_mean),minval(oz_ioapi),
!     &                               '/',minval(oz_mean)

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
         write(6,'(a)')OMI_filename(j)
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
            If( TOMS_FORMAT )oz_toms = 0

            rewind(io_files)
            do n = 1,nfiles
              read(io_files,'(a)')OMI_filename(n)
              write(6,'(a)')OMI_filename(n)
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

!          do i = 1, nlat
!             write(6,'(25(i3,1x))')(int(oz_mean(i,j)),j = 1, nlon)
!          end do
!          print*,'For mean, sum(weigth):maxval(weigth) = ',sum(weigth),":",maxval(weigth)
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
