       MODULE BIN_DATA

          IMPLICIT NONE

          integer, parameter :: NBO = 100
          integer, parameter :: NSO = 40000
!          integer, parameter :: NSO = 4463
          integer, parameter :: NZO = 13550
          integer, parameter :: NJO = 18

          INTEGER, SAVE      :: N_LOW_JXBANDS = 11 ! band structure for CMAQ release models
!          INTEGER, SAVE      :: N_LOW_JXBANDS = 6 ! band structure for MPAS-CMAQ models
          INTEGER, SAVE      :: NJO_NEW 
          INTEGER, SAVE      :: N_INLINE_BAND 

          INTEGER, SAVE      :: NB, J1, J2, K1, K2  ! array limits and markers
          INTEGER, SAVE      :: NB_NEW, J1_NEW, J2_NEW, K1_NEW, K2_NEW ! array limits and markers

          REAL(8), SAVE    :: SRB(15,NJO)
          REAL(8), SAVE    :: WBIN(NBO + 1 )
          INTEGER, SAVE    :: IJX(NBO)
          INTEGER, SAVE    :: IBINJ(NSO)

          REAL(8), ALLOCATABLE, SAVE    :: WBIN_NEW( : )
          INTEGER, SAVE                 :: IBINJ_NEW(NSO)

          REAL, ALLOCATABLE, SAVE :: W(:), F(:)
          REAL, ALLOCATABLE, SAVE :: WL(:), WU(:), WC(:)

! FASTJX has 18 bins but the wavelength interval of the bins can overlap

          REAL, SAVE   :: STR_WV_FASTJX( NJO + 2 )  ! nm, starting wavelength of FASTJ intervals
          DATA STR_WV_FASTJX  
     &  /  177.5,  177.5, 177.5, 177.5, 202.5, 206.5, 209.5,  
     &     212.5,  215.5, 221.5, 233.0, 275.5, 286.5, 291.0,
     &     298.3,  307.5, 312.5, 320.3, 345.0, 412.5 /

          REAL, SAVE    :: END_WV_FASTJX( NJO + 2 )  ! nm, ending wavelength of FASTJ intervals
          DATA END_WV_FASTJX  
     &  /  202.5,  202.5, 202.5, 202.5, 206.5, 209.5,  212.5,  
     &     215.5,  221.5, 233.0, 275.5, 286.5, 291.0,  298.3,  
     &     307.5,  312.5, 320.3, 345.0, 412.5, 850.0 /

         INTEGER, SAVE  :: FASTJX_BIN( NJO + 2 )   ! FASTJX bin number for the intervals
         DATA FASTJX_BIN 
     &  /      1,      2,     3,     4,     5,     6,       7,
     &         8,     11,    10,     9,    10,    11,      12,
     &        13,     14,    15,    16,    17,    18 /
! Effective Center wavelengths for 18 bins of FASTJX, not mean bin edges
!w-eff (nm)      187.      191.      193.      196.      202.      208.
!                211.      214.      261.      267.      277.      295.
!                303.      310.      316.      333.      380.      574.

          REAL, SAVE   :: EFF_WV_FASTJX( NJO )  ! nm 
          DATA EFF_WV_FASTJX  
     &  / 187.0,   191.0, 193.0, 196.0, 202.0, 208.0,
     &    211.0,   214.0, 261.0, 267.0, 277.0, 295.0,
     &    303.0,   310.0, 316.0, 333.0, 380.0, 574.0 /

          REAL, ALLOCATABLE, SAVE   :: EFFECTIVE_LAMBDA( : ) ! nm
          REAL, ALLOCATABLE, SAVE   :: EFFECTIVE_WVNUMB( : ) ! 1/cm
	  
          REAL, ALLOCATABLE, SAVE   :: SOLAR_PHOTONS(    : ) ! solar photon flux in bin, photons/cm2/s
  
          INTEGER, PARAMETER :: NWV_REGRESS = 27

          REAL, SAVE  ::  ENDWL_REGRESS( nwv_regress )       ! wavelength band upper limit
          REAL, SAVE  ::  MIDWL_REGRESS( nwv_regress )       ! wavelength midpoints
          REAL, SAVE  ::  STWL_REGRESS(  nwv_regress )       ! wavelength band lower limit

          REAL, ALLOCATABLE, SAVE     :: ENDWL_NEW( : )       ! wavelength band upper limit
          REAL, ALLOCATABLE, SAVE     :: MIDWL_NEW( : )       ! wavelength midpoints
          REAL, ALLOCATABLE, SAVE     :: STWL_NEW(  : )       ! wavelength band lower limit
          INTEGER, ALLOCATABLE, SAVE  :: NEWX_BIN(  : )       ! bin number for the intervals
          INTEGER, ALLOCATABLE, SAVE  :: IJX_CALC(:)
          INTEGER, ALLOCATABLE, SAVE  :: IJX_BIN_NEW(:)
          REAL(8), ALLOCATABLE, SAVE  :: SRB_NEW(:, :)

          INTEGER, SAVE               :: NWV_NEW              ! number of new bins

!          LOGICAL, PARAMETER          :: USE_REGRESS = .TRUE. ! .TRUE. ! .FALSE.
           LOGICAL, PARAMETER          :: USE_REGRESS = .FALSE. ! .TRUE. ! .FALSE.
           LOGICAL, SAVE               :: CHANGE_WBIN = .FALSE. 


!
       CONTAINS

        SUBROUTINE INIT_BIN_DATA()

          USE GET_ENV_VARS

          IMPLICIT NONE

          INTEGER            :: I, J, K, L             ! index counters
          INTEGER            :: STRT, FINI
          INTEGER            :: ITT_CALC
          INTEGER            :: STAT

          LOGICAL, SAVE      :: DEFINED  = .FALSE.

          REAL(8), PARAMETER    :: TINY = 0.06D0
          REAL(8), ALLOCATABLE  :: FBIN( : )
          REAL(8), ALLOCATABLE  :: FBIN_AVE( : )
          REAL(8), ALLOCATABLE  :: ABIN( : )
          INTEGER, ALLOCATABLE  :: SBIN( : )

          REAL(8), ALLOCATABLE   :: FFBIN( : )
          REAL(8), ALLOCATABLE   :: FFBIN_AVE( : )
          REAL(8), ALLOCATABLE   :: AABIN( : )
          REAL,    ALLOCATABLE   :: SSBIN( : )
          REAL(8)                :: DELTA

          CHARACTER(16) :: PNAME = ' INIT_BIN_DATA '

          CHARACTER(255) :: EQNAME
          CHARACTER(586) :: EQFILE
          INTEGER        :: LASTNB1
          INTEGER        :: LASTNB2
          INTEGER        :: IOUNIT = 125
          
          CHARACTER(586) :: WVBIN_FILE = 'WVBIN_FILE'       ! 'wavel-bins.dat'
          CHARACTER(586) :: FLUX_FILE  = 'FLUX_FILE'        ! 'solar-p05nm-UCI.dat'
          CHARACTER(16)  :: NBANDS_OUT = 'N_WAVEBANDS_OUT'  ! Number of wavebands for output files



          IF( DEFINED )RETURN

          DEFINED = .TRUE.

! get path to wavelength bin data file          
          EQNAME = TRIM( WVBIN_FILE )
          CALL VALUE_NAME( EQNAME,  WVBIN_FILE )
          OPEN (UNIT  = IOUNIT, FILE= WVBIN_FILE, STATUS='OLD')

          SRB = 0.0D0
          read(IOUNIT,'(i5)') NB

          if (NB .gt. NBO)THEN
             PRINT*,'NB exceeds ',NBO,' in file wavel-bins.dat '
             stop
          ENDIF

          read(IOUNIT,'(5x,f8.3)') (WBIN(I), I=1,NB+1)
          read(IOUNIT,*)
          read(IOUNIT,*)
          read(IOUNIT,'(2x,15f5.1)') ((SRB(I,J),I=1,15),J=1,8)
          read(IOUNIT,*)
          read(IOUNIT,'(5x,i5)') (IJX(I),I=16,NB)
          close (IOUNIT)

!          write(6,'(2x,15f5.1)') ((SRB(I,J),I=1,15),J=1,NJO)

          IF( USE_REGRESS )CHANGE_WBIN = .TRUE.
          
          IF( CHANGE_WBIN )THEN

             STRT = 64
             FINI = NB

             DELTA = 0.50D0
             L = INT((WBIN(FINI) - WBIN(STRT))*DELTA) 

             NB_NEW = STRT +  L  + ( NB - FINI ) 

             ALLOCATE( WBIN_NEW( NB_NEW + 1 ) )
          
             WBIN_NEW = 0.0D0
 
             WBIN_NEW( 1 : STRT ) = WBIN( 1 : STRT )
          
             DO I = (STRT + 1), (L + STRT + 1)
                WBIN_NEW( I ) = WBIN_NEW( I - 1 ) + DELTA
             ENDDO

             DO I = 1, (NB - FINI)
                WBIN_NEW( I + L + STRT + 1) = WBIN( I + FINI  ) 
             ENDDO
              
              IF( WBIN_NEW( NB_NEW ) .NE. WBIN(NB) )THEN
                  WBIN_NEW( NB_NEW ) = WBIN(NB)
              ENDIF
              WBIN_NEW( NB_NEW + 1 ) = WBIN(NB+1)

              ALLOCATE( FBIN( NB_NEW + 1 ) )
              ALLOCATE( FBIN_AVE( NB_NEW + 1 ) )
              ALLOCATE( ABIN( NB_NEW + 1 ) )
              ALLOCATE( SBIN( NB_NEW + 1 ) )
              ALLOCATE(  IJX_BIN_NEW(  NB_NEW + 1 )   ) 
              ALLOCATE(  IJX_CALC(  NB_NEW + 1 )   ) 

           ELSE
              
              NB_NEW = NB
              
              ALLOCATE( WBIN_NEW( SIZE( WBIN ) ) )            
              ALLOCATE( FBIN( NBO ) )
              ALLOCATE( FBIN_AVE( NBO ) )
              ALLOCATE( ABIN( NBO ) )
              ALLOCATE( SBIN( NBO ) )
              ALLOCATE(  IJX_BIN_NEW(  NBO )   ) 
              ALLOCATE(  IJX_CALC(  NBO )   ) 

              WBIN_NEW = WBIN 

           ENDIF

          WRITE(6,'(I4,2X,F7.2,2X,F7.2)')(I,WBIN_NEW(I),WBIN_NEW(I+1),I=1,NB_NEW)


          ALLOCATE(W(NSO), F(NSO))

! get path to flux spectrum data file          
          EQNAME = TRIM( FLUX_FILE )
          CALL VALUE_NAME( EQNAME,  FLUX_FILE )
          open (UNIT  = IOUNIT, file = FLUX_FILE, status = 'OLD')
          read(IOUNIT,*)
          read(IOUNIT,*)
          read(IOUNIT,'(f10.4,e10.3)') (W(J),F(J), J=1,NSO)
          close (IOUNIT)

         ALLOCATE(WC(NSO), WL(NSO), WU(NSO))

         I = 1
         WC( 1 ) = REAL( W( 1 ) )
         WL( 1 ) = REAL( W( 1 ) - 0.5D0*( W(2)-W(1) ) )
         WU( 1 ) = REAL( 0.5D0*(W( 2 ) + W( 1 )) )

         DO I = 2, NSO-1
            WC( I ) = REAL(W( I ))
            WL( I ) = REAL(0.5D0*(W( I ) + W(I-1)))
            WU( I ) = REAL(0.5D0*(W(I+1) + W( I )))
         ENDDO
          
         I = NSO
         WC( NSO ) = REAL( W( NSO ) )
         WU( NSO ) = REAL( W(NSO) + 0.5D0*( W(NSO)-W(NSO-1) ) )
         WL( NSO ) = REAL(0.5*(W( NSO ) + W(NSO-1)))


!         DO I = 1, NSO
!            WC( I ) = REAL(W( I ))
!            WL( I ) = WC( I ) - 0.025
!            WU( I ) = WC( I ) + 0.025
!         ENDDO
          

! initialize the regress wavelength bins
         STWL_REGRESS(1)  = 287.5
         MIDWL_REGRESS(1) = 290.0
         ENDWL_REGRESS(1) = 292.5

         do i = 2, 23
            STWL_REGRESS(i) = STWL_REGRESS(i-1) + 5.0
            MIDWL_REGRESS(i) = MIDWL_REGRESS(i-1) + 5.0
            ENDWL_REGRESS(i) = ENDWL_REGRESS(i-1) + 5.0
         enddo

         STWL_REGRESS(24)  = ENDWL_REGRESS(23)
         MIDWL_REGRESS(24) = 450.0
         ENDWL_REGRESS(24) = 500.0
         do i = 25, nwv_regress
            STWL_REGRESS(i)  = ENDWL_REGRESS(i-1)
            MIDWL_REGRESS(i) = MIDWL_REGRESS(i-1) + 100.0
            ENDWL_REGRESS(i) = ENDWL_REGRESS(i-1) + 100.0
         enddo

         do i = 1, nwv_regress
            WRITE(6,'(i3,2(2x,f6.2))')i,STWL_REGRESS(i),ENDWL_REGRESS(i)
         enddo



! find where regress bins start and stop in FASTJX bins

         if( STR_WV_FASTJX( 1 ) .gt. STWL_REGRESS(1) )THEN
             print*,'ERROR 1 in finding starting point '
             stop
         endif

         if( END_WV_FASTJX( NJO + 2 ) .lt. STWL_REGRESS(1) )THEN
             print*,'ERROR 2 in finding starting point '
             stop
         endif
     
         LOOP_START: do J = 1, NJO + 2
             if( STR_WV_FASTJX( J ) .ge. STWL_REGRESS(1) )THEN
                 STRT = J - 1
                 EXIT LOOP_START
             ENDIF
            if( J .eq. (NJO + 2) )THEN
               print*,'ERROR 3 in finding starting point '
               stop
            endif
         ENDDO LOOP_START


         FINI = NJO + 2
         LOOP_STOP: do K = 1, NJO + 2
             if( END_WV_FASTJX( J ) .gt. ENDWL_REGRESS(nwv_regress) )THEN
                 FINI = K 
                 EXIT LOOP_STOP
             ENDIF
         ENDDO LOOP_STOP


         IF( FINI .EQ. ( NJO + 2 ) )THEN

             IF(INT(END_WV_FASTJX(FINI))
     &           .NE.INT(ENDWL_REGRESS(nwv_regress)))THEN
                print*,'Resetting ENDWL_REGRESS(last) to END_WV_FASTJX(last) '
                print*,' INT(END_WV_FASTJX(FINI)) = ',INT(END_WV_FASTJX(FINI))
                print*,' INT(ENDWL_REGRESS(nwv_regress)) = ',
     &                INT(ENDWL_REGRESS(nwv_regress))
                ENDWL_REGRESS(nwv_regress) = END_WV_FASTJX(FINI)
             ENDIF
      
         ENDIF
       
         IF( USE_REGRESS )THEN
             NWV_NEW = STRT + nwv_regress + ( NJO + 2 - FINI )
             NJO_NEW = NWV_NEW - 2
         ELSE
             NWV_NEW = NJO + 2 
             NJO_NEW = NJO
         ENDIF

         N_INLINE_BAND = NJO_NEW - N_LOW_JXBANDS
         N_INLINE_BAND = GET_ENV_INT( NBANDS_OUT, ' ', N_INLINE_BAND, STAT)
         IF( STAT .EQ. 0 )THEN
             N_LOW_JXBANDS = NJO_NEW - N_INLINE_BAND
         END IF

         IF( N_INLINE_BAND .LE. 0 )THEN
             WRITE(6,*)TRIM(PNAME) // 'ERROR: ' // TRIM( NBANDS_OUT ) //
     &                                '  <= 0 '
             WRITE(6,*)'Total number of bands = ',NJO_NEW
!            WRITE(6,*)'Number of high frequency bands = ', N_LOW_JXBANDS
             WRITE(6,*)'Requested Number of Wavebands = ',N_INLINE_BAND
             STOP
         ELSE IF( N_INLINE_BAND .GT. NJO_NEW )THEN
             WRITE(6,*)TRIM(PNAME) // 'ERROR: ' // TRIM( NBANDS_OUT ) // 
     &                                ' greater Total Number of Bands'
             WRITE(6,*)'Total number of bands = ',NJO_NEW
!            WRITE(6,*)'Number of high frequency bands = ', N_LOW_JXBANDS
             WRITE(6,*)'Requested Number of Wavebands = ',N_INLINE_BAND
             STOP
         ELSE
            WRITE(6,*)'Requested Number of Wavebands = ',N_INLINE_BAND
            WRITE(6,*)'Total number of bands = ',NJO_NEW
         END IF
      
         ALLOCATE( FFBIN(     NJO_NEW ) )
         ALLOCATE( FFBIN_AVE( NJO_NEW ) )
         ALLOCATE( AABIN(     NJO_NEW ) )
         ALLOCATE( SSBIN(     NJO_NEW ) )

         
         ALLOCATE(  STWL_NEW  ( NWV_NEW ) )      
         ALLOCATE(  MIDWL_NEW ( NWV_NEW ) )      
         ALLOCATE(  ENDWL_NEW ( NWV_NEW ) ) 
         ALLOCATE(  NEWX_BIN  ( NWV_NEW ) ) 
         ALLOCATE(  SRB_NEW   ( 15, NJO_NEW ) )

         MIDWL_NEW = 0.0

         ALLOCATE(  EFFECTIVE_LAMBDA( NJO_NEW ) )
         ALLOCATE(  EFFECTIVE_WVNUMB( NJO_NEW ) )
         ALLOCATE(  SOLAR_PHOTONS(    NJO_NEW ) )

       IF( USE_REGRESS )THEN

         IF( FINI .EQ. ( NJO + 2 ) )THEN 
         
             STWL_NEW( 1:STRT )            = STR_WV_FASTJX( 1:STRT )
             STWL_NEW( (STRT+1):NWV_NEW )  = STWL_REGRESS( 1:nwv_regress )

             ENDWL_NEW( 1:(STRT-1) )       = END_WV_FASTJX( 1:(STRT-1) )
             ENDWL_NEW( STRT )             = STWL_REGRESS( 1 )
             ENDWL_NEW( (STRT+1):NWV_NEW ) = ENDWL_REGRESS( 1:nwv_regress )

             NEWX_BIN( 1:STRT )            = FASTJX_BIN(1:STRT )
             DO I = STRT+1, NWV_NEW
                NEWX_BIN( I ) = NEWX_BIN( I - 1 ) + 1
             ENDDO
                

         ELSE

             J = STRT + nwv_regress
             STWL_NEW( 1:STRT )      = STR_WV_FASTJX( 1:STRT )
             STWL_NEW( (STRT+1): J ) = STWL_REGRESS( 1:nwv_regress )
             STWL_NEW( J + 1 )     = ENDWL_REGRESS( nwv_regress )
             STWL_NEW( (J+2):NWV_NEW )  =  STR_WV_FASTJX( FINI:(NBO + 2 ) )

          
             ENDWL_NEW( 1:(STRT-1) )   = END_WV_FASTJX( 1:(STRT-1) )
             ENDWL_NEW( STRT )         = STWL_REGRESS( 1 )
             ENDWL_NEW( (STRT+1):J )   = ENDWL_REGRESS( 1:nwv_regress )
             ENDWL_NEW( J+1:NWV_NEW )  = END_WV_FASTJX( FINI:(NBO + 2) )

             NEWX_BIN( 1:STRT )        = FASTJX_BIN(1:STRT )
             DO I = STRT+1, NWV_NEW
                NEWX_BIN( I ) = NEWX_BIN( I -1 ) + 1
!                print*,i,NEWX_BIN( I )
             ENDDO

         ENDIF


       ELSE

          STWL_NEW   = STR_WV_FASTJX
          ENDWL_NEW  = END_WV_FASTJX
          NEWX_BIN   = FASTJX_BIN
 
       ENDIF

       print*,NWV_NEW 
       do i = 1, NWV_NEW
          J = i ! NEWX_BIN(I)
          MIDWL_NEW( J ) = 0.5*( STWL_NEW(J) + ENDWL_NEW(J) )
!          WRITE(6,'(i3,3(2x,f6.2),2x,i3)')i,STWL_NEW(J),MIDWL_NEW( J ),
!     &                                      ENDWL_NEW(J),NEWX_BIN(I)
       enddo

       IJX_CALC = 0
  
        DO I = 16, NB_NEW
           DO J = 1, NJO + 2
              IF(WBIN_NEW(I)+TINY .GE. STR_WV_FASTJX(J) 
     &                 .AND. WBIN_NEW(I)-TINY .LT. END_WV_FASTJX(J))THEN
                   IJX_CALC( I ) = FASTJX_BIN ( J )
              ENDIF
           ENDDO
           if(NB_NEW .EQ. NB )THEN
              print*,I,' IJX_CALC(I) - IJX(I) = ', IJX_CALC(I) - IJX(I)
           ENDIF
           DO J = 1, NWV_NEW
              IF(WBIN_NEW(I)+TINY .GE. STWL_NEW(J) 
     &                 .AND. WBIN_NEW(I)-TINY .LT. ENDWL_NEW(J))THEN
                   IJX_BIN_NEW( I ) = NEWX_BIN ( J )
              ENDIF
           ENDDO
        ENDDO
!        print*,'IJX for FASTJX bins'
!        WRITE(6,'(i5,2x,i5,2x,F6.2,2x,F6.2)') 
!     &       (I,IJX_CALC(I),WBIN_NEW(I),WBIN_NEW(I+1),I=16,NB_NEW)
!        print*,'IJX for New bins'
!        WRITE(6,'(i5,2x,i5,2x,F6.2,2x,F6.2)') 
!     &       (I,IJX_BIN_NEW(I),WBIN_NEW(I),WBIN_NEW(I+1),I=16,NB_NEW)




 
       SRB_NEW = 0.0D0
       DO I = 1, 15
          DO J = 1, 8
            SRB_NEW( I, J) = SRB( I, J)
          ENDDO
       ENDDO


C---now assign bin_new #(I=1,NB_NEW) to each p05nm microbin J (1:40000)

        IBINJ_NEW = 0
        do I=1,NB_NEW
           do J=1,NSO
              if (W(J) .gt. WBIN_NEW(I)) goto 110
           enddo
           J      = NSO + 1
110        J1_NEW = J
           do J=J1_NEW,NSO
              if (W(J) .gt. WBIN_NEW(I+1)) goto 120
           enddo
           J      = NSO + 1
120        J2_NEW = J-1
           do J=J1_NEW,J2_NEW
              IBINJ_NEW(J) = I
           enddo
        enddo

c---now assign bin #(I=1:77) to each p05nm microbin J (1:40000)

        IBINJ = 0
        do I=1,NB
           do J=1,NSO
              if (W(J) .gt. WBIN(I)) goto 11
           enddo
           J = NSO + 1
11         J1 = J
           do J=J1,NSO
              if (W(J) .gt. WBIN(I+1)) goto 12
           enddo
           J = NSO + 1
12         J2 = J-1
           do J=J1,J2
              IBINJ(J) = I
           enddo
        enddo

         K1 = 1
         K2 = NSO

         K1_NEW = 1
         K2_NEW = NSO

c--- find flux-weighted effective wavelength over the bins
         FBIN(:) = 0.0d0
         ABIN(:) = 0.0d0  
         SBIN(:) = 0
         do J=K1,K2
            K = J - K1 + 1
            I = IBINJ_NEW(J)
            if (I .gt. 0) then
               FBIN(I) = FBIN(I) + F(J)
               ABIN(I) = ABIN(I) + F(J)*REAL(1.0/WC(J), 8)
               SBIN(I) = SBIN(I) + 1
           endif
         enddo

         do I=1,NB_NEW
            if (FBIN(I) .gt. 0.d0)ABIN(I) = ABIN(I)/FBIN(I)
            if (SBIN(I) .gt. 0)FBIN_AVE(I)= FBIN(I)/REAL(SBIN(I),8)
         enddo


         FFBIN(:)       = 0.0D0
         AABIN(:)       = 0.0D0
         FFBIN_AVE( : ) = 0.0D0
         SSBIN(:)       = 0.0

         do I=16,NB_NEW
!            J = IJX(I)
!            J = IJX_CALC(I)
            J = IJX_BIN_NEW( I )
            FFBIN(J) = FFBIN(J) + FBIN(I)
            AABIN(J) = AABIN(J) + FBIN(I)*ABIN(I)
            FFBIN_AVE(J) = FFBIN_AVE(J) + FBIN_AVE(I)
            SSBIN(J)     =    1         + SSBIN(J)
         enddo


         do I=1,15
            do J=1,NJO_NEW
               FFBIN(J) = FFBIN(J) + FBIN(I)*SRB_NEW(I,J)
               AABIN(J) = AABIN(J) + FBIN(I)*ABIN(I)*SRB_NEW(I,J)
               SSBIN(J)     = SRB_NEW(I,J) + SSBIN(J)
               FFBIN_AVE(J) = FFBIN_AVE(J) + FBIN_AVE(I)*SRB_NEW(I,J)
            enddo
        enddo

        do J=1,NJO_NEW
           if(SSBIN(J) .gt. 0.0)FFBIN_AVE(J) = FFBIN_AVE(J)/REAL(SSBIN(J),8)
           if(FFBIN(J) .gt. 0.d0)THEN
	          EFFECTIVE_LAMBDA(J) = REAL(FFBIN(J)/AABIN(J))
	          EFFECTIVE_WVNUMB(J) = 1.0E7 / EFFECTIVE_LAMBDA(J)
	       end if
           WRITE(6,'(A18,I3,A4,F6.1,2X,A25,ES12.4)')
     &     'EFFECTIVE_LAMBDA(',J,') = ',EFFECTIVE_LAMBDA(J),
     &     'Mean Solar Photons = ',FFBIN_AVE(J)
            SOLAR_PHOTONS( J ) = FFBIN(J)
        enddo

        print*,' completed INIT_BIN_DATA '
 
           
           RETURN
        END SUBROUTINE INIT_BIN_DATA
       END MODULE BIN_DATA
