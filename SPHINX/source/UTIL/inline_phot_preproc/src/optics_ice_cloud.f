       SUBROUTINE OPTICS_ICE_CLOUD(CLOUD_DIAMETER_ICE, ICE_EXT, ICE_ASY, ICE_SSA, ICE_DEL)

         USE GET_ENV_VARS
         USE BIN_DATA
         USE CSQY_PARAMETERS

         IMPLICIT NONE

         REAL, INTENT( IN )  :: CLOUD_DIAMETER_ICE   ! effective ice droplet size or diameter, um
         REAL, INTENT( OUT ) :: ICE_EXT( : )         ! (cloud extinction coef)/IWC, 1/m*/(g/m**3)
         REAL, INTENT( OUT ) :: ICE_ASY( : )         !  cloud asymmetry factor
         REAL, INTENT( OUT ) :: ICE_SSA( : )         !  cloud coalbedo
         REAL, INTENT( OUT ) :: ICE_DEL( : )         ! Delta function transmission for 
                                                      ! Scattering at zero scattering angle

         INTEGER, PARAMETER :: IUNIT  = 155
         INTEGER, PARAMETER :: DATA_UNIT = 156
         INTEGER, PARAMETER :: mxcof  = 3
         INTEGER, PARAMETER :: mxsiz  = 3
         INTEGER, PARAMETER :: mxwvwc = 74

         INTEGER icof, isize, ivwc, lc, nlyr, nstring
         INTEGER wclyr
         LOGICAL, SAVE ::  FIRST = .TRUE.
         LOGICAL       ::  FIRST_LINE, NEWSIZ
         LOGICAL, SAVE ::  READ_DATA  = .FALSE.
         LOGICAL, SAVE ::  WRITE_DATA = .FALSE.

         REAL( 8 )  ::  WREXT_COEFFS( MXWVWC )

         REAL, ALLOCATABLE, SAVE ::  CLOUD_ICE_EXT( : ), CLOUD_ICE_SSA( : ), CLOUD_ICE_ASY( : ) ,
     &                                 CLOUD_ICE_DEL( : )

         INTEGER, SAVE            :: INTERVALS_EXT
         REAL, ALLOCATABLE, SAVE :: WVL_EXT_LOWER( : ), WVL_EXT_UPPER( : ), WVL_EXT( : )
         REAL, ALLOCATABLE, SAVE :: A0_EXT( : ), A1_EXT( : )

         INTEGER, SAVE            :: INTERVALS_ASY
         REAL, ALLOCATABLE, SAVE :: WVL_ASY_LOWER( : ), WVL_ASY_UPPER( : ), WVL_ASY( : )
         REAL, ALLOCATABLE, SAVE :: C0_ASY( : ), C1_ASY( : ), C2_ASY( : ), C3_ASY( : )

         INTEGER, SAVE            :: INTERVALS_SSA
         REAL, ALLOCATABLE, SAVE :: WVL_SSA_LOWER( : ), WVL_SSA_UPPER( : ), WVL_SSA( : )
         REAL, ALLOCATABLE, SAVE :: B0_SSA( : ), B1_SSA( : ), B2_SSA( : ), B3_SSA( : )

         INTEGER, SAVE            :: INTERVALS_DEL
         REAL, ALLOCATABLE, SAVE :: WVL_DEL_LOWER( : ), WVL_DEL_UPPER( : ), WVL_DEL( : )
         REAL, ALLOCATABLE, SAVE :: B0_DEL( : ), B1_DEL( : ), B2_DEL( : ), B3_DEL( : )

         INTEGER, SAVE            :: I_CUTTOFF_EXT
         INTEGER, SAVE            :: I_CUTTOFF_ASY
         INTEGER, SAVE            :: I_CUTTOFF_SSA
         INTEGER, SAVE            :: I_CUTTOFF_DEL

         CHARACTER(100)            :: CHAR
         CHARACTER(19)             :: FILENM    ! No. of characters must equal
                                                 ! length of filename.

         CHARACTER(586)            :: FULLNAME, LINE

         REAL                       :: DIAMETER_SQUARED, DIAMETER_CUBED, INVERSE_DIAMETER

         REAL AVG1, AVG2, REFF_AVG1, REFF_AVG2

         REAL, ALLOCATABLE, SAVE :: WAVE( : ), XDUMB( : ), YDUMB( : ), ZDUMB( : )
         REAL                      :: FACTOR
         INTEGER                   :: NDUMB
         REAL, ALLOCATABLE, SAVE :: WAVE_OUTL( : ), WAVE_OUTU( : ), WAVE_OUTC( : )

         REAL, ALLOCATABLE, SAVE ::  NEW_CL_ASY(:, :, :), NEW_CL_EXT(:, :, :),
     &                                NEW_CL_SSA(:, :, :)
     
         INTEGER                  :: I, J, K
         INTEGER                  :: POUND_COUNT


      INTERFACE
        SUBROUTINE WVBIN_AVERAGE(WL_CS_IN, CS_IN, NWL_CS_IN,  
     &                         WL_QY_IN, QY_IN, NWL_QY_IN,  
     &                         SPECTRA_TYPE,
     &                         WLL_AVE, WLU_AVE, NWL_AVE, 
     &                         CS_AVE, QY_AVE )
          CHARACTER(1), INTENT( IN ) :: SPECTRA_TYPE        ! spectra type
          INTEGER, INTENT( IN )      :: NWL_AVE             ! number of intervals average 
          INTEGER, INTENT( IN )      :: NWL_CS_IN           ! number of intervals CS_IN
          INTEGER, INTENT( IN )      :: NWL_QY_IN           ! number of intervals CS_IN
          REAL, INTENT( IN )         :: WL_CS_IN( : )  ! wl for CS_IN
          REAL, INTENT( IN )         :: WL_QY_IN( : )  ! wl for QY_IN
          REAL, INTENT( IN )         :: CS_IN( : )     ! cross-section as f(WLIN)
          REAL, INTENT( IN )         :: QY_IN( : )     ! quantum yield as f(WLIN)
          REAL, INTENT( INOUT )      :: WLL_AVE( : )   ! lower limit on wl effective interval
          REAL, INTENT( INOUT )      :: WLU_AVE( : )   ! upper limit on wl effective interval
          REAL, INTENT( INOUT )      :: CS_AVE( : )    ! cross-section as f(WL_AVE)
          REAL, INTENT( INOUT )      :: QY_AVE( : )    ! quantum yield as f(WL_AVE)
        END SUBROUTINE WVBIN_AVERAGE
        SUBROUTINE WVBIN_AVERAGE_B(WL_CS_IN, CS_IN, NWL_CS_IN,  

     &                         WL_QY_IN, QY_IN, NWL_QY_IN,  
     &                         SPECTRA_TYPE,
     &                         WLL_AVE, WLU_AVE, NWL_AVE, 
     &                         CS_AVE, QY_AVE )
          CHARACTER(1), INTENT( IN ) :: SPECTRA_TYPE        ! spectra type
          INTEGER, INTENT( IN )      :: NWL_AVE             ! number of intervals average 
          INTEGER, INTENT( IN )      :: NWL_CS_IN           ! number of intervals CS_IN
          INTEGER, INTENT( IN )      :: NWL_QY_IN           ! number of intervals CS_IN
          REAL, INTENT( IN )         :: WL_CS_IN( : )  ! wl for CS_IN
          REAL, INTENT( IN )         :: WL_QY_IN( : )  ! wl for QY_IN
          REAL, INTENT( IN )         :: CS_IN( : )     ! cross-section as f(WLIN)
          REAL, INTENT( IN )         :: QY_IN( : )     ! quantum yield as f(WLIN)
          REAL, INTENT( INOUT )      :: WLL_AVE( : )   ! lower limit on wl effective interval
          REAL, INTENT( INOUT )      :: WLU_AVE( : )   ! upper limit on wl effective interval
          REAL, INTENT( INOUT )      :: CS_AVE( : )    ! cross-section as f(WL_AVE)
          REAL, INTENT( INOUT )      :: QY_AVE( : )    ! quantum yield as f(WL_AVE)
        END SUBROUTINE WVBIN_AVERAGE_B
      END INTERFACE  

         IF ( FIRST ) THEN

              FIRST = .FALSE.

C     Read in ice particle optical parameters.
     
              ALLOCATE( WAVE( MXWLIN ), XDUMB( MXWLIN ), ZDUMB( MXWLIN ), 
     &                  YDUMB( MXWLIN) )

              ALLOCATE( WAVE_OUTC( MXWLIN ), WAVE_OUTL( MXWLIN ), WAVE_OUTU( MXWLIN ))
     
               
!                filenm   = 'ice_clouds/fu96.ext'
!                fullname = filenm
                filenm   = 'ICE_CLD_EXT'
                CALL VALUE_NAME( filenm, fullname )   
                OPEN(UNIT=iunit,FILE=fullname,FORM='FORMATTED',STATUS='OLD',ERR=99)
                POUND_COUNT = 0
                CHAR(1:1) = '#'
                DO WHILE( CHAR(1:1) .EQ. '#' )
                    READ(IUNIT,'(A)',ERR=99) CHAR
                    POUND_COUNT = POUND_COUNT + 1
!                    PRINT*,TRIM(CHAR)
                END DO
                POUND_COUNT = POUND_COUNT - 1
                INTERVALS_EXT = 0
                DO 
                   INTERVALS_EXT = INTERVALS_EXT + 1
                   READ(iunit,'(A)',ERR=99, END = 101 ) CHAR
!                   PRINT*,TRIM(CHAR)
                END DO
101             REWIND(IUNIT)

                ALLOCATE( WVL_EXT_LOWER( INTERVALS_EXT ), WVL_EXT_UPPER( INTERVALS_EXT ),
     &                    A0_EXT( INTERVALS_EXT ), A1_EXT( INTERVALS_EXT ) )

                ALLOCATE( WVL_EXT( INTERVALS_EXT ), CLOUD_ICE_EXT( INTERVALS_EXT ) )

                DO I = 1, POUND_COUNT
                   READ(IUNIT,'(A)',ERR=99)CHAR
                END DO
     
                DO I = 1, INTERVALS_EXT
                   READ(IUNIT,*)WVL_EXT_LOWER( I ), WVL_EXT_UPPER( I ),  A0_EXT( I ), A1_EXT( I )
                   WVL_EXT( I ) = WVL_EXT_UPPER( I ) + 0.5 * (WVL_EXT_LOWER( I ) - WVL_EXT_UPPER( I ))
                END DO

                CLOSE( IUNIT )
                
!                filenm   = 'ice_clouds/fu96.asy'
                filenm   = 'ICE_CLD_ASY'
                fullname = filenm
                CALL VALUE_NAME( filenm, fullname )   
                OPEN(UNIT=iunit,FILE=fullname,FORM='FORMATTED',STATUS='OLD',ERR=99)
                POUND_COUNT = 0
                CHAR(1:1) = '#'
                DO WHILE( CHAR(1:1) .EQ. '#' )
                    READ(IUNIT,'(A)',ERR=99) CHAR
                    POUND_COUNT = POUND_COUNT + 1
!                    PRINT*,TRIM(CHAR)
                END DO
                POUND_COUNT = POUND_COUNT - 1
                INTERVALS_ASY = 0
                DO 
                   INTERVALS_ASY = INTERVALS_ASY + 1
                   READ(iunit,'(A)',ERR=99, END = 201 ) CHAR
!                   PRINT*,TRIM(CHAR)
                END DO
201             REWIND(IUNIT)

                ALLOCATE( WVL_ASY_LOWER( INTERVALS_ASY ), WVL_ASY_UPPER( INTERVALS_ASY ),
     &                    C0_ASY( INTERVALS_ASY ), C1_ASY( INTERVALS_ASY ), 
     &                    C2_ASY( INTERVALS_ASY ), C3_ASY( INTERVALS_ASY ) )

                ALLOCATE( WVL_ASY( INTERVALS_ASY ), CLOUD_ICE_ASY( INTERVALS_ASY ) )


                DO I = 1, POUND_COUNT
                   READ(IUNIT,'(A)',ERR=99)CHAR
                END DO
     
                J = 0
                DO I = 1, INTERVALS_ASY
                   READ(IUNIT,*)WVL_ASY_LOWER( I ), WVL_ASY_UPPER( I ),  
     &                          C0_ASY( I ), C1_ASY( I ),
     &                          C2_ASY( I ), C3_ASY( I )
                   WVL_ASY( I ) = WVL_ASY_UPPER( I ) + 0.5 * (WVL_ASY_LOWER( I ) - WVL_ASY_UPPER( I ))
                   
                END DO

                CLOSE( IUNIT )

!                filenm   = 'ice_clouds/fu96.ssa'
!                fullname = filenm
                filenm   = 'ICE_CLD_SSA'
                CALL VALUE_NAME( filenm, fullname )   
                OPEN(UNIT=iunit,FILE=fullname,FORM='FORMATTED',STATUS='OLD',ERR=99)
                POUND_COUNT = 0
                CHAR(1:1) = '#'
                DO WHILE( CHAR(1:1) .EQ. '#' )
                    READ(IUNIT,'(A)',ERR=99) CHAR
                    POUND_COUNT = POUND_COUNT + 1
!                    PRINT*,TRIM(CHAR)
                END DO
                POUND_COUNT = POUND_COUNT - 1
                INTERVALS_SSA = 0
                DO 
                   INTERVALS_SSA = INTERVALS_SSA + 1
                   READ(iunit,'(A)',ERR=99, END = 301 ) CHAR
!                   PRINT*,TRIM(CHAR)
                END DO
301             REWIND(IUNIT)

                ALLOCATE( WVL_SSA_LOWER( INTERVALS_SSA ), WVL_SSA_UPPER( INTERVALS_SSA ),
     &                    B0_SSA( INTERVALS_SSA ), B1_SSA( INTERVALS_SSA ), 
     &                    B2_SSA( INTERVALS_SSA ), B3_SSA( INTERVALS_SSA ) )

                DO I = 1, POUND_COUNT
                   READ(IUNIT,'(A)',ERR=99)CHAR
                END DO

                ALLOCATE( WVL_SSA( INTERVALS_SSA ), CLOUD_ICE_SSA( INTERVALS_SSA ) )
     
                DO I = 1, INTERVALS_SSA
                   READ(IUNIT,*)WVL_SSA_LOWER( I ), WVL_SSA_UPPER( I ),  
     &                    B0_SSA( I ), B1_SSA( I ), 
     &                    B2_SSA( I ), B3_SSA( I ) 
                   WVL_SSA( I ) = WVL_SSA_UPPER( I ) + 0.5 * (WVL_SSA_LOWER( I ) - WVL_SSA_UPPER( I ))
                END DO

                CLOSE( IUNIT )

!                filenm   = 'ice_clouds/fu96.del'
!                fullname = filenm
                filenm   = 'ICE_CLD_DEL'
                CALL VALUE_NAME( filenm, fullname )   
                OPEN(UNIT=iunit,FILE=fullname,FORM='FORMATTED',STATUS='OLD',ERR=99)
                POUND_COUNT = 0
                CHAR(1:1) = '#'
                DO WHILE( CHAR(1:1) .EQ. '#' )
                    READ(IUNIT,'(A)',ERR=99) CHAR
                    POUND_COUNT = POUND_COUNT + 1
!                    PRINT*,TRIM(CHAR)
                END DO
                POUND_COUNT = POUND_COUNT - 1
                INTERVALS_DEL = 0
                DO 
                   INTERVALS_DEL = INTERVALS_DEL + 1
                   READ(iunit,'(A)',ERR=99, END = 401 ) CHAR
!                   PRINT*,TRIM(CHAR)
                END DO
401             REWIND(IUNIT)

                ALLOCATE( WVL_DEL_LOWER( INTERVALS_DEL ), WVL_DEL_UPPER( INTERVALS_DEL ),
     &                    B0_DEL( INTERVALS_DEL ), B1_DEL( INTERVALS_DEL ), 
     &                    B2_DEL( INTERVALS_DEL ), B3_DEL( INTERVALS_DEL ) )

                DO I = 1, POUND_COUNT
                   READ(IUNIT,'(A)',ERR=99)CHAR
                END DO

                ALLOCATE( WVL_DEL( INTERVALS_DEL ), CLOUD_ICE_DEL( INTERVALS_DEL ) )
     
                DO I = 1, INTERVALS_DEL
                   READ(IUNIT,*)WVL_DEL_LOWER( I ), WVL_DEL_UPPER( I ),  
     &                    B0_DEL( I ), B1_DEL( I ), 
     &                    B2_DEL( I ), B3_DEL( I ) 
                   WVL_DEL( I ) = WVL_DEL_UPPER( I ) + 0.5 * (WVL_DEL_LOWER( I ) - WVL_DEL_UPPER( I ))
                END DO

               DO I = NJO_NEW, 1, -1
                  IF ( EFFECTIVE_LAMBDA(I) .LT. 1.0E+3*WVL_EXT( 1 )  )THEN
                     I_CUTTOFF_EXT = I
                     EXIT
                 END IF
               END DO
               DO I = NJO_NEW, 1, -1
                  IF ( EFFECTIVE_LAMBDA(I) .LT. 1.0E+3*WVL_ASY( 1 )  )THEN
                     I_CUTTOFF_ASY = I
                     EXIT
                 END IF
               END DO
               DO I = NJO_NEW, 1, -1
                  IF ( EFFECTIVE_LAMBDA(I) .LT. 1.0E+3*WVL_SSA( 1 )  )THEN
                     I_CUTTOFF_SSA = I
                     EXIT
                 END IF
               END DO
               DO I = NJO_NEW, 1, -1
                  IF ( EFFECTIVE_LAMBDA(I) .LT. 1.0E+3*WVL_DEL( 1 )  )THEN
                     I_CUTTOFF_DEL = I
                     EXIT
                 END IF
               END DO

               CLOSE( IUNIT )


         ENDIF  ! FIRST


         IF(CLOUD_DIAMETER_ICE .LT. 4.99 .OR. CLOUD_DIAMETER_ICE .GT. 140.01 )THEN
            WRITE(*,*)'Error, in ice cloud parameterization Fu et al. (1996)'
            WRITE(*,*)'ice droplet diameter = ', CLOUD_DIAMETER_ICE
            WRITE(*,*)'and is greater than 5.0 um or larger than 140.0 um. '
            STOP
         ENDIF
         
         DIAMETER_SQUARED = CLOUD_DIAMETER_ICE * CLOUD_DIAMETER_ICE
         DIAMETER_CUBED   = DIAMETER_SQUARED   * CLOUD_DIAMETER_ICE
         INVERSE_DIAMETER = 1.0 / CLOUD_DIAMETER_ICE

         DO I = 1, INTERVALS_EXT
C     Extinction coefficient
            CLOUD_ICE_EXT( I ) = A0_EXT( I ) + A1_EXT( I ) * INVERSE_DIAMETER
            WRITE(6,'(A,8(es12.4,1X))')'ICE_EXT: ',WVL_EXT(I),CLOUD_ICE_EXT( I ),A0_EXT( I ),A1_EXT( I ),
     &     INVERSE_DIAMETER
            CLOUD_ICE_EXT( I ) = MAX(CLOUD_ICE_EXT( I ), 0.0)
         END DO

         DO I = 1, INTERVALS_ASY
C     Asymmetery coefficient
            CLOUD_ICE_ASY( I ) = C0_ASY( I ) 
     &                         + C1_ASY( I ) * CLOUD_DIAMETER_ICE
     &                         + C2_ASY( I ) * DIAMETER_SQUARED
     &                         + C3_ASY( I ) * DIAMETER_CUBED
            WRITE(6,'(A,12(es12.4,1X))')'ICE_SYM: ',WVL_ASY(I),CLOUD_ICE_ASY( I ),C0_ASY( I ),C1_ASY( I ),
     &      CLOUD_DIAMETER_ICE, C2_ASY( I ), DIAMETER_SQUARED,  C3_ASY( I ), DIAMETER_CUBED     
         END DO

         DO I = 1, INTERVALS_SSA
C     Single Scattering Co-Albedo
            CLOUD_ICE_SSA( I ) = REAL(B0_SSA( I ), 8)
     &                         + REAL(B1_SSA( I ) * CLOUD_DIAMETER_ICE, 8)
     &                         + REAL(B2_SSA( I ) * DIAMETER_SQUARED, 8 )
     &                         + REAL(B3_SSA( I ) * DIAMETER_CUBED, 8)
!            CLOUD_ICE_SSA( I ) = MAX(0.0, CLOUD_ICE_SSA( I ))
            WRITE(6,'(A,12(es12.4,1X))')'ICE_COA: ',WVL_SSA(i),CLOUD_ICE_SSA( I ),B0_SSA( I ),B1_SSA( I ),
     &     CLOUD_DIAMETER_ICE, B2_SSA( I ), DIAMETER_SQUARED,  B3_SSA( I ), DIAMETER_CUBED     
         END DO


         DO I = 1, INTERVALS_DEL
C     Delta function transmission for Scattering at zero scattering angle
            CLOUD_ICE_DEL( I ) = REAL(B0_DEL( I ), 8)
     &                         + REAL(B1_DEL( I ) * CLOUD_DIAMETER_ICE, 8)
     &                         + REAL(B2_DEL( I ) * DIAMETER_SQUARED, 8 )
     &                         + REAL(B3_DEL( I ) * DIAMETER_CUBED, 8)
            CLOUD_ICE_DEL( I ) = MAX(0.0, CLOUD_ICE_DEL( I ))
            WRITE(6,'(A,12(es12.4,1X))')'ICE_DEL: ',WVL_DEL(i),CLOUD_ICE_DEL( I ),B0_DEL( I ),B1_DEL( I ),
     &     CLOUD_DIAMETER_ICE, B2_DEL( I ), DIAMETER_SQUARED,  B3_DEL( I ), DIAMETER_CUBED     
         END DO




         XDUMB = 1.0
         NDUMB = INTERVALS_EXT
         WAVE  = 0.0
         WAVE( 1:NDUMB ) =  1.0E+3 * WVL_EXT( 1:NDUMB ) ! convert from um to nm

         ICE_EXT = 0.0
         ICE_ASY = 0.0
         ICE_SSA = 1.0
         ICE_DEL = 0.0

         CALL WVBIN_AVERAGE(WAVE, CLOUD_ICE_EXT, NDUMB, WAVE, XDUMB, NDUMB, 'P',
     &                      WAVE_OUTL, WAVE_OUTU, MXWLIN, YDUMB, ZDUMB )

         ICE_EXT( 1:NJO_NEW ) = YDUMB( 1:NJO_NEW )


         XDUMB = 1.0
         NDUMB = INTERVALS_ASY
         WAVE  = 0.0
         WAVE( 1:NDUMB ) =  1.0E+3 * WVL_ASY( 1:NDUMB )
         
         CALL WVBIN_AVERAGE(WAVE, CLOUD_ICE_ASY, NDUMB, WAVE, XDUMB, NDUMB, 'P',
     &                      WAVE_OUTL, WAVE_OUTU, MXWLIN, YDUMB, ZDUMB )

         ICE_ASY( 1:NJO_NEW ) = YDUMB( 1:NJO_NEW )

         XDUMB = 1.0
         NDUMB = INTERVALS_SSA
         WAVE  = 0.0
         WAVE( 1:NDUMB ) =  1.0E+3 * WVL_SSA( 1:NDUMB ) ! convert from um to nm

         CALL WVBIN_AVERAGE(WAVE, CLOUD_ICE_SSA, NDUMB, WAVE, XDUMB, NDUMB, 'P',
     &                      WAVE_OUTL, WAVE_OUTU, MXWLIN, YDUMB, XDUMB )

         ICE_SSA( 1:NJO_NEW ) = YDUMB( 1:NJO_NEW )

         XDUMB = 1.0
         NDUMB = INTERVALS_DEL
         WAVE  = 0.0
         WAVE( 1:NDUMB ) =  1.0E+3 * WVL_DEL( 1:NDUMB ) ! convert from um to nm

         CALL WVBIN_AVERAGE(WAVE, CLOUD_ICE_DEL, NDUMB, WAVE, XDUMB, NDUMB, 'P',
     &                      WAVE_OUTL, WAVE_OUTU, MXWLIN, YDUMB, XDUMB )

         ICE_DEL( 1:NJO_NEW ) = YDUMB( 1:NJO_NEW )
         
! replace zero values
         ICE_EXT( 1:I_CUTTOFF_EXT ) = ICE_EXT( I_CUTTOFF_EXT + 1 )
         ICE_ASY( 1:I_CUTTOFF_ASY ) = ICE_ASY( I_CUTTOFF_ASY + 1 )
         ICE_SSA( 1:I_CUTTOFF_SSA ) = ICE_SSA( I_CUTTOFF_SSA + 1 )
         ICE_DEL( 1:I_CUTTOFF_DEL ) = ICE_DEL( I_CUTTOFF_DEL + 1 )
              WRITE(6,'(A,8(es12.4,1X))')'ICE_CUTTOFF: ',WAVE_OUTL(I_CUTTOFF_EXT + 1),ICE_EXT(I_CUTTOFF_EXT + 1),
     &                                    ICE_ASY(I_CUTTOFF_ASY + 1),ICE_SSA(I_CUTTOFF_SSA + 1),
     &                                    ICE_DEL(I_CUTTOFF_DEL + 1)

         do ivwc = 1, NJO_NEW
              WRITE(6,'(A,8(es12.4,1X))')'ICE: ',WAVE_OUTL(ivwc),ICE_EXT(ivwc),ICE_ASY(ivwc),ICE_SSA(ivwc),
     &                                    ICE_DEL(ivwc)
         end do

         RETURN
          
98      WRITE(6,*)'ERROR opening output file for Fu (1996) data'
         STOP

99       WRITE(6,'(2A)')'Error during reading of file: ',TRIM(fullname)
         STOP

       END
