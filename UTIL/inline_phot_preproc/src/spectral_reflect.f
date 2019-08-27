      SUBROUTINE SPECTRAL_REFLECT(MODIS_ALBEDO)

! PURPOSE: Populate array ALBEDO with wavelength dependent reflectances (i.e.
!	     spectral albedos) for 20 MODIS land use types.  Also returns correction factors
!          for solar zenith angle and season. Note that an adjustment is applied to 
!          better match spectral estimates for each land use type.

! REFERENCE:  CERES/SARB Surface Properties database, May 2006
!             http://www-surf/larc.nasa.gov/surf/pages/data-page.html

! INPUT:  	SOLAR_ZENITH [deg]
! OUTPUT: 	NWAVE       
!             ALBEDO(20,15)
! CALLS:  	none
! CALLED BY: 	veg
      
      USE BIN_DATA
      USE CSQY_PARAMETERS
      USE ALBEDO_REFER_DATA
      
      IMPLICIT NONE 

      REAL, INTENT( OUT ) :: MODIS_ALBEDO( :, :)

      INTEGER NWAVE
      INTEGER NUSES

      REAL(4) :: SCRATCH_ALBEDO(NUMB_BANDS_MODIS)

      REAL(4) :: ALBEDO_WAVE(NUMB_BANDS_MODIS)

      REAL(4) :: SOLAR_ZENITH, COSZEN

      REAL(8) :: PI

      REAL(4) :: ZENITH_CORRECTION

      INTEGER           :: NDUMB
      REAL, ALLOCATABLE :: WAVE( : ), XDUMB( : ), YDUMB( : ), ZDUMB( : )
      REAL, ALLOCATABLE :: WAVE_OUTL( : ), WAVE_OUTU( : ), WAVE_OUTC( : )

      INTEGER   I, J, K
      INTEGER   I_CUTTOFF
     
      INTEGER   :: IWL

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

      ALLOCATE(WAVE(MXWLIN), XDUMB(MXWLIN), ZDUMB(MXWLIN), YDUMB(MXWLIN))

      ALLOCATE(WAVE_OUTC(MXWLIN), WAVE_OUTL(MXWLIN), WAVE_OUTU(MXWLIN))

      NWAVE = NUMB_BANDS_MODIS
      NUSES = NUMB_LANDUSE_MODIS

      MODIS_ALBEDO = 0.0

      PI = DACOS(-1.D0)

!The following loops indicate use LUC (as "j), and band (as "i")

       COSZEN       = 0.5 ! dcos((pi/180.d0)*SOLAR_ZENITH)

       SOLAR_ZENITH = PI/180.0D0*DACOS(0.5D0)

       DO I = NJO_NEW, 1, -1
          IF ( EFFECTIVE_LAMBDA(I) .LT.  WVBAND( 1 )  )THEN
             I_CUTTOFF = I
             EXIT
          END IF
       END DO

       DO 270 J = 1, NUMB_LANDUSE_MODIS
       
        DO 250 I = 1, NUMB_BANDS_MODIS

            ALBEDO_WAVE(I) = WVBAND( I )

            ZENITH_CORRECTION = (1.0 + ZENITH_COEFF(J))
     &                        / (1.0 + 2.0*COSZEN*ZENITH_COEFF(J))

            ZENITH_CORRECTION = MAX(0.8, ZENITH_CORRECTION)

            SCRATCH_ALBEDO( I ) = MIN(1.0, 
     &                            (ZENITH_CORRECTION*SPECTRAL_ALBEDO( I, J)))

            IF((MODIS2SPECTRAL( J )*SCRATCH_ALBEDO( I )).LE. 1.0)THEN
                SCRATCH_ALBEDO( I ) = MODIS2SPECTRAL( J )
     &                              * SCRATCH_ALBEDO( I )
            ELSE
                SCRATCH_ALBEDO( I ) = 1.0
            ENDIF

!         print*,landuse(j),ALBEDO_WAVE(I),SCRATCH_ALBEDO( I )	 
250      CONTINUE

         XDUMB = 1.0
         NDUMB = NUMB_BANDS_MODIS
         WAVE  = 0.0
         WAVE( 1:NDUMB ) =  ALBEDO_WAVE( 1:NDUMB )

         CALL WVBIN_AVERAGE(WAVE, SCRATCH_ALBEDO, NDUMB, WAVE, XDUMB, NDUMB, 
     &                      'P', WAVE_OUTL, WAVE_OUTU, MXWLIN, YDUMB, ZDUMB )

         MODIS_ALBEDO( 1:NJO_NEW, J)   = YDUMB( 1:NJO_NEW )
         MODIS_ALBEDO( 1:I_CUTTOFF, J) = YDUMB( I_CUTTOFF + 1 )

!         DO I = 1, NJO_NEW
!         WRITE(6,'(3(es12.4,1x))')STWL_NEW(i),MODIS_ALBEDO( I, J), YDUMB(I)
!         ENDDO
!         pause
270    CONTINUE


       

      RETURN
      END
