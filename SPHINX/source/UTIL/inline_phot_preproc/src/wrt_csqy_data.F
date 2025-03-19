      SUBROUTINE WRT_CSQY_DATA( WLIN, CS_IN, QY_IN, NWLIN, SPECTRA_NAME, SPECTRA_TYPE,
     &                    WLL_AVE, WLU_AVE, CS_AVE, QY_AVE, NWL_AVE )

!      USE JPROC_PROFILE
      USE GET_ENV_VARS      
      USE BIN_DATA
      USE CSQY_REFER_DATA
      USE ALBEDO_REFER_DATA

      IMPLICIT NONE      
      
!      INCLUDE 'JVALPARMS.EXT'         ! jproc parameters


C...........ARGUMENTS and their descriptions

             CHARACTER( 1), INTENT( IN )  :: SPECTRA_TYPE    ! spectra type
             CHARACTER(16), INTENT( IN )  :: SPECTRA_NAME    ! spectra type
             INTEGER,       INTENT( IN )  :: NWLIN           ! number of intervals CQin
             REAL,          INTENT( IN )  :: WLIN ( : )      ! wl for CQin
             REAL,          INTENT( IN )  :: CS_IN( : )      ! cross-section as f(WLIN)
             REAL,          INTENT( IN )  :: QY_IN( : )      ! quantum yield as f(WLIN)
             REAL,          INTENT( OUT)  :: WLL_AVE( : )    ! lower limit on wl int ETin
             REAL,          INTENT( OUT ) :: WLU_AVE( : )    ! upper limit on wl int ETin
             REAL,          INTENT( OUT ) :: CS_AVE(  : )    ! cross-section as f(WL_AVE)
             REAL,          INTENT( OUT ) :: QY_AVE(  : )    ! quantum yield as f(WL_AVE)
             INTEGER,       INTENT( OUT ) :: NWL_AVE

C...........LOCAL VARIABLES and their descriptions:
      
      CHARACTER(16)   ::  PNAME  = 'WRT_CSQY_DATA'    ! program name
      CHARACTER(80)   ::  MSG    = ' '                ! message

C...........PARAMETERS and their descriptions

!      INTEGER, PARAMETER :: XSTAT2  = 2       ! Program ERROR exit status
!      integer, parameter :: NBO = 100
!      integer, parameter :: NSO = 40000
!      integer, parameter :: NZO = 13550
!      integer, parameter :: NJO = 18 

!      INTEGER, SAVE      :: NB, J1, J2, K1, K2  ! array limits and markers
      INTEGER            :: I, J, K             ! index counters

      REAL, ALLOCATABLE, SAVE :: WLIN1( : )     ! lower limit on wl int CQin
      REAL, ALLOCATABLE, SAVE :: WLIN2( : )     ! upper limit on wl int CQin

!      REAL(8), SAVE    :: SRB(15,NJO)
!      REAL(8), SAVE    :: WBIN(NBO + 1 )
!      REAL(8)   :: FBIN(NBO)
!      REAL(8)   :: ABIN(NBO)
!      REAL(8)   :: CBIN(NBO)
!      REAL(8)   :: DBIN(NBO)
!      REAL(8)   :: EBIN(NBO)

      REAL(8), ALLOCATABLE, SAVE :: FBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: ABIN( : )
      REAL(8), ALLOCATABLE, SAVE :: CBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: DBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: EBIN( : )

      REAL      :: TEMP        ! temperature, K
      REAL(8)   :: WW       

      REAL(8), ALLOCATABLE, SAVE :: FFBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: AABIN( : )
      REAL(8), ALLOCATABLE, SAVE :: CCBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: DDBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: EEBIN( : )


!      INTEGER, SAVE    :: IJX(NBO)
      INTEGER          :: ITTR

!      REAL, ALLOCATABLE, SAVE :: W(:), F(:)
      REAL, ALLOCATABLE, SAVE :: XCOUT(:), QYOUT(:)
!      REAL, ALLOCATABLE, SAVE :: WL(:), WU(:), WC(:)
!      INTEGER, SAVE    :: IBINJ(NSO)
      REAL              :: XNEW

      CHARACTER(8)     :: TITLNEW
      CHARACTER(16)    :: SAFE_NAME
      CHARACTER(60)    :: VNAME



      LOGICAL, SAVE            :: FIRSTCALL  = .TRUE.


      REAL,    SAVE            :: TEMPERATURE(N_TEMPERATURE)
      REAL                     :: DELTA_TEMP
      REAL                     :: TEMP_STRT, TEMP_FINI
      REAL,    SAVE            :: TEMP_STRAT(N_TEMP_STRAT)

      INTEGER                  :: IRXN

      INTEGER, PARAMETER       :: NZ = N_TEMPERATURE
      REAL                     :: AIR_TEMP(NZ) = 298      ! air temperature [K]
      REAL                     :: NUM_DENS(NZ) = DENS0    ! air number density [molec/cm3]
      LOGICAL                  :: REPLACE                 ! flag to use sq values in calling routine 

      REAL, ALLOCATABLE, SAVE ::  XCROSS( : , : )    ! cross-section [CM2]
      REAL, ALLOCATABLE, SAVE ::  YIELD(: , : )      ! quantum yield [dimensionaless]
      REAL, ALLOCATABLE, SAVE ::  XCROSS_EFF(: , : ) ! cross-section times yield [CM2]

      REAL, ALLOCATABLE, SAVE ::  WC_EXT( : )
      REAL, ALLOCATABLE, SAVE ::  WC_ASF( : )
      REAL, ALLOCATABLE, SAVE ::  WC_SSA( : )

      REAL, PARAMETER :: CLOUD_RADIUS_LIQUID = 10.0 ! mean cloud droplet radii, um

      INTEGER, PARAMETER :: NAERO_REFRACT = 5      

      LOGICAL, SAVE      :: WRITE_AE_REFRACT = .FALSE. ! .FALSE.

      CHARACTER(  5 )    :: WVL_AE_REFRAC
      CHARACTER( 13 )    :: AERO_INDEX    = 'WVL_AE_REFRAC'

      CHARACTER( 16 )    :: AERO_REFRACT_INDX( NAERO_REFRACT ) 
      DATA  AERO_REFRACT_INDX / "WATER", "SOLUTE", "DUST", "SEASALT", "SOOT" /

      REAL, ALLOCATABLE :: AERO_IMAG_REFRACT( :, : )
      REAL, ALLOCATABLE :: AERO_REAL_REFRACT( :, : )

      REAL, ALLOCATABLE, SAVE :: MODIS_ALBEDO( :, :) ! NJO, NUMB_LANDUSE_MODIS )

      REAL, ALLOCATABLE, SAVE :: CS_PHOT(  :, :, :) ! N_TEMPERATURE, N_INLINE_BAND, NPHOTAB)
      REAL, ALLOCATABLE, SAVE :: QY_PHOT(  :, :, :) ! N_TEMPERATURE, N_INLINE_BAND, NPHOTAB)
      REAL, ALLOCATABLE, SAVE :: ECS_PHOT( :, :, :) ! N_TEMPERATURE, N_INLINE_BAND, NPHOTAB)
      REAL, ALLOCATABLE, SAVE :: EQY_PHOT( :, :, :) ! N_TEMPERATURE, N_INLINE_BAND, NPHOTAB)

      REAL, ALLOCATABLE, SAVE :: O3_CS_STRAT(  :, :) ! N_TEMP_STRAT, N_INLINE_BAND)
      REAL, ALLOCATABLE, SAVE :: O3_QY_STRAT(  :, :) ! N_TEMP_STRAT, N_INLINE_BAND)
      REAL, ALLOCATABLE, SAVE :: O3_ECS_STRAT( :, :) ! N_TEMP_STRAT, N_INLINE_BAND)
      REAL, ALLOCATABLE, SAVE :: O3_EQY_STRAT( :, :) ! N_TEMP_STRAT, N_INLINE_BAND)

      REAL, EXTERNAL  :: OZONE_YIELD

      INTERFACE
        SUBROUTINE XC_QY_TD_EFFECT(WC,NW,TLEV,DENS,NZ,
     &                                JLABEL,XC,QY,SQ,REPLACE)
            INTEGER, INTENT( IN )       ::  NW
            REAL, INTENT( IN )          ::  WC(:)
            INTEGER, INTENT( IN )       ::  NZ
            REAL, INTENT( IN )          ::  TLEV(:)         ! AIR TEMPERATURE OVER MODEL LEVELS, DEG K
            REAL, INTENT( IN )          ::  DENS(:)         ! AIR NUMBER DENSITY OVER LEVEL, 1/CM3
            CHARACTER(16), INTENT( IN ) ::  JLABEL          ! NAME OF PHOTOLYSIS RATE
            REAL, INTENT( INOUT )       ::  XC(:,:)         ! CROSS-SECTION FROM FILE
            REAL, INTENT( INOUT )       ::  QY(:,:)         ! QUANTUM YIELD FROM FILE
            REAL, INTENT( OUT )         ::  SQ(:,:)         ! CROSS-SECTION TIMES QUANTUM YIELD OVER MODEL LEVELS
            LOGICAL, INTENT( OUT )      ::  REPLACE         ! FLAG TO USE SQ VALUES IN CALLING ROUTINE 
        END SUBROUTINE XC_QY_TD_EFFECT
        SUBROUTINE INTAVG ( WLIN, CQIN, NWLIN, SPECTRA_TYPE,
     &                    NWLOUT, WLOUT1, WLOUT2, CQOUT )
          CHARACTER(1), INTENT( IN )  :: SPECTRA_TYPE     ! spectra type
          INTEGER, INTENT( IN )  ::      NWLOUT           ! number of intervals ETin
          INTEGER, INTENT( IN )  ::      NWLIN            ! number of intervals CQin
          REAL, INTENT( IN )  ::         WLIN ( : )       ! wl for CQin
          REAL, INTENT( IN )  ::         CQIN( : )        ! quantity (CS or QY) as f(WLIN)
          REAL, INTENT( INOUT ) ::       WLOUT1( : )      ! lower limit on wl int ETin
          REAL, INTENT( INOUT ) ::       WLOUT2( : )      ! upper limit on wl int ETin
          REAL, INTENT( OUT ) ::         CQOUT ( : )      ! quantity (CS or QY) as f(WLOUT)
        END SUBROUTINE INTAVG
        SUBROUTINE INTAVG_C ( WLIN, CQIN, NWLIN, SPECTRA_TYPE,
     &                        NWLOUT, WLOUT1, WLOUT2, CQOUT )
          CHARACTER(1), INTENT( IN ) ::   SPECTRA_TYPE   ! spectra type
          INTEGER, INTENT( IN )      ::   NWLOUT         ! number of intervals ETin
          INTEGER, INTENT( IN )      ::   NWLIN          ! number of intervals CQin
          REAL, INTENT( IN )   ::         WLIN ( : )     ! wl for CQin
          REAL, INTENT( IN )   ::         CQIN( : )      ! quantity (CS or QY) as f(WLIN)
          REAL, INTENT( INOUT ) ::       WLOUT1( : )     ! lower limit on wl int ETin
          REAL, INTENT( INOUT ) ::       WLOUT2( : )     ! upper limit on wl int ETin
          REAL, INTENT( OUT )  ::         CQOUT ( : )    ! quantity (CS or QY) as f(WLOUT)
        END SUBROUTINE INTAVG_C
        SUBROUTINE OPTICS_WATER_CLOUD(CLOUD_RADIUS_LIQUID, WC_EXT, WC_ASF, WC_SSA)
           REAL, INTENT( IN )  :: CLOUD_RADIUS_LIQUID
           REAL, INTENT( OUT ) :: WC_EXT( : )
           REAL, INTENT( OUT ) :: WC_ASF( : )
           REAL, INTENT( OUT ) :: WC_SSA( : )
        END SUBROUTINE OPTICS_WATER_CLOUD
        SUBROUTINE AERO_REFRACT_INDEX ( NAERO_REFRACT, AERO_REFRACT_INDX, AERO_REAL_REFRACT,
     &                                AERO_IMAG_REFRACT )
            INTEGER,         INTENT( IN )  :: NAERO_REFRACT             ! number of refractive indices
            CHARACTER( 16 ), INTENT( IN )  :: AERO_REFRACT_INDX( : )    ! names of refractive indices
            REAL,            INTENT( OUT ) :: AERO_IMAG_REFRACT( :, : ) ! imaginary part of index [Dimensionaless]
            REAL,            INTENT( OUT ) :: AERO_REAL_REFRACT( :, : ) ! real part of index [Dimensionaless]
        END SUBROUTINE AERO_REFRACT_INDEX
        SUBROUTINE SPECTRAL_REFLECT(MODIS_ALBEDO)
           IMPLICIT NONE
           REAL, INTENT( OUT ) :: MODIS_ALBEDO( :, : )
        END SUBROUTINE SPECTRAL_REFLECT
        SUBROUTINE CONVERT_CASE ( BUFFER, UPPER )
            CHARACTER(LEN= *), INTENT( INOUT ) :: BUFFER
            LOGICAL,           INTENT( IN    ) :: UPPER
        END SUBROUTINE CONVERT_CASE
       END INTERFACE

      IF( FIRSTCALL )THEN

          FIRSTCALL = .FALSE.

          CALL INIT_BIN_DATA

          IF( CHANGE_WBIN )THEN


              ALLOCATE( FBIN( NB_NEW + 1 ) )
              ALLOCATE( ABIN( NB_NEW + 1 ) )
              ALLOCATE( CBIN( NB_NEW + 1 ) )
              ALLOCATE( DBIN( NB_NEW + 1 ) )
              ALLOCATE( EBIN( NB_NEW + 1 ) )

           ELSE
              
              ALLOCATE( FBIN( NBO ) )
              ALLOCATE( ABIN( NBO ) )
              ALLOCATE( CBIN( NBO ) )
              ALLOCATE( DBIN( NBO ) )
              ALLOCATE( EBIN( NBO ) )

           ENDIF


         ITTR = 0
         IF(N_TEMPERATURE .LT. 4)THEN
           TEMP_FINI = 298.0
           TEMP_STRT = 248.0
         ELSE
           TEMP_FINI = 310.0
           TEMP_STRT = 210.0
         ENDIF
         DELTA_TEMP = (TEMP_FINI-TEMP_STRT)/FLOAT(N_TEMPERATURE-1)
         
         WRITE(JTABLE_UNIT,'(A8,1X,I3)')'NTEMP = ', N_TEMPERATURE
         WRITE(JTABLE_UNIT,'(A)')'! I   TEMP( I ) K'
         DO I = 1, N_TEMPERATURE ! 248, 298, 25
            ITTR = ITTR + 1
            TEMPERATURE(ITTR) = TEMP_STRT + DELTA_TEMP*FLOAT(I-1)
            WRITE(JTABLE_UNIT,'(I3,2X,F6.2)')ITTR, TEMPERATURE(ITTR)
         ENDDO
         TEMP_STRAT( 1 ) = 180.0
         ITTR = 1
         DO I = 2, N_TEMP_STRAT ! 248, 298, 25
            ITTR = ITTR + 1
            TEMP_STRAT(ITTR) = TEMP_STRAT( 1 ) + 40.0*FLOAT(I)
C            WRITE(JTABLE_UNIT,'(I3,2X,F6.2)')ITTR, TEMPERATURE(ITTR)
         ENDDO

!         TEMPERATURE = 298.0
       
!        READ(PHOTAB_UNIT,'(10X,I3,//)')NTEMP_REF         
!        DO I = 1, NTEMP_REF 
!           READ(PHOTAB_UNIT,'(5X,F6.2)')TEMP_REF( I )
!        ENDDO

        ALLOCATE( XCOUT(NSO), QYOUT(NSO))
        ALLOCATE( AABIN(NJO_NEW), CCBIN( NJO_NEW), DDBIN(NJO_NEW), 
     %            EEBIN(NJO_NEW), FFBIN(NJO_NEW))
        ALLOCATE( XCROSS(NZ, NJO_NEW), YIELD(NZ, NJO_NEW), XCROSS_EFF(NZ,NJO_NEW) ) 
        ALLOCATE( WC_EXT( NJO_NEW ), WC_ASF( NJO_NEW ), WC_SSA( NJO_NEW ))
        ALLOCATE( AERO_IMAG_REFRACT( NJO_NEW, NAERO_REFRACT ) )
        ALLOCATE( AERO_REAL_REFRACT( NJO_NEW, NAERO_REFRACT ) )

         WRITE(JTABLE_UNIT,5001)
         WRITE(JTABLE_UNIT,5002)
         WRITE(JTABLE_UNIT,5003)
         WRITE(JTABLE_UNIT,5004)
         WRITE(JTABLE_UNIT,5005)
         WRITE(JTABLE_UNIT,5006)

! determine whether to write out spectral values of refractive index for aerosol species

         CALL VALUE_NAME ( AERO_INDEX,  WVL_AE_REFRAC)

         CALL CONVERT_CASE( WVL_AE_REFRAC, .TRUE.)

         IF( WVL_AE_REFRAC(1:1) .EQ. 'T' .OR. WVL_AE_REFRAC(1:1) .EQ. 'Y' )THEN
             WRITE_AE_REFRACT = .TRUE.
             WRITE(6,'(A)')'Environment Variable  WVL_AE_REFRAC set to '
     &       // TRIM( WVL_AE_REFRAC ) // ' and adding aerosol refractive indice to'
     &      //  ' output '
         ELSE IF(  WVL_AE_REFRAC(1:1) .EQ. 'F' .OR. WVL_AE_REFRAC(1:1) .EQ. 'N' )THEN
             WRITE_AE_REFRACT = .FALSE.
             WRITE(6,'(A)')'Environment Variable  WVL_AE_REFRAC set to '
     &      // TRIM( WVL_AE_REFRAC ) // ' and not writing aerosol refractive indice to'
     &      //  ' output '
         ELSE
             WRITE(6,' (A)')'Environment Variable  WVL_AE_REFRAC set to '
     &       // TRIM( WVL_AE_REFRAC ) // ' and must equal T, Y, F, or N.'
     &       // ' Using default value of F'
             WRITE_AE_REFRACT = .FALSE.
         END IF

         IF( WRITE_AE_REFRACT )WRITE(JTABLE_UNIT,5007)

         IF( WRITE_AE_REFRACT )THEN
             WRITE(JTABLE_UNIT,4999)'N_INLINE_BAND = ', N_INLINE_BAND
             WRITE(JTABLE_UNIT,4999)'NAERO_REFRACT = ', NAERO_REFRACT
             WRITE(JTABLE_UNIT,4998)'NAMES_REFRACT_INDX = ', 
     &       ( TRIM( AERO_REFRACT_INDX( I ) ), I = 1, NAERO_REFRACT )
         ELSE
             WRITE(JTABLE_UNIT,4997)'N_INLINE_BAND = ', N_INLINE_BAND
         END IF


4997     FORMAT(A16, 1X, I3)
4998     FORMAT(A20, 1X, 4(A, ', '), A)
4999     FORMAT(2(A16, 1X, I3, ', '), A20, 1X, 4(A, ', '), A)

         IF( WRITE_AE_REFRACT )THEN
             WRITE(JTABLE_UNIT,5009)'! I','START_WL_BIN(nm)',
     &       'EFFECT_WL_BIN_(nm)', 'END_WL_BIN_(nm)','solar_photons_(cm-2*s-1)',
     &       'CLD_EXT/LWC_(m2/g)','CLD_ASYM_FACT','CLD_COALBEDO',
     &       ( 'AE_' // TRIM( AERO_REFRACT_INDX( I ) ) // "_REAL", 
     &         'AE_' // TRIM( AERO_REFRACT_INDX ( I ) ) // "_IMAG",
     &          I = 1, NAERO_REFRACT )
         ELSE
             WRITE(JTABLE_UNIT,5009)'! I','START_WL_BIN(nm)',
     &       'EFFECT_WL_BIN_(nm)', 'END_WL_BIN_(nm)','solar_photons_(cm-2*s-1)',
     &       'CLD_EXT/LWC_(m2/g)','CLD_ASYM_FACT','CLD_COALBEDO'
         END IF

         CALL OPTICS_WATER_CLOUD(CLOUD_RADIUS_LIQUID, WC_EXT, WC_ASF, WC_SSA)

!         DO I = 1, N_INLINE_BAND ! 248, 298, 25
!            ITTR = NJO_NEW - N_INLINE_BAND + I 
!            print*,ITTR, WC_EXT(ITTR), WC_ASF(ITTR), WC_SSA(ITTR)
!         ENDDO
!         pause

         AERO_IMAG_REFRACT = 0.0
         AERO_REAL_REFRACT = 0.0

         CAll AERO_REFRACT_INDEX ( NAERO_REFRACT, AERO_REFRACT_INDX, AERO_REAL_REFRACT,
     &                             AERO_IMAG_REFRACT )

         IF( WRITE_AE_REFRACT )THEN
            DO I = 1, N_INLINE_BAND ! 248, 298, 25
               ITTR = NJO_NEW - N_INLINE_BAND + I            
               WRITE(JTABLE_UNIT,5008)I,
     &         STWL_NEW(ITTR+2), EFFECTIVE_LAMBDA(ITTR), ENDWL_NEW(ITTR+2),
     &         SOLAR_PHOTONS( ITTR ), WC_EXT(ITTR), WC_ASF(ITTR), WC_SSA(ITTR),
     &         ( AERO_REAL_REFRACT( ITTR, J ), AERO_IMAG_REFRACT( ITTR, J ), 
     &           J = 1, NAERO_REFRACT )
            ENDDO
         ELSE
            DO I = 1, N_INLINE_BAND ! 248, 298, 25 
               ITTR = NJO_NEW - N_INLINE_BAND + I   
               WRITE(JTABLE_UNIT,5008)I,
     &         STWL_NEW(ITTR+2), EFFECTIVE_LAMBDA(ITTR), ENDWL_NEW(ITTR+2),
     &         SOLAR_PHOTONS( ITTR ), WC_EXT(ITTR), WC_ASF(ITTR), WC_SSA(ITTR)
            ENDDO
         END IF


!         READ(PHOTAB_UNIT,'(17X,I3,2/)')NWL
!         DO I = 1, NWL
!            READ(PHOTAB_UNIT,'(4X,,3(5X,F8.3,5X))')
!    &       STWL(I), WAVELENGTH(ITTR), ENDWL(I)            
!         ENDDO

!            WRITE(JTABLE_UNIT,3011)
!3011        FORMAT('!Solar Flux at top of atmosphere, photon/cm2/s'
!         DO I = 1, N_INLINE_BAND ! 248, 298, 25
!            ITTR = NJO_NEW - N_INLINE_BAND + I            
!            WRITE(JTABLE_UNIT,'(I3,1X,3(5X,F8.3,5X))')I,
!     &      SOLAR_PHOTONS(ITTR+2)
!         ENDDO

         write(JTABLE_UNIT,2007)
         write(JTABLE_UNIT,2009)
         write(JTABLE_UNIT,3010)

         WRITE(JTABLE_UNIT,'(A7,7X,A8,3X,A4,3X,40(A5,I3,A1,5X))')'!PHOTAB',
     &     'QUANTITY','TEMP',('WBIN(',J,')', J = 1, N_INLINE_BAND)


!         DO J = 1, NZ
!            XCROSS(J, 1:NJO_NEW) = 6.5E-19
!            YIELD(J, 1:NJO_NEW)  = 1.0
!         ENDDO

         ALLOCATE( CS_PHOT(  N_TEMPERATURE, N_INLINE_BAND, NPHOTAB) )
         ALLOCATE( QY_PHOT(  N_TEMPERATURE, N_INLINE_BAND, NPHOTAB) )
         ALLOCATE( ECS_PHOT( N_TEMPERATURE, N_INLINE_BAND, NPHOTAB) )
         ALLOCATE( EQY_PHOT( N_TEMPERATURE, N_INLINE_BAND, NPHOTAB) )

         ALLOCATE( O3_CS_STRAT(  N_TEMP_STRAT, N_INLINE_BAND) ) 
         ALLOCATE( O3_QY_STRAT(  N_TEMP_STRAT, N_INLINE_BAND) )
         ALLOCATE( O3_ECS_STRAT( N_TEMP_STRAT, N_INLINE_BAND) )
         ALLOCATE( O3_EQY_STRAT( N_TEMP_STRAT, N_INLINE_BAND) )

!...Compute Stratospheric Ozone Cross-Sections

         CALL INIT_CSQY_REFER_DATA()

          LOOP_STRAT: DO ITTR = 1, N_TEMP_STRAT

             TEMP = TEMP_STRAT(ITTR)

             DO J= 1, N_INLINE_BAND

                I = NJO_NEW - N_INLINE_BAND + J 
                
                IF( TEMP_STRAT(ITTR) .LT. 293.0 .AND. TEMP_STRAT(ITTR) .GT. 218.0)THEN
                    O3_CS_STRAT( ITTR, J) = (O3_XCROSS_293K(I)-O3_XCROSS_218K(I))
     &                                    /  75.0
     &                                    * (TEMP_STRAT(ITTR) - 218.0)
     &                                    + O3_XCROSS_218K(I)
                ELSEIF( TEMP_STRAT(ITTR) .LE. 218.0)THEN
                    O3_CS_STRAT( ITTR, J) = O3_XCROSS_218K(I)
                ELSEIF( TEMP_STRAT(ITTR) .GE. 293.0)THEN
                    O3_CS_STRAT( ITTR, J) = O3_XCROSS_293K(I)
                END IF

                O3_QY_STRAT(  ITTR, J) = OZONE_YIELD(WC(I),TEMP_STRAT(ITTR)) 
                O3_EQY_STRAT( ITTR, J) = OZONE_YIELD(WC(I),TEMP_STRAT(ITTR))
                
                WRITE(6,99959)I,EFFECTIVE_LAMBDA(I),O3_CS_STRAT( ITTR, J),O3_QY_STRAT(  ITTR, J)
99959           FORMAT("O3_STRAT: ",I2,1X,"LAMDBA: ",F7.2,1X,3(1PE12.4,1X)) 
!                O3_CS_STRAT( ITTR, J)  = 6.5E-19
!                O3_QY_STRAT( ITTR, J)  = 1.0 
!                O3_ECS_STRAT( ITTR, J) = 6.5E-19
!                O3_EQY_STRAT( ITTR, J) = 1.0

             ENDDO

!note assumes ozone dissociation cross-sections and yield do not depend on number density
! on 08/02/13 this assumption is correct for the xc_yq_td_code.F code

!             CALL XC_QY_TD_EFFECT(EFFECTIVE_LAMBDA, NJO_NEW,
!     &                     AIR_TEMP,NUM_DENS,NZ,'O3O1D-06        ',
!     &                     XCROSS,YIELD,XCROSS_EFF,REPLACE)
!

!             IF( REPLACE )THEN
!                  print*,'using xcqy file for O3 Stratosphere'
!                 DO J= 1, N_INLINE_BAND
!                    O3_CS_STRAT( ITTR, J)  = XCROSS(NZ, NJO_NEW - N_INLINE_BAND + J )
!                    O3_QY_STRAT( ITTR, J)  = YIELD(NZ,  NJO_NEW - N_INLINE_BAND + J ) 
!                    O3_EQY_STRAT( ITTR, J) = YIELD(NZ,  NJO_NEW - N_INLINE_BAND + J ) 
!                 ENDDO
!             ELSE
!                 print*,'using xcqy file for O3 Stratosphere'
!             ENDIF

         ENDDO LOOP_STRAT

         IF( NPHOTAB .EQ. 0 )THEN
             WRITE(JTABLE_UNIT,5010)
             WRITE(JTABLE_UNIT,'(A14,1X,I3)')'NTEMP_STRAT = ', N_TEMP_STRAT
             WRITE(JTABLE_UNIT,'(A7,7X,A8,3X,A4,3X,40(A5,I3,A1,5X))')'!      ',
     &      'QUANTITY','TEMP',('WBIN(',J,')', J = 1, N_INLINE_BAND)
             DO I = 1, N_TEMP_STRAT
                WRITE(JTABLE_UNIT,'(A16,1X,A5,1X,F8.3,1X,40(1PE12.6,2X))')'O3_STRAT ',
     &          'CS',TEMP_STRAT(I),(O3_CS_STRAT( I, J), J = 1, N_INLINE_BAND)
             ENDDO
             RETURN
         END IF

         ALLOCATE( MODIS_ALBEDO( NJO_NEW, NUMB_LANDUSE_MODIS ) )

         CALL SPECTRAL_REFLECT(MODIS_ALBEDO)


         print*,'completed firstcall in create module '

      END IF ! FIRSTCALL


      IRXN = -1

      DO I = 1, NPHOTAB
         IF( SPECTRA_NAME .EQ. PHOTAB(I) )THEN
             IRXN  = I
             EXIT
         ENDIF
      ENDDO
      IF( IRXN .LE. 0 )THEN
        WRITE(6,*)'ERROR IN CREATE_MODULE: SPECTRA_NAME NOT in PHOTAB array '
        STOP
      ENDIF

      SAFE_NAME = SPECTRA_NAME
      J = LEN(SAFE_NAME)
      DO I = 1, LEN(SAFE_NAME)
         IF( SAFE_NAME(I:I) .EQ. '-' ) SAFE_NAME(I:I) = '_'
      ENDDO          

      DO I = 1, NPHOT_DONE
         IF( SPECTRA_NAME .EQ. PHOT_DONE(I) )THEN
             WRITE(6,*)TRIM(SPECTRA_NAME),' already processed by ',TRIM(PHOT_DONE(I))
             RETURN
         ENDIF
      ENDDO
      NPHOT_DONE = NPHOT_DONE + 1
      PHOT_DONE(NPHOT_DONE) = PHOTAB(IRXN)


      CALL INTAVG_C(WLIN, CS_IN, NWLIN, SPECTRA_TYPE, NSO, WL, WU, XCOUT)
      CALL INTAVG_C(WLIN, QY_IN, NWLIN, SPECTRA_TYPE, NSO, WL, WU, QYOUT)

c---now ready to do any flux-weighted means over the bins
         FBIN(:) = 0.d0
         ABIN(:) = 0.0d0  
         CBIN(:) = 0.0d0  
         DBIN(:) = 0.0d0  
         EBIN(:) = 0.0d0  


      do J=K1,K2
        K = J - K1 + 1

        I = IBINJ_NEW(J)
        if (I .gt. 0) then
          WW = W(J)
          FBIN(I) = FBIN(I) + F(J)
          ABIN(I) = ABIN(I) + F(J)*DBLE(XCOUT(J))
          CBIN(I) = CBIN(I) + F(J)*DBLE(QYOUT(J))
          DBIN(I) = DBIN(I) + F(J)*DBLE(XCOUT(J))*DBLE(QYOUT(J))
          EBIN(I) = DBIN(I)
!          ABIN(I) = ABIN(I) + F(J)*XNEW
        endif
      enddo

      do I=1,NB_NEW
        if (ABIN(I) .gt. 0.d0)EBIN(I) = EBIN(I)/ABIN(I)
        if (FBIN(I) .gt. 0.d0) then
            ABIN(I) = ABIN(I)/FBIN(I)
            CBIN(I) = CBIN(I)/FBIN(I)
            DBIN(I) = DBIN(I)/FBIN(I)
        endif
      enddo

      print*,'completed first average in create module '

c---combine fast-JX bins: 
c---    non-SR bands (16:NB) are assigned a single JX bin
c---    SR bands are split (by Opacity Distrib Fn) into a range of JX bins
        FFBIN(:) = 0.d0
        AABIN(:) = 0.d0
        CCBIN(:) = 0.d0
        DDBIN(:) = 0.d0
        EEBIN(:) = 0.d0
        XCROSS(:,:)     = 0.d0
        YIELD(:,:)      = 0.d0
        XCROSS_EFF(:,:) = 0.d0



      FFBIN(:) = 0.d0
      do I=16,NB_NEW
!        J = IJX(I)
!        J = IJX_CALC(I)
        J = IJX_BIN_NEW( I )
        FFBIN(J) = FFBIN(J) + FBIN(I)
        AABIN(J) = AABIN(J) + FBIN(I)*ABIN(I)
        CCBIN(J) = CCBIN(J) + FBIN(I)*CBIN(I)
        DDBIN(J) = DDBIN(J) + FBIN(I)*DBIN(I)
        EEBIN(J) = EEBIN(J) + FBIN(I)*ABIN(I)*EBIN(I)
      enddo
      do I=1,15
        do J=1,NJO_NEW
          FFBIN(J) = FFBIN(J) + FBIN(I)*SRB_NEW(I,J)
          AABIN(J) = AABIN(J) + FBIN(I)*ABIN(I)*SRB_NEW(I,J)
          CCBIN(J) = CCBIN(J) + FBIN(I)*CBIN(I)*SRB_NEW(I,J)
          DDBIN(J) = DDBIN(J) + FBIN(I)*DBIN(I)*SRB_NEW(I,J)
          EEBIN(J) = EEBIN(J) + FBIN(I)*ABIN(I)*EBIN(I)*SRB_NEW(I,J)
        enddo
      enddo


 

      NWL_AVE = NJO_NEW
      WLL_AVE = 0.0
      WLU_AVE = 0.0
      CS_AVE  = 0.0
      QY_AVE  = 0.0



      do J = 1, 8
!        WLL_AVE( J ) = STR_WV_FASTJX( J )
!        WLU_AVE( J ) = END_WV_FASTJX( J )
        WLL_AVE( J ) = STWL_NEW( J )
        WLU_AVE( J ) = ENDWL_NEW( J )
      enddo 



      do J = 9, NJO_NEW
!        WLL_AVE( J ) = STR_WV_FASTJX( J + 2 )
!        WLU_AVE( J ) = END_WV_FASTJX( J + 2 )
        WLL_AVE( J ) = STWL_NEW( J + 2 )
        WLU_AVE( J ) = ENDWL_NEW( J + 2)
      enddo 


      do J=1,NJO_NEW
        if (AABIN(J) .gt. 0.d0) EEBIN(J) = EEBIN(J)/AABIN(J)
        if (FFBIN(J) .gt. 0.d0)THEN
            AABIN(J)    = AABIN(J)/FFBIN(J)
            CCBIN(J)    = CCBIN(J)/FFBIN(J)
            DDBIN(J)    = DDBIN(J)/FFBIN(J)
            CS_AVE( J ) = AABIN(J)
            QY_AVE( J ) = CCBIN(J)
        endif
      enddo

      print*,'completed second average in create module '


      DO ITTR = 1, N_TEMPERATURE ! note that NZ is 1 

         XCROSS(ITTR, 1:NJO_NEW) = CS_AVE(1:NJO_NEW)
         YIELD(ITTR, 1:NJO_NEW)  = EEBIN(1:NJO_NEW)
         
!         NUM_DENS( ITTR ) = DENS0 ( DENS0 *  T298K )/ TEMPERATURE(ITTR)

         DO J= 1, N_INLINE_BAND
            CS_PHOT( ITTR, J, IRXN)  = CS_AVE( NJO_NEW - N_INLINE_BAND + J )
            QY_PHOT( ITTR, J, IRXN)  = QY_AVE( NJO_NEW - N_INLINE_BAND + J ) 
            ECS_PHOT( ITTR, J, IRXN) = DDBIN(  NJO_NEW - N_INLINE_BAND + J )
            EQY_PHOT( ITTR, J, IRXN) = EEBIN(  NJO_NEW - N_INLINE_BAND + J )
         ENDDO
      ENDDO

      REPLACE = .FALSE.

      print*,'calling XC_QY_TD_EFFECT for ',SPECTRA_NAME


      CALL XC_QY_TD_EFFECT(EFFECTIVE_LAMBDA, NJO_NEW,
     &                     TEMPERATURE,NUM_DENS,N_TEMPERATURE,SPECTRA_NAME,
     &                     XCROSS,YIELD,XCROSS_EFF,REPLACE)


      LOOP_TROP: DO ITTR = 1, N_TEMPERATURE

        TEMP = TEMPERATURE(ITTR)
        
        IF( REPLACE )THEN
        
        
         WRITE(6,99951)TRIM( SPECTRA_NAME ),TEMP,NUM_DENS(ITTR)
99951    FORMAT('called XC_QY_TD_EFFECT for ',A,' at TEMP = ',f7.2,
     &          ' and # DENS = ', 1PE12.4)
     
          DO J= 1, N_INLINE_BAND
             CS_PHOT( ITTR, J, IRXN)  = XCROSS(ITTR,  NJO_NEW - N_INLINE_BAND + J )
             QY_PHOT( ITTR, J, IRXN)  = YIELD( ITTR,  NJO_NEW - N_INLINE_BAND + J ) 
             EQY_PHOT( ITTR, J, IRXN) = YIELD( ITTR,  NJO_NEW - N_INLINE_BAND + J ) 
          ENDDO
        ELSE
          print*,'using xcqy file for ',SPECTRA_NAME
        ENDIF

      ENDDO LOOP_TROP

       TEMP = 298.0
       write(6,'(A16,1X,A5,1X,F8.3,1X,40(1PE12.6,2X))')
     &    TRIM(SAFE_NAME), 'CS0', TEMP, 
     &    (AABIN(NJO_NEW - N_INLINE_BAND + J),J= 1, N_INLINE_BAND)


      DO I = 1, N_TEMPERATURE

        WRITE(JTABLE_UNIT,'(A16,1X,A5,1X,F8.3,1X,40(1PE12.6,2X))')SPECTRA_NAME,
     &       'CS',TEMPERATURE(I),(CS_PHOT( I, J, IRXN), J = 1, N_INLINE_BAND)
        WRITE(JTABLE_UNIT,'(A16,1X,A5,1X,F8.3,1X,40(1PE12.6,2X))')SPECTRA_NAME,
     &       'EQY',TEMPERATURE(I),(EQY_PHOT( I, J, IRXN), J = 1, N_INLINE_BAND)

        WRITE(6,'(A16,1X,A5,1X,F8.3,1X,40(1PE12.6,2X))')SPECTRA_NAME,
     &       'CS',TEMPERATURE(I),(CS_PHOT( I, J, IRXN), J = 1, N_INLINE_BAND)


      ENDDO

      print*, ' Wrote CSQY DATA for ',SPECTRA_NAME

      IF( SPECTRA_NAME .EQ. PHOTAB( MAX( NPHOTAB,1 ))  )THEN

! write out the ozone cross-section for stratospheric temperatures

          WRITE(JTABLE_UNIT,5010)
          WRITE(JTABLE_UNIT,'(A14,1X,I3)')'NTEMP_STRAT = ', N_TEMP_STRAT
          WRITE(JTABLE_UNIT,'(A7,7X,A8,3X,A4,3X,40(A5,I3,A1,5X))')'!      ',
     &    'QUANTITY','TEMP',('WBIN(',J,')', J = 1, N_INLINE_BAND)
          DO I = 1, N_TEMP_STRAT
             WRITE(JTABLE_UNIT,'(A16,1X,A5,1X,F8.3,1X,40(1PE12.6,2X))')'O3_STRAT        ',
     &       'CS',TEMP_STRAT(I),(O3_CS_STRAT( I, J), J = 1, N_INLINE_BAND)
          ENDDO

! write out the suface albedo data for each landuse

          WRITE(JTABLE_UNIT,5011)
          WRITE(JTABLE_UNIT,5013)'NUMB_LANDUSE_MODIS = ', NUMB_LANDUSE_MODIS
          WRITE(JTABLE_UNIT,5014)
          WRITE(JTABLE_UNIT,5013)'INDEX_GRASSLAND    = ', INDEX_GRASSLAND
          WRITE(JTABLE_UNIT,5013)'INDEX_OCEAN_WATER  = ', INDEX_OCEAN_WATER
          WRITE(JTABLE_UNIT,5013)'INDEX_SEA_ICE     = ', INDEX_SEA_ICE
          WRITE(JTABLE_UNIT,5009)'! I','LANDUSE_CATEGORY',
     &   'ZENITH_ANGLE_COEFF', 'SEASONAL_COEFF','SNOW_COVER_COEEF'
          DO I = 1, NUMB_LANDUSE_MODIS
            WRITE(JTABLE_UNIT,5012)I, LANDUSE( I ), 
     &      ZENITH_COEFF( I ), SEASON_COEFF( I ), SNOW_COEFF( I )
          ENDDO
          WRITE(JTABLE_UNIT,'(A7,7X,A16,2X,40(A5,I3,A1,5X))')'!      ',
     &    'LANDUSE CATEGORY',('WBIN(',J,')', J = 1, N_INLINE_BAND)
          DO I = 1, NUMB_LANDUSE_MODIS
             WRITE(JTABLE_UNIT,'(A30,1X,40(1PE12.6,2X))')
     &       TRIM(LANDUSE(I)),(MODIS_ALBEDO( NJO_NEW - N_INLINE_BAND + J, I), J = 1, N_INLINE_BAND)
          ENDDO

          WRITE(JTABLE_UNIT,5015)

          WRITE(JTABLE_UNIT,5013)'NUMB_NLCD_MODIS = ', NUMB_NLCD_MODIS
          WRITE(JTABLE_UNIT,5009)'! I','LANDUSE_NLCD-MODIS','INDEX_ALBREF',
     &    'FAC_ALBREF'
          DO I = 1, NUMB_NLCD_MODIS
            WRITE(JTABLE_UNIT,5016)I, NLCD_LANDUSE( I ), 
     &      INT(ALBMAP_REF2NLCD( I )), ALBFAC_REF2NLCD( I )
          ENDDO

          WRITE(JTABLE_UNIT,5013)'NUMB_USGS = ', NUMB_USGS
          WRITE(JTABLE_UNIT,5009)'! I','LANDUSE_USGS','INDEX_ALBREF',
     &    'FAC_ALBREF'
          DO I = 1, NUMB_USGS
            WRITE(JTABLE_UNIT,5016)I, USGS_LANDUSE( I ), 
     &      INT(ALBMAP_REF2USGS( I )), ALBFAC_REF2USGS( I )
          ENDDO

          WRITE(JTABLE_UNIT,5013)'NUMB_MODIS_NOAH = ', NUMB_MODIS_NOAH
          WRITE(JTABLE_UNIT,5009)'! I','LANDUSE_MODIS-NOAH','INDEX_ALBREF',
     &    'FAC_ALBREF'
          DO I = 1, NUMB_MODIS_NOAH
            WRITE(JTABLE_UNIT,5016)I, MODIS_LANDUSE( I ), 
     &      INT(ALBMAP_REF2MODIS( I )), ALBFAC_REF2MODIS( I )
          ENDDO


          WRITE(JTABLE_UNIT,5013)'NUMB_NLCD40_MODIS = ', NUMB_NLCD40_MODIS
          WRITE(JTABLE_UNIT,5009)'! I','LANDUSE_NLCD40-MODIS','INDEX_ALBREF',
     &    'FAC_ALBREF'
          DO I = 1, NUMB_NLCD40_MODIS
            WRITE(JTABLE_UNIT,5016)I, NLCD40_LANDUSE( I ), 
     &      INT(ALBMAP_REF2NLCD40( I )), ALBFAC_REF2NLCD40( I )
          ENDDO


      ENDIF

      RETURN


1001  FORMAT(A16,22X,F5.1,2X,F5.1)
1003  FORMAT(61X,F9.1)
2007  format('!...  CS  = absorption cross sections averaged over UCI Solar Flux')
2009  format('!...  QY  = quantum yields averaged over UCI Solar Flux')
3010  format('!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model')
5001  FORMAT('! Cloud properties and aerosol refractive indices are weighted bin averages.')
5002  FORMAT('! The former is based on HU & Stamnes (1993), An accurate parameterization of')
5003  FORMAT('! the radiative properties of water clouds suitable for use in climate models,')
5004  FORMAT('! J. of Climate, vol. 6, pp. 728-742. The values were calculated with an')
5005  FORMAT('! equivalent radius of 10 micrometers. ')
5006  FORMAT('! Note: Cloud extinction/LWC is in [1/m] over [g/m **3] where LWC is in g/m**3',
     &       /, '! ')
5007  FORMAT('! Complex refractive indices for aerosol components are based on ',
     &       /,'! 1) Hess, M. P. Koepke, and I. Schult, Optical properties of aerosols and',
     &       /,'! clouds: The software package OPAC, Bulletin of the American Meteorological', 
     &       /,'! Society, Vol 79, No 5, pp 831 - 844, May 1998. Available at',
     &       /,'! www.lrz-muenchen.de/~uh234an/www/radaer/opac.htm',
     &       /,'! 2) Segelstein, D., 1981: The Complex Refractive Index of Water, M.S. Thesis,',
     &       /,'! University of Missouri--Kansas City, MO' )
!5008  FORMAT(I3,1X,3(F8.3,2X),2X,ES12.4,2X,2(F8.3,2X),ES12.4,2X,8(F8.3,1X,ES12.4,1X))
5008  FORMAT(I3,', ',3(F8.3,', '),ES12.4,', ',2(F8.3,', '),ES12.4,5(', ',F8.3,', ',ES12.4))
!5009  FORMAT(A3, 20(', ',A))
5009  FORMAT(A3,', ',3(A,', '),A,', ',2(A,', '),A,5(', ',A,', ',A))
5010  FORMAT('! Ozone Cross-Section for Stratospheric Opacity based on Table 4-5 in',
     &       /,'! Chemical Kinetics and Photochemical Data for Use in Atmospheric',
     &       /,'! Studies Evaluation Number 15. Sander et. al: NASA-JPL 2006 ')
5011  FORMAT('! Surface Albedo with wavelength dependence for 20 MODIS land use types.',
     &       /,'!  Correction factors are included for solar zenith angle and season.',
     &       /,'! Note that an adjustment is used better match spectral estimates.',
     &       /,'! REFERENCE:  CERES/SARB Surface Properties database, May 2006',
     &       /,'! http://www-surf.larc.nasa.gov/surf/pages/explan.html.')
5012  FORMAT(I3,1X,A30,1X,3(F8.3,2X))
5013  FORMAT(A21,1X,I3)
5014  FORMAT('! Following two indices denote the categories assigned as generic',
     &       /,'! land and water surfaces. The surfaces are used as the default',
     &       /,'! land characterization scheme if scheme used is not known.')
5015  FORMAT('! Following maps the above albedo data to three land characterization',
     &       /,'! schemes commonly used in CMAQ applications.',
     &       /,'! ')
5016  FORMAT(I3,1X,A60,1X,I3,2X,3(F8.3,2X))

      END SUBROUTINE WRT_CSQY_DATA
