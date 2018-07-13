      SUBROUTINE WRT_OPTICS_DATA( )

      USE GET_ENV_VARS       
      USE BIN_DATA
      USE CSQY_REFER_DATA
      USE ALBEDO_REFER_DATA

      IMPLICIT NONE      
      
!Purpose: compute then write suface albedo, cloud water and ice optical data to file
!         as a CMAQ input

!..........ARGUMENTS and their descriptions: NONE


!...........LOCAL VARIABLES and their descriptions:
      
      CHARACTER(16)   ::  PNAME  = 'WRT_OPTICS_DATA'    ! program name
      CHARACTER(80)   ::  MSG    = ' '                ! message

!...........PARAMETERS and their descriptions
      REAL, PARAMETER    :: CLOUD_RADIUS_LIQUID = 10.0 ! mean cloud droplet radii, um
      REAL               :: PARTICLE_SIZE              ! hydrometer size for calculating optical properties

      INTEGER, PARAMETER :: MAX_LEN_WORD = 16
      INTEGER, PARAMETER :: NRADIUS_LIQUID    = 58  ! number of liquid droplets to compute data
      REAL, PARAMETER    :: LIQUID_RADIUS_INT = 2.5 ! starting liquid droplet radii, um
      REAL, PARAMETER    :: LIQUID_RADIUS_INC = 1.0 ! increment for liquid droplet radii, um
      
      REAL, PARAMETER    :: MAX_COALB = 9.999999E-1 ! Maximum value for single scattering coalbedo


      INTEGER, PARAMETER :: NDIAM_ICE      = 46  ! number of ice droplets to compute data 
      REAL, PARAMETER    :: ICE_DIAM_INT   = 5.0 ! starting ice droplet diameter, um
      REAL, PARAMETER    :: ICE_DIAM_INC   = 3.0 ! increment for ice droplet diameter, um

      INTEGER, SAVE      :: NAERO_REFRACT      = 5      
      INTEGER, SAVE      :: MAX_NAERO_REFRACT  = 5      

!Local:

      INTEGER                 :: I, J, K    ! index counters
      INTEGER                 :: ITTR       ! array index
      INTEGER                 :: ENV_STAT 


      CHARACTER(255)          :: EQNAME
      CHARACTER(600)          :: ODATA_NAME

      LOGICAL, SAVE           :: FIRSTCALL  = .TRUE.

      REAL, ALLOCATABLE, SAVE ::  WC_EXT( :, : )
      REAL, ALLOCATABLE, SAVE ::  WC_ASF( :, : )
      REAL, ALLOCATABLE, SAVE ::  WC_SSA( :, : )

      REAL, ALLOCATABLE, SAVE ::  ICE_EXT( :, : )
      REAL, ALLOCATABLE, SAVE ::  ICE_ASY( :, : )
      REAL, ALLOCATABLE, SAVE ::  ICE_SSA( :, : )
      REAL, ALLOCATABLE, SAVE ::  ICE_DEL( :, : )     ! Delta function transmission 


      LOGICAL, SAVE      :: WRITE_AE_REFRACT = .FALSE. ! .FALSE.

      CHARACTER(   5 )    :: WVL_AE_REFRAC
      CHARACTER(  13 )    :: AERO_INDEX       = 'WVL_AE_REFRAC'
      CHARACTER(  32 )    :: ENV_VAR_NAME  
      CHARACTER( 256 )    :: ENV_VAR_LIST  
      
       CHARACTER( MAX_LEN_WORD ), ALLOCATABLE, SAVE :: AERO_REFRACT_INDX( : ) 
!      CHARACTER( MAX_LEN_WORD )    :: AERO_REFRACT_INDX( NAERO_REFRACT ) 
!      DATA  AERO_REFRACT_INDX / "WATER", "SOLUTE", "DUST", "SEASALT", "SOOT" /
      
      INTEGER                      :: NVARS
      CHARACTER( MAX_LEN_WORD ), ALLOCATABLE :: VAR_LIST( : )

      REAL, ALLOCATABLE :: AERO_IMAG_REFRACT( :, : )
      REAL, ALLOCATABLE :: AERO_REAL_REFRACT( :, : )

      REAL, ALLOCATABLE, SAVE :: MODIS_ALBEDO( :, :) ! NJO, NUMB_LANDUSE_MODIS )

      INTERFACE
        SUBROUTINE OPTICS_WATER_CLOUD(CLOUD_RADIUS_LIQUID, WC_EXT, WC_ASF, WC_SSA)
           REAL, INTENT( IN )  :: CLOUD_RADIUS_LIQUID
           REAL, INTENT( OUT ) :: WC_EXT( : )
           REAL, INTENT( OUT ) :: WC_ASF( : )
           REAL, INTENT( OUT ) :: WC_SSA( : )
        END SUBROUTINE OPTICS_WATER_CLOUD
        SUBROUTINE OPTICS_ICE_CLOUD(CLOUD_DIAMETER_ICE, ICE_EXT, ICE_ASY, ICE_SSA, ICE_DEL)
           REAL, INTENT( IN )  :: CLOUD_DIAMETER_ICE   ! effective ice droplet size or diameter, um
           REAL, INTENT( OUT ) :: ICE_EXT( : )         ! (cloud extinction coef)/IWC, 1/m*/(g/m**3)
           REAL, INTENT( OUT ) :: ICE_ASY( : )         !  cloud asymmetry factor
           REAL, INTENT( OUT ) :: ICE_SSA( : )         !  cloud coalbedo
           REAL, INTENT( OUT ) :: ICE_DEL( : )         !  Delta function transmission
        END SUBROUTINE OPTICS_ICE_CLOUD
        SUBROUTINE AERO_REFRACT_INDEX ( NAERO_REFRACT, AERO_REFRACT_INDX, AERO_REAL_REFRACT,
     &                                AERO_IMAG_REFRACT )
            INTEGER,         INTENT( IN )  :: NAERO_REFRACT             ! number of refractive indices
            CHARACTER( 16 ), INTENT( IN )  :: AERO_REFRACT_INDX( : )    ! names of refractive indices
            REAL,            INTENT( OUT ) :: AERO_IMAG_REFRACT( :, : ) ! imaginary part of index [Dimensionaless]
            REAL,            INTENT( OUT ) :: AERO_REAL_REFRACT( :, : ) ! real part of index [Dimensionaless]
        END SUBROUTINE AERO_REFRACT_INDEX
        SUBROUTINE SPECTRAL_REFLECT(MODIS_ALBEDO)
           REAL, INTENT( OUT ) :: MODIS_ALBEDO( :, : )
        END SUBROUTINE SPECTRAL_REFLECT
        SUBROUTINE CONVERT_CASE ( BUFFER, UPPER )
            CHARACTER(LEN= *), INTENT( INOUT ) :: BUFFER
            LOGICAL,           INTENT( IN    ) :: UPPER
        END SUBROUTINE CONVERT_CASE
       END INTERFACE


        CALL INIT_BIN_DATA
        
!        EQNAME = 'OUT_DIR'

        ODATA_NAME = TRIM( OUT_DIR )
     &            // '/'  // 'PHOT_OPTICS.dat'
     
        OPEN(ODATA_UNIT,  FILE = ODATA_NAME, STATUS = 'UNKNOWN')
          
        IF( FIRSTCALL )THEN
            FIRSTCALL = .FALSE.
            
            ALLOCATE(  WC_EXT( NJO_NEW, NRADIUS_LIQUID ),
     &                 WC_ASF( NJO_NEW, NRADIUS_LIQUID ),
     &                 WC_SSA( NJO_NEW, NRADIUS_LIQUID ) )

            ALLOCATE(  ICE_EXT( NJO_NEW, NRADIUS_LIQUID ),  
     &                 ICE_ASY( NJO_NEW, NRADIUS_LIQUID ),  
     &                 ICE_SSA( NJO_NEW, NRADIUS_LIQUID ),
     &                 ICE_DEL( NJO_NEW, NRADIUS_LIQUID ))
              
            ENV_VAR_NAME = 'MAX_NUMB_REFRACT'
            MAX_NAERO_REFRACT = GET_ENV_INT( ENV_VAR_NAME, ' ', MAX_NAERO_REFRACT, ENV_STAT)
            PRINT*,'MAX_NUMB_REFRACT = ', MAX_NAERO_REFRACT
            
            ENV_VAR_LIST = 'AE_REFRAC_LIST'
            ALLOCATE( VAR_LIST( MAX_NAERO_REFRACT ) )
            CALL GET_ENVLIST ( ENV_VAR_LIST, NVARS, VAR_LIST, ENV_STAT )
            IF( NVARS .GT. 0 )THEN
                NAERO_REFRACT = NVARS
                ALLOCATE( AERO_REFRACT_INDX( NAERO_REFRACT ) )
                DO I = 1, NVARS
                   AERO_REFRACT_INDX( I ) =  VAR_LIST( I )
                END DO
            END IF
            ALLOCATE( AERO_IMAG_REFRACT( NJO_NEW, NAERO_REFRACT ) )
            ALLOCATE( AERO_REAL_REFRACT( NJO_NEW, NAERO_REFRACT ) )
        END IF
        
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


         WRITE(ODATA_UNIT,4997)'N_INLINE_BAND = ', N_INLINE_BAND


4997     FORMAT(A16, 1X, I3)
4998     FORMAT(A20, 1X, 20(A, ', '), A)
4999     FORMAT(2(A16, 1X, I3, ', '), A20, 1X, 4(A, ', '), A)

         WRITE(ODATA_UNIT,5001)

         CALL OPTICS_WATER_CLOUD(CLOUD_RADIUS_LIQUID, WC_EXT(:,1), WC_ASF(:,1), WC_SSA(:,1))

         WRITE(ODATA_UNIT,5009)'! I','START_WL_BIN(nm)',
     &   'EFFECT_WL_BIN_(nm)', 'END_WL_BIN_(nm)','photon_flux(cm-2*s-1)',
     &   'CLD_EXT/LWC_(m2/g)','CLD_ASYM_FACT','CLD_COALBEDO'
     
         DO I = 1, N_INLINE_BAND
            ITTR = NJO_NEW - N_INLINE_BAND + I   
            WRITE(ODATA_UNIT,5008)I,
     &      STWL_NEW(ITTR+2), EFFECTIVE_LAMBDA(ITTR), ENDWL_NEW(ITTR+2),
     &      SOLAR_PHOTONS( ITTR ), WC_EXT(ITTR, 1), WC_ASF(ITTR, 1), WC_SSA(ITTR, 1)
         ENDDO

         IF( WRITE_AE_REFRACT )THEN
         

            AERO_IMAG_REFRACT = 0.0
            AERO_REAL_REFRACT = 0.0

            CAll AERO_REFRACT_INDEX ( NAERO_REFRACT, AERO_REFRACT_INDX, AERO_REAL_REFRACT,
     &                                AERO_IMAG_REFRACT )

             WRITE(ODATA_UNIT,5007)
             WRITE(ODATA_UNIT,4999)'NAERO_REFRACT = ', NAERO_REFRACT
             WRITE(ODATA_UNIT,4998)'NAMES_REFRACT_INDX = ', 
     &       ( TRIM( AERO_REFRACT_INDX( I ) ), I = 1, NAERO_REFRACT )
     
             WRITE(ODATA_UNIT,6009)'! I','STRT_WVL(nm)',
     &       'EFFECT_WVL(nm)', 'END_WVL(nm)',
     &       ( 'NR_' // TRIM( AERO_REFRACT_INDX( I ) ), 'NI_' // TRIM( AERO_REFRACT_INDX ( I ) ),
     &          I = 1, NAERO_REFRACT )

            DO I = 1, N_INLINE_BAND
               ITTR = NJO_NEW - N_INLINE_BAND + I            
               WRITE(ODATA_UNIT,6008)I,
     &         STWL_NEW(ITTR+2), EFFECTIVE_LAMBDA(ITTR), ENDWL_NEW(ITTR+2),
     &         ( AERO_REAL_REFRACT( ITTR, J ), AERO_IMAG_REFRACT( ITTR, J ), 
     &           J = 1, NAERO_REFRACT )
            ENDDO
            
         END IF

         WRITE(ODATA_UNIT,6001)
         WRITE(ODATA_UNIT,5002)
         WRITE(ODATA_UNIT,5003)
         WRITE(ODATA_UNIT,5004)
         WRITE(ODATA_UNIT,5005)
         WRITE(ODATA_UNIT,5006)

         WRITE(ODATA_UNIT,4997)'NRADIUS_LIQUID = ', NRADIUS_LIQUID

         WRITE(ODATA_UNIT,6011)'QUANTITY','Radius, um',
     &   (EFFECTIVE_LAMBDA(NJO_NEW - N_INLINE_BAND + J), J = 1, N_INLINE_BAND)

6011     FORMAT(A8,1X,A10,40(1X,F8.1," nm "))

! compute and write out table for liquid droplet properties


         PARTICLE_SIZE = LIQUID_RADIUS_INT
         DO J = 1, NRADIUS_LIQUID
            CALL OPTICS_WATER_CLOUD(PARTICLE_SIZE, WC_EXT(:,J), WC_ASF(:,J), WC_SSA(:,J))
            PARTICLE_SIZE = PARTICLE_SIZE + LIQUID_RADIUS_INC
         END DO

         PARTICLE_SIZE = LIQUID_RADIUS_INT
         DO J = 1, NRADIUS_LIQUID

           WRITE(ODATA_UNIT,'(A8,1X,F10.3)',ADVANCE = 'NO')'LIQ_EXT',PARTICLE_SIZE
           DO I = 1, N_INLINE_BAND
              ITTR = NJO_NEW - N_INLINE_BAND + I   
              WRITE(ODATA_UNIT,'(1X,1PE12.4)',ADVANCE = 'NO')WC_EXT(ITTR, J)
           ENDDO
           PARTICLE_SIZE = PARTICLE_SIZE + LIQUID_RADIUS_INC
           WRITE(ODATA_UNIT,*)
           
         END DO 

         WRITE(ODATA_UNIT,6011)'QUANTITY','Radius, um',
     &   (EFFECTIVE_LAMBDA(NJO_NEW - N_INLINE_BAND + J), J = 1, N_INLINE_BAND)

         PARTICLE_SIZE = LIQUID_RADIUS_INT
         DO J = 1, NRADIUS_LIQUID

           WRITE(ODATA_UNIT,'(A8,1X,F10.3)',ADVANCE = 'NO')'LIQ_ASY',PARTICLE_SIZE
           DO I = 1, N_INLINE_BAND
              ITTR = NJO_NEW - N_INLINE_BAND + I   
              WRITE(ODATA_UNIT,'(1X,1PE12.4)',ADVANCE = 'NO')WC_ASF(ITTR, J)
           ENDDO
           PARTICLE_SIZE = PARTICLE_SIZE + LIQUID_RADIUS_INC
           WRITE(ODATA_UNIT,*)
           
         END DO 

         WRITE(ODATA_UNIT,6011)'QUANTITY','Radius, um',
     &   (EFFECTIVE_LAMBDA(NJO_NEW - N_INLINE_BAND + J), J = 1, N_INLINE_BAND)

         PARTICLE_SIZE = LIQUID_RADIUS_INT
         DO J = 1, NRADIUS_LIQUID

           WRITE(ODATA_UNIT,'(A8,1X,F10.3)',ADVANCE = 'NO')'LIQ_COA',PARTICLE_SIZE
           DO I = 1, N_INLINE_BAND
              ITTR = NJO_NEW - N_INLINE_BAND + I   
              WRITE(ODATA_UNIT,'(1X,1PE12.4)',ADVANCE = 'NO')
     &        MAX(0.0, MIN(MAX_COALB, WC_SSA(ITTR, J)))
           ENDDO
           PARTICLE_SIZE = PARTICLE_SIZE + LIQUID_RADIUS_INC
           WRITE(ODATA_UNIT,*)
           
         END DO 
         
! compute and write out tables for ice droplet properties
         
         WRITE(ODATA_UNIT,7001)
         WRITE(ODATA_UNIT,7002)
         WRITE(ODATA_UNIT,7003)
         WRITE(ODATA_UNIT,7004)
         WRITE(ODATA_UNIT,7005)
         WRITE(ODATA_UNIT,7006)

         WRITE(ODATA_UNIT,4997)'NDIAM_ICE = ', NDIAM_ICE

         PARTICLE_SIZE = ICE_DIAM_INT
         DO J = 1, NDIAM_ICE
            CALL OPTICS_ICE_CLOUD(PARTICLE_SIZE, ICE_EXT(:,J), ICE_ASY(:,J), ICE_SSA(:,J),
     &                            ICE_DEL(:,J))
            PARTICLE_SIZE = PARTICLE_SIZE + ICE_DIAM_INC
         END DO

         WRITE(ODATA_UNIT,6011)'QUANTITY','EDIAM, um',
     &   (EFFECTIVE_LAMBDA(NJO_NEW - N_INLINE_BAND + J), J = 1, N_INLINE_BAND)

         PARTICLE_SIZE = ICE_DIAM_INT
         DO J = 1, NDIAM_ICE

           WRITE(ODATA_UNIT,'(A8,1X,F10.3)',ADVANCE = 'NO')'ICE_EXT',PARTICLE_SIZE
           DO I = 1, N_INLINE_BAND
              ITTR = NJO_NEW - N_INLINE_BAND + I   
              WRITE(ODATA_UNIT,'(1X,1PE12.4)',ADVANCE = 'NO')ICE_EXT(ITTR, J)
           ENDDO
           PARTICLE_SIZE = PARTICLE_SIZE + ICE_DIAM_INC
           WRITE(ODATA_UNIT,*)
           
         END DO 
         
         WRITE(ODATA_UNIT,6011)'QUANTITY','EDIAM, um',
     &   (EFFECTIVE_LAMBDA(NJO_NEW - N_INLINE_BAND + J), J = 1, N_INLINE_BAND)

         PARTICLE_SIZE = ICE_DIAM_INT
         DO J = 1, NDIAM_ICE

           WRITE(ODATA_UNIT,'(A8,1X,F10.3)',ADVANCE = 'NO')'ICE_ASY',PARTICLE_SIZE
           DO I = 1, N_INLINE_BAND
              ITTR = NJO_NEW - N_INLINE_BAND + I   
              WRITE(ODATA_UNIT,'(1X,1PE12.4)',ADVANCE = 'NO')ICE_ASY(ITTR, J)
           ENDDO
           PARTICLE_SIZE = PARTICLE_SIZE + ICE_DIAM_INC
           WRITE(ODATA_UNIT,*)
           
         END DO 

         WRITE(ODATA_UNIT,6011)'QUANTITY','EDIAM, um',
     &   (EFFECTIVE_LAMBDA(NJO_NEW - N_INLINE_BAND + J), J = 1, N_INLINE_BAND)

         PARTICLE_SIZE = ICE_DIAM_INT
         DO J = 1, NDIAM_ICE

           WRITE(ODATA_UNIT,'(A8,1X,F10.3)',ADVANCE = 'NO')'ICE_COA',PARTICLE_SIZE
           DO I = 1, N_INLINE_BAND
              ITTR = NJO_NEW - N_INLINE_BAND + I   
              WRITE(ODATA_UNIT,'(1X,1PE12.4)',ADVANCE = 'NO')
     &        MAX(0.0, MIN(MAX_COALB, ICE_SSA(ITTR, J)))
           ENDDO
           PARTICLE_SIZE = PARTICLE_SIZE + ICE_DIAM_INC
           WRITE(ODATA_UNIT,*)
           
         END DO 

         WRITE(ODATA_UNIT,6011)'QUANTITY','EDIAM, um',
     &   (EFFECTIVE_LAMBDA(NJO_NEW - N_INLINE_BAND + J), J = 1, N_INLINE_BAND)

         PARTICLE_SIZE = ICE_DIAM_INT
         DO J = 1, NDIAM_ICE

           WRITE(ODATA_UNIT,'(A8,1X,F10.3)',ADVANCE = 'NO')'ICE_DEL',PARTICLE_SIZE
           DO I = 1, N_INLINE_BAND
              ITTR = NJO_NEW - N_INLINE_BAND + I   
              WRITE(ODATA_UNIT,'(1X,1PE12.4)',ADVANCE = 'NO')
     &        MAX(0.0, MIN(MAX_COALB, ICE_DEL(ITTR, J)))
           ENDDO
           PARTICLE_SIZE = PARTICLE_SIZE + ICE_DIAM_INC
           WRITE(ODATA_UNIT,*)
           
         END DO 

         ALLOCATE( MODIS_ALBEDO( NJO_NEW, NUMB_LANDUSE_MODIS ) )

         CALL SPECTRAL_REFLECT(MODIS_ALBEDO)

! write out the suface albedo data for each landuse

          WRITE(ODATA_UNIT,5011)
          WRITE(ODATA_UNIT,5013)'NUMB_LANDUSE_MODIS = ', NUMB_LANDUSE_MODIS
          WRITE(ODATA_UNIT,5014)
          WRITE(ODATA_UNIT,5013)'INDEX_GRASSLAND    = ', INDEX_GRASSLAND
          WRITE(ODATA_UNIT,5013)'INDEX_OCEAN_WATER  = ', INDEX_OCEAN_WATER
          WRITE(ODATA_UNIT,5013)'INDEX_SEA_ICE     = ', INDEX_SEA_ICE
          WRITE(ODATA_UNIT,5009)'! I','LANDUSE_CATEGORY',
     &   'ZENITH_ANGLE_COEFF', 'SEASONAL_COEFF','SNOW_COVER_COEEF'
          DO I = 1, NUMB_LANDUSE_MODIS
            WRITE(ODATA_UNIT,5012)I, LANDUSE( I ), 
     &      ZENITH_COEFF( I ), SEASON_COEFF( I ), SNOW_COEFF( I )
          ENDDO
          WRITE(ODATA_UNIT,'(A7,7X,A16,2X,40(A5,I3,A1,5X))')'!      ',
     &    'LANDUSE CATEGORY',('WBIN(',J,')', J = 1, N_INLINE_BAND)
          DO I = 1, NUMB_LANDUSE_MODIS
             WRITE(ODATA_UNIT,'(A30,1X,40(1PE12.6,2X))')
     &       TRIM(LANDUSE(I)),(MODIS_ALBEDO( NJO_NEW - N_INLINE_BAND + J, I), J = 1, N_INLINE_BAND)
          ENDDO

          WRITE(ODATA_UNIT,5015)

          WRITE(ODATA_UNIT,5013)'NUMB_NLCD_MODIS = ', NUMB_NLCD_MODIS
          WRITE(ODATA_UNIT,5009)'! I','LANDUSE_NLCD-MODIS','INDEX_ALBREF',
     &    'FAC_ALBREF'
          DO I = 1, NUMB_NLCD_MODIS
            WRITE(ODATA_UNIT,5016)I, NLCD_LANDUSE( I ), 
     &      INT(ALBMAP_REF2NLCD( I )), ALBFAC_REF2NLCD( I )
          ENDDO

          WRITE(ODATA_UNIT,5013)'NUMB_USGS = ', NUMB_USGS
          WRITE(ODATA_UNIT,5009)'! I','LANDUSE_USGS','INDEX_ALBREF',
     &    'FAC_ALBREF'
          DO I = 1, NUMB_USGS
            WRITE(ODATA_UNIT,5016)I, USGS_LANDUSE( I ), 
     &      INT(ALBMAP_REF2USGS( I )), ALBFAC_REF2USGS( I )
          ENDDO

          WRITE(ODATA_UNIT,5013)'NUMB_MODIS_NOAH = ', NUMB_MODIS_NOAH
          WRITE(ODATA_UNIT,5009)'! I','LANDUSE_MODIS-NOAH','INDEX_ALBREF',
     &    'FAC_ALBREF'
          DO I = 1, NUMB_MODIS_NOAH
            WRITE(ODATA_UNIT,5016)I, MODIS_LANDUSE( I ), 
     &      INT(ALBMAP_REF2MODIS( I )), ALBFAC_REF2MODIS( I )
          ENDDO


          WRITE(ODATA_UNIT,5013)'NUMB_NLCD40_MODIS = ', NUMB_NLCD40_MODIS
          WRITE(ODATA_UNIT,5009)'! I','LANDUSE_NLCD40-MODIS','INDEX_ALBREF',
     &    'FAC_ALBREF'
          DO I = 1, NUMB_NLCD40_MODIS
            WRITE(ODATA_UNIT,5016)I, NLCD40_LANDUSE( I ), 
     &      INT(ALBMAP_REF2NLCD40( I )), ALBFAC_REF2NLCD40( I )
          ENDDO

      CLOSE( ODATA_UNIT )


1001  FORMAT(A16,22X,F5.1,2X,F5.1)
1003  FORMAT(61X,F9.1)
5001  FORMAT('! Wavelength, Optical and Surface Albedo Parameters for CMAQ In-Line Photolysis',
     &     /, '! calculation . The values are determined by averaging technique over the above',
     &     /, '! number of wavelength bins. The technique employs an approach used by the FASTJX', 
     &     /, '! versions 6 and 7.',
     &     /, '!References:',
     &     /, '! 1) Bian, H. S. and Prather, M. J.: Fast-J2: accurate simulation of stratospheric',
     &     /, '!    photolysis in global chemical models, J. Atmos. Chem., 41, 281-296', 
     &     /, '!    doi:10.1023/A:1014980619462, 2002.',
     &     /, '! 2) Hsu, J. and Prather, M. J.: Stratospheric variability and tropospheric ozone,',
     &     /, '!    J. Geophys. Res., 114, D06102, doi:10.1029/2008JD010942, 2009.',
     &     /, '! 3) Prather, M. J.: Fast-JX versions and utilities, available at:',
     &     /, '!    http://www.ess.uci.edu/researchgrp/prather/scholar_software/fast-jx',
     &     /, '!    (last accessed: 24 January 2014), 2014.',
     &     /, '!Last three numbers are cloud droplet optical properties for 10um radius (Hu & Stamnes, 1993)')
5002  FORMAT('! Values are based on Hu & Stamnes (1993), An accurate parameterization of')
5003  FORMAT('! the radiative properties of water clouds suitable for use in climate models,')
5004  FORMAT('! J. of Climate, vol. 6, pp. 728-742.')
5006  FORMAT('!  LIQ_EXT=extinction coefficient, LIQ_ASY=asymmeter factor, LIQ_COA=single scattering co-albedo')
5005  FORMAT('! Note: Cloud extinction/Liquid Water Content is in [1/m] over [g/m **3] where LWC is in g/m**3',
     &       /, '! ')
5007  FORMAT('! Complex refractive indices for aerosol components are based on ',
     &       /,'! 1) Hess, M. P. Koepke, and I. Schult, Optical properties of aerosols and',
     &       /,'! clouds: The software package OPAC, Bulletin of the American Meteorological', 
     &       /,'! Society, Vol 79, No 5, pp 831 - 844, May 1998. Available at',
     &       /,'! www.lrz-muenchen.de/~uh234an/www/radaer/opac.htm',
     &       /,'! 2) Segelstein, D., 1981: The Complex Refractive Index of Water, M.S. Thesis,',
     &       /,'! University of Missouri--Kansas City, MO' )
5008  FORMAT(I3,', ',3(F21.3,', '),ES21.4,', ',2(F21.3,', '),ES21.4,5(', ',F21.3,', ',ES21.4))
5009  FORMAT(A3,', ',3(A21,', '),A21,', ',2(A21,', '),A21,5(', ',A21,', ',A21))
5010  FORMAT('! Ozone Cross-Section for Stratospheric Opacity based on Table 4-5 in',
     &       /,'! Chemical Kinetics and Photochemical Data for Use in Atmospheric',
     &       /,'! Studies Evaluation Number 15. Sander et. al: NASA-JPL 2006 ')
5011  FORMAT('! Surface Albedo with wavelength dependence for 20 MODIS land use types.',
     &       /,'!  Correction factors are included for solar zenith angle and season.',
     &       /,'! Note that an adjustment is used better match spectral estimates.',
     &       /,'! REFERENCE:  CERES/SARB Surface Properties database',
     &       /,'! http://www-surf.larc.nasa.gov/surf/pages/explan.html',
     &       /,'!(last accessed: 30 January 2014).')
5012  FORMAT(I3,1X,A30,1X,3(F8.3,2X))
5013  FORMAT(A21,1X,I3)
5014  FORMAT('! Following two indices denote the categories assigned as generic',
     &       /,'! land and water surfaces. The surfaces are used as the default',
     &       /,'! land characterization scheme if scheme used is not known.')
5015  FORMAT('! Following maps the above albedo data to three land characterization',
     &       /,'! schemes commonly used in CMAQ applications.',
     &       /,'!   ')
5016  FORMAT(I3,1X,A60,1X,I3,2X,3(F8.3,2X))

6001  FORMAT('! Liquid Droplet Optical Properties versus Effective Wavelength and equivalent radius.')
6008  FORMAT(I3,', ',3(F14.3,', '),20(F11.3,', ',ES11.4,', '),F11.3,', ',ES11.4)
6009  FORMAT(A3,', ',3(A14,', '),20(A11,', ',A11,', '),A11,', ',A11)
7001  FORMAT('! Ice Particle Optical Properties versus Effective Wavelength and Effective Diameter.')
7002  FORMAT('! Values are based on Fu (1996), An accurate parameterization of')
7003  FORMAT('! the solar radiative properties of cirrus ice  suitable for climate models,')
7004  FORMAT('! J. of Climate, vol. 9, pp. 2058-2082.')
7005  FORMAT('! ICE_EXT=extinction coefficient, ICE_ASY=asymmeter factor, ICE_COA=co-albedo, ICE_DEL=Delta Transmission function')
7006  FORMAT('! Note: Extinction/Ice Water Content is in [1/m] over [g/m **3] where IWC is in g/m**3')
      RETURN
      END SUBROUTINE WRT_OPTICS_DATA
