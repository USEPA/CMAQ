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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE EMIS_UTIL

!-----------------------------------------------------------------------
! Function: Define Emissions Utility Subroutines

! Revision History:
!     21 July 2020 B.Murphy: Extracted these variables from EMIS_DEFN
!-----------------------------------------------------------------------
      USE EMIS_VARS

      CONTAINS

!-----------------------------------------------------------------------
      SUBROUTINE INTERPRET_EM_RULE_STREAM( RULE_STREAM0, IRULE, 
     &              EM_STREAM_RULE, LREMOVE )
!
! This subroutine analyzes the part of a rule identifying which streams
! to affect and searches for the individual streams, or, if the rule uses
! the 'ALL' keyword, this routine identifies every stream as impacted.
!-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE em_param_module
      USE UTIL_FAMILY_MODULE


      IMPLICIT NONE

      CHARACTER(32), INTENT( IN ) :: RULE_STREAM0
      CHARACTER(32)               :: RULE_STREAM
      CHARACTER(32)               :: STREAM_NAME( N_EM_SRM )
      INTEGER, INTENT( IN )       :: IRULE
      LOGICAL, INTENT( INOUT )    :: EM_STREAM_RULE( N_EM_SRM )
      LOGICAL, INTENT( INOUT )    :: LREMOVE

      INTEGER ISRM, IDX, NSRM, IFAM
      CHARACTER( 200 ) :: XMSG
      CHARACTER( 16 )  :: PNAME = "EMIS_SPC_MAP"
      INTEGER  :: JDATE = 0
      INTEGER  :: JTIME = 0

      ! Initialize the output vector
      EM_STREAM_RULE = .FALSE.
      
      ! First Capitalize the Rule so it is easier to error-check
      RULE_STREAM = RULE_STREAM0
      CALL UPCASE( RULE_STREAM )

      LREMOVE = .FALSE.

      IF ( RULE_STREAM .EQ. 'ALL' .OR. RULE_STREAM .EQ. 'TOTAL' ) THEN
         ! Special Case Where the Stream Identifier in the rule equals
         ! equals all 'ALL'. Expand the Rule to Apply to All Streams
         ISRM = 0
         IF ( N_FILE_GR .GT. 0 ) THEN
           EM_STREAM_RULE( ISRM+1:N_FILE_GR ) = .TRUE.
           ISRM = ISRM + N_FILE_GR
         END IF

         IF ( NPTGRPS .GT. 0 ) THEN
           EM_STREAM_RULE( ISRM+1:ISRM+NPTGRPS ) = .TRUE.
           ISRM = ISRM + NPTGRPS
         END IF
         IF ( N_FILE_TR .GT. 0 ) EM_STREAM_RULE( ISRM+1:ISRM+N_FILE_TR ) = .TRUE.
         IF ( IBIOSRM .GT.0 ) EM_STREAM_RULE( IBIOSRM  ) = .TRUE.
         IF ( IMGSRM  .GT.0 ) EM_STREAM_RULE( IMGSRM   ) = .TRUE.
         IF ( ILTSRM  .GT.0 ) EM_STREAM_RULE( ILTSRM   ) = .TRUE.
         IF ( ISEASRM .GT.0 ) EM_STREAM_RULE( ISEASRM  ) = .TRUE.
         IF ( IDUSTSRM.GT.0 ) EM_STREAM_RULE( IDUSTSRM ) = .TRUE.
         IF ( IMIOGSRM.GT.0 ) EM_STREAM_RULE( IMIOGSRM ) = .TRUE.


      ELSE
         ! Determine if the Stream Label Refers to A Family and if So, 
         ! Apply the Rule to all members of that Family
         IFAM = INDEX1( RULE_STREAM, NStreamFamilies, StreamFamilyName )
         IF ( IFAM .EQ. 0 ) THEN
             NSRM = 1
             STREAM_NAME(1) = RULE_STREAM
         ELSE
             NSRM = StreamFamilyNum( IFAM )
             STREAM_NAME(1:NSRM) = StreamFamilyMembers( IFAM,1:NSRM )
         END IF

         ! Find the Specific Stream this Rule Identifies
         DO ISRM = 1,NSRM
           IDX = INDEX1( STREAM_NAME( ISRM ), N_EM_SRM, EM_FILE_LAB ) 
           IF ( IDX .NE. 0 ) THEN
             EM_STREAM_RULE( IDX ) = .TRUE.
           ELSE 
             ! Print warning that this stream is being ignored
             WRITE( LOGDEV, '(/,5x,3A,I3,/,5x,2A,4(/,5x,A))' ),
     &         'ATTENTION: The Emissions Stream Label (',TRIM( RULE_STREAM ),
     &         ') applied for Rule ',IRULE,'in the Emissions Control ',
     &         'Namelist does not match any of the emissions streams ',
     &         'labels or families provided to CMAQ. Please check the ',
     &         'runscript against your emissions control inputs',
     &         'confirm that this stream should be ignored.'

             ! Remove this rule from the list of rules
             LREMOVE = .TRUE.
           END IF
         END DO
      END IF
 
      END SUBROUTINE INTERPRET_EM_RULE_STREAM

!-----------------------------------------------------------------------
      SUBROUTINE MAP_EMISSDIAG_SPEC( Spec, IDIAG, SPEC_VECTOR, JDIAG ) 
!
! This subroutine creates a map of emissions species taking into account 
! any references to chemical families or aerosol bulk names.
!-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE em_param_module
      USE UTIL_FAMILY_MODULE
      USE VDIFF_MAP, ONLY : DIFF_SPC, N_SPC_DIFF, DIFF_MASK_GAS, 
     &                      DIFF_MASK_NUM, DIFF_MASK_SRF

      IMPLICIT NONE

      CHARACTER(16), INTENT( IN )  :: Spec( N_EMDIAG_SPEC_REG )
      INTEGER, INTENT( IN )        :: IDIAG, JDIAG
      LOGICAL, INTENT( IN )        :: SPEC_VECTOR( N_SPC_DIFF )

      INTEGER ISRM, IDX, NSRM, IFAM, I, J
      INTEGER NSPEC_REG, NPAIRS, NSPEC
      LOGICAL EXPAND_SPEC
      CHARACTER(16)    :: SPECIES
      CHARACTER( 200 ) :: XMSG
      CHARACTER( 16 )  :: PNAME = "EMIS_SPC_MAP"
      INTEGER  :: JDATE = 0
      INTEGER  :: JTIME = 0
      LOGICAL  :: DIFF_VEC( N_SPC_DIFF )
      INTEGER  :: MAP_toDIFF( N_EMDIAG_SPEC_REG*10 ), 
     &            MAP_toEMDIAG( N_EMDIAG_SPEC_REG*10 )
      CHARACTER(16) :: UNITS( N_EMDIAG_SPEC_REG*10 ),
     &                 DIAGSPEC( N_EMDIAG_SPEC_REG*10 )


      ! Determine number of diagnostic species to output
      NSPEC_REG = INDEX1( '', N_EMDIAG_SPEC_REG, Spec ) - 1
      IF ( NSPEC_REG .LE. 0 ) THEN
          WRITE( LOGDEV, * )
          WRITE( XMSG, '(A,I3,A)' ),
     &        'No Emission Species have been selected for group ',IDIAG,
     &        ' of the emission diagnostic input. Please correct.'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )     
      END IF

      ! Now Error Check and Expand the CMAQ Species Field
      NPAIRS = 0
      NSPEC  = 0
      
      DO I = 1,NSPEC_REG
         SPECIES = SPEC( I )
         Expand_Spec = .FALSE.
         IF ( SPECIES(1:1) .EQ. '*' ) THEN
             Expand_Spec = .TRUE.
             SPECIES(1:15) = SPECIES(2:16)
         END IF

         ! Retrieve logical vector, DIFF_VEC, indicating diffused species
         ! relevant for DiagSpec(I)
         CALL MAP_CHEM_FAMILIES( SPECIES, DIFF_SPC, N_SPC_DIFF, DIFF_VEC )
         DIFF_VEC = DIFF_VEC .AND. SPEC_VECTOR

         ! Save Map to translate each pair to diffused species and
         ! diagnostic species
         IF ( EXPAND_SPEC ) THEN 
            ! Add a Diagnostic Species for every expanded species
            DO J = 1,N_SPC_DIFF
               IF ( DIFF_VEC( J ) ) THEN
                   NSPEC = NSPEC + 1
                   DIAGSPEC( NSPEC ) = DIFF_SPC( J )

                   NPAIRS = NPAIRS + 1
                   MAP_toDIFF( NPAIRS ) = J
                   MAP_toEMDIAG( NPAIRS ) = NSPEC
                   IF ( DIFF_MASK_GAS( J ) ) THEN
                       ! GAS SPECIES
                       UNITS( NSPEC ) = 'mol s-1'
                   ELSE IF ( DIFF_MASK_NR( J ) ) THEN
                       ! GAS SPECIES
                       UNITS( NSPEC ) = 'mol s-1'
                   ELSE IF ( DIFF_MASK_TRAC( J ) ) THEN
                       ! GAS SPECIES
                       UNITS( NSPEC ) = 'mol s-1'
                   ELSE IF ( DIFF_MASK_NUM( J ) ) THEN
                       ! AEROSOL NUMBER SPECIES
                       UNITS( NSPEC ) = 'particles s-1'
                   ELSE IF ( DIFF_MASK_SRF( J ) ) THEN
                       ! AEROSOL SURFACE AREA SPECIES
                       UNITS( NSPEC ) = 'm2 s-1'
                   ELSE
                       ! AEROSOL MASS SPECIES
                       UNITS( NSPEC ) = 'g s-1'
                   END IF
               END IF
            END DO
         ELSE
            ! Keep only 1 diagnostic species and map all of the diffused
            ! species to it
            IF ( ANY( DIFF_VEC ) ) THEN
               NSPEC = NSPEC + 1
               DIAGSPEC( NSPEC ) = SPECIES
            END IF
            DO J = 1,N_SPC_DIFF
               IF ( DIFF_VEC( J ) ) THEN
                   NPAIRS = NPAIRS + 1
                   MAP_toDIFF( NPAIRS ) = J
                   MAP_toEMDIAG( NPAIRS ) = NSPEC
                   IF ( DIFF_MASK_GAS( J ) ) THEN
                       ! GAS SPECIES
                       UNITS( NSPEC ) = 'mol s-1'
                   ELSE IF ( DIFF_MASK_NR( J ) ) THEN
                       ! GAS SPECIES
                       UNITS( NSPEC ) = 'mol s-1'
                   ELSE IF ( DIFF_MASK_TRAC( J ) ) THEN
                       ! GAS SPECIES
                       UNITS( NSPEC ) = 'mol s-1'
                   ELSE IF ( DIFF_MASK_NUM( J ) ) THEN
                       ! AEROSOL NUMBER SPECIES
                       UNITS( NSPEC ) = 'particles s-1'
                   ELSE IF ( DIFF_MASK_SRF( J ) ) THEN
                       ! AEROSOL SURFACE AREA SPECIES
                       UNITS( NSPEC ) = 'm2 s-1'
                   ELSE
                       ! AEROSOL MASS SPECIES
                       UNITS( NSPEC ) = 'g s-1'
                   END IF
               END IF
            END DO
         END IF
      END DO

      EMDIAG_SPEC( JDIAG )%NSPEC = NSPEC
      EMDIAG_SPEC( JDIAG )%NPAIRS = NPAIRS 
      IF ( NSPEC .GT. 0 ) THEN
        IF ( ALLOCATED( EMDIAG_SPEC( JDIAG )%SPEC ) )
     &    DEALLOCATE( EMDIAG_SPEC( JDIAG )%SPEC )
        IF ( ALLOCATED( EMDIAG_SPEC( JDIAG )%MAP_toDIFF ) )
     &    DEALLOCATE( EMDIAG_SPEC( JDIAG )%MAP_toDIFF )
        IF ( ALLOCATED( EMDIAG_SPEC( JDIAG )%MAP_toEMDIAG ) ) 
     &    DEALLOCATE( EMDIAG_SPEC( JDIAG )%MAP_toEMDIAG )
        IF ( ALLOCATED( EMDIAG_SPEC( JDIAG )%UNITS ) )
     &    DEALLOCATE( EMDIAG_SPEC( JDIAG )%UNITS )
        ALLOCATE( EMDIAG_SPEC( JDIAG )%SPEC( NSPEC ) )
        ALLOCATE( EMDIAG_SPEC( JDIAG )%MAP_toDIFF( NPAIRS ) )
        ALLOCATE( EMDIAG_SPEC( JDIAG )%MAP_toEMDIAG( NPAIRS ) )
        ALLOCATE( EMDIAG_SPEC( JDIAG )%UNITS( NSPEC ) )

        EMDIAG_SPEC( JDIAG )%SPEC = DIAGSPEC( 1:NSPEC )
        EMDIAG_SPEC( JDIAG )%MAP_toDIFF   = MAP_toDIFF( 1:NPAIRS )
        EMDIAG_SPEC( JDIAG )%MAP_toEMDIAG = MAP_toEMDIAG( 1:NPAIRS )
        EMDIAG_SPEC( JDIAG )%UNITS = UNITS( 1:NSPEC )
      END IF

      END SUBROUTINE MAP_EMISSDIAG_SPEC
 
!-----------------------------------------------------------------------
      SUBROUTINE CUSTOM_EM_RULES( N_EM_RULE )
!
!     This subroutine defines several hardcoded rules for emissions
!     scaling that will apply by default. These include subtracting NH3
!     from fertilizer emissions if BiDi is turned on, moving all
!     sulfuric acid vapor to the particle phase upon emission and
!     splitting up the coarse mode anthropogenic emissions mass into
!     speciated compounds.
!-----------------------------------------------------------------------

      USE AERO_DATA, ONLY : ACORSEM_ASO4_FAC

      IMPLICIT NONE
      
      INTEGER,         INTENT( INOUT ) :: N_EM_RULE
      TYPE( EM_RULE_REG )              :: EM_NML_B( N_EM_RULE_REG  )

      INTEGER          :: N_EM_RULE_B, IRULE, ISRM

      N_EM_RULE_B = 0
 
      ! Add a rule For Removing Fertilizer Ammonia if BiDi is
      ! activated and the user has requested the correction
         IF ( ABFLUX ) THEN
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'NH3'
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'NH3_FERT'
            EM_NML_B(N_EM_RULE_B)%FAC   = -1.0
            EM_NML_B(N_EM_RULE_B)%BASIS = 'UNIT'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a'
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
         END IF

      ! sulfur tracking instrument model option
         IF ( STM ) THEN
      ! SULF emissions
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4EMIS'
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'SULF'
            EM_NML_B(N_EM_RULE_B)%FAC   = 1.0
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
      !  PSO4 emissions 
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4EMIS'
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PSO4'
            EM_NML_B(N_EM_RULE_B)%FAC   = 1.0
            EM_NML_B(N_EM_RULE_B)%BASIS = 'UNIT'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
      ! Coarse-Mode Sulfate
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4EMIS'
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
            EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_ASO4_FAC, 4 ) 
            EM_NML_B(N_EM_RULE_B)%BASIS = 'UNIT'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
      !  Wind blown dust and sea spray SO4 emissions 
      !  Fine
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4EMIS'
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PMFINE_SO4'
            EM_NML_B(N_EM_RULE_B)%FAC   = 1.0
            EM_NML_B(N_EM_RULE_B)%BASIS = 'UNIT'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
      !  Coarse 
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4EMIS'
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PMCOARSE_SO4'
            EM_NML_B(N_EM_RULE_B)%FAC   = 1.0
            EM_NML_B(N_EM_RULE_B)%BASIS = 'UNIT'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
         END IF ! stm

      ! Add Rules to Existing List at Beginning
         N_EM_RULE = N_EM_RULE + N_EM_RULE_B
         DO IRULE = N_EM_RULE, N_EM_RULE_B+1, -1
            EM_NML( IRULE ) = EM_NML( IRULE - N_EM_RULE_B )
         END DO
         DO IRULE = 1,N_EM_RULE_B
            EM_NML( IRULE ) = EM_NML_B( IRULE ) 
         END DO

      END SUBROUTINE CUSTOM_EM_RULES

!-----------------------------------------------------------------------
         SUBROUTINE CHECK_EMIS_UNITS( ISRM, ISUR, SPEC, UNITS, CONV, BASIS,
     &                                LAREA_UNIT )

! This subroutine checks for invalid values of the operation parameter 
! in the rules from the emission control list

         IMPLICIT NONE

         REAL, INTENT( OUT )              :: CONV
         CHARACTER( 16 ) , INTENT( OUT )  :: BASIS
         LOGICAL, INTENT( INOUT )         :: LAREA_UNIT
         CHARACTER( 16 ), INTENT( INOUT ) :: UNITS
         INTEGER, INTENT( IN )            :: ISRM
         INTEGER, INTENT( IN )            :: ISUR
         CHARACTER( 16 ), INTENT( IN )    :: SPEC
         CHARACTER( 400 )      :: XMSG
         INTEGER               :: X, SLASH_IND
         
         CHARACTER( 16 ) :: NUMER
         CHARACTER( 16 ) :: DENOM
         CHARACTER( 16 ) :: DENOM1
         CHARACTER( 16 ) :: DENOM2
         LOGICAL         :: LTIME_UNIT

         ! The Default Finding is an unknown Basis unit and conversion
         ! equal to 1.0
         CONV = 1.0
         LAREA_UNIT = .FALSE.
         LTIME_UNIT = .FALSE.
         BASIS = 'UNKNOWN'
         IF ( UNITS .EQ. '' ) RETURN
         
         ! Capitalize UNITS string and remove any leading spaces 
         CALL UPCASE( UNITS ) 
         DO WHILE ( UNITS(1:1) .EQ. ' ' ) 
             UNITS = UNITS(2:16) // ' '
         END DO

         ! Find First Break between numerator and denominator. If it's not a
         ! slash, then it should be the first space. 
         SLASH_IND = INDEX( UNITS, '/' )
         IF ( SLASH_IND .EQ. 0 ) SLASH_IND = INDEX( UNITS, ' ' )
         X = SLASH_IND - 1
         NUMER = UNITS(1:X)

         ! If there are no slashes or spaces, then exit
         IF ( SLASH_IND .EQ. 0 ) RETURN

         ! Check Numerator for Molar or Mass Units
         IF ( NUMER(1:4) .EQ. 'MOLE' .OR. NUMER(1:5) .EQ. 'MOLES' .OR.
     &        NUMER(1:3) .EQ. 'MOL' ) THEN
            ! No Conversion Needed for Moles to Moles
            CONV = 1.0 
            BASIS = 'MOLE'
         ELSE IF ( NUMER(1:5) .EQ. 'KMOLE' .OR. NUMER(1:6) .EQ. 'KMOLES' .OR.
     &             NUMER(1:4) .EQ. 'KMOL' ) THEN
            ! Convert kmol to mol
            CONV = 1000.0
            BASIS = 'MOLE'
         ELSE IF ( NUMER(1:5) .EQ. 'MMOLE' .OR. NUMER(1:6) .EQ. 'MMOLES' .OR.
     &             NUMER(1:4) .EQ. 'MMOL' ) THEN
            ! Convert mmol to mol
            CONV = 1.0e-3
            BASIS = 'MOLE'
         ELSE IF ( NUMER(1:5) .EQ. 'UMOLE' .OR. NUMER(1:6) .EQ. 'UMOLES' .OR.
     &             NUMER(1:4) .EQ. 'UMOL' ) THEN
            ! Convert umol to mol
            CONV = 1.0e-6
            BASIS = 'MOLE'
         ELSE IF ( NUMER(1:4) .EQ. 'GRAM' .OR. NUMER(1:5) .EQ. 'GRAMS' .OR.
     &             NUMER(1:1) .EQ. 'G'    .OR. NUMER(1:2) .EQ. 'GM'    .OR.
     &             NUMER(1:3) .EQ. 'GMS'  .OR. NUMER(1:2) .EQ. 'GS'  ) THEN
            ! No Conversion Needed for Grams to Grams
            CONV = 1.0
            BASIS = 'MASS'
         ELSE IF ( NUMER(1:5) .EQ. 'KGRAM' .OR. NUMER(1:6) .EQ. 'KGRAMS' .OR.
     &             NUMER(1:2) .EQ. 'KG'    .OR. NUMER(1:3) .EQ. 'KGM'    .OR.
     &             NUMER(1:4) .EQ. 'KGMS'  .OR. NUMER(1:3) .EQ. 'KGS' ) THEN
            ! Convert kg -> g
            CONV = 1000.0
            BASIS = 'MASS'
         ELSE IF ( NUMER(1:5) .EQ. 'MGRAM' .OR. NUMER(1:6) .EQ. 'MGRAMS' .OR.
     &             NUMER(1:2) .EQ. 'MG'    .OR. NUMER(1:3) .EQ. 'MGM'    .OR.
     &             NUMER(1:4) .EQ. 'MGMS'  .OR. NUMER(1:3) .EQ. 'MGS' ) THEN
            ! Convert mg -> g
            CONV = 1.0e-3
            BASIS = 'MASS'
         ELSE IF ( NUMER(1:5) .EQ. 'UGRAM' .OR. NUMER(1:6) .EQ. 'UGRAMS' .OR.
     &             NUMER(1:2) .EQ. 'UG'    .OR. NUMER(1:3) .EQ. 'UGM'    .OR.
     &             NUMER(1:3) .EQ. 'UGMS'  .OR. NUMER(1:3) .EQ. 'UGS' ) THEN
            ! Convert ug -> g
            CONV = 1.0e-6
            BASIS = 'MASS'
         ELSE IF ( NUMER(1:5) .EQ. 'NGRAM' .OR. NUMER(1:6) .EQ. 'NGRAMS' .OR.
     &             NUMER(1:2) .EQ. 'NG'    .OR. NUMER(1:3) .EQ. 'NGM'    .OR.
     &             NUMER(1:4) .EQ. 'NGMS'  .OR. NUMER(1:3) .EQ. 'NGS' ) THEN
            ! Convert ng -> g
            CONV = 1.0e-9
            BASIS = 'MASS'
         ELSE
            WRITE( XMSG,'(A,A16,A,I3,A11,A16,A,A,A)' ),
     &              'ERROR: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &              ISRM, ' has units ',TRIM(UNITS),' which are not recognized ',
     &              'as an emission rate. If you wish to use this variable for ',
     &              'emissions, please correct the units (e.g. g/s or moles/s).'
            CALL M3WARN( 'CHECK_EMIS_UNITS', 0, 0, XMSG )
            ! No Conversion and and Unknown Basis
            CONV = 1.0e0
            BASIS = 'UNKNOWN'
         END IF
         
         ! Identify Denominator. Return from subroutine if the
         ! denominator is empty.
         DENOM = UNITS(X+2:)
         IF ( DENOM .EQ. '' ) THEN
            WRITE( XMSG,'(A,A16,A,I3,A11,A16,A,A,A,A)' ),
     &              'ERROR: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &              ISRM, ' has units ',TRIM(UNITS),' which are not recognized ',
     &              'as an emission rate. If you wish to use this variable for ',
     &              'emissions, please correct the units (e.g. g/s, moles/s, ',
     &              'g/m2/s, or g/s/m2, etc.).'
            CALL M3WARN( 'CHECK_EMIS_UNITS', 0, 0, XMSG )
            ! No Conversion and and Unknown Basis
            CONV = CONV * 1.0e0
            BASIS = 'UNKNOWN' 
            RETURN
         END IF

         ! Remove Leading Spaces, if any
         DO WHILE ( DENOM(1:1) .EQ. ' ' ) 
             DENOM = DENOM(2:16) // ' '
         END DO

         ! Now split the Denominator into two parts, before and after
         ! the slash or space. There will be at least one space because the
         ! numerator has been cut from the units string, thus leaving at
         ! least one empty space.
         SLASH_IND = INDEX( DENOM, '/' )
         IF ( SLASH_IND .EQ. 0 ) SLASH_IND = INDEX( DENOM, ' ' )
         X = SLASH_IND - 1 
         DENOM1 = DENOM(1:X)
         DENOM2 = DENOM(X+2:)
         
         ! Parse the first Denominator and return attributes
         CALL CHECK_UNIT_DENOMINATOR( DENOM1, LAREA_UNIT, LTIME_UNIT, 
     &                                BASIS, CONV )
         IF ( BASIS .EQ. 'UNKNOWN' ) THEN
            WRITE( XMSG,'(A,A16,A,I3,A11,A16,A,A,A,A)' ),
     &              'ERROR: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &              ISRM, ' has units ',TRIM(UNITS),' which are not recognized ',
     &              'as an emission rate. If you wish to use this variable for ',
     &              'emissions, please correct the units (e.g. g/s, moles/s, ',
     &              'g/m2/s, or g/s/m2, etc.).'
            CALL M3WARN( 'CHECK_EMIS_UNITS', 0, 0, XMSG )
            RETURN
         END IF

         ! Parse the second Denominator and return attributes
         IF ( DENOM2 .NE. '' ) THEN
            CALL CHECK_UNIT_DENOMINATOR( DENOM2, LAREA_UNIT, LTIME_UNIT, 
     &                                   BASIS, CONV )
            IF ( BASIS .EQ. 'UNKNOWN' ) THEN
               WRITE( XMSG,'(A,A16,A,I3,A11,A16,A,A,A,A)' ),
     &                'ERROR: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &                ISRM, ' has units ',TRIM(UNITS),' which are not recognized ',
     &                'as an emission rate. If you wish to use this variable for ',
     &                'emissions, please correct the units (e.g. g/s, moles/s, ',
     &                'g/m2/s, or g/s/m2, etc.).'
               CALL M3WARN( 'CHECK_EMIS_UNITS', 0, 0, XMSG )
               RETURN
            END IF
         END IF

         ! Check to make sure the time component has been defined 
         IF ( .NOT. LTIME_UNIT ) THEN
            WRITE( XMSG,'(A,A16,A,I3,A11,A16,A,A,A,A)' ),
     &             'ERROR: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &             ISRM, ' has units ',TRIM(UNITS),' which are not recognized ',
     &             'as an emission rate. If you wish to use this variable for ',
     &             'emissions, please correct the units (e.g. g/s, moles/s, ',
     &             'g/m2/s, or g/s/m2, etc.).'
            CALL M3WARN( 'CHECK_EMIS_UNITS', 0, 0, XMSG )
            BASIS = 'UNKNOWN'
            RETURN
         END IF 

         END SUBROUTINE CHECK_EMIS_UNITS
 
!-----------------------------------------------------------------------
         SUBROUTINE CHECK_UNIT_DENOMINATOR( DENOM, LAREA, LTIME, BASIS, CONV)

! This subroutine compares the denominator from the units string of an
! emission variables to potential time and area units and determines how
! to convert the emissions online in DESID.
!-----------------------------------------------------------------------

         IMPLICIT NONE

         CHARACTER(16), INTENT( IN )   :: DENOM
         LOGICAL, INTENT( OUT )        :: LAREA
         LOGICAL, INTENT( OUT )        :: LTIME
         CHARACTER(16), INTENT( INOUT ):: BASIS
         REAL, INTENT( INOUT)          :: CONV

         ! Check 1st Denominator for Time Units
         IF ( DENOM .EQ. 'S' .OR. DENOM .EQ. 'S-1' .OR.
     &        DENOM .EQ. 'SEC'.OR.DENOM .EQ. 'SEC-1' .OR.
     &        DENOM .EQ. 'SECOND'.OR.DENOM .EQ. 'SECOND-1' .OR.
     &        DENOM .EQ. 'SECONDS'.OR.DENOM .EQ. 'SECONDS-1' ) THEN
            ! No Conversion Necessary for seconds -> seconds
            CONV = CONV * 1.0
            LTIME = .TRUE.
         ELSE IF ( DENOM .EQ. 'H' .OR. DENOM .EQ. 'H-1' .OR.
     &             DENOM .EQ. 'HR'.OR.DENOM .EQ. 'HR-1' .OR.
     &             DENOM .EQ. 'HRS'.OR.DENOM .EQ. 'HRS-1' .OR.
     &             DENOM .EQ. 'HOUR'.OR.DENOM .EQ. 'HOUR-1' .OR.
     &             DENOM .EQ. 'HOURS'.OR.DENOM .EQ. 'HOURS-1' ) THEN
            ! Convert hours -> seconds
            CONV = CONV * 3600.0
            LTIME = .TRUE.
         ELSE IF ( DENOM .EQ. 'M' .OR. DENOM .EQ. 'M-1' .OR.
     &             DENOM .EQ. 'MIN'.OR.DENOM .EQ. 'MIN-1' .OR.
     &             DENOM .EQ. 'MINUTE'.OR.DENOM .EQ. 'MINUTE-1' .OR.
     &             DENOM .EQ. 'MINUTES'.OR.DENOM .EQ. 'MINUTES-1' ) THEN
            ! Convert minutes -> seconds
            CONV = CONV * 60.0
            LTIME = .TRUE.
         ELSE IF ( DENOM .EQ. 'M2' .OR. DENOM .EQ. 'M-2' .OR.
     &             DENOM .EQ. 'M^2'.OR.DENOM .EQ. 'METER-2' .OR.
     &             DENOM .EQ. 'METER^2'.OR.DENOM .EQ. 'METER^-2' .OR.
     &             DENOM .EQ. 'METER2'.OR.DENOM .EQ. 'METER-2' .OR.
     &             DENOM .EQ. 'METERS^2'.OR.DENOM .EQ. 'METERS^-2' .OR.
     &             DENOM .EQ. 'METERS2'.OR.DENOM .EQ. 'METERS-2' ) THEN
            ! No conversion necessary for meters^2 -> meters^2
            CONV = CONV * 1.0
            LAREA = .TRUE. 
         ELSE IF ( DENOM .EQ. 'KM2' .OR. DENOM .EQ. 'KM-2' .OR.
     &             DENOM .EQ. 'KM^2'.OR.DENOM .EQ. 'KM^-2' .OR.
     &             DENOM .EQ. 'KILOMETER2'.OR.DENOM .EQ. 'KILOMETER-2' .OR.
     &             DENOM .EQ. 'KILOMETER^2'.OR.DENOM .EQ. 'KILOMETER^-2' .OR.
     &             DENOM .EQ. 'KILOMETERS2'.OR.DENOM .EQ. 'KILOMETERS-2' .OR.
     &             DENOM .EQ. 'KILOMETERS^2'.OR.DENOM .EQ. 'KILOMETERS-2' ) THEN
            ! Convert kilometers^2 -> meters^2
            CONV = CONV * 1.0E6
            LAREA = .TRUE. 
         ELSE IF ( DENOM .EQ. 'MI2' .OR. DENOM .EQ. 'MI-2' .OR.
     &             DENOM .EQ. 'MI^2'.OR.DENOM .EQ. 'MI^-2' .OR.
     &             DENOM .EQ. 'MILE2'.OR.DENOM .EQ. 'MILE-2' .OR.
     &             DENOM .EQ. 'MILE^2'.OR.DENOM .EQ. 'MILE^-2' .OR.
     &             DENOM .EQ. 'MILES2'.OR.DENOM .EQ. 'MILES-2' .OR.
     &             DENOM .EQ. 'MILES^2'.OR.DENOM .EQ. 'MILES-2' ) THEN
            ! Convert mile^2 -> meters^2
            CONV = CONV * 2.59E6
            LAREA = .TRUE.  
         ELSE IF ( DENOM .EQ. 'HA' .OR. DENOM .EQ. 'HA-1' .OR.
     &             DENOM .EQ. 'HA^1'.OR.DENOM .EQ. 'HA^-1' .OR.
     &             DENOM .EQ. 'HECTARE'.OR.DENOM .EQ. 'HECTARE-1' .OR.
     &             DENOM .EQ. 'HECTARE^1'.OR.DENOM .EQ. 'HECTARE^-1' .OR.
     &             DENOM .EQ. 'HECTARES'.OR.DENOM .EQ. 'HECTARES-1' .OR.
     &             DENOM .EQ. 'HECTARES^1'.OR.DENOM .EQ. 'HECTARES^-1' ) THEN
            ! Convert hectares -> meters^2
            CONV = CONV * 10000.0
            LAREA = .TRUE.   
         ELSE
            ! No Conversion and and Unknown Basis
            CONV = CONV * 1.0e0
            BASIS = 'UNKNOWN'
         END IF

      END SUBROUTINE CHECK_UNIT_DENOMINATOR
 
!-----------------------------------------------------------------------
         SUBROUTINE CHECK_OP( OP, IRULE )

! This subroutine checks for invalid values of the operation parameter 
! in the rules from the emission control list

         IMPLICIT NONE

         CHARACTER( 1 )        :: OP
         INTEGER, INTENT( IN ) :: IRULE
         CHARACTER( 200 )      :: XMSG

         IF ( OP .EQ. 'A' .OR. OP .EQ. 'a' ) THEN
             OP = 'a'
         ELSE IF ( OP .EQ. 'M' .OR. OP .EQ. 'm' ) THEN
             OP = 'm'
         ELSE IF ( OP .EQ. 'O' .OR. OP .EQ. 'o' ) THEN
             OP = 'o'
         ELSE
             WRITE( XMSG,'(A,I3,A,A1)' ),
     &              'ERROR: OP parameter for rule ',IRULE,
     &              ' has invalid value: ',OP
             CALL M3EXIT( 'CHECK_OP (EMIS_DEFN)', 0, 0, XMSG, 2 )
         END IF

         END SUBROUTINE CHECK_OP
 
!-----------------------------------------------------------------------
         SUBROUTINE CHECK_BASIS( BASIS, IRULE )

! This subroutine checks for invalid values of the operation parameter 
! in the rules from the emission control list

         IMPLICIT NONE

         CHARACTER( 4 )        :: BASIS
         INTEGER, INTENT( IN ) :: IRULE
         CHARACTER( 200 )      :: XMSG

         IF ( BASIS .EQ. 'mole' .OR. BASIS .EQ. 'MOLE' .OR.
     &        BASIS .EQ. 'Mole' ) THEN
             BASIS = 'MOLE'
         ELSE IF ( BASIS .EQ. 'mass' .OR. BASIS .EQ. 'MASS' .OR.
     &             BASIS .EQ. 'Mass' ) THEN
             BASIS = 'MASS'
         ELSE IF ( BASIS .EQ. 'unit' .OR. BASIS .EQ. 'UNIT' .OR.
     &             BASIS .EQ. 'Unit' ) THEN
             BASIS = 'UNIT'
         ELSE
             WRITE( XMSG,'(A,I3,A,A4)' ),
     &              'ERROR: BASIS parameter for rule ',IRULE,
     &              ' has invalid value: ',BASIS
             CALL M3EXIT( 'CHECK_BASIS (EMIS_DEFN)', 0, 0, XMSG, 2 )
         END IF

         END SUBROUTINE CHECK_BASIS
 
!-----------------------------------------------------------------------
      FUNCTION RESOLVE_YN_TF_2D3D( ARG ) RESULT( ARGOUT )

!-----------------------------------------------------------------------
        IMPLICIT NONE

        CHARACTER( 6 ) :: ARG, ARGOUT
 
         IF ( ARG .EQ. 'FALSE' .OR. ARG .EQ. 'F' .OR.
     &        ARG .EQ. 'NO'    .OR. ARG .EQ. 'N' ) THEN
            ARGOUT = 'FALSE'
         ELSEIF ( ARG .EQ. 'TRUE' .OR. ARG .EQ. 'T' .OR.
     &            ARG .EQ. 'YES'  .OR. ARG .EQ. 'Y' ) THEN
            ARGOUT = 'TRUE'
         ELSEIF ( ARG .EQ. '2D'  .OR. ARG .EQ. '2d' ) THEN
            ARGOUT = 'TRUE'
         ELSEIF   ( ARG .EQ. '3D'  .OR. ARG .EQ. '3d' ) THEN
            ARGOUT = '3D'
         ELSEIF   ( ARG .EQ. '2DSUM' .OR. ARG .EQ. '2dSUM' .OR.
     &              ARG .EQ. '2dsum' ) THEN
            ARGOUT = '2DSUM'
         END IF
 
         RETURN

      END FUNCTION RESOLVE_YN_TF_2D3D

 
      END MODULE EMIS_UTIL
