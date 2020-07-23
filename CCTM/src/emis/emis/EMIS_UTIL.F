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
         SUBROUTINE CHECK_EMIS_UNITS( ISRM, ISUR, SPEC, UNITS, CONV, BASIS )

! This subroutine checks for invalid values of the operation parameter 
! in the rules from the emission control list

         IMPLICIT NONE

         REAL, INTENT( OUT )              :: CONV
         CHARACTER( 4 ) , INTENT( OUT )   :: BASIS
         CHARACTER( 16 ), INTENT( INOUT ) :: UNITS
         INTEGER, INTENT( IN )            :: ISRM
         INTEGER, INTENT( IN )            :: ISUR
         CHARACTER( 16 ), INTENT( IN )    :: SPEC
         CHARACTER( 200 )      :: XMSG
         INTEGER               :: X, SLASH_IND

         CONV = 1.0

         CALL UPCASE( UNITS )

         ! Find Break between numerator and denominator. If it's not a
         ! slash, then it should be the first space
         SLASH_IND = INDEX( UNITS, '/' )
         IF ( SLASH_IND .EQ. 0 ) SLASH_IND = INDEX( UNITS, ' ' )
         X = SLASH_IND - 1

         ! Check for Molar or Mass Units
         IF ( UNITS(1:X) .EQ. 'MOLE' .OR. UNITS(1:X) .EQ. 'MOLES' .OR.
     &        UNITS(1:X) .EQ. 'MOL' ) THEN
            ! No Conversion Needed for Moles to Moles
            CONV = 1.0 
            BASIS = 'MOLE'
         ELSE IF ( UNITS(1:X) .EQ. 'KMOLE' .OR. UNITS(1:X) .EQ. 'KMOLES' .OR.
     &             UNITS(1:X) .EQ. 'KMOL' ) THEN
            ! Convert kmol to mol
            CONV = 1000.0
            BASIS = 'MOLE'
         ELSE IF ( UNITS(1:X) .EQ. 'MMOLE' .OR. UNITS(1:X) .EQ. 'MMOLES' .OR.
     &             UNITS(1:X) .EQ. 'MMOL' ) THEN
            ! Convert mmol to mol
            CONV = 1.0e-3
            BASIS = 'MOLE'
         ELSE IF ( UNITS(1:X) .EQ. 'UMOLE' .OR. UNITS(1:X) .EQ. 'UMOLES' .OR.
     &             UNITS(1:X) .EQ. 'UMOL' ) THEN
            ! Convert umol to mol
            CONV = 1.0e-6
            BASIS = 'MOLE'
         ELSE IF ( UNITS(1:X) .EQ. 'GRAM' .OR. UNITS(1:X) .EQ. 'GRAMS' .OR.
     &             UNITS(1:X) .EQ. 'G'    .OR. UNITS(1:X) .EQ. 'GM'    .OR.
     &             UNITS(1:X) .EQ. 'GMS'  .OR. UNITS(1:X) .EQ. 'GS'  ) THEN
            ! No Conversion Needed for Grams to Grams
            CONV = 1.0
            BASIS = 'MASS'
         ELSE IF ( UNITS(1:X) .EQ. 'KGRAM' .OR. UNITS(1:X) .EQ. 'KGRAMS' .OR.
     &             UNITS(1:X) .EQ. 'KG'    .OR. UNITS(1:X) .EQ. 'KGM'    .OR.
     &             UNITS(1:X) .EQ. 'KGMS'  .OR. UNITS(1:X) .EQ. 'KGS' ) THEN
            ! Convert kg -> g
            CONV = 1000.0
            BASIS = 'MASS'
         ELSE IF ( UNITS(1:X) .EQ. 'MGRAM' .OR. UNITS(1:X) .EQ. 'MGRAMS' .OR.
     &             UNITS(1:X) .EQ. 'MG'    .OR. UNITS(1:X) .EQ. 'MGM'    .OR.
     &             UNITS(1:X) .EQ. 'MGMS'  .OR. UNITS(1:X) .EQ. 'MGS' ) THEN
            ! Convert mg -> g
            CONV = 1.0e-3
            BASIS = 'MASS'
         ELSE IF ( UNITS(1:X) .EQ. 'UGRAM' .OR. UNITS(1:X) .EQ. 'UGRAMS' .OR.
     &             UNITS(1:X) .EQ. 'UG'    .OR. UNITS(1:X) .EQ. 'UGM'    .OR.
     &             UNITS(1:X) .EQ. 'UGMS'  .OR. UNITS(1:X) .EQ. 'UGS' ) THEN
            ! Convert ug -> g
            CONV = 1.0e-6
            BASIS = 'MASS'
         ELSE IF ( UNITS(1:X) .EQ. 'NGRAM' .OR. UNITS(1:X) .EQ. 'NGRAMS' .OR.
     &             UNITS(1:X) .EQ. 'NG'    .OR. UNITS(1:X) .EQ. 'NGM'    .OR.
     &             UNITS(1:X) .EQ. 'NGMS'  .OR. UNITS(1:X) .EQ. 'NGS' ) THEN
            ! Convert ng -> g
            CONV = 1.0e-9
            BASIS = 'MASS'
         ELSE
            WRITE( XMSG,'(A,A16,A,/,I3,A11,A16,A,/,A)' ),
     &              'ERROR: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &              ISRM, ' has units ',TRIM(UNITS),' which are unkown to CMAQ. ',
     &              'Please correct them to proceed.'
            CALL M3EXIT( 'CHECK_EMIS_UNITS', 0, 0, XMSG, 2 )
         END IF
          
         ! Check for Time Units
         X = X + 2
         IF ( UNITS(X:) .EQ. 'S' .OR. UNITS(X:) .EQ. 'S-1' .OR.
     &        UNITS(X:) .EQ. 'SEC'.OR.UNITS(X:) .EQ. 'SEC-1' .OR.
     &        UNITS(X:) .EQ. 'SECOND'.OR.UNITS(X:) .EQ. 'SECOND-1' .OR.
     &        UNITS(X:) .EQ. 'SECONDS'.OR.UNITS(X:) .EQ. 'SECONDS-1' ) THEN
            ! No Conversion Necessary for seconds -> seconds
            CONV = CONV * 1.0
         ELSE IF ( UNITS(X:) .EQ. 'H' .OR. UNITS(X:) .EQ. 'H-1' .OR.
     &             UNITS(X:) .EQ. 'HR'.OR.UNITS(X:) .EQ. 'HR-1' .OR.
     &             UNITS(X:) .EQ. 'HOUR'.OR.UNITS(X:) .EQ. 'HOUR-1' .OR.
     &             UNITS(X:) .EQ. 'HOURS'.OR.UNITS(X:) .EQ. 'HOURS-1' ) THEN
            ! Convert hours -> seconds
            CONV = CONV * 3600.0
         ELSE IF ( UNITS(X:) .EQ. 'M' .OR. UNITS(X:) .EQ. 'M-1' .OR.
     &             UNITS(X:) .EQ. 'MIN'.OR.UNITS(X:) .EQ. 'MIN-1' .OR.
     &             UNITS(X:) .EQ. 'MINUTE'.OR.UNITS(X:) .EQ. 'MINUTE-1' .OR.
     &             UNITS(X:) .EQ. 'MINUTES'.OR.UNITS(X:) .EQ. 'MINUTES-1' ) THEN
            ! Convert minutes -> seconds
            CONV = CONV * 60.0
         ELSE
            WRITE( XMSG,'(A,A,A,I3,A11,A,A,A)' ),
     &              'ERROR: Surrogate Species ',TRIM(SPEC),' on emission stream ',
     &              ISRM, ' has units ',TRIM(UNITS),' which are unkown to CMAQ. ',
     &              'Please correct them to proceed.'
            CALL M3EXIT( 'CHECK_EMIS_UNITS', 0, 0, XMSG, 2 )
         END IF

         END SUBROUTINE CHECK_EMIS_UNITS

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
