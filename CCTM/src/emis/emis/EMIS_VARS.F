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
      MODULE EMIS_VARS

!-----------------------------------------------------------------------
! Function: Define global variables used for emissions processor

! Revision History:
!     09 Nov 2017 B.Murphy: Extracted these variables from EMIS_DEFN
!-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE em_param_module

      IMPLICIT NONE

      SAVE

      REAL,    ALLOCATABLE :: VDEMIS_DIFF( :,:,:,: ) ! emissions array mapped to diffused species

      INTEGER              :: EMLAYS

      CHARACTER( 16 ), ALLOCATABLE :: EM_SPEC  ( : ) ! Vector of Internal CMAQ Species names for 
                                                     ! each emission operation
      CHARACTER( 16 ), ALLOCATABLE :: EM_SURR( :,: ) ! Emissions Surrogate to be read ( species, stream )
      
      CHARACTER( 16 ), ALLOCATABLE :: TRACSPC( : )   ! Variables from Tracer Emission File
      CHARACTER( 16 ), ALLOCATABLE :: AREASPC( : )   ! Variables straight from the Area Emission File

      TYPE EM_SURR_TYPE
          INTEGER         :: LEN    ! Length of Sub-Vector
          CHARACTER( 16 ), ALLOCATABLE :: ARRY( : )   ! Surrogate Name
          CHARACTER( 16 ), ALLOCATABLE :: UNITS( : )  ! Units determined from input files
          REAL,            ALLOCATABLE :: MW( : )     ! Default MW from SPECIATE/SMOKE/MOVE
          LOGICAL,         ALLOCATABLE :: USED( : )   ! Whether or not the surrogate is used in CMAQ
          Real,            ALLOCATABLE :: CONV( : )   ! Conversion factor to account for kg -> g or hr -> s
          CHARACTER( 4 ),  ALLOCATABLE :: BASIS( : )  ! 'MOLE' or 'MASS'
      END TYPE EM_SURR_TYPE
      TYPE( EM_SURR_TYPE ),ALLOCATABLE :: EM_FILE_SURR( : )! Species to be read from each stream file

      TYPE EM_FAC_STRUCTURE
          INTEGER              :: LEN     ! Number of scaling instructions that apply to this element of 
                                          !   the EM_FAC_ST structure. The other attributes will be of 
                                          !   length LEN.
          REAL, ALLOCATABLE    :: FAC(:)  ! Scale Factor applied with potential aerosol splitting
          REAL, ALLOCATABLE    :: BULK(:) ! Scale Factor for bulk emission without
                                          !   splitting for aerosols
          REAL, ALLOCATABLE    :: BASIS(:)! Conversion factor to account for mole or mass basis
          INTEGER, ALLOCATABLE :: REG(:)  ! Index Mapping this entry to a particlar region in EM_REGIONS
          INTEGER, ALLOCATABLE :: OP(:)   ! Operation to perform for this scaling
                                          !   (addition,'a' = 1, 
                                          !    multiplication,'m' = 2, 
                                          !    overwrite,'o' = 3)
      END TYPE EM_FAC_STRUCTURE
      TYPE( EM_FAC_STRUCTURE), ALLOCATABLE :: EM_FAC_ST ( :,: ) ! Emissions Scale Factors ( species, stream )
      CHARACTER( 1 ) :: EM_OP_NAME( 3 ) = (/'a','m','o'/)
       
      ! Define Structure for Holding Emissions Aerosol Mode Information Globally
      TYPE EM_STREAM_MODES
         INTEGER  :: LEN
         INTEGER,         ALLOCATABLE :: REF( : )
         CHARACTER( 16 ), ALLOCATABLE :: NAME( : )
         REAL,            ALLOCATABLE :: FACNUM( :,: )
         REAL,            ALLOCATABLE :: FACSRF( :,: )
      END TYPE EM_STREAM_MODES
      TYPE( EM_STREAM_MODES ), ALLOCATABLE :: EM_STREAM_SIZE( : )
 
      ! Define Emissions File Attribute Variables
      CHARACTER( 200 ),ALLOCATABLE :: EM_FILE_NAME( : )  ! Filename of each emissions stream
      CHARACTER( 30  ),ALLOCATABLE :: EM_FILE_LAB ( : )  ! Nickname of each emissions stream
      CHARACTER( 16  ),ALLOCATABLE :: EM_FILE_TYPE( : )  ! shortname for the type of each emission
      INTEGER,         ALLOCATABLE :: EM_FILE_ITYPE( : ) ! shortname for the type of each emission
      CHARACTER( 100 ),ALLOCATABLE :: EM_FILE_DESC( : )  ! Description of each emissions stream
      LOGICAL,         ALLOCATABLE :: EM_FILE_LAPPLY( : )! Flag to use each emissions stream
      CHARACTER( 6   ),ALLOCATABLE :: EM_FILE_LDIAG( : ) ! Toggle for Default Emissions Diagnostic Output
      CHARACTER( 16  ),ALLOCATABLE :: EM_DIAG_FILE( : )  ! Filename for Emissions Diagnostic Output
      LOGICAL         ,ALLOCATABLE :: EM_FILE_DIFF( :,: )! Test for whether or not a stream contributes to 
                                                         !   a particular transported variable
      LOGICAL         ,ALLOCATABLE :: EM_FILE_SYM_DATE( : ) ! Toggle for whether or not to let the 
                                                             ! emission stream date override the model
      INTEGER         ,ALLOCATABLE :: EM_FILE_DATE( : )! Date to be read in from each emissions stream
      LOGICAL         ,ALLOCATABLE :: EM_FILE_FIRE( : )! Is this Emission Stream Representing Fires (1=Yes)

      INTEGER              :: IBIOSRM, IMGSRM, ILTSRM, ISEASRM, IDUSTSRM
      INTEGER, ALLOCATABLE :: IGSRM( : ), IPSRM( : ), ITSRM( : ), MAP_PTtoISRM( : )

      INTEGER, ALLOCATABLE :: MAP_EMtoSURR( :,: ), MAP_EMtoDIFF( : ), MAP_EMtoGAS( : )

      ! Create Lookup table of Molecular Weight for likely emission
      ! surrogate species. These values are used to compute mole <-> mass
      ! conversions if requested. 
      Type emis_legend
          Character( 16 ) :: NAME  ! Emissions Surrogate Species
          REAL            :: MW    ! Emissions Surrogate Molecular Weight [g mol-1]
      End Type emis_legend
      
      Integer, Parameter :: N_EMIS_SURR_TABLE = 150
      Type ( emis_legend ), Parameter :: 
     &  EMIS_SURR_TABLE( N_EMIS_SURR_TABLE ) = (/
     &    emis_legend( 'NO2          ' ,046.0 ),
     &    emis_legend( 'NO           ' ,030.0 ),
     &    emis_legend( 'HONO         ' ,047.0 ),
     &    emis_legend( 'SO2          ' ,064.0 ),
     &    emis_legend( 'SULF         ' ,098.0 ),
     &    emis_legend( 'NH3          ' ,017.0 ),
     &    emis_legend( 'NH3_FERT     ' ,017.0 ),
     &    emis_legend( 'CO           ' ,028.0 ),
     &    emis_legend( 'PACD         ' ,076.0 ),
     &    emis_legend( 'AACD         ' ,060.1 ),
     &    emis_legend( 'ALD2         ' ,044.0 ),
     &    emis_legend( 'FORM         ' ,030.0 ),
     &    emis_legend( 'HCHO         ' ,030.0 ),
     &    emis_legend( 'MEOH         ' ,032.0 ),
     &    emis_legend( 'FACD         ' ,046.0 ),
     &    emis_legend( 'ALDX         ' ,058.1 ),
     &    emis_legend( 'GLYXL        ' ,058.0 ),
     &    emis_legend( 'GLY          ' ,058.0 ),
     &    emis_legend( 'MGLY         ' ,072.1 ),
     &    emis_legend( 'ETHA         ' ,030.1 ),
     &    emis_legend( 'ETH          ' ,028.0 ),
     &    emis_legend( 'ETE          ' ,028.0 ),
     &    emis_legend( 'HC3          ' ,044.0 ),
     &    emis_legend( 'HC5          ' ,072.0 ),
     &    emis_legend( 'HC8          ' ,114.0 ),
     &    emis_legend( 'OLT          ' ,042.0 ),
     &    emis_legend( 'OLI          ' ,068.0 ),
     &    emis_legend( 'DIEN         ' ,054.0 ),
     &    emis_legend( 'ACE          ' ,026.0 ),
     &    emis_legend( 'ORA1         ' ,046.0 ),
     &    emis_legend( 'ORA2         ' ,060.0 ),
     &    emis_legend( 'ETOH         ' ,046.1 ),
     &    emis_legend( 'KET          ' ,072.1 ),
     &    emis_legend( 'PAR          ' ,014.0 ),
     &    emis_legend( 'ACET         ' ,058.1 ),
     &    emis_legend( 'ACD          ' ,044.0 ),
     &    emis_legend( 'ALD          ' ,058.0 ),
     &    emis_legend( 'ACT          ' ,058.0 ),
     &    emis_legend( 'UALD         ' ,084.0 ),
     &    emis_legend( 'HKET         ' ,074.0 ),
     &    emis_legend( 'PRPA         ' ,044.1 ),
     &    emis_legend( 'ETHY         ' ,026.0 ),
     &    emis_legend( 'OLE          ' ,042.1 ),
     &    emis_legend( 'IOLE         ' ,056.1 ),
     &    emis_legend( 'OLE1         ' ,072.3 ),
     &    emis_legend( 'OLE2         ' ,075.8 ),
     &    emis_legend( 'RNO3         ' ,147.2 ),
     &    emis_legend( 'CCHO         ' ,044.1 ),
     &    emis_legend( 'RCHO         ' ,058.1 ),
     &    emis_legend( 'ALK1         ' ,030.1 ),
     &    emis_legend( 'ALK2         ' ,036.7 ),
     &    emis_legend( 'ALK3         ' ,058.6 ),
     &    emis_legend( 'ALK4         ' ,077.6 ),
     &    emis_legend( 'ALK5         ' ,118.9 ),
     &    emis_legend( 'MEK          ' ,072.1 ),
     &    emis_legend( 'BACL         ' ,086.1 ),
     &    emis_legend( 'BALD         ' ,106.1 ),
     &    emis_legend( 'MACR         ' ,070.1 ),
     &    emis_legend( 'MVK          ' ,070.1 ),
     &    emis_legend( 'IPRD         ' ,100.1 ),
     &    emis_legend( 'PRD2         ' ,116.2 ),
     &    emis_legend( 'ISOP         ' ,068.1 ),
     &    emis_legend( 'ISO          ' ,068.1 ),
     &    emis_legend( 'TERP         ' ,136.2 ),
     &    emis_legend( 'APIN         ' ,136.2 ),
     &    emis_legend( 'API          ' ,136.2 ),
     &    emis_legend( 'LIM          ' ,136.2 ),
     &    emis_legend( 'SESQ         ' ,204.0 ),
     &    emis_legend( 'BENZ         ' ,078.1 ),
     &    emis_legend( 'BEN          ' ,078.1 ),
     &    emis_legend( 'BENZENE      ' ,078.1 ),
     &    emis_legend( 'PHEN         ' ,094.0 ),
     &    emis_legend( 'CRES         ' ,108.1 ),
     &    emis_legend( 'CSL          ' ,108.0 ),
     &    emis_legend( 'TOL          ' ,092.1 ),
     &    emis_legend( 'XYM          ' ,106.0 ),
     &    emis_legend( 'XYP          ' ,106.0 ),
     &    emis_legend( 'XYO          ' ,106.0 ),
     &    emis_legend( 'XYLMN        ' ,106.2 ),
     &    emis_legend( 'MXYL         ' ,106.2 ),
     &    emis_legend( 'OXYL         ' ,106.2 ),
     &    emis_legend( 'PXYL         ' ,106.2 ),
     &    emis_legend( 'ARO1         ' ,095.2 ),
     &    emis_legend( 'ARO2MN       ' ,118.7 ),
     &    emis_legend( 'NAPH         ' ,128.2 ),
     &    emis_legend( 'B124         ' ,120.2 ),
     &    emis_legend( 'CH4          ' ,016.0 ),
     &    emis_legend( 'CL2          ' ,071.0 ),
     &    emis_legend( 'HCL          ' ,036.5 ),
     &    emis_legend( 'SOAALK       ' ,112.0 ),
     &    emis_legend( 'FORM_PRIMARY ' ,030.0 ),
     &    emis_legend( 'ALD2_PRIMARY ' ,044.0 ),
     &    emis_legend( 'BUTADIENE13  ' ,054.0 ),
     &    emis_legend( 'ACRO         ' ,056.1 ),
     &    emis_legend( 'ACROLEIN     ' ,056.1 ),
     &    emis_legend( 'TOLU         ' ,092.0 ),
     &    emis_legend( 'HGNRVA       ' ,200.6 ),
     &    emis_legend( 'HGIIGAS      ' ,200.6 ),
     &    emis_legend( 'ETHE         ' ,028.1 ),
     &    emis_legend( 'PRPE         ' ,042.1 ),
     &    emis_legend( '13BDE        ' ,054.1 ),
     &    emis_legend( 'ACYE         ' ,026.0 ),
     &    emis_legend( 'MOH          ' ,032.0 ),
     &    emis_legend( 'EOH          ' ,046.0 ),
     &    emis_legend( 'ROH          ' ,060.0 ),
     &    emis_legend( 'ETEG         ' ,062.0 ),
     &    emis_legend( 'PSO4         ' ,096.0 ),
     &    emis_legend( 'PNO3         ' ,062.0 ),
     &    emis_legend( 'PNH4         ' ,018.0 ),
     &    emis_legend( 'PEC          ' ,012.0 ),
     &    emis_legend( 'POC          ' ,220.0 ),
     &    emis_legend( 'PNCOM        ' ,220.0 ),
     &    emis_legend( 'PMOTHR       ' ,200.0 ),
     &    emis_legend( 'PFE          ' ,055.8 ),
     &    emis_legend( 'PAL          ' ,027.0 ),
     &    emis_legend( 'PSI          ' ,028.1 ),
     &    emis_legend( 'PTI          ' ,047.9 ),
     &    emis_legend( 'PCA          ' ,040.1 ),
     &    emis_legend( 'PMG          ' ,024.3 ),
     &    emis_legend( 'PK           ' ,039.1 ),
     &    emis_legend( 'PMN          ' ,054.9 ),
     &    emis_legend( 'PH2O         ' ,018.0 ),
     &    emis_legend( 'PNA          ' ,023.0 ),
     &    emis_legend( 'PCL          ' ,035.5 ),
     &    emis_legend( 'PMC          ' ,100.0 ),
     &    emis_legend( 'ASOIL        ' ,100.0 ),
     &    emis_legend( 'ASEACAT      ' ,023.75),
     &    emis_legend( 'NICKEL_F     ' ,058.7 ),
     &    emis_legend( 'NICKEL_C     ' ,058.7 ),
     &    emis_legend( 'CHROMEHEX_F  ' ,052.0 ),
     &    emis_legend( 'CHROMEHEX_C  ' ,052.0 ),
     &    emis_legend( 'CHROMTRI_F   ' ,052.0 ),
     &    emis_legend( 'CHROMTRI_C   ' ,052.0 ),
     &    emis_legend( 'BERYLLIUM_F  ' ,009.0 ),
     &    emis_legend( 'BERYLLIUM_C  ' ,009.0 ),
     &    emis_legend( 'LEAD_F       ' ,207.2 ),
     &    emis_legend( 'LEAD_C       ' ,207.2 ),
     &    emis_legend( 'DIESEL_PMFINE' ,200.0 ),
     &    emis_legend( 'DIESEL_PMEC  ' ,012.0 ),
     &    emis_legend( 'DIESEL_PMOC  ' ,220.0 ),
     &    emis_legend( 'DIESEL_PMNO3 ' ,062.0 ),
     &    emis_legend( 'DIESEL_PMSO4 ' ,096.0 ),
     &    emis_legend( 'DIESEL_PMC   ' ,100.0 ),
     &    emis_legend( 'CADMIUM_F    ' ,112.4 ),
     &    emis_legend( 'CADMIUM_C    ' ,112.4 ),
     &    emis_legend( 'MANGANESE_F  ' ,054.9 ),
     &    emis_legend( 'MANGANESE_C  ' ,054.9 ),
     &    emis_legend( 'ARSENIC_F    ' ,074.92),
     &    emis_legend( 'ARSENIC_C    ' ,074.92),
     &    emis_legend( 'PHGI         ' ,200.5 )  /)

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

      IF ( RULE_STREAM .EQ. 'ALL' ) THEN
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
         IF ( IBIOSRM .GT.0 .AND. .NOT. Guard_BiogenicVOC   ) EM_STREAM_RULE( IBIOSRM  ) = .TRUE.
         IF ( IMGSRM  .GT.0 .AND. .NOT. Guard_MarineGas     ) EM_STREAM_RULE( IMGSRM   ) = .TRUE.
         IF ( ILTSRM  .GT.0 .AND. .NOT. Guard_LightningNO   ) EM_STREAM_RULE( ILTSRM   ) = .TRUE.
         IF ( ISEASRM .GT.0 .AND. .NOT. Guard_SeaSpray      ) EM_STREAM_RULE( ISEASRM  ) = .TRUE.
         IF ( IDUSTSRM.GT.0 .AND. .NOT. Guard_WindBlownDust ) EM_STREAM_RULE( IDUSTSRM ) = .TRUE.

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
 
      END MODULE EMIS_VARS
