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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/arc/CCTM/src/emis/emis/EMIS_DEFN.F,v 1.10 2011/10/21 16:10:46 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE EMIS_VARS

C-----------------------------------------------------------------------
C Function: Define global variables used for emissions processor

C Revision History:
C     09 Nov 2017 B.Murphy: Extracted these variables from EMIS_DEFN
C-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: )      ! total emissions array
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_DIFF( :,:,:,: ) ! emissions array mapped to diffused species

      INTEGER,              SAVE :: N_EM_SRC   ! global number of emissions sources
      INTEGER,              SAVE :: N_FILE_GR, NPTGRPS, N_FILE_TR  
      INTEGER,              SAVE :: N_EMIS_ISTR! global emissions species
      INTEGER,              SAVE :: EMLAYS     ! no. of emis integration layers
                                               ! EMLAYS must be .LE. NLAYS

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EM_SPEC  ( : ) ! Vector of Internal CMAQ Species names for 
                                                           ! each emission operation
      REAL, ALLOCATABLE, SAVE            :: EM_FAC ( :,: ) ! Emissions Scale Factors ( species, source )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EM_SURR( :,: ) ! Emissions Surrogate to be read ( species, source )
      
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: TRACSPC( : )   ! Variables from Tracer Emission File
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: AREASPC( : )   ! Variables straight from the Area Emission File

      TYPE EM_SURR_TYPE
          INTEGER         :: LEN    ! Length of Sub-Vector
          CHARACTER( 16 ), ALLOCATABLE :: ARRY( : )   ! Surrogate Name
          CHARACTER( 16 ), ALLOCATABLE :: CMAQ( : )   ! Default Destination Species
          CHARACTER( 16 ), ALLOCATABLE :: UNITS( : )  ! Units determined from input files
          CHARACTER( 16 ), ALLOCATABLE :: PHASE( : )  ! Default Destination Size Dist or Phase
          REAL,            ALLOCATABLE :: MW( : )     ! Default MW from SPECIATE/SMOKE/MOVE
      END TYPE EM_SURR_TYPE
      TYPE( EM_SURR_TYPE ),ALLOCATABLE,SAVE :: EM_FILE_SURR( : )! Species to be read from each source file

      CHARACTER( 200 ),ALLOCATABLE, SAVE :: EM_FILE_NAME( : )! Filename of each emissions file
      CHARACTER( 30  ),ALLOCATABLE, SAVE :: EM_FILE_LAB ( : )! Nickname of each emissions file
      CHARACTER( 16  ),ALLOCATABLE, SAVE :: EM_FILE_TYPE( : )! shortname for the type of each emission
      CHARACTER( 100 ),ALLOCATABLE, SAVE :: EM_FILE_DESC( : )! Description of each emissions file

      INTEGER, SAVE :: IBIOSRC, IMGSRC, ILTSRC, ISEASRC, IDUSTSRC
      INTEGER, ALLOCATABLE, SAVE :: IGSRC( : ), IPSRC( : ), ITSRC( : ), MAP_PTtoISRC( : )

      INTEGER, ALLOCATABLE, SAVE :: MAP_EMtoSURR( :,: ), MAP_EMtoDIFF( : )

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EMIS_AER_UNITS( : )   ! Initialize Aerosol Units with no value
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EMIS_GAS_UNITS( : )   ! Initialize Vapor Units with no value
      REAL, ALLOCATABLE, SAVE :: CONVEM( : ), CONVEM_PM_MASS( : )
     
      ! Define Data Structure for Storing Emission Control Namelist
      ! Options                               
      TYPE EM_RULE
             CHARACTER( 16 ) :: SPEC
             CHARACTER( 32 ) :: SRC
             CHARACTER( 16 ) :: PHASE
             CHARACTER( 16 ) :: SURR
             REAL            :: FAC
             CHARACTER( 1  ) :: OP
      END TYPE EM_RULE
      INTEGER, PARAMETER  :: N_EM_RULE0 = 2000
   

      LOGICAL :: Guard_BiogenicVOC   = .FALSE.
      LOGICAL :: Guard_MarineGas     = .FALSE.
      LOGICAL :: Guard_LightningNO   = .FALSE.
      LOGICAL :: Guard_WindBlownDust = .FALSE.
      LOGICAL :: Guard_SeaSpray      = .FALSE.
 
      ! Create Map from CMAQ Species Names to Common Surrogate
      ! Names on Emissions Input Files. Also include default aerosol mode
      ! for aerosols and molecular weight assumed in SPECIATE/SMOKE/MOVES.
      ! In order to match Emissions surrogates to transported CMAQ
      ! species, the code will first look for the name that matches the
      ! surrogate completely, then it will look for a species name that
      ! matches the alternate default, stored here in the CMAQ
      ! sub-vector.
      Type emis_legend
          Character( 16 ) :: SURR  ! Emissions Surrogate Species
          Character( 16 ) :: CMAQ  ! Destination CMAQ Internal Species
          Character( 16 ) :: PHASE ! Surrogate Emission Mode
          REAL            :: MW    ! Emissions Surrogate Molecular Weight [kg mol-1]
      End Type emis_legend
      
      Integer, Parameter :: N_Map_EMIStoCMAQ = 118
      Type ( emis_legend ), Parameter :: 
     &  Map_EMIStoCMAQ( N_Map_EMIStoCMAQ ) = (/
     &    emis_legend( 'NO2          ' ,'NO2         ','GAS   ',0.046 ),
     &    emis_legend( 'NO           ' ,'NO          ','GAS   ',0.030 ),
     &    emis_legend( 'HONO         ' ,'HONO        ','GAS   ',0.047 ),
     &    emis_legend( 'SO2          ' ,'SO2         ','GAS   ',0.064 ),
     &    emis_legend( 'SULF         ' ,'SULF        ','GAS   ',0.098 ),
     &    emis_legend( 'CO           ' ,'CO          ','GAS   ',0.028 ),
     &    emis_legend( 'PACD         ' ,'RCOOH       ','GAS   ',0.076 ),
     &    emis_legend( 'AACD         ' ,'CCOOH       ','GAS   ',0.0601),
     &    emis_legend( 'ALD2         ' ,'ALD2        ','GAS   ',0.044 ),
     &    emis_legend( 'FORM         ' ,'FORM        ','GAS   ',0.030 ),
     &    emis_legend( 'HCHO         ' ,'HCHO_PRIMARY','GAS   ',0.030 ),
     &    emis_legend( 'MEOH         ' ,'MEOH        ','GAS   ',0.032 ),
     &    emis_legend( 'FACD         ' ,'HCOOH       ','GAS   ',0.046 ),
     &    emis_legend( 'ALDX         ' ,'ALDX        ','GAS   ',0.058 ),
     &    emis_legend( 'GLYXL        ' ,'GLY         ','GAS   ',0.058 ),
     &    emis_legend( 'GLY          ' ,'GLY         ','GAS   ',0.058 ),
     &    emis_legend( 'MGLY         ' ,'MGLY        ','GAS   ',0.0721),
     &    emis_legend( 'ETHA         ' ,'ETHA        ','GAS   ',0.0301),
     &    emis_legend( 'ETOH         ' ,'ETOH        ','GAS   ',0.0461),
     &    emis_legend( 'KET          ' ,'KET         ','GAS   ',0.0721),
     &    emis_legend( 'PAR          ' ,'PAR         ','GAS   ',0.0721),
     &    emis_legend( 'ACET         ' ,'ACETONE     ','GAS   ',0.0581),
     &    emis_legend( 'PRPA         ' ,'PRPA        ','GAS   ',0.0441),
     &    emis_legend( 'ETHY         ' ,'ETHY        ','GAS   ',0.026 ),
     &    emis_legend( 'OLE          ' ,'OLE         ','GAS   ',0.0421),
     &    emis_legend( 'IOLE         ' ,'IOLE        ','GAS   ',0.0561),
     &    emis_legend( 'OLE1         ' ,'OLE1        ','GAS   ',0.0723),
     &    emis_legend( 'OLE2         ' ,'OLE2        ','GAS   ',0.0758),
     &    emis_legend( 'RNO3         ' ,'RNO3        ','GAS   ',0.1472),
     &    emis_legend( 'CCHO         ' ,'CCHO_PRIMARY','GAS   ',0.0441),
     &    emis_legend( 'RCHO         ' ,'RCHO        ','GAS   ',0.0581),
     &    emis_legend( 'ALK1         ' ,'ALK1        ','GAS   ',0.0301),
     &    emis_legend( 'ALK2         ' ,'ALK2        ','GAS   ',0.0367),
     &    emis_legend( 'ALK3         ' ,'ALK3        ','GAS   ',0.0586),
     &    emis_legend( 'ALK4         ' ,'ALK4        ','GAS   ',0.0776),
     &    emis_legend( 'ALK5         ' ,'ALK5        ','GAS   ',0.1189),
     &    emis_legend( 'MEK          ' ,'MEK         ','GAS   ',0.0721),
     &    emis_legend( 'BACL         ' ,'BACL        ','GAS   ',0.0861),
     &    emis_legend( 'BALD         ' ,'BALD        ','GAS   ',0.1061),
     &    emis_legend( 'MACR         ' ,'MACR        ','GAS   ',0.0701),
     &    emis_legend( 'MVK          ' ,'MVK         ','GAS   ',0.0701),
     &    emis_legend( 'IPRD         ' ,'IPRD        ','GAS   ',0.1001),
     &    emis_legend( 'PRD2         ' ,'PRD2        ','GAS   ',0.1162),
     &    emis_legend( 'ISOP         ' ,'ISOPRENE    ','GAS   ',0.0681),
     &    emis_legend( 'TERP         ' ,'TERP        ','GAS   ',0.1362),
     &    emis_legend( 'APIN         ' ,'APIN        ','GAS   ',0.1362),
     &    emis_legend( 'SESQ         ' ,'SESQ        ','GAS   ',0.204 ),
     &    emis_legend( 'BENZ         ' ,'BENZENE     ','GAS   ',0.0781),
     &    emis_legend( 'CRES         ' ,'CRES        ','GAS   ',0.1081),
     &    emis_legend( 'TOL          ' ,'TOL         ','GAS   ',0.0921),
     &    emis_legend( 'XYLMN        ' ,'XYLMN       ','GAS   ',0.1062),
     &    emis_legend( 'MXYL         ' ,'MXYL        ','GAS   ',0.1062),
     &    emis_legend( 'OXYL         ' ,'OXYL        ','GAS   ',0.1062),
     &    emis_legend( 'PXYL         ' ,'PXYL        ','GAS   ',0.1062),
     &    emis_legend( 'ARO1         ' ,'ARO1        ','GAS   ',0.0952),
     &    emis_legend( 'ARO2MN       ' ,'ARO2MN      ','GAS   ',0.1187),
     &    emis_legend( 'NAPH         ' ,'NAPHTHAL    ','GAS   ',0.1282),
     &    emis_legend( 'B124         ' ,'TMBENZ124   ','GAS   ',0.1202),
     &    emis_legend( 'ECH4         ' ,'CH4         ','GAS   ',0.016 ),
     &    emis_legend( 'CL2          ' ,'CL2         ','GAS   ',0.071 ),
     &    emis_legend( 'HCL          ' ,'HCL         ','GAS   ',0.0365),
     &    emis_legend( 'SOAALK       ' ,'SOAALK      ','GAS   ',0.112 ),
     &    emis_legend( 'FORM_PRIMARY ' ,'FORM_PRIMARY','GAS   ',0.030 ),
     &    emis_legend( 'ALD2_PRIMARY ' ,'ALD2_PRIMARY','GAS   ',0.044 ),
     &    emis_legend( 'BUTADIENE13  ' ,'BUTADIENE13 ','GAS   ',0.054 ),
     &    emis_legend( 'ACRO         ' ,'ACROLEIN    ','GAS   ',0.0561),
     &    emis_legend( 'ACROLEIN     ' ,'ACRO_PRIMARY','GAS   ',0.0561),
     &    emis_legend( 'TOLU         ' ,'TOLUENE     ','GAS   ',0.092 ),
     &    emis_legend( 'HGNRVA       ' ,'HG          ','GAS   ',0.2006),
     &    emis_legend( 'HGIIGAS      ' ,'HGIIGAS     ','GAS   ',0.2006),
     &    emis_legend( 'ETHE         ' ,'ETHENE      ','GAS   ',0.0281),
     &    emis_legend( 'PRPE         ' ,'PROPENE     ','GAS   ',0.0421),
     &    emis_legend( '13BDE        ' ,'BUTADIENE13 ','GAS   ',0.0541),
     &    emis_legend( 'ACYE         ' ,'ACETYLENE   ','GAS   ',0.026 ),
     &    emis_legend( 'PSO4         ' ,'ASO4        ','FINE  ',0.096 ),
     &    emis_legend( 'PNO3         ' ,'ANO3        ','FINE  ',0.062 ),
     &    emis_legend( 'PNH4         ' ,'ANH4        ','FINE  ',0.018 ),
     &    emis_legend( 'PEC          ' ,'AEC         ','FINE  ',0.012 ),
     &    emis_legend( 'POC          ' ,'APOC        ','FINE  ',0.220 ),
     &    emis_legend( 'PNCOM        ' ,'APNCOM      ','FINE  ',0.220 ),
     &    emis_legend( 'PMOTHR       ' ,'AOTHR       ','FINE  ',0.200 ),
     &    emis_legend( 'PFE          ' ,'AFE         ','FINE  ',0.0558),
     &    emis_legend( 'PAL          ' ,'AAL         ','FINE  ',0.027 ),
     &    emis_legend( 'PSI          ' ,'ASI         ','FINE  ',0.0281),
     &    emis_legend( 'PTI          ' ,'ATI         ','FINE  ',0.0479),
     &    emis_legend( 'PCA          ' ,'ACA         ','FINE  ',0.0401),
     &    emis_legend( 'PMG          ' ,'AMG         ','FINE  ',0.0243),
     &    emis_legend( 'PK           ' ,'AK          ','FINE  ',0.0391),
     &    emis_legend( 'PMN          ' ,'AMN         ','FINE  ',0.0549),
     &    emis_legend( 'PH2O         ' ,'AH2O        ','FINE  ',0.018 ),
     &    emis_legend( 'PNA          ' ,'ANA         ','FINE  ',0.023 ),
     &    emis_legend( 'PCL          ' ,'ACL         ','FINE  ',0.0355),
     &    emis_legend( 'PMC          ' ,'ACORS       ','COARSE',0.100 ),
     &    emis_legend( 'ASOIL        ' ,'ASOIL       ','COARSE',0.100 ),
     &    emis_legend( 'ASEACAT      ' ,'ASEACAT     ','COARSE',0.02375),
     &    emis_legend( 'NICKEL_F     ' ,'ANI         ','FINE  ',0.0587),
     &    emis_legend( 'NICKEL_C     ' ,'ANI         ','COARSE',0.0587),
     &    emis_legend( 'CHROMEHEX_F  ' ,'ACR_VI      ','FINE  ',0.052 ),
     &    emis_legend( 'CHROMEHEX_C  ' ,'ACR_VI      ','COARSE',0.052 ),
     &    emis_legend( 'CHROMTRI_F   ' ,'ACR_III     ','FINE  ',0.052 ),
     &    emis_legend( 'CHROMTRI_C   ' ,'ACR_III     ','COARSE',0.052 ),
     &    emis_legend( 'BERYLLIUM_F  ' ,'ABE         ','FINE  ',0.009 ),
     &    emis_legend( 'BERYLLIUM_C  ' ,'ABE         ','COARSE',0.009 ),
     &    emis_legend( 'LEAD_F       ' ,'APB         ','FINE  ',0.2072),
     &    emis_legend( 'LEAD_C       ' ,'APB         ','COARSE',0.2072),
     &    emis_legend( 'DIESEL_PMFINE' ,'ADE_OTHR    ','FINE  ',0.200 ),
     &    emis_legend( 'DIESEL_PMEC  ' ,'ADE_EC      ','FINE  ',0.012 ),
     &    emis_legend( 'DIESEL_PMOC  ' ,'ADE_OC      ','FINE  ',0.220 ),
     &    emis_legend( 'DIESEL_PMNO3 ' ,'ADE_NO3     ','FINE  ',0.062 ),
     &    emis_legend( 'DIESEL_PMSO4 ' ,'ADE_SO4     ','FINE  ',0.096 ),
     &    emis_legend( 'DIESEL_PMC   ' ,'ADE_        ','COARSE',0.100 ),
     &    emis_legend( 'CADMIUM_F    ' ,'ACD         ','FINE  ',0.1124),
     &    emis_legend( 'CADMIUM_C    ' ,'ACD         ','COARSE',0.1124),
     &    emis_legend( 'MANGANESE_F  ' ,'AMN_HAPS    ','FINE  ',0.0549),
     &    emis_legend( 'MANGANESE_C  ' ,'AMN_HAPS    ','COARSE',0.0549),
     &    emis_legend( 'ARSENIC_F    ' ,'AAS         ','FINE  ',0.07492),
     &    emis_legend( 'ARSENIC_C    ' ,'AAS         ','COARSE',0.07492),
     &    emis_legend( 'PHGI         ' ,'APHG        ','FINE  ',0.2005)  /)

      CONTAINS
C-----------------------------------------------------------------------
      SUBROUTINE INTERPRET_EM_RULE_SRC( RULE_SRC0, EM_SRC_RULE )

! This subroutine analyzes the part of a rule identifying which sources
! to affect and searches for the individual source, or, if the rule uses
! the 'ALL' keyword, this routine identifies every source as impacted.

      USE UTILIO_DEFN


      IMPLICIT NONE

      CHARACTER(32), INTENT( IN ) :: RULE_SRC0
      CHARACTER(32)               :: RULE_SRC
      LOGICAL, INTENT( INOUT )       :: EM_SRC_RULE( N_EM_SRC )

      INTEGER ISRC, IDX, LOGDEV, IRULE
      CHARACTER( 200 ) :: XMSG
      CHARACTER( 16 )  :: PNAME = "EMIS_SPC_MAP"
      INTEGER  :: JDATE = 0
      INTEGER  :: JTIME = 0

      LOGDEV = INIT3()

      ! Initialize the output vector
      EM_SRC_RULE = .FALSE.
      
      ! First Capitalize the Rule so it is easier to error-check
      RULE_SRC = RULE_SRC0
      CALL UPCASE( RULE_SRC )

      IF ( RULE_SRC .EQ. 'ALL' ) THEN
         ! Special Case Where the Source Identifier in the rule equals
         ! equals all 'ALL'. Expand the Rule to Apply to All Sources
         ISRC = 0
         IF ( N_FILE_GR .GT. 0 ) THEN
           EM_SRC_RULE( ISRC+1:N_FILE_GR ) = .TRUE.
           ISRC = ISRC + N_FILE_GR
         END IF

         IF ( NPTGRPS .GT. 0 ) THEN
           EM_SRC_RULE( ISRC+1:ISRC+NPTGRPS ) = .TRUE.
           ISRC = ISRC + NPTGRPS
         END IF
         IF ( N_FILE_TR .GT. 0 ) EM_SRC_RULE( ISRC+1:ISRC+N_FILE_TR ) = .TRUE.
         IF ( IBIOSRC .GT.0 .AND. .NOT. Guard_BiogenicVOC   ) EM_SRC_RULE( IBIOSRC  ) = .TRUE.
         IF ( IMGSRC  .GT.0 .AND. .NOT. Guard_MarineGas     ) EM_SRC_RULE( IMGSRC   ) = .TRUE.
         IF ( ILTSRC  .GT.0 .AND. .NOT. Guard_LightningNO   ) EM_SRC_RULE( ILTSRC   ) = .TRUE.
         IF ( ISEASRC .GT.0 .AND. .NOT. Guard_SeaSpray      ) EM_SRC_RULE( ISEASRC  ) = .TRUE.
         IF ( IDUSTSRC.GT.0 .AND. .NOT. Guard_WindBlownDust ) EM_SRC_RULE( IDUSTSRC ) = .TRUE.

      ELSE
         ! Find the Specific Source this Rule Identifies
         IDX = INDEX1( RULE_SRC, N_EM_SRC, EM_FILE_LAB ) 
         IF ( IDX .NE. 0 ) THEN
           EM_SRC_RULE( IDX ) = .TRUE.
         ELSE
           WRITE( LOGDEV, * )
           WRITE( LOGDEV, '(/,3A,I3,/,2A,/,2A)' ),
     &         'The Emissions Source Label (',TRIM( RULE_SRC ),
     &         ') applied for Rule ',IRULE,' in the Emissions Control ',
     &         'Namelist does not match any of the emissions source ',
     &         'labels provided to CMAQ. Please check the runscript against ',
     &         'your emissions control inputs.'
           XMSG = 'Error in Emissions Map Processing.'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF
 
      END SUBROUTINE INTERPRET_EM_RULE_SRC

C-----------------------------------------------------------------------
      SUBROUTINE CUSTOM_EM_RULES( N_EM_RULE, EM_NML )

      USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
      USE AERO_DATA, ONLY: ACORSEM_ASO4_FAC, ACORSEM_ANO3_FAC,
     &                     ACORSEM_ACL_FAC,  ACORSEM_AH2O_FAC,
     &                     ACORSEM_RENORM

      IMPLICIT NONE

      INTEGER,         INTENT( INOUT ) :: N_EM_RULE
      TYPE( EM_RULE ), INTENT( INOUT ) :: EM_NML( N_EM_RULE0  )

      LOGICAL          :: SUBTRACT_FERTILIZER_NH3 = .FALSE.
 
 
      ! Add a rule For Removing Fertilizer Ammonia if BiDi is
      ! activated and the user has requested the correction
         IF ( ABFLUX .AND. SUBTRACT_FERTILIZER_NH3 ) THEN
            N_EM_RULE = N_EM_RULE + 1
            EM_NML(N_EM_RULE)%SPEC  = 'NH3'
            EM_NML(N_EM_RULE)%SRC   = 'ALL'
            EM_NML(N_EM_RULE)%PHASE = 'GAS'
            EM_NML(N_EM_RULE)%SURR  = 'NH3_FERT'
            EM_NML(N_EM_RULE)%FAC   = -1.0
            EM_NML(N_EM_RULE)%OP    = 'a'
         END IF

      ! Add rules for moving sulfuric acid emissions from the gas phase
      ! to the aerosol phase
         N_EM_RULE = N_EM_RULE + 1
         EM_NML(N_EM_RULE)%SPEC  = 'SULF'
         EM_NML(N_EM_RULE)%SRC   = 'ALL'
         EM_NML(N_EM_RULE)%PHASE = 'GAS'
         EM_NML(N_EM_RULE)%SURR  = 'SULF'
         EM_NML(N_EM_RULE)%FAC   = 0.0
         EM_NML(N_EM_RULE)%OP    = 'o' 

         N_EM_RULE = N_EM_RULE + 1
         EM_NML(N_EM_RULE)%SPEC  = 'ASO4'
         EM_NML(N_EM_RULE)%SRC   = 'ALL'
         EM_NML(N_EM_RULE)%PHASE = 'FINE'
         EM_NML(N_EM_RULE)%SURR  = 'SULF'
         EM_NML(N_EM_RULE)%FAC   = 1.0
         EM_NML(N_EM_RULE)%OP    = 'a' 

      ! Add rules for speciating coarse PM from Anthropogenic Sources
      !  Coarse-Mode Sulfate
         N_EM_RULE = N_EM_RULE + 1
         EM_NML(N_EM_RULE)%SPEC  = 'ASO4'
         EM_NML(N_EM_RULE)%SRC   = 'ALL'
         EM_NML(N_EM_RULE)%PHASE = 'COARSE'
         EM_NML(N_EM_RULE)%SURR  = 'PMC'
         EM_NML(N_EM_RULE)%FAC   = acorsem_aso4_fac
         EM_NML(N_EM_RULE)%OP    = 'a' 

      !  Coarse-Mode Nitrate
         N_EM_RULE = N_EM_RULE + 1
         EM_NML(N_EM_RULE)%SPEC  = 'ANO3'
         EM_NML(N_EM_RULE)%SRC   = 'ALL'
         EM_NML(N_EM_RULE)%PHASE = 'COARSE'
         EM_NML(N_EM_RULE)%SURR  = 'PMC'
         EM_NML(N_EM_RULE)%FAC   = acorsem_ano3_fac
         EM_NML(N_EM_RULE)%OP    = 'a' 

      !  Coarse-Mode Chloride
         N_EM_RULE = N_EM_RULE + 1
         EM_NML(N_EM_RULE)%SPEC  = 'ACL'
         EM_NML(N_EM_RULE)%SRC   = 'ALL'
         EM_NML(N_EM_RULE)%PHASE = 'COARSE'
         EM_NML(N_EM_RULE)%SURR  = 'PMC'
         EM_NML(N_EM_RULE)%FAC   = acorsem_acl_fac
         EM_NML(N_EM_RULE)%OP    = 'a' 

      !  Coarse-Mode Water
         N_EM_RULE = N_EM_RULE + 1
         EM_NML(N_EM_RULE)%SPEC  = 'AH2O'
         EM_NML(N_EM_RULE)%SRC   = 'ALL'
         EM_NML(N_EM_RULE)%PHASE = 'COARSE'
         EM_NML(N_EM_RULE)%SURR  = 'PMC'
         EM_NML(N_EM_RULE)%FAC   = acorsem_ah2o_fac
         EM_NML(N_EM_RULE)%OP    = 'a' 
 
      !  Anthropogenic Coarse-Mode Other
         N_EM_RULE = N_EM_RULE + 1
         EM_NML(N_EM_RULE)%SPEC  = 'ASO4'
         EM_NML(N_EM_RULE)%SRC   = 'ALL'
         EM_NML(N_EM_RULE)%PHASE = 'COARSE'
         EM_NML(N_EM_RULE)%SURR  = 'PMC'
         EM_NML(N_EM_RULE)%FAC   = acorsem_renorm
         EM_NML(N_EM_RULE)%OP    = 'a' 
 
      END SUBROUTINE CUSTOM_EM_RULES
 
      END MODULE EMIS_VARS
