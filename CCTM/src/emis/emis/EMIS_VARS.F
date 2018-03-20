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
      IMPLICIT NONE

      SAVE

      REAL,    ALLOCATABLE :: VDEMIS( :,:,:,: )      ! total emissions array
      REAL,    ALLOCATABLE :: VDEMIS_DIFF( :,:,:,: ) ! emissions array mapped to diffused species
      REAL,    ALLOCATABLE :: VDEMIS_CONV( :,:,:,: ) ! emissions array for rates that have been 
                                                     !   converted for proper inits in VDIFF module
      REAL,    ALLOCATABLE :: VDEMIS_SCLD( :,:,:,: ) ! emissions array for rates that have been 
                                                     !   scaled for input to CMAQ

      INTEGER              :: N_EM_SRC   ! global number of emissions sources
      INTEGER              :: N_FILE_GR, NPTGRPS, N_FILE_TR  
      INTEGER              :: N_EMIS_ISTR! global emissions instructions
      INTEGER              :: EMLAYS     ! no. of emis integration layers
                                         ! EMLAYS must be .LE. NLAYS
      INTEGER              :: N_EM_RGN   ! no. of region masks

      CHARACTER( 16 ), ALLOCATABLE :: EM_SPEC  ( : ) ! Vector of Internal CMAQ Species names for 
                                                     ! each emission operation
      CHARACTER( 16 ), ALLOCATABLE :: EM_SURR( :,: ) ! Emissions Surrogate to be read ( species, source )
      
      CHARACTER( 16 ), ALLOCATABLE :: TRACSPC( : )   ! Variables from Tracer Emission File
      CHARACTER( 16 ), ALLOCATABLE :: AREASPC( : )   ! Variables straight from the Area Emission File

      TYPE EM_SURR_TYPE
          INTEGER         :: LEN    ! Length of Sub-Vector
          CHARACTER( 16 ), ALLOCATABLE :: ARRY( : )   ! Surrogate Name
          CHARACTER( 16 ), ALLOCATABLE :: CMAQ( : )   ! Default Destination Species
          CHARACTER( 16 ), ALLOCATABLE :: UNITS( : )  ! Units determined from input files
          CHARACTER( 16 ), ALLOCATABLE :: PHASE( : )  ! Default Destination Size Dist or Phase
          REAL,            ALLOCATABLE :: MW( : )     ! Default MW from SPECIATE/SMOKE/MOVE
          LOGICAL,         ALLOCATABLE :: USED( : )   ! Whether or not the surrogate is used in CMAQ
      END TYPE EM_SURR_TYPE
      TYPE( EM_SURR_TYPE ),ALLOCATABLE :: EM_FILE_SURR( : )! Species to be read from each source file

      TYPE EM_REG
          CHARACTER( 32 ) :: LABEL
          CHARACTER( 32 ) :: FILE
          CHARACTER( 32 ) :: VAR
          INTEGER         :: FILENUM
      END TYPE EM_REG
      TYPE( EM_REG ), ALLOCATABLE :: EM_REGIONS( : )
      REAL, ALLOCATABLE           :: EM_REG_FAC( :,:,: )

      TYPE EM_FAC_STRUCTURE
          INTEGER              :: LEN     ! Number of scaling instructions that apply to this element of 
                                          !   the EM_FAC_ST structure. The other attributes will be of 
                                          !   length LEN.
          REAL, ALLOCATABLE    :: FAC(:)  ! Scale Factor applied with potential aerosol splitting
          REAL, ALLOCATABLE    :: BULK(:) ! Scale Factor for bulk emission without
                                          !   splitting for aerosols
          INTEGER, ALLOCATABLE :: REG(:)  ! Index Mapping this entry to a particlar region in EM_REGIONS
          INTEGER, ALLOCATABLE :: OP(:)   ! Operation to perform for this scaling
                                          !   (addition,'a' = 1, 
                                          !    multiplication,'m' = 2, 
                                          !    overwrite,'o' = 3)
      END TYPE EM_FAC_STRUCTURE
      TYPE( EM_FAC_STRUCTURE), ALLOCATABLE :: EM_FAC_ST ( :,: ) ! Emissions Scale Factors ( species, source )
      CHARACTER( 1 ) :: EM_OP_NAME( 3 ) = (/'a','m','o'/)
      
      
      CHARACTER( 200 ),ALLOCATABLE :: EM_FILE_NAME( : )! Filename of each emissions file
      CHARACTER( 30  ),ALLOCATABLE :: EM_FILE_LAB ( : )! Nickname of each emissions file
      CHARACTER( 16  ),ALLOCATABLE :: EM_FILE_TYPE( : )! shortname for the type of each emission
      INTEGER,         ALLOCATABLE :: EM_FILE_ITYPE( : )! shortname for the type of each emission
      CHARACTER( 100 ),ALLOCATABLE :: EM_FILE_DESC( : )! Description of each emissions file

      INTEGER              :: IBIOSRC, IMGSRC, ILTSRC, ISEASRC, IDUSTSRC
      INTEGER, ALLOCATABLE :: IGSRC( : ), IPSRC( : ), ITSRC( : ), MAP_PTtoISRC( : )

      INTEGER, ALLOCATABLE :: MAP_EMtoSURR( :,: ), MAP_EMtoDIFF( : ), MAP_EMtoGAS( : )

      CHARACTER( 16 ), ALLOCATABLE :: EMIS_AER_UNITS( : )   ! Initialize Aerosol Units with no value
      CHARACTER( 16 ), ALLOCATABLE :: EMIS_GAS_UNITS( : )   ! Initialize Vapor Units with no value
      REAL, ALLOCATABLE            :: CONVEM_GAS( : ), CONVEM_PM_MASS( : )
     
      ! Define Data Structure for Storing Emission Control Namelist
      ! Options                               
      TYPE EM_RULE_REG
             CHARACTER( 32 ) :: REGION
             CHARACTER( 32 ) :: SRC
             CHARACTER( 16 ) :: SURR
             CHARACTER( 16 ) :: SPEC
             CHARACTER( 16 ) :: PHASE
             REAL            :: FAC
             CHARACTER( 1  ) :: OP
      END TYPE EM_RULE_REG
      INTEGER, PARAMETER  :: N_EM_RULE_REG = 2000
   
      TYPE REGIONS_REG
             CHARACTER( 32 ) :: LABEL
             CHARACTER( 32 ) :: FILE
             CHARACTER( 32 ) :: VAR
      END TYPE REGIONS_REG
      INTEGER, PARAMETER  :: N_REGIONS_REG = 200


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
          REAL            :: MW    ! Emissions Surrogate Molecular Weight [g mol-1]
      End Type emis_legend
      
      Integer, Parameter :: N_Map_EMIStoCMAQ = 148
      Type ( emis_legend ), Parameter :: 
     &  Map_EMIStoCMAQ( N_Map_EMIStoCMAQ ) = (/
     &    emis_legend( 'NO2          ' ,'NO2         ','GAS   ',046.0 ),
     &    emis_legend( 'NO           ' ,'NO          ','GAS   ',030.0 ),
     &    emis_legend( 'HONO         ' ,'HONO        ','GAS   ',047.0 ),
     &    emis_legend( 'SO2          ' ,'SO2         ','GAS   ',064.0 ),
     &    emis_legend( 'SULF         ' ,'SULF        ','GAS   ',098.0 ),
     &    emis_legend( 'NH3          ' ,'NH3         ','GAS   ',017.0 ),
     &    emis_legend( 'CO           ' ,'CO          ','GAS   ',028.0 ),
     &    emis_legend( 'PACD         ' ,'RCOOH       ','GAS   ',076.0 ),
     &    emis_legend( 'AACD         ' ,'CCOOH       ','GAS   ',060.1 ),
     &    emis_legend( 'ALD2         ' ,'ALD2        ','GAS   ',044.0 ),
     &    emis_legend( 'FORM         ' ,'FORM        ','GAS   ',030.0 ),
     &    emis_legend( 'HCHO         ' ,'HCHO_PRIMARY','GAS   ',030.0 ),
     &    emis_legend( 'MEOH         ' ,'MEOH        ','GAS   ',032.0 ),
     &    emis_legend( 'FACD         ' ,'HCOOH       ','GAS   ',046.0 ),
     &    emis_legend( 'ALDX         ' ,'ALDX        ','GAS   ',058.0 ),
     &    emis_legend( 'GLYXL        ' ,'GLY         ','GAS   ',058.0 ),
     &    emis_legend( 'GLY          ' ,'GLY         ','GAS   ',058.0 ),
     &    emis_legend( 'MGLY         ' ,'MGLY        ','GAS   ',072.1 ),
     &    emis_legend( 'ETHA         ' ,'ETHA        ','GAS   ',030.1 ),
     &    emis_legend( 'ETH          ' ,'ETH         ','GAS   ',030.0 ),
     &    emis_legend( 'ETE          ' ,'ETE         ','GAS   ',028.0 ),
     &    emis_legend( 'HC3          ' ,'HC3         ','GAS   ',044.0 ),
     &    emis_legend( 'HC5          ' ,'HC5         ','GAS   ',072.0 ),
     &    emis_legend( 'HC8          ' ,'HC8         ','GAS   ',114.0 ),
     &    emis_legend( 'OLT          ' ,'OLT         ','GAS   ',042.0 ),
     &    emis_legend( 'OLI          ' ,'OLI         ','GAS   ',068.0 ),
     &    emis_legend( 'DIEN         ' ,'DIEN        ','GAS   ',054.0 ),
     &    emis_legend( 'ACE          ' ,'ACE         ','GAS   ',026.0 ),
     &    emis_legend( 'ORA1         ' ,'ORA1        ','GAS   ',046.0 ),
     &    emis_legend( 'ORA2         ' ,'ORA2        ','GAS   ',060.0 ),
     &    emis_legend( 'ETOH         ' ,'ETOH        ','GAS   ',046.1 ),
     &    emis_legend( 'KET          ' ,'KET         ','GAS   ',072.1 ),
     &    emis_legend( 'PAR          ' ,'PAR         ','GAS   ',072.1 ),
     &    emis_legend( 'ACET         ' ,'ACETONE     ','GAS   ',058.1 ),
     &    emis_legend( 'ACD          ' ,'ACD         ','GAS   ',044.0 ),
     &    emis_legend( 'ALD          ' ,'ALD         ','GAS   ',058.0 ),
     &    emis_legend( 'ACT          ' ,'ACT         ','GAS   ',058.0 ),
     &    emis_legend( 'UALD         ' ,'UALD        ','GAS   ',084.0 ),
     &    emis_legend( 'HKET         ' ,'HKET        ','GAS   ',074.0 ),
     &    emis_legend( 'PRPA         ' ,'PRPA        ','GAS   ',044.1 ),
     &    emis_legend( 'ETHY         ' ,'ETHY        ','GAS   ',026.0 ),
     &    emis_legend( 'OLE          ' ,'OLE         ','GAS   ',042.1 ),
     &    emis_legend( 'IOLE         ' ,'IOLE        ','GAS   ',056.1 ),
     &    emis_legend( 'OLE1         ' ,'OLE1        ','GAS   ',072.3 ),
     &    emis_legend( 'OLE2         ' ,'OLE2        ','GAS   ',075.8 ),
     &    emis_legend( 'RNO3         ' ,'RNO3        ','GAS   ',147.2 ),
     &    emis_legend( 'CCHO         ' ,'CCHO_PRIMARY','GAS   ',044.1 ),
     &    emis_legend( 'RCHO         ' ,'RCHO        ','GAS   ',058.1 ),
     &    emis_legend( 'ALK1         ' ,'ALK1        ','GAS   ',030.1 ),
     &    emis_legend( 'ALK2         ' ,'ALK2        ','GAS   ',036.7 ),
     &    emis_legend( 'ALK3         ' ,'ALK3        ','GAS   ',058.6 ),
     &    emis_legend( 'ALK4         ' ,'ALK4        ','GAS   ',077.6 ),
     &    emis_legend( 'ALK5         ' ,'ALK5        ','GAS   ',118.9 ),
     &    emis_legend( 'MEK          ' ,'MEK         ','GAS   ',072.1 ),
     &    emis_legend( 'BACL         ' ,'BACL        ','GAS   ',086.1 ),
     &    emis_legend( 'BALD         ' ,'BALD        ','GAS   ',106.1 ),
     &    emis_legend( 'MACR         ' ,'MACR        ','GAS   ',070.1 ),
     &    emis_legend( 'MVK          ' ,'MVK         ','GAS   ',070.1 ),
     &    emis_legend( 'IPRD         ' ,'IPRD        ','GAS   ',100.1 ),
     &    emis_legend( 'PRD2         ' ,'PRD2        ','GAS   ',116.2 ),
     &    emis_legend( 'ISOP         ' ,'ISOPRENE    ','GAS   ',068.1 ),
     &    emis_legend( 'ISO          ' ,'ISO         ','GAS   ',068.1 ),
     &    emis_legend( 'TERP         ' ,'TERP        ','GAS   ',136.2 ),
     &    emis_legend( 'APIN         ' ,'APIN        ','GAS   ',136.2 ),
     &    emis_legend( 'API          ' ,'API         ','GAS   ',136.2 ),
     &    emis_legend( 'LIM          ' ,'LIM         ','GAS   ',136.2 ),
     &    emis_legend( 'SESQ         ' ,'SESQ        ','GAS   ',204.0 ),
     &    emis_legend( 'BENZ         ' ,'BENZENE     ','GAS   ',078.1 ),
     &    emis_legend( 'BEN          ' ,'BENZENE     ','GAS   ',078.1 ),
     &    emis_legend( 'PHEN         ' ,'PHEN        ','GAS   ',094.0 ),
     &    emis_legend( 'CRES         ' ,'CRES        ','GAS   ',108.1 ),
     &    emis_legend( 'CSL          ' ,'CSL         ','GAS   ',108.0 ),
     &    emis_legend( 'TOL          ' ,'TOL         ','GAS   ',092.1 ),
     &    emis_legend( 'XYM          ' ,'XYM         ','GAS   ',106.0 ),
     &    emis_legend( 'XYP          ' ,'XYP         ','GAS   ',106.0 ),
     &    emis_legend( 'XYO          ' ,'XYO         ','GAS   ',106.0 ),
     &    emis_legend( 'XYLMN        ' ,'XYLMN       ','GAS   ',106.2 ),
     &    emis_legend( 'MXYL         ' ,'MXYL        ','GAS   ',106.2 ),
     &    emis_legend( 'OXYL         ' ,'OXYL        ','GAS   ',106.2 ),
     &    emis_legend( 'PXYL         ' ,'PXYL        ','GAS   ',106.2 ),
     &    emis_legend( 'ARO1         ' ,'ARO1        ','GAS   ',095.2 ),
     &    emis_legend( 'ARO2MN       ' ,'ARO2MN      ','GAS   ',118.7 ),
     &    emis_legend( 'NAPH         ' ,'NAPHTHAL    ','GAS   ',128.2 ),
     &    emis_legend( 'B124         ' ,'TMBENZ124   ','GAS   ',120.2 ),
     &    emis_legend( 'CH4          ' ,'ECH4        ','GAS   ',016.0 ),
     &    emis_legend( 'CL2          ' ,'CL2         ','GAS   ',071.0 ),
     &    emis_legend( 'HCL          ' ,'HCL         ','GAS   ',036.5 ),
     &    emis_legend( 'SOAALK       ' ,'SOAALK      ','GAS   ',112.0 ),
     &    emis_legend( 'FORM_PRIMARY ' ,'FORM_PRIMARY','GAS   ',030.0 ),
     &    emis_legend( 'ALD2_PRIMARY ' ,'ALD2_PRIMARY','GAS   ',044.0 ),
     &    emis_legend( 'BUTADIENE13  ' ,'BUTADIENE13 ','GAS   ',054.0 ),
     &    emis_legend( 'ACRO         ' ,'ACROLEIN    ','GAS   ',056.1 ),
     &    emis_legend( 'ACROLEIN     ' ,'ACRO_PRIMARY','GAS   ',056.1 ),
     &    emis_legend( 'TOLU         ' ,'TOLUENE     ','GAS   ',092.0 ),
     &    emis_legend( 'HGNRVA       ' ,'HG          ','GAS   ',200.6 ),
     &    emis_legend( 'HGIIGAS      ' ,'HGIIGAS     ','GAS   ',200.6 ),
     &    emis_legend( 'ETHE         ' ,'ETHENE      ','GAS   ',028.1 ),
     &    emis_legend( 'PRPE         ' ,'PROPENE     ','GAS   ',042.1 ),
     &    emis_legend( '13BDE        ' ,'BUTADIENE13 ','GAS   ',054.1 ),
     &    emis_legend( 'ACYE         ' ,'ACETYLENE   ','GAS   ',026.0 ),
     &    emis_legend( 'MOH          ' ,'MOH         ','GAS   ',032.0 ),
     &    emis_legend( 'EOH          ' ,'EOH         ','GAS   ',046.0 ),
     &    emis_legend( 'ROH          ' ,'ROH         ','GAS   ',060.0 ),
     &    emis_legend( 'ETEG         ' ,'ETEG        ','GAS   ',062.0 ),
     &    emis_legend( 'PSO4         ' ,'ASO4        ','FINE  ',096.0 ),
     &    emis_legend( 'PNO3         ' ,'ANO3        ','FINE  ',062.0 ),
     &    emis_legend( 'PNH4         ' ,'ANH4        ','FINE  ',018.0 ),
     &    emis_legend( 'PEC          ' ,'AEC         ','FINE  ',012.0 ),
     &    emis_legend( 'POC          ' ,'APOC        ','FINE  ',220.0 ),
     &    emis_legend( 'PNCOM        ' ,'APNCOM      ','FINE  ',220.0 ),
     &    emis_legend( 'PMOTHR       ' ,'AOTHR       ','FINE  ',200.0 ),
     &    emis_legend( 'PFE          ' ,'AFE         ','FINE  ',055.8 ),
     &    emis_legend( 'PAL          ' ,'AAL         ','FINE  ',027.0 ),
     &    emis_legend( 'PSI          ' ,'ASI         ','FINE  ',028.1 ),
     &    emis_legend( 'PTI          ' ,'ATI         ','FINE  ',047.9 ),
     &    emis_legend( 'PCA          ' ,'ACA         ','FINE  ',040.1 ),
     &    emis_legend( 'PMG          ' ,'AMG         ','FINE  ',024.3 ),
     &    emis_legend( 'PK           ' ,'AK          ','FINE  ',039.1 ),
     &    emis_legend( 'PMN          ' ,'AMN         ','FINE  ',054.9 ),
     &    emis_legend( 'PH2O         ' ,'AH2O        ','FINE  ',018.0 ),
     &    emis_legend( 'PNA          ' ,'ANA         ','FINE  ',023.0 ),
     &    emis_legend( 'PCL          ' ,'ACL         ','FINE  ',035.5 ),
     &    emis_legend( 'PMC          ' ,'ACORS       ','COARSE',100.0 ),
     &    emis_legend( 'ASOIL        ' ,'ASOIL       ','COARSE',100.0 ),
     &    emis_legend( 'ASEACAT      ' ,'ASEACAT     ','COARSE',023.75),
     &    emis_legend( 'NICKEL_F     ' ,'ANI         ','FINE  ',058.7 ),
     &    emis_legend( 'NICKEL_C     ' ,'ANI         ','COARSE',058.7 ),
     &    emis_legend( 'CHROMEHEX_F  ' ,'ACR_VI      ','FINE  ',052.0 ),
     &    emis_legend( 'CHROMEHEX_C  ' ,'ACR_VI      ','COARSE',052.0 ),
     &    emis_legend( 'CHROMTRI_F   ' ,'ACR_III     ','FINE  ',052.0 ),
     &    emis_legend( 'CHROMTRI_C   ' ,'ACR_III     ','COARSE',052.0 ),
     &    emis_legend( 'BERYLLIUM_F  ' ,'ABE         ','FINE  ',009.0 ),
     &    emis_legend( 'BERYLLIUM_C  ' ,'ABE         ','COARSE',009.0 ),
     &    emis_legend( 'LEAD_F       ' ,'APB         ','FINE  ',207.2 ),
     &    emis_legend( 'LEAD_C       ' ,'APB         ','COARSE',207.2 ),
     &    emis_legend( 'DIESEL_PMFINE' ,'ADE_OTHR    ','FINE  ',200.0 ),
     &    emis_legend( 'DIESEL_PMEC  ' ,'ADE_EC      ','FINE  ',012.0 ),
     &    emis_legend( 'DIESEL_PMOC  ' ,'ADE_OC      ','FINE  ',220.0 ),
     &    emis_legend( 'DIESEL_PMNO3 ' ,'ADE_NO3     ','FINE  ',062.0 ),
     &    emis_legend( 'DIESEL_PMSO4 ' ,'ADE_SO4     ','FINE  ',096.0 ),
     &    emis_legend( 'DIESEL_PMC   ' ,'ADE_CORS    ','COARSE',100.0 ),
     &    emis_legend( 'CADMIUM_F    ' ,'ACD         ','FINE  ',112.4 ),
     &    emis_legend( 'CADMIUM_C    ' ,'ACD         ','COARSE',112.4 ),
     &    emis_legend( 'MANGANESE_F  ' ,'AMN_HAPS    ','FINE  ',054.9 ),
     &    emis_legend( 'MANGANESE_C  ' ,'AMN_HAPS    ','COARSE',054.9 ),
     &    emis_legend( 'ARSENIC_F    ' ,'AAS         ','FINE  ',074.92),
     &    emis_legend( 'ARSENIC_C    ' ,'AAS         ','COARSE',074.92),
     &    emis_legend( 'PHGI         ' ,'APHG        ','FINE  ',200.5 )  /)

      CONTAINS

!-----------------------------------------------------------------------
      SUBROUTINE INTERPRET_EM_RULE_SRC( RULE_SRC0, IRULE, EM_SRC_RULE )
!
! This subroutine analyzes the part of a rule identifying which sources
! to affect and searches for the individual source, or, if the rule uses
! the 'ALL' keyword, this routine identifies every source as impacted.
!-----------------------------------------------------------------------

      USE UTILIO_DEFN


      IMPLICIT NONE

      CHARACTER(32), INTENT( IN ) :: RULE_SRC0
      CHARACTER(32)               :: RULE_SRC
      INTEGER, INTENT( IN )       :: IRULE
      LOGICAL, INTENT( INOUT )    :: EM_SRC_RULE( N_EM_SRC )

      INTEGER ISRC, IDX, LOGDEV
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

!-----------------------------------------------------------------------
      SUBROUTINE CUSTOM_EM_RULES( N_EM_RULE, EM_NML )
!
!     This subroutine defines several hardcoded rules for emissions
!     scaling that will apply by default. These include subtracting NH3
!     from fertilizer emissions if BiDi is turned on, moving all
!     sulfuric acid vapor to the particle phase upon emission and
!     splitting up the coarse mode anthropogenic emissions mass into
!     speciated compounds.
!-----------------------------------------------------------------------

      USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
      USE AERO_DATA, ONLY: ACORSEM_ASO4_FAC, ACORSEM_ANO3_FAC,
     &                     ACORSEM_ACL_FAC,  ACORSEM_AH2O_FAC,
     &                     ACORSEM_RENORM

      IMPLICIT NONE

      INTEGER,         INTENT( INOUT ) :: N_EM_RULE
      TYPE( EM_RULE_REG ), INTENT( INOUT ) :: EM_NML( N_EM_RULE_REG  )
      TYPE( EM_RULE_REG )                  :: EM_NML_B( N_EM_RULE_REG  )

      INTEGER          :: N_EM_RULE_B, IRULE
      LOGICAL          :: SUBTRACT_FERTILIZER_NH3 = .FALSE.
 
      N_EM_RULE_B = 0
 
      ! Add a rule For Removing Fertilizer Ammonia if BiDi is
      ! activated and the user has requested the correction
         IF ( ABFLUX .AND. SUBTRACT_FERTILIZER_NH3 ) THEN
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'NH3'
            EM_NML_B(N_EM_RULE_B)%SRC   = 'ALL'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'NH3_FERT'
            EM_NML_B(N_EM_RULE_B)%FAC   = -1.0
            EM_NML_B(N_EM_RULE_B)%OP    = 'a'
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
         END IF

      ! Add rules for moving sulfuric acid emissions from the gas phase
      ! to the aerosol phase
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'SULF'
         EM_NML_B(N_EM_RULE_B)%SRC   = 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'SULF'
         EM_NML_B(N_EM_RULE_B)%FAC   = 0.0
         EM_NML_B(N_EM_RULE_B)%OP    = 'o' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4'
         EM_NML_B(N_EM_RULE_B)%SRC   = 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'SULF'
         EM_NML_B(N_EM_RULE_B)%FAC   = 1.0
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

      ! Add rules for speciating coarse PM from Anthropogenic Sources
      !  Coarse-Mode Sulfate
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4'
         EM_NML_B(N_EM_RULE_B)%SRC   = 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_ASO4_FAC, 4 ) 
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

      !  Coarse-Mode Nitrate
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ANO3'
         EM_NML_B(N_EM_RULE_B)%SRC   = 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_ANO3_FAC, 4 )
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

      !  Coarse-Mode Chloride
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ACL'
         EM_NML_B(N_EM_RULE_B)%SRC   = 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_ACL_FAC, 4 ) 
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

      !  Coarse-Mode Water
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'AH2O'
         EM_NML_B(N_EM_RULE_B)%SRC   = 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_AH2O_FAC, 4 )
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
 
      !  Anthropogenic Coarse-Mode Other
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ACORS'
         EM_NML_B(N_EM_RULE_B)%SRC   = 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_RENORM, 4 )
         EM_NML_B(N_EM_RULE_B)%OP    = 'm' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
 
      ! [ INSERT MORE DEFAULT RULES HERE AS DESIRED ]

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
      SUBROUTINE INIT_EMIS_REGIONS( RGN_NML )
!
!     This subroutine defines several hardcoded rules for emissions
!     scaling that will apply by default. These include subtracting NH3
!     from fertilizer emissions if BiDi is turned on, moving all
!     sulfuric acid vapor to the particle phase upon emission and
!     splitting up the coarse mode anthropogenic emissions mass into
!     speciated compounds.
!----------------------------------------------------------------------- 
      USE GRID_CONF
      USE UTILIO_DEFN

      IMPLICIT NONE
                                                     
      TYPE( REGIONS_REG ) :: RGN_NML( N_REGIONS_REG )
      INTEGER, PARAMETER :: NFILE0 = 200
      CHARACTER( 32 )    :: FILENAMES( NFILE0 ) = ''
      CHARACTER( 32 ) :: VNAME

      INTEGER :: IRGN, NFILE, IDX, IFILE 
      INTEGER :: GXOFF, GYOFF, ENDCOL, ENDROW, STARTCOL, STARTROW
      INTEGER :: STDATE, STTIME
      CHARACTER( 16 )    :: PNAME = "INIT_REGIONS"
      CHARACTER( 250)    :: XMSG

      ! Find the total number of regions to be processed
      N_EM_RGN = 0  ! The first region is the entire domain
      DO IRGN = 1,N_REGIONS_REG
          IF ( RGN_NML( IRGN )%LABEL .EQ. '' ) EXIT
          N_EM_RGN = N_EM_RGN + 1
      END DO

      ! Allocate Vectors and Arrays for Each Region
      ALLOCATE( EM_REGIONS( N_EM_RGN ) )
      
      EM_REGIONS( 1 )%LABEL = 'EVERYWHERE'
      EM_REGIONS( 1 )%FILE  = 'N/A'
      EM_REGIONS( 1 )%VAR   = 'N/A'
      EM_REGIONS( 1 )%FILENUM = 1
      FILENAMES( 1 ) = 'N/A'
         
      ALLOCATE( EM_REG_FAC( NCOLS,NROWS,N_EM_RGN ) )
      EM_REG_FAC = 0.0
      EM_REG_FAC( :,:,1 ) = 1.0

      ! Populate global Region properties structure. Also assign each
      ! region a number according to the file it comes from. 1 =
      ! domain-wide.
      NFILE = 1
      IF ( N_EM_RGN .GT. 1 ) THEN
        DO IRGN = 2,N_EM_RGN
           EM_REGIONS( IRGN )%LABEL = RGN_NML( IRGN )%LABEL  ! Region Name
           EM_REGIONS( IRGN )%FILE  = RGN_NML( IRGN )%FILE   ! Logical filename
           EM_REGIONS( IRGN )%VAR   = RGN_NML( IRGN )%VAR    ! Variable from file 
                                                           !   used to inform mask

           IDX = INDEX1( EM_REGIONS( IRGN )%FILE, NFILE, FILENAMES(1:NFILE) )
           IF ( IDX .NE. 0 ) THEN
             EM_REGIONS( IRGN )%FILENUM = IDX
           ELSE
             NFILE = NFILE + 1
             EM_REGIONS( IRGN )%FILENUM = NFILE
             FILENAMES( NFILE ) = EM_REGIONS( IRGN )%FILE
           END IF                                                           
        END DO
      
        ! Process each region by looping through the pertinent files, 
        ! look up maps and save the data in a global array
        DO IFILE = 1,NFILE
          IF ( FILENAMES( IFILE ) .EQ. 'N/A' ) CYCLE
         
          ! Get domain decomp info from the emissions file
          CALL SUBHFILE ( FILENAMES( IFILE ), GXOFF, GYOFF,
     &                    STARTCOL, ENDCOL, STARTROW, ENDROW )
         
          ! Open input file
          IF ( .NOT. OPEN3( FILENAMES( IFILE ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open '// FILENAMES( IFILE ) // ' file'
             CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
          END IF
         
          ! Retrieve File Header Information
          IF ( .NOT. DESC3( FILENAMES( IFILE ) ) ) THEN
              XMSG = 'Could not get ' // FILENAMES( IFILE ) // ' file description'
             CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
          END IF
         
          ! Read data from regions file into region array 
          DO IRGN = 2,N_EM_RGN
              IF ( EM_REGIONS( IRGN )%FILENUM .EQ. IFILE ) THEN
                  VNAME = EM_REGIONS( IRGN )%VAR
                  IF ( .NOT. INTERPX( FILENAMES( IFILE ), VNAME, PNAME,
     &                 STARTCOL, ENDCOL, STARTROW, ENDROW, 1, 1,
     &                 SDATE3D, STIME3D, EM_REG_FAC( 1,1,IRGN ) ) )
     &                 CALL M3ERR( PNAME, SDATE3D, STIME3D,
     &                     'Could not read ' // VNAME //
     &                     'from ' // FILENAMES( IFILE ) // ' file', .TRUE. )
              END IF
          END DO

          ! Close Regions File
          IF ( .NOT. CLOSE3( FILENAMES( IFILE ) ) ) THEN
            XMSG = 'Could not close ' // FILENAMES( IFILE )
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
          END IF
         
        END DO
      END IF
      
      END SUBROUTINE INIT_EMIS_REGIONS

      END MODULE EMIS_VARS
