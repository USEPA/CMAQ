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

      IMPLICIT NONE

      SAVE

      REAL,    ALLOCATABLE :: VDEMIS_DIFF( :,:,:,: ) ! emissions array mapped to diffused species

      INTEGER              :: N_EM_SRM   ! global number of emissions streams
      INTEGER              :: N_EMIS_ISTR! global emissions instructions
      INTEGER              :: N_EM_RGN   ! no. of region masks
      INTEGER              :: EMLAYS

      CHARACTER( 16 ), ALLOCATABLE :: EM_SPEC  ( : ) ! Vector of Internal CMAQ Species names for 
                                                     ! each emission operation
      CHARACTER( 16 ), ALLOCATABLE :: EM_SURR( :,: ) ! Emissions Surrogate to be read ( species, stream )
      
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
      TYPE( EM_SURR_TYPE ),ALLOCATABLE :: EM_FILE_SURR( : )! Species to be read from each stream file

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
      LOGICAL         ,ALLOCATABLE :: EM_FILE_DATE_OVRD( : ) ! Toggle for whether or not to let the 
                                                                ! emission stream date override the model
      INTEGER         ,ALLOCATABLE :: EM_FILE_DATE( : )! Date to be read in from each emissions stream
      LOGICAL         ,ALLOCATABLE :: EM_FILE_FIRE( : )! Is this Emission Stream Representing Fires (1=Yes)

      INTEGER              :: IBIOSRM, IMGSRM, ILTSRM, ISEASRM, IDUSTSRM
      INTEGER, ALLOCATABLE :: IGSRM( : ), IPSRM( : ), ITSRM( : ), MAP_PTtoISRM( : )

      INTEGER, ALLOCATABLE :: MAP_EMtoSURR( :,: ), MAP_EMtoDIFF( : ), MAP_EMtoGAS( : )

      CHARACTER( 16 ), ALLOCATABLE :: EMIS_AER_UNITS( : )   ! Initialize Aerosol Units with no value
      CHARACTER( 16 ), ALLOCATABLE :: EMIS_GAS_UNITS( : )   ! Initialize Vapor Units with no value
      REAL, ALLOCATABLE            :: CONVEM_GAS( : ), CONVEM_PM_MASS( : )
     
      ! Define Data Structure for Storing Emission Control Namelist
      ! Options                               
      TYPE EM_RULE_REG
             CHARACTER( 32 ) :: REGION
             CHARACTER( 32 ) :: STREAM
             CHARACTER( 16 ) :: SURR
             CHARACTER( 16 ) :: SPEC
             CHARACTER( 16 ) :: PHASE
             REAL            :: FAC
             CHARACTER( 4  ) :: BASIS
             CHARACTER( 1  ) :: OP
      END TYPE EM_RULE_REG
      INTEGER, PARAMETER  :: N_EM_RULE_REG = 2000
      TYPE( EM_RULE_REG ), ALLOCATABLE :: EM_NML( : )
   
      TYPE REGIONS_REG
             CHARACTER( 32 ) :: LABEL
             CHARACTER( 32 ) :: FILE
             CHARACTER( 32 ) :: VAR
      END TYPE REGIONS_REG
      INTEGER, PARAMETER  :: N_REGIONS_REG = 200
      TYPE( REGIONS_REG ), ALLOCATABLE :: RGN_NML( : )

      ! Define Structure for Size Distribution Rule      
      TYPE SD_RULE
             CHARACTER( 32 ) :: STREAM
             CHARACTER( 16 ) :: MODE
             CHARACTER( 16 ) :: MODE_REF
      END TYPE SD_RULE
      INTEGER, PARAMETER :: N_SD_REG = 200
      TYPE( SD_RULE ), ALLOCATABLE :: SD_NML( : )

      LOGICAL :: DefaultScaling      = .TRUE.
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
      
      Integer, Parameter :: N_Map_EMIStoCMAQ = 149
      Type ( emis_legend ), Parameter :: 
     &  Map_EMIStoCMAQ( N_Map_EMIStoCMAQ ) = (/
     &    emis_legend( 'NO2          ' ,'NO2         ','GAS   ',046.0 ),
     &    emis_legend( 'NO           ' ,'NO          ','GAS   ',030.0 ),
     &    emis_legend( 'HONO         ' ,'HONO        ','GAS   ',047.0 ),
     &    emis_legend( 'SO2          ' ,'SO2         ','GAS   ',064.0 ),
     &    emis_legend( 'SULF         ' ,'SULF        ','GAS   ',098.0 ),
     &    emis_legend( 'NH3          ' ,'NH3         ','GAS   ',017.0 ),
     &    emis_legend( 'NH3_FERT     ' ,'NH3_FERT    ','GAS   ',017.0 ),
     &    emis_legend( 'CO           ' ,'CO          ','GAS   ',028.0 ),
     &    emis_legend( 'PACD         ' ,'RCOOH       ','GAS   ',076.0 ),
     &    emis_legend( 'AACD         ' ,'CCOOH       ','GAS   ',060.1 ),
     &    emis_legend( 'ALD2         ' ,'ALD2        ','GAS   ',044.0 ),
     &    emis_legend( 'FORM         ' ,'FORM        ','GAS   ',030.0 ),
     &    emis_legend( 'HCHO         ' ,'HCHO_PRIMARY','GAS   ',030.0 ),
     &    emis_legend( 'MEOH         ' ,'MEOH        ','GAS   ',032.0 ),
     &    emis_legend( 'FACD         ' ,'HCOOH       ','GAS   ',046.0 ),
     &    emis_legend( 'ALDX         ' ,'ALDX        ','GAS   ',058.1 ),
     &    emis_legend( 'GLYXL        ' ,'GLY         ','GAS   ',058.0 ),
     &    emis_legend( 'GLY          ' ,'GLY         ','GAS   ',058.0 ),
     &    emis_legend( 'MGLY         ' ,'MGLY        ','GAS   ',072.1 ),
     &    emis_legend( 'ETHA         ' ,'ETHA        ','GAS   ',030.1 ),
     &    emis_legend( 'ETH          ' ,'ETH         ','GAS   ',028.0 ),
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
     &    emis_legend( 'PAR          ' ,'PAR         ','GAS   ',014.0 ),
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
      SUBROUTINE INTERPRET_EM_RULE_STREAM( RULE_STREAM0, IRULE, EM_STREAM_RULE )
!
! This subroutine analyzes the part of a rule identifying which streams
! to affect and searches for the individual streams, or, if the rule uses
! the 'ALL' keyword, this routine identifies every stream as impacted.
!-----------------------------------------------------------------------

      USE UTILIO_DEFN


      IMPLICIT NONE

      CHARACTER(32), INTENT( IN ) :: RULE_STREAM0
      CHARACTER(32)               :: RULE_STREAM
      INTEGER, INTENT( IN )       :: IRULE
      LOGICAL, INTENT( INOUT )    :: EM_STREAM_RULE( N_EM_SRM )

      INTEGER ISRM, IDX
      CHARACTER( 200 ) :: XMSG
      CHARACTER( 16 )  :: PNAME = "EMIS_SPC_MAP"
      INTEGER  :: JDATE = 0
      INTEGER  :: JTIME = 0

      ! Initialize the output vector
      EM_STREAM_RULE = .FALSE.
      
      ! First Capitalize the Rule so it is easier to error-check
      RULE_STREAM = RULE_STREAM0
      CALL UPCASE( RULE_STREAM )

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
         ! Find the Specific Stream this Rule Identifies
         IDX = INDEX1( RULE_STREAM, N_EM_SRM, EM_FILE_LAB ) 
         IF ( IDX .NE. 0 ) THEN
           EM_STREAM_RULE( IDX ) = .TRUE.
         ELSE
           WRITE( LOGDEV, * )
           WRITE( LOGDEV, '(/,3A,I3,/,2A,/,2A)' ),
     &         'The Emissions Stream Label (',TRIM( RULE_STREAM ),
     &         ') applied for Rule ',IRULE,' in the Emissions Control ',
     &         'Namelist does not match any of the emissions streams ',
     &         'labels provided to CMAQ. Please check the runscript against ',
     &         'your emissions control inputs.'
           XMSG = 'Error in Emissions Map Processing.'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
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

      USE AERO_DATA, ONLY: ACORSEM_ASO4_FAC, ACORSEM_ANO3_FAC,
     &                     ACORSEM_ACL_FAC,  ACORSEM_AH2O_FAC,
     &                     ACORSEM_RENORM,   POA_OP_VF,
     &                     POA_AMF, POG_AMF, PCVOC_FAC
      USE RXNS_DATA, ONLY: MECHNAME

      IMPLICIT NONE

      INTEGER,         INTENT( INOUT ) :: N_EM_RULE
      TYPE( EM_RULE_REG )              :: EM_NML_B( N_EM_RULE_REG  )

      INTEGER          :: N_EM_RULE_B, IRULE, ISRM

      N_EM_RULE_B = 0
 
      ! Add a rule For Removing Fertilizer Ammonia if BiDi is
      ! activated and the user has requested the correction
         IF ( ABFLUX .AND. BIDI_FERT_NH3 ) THEN
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'NH3'
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'NH3_FERT'
            EM_NML_B(N_EM_RULE_B)%FAC   = -1.0
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MOLE'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a'
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
         END IF

      ! Add rules for moving sulfuric acid emissions from the gas phase
      ! to the aerosol phase
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'SULF'
         EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'SULF'
         EM_NML_B(N_EM_RULE_B)%FAC   = 0.0
         EM_NML_B(N_EM_RULE_B)%BASIS = 'MOLE'
         EM_NML_B(N_EM_RULE_B)%OP    = 'o' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4'
         EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'SULF'
         EM_NML_B(N_EM_RULE_B)%FAC   = 1.0
         EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

      ! Add rules for speciating coarse PM from Anthropogenic Streams
      !  Coarse-Mode Sulfate
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASO4'
         EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_ASO4_FAC, 4 ) 
         EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

      !  Coarse-Mode Nitrate
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ANO3'
         EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_ANO3_FAC, 4 )
         EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

      !  Coarse-Mode Chloride
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ACL'
         EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_ACL_FAC, 4 ) 
         EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'

      !  Coarse-Mode Water
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'AH2O'
         EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_AH2O_FAC, 4 )
         EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
         EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
 
      !  Anthropogenic Coarse-Mode Other
         N_EM_RULE_B = N_EM_RULE_B + 1
         EM_NML_B(N_EM_RULE_B)%SPEC  = 'ACORS'
         EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
         EM_NML_B(N_EM_RULE_B)%PHASE = 'COARSE'
         EM_NML_B(N_EM_RULE_B)%SURR  = 'PMC'
         EM_NML_B(N_EM_RULE_B)%FAC   = REAL( ACORSEM_RENORM, 4 )
         EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
         EM_NML_B(N_EM_RULE_B)%OP    = 'm' 
         EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE'
 
      ! Volatility Basis Set Scaling for Use with Semivolatile POA
      ! Mechanisms
         IF ( INDEX( MECHNAME, 'NVPOA' ) .EQ. 0 ) THEN


            ! VLVPO1 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VLVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(1) * POG_AMF(1)
            ! VLVPO1 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VLVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(1) * POG_AMF(1)

            ! VSVPO1 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VSVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(2) * POG_AMF(2)
            ! VSVPO1 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VSVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(2) * POG_AMF(2)

            ! VSVPO2 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VSVPO2'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(3) * POG_AMF(3)
            ! VSVPO2 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VSVPO2'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(3) * POG_AMF(3)

            ! VSVPO3 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VSVPO3'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(4) * POG_AMF(4)
            ! VSVPO3 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VSVPO3'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(4) * POG_AMF(4)

            ! VIVPO1 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VIVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(5) * POG_AMF(5)
            ! VIVPO1 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'VIVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(5) * POG_AMF(5)

            ! ALVPO1 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ALVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(1) * POA_AMF(1)
            ! ALVPO1 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ALVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(1) * POA_AMF(1)

            ! ASVPO1 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(2) * POA_AMF(2)
            ! ASVPO1 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(2) * POA_AMF(2)

            ! ASVPO2 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASVPO2'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(3) * POA_AMF(3)
            ! ASVPO2 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASVPO2'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(3) * POA_AMF(3)

            ! ASVPO3 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASVPO3'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(4) * POA_AMF(4)
            ! ASVPO3 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'ASVPO3'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(4) * POA_AMF(4)

            ! AIVPO1 <- POC
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'AIVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(5) * POA_AMF(5)
            ! AIVPO1 <- PNCOM
            N_EM_RULE_B = N_EM_RULE_B + 1
            EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
            EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
            EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
            EM_NML_B(N_EM_RULE_B)%SPEC  = 'AIVPO1'
            EM_NML_B(N_EM_RULE_B)%PHASE = 'FINE'
            EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
            EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
            EM_NML_B(N_EM_RULE_B)%FAC   = POA_OP_VF(5) * POA_AMF(5)
          
            IF ( .NOT. ZERO_PCSOA ) THEN
              ! PCVOC <- POC
              N_EM_RULE_B = N_EM_RULE_B + 1
              EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
              EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
              EM_NML_B(N_EM_RULE_B)%SURR  = 'POC'
              EM_NML_B(N_EM_RULE_B)%SPEC  = 'PCVOC'
              EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
              EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
              EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
              EM_NML_B(N_EM_RULE_B)%FAC   = PCVOC_FAC
              ! PCVOC <- PNCOM
              N_EM_RULE_B = N_EM_RULE_B + 1
              EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
              EM_NML_B(N_EM_RULE_B)%STREAM= 'ALL'
              EM_NML_B(N_EM_RULE_B)%SURR  = 'PNCOM'
              EM_NML_B(N_EM_RULE_B)%SPEC  = 'PCVOC'
              EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
              EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
              EM_NML_B(N_EM_RULE_B)%OP    = 'a' 
              EM_NML_B(N_EM_RULE_B)%FAC   = PCVOC_FAC
              ! PCVOC is turned off for Fires
              DO ISRM = 1,N_EM_SRM
                IF ( EM_FILE_FIRE( ISRM ) ) THEN
                  N_EM_RULE_B = N_EM_RULE_B + 1
                  EM_NML_B(N_EM_RULE_B)%REGION= 'EVERYWHERE' 
                  EM_NML_B(N_EM_RULE_B)%STREAM= EM_FILE_LAB( ISRM )
                  EM_NML_B(N_EM_RULE_B)%SURR  = 'ALL'
                  EM_NML_B(N_EM_RULE_B)%SPEC  = 'PCVOC'
                  EM_NML_B(N_EM_RULE_B)%PHASE = 'GAS'
                  EM_NML_B(N_EM_RULE_B)%BASIS = 'MASS'
                  EM_NML_B(N_EM_RULE_B)%OP    = 'o' 
                  EM_NML_B(N_EM_RULE_B)%FAC   = 0.0
                END IF
              END DO
            END IF
         END IF

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
      SUBROUTINE INIT_EMIS_REGIONS( )
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
                                                     
      TYPE( EM_REG      ) :: EM_REG_READ( N_REGIONS_REG )
      INTEGER, PARAMETER :: NFILE0 = 200
      CHARACTER( 32 )    :: FILENAMES( NFILE0 ) = ''
      CHARACTER( 32 ) :: VNAME

      INTEGER :: IRGN, NFILE, IDX, IFILE, IREAD, IVAR
      INTEGER :: GXOFF, GYOFF, ENDCOL, ENDROW, STARTCOL, STARTROW
      INTEGER :: N_EM_READ
      CHARACTER( 16 )    :: PNAME = "INIT_REGIONS"
      CHARACTER( 250)    :: XMSG

      ! Find the total number of regions to be processed
      N_EM_READ = 0  ! The first region is the entire domain
      DO IRGN = 1,N_REGIONS_REG
          IF ( RGN_NML( IRGN )%LABEL .EQ. '' ) EXIT
          N_EM_READ = N_EM_READ + 1
      END DO

      ! Allocate Vectors and Arrays for Each Region
      ALLOCATE( EM_REGIONS( N_REGIONS_REG ) )
      EM_REGIONS( 1 )%LABEL = 'EVERYWHERE'
      EM_REGIONS( 1 )%FILE  = 'N/A'
      EM_REGIONS( 1 )%VAR   = 'N/A'
      EM_REGIONS( 1 )%FILENUM = 1
      N_EM_RGN = 1
      
      ALLOCATE( EM_REG_FAC( NCOLS,NROWS,N_REGIONS_REG ) )
      EM_REG_FAC = 0.0
      EM_REG_FAC( :,:,1 ) = 1.0

      ! Populate global Region properties structure. Also assign each
      ! region a number according to the file it comes from. 1 =
      ! domain-wide.
      NFILE = 1
      FILENAMES( 1 ) = 'N/A'

      IF ( N_EM_READ .GT. 0 ) THEN
        DO IREAD = 1,N_EM_READ
           CALL UPCASE( RGN_NML( IREAD )%LABEL )
           CALL UPCASE( RGN_NML( IREAD )%FILE  )
           CALL UPCASE( RGN_NML( IREAD )%VAR   )

           EM_REG_READ( IREAD )%LABEL = RGN_NML( IREAD )%LABEL  ! Region Name
           EM_REG_READ( IREAD )%FILE  = RGN_NML( IREAD )%FILE   ! Logical filename
           EM_REG_READ( IREAD )%VAR   = RGN_NML( IREAD )%VAR    ! Variable from file 
                                                                !   used to inform mask

           IDX = INDEX1( EM_REG_READ( IREAD )%FILE, NFILE, FILENAMES(1:NFILE) )
           IF ( IDX .NE. 0 ) THEN
             EM_REG_READ( IREAD )%FILENUM = IDX
           ELSE
             NFILE = NFILE + 1
             EM_REG_READ( IREAD )%FILENUM = NFILE
             FILENAMES( NFILE ) = EM_REG_READ( IREAD )%FILE
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
          DO IREAD = 1,N_EM_READ
              IF ( EM_REG_READ( IREAD )%FILENUM .EQ. IFILE ) THEN
                  IF ( EM_REG_READ( IREAD )%VAR .EQ. 'ALL' ) THEN
                     ! Populate the region array with all of the
                     ! variables on this file
                     IF ( EM_REG_READ( IREAD )%LABEL .NE. 'ALL' ) THEN
                        XMSG = 'Error reading Region input in Emissions Control file.'//
     &                         'If the variable name is set to "ALL", then the label must'//
     &                         'also be set to "ALL".'
                        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
                     ELSE   
                        DO IVAR = 1,NVARS3D
                           N_EM_RGN = N_EM_RGN + 1
                           EM_REGIONS( N_EM_RGN )%LABEL   = VNAME3D( IVAR )
                           EM_REGIONS( N_EM_RGN )%VAR     = VNAME3D( IVAR )
                           EM_REGIONS( N_EM_RGN )%FILE    = EM_REG_READ( IREAD )%FILE
                           EM_REGIONS( N_EM_RGN )%FILENUM = EM_REG_READ( IREAD )%FILENUM

                           IF ( .NOT. INTERPX( FILENAMES( IFILE ), VNAME3D(IVAR), 
     &                          PNAME, STARTCOL, ENDCOL, STARTROW, ENDROW, 1, 1,
     &                          SDATE3D, STIME3D, EM_REG_FAC( 1,1,N_EM_RGN ) ) )
     &                          CALL M3ERR( PNAME, SDATE3D, STIME3D,
     &                              'Could not read ' // VNAME //
     &                              'from ' // FILENAMES( IFILE ) // ' file', .TRUE. )
                        END DO
                     END IF
                  ELSE
                     ! Populate the region array with only this variable
                     N_EM_RGN = N_EM_RGN + 1
                     EM_REGIONS( N_EM_RGN ) = EM_REG_READ( IREAD )
                     VNAME = EM_REG_READ( IREAD )%VAR

                     IF ( .NOT. INTERPX( FILENAMES( IFILE ), VNAME, PNAME,
     &                    STARTCOL, ENDCOL, STARTROW, ENDROW, 1, 1,
     &                    SDATE3D, STIME3D, EM_REG_FAC( 1,1,N_EM_RGN ) ) )
     &                    CALL M3ERR( PNAME, SDATE3D, STIME3D,
     &                        'Could not read ' // VNAME //
     &                        'from ' // FILENAMES( IFILE ) // ' file', .TRUE. )
                  END IF
              END IF
          END DO
          EM_REGIONS = EM_REGIONS( 1:N_EM_RGN )
          EM_REG_FAC = EM_REG_FAC( :,:,1:N_EM_RGN )

          ! Close Regions File
          IF ( .NOT. CLOSE3( FILENAMES( IFILE ) ) ) THEN
            XMSG = 'Could not close ' // FILENAMES( IFILE )
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
          END IF
         
          ! Error Check the Regions Array
          ! Any Negatives?
          DO IRGN = 1,N_EM_RGN
            IF ( ANY( EM_REG_FAC( :,:,IRGN ) .LT. 0.0 ) ) THEN
               XMSG = 'Region ' // TRIM( EM_REGIONS( IRGN )%LABEL) // ' on file ' //
     &                TRIM( FILENAMES( IFILE )) // ' contains a ' //
     &                'negative value. The expected range is 0-1.'
               CALL M3ERR( PNAME, STDATE, STTIME, XMSG, .TRUE. )
            ELSE IF ( ANY( EM_REG_FAC( :,:,IRGN ) .GT. 1.01 ) ) THEN
               XMSG = 'Region ' // TRIM( EM_REGIONS( IRGN )%LABEL) // ' on file ' //
     &                TRIM( FILENAMES( IFILE )) // ' contains a ' //
     &                'value greater than 1. The expected range is 0-1.'
               CALL M3ERR( PNAME, STDATE, STTIME, XMSG, .TRUE. )
            END IF
          END DO

        END DO ! IFILE
      END IF
      
      END SUBROUTINE INIT_EMIS_REGIONS


!-----------------------------------------------------------------------
      SUBROUTINE READ_EMISS_NAMELIST( )
!
!     This subroutine opens and reads the Emissions Control Namelist. It 
!     attempts to deal with errors like missing file or missing file 
!     sections by error checking and setting defaults.
!-----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE

         ! Define Dummy Variables for Opening Emission Control Namelist
         CHARACTER( 300 ) :: EQNAME
         INTEGER          :: EMCTRL_NML
         INTEGER          :: STAT

         ! Define General Parameters in Emission Control Namelist
         NAMELIST / GeneralSpecs / DefaultScaling, Guard_BiogenicVOC,
     &              Guard_MarineGas, Guard_LightningNO, Guard_WindBlownDust,
     &              Guard_SeaSpray

         ! Define Emissions Rules Input from Emissions Control Namelist
         NAMELIST / EmissionScalingRules / EM_NML
         NAMELIST / SizeDistributions    / SD_NML 
         NAMELIST / RegionsRegistry      / RGN_NML

         ! Allocate and Initialize Namelist Variables
         ALLOCATE( EM_NML( N_EM_RULE_REG ) )
         EM_NML%REGION = ''
         EM_NML%STREAM = ''
         EM_NML%SURR   = ''
         EM_NML%SPEC   = ''
         EM_NML%PHASE  = ''
         EM_NML%OP     = ''
         EM_NML%BASIS  = ''
         EM_NML%FAC    = 0.

         ALLOCATE( SD_NML( N_SD_REG ) )
         SD_NML%STREAM   = ''
         SD_NML%MODE     = ''
         SD_NML%MODE_REF = ''

         ALLOCATE( RGN_NML( N_REGIONS_REG ) )
         RGN_NML%LABEL = ''
         RGN_NML%FILE  = ''
         RGN_NML%VAR   = ''

         ! Retrieve the Name of the Emission Control File
         IF ( EMISSCTRL .EQ. "EMISSCTRL_NML" ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)"),
     &              'You have chosen not to indicate the location of an',
     &              'Emission Control namelist file. Default settings ',
     &              'will be assumed.'
             DefaultScaling      = .TRUE.
             Guard_BiogenicVOC   = .FALSE.
             Guard_MarineGas     = .FALSE.
             Guard_LightningNO   = .FALSE.
             Guard_WindBlownDust = .FALSE.
             Guard_SeaSpray      = .FALSE.
             RETURN
         END IF

         ! Open Emission Control Namelist File
         EMCTRL_NML = JUNIT()
         OPEN( FILE = EMISSCTRL, UNIT = EMCTRL_NML, STATUS = 'OLD',
     &         POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

         ! Check for Error in File Open Process
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, '(5x,A,A,/,8x,A)' ),'ERROR: Could not read ',
     &              'emissions control namelist file: ',TRIM( EQNAME )
             CALL EXIT(1)
         END IF
         
         ! Read General Specification Section
         READ( NML = GeneralSpecs, UNIT = EMCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &              'Warning! Something went wrong while reading the ',
     &              'GeneralSpecs section of the Emissions Control ',
     &              'Namelist. Default values for this section will be ',
     &              'assumed.'
             DefaultScaling      = .TRUE.
             Guard_BiogenicVOC   = .FALSE.
             Guard_MarineGas     = .FALSE.
             Guard_LightningNO   = .FALSE.
             Guard_WindBlownDust = .FALSE.
             Guard_SeaSpray      = .FALSE.
         END IF

         ! Read the Regions Registry
         REWIND( EMCTRL_NML )
         READ( NML = RegionsRegistry, UNIT = EMCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &              'Warning! Something went wrong while reading the ',
     &              'Regions_Registry section of the Emissions Control ',
     &              'Namelist. Default values for this section will be ',
     &              'assumed.'
             RGN_NML%LABEL = ''
             RGN_NML%FILE  = ''
             RGN_NML%VAR   = ''
         END IF

         ! Read the size distribution specification section
         REWIND( EMCTRL_NML )
         READ( NML = SizeDistributions, UNIT = EMCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &              'Warning! Something went wrong while reading the ',
     &              'SizeDistReg section of the Emissions Control ',
     &              'Namelist. Default values for this section will be ',
     &              'assumed.'
             SD_NML%STREAM   = ''
             SD_NML%MODE     = ''
             SD_NML%MODE_REF = ''
         END IF

         ! Read the Emissions Rules to inform scaling operations
         REWIND( EMCTRL_NML )
         READ( NML = EmissionScalingRules, UNIT = EMCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
             WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &              'Warning! Something went wrong while reading the ',
     &              'EmissionScaling section of the Emissions Control ',
     &              'Namelist. Default values for this section will be ',
     &              'assumed.'
             EM_NML%REGION = ''
             EM_NML%STREAM = ''
             EM_NML%SURR   = ''
             EM_NML%SPEC   = ''
             EM_NML%PHASE  = ''
             EM_NML%OP     = ''
             EM_NML%BASIS  = ''
             EM_NML%FAC    = 0.
         END IF
 
         CLOSE( UNIT = EMCTRL_NML )

      END SUBROUTINE READ_EMISS_NAMELIST  

      END MODULE EMIS_VARS
