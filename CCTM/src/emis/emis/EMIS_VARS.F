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
      REAL,    ALLOCATABLE :: CELLAREA( :,: )
      REAL,    ALLOCATABLE :: CELLHGT ( : )
      REAL,    ALLOCATABLE :: CELLVOL ( :,:,: )
      REAL,    ALLOCATABLE :: VDEMIS_DIFF( :,:,:,: ) ! emissions array mapped to diffused species

      INTEGER              :: EMLAYS

      CHARACTER( 16 ), ALLOCATABLE :: EM_SPEC  ( : ) ! Vector of Internal CMAQ Species names for 
                                                     ! each emission operation
      CHARACTER( 16 ), ALLOCATABLE :: EM_SURR( :,: ) ! Emissions Surrogate to be read ( species, stream )
      
      CHARACTER( 16 ), ALLOCATABLE :: TRACSPC( : )   ! Variables from Tracer Emission File
      CHARACTER( 16 ), ALLOCATABLE :: AREASPC( : )   ! Variables straight from the Area Emission File

      TYPE EM_SURR_TYPE
          INTEGER         :: LEN    ! Length of Sub-Vector
          CHARACTER( 16 ), ALLOCATABLE :: ARRY( : )    ! Surrogate Name
          CHARACTER( 16 ), ALLOCATABLE :: UNITS( : )   ! Units determined from input files
          REAL,            ALLOCATABLE :: MW( : )      ! Default MW from SPECIATE/SMOKE/MOVE
          LOGICAL,         ALLOCATABLE :: USED( : )    ! Whether or not the surrogate is used in CMAQ
          Real,            ALLOCATABLE :: CONV( : )    ! Conversion factor to account for kg -> g or hr -> s
          CHARACTER( 16 ), ALLOCATABLE :: BASIS( : )   ! 'MOLE', 'MASS', or 'UNKNOWN' 
          LOGICAL,         ALLOCATABLE :: LAREA( : )   ! Whether or not the variable is an area flux
          LOGICAL,         ALLOCATABLE :: LAREAADJ( : )! Whether or not the emissions should be adjusted 
                                                       !    by the map scale factor
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
          LOGICAL, ALLOCATABLE :: AREA(:) ! True if the emission is a flux that needs to be multiplied 
                                          !   by the grid cell area
          LOGICAL, ALLOCATABLE :: AREAADJ(:) ! True if the emissions need to be adjusted by the map scale
                                             !   factor to convert from real space to projected space
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
      CHARACTER( 32  ),ALLOCATABLE :: EM_FILE_LAB ( : )  ! Nickname of each emissions stream
      CHARACTER( 16  ),ALLOCATABLE :: EM_FILE_TYPE( : )  ! shortname for the type of each emission
      INTEGER,         ALLOCATABLE :: EM_FILE_ITYPE( : ) ! shortname for the type of each emission
      CHARACTER( 100 ),ALLOCATABLE :: EM_FILE_DESC( : )  ! Description of each emissions stream
      LOGICAL,         ALLOCATABLE :: EM_FILE_LAPPLY( : )! Flag to use each emissions stream
      LOGICAL         ,ALLOCATABLE :: EM_FILE_DIFF( :,: )! Test for whether or not a stream contributes to 
                                                         !   a particular transported variable
      LOGICAL         ,ALLOCATABLE :: EM_FILE_SYM_DATE( : ) ! Toggle for whether or not to let the 
                                                             ! emission stream date override the model
      INTEGER         ,ALLOCATABLE :: EM_FILE_DATE( : )! Date to be read in from each emissions stream
      LOGICAL         ,ALLOCATABLE :: EM_FILE_FIRE( : )! Is this Emission Stream Representing Fires (1=Yes)

      INTEGER                      :: N_EMDIAG
      INTEGER         ,ALLOCATABLE :: EMDIAG_NSTREAM( : )       ! Number of Streams for each diagnostic file
      LOGICAL         ,ALLOCATABLE :: EMDIAG_STREAM_MASK( :,: ) ! True if a stream contributes to a diagnostic
      CHARACTER( 16 ) ,ALLOCATABLE :: EMDIAG_LOGICAL( : )       ! I/O Logical Name for each diagnostic file
      CHARACTER( 6 )  ,ALLOCATABLE :: EMDIAG_FORMAT( : )        ! FALSE/TRUE/2D/3D/2DCOL
      CHARACTER( 300 ),ALLOCATABLE :: EMDIAG_FILENAME( : )      ! Filename for diagnostic
      CHARACTER( 32 ) ,ALLOCATABLE :: EMDIAG_LAB( : )           ! Shorthand Label for Diagnostic
      INTEGER         ,ALLOCATABLE :: EMDIAG_LAYS( : )          ! Layers for each diagnostic file
      INTEGER         ,ALLOCATABLE :: SUMDIAG( : )
      REAL            ,ALLOCATABLE :: VDEMIS_EMDIAG( :,:,:,: )  ! Rates for aggregate diagnostic files

      TYPE EMDIAG_SPEC_STRUCT
         INTEGER                   :: NSPEC
         CHARACTER(16),ALLOCATABLE :: SPEC( : )
         CHARACTER(16),ALLOCATABLE :: UNITS( : )
         INTEGER                   :: NPAIRS
         INTEGER      ,ALLOCATABLE :: MAP_toDIFF( : )
         INTEGER      ,ALLOCATABLE :: MAP_toEMDIAG( : )
      END TYPE EMDIAG_SPEC_STRUCT
      TYPE( EMDIAG_SPEC_STRUCT ), ALLOCATABLE :: EMDIAG_SPEC( : )

      INTEGER         ,ALLOCATABLE :: MAP_EMDIAGtoVDEMIS( :,: )

      INTEGER              :: IBIOSRM, IMIOGSRM, IMGSRM, ILTSRM, ISEASRM, IDUSTSRM
      INTEGER, ALLOCATABLE :: IGSRM( : ), IPSRM( : ), ITSRM( : ), MAP_PTtoISRM( : )

      INTEGER, ALLOCATABLE :: MAP_EMtoSURR( :,: ), MAP_EMtoDIFF( : ), MAP_EMtoGAS( : )

      ! Create Lookup table of Molecular Weight for likely emission
      ! surrogate species. These values are used to compute mole <-> mass
      ! conversions if requested. 
      Type emis_legend
          Character( 16 ) :: NAME  ! Emissions Surrogate Species
          REAL            :: MW    ! Emissions Surrogate Molecular Weight [g mol-1]
      End Type emis_legend
      
      Integer, Parameter :: N_EMIS_SURR_TABLE = 162
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
     &    emis_legend( 'PHGI         ' ,200.5 ),
     &    emis_legend( 'PMNCOMN2     ' ,220.0 ), ! CRACMM/ae8, POC value
     &    emis_legend( 'PMOCN2       ' ,220.0 ), ! CRACMM/ae8, PNCOM value
     &    emis_legend( 'PNCOMN2      ' ,506.99), ! CRACMM/ae8, based on ALK equivalent C*
     &    emis_legend( 'POCN2        ' ,506.99), ! CRACMM/ae8, "
     &    emis_legend( 'PNCOMN1      ' ,408.80), ! CRACMM/ae8, "
     &    emis_legend( 'POCN1        ' ,408.80), ! CRACMM/ae8, "
     &    emis_legend( 'PNCOMP0      ' ,394.77), ! CRACMM/ae8, "
     &    emis_legend( 'POCP0        ' ,394.77), ! CRACMM/ae8, "
     &    emis_legend( 'PNCOMP1      ' ,380.75), ! CRACMM/ae8, "
     &    emis_legend( 'POCP1        ' ,380.75), ! CRACMM/ae8, "
     &    emis_legend( 'PNCOMP2      ' ,338.66), ! CRACMM/ae8, "
     &    emis_legend( 'POCP2        ' ,338.66)/)! CRACMM/ae8, "
   
      END MODULE EMIS_VARS
