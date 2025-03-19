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
      module desid_vars

!-----------------------------------------------------------------------
! Function: Define global variables used for emissions processor

! Revision History:
!     09 Nov 2017 B.Murphy: Extracted these variables from EMIS_DEFN
!-----------------------------------------------------------------------
      use runtime_vars
      use desid_param_module

      IMPLICIT NONE

      SAVE
      REAL,    ALLOCATABLE :: CELLAREA( :,: )
      REAL,    ALLOCATABLE :: CELLHGT ( :,:,: )
      REAL,    ALLOCATABLE :: CELLVOL ( :,:,: )
      REAL                 :: DX1, DX2
      REAL,    ALLOCATABLE :: VDEMIS_DIFF( :,:,:,: ) ! emissions array mapped to diffused species

      INTEGER              :: DESID_LAYS

      CHARACTER( 16 ), ALLOCATABLE :: LOCAL_SPEC( : )   ! Vector of Internal CMAQ Species names for each 
                                                        ! emission rule
      CHARACTER( 16 ), ALLOCATABLE :: LOCAL_EMVAR( :,: )! Emissions Variable to be read ( species, stream )

      TYPE DESID_EMVAR_TYPE
          INTEGER         :: LEN    ! Length of Sub-Vector
          CHARACTER( 16 ), ALLOCATABLE :: ARRY( : )    ! Emission Variable Name
          CHARACTER( 16 ), ALLOCATABLE :: UNITS( : )   ! Units determined from input files
          REAL,            ALLOCATABLE :: MW( : )      ! Default MW from SPECIATE/SMOKE/MOVE
          LOGICAL,         ALLOCATABLE :: USED( : )    ! Whether or not the variable is used in CMAQ
          Real,            ALLOCATABLE :: CONV( : )    ! Conversion factor to account for kg -> g or hr -> s
          CHARACTER( 16 ), ALLOCATABLE :: BASIS( : )   ! 'MOLE', 'MASS', or 'UNKNOWN' 
          LOGICAL,         ALLOCATABLE :: LAREA( : )   ! Whether or not the variable is an area flux
          LOGICAL,         ALLOCATABLE :: LAREAADJ( : )! Whether or not the emissions should be adjusted 
                                                       !    by the map scale factor
      END TYPE DESID_EMVAR_TYPE
      TYPE( DESID_EMVAR_TYPE ),ALLOCATABLE :: DESID_EMVAR( : )! Species to be read from each stream file

      TYPE DESID_FAC_TYPE
          INTEGER              :: NFAC    ! Number of scaling instructions that apply to this element of 
                                          !   the EM_FAC_ST structure. The other attributes will be of 
                                          !   length LEN.
          INTEGER              :: NREG    ! Number of unique regions for each instruction stack
          REAL, ALLOCATABLE    :: FAC(:)  ! Scale Factor applied with potential aerosol splitting
          REAL, ALLOCATABLE    :: BULK(:) ! Scale Factor for bulk emission without
                                          !   splitting for aerosols
          REAL, ALLOCATABLE    :: BASIS(:)! Conversion factor to account for mole or mass basis
          LOGICAL, ALLOCATABLE :: AREA(:) ! True if the emission is a flux that needs to be multiplied 
                                          !   by the grid cell area
          LOGICAL, ALLOCATABLE :: AREAADJ(:) ! True if the emissions need to be adjusted by the map scale
                                             !   factor to convert from real space to projected space
          INTEGER, ALLOCATABLE :: REG(:)  ! Index Mapping this entry to a particlar region in EM_REGIONS
          INTEGER, ALLOCATABLE :: REG_UNQ(:)    ! Record of Unique Regions for each instruction stack
          INTEGER, ALLOCATABLE :: REG_RMDR(:)   ! Map to remainder map
          INTEGER, ALLOCATABLE :: OP(:)   ! Operation to perform for this scaling
                                          !   (addition,'a' = 1, 
                                          !    multiplication,'m' = 2, 
                                          !    overwrite,'o' = 3)
      END TYPE DESID_FAC_TYPE
      TYPE( DESID_FAC_TYPE ), ALLOCATABLE :: DESID_FAC ( :,: ) ! Emissions Scale Factors ( species, stream )
      CHARACTER( 1 ) :: DESID_OP_NAME( 3 ) = (/'a','m','o'/)
      
      ! Define Region Remainder Mask Structure
      TYPE DESID_REG_RMDR_TYPE
          INTEGER              :: REG        ! large region with variable number of subset regions
          LOGICAL, ALLOCATABLE :: SUB(:)     ! If an element true, then that subset region is active in 
                                             !   the same instruction as the large region
          REAL, ALLOCATABLE    :: MASK(:,:)  ! Remainder of the large region minus all subset regions in SUB
      END TYPE DESID_REG_RMDR_TYPE
      TYPE( DESID_REG_RMDR_TYPE ),ALLOCATABLE :: DESID_REG_RMDR( : ) ! Structure of Masks defining the remainder of 
                                                                     ! regions after subtracting their subset regions

      ! Define Structure for Holding Emissions Aerosol Mode Information Globally
      TYPE DESID_STREAM_AERO_TYPE
         INTEGER  :: LEN
         INTEGER,         ALLOCATABLE :: REF( : )
         CHARACTER( 16 ), ALLOCATABLE :: NAME( : )
         REAL,            ALLOCATABLE :: FACNUM( :,: )
         REAL,            ALLOCATABLE :: FACSRF( :,: )
      END TYPE DESID_STREAM_AERO_TYPE
      TYPE( DESID_STREAM_AERO_TYPE ), ALLOCATABLE :: DESID_STREAM_AERO( : )
 
      ! Define Emissions File Attribute Variables
      CHARACTER( 200 ),ALLOCATABLE :: DESID_STREAM_NAME( : )  ! Filename of each emissions stream
      CHARACTER( 32  ),ALLOCATABLE :: DESID_STREAM_LAB ( : )  ! Nickname of each emissions stream
      CHARACTER( 16  ),ALLOCATABLE :: DESID_STREAM_TYPE( : )  ! shortname for the type of each emission
      INTEGER,         ALLOCATABLE :: DESID_STREAM_ITYPE( : ) ! shortname for the type of each emission
      CHARACTER( 100 ),ALLOCATABLE :: DESID_STREAM_DESC( : )  ! Description of each emissions stream
      LOGICAL,         ALLOCATABLE :: DESID_STREAM_LAPPLY( : )! Flag to use each emissions stream
      LOGICAL         ,ALLOCATABLE :: DESID_STREAM_DIFF( :,: )! Test for whether or not a stream contributes to 
                                                         !   a particular transported variable
      LOGICAL         ,ALLOCATABLE :: DESID_STREAM_SYM_DATE( : ) ! Toggle for whether or not to let the 
                                                             ! emission stream date override the model
      INTEGER         ,ALLOCATABLE :: DESID_STREAM_DATE( : )! Date to be read in from each emissions stream
      LOGICAL         ,ALLOCATABLE :: DESID_STREAM_FIRE( : )! Is this Emission Stream Representing Fires (1=Yes)

      INTEGER         ,ALLOCATABLE :: DESID_DIAG_N_STREAM( : )    ! Number of Streams for each diagnostic file
      LOGICAL         ,ALLOCATABLE :: DESID_DIAG_STREAM_MASK(:,:) ! True if a stream contributes to a diagnostic
      CHARACTER( 16 ) ,ALLOCATABLE :: DESID_DIAG_LOGICAL( : )     ! I/O Logical Name for each diagnostic file
      CHARACTER( 6 )  ,ALLOCATABLE :: DESID_DIAG_FORMAT( : )      ! FALSE/TRUE/2D/3D/2DCOL
      CHARACTER( 300 ),ALLOCATABLE :: DESID_DIAG_FILENAME( : )    ! Filename for diagnostic
      CHARACTER( 32 ) ,ALLOCATABLE :: DESID_DIAG_LAB( : )         ! Shorthand Label for Diagnostic
      INTEGER         ,ALLOCATABLE :: DESID_DIAG_LAYS( : )        ! Layers for each diagnostic file
      INTEGER         ,ALLOCATABLE :: DESID_DIAG_SUM( : )
      REAL            ,ALLOCATABLE :: VDEMIS_DIAG( :,:,:,: )      ! Rates for aggregate diagnostic files

      TYPE DESID_DIAG_SPEC_TYPE
         INTEGER                   :: NSPEC
         CHARACTER(16),ALLOCATABLE :: SPEC( : )
         CHARACTER(16),ALLOCATABLE :: UNITS( : )
         INTEGER                   :: NPAIRS
         INTEGER      ,ALLOCATABLE :: MAP_toDIFF( : )
         INTEGER      ,ALLOCATABLE :: MAP_toDIAG( : )
      END TYPE DESID_DIAG_SPEC_TYPE
      TYPE( DESID_DIAG_SPEC_TYPE ), ALLOCATABLE :: DESID_DIAG_SPEC( : )
      TYPE( DESID_DIAG_SPEC_TYPE ), ALLOCATABLE :: DESID_DIAG_SPEC_BUFF( : )

      INTEGER         ,ALLOCATABLE :: Map_DIAGtoVDEMIS( :,: )

      INTEGER              :: IBIOSRM, IMIOGSRM, IMGSRM, ILTSRM, ISEASRM, IDUSTSRM
      INTEGER, ALLOCATABLE :: IGSRM( : ), IPSRM( : ), ITSRM( : ), MAP_PTtoISRM( : )

      INTEGER, ALLOCATABLE :: Map_ISTRtoEMVAR( :,: ), Map_ISTRtoDIFF( : ), Map_ISTRtoGAS( : )

      ! Create Lookup table of Molecular Weight for likely emission
      ! variable species. These values are used to compute mole <-> mass
      ! conversions if requested. This is a comprehensive list of all
      ! possible emitted species across all chemical mechanisms
      ! supported by CMAQ.
      Type desid_legend
          Character( 16 ) :: NAME  ! Emissions Variable Species
          REAL            :: MW    ! Emissions Variable Molecular Weight [g mol-1]
      End Type desid_legend
      
      Integer, Parameter :: Desid_N_Emvar_Table = 208
      Type( desid_legend ), save :: Desid_Emvar_Table( Desid_N_Emvar_Table)

      CONTAINS

        SUBROUTINE INIT_DESID_EMVAR_MW()

                                                                        ! CB SAPRC  CRACMM  
        ! Inorganic Gases
        Desid_Emvar_Table(  1) = desid_legend( 'CH4          ' ,016.0 ) ! x
        Desid_Emvar_Table(  2) = desid_legend( 'CL2          ' ,071.0 ) ! x    x
        Desid_Emvar_Table(  3) = desid_legend( 'CO           ' ,028.0 ) ! x    x      x
        Desid_Emvar_Table(  4) = desid_legend( 'HCL          ' ,036.5 ) ! x    x      x
        Desid_Emvar_Table(  5) = desid_legend( 'HONO         ' ,047.0 ) ! x    x      x
        Desid_Emvar_Table(  6) = desid_legend( 'NH3          ' ,017.0 ) ! x    x      x
        Desid_Emvar_Table(  7) = desid_legend( 'NH3_FERT     ' ,017.0 ) ! x    x      x
        Desid_Emvar_Table(  8) = desid_legend( 'NO           ' ,030.0 ) ! x    x      x
        Desid_Emvar_Table(  9) = desid_legend( 'NO2          ' ,046.0 ) ! x    x      x
        Desid_Emvar_Table( 10) = desid_legend( 'SO2          ' ,064.0 ) ! x    x      x
        Desid_Emvar_Table( 11) = desid_legend( 'SULF         ' ,098.0 ) ! x    x      x

        ! Organic Gases
        Desid_Emvar_Table( 12) = desid_legend( '13BDE        ' ,054.1 ) !
        Desid_Emvar_Table( 13) = desid_legend( 'AACD         ' ,060.1 ) ! x    x
        Desid_Emvar_Table( 14) = desid_legend( 'ACD          ' ,044.0 ) !             x
        Desid_Emvar_Table( 15) = desid_legend( 'ACE          ' ,026.0 ) !             x
        Desid_Emvar_Table( 16) = desid_legend( 'ACET         ' ,058.1 ) ! x    x
        Desid_Emvar_Table( 17) = desid_legend( 'ACETONE      ' ,058.1 ) !      x
        Desid_Emvar_Table( 18) = desid_legend( 'ACRO         ' ,056.1 ) !      x      x
        Desid_Emvar_Table( 19) = desid_legend( 'ACROLEIN     ' ,056.1 ) ! x    x
        Desid_Emvar_Table( 20) = desid_legend( 'ACT          ' ,058.0 ) !             x
        Desid_Emvar_Table( 21) = desid_legend( 'ACYE         ' ,026.0 ) !      x
        Desid_Emvar_Table( 22) = desid_legend( 'ALD          ' ,058.0 ) !             x
        Desid_Emvar_Table( 23) = desid_legend( 'ALD2         ' ,044.0 ) ! x
        Desid_Emvar_Table( 24) = desid_legend( 'ALD2_PRIMARY ' ,044.0 ) ! x
        Desid_Emvar_Table( 25) = desid_legend( 'ALDX         ' ,058.1 ) ! x
        Desid_Emvar_Table( 26) = desid_legend( 'ALK1         ' ,030.1 ) !      x
        Desid_Emvar_Table( 27) = desid_legend( 'ALK2         ' ,036.7 ) !      x
        Desid_Emvar_Table( 28) = desid_legend( 'ALK3         ' ,058.6 ) !      x
        Desid_Emvar_Table( 29) = desid_legend( 'ALK4         ' ,077.6 ) !      x
        Desid_Emvar_Table( 30) = desid_legend( 'ALK5         ' ,118.9 ) !      x
        Desid_Emvar_Table( 31) = desid_legend( 'API          ' ,136.4 ) !             x
        Desid_Emvar_Table( 32) = desid_legend( 'APIN         ' ,136.2 ) ! x    x
        Desid_Emvar_Table( 33) = desid_legend( 'ARO1         ' ,095.2 ) !      x
        Desid_Emvar_Table( 34) = desid_legend( 'ARO2MN       ' ,118.7 ) !      x
        Desid_Emvar_Table( 35) = desid_legend( 'B124         ' ,120.2 ) !      x
        Desid_Emvar_Table( 36) = desid_legend( 'BACL         ' ,086.1 ) !      x
        Desid_Emvar_Table( 37) = desid_legend( 'BALD         ' ,106.0 ) !      x      x
        Desid_Emvar_Table( 38) = desid_legend( 'BDE13        ' ,054.1 ) !      x      x
        Desid_Emvar_Table( 39) = desid_legend( 'BEN          ' ,078.1 ) !             x
        Desid_Emvar_Table( 40) = desid_legend( 'BENZ         ' ,078.1 ) ! x    x
        Desid_Emvar_Table( 41) = desid_legend( 'BENZENE      ' ,078.1 ) ! x
        Desid_Emvar_Table( 42) = desid_legend( 'BUTADIENE13  ' ,054.0 ) ! x
        Desid_Emvar_Table( 43) = desid_legend( 'CCHO         ' ,044.1 ) !      x
        Desid_Emvar_Table( 44) = desid_legend( 'CCOOH        ' ,060.05) !      x
        Desid_Emvar_Table( 45) = desid_legend( 'CRES         ' ,108.1 ) ! x    x
        Desid_Emvar_Table( 46) = desid_legend( 'CSL          ' ,136.2 ) !             x
        Desid_Emvar_Table( 47) = desid_legend( 'DCB1         ' ,098.0 ) !             x
        Desid_Emvar_Table( 48) = desid_legend( 'DIEN         ' ,054.0 ) !             x
        Desid_Emvar_Table( 49) = desid_legend( 'ETE          ' ,028.1 ) !             x
        Desid_Emvar_Table( 50) = desid_legend( 'ETEG         ' ,062.1 ) !             x
        Desid_Emvar_Table( 51) = desid_legend( 'ETHA         ' ,030.1 ) ! x
        Desid_Emvar_Table( 52) = desid_legend( 'ETH          ' ,030.1 ) ! x           x
        Desid_Emvar_Table( 53) = desid_legend( 'ETHE         ' ,028.1 ) !      x
        Desid_Emvar_Table( 54) = desid_legend( 'ETHENE       ' ,028.1 ) !      x
        Desid_Emvar_Table( 55) = desid_legend( 'ETHY         ' ,026.0 ) ! x
        Desid_Emvar_Table( 56) = desid_legend( 'ETOH         ' ,046.1 ) ! x    x
        Desid_Emvar_Table( 57) = desid_legend( 'EOH          ' ,046.1 ) !             x
        Desid_Emvar_Table( 58) = desid_legend( 'FACD         ' ,046.0 ) ! x    x
        Desid_Emvar_Table( 59) = desid_legend( 'FORM         ' ,030.0 ) ! x
        Desid_Emvar_Table( 60) = desid_legend( 'FORM_PRIMARY ' ,030.0 ) ! x
        Desid_Emvar_Table( 61) = desid_legend( 'FURAN        ' ,096.1 ) !             x
        Desid_Emvar_Table( 62) = desid_legend( 'GLY          ' ,058.0 ) ! x    x      x
        Desid_Emvar_Table( 63) = desid_legend( 'HC3          ' ,044.1 ) !             x
        Desid_Emvar_Table( 64) = desid_legend( 'HC5          ' ,072.1 ) !             x
        Desid_Emvar_Table( 65) = desid_legend( 'HC8          ' ,114.0 ) !             x
        Desid_Emvar_Table( 66) = desid_legend( 'HC10         ' ,142.28) !             x
        Desid_Emvar_Table( 67) = desid_legend( 'HCHO         ' ,030.0 ) !      x      x
        Desid_Emvar_Table( 68) = desid_legend( 'HCOOH        ' ,046.03) !      x
        Desid_Emvar_Table( 69) = desid_legend( 'HKET         ' ,074.0 ) !             x
        Desid_Emvar_Table( 70) = desid_legend( 'IOLE         ' ,056.1 ) ! x
        Desid_Emvar_Table( 71) = desid_legend( 'IPRD         ' ,100.1 ) !      x
        Desid_Emvar_Table( 72) = desid_legend( 'ISO          ' ,068.1 ) !             x
        Desid_Emvar_Table( 73) = desid_legend( 'ISOP         ' ,068.1 ) ! x    x
        Desid_Emvar_Table( 74) = desid_legend( 'ISOPRENE     ' ,068.1 ) !      x
        Desid_Emvar_Table( 75) = desid_legend( 'KET          ' ,086.0 ) ! x           x
        Desid_Emvar_Table( 76) = desid_legend( 'LIM          ' ,136.3 ) !             x
        Desid_Emvar_Table( 77) = desid_legend( 'MACR         ' ,070.0 ) !      x      x
        Desid_Emvar_Table( 78) = desid_legend( 'MCT          ' ,124.1 ) !             x
        Desid_Emvar_Table( 79) = desid_legend( 'MEK          ' ,072.1 ) !      x      x
        Desid_Emvar_Table( 80) = desid_legend( 'MEOH         ' ,032.0 ) ! x    x
        Desid_Emvar_Table( 81) = desid_legend( 'MGLY         ' ,072.0 ) ! x    x      x
        Desid_Emvar_Table( 82) = desid_legend( 'MOH          ' ,032.0 ) !             x
        Desid_Emvar_Table( 83) = desid_legend( 'MVK          ' ,070.1 ) !      x      x
        Desid_Emvar_Table( 84) = desid_legend( 'MXYL         ' ,106.2 ) !      x
        Desid_Emvar_Table( 85) = desid_legend( 'NAPH         ' ,128.17) ! x           x
        Desid_Emvar_Table( 86) = desid_legend( 'NAPHTHALENE  ' ,128.17) !      x
        Desid_Emvar_Table( 87) = desid_legend( 'OLE          ' ,042.1 ) ! x
        Desid_Emvar_Table( 88) = desid_legend( 'OLE1         ' ,072.3 ) !      x
        Desid_Emvar_Table( 89) = desid_legend( 'OLE2         ' ,075.8 ) !      x
        Desid_Emvar_Table( 90) = desid_legend( 'OLI          ' ,070.1 ) !             x
        Desid_Emvar_Table( 91) = desid_legend( 'OLT          ' ,042.0 ) !             x
        Desid_Emvar_Table( 92) = desid_legend( 'ONIT         ' ,119.0 ) !             x
        Desid_Emvar_Table( 93) = desid_legend( 'ORA1         ' ,046.0 ) !             x
        Desid_Emvar_Table( 94) = desid_legend( 'ORA2         ' ,060.2 ) !             x
        Desid_Emvar_Table( 95) = desid_legend( 'OXYL         ' ,106.2 ) !      x
        Desid_Emvar_Table( 96) = desid_legend( 'PACD         ' ,076.0 ) ! x    x
        Desid_Emvar_Table( 97) = desid_legend( 'PAR          ' ,014.0 ) ! x
        Desid_Emvar_Table( 98) = desid_legend( 'PHEN         ' ,110.1 ) !             x
        Desid_Emvar_Table( 99) = desid_legend( 'PRD2         ' ,116.2 ) !      x
        Desid_Emvar_Table(100) = desid_legend( 'PROG         ' ,076.1 ) !             x
        Desid_Emvar_Table(101) = desid_legend( 'PROPENE      ' ,042.1 ) !      x
        Desid_Emvar_Table(102) = desid_legend( 'PRPA         ' ,044.1 ) ! x
        Desid_Emvar_Table(103) = desid_legend( 'PRPE         ' ,042.1 ) !      x
        Desid_Emvar_Table(104) = desid_legend( 'PXYL         ' ,106.2 ) !      x
        Desid_Emvar_Table(105) = desid_legend( 'RCHO         ' ,058.1 ) !      x
        Desid_Emvar_Table(106) = desid_legend( 'RNO3         ' ,147.2 ) !      x
        Desid_Emvar_Table(107) = desid_legend( 'ROH          ' ,060.0 ) !             x
        Desid_Emvar_Table(108) = desid_legend( 'SESQ         ' ,204.4 ) ! x    x      x
        Desid_Emvar_Table(109) = desid_legend( 'SLOWROC      ' ,075.4 ) !             x      
        Desid_Emvar_Table(110) = desid_legend( 'SOAALK       ' ,112.0 ) ! x    x
        Desid_Emvar_Table(111) = desid_legend( 'TERP         ' ,136.2 ) ! x    x
        Desid_Emvar_Table(112) = desid_legend( 'TOL          ' ,092.1 ) ! x           x
        Desid_Emvar_Table(113) = desid_legend( 'TOLU         ' ,092.0 ) ! x    x
        Desid_Emvar_Table(114) = desid_legend( 'TOLUENE      ' ,092.1 ) !      x
        Desid_Emvar_Table(115) = desid_legend( 'UALD         ' ,084.1 ) !             x
        Desid_Emvar_Table(116) = desid_legend( 'VROCIOXY     ' ,247.0 ) !             x      
        Desid_Emvar_Table(117) = desid_legend( 'XYE          ' ,106.2 ) !             x
        Desid_Emvar_Table(118) = desid_legend( 'XYLMN        ' ,106.2 ) ! x
        Desid_Emvar_Table(119) = desid_legend( 'XYM          ' ,106.2 ) !             x
        Desid_Emvar_Table(120) = desid_legend( 'XYO          ' ,106.0 ) !             x
        Desid_Emvar_Table(121) = desid_legend( 'XYOP         ' ,106.2 ) !             x
        Desid_Emvar_Table(122) = desid_legend( 'XYP          ' ,106.0 ) !             x
                          

        ! Particles
        Desid_Emvar_Table(123) = desid_legend( 'ASOIL        ' ,100.0 ) ! x    x      x
        Desid_Emvar_Table(124) = desid_legend( 'ASEACAT      ' ,023.75) ! x    x      x
        Desid_Emvar_Table(125) = desid_legend( 'PAL          ' ,027.0 ) ! x    x      x
        Desid_Emvar_Table(126) = desid_legend( 'PCA          ' ,040.1 ) ! x    x      x
        Desid_Emvar_Table(127) = desid_legend( 'PCL          ' ,035.5 ) ! x    x      x
        Desid_Emvar_Table(128) = desid_legend( 'PEC          ' ,012.0 ) ! x    x      x
        Desid_Emvar_Table(129) = desid_legend( 'PFE          ' ,055.8 ) ! x    x      x
        Desid_Emvar_Table(130) = desid_legend( 'PK           ' ,039.1 ) ! x    x      x
        Desid_Emvar_Table(131) = desid_legend( 'PH2O         ' ,018.0 ) ! x    x      x
        Desid_Emvar_Table(132) = desid_legend( 'PMC          ' ,100.0 ) ! x    x      x
        Desid_Emvar_Table(133) = desid_legend( 'PMG          ' ,024.3 ) ! x    x      x
        Desid_Emvar_Table(134) = desid_legend( 'PMN          ' ,054.9 ) ! x    x      x
        Desid_Emvar_Table(135) = desid_legend( 'PMNCOMN2     ' ,422.83) ! x    x      x        POC value
        Desid_Emvar_Table(136) = desid_legend( 'PMOCN2       ' ,422.83) ! x    x      x        PNCOM value
        Desid_Emvar_Table(137) = desid_legend( 'PMOTHR       ' ,200.0 ) ! x    x      x
        Desid_Emvar_Table(138) = desid_legend( 'PNA          ' ,023.0 ) ! x    x      x
        Desid_Emvar_Table(139) = desid_legend( 'PNCOM        ' ,220.0 ) ! x    x      x
        Desid_Emvar_Table(140) = desid_legend( 'PNCOMN2      ' ,506.99) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(141) = desid_legend( 'PNCOMN1      ' ,408.80) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(142) = desid_legend( 'PNCOMP0      ' ,394.77) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(143) = desid_legend( 'PNCOMP1      ' ,380.75) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(144) = desid_legend( 'PNCOMP2      ' ,338.66) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(145) = desid_legend( 'PNH4         ' ,018.0 ) ! x    x      x
        Desid_Emvar_Table(146) = desid_legend( 'PNO3         ' ,062.0 ) ! x    x      x
        Desid_Emvar_Table(147) = desid_legend( 'POC          ' ,220.0 ) ! x    x      x
        Desid_Emvar_Table(148) = desid_legend( 'POCN2        ' ,506.99) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(149) = desid_legend( 'POCN1        ' ,408.80) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(150) = desid_legend( 'POCP0        ' ,394.77) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(151) = desid_legend( 'POCP1        ' ,380.75) ! x    x      x        based on ALK equivalent C*
        Desid_Emvar_Table(152) = desid_legend( 'POCP2        ' ,338.66) ! x    x      x      
        Desid_Emvar_Table(153) = desid_legend( 'PSI          ' ,028.1 ) ! x    x      x
        Desid_Emvar_Table(154) = desid_legend( 'PSO4         ' ,096.0 ) ! x    x      x
        Desid_Emvar_Table(155) = desid_legend( 'PTI          ' ,047.9 ) ! x    x      x
        Desid_Emvar_Table(156) = desid_legend( 'ROCN2        ' ,422.83) ! x    x      x      
        Desid_Emvar_Table(157) = desid_legend( 'ROCN1        ' ,408.8 ) ! x    x      x      
        Desid_Emvar_Table(158) = desid_legend( 'ROCP0        ' ,394.77) ! x    x      x      
        Desid_Emvar_Table(159) = desid_legend( 'ROCP1        ' ,380.75) ! x    x      x      
        Desid_Emvar_Table(160) = desid_legend( 'ROCP2        ' ,338.66) ! x    x      x      
        Desid_Emvar_Table(161) = desid_legend( 'ROCP3        ' ,296.58) ! x    x      x      
        Desid_Emvar_Table(162) = desid_legend( 'ROCP4        ' ,254.50) ! x    x      x      
        Desid_Emvar_Table(163) = desid_legend( 'ROCP5        ' ,198.39) ! x    x      x      
        Desid_Emvar_Table(164) = desid_legend( 'ROCP6        ' ,184.37) ! x    x      x      
        Desid_Emvar_Table(165) = desid_legend( 'ROCN2ALK     ' ,422.83) ! x    x      x      
        Desid_Emvar_Table(166) = desid_legend( 'ROCN1ALK     ' ,408.8 ) ! x    x      x      
        Desid_Emvar_Table(167) = desid_legend( 'ROCP0ALK     ' ,394.77) ! x    x      x      
        Desid_Emvar_Table(168) = desid_legend( 'ROCP1ALK     ' ,380.75) ! x    x      x      
        Desid_Emvar_Table(169) = desid_legend( 'ROCP2ALK     ' ,338.66) ! x    x      x      
        Desid_Emvar_Table(160) = desid_legend( 'ROCP3ALK     ' ,296.58) ! x    x      x      
        Desid_Emvar_Table(171) = desid_legend( 'ROCP4ALK     ' ,254.50) ! x    x      x      
        Desid_Emvar_Table(172) = desid_legend( 'ROCP5ALK     ' ,198.39) ! x    x      x      
        Desid_Emvar_Table(173) = desid_legend( 'ROCP6ALK     ' ,184.37) ! x    x      x      
        Desid_Emvar_Table(174) = desid_legend( 'ROCP5ARO     ' ,190.33) ! x    x      x      
        Desid_Emvar_Table(175) = desid_legend( 'ROCP6ARO     ' ,176.30) ! x    x      x      
        Desid_Emvar_Table(176) = desid_legend( 'ROCN2OXY2    ' ,282.4 ) ! x    x      x      
        Desid_Emvar_Table(177) = desid_legend( 'ROCN2OXY4    ' ,232.3 ) ! x    x      x      
        Desid_Emvar_Table(178) = desid_legend( 'ROCP0OXY2    ' ,242.4 ) ! x    x      x      
        Desid_Emvar_Table(179) = desid_legend( 'ROCP0OXY4    ' ,202.3 ) ! x    x      x      
        Desid_Emvar_Table(170) = desid_legend( 'ROCP1OXY1    ' ,270.5 ) ! x    x      x      
        Desid_Emvar_Table(181) = desid_legend( 'ROCP1OXY3    ' ,202.3 ) ! x    x      x      
        Desid_Emvar_Table(182) = desid_legend( 'ROCP2OXY2    ' ,200.3 ) ! x    x      x      
        Desid_Emvar_Table(183) = desid_legend( 'ROCP3OXY2    ' ,186.3 ) ! x    x      x      
                           
        ! Toxics           
        Desid_Emvar_Table(184) = desid_legend( 'HGNRVA       ' ,200.6 ) 
        Desid_Emvar_Table(185) = desid_legend( 'HGIIGAS      ' ,200.6 ) 
        Desid_Emvar_Table(186) = desid_legend( 'NICKEL_F     ' ,058.7 ) 
        Desid_Emvar_Table(187) = desid_legend( 'NICKEL_C     ' ,058.7 ) 
        Desid_Emvar_Table(188) = desid_legend( 'CHROMEHEX_F  ' ,052.0 ) 
        Desid_Emvar_Table(189) = desid_legend( 'CHROMEHEX_C  ' ,052.0 ) 
        Desid_Emvar_Table(190) = desid_legend( 'CHROMTRI_F   ' ,052.0 ) 
        Desid_Emvar_Table(191) = desid_legend( 'CHROMTRI_C   ' ,052.0 ) 
        Desid_Emvar_Table(192) = desid_legend( 'BERYLLIUM_F  ' ,009.0 ) 
        Desid_Emvar_Table(193) = desid_legend( 'BERYLLIUM_C  ' ,009.0 ) 
        Desid_Emvar_Table(194) = desid_legend( 'LEAD_F       ' ,207.2 ) 
        Desid_Emvar_Table(195) = desid_legend( 'LEAD_C       ' ,207.2 ) 
        Desid_Emvar_Table(196) = desid_legend( 'DIESEL_PMFINE' ,200.0 ) 
        Desid_Emvar_Table(197) = desid_legend( 'DIESEL_PMEC  ' ,012.0 ) 
        Desid_Emvar_Table(198) = desid_legend( 'DIESEL_PMOC  ' ,220.0 ) 
        Desid_Emvar_Table(199) = desid_legend( 'DIESEL_PMNO3 ' ,062.0 ) 
        Desid_Emvar_Table(200) = desid_legend( 'DIESEL_PMSO4 ' ,096.0 ) 
        Desid_Emvar_Table(201) = desid_legend( 'DIESEL_PMC   ' ,100.0 ) 
        Desid_Emvar_Table(202) = desid_legend( 'CADMIUM_F    ' ,112.4 ) 
        Desid_Emvar_Table(203) = desid_legend( 'CADMIUM_C    ' ,112.4 ) 
        Desid_Emvar_Table(204) = desid_legend( 'MANGANESE_F  ' ,054.9 ) 
        Desid_Emvar_Table(205) = desid_legend( 'MANGANESE_C  ' ,054.9 ) 
        Desid_Emvar_Table(206) = desid_legend( 'ARSENIC_F    ' ,074.92) 
        Desid_Emvar_Table(207) = desid_legend( 'ARSENIC_C    ' ,074.92) 
        Desid_Emvar_Table(208) = desid_legend( 'PHGI         ' ,200.5 ) 
                                                                       
        END SUBROUTINE                                                  
                                                                        
                                                                        
      END MODULE DESID_VARS                                             
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
