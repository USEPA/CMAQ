C=======================================================================
      MODULE PA_DEFN
 
C Contains: Control parameters for Process Analysis
C
C Dependent On: Mechanism Input file
C
C Created By: Process Analysis Control Program
C           11:05:01 EDT  July 15, 2016

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The following variables are used to control calls to the Integrated
c  Process Rate and Integrated Reaction Rate routines:
c     LIPR = Flag for calculating Integrated Process Rates
c     LIRR = Flag for calculating Integrated Reaction Rates
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c these are set in pa_read.F
      LOGICAL :: LIPR = .FALSE.
      LOGICAL :: LIRR = .FALSE.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The following variables are used in the output file descriptions
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     NIPRVAR = Number of IPR output variables
c     NIRRVAR = Number of IRR output variables
c     IPRNAME = Name of IPR output variable
c     IRRNAME = Name of IRR output variable
c     IPRDESC = Extended name of IPR output variable
c     IRRDESC = Extended name of IRR output variable
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c these are set in pa_setup_ipr.F, pa_read.F, respectively
c   change to pa_read
      INTEGER :: NIPRVAR = 0
      INTEGER :: NIRRVAR = 0

c these are set in pa_setup_ipr.F, pa_getirrout.F (pa_setup_irr.F)
c   allocate in pa_read
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: IPRNAME( : ) ! IPRNAME( NIPRVAR )
      CHARACTER( 60 ), ALLOCATABLE, SAVE :: IPRDESC( : ) ! IPRDESC( NIPRVAR )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: IRRNAME( : ) ! IRRNAME( NIRRVAR )
      CHARACTER( 60 ), ALLOCATABLE, SAVE :: IRRDESC( : ) ! IRRDESC( NIRRVAR )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The following variables are used in the integrated process rate 
c  computations.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    NPRCS     = No. of science processes that can be monitored
c    PROCNAME  =
c    N_IPR_SPC = Number of families for IPR
c    NCSAVE    = Number of CTM species concs to save for IPR
c    MXCGRID   = Max number of CTM species in an IPR family
c    LPROCOUT  =
c    NCGRID    = Array of no. of CTM species in an IPR family
c    IPROUT    = Array of IPR output variable indices
c    IPR2GRD   = Array of CTM species indices for IPR families
c    IPR2SAV   = Array of saved species indices for IPR families
c    SV2GRD    = Array of CTM indicies for saved concs
c    SPCOEF    = Array of coefficients for IPR familys
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      INTEGER, PARAMETER :: NPRCS = 14
 
      INTEGER, PARAMETER :: IPR_XADV = 1
      INTEGER, PARAMETER :: IPR_YADV = 2
      INTEGER, PARAMETER :: IPR_ZADV = 3
      INTEGER, PARAMETER :: IPR_HDIF = 4
      INTEGER, PARAMETER :: IPR_VDIF = 5
      INTEGER, PARAMETER :: IPR_EMIS = 6
      INTEGER, PARAMETER :: IPR_DDEP = 7
      INTEGER, PARAMETER :: IPR_PVO3 = 8
      INTEGER, PARAMETER :: IPR_CLDS = 9
      INTEGER, PARAMETER :: IPR_CHEM = 10
      INTEGER, PARAMETER :: IPR_COND = 11
      INTEGER, PARAMETER :: IPR_COAG = 12
      INTEGER, PARAMETER :: IPR_NPF  = 13
      INTEGER, PARAMETER :: IPR_GROW = 14
 
      CHARACTER(4), PARAMETER :: PROCNAME( NPRCS ) =
     & (/'XADV','YADV','ZADV','HDIF','VDIF','EMIS','DDEP',
     &   'PVO3','CLDS','CHEM','COND','COAG','NPF ','GROW' /)

      REAL, ALLOCATABLE, SAVE :: CSAV( :,:,:,: )

      LOGICAL, SAVE :: COUPLE_WRF = .FALSE.

c this is set in pa_getiprout.F, pa_errcheck.F?
      INTEGER :: N_IPR_SPC = 0
c these are set in pa_setup_ipr.F
      INTEGER :: MXCGRID   = 0

c these are allocated in pa_setpadefn.F
      INTEGER, ALLOCATABLE, SAVE :: MAP_IPRVARtoSPC( : )  ! ( NIPRVAR )
      INTEGER, ALLOCATABLE, SAVE :: NCGRID ( : )          ! ( N_IPR_SPC )
      INTEGER, ALLOCATABLE, SAVE :: MAP_SAVEtoCGRID( : )  ! ( NCSAVE )
      INTEGER, ALLOCATABLE, SAVE :: MAP_IPRtoCGRID( :,: ) ! ( N_IPR_SPC,MXCGRID )
      INTEGER, ALLOCATABLE, SAVE :: MAP_IPRtoSAVE( :,: )  ! ( N_IPR_SPC,MXCGRID )
      REAL,    ALLOCATABLE, SAVE :: SPCOEF ( :,: )  ! ( N_IPR_SPC,MXCGRID )
      LOGICAL, ALLOCATABLE, SAVE :: MASK_IPR_PROC( :,: )

      LOGICAL, SAVE              :: BUDGET_DIAG = .FALSE.
      LOGICAL, SAVE              :: BUDGET_HPPM = .FALSE.
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: BUDGETVARIABLES( : )
      INTEGER, PARAMETER :: MAX_BUDGET_VARS_NML = 200

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The following variables are used in the integrated reaction rate
c  computations.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    LFULLIRR     = Flag for computing IRRs for every reaction
c    LPARTIRR     = Flag for computing IRRs for selected reactions
c    NUMOUTPOS    = No. of temp. IRR terms to add iff the IRR > 0
c    NUMOUTNEG    = No. of temp. IRR terms to add iff the IRR < 0
c    NUMOUTIND    = No. of temp. IRR terms to add regardless of sign
c    MXNIRRRXNS   = Max no. of reaction terms in an output IRR
c    MXTEMPTRMS   = Max no. of reaction terms in a temp. IRR
c    NIRRRXNS     = Vector of no. of reaction terms in an output IRR
c    TEMPTERMS    = Vector of no. of terms in a temporary IRR
c    TEMPOUTPOS   = Vector of no. of IRR terms to add if IRR > 0
c    TEMPOUTNEG   = Vector of no. of IRR terms to add if IRR < 0
c    TEMPOUTIND   = Vector of no. of IRR terms to add regardless of sign
c    INDXOUTPOS   = Vector of indices pointing to IRR output variable
c    INDXOUTNEG   = Vector of indices pointing to IRR output variable
c    INDXOUTIND   = Vector of indices pointing to IRR output variable
c    COEFOUTPOS   = Vector of temp IRR coefficients
c    COEFOUTNEG   = Vector of temp IRR coefficients
c    COEFOUTIND   = Vector of temp IRR coefficients
c    NUMTEMPS     = No. of temporary intermediate IRRs to compute
c    TEMPRXN      = Array of reaction nos. for a temp. IRR
c    IRRRXN       = Array of reaction nos. for an output IRR
c    TEMPCOEF     = Array of reaction coefficients for a temp IRR
c    IRRCOEF      = Array of reaction coefficients for an output IRR
c    IRRSTEP      = Array of output IRRs for one time step
c    IRRBLK       = Array of output IRRs for a block of cells
c    IRROUT       = Array of IRRs to output
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c these are set in pa_read.F
      LOGICAL, SAVE :: LFULLIRR = .FALSE.
      LOGICAL, SAVE :: LPARTIRR = .FALSE.

c these are set in pa_setup_irr.F, pa_errcheck.F
      INTEGER, SAVE :: NUMOUTPOS  = 0
      INTEGER, SAVE :: NUMOUTNEG  = 0
      INTEGER, SAVE :: NUMOUTIND  = 0
      INTEGER, SAVE :: MXNIRRRXNS = 0
      INTEGER, SAVE :: NUMTEMPS   = 0
      INTEGER, SAVE :: MXTEMPTRMS = 0

c this is set in the Model`s GRID_CONF
      INTEGER, SAVE :: BLKSIZED = 50

c these are allocated in pa_setup_irr.F
      INTEGER, ALLOCATABLE, SAVE :: NIRRRXNS  ( : )    ! ( NIRRVAR )
      INTEGER, ALLOCATABLE, SAVE :: IRRRXN    ( :,: )  ! ( NIRRVAR,MXNIRRRXNS )
      INTEGER, ALLOCATABLE, SAVE :: TEMPOUTPOS( : )    ! ( NUMOUTPOS )
      INTEGER, ALLOCATABLE, SAVE :: INDXOUTPOS( : )    ! ( NUMOUTPOS )
      INTEGER, ALLOCATABLE, SAVE :: TEMPOUTNEG( : )    ! ( NUMOUTNEG )
      INTEGER, ALLOCATABLE, SAVE :: INDXOUTNEG( : )    ! ( NUMOUTNEG )
      INTEGER, ALLOCATABLE, SAVE :: TEMPOUTIND( : )    ! ( NUMOUTIND )
      INTEGER, ALLOCATABLE, SAVE :: INDXOUTIND( : )    ! ( NUMOUTIND )
      INTEGER, ALLOCATABLE, SAVE :: TEMPTERMS ( : )    ! ( NUMTEMPS )
      INTEGER, ALLOCATABLE, SAVE :: TEMPRXN   ( :,: )  ! ( NUMTEMPS,MXTEMPTRMS )

      REAL,    ALLOCATABLE, SAVE :: COEFOUTPOS( : )    ! ( NUMOUTPOS )
      REAL,    ALLOCATABLE, SAVE :: COEFOUTNEG( : )    ! ( NUMOUTNEG )
      REAL,    ALLOCATABLE, SAVE :: COEFOUTIND( : )    ! ( NUMOUTIND )
      REAL,    ALLOCATABLE, SAVE :: TEMPCOEF  ( :,: )  ! ( NUMTEMPS,MXTEMPTRMS )
      REAL,    ALLOCATABLE, SAVE :: IRRCOEF   ( :,: )  ! ( NIRRVAR,MXNIRRRXNS )
      REAL,    ALLOCATABLE, SAVE :: IRRSTEP   ( :,: )  ! ( BLKSIZED,NIRRVAR )
      REAL,    ALLOCATABLE, SAVE :: IRRBLK    ( :,: )  ! ( BLKSIZED,NIRRVAR )
      REAL,    ALLOCATABLE, SAVE :: IRRDEL    ( : )    ! ( NIRRVAR )
      REAL,    ALLOCATABLE, SAVE :: IRRSUM    ( : )    ! ( NIRRVAR )

      END MODULE PA_DEFN
