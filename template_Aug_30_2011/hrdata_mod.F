
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      MODULE  HRDATA

C*************************************************************************
C
C  FUNCTION:  Mechanism & solver data for EBI solver
C             
R1  PRECONDITIONS: For CB4 family of mechanisms only
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
R2  REVISION HISTORY: Prototype created by Jerry Gipson, April, 2003
C
C*************************************************************************

c..EBI solver fixed parameters
      INTEGER, PARAMETER ::  NEBITER = 1000  ! No. of iterations for EBI

      REAL, PARAMETER    ::  DELTAT = 2.5D+00     ! EBI time step

c..Mechanism specific variables
      INTEGER   N_SPEC       ! No. of species in mechanism
      INTEGER   N_RXNS       ! No. of reactions in mechanism
      INTEGER   N_EBISP      ! No. of species solved by EBI
      INTEGER   NING1        ! No. of species in group 1
      INTEGER   NING2        ! No. of species in group 2
      INTEGER   N_SPCL_RKS   ! No. of special rate constants
      INTEGER   N_ACTSP      ! No. of species in mechanism (non-ss spec)

c..Control flags
      LOGICAL   L_AE_VRSN    ! Flag for aerosol version of mech
      LOGICAL   L_AQ_VRSN    ! Flag for aqueous chemistry version of mech

R5! SWITCH for calling DEGRADE routine
R5    LOGICAL, SAVE :: CALL_DEG = .TRUE.

c..Miscellaneous variables
      INTEGER   LOGDEV        ! Unit number of output log
      INTEGER   N_EBI_IT      ! No. of iterations for EBI
      INTEGER   N_EBI_STEPS   ! No. of time steps for EBI
      INTEGER   N_INR_STEPS   ! No. of inner time steps for EBI
R5
R5    INTEGER, SAVE ::   MODEL_SPECIES ! No. of species in CGRID

      REAL    EBI_TMSTEP   ! Time step for EBI loops (min)

c...Allocatable arrays
      INTEGER, ALLOCATABLE :: EBISP( : )         ! Index of EBI species
      INTEGER, ALLOCATABLE :: SSASP( : )         ! Index of ss species
      INTEGER, ALLOCATABLE :: ACTSP( : )         ! Index of active species

      REAL( 8 ), ALLOCATABLE ::  RKI(   : )   ! Rate constants 
      REAL( 8 ), ALLOCATABLE :: RKI_SAV( : )  ! Saved values of RKI for SS species calcs
      REAL, ALLOCATABLE ::  RXRAT( : )     ! Reaction rates 
      REAL, ALLOCATABLE ::  RTOL(  : )     ! Species tolerances 
      REAL, ALLOCATABLE ::  YC(    : )     ! Species concentrations
R5    REAL, ALLOCATABLE ::  YCCELL( : )    ! ALL Species in cgrid
      REAL, ALLOCATABLE ::  YC0(   : )     ! Species concentrations
      REAL, ALLOCATABLE ::  YCP(   : )     ! Species concentrations
      REAL, ALLOCATABLE ::  PROD(  : )     ! Prod of species 
      REAL, ALLOCATABLE ::  LOSS(  : )     ! Loss of species 
R3      REAL, ALLOCATABLE ::  PNEG(  : )     ! Negative production rates


