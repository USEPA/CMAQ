
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
C  PRECONDITIONS: For the CB05TUCL family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Oct. 18, 2012
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


c..Miscellaneous variables
      INTEGER   LOGDEV        ! Unit number of output log
      INTEGER   N_EBI_IT      ! No. of iterations for EBI
      INTEGER   N_EBI_STEPS   ! No. of time steps for EBI
      INTEGER   N_INR_STEPS   ! No. of inner time steps for EBI

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
      REAL, ALLOCATABLE ::  YC0(   : )     ! Species concentrations
      REAL, ALLOCATABLE ::  YCP(   : )     ! Species concentrations
      REAL, ALLOCATABLE ::  PROD(  : )     ! Prod of species
      REAL, ALLOCATABLE ::  LOSS(  : )     ! Loss of species
      REAL, ALLOCATABLE ::  PNEG(  : )     ! Negative production rates


c..Species indices
      INTEGER   NO2
      INTEGER   NO
      INTEGER   O
      INTEGER   O3
      INTEGER   NO3
      INTEGER   O1D
      INTEGER   OH
      INTEGER   HO2
      INTEGER   N2O5
      INTEGER   HNO3
      INTEGER   HONO
      INTEGER   PNA
      INTEGER   H2O2
      INTEGER   XO2
      INTEGER   XO2N
      INTEGER   NTR
      INTEGER   ROOH
      INTEGER   FORM
      INTEGER   ALD2
      INTEGER   ALDX
      INTEGER   PAR
      INTEGER   CO
      INTEGER   MEO2
      INTEGER   MEPX
      INTEGER   MEOH
      INTEGER   HCO3
      INTEGER   FACD
      INTEGER   C2O3
      INTEGER   PAN
      INTEGER   PACD
      INTEGER   AACD
      INTEGER   CXO3
      INTEGER   PANX
      INTEGER   ROR
      INTEGER   OLE
      INTEGER   ETH
      INTEGER   IOLE
      INTEGER   TOL
      INTEGER   CRES
      INTEGER   TO2
      INTEGER   TOLRO2
      INTEGER   OPEN
      INTEGER   MGLY
      INTEGER   CRO
      INTEGER   CAT1
      INTEGER   CRON
      INTEGER   CRNO
      INTEGER   CRN2
      INTEGER   CRPX
      INTEGER   OPO3
      INTEGER   CAO2
      INTEGER   OPAN
      INTEGER   XYL
      INTEGER   XYLRO2
      INTEGER   ISOP
      INTEGER   ISPD
      INTEGER   ISOPRO2
      INTEGER   TERP
      INTEGER   TRPRO2
      INTEGER   SO2
      INTEGER   SULF
      INTEGER   SULRXN
      INTEGER   ETOH
      INTEGER   ETHA
      INTEGER   CL2
      INTEGER   CL
      INTEGER   HOCL
      INTEGER   CLO
      INTEGER   FMCL
      INTEGER   HCL
      INTEGER   SV_AVB1
      INTEGER   SV_AVB2
      INTEGER   SV_AVB3
      INTEGER   SV_AVB4
      INTEGER   BENZENE
      INTEGER   BENZRO2
      INTEGER   SV_BVB1
      INTEGER   SV_BVB2
      INTEGER   SV_BVB3
      INTEGER   SV_BVB4
      INTEGER   SESQ
      INTEGER   SV_AVB0
      INTEGER   SV_BVB0
      INTEGER   SV_PVB1
      INTEGER   SV_PVB0
      INTEGER   SV_PVB2
      INTEGER   SV_PVB3
      INTEGER   SV_PVB4
      INTEGER   SV_FVB1
      INTEGER   SV_FVB0
      INTEGER   SV_FVB2
      INTEGER   SV_FVB3
      INTEGER   SV_FVB4
      INTEGER   IVOC_P
      INTEGER   IVOC_F

      END MODULE HRDATA
