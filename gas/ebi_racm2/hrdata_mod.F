
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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      MODULE  HRDATA

C*************************************************************************
C
C  FUNCTION:  Mechanism & solver data for EBI solver
C
C  PRECONDITIONS: For the RACM2 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, March 4, 2013
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


c..Species indices
      INTEGER   O3
      INTEGER   O3P
      INTEGER   O1D
      INTEGER   H2O2
      INTEGER   HO
      INTEGER   NO2
      INTEGER   NO
      INTEGER   NO3
      INTEGER   HONO
      INTEGER   HNO3
      INTEGER   HNO4
      INTEGER   HO2
      INTEGER   HCHO
      INTEGER   CO
      INTEGER   ACD
      INTEGER   MO2
      INTEGER   ALD
      INTEGER   ETHP
      INTEGER   ACT
      INTEGER   ACO3
      INTEGER   UALD
      INTEGER   KET
      INTEGER   MEK
      INTEGER   HKET
      INTEGER   MACR
      INTEGER   MACP
      INTEGER   XO2
      INTEGER   MVK
      INTEGER   GLY
      INTEGER   MGLY
      INTEGER   DCB1
      INTEGER   DCB2
      INTEGER   BALD
      INTEGER   CHO
      INTEGER   OP1
      INTEGER   OP2
      INTEGER   PAA
      INTEGER   ONIT
      INTEGER   PAN
      INTEGER   N2O5
      INTEGER   SO2
      INTEGER   SULF
      INTEGER   SULRXN
      INTEGER   ETH
      INTEGER   HC3
      INTEGER   HC3P
      INTEGER   HC5
      INTEGER   HC5P
      INTEGER   HC8
      INTEGER   HC8P
      INTEGER   ALK5RXN
      INTEGER   ETE
      INTEGER   ETEP
      INTEGER   OLT
      INTEGER   OLTP
      INTEGER   OLI
      INTEGER   OLIP
      INTEGER   DIEN
      INTEGER   ACE
      INTEGER   ORA1
      INTEGER   BENZENE
      INTEGER   BENP
      INTEGER   EPX
      INTEGER   PHEN
      INTEGER   BENZRO2
      INTEGER   TOL
      INTEGER   TR2
      INTEGER   TLP1
      INTEGER   CSL
      INTEGER   TOLRO2
      INTEGER   XYM
      INTEGER   XY2
      INTEGER   XYL1
      INTEGER   XYLRO2
      INTEGER   XYP
      INTEGER   XYO
      INTEGER   XYO2
      INTEGER   ISO
      INTEGER   ISOP
      INTEGER   ISOPRXN
      INTEGER   API
      INTEGER   APIP
      INTEGER   TRPRXN
      INTEGER   LIM
      INTEGER   LIMP
      INTEGER   RCO3
      INTEGER   ACTP
      INTEGER   MEKP
      INTEGER   KETP
      INTEGER   MCP
      INTEGER   MVKP
      INTEGER   UALP
      INTEGER   DCB3
      INTEGER   BALP
      INTEGER   ADDC
      INTEGER   MCT
      INTEGER   MCTO
      INTEGER   MOH
      INTEGER   EOH
      INTEGER   ROH
      INTEGER   ETEG
      INTEGER   ISHP
      INTEGER   MAHP
      INTEGER   ORA2
      INTEGER   ORAP
      INTEGER   PPN
      INTEGER   MPAN
      INTEGER   NALD
      INTEGER   ISON
      INTEGER   MCTP
      INTEGER   OLNN
      INTEGER   OLND
      INTEGER   ADCN
      INTEGER   TOLP
      INTEGER   PER1
      INTEGER   XYLP
      INTEGER   PER2
      INTEGER   XYOP
      INTEGER   BAL1
      INTEGER   BAL2
      INTEGER   TOLNRXN
      INTEGER   TOLHRXN
      INTEGER   XYLNRXN
      INTEGER   XYLHRXN
      INTEGER   BNZNRXN
      INTEGER   BNZHRXN
      INTEGER   SESQ
      INTEGER   SESQRXN

      END MODULE HRDATA
