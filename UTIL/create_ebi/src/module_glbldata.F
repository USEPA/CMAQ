      MODULE GLOBAL_DATA

      IMPLICIT NONE

      INTEGER, PARAMETER      ::  MXRCT = 3      ! Max no. of reactants in a rxn

      INTEGER, PARAMETER      ::  MAXOUTFLS = 20 ! Max No. of output files generated


c..Group species indices
      INTEGER    ::  NO   = 0
      INTEGER    ::  NO2  = 0
      INTEGER    ::  O3   = 0
      INTEGER    ::  O3P  = 0
      INTEGER    ::  O1D  = 0
      INTEGER    ::  OH   = 0
      INTEGER    ::  HO2  = 0
      INTEGER    ::  HONO = 0
      INTEGER    ::  HNO4 = 0
      INTEGER    ::  NO3  = 0
      INTEGER    ::  N2O5 = 0
      INTEGER    ::  C2O3 = 0
      INTEGER    ::  PAN  = 0
      INTEGER    ::  NO2EX = 0
      INTEGER    ::  RXN_C2O3_SELF = 0


      INTEGER    ::  CL = 0               ! Largest species character length
      INTEGER    ::  LL = 0               ! Largest reaction label length

      INTEGER    ::  N_AE_SPC       = 0   ! No. of AE only species
      INTEGER    ::  N_AQ_SPC       = 0   ! No. of AQ only species
      INTEGER    ::  N_GC_AE_SPC    = 0   ! No. of species in AE mech
      INTEGER    ::  N_GC_AQ_SPC    = 0   ! No. of species in AQ mech
      INTEGER    ::  N_GC_ONLY_SPC  = 0   ! No. of species in gas-phase mech

      INTEGER    ::  N_GC_EBI       = 0   ! No. of EBI species in GC mech
      INTEGER    ::  N_GC_AE_EBI    = 0   ! No. of EBI species in AE mech
      INTEGER    ::  N_GC_AQ_EBI    = 0   ! No. of EBI species in AQ mech
      INTEGER    ::  N_GC_ONLY_EBI  = 0   ! No. of EBI species in gas-phase mech

      INTEGER    ::  NOUTFLS        = 0   ! No. of output files generated
 

C names of mechanism species , taken from GASCHEM_SPC in the RXDT.EXT file
      INTEGER,                      SAVE :: N_SPECIES
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: SPECIES( : )
      
c..list of species in only in the AE & AQ mechanisms
      CHARACTER( 16 ), ALLOCATABLE :: AE_SPC( : )  
      CHARACTER( 16 ), ALLOCATABLE :: AQ_SPC( : ) 

c..complete list of species in AE, AQ, & gas-phase mechanisms
      CHARACTER( 16 ), ALLOCATABLE :: GC_AE_SPC( : )
      CHARACTER( 16 ), ALLOCATABLE :: GC_AQ_SPC( : )
      CHARACTER( 16 ), ALLOCATABLE :: GC_ONLY_SPC( : )

      CHARACTER( 32 ) :: OUTFLNAM( MAXOUTFLS )

      LOGICAL, ALLOCATABLE :: L_GC_EBI( : )        ! Flag for EBI GC species
      LOGICAL, ALLOCATABLE :: L_GC_AE_EBI( : )
      LOGICAL, ALLOCATABLE :: L_GC_AQ_EBI( : )
      LOGICAL, ALLOCATABLE :: L_GC_ONLY_EBI( : )
      LOGICAL, ALLOCATABLE :: L_GC_ONLY_SPC( : )  ! Flag for gas-only species
                                                  ! in SPECIES list

      LOGICAL, ALLOCATABLE :: L_LOW_TOL_SPC( : )  ! Low tolerance flag
      LOGICAL, ALLOCATABLE :: L_HII_TOL_SPC( : )  ! High or Tight tolerance flag

      LOGICAL, ALLOCATABLE :: L_RXFLAG( : )

      LOGICAL, ALLOCATABLE :: L_SS_RXN_FLAG( : )

      END MODULE GLOBAL_DATA
