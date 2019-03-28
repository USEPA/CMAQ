       MODULE MECHANISM_PARMS
       
         IMPLICIT NONE
         
         INTEGER, PARAMETER :: MAXRXNUM    = 2000
  
         INTEGER, PARAMETER :: MAXSPEC     = 700

         INTEGER, PARAMETER :: MAXPRODS    = 40   ! mechanism products dimension

         INTEGER, PARAMETER :: MAXRCTNTS   = 3    ! mechanism reactants dimension

         INTEGER, PARAMETER :: MAXPHOTRXNS = 600
 
         INTEGER, PARAMETER :: MAXSPECRXNS = 600

         INTEGER, PARAMETER :: MAXFUNCTIONS = 6 * MAXRXNUM

         INTEGER, PARAMETER :: MAXSPECTERMS = MAXSPEC

         INTEGER, PARAMETER :: MAXFALLOFF  = 150

         INTEGER, PARAMETER :: MAX3BODIES  = 150

         INTEGER, PARAMETER :: MAXWRDLEN   = 16

         INTEGER, PARAMETER :: MAXCONSTS   = 5    ! mechanism "constants"

         INTEGER, PARAMETER :: MAXNLIST     =  50   ! Max no. of species in SS or Eliminate lists

         INTEGER, PARAMETER :: KPPEQN_UNIT  =  95    

         LOGICAL :: END_OF_IMECH       = .FALSE.   ! end of mech_def encountered
         LOGICAL :: REORDER_SPECIES    = .FALSE.   ! .TRUE.   ! reorder species based on #terms in time derivative
         LOGICAL :: ASSESS_EFFECTS     = .FALSE.   ! remove terms in time derivative if reaction loss and production cancel out
         LOGICAL :: REORDER_REACTIONS  = .FALSE.   ! .TRUE.
         LOGICAL :: OMIT_RCONST_DATA   = .FALSE.   ! Do not write rate constant parameters of reaction data module

         END MODULE MECHANISM_PARMS
