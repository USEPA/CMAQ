       MODULE MECHANISM_PARMS
       
         IMPLICIT NONE
         
         INTEGER, PARAMETER :: MAXRXNUM    = 2000
  
         INTEGER, PARAMETER :: MAXSPEC     = 700

         INTEGER, PARAMETER :: MAXPRODS    = 40   ! mechanism products dimension

         INTEGER, PARAMETER :: MAXRCTNTS   = 3    ! mechanism reactants dimension

         INTEGER, PARAMETER :: MAXPHOTRXNS = 100
 
         INTEGER, PARAMETER :: MAXSPECRXNS = 100

         INTEGER, PARAMETER :: MAXSPECTERMS = MAXSPEC
         INTEGER, PARAMETER :: MAXFALLOFF  = 50

         INTEGER, PARAMETER :: MAX3BODIES  = 50

         INTEGER, PARAMETER :: MAXWRDLEN   = 16

         INTEGER, PARAMETER :: MAXCONSTS   = 5    ! mechanism "constants"

         INTEGER, PARAMETER :: MAXNLIST     =  50   ! Max no. of species in SS or Eliminate lists

         INTEGER, PARAMETER :: KPPEQN_UNIT  =  95    
      
      
         END MODULE MECHANISM_PARMS
