       MODULE KPP_DATA
       
         IMPLICIT NONE

         
         INTEGER, PARAMETER :: NFIXED_SPECIES  = 6
         CHARACTER(  16 ) :: FIXED_SPECIES( NFIXED_SPECIES ) = (/
     &                       'M               ',
     &                       'H2O             ',
     &                       'O2              ',
     &                       'N2              ',
     &                       'CH4             ',
     &                       'H2              ' /)
     
         INTEGER              :: NRXN_FIXED_SPECIES( NFIXED_SPECIES )

         INTEGER, PARAMETER :: ind_M   = 1 
         INTEGER, PARAMETER :: ind_H2O = 2 
         INTEGER, PARAMETER :: ind_O2  = 3
         INTEGER, PARAMETER :: ind_N2  = 4 
         INTEGER, PARAMETER :: ind_CH4 = 5
         INTEGER, PARAMETER :: ind_H2 =  6 

         INTEGER, PARAMETER :: ind_DUMMY = 7 



         INTEGER, ALLOCATABLE, SAVE   :: INDEX_FIXED_SPECIES( :, : )
         
         
         
       END MODULE KPP_DATA
