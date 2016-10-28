C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CHECK_SS_SPC ( LUNOUT, NS, SPCLIS, NR, LABEL, SS1RX )

C=======================================================================
C Checks to make sure all species selected to be in steady state do not
C violate special steady-state species rules. It also reorders the
C steady-state species if necessary to insure any species dependencies
C are properly maintained.  A species must be meet the following criteria
C to be allowed:
C   1) The defined steady-state species name must be found in the mechanism
C   2) A steady-state species may not react with another steady-state species
C      (including reacting with itself)
C   3) A steady-state species cannot be produced from another steady-state
C      species if it also reacts to produce that same species (i.e.. 
C      inner dependencies are not allowed)
C=======================================================================


      USE MECHANISM_DATA
      
      IMPLICIT NONE

C..Input Arguments
      INTEGER,         INTENT ( IN )    :: LUNOUT               ! Output unit number
      INTEGER,         INTENT ( IN )    ::  NS                  ! No. of species in mechanism
      CHARACTER( 16 ), INTENT ( IN )    ::  SPCLIS( : )   ! List of mechanism species
      INTEGER,         INTENT ( IN )    ::  NR                  ! No. of reactions
      CHARACTER( 16 ), INTENT ( IN )    ::  LABEL( :,: ) ! Reaction labels
      INTEGER,        INTENT ( INOUT )  ::  SS1RX( : )



C..Functions
      INTEGER INDEX1

C..Local variables

      CHARACTER( 16 ), ALLOCATABLE  :: SS_SPC_TEMP( : )  ! Temp SS species name array

      INTEGER  IND                              ! Species index
      INTEGER  IPRD                             ! Product loop index
      INTEGER  IRCT                             ! Reactant loop index
      INTEGER  SS1, SS2, SS3                    ! Loop indices for steady-state species
      INTEGER  NRX                              ! Loop index for reactions
      INTEGER  N_SS_RCTNT                       ! Counter
      INTEGER  NUM_RSLVD                        ! Number of SS species with dependencies reolved
      INTEGER  SAVD_NUM_RSLVD                   ! Saved value of above

      INTEGER  ::   SUM_RCT_COEF                ! Sum of all SS species reactant coefficients
      INTEGER  ::   SUM_PRD_COEF                ! Sum of all SS species product coefficients


      INTEGER, ALLOCATABLE  ::  SS_SPC_IND( : )     ! SS speciecies index array
      INTEGER, ALLOCATABLE  ::  NUM_DEPEND( : )     ! No. of depencies for each species
      INTEGER, ALLOCATABLE  ::  RSLVD_IND( : )      ! Resolved index for species 

      INTEGER, ALLOCATABLE  ::  SS_DEPEND( : , : )  ! SS dependency matrix

      LOGICAL   ::    LERROR           ! Error found flag
      LOGICAL   ::    LFOUND           ! Found flag
      LOGICAL   ::    L_REORDER        ! Flag for re-orderining SS species
      LOGICAL   ::    LDEPOK           ! Flag for dependencies identified

      LOGICAL, ALLOCATABLE  ::    LRSLVD( : )       ! Flag to indicate species dependencies resolved
      LOGICAL, ALLOCATABLE  ::    LREACT( : )       ! Flag to indicate species dependencies resolved
      LOGICAL, ALLOCATABLE  ::    LPROD( : )        ! Flag to indicate species dependencies resolved

      INTEGER, ALLOCATABLE  ::    TEMP_RCT_COEF( : , : )  ! Temp array holding reactant coefficients
      REAL, ALLOCATABLE     ::    TEMP_PRD_COEF( : , : )  ! Temp array holding product coefficients


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Allocate arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ALLOCATE( SS_SPC_IND( N_SS_SPC ) )
      ALLOCATE( NUM_DEPEND( N_SS_SPC ) )
      ALLOCATE( LRSLVD( N_SS_SPC ) )
      ALLOCATE( LREACT( N_SS_SPC ) )
      ALLOCATE( LPROD( N_SS_SPC ) )
      ALLOCATE( SS_SPC_TEMP( N_SS_SPC ) )
      ALLOCATE( RSLVD_IND( N_SS_SPC ) )

      ALLOCATE( SS_DEPEND( N_SS_SPC, N_SS_SPC ) )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to make sure all species defined as being in steady-state are in the
c  mechanism; if any are not found, list them and stop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LERROR = .FALSE. 

      DO SS1 = 1 , N_SS_SPC

         SUM_PRD_COEF = 0.0
         SUM_RCT_COEF = 0
         DO NRX = 1, NR
            SUM_PRD_COEF = SUM_PRD_COEF + SS_PRD_COEF( SS1, NRX )
            SUM_RCT_COEF = SUM_RCT_COEF + SS_RCT_COEF( SS1, NRX )
         ENDDO

 
         IF( SUM_RCT_COEF .LE. 0 .AND. SUM_PRD_COEF .LE. 0.0 ) THEN
            IF( .NOT. LERROR ) THEN
               LERROR = .TRUE.
               WRITE( LUNOUT, 9000 ) 
            ENDIF
            WRITE( LUNOUT, 9500 ) SS1, SS_SPC( SS1 ) 
         ENDIF
      
      ENDDO  

      IF( LERROR ) THEN
         WRITE( LUNOUT , 10000 )
         STOP
      ENDIF       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to make sure that no steady state species reacts with any other steady-state
c  species (including itself). If any cases are found, list them and stop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LERROR = .FALSE.

      DO NRX = 1, NR

         SUM_RCT_COEF = 0

         DO SS1 = 1, N_SS_SPC
            SUM_RCT_COEF = SUM_RCT_COEF + SS_RCT_COEF( SS1, NRX )
         ENDDO
                
         IF( SUM_RCT_COEF .GT. 1 ) THEN

            IF( .NOT. LERROR ) THEN
               LERROR = .TRUE.
               WRITE( LUNOUT, 9020 ) 
            ENDIF
            WRITE( LUNOUT, 9520 ) NRX, LABEL( NRX, 1 ) 

         ENDIF

      ENDDO

      IF( LERROR ) THEN
         WRITE( LUNOUT , 10000 )
         STOP
      ENDIF      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to make sure that no steady state species appears as both a reactant and
c  a product in the same reaction. If any cases are found, list them and stop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LERROR = .FALSE.

      DO NRX = 1, NR

         DO SS1 = 1, N_SS_SPC
                
            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0 .AND. 
     &          SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) THEN

               IF( .NOT. LERROR ) THEN
                  LERROR = .TRUE.
                  WRITE( LUNOUT, 9040 ) 
               ENDIF
               WRITE( LUNOUT, 9520 ) NRX, LABEL( NRX, 1 ) 

             ENDIF

          ENDDO

      ENDDO

      IF( LERROR ) THEN
         WRITE( LUNOUT , 10000 )
         STOP
      ENDIF      


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to make sure that each steady-state species appears as both a reactant 
c  and a product in separate reactions
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LREACT = .FALSE.   ! Array
      LPROD  = .FALSE.   ! Array
 
      DO NRX = 1, NR

         DO SS1 = 1, N_SS_SPC   

c..Flag the species if it is a reactant or a product in this reaction
            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0   ) LREACT( SS1 ) = .TRUE.
            IF( SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) LPROD( SS1 ) = .TRUE. 

         ENDDO

      ENDDO

      LERROR = .FALSE.
      DO SS1 = 1, N_SS_SPC

         IF( .NOT. LREACT( SS1 ) .OR.  .NOT. LPROD( SS1 ) ) THEN
            IF( .NOT. LERROR ) THEN
               LERROR = .TRUE.
               WRITE( LUNOUT, 9060 ) 
            ENDIF
            WRITE( LUNOUT, 9500 ) SS1, SS_SPC( SS1 )

         ENDIF

      ENDDO

      
      IF( LERROR ) THEN
         WRITE( LUNOUT , 10000 )
         STOP
      ENDIF      
   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Steady-state species dependency analysis Algorithm
c
c   1. Build a dependency matrix -- A value of 1 in the matrix indicates that 
c      the steady species corresponding to the row is produced by the 
c      steady-state species in the corresponding column
c
c   2. Sum up the number of dependencies for each species
c
c   3. Put the species with zero dependencies at the top of the list
c
c   4. Next, check the remaining species, adding them when all the all species
c      they depend upon are in the list. Stop when all have been added or 
c      no more can be added ( the latter indicates an interdependency error
c
c   5. Reorder the species and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..Create Dependency matrix
      SS_DEPEND = 0      ! Array

      DO SS1 = 1, N_SS_SPC

         DO NRX = 1, NR

            IF( SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) THEN            ! SS1 species is a product

               DO SS2 = 1, N_SS_SPC
                  IF( SS_RCT_COEF( SS2, NRX ) .GT. 0 ) THEN        ! SS2 species is a reactant
                     SS_DEPEND( SS1, SS2 ) = 1
                  ENDIF
               ENDDO

            ENDIF

         ENDDO           ! loop over reactions

      ENDDO              ! loop over SS1

           

c..Sum the number of dependencies for each steady-state species

      DO SS1 = 1, N_SS_SPC
         NUM_DEPEND( SS1 ) = 0
         DO SS2 = 1, N_SS_SPC
            IF( SS_DEPEND( SS1, SS2 ) .EQ. 1 ) NUM_DEPEND( SS1 ) = NUM_DEPEND( SS1 ) + 1
         ENDDO
      ENDDO

      


c..Order the species so that any steady-state species that is dependent upon another
c..comes after that species in the list of steady-state species

c..First, check for no dependencies (if none, no ee-ordering is necessary)

      L_REORDER = .FALSE.
      DO SS1 = 1, N_SS_SPC
         IF( NUM_DEPEND( SS1 ) .GT. 0 ) L_REORDER = .TRUE.
      ENDDO

      IF( .NOT. L_REORDER ) GO TO 1000

c..Put all steady-state species with zero dependencies at the top of the list
      NUM_RSLVD = 0
      LRSLVD= .FALSE.    ! Array
      RSLVD_IND = 0      ! Array
      
      DO SS1 = 1, N_SS_SPC
  
         IF( NUM_DEPEND( SS1 ) .EQ. 0 ) THEN
            NUM_RSLVD = NUM_RSLVD + 1
            RSLVD_IND( NUM_RSLVD ) = SS1
            LRSLVD( SS1 ) = .TRUE.
         ENDIF

      ENDDO


c..Now do rest of species by checking dependencies
 

100   CONTINUE

      SAVD_NUM_RSLVD = NUM_RSLVD 

      DO SS1 = 1, N_SS_SPC                   ! Loop over all the SS species
 
        IF( .NOT. LRSLVD( SS1 ) ) THEN       ! Consider only species not yet resolved

           

c..see if all the reactant SS species for this SS product have been resolved
            LDEPOK = .TRUE.
            DO SS2 = 1, N_SS_SPC            
               IF( SS_DEPEND( SS1, SS2 ) .EQ. 1 .AND. .NOT. LRSLVD( SS2 ) ) LDEPOK = .FALSE.
            ENDDO
             
c..If they have, add this species to list of resolved species 
           IF( LDEPOK ) THEN
                NUM_RSLVD = NUM_RSLVD + 1
                RSLVD_IND( NUM_RSLVD ) = SS1
                LRSLVD( SS1 ) = .TRUE.
            ENDIF

         ENDIF

      ENDDO


c..Check to see if all species have been resolved.
c..IF NO
c     1) check to make sure at least one species was added in last pass
c     2) go back and do another pass
c..IF YES
c     1) reorder the incoming array and return


      IF( NUM_RSLVD .LT. N_SS_SPC ) THEN        ! Not finished - check for error or go back

         IF( NUM_RSLVD .EQ. SAVD_NUM_RSLVD ) THEN  ! Error - could not add any more to list
            WRITE( LUNOUT ,  9600 )
            DO SS1 = 1, N_SS_SPC
               IF( .NOT. LRSLVD( SS1 ) ) WRITE( LUNOUT, 9620 ), SS_SPC( SS1 )
            ENDDO
            WRITE( LUNOUT , 10000 )
            STOP
         ELSE
            GO TO 100 
         ENDIF

      ELSE                                    ! Finished -- do the final reordering

c..load names into temporary array
         DO SS1 = 1, NUM_RSLVD
           SS_SPC_TEMP( SS1 ) = SS_SPC( SS1 )
         ENDDO    

c..Reorder the original incoming list
         DO SS1 = 1, NUM_RSLVD
            SS_SPC( SS1 ) = SS_SPC_TEMP( RSLVD_IND( SS1 ) )
         ENDDO

c..Reorder the production and loss coefficients too
         ALLOCATE( TEMP_RCT_COEF( N_SS_SPC, NR ) )
         ALLOCATE( TEMP_PRD_COEF( N_SS_SPC, NR ) )

         DO SS1 = 1, N_SS_SPC
            DO NRX = 1, NR
               TEMP_RCT_COEF( SS1, NRX ) = SS_RCT_COEF( SS1, NRX )
               TEMP_PRD_COEF( SS1, NRX ) = SS_PRD_COEF( SS1, NRX )
            ENDDO
         ENDDO

         DO SS1 = 1, N_SS_SPC
            DO NRX = 1, NR
               SS_RCT_COEF( SS1, NRX ) = TEMP_RCT_COEF( RSLVD_IND( SS1 ), NRX ) 
               SS_PRD_COEF( SS1, NRX ) = TEMP_PRD_COEF( RSLVD_IND( SS1 ), NRX )
            ENDDO
         ENDDO

         DEALLOCATE( TEMP_RCT_COEF )
         DEALLOCATE( TEMP_PRD_COEF )

      ENDIF



1000  CONTINUE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find the first reaction in which each SS species appears (for the SPCS.EXT file output)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SS1RX = 0     !  Array

      DO SS1 = 1, N_SS_SPC

         DO NRX = 1, NR

            IF( SS_RCT_COEF( SS1, NRX ) .GT. 0 .OR. SS_PRD_COEF( SS1, NRX ) .GT. 0.0 ) THEN
                SS1RX( SS1 ) = NRX
                EXIT
            ENDIF

         ENDDO

      ENDDO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Deallocate and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 

      DEALLOCATE ( SS_SPC_IND )
      DEALLOCATE ( NUM_DEPEND )
      DEALLOCATE ( LRSLVD )
      DEALLOCATE ( LREACT )
      DEALLOCATE ( LPROD )
      DEALLOCATE ( SS_SPC_TEMP )
      DEALLOCATE ( SS_DEPEND )

      RETURN       



 9000 FORMAT( / 5X, '*** ERROR: The following species defined as steady-state are not in '
     &              'the mechanism:')

 9020 FORMAT( / 5X, '*** ERROR: The following reactions have more than 1 steady-state '
     &              'species reacting:')
 9040 FORMAT( / 5X, '*** ERROR: The following reactions have a steady-state species '
     &              'that is both a reactant and product:')
 9060 FORMAT( / 5X, '*** ERROR: The following steady-state species do not appear as'
     &              ' both a reactant and a product')

 9500 FORMAT( /10X, 'Steady-state species number ', I3,': ', A )
 9520 FORMAT( /10X, 'Reaction number ', I3,': <', A, '>' )
 9600 FORMAT( / 5X, '***ERROR: Could not resolve dependencies for the following '
     &              'steady-state species:' )
 9620 FORMAT( /10X, A )

10000 FORMAT( //5X, ' Abnormal termination of CHEMMECH ' )

      END

     

