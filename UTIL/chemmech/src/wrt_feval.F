
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
C $Header: /project/work/rep/arc/CCTM/src/gas/ros3/rbfeval.F,v 1.3 2011/10/21 16:11:10 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE WRT_FEVAL(  )

C***********************************************************************
C
C  Function:  Compute YDOT = dc/dt for each species. YDOT is the
C             net rate of change in species concentrations resulting
C             from chemical production minus chemical loss.
C
C  Preconditions: None
C                                                                     
C  Key Subroutines/Functions Called: None
C
C  Revision History: Prototype created by Jerry Gipson, August, 2004
C                    Based on the SMVGEAR code originally developed by 
C                    M. Jacobson, (Atm. Env., Vol 28, No 2, 1994).
C                    31 Jan 05 J.Young: get BLKSIZE from dyn alloc horizontal
C                    & vertical domain specifications module (GRID_CONF)
C                    28 Jun 10 J.Young: remove unnecessary modules and includes
C
C***********************************************************************

      USE MECHANISM_DATA   

      IMPLICIT NONE

C..Includes: None


C..Arguments:
      INTEGER NCSP                       ! Index of chem mech to use
                                         ! 1=gas/day, 2=gas/night

C..Parameters: None

C..External Functions: None

      INTEGER, EXTERNAL :: JUNIT   ! defines IO unit

C..Local Variables:
      INTEGER ISP              ! Loop index for species
      INTEGER ISP1, ISP2, ISP3 ! Pointers to species numbers
      INTEGER NP               ! Loop index for number of products
      INTEGER NR               ! Loop index for number of reactants
      INTEGER NRK              ! Reaction number
      INTEGER NRX              ! Loop index for number of reactions
      INTEGER IOUT
      INTEGER N_TERMS

      CHARACTER( 132 ), ALLOCATABLE :: STR_RXRAT ( : )      ! reaction rate strings
    
C***********************************************************************      

       IOUT = JUNIT()
       

       ALLOCATE ( STR_RXRAT( NRXNS)  )

       IF( .NOT. ALLOCATED( NET_EFFECT ) )THEN
           ALLOCATE( NET_EFFECT( ,)
       END IF

       IF( .NOT. ALLOCATED( NET_EFFECT ) )THEN
           ALLOCATE( NET_RCOEFF( ,) )
       END IF

       NET_EFFECT    = 0     ! default setting, species not net reactant or product
       NET_RCOEFF = 0.0D0 ! initialize   
               
       NCSP = 1
        OPEN(IOUT,FILE = TRIM(OUTDIR) // '/evaluate_dydt_new_sort.F', STATUS='UNKNOWN')

       WRITE(IOUT, 95550)
!      WRITE(IOUT, 94998)
94998 FORMAT(/7X,'IF ( NSPECIAL_RXN .GT. 0 ) CALL EVAL_SPECIAL( YIN )',
     &          4X,'! calculate special rate coefficients '/)
     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize dc/dt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


!      WRITE(IOUT, 94999)
94999 FORMAT(/'c  Initialize dc/dt to zero'
     &      /7X,'YDOT = 0.0D+0 ')
     

       WRITE( IOUT,85001)
85001 FORMAT(2/ 11X,'IF( NCSP .EQ. 1 )THEN ! set sunlight dependent rates' /)

      DO 150 NRX = 1, NUSERAT( NCSP )

         NRK = NKUSERAT( NRX,NCSP )

         IF ( BTEST ( IRXBITS( NRX ),1 ) ) THEN

c..write reaction rates
             IF ( NREACT( NRK ) .EQ. 1 ) THEN
                ISP1 = IRM2( NRK, 1, NCS )
                WRITE(STR_RXRAT( NRK ),95000)NRK, ISP1 ! , 'Reaction ' // RXLABEL( NRK )
             ELSE IF ( NREACT( NRK ) .EQ. 2 ) THEN
                ISP1 = IRM2( NRK,1,NCS )
                ISP2 = IRM2( NRK,2,NCS )
                WRITE(STR_RXRAT( NRK ),95001)NRK, ISP1, ISP2 ! , 'Reaction ' // RXLABEL( NRK )
             ELSE IF ( NREACT( NRK ) .EQ. 3 ) THEN
                ISP1 = IRM2( NRK,1,NCS )
                ISP2 = IRM2( NRK,2,NCS )
                ISP3 = IRM2( NRK,3,NCS )
                WRITE(STR_RXRAT( NRK ),95002)NRK, ISP1, ISP2, ISP3   ! , 'Reaction ' // RXLABEL( NRK )
             ELSE IF ( NREACT( NRK ) .EQ. 0 ) THEN
                WRITE(STR_RXRAT( NRK ),95003)NRK ! , 'Reaction ' // RXLABEL( NRK )
             END IF
             WRITE( IOUT,85002)NRK,TRIM( STR_RXRAT( NRK ) )
85002        FORMAT(14X,'RXRAT( NCELL, ', I4,') = ',A)   

         END IF
         
150   CONTINUE    ! END LOOP for writing sunlight dependent rates

      WRITE( IOUT,85003)
85003 FORMAT(/ 11X,'END IF' / )
     
      
      DO 100 NRX = 1, NUSERAT( NCSP )

         NRK = NKUSERAT( NRX,NCSP )

         IF ( .NOT. BTEST ( IRXBITS( NRX ),1 ) ) THEN
 
c..write reaction rates
             IF ( NREACT( NRK ) .EQ. 1 ) THEN
                ISP1 = IRM2( NRK, 1, NCS )
                WRITE(STR_RXRAT( NRK ),95000)NRK, ISP1 ! , 'Reaction ' // RXLABEL( NRK )
95000           FORMAT('RKI( NCELL,', I4,' ) * YIN( NCELL, ', I4,' )  ')
             ELSE IF ( NREACT( NRK ) .EQ. 2 ) THEN
                ISP1 = IRM2( NRK,1,NCS )
                ISP2 = IRM2( NRK,2,NCS )
                WRITE(STR_RXRAT( NRK ),95001)NRK, ISP1, ISP2 ! , 'Reaction ' // RXLABEL( NRK )
95001           FORMAT('RKI( NCELL,', I4,' ) * YIN( NCELL,', I4,' ) * YIN( NCELL, ', I4, ' )  ' )
                
             ELSE IF ( NREACT( NRK ) .EQ. 3 ) THEN
                ISP1 = IRM2( NRK,1,NCS )
                ISP2 = IRM2( NRK,2,NCS )
                ISP3 = IRM2( NRK,3,NCS )
                WRITE(STR_RXRAT( NRK ),95002)NRK, ISP1, ISP2, ISP3   ! , 'Reaction ' // RXLABEL( NRK )
95002           FORMAT('RKI( NCELL, ', I4,' ) * YIN( NCELL, ', I4,' ) * YIN( NCELL, ', I4, ' ) * YIN( NCELL, ', I4, ' )  ' )
             ELSE IF ( NREACT( NRK ) .EQ. 0 ) THEN
                WRITE(STR_RXRAT( NRK ),95003)NRK ! , 'Reaction ' // RXLABEL( NRK )
95003           FORMAT('RKI( NCELL, ', I4,' ) ')
             END IF
             WRITE( IOUT,85000)NRK,TRIM( STR_RXRAT( NRK ) )
85000        FORMAT(11X,'RXRAT( NCELL, ', I4,') = ',A)   

         END IF ! END LOOP for writing sunlight independent rates

100   CONTINUE

      WRITE(IOUT,'(/ 7X,"END DO LOOP_RATES")')

      DO NRX = 1, NUSERAT( NCSP )

         NRK = NKUSERAT( NRX,NCSP )

C...Set NET_EFFECT for reactants 

         DO NP = 1, NPRDCT( NRK )
            ISP1 = IRM2( NRK,NP+3,NCS )
            
            NET_EFFECT( ISP1, NRK )    = 3
            
            NET_RCOEFF( ISP1, NRK ) = NET_RCOEFF( ISP1, NRK )
     &                                 + REAL(SC( NRK,NP ), 8)
         END DO

C..Check whether reaction has a species as both reactant and product
         
         DO NR = 1, NREACT( NRK )

            ISP = IRM2( NRK,NR,NCS )
            NET_RCOEFF( ISP, NRK ) = NET_RCOEFF( ISP, NRK ) - 1.0D0
            DO NP = 1, NPRDCT( NRK )
               ISP1 = IRM2( NRK,NP+3,NCS )

               IF( ISP .EQ. ISP1 )THEN
                   IF( NET_RCOEFF( ISP, NRK ) .EQ. 0.0D0 )THEN ! reaction has no net effect

                       NET_EFFECT( ISP, NRK ) = 0

                   ELSE IF( NET_RCOEFF( ISP, NRK ) .LT. 0.0D0 )THEN ! net loss

                       IF( NET_RCOEFF( ISP, NRK ) .EQ. -1.0D0 )THEN ! only loss process
                           NET_EFFECT( ISP, NRK ) = -1
                       ELSE
                           NET_EFFECT( ISP, NRK ) = -2
                       END IF

                   ELSE IF( NET_RCOEFF( ISP, NRK ) .GT. 0.0D0 )THEN ! loss is not 100% 

                       NET_EFFECT( ISP, NRK ) = 2

                   END IF
               END IF
            END DO
            IF( NET_RCOEFF( ISP, NRK ) .LT. 0.0D0 )THEN
                IF( NET_RCOEFF( ISP, NRK ) .EQ. -1.0D0 )THEN
                     NET_EFFECT( ISP, NRK ) = -1
                ELSE
                     NET_EFFECT( ISP, NRK ) = -2
                END IF
            END IF                  
         END DO                      

         WRITE(6,'(5A,I2,A,ES12.4)')'For reactant ', TRIM(MECHANISM_SPC( INEW2OLD(ISP, NCS) )),' : reaction ',
     &   RXLABEL( NRK ),' NET_EFFECT = ',NET_EFFECT( ISP, NRK ),' NET_RCOEFF = ', NET_RCOEFF( ISP, NRK )
                       
      END DO  ! END LOOP for determining net efffect of each reaction on species
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  write expression to calculate dc/dt values
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO ISP = 1, ISCHAN

         N_TERMS = 0
         
         DO 200 NRX = 1, NUSERAT( NCSP )

            NRK = NKUSERAT( NRX,NCSP )

             IF( NET_EFFECT( ISP, NRK ) .EQ. 0 )CYCLE
             
                  IF( N_TERMS .EQ. 0 )THEN
                      WRITE(IOUT,95008)MECHANISM_SPC( INEW2OLD(ISP, NCS) )
                      WRITE(IOUT,'(7X,"LOOP_",A,": DO NCELL = 1, NUMCELLS")')
     &                 TRIM(MECHANISM_SPC( INEW2OLD(ISP, NCS) ))
                        WRITE(IOUT,95004)ISP, ISP
                   END IF
                   SELECT CASE( NET_EFFECT( ISP, NRK ) )
                      CASE( -1 )
!                          WRITE(IOUT, 95005)TRIM(STR_RXRAT(NRK )), 
!    &                   'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
                          N_TERMS = N_TERMS + 1
                          WRITE(IOUT, 95025)NRK, 
     &                   'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
                      CASE( -2 )
!                          WRITE(IOUT, 95016)ABS(NET_RCOEFF( ISP, NRK )),
!     &                    TRIM(STR_RXRAT(NRK )), 'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
                          N_TERMS = N_TERMS + 1
                          WRITE(IOUT, 95036)ABS(NET_RCOEFF( ISP, NRK )),
     &                    NRK, 'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
                      CASE( 2, 3 )
                          IF( NET_RCOEFF( ISP, NRK ) .NE. 1.0D0 )THEN
!                             WRITE(IOUT, 95006)NET_RCOEFF( ISP, NRK ), TRIM(STR_RXRAT( NRK )),
!     &                      'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
                             WRITE(IOUT, 95026)NET_RCOEFF( ISP, NRK ), NRK,
     &                      'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
                          ELSE 
!                             WRITE(IOUT, 95017)TRIM(STR_RXRAT( NRK )),
!     &                      'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
                             WRITE(IOUT, 95037)NRK,
     &                      'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
                          END IF
                          N_TERMS = N_TERMS + 1
!                      CASE( 3 )
!                          IF( NET_RCOEFF( ISP, NRK ) .NE. 1.0D0 )THEN
!                             WRITE(IOUT, 95006)NET_RCOEFF( ISP, NRK ), TRIM(STR_RXRAT( NRK )),
!     &                      'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
!                          ELSE 
!                             WRITE(IOUT, 95017)TRIM(STR_RXRAT( NRK )),
!     &                      'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
!                         END IF
!                          N_TERMS = N_TERMS + 1
                  END SELECT
            
             
            
!            IF( NET_EFFECT( ISP, NRK ) .EQ. 0 )CYCLE
            
!c..Subtract loss terms from dc/dt for this reaction 
!            DO NR = 1, NREACT( NRK )
!               ISP1 = IRM2( NRK,NR,NCS )
!               IF(NET_EFFECT( ISP, NRK ) .EQ. -2)CYCLE
!               IF( ISP1 .EQ. ISP )THEN
!                   IF( N_TERMS .EQ. 0 )THEN
!                       WRITE(IOUT,95008)MECHANISM_SPC( INEW2OLD(ISP1, NCS) )
!                       WRITE(IOUT,95004)ISP, ISP
!                   END IF
!                   IF( NET_EFFECT( ISP, NRK ) .EQ. 3 .OR. NET_EFFECT( ISP, NRK ) .EQ. -1)THEN
!                       WRITE(IOUT, 95005)TRIM(STR_RXRAT(NRK )),
!     &                 'RXN_LABEL ' // TRIM(RXLABEL( NRK )) // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
!                        N_TERMS = N_TERMS + 1
!                   END IF
!               END IF
!            END DO
  
!c..Add production terms to dc/dt for this reaction
!            DO NP = 1, NPRDCT( NRK )

!               IF( NET_EFFECT( ISP, NRK ) .EQ. -1)CYCLE

!               ISP1 = IRM2( NRK,NP+3,NCS )
!                IF( ISP1 .EQ. ISP )THEN
!                   IF( N_TERMS .EQ. 0 )THEN
!                       WRITE(IOUT,95008)MECHANISM_SPC( INEW2OLD(ISP1, NCS) )
!                       WRITE(IOUT,95004)ISP, ISP
!                   END IF
!                   SELECT CASE( NET_EFFECT( ISP, NRK ) )
!                      CASE( -2 )
!                          WRITE(IOUT, 95016)ABS(REAL(SC( NRK,NP ), 8) + 1.0D0),
!     &                    TRIM(STR_RXRAT(NRK )), 'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
!                          N_TERMS = N_TERMS + 1
!                      CASE( 2 )
!                          WRITE(IOUT, 95016)ABS(REAL(SC( NRK,NP ), 8) - 1.0D0),
!     &                    TRIM(STR_RXRAT(NRK )), 'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
!                          N_TERMS = N_TERMS + 1
!                      CASE( 3 )
!                          IF( REAL(SC( NRK,NP ), 8) .NE. 1.0D0 )THEN
!                             WRITE(IOUT, 95006)REAL(SC( NRK,NP ), 8), TRIM(STR_RXRAT( NRK )),
!     &                      'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
!                          ELSE 
!                             WRITE(IOUT, 95017)TRIM(STR_RXRAT( NRK )),
!     &                      'RXN_LABEL ' // TRIM(RXLABEL( NRK ))  // ' NET_EFFECT = ',NET_EFFECT( ISP, NRK )
!                          END IF
!                          N_TERMS = N_TERMS + 1
!                  END SELECT
!               END IF
!            END DO
            
95008       FORMAT(/ 'C... dc/dt for ', A )
95004       FORMAT(11X,'YDOT( NCELL, ', I4, ' ) = YDOT( NCELL, ', I4, ' ) ')
95005       FORMAT(5X,'&',11X,'        - ', A, 14X,' ! ',A,I4)
95006       FORMAT(5X,'&',11X,'        + ', 1PD10.4,' * ', A,' ! ', A, I4)
95016       FORMAT(5X,'&',11X,'        - ', 1PD10.4,' * ', A,' ! ', A, I4)
95017       FORMAT(5X,'&',17X,'        + ', A, 14X,' ! ', A, I4)
95025       FORMAT(5X,'&',17X,'        - RXRAT( NCELL, ', I4,' ) ! ', 13X, A,I4)
95026       FORMAT(5X,'&',17X,'        + ', 1PD10.4,' * RXRAT( NCELL, ', I4 ,' ) ! ', A, I4)
95036       FORMAT(5X,'&',17X,'        - ', 1PD10.4,' * RXRAT( NCELL, ', I4 ,' ) ! ', A, I4)
95037       FORMAT(5X,'&',17X,'        + RXRAT( NCELL, ', I4,' ) ! ', 13X, A, I4)
200     CONTINUE
        IF( N_TERMS .GT. 0 )THEN
           WRITE(IOUT,'(7X,"END DO LOOP_",A /)')TRIM(MECHANISM_SPC( INEW2OLD(ISP, NCS) ))
        END IF
      END DO
      
      WRITE(IOUT, 97911)

95550 FORMAT(7X,'SUBROUTINE EVALUATE_DYDT( RKI, YIN, YDOT, NUMCELLS, NCSP )'
     &  /'C***********************************************************************' 
     &  /'C'
     &  /'C  Function:  Compute YDOT = dc/dt for each species. YDOT is the'
     &  /'C             net rate of change in species concentrations resulting'
     &  /'C             from chemical production minus chemical loss.'
     &  /'C'
     &  /'C  Preconditions: None'
     &  /'C'
     &  /'C  Key Subroutines/Functions Called: None'
     &  /'C'
     &  /'C'
     &  /'C***********************************************************************' 
     &  /7X,'USE GRID_CONF           ! horizontal & vertical domain specifications',
     &  /7X,'IMPLICIT NONE'/
     &  /'C..Includes:'
     &  /7X,'INCLUDE SUBST_RXCMMN ' //
     &  /'C... arguments'
     &  /7X,'REAL( 8 ), INTENT(  IN )  ::   YIN( :, : )     ! Species concs, ppm'
     &  /7X,'REAL( 8 ), INTENT(  IN )  ::   RKI( :, : )     ! Reaction Rate Constant so YDOTs are in ppm/min'
     &  /7X,'REAL( 8 ), INTENT( OUT )  ::   YDOT( :, : )    ! Species rates of change, ppm/min'
     &  /7X,'INTEGER,   INTENT(  IN  ) ::   NUMCELLS        ! Number of cells in block'
     &  /7X,'INTEGER,   INTENT(  IN  ) ::   NCSP            ! Indicates sunlite conditions, 1=yes, 2=no'
     &  /'C... local'
     &  /7X,'INTEGER   ISP'/
     &  /7X,'REAL( 8 ) RXRAT( BLKSIZE, NRXNS )' /
     &  /7X, 'INTEGER NCELL                ! Loop index for # of cells in the block' ////
     &  /'c  Initialize reaction rates and dc/dt to zero'
     &  /7X,'RXRAT = 0.0D+0 '
     &  /7X,'YDOT  = 0.0D+0 '
     &  //7X,'IF ( NSPECIAL_RXN .GT. 0 ) CALL EVAL_SPECIAL( YIN )',
     &    4X,'! calculate special rate coefficients '/
     &  //7X,'LOOP_RATES: DO NCELL = 1, NUMCELLS ' / )
     

97911   FORMAT(// 7X
     &          / 7X, 'RETURN'
     &          / 7X, 'END SUBROUTINE EVALUATE_DYDT' )

      CLOSE(IOUT)
      RETURN
      END

