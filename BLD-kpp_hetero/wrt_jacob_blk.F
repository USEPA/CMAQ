       SUBROUTINE WRT_JACOB_BLK( )

C***********************************************************************
C
C  Function: write subroutines that calculate the
C  Jacobian vector, [J] ( Jij = d[dCi/dt]/dCj )
C
C  Preconditions: None
C
C  Key Subroutines/Functions Called: None
C
C  Revision History: Prototype created by Bill Hutzell, March 2013
C                    Based on the SMVGEAR code originally developed by 
C                    M. Jacobson, (Atm. Env., Vol 28, No 2, 1994).
C
C***********************************************************************

      USE MECHANISM_DATA


      IMPLICIT NONE

C..Includes:   None

C..Arguments:  None

C..Parameters: None

C..External Functions: None

      INTEGER, EXTERNAL :: JUNIT   ! defines IO unit

C..Local Variables:
      INTEGER NMECH          ! Loop index for chem mech to use; 1=gas/day, 2=gas/night
      INTEGER IALP           ! Pointer to location of PD term in EXPLIC
      INTEGER IAR            ! Loop index for entries in [P]
      INTEGER IARC           ! captures values for entries in [P]
      INTEGER IARP           ! Pointer to location of PD term in [P]
      INTEGER IARRY          ! Pointer to end of [P] entries
      INTEGER ISCP           ! Pointer to stoichiometric coefficient
!      INTEGER ISPC           ! Loop index for species
      INTEGER JR1, JR2, JR3  ! Pointer to reactant species conc.
      INTEGER NL             ! Loop index for loss PD terms
      INTEGER NLD            ! Number of loss PD terms for each rxn.
      INTEGER NP             ! Loop index for prod PD terms
      INTEGER NPD            ! Number of prod PD terms for each rxn.
      INTEGER NRK            ! Reaction number
      INTEGER NRX            ! Loop index for number of reactions
      INTEGER NONDIAG        ! Pointer to end of off-diagonal entries
      INTEGER NONDIAG1       ! Pointer to start of diagonal entries
      INTEGER IOUT           ! Output unit
      INTEGER NTERMS         ! counts terms in vector component
      INTEGER MTERMS         ! counts terms in vector component
      
      INTEGER PDCOUNT        ! counts the number of partial derivatives
      INTEGER N1COUNT        ! counts negative one times partial derivative additions
      INTEGER P1COUNT        ! counts positive one times partial derivative additions
      INTEGER COCOUNT        ! counts other number times partial derivative additions


      INTEGER,          ALLOCATABLE ::     RXN_PD( :, : ) ! 
      REAL( 8 ),        ALLOCATABLE ::      FRACN( :, : ) ! Stoichiometric coeff. times b*h
      CHARACTER( 132 ), ALLOCATABLE :: CC2_STRING( :, : ) !

      CHARACTER( 132 ) :: STR_EXPLIC( 3 )   ! Reaction partial derivative terms
      CHARACTER(  16 ) :: STR_FRACN
      
      LOGICAL          :: WRITE_TO_CC2

C***********************************************************************

          IOUT = JUNIT()
          ALLOCATE( RXN_PD( 3, NRXNS ), FRACN( 3, NRXNS ), CC2_STRING( 3, NRXNS ) )          
          
          LOOP_NMECH: DO NMECH = 1, 2
          
             IF( NMECH .LT. 2 )THEN
                 OPEN(IOUT,FILE = TRIM(OUTDIR) // '/light_jacobian-new.f', STATUS='UNKNOWN')
                 WRITE(IOUT,97547)
             ELSE
                 OPEN(IOUT,FILE = TRIM(OUTDIR) // '/night_jacobian-new.f', STATUS='UNKNOWN')
                 WRITE(IOUT,97548)
             END IF
             
             WRITE(IOUT,97549)
             WRITE(IOUT,97950)
             WRITE(IOUT,97801)
             WRITE(IOUT,97802)
             WRITE(IOUT,97501)NPDERIV 
             WRITE(IOUT,97902)
           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Zero out Jacobian ( stored in sparse matrix array cc2 )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IARRY    = IARRAY( NMECH ) 
            NONDIAG  = IARRY - ISCHAN  
            NONDIAG1 = NONDIAG + 1
            PDCOUNT  = 0
            RXN_PD   = 0

            DO NRX = 1, NUSERAT( NMECH )

                NRK     = NKUSERAT( NRX,NMECH )
                     
                     
C...build strings for partial derivatives

                 SELECT CASE ( NREACT( NRK ) )
                    CASE( 1 )
c...partial derivative term for reactions with 1 reactant
                       PDCOUNT           = PDCOUNT + 1
                       RXN_PD( 1 , NRK ) = PDCOUNT
                       WRITE( IOUT, 84999) PDCOUNT, NRK
84999                  FORMAT(17X,'PARTDER( ', I5,' ) = RKI( NCELL, ', I4,' ) ')
                    CASE( 2 )
c...partial derivative terms for reactions with 2 reactants
                       JR1 = IRM2( NRK,1,NCS )
                       JR2 = IRM2( NRK,2,NCS )
                       PDCOUNT = PDCOUNT + 1
                       RXN_PD( 1 , NRK ) = PDCOUNT
                       WRITE( IOUT, 85000) PDCOUNT, NRK, JR2
                       PDCOUNT = PDCOUNT + 1
                       RXN_PD( 2 , NRK ) = PDCOUNT
                       WRITE( IOUT, 85000) PDCOUNT, NRK, JR1
85000                  FORMAT(17X,'PARTDER( ', I5,' ) = RKI( NCELL, ', I4, ' ) * YIN( NCELL, ', I4,' ) ') 
                    CASE( 3 )
c.....partial derivative terms for reactions with 3 reactants
                       JR1 = IRM2( NRK,1,NCS )
                       JR2 = IRM2( NRK,2,NCS )
                       JR3 = IRM2( NRK,3,NCS )
                       WRITE( IOUT,* )JR1, JR2, JR3
                       PDCOUNT = PDCOUNT + 1
                       RXN_PD( 1 , NRK ) = PDCOUNT
                       WRITE( IOUT, 85001) PDCOUNT, NRK, JR2, JR3
                       PDCOUNT = PDCOUNT + 1
                       RXN_PD( 2 , NRK ) = PDCOUNT
                       WRITE( IOUT, 85001) PDCOUNT, NRK, JR1, JR3
                       PDCOUNT = PDCOUNT + 1
                       RXN_PD( 3 , NRK ) = PDCOUNT
                       WRITE( IOUT, 85001) PDCOUNT, NRK, JR2, JR1
85001                  FORMAT(17X,'PARTDER( ', I5,' ) = RKI( NCELL, ', I4,' ) * YIN( NCELL,', I4,' )'
     &                        '* YIN( NCELL, ', I4, ' )' )
                 END SELECT
            END DO

            WRITE(IOUT,97803)

            WRITE(IOUT,90540)IARRAY( NMECH ),(IARRAY( NMECH )-ISCHANG( NCS )),
     &      (IARRAY( NMECH )-ISCHANG( NCS ))+1,IARRAY( NMECH )
     
90540       FORMAT(/ 17X, 'IARRY = ' , I6
     &             / 17X, 'NONDIAG = IARRY - ISCHAN' 
     &             / 17X, 'NONDIAG1 = NONDIAG + 1' /
     &             / 17X, 'DO IAR = 0, ' , I5
     &             / 21X, 'CC2( NCELL, IAR ) = 0.0D+0' 
     &             / 17X, 'END DO ' 
     &             / 17X, 'DO IAR = ' , I5 , ', ', I5
     &             / 21X, 'CC2( NCELL, IAR ) = 0.0D+0' 
     &             / 17X, 'END DO' /)     

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over reaction rates adding partial derivative terms; EXPLIC
c  holds the PD terms according to number of reactants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            DO 340 IAR = 0, IARRY
              
               IF( IAR .GT. NONDIAG .AND. IAR .LT. NONDIAG1 )CYCLE
               
               NTERMS  = 0
               MTERMS  = 0
               N1COUNT = 0
               P1COUNT = 0
               COCOUNT = 0
                 
               FRACN      = 0.0D0
               CC2_STRING = 'BLANK'
               
               DO 240 NRX = 1, NUSERAT( NMECH )

                     NRK     = NKUSERAT( NRX,NMECH )
                     
                    
C...build strings for partial derivatives

                     SELECT CASE ( NREACT( NRK ) )
                       CASE( 1 )
c...partial derivative term for reactions with 1 reactant
                          WRITE( STR_EXPLIC( 1 ), 94999)NRK
94999                     FORMAT('RKI( NCELL, ', I4,' ) ')
                       CASE( 2 )
c...partial derivative terms for reactions with 2 reactants
                          JR1 = IRM2( NRK,1,NCS )
                          JR2 = IRM2( NRK,2,NCS )
                          WRITE( STR_EXPLIC( 1 ), 95000)NRK, JR2
                          WRITE( STR_EXPLIC( 2 ), 95000)NRK, JR1
95000                     FORMAT('RKI( NCELL, ', I4, ' ) * YIN( NCELL, ', I4,' ) ') 
                      CASE( 3 )
c.....partial derivative terms for reactions with 3 reactants
                          JR1 = IRM2( NRK,1,NCS )
                          JR2 = IRM2( NRK,2,NCS )
                          JR3 = IRM2( NRK,3,NCS )
                          WRITE( STR_EXPLIC( 1 ), 95001) NRK, JR2, JR3
                          WRITE( STR_EXPLIC( 2 ), 95001) NRK, JR1, JR3
                          WRITE( STR_EXPLIC( 3 ), 95001) NRK, JR2, JR1
95001                     FORMAT('RKI( NCELL, ', I4,' ) * YIN( NCELL,', I4,' )* YIN( NCELL, ', I4, ' )' )
                     END SELECT
                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Add PD terms to [J] for this reaction
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c...loss terms
                     WRITE_TO_CC2 = .FALSE.
                     
                     NLD = NDERIVL( NRK,NMECH )         
                     DO NL = 1, NLD
                        IARP = JARRL( NRK,NL,NMECH )
                        IALP = JLIAL( NRK,NL,NMECH )
                        IF( IAR .EQ. IARP )THEN
                            IARC = IARP
                            PDCOUNT = RXN_PD( IALP , NRK)
                            WRITE_TO_CC2 = .TRUE.
!                            IF(NTERMS .LT. 1)WRITE(IOUT,98801)IARP, IARP
                            FRACN( IALP, NRK ) = -1.0D0 + FRACN( IALP, NRK )
                            WRITE(CC2_STRING( IALP, NRK ),'(A)')TRIM(STR_EXPLIC( IALP ))
                            NTERMS = NTERMS + 1
!                            WRITE(IOUT,98802)TRIM(STR_EXPLIC( IALP ))
                         END IF
                     END DO    ! End loop over loss terms
                     
c...production terms with stoichiomteric coeff EQ 1.0 and NE 1.0
                     NPD = NDERIVP( NRK,NMECH )
                     DO NP = 1, NPD
                        IARP = JARRP( NRK,NP,NMECH )
                        IALP = JPIAL( NRK,NP,NMECH )
                        IF ( ICOEFF( NRK,NP,NMECH ) .EQ. 0 ) THEN
c..production terms with unit stoichiometry
                           IF( IAR .EQ. IARP )THEN
                               IARC = IARP
                               PDCOUNT = RXN_PD( IALP , NRK)
                               WRITE_TO_CC2 = .TRUE.
!                               IF(NTERMS .LT. 1)WRITE(IOUT,98801)IARP, IARP
                               FRACN( IALP, NRK ) = FRACN( IALP, NRK ) + 1.0D0
!                               WRITE(IOUT,98803)TRIM(STR_EXPLIC( IALP ))
                               WRITE(CC2_STRING( IALP, NRK ),'(A)')TRIM(STR_EXPLIC( IALP ))
                               NTERMS = NTERMS + 1
                           END IF
                        ELSE
c..production terms with non-unit stoichiometry
                            ISCP = ICOEFF( NRK,NP,NMECH )
                            WRITE(STR_FRACN,'(D10.4)')REAL(SC( NRK,ISCP ), 8)
                            IF( IAR .EQ. IARP )THEN
                              IARC = IARP
                              PDCOUNT = RXN_PD( IALP , NRK)
                              WRITE_TO_CC2 = .TRUE.
                              FRACN( IALP, NRK ) = FRACN( IALP, NRK ) + REAL(SC( NRK,ISCP ), 8)
!                              IF(NTERMS .LT. 1)WRITE(IOUT,98801)IARP, IARP
                              NTERMS = NTERMS + 1
                              WRITE(CC2_STRING( IALP, NRK ),'(A)')TRIM(STR_EXPLIC( IALP ))
!                              WRITE(IOUT,98804)TRIM(STR_FRACN), TRIM(STR_EXPLIC( IALP )) 
                            END IF
                        END IF
                     END DO      ! End loop over production terms

                     
                     IF( WRITE_TO_CC2 )THEN
                        DO IALP = 1, 3
                          IF( FRACN( IALP, NRK ) .NE. 0.0D0 )THEN
                               IF(MTERMS .LT. 1)WRITE(IOUT,98801)IAR, IAR
                               MTERMS = MTERMS + 1
                               IF( ABS( FRACN( IALP, NRK ) ) .EQ. 1.0D0 )THEN
                                   IF( FRACN( IALP, NRK ) .GT. 0.0D0 )THEN
                                       P1COUNT = P1COUNT + 1
                                       NDERIVP1(     IAR, NMECH )      = P1COUNT
                                       PDERIVP1( P1COUNT, IAR, NMECH ) = RXN_PD( IALP , NRK) ! PDCOUNT
!                                       WRITE(IOUT,98803)TRIM(CC2_STRING( IALP, NRK )) 
                                       WRITE(IOUT,88803)RXN_PD( IALP , NRK), TRIM(CC2_STRING( IALP, NRK ))
                                   ELSE IF( FRACN( IALP, NRK ) .LT. 0.0D0 )THEN
                                       N1COUNT = N1COUNT + 1
                                       NDERIVN1(     IAR, NMECH )      = N1COUNT
                                       PDERIVN1( N1COUNT, IAR, NMECH ) = RXN_PD( IALP , NRK) ! PDCOUNT
!                                       WRITE(IOUT,98802)TRIM(CC2_STRING( IALP, NRK )) 
                                       WRITE(IOUT,88802)RXN_PD( IALP , NRK), TRIM(CC2_STRING( IALP, NRK ))
                                   END IF
                               ELSE
                                   WRITE(STR_FRACN,'(1PD10.4)') ABS( FRACN( IALP, NRK ) )
                                   COCOUNT = COCOUNT + 1
                                   NDERIVCO(     IAR, NMECH )      = COCOUNT
                                   PDERIVCO( COCOUNT, IAR, NMECH ) = RXN_PD( IALP , NRK) ! PDCOUNT
                                   PD_COEFF( COCOUNT, IAR, NMECH ) = FRACN( IALP, NRK )
                                   IF( FRACN( IALP, NRK ) .GE. 0.0D0 )THEN
!                                       WRITE(IOUT,98804)TRIM(STR_FRACN), TRIM(CC2_STRING( IALP, NRK )) 
                                       WRITE(IOUT,88804)TRIM(STR_FRACN), RXN_PD( IALP , NRK), TRIM(CC2_STRING( IALP, NRK )) ! PDCOUNT
                                   ELSE IF( FRACN( IALP, NRK ) .LT. 0.0D0 )THEN
!                                       WRITE(IOUT,98808)TRIM(STR_FRACN), TRIM(CC2_STRING( IALP, NRK )) 
                                       WRITE(IOUT,88808)TRIM(STR_FRACN), RXN_PD( IALP , NRK), TRIM(CC2_STRING( IALP, NRK )) ! PDCOUNT
                                   END IF
                               END IF
                          END IF
                        END DO
                     END IF
                     
240            CONTINUE      ! End loop over reactions

                
!               WRITE(IOUT,*)'! NDERIVP1(     IAR, NMECH ) = ',NDERIVP1( IAR, NMECH )  
!               WRITE(IOUT,'("C PDERIVP1(  :, IAR, NMECH ) = ",I5)')PDERIVP1(  1:NDERIVP1( IAR, NMECH ), IAR, NMECH )
!               WRITE(IOUT,*)"! NDERIVN1(     IAR, NMECH ) = ",NDERIVN1( IAR, NMECH )  
!               WRITE(IOUT,'("C PDERIVN1(  :, IAR, NMECH ) = ",I5)')PDERIVN1(  1:NDERIVN1( IAR, NMECH ), IAR, NMECH )
!               WRITE(IOUT,*)"! NDERIVCO(     IAR, NMECH ) = ",NDERIVCO(     IAR, NMECH )  
!               WRITE(IOUT,'("C PDERIVCO(  :, IAR, NMECH ) = ",I5)')PDERIVCO( 1 :NDERIVCO( IAR, NMECH ), IAR, NMECH )
!               WRITE(IOUT,'("C PD_COEFF(  :, IAR, NMECH ) = ",PD12.4)')PD_COEFF(  1:NDERIVCO( IAR, NMECH ), IAR, NMECH )


340         CONTINUE    ! End loop over matrix position

            SELECT CASE ( NMECH )
               CASE ( 1 )
                  WRITE(IOUT, 97911)
               CASE ( 2 )
                  WRITE(IOUT, 97912)
            END SELECT
            
       END DO LOOP_NMECH  


!        WRITE(IOUT, 97910)

        CLOSE(IOUT) 
        
97547   FORMAT('      SUBROUTINE LIGHT_JACOB( RKI, YIN, CC2, NUMCELLS )' //
     &         'C     routine evaluate the Jabocian for day or light conditions.')
97548   FORMAT('      SUBROUTINE NIGHT_JACOB( RKI, YIN, CC2, NUMCELLS )' //
     &         'C     routine evaluate the Jabocian for night or dark conditions.')
     
97549   FORMAT(// 7X, 'IMPLICIT NONE '/)
97950   FORMAT('C..Arguments:' /)
97951   FORMAT('      INTEGER                  :: NCSP         ! Index of chem mech to use; 1=gas/day, 2=gas/night')
97801   FORMAT('      REAL( 8 ), INTENT( IN  ) :: YIN( :, : )     ! Species concs, ppm' /
     &         '      REAL( 8 ), INTENT( IN  ) :: RKI( :, : )     ! Reaction Rate Constant so YDOTs are in ppm/min'/
     &         '      REAL( 8 ), INTENT( OUT ) :: CC2( :, : )     ! Jacobian vectorized and sorted based on spareness'/ 
     &         '      INTEGER,   INTENT( IN  ) :: NUMCELLS        ! Number of cells in block' )
     
97802   FORMAT('C...Local:' 
     &                      / 7X,'REAL( 8 ), ALLOCATABLE, SAVE  :: PARTDER( : )   ! reaction partial derivatives, 1/min' /
     &                      / 7X,'INTEGER   NCELL              ! Loop index for cell number'
     &                      / 7X,'INTEGER   IAR                ! Loop index for vector component'
     &                      / 7X,'INTEGER   IARRY              ! Pointer to end of [P] entries'
     &                      / 7X,'INTEGER   NONDIAG            ! Pointer to end of off-diagonal entries'
     &                      / 7X,'INTEGER   NONDIAG1           ! Pointer to start of diagonal entries' ///)
97803   FORMAT(//'C  Zero out nondiagonal elements of Jacobian ( stored in sparse matrix array cc2 )' /)
97504   FORMAT('      NCSP = 2 ')        
97901   FORMAT('      NCSP = 1 ')        
97501   FORMAT( '      IF( .NOT. ALLOCATED( PARTDEV ) )THEN '
     &         /'           ALLOCATE( PARTDEV ( ', I6,' ) '
     &         /'      END IF ' 
     &        3/'      PARTDEV = 0.0D0' / )
            
97902   FORMAT('      LOOP_CELLS: DO NCELL = 1, NUMCELLS'
     &         //'C  Calculate partial derivatives' )
97903   FORMAT('C   Add PD terms to [J] for each reaction' /)

98801   FORMAT(/ 17X,'CC2( NCELL, ', I4,') = CC2( NCELL, ', I4,')' )

98802   FORMAT(5X,'&',29X,'- ',A)
98803   FORMAT(5X,'&',29X,'+ ',A)
98804   FORMAT(5X,'&',29X,'+ ',A,'*',A)
98808   FORMAT(5X,'&',29X,'- ',A,'*',A)

88802   FORMAT(5X,'&',29X,'- PARTDER( ', I5, ' )',13X,' ! ',A)
88803   FORMAT(5X,'&',29X,'+ PARTDER( ', I5, ' )',13X,' ! ',A)
88804   FORMAT(5X,'&',29X,'+ ',A,' * PARTDER( ', I5, ' )',' ! ',A)
88808   FORMAT(5X,'&',29X,'- ',A,' * PARTDER( ', I5, ' )',' ! ',A)

98805   FORMAT(17X,'CC2(NCELL, ', I4,') = CC2(NCELL, ', I4,') ','- ',A)
98806   FORMAT(17X,'CC2(NCELL, ', I4,') = CC2(NCELL, ', I4,') ','+ ',A)
98807   FORMAT(17X,'CC2(NCELL, ', I4,') = CC2(NCELL, ', I4,') + ',A,' * ',A)
98809   FORMAT(/ 7X,'CC2( NCELL, ', I4,') = 0.0D0 ' )
97910   FORMAT(7X,'END DO LOOP_CELLS')
97911   FORMAT(// 7X, 'RETURN' / 7X, 'END SUBROUTINE LIGHT_JACOB' )
97912   FORMAT(// 7X, 'RETURN' / 7X, 'END SUBROUTINE NIGHT_JACOB' )

      RETURN 
      END
