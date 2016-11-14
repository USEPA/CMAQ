
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
C $Header: /project/work/rep/arc/CCTM/src/gas/ros3/rbjacob.F,v 1.3 2011/10/21 16:11:10 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE WRT_JACOB( NCSP )

C***********************************************************************
C
C  Function: Compute the Jacobian matrix, [J] ( Jij = d[dCi/dt]/dCj )
C
C  Preconditions: None
C
C  Key Subroutines/Functions Called: None
C
C  Revision History: Prototype created by Jerry Gipson, August, 2004
C                    Based on the SMVGEAR code originally developed by 
C                    M. Jacobson, (Atm. Env., Vol 28, No 2, 1994).
C
C                    31 Jan 05 J.Young: get BLKSIZE from dyn alloc horizontal
C                    & vertical domain specifications module (GRID_CONF)
C                    28 Jun 10 J.Young: remove unneccesary modules and include files
C                    22 Aug 11 J.Young: fixed bug: initialize CC2( NCELL,0 )
C
C***********************************************************************

      USE CGRID_SPCS            ! CGRID mechanism species
      USE MECHANISM_DATA


      IMPLICIT NONE

C..Includes:

C..Arguments:
      INTEGER, INTENT( IN ) :: NCSP         ! Index of chem mech to use; 1=gas/day, 2=gas/night


C..Parameters: None

C..External Functions: None
C..External Functions: None

      INTEGER, EXTERNAL :: JUNIT   ! defines IO unit

C..Local Variables:
      INTEGER INCS           ! Loop index for day/night Jacobian
      INTEGER IALP           ! Pointer to location of PD term in EXPLIC
      INTEGER IAR            ! Loop index for non-zero entries in [P]
      INTEGER IARP           ! Pointer to location of PD term in [P]
      INTEGER IARRY          ! Pointer to end of [P] entries
      INTEGER ISCP           ! Pointer to stoichiometric coefficient
      INTEGER ISPC           ! Loop index for species
      INTEGER JR1, JR2, JR3  ! Pointer to reactant species conc.
      INTEGER NCELL          ! Loop index for number of cells
      INTEGER NL             ! Loop index for loss PD terms
      INTEGER NLD            ! Number of loss PD terms for each rxn.
      INTEGER NP             ! Loop index for prod PD terms
      INTEGER NPD            ! Number of prod PD terms for each rxn.
      INTEGER NRK            ! Reaction number
      INTEGER NRX            ! Loop index for number of reactions
      INTEGER NONDIAG        ! Pointer to end of off-diagonal entries
      INTEGER NONDIAG1       ! Pointer to start of diagonal entries
      INTEGER IOUT
      INTEGER NTERMS
      INTEGER NUMCELLS
      
      REAL( 8 ) :: CR2                   ! Temporary product for 3 reactant rxns
      REAL( 8 ) :: FRACN                 ! Stoichiometric coeff. times b*h
!     REAL( 8 ) :: EXPLIC( BLKSIZE,3 )   ! Reaction partial derivative terms

      CHARACTER( 132 ) :: STR_EXPLIC( 3 )   ! Reaction partial derivative terms
      CHARACTER(  16 ) :: STR_FRACN

C***********************************************************************

        IOUT = JUNIT()
        
        NUMCELLS = 1
        
        IF( NCSP .LT. 2 )THEN
            OPEN(IOUT,FILE = TRIM(OUTDIR) // '/light_jacobian2.f', STATUS='UNKNOWN')
            WRITE(IOUT,97547)
        ELSE
            OPEN(IOUT,FILE = TRIM(OUTDIR) // '/night_jacobian2.f', STATUS='UNKNOWN')
            WRITE(IOUT,97548)
        END IF
        WRITE(IOUT,97803)
        
        WRITE(IOUT,97549)
        WRITE(IOUT,97950)
!        WRITE(IOUT,97951)

        WRITE(IOUT,97801)
        WRITE(IOUT,97802)
        
!        IF( NCSP .LT. 2 )THEN
!            WRITE(IOUT,97901)
!        ELSE
!            WRITE(IOUT,97904)
!        END IF

        WRITE(IOUT,97803)


        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Zero out Jacobian ( stored in sparse matrix array cc2 )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IARRY = IARRAY( NCSP ) 
      NONDIAG = IARRY - ISCHAN  
      NONDIAG1 = NONDIAG + 1
!     DO IAR = 1, NONDIAG
!     DO IAR = 0, NONDIAG
!        DO NCELL = 1, NUMCELLS
!           CC2( NCELL,IAR ) = 0.0
!        END DO
!     END DO
     
!     DO IAR = NONDIAG1, IARRY
!        DO NCELL = 1, NUMCELLS
!           CC2( NCELL,IAR ) = 0.0
!        END DO
!     END DO

        WRITE(IOUT,9054)IARRAY( NCSP ),(IARRAY( NCSP )-ISCHANG( NCS )),
     &  (IARRAY( NCSP )-ISCHANG( NCS ))+1,IARRAY( NCSP )

9054    FORMAT(/ 7X, "IARRY = " , I5
     &         / 7X, "NONDIAG = IARRY - ISCHAN" 
     &         / 7X, "NONDIAG1 = NONDIAG + 1" /
     &         / 7X, "DO IAR = 0, " , I5
     &         / 11X,"CC2( IAR ) = 0.0D+0" 
     &         / 7X, "END DO "
     &         / 7X, "DO IAR = " , I5 , ", ", I5
     &         / 11X,"CC2( IAR ) = 0.0D+0" 
     &         / 7X, "END DO" /)     

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over reaction rates adding partial derivative terms; EXPLIC
c  holds the PD terms according to number of reactants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 340 IAR = 0, IARRY
        
         IF( IAR .GT. NONDIAG .AND. IAR .LT. NONDIAG1 )CYCLE
         
             PRINT*,'Writing terms for CC2( ', IAR, ' )'
             NTERMS = 0
             
             DO 240 NRX = 1, NUSERAT( NCSP )
             
                NRK = NKUSERAT( NRX,NCSP )
                
c...partial derivative term for reactions with 1 reactant
               IF ( NREACT( NRK ) .EQ. 1 ) THEN
!                 DO NCELL = 1, NUMCELLS
!                    EXPLIC( NCELL,1 ) = RKI( NCELL,NRK ) 
!                 END DO
                  WRITE( STR_EXPLIC( 1 ), 94999)NRK
94999             FORMAT('RKI( ', I4,' ) ')
               
c...partial derivative terms for reactions with 2 reactants
               ELSE IF ( NREACT( NRK ) .EQ. 2 ) THEN
                  JR1 = IRM2( NRK,1,NCS )
                  JR2 = IRM2( NRK,2,NCS )
!                 DO NCELL  = 1, NUMCELLS
!                    EXPLIC( NCELL,1 )  = RKI( NCELL,NRK )
!    &                                  * YIN( NCELL,JR2 )
!                    EXPLIC( NCELL,2 )  = RKI( NCELL,NRK )
!    &                                  * YIN( NCELL,JR1 )
!                 END DO
                  WRITE( STR_EXPLIC( 1 ), 95000)NRK, JR2
                  WRITE( STR_EXPLIC( 2 ), 95000)NRK, JR1
95000             FORMAT('RKI( ', I4, ' ) * YIN( ', I4,' ) ') 
               
c.....partial derivative terms for reactions with 3 reactants
              ELSE IF ( NREACT( NRK ) .EQ. 3 ) THEN
                 JR1 = IRM2( NRK,1,NCS )
                 JR2 = IRM2( NRK,2,NCS )
                 JR3 = IRM2( NRK,3,NCS )
!                DO NCELL = 1, NUMCELLS
!                   CR2 = RKI( NCELL,NRK ) * YIN( NCELL,JR2 )
!                   EXPLIC( NCELL,1 ) = CR2 * YIN( NCELL,JR3 )
!             
!             
!                   EXPLIC( NCELL,2 ) = RKI( NCELL,NRK )
!    &                                * YIN( NCELL,JR1 )
!    &                                * YIN( NCELL,JR3 ) 
!             
!             
!                   EXPLIC( NCELL,3 ) = CR2 * YIN( NCELL,JR1 )
!                END DO
                    WRITE( STR_EXPLIC( 1 ), 95001) NRK, JR2, JR3
                    WRITE( STR_EXPLIC( 2 ), 95001) NRK, JR1, JR3
                    WRITE( STR_EXPLIC( 3 ), 95001) NRK, JR2, JR1
                    
95001               FORMAT(11X,'RKI( ', I4,' ) * YIN( ', I4,' )* YIN(  ', I4, ' )' )
              END IF
              

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Add PD terms to [J] for this reaction
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c...loss terms
              NLD = NDERIVL( NRK,NCSP )         
              DO NL = 1, NLD
                 IARP = JARRL( NRK,NL,NCSP )
                 IALP = JLIAL( NRK,NL,NCSP )
!                 IF(NL .EQ. 1)WRITE(IOUT,98801)IARP, IARP
!                DO NCELL = 1, NUMCELLS
!                   CC2( NCELL,IARP ) = CC2( NCELL,IARP ) - EXPLIC( NCELL,IALP ) 
!                END DO
                  IF( IAR .EQ. IARP )THEN
                     IF(NTERMS .LT. 1)WRITE(IOUT,98801)IARP, IARP
                     NTERMS = NTERMS + 1
                     WRITE(IOUT,98802)TRIM(STR_EXPLIC( IALP ))
!                      WRITE(IOUT,98805)IARP, IARP, TRIM(STR_EXPLIC( IALP ))
                  END IF
              END DO    ! End loop over loss terms
              
c...production terms with stoichiomteric coeff EQ 1.0 and NE 1.0
             NPD = NDERIVP( NRK,NCSP )
             DO 220 NP = 1, NPD
             
                IARP = JARRP( NRK,NP,NCSP )
                IALP = JPIAL( NRK,NP,NCSP )
!                IF(NP .EQ. 1)WRITE(IOUT,98801)IARP, IARP
             
                IF ( ICOEFF( NRK,NP,NCSP ) .EQ. 0 ) THEN
c..production terms with unit stoichiometry
!                DO NCELL = 1, NUMCELLS
!                   CC2( NCELL,IARP ) = CC2( NCELL,IARP ) + EXPLIC( NCELL,IALP ) 
!                END DO
                 IF( IAR .EQ. IARP )THEN
                     IF(NTERMS .LT. 1)WRITE(IOUT,98801)IARP, IARP
                     WRITE(IOUT,98803)TRIM(STR_EXPLIC( IALP ))
                     NTERMS = NTERMS + 1
!                     WRITE(IOUT,98806)IARP, IARP, TRIM(STR_EXPLIC( IALP )) 
                 END IF
!                 WRITE(IOUT,98803)TRIM(STR_EXPLIC( IALP ))
                ELSE
c..production terms with non-unit stoichiometry
                   ISCP = ICOEFF( NRK,NP,NCSP )
                   FRACN = SC( NRK,ISCP )
                   WRITE(STR_FRACN,'(D10.4)')REAL(SC( NRK,ISCP ), 8)
!                  DO NCELL = 1, NUMCELLS
!                     CC2( NCELL,IARP ) = CC2( NCELL,IARP ) + FRACN
!    &                                  * EXPLIC( NCELL,IALP ) 
!                  END DO
                   IF( IAR .EQ. IARP )THEN
                     IF(NTERMS .LT. 1)WRITE(IOUT,98801)IARP, IARP
                     NTERMS = NTERMS + 1
!                     WRITE(IOUT,98807)IARP, IARP, TRIM(STR_FRACN), TRIM(STR_EXPLIC( IALP )) 
                     WRITE(IOUT,98804)TRIM(STR_FRACN), TRIM(STR_EXPLIC( IALP )) 
                   END IF
                END IF
             
220          CONTINUE      ! End loop over production terms
             
240         CONTINUE      ! End loop over reactions
340    CONTINUE   
!        WRITE(IOUT, 97910)
        WRITE(IOUT, 97911)

        CLOSE(IOUT) 
        
97547   FORMAT('      SUBROUTINE LIGHT_JACOB( RKI, YIN, CC2 )' //
     &         'C     routine evaluate the Jabocian for day or light conditions.')
97548   FORMAT('      SUBROUTINE NIGHT_JACOB( RKI, YIN, CC2 )' //
     &         'C     routine evaluate the Jabocian for night or dark conditions.')
     
97549   FORMAT(  7X, 'USE SPARSE_LUD_DATA'
     &         / 7X, 'IMPLICIT NONE '/)
97950   FORMAT('C..Arguments:' /)
97951   FORMAT('      INTEGER                  :: NCSP         ! Index of chem mech to use; 1=gas/day, 2=gas/night')
97801   FORMAT('      REAL( 8 ), INTENT( IN  ) :: YIN( : )     ! Species concs, ppm' /
     &         '      REAL( 8 ), INTENT( IN  ) :: RKI( : )     ! Reaction Rate Constant so YDOTs are in ppm/min'/
     &         '      REAL( 8 ), INTENT( OUT ) :: CC2( : )     ! Jacobian vectorized and sorted based on spareness' )
97802   FORMAT('C...Local:' 
     &                      / 7X,'INTEGER IAR                ! Loop index for number of cells'
     &                      / 7X,'INTEGER IARRY              ! Pointer to end of [P] entries'
     &                      / 7X,'INTEGER NONDIAG            ! Pointer to end of off-diagonal entries'
     &                      / 7X,'INTEGER NONDIAG1           ! Pointer to start of diagonal entries' ///)
97803   FORMAT('c  Zero out nondiagonal elements of Jacobian ( stored in sparse matrix array cc2 )' /)
97501   FORMAT('      NCSP = 1 ')        
97504   FORMAT('      NCSP = 2 ')        
97901   FORMAT('      NCSP = 1 ')        
97904   FORMAT('      NCSP = 2 ')        
97902   FORMAT('      DO NCELL = 1, NUMCELLS' /)
97903   FORMAT('c   Add PD terms to [J] for each reaction' /)

98801   FORMAT(7X,'CC2(', I4,') = CC2(', I4,')' )
98802   FORMAT(5X,'&',11X,'- ',A)
98803   FORMAT(5X,'&',11X,'+ ',A)
98804   FORMAT(5X,'&',11X,'+ ',A,'*',A)
98805   FORMAT(7X,'CC2(', I4,') = CC2(', I4,') ','- ',A)
98806   FORMAT(7X,'CC2(', I4,') = CC2(', I4,') ','+ ',A)
98807   FORMAT(7X,'CC2(', I4,') = CC2(', I4,') + ',A,' * ',A)
97910   FORMAT(7X,'END DO')
97911   FORMAT(// 7X, 'RETURN' / 7X, 'END' )

      RETURN 
      END
