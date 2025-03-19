
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

       SUBROUTINE WRT_RATES( IOUNIT )

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
C...arguments:
      INTEGER, INTENT( IN ) :: IOUNIT
     
C..Includes: None     

C..Parameters: None

C..External Functions: None

C..Local Variables:
      INTEGER ISP              ! Loop index for species
      INTEGER ISP1, ISP2, ISP3 ! Pointers to species numbers
      INTEGER NP               ! Loop index for number of products
      INTEGER NR               ! Loop index for number of reactants
      INTEGER NRK              ! Reaction number
      INTEGER NRX              ! Loop index for number of reactions
      INTEGER N_TERMS

      CHARACTER( 132 ), ALLOCATABLE :: STR_RXRAT ( : )      ! reaction rate strings
    
C***********************************************************************      

      RETURN
       ALLOCATE ( STR_RXRAT( NRXNS)  )

       WRITE(IOUNIT, 95550) ! write subroutine declarations

!       WRITE(IOUNIT, 95551)     
       WRITE(IOUNIT, 95552)     
               
      DO NRK = 1, NTHERMAL
c..write thermal reaction rates
             IF ( NREACT( NRK ) .EQ. 1 ) THEN
                ISP1 = INEW2OLD( IRR( NRK, 1 ) )
!                WRITE(STR_RXRAT( NRK ),95000)NRK, TRIM( MECHANISM_SPC( ISP1 ) ) ! , 'Reaction ' // RXLABEL( NRK )
                WRITE(STR_RXRAT( NRK ),95006)NRK, TRIM( MECHANISM_SPC( ISP1 ) ) ! , 'Reaction ' // RXLABEL( NRK )
             ELSE IF ( NREACT( NRK ) .EQ. 2 ) THEN
                ISP1 = INEW2OLD( IRR( NRK, 1 ) )
                ISP2 = INEW2OLD( IRR( NRK, 2 ) )
!                WRITE(STR_RXRAT( NRK ),95001)NRK, TRIM(MECHANISM_SPC( ISP1 )), TRIM(MECHANISM_SPC( ISP2 )) ! , 'Reaction ' // RXLABEL( NRK )                
                WRITE(STR_RXRAT( NRK ),95007)NRK, TRIM(MECHANISM_SPC( ISP1 )), TRIM(MECHANISM_SPC( ISP2 )) ! , 'Reaction ' // RXLABEL( NRK )                
             ELSE IF ( NREACT( NRK ) .EQ. 3 ) THEN
                ISP1 = INEW2OLD( IRR( NRK, 1 ) )
                ISP2 = INEW2OLD( IRR( NRK, 2 ) )
                ISP3 = INEW2OLD( IRR( NRK, 3 ) )
!                WRITE(STR_RXRAT( NRK ),95002)NRK, TRIM(MECHANISM_SPC( ISP1 )), TRIM(MECHANISM_SPC( ISP2 )), 
!     &          TRIM( MECHANISM_SPC( ISP3 ) ) ! , 'Reaction ' // RXLABEL( NRK )
                WRITE(STR_RXRAT( NRK ),95008)NRK, TRIM(MECHANISM_SPC( ISP1 )), TRIM(MECHANISM_SPC( ISP2 )), 
     &          TRIM( MECHANISM_SPC( ISP3 ) ) ! , 'Reaction ' // RXLABEL( NRK )
             ELSE IF ( NREACT( NRK ) .EQ. 0 ) THEN
!                WRITE(STR_RXRAT( NRK ),95003)NRK ! , 'Reaction ' // RXLABEL( NRK )
                WRITE(STR_RXRAT( NRK ),95009)NRK ! , 'Reaction ' // RXLABEL( NRK )
             END IF
!             WRITE( IOUNIT,85000)NRK,TRIM( STR_RXRAT( NRK ) )
             WRITE( IOUNIT,85003)NRK,TRIM( STR_RXRAT( NRK ) )
100   END DO

!      WRITE( IOUNIT,95004)
      WRITE( IOUNIT,95014)
C...write sunlight dependent reaction rates
      DO NRK = NTHERMAL+1, NRXNS
c..write reaction rates
             IF ( NREACT( NRK ) .EQ. 1 ) THEN
                ISP1 = INEW2OLD( IRR( NRK, 1 ) )
!                WRITE(STR_RXRAT( NRK ),95000)NRK, TRIM( MECHANISM_SPC( ISP1 ) ) ! , 'Reaction ' // RXLABEL( NRK )
                WRITE(STR_RXRAT( NRK ),95006)NRK, TRIM( MECHANISM_SPC( ISP1 ) ) ! , 'Reaction ' // RXLABEL( NRK )
             ELSE IF ( NREACT( NRK ) .EQ. 2 ) THEN
                ISP1 = INEW2OLD( IRR( NRK, 1 ) )
                ISP2 = INEW2OLD( IRR( NRK, 2 ) )
!                WRITE(STR_RXRAT( NRK ),95001)NRK, TRIM( MECHANISM_SPC( ISP1 ) ), TRIM( MECHANISM_SPC( ISP2 ) ) ! , 'Reaction ' // RXLABEL( NRK )
                WRITE(STR_RXRAT( NRK ),95006)NRK, TRIM( MECHANISM_SPC( ISP1 ) ), TRIM( MECHANISM_SPC( ISP2 ) ) ! , 'Reaction ' // RXLABEL( NRK )
             ELSE IF ( NREACT( NRK ) .EQ. 3 ) THEN
                ISP1 = INEW2OLD( IRR( NRK, 1 ) )
                ISP2 = INEW2OLD( IRR( NRK, 2 ) )
                ISP3 = INEW2OLD( IRR( NRK, 3 ) )
!                WRITE(STR_RXRAT( NRK ),95002)NRK, TRIM( MECHANISM_SPC( ISP1 ) ), TRIM( MECHANISM_SPC( ISP2 ) ), 
!     &          TRIM( MECHANISM_SPC( ISP3 ) ) ! , 'Reaction ' // RXLABEL( NRK )
                WRITE(STR_RXRAT( NRK ),95007)NRK, TRIM( MECHANISM_SPC( ISP1 ) ), TRIM( MECHANISM_SPC( ISP2 ) ), 
     &          TRIM( MECHANISM_SPC( ISP3 ) ) ! , 'Reaction ' // RXLABEL( NRK )
             ELSE IF ( NREACT( NRK ) .EQ. 0 ) THEN
!                WRITE(STR_RXRAT( NRK ),95003)NRK ! , 'Reaction ' // RXLABEL( NRK )
                WRITE(STR_RXRAT( NRK ),95009)NRK ! , 'Reaction ' // RXLABEL( NRK )
             END IF
!             WRITE( IOUNIT,85001)NRK,TRIM( STR_RXRAT( NRK ) )
             WRITE( IOUNIT,85011)NRK,TRIM( STR_RXRAT( NRK ) )
      END DO

      WRITE( IOUNIT,95016)
      WRITE(IOUNIT, 97911)

95551  FORMAT(//7X,'IF ( NSPECIAL_RXN .GT. 0 ) CALL SPECIAL_RATES( NUMCELLS, YIN, RKI )',
     &    4X,'! calculate special rate coefficients '/ )
     
95552  FORMAT(//7X,'IF ( NSPECIAL_RXN .GT. 0 ) CALL SPECIAL_RATES( NUMCELLS, YIN, RKI )',
     &    4X,'! calculate special rate coefficients '/
     &        /7X,'DO NCELL = 1, NUMCELLS' )

95000 FORMAT('RKI( 1:NUMCELLS, ', I4,' ) * YIN( 1:NUMCELLS, INDEX_', A,' )  ')
95001 FORMAT('RKI( 1:NUMCELLS, ', I4,' ) * YIN( 1:NUMCELLS, INDEX_', A,' ) * YIN( 1:NUMCELLS, INDEX_', A, ' )' )
95002 FORMAT('RKI( 1:NUMCELLS, ', I4,' ) * YIN( INDEX_', A,' ) * YIN( 1:NUMCELLS, INDEX_', 
     &  A, ' ) * YIN( 1:NUMCELLS, INDEX_', A, ' )' )
95003           FORMAT('RKI( 1:NUMCELLS, ', I4,' ) ')
95004 FORMAT(/7X,'IF ( SUNLIGHT ) THEN' /)
85000 FORMAT(7X,'RATE( 1:NUMCELLS,', I4,') = ',A)   
95015 FORMAT(/7X,'ELSE'
     &       /11X,'RATE( 1:NUMCELLS,NSUN_RXNS_START:NRXNS ) = 0.0D0' 
     &       /7X,'END IF')

95006 FORMAT('RKI( NCELL, ', I4,' ) * YIN( NCELL, INDEX_', A,' )  ')
95007 FORMAT('RKI( NCELL, ', I4,' ) * YIN( NCELL, INDEX_', A,' ) * YIN( NCELL, INDEX_', A, ' )' )
95008 FORMAT('RKI( NCELL, ', I4,' ) * YIN( NCELL, INDEX_', A,' ) * YIN( NCELL, INDEX_', A, 
     &       ' ) * YIN( NCELL, INDEX_', A, ' )' )
95009           FORMAT('RKI( NCELL, ', I4,' ) ')

95014 FORMAT(/7X,'END DO'
     &       /7X,'IF ( SUNLIGHT ) THEN'
     &       /11X,'   DO NCELL = 1,  NUMCELLS')
85003 FORMAT(11X,'RATE( NCELL,', I4,') = ',A)   
95016 FORMAT(/11X,'   END DO'
     &       /7X,'ELSE'
     &       /11X,'  DO NRK = NSUN_RXNS_START, NRXNS'
     &       /11X,'     DO NCELL = 1, NUMCELLS'
     &       /11X,'        RATE( NCELL, NRK ) = 0.0D0' 
     &       /11X,'     END DO'
     &       /11X,'  END DO'
     &       /7X,'END IF')
      
85001 FORMAT(11X,'RATE( 1:NUMCELLS,', I4,') = ',A)   
85011 FORMAT(11X,'   RATE( NCELL,', I4,') = ',A)   

95550 FORMAT(7X,'SUBROUTINE EVALUATE_RATES( RKI, YIN, NUMCELLS, SUNLIGHT, RATE )'
     &  /'!***********************************************************************' 
     &  /'!'
     &  /'!  Function:  Compute Reaction Rates for Cells in block'
     &  /'!'
     &  /'!  Preconditions: None'
     &  /'!'
     &  /'!  Key Subroutines/Functions Called: None'
     &  /'!'
     &  /'!'
     &  /'!***********************************************************************' 
     &  /7X,'USE RXNS_DATA ' //
     &  /7X,'IMPLICIT NONE'/
     &  /'!..Includes:'
     &  /'!... arguments'
     &  /7X,'REAL( 8 ), INTENT(  IN )  ::   YIN  ( :, : )       ! Species concs, ppm'
     &  /7X,'REAL( 8 ), INTENT(INOUT)  ::   RKI  ( :, : )       ! Reaction Rate Constant so YDOTs are in ppm/min'
     &  /7X,'INTEGER,   INTENT(  IN )  ::   NUMCELLS            ! number of cells in block'
     &  /7X,'LOGICAL,   INTENT(  IN )  ::   SUNLIGHT            ! Is sun above horizon?'
     &  /7X,'REAL( 8 ), INTENT( OUT )  ::   RATE ( :, : )       ! Species rates of change, ppm/min'
     &  /'!... local'
     &  /7X,'INTEGER  :: NRK'/
     &  /7X,'INTEGER  :: NCELL'/
     &  /'!... Parameters: ')

97911   FORMAT(// 7X
     &          / 7X, 'RETURN'
     &          / 7X, 'END SUBROUTINE EVALUATE_RATES' )

      END SUBROUTINE WRT_RATES

