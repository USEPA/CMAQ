
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
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/rbfeval.F,v 1.3 2011/10/21 16:11:10 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE RBFEVAL( NCSP, YIN, YDOT )

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
C                    15 Jul 14 B.Hutzell: replaced mechanism include files with 
C                    RXNS_DATA module, replaced call to CALC_SPECIAL with 
C                    SPECIAL_RATES in RXNS_FUNCTION module and added intent
C                    declarations to arguments
C***********************************************************************

      USE RXNS_DATA
      USE RXNS_FUNCTION
      USE RBDATA               ! ROS3 solver data

      IMPLICIT NONE

C..Includes:

C..Arguments:
      INTEGER,   INTENT(    IN ) :: NCSP          ! Index of mech to use: 1=gas/day, 2=gas/night
      REAL( 8 ), INTENT( INOUT ) :: YIN(  :, : )  ! Species concs, ppm
      REAL( 8 ), INTENT(   OUT ) :: YDOT( :, : )  ! Species rates of change, ppm/min

C..Parameters: None

C..External FUNCTIONS: None

C..Local Variables:
      INTEGER ISP              ! Loop index for species
      INTEGER ISP1, ISP2, ISP3 ! Pointers to species numbers
      INTEGER NCELL            ! Loop index for number of cells
      INTEGER NP               ! Loop index for number of products
      INTEGER NR               ! Loop index for number of reactants
      INTEGER NRK              ! Reaction number
      INTEGER NRX              ! Loop index for number of reactions

      REAL( 8 ) :: RXRAT( BLKSIZE,NRXNS )   ! Computed rxn rates

C***********************************************************************      

      IF ( NSPECIAL_RXN .GT. 0 ) THEN  ! calculate special rate coefficients
          CALL SPECIAL_RATES( NUMCELLS, YIN, BLKTEMP, BLKDENS, RKI )
      END IF
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize dc/dt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO ISP = 1, ISCHAN
         DO NCELL = 1, NUMCELLS
            YDOT( NCELL,ISP ) = 0.0D0
         END DO
      END DO
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over reactions to calculate dc/dt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO 100 NRX = 1, NUSERAT( NCSP )

         NRK = NKUSERAT( NRX,NCSP )

c..Calculate reaction rate
         IF ( NREACT( NRK ) .EQ. 1 ) THEN
            ISP1 = IRM2( NRK, 1, NCS )
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL,NRK ) = RKI( NCELL,NRK )
     &                            * YIN( NCELL,ISP1 )
            END DO
         ELSE IF ( NREACT( NRK ) .EQ. 2 ) THEN
            ISP1 = IRM2( NRK,1,NCS )
            ISP2 = IRM2( NRK,2,NCS )
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL,NRK ) = RKI( NCELL,NRK )
     &                            * YIN( NCELL,ISP1 )
     &                            * YIN( NCELL,ISP2 )
            END DO
         ELSE IF ( NREACT( NRK ) .EQ. 3 ) THEN
            ISP1 = IRM2( NRK,1,NCS )
            ISP2 = IRM2( NRK,2,NCS )
            ISP3 = IRM2( NRK,3,NCS )
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL,NRK ) = RKI( NCELL,NRK )
     &                            * YIN( NCELL,ISP1 )
     &                            * YIN( NCELL,ISP2 )
     &                            * YIN( NCELL,ISP3 )
            END DO 
         ELSE IF ( NREACT( NRK ) .EQ. 0 ) THEN
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL,NRK ) = RKI( NCELL,NRK )
            END DO
         END IF
         
c..Subtract loss terms from dc/dt for this reaction 
         DO NR = 1, NREACT( NRK )
            ISP1 = IRM2( NRK,NR,NCS )
            DO NCELL = 1, NUMCELLS
               YDOT( NCELL,ISP1 ) = YDOT( NCELL,ISP1 )
     &                            - RXRAT( NCELL,NRK )
            END DO
         END DO
  
c..Add production terms to dc/dt for this reaction
         DO NP = 1, NPRDCT( NRK )
            ISP1 = IRM2( NRK,NP+3,NCS )
            DO NCELL = 1, NUMCELLS
             YDOT( NCELL,ISP1 ) = YDOT( NCELL,ISP1 )
     &                          + SC( NRK,NP ) * RXRAT( NCELL,NRK )
            END DO
         END DO

100   CONTINUE               ! END LOOP OVER REACTIONS

      RETURN
      END

