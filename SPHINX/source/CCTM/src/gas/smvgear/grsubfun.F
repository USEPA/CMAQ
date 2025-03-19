
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/grsubfun.F,v 1.2 2011/10/21 16:11:15 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)grsubfun.F        1.1 /project/mod3/CMAQ/src/chem/smvgear/SCCS/s.grsubfun.  F 07 Jul 1997 12:45:32

       SUBROUTINE SUBFUN

C***********************************************************************
C
C  FUNCTION:  To Compute GLOSS = dc/dt for each species. GLOSS is the
C             net rate of change in species concentrations resulting
C             from chemical production minus chemical loss.
C
C  PRECONDITIONS: None
C                                                                     
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995.
C                      Based on  the code originally developed by 
C                      M. Jacobson, (Atm. Env., Vol 28, No 2, 1994).
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      the Models-3 minimum IOV configuration
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C                    Modified September, 1997 by Jerry Gipson to be 
C                      consistent with the targeted CTM
C                    16 Aug 01 J.Young: Use HGRD_DEFN
C                    31 Jan 05 J.Young: get BLKSIZE from dyn alloc horizontal
C                    & vertical domain specifications module (GRID_CONF)
C                    28 Jun 10 J.Young: remove unnecessary modules and includes
C                    30 Jun 10 J.Young: convert for Namelist redesign; move all
C                    local include file variables into GRVARS module
C                    15 Jul 14 B.Hutzell: replaced mechanism include files with 
C                    RXNS_DATA module and replace call to CALC_SPECIAL with 
C                    SPECIAL_RATES in RXNS_FUNCTION module
C***********************************************************************

      USE RXNS_DATA
      USE RXNS_FUNCTION
      USE GRVARS              ! inherits GRID_CONF

      IMPLICIT NONE

C..INCLUDES: None
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      INTEGER ISP              ! Loop index for species
      INTEGER ISP1, ISP2, ISP3 ! Pointers to species numbers
      INTEGER NCELL            ! Loop index for number of cells
      INTEGER NP               ! Loop index for number of products
      INTEGER NR               ! Loop index for number of reactants
      INTEGER NRK              ! Reaction number
      INTEGER NRX              ! Loop index for number of reactions
C***********************************************************************      
      
      IF ( NSPECIAL_RXN .GT. 0 ) THEN  ! calculate special rate coefficients
          CALL SPECIAL_RATES( NUMCELLS, CNEW, BLKTEMP, BLKDENS, RK )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Increment call counter and initialize dcdt=gloss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NSUBFUN = NSUBFUN + 1
      DO ISP = 1, ISCHAN
         DO NCELL = 1, NUMCELLS
            GLOSS( NCELL, ISP ) = EMBLK( NCELL, ISP )
         ENDDO
      ENDDO
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  loop over reactions and calculate rate of reaction 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 220 NRX = 1, NUSERAT( NCSP )
        NRK = NKUSERAT( NRX, NCSP )
        IF( NREACT( NRK ) .EQ. 1 ) THEN
            ISP1 = IRM2( 1, NRK, NCS )
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL, NRK ) = RK( NCELL, NRK )
     &                             * CNEW( NCELL, ISP1 )
            ENDDO
         ELSEIF( NREACT( NRK ) .EQ. 2 ) THEN
            ISP1 = IRM2( 1, NRK, NCS )
            ISP2 = IRM2( 2, NRK, NCS )
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL, NRK ) = RK( NCELL, NRK )
     &                             * CNEW( NCELL, ISP1 )
     &                             * CNEW( NCELL, ISP2 )
            ENDDO
         ELSEIF( NREACT( NRK ) .EQ. 3 ) THEN
            ISP1 = IRM2( 1, NRK, NCS )
            ISP2 = IRM2( 2, NRK, NCS )
            ISP3 = IRM2( 3, NRK, NCS )
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL, NRK ) = RK( NCELL, NRK )
     &                             * CNEW( NCELL, ISP1 )
     &                             * CNEW( NCELL, ISP2 )
     &                             * CNEW( NCELL, ISP3 )
            ENDDO 
         ELSEIF( NREACT( NRK ) .EQ. 0 ) THEN
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL, NRK ) = RK( NCELL, NRK )
            ENDDO
         ENDIF
         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Subtract loss terms from gloss for this reaction 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NR = 1, NREACT( NRK )
            ISP1 = IRM2( NR, NRK, NCS )
            DO NCELL = 1, NUMCELLS
               GLOSS( NCELL, ISP1 ) = GLOSS( NCELL, ISP1 )
     &                              - RXRAT( NCELL, NRK )
            ENDDO
         ENDDO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Add production terms to gloss for this reaction
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          DO NP = 1, NPRDCT( NRK )
            ISP1 = IRM2( NP + 3, NRK, NCS )
            DO NCELL = 1, NUMCELLS
             GLOSS( NCELL, ISP1 ) = GLOSS( NCELL, ISP1 )
     &                            + REAL( SC( NRK, NP ), 8 ) * RXRAT( NCELL, NRK )
            ENDDO
         ENDDO

220   CONTINUE               ! END LOOP OVER REACTIONS
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  If the species concentration is at or below the threshold and it
c  is being destroyed, zero the loss rate.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO ISP = 1, ISCHAN
         DO NCELL = 1, NUMCELLS
            IF( CNEW( NCELL, ISP ) .LE. ZBOUND .AND. 
     &         GLOSS( NCELL, ISP ) .LT. 0.0D0 ) THEN
               GLOSS( NCELL, ISP ) = 0.0D0
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END

