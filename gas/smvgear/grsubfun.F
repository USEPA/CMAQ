
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/grsubfun.F,v 1.1 2011/03/23 21:13:16 sjr Exp $ 

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
C***********************************************************************

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
      
      IF( NSPECIAL_RXN .GT. 0 ) CALL CALC_SPECIAL   ! calculate special rate coefficients

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
            ISP1 = IRM2( NRK, 1, NCS )
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL, NRK ) = RK( NCELL, NRK )
     &                             * CNEW( NCELL, ISP1 )
            ENDDO
         ELSEIF( NREACT( NRK ) .EQ. 2 ) THEN
            ISP1 = IRM2( NRK, 1, NCS )
            ISP2 = IRM2( NRK, 2, NCS )
            DO NCELL = 1, NUMCELLS
               RXRAT( NCELL, NRK ) = RK( NCELL, NRK )
     &                             * CNEW( NCELL, ISP1 )
     &                             * CNEW( NCELL, ISP2 )
            ENDDO
         ELSEIF( NREACT( NRK ) .EQ. 3 ) THEN
            ISP1 = IRM2( NRK, 1, NCS )
            ISP2 = IRM2( NRK, 2, NCS )
            ISP3 = IRM2( NRK, 3, NCS )
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
            ISP1 = IRM2( NRK, NR, NCS )
            DO NCELL = 1, NUMCELLS
               GLOSS( NCELL, ISP1 ) = GLOSS( NCELL, ISP1 )
     &                              - RXRAT( NCELL, NRK )
            ENDDO
         ENDDO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Add production terms to gloss for this reaction
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          DO NP = 1, NPRDCT( NRK )
            ISP1 = IRM2( NRK, NP + 3, NCS )
            DO NCELL = 1, NUMCELLS
             GLOSS( NCELL, ISP1 ) = GLOSS( NCELL, ISP1 )
     &                            + SC( NRK, NP ) * RXRAT( NCELL, NRK )
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
     &         GLOSS( NCELL, ISP ) .LT. 0.0 ) THEN
               GLOSS( NCELL, ISP ) = 0.0
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END

