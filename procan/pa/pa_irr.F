
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
C $Header: /project/yoj/arc/CCTM/src/procan/pa/pa_irr.F,v 1.4 2011/10/21 16:11:32 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PA_IRR ( LSTART, LCHGVL, RK, CONC, DELT, NUMCELLS, ICLND )

C-----------------------------------------------------------------------
C  Function: Integrate chemical rates of reaction for an IRR/MB analysis
 
C  Preconditions: None
 
C  Key Subroutines/Functions Called: None
 
C  Revision History:
C   Prototype created by Jerry Gipson, September, 1996
C   global BLKPRM Jeff Dec 96
C   Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C   Modified Jun, 1998 by Jerry Gipson to add reaction number error checks
C   Modified 1/19/99 by David Wong at LM:
C                      -- add four include files because of new PA_CMN.EXT
C   Modified 2/26/99 by David Wong at LM:
C                      -- remove SUBST_AE_SPC, SUBST_NR_SPC, SUBST_TR_SPC,
C                         three .EXT files
C   31 Mar 01 J.Young: Use HGRD_DEFN; eliminate BLKPRM.EXT
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   21 Jun 10 J.Young: convert for Namelist redesign
C   19 Aug 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module

C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE RXNS_DATA             ! chemical mechanism data
      USE CGRID_SPCS            ! CGRID mechanism species
      USE PA_DEFN               ! Process Anaylsis control and data variables
      USE UTILIO_DEFN

      IMPLICIT NONE 

C..Includes: None
      
C..Arguments: 
      LOGICAL, INTENT( IN ) :: LSTART   ! Flag to indicate start of chemical integration period
      LOGICAL, INTENT( IN ) :: LCHGVL   ! Flag to indicate vector length is changing

      REAL,    INTENT( IN ) :: RK( :,: )    ! Reaction rate coefficients
!     REAL,    INTENT( IN ) :: CONC( BLKSIZE,N_GC_SPC ) ! Species concentrations
      REAL,    INTENT( IN ) :: CONC( :,: )  ! Species concentrations
      REAL,    INTENT( IN ) :: DELT( : )    ! Chemistry integration time size
      INTEGER, INTENT( IN ) :: NUMCELLS     ! Number of cells to process
      INTEGER, INTENT( IN ) :: ICLND( : )   ! Original cell number 

C..Parameters: None

C..External Functions: None
 
C..Saved Local Variables:
      CHARACTER( 16 ) , SAVE :: PNAME = 'PA_IRR'   ! Program name

      LOGICAL, SAVE :: LFIRST = .TRUE.   ! Flag for first call to subroutine
      LOGICAL, SAVE :: LINTRXN( NRXNS )  ! Flag for reaction integration

      REAL,    SAVE :: RXOLD( BLKSIZE,NRXNS )
      REAL,    SAVE :: RXSAV( BLKSIZE,NRXNS )

C..Scratch Local Variables:
      INTEGER ISP1, ISP2, ISP3  ! Species indices
      INTEGER NCELL             ! Loop index for cells
      INTEGER NIRR              ! Loop index for IRR outputs
      INTEGER NOUT              ! IRR output index
      INTEGER NRX               ! Loop index for reactions
      INTEGER NTEMP             ! Loop index for temp IRRs
      INTEGER NTERM             ! Loop index for terms

      REAL    COEFF                           ! Coefficient of IRR term
      REAL    :: RXRAT  ( BLKSIZE,NRXNS )     ! Calcualted reaction rates
      REAL    :: INTRXN ( BLKSIZE,NRXNS )     ! Integrated reaction rates
      REAL    :: TEMPIRR( BLKSIZE,NUMTEMPS )  ! Array of computed temp IRRs

C-----------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, flag the reactions for which to calculate IRRS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN

         IF ( LFULLIRR .AND. NIRRVAR .NE. NRXNS ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &        'Number of reactions for PA does not match number of ' //
     &        'reactions in mechanism', XSTAT2 )
         END IF
 
         DO NRX = 1, NRXNS
            IF ( LFULLIRR ) THEN
               LINTRXN( NRX ) = .TRUE.
            ELSE
               LINTRXN( NRX ) = .FALSE.
            END IF
         END DO

         IF ( .NOT. LFULLIRR ) THEN

            IF ( NUMTEMPS .GT. 0 ) THEN            
               DO NTEMP = 1, NUMTEMPS 
                  DO NTERM = 1, TEMPTERMS( NTEMP )
                     NRX = TEMPRXN( NTEMP,NTERM )
                     IF ( NRX .GT. NRXNS ) THEN
                        CALL M3EXIT( PNAME, 0, 0,
     &                    'Number of reactions for PA does not match ' //
     &                    'number of reactions in mechanism', XSTAT2 )
                     END IF
                     LINTRXN( NRX ) = .TRUE.
                  END DO
               END DO
            END IF

            IF ( NIRRVAR .GT. 0 ) THEN
               DO NOUT = 1, NIRRVAR
                  IF ( NIRRRXNS( NOUT ) .GT. 0 ) THEN
                     DO NTERM = 1, NIRRRXNS( NOUT )
                        NRX = IRRRXN( NOUT,NTERM )
                        IF ( NRX .GT. NRXNS ) THEN
                           CALL M3EXIT( PNAME, 0, 0,
     &                       'Number of reactions for PA does not match ' //
     &                       'number of reactions in mechanism', XSTAT2 )
                        END IF
                        LINTRXN( NRX ) = .TRUE.
                     END DO
                  END IF
               END DO
            END IF

         END IF

         LFIRST = .FALSE.

      END IF
    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over reactions and calculate rate of reaction with current
c  concentrations (This needs to be optimized for small NUMCELLS)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 100 NRX = 1, NRXNS
         IF ( LINTRXN( NRX ) ) THEN
            IF ( NREACT( NRX ) .EQ. 1 ) THEN
               ISP1 = IRR( NRX,1 )
               DO NCELL = 1, NUMCELLS
                  RXRAT( NCELL,NRX ) = RK( NCELL,NRX )
     &                               * CONC( NCELL,ISP1 )
               END DO
            ELSE IF ( NREACT( NRX ) .EQ. 2 ) THEN
               ISP1 = IRR( NRX,1 )
               ISP2 = IRR( NRX,2 )
               DO NCELL = 1, NUMCELLS
                  RXRAT( NCELL,NRX ) = RK( NCELL,NRX )
     &                               * CONC( NCELL,ISP1 )
     &                               * CONC( NCELL,ISP2 ) 
               END DO
            ELSE IF ( NREACT( NRX ) .EQ. 3 ) THEN
               ISP1 = IRR( NRX,1 )
               ISP2 = IRR( NRX,2 )
               ISP3 = IRR( NRX,3 )
               DO  NCELL = 1, NUMCELLS
                  RXRAT( NCELL,NRX ) = RK( NCELL,NRX )
     &                               * CONC( NCELL,ISP1 )
     &                               * CONC( NCELL,ISP2 )
     &                               * CONC( NCELL,ISP3 ) 
               END DO 
            ELSE IF (NREACT( NRX ) .EQ. 0 ) THEN
               DO NCELL = 1, NUMCELLS
                  RXRAT( NCELL,NRX ) = RK( NCELL,NRX )
               END DO
            END IF
         END IF         
100   CONTINUE

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c If this is the start of the chemistry integration period, save the 
c reaction rates, and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LSTART ) THEN
         DO NRX = 1, NRXNS
            IF ( LINTRXN( NRX ) ) THEN
               DO NCELL = 1, NUMCELLS
                  RXOLD( NCELL,NRX ) = RXRAT( NCELL,NRX )
                  RXSAV( NCELL,NRX ) = RXRAT( NCELL,NRX )
               END DO 
            END IF
         END DO
         RETURN
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize calculated IRRs to zero and get the reaction rate at
c  the previous step if the cell vector length is changing
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NIRR = 1, NIRRVAR
         DO NCELL = 1, NUMCELLS
            IRRSTEP( NCELL,NIRR ) = 0.0
         END DO
      END DO

      IF ( LCHGVL ) THEN
         DO NIRR = 1, NRXNS
            IF ( LINTRXN( NIRR ) ) THEN
                DO NCELL = 1, NUMCELLS
                   RXOLD( NCELL,NIRR ) = RXSAV( ICLND( NCELL ),NIRR )
                END DO
             END IF
         END DO
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute integrated reaction rates for each reaction and return
c  if a Full IRR analysis is being done 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRX = 1, NRXNS
         IF ( LINTRXN( NRX ) ) THEN
            DO NCELL = 1, NUMCELLS
               INTRXN( NCELL,NRX ) = 0.5 * DELT( NCELL )
     &                             * ( RXOLD( NCELL,NRX )
     &                             +   RXRAT( NCELL,NRX ) )
            END DO
         END IF
      END DO

      IF ( LFULLIRR ) THEN
         DO NRX = 1, NRXNS
            DO NCELL = 1, NUMCELLS
               IRRSTEP( NCELL,NRX ) = INTRXN( NCELL,NRX ) 
            END DO
         END DO
         GO TO 500
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute the temporary IRRs that are used below
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( NUMTEMPS .GT. 0 ) THEN

         DO NTEMP = 1, NUMTEMPS
            DO NCELL = 1, NUMCELLS
               TEMPIRR( NCELL,NTEMP ) = 0.0
            END DO
         END DO

         DO NTEMP =1, NUMTEMPS                    
            DO NTERM = 1, TEMPTERMS( NTEMP )
               NRX = TEMPRXN( NTEMP,NTERM )
               COEFF = TEMPCOEF( NTEMP, NTERM )               
               DO NCELL = 1, NUMCELLS
                  TEMPIRR( NCELL,NTEMP ) = TEMPIRR( NCELL,NTEMP )
     &                                   + COEFF * INTRXN( NCELL,NRX )
               END DO
            END DO
         END DO

      END IF
         
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Sum up all IRRs for the output IRR for this step
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c..Add required temporary IRRs if they are positive
c
      IF ( NUMOUTPOS .GT. 0 ) THEN
         DO NTERM = 1, NUMOUTPOS
            NTEMP = TEMPOUTPOS( NTERM )
            NOUT  = INDXOUTPOS( NTERM )
            COEFF = COEFOUTPOS( NTERM )
            DO NCELL = 1, NUMCELLS
               IF ( TEMPIRR( NCELL,NTEMP ) .GT. 0.0 ) THEN
                  IRRSTEP( NCELL,NOUT ) = IRRSTEP( NCELL,NOUT )
     &                                  + COEFF * TEMPIRR( NCELL,NTEMP )
               END IF
            END DO
         END DO
      END IF

c..Add required temporary IRRs if they are negative
      IF ( NUMOUTNEG .GT. 0 ) THEN
         DO NTERM = 1, NUMOUTNEG
            NTEMP = TEMPOUTNEG( NTERM )
            NOUT  = INDXOUTNEG( NTERM )
            COEFF = COEFOUTNEG( NTERM )
            DO NCELL = 1, NUMCELLS
               IF ( TEMPIRR( NCELL,NTEMP ) .LT. 0.0 ) THEN
                  IRRSTEP( NCELL,NOUT ) = IRRSTEP( NCELL,NOUT )
     &                                  + COEFF * ABS( TEMPIRR( NCELL,NTEMP ) )
               END IF
            END DO
         END DO
      END IF

c..Add temporary IRRs that do not depend on sign
      IF ( NUMOUTIND .GT. 0 ) THEN
         DO NTERM = 1, NUMOUTIND
            NTEMP = TEMPOUTIND( NTERM )
            NOUT  = INDXOUTIND( NTERM )
            COEFF = COEFOUTIND( NTERM )
            DO NCELL = 1, NUMCELLS
               IRRSTEP( NCELL,NOUT ) = IRRSTEP( NCELL,NOUT )
     &                               + COEFF * TEMPIRR( NCELL,NTEMP )
            END DO
         END DO
      END IF

c..Add all remaining IRRs terms
      DO NOUT = 1, NIRRVAR
         IF ( NIRRRXNS( NOUT ) .GT. 0 ) THEN
            DO NTERM = 1, NIRRRXNS( NOUT )
               COEFF = IRRCOEF( NOUT,NTERM )
               NRX   = IRRRXN( NOUT,NTERM )
               DO NCELL = 1, NUMCELLS
                  IRRSTEP( NCELL,NOUT ) = IRRSTEP( NCELL,NOUT )
     &                                  + COEFF * INTRXN( NCELL,NRX )
               END DO
            END DO
         END IF
      END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Update the array holding the cumulative results over all steps and
c  save the rxrates for the next step
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
500   CONTINUE

      IF ( LCHGVL ) THEN

c..For changing block lengths
         DO NIRR = 1, NIRRVAR
            DO NCELL = 1, NUMCELLS
               IRRBLK( ICLND( NCELL ),NIRR ) = 
     &         IRRBLK( ICLND( NCELL ),NIRR ) + IRRSTEP( NCELL,NIRR )
            END DO
         END DO

         DO NIRR = 1, NRXNS
            IF ( LINTRXN( NIRR ) ) THEN
               DO NCELL = 1, NUMCELLS
                  RXSAV( ICLND( NCELL ),NIRR ) = RXRAT( NCELL,NIRR )
               END DO
            END IF
         END DO

      ELSE

c..For static block lengths
         DO NIRR = 1, NIRRVAR
            DO NCELL = 1, NUMCELLS
               IRRBLK( NCELL,NIRR ) = IRRBLK( NCELL,NIRR )
     &                              + IRRSTEP( NCELL,NIRR )
            END DO
         END DO

         DO NIRR = 1, NRXNS
            IF ( LINTRXN( NIRR ) ) THEN
               DO NCELL = 1, NUMCELLS
                  RXOLD( NCELL,NIRR ) = RXRAT( NCELL,NIRR )
               END DO
            END IF
         END DO
      END IF

      RETURN
      END
