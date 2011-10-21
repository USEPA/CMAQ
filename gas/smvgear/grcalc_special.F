
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/grcalc_special.F,v 1.2 2011/10/21 16:11:13 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE CALC_SPECIAL()
C**********************************************************************
C
C  Function: To calculate the rate expressions expressed by
C            SPECIAL Block in the mechanism definition file. These
C            expressions sum terms that are weighted products of rate
C            constants and species concentrations within the REACTIONS
C            Block. The expressions increase computational efficiency
C            by combining several reactions into one reaction. The
C            SAPRC07TB mechanism is derived from the SAPRC07TC by using
C            these expressions. Note that term SPECIAL is a label employed
C            by the SAPRC07 delevoper, W.P.L. Carter.
C
C  References:
C            1) Carter, W.P.L., 2010a. Documentation of the SAPRC-07 chemical
C            mechanism and updated ozone reactivity scales. Report to the
C            California Air Resources Board, January 27. Available at
C            www.cert.ucr.edu/~carter/SAPRC.
C
C            2) Carter, W.P.L., 2010b. Development of the SAPRC-07
C            Chemical Mechanism. Atmospheric Environment  44, 5336-5345.
C
C  Preconditions: None
C
C  Key Subroutines/Functions Called: None
C
C  Revision History: Prototype created by Bill Hutzell, Dec, 2009
C
C                     17 Mar 11 B.Hutzell: initial version submitted
C**********************************************************************

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID species number and offsets
      USE GRVARS

      IMPLICIT NONE

C..Includes: None

C..Arguments: None

C..Parameters: None

C..External Functions: None

C..Scratch Local Variables:

      INTEGER ISP            ! Loop index for special rates
      INTEGER ISP1,ISP2,ISP3 ! Pointers to species numbers
      INTEGER ISP4,ISP5,ISP6 ! Pointers to species numbers
      INTEGER NCELL          ! Loop index for number of cells
      INTEGER IKC_TERM       ! Loop index for special of KC terms
      INTEGER I_OPERATOR     ! Loop index for number of preceeding special rates
      INTEGER NRK            ! Reaction number
      INTEGER NRX            ! Loop index for number of reactions

      REAL( 8 ) TERM_COEFF                         ! weighting coefficient
      REAL( 8 ) SPECIAL_RATE( BLKSIZE, NSPECIAL+1 )! sums of weighted products

C..Saved Local Variables: None

      LOGICAL, SAVE :: FIRSTCALL = .TRUE.

      IF ( NSPECIAL .LT. 1 ) RETURN

      SPECIAL_RATE = 0.0D+00

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  loop over reactions and calculate rate of reaction
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO 220 ISP = 1, NSPECIAL

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Compute special rates expressions that sum terms equal to
C  coefficient*rk*concentration
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         LOOP_KC: DO IKC_TERM = 1, MAXSPECTERMS
            TERM_COEFF = KC_COEFFS( ISP, IKC_TERM )
            ISP1 = INDEX_KTERMS( ISP, IKC_TERM )
            IF ( ISP1 .LT. 1 ) CYCLE LOOP_KC  ! empty array entery
            ISP2 = INDEX_CTERMS( ISP, IKC_TERM )
            ISP3 = IOLD2NEW( ISP2, NCS )
            ISP4 = INEW2OLD( ISP3, NCS )

            LOOP_CELL: DO NCELL = 1, NUMCELLS
               IF ( ISP2 .LT. 0 ) THEN   ! first order reaction rate
                  SPECIAL_RATE( NCELL, ISP ) = TERM_COEFF * RK( NCELL, ISP1 )
     &                                       + SPECIAL_RATE( NCELL, ISP )
                  CYCLE LOOP_CELL
               END IF
               SPECIAL_RATE( NCELL, ISP ) = TERM_COEFF
     &                             * RK( NCELL, ISP1 ) * CNEW( NCELL, ISP3 )
     &                             + SPECIAL_RATE( NCELL, ISP )
            END DO LOOP_CELL
         END DO LOOP_KC

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Compute special rates expressions that sum the expression computed in the
C  above loop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         LOOP_OP: DO I_OPERATOR = 1, MAXSPECTERMS
            TERM_COEFF = OPERATOR_COEFFS( ISP, I_OPERATOR )
            ISP1 = OPERATORS( ISP, I_OPERATOR )

            IF ( ISP1 .LT. 1 ) CYCLE LOOP_OP
            DO NCELL = 1, NUMCELLS
               SPECIAL_RATE( NCELL, ISP ) = TERM_COEFF * SPECIAL_RATE( NCELL, ISP1 )
     &                                    + SPECIAL_RATE( NCELL, ISP )
            END DO

         END DO LOOP_OP

220   CONTINUE

      DO 320 ISP = 1, NSPECIAL_RXN ! load results in RK array
         ISP1 = ISPECIAL( ISP, 1 )
         ISP2 = ISPECIAL( ISP, 2 )
         DO NCELL = 1, NUMCELLS
            RK( NCELL, ISP1 ) = RTDAT( 1, ISP1 ) * SPECIAL_RATE( NCELL, ISP2 )

#ifdef Verbose
            if ( firstcall ) then
               write(*,*) 'Using ', rtdat( 1, isp1 ), '*',
     &                    special_rate( ncell, isp2 ), isp2, ' ',
     &                    trim( special( isp2 ) ),
     &                    ' for rate constant ',
     &                    trim( rxlabel( isp1 ) )

               write(*,*) 'For reaction #', isp1, ' ',
     &                    trim( rxlabel( isp1 ) ), ', ',
     &                    'rate constant is ', rk( ncell, isp1 )
            END IF
#endif

         END DO
320   END DO

      IF ( FIRSTCALL ) THEN
         FIRSTCALL = .FALSE.
      END IF

      RETURN
      END
