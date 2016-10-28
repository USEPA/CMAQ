
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
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/rbjacob.F,v 1.3 2011/10/21 16:11:10 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE RBJACOB( NCSP, YIN )

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
C                    15 Jul 14 B.Hutzell: replaced mechanism include files with 
C                    RXNS_DATA module and added intent declarations to arguments
C
C***********************************************************************

      USE RXNS_DATA
      USE RBDATA                ! ROS3 solver data

      IMPLICIT NONE

C..Includes:

C..Arguments:
      INTEGER,   INTENT( IN ) :: NCSP         ! Index of chem mech to use; 1=gas/day, 2=gas/night
      REAL( 8 ), INTENT( IN ) :: YIN( :, : )    ! Species concs, ppm

C..Parameters: None

C..External Functions: None

C..Local Variables:
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
      
      REAL( 8 ) :: CR2                   ! Temporary product for 3 reactant rxns
      REAL( 8 ) :: FRACN                 ! Stoichiometric coeff. times b*h
      REAL( 8 ) :: EXPLIC( BLKSIZE,3 )   ! Reaction partial derivative terms

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Zero out Jacobian ( stored in sparse matrix array cc2 )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IARRY = IARRAY( NCSP ) 
      NONDIAG = IARRY - ISCHAN  
      NONDIAG1 = NONDIAG + 1
!     DO IAR = 1, NONDIAG
      DO IAR = 0, NONDIAG
         DO NCELL = 1, NUMCELLS
            CC2( NCELL,IAR ) = 0.0D0
         END DO
      END DO
      DO IAR = NONDIAG1, IARRY
         DO NCELL = 1, NUMCELLS
            CC2( NCELL,IAR ) = 0.0D0
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over reaction rates adding partial derivative terms; EXPLIC
c  holds the PD terms according to number of reactants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 240 NRX = 1, NUSERAT( NCSP )

         NRK = NKUSERAT( NRX,NCSP )
         
c...partial derivative term for reactions with 1 reactant
         IF ( NREACT( NRK ) .EQ. 1 ) THEN
            DO NCELL = 1, NUMCELLS
               EXPLIC( NCELL,1 ) = RKI( NCELL,NRK ) 
            END DO
  
c...partial derivative terms for reactions with 2 reactants
         ELSE IF ( NREACT( NRK ) .EQ. 2 ) THEN
            JR1 = IRM2( NRK,1,NCS )
            JR2 = IRM2( NRK,2,NCS )
            DO NCELL  = 1, NUMCELLS
               EXPLIC( NCELL,1 )  = RKI( NCELL,NRK )
     &                            * YIN( NCELL,JR2 )
               EXPLIC( NCELL,2 )  = RKI( NCELL,NRK )
     &                            * YIN( NCELL,JR1 )
            END DO
 
c.....partial derivative terms for reactions with 3 reactants
         ELSE IF ( NREACT( NRK ) .EQ. 3 ) THEN
            JR1 = IRM2( NRK,1,NCS )
            JR2 = IRM2( NRK,2,NCS )
            JR3 = IRM2( NRK,3,NCS )
            DO NCELL = 1, NUMCELLS
               CR2 = RKI( NCELL,NRK ) * YIN( NCELL,JR2 )
               EXPLIC( NCELL,1 ) = CR2 * YIN( NCELL,JR3 )
               EXPLIC( NCELL,2 ) = RKI( NCELL,NRK )
     &                           * YIN( NCELL,JR1 )
     &                           * YIN( NCELL,JR3 ) 
               EXPLIC( NCELL,3 ) = CR2 * YIN( NCELL,JR1 )
            END DO
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Add PD terms to [J] for this reaction
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c...loss terms
         NLD = NDERIVL( NRK,NCSP )         
         DO NL = 1, NLD
            IARP = JARRL( NRK,NL,NCSP )
            IALP = JLIAL( NRK,NL,NCSP )
            DO NCELL = 1, NUMCELLS
               CC2( NCELL,IARP ) = CC2( NCELL,IARP ) - EXPLIC( NCELL,IALP ) 
            END DO
         END DO    ! End loop over loss terms

c...production terms with stoichiomteric coeff EQ 1.0 and NE 1.0
         NPD = NDERIVP( NRK,NCSP )
         DO 220 NP = 1, NPD

            IARP = JARRP( NRK,NP,NCSP )
            IALP = JPIAL( NRK,NP,NCSP )

            IF ( ICOEFF( NRK,NP,NCSP ) .EQ. 0 ) THEN
c..production terms with unit stoichiometry
               DO NCELL = 1, NUMCELLS
                  CC2( NCELL,IARP ) = CC2( NCELL,IARP ) + EXPLIC( NCELL,IALP ) 
               END DO

            ELSE
c..production terms with non-unit stoichiometry
               ISCP = ICOEFF( NRK,NP,NCSP )
               FRACN = SC( NRK,ISCP ) 
               DO NCELL = 1, NUMCELLS
                  CC2( NCELL,IARP ) = CC2( NCELL,IARP ) + FRACN
     &                              * EXPLIC( NCELL,IALP ) 
               END DO
 
            END IF

220      CONTINUE      ! End loop over production terms

240   CONTINUE      ! End loop over reactions

      RETURN 
      END
