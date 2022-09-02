
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/grpderiv.F,v 1.4 2011/10/21 16:11:14 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)grpderiv.F        1.1 /project/mod3/CMAQ/src/chem/smvgear/SCCS/s.grpderiv.  F 07 Jul 1997 12:45:28

       SUBROUTINE PDERIV

C***********************************************************************
C
C  FUNCTION: Compute [P]=[I]-bh[J] where J is the Jacobian matrix,
C            (i.e., [J] = d[dCi/dt]/dCi), b is the Gear coefficient,
C            and h is the time-step
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995,
C                      Based on  the code originally developed by 
C                      M. Jacobson, (Atm. Env., Vol 28, No 2, 1994).
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      the Models-3 minimum IOV configuration
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Revised April 1997 to distinguish NSPCS from NSPCSD
C                    Revised April 1997 to conform to Models-3 framework
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C                    Modified September, 1997 by Jerry Gipson to be 
C                      consistent with the targeted CTM
C                    16 Aug 01 J.Young: Use HGRD_DEFN
C                    31 Jan 05 J.Young: get BLKSIZE from dyn alloc horizontal
C                    & vertical domain specifications module (GRID_CONF)
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    29 Mar 11 S.Roselle: Replaced I/O API include files 
C                    with UTILIO_DEFN
C                    22 Aug 11 J.Young: fixed bug: initialize CC2( NCELL,0 )
C                    15 Jul 14 B.Hutzell: replaced mechanism include files with 
C                    RXNS_DATA module
C***********************************************************************

      USE RXNS_DATA
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE GRVARS                ! inherits GRID_CONF

      IMPLICIT NONE
      
C..INCLUDES: None
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:

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
      INTEGER IOS            ! Allocate status
      
      REAL( 8 ) :: CR2                   ! Temporary product for 3 reactant rxns
      REAL( 8 ) :: FRACN                 ! Stoichiometric coeff. times b*h
      REAL( 8 ) :: EXPLIC( BLKSIZE, 3 )  ! Reaction partial derivative terms
      
      REAL( 8 ), ALLOCATABLE, SAVE :: CEFF( :, : ) ! Effective species concentrations
                                                   ! (i.e., zeroed if below ZBOUND)

      LOGICAL, SAVE    :: LFIRST = .TRUE.    ! Flag for first call to this subroutine
      CHARACTER(  16 ) :: PNAME = 'GRPDERIV' ! Procedure name
      CHARACTER( 144 ) :: MSG                ! Message text

C***********************************************************************

      IF ( LFIRST ) THEN
         LFIRST = .FALSE.
         ALLOCATE ( CEFF( BLKSIZE, NUMB_MECH_SPC ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation error for CEFF'
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
         END IF
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Increment call counter and set up identity matrix stored in sparse
c  matrix format (i.e., values of cc2 are entries in P that may be 
c  nonzero). Diagonals come after non-diagonal entries.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NPDERIV = NPDERIV + 1
      IARRY = IARRAY( NCSP ) 
      NONDIAG = IARRY - ISCHAN  
      NONDIAG1 = NONDIAG + 1
!     DO IAR = 1, NONDIAG
      DO IAR = 0, NONDIAG
         DO NCELL = 1, NUMCELLS
            CC2( NCELL, IAR ) = 0.0D0
         END DO
      END DO
      DO IAR = NONDIAG1, IARRY
         DO NCELL = 1, NUMCELLS
            CC2( NCELL, IAR ) = 1.0D0
         END DO
      END DO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set effective concentrations to be used in PD calculations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO ISPC=1,ISCHAN
         DO NCELL=1,NUMCELLS
            IF( CNEW( NCELL, ISPC ) .LE. ZBOUND ) THEN
               CEFF( NCELL, ISPC ) = 0.0D0
            ELSE
               CEFF( NCELL, ISPC ) = CNEW( NCELL, ISPC )
            ENDIF
         END DO
      END DO
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Loop over reaction rates adding partial derivative terms; EXPLIC
c  holds the PD terms according to number of reactants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 240 NRX = 1, NUSERAT( NCSP )
         NRK = NKUSERAT( NRX, NCSP )
         
c...partial derivative term for reactions with 1 reactant
         IF( NREACT( NRK ) .EQ. 1 ) THEN
            DO NCELL = 1, NUMCELLS
               EXPLIC( NCELL, 1 )  = RK( NCELL, NRK ) 
            END DO
  
c...partial derivative terms for reactions with 2 reactants
         ELSEIF( NREACT( NRK ) .EQ. 2 ) THEN
            JR1 = IRM2( 1, NRK, NCS )
            JR2 = IRM2( 2, NRK, NCS )
            DO NCELL  = 1, NUMCELLS
               EXPLIC( NCELL, 1 )  = RK( NCELL, NRK ) * CEFF( NCELL, JR2 )
               EXPLIC( NCELL, 2 )  = RK( NCELL, NRK ) * CEFF( NCELL, JR1 )
            END DO
 
c.....partial derivative terms for reactions with 3 reactants
         ELSEIF( NREACT( NRK ).EQ.3 ) THEN
            JR1 = IRM2( 1, NRK, NCS )
            JR2 = IRM2( 2, NRK, NCS )
            JR3 = IRM2( 3, NRK, NCS )
            DO NCELL = 1, NUMCELLS
               CR2 = RK( NCELL, NRK ) * CEFF( NCELL, JR2 )
               EXPLIC( NCELL, 1 ) = CR2 * CEFF( NCELL, JR3 )
               EXPLIC( NCELL, 2 ) = RK(   NCELL, NRK )
     &                            * CEFF( NCELL, JR1 ) * CEFF( NCELL, JR3 ) 
               EXPLIC( NCELL, 3 ) = CR2 * CEFF( NCELL, JR1 )
            END DO
         ENDIF
         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Add PD terms to [P] for this reaction
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c...loss terms
         NLD = NDERIVL( NRK, NCSP )         
         DO NL = 1, NLD
            IARP = JARRL( NL, NRK, NCSP )
            IALP = JLIAL( NL, NRK, NCSP )
            DO NCELL = 1, NUMCELLS
               CC2( NCELL, IARP ) = CC2( NCELL, IARP )
     &                           - R1DELT * EXPLIC( NCELL, IALP ) 
            END DO
         END DO
  
c...production terms with stoichiomteric coeff EQ 1.0 and NE 1.0
         NPD = NDERIVP( NRK, NCSP )
         DO NP = 1, NPD
            IARP = JARRP( NP, NRK, NCSP )
            IALP = JPIAL( NP, NRK, NCSP )
            IF( ICOEFF( NP, NRK, NCSP ) .EQ. 0 ) THEN
               DO NCELL = 1, NUMCELLS
                  CC2( NCELL, IARP ) = CC2( NCELL, IARP ) 
     &                              + R1DELT * EXPLIC( NCELL, IALP ) 
               END DO
            ELSE
               ISCP = ICOEFF( NP, NRK, NCSP )
               FRACN = REAL( SC( NRK, ISCP ), 8 ) * R1DELT 
               DO NCELL = 1, NUMCELLS
                  CC2( NCELL, IARP ) = CC2( NCELL, IARP ) 
     &                               + FRACN * EXPLIC( NCELL, IALP ) 
               END DO 
            ENDIF
         END DO 

240   CONTINUE

      RETURN 
      END
