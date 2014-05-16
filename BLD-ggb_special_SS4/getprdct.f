
C***************************************************************************
C  Significant portions of Models-3/CMAQ software were developed by        *
C  Government employees and under a United States Government contract.     *
C  Portions of the software were also based on information from non-       *
C  Federal sources, including software developed by research institutions  *
C  through jointly funded cooperative agreements. These research institu-  *
C  tions have given the Government permission to use, prepare derivative   *
C  works, and distribute copies of their work to the public within the     *
C  Models-3/CMAQ software release and to permit others to do so. EPA       *
C  therefore grants similar permissions for use of Models-3/CMAQ software, *
C  but users are requested to provide copies of derivative works to the    *
C  Government without re-strictions as to use by others.  Users are        *
C  responsible for acquiring their own copies of commercial software       *
C  associated with the Models-3/CMAQ release and are also responsible      *
C  to those vendors for complying with any of the vendors' copyright and   *
C  license restrictions. In particular users must obtain a Runtime license *
C  for Orbix from IONA Technologies for each CPU used in Models-3/CMAQ     *
C  applications.                                                           *
C                                                                          *
C  Portions of I/O API, PAVE, and the model builder are Copyrighted        *
C  1993-1997 by MCNC--North Carolina Supercomputing Center and are         *
C  used with their permissions subject to the above restrictions.          *
C***************************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)GETPRDCT.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETPRDCT.F 02 Jan 1997 15:26:44

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETPRDCT ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD,
     &                      NXX, NS, SPCLIS, SPC1RX,
     &                      ICOL, IRXBITS, IRR, SC,
     &                      N_DROP_SPC, DROP_SPC,
     &                      N_SS_SPC, SS_SPC, SS_PRD_COEF )
 

C=======================================================================
C sets up all product species and stoichiometric coefficients
C Maintain ITAB, which controls stoic. coeff. table, whereas ICOL
C controls NPRDCTS
C input: IMECH (for RDLINE), NXX, ICOL
C output: IRR, IRXBITS, SC
C updates: NS, SPCLIS
C=======================================================================
      IMPLICIT NONE
      INCLUDE 'PARMS.e'
      CHARACTER(  1 ) :: CHR
!     CHARACTER(  4 ) :: WORD
      CHARACTER( 16 ) :: WORD
      CHARACTER( 81 ) :: INBUF
      INTEGER IMECH, IEOL, LPOINT
      CHARACTER( 16 ) :: SPECIES, SPCLIS( MAXSPEC )
      INTEGER NXX, NS, NSPEC
      INTEGER ICOL
      INTEGER IRXBITS( MAXRXNUM )
      INTEGER IRR( MAXRXNUM,MAXPRODS+3 )
      REAL    SC( MAXRXNUM,MAXPRODS )
      INTEGER SPC1RX( MAXSPEC )
      INTEGER ICHR
      LOGICAL LCOEFF, LNEG
      INTEGER, SAVE :: ITAB
      REAL( 8 )    :: NUMBER

c..ELIMINATE related variables
      INTEGER          :: N_DROP_SPC
      CHARACTER*( 16 ) :: DROP_SPC( MAXNLIST )

c..STEADY_STATE related variables
      INTEGER SS_INDEX
      INTEGER          :: N_SS_SPC
      CHARACTER*( 16 ) :: SS_SPC( MAXNLIST )
      REAL             :: SS_PRD_COEF( MAXNLIST, MAXRXNUM )
      REAL             :: SS_COEF

      INTEGER, EXTERNAL :: INDEX1


      IF ( ICOL .EQ. 3 )THEN
           ITAB = 0  ! ICOL = 3 initially for each reaction
      ENDIF
      ITAB = ITAB + 1
      LCOEFF = .FALSE.
      LNEG = .FALSE.     
      IF ( CHR .EQ. '+' ) THEN
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      ELSE IF ( CHR .EQ. '-' ) THEN
         LNEG = .TRUE.
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      END IF
      ICHR = ICHAR ( CHR )
C characters 0,1,2,...,9
      IF ( ICHR .GE. 48 .AND. ICHR .LE. 57 ) LCOEFF = .TRUE.
      IF ( CHR .EQ. '.' ) LCOEFF = .TRUE.
      IF ( LCOEFF ) THEN
         CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
         IF ( CHR .EQ. '*' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         ELSE
            WRITE( *,2003 ) INBUF
            STOP
         END IF
      END IF
      CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
      SPECIES = WORD

c..Skip any product that is in the eliminate list
      IF( INDEX1( SPECIES, N_DROP_SPC, DROP_SPC ) .NE. 0 ) THEN
         ITAB = ITAB - 1
         RETURN
      ENDIF

c..Skip any steady-state species, but sum-up its coefficients in each reaction
      SS_INDEX = INDEX1( SPECIES, N_SS_SPC, SS_SPC )
      IF( SS_INDEX .NE. 0 ) THEN
         IF(       LCOEFF .AND.       LNEG )    SS_COEF = -NUMBER
         IF(       LCOEFF .AND. .NOT. LNEG )    SS_COEF = NUMBER
         IF( .NOT. LCOEFF .AND.       LNEG )    SS_COEF = -1.0
         IF( .NOT. LCOEFF .AND. .NOT. LNEG )    SS_COEF = 1.0
         SS_PRD_COEF( SS_INDEX, NXX ) = SS_PRD_COEF( SS_INDEX, NXX ) + SS_COEF
         ITAB = ITAB - 1
         RETURN
      ENDIF

      IF ( SPECIES( 1:4 ) .EQ. 'M   '  .OR.
     &     SPECIES( 1:4 ) .EQ. 'm   ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 8)
      ELSE IF ( SPECIES( 1:4 ) .EQ. 'H2O '  .OR.
     &          SPECIES( 1:4 ) .EQ. 'h2o ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 9)
      ELSE IF ( SPECIES( 1:4 ) .EQ. 'O2  '  .OR.
     &          SPECIES( 1:4 ) .EQ. 'o2  ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 10)
      ELSE IF ( SPECIES( 1:4 ) .EQ. 'N2  '  .OR.
     &          SPECIES( 1:4 ) .EQ. 'n2  ' ) THEN
         IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 11)
      ELSE
         ICOL = ICOL + 1
         IF ( ICOL .GT. MAXPRODS+3 ) THEN
            WRITE( *,2005 ) INBUF
            STOP
         END IF
         CALL LKUPSPEC ( NS, SPECIES, SPCLIS, NXX, SPC1RX, NSPEC )
         IRR( NXX,ICOL ) = NSPEC
      END IF  ! SPECIES .EQ. 'M   '

      IF ( LCOEFF ) THEN
         IF ( LNEG ) THEN
            SC( NXX,ITAB ) = -NUMBER
         ELSE
            SC( NXX,ITAB ) = NUMBER
         END IF
      ELSE IF ( LNEG ) THEN
         SC( NXX,ITAB ) = -1.0
      ELSE
         SC( NXX,ITAB ) = 1.0
      END IF
      
      RETURN
2001  FORMAT( / 5X, '*** ERROR: Equal sign expected after reactants'
     &        / 5X, 'Last line read was:' / A81 )
2003  FORMAT( / 5X, '*** ERROR: An asterisk must follow a coefficient'
     &        / 5X, 'Last line read was:' / A81 )
2005  FORMAT( / 5X, '*** ERROR: Maximum number of products exceeded'
     &        / 5X, 'Last line read was:' / A81 )      
      END
