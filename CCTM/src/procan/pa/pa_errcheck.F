
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

C-----------------------------------------------------------------------
      SUBROUTINE PA_ERRCHECK

C-----------------------------------------------------------------------
C Function: To check the Process Analysis inputs for errors
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified by Jerry Gipson April, 1997 to add ADJC process and to add
C  species = all for IPR outputs
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Dec. 1, 1998 to correct operator name check problem
C  Modified Apr 11, 2000 to account for AE species w/ surface area units
C  J.Young: fix LCHEMFAM array index bug, fix N_IPR_SPC value for no ipr,
C  fix blank in 'Air density' species name (found by Michael Bane in UK)
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI`s M3UTILIO
C  Jul 2016 Jeff Young: modify for model inline
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------

      USE UTILIO_DEFN   
      USE PA_GLOBAL     ! Mech and Grid data used 
      USE CGRID_SPCS, Only: N_CGRID_SPC, CGRID_NAME
      USE PA_IPRVARS
      USE PA_VARS
      USE PA_PARSE
      USE PA_DEFN

      IMPLICIT NONE
      
C Includes: None
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables
      CHARACTER(  4 ) :: TYPE       ! Operator type
      CHARACTER(  7 ) :: PNFLAG     ! Initialization flag
      CHARACTER( 16 ) :: FAM_UNITS  ! Units for first species in a family
      CHARACTER( 16 ) :: VALNAME( MAXDEFNAMES )  ! Vector of valid defined names
      CHARACTER( 16 ) :: SP_UNITS(  MAXFAMMEM )  ! Units for each species

      LOGICAL LDIFF      ! Flag for difference found
      LOGICAL LERROR     ! Flag for an error found
      LOGICAL LFOUND     ! Flag for a name found
      LOGICAL LFIRST     ! Flag for first time
      LOGICAL :: LERREND = .FALSE.   ! Flag to stop processing because of errors
      LOGICAL :: LCHEMFAM( 0:MAXFAMLYS ) ! Flag for gas-chem species family
      LOGICAL LRXINC( MXRXNS )     ! Flag for rxns included in cycles
      LOGICAL LCYUSED( MAXCYCLES ) ! Flag to indicate cycle referenced
      LOGICAL LRXUSED( MAXRXNSUM ) ! Flag to indicate rxnsum referenced
     
      INTEGER CINDX      ! Current index for IPR arrays
      INTEGER HINDX      ! Index for rearranging IPR arrays
      INTEGER INDX       ! Index returned from function INDEX1
      INTEGER JNDX       ! Index returned from function INDEX1
      INTEGER IRRIND     ! Index for IRR reaction number
      INTEGER LINDX      ! Index for rearranging IPR arrays
      INTEGER N          ! Loop index for specis and defined names
      INTEGER NCYC       ! Loop index for number of cycles
      INTEGER NEWNPASP   ! Number of IPR species after ALL species option
      INTEGER NFAM       ! Loop index for number of families
      INTEGER NMEM       ! Loop index for number of family members
      INTEGER NOP        ! Loop index for number of IPR operators
      INTEGER NOPS       ! Number of IPR_OUTPUT operators
      INTEGER NOUT       ! Loop index for number of output requests
      INTEGER NR         ! No. of Rxns found by getrxns
      INTEGER NRX        ! Loop index for reactions
      INTEGER NRXS       ! Loop index for number of reaction sums
      INTEGER NTERM      ! Loop index for number of terms
      INTEGER NUMRXNS    ! No. of IRR reactions
      INTEGER NVALID     ! Number of valid defined names
      INTEGER OFFSET     ! Offset use to compute IPR output index
      INTEGER PAINDX     ! Index for IPR outputs
      INTEGER :: RXNUMS( MXRXNS ) ! Rxn nos. of Rxns found bt getrxns
      INTEGER ASTAT      ! Memory

      REAL    :: COEFFS( MXRXNS )    ! Coefficient found with getrxns
         
C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write log header message
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( MSG, 93000 )
      CALL M3MESG( MSG )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check that all family names are valid
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      NVALID = 0
      INDX = 0
      LCHEMFAM( 0 ) = .FALSE.
      DO 40 NFAM = 1, NFAMLYS
         INDX = INDEX1( FAMNAME( NFAM ), N_CGRID_SPC, CGRID_NAME )
         IF ( INDX .NE. 0 ) THEN
            WRITE( MSG, 94000 ) NFAM, FAMNAME( NFAM )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         INDX = INDEX1( FAMNAME( NFAM ), NVALID, VALNAME )
         IF ( INDX .NE. 0 ) THEN
            WRITE( MSG, 94020 ) NFAM, FAMNAME( NFAM )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         LCHEMFAM( NFAM ) = .TRUE.
         DO NMEM = 1, NUMFAMMEM( NFAM )
            INDX = INDEX1( FAMMEMNAM( NFAM,NMEM ), N_CGRID_SPC, CGRID_NAME ) 
            IF ( INDX .NE. 0 ) THEN               
               FAMSPIDS( NFAM, NMEM ) = INDX
               JNDX = INDEX1( FAMMEMNAM( NFAM,NMEM ), NUMB_MECH_SPC, CHEMISTRY_SPC )              
               IF ( JNDX .EQ. 0 ) LCHEMFAM( NFAM ) = .FALSE.
            ELSE
               WRITE( MSG, 94040 ) NFAM, FAMNAME( NFAM ),
     &                             FAMMEMNAM( NFAM,NMEM )
               CALL M3MESG( MSG )
               LERROR = .TRUE.
            END IF
         END DO

         IF ( .NOT. LERROR ) THEN
            NVALID = NVALID + 1
            VALNAME( NVALID ) = FAMNAME( NFAM )
         END IF
40    CONTINUE
      IF ( LERROR ) LERREND = .TRUE. 
     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check rxnsums inputs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      DO 80 NRXS = 1, NRXSUMS
         LRXUSED( NRXS ) = .FALSE.

         INDX = INDEX1( RXSUMNAME( NRXS ), N_CGRID_SPC, CGRID_NAME )
         IF ( INDX .NE. 0 ) THEN 
            WRITE( MSG, 94060 ) NRXS, RXSUMNAME( NRXS )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         INDX = INDEX1( RXSUMNAME( NRXS ), NVALID, VALNAME )
         IF ( INDX .NE. 0 ) THEN
            WRITE( MSG, 94080 ) NRXS, RXSUMNAME( NRXS )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         DO NTERM = 1, NRXTERMS( NRXS )
            INDX = INDEX1( RXSUMLBL( NRXS,NTERM ), NRXNS, RXLABEL ) 
            IF ( INDX .NE. 0 ) THEN
               RXSUMRN( NRXS, NTERM ) = INDX
            ELSE
               WRITE( MSG, 94100 )
               CALL M3MESG( MSG )
               WRITE( MSG, 94105 ) NRXS, RXSUMNAME( NRXS )
               CALL M3MESG( MSG )
               WRITE( MSG, 94110 ) RXSUMLBL( NRXS,NTERM )
               CALL M3MESG( MSG )
               LERROR = .TRUE.
            END IF
         END DO

         IF ( .NOT. LERROR ) THEN
            NVALID = NVALID + 1
            VALNAME( NVALID ) = RXSUMNAME( NRXS )
         END IF
80    CONTINUE

      IF ( LERROR ) LERREND = .TRUE. 
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check cycle names and cycle species names
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      DO 100 NCYC = 1, NCYCLES
         LCYUSED( NCYC ) = .FALSE.

         INDX = INDEX1( CYCLNAME( NCYC ), N_CGRID_SPC, CGRID_NAME )
         IF ( INDX .NE. 0 ) THEN
            WRITE( MSG, 94120 ) NCYC, CYCLNAME( NCYC )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         INDX = INDEX1( CYCLNAME( NCYC ), NVALID, VALNAME )
         IF ( INDX .NE. 0 ) THEN
            WRITE( MSG, 94140 ) NCYC, CYCLNAME( NCYC )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         LFOUND = .FALSE.
         INDX = INDEX1( CYCLSPEC( NCYC ), NUMB_MECH_SPC, CHEMISTRY_SPC )
         IF ( INDX .NE. 0 ) THEN
            LFOUND = .TRUE.
            CYSPTYP( NCYC ) = 'MECH'
            CYSPNUM( NCYC ) = INDX
         END IF

         INDX = INDEX1( CYCLSPEC( NCYC ), NFAMLYS, FAMNAME ) 
         IF ( INDX .NE. 0 ) THEN
            LFOUND = .TRUE.
            CYSPTYP( NCYC ) = 'FAM'
            CYSPNUM( NCYC ) = INDX
         END IF

         IF ( .NOT. LFOUND ) THEN
            WRITE( MSG, 94160 ) NCYC, CYCLNAME( NCYC ), CYCLSPEC( NCYC )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF
100   CONTINUE
      IF ( LERROR ) LERREND = .TRUE. 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check IRR_OUTPUT species names and reaction labels
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      DO 140 NOUT = 1, NIRROUT

c..check that the output name itself is unique
         INDX = INDEX1( IRRNAME( NOUT ), N_CGRID_SPC, CGRID_NAME )
         IF ( INDX .NE. 0 ) THEN 
            WRITE( MSG, 94180 ) NOUT, IRRNAME( NOUT )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF
         
         INDX = INDEX1( IRRNAME( NOUT ), NVALID, VALNAME )
         IF ( INDX .NE. 0 ) THEN
            WRITE( MSG, 94200 ) NOUT, IRRNAME( NOUT )
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         IF ( .NOT. LERROR ) THEN
            NVALID = NVALID + 1
            VALNAME( NVALID ) = IRRNAME( NOUT )
         END IF

         DO 120 NTERM = 1, NIRRTERMS( NOUT )

c...check names used in production/loss/net operators
            TYPE = OUTTYPE( NOUT,NTERM )
            IF ( TYPE .EQ. 'PROD' .OR. TYPE .EQ. 'NETP' .OR. 
     &           TYPE .EQ. 'LOSS' .OR. TYPE .EQ. 'NETL' .OR.
     &           TYPE .EQ. 'NET' ) THEN

               LFOUND = .FALSE.

               INDX = INDEX1( OUTSPEC1( NOUT,NTERM ), NUMB_MECH_SPC, CHEMISTRY_SPC )
               IF ( INDX .NE. 0 ) THEN
                  LFOUND = .TRUE.
                  OUTS1TYP( NOUT, NTERM ) = 'MECH'
                  OUTS1NUM( NOUT, NTERM ) = INDX
               END IF

               INDX = INDEX1( OUTSPEC1( NOUT,NTERM ), NFAMLYS, FAMNAME )
               IF ( INDX .NE. 0 .AND. LCHEMFAM( INDX ) ) THEN
                  LFOUND = .TRUE.
                  OUTS1TYP( NOUT, NTERM ) = 'FAM'
                  OUTS1NUM( NOUT, NTERM ) = INDX
               END IF
 
               IF ( .NOT. LFOUND ) THEN
                  LERROR = .TRUE.
                  IF ( INDX .NE. 0 .AND. .NOT. LCHEMFAM( INDX ) ) THEN
                     WRITE( MSG, 94220 )
                     CALL M3MESG( MSG )
                     WRITE( MSG, 94260 ) NOUT, IRRNAME( NOUT ) 
                     CALL M3MESG( MSG )
                     WRITE( MSG, 94230 ) OUTSPEC1( NOUT, NTERM )
                     CALL M3MESG( MSG )
                  ELSE
                     WRITE( MSG, 94240 )
                     CALL M3MESG( MSG )
                     WRITE( MSG,94260 ) NOUT, IRRNAME( NOUT ) 
                     CALL M3MESG( MSG )
                     WRITE( MSG, 94280 ) OUTSPEC1( NOUT, NTERM )
                     CALL M3MESG( MSG )
                  END IF
               END IF

               IF ( OUTSPEC2( NOUT,NTERM ) .NE. '' .AND. 
     &              ( TYPE .EQ. 'PROD' .OR. TYPE .NE. 'NETP' ) ) THEN
                  LFOUND = .FALSE.
                  INDX = INDEX1( OUTSPEC2( NOUT,NTERM ), NUMB_MECH_SPC, CHEMISTRY_SPC )
                  IF ( INDX .NE. 0 ) THEN
                     LFOUND = .TRUE.
                     OUTS2TYP( NOUT,NTERM ) = 'MECH'
                     OUTS2NUM( NOUT,NTERM ) = INDX
                  END IF

                  INDX = INDEX1( OUTSPEC2( NOUT,NTERM ), NFAMLYS, FAMNAME )
                  IF ( INDX .NE. 0 .AND. LCHEMFAM( INDX ) ) THEN
                     LFOUND = .TRUE.
                     OUTS2TYP( NOUT,NTERM ) = 'FAM'
                     OUTS2NUM( NOUT,NTERM ) = INDX
                  END IF

                  IF ( OUTSPEC2( NOUT,NTERM ) .EQ. 'hv' ) THEN
                     LFOUND = .TRUE.
                     OUTS2TYP( NOUT,NTERM ) = 'MECH'
                     OUTS2NUM( NOUT,NTERM ) = 0
                  END IF
                        
                  IF ( .NOT. LFOUND ) THEN
                     LERROR = .TRUE.
                     IF ( INDX .NE. 0 .AND. .NOT. LCHEMFAM( INDX ) ) THEN
                        WRITE( MSG, 94220 )
                        CALL M3MESG( MSG )
                        WRITE( MSG, 94260 ) NOUT, IRRNAME( NOUT ) 
                        CALL M3MESG( MSG )
                        WRITE( MSG, 94230 ) OUTSPEC2( NOUT,NTERM )
                        CALL M3MESG( MSG )
                     ELSE
                        WRITE( MSG, 94240 )
                        CALL M3MESG( MSG )
                        WRITE( MSG, 94260 ) NOUT, IRRNAME( NOUT )
                        CALL M3MESG( MSG )
                        WRITE( MSG, 94280 ) OUTSPEC2( NOUT,NTERM )
                        CALL M3MESG( MSG )
                     END IF
                  END IF
               END IF
               
               IF ( OUTSPEC3( NOUT, NTERM ) .NE. '' .AND. 
     &                TYPE .NE. 'NET' ) THEN
                  LFOUND = .FALSE.
                  INDX = INDEX1( OUTSPEC3( NOUT,NTERM ), NUMB_MECH_SPC, CHEMISTRY_SPC )
                  IF ( INDX .NE. 0 ) THEN
                     LFOUND = .TRUE.
                     OUTS3TYP( NOUT,NTERM ) = 'MECH'
                     OUTS3NUM( NOUT,NTERM ) = INDX
                  END IF

                  INDX = INDEX1( OUTSPEC3( NOUT,NTERM ), NFAMLYS, FAMNAME )
                  IF ( INDX .NE. 0 .AND. LCHEMFAM( INDX ) ) THEN
                     LFOUND = .TRUE.
                     OUTS3TYP( NOUT,NTERM ) = 'FAM'
                     OUTS3NUM( NOUT,NTERM ) = INDX
                  END IF

                  IF ( OUTSPEC3( NOUT,NTERM ) .EQ. 'hv ' ) THEN
                     LFOUND = .TRUE.
                     OUTS3TYP( NOUT,NTERM ) = 'MECH'
                     OUTS3NUM( NOUT,NTERM ) = 0
                  END IF
                    
                  IF ( .NOT. LFOUND ) THEN
                     LERROR = .TRUE.
                     IF ( INDX .NE. 0 .AND. .NOT. LCHEMFAM( INDX ) ) THEN
                        WRITE( MSG, 94220 )
                        CALL M3MESG( MSG )
                        WRITE( MSG, 94260 ) NOUT, IRRNAME( NOUT ) 
                        CALL M3MESG( MSG )
                        WRITE( MSG,94230 ) OUTSPEC2( NOUT,NTERM )
                        CALL M3MESG( MSG )
                     ELSE
                        WRITE( MSG, 94240 )
                        CALL M3MESG( MSG )
                        WRITE( MSG, 94260 ) NOUT, IRRNAME( NOUT )
                        CALL M3MESG( MSG )
                        WRITE( MSG, 94280 ) OUTSPEC3( NOUT,NTERM ) 
                        CALL M3MESG( MSG )
                     END IF
                  END IF
               END IF

c..check cycle names or reaction sum names
            ELSE IF ( TYPE .EQ. 'NAME' ) THEN
               LFOUND = .FALSE.
               INDX = INDEX1( OUTSPEC1( NOUT,NTERM ), NRXSUMS, RXSUMNAME )
               IF ( INDX .NE. 0 ) THEN               
                  LFOUND = .TRUE.
                  OUTS1TYP( NOUT, NTERM ) = 'RXSM'
                  OUTS1NUM( NOUT, NTERM ) = INDX
                  LRXUSED( INDX ) = .TRUE.
               END IF
               INDX = INDEX1( OUTSPEC1( NOUT,NTERM ), NCYCLES, CYCLNAME )
               IF ( INDX .NE. 0 ) THEN               
                  LFOUND = .TRUE.
                  OUTS1TYP( NOUT,NTERM ) = 'CYCL'
                  OUTS1NUM( NOUT,NTERM ) = INDX
                  LCYUSED( INDX ) = .TRUE.
               END IF

               IF ( .NOT. LFOUND ) THEN
                  WRITE( MSG, 94300 )
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94320 ) NOUT, IRRNAME( NOUT ) 
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94340 ) OUTSPEC1( NOUT,NTERM )
                  CALL M3MESG( MSG )
                  LERROR = .TRUE.
               END IF

c..check for reaction label name
            ELSE IF ( TYPE .EQ. 'RXN' ) THEN
               LFOUND = .FALSE.
               INDX = INDEX1( OUTRXLBL( NOUT,NTERM ), NRXNS, RXLABEL )
               IF ( INDX .NE. 0 ) THEN               
                  LFOUND = .TRUE.
                  OUTRN( NOUT,NTERM ) = INDX
               END IF

               IF ( .NOT. LFOUND ) THEN
                  WRITE( MSG, 94360 )
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94380 ) NOUT, IRRNAME( NOUT ) 
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94400 ) OUTRXLBL( NOUT,NTERM )
                  CALL M3MESG( MSG )
                  LERROR = .TRUE.
               END IF
            ELSE
               WRITE( MSG, 94420 )
               CALL M3MESG( MSG )
               WRITE( MSG, 94440 ) NOUT, IRRNAME( NOUT )
               CALL M3MESG( MSG )
               WRITE( MSG, 94460 ) TYPE
               CALL M3MESG( MSG )
               LERROR = .TRUE.
            END IF
120      CONTINUE
140   CONTINUE         
      IF ( LERROR ) LERREND = .TRUE.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check IPR_OUTPUT species names
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      IF ( N_IPR_SPC .GT. 0 ) THEN
         OFFSET = 0
         NEWNPASP = N_IPR_SPC
         DO 160 NOUT = 1, N_IPR_SPC
            PAINDX = NOUT + OFFSET 
            LFOUND = .FALSE.
            INDX = INDEX1( IPR_SPNAM( PAINDX ), N_CGRID_SPC, CGRID_NAME )
            IF ( INDX .NE. 0 ) THEN 
               LFOUND = .TRUE.
               IPR_SPTYPE( PAINDX ) = 'SPEC'
               IPR_SPNUM( PAINDX ) = INDX
            END IF
  
            IF ( .NOT. LFOUND ) THEN
               INDX = INDEX1( IPR_SPNAM( PAINDX ), NFAMLYS, FAMNAME )
               IF ( INDX .NE. 0 ) THEN 
                  LFOUND = .TRUE.
                  IPR_SPTYPE( PAINDX ) = 'FAM'
                  IPR_SPNUM( PAINDX ) = INDX
               END IF
            END IF

c..If all species requested, insert all species into position
            IF ( .NOT. LFOUND. AND. 
     &            ( IPR_SPNAM( PAINDX )( 1:3 ) .EQ. 'ALL' .OR.
     &              IPR_SPNAM( NOUT )( 1:3 ) .EQ. 'all' ) ) THEN
               LFOUND = .TRUE.
               NEWNPASP = PAINDX - 1 + N_CGRID_SPC + ( N_IPR_SPC - NOUT )
               IF ( NEWNPASP .GT. MAXIPRSPC ) THEN
                  WRITE( MSG, 94800 )
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94820 ) MAXIPRSPC
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94840 )
                  CALL M3MESG( MSG )
                  CALL M3EXIT( 'ERRCHECK', IZERO, IZERO, ' ', XSTAT1 )
               END IF

               OFFSET = PAINDX - 1 + N_CGRID_SPC - NOUT
               IF ( N_IPR_SPC .GT. NOUT ) THEN
                  CINDX = PAINDX
                  DO N = NOUT + 1, N_IPR_SPC
                     CINDX = CINDX + 1
                     HINDX = N + OFFSET
                     IPR_SPNAM( HINDX ) = IPR_SPNAM( CINDX )
                     N_IPR_OPS( HINDX ) = N_IPR_OPS( CINDX )
                     IF ( N_IPR_OPS( HINDX ) .GT. 0 ) THEN
                        DO NOP = 1, N_IPR_OPS( HINDX )
                           IPR_OPNAME( HINDX, NOP ) = IPR_OPNAME( CINDX,NOP )
                        END DO
                     END IF
                  END DO
               END IF

               LINDX = PAINDX
               DO N = 1, N_CGRID_SPC
                  IPR_SPNAM( LINDX )  = CGRID_NAME( N )
                  N_IPR_OPS( LINDX )  = N_IPR_OPS( PAINDX )
                  IPR_SPTYPE( LINDX ) = 'SPEC'
                  IPR_SPNUM( LINDX )  = N
                  DO NOP = 1, N_IPR_OPS( LINDX )
                     IPR_OPNAME( LINDX,NOP ) = IPR_OPNAME( PAINDX,NOP )
                  END DO
                  LINDX = LINDX + 1
               END DO
            END IF
                         
            IF ( .NOT. LFOUND ) THEN
               WRITE( MSG, 94480 )
               CALL M3MESG( MSG )
               WRITE( MSG, 94500 ) IPR_SPNAM( PAINDX )
               CALL M3MESG( MSG )
               LERROR = .TRUE.
            END IF
160      CONTINUE
         N_IPR_SPC = NEWNPASP
      END IF               
      IF ( LERROR ) LERREND = .TRUE.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check IPR_OUTPUT operators
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      DO 180 NOUT = 1, N_IPR_SPC
         NOPS = N_IPR_OPS( NOUT )
         IF ( NOPS .EQ. 0 ) THEN
            DO N = 1, NPRCS
               IPR_OPNAME( NOUT,N ) = PROCNAME( N )
            END DO
            N_IPR_OPS( NOUT ) = NPRCS
         END IF
180   CONTINUE

      IF ( LERROR ) LERREND = .TRUE.
               
      IF ( LERROR .OR. LERREND ) GO TO 999  ! If errors, skip next error checks

c determine NIRRRXNS
      ALLOCATE( NIRRRXNS( NIRRVAR ), STAT = ASTAT )   ! NIRRVAR .ge. NIRROUT
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 1 allocating IRR variables'
         CALL M3EXIT ( 'PA_ERRCHECK', 0, 0, MSG, XSTAT2 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get IRR reaction numbers excluding those included in CYCLES & RXNSUMS 
c  and check dimension magnitudes
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      MXNIRRRXNS = 0
      DO 300 NOUT = 1, NIRROUT
         NIRRRXNS( NOUT ) = 0
         
c..Flag reactions included in Cycles for elimination in operators
         DO NRX = 1, NRXNS
            LRXINC( NRX ) = .TRUE.
         END DO

         IF ( NOUTCYCS( NOUT ) .GT. 0 ) THEN
            DO N = 1, NOUTCYCS( NOUT )
               NCYC = OUTCYCS( NOUT, N )
               DO NTERM = 1, NCYTERMS( NCYC )
                  INDX = CYRXNUM( NCYC,NTERM )
                  LRXINC( INDX ) = .FALSE.
               END DO
            END DO
         END IF

         DO 280 NTERM = 1, NIRRTERMS( NOUT )
            TYPE = OUTTYPE( NOUT, NTERM ) 
            IF ( TYPE .NE. 'NAME' .AND. TYPE .NE. 'RXN' ) THEN             
               CALL GETRXNS( NOUT, NTERM, NR, RXNUMS, COEFFS, LRXINC )
               IF ( NR .GT. 0 ) THEN
                  NUMRXNS = NIRRRXNS( NOUT ) + NR
                  IF ( NUMRXNS .GT. MAXTERMS ) THEN
                     WRITE( MSG, 94560 )
                     CALL M3MESG( MSG )
                     WRITE( MSG, 94580 ) NOUT, IRRNAME( NOUT )
                     CALL M3MESG( MSG )
                     LERROR = .TRUE.
                     GO TO 300
                  END IF
                  DO NRX = 1, NR
                     INDX = RXNUMS( NRX )
                     IRRIND = NIRRRXNS( NOUT ) + NRX
                     IRRRXN_TMP( NOUT,IRRIND ) = INDX
                     IRRCOEF_TMP( NOUT,IRRIND ) = COEFFS( NRX ) * OUTSC( NOUT,NTERM ) 
                  END DO                     
                  NIRRRXNS( NOUT ) = NIRRRXNS( NOUT ) + NR
               END IF
            ELSE IF ( TYPE .EQ. 'RXN' ) THEN
               NUMRXNS = NIRRRXNS( NOUT ) + 1
               IF ( NUMRXNS .GT. MAXTERMS ) THEN
                  WRITE( MSG, 94560 )
                  CALL M3MESG( MSG )
                  WRITE( MSG, 94580 ) NOUT, IRRNAME( NOUT )
                  CALL M3MESG( MSG )
                  LERROR = .TRUE.
                  GO TO 300 
               END IF
               NIRRRXNS( NOUT ) = NIRRRXNS( NOUT ) + 1                  
               IRRRXN_TMP(  NOUT,NIRRRXNS( NOUT ) ) = OUTRN( NOUT,NTERM ) 
               IRRCOEF_TMP( NOUT,NIRRRXNS( NOUT ) ) = OUTSC( NOUT,NTERM )
            END IF
280      CONTINUE
         MXNIRRRXNS = MAX( MXNIRRRXNS, NIRRRXNS( NOUT ) ) 
300   CONTINUE

      ALLOCATE( IRRRXN( NIRRVAR,MXNIRRRXNS ),
     &          IRRCOEF( NIRRVAR,MXNIRRRXNS ), STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 2 allocating IRR variables'
         CALL M3EXIT ( 'PA_ERRCHECK', 0, 0, MSG, XSTAT2 )
      END IF

      IRRRXN  = IRRRXN_TMP( 1:NIRRVAR,1:MXNIRRRXNS )
      IRRCOEF = IRRCOEF_TMP( 1:NIRRVAR,1:MXNIRRRXNS )

      IF ( LERROR ) LERREND = .TRUE.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check some dimension limits, saving some info
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NUMOUTPOS = 0
      NUMOUTNEG = 0
      NUMOUTIND = 0
      DO NOUT = 1, NIRROUT
         DO NTERM = 1, NIRRTERMS( NOUT )
            TYPE = OUTS1TYP( NOUT, NTERM )
            IF ( TYPE .EQ. 'CYCL' .OR. TYPE .EQ. 'RXSM' ) THEN            
               PNFLAG = OUTPNFLAG( NOUT,NTERM )
               IF ( PNFLAG .EQ. 'POSONLY' ) THEN
                  NUMOUTPOS = NUMOUTPOS + 1
               ELSE IF ( PNFLAG .EQ. 'NEGONLY' ) THEN
                  NUMOUTNEG = NUMOUTNEG + 1
               ELSE IF ( PNFLAG .EQ. '' ) THEN
                  NUMOUTIND = NUMOUTIND + 1
               END IF 
            END IF
         END DO
      END DO

      IF ( NUMOUTPOS .GT. MAXTERMS .OR. NUMOUTNEG .GT. MAXTERMS .OR.
     &     NUMOUTIND .GT. MAXTERMS ) THEN
         WRITE( MSG, 94600 )  MAXTERMS
         CALL M3MESG( MSG )
         LERREND = .TRUE.
      END IF

      IF ( LFULLIRR .AND. NRXNS .GT. MAXIRROUT ) THEN
         WRITE( MSG, 94610 )  MAXIRROUT
         CALL M3MESG( MSG )
         LERREND = .TRUE.
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to see that a species is not named more than once in a FAMILY
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      LFIRST = .TRUE.
      DO NFAM = 1, NFAMLYS
         DO NMEM = 1, NUMFAMMEM( NFAM )
            DO N = 1, NUMFAMMEM( NFAM )
               IF ( NMEM .NE. N ) THEN
                  IF ( FAMMEMNAM( NFAM,NMEM ) .EQ. 
     &                FAMMEMNAM( NFAM,N ) ) THEN
                     IF ( LFIRST ) THEN
                        WRITE( MSG, 94620 ) 
                        CALL M3MESG( MSG )
                        LFIRST = .FALSE.
                     END IF
                     WRITE( MSG, 94640 ) NFAM, FAMNAME( NFAM ), 
     &                                   FAMMEMNAM( N,NMEM )
                     CALL M3MESG( MSG )
                     LERROR = .TRUE.
                  END IF
               END IF
            END DO
         END DO
      END DO
      IF ( LERROR ) LERREND = .TRUE. 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to see that all species in a FAMILY have the same units
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LERROR = .FALSE.
      DO NFAM = 1, NFAMLYS
         LDIFF = .FALSE.
         DO NMEM = 1, NUMFAMMEM( NFAM )
            SP_UNITS( NMEM ) = 'ppm'
            INDX = INDEX1( FAMMEMNAM( NFAM,NMEM ), N_CGRID_SPC, CGRID_NAME )
            IF ( NMEM .EQ. 1 ) THEN
               FAM_UNITS = SP_UNITS( NMEM )
            ELSE
               IF ( SP_UNITS( NMEM ) .NE. FAM_UNITS ) LDIFF = .TRUE.   
            END IF
         END DO

         IF ( LDIFF ) THEN
            LERROR = .TRUE.
            WRITE( MSG, 94650 ) NFAM, FAMNAME( NFAM ) 
            CALL M3MESG( MSG )
            DO NMEM = 1, NUMFAMMEM( NFAM )
               WRITE( MSG,94652 ) NMEM, FAMMEMNAM( NFAM,NMEM ), SP_UNITS( NMEM ) 
               CALL M3MESG( MSG )
            END DO
         END IF
      END DO                   
      IF ( LERROR ) LERREND = .TRUE. 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to see if any defined CYCLES or RXNSUMS are not used
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LFIRST = .TRUE.
      DO NCYC = 1, NCYCLES
         IF ( .NOT. LCYUSED( NCYC ) ) THEN
            IF ( LFIRST ) THEN 
               WRITE( MSG, 94660 ) 
               CALL M3MESG( MSG )
               LFIRST = .FALSE.
            END IF
            WRITE( MSG, 94680 ) NCYC, CYCLNAME( NCYC )
            CALL M3MESG( MSG )
         END IF
      END DO

      LFIRST = .TRUE.
      DO NRXS = 1, NRXSUMS
         IF ( .NOT. LRXUSED( NRXS ) ) THEN
            IF ( LFIRST ) THEN 
               WRITE( MSG, 95700 ) 
               CALL M3MESG( MSG )
               LFIRST = .FALSE.
            END IF
            WRITE( MSG, 95720 ) NRXS, RXSUMNAME( NRXS )
            CALL M3MESG( MSG )
         END IF
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Return or stop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
999   CONTINUE
      IF ( LERREND ) THEN
         WRITE( MSG, 95740 )
         CALL M3MESG( MSG )
         CALL M3EXIT( 'ERRCHECK', IZERO, IZERO, ' ', XSTAT1 )
      ELSE
         WRITE( MSG, 95760 )
         CALL M3MESG( MSG )
         RETURN
      END IF
 
C---------------------- Format Statements -----------------------------     
      
93000 FORMAT( 'Entering the Process Analysis error checking routine' )
94000 FORMAT( 'ERROR: FAMILY name ', I3, ' ', A16, 
     &               ' duplicates a species name' )
94020 FORMAT( 'ERROR: FAMILY name ', I3, ' ', A16, ' duplicates another',
     &               ' DEFINEd name' )
94040 FORMAT( 'ERROR: Invalid member name for FAMILY ', I3, ' ', A16,
     &               ':', A16 )
94060 FORMAT( 'ERROR: RXNSUM name ', I3, ' ', A16,
     &               ' duplicates a species name' )
94080 FORMAT( 'ERROR: RXNSUM name ', I3, ' ', A16,' duplicates another',
     &               ' DEFINEd name' )
94100 FORMAT( 'ERROR: Invalid reaction label for RXNSUM' )
94105 FORMAT( '       RXNSUM name ', I3, ': ', A16 )
94110 FORMAT( '       Invalid reaction label: ', A16 )         
94120 FORMAT( 'ERROR: CYCLE name ', I3, ' ', A16,
     &               ' duplicates a species name' )
94140 FORMAT( 'ERROR: CYCLE name ', I3, ' ', A16, ' duplicates another',
     &               ' DEFINEd name' )
94160 FORMAT( 'ERROR: Invalid species name for CYCLE ', I3,' ',A16,
     &               ': ', A16 ) 
94180 FORMAT( 'ERROR: IRR_OUTPUT name ', I3, ' ', A16, ' duplicates',
     &               ' a species name' )
94200 FORMAT( 'ERROR: IRR_OUTPUT name ', I3, ' ', A16, ' duplicates',
     &               ' another DEFINEd name' )
94220 FORMAT( 'ERROR: Family in an IRR_OUTPUT operator contains a non-',
     &               'gas chemistry species' )
94230 FORMAT( '       Family name: ', A16 )
94240 FORMAT( 'ERROR: Invalid species name in an IRR_OUTPUT operator' )
94260 FORMAT( '       IRR_OUTPUT name ', I3, ': ', A16 )
94280 FORMAT( '       Invalid name: ', A16 ) 
94300 FORMAT( 'ERROR: Invalid CYCLE or RXNSUM name in an IRR_OUTPUT' )
94320 FORMAT( '       IRR_OUTPUT name ', I3, ': ', A16 )
94340 FORMAT( '       Invalid name: ', A16 )
94360 FORMAT( 'ERROR: Invalid reaction label for an IRR_OUTPUT' )
94380 FORMAT( '       IRR_OUTPUT name ', I3, ': ', A16 )
94400 FORMAT( '       Invalid reaction label: ', A16 ) 
94420 FORMAT( 'ERROR: Invalid operation type for IRR_OUTPUT' )
94440 FORMAT( '       IRR_OUTPUT name ', I3, ': ', A16 )
94460 FORMAT( '       Invalid operation name: ', A16 ) 
94480 FORMAT( 'ERROR: Invalid species name for IPR_OUTPUT operation' )
94500 FORMAT( '       Invalid name: ', A16 )
94560 FORMAT( 'ERROR: The total number of reactions in the IRR_OUTPUT',
     &               ' operation exceeds the maximum allowable of 500' )
94580 FORMAT( '       IRR_OUTPUT name ', I3, ': ', A16 )
94600 FORMAT( 'ERROR: The number of CYCLEs plus RXSUMs exceeds the',
     &               ' maximum allowable of 500' )
94610 FORMAT( 'ERROR: The number of reactions for IRRTYPE=FULL exceeds',
     &               ' the maximum allowable of 500' )
94620 FORMAT( 'ERROR: A species is listed more than once in a FAMILY' )
94640 FORMAT( '       FAMILY ', I3, ': ', A,'   SPECIES: ', A )
94650 FORMAT( 'ERROR: Inconsistent species units in FAMILY ', I3, ': ', A )
94652 FORMAT( '       SPECIES ', I3, ': ', A, '    UNITS: ', A )
94660 FORMAT( 'WARNING: The following cycles were defined but not used:' )
94680 FORMAT( '         CYCLE ', I3, ': ', A )
95700 FORMAT( 'WARNING: The following rxsums were defined but not used:' )
95720 FORMAT( '         RXSUM ', I3, ': ', A )
95740 FORMAT( 'STOPPING in ERRCHECK because of errors' )
95760 FORMAT( 'No input errors detected, continuing....' ) 
94800 FORMAT( 'ERROR: Maximum number of PA species exceeded' )
94820 FORMAT( '       Modify PARAMETER ( MAXIPRSPC =', I3,' ) or decrease',
     &              ' the number of PA species' )
94840 FORMAT( '       Error occurred while processing species option ',
     &               ' ALL' )

      END SUBROUTINE PA_ERRCHECK
