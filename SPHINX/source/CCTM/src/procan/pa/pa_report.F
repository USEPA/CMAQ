
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

      SUBROUTINE PA_REPORT

C-----------------------------------------------------------------------
C Function: To provide an output listing that symbolically represents
C           the operations to be performed in Process Analysis 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified Jun, 1998 by Jerry Gipson to change name of output report
C  Modified May 2002 by Jerry Gipson to add comma in format statements
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI`s M3UTILIO
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE PA_GLOBAL     ! Mech data used 
      USE PA_VARS
      USE PA_IPRVARS
      USE PA_DEFN, Only: LFULLIRR, LPARTIRR, N_IPR_SPC, IRRNAME
      USE PA_PARSE
      use get_env_module 

      IMPLICIT NONE
      
C Includes: None
C Arguments: None
C Parameters:
      CHARACTER( 20 ), PARAMETER :: PACM_REPORT = 'PACM_REPORT'  
      CHARACTER( 16 ), PARAMETER :: PNAME = 'PA_REPORT'  

C External Functions: None

C Local Variables
      CHARACTER(  1 ) :: C = 'c'   ! Lowercase c
      CHARACTER(  3 ) :: LABL      ! Output reaction label or term number
      CHARACTER(  3 ) :: OUTLBL    ! Output label number
      CHARACTER(  4 ) :: OPNAM     ! Operator variable name
      CHARACTER(  4 ) :: TYPE      ! Operator type
      CHARACTER(  7 ) :: PNFLG     ! Pos or Neg only flag
      CHARACTER( 11 ) :: COUT      ! Output coefficient
      CHARACTER( 80 ) :: CEES      ! Line of asterisks
      CHARACTER( 80 ) :: LINOUT    ! line of output
      CHARACTER(  5 ) :: TZ        ! Time zone label
      CHARACTER( 24 ) :: TIMNDAT   ! Wall clock time and date

      CHARACTER( 50 ) :: STRING( 20 )             ! Strings to concatenate
      CHARACTER( LABLEN ) :: RJLABL( MXRXNS ) ! Right justified reaction label
      CHARACTER(  4 ) ::  SAVOP( MAXTERMS )        ! Operator name
      CHARACTER(  3 ) ::  SAVLBL( MAXTERMS )       ! Operator label

      INTEGER E1, E2, E3 ! Ending character positions
      INTEGER EQPOS1     ! Position of equal sign plus 1
      INTEGER ICOUNT     ! Counter of no. of output terms
      INTEGER IE1, IE2   ! Pointer to ending positions in a string
      INTEGER IEND       ! Pointer to end of output line 
      INTEGER INDX       ! Pointer to reaction number
      INTEGER IOUT       ! Unit number for output report   
      INTEGER IS1, IS2   ! Pointer to startinging positions in a string
      INTEGER LEN1       ! Length of a string
      INTEGER :: LINLEN = 80  ! Length of output line
      INTEGER MAXLEN     ! Length of longest reaction label
      INTEGER N          ! Loop index for specis and defined names
      INTEGER NCYC       ! Loop index for number of cycles
      INTEGER NNETR      ! No. of net reaction operators in OUTPUT request
      INTEGER NNLOS      ! No. of net loss operators in OUTPUT request
      INTEGER NLN        ! Loop index for no. of mech lines
      INTEGER NLOSS      ! No. of loss operators in OUTPUT request
      INTEGER NFAM       ! Loop index for number of familys
      INTEGER NNPRD      ! No. of net production operators in OUTPUT request
      INTEGER NOUT       ! Loop index for number of output requests
      INTEGER NIPR       ! Loop index for IPR species
      INTEGER NPROD      ! No. of net production operators in OUTPUT request
      INTEGER NRX        ! Loop index for number of reactions
      INTEGER NR         ! Total number of reactions in operator
      INTEGER NTERM      ! Loop index for number of terms in output request
      INTEGER NUMSTRNGS  ! Number of strings to concatenate
      INTEGER SPOS       ! Starting position of right justified label
      INTEGER IDATE      ! Wall-clock date
      INTEGER ITIME      ! Wall clock time

      INTEGER RXNUMS( MXRXNS )  ! Rx nos. of reactions in operator term

      LOGICAL LFIRST     ! Flag for first time throuhh a loop
      LOGICAL LINITFLG   ! Flag to indicate initialization present

      LOGICAL LRDONLY    ! Flag for read only file
      LOGICAL LFMTTED    ! Flag for formatted file

      LOGICAL LRXINC( MXRXNS )  ! Flag to include reaction in prod,
                                ! loss, or net operations
      REAL    COEFFS( MXRXNS )  ! Coefficients of operator reactions
      INTEGER ASTAT
      CHARACTER(200) :: PACM_NAME

C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open file and write report header
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LRDONLY = .FALSE.
      LFMTTED = .TRUE.

      !IOUT = GETEFILE ( PACM_REPORT, LRDONLY, LFMTTED, PNAME )
      IOUT = JUNIT()
      CALL GET_ENV( PACM_NAME, 'PACM_REPORT', 'PACM_REPORT' )
      OPEN ( UNIT = IOUT, FILE = PACM_NAME, STATUS='UNKNOWN', FORM='FORMATTED', IOSTAT=ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR: Could not open PA_REPORT output file'
         CALL M3EXIT( PNAME, IZERO, IZERO, MSG, ASTAT )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the time and date to be used as the time stamp for all outputs;
c  Convert from GMT to EDT or EST
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GETDTTIME( IDATE, ITIME )

      IF ( ISDSTIME( IDATE ) ) THEN
         CALL NEXTIME( IDATE, ITIME, -40000 )
         TZ = ' EDT '
      ELSE
         CALL NEXTIME( IDATE, ITIME, -50000 )
         TZ = ' EST '
      END IF

      TIMNDAT = DT2STR( IDATE, ITIME )
      TDATOUT = TIMNDAT( 1:8 ) // TZ // TIMNDAT( 10:24 )

      WRITE( IOUT, 93020 ) TDATOUT
      WRITE( IOUT, 93000 )

      DO N = 1, 80
         CEES( N:N ) = C
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the integrated process rate section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LINOUT = CEES
      WRITE( IOUT, 93000 ) LINOUT
      IF ( N_IPR_SPC .LE. 0 ) THEN
         WRITE( IOUT, 93080 )
         LINOUT = CEES
         WRITE( IOUT, 93000 ) LINOUT
         WRITE( IOUT, 93000 )
         WRITE( IOUT, 93000 )
      ELSE 
         WRITE( IOUT, 93100 )
         LINOUT = CEES
         WRITE( IOUT, 93000 ) LINOUT
         LFIRST = .TRUE.
         DO NIPR = 1, N_IPR_SPC
            IF ( IPR_SPTYPE( NIPR ) .EQ. 'FAM' ) THEN
               IF ( LFIRST ) THEN
                  WRITE( IOUT, 93120 )
                  LFIRST = .FALSE.
               END IF 
               INDX = IPR_SPNUM( NIPR )
               DO N = 1, FAMNUMLNS( INDX )
                  WRITE( IOUT, 93000 ) FAMLINE( INDX,N )
               END DO
               WRITE( IOUT, 93140 ) 
               WRITE( IOUT, 93000 ) LINOUT
            END IF
         END DO
         ICOUNT = 0
         DO NIPR = 1, N_IPR_SPC  
            DO N = 1, N_IPR_OPS( NIPR )
               ICOUNT = ICOUNT + 1
               WRITE( IOUT, '(A17,I3,A4,A4,A4,A)' ) '      IPR_OUTPUT(', 
     &                ICOUNT, ') = ', IPR_OPNAME( NIPR, N ),
     &                ' of ', IPR_SPNAM( NIPR ) 
            END DO
         END DO
         WRITE( IOUT, 93000 ) 
         WRITE( IOUT, 93000 ) 
      END IF              

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  If not doing a partial IRR,  write a header and skip to the end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. LPARTIRR ) THEN
         LINOUT = CEES
         WRITE( IOUT, 93000 ) LINOUT
         IF ( LFULLIRR ) THEN 
            WRITE( IOUT, 93180 ) 
         ELSE
            WRITE( IOUT, 93200 )
         END IF
         LINOUT = CEES
         WRITE( IOUT, 93000 ) LINOUT
         GO TO 999
      END IF 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the section header for the partial IRR section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LINOUT = CEES
      WRITE( IOUT, 93000 ) LINOUT
      WRITE( IOUT, 93220 )
      LINOUT = CEES
      WRITE( IOUT, 93000 ) LINOUT

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Find the longest reaction label length, right justify and save
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MAXLEN = 0
      DO NRX = 1, NRXNS
         MAXLEN = MAX( MAXLEN, LEN_TRIM( RXLABEL( NRX ) ) )
      END DO
       
      DO NRX = 1, NRXNS
         DO N = 1, LABLEN
            RJLABL( NRX )( N:N ) = ' '
         END DO
         IS1 = LBLANK( RXLABEL( NRX ) ) + 1
         IE1 = LEN_TRIM( RXLABEL( NRX ) )
         LEN1 = IE1 - IS1 + 1
         IE2 = LABLEN
         IS2 = LABLEN - LEN1 + 1
         RJLABL( NRX )( IS2:IE2 ) = RXLABEL( NRX )( IS1:IE1 )       
      END DO

      SPOS = LABLEN - MAXLEN + 1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Top of loop for doing each IRR_OUTPUT report
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 540 NOUT = 1, NIRROUT
         WRITE( OUTLBL, '( I3 )' ) NOUT

c..Write the header for each output
         WRITE( IOUT, 93240 )
         LINOUT = CEES
         WRITE( IOUT, 93000 ) LINOUT
         STRING( 1 ) = '   Summary of Input IRR Commands for OUTPUT'
         STRING( 2 ) = INT2CHR( NOUT )
         IEND = LEN_TRIM( STRING( 2 ) )
         STRING( 2 ) = STRING( 2 )( 1:IEND ) // ':'
         STRING( 3 ) = IRRNAME( NOUT )
         NUMSTRNGS = 3         
         CALL CATSTRING( NUMSTRNGS, STRING, LINOUT, LINLEN )
         WRITE( IOUT, 93000 ) LINOUT
         LINOUT = CEES
         WRITE( IOUT, 93000 ) LINOUT

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the input IRR commands
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         WRITE( IOUT, 93140 )
         WRITE( IOUT, 93260 )
         WRITE( IOUT, 93140 )
         LINOUT = '       IRROUT(' // OUTLBL // ') = '
         IEND = LEN_TRIM( LINOUT ) + 1
         EQPOS1 = IEND
         DO 100 NTERM = 1, NIRRTERMS( NOUT )

            CALL DOCOEFF( OUTSC( NOUT,NTERM ), COUT )
            IF ( NTERM .EQ. 1 .AND. OUTSC( NOUT,NTERM ) .GT. 0.0 )
     &            COUT( 1:1 ) = ' '
            LINOUT = LINOUT( 1:EQPOS1 ) // COUT
            IEND = EQPOS1 + 12
            TYPE = OUTTYPE( NOUT,NTERM )
            E1 = MIN( LEN_TRIM( OUTSPEC1( NOUT,NTERM ) ), 6 )
            E2 = MIN( LEN_TRIM( OUTSPEC2( NOUT,NTERM ) ), 6 )
            E3 = MIN( LEN_TRIM( OUTSPEC3( NOUT,NTERM ) ), 6 )

            IF ( TYPE .EQ. 'NAME' ) THEN

               LINOUT = LINOUT( 1:IEND ) // OUTSPEC1( NOUT, NTERM )
               IEND = LEN_TRIM( LINOUT )
               IF ( OUTPNFLAG( NOUT,NTERM ) .EQ. 'POSONLY' ) THEN
                  LINOUT = LINOUT( 1:IEND ) // '[POSONLY]'
               ELSE IF ( OUTPNFLAG( NOUT,NTERM ) .EQ. 'NEGONLY' ) THEN
                  LINOUT = LINOUT( 1:IEND ) // '[NEGONLY]'
               END IF

            ELSE IF ( TYPE .EQ. 'PROD' .OR. TYPE .EQ. 'NETP' ) THEN

               OPNAM = TYPE
               LINOUT = LINOUT( 1:IEND ) // OPNAM // ' [' //
     &                  OUTSPEC1( NOUT,NTERM )( 1:E1 ) // ']'
               IEND   = LEN_TRIM( LINOUT )
               IF ( OUTSPEC2( NOUT,NTERM ) .NE. '' ) THEN
                  LINOUT = LINOUT( 1:IEND ) // ' FROM [' //
     &                     OUTSPEC2( NOUT,NTERM )( 1:E2 ) // ']'
                  IEND   = LEN_TRIM( LINOUT )
               END IF
               IF ( OUTSPEC3( NOUT,NTERM ) .NE. '' ) THEN
                  IF ( OUTOP2( NOUT,NTERM ) .EQ. 'AND' ) THEN
                     LINOUT = LINOUT( 1:IEND ) // ' AND [' //
     &                        OUTSPEC3( NOUT,NTERM )( 1:E3 ) // ']'
                  ELSE
                     LINOUT = LINOUT( 1:IEND ) // ' OR [' //
     &                        OUTSPEC3( NOUT,NTERM )( 1:E3 ) // ']'
                  END IF
                  IEND = LEN_TRIM( LINOUT )
               END IF

            ELSE IF ( TYPE .EQ. 'LOSS' .OR. TYPE .EQ. 'NETL' ) THEN

               OPNAM = TYPE
               LINOUT = LINOUT( 1:IEND ) // OPNAM // ' [' //
     &                  OUTSPEC1( NOUT,NTERM )( 1:E1 ) // ']'
               IEND = LEN_TRIM( LINOUT )
               IF ( OUTSPEC3( NOUT,NTERM ) .NE. '' ) THEN
                  IF ( OUTOP2( NOUT,NTERM ) .EQ. 'AND' ) THEN
                     LINOUT = LINOUT( 1:IEND ) // ' AND [' //
     &                        OUTSPEC3( NOUT,NTERM )( 1:E3 ) // ']'
                  ELSE
                     LINOUT = LINOUT( 1:IEND ) // ' OR [' //
     &                        OUTSPEC3( NOUT,NTERM )( 1:E3 ) // ']'
                  END IF
                  IEND = LEN_TRIM( LINOUT )
               END IF

            ELSE IF ( TYPE .EQ. 'NET' ) THEN

               OPNAM = TYPE
               LINOUT = LINOUT( 1:IEND ) // OPNAM // ' [' //
     &                  OUTSPEC1( NOUT,NTERM )( 1:E1 ) // ']'
               IEND = LEN_TRIM( LINOUT )

            ELSE IF ( TYPE .EQ. 'RXN' ) THEN

               INDX = OUTRN( NOUT,NTERM )
               LINOUT = LINOUT( 1:IEND ) // 'IRR<' //
     &                  RJLABL( INDX )( SPOS:LABLEN ) // '>'
            END IF

            WRITE( IOUT, 93000 ) LINOUT
            LINOUT = '      &'
            LINOUT = LINOUT( 1:EQPOS1 )

100      CONTINUE
         WRITE( IOUT, 93140 )
         LINOUT = CEES
         WRITE( IOUT, 93000 ) LINOUT   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the descriptions for any families referenced
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( NOUTFAMS( NOUT ) .GT. 0 ) THEN
            WRITE( IOUT, 93140 )
            WRITE( IOUT, 93280 )
            DO NFAM = 1, NOUTFAMS( NOUT )
               INDX = OUTFAMS( NOUT,NFAM )
               DO N = 1, FAMNUMLNS( INDX )
                  WRITE( IOUT, 93000 ) FAMLINE( INDX,N )
               END DO
               WRITE( IOUT, 93140 ) 
            END DO
            LINOUT = CEES
            WRITE( IOUT, 93000 ) LINOUT
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Output descriptions of all cycles referenced
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( NOUTCYCS( NOUT ) .GT. 0 ) THEN
            WRITE( IOUT, 93140 )
            WRITE( IOUT, 93300 )
            DO 200 N = 1, NOUTCYCS( NOUT )
               NCYC = OUTCYCS( NOUT,N )

c..Write the cycle name
               STRING( 1 ) = 'c..CYCLE'
               STRING( 2 ) = INT2CHR( NCYC )
               IEND = LEN_TRIM( STRING( 2 ) )
               STRING( 2 ) = STRING( 2 )( 1:IEND )
               STRING( 3 ) = CYCLNAME( NCYC )
               NUMSTRNGS = 3         
               CALL CATSTRING( NUMSTRNGS, STRING, LINOUT, LINLEN )
               WRITE( IOUT, 93000 ) LINOUT

c..Write reactions used in the cycle
               WRITE( LABL, '( I3 )' ) NCYC
               LINOUT = '      CYSM(' // LABL // ') ='
               IEND = LEN_TRIM( STRING( 2 ) )

               IF ( NCYTERMS( NCYC ) .EQ. 0 ) THEN
                  LINOUT = LINOUT( 1:IEND ) // ' 0.0'
                  WRITE( IOUT, 93000 ) LINOUT
               ELSE
                  WRITE( IOUT, 93000 ) LINOUT
                  DO NTERM = 1, NCYTERMS( NCYC )
                     INDX = CYRXNUM( NCYC,NTERM )
                     CALL DOCOEFF( CYSC( NCYC,NTERM ), COUT )
                     LINOUT = MECLINE( INDX,1 )
                     LINOUT( 9:19 ) = COUT
                     WRITE( IOUT, 93000 ) LINOUT
                     IF ( MECNUMLNS( INDX ) .GT. 1 ) THEN
                        DO NLN = 2, MECNUMLNS( INDX )
                           WRITE( IOUT, 93000 ) MECLINE( INDX,NLN )
                        END DO
                     END IF
                  END DO
                  WRITE( IOUT, 93140 )
               END IF

200         CONTINUE
            LINOUT = CEES
            WRITE( IOUT, 93000 ) LINOUT
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Output descriptions of all reaction sums referenced
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( NOUTRXSUMS( NOUT ) .GT. 0 ) THEN
            WRITE( IOUT, 93140 )
            WRITE( IOUT, 93320 )
            DO 300 N = 1, NOUTRXSUMS( NOUT )
               NRX = OUTRXSUMS( NOUT,N )

c..Write the reaction sum name
               STRING( 1 ) = 'c..RXSUM'
               STRING( 2 ) = INT2CHR( NRX )
               IEND = LEN_TRIM( STRING( 2 ) )
               STRING( 2 ) = STRING( 2 )( 1:IEND )
               STRING( 3 ) = RXSUMNAME( NRX )
               NUMSTRNGS = 3         
               CALL CATSTRING( NUMSTRNGS, STRING, LINOUT, LINLEN )
               WRITE( IOUT, 93000 ) LINOUT

c..Write reactions used in the reaction sum
               WRITE( LABL, '( I3 )' ) NRX
               LINOUT = '      RXSM(' // LABL // ') ='
               WRITE( IOUT, 93000 ) LINOUT
               IEND = LEN_TRIM( STRING( 2 ) )

               IF ( NRXTERMS( NRX ) .EQ. 0 ) THEN
                  LINOUT = LINOUT( 1:IEND ) // ' 0.0'
                  WRITE( IOUT, LINOUT )
               ELSE
                  DO NTERM = 1, NRXTERMS( NRX )
                     INDX = RXSUMRN( NRX,NTERM )
                     CALL DOCOEFF( RXNSC( NRX,NTERM ), COUT )
                     LINOUT = MECLINE( INDX,1 )
                     LINOUT( 9:19 ) = COUT
                     WRITE( IOUT, 93000 ) LINOUT
                     IF ( MECNUMLNS( INDX ) .GT. 1 ) THEN
                        DO NLN = 2, MECNUMLNS( INDX )
                           WRITE( IOUT, 93000 ) MECLINE( INDX,NLN )
                        END DO
                     END IF
                  END DO
               END IF 
               WRITE( IOUT, 93140 )
300         CONTINUE
            LINOUT = CEES
            WRITE( IOUT, 93000 ) LINOUT
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Before doing operators, flag all reactions included in cycles for
c  elimination from production/loss/net operators 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NRXNS
            LRXINC( NRX ) = .TRUE.
         END DO

         IF ( NOUTCYCS( NOUT ) .GT. 0 ) THEN
            DO N = 1, NOUTCYCS( NOUT )
               NCYC = OUTCYCS( NOUT, N )
               DO NTERM = 1, NCYTERMS( NCYC )
                  INDX = CYRXNUM( NCYC, NTERM )
                  LRXINC( INDX ) = .FALSE.
               END DO
            END DO
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Output descriptions of production/loss/net operator
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( NOUTOPS( NOUT ) .GT. 0 ) THEN
            WRITE( IOUT, 93140 )
            WRITE( IOUT, 93340 )
            NPROD = 0
            NNPRD = 0
            NLOSS = 0
            NNLOS = 0
            NNETR = 0
            DO 400 NTERM = 1, NIRRTERMS( NOUT )

               TYPE = OUTTYPE( NOUT,NTERM ) 

               IF ( TYPE .NE. 'NAME' .AND. TYPE .NE. 'RXN' ) THEN             

                  CALL GETRXNS( NOUT, NTERM, NR, RXNUMS, COEFFS, LRXINC )

c..Write the operator description
                  IF ( TYPE .EQ. 'PROD' ) THEN
                     STRING( 1 ) = 'c..Production of'
                     OPNAM = 'PROD'
                     NPROD = NPROD + 1
                     WRITE( LABL, '( I3 )' ) NPROD
                  ELSE IF ( TYPE .EQ. 'NETP' ) THEN
                     STRING( 1 ) = 'c..Net production of'
                     OPNAM = 'NPRD'
                     NNPRD = NNPRD + 1
                     WRITE( LABL, '( I3 )' ) NNPRD
                  ELSE IF ( TYPE .EQ. 'LOSS' ) THEN
                     STRING( 1 ) = 'c..Loss of'
                     OPNAM = 'LOSS'
                     NLOSS = NLOSS + 1
                     WRITE( LABL, '( I3 )' ) NLOSS
                  ELSE IF ( TYPE .EQ. 'NETL' ) THEN
                     STRING( 1 ) = 'c..Net loss of'
                     OPNAM = 'NLOS'
                     NNLOS = NNLOS + 1
                     WRITE( LABL, '( I3 )' ) NNLOS
                  ELSE IF ( TYPE .EQ. 'NET' ) THEN
                     STRING( 1 ) = 'c..Net reaction of'
                     OPNAM = 'NETR'
                     NNETR = NNETR + 1
                     WRITE( LABL, '( I3 )' ) NNETR
                  END IF

                  SAVOP( NTERM ) = OPNAM
                  SAVLBL( NTERM ) = LABL

                  STRING( 2 ) = OUTSPEC1( NOUT,NTERM )
                  NUMSTRNGS = 2

                  IF ( OUTSPEC2( NOUT,NTERM ) .NE. '' ) THEN
                     STRING( 3 ) = 'from'
                     STRING( 4 ) = OUTSPEC2( NOUT,NTERM )
                     NUMSTRNGS = 4
                  END IF                  

                  IF ( OUTSPEC3( NOUT,NTERM ) .NE. '' ) THEN
                     IF ( OUTOP2( NOUT,NTERM ) .EQ. 'AND' ) THEN
                        STRING( NUMSTRNGS + 1 ) = 'and'
                     ELSE
                        STRING( NUMSTRNGS + 1 ) = 'or'
                     END IF
                     STRING( NUMSTRNGS + 2 ) = OUTSPEC3( NOUT,NTERM )
                     NUMSTRNGS = NUMSTRNGS + 2
                  END IF

                  CALL CATSTRING( NUMSTRNGS, STRING, LINOUT, LINLEN )
                  IEND = LEN_TRIM( LINOUT )
                  WRITE( IOUT, 93000 ) LINOUT

c..Write reactions used in the operator
                           !123456     7890      1     234      567
                  LINOUT = '      ' // OPNAM // '(' // LABL // ') ='
                  IEND = LEN_TRIM( LINOUT )   ! = 17
                  IF ( NR .LE. 0 ) THEN
                     LINOUT = LINOUT( 1:IEND ) // ' 0.0'
                     WRITE( IOUT, 93000 ) LINOUT                  
                  ELSE
                     WRITE( IOUT, 93000 ) LINOUT
                     DO N = 1, NR
                        INDX = RXNUMS( N )
                        CALL DOCOEFF( COEFFS( N ), COUT )
                        LINOUT = MECLINE( INDX,1 )
                        LINOUT( 9:19 ) = COUT
                        WRITE( IOUT, 93000 ) LINOUT
                        IF ( MECNUMLNS( INDX ) .GT. 1 ) THEN
                           DO NLN = 2, MECNUMLNS( INDX )
                              WRITE( IOUT, 93000 ) MECLINE( INDX,NLN )
                           END DO
                        END IF
                     END DO
                  END IF
                  WRITE( IOUT, 93140 )
               END IF
400         CONTINUE 
            LINOUT = CEES
            WRITE( IOUT, 93000 ) LINOUT
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Output list of all reactions referenced in the OUTPUT command
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( NOUTRXN( NOUT ) .GT. 0 ) THEN
            WRITE( IOUT, 93140 )
            WRITE( IOUT, 93360 )
            IF ( NOUTRXN( NOUT ) .GT. 0 ) THEN
               DO NTERM = 1, NIRRTERMS( NOUT )
                  TYPE = OUTTYPE( NOUT,NTERM ) 
                  IF ( TYPE .EQ. 'RXN' ) THEN
                     INDX = OUTRN( NOUT,NTERM )
                     LINOUT = MECLINE( INDX, 1 )
                     LINOUT( 6:6 ) = ' '
                     WRITE( IOUT, 93000 ) LINOUT
                     IF ( MECNUMLNS( INDX ) .GT. 1 ) THEN
                        DO NLN = 2, MECNUMLNS( INDX )
                           WRITE( IOUT, 93000 ) MECLINE( INDX,NLN )
                        END DO
                     END IF
                  END IF
               END DO
            END IF
            WRITE( IOUT, 93140 )
            LINOUT = CEES
            WRITE( IOUT, 93000 ) LINOUT
         END IF           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the pseudocode for IRROUT
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         STRING( 1 ) = '   Pseudocode for OUTPUT'
         STRING( 2 ) = INT2CHR( NOUT )
         IEND = LEN_TRIM( STRING( 2 ) )
         STRING( 2 ) = STRING( 2 )( 1:IEND ) // ':'
         STRING( 3 ) = IRRNAME( NOUT )
         NUMSTRNGS = 3         
         CALL CATSTRING( NUMSTRNGS, STRING, LINOUT, LINLEN )
         WRITE( IOUT, 93000 ) LINOUT
         LINOUT = CEES
         WRITE( IOUT, 93000 ) LINOUT

c..Write the pseudocode for the initialization terms
         LINITFLG = .FALSE.
         LFIRST = .TRUE.
         DO 500  NTERM = 1, NIRRTERMS( NOUT )
            PNFLG = OUTPNFLAG( NOUT,NTERM )

            IF ( PNFLG .NE. '' ) THEN
               LINITFLG = .TRUE.
               TYPE = OUTS1TYP( NOUT,NTERM )
               WRITE( LABL, '( I3 )' ) OUTS1NUM( NOUT,NTERM )
               IF ( LFIRST ) THEN
                  LINOUT = '      IRROUT(' // OUTLBL // ') = 0.0' 
                  WRITE( IOUT, 93000 ) LINOUT
                  WRITE( IOUT, 93000 )
                  LFIRST = .FALSE.
               END IF

               IF ( TYPE .EQ. 'CYCL' ) THEN
                  OPNAM = 'CYSM'
               ELSE
                  OPNAM = 'RXSM'
               END IF

               CALL DOCOEFF( OUTSC( NOUT,NTERM ), COUT )
               IF ( ABS( OUTSC( NOUT,NTERM ) ) .NE. 1.0 ) THEN
                  E1 = 11
               ELSE
                  E1 = 1
               END IF      

               IF ( PNFLG .EQ. 'POSONLY' ) THEN
                  LINOUT = '      IF ( ' // OPNAM // '(' // LABL // 
     &                     ') .GT. 0.0 ) THEN'
               ELSE
                  LINOUT = '      IF ( ' // OPNAM // '(' // LABL // 
     &                     ') .LT. 0.0 ) THEN'
               END IF

               WRITE( IOUT, 93000 ) LINOUT
               IF ( PNFLG .EQ. 'POSONLY' ) THEN
                  LINOUT = '         IRROUT(' // OUTLBL // 
     &                     ') = IRROUT(' // OUTLBL // ') ' //
     &                     COUT( 1:E1 ) // ' ' // OPNAM  //
     &                    '(' // LABL // ')'
               ELSE
                  LINOUT = '         IRROUT(' // OUTLBL //
     &                     ') = IRROUT(' // OUTLBL // ') ' //
     &                     COUT( 1:E1 ) // ' ABS('  // OPNAM //
     &                     '(' // LABL // '))'
               END IF

               WRITE( IOUT, 93000 ) LINOUT
               LINOUT = '      END IF'
               WRITE( IOUT, 93000 ) LINOUT
               WRITE( IOUT, 93000 )
            END IF

500      CONTINUE               

c..Write the final equation for the output integrated reaction rates
         IF ( NIRRTERMS( NOUT ) .EQ. 0 ) THEN
            LINOUT = '       IRROUT(' // OUTLBL // ') = 0.0'
            WRITE( IOUT, 93000 ) LINOUT
         ELSE
            ICOUNT = 0
            DO 520 NTERM = 1, NIRRTERMS( NOUT )
               IF ( NTERM .EQ. 1 ) THEN
                  LINOUT = '      IRROUT(' // OUTLBL // ') = '
                  EQPOS1 = LEN_TRIM( LINOUT ) 
                  IF ( LINITFLG ) THEN
                     LINOUT = LINOUT( 1:EQPOS1 ) // ' IRROUT(' //
     &                        OUTLBL // ')'
                     WRITE( IOUT, 93000 ) LINOUT
                     LINOUT ='     &'
                     LINOUT = LINOUT( 1:EQPOS1 )
                     ICOUNT = ICOUNT + 1
                  END IF 
               ELSE
                  LINOUT ='     &'
                  LINOUT = LINOUT( 1:EQPOS1 )
               END IF

               TYPE = OUTTYPE( NOUT,NTERM )
               IF ( TYPE .EQ. 'NAME' ) TYPE = OUTS1TYP( NOUT,NTERM ) 
               PNFLG = OUTPNFLAG( NOUT,NTERM )

               IF ( PNFLG .EQ. 'POSONLY' .OR. PNFLG .EQ. 'NEGONLY' )
     &             GO TO 520
               IF ( TYPE .EQ. 'CYCL' .AND. PNFLG .EQ. '' ) THEN
                  OPNAM = 'CYSM'
                  WRITE( LABL, '( I3 )' ) OUTS1NUM( NOUT,NTERM )
               ELSE IF ( TYPE .EQ. 'RXSM' .AND. PNFLG .EQ. '' ) THEN
                  OPNAM = 'RXSM'
                  WRITE( LABL, '( I3 )' ) OUTS1NUM( NOUT,NTERM )
               ELSE IF ( TYPE .EQ. 'RXN' ) THEN
                  OPNAM = ' IRR' 
                  INDX = OUTRN( NOUT,NTERM )
               ELSE
                  OPNAM = SAVOP( NTERM )
                  LABL  = SAVLBL( NTERM )
               END IF

               CALL DOCOEFF( OUTSC( NOUT,NTERM ), COUT )
               ICOUNT = ICOUNT + 1
               IF ( OUTSC( NOUT,NTERM ) .GT. 0.0 .AND. ICOUNT .EQ. 1 ) 
     &              COUT( 1:1 ) = ' '
               IF ( TYPE .EQ. 'RXN' ) THEN
                  LINOUT = LINOUT( 1:EQPOS1 ) // COUT // ' ' // 
     &                     OPNAM // '<'  // 
     &                     RJLABL( INDX )( SPOS:LABLEN ) // '>'
               ELSE
                  LINOUT = LINOUT( 1:EQPOS1 ) // COUT // ' ' // 
     &                     OPNAM // '(' // LABL( 1:3 ) // ')'
               END IF
               WRITE( IOUT, 93000 ) LINOUT
520         CONTINUE
         END IF
540   CONTINUE

999   CONTINUE
    
      WRITE( IOUT, 93380 ) 
      CLOSE( IOUT )
      RETURN
      
C----------------------- Format Statements -----------------------------
93000 FORMAT( A )

93020 FORMAT( //'============ Process Analysis Control Module Output', 
     &        ' Report ============'/18X, 'Created  ', A/ )

!3040 FORMAT( ' ', 24X, 'OUTPUT DOMAIN SECTION'/
!    &        '   Process Monitoring and IRR outputs will be generated', 
!    &        ' for the'/
!    &        '   following cells relative to the full modeling domain.' )
!3060 FORMAT( '       Starting column:', I4/
!    &        '       Ending column:  ', I4/
!    &        '       Starting row:   ', I4/
!    &        '       Ending row:     ', I4/ 
!    &        '       Starting layer: ', I4/
!    &        '       Ending layer:   ', I4 ) 

93080 FORMAT( 20X, 'Integrated Process Rate Section'/
     &        '   No requests for IPR outputs were included', 
     &        ' in the command file.' )

93100 FORMAT( 20X, 'Integrated Process Rate Section'/
     &        '   The following section lists the IPR ', 
     &        'outputs generated from the'/
     &        '   command file inputs.' )

93120 FORMAT( ' '/
     &        '   The referenced families are defined as follows:'/
     &        ' ' )
93140 FORMAT( ' ' )
93180 FORMAT( 20X, 'Integrated Reaction Rate Section'/
     &        '   A FULL IRR analysis has been requested. Each reaction', 
     &          ' rate will be'/
     &        '   integrated individually and output for the',
     &          ' specified domain.' )
93200 FORMAT( 20X, 'Integrated Reaction Rate Section'/
     &        '   No requests for IRR analysis were included in the', 
     &        ' command file.' )
93220 FORMAT( 20X, 'Integrated Reaction Rate Section'/
     &        '   The following section gives a symbolic representation', 
     &        ' of how the'/
     &        '   PARTIAL IRR outputs are calculated given the commands', 
     &        ' that were'/
     &        '   included in the command file. The report includes', 
     &        ' pseudocode showing'/
     &        '   how individual integrated reactions rates are', 
     &        ' combined to produce the '/
     &        '   requested output.  In the pseudocode that follows'/
     &        ' '/
     &        '         IRR< x > = the integrated reaction rate for '/
     &        '                    the reaction with label x as '/
     &        '                    defined in the chemical mechanism'/
     &        ' '/
     &        '         IRROUT( n ) = the nth integrated reaction rate'/
     &        '                       output that was requested'/
     &        ' ' )  
93240 FORMAT( //1X )
93260 FORMAT( '   The IRR_OUTPUT command was specified as follows:' )
93280 FORMAT( '   The referenced families are defined as follows:'/' ' )
93300 FORMAT( '   The referenced cycles are defined as follows:'/' ' )
93320 FORMAT( '   The referenced reaction sums are defined as follows:'/
     &        ' ' )
93340 FORMAT( '   The referenced operators are defined as follows:'/' ' )
93360 FORMAT( '   The following reactions are referenced individually:'/
     &        ' ' )
93380 FORMAT( ///'========= End of Process Analysis Control Module', 
     &         ' Output Report ========='  )  
C-----------------------------------------------------------------------

      CONTAINS

         SUBROUTINE DOCOEFF( COEF, COUT )

C-----------------------------------------------------------------------
C Function: Transform a coefficient from numerical to character form for
C           output reports   
C-----------------------------------------------------------------------

         IMPLICIT NONE
      
C Arguments:
         REAL    COEF              ! Coefficient for output term 
         CHARACTER( * ) :: COUT    ! Output coefficient
                                        
C Local Variables
         INTEGER N   ! Loop index
         
C-----------------------------------------------------------------------

         DO N = 1, LEN( COUT )
            COUT( N:N ) = ' '
         END DO

         IF ( ABS( COEF ) .NE. 1.0 ) THEN
            WRITE( COUT, '( 2X, F7.4 )' ) ABS( COEF )
            COUT( 11:11 ) = '*'
         END IF   

         IF ( COEF .GE. 0.0 ) THEN
            COUT( 1:1 ) = '+'
         ELSE
            COUT( 1:1 ) = '-'
         END IF

         RETURN 

         END SUBROUTINE DOCOEFF

         FUNCTION INT2CHR( NOUT ) RESULT( INTSTR )

C-----------------------------------------------------------------------
C Function: Convert an integer number into character representation
C           that is left justified in a string 
C-----------------------------------------------------------------------
         USE UTILIO_DEFN

         IMPLICIT NONE
      
C Arguments:
         INTEGER NOUT               ! Integer number to convert
         CHARACTER( 10 ) :: INTSTR  ! string holder

C Local Variables:
         INTEGER EPOS1         ! Ending positions
         INTEGER SPOS1         ! Starting positions
         INTEGER LEN1          ! Length of string
         INTEGER N             ! Loop index
C-----------------------------------------------------------------------

         WRITE( INTSTR, '( I10 )' ) NOUT
         DO N = 1, 10
            INTSTR( N:N ) = ' '
         END DO

c..Left justify the numeric string
         SPOS1 = LBLANK( INTSTR ) + 1
         EPOS1 = LEN_TRIM( INTSTR )
         LEN1 = EPOS1 - SPOS1 + 1
         INTSTR( 1:LEN1 ) = INTSTR( SPOS1:EPOS1 )

         RETURN
         END FUNCTION INT2CHR

      END SUBROUTINE PA_REPORT
