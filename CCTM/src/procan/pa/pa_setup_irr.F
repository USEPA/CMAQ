
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

      SUBROUTINE PA_SETUP_IRR

C-----------------------------------------------------------------------
C Function: To store IRR data that will be needed to
C           generate the PA report and output INCLUDE files
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI`s M3UTILIO
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------
 
      USE UTILIO_DEFN
      USE PA_GLOBAL     ! Mech data used 
      USE PA_DEFN
      USE PA_VARS
      USE PA_PARSE

      IMPLICIT NONE
      
C Includes: None
      
C Arguments: None
                                        
C Parameters: None

C External Functions:
      REAL MOLCLOSS    ! Function to get number of species or 
                       ! family molecules lost in a reaction
      REAL MOLCPROD    ! Function to get number of species or 
                       ! family molecules produced in a reaction

C Local Variables: 

      CHARACTER(  4 ) :: NAMETYPE   ! Type of name (mech or family)
      CHARACTER(  4 ) :: TYPE       ! Operator type
      CHARACTER(  7 ) :: PNFLAG     ! Initialization flag
      CHARACTER( 10 ) :: LBLOUT    ! Holder for output label

      INTEGER EPOS1, EPOS2 ! Position of last non-blank character in string
      INTEGER ICOUNT       ! Count of number of output terms in sum
      INTEGER INDX         ! Pointer to cycle or rxsum number
      INTEGER N            ! Loop index for specis and defined names
      INTEGER NAMINDX      ! Pointer to species or family name
      INTEGER NCYC         ! Loop index for number of cycles
      INTEGER NFAM         ! Loop index for number of familys
      INTEGER NFAMIN       ! Counter of no. of families in output
      INTEGER NFAMOUT      ! Counter of no. of families listed for output
      INTEGER NOUT         ! Loop index for number of output requests
      INTEGER NRFND        ! Number of reactant species found
      INTEGER NPFND        ! Number of reactant species found
      INTEGER NRX          ! Loop index for number of reactions or Rxsums
      INTEGER NTERM        ! Loop index for number of terms in output request
      INTEGER SPOS1, SPOS2 ! Starting position for first non-blank character
      INTEGER ASTAT        ! Memory allocation staus

      INTEGER FAMINDX( MAXOUTTERMS )   ! Index of family

      REAL SUM           ! Sum of molecular production and loss
         
C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Store reaction data for all cycles  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NCYC = 1, NCYCLES         
         IF ( CYCLSPEC( NCYC ) .NE. '' ) THEN
            NAMETYPE = CYSPTYP( NCYC ) 
            NAMINDX  = CYSPNUM( NCYC ) 
            ICOUNT = 0
            DO NRX = 1, NRXNS
               SUM = MOLCPROD( NAMETYPE, NAMINDX, NRX, NPFND )  - 
     &               MOLCLOSS( NAMETYPE, NAMINDX, NRX, NRFND ) 
               IF ( ABS( SUM ) .GT. 0.0001 )  THEN
                  ICOUNT = ICOUNT + 1
                  CYRXNUM( NCYC,ICOUNT ) = NRX
                  NCYTERMS( NCYC ) = ICOUNT 
                  CYSC( NCYC, ICOUNT ) = SUM
               END IF
            END DO
         END IF
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Store data on the families used in each IRR_OUTPUT command
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NOUT = 1, NIRROUT
         NFAMIN = 0

         DO N = 1, NIRRTERMS( NOUT ) 
            IF ( OUTS1TYP( NOUT,N ) .EQ. 'FAM' )  THEN
               NFAMIN = NFAMIN + 1
               FAMINDX( NFAMIN ) = OUTS1NUM( NOUT,N ) 
            END IF
            IF ( OUTS2TYP( NOUT,N ) .EQ. 'FAM' )  THEN
               NFAMIN = NFAMIN + 1
               FAMINDX( NFAMIN ) = OUTS2NUM( NOUT,N ) 
            END IF
            IF ( OUTS3TYP( NOUT,N ) .EQ. 'FAM' )  THEN
               NFAMIN = NFAMIN + 1
               FAMINDX( NFAMIN ) = OUTS3NUM( NOUT,N ) 
            END IF
         END DO

         IF ( NFAMIN .GT. 0 ) THEN
            NFAMOUT = 0
            DO NFAM = 1, NFAMIN
               INDX = 0
               DO N = 1,  NFAMOUT
                  IF ( FAMINDX( NFAM ) .EQ. OUTFAMS( NOUT,N ) ) INDX = N
               END DO
               IF ( INDX .EQ. 0 ) THEN
                  NFAMOUT = NFAMOUT + 1
                  OUTFAMS( NOUT,NFAMOUT )  = FAMINDX( NFAM ) 
               END IF
            END DO
            NOUTFAMS( NOUT ) = NFAMOUT
         END IF
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Store data on the cycles used in each IRR_OUTPUT command
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NOUT = 1, NIRROUT
         ICOUNT = 0
         DO NTERM = 1, NIRRTERMS( NOUT ) 
            IF ( OUTS1TYP( NOUT, NTERM ) .EQ. 'CYCL' ) THEN
               ICOUNT = ICOUNT + 1
               NOUTCYCS( NOUT ) = ICOUNT
               OUTCYCS( NOUT,ICOUNT ) = OUTS1NUM( NOUT,NTERM )        
            END IF
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Store data on all reaction sums used in each IRR_OUTPUT command
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NOUT = 1, NIRROUT
         ICOUNT = 0
         DO NTERM = 1, NIRRTERMS( NOUT ) 
            IF ( OUTS1TYP( NOUT, NTERM ) .EQ. 'RXSM' ) THEN
               ICOUNT = ICOUNT + 1
               NOUTRXSUMS( NOUT ) = ICOUNT
               OUTRXSUMS( NOUT,ICOUNT ) = OUTS1NUM( NOUT,NTERM ) 
            END IF
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Store data on all operators used in each IRR_OUTPUT command
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NOUT = 1, NIRROUT
         ICOUNT = 0
         DO NTERM = 1, NIRRTERMS( NOUT ) 
            TYPE = OUTTYPE( NOUT,NTERM ) 
            IF ( TYPE .NE. 'NAME' .AND. TYPE .NE. 'RXN' ) THEN
               ICOUNT = ICOUNT + 1
               NOUTOPS( NOUT ) = ICOUNT
            END IF
         END DO
      END DO
           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Store data on individual reactions referenced in each IRR_OUTPUT
c  command
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NOUT = 1, NIRROUT
         ICOUNT = 0
         DO NTERM = 1, NIRRTERMS( NOUT ) 
            TYPE = OUTTYPE( NOUT,NTERM ) 
            IF ( TYPE .EQ. 'RXN' ) THEN
               ICOUNT = ICOUNT + 1
               NOUTRXN( NOUT ) = ICOUNT
            END IF
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Store data on the defined reaction sums that initialize IRR outputs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NUMOUTPOS = 0
      NUMOUTNEG = 0
      NUMOUTIND = 0
      DO NOUT = 1, NIRROUT 
         DO NTERM = 1, NIRRTERMS( NOUT ) 
            TYPE = OUTS1TYP( NOUT,NTERM ) 

            IF ( TYPE .EQ. 'CYCL' .OR. TYPE .EQ. 'RXSM' ) THEN
               INDX = OUTS1NUM( NOUT,NTERM ) 
               IF ( TYPE .EQ. 'RXSM' ) INDX = INDX + NCYCLES              
               PNFLAG = OUTPNFLAG( NOUT,NTERM ) 
               IF ( PNFLAG .EQ. 'POSONLY' )  THEN
                  NUMOUTPOS = NUMOUTPOS + 1
                  TEMPOUTPOS_TMP( NUMOUTPOS ) = INDX
                  INDXOUTPOS_TMP( NUMOUTPOS ) = NOUT
                  COEFOUTPOS_TMP( NUMOUTPOS ) = OUTSC( NOUT,NTERM ) 
               ELSE IF ( PNFLAG .EQ. 'NEGONLY' ) THEN
                  NUMOUTNEG = NUMOUTNEG + 1
                  TEMPOUTNEG_TMP( NUMOUTNEG ) = INDX
                  INDXOUTNEG_TMP( NUMOUTNEG ) = NOUT
                  COEFOUTNEG_TMP( NUMOUTNEG ) = OUTSC( NOUT,NTERM )  
               ELSE IF ( PNFLAG .EQ. '' ) THEN
                  NUMOUTIND = NUMOUTIND + 1
                  TEMPOUTIND_TMP( NUMOUTIND ) = INDX
                  INDXOUTIND_TMP( NUMOUTIND ) = NOUT
                  COEFOUTIND_TMP( NUMOUTIND ) = OUTSC( NOUT,NTERM ) 
               END IF 
            END IF

         END DO
      END DO

      ALLOCATE( TEMPOUTPOS( NUMOUTPOS ),
     &          INDXOUTPOS( NUMOUTPOS ),
     &          COEFOUTPOS( NUMOUTPOS ),
     &          TEMPOUTNEG( NUMOUTNEG ),
     &          INDXOUTNEG( NUMOUTNEG ),
     &          COEFOUTNEG( NUMOUTNEG ),
     &          TEMPOUTIND( NUMOUTIND ),
     &          INDXOUTIND( NUMOUTIND ),
     &          COEFOUTIND( NUMOUTIND ), STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 1 allocating IRR variables'
         CALL M3EXIT ( 'PA_SETUP_IRR', 0, 0, MSG, XSTAT2 )
      END IF

      TEMPOUTPOS = TEMPOUTPOS_TMP( 1:NUMOUTPOS )
      INDXOUTPOS = INDXOUTPOS_TMP( 1:NUMOUTPOS )
      COEFOUTPOS = COEFOUTPOS_TMP( 1:NUMOUTPOS )
      TEMPOUTNEG = TEMPOUTNEG_TMP( 1:NUMOUTNEG )
      INDXOUTNEG = INDXOUTNEG_TMP( 1:NUMOUTNEG )
      COEFOUTNEG = COEFOUTNEG_TMP( 1:NUMOUTNEG )
      TEMPOUTIND = TEMPOUTIND_TMP( 1:NUMOUTIND )
      INDXOUTIND = INDXOUTIND_TMP( 1:NUMOUTIND )
      COEFOUTIND = COEFOUTIND_TMP( 1:NUMOUTIND )

      NUMTEMPS = 0                                   
      MXTEMPTRMS = 0
      DO NCYC = 1, NCYCLES
         NUMTEMPS   = NUMTEMPS + 1
         MXTEMPTRMS = MAX( MXTEMPTRMS, NCYTERMS( NCYC ) ) 
         TEMPTERMS_TMP( NUMTEMPS ) = NCYTERMS( NCYC ) 
         DO N = 1,  NCYTERMS( NCYC ) 
            TEMPRXN_TMP( NUMTEMPS,N )  = CYRXNUM( NCYC,N ) 
            TEMPCOEF_TMP( NUMTEMPS,N ) = CYSC( NCYC,N ) 
         END DO
      END DO

      DO NRX = 1, NRXSUMS
         NUMTEMPS   = NUMTEMPS + 1
         MXTEMPTRMS = MAX( MXTEMPTRMS, NRXTERMS( NRX )  ) 
         TEMPTERMS_TMP( NUMTEMPS ) = NRXTERMS( NRX ) 
         DO N = 1,  NRXTERMS( NRX ) 
            TEMPRXN_TMP( NUMTEMPS,N )  = RXSUMRN( NRX,N ) 
            TEMPCOEF_TMP( NUMTEMPS,N ) = RXNSC( NRX,N ) 
         END DO
      END DO

      ALLOCATE( TEMPTERMS( NUMTEMPS ),
     &          TEMPRXN  ( NUMTEMPS,MXTEMPTRMS ),
     &          TEMPCOEF ( NUMTEMPS,MXTEMPTRMS ), STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 2 allocating IRR variables'
         CALL M3EXIT ( 'PA_SETUP_IRR', 0, 0, MSG, XSTAT2 )
      END IF

      TEMPTERMS = TEMPTERMS_TMP( 1:NUMTEMPS )
      TEMPRXN   = TEMPRXN_TMP  ( 1:NUMTEMPS,1:MXTEMPTRMS )
      TEMPCOEF  = TEMPCOEF_TMP ( 1:NUMTEMPS,1:MXTEMPTRMS )

      IF ( LFULLIRR )  THEN
         DO N = 1,  NRXNS
            WRITE( LBLOUT, '( I10 )' ) N
            SPOS1 = LBLANK(  LBLOUT ) + 1
            EPOS1 = LEN_TRIM( LBLOUT )           
            SPOS2 = LBLANK(  RXLABEL( N ) ) + 1
            EPOS2 = LEN_TRIM( RXLABEL( N )  )           
            IRRNAME( N ) = 'IRR_' // LBLOUT( SPOS1:EPOS1 ) 
            IRRDESC( N ) = 'IRR_' // LBLOUT( SPOS1:EPOS1 )
     &                   // ': ' // '<' //
     &                      RXLABEL( N )( SPOS2:EPOS2 ) // '>'
         END DO
      END IF

      RETURN

      END SUBROUTINE PA_SETUP_IRR
