
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/grsprse.F,v 1.3 2011/10/21 16:11:14 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)grsprse.F 1.1 /project/mod3/CMAQ/src/chem/smvgear/SCCS/s.grsprse.F 07 Jul 1997 12:45:30

      SUBROUTINE JSPARSE
 
C***********************************************************************      
C
C  FUNCTION: To define array pointers for sparse matrix storage by
C            doing symbolic LU decomposition
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995. 
C                      Based on  the code originally developed by 
C                      M. Jacobson, (Atm. Env., Vol 28, No 2, 1994).
C                    Revised 3/24/96 By Jerry Gipson to conform to 
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
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    30 Jun 10 J.Young: convert for Namelist redesign; move all
C                    local include file variables into GRVARS module
C                    29 Mar 11 S.Roselle: Replaced I/O API include files 
C                    with UTILIO_DEFN
C                    15 Jul 14 B.Hutzell: replaced mechanism include files with 
C                    RXNS_DATA module and supplement error message when array
C                    bounds exceed maximum values
C***********************************************************************

      USE RXNS_DATA
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE GRVARS              ! inherits GRID_CONF

      IMPLICIT NONE
      
C..INCLUDES: None
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: 
      LOGICAL, SAVE :: INITIALIZED = .FALSE. ! Flag for first call to this subroutine

      INTEGER, SAVE :: IFNEVER = 0  ! Flag for counter initialization
      INTEGER, SAVE :: NDLMAX  = 0  ! Max # of PD loss terms in any reaction
      INTEGER, SAVE :: NDPMAX  = 0  ! Max # of PD prod terms in any reaction

C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 16 ) :: PNAME = 'GRSPRSE'     ! Program name
      CHARACTER( 80 ) :: MSG       ! Mesaage text for output to log

      INTEGER EXITSTAT       ! Exit status code
      INTEGER I,J,K,I1,J1,I2 ! Matrix loop indices
      INTEGER IA, IB         ! I,J index holders for decomp loop 2
      INTEGER INEW, JNEW     ! Index for sorted species number
      INTEGER IOLD, JOLD     ! Index for old species number
      INTEGER IPA, KPA       ! I,K index holders for decomp loop 1
      INTEGER IPB, KPB       ! I,K index holders for decomp loop 1
      INTEGER IPROD, JP      ! Species number of a product
      INTEGER IREACT, IR, JR ! Species number of a reactant
      INTEGER ISP, ISP2, JSP ! Species loop indices
      INTEGER JRE, JPR, IRE  ! Indices for nonzero Jacobian entries 
      INTEGER JZ3, JZ4       ! Counter for calcs in backsub groupings
      INTEGER NP, IAP        ! Product loop indices
      INTEGER NR, IAL, JAL   ! Reactant loop indices
      INTEGER IAR            ! Pointer to location of PD term
      INTEGER IARRAY2        ! Final # of matrix entries w/ Sp. Mat
      INTEGER ICB            ! Counter for # of terms in decomp loop 1
      INTEGER ICBSUM         ! Running count of calcs for j index 
                             ! in decomp loop 1
      INTEGER ICCOUNT        ! Two term op count for decomp loop 1
      INTEGER ICNT           ! Total op counter for decomp loop 1
      INTEGER ICNTA          ! op. counter for decomp loop 1 w/ Sp Mat 
      INTEGER ICNTB          ! op. counter for decomp loop 1 w/ Sp Mat
      INTEGER IFSUN          ! Day/night loop index
      INTEGER IJSTEP         ! Number of terms to calc in decomp loops
      INTEGER IMINNEW        ! Index holder for sort routine
      INTEGER IMINOLD        ! Index holder for sort routine
      INTEGER IPORR          ! Species number of a product or reactant      
      INTEGER, SAVE :: IZERO = 0   ! Integer zero
      INTEGER JCB            ! Counter for # of terms in decomp loop 2
      INTEGER JCCOUNT        ! Two term op count for decomp loop 2
      INTEGER JCNT           ! Total op counter for decomp loop 2 
      INTEGER JCNTA          ! op. counter for decomp loop 2 w/o Sp Mat
      INTEGER JCNTB          ! op. counter for decomp loop 2 w/ Sp Mat
      INTEGER JZ             ! Loop index for backsub loops
      INTEGER KA             ! Loop index for decomposition loops
      INTEGER KCNT           ! op. counter for bksub loop 1 w/ Sp. Mat.
      INTEGER KCNTA          ! op. counter for bksub loop 1 w/o Sp Mat
      INTEGER KNTARRAY       ! Final # of matrix entries w/o Sp. Mat
      INTEGER KOUNT0         ! Initial # of matrix entries w/ Sp. Mat
      INTEGER KOUNT0A        ! Initial # of matrix entries w/o Sp. Mat
      INTEGER KZ             ! # of nonzero calcs in backsub loop 1
      INTEGER NCS12          ! Mechanism number NCS+1=day NCS+2=night
      INTEGER NK             ! Reaction number
      INTEGER NLS            ! Number of loss PD terms
      INTEGER NOCHANG        ! Count of number of species not reacting
      INTEGER NPR            ! Number of prod PD terms
      INTEGER NQQ            ! Loop index for Gear order      
      INTEGER NRPP           ! Reactant plus product loop index
      INTEGER NRX            ! Reaction loop index
      INTEGER NU             ! Active reaction count holder
      INTEGER MCNT           ! op. counter for bksub loop 2 w/ Sp. Mat.
      INTEGER MCNTA          ! op. counter for bksub loop 2 w/o Sp. Mat.
      INTEGER MINVALU        ! Current number of PD terms in sort
      INTEGER MZ             ! # of nonzero calcs in backsub loop 2
!KSPARSE

      INTEGER, ALLOCATABLE :: ICLO( : )    ! Pointer to # of ops in decomp loop 1
      INTEGER, ALLOCATABLE :: JCLO( : )    ! Pointer to # of ops in decomp loop 2
      INTEGER, ALLOCATABLE :: IZEROI( : )  ! Pointer to decomp loop 1 i index
      INTEGER, ALLOCATABLE :: IZEROK( : )  ! Pointer to decomp loop 1 k index
      INTEGER, ALLOCATABLE :: JZERO ( : )  ! Pointer to decomp loop 2 i index
      INTEGER, ALLOCATABLE :: IZILCH  ( :,: )  ! # of nonzero calcs in decomp loop 1
      INTEGER, ALLOCATABLE :: JZILCH  ( :,: )  ! # of nonzero calcs in decomp loop 2
      INTEGER, ALLOCATABLE :: LZERO   ( :,: )  ! Symbolic Jacobian matrix
     
!JSPARSE
      
      INTEGER, ALLOCATABLE :: ISAPORL( : )  ! Count of PD terms for each species
      INTEGER, ALLOCATABLE :: ISPARDER( :,: )  ! Indicator of a PD term in the 

      INTEGER IOS            ! Allocate status

C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize some variables on first call
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( INITIALIZED )THEN
          RETURN
      ELSE
          INITIALIZED = .TRUE.
      END IF 

         

         ALLOCATE( ISAPORL ( NUMB_MECH_SPC ),
     &             ISPARDER( NUMB_MECH_SPC,NUMB_MECH_SPC ), 
     &             STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failed'
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
         END IF
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Initialize Prod/loss and PD tabulator arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NCS12 = NCS

        ISAPORL  = 0
        ISPARDER = 0
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set the number of Partial derivative terms in the Jacobian and
c  count the number of terms for each species
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRX = 1, NRXNS
        DO NR = 1, 3
            IREACT = IRR( NRX, NR )
            IF( IREACT .NE. 0 ) THEN
               DO NRPP = 1, 3 + MXPRD
                  IPORR = IRR( NRX, NRPP )
                  IF( IPORR .NE. 0 ) ISPARDER( IPORR, IREACT ) = 1
               ENDDO
            ENDIF
         ENDDO
      ENDDO

      DO IREACT = 1, NUMB_MECH_SPC
         DO IPORR = 1, NUMB_MECH_SPC
            IF( ISPARDER( IPORR, IREACT ) .EQ. 1 ) 
     &          ISAPORL( IPORR ) = ISAPORL( IPORR ) + 1
         ENDDO
      ENDDO
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Sort the species, putting all with zero partial derivative 
c  terms at the bottom and those with fewest PD terms at top.
c  Set arrays for species with zero PD terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ISCHANG = 0
      NOCHANG = NUMB_MECH_SPC
      DO JOLD = 1, NUMB_MECH_SPC
         IF( ISAPORL( JOLD ) .GT. 0 ) THEN
            ISCHANG( NCS ) = ISCHANG( NCS ) + 1
            JNEW = ISCHANG( NCS )
            INEW2OLD( JNEW, NCS ) = JOLD
            IOLD2NEW( JOLD, NCS ) = JNEW
         ELSE
            INEW2OLD( NOCHANG, NCS ) = JOLD
            IOLD2NEW( JOLD, NCS ) = NOCHANG
            NOCHANG = NOCHANG - 1
         ENDIF
      ENDDO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Now sort by number of PD terms, fewest at position 1, most at
c  the end position. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO JNEW = 1, ISCHANG( NCS )
c  uncomment the following three lines to turn off ordering
!         INEW2OLD( JNEW, NCS ) = JNEW
!         IOLD2NEW( JNEW, NCS ) = JNEW
!         IF( JNEW .NE. 0 ) GO TO 180
c  uncomment the above three lines to turn off ordering
         JOLD = INEW2OLD( JNEW, NCS )
         MINVALU = ISAPORL( JOLD )
         IMINOLD = JOLD
         IMINNEW = JNEW

         DO INEW = JNEW + 1, ISCHANG( NCS )
            IOLD = INEW2OLD( INEW, NCS )
            IF( ISAPORL( IOLD ) .LT. MINVALU ) THEN
               MINVALU = ISAPORL( IOLD )
               IMINOLD = IOLD
               IMINNEW = INEW
            ENDIF
         ENDDO

         INEW2OLD( IMINNEW, NCS ) = JOLD
         INEW2OLD( JNEW, NCS )    = IMINOLD
         IOLD2NEW( JOLD, NCS )    = IMINNEW
         IOLD2NEW( IMINOLD, NCS ) = JNEW
      ENDDO
               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Fill the irm2 array using the new species order developed above.
c  Also determine active reactions for day and then night (i.e., photo
c  reactions determined by BTEST=.TRUE. are not included for nighttime)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NUSERAT = 0
      DO NRX = 1, NRXNS
         DO NR = 1, NREACT( NRX )
            IREACT = IRR( NRX, NR )
            IRM2( NR, NRX, NCS ) = IOLD2NEW( IREACT,NCS ) 
         ENDDO

         DO NP = 1, NPRDCT( NRX )
            IPROD = IRR( NRX, NP + 3 )
            IRM2( NP+3, NRX, NCS ) = IOLD2NEW( IPROD, NCS )
         ENDDO
         
         IF( NREACT( NRX ) .GT. 0 ) THEN
            NUSERAT( NCS ) = NUSERAT( NCS ) + 1
            NU = NUSERAT( NCS )
            NKUSERAT( NU, NCS ) = NRX
            IF( .NOT. ( BTEST ( IRXBITS( NRX ),1 ) ) ) THEN
               NUSERAT( NCS + 1 ) = NUSERAT( NCS + 1 ) + 1
               NU = NUSERAT( NCS + 1 )
               NKUSERAT( NU, NCS + 1 ) = NRX
            ENDIF
         ENDIF
      ENDDO

      DEALLOCATE( ISAPORL, ISPARDER )         

      ALLOCATE( ICLO( NCS2 ),
     &          JCLO( NCS2 ),
     &          IZEROI( MXCOUNT1 ),
     &          IZEROK( MXCOUNT2 ),
     &          JZERO ( MXCOUNT1 ), STAT = IOS )
      IF ( IOS .NE. 0 ) THEN
         MSG = '*** Memory allocation failed'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( LZERO   ( NUMB_MECH_SPC,NUMB_MECH_SPC ),
     &          IZILCH  ( NUMB_MECH_SPC,NCS2 ),
     &          JZILCH  ( NUMB_MECH_SPC,NCS2 ), STAT = IOS )
      IF ( IOS .NE. 0 ) THEN
         MSG = '*** Memory allocation failed'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      
      IZILCH  = 0
      JZILCH  = 0
      JHIZ1   = 0
      JHIZ2   = 0
      KZILCH  = 0
      MZILCH  = 0

      NDERIVL = 0
      NDERIVP = 0
      
      
      JARRAYPT = 0

      IJDECA = 0
      IKDECA = 0
      KJDECA = 0

      IJDECB = 0
      IKDECB = 0
      KJDECB = 0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do symbolic LU decomposition to determine sparse storage array
c  structures. Done twice, first for day and then for night. An entry
c  of 1 in lzero means a non-negative entry in the Jacobian. First
c  put ones on the diagonal and zeroes everywhere else.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 700 IFSUN = 1, 2
         NCS12 = IFSUN
         DO I = 1, NUMB_MECH_SPC
            DO J = 1, NUMB_MECH_SPC
               LZERO( J, I ) = 0
            ENDDO
            LZERO( I, I ) = 1
         ENDDO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill in the rest of the entries in the Jacobian
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NUSERAT( NCS12 )
            NK = NKUSERAT( NRX, NCS12 )
            DO NR = 1, NREACT( NK )
               IRE = IRM2( NR, NK, NCS )
               DO JAL = 1, NREACT( NK )
                  JRE = IRM2( JAL, NK, NCS )
                  LZERO( JRE, IRE ) = 1
               ENDDO
               DO IAP = 1, NPRDCT( NK )
                  JPR = IRM2( 3+IAP, NK, NCS )
                  LZERO( JPR, IRE ) = 1 
               ENDDO
           ENDDO
         ENDDO
   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set up arrays for decomposition / back-substitution of sparse     
c  matrices by removing all calculations involving a zero.          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( IFNEVER.EQ.0 ) THEN
            IFNEVER = 1
            ICNT    = 0 
            JCNT    = 0 
            ICCOUNT = 0
            JCCOUNT = 0
         ENDIF
         KOUNT0A = 0
         KOUNT0  = 0
         ICNTA   = 0
         ICNTB   = 0
         JCNTA   = 0
         JCNTB   = 0
         KCNTA   = 0
         MCNTA   = 0
         KCNT    = 0
         MCNT    = 0
         IARRAY2 = 0
         
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Count number of entries w/ and w/o sparse matrix storage
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc         
         DO J = 1, ISCHANG( NCS )
            DO K = 1, ISCHANG( NCS )
               KOUNT0A = KOUNT0A + 1
               IF( LZERO( J, K ) .EQ. 1 ) KOUNT0 = KOUNT0 + 1
            ENDDO
         ENDDO
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the symbolic decomposition (ludcmp) converting [A] to [L][U] 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ICLO( NCS12 ) = ICNT + 1
         JCLO( NCS12 ) = JCNT + 1
         DO J = 1, ISCHANG( NCS )
            J1 = J - 1
            
c...  First loop of decomposition
            DO I = 2, ISCHANG( NCS ) 
               I1 = J1 
               IF( I .LE. J1 ) I1 = I - 1
               DO K = 1, I1
                  ICNTA = ICNTA + 1
                  IF( LZERO( I, K ) .EQ. 1 .AND. LZERO( K, J ) .EQ. 1 )
     &                  THEN
                     IZILCH( J, NCS12 ) = IZILCH( J, NCS12 ) + 1
                     ICNT               = ICNT + 1
                     ICNTB              = ICNTB + 1
                     IZEROK( ICNT )     = K   
                     IZEROI( ICNT )     = I
                     LZERO( I, J )      = 1 
                  ENDIF
               ENDDO
            ENDDO
c... Second loop of decomposition 
            DO I = J + 1, ISCHANG( NCS ) 
               JCNTA = JCNTA + 1
               IF( LZERO( I, J ) .EQ. 1 ) THEN
                  JZILCH( J, NCS12 ) = JZILCH( J, NCS12 ) + 1
                  JCNT               = JCNT  + 1
                  JCNTB              = JCNTB + 1
                  JZERO( JCNT )      = I  
               ENDIF
            ENDDO 
         ENDDO
  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do symbolic back-substition for solving [L][U]{x}={b}. Store data
c  in sparse matrix pointer jarraypt.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c... First loop of back-substitution
         DO I = 2, ISCHANG( NCS )
            I1 = I - 1
            DO J = 1, I1    
               KCNTA = KCNTA + 1
               IF( LZERO( I, J ) .EQ. 1 ) THEN 
                  KZILCH( I, NCS12 ) = KZILCH( I, NCS12 ) + 1
                  KCNT = KCNT + 1
                  IARRAY2 = IARRAY2 + 1
                  KZERO( IARRAY2, NCS12 ) = J
                  JARRAYPT( I, J, NCS12 ) = IARRAY2 
               ENDIF
            ENDDO
         ENDDO 

c... Second loop of back-substitution 
         DO I = ISCHANG( NCS ) - 1, 1, -1
            I2 = I + 1
            DO J = I + 1, ISCHANG( NCS )
               MCNTA = MCNTA + 1
               IF( LZERO( I, J ) .EQ. 1 ) THEN 
                  MZILCH( I, NCS12 )      = MZILCH( I, NCS12 ) + 1
                  MCNT                    = MCNT + 1
                  IARRAY2                 = IARRAY2 + 1
                  KZERO( IARRAY2, NCS12 ) = J
                  JARRAYPT( I, J, NCS12 ) = IARRAY2 
               ENDIF
            ENDDO
         ENDDO
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill jarraypt with remaining diagonal array points and save counts
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO I = 1, ISCHANG( NCS ) 
            IARRAY2 = IARRAY2 + 1
            JARRAYPT( I, I, NCS12 ) = IARRAY2 
         ENDDO
         IARRAY( NCS12 ) = IARRAY2 
         KNTARRAY = KCNTA + MCNTA + ISCHANG( NCS )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do decomposition again to change arrays to use jarraypt
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         JCB = JCLO( NCS12 ) 
         JZLO( NCS12 ) = JCCOUNT
         ICBSUM = ICLO( NCS12 ) - 1 
         IJSTEP = 2   
         DO J = 1, ISCHANG( NCS )

c...First loop of decomposition
            IDEC1LO( J, NCS12 ) = ICCOUNT + 1
            ICB = ICBSUM  + 1
            ICBSUM = ICBSUM + IZILCH( J, NCS12 ) 

            DO KA = 1, IZILCH( J, NCS12 ), IJSTEP
               ICCOUNT = ICCOUNT + 1
               IPA = IZEROI( ICB ) 
               KPA = IZEROK( ICB ) 
               IJDECA( ICCOUNT ) = JARRAYPT( IPA,   J, NCS12 ) 
               IKDECA( ICCOUNT ) = JARRAYPT( IPA, KPA, NCS12 )
               KJDECA( ICCOUNT ) = JARRAYPT( KPA,   J, NCS12 )
               IF( ICB + 1 .LE. ICBSUM ) THEN
                  IPB = IZEROI( ICB + 1 ) 
                  KPB = IZEROK( ICB + 1 ) 
                  IJDECB( ICCOUNT ) = JARRAYPT( IPB,   J, NCS12 ) 
                  IKDECB( ICCOUNT ) = JARRAYPT( IPB, KPB, NCS12 )
                  KJDECB( ICCOUNT ) = JARRAYPT( KPB,   J, NCS12 )
               ENDIF
               ICB = ICB + IJSTEP   
            ENDDO

            IDEC1HI( J, NCS12 ) = ICCOUNT  
            
c...Second loop of decomposition
            JZ = JZILCH( J, NCS12 )

            DO I = 1, JZ - 1, 2
               JCCOUNT           = JCCOUNT + 1
               JHIZ1( J, NCS12 ) = JHIZ1( J, NCS12 ) + 1
               IA                = JZERO( JCB )
               IB                = JZERO( JCB + 1 )
               JZEROA( JCCOUNT ) = JARRAYPT( IA, J, NCS12 )
               JZEROB( JCCOUNT ) = JARRAYPT( IB, J, NCS12 )
               JCB = JCB + 2
            ENDDO

            IF( MOD( JZ, 2 ) .EQ. 1 ) THEN 
               JCCOUNT           = JCCOUNT + 1
               JHIZ2( J, NCS12 ) = JHIZ2( J, NCS12 ) + 1
               IA                = JZERO( JCB )
               JZEROA( JCCOUNT ) = JARRAYPT( IA, J, NCS12 )
               JCB               = JCB + 1 
            ENDIF
         ENDDO
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Group terms to increase efficiency in back-substition
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c... First back-substitution loop
         DO I = 1, ISCHANG( NCS ) 
            KZ                = KZILCH( I, NCS12 )
            KZHI0( I, NCS12 ) = KZ - 4 
            JZ3               = 0

            DO JZ = 1, KZHI0( I, NCS12 ), 5     
               JZ3 = JZ + 4
            ENDDO  

            KZLO1( I, NCS12 ) = JZ3 + 1
            KZHI1( I, NCS12 ) = KZ  - 1 
            JZ4 = JZ3 

            DO JZ = JZ3 + 1, KZ - 1, 2    
               JZ4 = JZ + 1
            ENDDO

            KZLO2( I, NCS12 ) = JZ4 + 1
         ENDDO
 
c... Second loop of back-substitution
         DO I = ISCHANG( NCS ), 1, -1
            MZ = MZILCH( I, NCS12 ) 
            MZHI0( I, NCS12 ) = MZ - 4  
            JZ3 = 0 

            DO JZ = 1, MZHI0( I, NCS12 ), 5  
               JZ3 = JZ + 4 
            ENDDO

            MZLO1( I, NCS12 ) = JZ3 + 1
            MZHI1( I, NCS12 ) = MZ  - 1
            JZ4 = JZ3 

            DO JZ = JZ3+1, MZ-1, 2 
               JZ4 = JZ + 1 
            ENDDO

            MZLO2( I, NCS12 ) = JZ4 + 1
         ENDDO
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check dimensions and print out array savings if ldebug on 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF( ICNT.GT.MXCOUNT2 .OR. JCNT .GT. MXCOUNT1 .OR. 
     &         IARRAY2 .GT. MXARRAY .OR. ICCOUNT .GT. MXCOUNT2 .OR.
     &         JCCOUNT .GT. MXARRAY ) THEN
            WRITE( MSG, 94000 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94020 ) MXCOUNT2, ICNT 
            CALL M3MESG( MSG )
            WRITE( MSG, 94040 ) MXCOUNT1, JCNT 
            CALL M3MESG( MSG )
            WRITE( MSG, 94060 ) MXARRAY, IARRAY2 
            CALL M3MESG( MSG )
            WRITE( MSG, 94080 ) MXCOUNT2, ICCOUNT 
            CALL M3MESG( MSG )
            WRITE( MSG, 94100 ) MXARRAY, JCCOUNT, MAXGL3 
            CALL M3MESG( MSG )
            WRITE( MSG,94110 )
            CALL M3MESG( MSG )
            EXITSTAT = 2
            CALL M3EXIT( PNAME, IZERO, IZERO, ' ', EXITSTAT )
         ENDIF           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set final arrays for partial derivative calculations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NUSERAT( NCS12 )
            NK = NKUSERAT( NRX, NCS12 )
            DO IAL = 1, NREACT( NK )
               IR = IRM2( IAL, NK, NCS )

               DO JAL = 1, NREACT( NK )
                  JR = IRM2( JAL, NK, NCS )
                  IAR = JARRAYPT( JR, IR, NCS12 )
                  NDERIVL( NK, NCS12 ) = NDERIVL( NK, NCS12 ) + 1
                  NLS = NDERIVL( NK, NCS12 )
                  JARRL( NLS, NK, NCS12 ) = IAR
                  JLIAL( NLS, NK, NCS12 ) = IAL
                  NDLMAX = MAX( NLS, NDLMAX )
               ENDDO
               
               DO IAP = 1, NPRDCT( NK )
                  JP = IRM2( IAP + 3, NK, NCS )
                  IAR = JARRAYPT( JP, IR, NCS12 )
                  NDERIVP( NK, NCS12 ) = NDERIVP( NK, NCS12 ) + 1
                  NPR = NDERIVP( NK, NCS12 )
                  JARRP(  NPR, NK, NCS12 ) = IAR
                  JPIAL(  NPR, NK, NCS12 ) = IAL
                  ICOEFF( NPR, NK, NCS12 ) = 0
                  IF( ABS( SC( NK, IAP ) - 1.0D0 ) .GT. 1.0D-06 ) THEN
                     ICOEFF( NPR, NK, NCS12 ) = IAP
                  ENDIF
                  NDPMAX = MAX( NPR, NDPMAX )
               ENDDO
            ENDDO     
         ENDDO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check dimensions of PD arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF( NDPMAX .GT. MXRP .OR. NDLMAX .GT. MXRR ) THEN
            WRITE( MSG, 94000 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94200 ) MXRP, NDPMAX 
            CALL M3MESG( MSG )
            WRITE( MSG, 94220 ) MXRR, NDLMAX 
            CALL M3MESG( MSG )
            EXITSTAT = 2
            CALL M3EXIT( PNAME, IZERO, IZERO, ' ', EXITSTAT ) 
         ENDIF
700   CONTINUE


      DEALLOCATE( ICLO,
     &            JCLO,
     &            IZEROI,
     &            IZEROK,
     &            JZERO,
     &            LZERO,       
     &            IZILCH,  
     &            JZILCH ) 
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set some parameters for the Gear integration method 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         MSTEP    = 3
         MAXORD   = 5
         MBETWEEN = 50

         DO NQQ = 1, 7
            ENQQ1(  NQQ )    = 0.5E+00 / FLOAT( NQQ     )
            ENQQ2(  NQQ )    = 0.5E+00 / FLOAT( NQQ + 1 ) 
            ENQQ3(  NQQ )    = 0.5E+00 / FLOAT( NQQ + 2 )
            CONPST( NQQ )    = 1.0E+00 / ( PERTST( NQQ, 1 )
     &                       * ENQQ3( NQQ ) ) 
            CONP15( NQQ )    = 1.5E+00 * CONPST( NQQ )
            PERTST( NQQ, 1 ) = PERTST( NQQ, 1 ) * PERTST( NQQ, 1 )
            PERTST( NQQ, 2 ) = PERTST( NQQ, 2 ) * PERTST( NQQ, 2 )
            PERTST( NQQ, 3 ) = PERTST( NQQ, 3 ) * PERTST( NQQ, 3 )
         ENDDO

         DO I2 = 1, 6 
            ASET( I2, 2 ) = 1.0D+00
            ASET( I2, 8 ) = 0.0D0
         ENDDO
         ASET( 1, 1 ) =   1.0D0
         ASET( 2, 1 ) =   2.0D0 /    3.0D0
         ASET( 2, 3 ) =   1.0D0 /    3.0D0
         ASET( 3, 1 ) =   6.0D0 /   11.0D0
         ASET( 3, 3 ) =   6.0D0 /   11.0D0
         ASET( 3, 4 ) =   1.0D0 /   11.0D0
         ASET( 4, 1 ) =  12.0D0 /   25.0D0
         ASET( 4, 3 ) =   0.70D0
         ASET( 4, 4 ) =   0.20D0
         ASET( 4, 5 ) =   0.02D0
         ASET( 5, 1 ) =  60.0D0 /  137.0D0
         ASET( 5, 3 ) = 225.0D0 /  274.0D0
         ASET( 5, 4 ) =  85.0D0 /  274.0D0
         ASET( 5, 5 ) =  15.0D0 /  274.0D0
         ASET( 5, 6 ) =   1.0D0 /  274.0D0
         ASET( 6, 1 ) = 180.0D0 /  441.0D0
         ASET( 6, 3 ) = 406.0D0 /  441.0D0
         ASET( 6, 4 ) = 735.0D0 / 1764.0D0
         ASET( 6, 5 ) = 175.0D0 / 1764.0D0
         ASET( 6, 6 ) =  21.0D0 / 1764.0D0
         ASET( 6, 7 ) =   1.0D0 / 1764.0D0
      RETURN
      
C********************** FORMAT STATEMENTS ******************************      
93000 FORMAT( 1X,/'PARAM    POSS MATRIX POINTS -- NONZEROS -- NCS12=',I4/
     &        1X, 'INITMAT  ',4X,I8,9X,I8/                               
     &        1X, 'FINMAT   ',4X,I8,9X,I8/   
     &        1X, 'DECOMP1  ',4X,I8,9X,I8/ 
     &        1X, 'DECOMP2  ',4X,I8,9X,I8/ 
     &        1X, 'BACKSB1  ',4X,I8,9X,I8/ 
     &        1X, 'BACKSB2  ',4X,I8,9X,I8/)  
94000 FORMAT( 1X,'One of the dimensions below is too small:')
94020 FORMAT( 1X,'DIMENSION: MXCOUNT2 = ',I6,' VARIABLE: ICNT    = ',I6)  
94040 FORMAT( 1X,'DIMENSION: MXCOUNT1 = ',I6,' VARIABLE: JCNT    = ',I6)  
94060 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: IARRAY2 = ',I6)  
94080 FORMAT( 1X,'DIMENSION: MXCOUNT2 = ',I6,' VARIABLE: ICCOUNT = ',I6)  
94100 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: JCCOUNT = ',I6,' MAXGL3   = ',I6 )
94110 FORMAT( 1X,'NOTE:      MXCOUNT[1,2] = NUMB_MECH_SPC * MAXGL3 * 3' )
94200 FORMAT( 1X,'DIMENSION: MXRP     = ',I6,' VARIABLE: NDPMAX  = ',I6)
94220 FORMAT( 1X,'DIMENSION: MXRR     = ',I6,' VARIABLE: NDLMAX  = ',I6)
      END
