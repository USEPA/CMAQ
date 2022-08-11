
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
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/rbsparse.F,v 1.4 2011/10/21 16:11:11 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE RBSPARSE( )
 
C***********************************************************************      
 
C  Function: To define array pointers for sparse matrix storage by
C            doing symbolic LU decomposition
 
C  Preconditions: None
 
C  Key Subroutines/Functions Called: None
 
C  Revision History: Prototype created by Jerry Gipson, August, 2004. 
C                      Based on the SMVGEAR code originally developed by 
C                      M. Jacobson, (Atm. Env., Vol 28, No 2, 1994)
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    29 Mar 11 S.Roselle: Replaced I/O API include files
C                               with UTILIO_DEFN
C                    15 Jul 14 B.Hutzell: replaced mechanism include files with 
C                    RXNS_DATA module and supplement error message when array
C                    bounds exceed maximum values
C***********************************************************************

      USE RXNS_DATA
      USE RBDATA                       ! ROS3 solver data
      USE CGRID_SPCS                   ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE
      
C..Includes:
      
C..Arguments:

C..Parameters:
      INTEGER, PARAMETER :: IZERO = 0  ! Integer zero

C..External Functions: None

C..Local Variables: 
      LOGICAL, SAVE :: INITIALIZED = .FALSE. ! Flag for first call to this subroutine

      INTEGER :: IFNEVER = 0     ! Flag for counter initialization
      INTEGER :: NDLMAX  = 0     ! Max # of PD loss terms in any reaction
      INTEGER :: NDPMAX  = 0     ! Max # of PD prod terms in any reaction

      CHARACTER( 16 ), PARAMETER :: PNAME = 'RBSPRSE_A'     ! Procedure name
      CHARACTER( 80 ) :: MSG                     ! Mesaage text for output log

      INTEGER I,J,K,I1,J1,I2       ! Matrix loop indices
      INTEGER IA, IB               ! I,J index holders for decomp loop 2
      INTEGER INEW, JNEW           ! Index for sorted species number
      INTEGER IOLD, JOLD           ! Index for old species number
      INTEGER IPA, KPA             ! I,K index holders for decomp loop 1
      INTEGER IPB, KPB             ! I,K index holders for decomp loop 1
      INTEGER IPROD, JP            ! Species number of a product
      INTEGER IREACT, IR, JR       ! Species number of a reactant
      INTEGER ISP, ISP2            ! Species loop indices
      INTEGER JRE, JPR, IRE        ! Indices for nonzero Jacobian entries 
      INTEGER JZ3, JZ4             ! Counter for calcs in backsub groupings
      INTEGER NP, IAP              ! Product loop indices
      INTEGER NR, IAL, JAL         ! Reactant loop indices
      INTEGER IAR                  ! Pointer to location of PD term
      INTEGER IARRAY2              ! Final # of matrix entries w/ Sp. Mat
      INTEGER ICB                  ! Counter for # of terms in decomp loop 1
      INTEGER ICBSUM               ! Running count of calcs for j index 
                                   ! in decomp loop 1
      INTEGER ICCOUNT              ! Two term op count for decomp loop 1
      INTEGER ICNT                 ! Total op counter for decomp loop 1
      INTEGER ICNTA                ! op. counter for decomp loop 1 w/ Sp Mat 
      INTEGER ICNTB                ! op. counter for decomp loop 1 w/ Sp Mat
      INTEGER IFSUN                ! Day/night loop index
      INTEGER IJSTEP               ! Number of terms to calc in decomp loops
      INTEGER IMINNEW              ! Index holder for sort routine
      INTEGER IMINOLD              ! Index holder for sort routine
      INTEGER IPORR                ! Species number of a product or reactant
      INTEGER JCB                  ! Counter for # of terms in decomp loop 2
      INTEGER JCCOUNT              ! Two term op count for decomp loop 2
      INTEGER JCNT                 ! Total op counter for decomp loop 2 
      INTEGER JCNTA                ! op. counter for decomp loop 2 w/o Sp Mat
      INTEGER JCNTB                ! op. counter for decomp loop 2 w/ Sp Mat
      INTEGER JZ                   ! Loop index for backsub loops
      INTEGER KA                   ! Loop index for decomposition loops
      INTEGER KCNT                 ! op. counter for bksub loop 1 w/ Sp. Mat.
      INTEGER KCNTA                ! op. counter for bksub loop 1 w/o Sp Mat
      INTEGER KNTARRAY             ! Final # of matrix entries w/o Sp. Mat
      INTEGER KOUNT0               ! Initial # of matrix entries w/ Sp. Mat
      INTEGER KOUNT0A              ! Initial # of matrix entries w/o Sp. Mat
      INTEGER KZ                   ! # of nonzero calcs in backsub loop 1
      INTEGER NCSP                 ! Mechanism number NCS+1=day NCS+2=night
      INTEGER NK                   ! Reaction number
      INTEGER NLS                  ! Number of loss PD terms
      INTEGER NOCHANG              ! Count of number of species not reacting
      INTEGER NPR                  ! Number of prod PD terms
      INTEGER NQQ                  ! Loop index for Gear order      
      INTEGER NRPP                 ! Reactant plus product loop index
      INTEGER NRX                  ! Reaction loop index
      INTEGER NU                   ! Active reaction count holder
      INTEGER MCNT                 ! op. counter for bksub loop 2 w/ Sp. Mat.
      INTEGER MCNTA                ! op. counter for bksub loop 2 w/o Sp. Mat.
      INTEGER MINVALU              ! Current number of PD terms in sort
      INTEGER MZ                   ! # of nonzero calcs in backsub loop 2

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
                                                     ! Jacobian matrix
      INTEGER IOS                  ! status

c..The following can be uncommented to print symbolic J-matrix
c      integer iglg
c      character(1), allocatable :: ichrout( : )

C***********************************************************************                                             
c..The following can be uncommented to print symbolic J-matrix
c      allocate( ichrout( n_spec) )

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
        NCSP     = NCS
        ISAPORL  = 0
        ISPARDER = 0
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set the number of Partial derivative terms in the Jacobian and
c  count the number of terms for each species
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRX = 1, NRXNS
        DO NR = 1, 3
            IREACT = IRR( NRX,NR )
            IF ( IREACT .NE. 0 ) THEN
               DO NRPP = 1, 3 + MXPRD
                  IPORR = IRR( NRX,NRPP )
                  IF ( IPORR .NE. 0 ) ISPARDER( IPORR,IREACT ) = 1
               END DO
            END IF
         END DO
      END DO

      DO IREACT = 1, NUMB_MECH_SPC 
         DO IPORR = 1, NUMB_MECH_SPC
            IF ( ISPARDER( IPORR,IREACT ) .EQ. 1 ) 
     &           ISAPORL( IPORR ) = ISAPORL( IPORR ) + 1
         END DO
      END DO
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Sort the species, putting all with zero partial derivative 
c  terms at the bottom and those with fewest PD terms at top.
c  Set arrays for species with zero PD terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ISCHANG = 0
      NOCHANG = NUMB_MECH_SPC
      DO JOLD = 1, NUMB_MECH_SPC
         IF ( ISAPORL( JOLD ) .GT. 0 ) THEN
            ISCHANG( NCS ) = ISCHANG( NCS ) + 1
            JNEW = ISCHANG( NCS )
            INEW2OLD( JNEW,NCS ) = JOLD
            IOLD2NEW( JOLD,NCS ) = JNEW
         ELSE
            INEW2OLD( NOCHANG,NCS ) = JOLD
            IOLD2NEW( JOLD,NCS ) = NOCHANG
            NOCHANG = NOCHANG - 1
         END IF
      END DO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Now sort by number of PD terms, fewest at position 1, most at
c  the end position. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO JNEW = 1, ISCHANG( NCS )
c  Uncomment the following three lines to turn off species ordering;
c  not recommended since computational efficiency reduced
!        INEW2OLD( JNEW,NCS ) = JNEW
!        IOLD2NEW( JNEW,NCS ) = JNEW
!        IF ( JNEW .NE. 0 ) GO TO 180
         JOLD = INEW2OLD( JNEW,NCS )
         MINVALU = ISAPORL( JOLD )
         IMINOLD = JOLD
         IMINNEW = JNEW

         DO INEW = JNEW + 1, ISCHANG( NCS )
            IOLD = INEW2OLD( INEW,NCS )
            IF ( ISAPORL( IOLD ) .LT. MINVALU ) THEN
               MINVALU = ISAPORL( IOLD )
               IMINOLD = IOLD
               IMINNEW = INEW
            END IF
         END DO

         INEW2OLD( IMINNEW,NCS ) = JOLD
         INEW2OLD( JNEW,NCS )    = IMINOLD
         IOLD2NEW( JOLD,NCS )    = IMINNEW
         IOLD2NEW( IMINOLD,NCS ) = JNEW
      END DO
               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Fill the irm2 array using the new species order developed above.
c  Also determine active reactions for day and then night (i.e., photo
c  reactions determined by BTEST=.TRUE. are not included for nighttime)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NUSERAT = 0
      DO NRX = 1, NRXNS
         DO NR = 1, NREACT( NRX )
            IREACT = IRR( NRX,NR )
            IRM2( NRX,NR,NCS ) = IOLD2NEW( IREACT,NCS ) 
         END DO

         DO NP = 1, NPRDCT( NRX )
            IPROD = IRR( NRX, NP + 3 )
            IRM2( NRX,NP+3,NCS ) = IOLD2NEW( IPROD,NCS )
         END DO
         
         IF ( NREACT( NRX ) .GT. 0 ) THEN
            NUSERAT( NCS ) = NUSERAT( NCS ) + 1
            NU = NUSERAT( NCS )
            NKUSERAT( NU, NCS ) = NRX
            IF ( .NOT. ( BTEST ( IRXBITS( NRX ),1 ) ) ) THEN
               NUSERAT( NCS + 1 ) = NUSERAT( NCS + 1 ) + 1
               NU = NUSERAT( NCS + 1 )
               NKUSERAT( NU, NCS + 1 ) = NRX
            END IF
         END IF
      END DO

      DEALLOCATE( ISAPORL, ISPARDER )         

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do symbolic LU decomposition to determine sparse storage array
c  structures. Done twice, first for day and then for night. An entry
c  of 1 in lzero means a non-negative entry in the Jacobian. First
c  put ones on the diagonal and zeroes everywhere else.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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
                
      LOOP_SUN: DO IFSUN = 1, 2
         NCSP = IFSUN
         DO I = 1, NUMB_MECH_SPC
            DO J = 1, NUMB_MECH_SPC
               LZERO( J,I ) = 0
            END DO
            LZERO( I,I ) = 1
         END DO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill in the rest of the entries in the Jacobian
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NUSERAT( NCSP )
            NK = NKUSERAT( NRX,NCSP )
            DO NR = 1, NREACT( NK )
               IRE = IRM2( NK,NR,NCS )
               DO JAL = 1, NREACT( NK )
                  JRE = IRM2( NK,JAL,NCS )
                  LZERO( JRE,IRE ) = 1
               END DO
               DO IAP = 1, NPRDCT( NK )
                  JPR = IRM2( NK,3+IAP,NCS )
                  LZERO( JPR,IRE ) = 1 
               END DO
           END DO
         END DO
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Uncomment to print the undecomposed matrix symbolically
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        iglg = 0
c        write( logdev,8200 ) 
c8200    format(//1x,'jacobian ')
c        write( logdev,8211 ) (chemistry_spc(inew2old(j,ncs))(1:1),j=1,n_spec)
c8211    format( 9x, 40( 2x,A1 ) )
c        write( logdev,8211 ) (chemistry_spc(inew2old(j,ncs))(2:2),j=1,n_spec)
c        write( logdev,8211 ) (chemistry_spc(inew2old(j,ncs))(3:3),j=1,n_spec)
c        write( logdev,8211 ) (chemistry_spc(inew2old(j,ncs))(4:4),j=1,n_spec)
c        write( logdev,8210 ) (i,i=1,ischang(ncs))
c8210    format( /9x, 40I3 )
c        DO 585 i = 1, ischang( ncs )
c           k = inew2old( i,ncs )
c           DO 584 j=1,n_spec
c               if ( lzero(i,j ) .NE. 0 ) then
c                 iglg = iglg + 1
c                 ichrout( j ) = 'X'
c              else
c                 ichrout( j ) = ' '
c              end if
c584         continue
c            write( logdev,8220 ) chemistry_spc( k ), i, (ichrout( j ), j=1,n_spec)
c8220        format( 1x, A4, 1x, I2, 1x, 40( 2x,A1 ) )
c585     continue
c        write( logdev,8230 ) iglg
c8230    format( 1x,'Total number of nonzero entries=',I5 )
  
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
         END IF
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
               IF ( LZERO( J,K ) .EQ. 1 ) KOUNT0 = KOUNT0 + 1
            END DO
         END DO
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the symbolic decomposition (ludcmp) converting [A] to [L][U] 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ICLO( NCSP ) = ICNT + 1
         JCLO( NCSP ) = JCNT + 1
         DO J = 1, ISCHANG( NCS )
            J1 = J - 1
            
c...  First loop of decomposition
            DO I = 2, ISCHANG( NCS ) 
               I1 = J1 
               IF ( I .LE. J1 ) I1 = I - 1
               DO K = 1, I1
                  ICNTA = ICNTA + 1
                  IF ( LZERO( I,K ) .EQ. 1 .AND. LZERO( K,J ) .EQ. 1 )
     &               THEN
                     IZILCH( J,NCSP ) = IZILCH( J,NCSP ) + 1
                     ICNT             = ICNT + 1
                     ICNTB            = ICNTB + 1
                     IZEROK( ICNT )   = K   
                     IZEROI( ICNT )   = I
                     LZERO( I,J )     = 1 
                  END IF
               END DO
            END DO
c... Second loop of decomposition 
            DO I = J + 1, ISCHANG( NCS ) 
               JCNTA = JCNTA + 1
               IF ( LZERO( I,J ) .EQ. 1 ) THEN
                  JZILCH( J,NCSP ) = JZILCH( J,NCSP ) + 1
                  JCNT             = JCNT  + 1
                  JCNTB            = JCNTB + 1
                  JZERO( JCNT )    = I  
               END IF
            END DO 
         END DO
  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do symbolic back-substition for solving [L][U]{x}={b}. Store data
c  in sparse matrix pointer jarraypt.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c... First loop of back-substitution
         DO I = 2, ISCHANG( NCS )
            I1 = I - 1
            DO J = 1, I1    
               KCNTA = KCNTA + 1
               IF ( LZERO( I,J ) .EQ. 1 ) THEN 
                  KZILCH( I,NCSP ) = KZILCH( I,NCSP ) + 1
                  KCNT = KCNT + 1
                  IARRAY2 = IARRAY2 + 1
                  KZERO( IARRAY2,NCSP ) = J
                  JARRAYPT( I,J,NCSP ) = IARRAY2 
               END IF
            END DO
         END DO 

c... Second loop of back-substitution 
         DO I = ISCHANG( NCS ) - 1, 1, -1
            I2 = I + 1
            DO J = I + 1, ISCHANG( NCS )
               MCNTA = MCNTA + 1
               IF ( LZERO( I,J ) .EQ. 1 ) THEN 
                  MZILCH( I,NCSP )      = MZILCH( I,NCSP ) + 1
                  MCNT                  = MCNT + 1
                  IARRAY2               = IARRAY2 + 1
                  KZERO( IARRAY2,NCSP ) = J
                  JARRAYPT( I,J,NCSP )  = IARRAY2 
               END IF
            END DO
         END DO
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill jarraypt with remaining diagonal array points and save counts
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO I = 1, ISCHANG( NCS ) 
            IARRAY2 = IARRAY2 + 1
            JARRAYPT( I,I,NCSP ) = IARRAY2 
         END DO
         IARRAY( NCSP ) = IARRAY2 
         KNTARRAY = KCNTA + MCNTA + ISCHANG( NCS )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do decomposition again to change arrays to use jarraypt
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         JCB = JCLO( NCSP ) 
         JZLO( NCSP ) = JCCOUNT
         ICBSUM = ICLO( NCSP ) - 1 
         IJSTEP = 2   

         DO J = 1, ISCHANG( NCS )

c...First loop of decomposition
            IDEC1LO( J,NCSP ) = ICCOUNT + 1
            ICB = ICBSUM  + 1
            ICBSUM = ICBSUM + IZILCH( J, NCSP ) 

            DO KA = 1, IZILCH( J,NCSP ), IJSTEP
               ICCOUNT = ICCOUNT + 1
               IPA = IZEROI( ICB ) 
               KPA = IZEROK( ICB ) 
               IJDECA( ICCOUNT ) = JARRAYPT( IPA,  J,NCSP ) 
               IKDECA( ICCOUNT ) = JARRAYPT( IPA,KPA,NCSP )
               KJDECA( ICCOUNT ) = JARRAYPT( KPA,  J,NCSP )
               IF ( ICB + 1 .LE. ICBSUM ) THEN
                  IPB = IZEROI( ICB + 1 ) 
                  KPB = IZEROK( ICB + 1 ) 
                  IJDECB( ICCOUNT ) = JARRAYPT( IPB,  J,NCSP ) 
                  IKDECB( ICCOUNT ) = JARRAYPT( IPB,KPB,NCSP )
                  KJDECB( ICCOUNT ) = JARRAYPT( KPB,  J,NCSP )
               END IF
               ICB = ICB + IJSTEP   
            END DO

            IDEC1HI( J,NCSP ) = ICCOUNT  
            
c...Second loop of decomposition
            JZ = JZILCH( J, NCSP )

            DO I = 1, JZ - 1, 2
               JCCOUNT           = JCCOUNT + 1
               JHIZ1( J,NCSP )   = JHIZ1( J,NCSP ) + 1
               IA                = JZERO( JCB )
               IB                = JZERO( JCB + 1 )
               JZEROA( JCCOUNT ) = JARRAYPT( IA,J,NCSP )
               JZEROB( JCCOUNT ) = JARRAYPT( IB,J,NCSP )
               JCB = JCB + 2
            END DO

            IF ( MOD( JZ,2 ) .EQ. 1 ) THEN 
               JCCOUNT           = JCCOUNT + 1
               JHIZ2( J,NCSP )   = JHIZ2( J,NCSP ) + 1
               IA                = JZERO( JCB )
               JZEROA( JCCOUNT ) = JARRAYPT( IA,J,NCSP )
               JCB               = JCB + 1 
            END IF
         END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Group terms to increase efficiency in back-substition
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c... First back-substitution loop
         DO I = 1, ISCHANG( NCS ) 
            KZ              = KZILCH( I,NCSP )
            KZHI0( I,NCSP ) = KZ - 4 
            JZ3             = 0

            DO JZ = 1, KZHI0( I,NCSP ), 5     
               JZ3 = JZ + 4
            END DO  

            KZLO1( I,NCSP ) = JZ3 + 1
            KZHI1( I,NCSP ) = KZ  - 1 
            JZ4 = JZ3 

            DO JZ = JZ3 + 1, KZ - 1, 2    
               JZ4 = JZ + 1
            END DO

            KZLO2( I,NCSP ) = JZ4 + 1
         END DO
 
c... Second loop of back-substitution
         DO I = ISCHANG( NCS ), 1, -1
            MZ = MZILCH( I,NCSP ) 
            MZHI0( I,NCSP ) = MZ - 4  
            JZ3 = 0 

            DO JZ = 1, MZHI0( I,NCSP ), 5  
               JZ3 = JZ + 4 
            END DO

            MZLO1( I,NCSP ) = JZ3 + 1
            MZHI1( I,NCSP ) = MZ  - 1
            JZ4 = JZ3 

            DO JZ = JZ3+1, MZ-1, 2 
               JZ4 = JZ + 1 
            END DO

            MZLO2( I,NCSP ) = JZ4 + 1
         END DO
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check dimensions 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( ICNT .GT. MXCOUNT2 .OR. JCNT .GT. MXCOUNT1 .OR. 
     &        IARRAY2 .GT. MXARRAY .OR. ICCOUNT .GT. MXCOUNT2 .OR.
     &        JCCOUNT .GT. MXARRAY ) THEN
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
            WRITE( LOGDEV,94110 )
            CALL M3MESG( MSG )
            CALL M3EXIT( PNAME, IZERO, IZERO, ' ', XSTAT1 )
         END IF           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set final arrays for partial derivative calculations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NUSERAT( NCSP )
            NK = NKUSERAT( NRX,NCSP )
            DO IAL = 1, NREACT( NK )
               IR = IRM2( NK,IAL,NCS )

               DO JAL = 1, NREACT( NK )
                  JR = IRM2( NK,JAL,NCS )
                  IAR = JARRAYPT( JR,IR,NCSP )
                  NDERIVL( NK,NCSP ) = NDERIVL( NK,NCSP ) + 1
                  NLS = NDERIVL( NK,NCSP )
                  JARRL( NK,NLS,NCSP ) = IAR
                  JLIAL( NK,NLS,NCSP ) = IAL
                  NDLMAX = MAX( NLS,NDLMAX )
               END DO
               
               DO IAP = 1, NPRDCT( NK )
                  JP = IRM2( NK,IAP+3,NCS )
                  IAR = JARRAYPT( JP,IR,NCSP )
                  NDERIVP( NK,NCSP ) = NDERIVP( NK,NCSP ) + 1
                  NPR = NDERIVP( NK,NCSP )
                  JARRP(  NK,NPR,NCSP ) = IAR
                  JPIAL(  NK,NPR,NCSP ) = IAL
                  ICOEFF( NK,NPR,NCSP ) = 0
                  IF ( ABS( SC( NK,IAP ) - 1.0D0 ) .GT. 1.0D-06 ) THEN
                     ICOEFF( NK,NPR,NCSP ) = IAP
                  END IF
                  NDPMAX = MAX( NPR,NDPMAX )
               END DO
            END DO     
         END DO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check dimensions of PD arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( NDPMAX .GT. MXRP .OR. NDLMAX .GT. MXRR ) THEN
            WRITE( MSG, 94000 ) 
            CALL M3MESG( MSG )
            WRITE( MSG, 94200 ) MXRP, NDPMAX 
            CALL M3MESG( MSG )
            WRITE( MSG, 94220 ) MXRR, NDLMAX 
            CALL M3MESG( MSG )
            CALL M3EXIT( PNAME, IZERO, IZERO, ' ', XSTAT1 ) 
         END IF
700   END DO LOOP_SUN

      DEALLOCATE( ICLO,
     &            JCLO,
     &            IZEROI,
     &            IZEROK,
     &            JZERO,
     &            LZERO,       
     &            IZILCH,  
     &            JZILCH ) 
     
         
          
 
      RETURN
      
C********************** FORMAT STATEMENTS ******************************      
 
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
