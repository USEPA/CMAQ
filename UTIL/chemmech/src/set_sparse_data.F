
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

      SUBROUTINE SET_SPARSE_DATA( )
 
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

      USE MECHANISM_DATA

      IMPLICIT NONE
      
C..Includes:
      

C..Parameters:
      INTEGER, PARAMETER :: IZERO = 0  ! Integer zero

C..External Functions: None

C..Local Variables: 
      LOGICAL, SAVE :: LFIRST = .TRUE. ! Flag for first call to this subroutine

      INTEGER, SAVE :: LOGDEV  = 6     ! Logical unit number for log file
      INTEGER, SAVE :: IFNEVER = 0     ! Flag for counter initialization
      INTEGER, SAVE :: NDLMAX  = 0     ! Max # of PD loss terms in any reaction
      INTEGER, SAVE :: NDPMAX  = 0     ! Max # of PD prod terms in any reaction

      CHARACTER( 32 ) :: PNAME = 'SET_SPARSE_DATA' ! Procedure name
      CHARACTER( 80 ) :: MSG                       ! Mesaage text for output log

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
      INTEGER SWAPVALUE            ! swapping number of PD terms used in sort

      INTEGER, ALLOCATABLE  :: ICLO( : )        ! Pointer to # of ops in decomp loop 1
      INTEGER, ALLOCATABLE  :: JCLO( : )        ! Pointer to # of ops in decomp loop 2

      INTEGER, ALLOCATABLE :: IZEROI  ( : )       ! Pointer to decomp loop 1 i index
      INTEGER, ALLOCATABLE :: IZEROK  ( : )       ! Pointer to decomp loop 1 k index
      INTEGER, ALLOCATABLE :: JZERO   ( : )       ! Pointer to decomp loop 2 i index
      INTEGER, ALLOCATABLE :: ISAPORL ( : )       ! Count of PD terms for each species
      INTEGER, ALLOCATABLE :: ISPARDER( :,: )     ! Indicator of a PD term in the Jacobian matrix
      INTEGER, ALLOCATABLE :: IZILCH  ( :,: )     ! # of nonzero calcs in decomp loop 1
      INTEGER, ALLOCATABLE :: JZILCH  ( :,: )     ! # of nonzero calcs in decomp loop 2
      INTEGER, ALLOCATABLE :: LZERO   ( :,: )     ! Symbolic Jacobian matrix

      INTEGER, ALLOCATABLE :: MAX_JARRAY( : )     ! maximum nozero term in each Jacobian
      INTEGER, ALLOCATABLE :: MAX_JSOLVE( : )     ! maximum operations to solve each Jacobain

      INTEGER IOS                  ! status
      INTEGER, ALLOCATABLE     :: IOLD_BUBBLE( : )
      INTEGER, ALLOCATABLE     :: INEW_BUBBLE( : )

      LOGICAL                  :: SWAPPED

      REAL( 8 ),  ALLOCATABLE :: NET_COEFF ( :, : )
      LOGICAL,    ALLOCATABLE :: NET_CHANGE( :, : )  ! true  if species affected by reaction
      LOGICAL,    ALLOCATABLE :: NO_EFFECT ( :, : ) ! false if species affected by reaction

c..The following can be uncommented to print symbolic J-matrix
c      integer iglg
c      character(1), allocatable :: ichrout( : )

C***********************************************************************                                             
c..The following can be uncommented to print symbolic J-matrix
c      allocate( ichrout( n_spec) )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize some variables on first call
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN

         MXCOUNT1 = NUMB_MECH_SPCS * MAXGL3 * 3
         MXCOUNT2 = NUMB_MECH_SPCS * MAXGL3 * 3

         ALLOCATE( ICLO( NCS2 ),
     &             JCLO( NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating ICLO or JCLO'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( MAX_JARRAY( NCS2 ),
     &             MAX_JSOLVE( NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating MAX_JARRAY or MAX_JSOLVE'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( NKUSERAT( NRXNS,NCS2 ),
     &             NDERIVL ( NRXNS,NCS2 ),
     &             NDERIVP ( NRXNS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating NKUSERAT, NDERIVL or NDERIVP'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( IRM2( MAXRCTNTS+MXPRD,NRXNS ),
     &             ICOEFF( MXRR+MXRP,NRXNS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating IRM2 or ICOEFF'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         IF( NSPECIAL .GT. 0 )THEN
            ALLOCATE( IRM2SP( NSPECIAL + 1, MAXSPECTERMS ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = 'ERROR allocating IRM2SP'
               WRITE(LOGDEV,'(A)')MSG 
               STOP
            END IF
            IRM2SP = 0
         END IF 

         ALLOCATE( JARRAYPT( NUMB_MECH_SPCS,NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JARRAYPT'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( JARRL( NRXNS,MXRR,NCS2 ),
     &             JARRP( NRXNS,MXRP,NCS2 ),
     &             JLIAL( NRXNS,MXRR,NCS2 ),
     &             JPIAL( NRXNS,MXRP,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JARRL, JARRP, JLIAL, or JPIAL'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( INEW2OLD( NUMB_MECH_SPCS ),
     &             IOLD2NEW( NUMB_MECH_SPCS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating INEW2OLD or IOLD2NEW'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
      
         IF( .NOT. ALLOCATED( CONVERT_CONC ) )THEN
            ALLOCATE( CONVERT_CONC( NUMB_MECH_SPCS ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = 'ERROR allocating CONVERT_CONC '
               WRITE(LOGDEV,'(A)')MSG 
               STOP
            END IF
            CONVERT_CONC = .FALSE.
         END IF

         ALLOCATE( JZEROA( MXARRAY ),
     &             JZEROB( MXARRAY ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JZEROA or JZEROB'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
   
         ALLOCATE( JZLO( NCS2 ),
     &             IDEC1LO( NUMB_MECH_SPCS,NCS2 ),
     &             IDEC1HI( NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JZLO, IDEC1LO or IDEC1HI'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( IJDECA( MXCOUNT2 ),
     &             IJDECB( MXCOUNT2 ),
     &             IKDECA( MXCOUNT2 ),
     &             IKDECB( MXCOUNT2 ),
     &             KJDECA( MXCOUNT2 ),
     &             KJDECB( MXCOUNT2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating IJDECA, IJDECB, IKDECA, IKDECB, KJDECA, or KJDECB'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( JHIZ1( NUMB_MECH_SPCS,NCS2 ),
     &             JHIZ2( NUMB_MECH_SPCS,NCS2 ),
     &             KZLO1( NUMB_MECH_SPCS,NCS2 ),
     &             KZLO2( NUMB_MECH_SPCS,NCS2 ),
     &             KZHI0( NUMB_MECH_SPCS,NCS2 ),
     &             KZHI1( NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating JHIZ1, JHIZ2, KZLO1, KZLO2, KZHI0, or KZHI1'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( KZERO( MXARRAY,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating KZERO'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( KZILCH( NUMB_MECH_SPCS,NCS2 ),
     &             MZHI0 ( NUMB_MECH_SPCS,NCS2 ),
     &             MZHI1 ( NUMB_MECH_SPCS,NCS2 ),
     &             MZILCH( NUMB_MECH_SPCS,NCS2 ),
     &             MZLO1 ( NUMB_MECH_SPCS,NCS2 ),
     &             MZLO2 ( NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR allocating KZILCH, MZHI0, MZHI1, MZILCH, MZLO1, or MZLO2'
               WRITE(LOGDEV,'(A)')MSG 
               STOP
         END IF

         ALLOCATE( IZEROI ( MXCOUNT1 ),
     &             IZEROK ( MXCOUNT2 ),
     &             JZERO  ( MXCOUNT1 ),  STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failed'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( ISAPORL ( NUMB_MECH_SPCS ),
     &             INEW_BUBBLE( NUMB_MECH_SPCS ),
     &             IOLD_BUBBLE( NUMB_MECH_SPCS ),
     &             ISPARDER( NUMB_MECH_SPCS,NUMB_MECH_SPCS ),
     &             LZERO   ( NUMB_MECH_SPCS,NUMB_MECH_SPCS ),
     &             IZILCH  ( NUMB_MECH_SPCS,NCS2 ),
     &             JZILCH  ( NUMB_MECH_SPCS,NCS2 ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failed'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF

         ALLOCATE( NET_COEFF ( NUMB_MECH_SPCS, NRXNS ),
     &             NET_CHANGE( NUMB_MECH_SPCS, NRXNS ),
     &             NO_EFFECT ( NUMB_MECH_SPCS, NRXNS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failed: NET_COEFF etc.'
            WRITE(LOGDEV,'(A)')MSG 
         END IF

         MAX_JARRAY = 0
         MAX_JSOLVE = 0

         IZILCH = 0
         JZILCH = 0
         JHIZ1  = 0
         JHIZ2  = 0
         KZILCH = 0
         MZILCH = 0

         NUSERAT = 0
         NDERIVL = 0
         NDERIVP = 0
         
         ISCHANG = 0
         
         JARRAYPT = 0

         IJDECA = 0
         IKDECA = 0
         KJDECA = 0

         IJDECB = 0
         IKDECB = 0
         KJDECB = 0

      END IF   ! LFIRST
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Initialize Prod/loss and PD tabulator arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NCSP = NCS

      ISAPORL  = 0
      ISPARDER = 0
      NET_COEFF  = 0.0D0
      NET_CHANGE = .FALSE.

      DO NRX = 1, NRXNS
        DO NR = 1, 3
           IREACT = IRR( NRX,NR )
           IF( IREACT .GT. 0 ) THEN
               NET_COEFF( IREACT, NRX ) = NET_COEFF( IREACT, NRX ) - 1.0D0
               IF( .NOT. ASSESS_EFFECTS )NET_CHANGE( IREACT, NRX ) = .TRUE.
           END IF
        END DO
        DO NRPP = 4, 3 + MXPRD
           IPORR = IRR( NRX,NRPP )
           IF( IPORR .GT. 0 )THEN
               NET_COEFF( IPORR, NRX ) = NET_COEFF( IPORR, NRX ) 
     &                                 + REAL( SC( NRX,NRPP-3 ), 8 )
               IF( .NOT. ASSESS_EFFECTS )NET_CHANGE( IPORR, NRX ) = .TRUE.
           END IF 
        END DO
        IF( .NOT. ASSESS_EFFECTS )CYCLE
        DO ISP = 1, NUMB_MECH_SPCS
           IF( ABS( NET_COEFF( ISP, NRX ) ) .GT. 1.0D-6 )THEN
	       NET_CHANGE( ISP, NRX ) = .TRUE.
	   END IF
        END DO
      END DO
   
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
                  IF ( IPORR .LT. 1 ) CYCLE
		  IF( NET_CHANGE( IPORR, NRX ) )THEN
		      ISPARDER( IPORR,IREACT ) = 1
		  END IF
               END DO
            END IF
            CYCLE
            IF ( IREACT .NE. 0 ) THEN
               DO NRPP = 1, NET_SPECIES( NRX )
                  IPORR = IRR_NET( NRPP, NRX )
                  IF ( IPORR .NE. 0 ) ISPARDER( IPORR,IREACT ) = 1
               END DO
            END IF
         END DO
      END DO

      DO IREACT = 1, NUMB_MECH_SPCS 
         DO IPORR = 1, NUMB_MECH_SPCS
            IF ( ISPARDER( IPORR,IREACT ) .EQ. 1 ) 
     &           ISAPORL( IPORR ) = ISAPORL( IPORR ) + 1
         END DO
      END DO
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Sort the species, putting all with zero partial derivative 
c  terms at the bottom and those with fewest PD terms at top.
c  Set arrays for species with zero PD terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NOCHANG = NUMB_MECH_SPCS
      DO JOLD = 1, NUMB_MECH_SPCS
         IF ( ISAPORL( JOLD ) .GT. 0 ) THEN
            ISCHANG( NCS ) = ISCHANG( NCS ) + 1
            JNEW = ISCHANG( NCS )
            INEW2OLD( JNEW ) = JOLD
            IOLD2NEW( JOLD ) = JNEW
         ELSE
            INEW2OLD( NOCHANG ) = JOLD
            IOLD2NEW( JOLD )    = NOCHANG
            NOCHANG = NOCHANG - 1
         END IF
      END DO
      INEW_BUBBLE(:) = IOLD2NEW(:)

!     DO J = 1, NUMB_MECH_SPCS
!        write(6,'(A,2(I4,1X),A16,1X,I6)')'Before bubble sort: J, ISAPORL( J )= ',
!    &             J, ISAPORL( J ),MECHANISM_SPC( J ),INEW_BUBBLE(J)
!     END DO
 
      IF( REORDER_SPECIES )THEN 
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
         JOLD = INEW2OLD( JNEW )
         MINVALU = ISAPORL( JOLD )
         IMINOLD = JOLD
         IMINNEW = JNEW

         DO INEW = JNEW + 1, ISCHANG( NCS )
            IOLD = INEW2OLD( INEW )
            IF ( ISAPORL( IOLD ) .LT. MINVALU ) THEN
               MINVALU = ISAPORL( IOLD )
               IMINOLD = IOLD
               IMINNEW = INEW
            END IF
         END DO

         INEW2OLD( IMINNEW ) = JOLD
         INEW2OLD( JNEW )    = IMINOLD
         IOLD2NEW( JOLD )    = IMINNEW
         IOLD2NEW( IMINOLD ) = JNEW
      END DO


!      call Bubble_Sort(ISAPORL)
        DO j = NUMB_MECH_SPCS-1, 1, -1
         swapped = .FALSE.
         DO i = 1, j
           IF ( ISAPORL(i) .GT. ISAPORL(i+1) ) THEN
             SWAPVALUE    = ISAPORL(i)
             ISAPORL(i)   = ISAPORL(i+1)
             ISAPORL(i+1) = SWAPVALUE
             INEW         = INEW_BUBBLE( i )
             INEW_BUBBLE( i )     = INEW_BUBBLE( i + 1 )
             INEW_BUBBLE( i + 1 ) = INEW
             swapped = .TRUE.
           END IF
         END DO
         IF (.NOT. swapped) EXIT
       END DO
      END IF ! reorder species

      DO J = 1, NUMB_MECH_SPCS
         I = INEW_BUBBLE(J)
         INEW2OLD(J) = INEW_BUBBLE(J)
         IOLD_BUBBLE(I) = J
!        write(6,'(A,2(I4,1X),A16,1X,I6)')'After bubble sort: J, ISAPORL( J )= ',
!    &             J, ISAPORL( J ),MECHANISM_SPC( I ),INEW_BUBBLE(I)
      END DO

      
      DO J = 1, NUMB_MECH_SPCS
         I = IOLD_BUBBLE(J)
         IOLD2NEW(J) = IOLD_BUBBLE(J)
!        write(6,'(A,2(I4,1X),A16,1X,I6)')'IOLD_BUBBLE result: I, ISAPORL( I )= ',
!    &             I, ISAPORL( I ),MECHANISM_SPC( J ),J
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Define sparse_species based sort results
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          DO J = 1, NUMB_MECH_SPCS
              I = INEW2OLD( J )
              SPARSE_SPECIES( J ) = MECHANISM_SPC( I )
!             WRITE( 6, 2065 ) J, J, J, CGRID_INDEX( I ), 
!    &       SPECIES_TYPE( I ), CONVERT_CONC( I ), TRIM( MECHANISM_SPC( I ) )
          END DO
2065   FORMAT( 6X, 'DATA', 1X, 'CGRID_INDEX(', I4,' ), SPECIES_TYPE(', I4,' ), CONVERT_CONC(', I4,' ) / ', 
     &              I4, ', ''', A2, ''', ',  L1,' /  ! ', A)

               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Fill the irm2 array using the new species order developed above.
c  Also determine active reactions for day and then night (i.e., photo
c  reactions determined by BTEST=.TRUE. are not included for nighttime)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NO_EFFECT = .TRUE.
      DO NRX = 1, NRXNS
         DO NR = 1, NREACT( NRX )
            IREACT = IRR( NRX,NR )
            IRM2( NR,NRX ) = IOLD2NEW( IREACT ) 
         END DO

         DO NP = 1, NPRDCT( NRX )
            IPROD = IRR( NRX, NP + 3 )
            IRM2( NP+3,NRX ) = IOLD2NEW( IPROD )
         END DO

         DO NRPP = 1, ISCHANG( NCS )
            JNEW = IOLD2NEW( NRPP  )
            IF( NET_CHANGE( NRPP, NRX ) )NO_EFFECT( JNEW, NRX ) = .FALSE.
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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill the irm2sp array using the  new species order developed above.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO J = 1, NSPECIAL
         DO I = 1, MAXSPECTERMS
            IF( INDEX_CTERM( J, I) .LE. 0 )CYCLE
            IRM2SP( J, I ) =  IOLD2NEW( INDEX_CTERM( J,I ) )
         END DO
      END DO
          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do symbolic LU decomposition to determine sparse storage array
c  structures. Done twice, first for day and then for night. An entry
c  of 1 in lzero means a non-negative entry in the Jacobian. First
c  put ones on the diagonal and zeroes everywhere else.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 700 IFSUN = 1, 2
         NCSP = IFSUN
         DO I = 1, NUMB_MECH_SPCS
            DO J = 1, NUMB_MECH_SPCS
               LZERO( J,I ) = 0
            END DO
            LZERO( I,I ) = 1
         END DO
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill in the rest of the entries in the Jacobian
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NUSERAT( IFSUN )
            NK = NKUSERAT( NRX,IFSUN )
            DO NR = 1, NREACT( NK )
               IRE = IRM2( NR,NK )
               DO JAL = 1, NREACT( NK )
                  JRE = IRM2( JAL,NK )
                  IF( NO_EFFECT( JRE, NK ) )CYCLE
                  LZERO( JRE,IRE ) = 1
               END DO
               DO IAP = 1, NPRDCT( NK )
                  JPR = IRM2( 3+IAP,NK )
                  IF( NO_EFFECT( JPR, NK ) )CYCLE
                  LZERO( JPR,IRE ) = 1 
               END DO
               CYCLE

               DO JAL = 1, NET_SPECIES( NK )
                  JRE = IOLD2NEW( IRR_NET( JAL, NK ) )
                  LZERO( JRE,IRE ) = 1
               END DO
           END DO
         END DO
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Uncomment to print the undecomposed matrix symbolically
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        iglg = 0
c        write( logdev,8200 ) 
c8200    format(//1x,'jacobian ')
c        write( logdev,8211 ) (chemistry_spc(inew2old(j))(1:1),j=1,n_spec)
c8211    format( 9x, 40( 2x,A1 ) )
c        write( logdev,8211 ) (chemistry_spc(inew2old(j))(2:2),j=1,n_spec)
c        write( logdev,8211 ) (chemistry_spc(inew2old(j))(3:3),j=1,n_spec)
c        write( logdev,8211 ) (chemistry_spc(inew2old(j))(4:4),j=1,n_spec)
c        write( logdev,8210 ) (i,i=1,ischang(ncs))
c8210    format( /9x, 40I3 )
c        DO 585 i = 1, ischang( ncs )
c           k = inew2old( i )
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
         print*,KOUNT0A,KOUNT0


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the symbolic decomposition (ludcmp) converting [A] to [L][U] 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ICLO( IFSUN ) = ICNT + 1
         JCLO( IFSUN ) = JCNT + 1
         LUDECOMP: DO J = 1, ISCHANG( NCS )
            J1 = J - 1
            
c...  First loop of decomposition
            DO I = 2, ISCHANG( NCS ) 
               I1 = J1 
               IF ( I .LE. J1 ) I1 = I - 1
               DO K = 1, I1
                  ICNTA = ICNTA + 1
                  IF(LZERO( I,K ).EQ.1 .AND. LZERO( K,J ).EQ.1)THEN
                     IZILCH( J,IFSUN ) = IZILCH( J,IFSUN ) + 1
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
                  JZILCH( J,IFSUN ) = JZILCH( J,IFSUN ) + 1
                  JCNT             = JCNT  + 1
                  JCNTB            = JCNTB + 1
                  JZERO( JCNT )    = I  
               END IF
            END DO 
         END DO LUDECOMP
         KOUNT0A = 0
         KOUNT0  = 0
         DO J = 1, ISCHANG( NCS )
            DO K = 1, ISCHANG( NCS )
               KOUNT0A = KOUNT0A + 1
               IF ( LZERO( J,K ) .EQ. 1 ) KOUNT0 = KOUNT0 + 1
            END DO
         END DO
         print*,KOUNT0A,KOUNT0
  
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
                  KZILCH( I,IFSUN ) = KZILCH( I,IFSUN ) + 1
                  KCNT = KCNT + 1
                  IARRAY2 = IARRAY2 + 1
                  KZERO( IARRAY2,IFSUN ) = J
                  JARRAYPT( I,J,IFSUN ) = IARRAY2 
               END IF
            END DO
         END DO 
         print*,'First loop of back-substitution; IARRAY2: ', IARRAY2

c... Second loop of back-substitution 
         DO I = ISCHANG( NCS ) - 1, 1, -1
            I2 = I + 1
            DO J = I + 1, ISCHANG( NCS )
               MCNTA = MCNTA + 1
               IF ( LZERO( I,J ) .EQ. 1 ) THEN 
                  MZILCH( I,IFSUN )      = MZILCH( I,IFSUN ) + 1
                  MCNT                  = MCNT + 1
                  IARRAY2               = IARRAY2 + 1
                  KZERO( IARRAY2,IFSUN ) = J
                  JARRAYPT( I,J,IFSUN )  = IARRAY2 
               END IF
            END DO
         END DO
         print*,'Second loop of back-substitution; IARRAY2: ', IARRAY2
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill jarraypt with remaining diagonal array points and save counts
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO I = 1, ISCHANG( NCS ) 
            IARRAY2 = IARRAY2 + 1
            JARRAYPT( I,I,IFSUN ) = IARRAY2 
         END DO
         print*,'Fill jarraypt with remaining diagonal array points; IARRAY2: ',IARRAY2
         IARRAY( IFSUN ) = IARRAY2 
         KNTARRAY = KCNTA + MCNTA + ISCHANG( NCS )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do decomposition again to change arrays to use jarraypt
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         JCB = JCLO( IFSUN ) 
         JZLO( IFSUN ) = JCCOUNT
         ICBSUM = ICLO( IFSUN ) - 1 
         IJSTEP = 2   

         DO J = 1, ISCHANG( NCS )

c...First loop of decomposition
            IDEC1LO( J,IFSUN ) = ICCOUNT + 1
            ICB = ICBSUM  + 1
            ICBSUM = ICBSUM + IZILCH( J, IFSUN ) 

            DO KA = 1, IZILCH( J,IFSUN ), IJSTEP
               ICCOUNT = ICCOUNT + 1
               IPA = IZEROI( ICB ) 
               KPA = IZEROK( ICB ) 
               IJDECA( ICCOUNT ) = JARRAYPT( IPA,  J,IFSUN ) 
               IKDECA( ICCOUNT ) = JARRAYPT( IPA,KPA,IFSUN )
               KJDECA( ICCOUNT ) = JARRAYPT( KPA,  J,IFSUN )
               IF ( ICB + 1 .LE. ICBSUM ) THEN
                  IPB = IZEROI( ICB + 1 ) 
                  KPB = IZEROK( ICB + 1 ) 
                  IJDECB( ICCOUNT ) = JARRAYPT( IPB,  J,IFSUN ) 
                  IKDECB( ICCOUNT ) = JARRAYPT( IPB,KPB,IFSUN )
                  KJDECB( ICCOUNT ) = JARRAYPT( KPB,  J,IFSUN )
               END IF
               ICB = ICB + IJSTEP   
            END DO

            IDEC1HI( J,IFSUN ) = ICCOUNT  
            
c...Second loop of decomposition
            JZ = JZILCH( J, IFSUN )

            DO I = 1, JZ - 1, 2
               JCCOUNT           = JCCOUNT + 1
               JHIZ1( J,IFSUN )   = JHIZ1( J,IFSUN ) + 1
               IA                = JZERO( JCB )
               IB                = JZERO( JCB + 1 )
               JZEROA( JCCOUNT ) = JARRAYPT( IA,J,IFSUN )
               JZEROB( JCCOUNT ) = JARRAYPT( IB,J,IFSUN )
               JCB = JCB + 2
            END DO

            IF ( MOD( JZ,2 ) .EQ. 1 ) THEN 
               JCCOUNT           = JCCOUNT + 1
               JHIZ2( J,IFSUN )   = JHIZ2( J,IFSUN ) + 1
               IA                = JZERO( JCB )
               JZEROA( JCCOUNT ) = JARRAYPT( IA,J,IFSUN )
               JCB               = JCB + 1 
            END IF
         END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Group terms to increase efficiency in back-substition
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c... First back-substitution loop
         DO I = 1, ISCHANG( NCS ) 
            KZ              = KZILCH( I,IFSUN )
            KZHI0( I,IFSUN ) = KZ - 4 
            JZ3             = 0

            DO JZ = 1, KZHI0( I,IFSUN ), 5     
               JZ3 = JZ + 4
            END DO  

            KZLO1( I,IFSUN ) = JZ3 + 1
            KZHI1( I,IFSUN ) = KZ  - 1 
            JZ4 = JZ3 

            DO JZ = JZ3 + 1, KZ - 1, 2    
               JZ4 = JZ + 1
            END DO

            KZLO2( I,IFSUN ) = JZ4 + 1
         END DO
 
c... Second loop of back-substitution
         DO I = ISCHANG( NCS ), 1, -1
            MZ = MZILCH( I,IFSUN ) 
            MZHI0( I,IFSUN ) = MZ - 4  
            JZ3 = 0 

            DO JZ = 1, MZHI0( I,IFSUN ), 5  
               JZ3 = JZ + 4 
            END DO

            MZLO1( I,IFSUN ) = JZ3 + 1
            MZHI1( I,IFSUN ) = MZ  - 1
            JZ4 = JZ3 

            DO JZ = JZ3+1, MZ-1, 2 
               JZ4 = JZ + 1 
            END DO

            MZLO2( I,IFSUN ) = JZ4 + 1
         END DO
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check dimensions 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( ICNT .GT. MXCOUNT2 .OR. JCNT .GT. MXCOUNT1 .OR. 
     &        IARRAY2 .GT. MXARRAY .OR. ICCOUNT .GT. MXCOUNT2 .OR.
     &        JCCOUNT .GT. MXARRAY ) THEN
            WRITE( MSG, 94000 ) 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94020 ) MXCOUNT2, ICNT 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94040 ) MXCOUNT1, JCNT 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94060 ) MXARRAY, IARRAY2 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94080 ) MXCOUNT2, ICCOUNT 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94100 ) MXARRAY, JCCOUNT, MAXGL3
            WRITE( LOGDEV,94110 )
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF           

!         MAX_JSOLVE( IFSUN )  = INT(MAX( ICNT, JCNT, ICCOUNT ) / ( 3 * NUMB_MECH_SPCS )  ) + 1
         MAX_JSOLVE( IFSUN )  = MAX( ICNT, JCNT, ICCOUNT )
         MAX_JARRAY( IFSUN )  = MAX( IARRAY2,  JCCOUNT ) + 1
         WRITE(6,'(A,I3, 1X, 6(I8,1X))')'IFSUN, MAX_JSOLVE, MAX_JARRAY, IARRAY2, JCCOUNT: ',IFSUN, 
     &   MAX_JSOLVE( IFSUN ), MAX_JARRAY( IFSUN ),IARRAY2,  JCCOUNT

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set final arrays for partial derivative calculations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO NRX = 1, NUSERAT( IFSUN )
            NK = NKUSERAT( NRX,IFSUN )
            DO IAL = 1, NREACT( NK )
               IR = IRM2( IAL,NK )

               DO JAL = 1, NREACT( NK )
                  JR = IRM2( JAL,NK )
                  IF( NO_EFFECT( JR, NK ) ) CYCLE
                  IAR = JARRAYPT( JR,IR,NCSP )
                  NDERIVL( NK,NCSP ) = NDERIVL( NK,NCSP ) + 1
                  NLS = NDERIVL( NK,NCSP )
                  JARRL( NK,NLS,IFSUN ) = IAR
                  JLIAL( NK,NLS,IFSUN ) = IAL
                  NDLMAX = MAX( NLS,NDLMAX )
               END DO
               
               DO IAP = 1, NPRDCT( NK )
                  JP = IRM2( IAP+3,NK )
                  IF( NO_EFFECT( JP, NK ) ) CYCLE
                  IAR = JARRAYPT( JP,IR,NCSP )
                  NDERIVP( NK,NCSP ) = NDERIVP( NK,NCSP ) + 1
                  NPR = NDERIVP( NK,NCSP )
                  JARRP(  NK,NPR,IFSUN ) = IAR
                  JPIAL(  NK,NPR,IFSUN ) = IAL
                  ICOEFF( NPR,NK,NCSP ) = 0
                  IF ( ABS( SC( NK,IAP ) - 1.0 ) .GT. 1.0D-06 ) THEN
                     ICOEFF( NPR,NK,NCSP ) = IAP
                  END IF
                  NDPMAX = MAX( NPR,NDPMAX )
               END DO
               CYCLE

               DO JAL = 1, PURE_NREACT( NK )
!                  print*,NK,TRIM(RXLABEL(NK)),PURE_NREACT( NK ),NET_SPECIES( NK ),IRR_NET( JAL,NK ) 
                  JR = IOLD2NEW( IRR_NET( JAL,NK ) )
                  IAR = JARRAYPT( JR,IR,IFSUN )
                  NDERIVL( NK,IFSUN ) = NDERIVL( NK,IFSUN ) + 1
                  NLS = NDERIVL( NK,IFSUN )
                  JARRL( NK,NLS,IFSUN ) = IAR
                  JLIAL( NK,NLS,IFSUN ) = IAL
                  NDLMAX = MAX( NLS,NDLMAX )
               END DO
               
               DO IAP = (PURE_NREACT( NK )+1), NET_SPECIES( NK )
                  JP = IOLD2NEW( IRR_NET( IAP,NK ) )
                  IAR = JARRAYPT( JP,IR,IFSUN )
                  NDERIVP( NK,IFSUN ) = NDERIVP( NK,IFSUN ) + 1
                  NPR = NDERIVP( NK,IFSUN )
                  JARRP(  NK,NPR,IFSUN ) = IAR
                  JPIAL(  NK,NPR,IFSUN ) = IAL
!                  print*,NK,TRIM(RXLABEL(NK)),JP,NPR,PURE_NREACT( NK ),NET_SPECIES( NK ),(MXRP+MXRR)
                  ICOEFF( NPR,NK,IFSUN ) = 0
                  IF ( ABS( SC_NET( IAP, NK ) - 1.0 ) .GT. 1.0E-06 ) THEN
                     ICOEFF( NPR,NK,IFSUN ) = IAP
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
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94200 ) MXRP, NDPMAX 
            WRITE(LOGDEV,'(A)')MSG 
            WRITE( MSG, 94220 ) MXRR, NDLMAX 
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
700   CONTINUE

      

      WRITE(6,'(A,2(I8,1X))')'Initial MXCOUNT2, MXARRAY: ',MXCOUNT2, MXARRAY
      MAXGL3  = MAXVAL( MAX_JSOLVE )
      MXARRAY = MAXVAL( MAX_JARRAY )
      WRITE(6,'(A,2(I8,1X))')'  Reset MXCOUNT2, MXARRAY: ',MAXGL3, MXARRAY
  
!      PAUSE !      STOP
      RETURN
      
C********************** FORMAT STATEMENTS ******************************      
 
94000 FORMAT( 1X,'One of the dimensions below is too small:')
94020 FORMAT( 1X,'DIMENSION: MXCOUNT2 = ',I6,' VARIABLE: ICNT    = ',I6)  
94040 FORMAT( 1X,'DIMENSION: MXCOUNT1 = ',I6,' VARIABLE: JCNT    = ',I6)  
94060 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: IARRAY2 = ',I6)  
94080 FORMAT( 1X,'DIMENSION: MXCOUNT2 = ',I6,' VARIABLE: ICCOUNT = ',I6)  
94100 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: JCCOUNT = ',I6,' MAXGL3   = ',I6 )
94110 FORMAT( 1X,'NOTE:      MXCOUNT[1,2] = NUMB_MECH_SPCS * MAXGL3 * 3' )
94200 FORMAT( 1X,'DIMENSION: MXRP     = ',I6,' VARIABLE: NDPMAX  = ',I6)
94220 FORMAT( 1X,'DIMENSION: MXRR     = ',I6,' VARIABLE: NDLMAX  = ',I6)
      END SUBROUTINE SET_SPARSE_DATA
