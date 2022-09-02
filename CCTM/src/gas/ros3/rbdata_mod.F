
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
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/rbdata_mod.F,v 1.4 2011/10/21 16:11:10 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      MODULE RBDATA

C*************************************************************************
C
C  Function:  Mechanism & solver data for ROS3 solver
C             
C  Preconditions: None
C 
C  Key Subroutines/Functions Called: None
C
C  REVISIOn History: Prototype created by Jerry Gipson, August, 2004
C                    31 Jan 05 J.Young: dyn alloc - establish both horizontal
C                    & vertical domain specifications in one module (GRID_CONF)
C                    Get BLKSIZE from module GRID_CONF
C                    29 Jul 05 WTH: added variable used by degrade routines.
C                    10 Aug 11 J.Young: Replaced I/O API include files
C                                       with UTILIO_DEFN
C                   15 Jul 14 B.Hutzell: 1) added variable to variable to converted
C                   species concentration unit based on species type
C                   21 Mar 22 G . Sarwar: changed BLKLAND to BLKSEAWATER
C*************************************************************************

      USE GRID_CONF             ! horizontal & vertical domain specifications

      IMPLICIT NONE

#ifdef rbstats
      INTEGER NSTEPS            ! No. of steps for stats
      INTEGER NFAILS            ! No. of convergence failures for stats
      INTEGER N_BAD_STARTS      ! No. of failures at int start for stats
#endif

c..Miscellaneous variables
      INTEGER, PARAMETER :: NCS  = 1        ! no. of chemical mechanisms
      INTEGER, PARAMETER :: NCS2 = 2 * NCS  ! accounts for day/night 

      INTEGER, PARAMETER :: MXRCT = 3       ! max no. of reactants

c..Sparse Matrix maximum dimensions
      INTEGER, PARAMETER :: MAXGL   = 150   ! Max # of P/L terms per species
      INTEGER, PARAMETER :: MAXGL2  = 70    ! Dimension (smaller than maxgl)
      INTEGER, PARAMETER :: MAXGL3  = 100   ! Dimension (smaller than maxgl)
      INTEGER, PARAMETER :: MXARRAY = 5400  ! Max # of terms in I-hJ matrix

c..Mechanism specific variables
      INTEGER :: N_SPEC               ! No. of species in mech
      INTEGER :: N_RXNS               ! No. of reactions in mech

      INTEGER :: MXCOUNT1, MXCOUNT2   ! Sparse matrx pntr dimensions
      INTEGER :: MXRR, MXRP           ! Max # of PD terms

      LOGICAL :: LREORDER             ! Flag to reorder or not
      LOGICAL :: LORDERING            ! Flag for reordering
      LOGICAL :: LSUNLIGHT            ! Flag for sun

c...Allocatable arrays
      REAL( 8 ), ALLOCATABLE :: RKI( :,: )         ! Rate constants 
      REAL( 8 ), ALLOCATABLE :: ATOL( : )          ! Species absolute tolerances 
      REAL( 8 ), ALLOCATABLE :: RTOL( : )          ! Species relative tolerances 
      REAL,      ALLOCATABLE :: FORWARD_CONV( : )  ! CGRID to CHEM Species conversion factor 
      REAL( 8 ), ALLOCATABLE :: REVERSE_CONV( : )  ! CHEM to CGRID Species conversion factor
      REAL( 8 ), ALLOCATABLE :: Y( :,: )           ! Species concentrations
      REAL( 8 ), ALLOCATABLE :: Y_DEGRADE( :,: )   ! Concentration for degradation
#ifdef sens
      REAL( 8 ), ALLOCATABLE :: YAVE( :,: )        ! Species concentrations
#endif

c..Block variables
      INTEGER :: BLKID                           ! Block ID
      INTEGER :: NBLKS                           ! No. of blocks in domain
      INTEGER :: NUMCELLS                        ! No. of cells in a block

c..Sparse Matrix variables 
      INTEGER :: ISCHAN          ! No. of reacting species in current mech
      INTEGER :: ISCHANG( NCS  ) ! No. of reacting species in day & nite
      INTEGER :: NUSERAT( NCS2 ) ! No. of active rxns in day & nite
      INTEGER :: IARRAY(  NCS2 ) ! No. of PD terms in I-hJ matrix

C Most of the following are allocated in RBINIT
      INTEGER, ALLOCATABLE :: NKUSERAT( :,: )     ! Rxn nos of active rxns
      INTEGER, ALLOCATABLE :: IRM2  ( :,:,: )     ! Species rxn array
      INTEGER, ALLOCATABLE :: ICOEFF( :,:,: )     ! stoich coeff indx

      INTEGER, ALLOCATABLE :: JARRAYPT( :,:,: )   ! A-Matrix index
      INTEGER, ALLOCATABLE :: JARRL( :,:,: )      ! Pntr to PD Loss term
      INTEGER, ALLOCATABLE :: JARRP( :,:,: )      ! Pntr to PD Prod term
      INTEGER, ALLOCATABLE :: JLIAL( :,:,: )      ! Spec # for PD loss term
      INTEGER, ALLOCATABLE :: JPIAL( :,:,: )      ! Spec # for PD prod term 

      INTEGER, ALLOCATABLE :: INEW2OLD( :,: )     ! Spec index xref
      INTEGER, ALLOCATABLE :: IOLD2NEW( :,: )     ! Spec index xref

      INTEGER, ALLOCATABLE :: NDERIVL( :,: )      ! # of PD loss terms
      INTEGER, ALLOCATABLE :: NDERIVP( :,: )      ! # of PD prod terms


c..indices for decomposition
      INTEGER, ALLOCATABLE :: JZLO( : )           ! # of ops in decmp loop 1
      INTEGER, ALLOCATABLE :: IDEC1LO( :,: )      ! decomp loop 1 bound
      INTEGER, ALLOCATABLE :: IDEC1HI( :,: )      ! decomp loop 1 bound

      INTEGER, ALLOCATABLE :: IJDECA( : ) ! Pntr for ij term 1 in decomp loop 1
      INTEGER, ALLOCATABLE :: IJDECB( : ) ! Pntr for ij term 2 in decomp loop 1
      INTEGER, ALLOCATABLE :: IKDECA( : ) ! Pntr for ik term 1 in decomp loop 1
      INTEGER, ALLOCATABLE :: IKDECB( : ) ! Pntr for ik term 2 in decomp loop 1
      INTEGER, ALLOCATABLE :: KJDECA( : ) ! Pntr for kj term 1 in decomp loop 1
      INTEGER, ALLOCATABLE :: KJDECB( : ) ! Pntr for kj term 2 in decomp loop 1
      INTEGER, ALLOCATABLE :: JZEROA( : ) ! Pntr for j term 1 in decomp loop 2
      INTEGER, ALLOCATABLE :: JZEROB( : ) ! Pntr for j term 2 in decomp loop 2

      INTEGER, ALLOCATABLE :: JHIZ1( :,: )  ! # of 2-term groups in dcmp loop 2
      INTEGER, ALLOCATABLE :: JHIZ2( :,: )  ! # of 1-term groups in dcmp loop 2


      INTEGER, ALLOCATABLE :: KZLO1( :,: )  ! Start indx for 2-term bksb loop 1
      INTEGER, ALLOCATABLE :: KZLO2( :,: )  ! Start indx for 1-term bksb loop 1
      INTEGER, ALLOCATABLE :: KZHI0( :,: )  ! End index for 5-term bksub loop 1
      INTEGER, ALLOCATABLE :: KZHI1( :,: )  ! End index for 2-term bksub loop 1
      INTEGER, ALLOCATABLE :: KZERO( :,: )  ! Pointer to bksub j index

      INTEGER, ALLOCATABLE :: MZHI0 ( :,: ) ! End index for 5-term bksub loop 2
      INTEGER, ALLOCATABLE :: MZHI1 ( :,: ) ! End index for 2-term bksub loop 2
      INTEGER, ALLOCATABLE :: MZILCH( :,: ) ! # of calcs in bksub loop 2 (U)
      INTEGER, ALLOCATABLE :: MZLO1 ( :,: ) ! Start indx for 2-term bksb loop 2
      INTEGER, ALLOCATABLE :: MZLO2 ( :,: ) ! Start indx for 1-term bksb loop 2
      INTEGER, ALLOCATABLE :: KZILCH( :,: ) ! # of calcs in bksub loop 1 (L)

C Allocated here
      INTEGER, ALLOCATABLE :: CCOL( : )      ! Column indx of ordered cells
      INTEGER, ALLOCATABLE :: CROW( : )      ! Row indx for ordered cells
      INTEGER, ALLOCATABLE :: CLEV( : )      ! Layer indx of ordered cells
      INTEGER, ALLOCATABLE :: BLKCNO( : )    ! Cell offset for each block
      INTEGER, ALLOCATABLE :: BLKLEN( : )    ! # of cells in each block
      INTEGER, ALLOCATABLE :: NORDCELL( : )  ! Ordered cell's original cell # 

C Domain decomposition info from CONC file
      INTEGER :: STARTCOLCO
      INTEGER :: ENDCOLCO
      INTEGER :: STARTROWCO
      INTEGER :: ENDROWCO

      REAL( 8 ), ALLOCATABLE :: VDIAG( :,: )    ! LU-Matrix diagonal terms
      REAL( 8 ), ALLOCATABLE :: CC2( :,: )      ! Array holding LU-Matrix


      REAL( 8 ), ALLOCATABLE :: ERRMX2( : )       ! Estimated stiffness of each cell
      REAL( 8 ), ALLOCATABLE :: BLKTEMP( : )      ! Cell temp, deg K
      REAL( 8 ), ALLOCATABLE :: BLKPRES( : )      ! Cell press, Pa
      REAL( 8 ), ALLOCATABLE :: BLKCH2O( : )      ! Cell water conc, ppm
      REAL( 8 ), ALLOCATABLE :: BLKDENS( : )      ! Cell air density, kg/m^3
      REAL,      ALLOCATABLE :: BLKSVOL( : )      ! Cell air specific volume, m^3/kg

      INTEGER :: NJPHOT                           ! Number of J-values set in PHOT
      REAL( 8 ),  ALLOCATABLE :: RJBLK( :,: )     ! J-values for each cell in block

      LOGICAL :: CALL_DEG = .FALSE.               ! SWITCH for calling DEGRADE routine
         
      REAL( 8 ), ALLOCATABLE :: BLKSEAWATER( : )      ! fractional area of OPEN+SURF 
      
      LOGICAL :: PRINT_CELL
      INTEGER :: ROS3_LOG

      CONTAINS
         FUNCTION CELLVAR_ALLOC() RESULT ( SUCCESS )

         USE UTILIO_DEFN

         LOGICAL :: SUCCESS
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER :: ALST
         CHARACTER( 96 ) :: XMSG = ' '

C This function is expected to be called only once - at startup

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            SUCCESS = .TRUE.

            ALLOCATE ( CCOL( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** CCOL Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CCOL = 0

            ALLOCATE ( CROW( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** CROW Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CROW = 0

            ALLOCATE ( CLEV( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** CLEV Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CLEV = 0

            ALLOCATE ( BLKCNO( MXBLKS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** BLKCNO Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
                 
            BLKCNO = 0

            ALLOCATE ( BLKLEN( MXBLKS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** BLKLEN Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            BLKLEN = 0

            ALLOCATE ( NORDCELL( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** NORDCELL Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            NORDCELL = 0

            ALLOCATE ( ERRMX2( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** ERRMX2 Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ERRMX2 = 0

         ELSE   ! called more than once

            XMSG = 'Horizontal domain dependent variables already allocated'
            CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN

         END IF   ! FIRSTIME

         RETURN
         END FUNCTION CELLVAR_ALLOC

      END MODULE RBDATA
