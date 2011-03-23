C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/rbdata_mod.F,v 1.1 2011/03/23 21:10:54 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      MODULE  RBDATA

C*************************************************************************
C
C  FUNCTION:  Mechanism & solver data for ROS3 solver
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, August, 2004
C                    31 Jan 05 J.Young: dyn alloc - establish both horizontal
C                    & vertical domain specifications in one module (GRID_CONF)
C                    Get BLKSIZE from module GRID_CONF
C                    29 Jul 05 WTH: added variable used by degrade routines.
C
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
      INTEGER, PARAMETER :: MXARRAY = 4200  ! Max # of terms in I-hJ matrix


c..Mechanism specific variables
      INTEGER, SAVE :: N_SPEC               ! No. of species in mech
      INTEGER, SAVE :: N_RXNS               ! No. of reactions in mech

      INTEGER, SAVE :: MXCOUNT1, MXCOUNT2   ! Sparse matrx pntr dimensions
      INTEGER, SAVE :: MXRR, MXRP           ! Max # of PD terms

      LOGICAL, SAVE :: LREORDER             ! Flag to reorder or not
      LOGICAL, SAVE :: LORDERING            ! Flag for reordering
      LOGICAL, SAVE :: LSUNLIGHT            ! Flag for sun

c..Global tolerances
      REAL,    SAVE :: GLBL_ATOL                      ! Global absolute tolerances
      REAL,    SAVE :: GLBL_RTOL                      ! Global relative tolerances

c...Allocatable arrays
      REAL( 8 ), ALLOCATABLE, SAVE :: RKI( :,: )       ! Rate constants 
      REAL( 8 ), ALLOCATABLE, SAVE :: ATOL( : )        ! Species absolute tolerances 
      REAL( 8 ), ALLOCATABLE, SAVE :: RTOL( : )        ! Species relative tolerances 
      REAL( 8 ), ALLOCATABLE, SAVE :: Y( :,: )         ! Species concentrations
      REAL( 8 ), ALLOCATABLE, SAVE :: Y_DEGRADE( :,: ) ! Concentration for degradation

c..Block variables
      INTEGER BLKID                            ! Block ID
      INTEGER NBLKS                            ! No. of blocks in domain
      INTEGER NUMCELLS                         ! No. of cells in a block

c..Sparse Matrix variables 
      INTEGER :: ISCHAN          ! No. of reacting species in current mech
      INTEGER :: ISCHANG( NCS  ) ! No. of reacting species in day & nite
      INTEGER :: NUSERAT( NCS2 ) ! No. of active rxns in day & nite
      INTEGER :: IARRAY(  NCS2 ) ! No. of PD terms in I-hJ matrix

      INTEGER, ALLOCATABLE :: NKUSERAT( : , : )     ! Rxn nos of active rxns
      INTEGER, ALLOCATABLE :: IRM2( : , : , : )     ! Species rxn array
      INTEGER, ALLOCATABLE :: ICOEFF( : , : , : )   ! stoich coeff indx

      INTEGER, ALLOCATABLE :: JARRAYPT( : , : , : ) ! A-Matrix index
      INTEGER, ALLOCATABLE :: JARRL( : , : , : )    ! Pntr to PD Loss term
      INTEGER, ALLOCATABLE :: JARRP( : , : , : )    ! Pntr to PD Prod term
      INTEGER, ALLOCATABLE :: JLIAL( : , : , : )    ! Spec # for PD loss term
      INTEGER, ALLOCATABLE :: JPIAL( : , : , : )    ! Spec # for PD prod term 

      INTEGER, ALLOCATABLE :: INEW2OLD( :, : )      ! Spec index xref
      INTEGER, ALLOCATABLE :: IOLD2NEW( :, : )      ! Spec index xref

      INTEGER, ALLOCATABLE :: NDERIVL( : , : )      ! # of PD loss terms
      INTEGER, ALLOCATABLE :: NDERIVP( : , : )      ! # of PD prod terms


c..indices for decomposition
      INTEGER, ALLOCATABLE :: JZLO( : )             ! # of ops in decmp loop 1
      INTEGER, ALLOCATABLE :: IDEC1LO( :,: )      ! decomp loop 1 bound
      INTEGER, ALLOCATABLE :: IDEC1HI( :,: )      ! decomp loop 1 bound

      INTEGER, ALLOCATABLE::IJDECA( : ) ! Pntr for ij term 1 in decomp loop 1
      INTEGER, ALLOCATABLE::IJDECB( : ) ! Pntr for ij term 2 in decomp loop 1
      INTEGER, ALLOCATABLE::IKDECA( : ) ! Pntr for ik term 1 in decomp loop 1
      INTEGER, ALLOCATABLE::IKDECB( : ) ! Pntr for ik term 2 in decomp loop 1
      INTEGER, ALLOCATABLE::KJDECA( : ) ! Pntr for kj term 1 in decomp loop 1
      INTEGER, ALLOCATABLE::KJDECB( : ) ! Pntr for kj term 2 in decomp loop 1
      INTEGER, ALLOCATABLE::JZEROA( : ) ! Pntr for j term 1 in decomp loop 2
      INTEGER, ALLOCATABLE::JZEROB( : ) ! Pntr for j term 2 in decomp loop 2

      INTEGER, ALLOCATABLE :: JHIZ1( :, : )  ! # of 2-term groups in dcmp loop 2
      INTEGER, ALLOCATABLE :: JHIZ2( :, : )  ! # of 1-term groups in dcmp loop 2


      INTEGER, ALLOCATABLE :: KZLO1( :, : )  ! Start indx for 2-term bksb loop 1
      INTEGER, ALLOCATABLE :: KZLO2( :, : )  ! Start indx for 1-term bksb loop 1
      INTEGER, ALLOCATABLE :: KZHI0( :, : )  ! End index for 5-term bksub loop 1
      INTEGER, ALLOCATABLE :: KZHI1( :, : )  ! End index for 2-term bksub loop 1
      INTEGER, ALLOCATABLE :: KZERO( :, : )  ! Pointer to bksub j index

      INTEGER, ALLOCATABLE :: MZHI0(  :,: ) ! End index for 5-term bksub loop 2
      INTEGER, ALLOCATABLE :: MZHI1(  :,: ) ! End index for 2-term bksub loop 2
      INTEGER, ALLOCATABLE :: MZILCH( :,: ) ! # of calcs in bksub loop 2 (U)
      INTEGER, ALLOCATABLE :: MZLO1(  :,: ) ! Start indx for 2-term bksb loop 2
      INTEGER, ALLOCATABLE :: MZLO2(  :,: ) ! Start indx for 1-term bksb loop 2
      INTEGER, ALLOCATABLE :: KZILCH( :,: ) ! # of calcs in bksub loop 1 (L)


      INTEGER, ALLOCATABLE, SAVE :: CCOL( : )      ! Column indx of ordered cells
      INTEGER, ALLOCATABLE, SAVE :: CROW( : )      ! Row indx for ordered cells
      INTEGER, ALLOCATABLE, SAVE :: CLEV( : )      ! Layer indx of ordered cells
      INTEGER, ALLOCATABLE, SAVE :: BLKCNO( : )    ! Cell offset for each block
      INTEGER, ALLOCATABLE, SAVE :: BLKLEN( : )    ! # of cells in each block
      INTEGER, ALLOCATABLE, SAVE :: NORDCELL( : )  ! Ordered cell's original cell # 

      REAL( 8 ), ALLOCATABLE :: VDIAG( :,: )    ! LU-Matrix diagonal terms
      REAL( 8 ), ALLOCATABLE :: CC2( :,: )      ! Array holding LU-Matrix


      REAL( 8 ), ALLOCATABLE :: ERRMX2( : )       ! Estimated stiffness of each cell
      REAL( 8 ), ALLOCATABLE :: BLKTEMP( : )      ! Cell temp, deg K
      REAL( 8 ), ALLOCATABLE :: BLKPRES( : )      ! Cell press, Pa
      REAL( 8 ), ALLOCATABLE :: BLKCH2O( : )      ! Cell water conc, ppm
      REAL( 8 ), ALLOCATABLE :: BLKDENS( : )      ! Cell air denisty, kg/m^3

      LOGICAL, SAVE::CALL_DEG = .FALSE.          ! SWITCH for calling DEGRADE routine


      CONTAINS
         FUNCTION CELLVAR_ALLOC () RESULT ( SUCCESS )

         INCLUDE SUBST_IODECL      ! I/O definitions and declarations

         LOGICAL :: SUCCESS
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER :: ALST
         CHARACTER( 96 ) :: XMSG = ' '

C This function is expected to be called only once - at startup

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            SUCCESS = .TRUE.

            MXCELLS = NCOLS * NROWS * NLAYS
            MXBLKS  = 1 + ( MXCELLS - 1 ) / BLKSIZE

            ALLOCATE ( CCOL( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** CCOL Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

            ALLOCATE ( CROW( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** CROW Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

            ALLOCATE ( CLEV( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** CLEV Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

            ALLOCATE ( BLKCNO( MXBLKS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** BLKCNO Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

            ALLOCATE ( BLKLEN( MXBLKS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** BLKLEN Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

            ALLOCATE ( NORDCELL( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** NORDCELL Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

            ALLOCATE ( ERRMX2( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** ERRMX2 Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

            ELSE   ! called more than once

            XMSG = 'Horizontal domain dependent variables already allocated'
            CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN

            END IF   ! FIRSTIME

         RETURN
         END FUNCTION CELLVAR_ALLOC


      END MODULE RBDATA
