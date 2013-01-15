
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
cdjl      INTEGER, PARAMETER :: MXARRAY = 4200  ! Max # of terms in I-hJ matrix
      INTEGER, PARAMETER :: MXARRAY = 4400  ! Max # of terms in I-hJ matrix

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
      INTEGER, SAVE :: BLKID                           ! Block ID
      INTEGER, SAVE :: NBLKS                           ! No. of blocks in domain
      INTEGER, SAVE :: NUMCELLS                        ! No. of cells in a block

c..Sparse Matrix variables 
      INTEGER, SAVE :: ISCHAN          ! No. of reacting species in current mech
      INTEGER, SAVE :: ISCHANG( NCS  ) ! No. of reacting species in day & nite
      INTEGER, SAVE :: NUSERAT( NCS2 ) ! No. of active rxns in day & nite
      INTEGER, SAVE :: IARRAY(  NCS2 ) ! No. of PD terms in I-hJ matrix

C Most of the following are allocated in RBINIT
      INTEGER, ALLOCATABLE, SAVE :: NKUSERAT( :,: )     ! Rxn nos of active rxns
      INTEGER, ALLOCATABLE, SAVE :: IRM2  ( :,:,: )     ! Species rxn array
      INTEGER, ALLOCATABLE, SAVE :: ICOEFF( :,:,: )     ! stoich coeff indx

      INTEGER, ALLOCATABLE, SAVE :: JARRAYPT( :,:,: )   ! A-Matrix index
      INTEGER, ALLOCATABLE, SAVE :: JARRL( :,:,: )      ! Pntr to PD Loss term
      INTEGER, ALLOCATABLE, SAVE :: JARRP( :,:,: )      ! Pntr to PD Prod term
      INTEGER, ALLOCATABLE, SAVE :: JLIAL( :,:,: )      ! Spec # for PD loss term
      INTEGER, ALLOCATABLE, SAVE :: JPIAL( :,:,: )      ! Spec # for PD prod term 

      INTEGER, ALLOCATABLE, SAVE :: INEW2OLD( :,: )     ! Spec index xref
      INTEGER, ALLOCATABLE, SAVE :: IOLD2NEW( :,: )     ! Spec index xref

      INTEGER, ALLOCATABLE, SAVE :: NDERIVL( :,: )      ! # of PD loss terms
      INTEGER, ALLOCATABLE, SAVE :: NDERIVP( :,: )      ! # of PD prod terms


c..indices for decomposition
      INTEGER, ALLOCATABLE, SAVE :: JZLO( : )           ! # of ops in decmp loop 1
      INTEGER, ALLOCATABLE, SAVE :: IDEC1LO( :,: )      ! decomp loop 1 bound
      INTEGER, ALLOCATABLE, SAVE :: IDEC1HI( :,: )      ! decomp loop 1 bound

      INTEGER, ALLOCATABLE, SAVE :: IJDECA( : ) ! Pntr for ij term 1 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: IJDECB( : ) ! Pntr for ij term 2 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: IKDECA( : ) ! Pntr for ik term 1 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: IKDECB( : ) ! Pntr for ik term 2 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: KJDECA( : ) ! Pntr for kj term 1 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: KJDECB( : ) ! Pntr for kj term 2 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: JZEROA( : ) ! Pntr for j term 1 in decomp loop 2
      INTEGER, ALLOCATABLE, SAVE :: JZEROB( : ) ! Pntr for j term 2 in decomp loop 2

      INTEGER, ALLOCATABLE, SAVE :: JHIZ1( :,: )  ! # of 2-term groups in dcmp loop 2
      INTEGER, ALLOCATABLE, SAVE :: JHIZ2( :,: )  ! # of 1-term groups in dcmp loop 2


      INTEGER, ALLOCATABLE, SAVE :: KZLO1( :,: )  ! Start indx for 2-term bksb loop 1
      INTEGER, ALLOCATABLE, SAVE :: KZLO2( :,: )  ! Start indx for 1-term bksb loop 1
      INTEGER, ALLOCATABLE, SAVE :: KZHI0( :,: )  ! End index for 5-term bksub loop 1
      INTEGER, ALLOCATABLE, SAVE :: KZHI1( :,: )  ! End index for 2-term bksub loop 1
      INTEGER, ALLOCATABLE, SAVE :: KZERO( :,: )  ! Pointer to bksub j index

      INTEGER, ALLOCATABLE, SAVE :: MZHI0 ( :,: ) ! End index for 5-term bksub loop 2
      INTEGER, ALLOCATABLE, SAVE :: MZHI1 ( :,: ) ! End index for 2-term bksub loop 2
      INTEGER, ALLOCATABLE, SAVE :: MZILCH( :,: ) ! # of calcs in bksub loop 2 (U)
      INTEGER, ALLOCATABLE, SAVE :: MZLO1 ( :,: ) ! Start indx for 2-term bksb loop 2
      INTEGER, ALLOCATABLE, SAVE :: MZLO2 ( :,: ) ! Start indx for 1-term bksb loop 2
      INTEGER, ALLOCATABLE, SAVE :: KZILCH( :,: ) ! # of calcs in bksub loop 1 (L)

C Allocated here
      INTEGER, ALLOCATABLE, SAVE :: CCOL( : )      ! Column indx of ordered cells
      INTEGER, ALLOCATABLE, SAVE :: CROW( : )      ! Row indx for ordered cells
      INTEGER, ALLOCATABLE, SAVE :: CLEV( : )      ! Layer indx of ordered cells
      INTEGER, ALLOCATABLE, SAVE :: BLKCNO( : )    ! Cell offset for each block
      INTEGER, ALLOCATABLE, SAVE :: BLKLEN( : )    ! # of cells in each block
      INTEGER, ALLOCATABLE, SAVE :: NORDCELL( : )  ! Ordered cell's original cell # 

C Domain decomposition info from CONC file
      INTEGER, SAVE :: STARTCOLCO
      INTEGER, SAVE :: ENDCOLCO
      INTEGER, SAVE :: STARTROWCO
      INTEGER, SAVE :: ENDROWCO

      REAL( 8 ), ALLOCATABLE, SAVE :: VDIAG( :,: )    ! LU-Matrix diagonal terms
      REAL( 8 ), ALLOCATABLE, SAVE :: CC2( :,: )      ! Array holding LU-Matrix


      REAL( 8 ), ALLOCATABLE, SAVE :: ERRMX2( : )       ! Estimated stiffness of each cell
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKTEMP( : )      ! Cell temp, deg K
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKPRES( : )      ! Cell press, Pa
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKCH2O( : )      ! Cell water conc, ppm
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKDENS( : )      ! Cell air denisty, kg/m^3

      INTEGER, SAVE :: NJPHOT                       ! Number of J-values set in PHOT
      REAL,      ALLOCATABLE, SAVE :: RJBLK( :,: )  ! J-values for each cell in block

      LOGICAL, SAVE :: CALL_DEG = .FALSE.          ! SWITCH for calling DEGRADE routine

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

            ALLOCATE ( CROW( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** CROW Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( CLEV( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** CLEV Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( BLKCNO( MXBLKS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** BLKCNO Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( BLKLEN( MXBLKS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** BLKLEN Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( NORDCELL( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** NORDCELL Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( ERRMX2( MXCELLS ), STAT = ALST )
            IF ( ALST .NE. 0 ) THEN
               XMSG = '*** ERRMX2 Memory allocation failed'
               CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

         ELSE   ! called more than once

            XMSG = 'Horizontal domain dependent variables already allocated'
            CALL M3WARN ( 'CELLVAR_ALLOC', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN

         END IF   ! FIRSTIME

         RETURN
         END FUNCTION CELLVAR_ALLOC

      END MODULE RBDATA
