
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/GRVARS.F,v 1.3 2011/10/21 16:11:12 yoj Exp $

      MODULE GRVARS

!     USE GRID_CONF, ONLY: BLKSIZE  ! horizontal & vertical domain specifications
      USE GRID_CONF                 ! horizontal & vertical domain specifications

      IMPLICIT NONE


C......................................................................
C  INCLUDE FILE: GRPARMS.EXT
C  CONTAINS: Dimensioning parameters for Gear solver
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995
C                    Revised 3/15/96 by Jerry Gipson to conform to
C                      Models-3 minimum IOV configuration.
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Revised April 1997 to distinguish NSPCS from NSPCSD
C                    Revised April 1997 to conform to Models-3 framework
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C......................................................................

C # of chemical mechanisms used
      INTEGER, PARAMETER :: NCS = 1

C 2 * # of chemical mechanisms
      INTEGER, PARAMETER :: NCS2 = 2 * NCS
      INTEGER, PARAMETER :: MXRCT = 3       ! max no. of reactants

C Maximum # of terms in [P]=[I]-bh[J]
      INTEGER, PARAMETER :: MXARRAY = 5400

C Maximum # prod/loss terms for a species
      INTEGER, PARAMETER :: MAXGL = 150

C A dimension smaller that MAXGL
      INTEGER, PARAMETER :: MAXGL2 = 70

C A dimension smaller MAXGL2
      INTEGER, PARAMETER :: MAXGL3 = 100

C Maximum order possible      
      INTEGER, PARAMETER :: MXORDER = 7

C Sparse matrix pointer dimension     
      INTEGER :: MXCOUNT1

C Sparse matrix pointer dimension     
      INTEGER :: MXCOUNT2

C Sparse matrix pointer dimension     
      INTEGER :: MXCOUNT3 

C Sparse matrix pointer dimension
      INTEGER :: MXCOUNT4 

C Maximum # of reactant PD terms
      INTEGER :: MXRR

C Maximum # of product PD terms
      INTEGER :: MXRP 

      
C............................ end GRPARMS.EXT .........................

C......................................................................
C  INCLUDE FILE: GRVARS1.EXT
C  CONTAINS: Common block definitions for Gear control data that are 
C            set in subroutine GRINIT
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      the Models-3 minimum IOV configuration.
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C                    17 Aug 01 by J.Young: split out variables that are
C                    intrinsically dependent on NCOLS, NROWS into an f90
C                    module
C......................................................................
      LOGICAL LDEBUG          ! Flag to turn on/off debug output
      LOGICAL LDUMPBLK        ! Flag to dump IC data for one block
      LOGICAL LDUMPCELL       ! Flag to dump IC data for one cell
      LOGICAL LTRACE          ! Flag for trace report for one block
      LOGICAL LPERFSMRY       ! Flag for Gear summary statistics
      LOGICAL LCELLCONC       ! Flag to output cell concentrations
      LOGICAL LREORDER        ! Flag to reorder cells for Gear solver
      
C ldebug option -- output debug report for one run      
      INTEGER DBGCOL          ! Column number for debug data
      INTEGER DBGROW          ! Row number for debug data
      INTEGER DBGLEV          ! Layer number for debug data
      INTEGER IBLKBUG         ! Block number for debug output  
      INTEGER ICPR            ! Cell number for cell data output
      INTEGER IRUNBUG         ! Run number for debug output
      INTEGER IUNDBG          ! Unit number of output file
      INTEGER NPDOUT          ! Number for Jacobian evaluation output
      INTEGER NSTEPOUT        ! Gear step number for debug output
      INTEGER NSUBOUT         ! Number for RHS evaluation output
            
C ldumpblk option -- output IC data for one block
      INTEGER IBLKBLK         ! Block number to output
      INTEGER IRUNBLK         ! Run number to output
      INTEGER IUNBIC          ! Unit number of output file
      
C ldumpcell option -- output IC data for one cell      
      INTEGER IBLKCELL        ! Block number to output
      INTEGER INUMCELL        ! Cell number to output
      INTEGER IRUNCELL        ! Run number to output
      INTEGER IUNCIC          ! Unit number of output file
      
C ltrace option -- output trace report for one run
      INTEGER IBLKTRC         ! Block number to output 
      INTEGER IRUNTRC1        ! Run number to output
      INTEGER IRUNTRC2        ! Run number to output
      INTEGER IUNTRC          ! Unit number of output file
            
C lperfsmry option -- output Gear performance report for entire run
      INTEGER IUNPERF         ! Unit number for output file
      
C lcellconc option -- output conc. of one cell at Gear time steps      
      INTEGER CCOLOUT         ! Column index of cell to output
      INTEGER CLEVOUT         ! Layer index of cell to output
      INTEGER CROWOUT         ! Row index of cell to output
      INTEGER IRUNPRO1        ! Starting run number for output
      INTEGER IRUNPRO2        ! Ending run number for output
      INTEGER IUNCOUT         ! Unit number of output file
      INTEGER  NCELL1          ! cell where col, row, lev equal one
      
C  Other variables      
      INTEGER NBLKS               ! Number of blocks of cells

C real variables
      REAL( 8 ) :: CONCMIN        ! Zero threshold used in Gear solver
      REAL( 8 ) :: FRACDEC        ! Gear time step reduction factor 
      REAL( 8 ) :: HMAXNIT        ! Maximum Gear time step for nighttime
      REAL( 8 ) :: HMIN           ! Minimum Gear time step
      REAL( 8 ) :: RUNMIN         ! Simulation time for lcellconc option
      REAL( 8 ) :: ZBOUND         ! Lower bound of zero threshold
      REAL( 8 ) :: ERRMAX ( NCS ) ! Gear relative error tolerance
      REAL( 8 ) :: HMAXDAY( NCS ) ! Maximum Gear time step for daytime
      REAL( 8 ) :: YLOW   ( NCS ) ! Gear absolute error tolerance

C........................... end GRVARS1.EXT ..........................

C......................................................................
C  INCLUDE FILE: GRVARS2.EXT
C  CONTAINS: Common block definitions for Gear control data that are 
C            set in subroutine GRSPARS
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995.
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      the Models-3 minimum IOV configuration.
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Revised April 1997 to distinguish NSPCS from NSPCS
C                    Revised April 1997 to conform to Models-3 framework
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C......................................................................

      INTEGER MAXORD              ! Max order allowed
      INTEGER MBETWEEN            ! Max # of steps between calls to update
                                  ! the Jacobian
      INTEGER MSTEP               ! Max # of corrector iterations allowed
      INTEGER IARRAY(     NCS2 )  ! Number of PD terms in sparse matrix
      INTEGER, ALLOCATABLE, SAVE :: IJDECA( : )  ! Pointer for ij term 1 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: IJDECB( : )  ! Pointer for ij term 2 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: IKDECA( : )  ! Pointer for ik term 1 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: IKDECB( : )  ! Pointer for ik term 2 in decomp loop 1
      INTEGER ISCHANG(     NCS )  ! Number of reacting species
      INTEGER JZEROA(  MXARRAY )  ! Pointer for j term 1 in decomp loop 2
      INTEGER JZEROB(  MXARRAY )  ! Pointer for j term 2 in decomp loop 2
      INTEGER JZLO(       NCS2 )  ! # of ops in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: KJDECA( : )  ! Pointer for kj term 1 in decomp loop 1
      INTEGER, ALLOCATABLE, SAVE :: KJDECB( : )  ! Pointer for kj term 2 in decomp loop 1
      INTEGER NUSERAT(    NCS2 )  ! Number of active reactions in day and
                                  ! night mechanisms 
                                  
      INTEGER, ALLOCATABLE, SAVE :: IDEC1LO ( :, : ) ! Inner start index for dcmp loop 1
      INTEGER, ALLOCATABLE, SAVE :: IDEC1HI ( :, : ) ! Inner end index for dcmp loop 1
      INTEGER, ALLOCATABLE, SAVE :: INEW2OLD( :, : ) ! Gives sorted species number from
                                                     ! original species number index
      INTEGER, ALLOCATABLE, SAVE :: IOLD2NEW( :, : ) ! Gives original species number 
                                                     ! from sorted species number index
      INTEGER, ALLOCATABLE, SAVE :: JHIZ1   ( :, : ) ! # of 2-term groups in dcmp loop 2
      INTEGER, ALLOCATABLE, SAVE :: JHIZ2   ( :, : ) ! # of 1-term groups in dcmp loop 2

      INTEGER                    :: KZERO   ( MXARRAY, NCS2 )                ! Pointer to bksub j index 
      
      INTEGER, ALLOCATABLE, SAVE :: KZHI0 ( :, : )  ! End index for 5-term bksub loop 1
      INTEGER, ALLOCATABLE, SAVE :: KZHI1 ( :, : )  ! End index for 2-term bksub loop 1
      INTEGER, ALLOCATABLE, SAVE :: KZILCH( :, : )  ! # of calcs in bksub loop 1 (L) 
      INTEGER, ALLOCATABLE, SAVE :: KZLO1 ( :, : )  ! Start index for 2-term bksub loop 1
      INTEGER, ALLOCATABLE, SAVE :: KZLO2 ( :, : )  ! Start index for 1-term bksub loop 1
      INTEGER, ALLOCATABLE, SAVE :: MZHI0 ( :, : )  ! End index for 5-term bksub loop 2
      INTEGER, ALLOCATABLE, SAVE :: MZHI1 ( :, : )  ! End index for 2-term bksub loop 2
      INTEGER, ALLOCATABLE, SAVE :: MZILCH( :, : )  ! # of calcs in bksub loop 2 (U)
      INTEGER, ALLOCATABLE, SAVE :: MZLO1 ( :, : )  ! Start index for 2-term bksub loop 2
      INTEGER, ALLOCATABLE, SAVE :: MZLO2 ( :, : )  ! Start index for 1-term bksub loop 2
      
      INTEGER, ALLOCATABLE, SAVE :: NDERIVL ( :, : )  ! # of loss PD terms per reaction
      INTEGER, ALLOCATABLE, SAVE :: NDERIVP ( :, : )  ! # of prod PD terms per reaction
      INTEGER, ALLOCATABLE, SAVE :: NKUSERAT( :, : )  ! Rxn numbers of active reactions
                                                      ! in day and night  

      INTEGER, ALLOCATABLE, SAVE :: IRM2  ( :,:,: )     ! Species rxn array
      INTEGER, ALLOCATABLE, SAVE :: ICOEFF( :,:,: )     ! stoich coeff indx
      INTEGER, ALLOCATABLE, SAVE :: JARRL( :,:,: )      ! Pntr to PD Loss term
      INTEGER, ALLOCATABLE, SAVE :: JARRP( :,:,: )      ! Pntr to PD Prod term
      INTEGER, ALLOCATABLE, SAVE :: JLIAL( :,:,: )      ! Spec # for PD loss term
      INTEGER, ALLOCATABLE, SAVE :: JPIAL( :,:,: )      ! Spec # for PD prod term 
                                                                                           
      INTEGER, ALLOCATABLE, SAVE :: JARRAYPT( :, :, : ) ! Pointer to location of the
                                                        ! PD terms in the 1D vector
                                           
      REAL( 8 ) :: CONP15( MXORDER )   ! Gear parameters used in convergence test
      REAL( 8 ) :: CONPST( MXORDER )   ! Gear parameters used in convergence test
      REAL( 8 ) :: ENQQ1 ( MXORDER )   ! Gear coefficients used to select order   
      REAL( 8 ) :: ENQQ2 ( MXORDER )   ! and step size
      REAL( 8 ) :: ENQQ3 ( MXORDER )   !
      REAL( 8 ) :: ASET( 10, 8 )       ! Gear parameters for calculating [P] and
                                       ! and determining the order

      REAL( 8 ) :: PERTST( MXORDER, 3 )  ! Gear coefficients used to select order and step size
      DATA PERTST /   ! (7,3)
     &      2.0D0,  4.5D0,  7.333D0, 10.42D0,  13.7D0,     17.15D0,     1.0D0,
     &      3.0D0,  6.0D0,  9.167D0, 12.5D0,   15.98D0,     1.0D0,      1.0D0,
     &      1.0D0,  1.0D0,  0.5D0,    0.1667D0, 0.04133D0,  0.008267D0, 1.0D0 /

C......................................................................
C  INCLUDE FILE: GRVARS3.EXT
C  CONTAINS: Common block definitions for Gear data that are set in 
C            in the solver subroutines
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995.
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      the Models-3 minimum IOV configuration.
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Revised April 1997 to distinguish NSPCS from NSPCS
C                    Revised April 1997 to conform to Models-3 framework
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C                    17 Aug 01 by J.Young: split out variables that are
C                    intrinsically dependent on NCOLS, NROWS into an f90
C                    module
C......................................................................

      LOGICAL LCONCOUT       ! Flag to turn on cell conc. output
      LOGICAL LORDERING      ! Flag to indicate cell ordering on
      LOGICAL LSUNLIGHT      ! Flag for daytime
      LOGICAL LTRCOUT        ! Flag to turn on Trace output

      INTEGER BLKID          ! Number of block being processed
      INTEGER CELLOUT        ! Number of cell to output concentrations
      INTEGER IRSTART        ! Number of restarts at beginning
      INTEGER ISCHAN         ! Number of species in [P]
      INTEGER OFFSET         ! Pointer for start cell number in block
      INTEGER MXORDUSED      ! Maximum order used
      INTEGER NCFAIL         ! Number of convergence failures
      INTEGER NCSP           ! Day/night mechanism index;
                             ! = NCS ===>day; = NCS+1 ===>night      
      INTEGER NEFAIL         ! Number of error test failures
      INTEGER NPDERIV        ! Number of Jacobian updates
      INTEGER NSTEPS         ! Number of steps used
      INTEGER NSUBFUN        ! Number of RHS evaluations
      INTEGER NUMBKUPS       ! Number of backups
      INTEGER NUMCELLS       ! Number of cells in block being processed
      INTEGER NUMNEWT        ! Number of iterations

      REAL( 8 ) :: HMAX                ! Maximum Gear time step (min)
      REAL( 8 ) :: R1DELT              ! Time step times Gear coefficient

      REAL( 8 ), ALLOCATABLE, SAVE :: BLKTEMP( : )      ! Cell temp, deg K
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKPRES( : )      ! Cell press, Pa
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKCH2O( : )      ! Cell water conc, ppm
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKDENS( : )      ! Cell air denisty, kg/m^3
      REAL,      ALLOCATABLE, SAVE :: BLKSVOL( : )      ! Cell air specific volume, m^3/kg
      REAL( 8 ), ALLOCATABLE, SAVE :: RJBLK( :,: )      ! J-values for each cell in block
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKHET( :, : )    ! heterogeneous rate in block    
      REAL( 8 ), ALLOCATABLE, SAVE :: BLKSEAWATER( : )  ! fractional area of OPEN+SURF

      REAL( 8 ), ALLOCATABLE, SAVE :: BLKCONC( :, : ) ! Species conc. for cells in block
                                                      ! in original species order (ppm) 
      REAL( 8 ), ALLOCATABLE, SAVE :: CNEW( :, : )    ! Species conc. for cells in block
#ifdef sens
      REAL( 8 ), ALLOCATABLE       :: CAVEG( :,: )     ! Average species concentrations over time step
      REAL( 8 ), ALLOCATABLE       :: CINIT( :,: )     ! species concentrations at start of subtime step
      REAL( 8 ), ALLOCATABLE       :: CFINI( :,: )     ! species concentrations at start of subtime step
#endif
      
                                                      ! in sorted species order (ppm)
      REAL( 8 ), ALLOCATABLE, SAVE :: EMBLK( :, : )   ! Species emissions in each cell
      REAL( 8 ), ALLOCATABLE, SAVE :: GLOSS( :, : )   ! dc/dt for each species (i.e., RHS)
      REAL( 8 ), ALLOCATABLE, SAVE :: VDIAG( :, : )   ! L-U Diagonal terms  
      
      REAL( 8 ) :: CC2(     BLKSIZE, 0:MXARRAY )  ! Array holding Jacobian

      REAL( 8 ), ALLOCATABLE, SAVE :: RK   ( :,: )    ! Rate constants 
      REAL( 8 ), ALLOCATABLE, SAVE :: RXRAT( :,: )    ! Reaction rates for each cell


      REAL,      ALLOCATABLE, SAVE :: FORWARD_CONV( : )  ! CGRID to CHEM Species conversion factor 
      REAL( 8 ), ALLOCATABLE, SAVE :: REVERSE_CONV( : )  ! CHEM to CGRID Species conversion factor

C........................... end GRVARS3.EXT ..........................

C.......................................................................
C     MODULE GRVARS
C smvgear variables that are dependent on NCOLS, NROWS
C Revision History: J.Young 17 Aug 01: create
C                   J.Young 31 Jan 05: get MXCELLS from dyn alloc horizontal
C                   & vertical domain specifications module (GRID_CONF)
C                   29 Jul 05 WTH: added variable used by degrade routines.
C                   29 Mar 11 S.Roselle: Replaced I/O API include files 
C                   with UTILIO_DEFN
C                   15 Jul 14 B.Hutzell: 1) replaced mechanism include files with 
C                   RXNS_DATA module, 2) inserted call to function MAP_CHEMISTRY_SPECIES 
C                   RXNS_FUNCTION module, 3) changed several array declarations from fixed
C                   to allocatable dimensions, and 4) inserted do loop that calculates species
C                   unit conversion factors based on species type
C.......................................................................
C Column index of ordered cells
      INTEGER, ALLOCATABLE, SAVE :: CCOL( : )
C Row index for ordered cells
      INTEGER, ALLOCATABLE, SAVE :: CROW( : )
C Layer index of ordered cells
      INTEGER, ALLOCATABLE, SAVE :: CLEV( : )
C Cell number offset for each block
      INTEGER, ALLOCATABLE, SAVE :: BLKCNO( : )
C Number of cells in each block
      INTEGER, ALLOCATABLE, SAVE :: BLKLEN( : )

C Original cell number of ordered cell index
      INTEGER, ALLOCATABLE, SAVE :: NORDCELL( : )
C Estimated stiffness of each cell
      REAL( 8 ), ALLOCATABLE, SAVE :: ERRMX2( : )

      LOGICAL, SAVE :: CALL_DEG = .FALSE.    ! WTH: SWITCH for calling DEGRADE routine

      CONTAINS

         SUBROUTINE GRVARS_INIT( JDATE, JTIME )

            USE RXNS_DATA
            USE UTILIO_DEFN
            USE RXNS_FUNCTION

            IMPLICIT NONE

C.....Includes:
            INCLUDE SUBST_CONST          ! common constants

            INTEGER, INTENT( IN ) :: JDATE, JTIME
            INTEGER :: IOS, N
            CHARACTER( 16 ) :: PNAME = 'GRVARS_INIT'   ! Procedure name
            CHARACTER( 96 ) :: MSG = ' '

C-----------------------------------------------------------------------

            MXCOUNT1 = NUMB_MECH_SPC * MAXGL3 * 3
            MXCOUNT2 = NUMB_MECH_SPC * MAXGL3 * 3

            MXCOUNT3 = NRXNS * 4
            MXCOUNT4 = NRXNS * 5

            MXRR = 3 * 3

            MXRP = 3 * MXPRD

            ALLOCATE ( IJDECA( MXCOUNT2 ),
     &                 IJDECB( MXCOUNT2 ),
     &                 IKDECA( MXCOUNT2 ),
     &                 IKDECB( MXCOUNT2 ),
     &                 KJDECA( MXCOUNT2 ),
     &                 KJDECB( MXCOUNT2 ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = '*** Memory allocation failed for '
     &              // 'IJDECA, IJDECB, IKDECA, IKDECB, KJDECA,or KJDECB'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF
            IJDECA = 0   ! array assignment
            IJDECB = 0   ! array assignment
            IKDECA = 0   ! array assignment
            IKDECB = 0   ! array assignment
            KJDECA = 0   ! array assignment
            KJDECB = 0   ! array assignment

            ALLOCATE ( IDEC1LO ( NUMB_MECH_SPC, NCS2 ),
     &                 IDEC1HI ( NUMB_MECH_SPC, NCS2 ),
     &                 INEW2OLD( NUMB_MECH_SPC,  NCS ),
     &                 IOLD2NEW( NUMB_MECH_SPC,  NCS ),
     &                 JHIZ1   ( NUMB_MECH_SPC, NCS2 ),
     &                 JHIZ2   ( NUMB_MECH_SPC, NCS2 ),
     &                 KZHI0   ( NUMB_MECH_SPC, NCS2 ),
     &                 KZHI1   ( NUMB_MECH_SPC, NCS2 ),
     &                 KZILCH  ( NUMB_MECH_SPC, NCS2 ),
     &                 KZLO1   ( NUMB_MECH_SPC, NCS2 ),
     &                 KZLO2   ( NUMB_MECH_SPC, NCS2 ),
     &                 MZHI0   ( NUMB_MECH_SPC, NCS2 ),
     &                 MZHI1   ( NUMB_MECH_SPC, NCS2 ),
     &                 MZILCH  ( NUMB_MECH_SPC, NCS2 ),
     &                 MZLO1   ( NUMB_MECH_SPC, NCS2 ),
     &                 MZLO2   ( NUMB_MECH_SPC, NCS2 ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = '*** Memory allocation failed for'
     &              // 'IDEC1LO, IDEC1HI, INEW2OLD, IOLD2NEW, JHIZ1, JHIZ2, '
     &              // 'KZHI0, KZHI1, KZILCH, KZLO1, KZLO2, '
     &              // 'MZHI0, MZHI1, MZILCH, MZLO1, or MZLO2'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

            ALLOCATE ( JARRAYPT( NUMB_MECH_SPC, NUMB_MECH_SPC, NCS2 ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = '*** Memory allocation failed for JARRAYPT'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF
            JARRAYPT = 0   ! array assignment

            ALLOCATE ( BLKCONC( BLKSIZE,  NUMB_MECH_SPC ),
     &                 CNEW   ( BLKSIZE,  NUMB_MECH_SPC ),
     &                 EMBLK  ( BLKSIZE,  NUMB_MECH_SPC ),
     &                 GLOSS  ( BLKSIZE,  NUMB_MECH_SPC ),
     &                 VDIAG  ( BLKSIZE,  NUMB_MECH_SPC ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = '*** Memory allocation failed for '
     &             // 'BLKCONC, CNEW, EMBLK, GLOSS, or VDIAG'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

#ifdef sens
           ALLOCATE(  CINIT( BLKSIZE,NUMB_MECH_SPC ),
     &                CFINI( BLKSIZE,NUMB_MECH_SPC ),
     &                CAVEG( BLKSIZE,NUMB_MECH_SPC ), STAT = IOS )
           IF ( IOS .NE. 0 ) THEN
              MSG = 'ERROR allocating CINIT,CFINI,CAVEG'
              CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
           END IF
#endif

            ALLOCATE ( CCOL( MXCELLS ),
     &                 CROW( MXCELLS ),
     &                 CLEV( MXCELLS ),
     &                 BLKCNO( MXBLKS ),
     &                 BLKLEN( MXBLKS ),
     &                 NORDCELL( MXCELLS ),
     &                 ERRMX2( MXCELLS ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = '*** Memory allocation failed for'
     &              // ' CCOL, CROW, CLEV, BLKCNO, BLKLEN, NORDCELL, or ERRMX2'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

c..cell & solver data
           ALLOCATE( BLKTEMP( BLKSIZE ),
     &               BLKPRES( BLKSIZE ),
     &               BLKCH2O( BLKSIZE ),
     &               BLKDENS( BLKSIZE ),
     &               BLKSVOL( BLKSIZE ), 
     &               BLKSEAWATER( BLKSIZE ), STAT = IOS )
           IF ( IOS .NE. 0 ) THEN
              MSG = 'ERROR allocating BLKTEMP, BLKPRES, BLKCH2O, BLKDENS, '
     &            // 'BLKSVOL, BLKSEAWATER '
              CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
           END IF


           ALLOCATE( RJBLK( BLKSIZE,NPHOTAB ),
     &               BLKHET( BLKSIZE, NHETERO ), STAT = IOS )    
           IF ( IOS .NE. 0 ) THEN
                MSG = 'ERROR allocating RJBLK or BLKHET'
                CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
           END IF

          ALLOCATE( FORWARD_CONV( NUMB_MECH_SPC ),
     &              REVERSE_CONV( NUMB_MECH_SPC ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
             MSG = 'ERROR allocating FORWARD_CONV or REVERSE_CONV'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
          END IF

          IF( .NOT. MAP_CHEMISTRY_SPECIES() )THEN
               MSG = 'Detected above error(s) when mapping Chemistry species from CGRID species'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
          END IF

          DO N = 1, NUMB_MECH_SPC
             FORWARD_CONV( N ) = 1.0E-3 * MWAIR / REAL( SPECIES_MOLWT( N ) )
             REVERSE_CONV( N ) = REAL( 1.0 / FORWARD_CONV( N ), 8 )
          END DO

          ALLOCATE( NKUSERAT( NRXNS,NCS2 ),
     &              NDERIVL ( NRXNS,NCS2 ),
     &              NDERIVP ( NRXNS,NCS2 ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
               MSG = 'ERROR allocating NKUSERAT, NDERIVL or NDERIVP'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
          END IF

          ALLOCATE(    RK( BLKSIZE,NRXNS ),
     &              RXRAT( BLKSIZE,NRXNS ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
             MSG = 'ERROR allocating RK  or RXRAT'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
          END IF
         
          ALLOCATE( JARRL( MXRR,NRXNS,NCS2 ),
     &              JARRP( MXRP,NRXNS,NCS2 ),
     &              JLIAL( MXRR,NRXNS,NCS2 ),
     &              JPIAL( MXRP,NRXNS,NCS2 ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
             MSG = 'ERROR allocating JARRL, JARRP, JLIAL, or JPIAL'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
          END IF

          ALLOCATE( IRM2( MXRCT+MXPRD,NRXNS,NCS2 ),
     &              ICOEFF( MXRP,NRXNS,NCS2 ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
               MSG = 'ERROR allocating IRM2 or ICOEFF'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
          END IF

         END SUBROUTINE GRVARS_INIT
       SUBROUTINE OPTIMAL_ATOL_PPM( YP, NUMCELLS, YP_ABST )
       
          USE UTILIO_DEFN
          USE RXNS_DATA
          
          
          IMPLICIT NONE
! arguments:
          REAL( 8 ), INTENT( IN  ) :: YP( :,: )
          INTEGER,   INTENT( IN  ) :: NUMCELLS       
          REAL( 8 ), INTENT( INOUT  ) :: YP_ABST( : )
! local:          
          REAL( 8 ), PARAMETER :: MASS_TO_NUMBER = 2.079267D19             !  coefficient to air mass (kg/m3) to number density (molec/cm3)
          REAL( 8 ), PARAMETER :: COEFF1         = 1.0D-6 * MASS_TO_NUMBER !  coefficient to convert ppm to molec/cm3
          REAL( 8 ), PARAMETER :: COEFF2         = 1.0D0  / COEFF1         !  coefficient to convert molec/cm3 to ppm 
          REAL( 8 ), PARAMETER :: COEFF1_STP     = 2.54103D13
          
          INTEGER, PARAMETER   :: NBINS = 5
          REAL( 8 ), SAVE      :: ATOL_LEVEL( NBINS + 1 )
          REAL( 8 ), SAVE      :: ATOL_FLOOR
          
          INTEGER, ALLOCATABLE, SAVE :: SPECIES_CUTOFF( : )
          INTEGER, ALLOCATABLE, SAVE :: BIN_COUNTS( :,: ) 
          INTEGER, ALLOCATABLE, SAVE :: CUM_COUNTS( :,: ) 
          
          LOGICAL, SAVE :: FIRST_CALL = .TRUE.
          
          INTEGER   :: I       
          INTEGER   :: JSPC       
          INTEGER   :: NCELL
          INTEGER   :: NBIN
          
          REAL( 8 ) :: ABHI
          REAL( 8 ) :: ABLO
          
          CHARACTER( 82 ) :: XMSG

          IF ( FIRST_CALL ) THEN 
! ensure GEAR_MAX_ATOL is greater than GEAR_MAX_ATOL
             IF( GEAR_MAX_ATOL .LE. GEAR_MIN_ATOL )THEN
                 XMSG = "GEAR_MAX_ATOL must be greater than GEAR_MIN_ATOL"
                 CALL M3EXIT( 'OPTIMAL_ATOL_PPM', 0, 0, XMSG, XSTAT2 )
             END IF

! set absolute tolerance bins
             ABHI           = LOG10( REAL( GEAR_MAX_ATOL,8 ) ) 
             ABLO           = LOG10( REAL( GEAR_MIN_ATOL,8 ) ) 
!            
             ATOL_LEVEL( 1 ) = 10.0D0**ABHI
             ATOL_LEVEL( NBINS + 1 ) = 10.0D0**ABLO
             ATOL_FLOOR              = REAL( GEAR_CONC_FLOOR,8 ) 
! ensure GEAR_MAX_ATOL is greater than GEAR_MAX_ATOL
             IF( GEAR_MIN_ATOL .LE. GEAR_CONC_FLOOR )THEN
                 XMSG = "GEAR_MIN_ATOL must be greater than GEAR_CONC_FLOOR"
                 CALL M3EXIT( 'OPTIMAL_ATOL_PPM', 0, 0, XMSG, XSTAT2 )
             END IF

             DO I = 2, NBINS
                ATOL_LEVEL(I)  = 10.0D0**(ABLO + (ABHI - ABLO) * REAL( (NBINS-I+1),8 ) / REAL( NBINS,8 ) )
             END DO
             ALLOCATE ( BIN_COUNTS( BLKSIZE,NBINS+1 ),
     &                  CUM_COUNTS( BLKSIZE,NBINS+1 ),
     &                      SPECIES_CUTOFF( BLKSIZE ) )
             FIRST_CALL = .FALSE. 
             SPECIES_CUTOFF = INT( 0.4E0 * REAL( NUMB_MECH_SPC ) )        
          END IF
 
! *********************************************************************
!                determine initial absolute error tolerance 
! *********************************************************************
! iabovk  = number of species whose concentrations are larger than yabst
! BIN_COUNTS    = counts number of concentrations above ATOL_LEVEL(i), i = 1..  
! yabst   = absolute error tolerance (ppm) 
! abtol   = pre-defined absolute error tolerances 
!
          BIN_COUNTS = 0
          CUM_COUNTS = 0
!         
          DO  JSPC  = 1,  NUMB_MECH_SPC
              DO NCELL = 1, NUMCELLS
                 IF (YP(NCELL,JSPC).GE.ATOL_LEVEL(1)) THEN

                    BIN_COUNTS(NCELL,1) = BIN_COUNTS(NCELL,1) + 1

                 ELSE IF (YP(NCELL,JSPC).GE.ATOL_LEVEL(2)) THEN

                    BIN_COUNTS(NCELL,2) = BIN_COUNTS(NCELL,2) + 1

                 ELSE IF (YP(NCELL,JSPC).GE.ATOL_LEVEL(3)) THEN

                    BIN_COUNTS(NCELL,3) = BIN_COUNTS(NCELL,3) + 1

                 ELSE IF (YP(NCELL,JSPC).GE.ATOL_LEVEL(4)) THEN

                    BIN_COUNTS(NCELL,4) = BIN_COUNTS(NCELL,4) + 1

                 ELSE IF (YP(NCELL,JSPC).GE.ATOL_LEVEL(5)) THEN

                    BIN_COUNTS(NCELL,5) = BIN_COUNTS(NCELL,5) + 1

                 ELSE IF (YP(NCELL,JSPC).GE.ATOL_FLOOR)THEN

                    BIN_COUNTS(NCELL,6) = BIN_COUNTS(NCELL,6) + 1

                 ENDIF
              END DO   ! JSPC
          END DO  ! NCELL
          
          DO NCELL = 1,  NUMCELLS

!
             CUM_COUNTS(NCELL,1) = BIN_COUNTS(NCELL,1)

             CUM_COUNTS(NCELL,2) = BIN_COUNTS(NCELL,2)
     &                           + CUM_COUNTS(NCELL,1)

             CUM_COUNTS(NCELL,3) = BIN_COUNTS(NCELL,3)
     &                           + CUM_COUNTS(NCELL,2)

             CUM_COUNTS(NCELL,4) = BIN_COUNTS(NCELL,4)
     &                           + CUM_COUNTS(NCELL,3)

             CUM_COUNTS(NCELL,5) = BIN_COUNTS(NCELL,5)
     &                           + CUM_COUNTS(NCELL,4)

             CUM_COUNTS(NCELL,6) = BIN_COUNTS(NCELL,6)
     &                           + CUM_COUNTS(NCELL,5)

             SPECIES_CUTOFF( NCELL ) = INT( 0.4E0 * REAL(CUM_COUNTS(NCELL,6)) )
          END DO


!          YP_ABST = ATOL_LEVEL(6)
          DO NCELL = 1,  NUMCELLS
             IF (CUM_COUNTS(NCELL,1).GT.SPECIES_CUTOFF(NCELL)) THEN
                YP_ABST(NCELL) = ATOL_LEVEL(1)
                NBIN = 1
             ELSE IF (CUM_COUNTS(NCELL,2).GT.SPECIES_CUTOFF(NCELL) ) THEN
                YP_ABST(NCELL) = ATOL_LEVEL(2)
                NBIN = 2
             ELSE IF (CUM_COUNTS(NCELL,3).GT.SPECIES_CUTOFF(NCELL) ) THEN
                YP_ABST(NCELL) = ATOL_LEVEL(3)
                NBIN = 3
             ELSE IF (CUM_COUNTS(NCELL,4).GT.SPECIES_CUTOFF(NCELL) ) THEN
                YP_ABST(NCELL) = ATOL_LEVEL(4)
                NBIN = 4
             ELSE IF (CUM_COUNTS(NCELL,5).GT.SPECIES_CUTOFF(NCELL) ) THEN
                YP_ABST(NCELL) = ATOL_LEVEL(5)
                NBIN = 5
             ELSE
                YP_ABST(NCELL) = ATOL_LEVEL(6)
                NBIN = 6
             ENDIF
          END DO ! NCELL
          
          RETURN
          
        END SUBROUTINE OPTIMAL_ATOL_PPM

      END MODULE GRVARS
