
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

       SUBROUTINE CHEM( CONC, JDATE, JTIME, TSTEP )

C**********************************************************************
C
C  Function: To control gas phase chemistry calculations performed by
C            the vectorized Rosenbrock solver
C
C  Preconditions: None
C
C  Key Subroutines/Functions Called: RBINIT
C                                    RBSPARSE
C                                    CALCKS
C                                    RBSOLVER
C                                    FIND_DEGRADED
C                                    INIT_DEGRADE
C                                    FINAL_DEGRADE
C
C  Revision History: Prototype created by Jerry Gipson, August, 2004
C                    Based on the solver described by Sandu et al
C                    ( Atm. Env., Vol. 31, No. 20, 1997 ) and included
C                    in the Kinetic PreProcessor ( see for example 
C                    Sandu et al., At, Env., Vol. 37, 5097-5114, 
C                    2003). This code also incorporates efficiency
C                    concepts originally developed by M. Jacobson
C                    for SMVGEAR (Atm. Env., Vol 28, No 2, 1994).
C                    Adapted from Subroutine CHEM in CMAQ SMVGEAR
C
C                    31 Jan 05 J.Young: dyn alloc - establish both horizontal
C                    & vertical domain specifications in one module (GRID_CONF)
C                    29 Jul 05     WTH: Added IF blocks that call degrade 
C                                       routines if MECHNAME contains 'TX' 
C                                       substring.
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    29 Mar 11 S.Roselle: Replaced I/O API include files
C                               with UTILIO_DEFN
C                    31 Aug 11 B.Hutzell revised method that determines calling
C                              degrade routine
C                    29 Sep 11 D.Wong: incorporated twoway model implementation
C                    18 Jan 13 B.Hutzell: 1) added using heteorogeneous rate constants
C                    by using function in AEROSOL_CHEMISTRY module,
C                    15 Jul 14 B.Hutzell: 1) replaced mechanism include files with 
C                    RXNS_DATA module, 2) replaced call to CALCLK with CALC_RCONST in 
C                    RXNS_FUNCTION module, 3) enabled reactions between all species 
C                    types by using unit conversion factors and 4) revised usage for 
C                    INIT_DEGRADE and FINAL_DEGRADE routines
C                    02 Dec 14 B.Hutzell 1) added terrestrial data to conduct surface
C                    dependent reactions and 2) modified the call CALC_RCONST routine
C                    16 Sep 16 J.Young: update for inline procan (IRR)
C                    21 Mar 22 G . Sarwar: updated simple halogen chemistry 
C**********************************************************************

      USE RXNS_DATA
      USE RXNS_FUNCTION
      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE RBDATA                ! ROS3 solver data
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE PHOT_MOD, Only: INIT_PHOT_SHARED, RJ     ! photolysis rate, in-line module
      USE AEROSOL_CHEMISTRY
!     USE DEGRADE_SETUP_TOX, ONLY : NCELLS, N_REACT, RXTANT_MAP, 
      USE DEGRADE_ROUTINES,  ONLY : NCELLS, N_REACT, RXTANT_MAP, 
     &                              ICELL_WRITE, WRITE_CELL, WRITE_BLOCK,
     &                              FIND_DEGRADED, INIT_DEGRADE_BLK, FINAL_DEGRADE_BLK
      USE PA_DEFN, Only: LIRR   ! Process Anaylsis control and data variable
      USE PA_IRR_CLT
      USE CENTRALIZED_IO_MODULE, ONLY : INTERPOLATE_VAR, OCEAN, SZONE
#ifdef sens
      USE DDM3D_CHEM
      Use DDM3D_DEFN, Only: DATENUM, STARTDATE, IPT, IDATE, HIGH, NP, NPMAX, CKTIME
#endif
      
      IMPLICIT NONE 

C..Includes:

      INCLUDE SUBST_FILES_ID    ! CMAQ files
      INCLUDE SUBST_CONST       ! CMAQ constants
 
C..Arguments:

      REAL, POINTER :: CONC( :,:,:,: )  ! Concentrations

      INTEGER JDATE                     ! Current date (YYYYDDD)
      INTEGER JTIME                     ! Current time (HHMMSS)
      INTEGER TSTEP( 3 )                ! Time step vector (HHMMSS)

C..Parameters:

      INTEGER, PARAMETER :: IZERO = 0                ! Integer zero

      REAL,    PARAMETER :: CONCMIN   = 1.0E-30         ! Minimum conc
      REAL,    PARAMETER :: CONCOFM   = 1.0E+06         ! conc. of M = 1E+06 ppm
      REAL,    PARAMETER :: PA2ATM    = 1.0 / STDATMPA  ! Pascal to atm conv fac
      REAL,    PARAMETER :: MAOMV     = MWAIR / MWWAT   ! Mol Wt of air over Mol Wt of water
      REAL,    PARAMETER :: QV_TO_PPM = CONCOFM * MAOMV ! factor to convert water wapor into ppm
C..External Functions:

C..Local Variables:

      LOGICAL, SAVE :: LFIRST    = .TRUE. ! Flag for first call to this subroutine
      LOGICAL, SAVE :: FIRSTCALL = .TRUE. ! Another Flag for first call
      LOGICAL, SAVE :: LIRRBLK         ! Flag for IRR to be done for block

      INTEGER, SAVE :: NOXYZ           ! Total number of grid cells

      REAL,    SAVE :: AIRFC           ! Factor to convert gms air to ppm

      REAL( 8 )     :: CHEMSTEP   ! Chem integration interval (min)
      REAL( 8 )     :: VALLOW     ! Value holder for sort routine

      CHARACTER(  16 ) :: PNAME = 'RBDRIVER' ! Procedure name
      CHARACTER(  16 ) :: VNAME              ! Name of I/O API data variable
      CHARACTER( 144 ) :: MSG                ! Message text
     
      INTEGER C, R, L, S      ! Loop indices
      INTEGER ALLOCSTAT       ! Allocate status code
      INTEGER OFFSET          ! Starting cell number of a block
      INTEGER NCSP            ! Mech no: 1=gas/day 2=gas/night
      INTEGER BLK             ! Loop index for block of cells
      INTEGER CELLNUM         ! Cell number 
      INTEGER COL             ! Column index
      INTEGER IPAR            ! Pointer for cell sort routine
      INTEGER IRVAL           ! Pointer for cell sort routine
      INTEGER IRXN            ! Reaction number
      INTEGER ISP             ! Species index
      INTEGER ISPOLD          ! Species number in original order
      INTEGER ISPNEW          ! Species number in new sorted order 
      INTEGER ITMSTEP         ! Chemistry integration interval (sec)   
      INTEGER JPAR            ! Pointer for cell sort routine
      INTEGER JREORD          ! Index holder for sort routine
      INTEGER LEV             ! Layer index
      INTEGER LVAL            ! Pointer for cell sort routine
      INTEGER MIDDATE         ! Date at time step midpoint
      INTEGER MIDTIME         ! Time at time step midpoint
      INTEGER NCELL           ! Index for number of cells
      INTEGER NIRRCLS         ! No. of cells in block for IRR
      INTEGER NPH             ! Index for number of phot. rxns in PHOT
      INTEGER NRX             ! Index for number of reactions
      INTEGER ROW             ! Row index
      INTEGER SPC             ! Species loop index
      INTEGER VAR             ! Variable number on I/O API file
      
      INTEGER NUMB_CELLS

      INTEGER, ALLOCATABLE, SAVE :: IRRCELL( : )   ! Cell No. of an IRR cell
       
      REAL, ALLOCATABLE, SAVE :: SEAICE ( :, : )          ! fractional seaice cover, [-] 

!      REAL, ALLOCATABLE, SAVE :: DENSA_J( :, :, : )      ! Cell density (Kg/m**3)
      REAL, ALLOCATABLE, SAVE :: DENS   ( :, :, : )      ! Cell density (Kg/m**3)
      REAL, ALLOCATABLE, SAVE :: PRES   ( :, :, : )      ! Cell pressure (Pa)
      REAL, ALLOCATABLE, SAVE :: QV     ( :, :, : )      ! Cell water vapor (Kg/Kg air)
      REAL, ALLOCATABLE, SAVE :: TA     ( :, :, : )      ! Cell temperature (K)

      REAL, ALLOCATABLE, SAVE :: SEAWATER_ZONE   ( :,: )          ! fractional area of OPEN+SURF  


      REAL( 8 ), ALLOCATABLE, SAVE :: BLKHET( :, : )
#ifdef rbstats

      CHARACTER( 16 ), SAVE              :: CTM_RBSTATS_1 = 'CTM_RBSTATS_1' 
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: VSTATS( : )        !

      INTEGER, SAVE                      :: WSTEP = 0     
      INTEGER, ALLOCATABLE, SAVE         :: STAT_SUM( :,:,:,: )
      INTEGER  EDATE, ETIME

      REAL     ALLOCATABLE, SAVE         :: STATOUT( :, :, : )

#endif

      INTERFACE
         SUBROUTINE RBSOLVER ( JDATE, JTIME, CHEMSTEP, NCSP,
     &                         LIRRFLAG, NIRRCLS, IRRCELL )
            INTEGER,   INTENT( IN ) :: JDATE, JTIME
            REAL( 8 ), INTENT( IN ) :: CHEMSTEP
            INTEGER,   INTENT( IN ) :: NCSP
            LOGICAL,   INTENT( IN ) :: LIRRFLAG
            INTEGER,   INTENT( INOUT ) :: NIRRCLS
            INTEGER,   INTENT( IN ) :: IRRCELL( : )
         END SUBROUTINE RBSOLVER
         SUBROUTINE HETCHEM_UPDATE_AERO( CGRID )
           REAL, POINTER :: CGRID( :,:,:,: )    !  species concentration in cell
         END SUBROUTINE HETCHEM_UPDATE_AERO      
      END INTERFACE

C**********************************************************************


#ifdef isam
      MSG = 'ERROR: Rosenbrock Chemistry Solver does not perform source apportionment.'
      WRITE(LOGDEV,'(A)')TRIM( MSG )
      MSG = 'Must use the EBI solver for the chemical mechanism'
      CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
#endif


      IF ( NUMB_MECH_SPC .EQ. 0 ) THEN
         CALL M3MESG( '*** WARNING: Gas-Chemistry processing bypassed!' )
         RETURN
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for Gear solver and 
c  set-up to do emissions here if that option is invoked
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN
         LFIRST   = .FALSE.
         ROS3_LOG = LOGDEV
!        GASLOG   = LOGDEV

         IF ( .NOT. CELLVAR_ALLOC() ) THEN
            MSG = 'Failure allocating variables dependent on horizontal extents'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         CALL RBINIT
         CALL RBSPARSE( )
         CALL RESET_SPECIES_POINTERS( IOLD2NEW )

         NOXYZ = NCOLS * NROWS * NLAYS

C...Initialize and report data

         WRITE( LOGDEV, 92020 ) NOXYZ, BLKSIZE, NBLKS, BLKLEN( 1 ), BLKLEN( NBLKS )

         WRITE( LOGDEV, 92040 ) GLBL_RTOL, GLBL_ATOL

C.. Get fractional seawater coverage from the OCEAN file.
         ALLOCATE( SEAWATER_ZONE( NCOLS, NROWS ) )
         
         DO ROW = 1, NROWS
            DO COL = 1, NCOLS                
                 SEAWATER_ZONE( COL,ROW ) =  OCEAN( COL,ROW ) + SZONE( COL,ROW )                  
             END DO
         END DO

#ifdef mpas
         STARTCOLCO = 1
         ENDCOLCO = NCOLS
         STARTROWCO = 1
         ENDROWCO = 1
#else
         STARTCOLCO = COLSX_PE( 1, MYPE + 1 )
         ENDCOLCO   = COLSX_PE( 2, MYPE + 1 )
         STARTROWCO = ROWSX_PE( 1, MYPE + 1 )
         ENDROWCO   = ROWSX_PE( 2, MYPE + 1 )
#endif

         ALLOCATE( DENS( NCOLS, NROWS, NLAYS ), PRES( NCOLS, NROWS, NLAYS ),
     &             QV  ( NCOLS, NROWS, NLAYS ), TA  ( NCOLS, NROWS, NLAYS ),
     &             SEAICE( NCOLS, NROWS ) )

         ALLOCATE( IRRCELL( BLKSIZE ) )
         IRRCELL = 0

c..Open file for solver stats if requested
#ifdef rbstats
         ALLOCATE( VSTATS( 3 ) )
         VSTATS( 1 ) = 'N_STRT_FAILS'
         VSTATS( 2 ) = 'N_FAILS'
         VSTATS( 3 ) = 'N_STEPS'

         IF ( MYPE .EQ. 0 ) THEN

            IF ( .NOT. OPEN3( CTM_CONC_1, FSREAD3, PNAME ) ) THEN
               MSG = 'Could not open ' // CTM_CONC_1 // ' file for readonly'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

            IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
               MSG = 'Could not get description of concentration file '
     &             // CTM_CONC_1
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

            EDATE = JDATE
            ETIME = JTIME
            CALL NEXTIME( EDATE, ETIME, TSTEP( 1 ) )

            SDATE3D = EDATE
            STIME3D = ETIME
            NVARS3D = 3
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = NLAYS
            VNAME3D( 1 ) = 'N_STRT_FAILS'
            VNAME3D( 2 ) = 'N_FAILS'
            VNAME3D( 3 ) = 'N_STEPS'
            VDESC3D( 1 ) = 'Number of fails at start'
            VDESC3D( 2 ) = 'Number of step fails'
            VDESC3D( 3 ) = 'Number of steps'
            UNITS3D( 1 ) = ''
            UNITS3D( 2 ) = ''
            UNITS3D( 3 ) = ''
            VTYPE3D( 1 ) = M3REAL
            VTYPE3D( 2 ) = M3REAL
            VTYPE3D( 3 ) = M3REAL
            IF ( .NOT. OPEN3( CTM_RBSTATS_1, FSNEW3, PNAME ) ) THEN
               MSG = 'Could not create '// TRIM( CTM_RBSTATS_1 ) // ' file'
               CALL M3EXIT( PNAME, SDATE3D, STIME3D, MSG, XSTAT2 )
            END IF

         END IF

         ALLOCATE( STAT_SUM( NCOLS, NROWS, NLAYS, 3 ) )
         ALLOCATE(  STATOUT( NCOLS, NROWS, NLAYS ) )


         STAT_SUM = 0

#endif

C..Initialize shared photolysis data
         CALL INIT_PHOT_SHARED()

         ALLOCATE( BLKHET( BLKSIZE, NHETERO ) )

C Determine whether DEGRADE rountines are needed.

         CALL FIND_DEGRADED( JDATE, JTIME, CALL_DEG )
         IF( CALL_DEG ) THEN
            WRITE( LOGDEV, * ) 'DEGRADE ROUTINES USED'
            WRITE( LOGDEV, * ) 'Mechanism contains degraded species'
#ifdef verbose_gas         
            ALLOCATE( WRITE_CELLL( BLKSIZE ) )
         ELSE
            WRITE( LOGDEV, * ) 'DEGRADE ROUTINES not USED'
            WRITE( LOGDEV, * ) 'Mechanism contains NO degraded species'
#endif            
         ENDIF

C set up degradation array


         ALLOCATE( Y_DEGRADE( BLKSIZE, NSPCSD ) )

#ifdef sens
         CALL INIT_DDM3D_CHEM()

C For higher order sensitivities
         IF ( HIGH ) THEN
            DO IRXN = 1, NRXNS
               IF( NREACT( IRXN ) .LE. 1 ) THEN
                  ORDER1( IRXN ) = .TRUE.
               ELSE
                  ORDER1( IRXN ) = .FALSE.
               END IF
            END DO
         END IF
#endif
      END IF      ! First call

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Start of integration driver after first call
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NIRRCLS = 0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Set date and time to center of time step, get necessary physical 
C  data, and get photolysis rates
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MIDDATE = JDATE
      MIDTIME = JTIME
      ITMSTEP = TIME2SEC( TSTEP( 2 ) )
      CHEMSTEP = REAL( ITMSTEP, 8 ) / 60.0D0
      CALL NEXTIME( MIDDATE, MIDTIME, SEC2TIME( ITMSTEP / 2 ) )

C.. Get fractional seaice coverage from the METCRO2D file.

      CALL INTERPOLATE_VAR ('SEAICE', MIDDATE, MIDTIME, SEAICE)

C.. Get ambient temperature in K

      CALL INTERPOLATE_VAR ('TA', MIDDATE, MIDTIME, TA)

C.. Get specific humidity in Kg H2O / Kg air
      CALL INTERPOLATE_VAR ('QV', MIDDATE, MIDTIME, QV)

! Get ambient MASS DENSITY in Kg/m^3
      CALL INTERPOLATE_VAR ('DENS', MIDDATE, MIDTIME, DENS)

C.. Get pressure in Pascals
      CALL INTERPOLATE_VAR ('PRES', MIDDATE, MIDTIME, PRES)

C.. Get Heterogeneous Rates using Aerosol Surface Area. Also Store
C   a snapshot of the aerosol surface area so that it can be
C   appropriately updated after the solver finds a solution.

      CALL HETCHEM_RATES( TA, PRES, QV, CONC, DENS )

#ifdef sens
C Set the date and hour counters used in sensitivity calls
      DATENUM = 1 + JDATE - STARTDATE 

C For reaction rate sensitivities
      DO NP = 1, NPMAX
         IF ( IPT( NP ) .EQ. 5 ) THEN
            CALL CKTIME( JDATE,JTIME,NP,RXNFLAG(NP) ) ! Rxnflag set to true if ipt=5 and time, date within bounds
            IF ( IDATE( NP, DATENUM ) .NE. 1 ) RXNFLAG( NP ) = .FALSE.
         ELSE
            RXNFLAG( NP ) = .FALSE.
         END IF
      END DO
#endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set flag for reordering of cells and put cells in sequential  
c  order initially
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LORDERING = .TRUE.
      IF ( .NOT. LREORDER .OR. NBLKS .EQ. 1 ) LORDERING = .FALSE.
      DO NCELL = 1, NOXYZ
         NORDCELL( NCELL ) = NCELL
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Top of loop over blocks. This loop will be done once if
C  no reordering, twice if reordering is required
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
100   CONTINUE

      ERRMX2 = 0.0D0

      DO 500 BLK = 1, NBLKS
         BLKID = BLK
         NUMCELLS = BLKLEN( BLK )
         OFFSET = BLKCNO( BLK )
         IF ( .NOT. LORDERING .AND. LIRR ) THEN
             LIRRBLK = .FALSE.
             CALL PA_IRR_CKBLK ( NUMCELLS, LIRRBLK, OFFSET,
     &                           CCOL, CROW, CLEV, NORDCELL, NIRRCLS,
     &                           IRRCELL )
         END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell physical data in the block arrays, converting
C  pressure to atmospheres, water vapor to ppm, emissions to ppm/min,
C  assigning seawater values  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
            BLKTEMP( NCELL ) = REAL( TA( COL,ROW,LEV ), 8 )
            BLKDENS( NCELL ) = REAL( DENS( COL,ROW,LEV ), 8 )
            BLKSVOL( NCELL ) = 1.0 /  DENS( COL,ROW,LEV )
            BLKPRES( NCELL ) = REAL( PA2ATM * PRES( COL, ROW, LEV ), 8 )
            BLKCH2O( NCELL ) = REAL( MAX( QV_TO_PPM * QV( COL,ROW,LEV ), 0.0 ), 8)
            BLKSEAWATER (NCELL)  = MAX ( 0.0, ( SEAWATER_ZONE( COL,ROW ) - SEAICE (COL,ROW) ) )            
         END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell concentrations in the block arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO ISP = 1, ISCHANG( NCS )
            SPC    = CGRID_INDEX( ISP )
!            SPC    = ISP 
            ISPNEW = IOLD2NEW( ISP,NCS )
            DO NCELL = 1, NUMCELLS
               CELLNUM = NORDCELL( OFFSET + NCELL )
               COL = CCOL( CELLNUM )
               ROW = CROW( CELLNUM )
               LEV = CLEV( CELLNUM )
               IF( CONVERT_CONC( ISP ) )THEN 
                   Y( NCELL,ISPNEW ) = REAL( MAX( FORWARD_CONV( ISP ) * BLKSVOL( NCELL )
     &                               *       CONC( COL,ROW,LEV,SPC ), CONCMIN), 8 )
               ELSE
                   Y( NCELL,ISPNEW ) = REAL( MAX( CONC( COL,ROW,LEV,SPC ), CONCMIN), 8 )
               END IF
            END DO                 
         END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   Get photolytic, heteorogeneous and thermal rate constants & call solver
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LSUNLIGHT = .FALSE.

         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )   ! wrong order
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
            DO NPH = 1, NHETERO
               BLKHET( NCELL, NPH ) =  KHETERO( NPH, COL, ROW, LEV )
            END DO
            DO NPH = 1, NPHOTAB
               RJBLK( NCELL,NPH ) = REAL( RJ( COL,ROW,LEV,NPH ), 8 )
               IF ( RJBLK( NCELL, NPH ) .GT. 0.0D0 ) LSUNLIGHT = .TRUE.
            END DO                         
         END DO
         
         CALL CALC_RCONST( BLKTEMP, BLKPRES, BLKCH2O, RJBLK, BLKHET, LSUNLIGHT, BLKSEAWATER, RKI, NUMCELLS )

         IF ( LSUNLIGHT ) THEN
            NCSP = NCS
         ELSE
            NCSP = NCS + 1
         END IF

C  update cell concentrations for degradation routines

         IF ( CALL_DEG ) THEN

            Y_DEGRADE = 0.0
            DO ISP = 1, NSPCSD
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  COL = CCOL( CELLNUM )
                  ROW = CROW( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  Y_DEGRADE( NCELL,ISP ) = REAL(MAX( CONC( COL,ROW,LEV,ISP ), CONCMIN), 8 )
               END DO
            END DO

! Use FPP flag to check results from species degrade routines
#ifdef verbose_gas
            WRITE_BLOCK = .FALSE.
            ICELL_WRITE = 1
            DO NCELL = 1, NUMCELLS
                CELLNUM = NORDCELL( OFFSET + NCELL )
                COL = CCOL( CELLNUM )
                ROW = CROW( CELLNUM )
                LEV = CLEV( CELLNUM )
                IF( LEV .EQ. 1 .AND. ROW .EQ. 1 .AND. COL .EQ. 1 )THEN
                    WRITE_BLOCK = .TRUE.
                    ICELL_WRITE = NCELL
                    WRITE_CELL( NCELL ) = .TRUE.
                ELSE
                    WRITE_CELL( NCELL ) = .FALSE.
                END IF
            END DO
#endif

C..initialize degradation routines
            NCELLS = NUMCELLS 
            CALL INIT_DEGRADE_BLK( Y_DEGRADE, BLKTEMP, BLKDENS, BLKPRES, BLKCH2O, RJBLK, 
     &                         JDATE, JTIME )

         END IF

#ifdef rbstats

         NSTEPS = 0
         NFAILS = 0
         N_BAD_STARTS = 0

#endif

         CALL RBSOLVER( JDATE, JTIME, CHEMSTEP, NCSP,
     &                  LIRRBLK, NIRRCLS, IRRCELL ) 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  If not ordering cells, save performance statistics and
C  store updated concentrations.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( .NOT. LORDERING ) THEN

C..Update concentrations
            DO ISP = 1, ISCHANG( NCS )
               ISPOLD  = INEW2OLD( ISP,NCS )
               SPC     = CGRID_INDEX( ISPOLD )
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  COL = CCOL( CELLNUM )
                  ROW = CROW( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  IF( CONVERT_CONC( ISPOLD ) )THEN 
                     CONC( COL,ROW,LEV,SPC ) = REAL( REVERSE_CONV( ISPOLD ) 
     &                                       *       BLKDENS( NCELL ) * Y( NCELL,ISP ), 4)
                  ELSE
                     CONC( COL,ROW,LEV,SPC ) = REAL( Y( NCELL,ISP ), 4)
                  END IF
              END DO
            END DO




            IF ( CALL_DEG ) THEN
C  Update CGRID based on the degradation routines
               CALL FINAL_DEGRADE_BLK( Y_DEGRADE )
               UPDATE_DEGRADED: DO ISP = 1, N_REACT
                  S = RXTANT_MAP( ISP )
                  IF( S .LE. 0 )CYCLE UPDATE_DEGRADED                  
                  DO SPC = 1, NUMB_MECH_SPC
                     IF( S .EQ. CGRID_INDEX( SPC ) )CYCLE UPDATE_DEGRADED
                  END DO
                  DO NCELL = 1, NUMCELLS
                     CELLNUM = NORDCELL( OFFSET + NCELL )
                     COL = CCOL( CELLNUM )
                     ROW = CROW( CELLNUM )
                     LEV = CLEV( CELLNUM )
                     CONC( COL,ROW,LEV,S ) = REAL( Y_DEGRADE( NCELL,S ), 4)
                  END DO
               END DO UPDATE_DEGRADED
            END IF            !WTH

#ifdef sens

            NUMB_CELLS = NUMCELLS
            
            DO NCELL = 1, NUMB_CELLS
               DO IRXN = 1, NRXNS
                  SRK( IRXN ) = RKI( NCELL,IRXN )
                  IF ( HIGH ) THEN
                     IF ( NREACT( IRXN ) .LE. 1 ) THEN
                        SRK2( IRXN ) = 0.0
                     ELSE IF ( NREACT( IRXN ) .EQ. 2 ) THEN
                        SRK2( IRXN ) = REAL( RKI( NCELL,IRXN ), 4 )
                     ELSE IF ( NREACT( IRXN ) .EQ. 3 ) THEN
                        SRK2( IRXN ) = REAL( RKI( NCELL,IRXN ),4 )
                     ELSE
                        SRK2( IRXN ) = 0.0
                     END IF
                  END IF
              END DO

              DO ISP = 1,NUMB_MECH_SPC
!                  ISPOLD = INEW2OLD( ISP,NCS )
!                  YCDDM( ISPOLD ) = YAVE(NCELL,ISP)
                  YCDDM( ISP ) = YAVE(NCELL,ISP)
               END DO
               CELLNUM = NORDCELL( OFFSET + NCELL )
               COL = CCOL( CELLNUM )
               ROW = CROW( CELLNUM )
               LEV = CLEV( CELLNUM )

!                CALL SOLVE_DDM3D_CHEM( COL,ROW,LEV,CHEMSTEP )
               CALL SOLVE_DDM3D_CHEM( COL,ROW,LEV,CHEMSTEP,IOLD2NEW,INEW2OLD )
            END DO
            NUMCELLS = NUMB_CELLS
#endif

#ifdef rbstats

            DO NCELL = 1, NUMCELLS
               CELLNUM = NORDCELL( OFFSET + NCELL )
               COL = CCOL( CELLNUM )
               ROW = CROW( CELLNUM )
               LEV = CLEV( CELLNUM )
               STAT_SUM( COL,ROW,LEV,1 ) = STAT_SUM( COL,ROW,LEV,1 )
     &                                   + N_BAD_STARTS
               STAT_SUM( COL,ROW,LEV,2 ) = STAT_SUM( COL,ROW,LEV,2 )
     &                                   + NFAILS
               STAT_SUM( COL,ROW,LEV,3 ) = STAT_SUM( COL,ROW,LEV,3 )
     &                                   + NSTEPS
            END DO

#endif
                      
            IF ( LIRRBLK ) CALL PA_IRR_BLKENDC ( OFFSET, CCOL, CROW, CLEV,
     &                                           NORDCELL, NIRRCLS, IRRCELL )

         END IF

500   CONTINUE

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  End of block loop; reorder cells if necessary and go back do the  
C  block loop again.  Taken from Jacobson 1994.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LORDERING ) THEN
         LORDERING = .FALSE.     
         LVAL = NOXYZ / 2 + 1
         IRVAL = NOXYZ
600      CONTINUE
         IF ( LVAL .GT. 1 ) THEN
            LVAL = LVAL - 1
            VALLOW = ERRMX2( LVAL )
            JREORD = NORDCELL( LVAL )
         ELSE
            VALLOW = ERRMX2( IRVAL )
            JREORD = NORDCELL( IRVAL )
            ERRMX2( IRVAL ) = ERRMX2( 1 )
            NORDCELL( IRVAL ) = NORDCELL( 1 )
            IRVAL = IRVAL - 1
            IF ( IRVAL.EQ.1 ) THEN
               ERRMX2( IRVAL ) = VALLOW
               NORDCELL( IRVAL ) = JREORD
               GO TO 100
            END IF
         END IF
         IPAR = LVAL
         JPAR = LVAL + LVAL
650      CONTINUE
         IF ( JPAR .LE. IRVAL ) THEN
            IF ( JPAR .LT. IRVAL ) THEN
               IF ( ERRMX2( JPAR ) .LT. ERRMX2( JPAR + 1 ) ) JPAR = JPAR + 1
            END IF
            IF ( VALLOW .LT. ERRMX2( JPAR )) THEN
               ERRMX2( IPAR ) = ERRMX2( JPAR )
               NORDCELL( IPAR ) = NORDCELL( JPAR )
               IPAR = JPAR
               JPAR = JPAR + JPAR
            ELSE
               JPAR = IRVAL + 1
            END IF
            GO TO 650
         END IF
         ERRMX2( IPAR ) = VALLOW
         NORDCELL( IPAR ) = JREORD
         GO TO 600
      END IF
       
      !Update Aerosol Surface Area
      CALL HETCHEM_UPDATE_AERO( CONC )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Output performance statistics if required and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifdef rbstats

      WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
      EDATE = JDATE
      ETIME = JTIME
      CALL NEXTIME( EDATE, ETIME, TSTEP( 2 ) )
      IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN

         WSTEP = 0

         DO S = 1, 3
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DO L = 1, NLAYS
                     STATOUT( C, R, L ) = INT( STAT_SUM( C,R,L,S )
     &                                  +  0.00001 )
                  END DO
               END DO
            END DO

            IF ( .NOT. WRITE3( CTM_RBSTATS_1, VSTATS( S ),
     &            EDATE, ETIME, STATOUT ) ) THEN
               XMSG = 'Could not write ' // CTM_RBSTATS_1 // ' file'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF  

         END DO

         STAT_SUM = 0.0

      END IF
       
#endif


      IF( FIRSTCALL )FIRSTCALL = .FALSE.
      
      RETURN

C*********************** FORMAT STATEMENTS ****************************
92020 FORMAT( / 10X, 'Chemistry Solver Blocking Parameters ... ',
     &        / 10X, 'Domain Size (number of cells):             ', I10
     &        / 10X, 'Dimensioning Block Size (number of cells): ', I10
     &        / 10X, 'Number of Blocks:        ', I10
     &        / 10X, 'Size of General Blocks:  ', I10
     &        / 10X, 'Size of Last Block:      ', I10 )
92040 FORMAT( / 10X, 'Rosenbrock Chemistry Solver Error Control ',
     &               'Parameters ...',
     &        / 10X, 'RTOL : ', 1PE12.3,
     &        / 10X, 'ATOL : ', 1PE12.3, ' ppm' )

      END

