
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
C  FUNCTION: To control gas phase chemistry calculations performed by
C            the vectorized Gear solver (aka SMVGEAR)
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: GRINIT
C                                    JSPARSE
C                                    SIGMAFH
C                                    CALCKS
C                                    SMVGEAR
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995
C
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      the Models-3 minimum IOV configuration.
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Add DTIME performance stats as cpp option (Jeff Dec 96)
C                    Revised April 1997 to distinguish NSPCS from NSPCSD
C                    Revised April 1997 to conform to Models-3 framework
C                    Revised June 1997 to conform to beta version
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C                    Modified September, 1997 by Jerry Gipson to be
C                      consistent with the tartgtted CTM
C                    Modified March, 1998 by Jerry Gipson to read
C                      an emission file with units of moles/s
C                    Mod for unicode by Jeff, Feb. 1999
C                    16 Aug 01 J.Young: dyn alloc - Use HGRD_DEFN; replace
C                      INTERP3 with INTERPX; some allocatable arrays;
C                      Use GRVARS module
C                    31 Jan 05 J.Young: dyn alloc - establish both horizontal
C                    & vertical domain specifications in one module (GRID_CONF)
C                    29 Jul 05     WTH: Added IF blocks that call degrade 
C                                       routines if MECHNAME contains 'TX' 
C                                       substring.
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    29 Mar 11 S.Roselle: Replaced I/O API include files 
C                    with UTILIO_DEFN
C                    31 Aug 11 B.Hutzell revised method that determines calling
C                              degrade routine
C                    29 Sep 11 D.Wong: incorporated twoway model implementation
C                    15 Jul 14 B.Hutzell: 1) replaced mechanism include files with 
C                    RXNS_DATA module, 2) replaced call to CALCLK with CALC_RCONST
C                    in RXNS_FUNCTION module, 3) enabled reactions between all 
C                    species type by using unit conversion factors and 4) updated
C                    the explicit interace SMVGEAR, 5) added using heteorogeneous 
C                    rate constants by calling subroutine in AEROSOL_CHEMISTRY module,
C                    and 6) revised usage for INIT_DEGRADE and FINAL_DEGRADE routines
C                    02 Dec 14 B.Hutzell 1) added terrestrial data to conduct surface
C                    dependent reactions and 2) modified the call CALC_RCONST routine
C                    16 Sep 16 J.Young: update for inline procan (IRR)
C**********************************************************************

      USE RXNS_DATA
      USE RXNS_FUNCTION
      USE CGRID_SPCS          ! CGRID species number and offsets
      USE UTILIO_DEFN
      USE GRVARS              ! inherits GRID_CONF
      USE AEROSOL_CHEMISTRY
      USE DEGRADE_SETUP_TOX, ONLY : N_REACT, RXTANT_MAP
      USE PHOT_MOD, Only: INIT_PHOT_SHARED, RJ     ! photolysis rate, in-line module
      USE PA_DEFN,  Only: LIRR                     ! Process Anaylsis control and data variable
      USE PA_IRR_CLT

      IMPLICIT NONE 

C..INCLUDES:
      INCLUDE SUBST_FILES_ID  ! CMAQ files
      INCLUDE SUBST_CONST     ! CMAQ constants

C..ARGUMENTS:
      REAL, POINTER :: CONC( :,:,:,: )  ! concentrations
      INTEGER JDATE           ! Current date (YYYYDDD)
      INTEGER JTIME           ! Current time (HHMMSS)
      INTEGER TSTEP( 3 )      ! Time step vector (HHMMSS)

C..PARAMETERS:

C Integer zero
      INTEGER, PARAMETER :: IZERO = 0

C Conc. of M = 1E+06 ppm 
      REAL, PARAMETER :: CONCOFM = 1.0E+06

C Pascal to atm conversion factor
      REAL, PARAMETER :: PA2ATM = 1.0 / STDATMPA

C..EXTERNAL FUNCTIONS:
#ifdef sunws
      REAL, EXTERNAL :: DTIME          ! Execution time elapsed since last call
#endif

C..SAVED LOCAL VARIABLES:
      LOGICAL, SAVE :: LFIRST = .TRUE. ! Flag for first call to this subroutine
      LOGICAL, SAVE :: EMISVD = .TRUE. ! emission rates in vertical diffusion 

      INTEGER, SAVE :: IRUNC     ! Counter of calls to this subroutine
      INTEGER, SAVE :: NOXYZ     ! Total number of grid cells
      LOGICAL, SAVE :: LIRRBLK   ! Flag to indicate IRR to be done for block
      REAL,    SAVE :: AIRFC     ! Factor to convert gms air to ppm
      REAL,    SAVE :: MAOMV     ! Mol Wt of air over Mol Wt of water
      INTEGER, SAVE :: EMISLYRS  ! number of emission layers from file
      
C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 144 ) :: MSG       ! Message text
      CHARACTER( 16 ) :: PNAME = 'GRDRIVER' ! Procedure name
      CHARACTER( 16 ) :: UNITSCK    ! Units description
      CHARACTER( 16 ) :: VNAME      ! Name of I/O API data variable
      CHARACTER( 16 ) :: UC_UNITS   ! Units in upper case

      INTEGER BLK             ! Loop index for block of cells
      INTEGER CELLNUM         ! Cell number 
      INTEGER COL             ! Column index
      INTEGER ESP             ! Loop index for emissions species
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
      INTEGER NMID            ! Middle cell number in block
      INTEGER NPH             ! Index for number of phot. rxns in PHOT
      INTEGER NRX             ! Index for number of reactions
      INTEGER ROW             ! Row index
      INTEGER SPC             ! Species loop index
      INTEGER VAR             ! Variable number on I/O API file
      INTEGER ALLOCSTAT       ! test for array allocation status

      INTEGER, ALLOCATABLE, SAVE :: IRRCELL   ( : ) ! Cell No. of an IRR cell
      INTEGER, ALLOCATABLE       :: IRSPERF   ( : ) ! Number of restarts at beginning
      INTEGER, ALLOCATABLE       :: MXORDPERF ( : ) ! Maximum order used
      INTEGER, ALLOCATABLE       :: NBKUPS    ( : ) ! Number of backups
      INTEGER, ALLOCATABLE       :: NCFAILPERF( : ) ! Number of convergence failures
      INTEGER, ALLOCATABLE       :: NEFAILPERF( : ) ! Number of error test failures
      INTEGER, ALLOCATABLE       :: NITERPERF ( : ) ! Number of iterations
      INTEGER, ALLOCATABLE       :: NSTPERF   ( : ) ! Number of steps used
      INTEGER, ALLOCATABLE       :: NPDPERF   ( : ) ! Number of Jacobian updates
      INTEGER, ALLOCATABLE       :: NSUBPERF  ( : ) ! Number of RHS evaluations
  
      REAL( 8 ) CHEMSTEP      ! Chemistry integration interval (min)
      REAL( 8 ) VALLOW        ! Value holder for sort routine

      REAL CONVEM             ! Emissions conversion factor
      REAL CONVFC             ! Emissions conversion factor
      REAL DX                 ! Cell x-dimension
      REAL DY                 ! Cell  y-dimension
      
!     REAL, ALLOCATABLE, SAVE :: CONV   ( : )
      REAL, ALLOCATABLE, SAVE :: SEAICE ( :, : )          ! fractional seaice cover, [-] 

!      REAL, ALLOCATABLE, SAVE :: DENSA_J( :, :, : )      ! Cell density (Kg/m**3)
      REAL, ALLOCATABLE, SAVE :: DENS   ( :, :, : )      ! Cell density (Kg/m**3)
      REAL, ALLOCATABLE, SAVE :: PRES   ( :, :, : )      ! Cell pressure (Pa)
      REAL, ALLOCATABLE, SAVE :: QV     ( :, :, : )      ! Cell water vapor (Kg/Kg air)
      REAL, ALLOCATABLE, SAVE :: TA     ( :, :, : )      ! Cell temperature (K)

      REAL,    ALLOCATABLE       :: OPEN_ZONE   ( :,: )       ! fractional sea water cover, [-]
      REAL,    ALLOCATABLE       :: SURF_ZONE   ( :,: )       ! fractional surf-zone cover, [-]
      LOGICAL, ALLOCATABLE, SAVE :: LAND_ZONE   ( :,: )       ! Is the surface totally land?


      INTEGER     GXOFF, GYOFF          ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLEM1, ENDCOLEM1, STRTROWEM1, ENDROWEM1

      REAL( 8 ), ALLOCATABLE, SAVE :: Y_DEGRADE ( : , : ) ! concentration array used
                                                          ! by degradation routines

#ifdef sunws
      REAL    ERRSTAT          ! for DTIME()
      REAL    TARRAY( 2 )      ! for DTIME():TARRAY(1)=user time,
                               ! TARRAY(2)=system time
      REAL    DRIVER_TIME      ! accumulated driver user time for this step
      REAL    SOLVER_TIME      ! accumulated solver user time for this step
#endif

      INTERFACE
         SUBROUTINE SMVGEAR ( IRUN, JDATE, JTIME, CHEMSTEP,
     &                        LIRRFLAG, NIRRCLS, IRRCELL )
            INTEGER,   INTENT( IN ) :: IRUN         ! Counter of calls to calling subroutine
            INTEGER,   INTENT( IN ) :: JDATE        ! Date at start of integration
            INTEGER,   INTENT( IN ) :: JTIME        ! Time at start of integration
            INTEGER,   INTENT( IN ) :: NIRRCLS      ! No. of cells in block for IRR
            INTEGER,   INTENT( IN ) :: IRRCELL( : ) ! Cell No. of an IRR cell
            LOGICAL,   INTENT( IN ) :: LIRRFLAG     ! Flag for IRR calculations
            REAL( 8 ), INTENT( IN ) :: CHEMSTEP     ! Chemistry integration interval (min) 
         END SUBROUTINE SMVGEAR
         SUBROUTINE FIND_DEGRADED( JDATE, JTIME, CALL_DEGRADE )
           INTEGER, INTENT( IN )  :: JDATE        ! current model date , coded YYYYDDD
           INTEGER, INTENT( IN )  :: JTIME        ! current model time , coded HHMMSS
           LOGICAL, INTENT( OUT ) :: CALL_DEGRADE ! whether to call degradation routines
         END SUBROUTINE FIND_DEGRADED
         SUBROUTINE INIT_DEGRADE( CBLK, TCELL, DCELL, PHOTO_CELL, NUMCELLS,
     &                         JDATE, JTIME, BLKID )
           REAL( 8 ), INTENT( IN ) :: CBLK ( :, : )      !  species concentration in cell
           REAL( 8 ), INTENT( IN ) :: TCELL( : )         !  cell temperature  [ k ]
           REAL( 8 ), INTENT( IN ) :: DCELL( : )         !  cell air density  [ kg/m^3 ]
           REAL( 8 ), INTENT( IN ) :: PHOTO_CELL( :, : ) !  Photolysis table for cell [1/s]
           INTEGER,   INTENT( IN ) :: NUMCELLS  ! number of grid cells in block
           INTEGER,   INTENT( IN ) :: JDATE     ! current model date , coded YYYYDDD
           INTEGER,   INTENT( IN ) :: JTIME     ! current model time , coded HHMMSS
           INTEGER,   INTENT( IN ) :: BLKID     ! number for the block
         END SUBROUTINE INIT_DEGRADE      
         SUBROUTINE FINAL_DEGRADE( CBLK )
           REAL( 8 ), INTENT( INOUT ) :: CBLK( :, : ) ! species conc in cell
         END SUBROUTINE FINAL_DEGRADE
      END INTERFACE

!     logical, save :: bingo1 = .true.
!     logical, save :: bingo2

C**********************************************************************

      IF ( NUMB_MECH_SPC .EQ. 0 ) RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for Gear solver and 
c  set-up to do emissions here if that option is invoked
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN
         LFIRST = .FALSE.

         CALL GRVARS_INIT( JDATE, JTIME )

         CALL GRINIT
         CALL JSPARSE
         NOXYZ = MY_NCOLS * MY_NROWS * NLAYS
         MAOMV =  MWAIR / MWWAT
         
C...Initialize and report data
         EMBLK = 0.0

         WRITE( LOGDEV, 92020 ) NOXYZ, BLKSIZE, NBLKS, BLKLEN( 1 ), 
     &                          BLKLEN( NBLKS )

         WRITE( LOGDEV, 92040 ) ERRMAX( 1 ), YLOW( 1 )

C.. Get fractional seawater coverage from the OCEAN file.

         ALLOCATE( OPEN_ZONE( NCOLS, NROWS ), SURF_ZONE( NCOLS, NROWS ) )

         CALL SUBHFILE ( OCEAN_1, GXOFF, GYOFF,
     &                   STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3 )

         VNAME = 'OPEN'
         IF ( .NOT. INTERPX( OCEAN_1, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                       JDATE, JTIME, OPEN_ZONE ) ) THEN
            MSG = 'Could not read OPEN from ' // OCEAN_1
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C.. Get fractional surf-zone coverage from the OCEAN file.

         VNAME = 'SURF'
         IF ( .NOT. INTERPX( OCEAN_1, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                       JDATE, JTIME, SURF_ZONE ) ) THEN
            MSG = 'Could not read SURF from ' // OCEAN_1
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         ALLOCATE( LAND_ZONE( NCOLS, NROWS ) )
         
         DO ROW = 1, NROWS
            DO COL = 1, NCOLS
               IF( OPEN_ZONE( COL,ROW ) + SURF_ZONE( COL,ROW ) .GT. 0.0 )THEN
                   LAND_ZONE( COL,ROW ) = .FALSE.
               ELSE
                   LAND_ZONE( COL,ROW ) = .TRUE.
               END IF
             END DO
         END DO

         DEALLOCATE( OPEN_ZONE, SURF_ZONE )

         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

!        ALLOCATE ( CONV( NLAYS ), STAT = ALLOCSTAT )
!        IF ( ALLOCSTAT .NE. 0 ) THEN
!           MSG = 'Failure allocating CONV'
!           CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
!        END IF

         ALLOCATE( DENS( NCOLS, NROWS, NLAYS ), PRES( NCOLS, NROWS, NLAYS ),
     &             QV  ( NCOLS, NROWS, NLAYS ), TA  ( NCOLS, NROWS, NLAYS ),
     &             SEAICE( NCOLS, NROWS ) )

         ALLOCATE( IRRCELL( BLKSIZE ) )
         IRRCELL = 0

C..Initialize shared photolysis data
         CALL INIT_PHOT_SHARED()

C:WTH Determine whether DEGRADE routines are needed.

         CALL FIND_DEGRADED( JDATE, JTIME, CALL_DEG )
         IF( CALL_DEG ) THEN
            WRITE( LOGDEV, * ) 'DEGRADE ROUTINES USED'
            WRITE( LOGDEV, * ) 'Mechanism contains degraded species'
#ifdef verbose_gas         
         ELSE
            WRITE( LOGDEV, * ) 'DEGRADE ROUTINES not USED'
            WRITE( LOGDEV, * ) 'Mechanism contains NO degraded species'
#endif            
         ENDIF
C:WTH set up degradation array

         ALLOCATE( Y_DEGRADE( BLKSIZE, NSPCSD ) )

      ENDIF      ! First call

#ifdef sunws
C initialize run clock

      ERRSTAT = DTIME ( TARRAY )
      DRIVER_TIME = 0.0
      SOLVER_TIME = 0.0
#endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Start of integration driver after first call
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IRUNC = IRUNC + 1
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

      VNAME = 'SEAICE'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MIDDATE, MIDTIME, SEAICE ) ) THEN
         MSG = 'Could not read SEAICE from MET_CRO_2D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF
 
C.. Get ambient temperature in K

      VNAME = 'TA' 
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, TA ) ) THEN
         MSG = 'Could not read TA from MET_CRO_3D'
         CALL M3EXIT( PNAME, MIDDATE, MIDTIME, MSG, XSTAT1 )
      ENDIF
      
C.. Get specific humidity in Kg H2O / Kg air

      VNAME = 'QV'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, QV ) ) THEN
         MSG = 'Could not read QV from MET_CRO_3D'
         CALL M3EXIT( PNAME, MIDDATE, MIDTIME, MSG, XSTAT1 )
      ENDIF 
      
C.. Get density in Kg/m^3

      VNAME = 'DENS'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, DENS ) ) THEN
         MSG = 'Could not read DENS from MET_CRO_3D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      ENDIF

C.. Get pressure in Pascals

      VNAME = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, PRES ) ) THEN
         MSG = 'Could not read PRES from MET_CRO_3D'
         CALL M3EXIT ( PNAME, MIDDATE, MIDTIME, MSG, XSTAT1 )
      ENDIF
 
C.. Get Heterogeneous reaction rates using aerosol surface area. Also
C   store the initial surface area so that it can be updated after the
C   solver finds a solution.

      CALL HETCHEM_RATES( TA, PRES, QV, CONC, DENS )
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set flag for reordering of cells and put cells in sequential  
c  order initially
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LORDERING = .TRUE.
      IF ( .NOT. LREORDER .OR. NBLKS .EQ. 1 ) LORDERING = .FALSE.
      DO NCELL = 1, NOXYZ
         NORDCELL( NCELL ) = NCELL
      ENDDO

      IF( LPERFSMRY ) THEN
         ALLOCATE ( NSTPERF(    MXBLKS ), STAT = ALLOCSTAT )
         ALLOCATE ( MXORDPERF(  MXBLKS ), STAT = ALLOCSTAT )
         ALLOCATE ( NITERPERF(  MXBLKS ), STAT = ALLOCSTAT )
         ALLOCATE ( NSUBPERF(   MXBLKS ), STAT = ALLOCSTAT )
         ALLOCATE ( NPDPERF(    MXBLKS ), STAT = ALLOCSTAT )
         ALLOCATE ( NCFAILPERF( MXBLKS ), STAT = ALLOCSTAT )
         ALLOCATE ( NEFAILPERF( MXBLKS ), STAT = ALLOCSTAT )
         ALLOCATE ( IRSPERF(    MXBLKS ), STAT = ALLOCSTAT )
         ALLOCATE ( NBKUPS(     MXBLKS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            MSG = 'Failure allocating NSTPERF, MXORDPERF, NITERPERF,'
     &          // ' NSUBPERF, NPDPERF, NCFAILPERF, NEFAILPERF,'
     &          // ' IRSPERF, or NBKUPS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF
      END IF

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
         ENDIF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell physical data in the block arrays, converting
C  pressure to atmospheres, water vapor to ppm, emissions to ppm/min,
C  setting to land if seaice coverage is nonzero  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( .NOT. EMISVD )EMBLK = 0.0

         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
            BLKTEMP( NCELL ) = REAL( TA( COL,ROW,LEV ), 8 )
            BLKDENS( NCELL ) = REAL( DENS( COL,ROW,LEV ), 8 )
            BLKSVOL( NCELL ) = REAL( 1.0 / BLKDENS( NCELL ), 8 )
            BLKPRES( NCELL ) = REAL( PA2ATM * PRES( COL, ROW, LEV ), 8 )
            BLKCH2O( NCELL ) = REAL( MAX( QV( COL,ROW,LEV ) * MAOMV * CONCOFM, 0.0 ), 8)
            IF( LAND_ZONE( COL,ROW ) .OR. SEAICE (COL,ROW) .GT. 0.0 )THEN
                BLKLAND (NCELL) = .TRUE.
            ELSE
                BLKLAND (NCELL) = .FALSE.
            END IF            
         ENDDO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell concentrations in the block arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO ISP = 1, ISCHANG( NCS )
            SPC    = CGRID_INDEX( ISP )
            ISPNEW = IOLD2NEW( ISP, NCS )
            DO NCELL = 1, NUMCELLS
               CELLNUM = NORDCELL( OFFSET + NCELL )
               COL = CCOL( CELLNUM )
               ROW = CROW( CELLNUM )
               LEV = CLEV( CELLNUM )
               IF( CONVERT_CONC( ISP ) )THEN 
                   CNEW( NCELL,ISPNEW )  = REAL( MAX( FORWARD_CONV( ISP ) * BLKSVOL( NCELL )
     &                                   *       CONC( COL,ROW,LEV,SPC ), CONCMIN), 8 )
               ELSE
                   CNEW( NCELL,ISPNEW ) = REAL( MAX( CONC( COL,ROW,LEV,SPC ), CONCMIN), 8 )
               END IF
               BLKCONC( NCELL, ISP ) = CNEW( NCELL,ISPNEW )
            ENDDO                 
         ENDDO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   Get heterogeneous, photolytic and thermal rate constants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LSUNLIGHT = .FALSE.

         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
            DO NPH = 1, NPHOTAB
               RJBLK( NCELL, NPH ) = REAL( RJ( COL, ROW, LEV, NPH ), 8 )
               IF ( RJBLK( NCELL, NPH ) .GT. 0.0 ) LSUNLIGHT = .TRUE.
            ENDDO                         
            DO NPH = 1, NHETERO
               BLKHET( NCELL, NPH ) =  KHETERO( NPH, COL, ROW, LEV )
            END DO
         ENDDO
         
!        CALL CALCKS( NPHOTAB, RJBLK )
         CALL CALC_RCONST( BLKTEMP, BLKPRES, BLKCH2O, RJBLK, BLKHET, LSUNLIGHT, BLKLAND, RK, NUMCELLS )

         IF ( LSUNLIGHT ) THEN
            HMAX = HMAXDAY( NCS )
            NCSP = NCS
         ELSE
            HMAX = HMAXNIT
            NCSP = NCS + 1
         ENDIF

C..WTH: Put concentrations into degradation array

         IF ( CALL_DEG ) THEN
            Y_DEGRADE = 0.0D0
            DO ISP = 1, NSPCSD
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  COL = CCOL( CELLNUM )
                  ROW = CROW( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  Y_DEGRADE( NCELL,ISP ) = REAL(MAX( CONC( COL,ROW,LEV,ISP ), CONCMIN), 8 )
               ENDDO                 
            ENDDO

C..initialize degradation routines
              
            CALL INIT_DEGRADE( Y_DEGRADE, BLKTEMP, BLKDENS, RJBLK, 
     &                         NUMCELLS, JDATE, JTIME, BLKID )
  
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  If cells are not being reordered, do the outputs if needed and 
C  it is time
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( .NOT. LORDERING ) THEN         
C..Output a concentrations for a single cell
            IF ( LCELLCONC ) THEN
               LCONCOUT = .FALSE.
               CELLOUT = 0
               IF ( IRUNC .GE. IRUNPRO1 .AND. IRUNC .LE. IRUNPRO2 ) THEN
                  DO NCELL = 1, NUMCELLS
                     CELLNUM = NORDCELL( OFFSET + NCELL )
                     COL = CCOL( CELLNUM )
                     ROW = CROW( CELLNUM )
                     LEV = CLEV( CELLNUM )
                     IF ( COL.EQ.CCOLOUT .AND. ROW.EQ.CROWOUT .AND.
     &                    LEV.EQ.CLEVOUT ) THEN
                        LCONCOUT = .TRUE.
                        CELLOUT = NCELL
                        WRITE( IUNCOUT ) IRUNC, BLKID, CELLOUT, CCOLOUT,
     &                                   CROWOUT, CLEVOUT, IZERO,  
     &                                   RUNMIN, ( CNEW( CELLOUT,ISP ),
     &                                   ISP = 1, ISCHANG( NCS ) )
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
               
C..Output IC for a block                                    
            IF ( LDUMPBLK .AND. IRUNC .EQ. IRUNBLK .AND. BLKID .EQ. IBLKBLK ) THEN
               WRITE( IUNBIC ) NUMCELLS, CHEMSTEP
               WRITE( IUNBIC ) ( BLKTEMP( NCELL ),  NCELL = 1, NUMCELLS )
               WRITE( IUNBIC ) ( BLKPRES( NCELL ),  NCELL = 1, NUMCELLS )
               WRITE( IUNBIC ) ( BLKCH2O( NCELL ),  NCELL = 1, NUMCELLS )
               WRITE( IUNBIC ) ( BLKLAND( NCELL ),  NCELL = 1, NUMCELLS )

               DO ISP = 1, ISCHANG( NCS )
                  WRITE( IUNBIC ) CHEMISTRY_SPC( ISP ), ( BLKCONC( NCELL, ISP ),
     &                            NCELL = 1, NUMCELLS )
               ENDDO

               DO NPH = 1, NMPHOT
                  IRXN = IPH( NPH, 1 )
                  WRITE( IUNBIC ) ( RK( NCELL, IRXN ), NCELL = 1, NUMCELLS )
               ENDDO

               MSG = 'Stopping in CHEM after dumping block IC'
               CALL M3EXIT( 'CHEM', JDATE, JTIME, MSG, XSTAT0 )
            ENDIF
            
C..Output IC for a cell            
            IF ( LDUMPCELL .AND. IRUNC .EQ. IRUNCELL .AND. BLKID .EQ. IBLKCELL ) THEN 
               WRITE( IUNCIC, 93000 )

               DO ISP = 1, ISCHANG( NCS )
                  WRITE( IUNCIC, 93020 ) CHEMISTRY_SPC( ISP ),
     &                                BLKCONC( INUMCELL, ISP )
               ENDDO

               WRITE( IUNCIC,93040 ) BLKTEMP( INUMCELL ) 
               WRITE( IUNCIC,93060 ) BLKCH2O( INUMCELL )
               WRITE( IUNCIC,93080 ) BLKPRES( INUMCELL )
               WRITE( IUNCIC,93085 ) BLKLAND( INUMCELL )

               DO NPH = 1, NMPHOT
                  IRXN = IPH( NPH, 1 )
                  WRITE( IUNCIC,93100 ) IRXN, RK( INUMCELL, IRXN )
               ENDDO

               MSG = 'Stopping in CHEM after dumping cell ic'
               CALL M3EXIT( 'CHEM', JDATE, JTIME, MSG, XSTAT0 )
            ENDIF
            
C..debug output            
            IF ( LDEBUG .AND. IRUNC .EQ. IRUNBUG ) THEN
               ICPR = 0
               IBLKBUG = 0
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  COL = CCOL( CELLNUM )
                  ROW = CROW( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  IF( COL .EQ. DBGCOL .AND. ROW .EQ. DBGROW .AND. LEV .EQ. DBGLEV ) THEN
                     IBLKBUG = BLK
                     ICPR = NCELL
                     WRITE( IUNDBG, 93120 ) IRUNC, BLKID

                     DO NRX = 1, NRXNS
                        WRITE( IUNDBG, 93140 ) NRX, RK( ICPR, NRX )
                     ENDDO

                     WRITE( IUNDBG,93160 ) IRUNC, BLKID
                     NMID = NUMCELLS / 2
                     IF( NMID .LE. 0 ) NMID = 1
                     WRITE( IUNDBG,93360 ) NMID, NUMCELLS
               
                     DO ISP = 1, ISCHANG( NCS )
                        ISPOLD = INEW2OLD( ISP,NCS )
                        WRITE( IUNDBG, 93380 ) ISP, CHEMISTRY_SPC( ISPOLD ),
     &                                   CNEW( 1,ISP ), CNEW( NMID,ISP ),
     &                                   CNEW( NUMCELLS, ISP )
                     ENDDO
               
                     WRITE( IUNDBG, 93200 ) ISCHANG( NCS ) + 1, CONCOFM
                     WRITE( IUNDBG, 93220 ) ISCHANG( NCS ) + 2, 0.2095*CONCOFM
                     WRITE( IUNDBG, 93240 ) ISCHANG( NCS ) + 3, 0.7808*CONCOFM
                     WRITE( IUNDBG, 93260 ) ISCHANG( NCS ) + 4, BLKCH2O( 1 ),
     &                                      BLKCH2O( NMID ), BLKCH2O( NUMCELLS )
                     WRITE( IUNDBG, 93280 ) BLKTEMP( 1 ), BLKTEMP( NMID ),
     &                                      BLKTEMP( NUMCELLS )
                     WRITE( IUNDBG, 93300 ) BLKPRES( 1 ), BLKPRES( NMID ),
     &                                      BLKPRES( NUMCELLS ) 
                     WRITE( IUNDBG, 93305 ) BLKLAND( 1 ), BLKLAND( NMID ),
     &                                      BLKLAND( NUMCELLS ) 
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         
         IF ( LTRACE .AND. IRUNC .GE. IRUNTRC1 .AND. IRUNC .LE. IRUNTRC2
     &               .AND. BLKID .EQ. IBLKTRC ) THEN 
            LTRCOUT = .TRUE.
         ELSE
            LTRCOUT = .FALSE.
         ENDIF 

#ifdef sunws
         ERRSTAT = DTIME ( TARRAY )
         DRIVER_TIME = DRIVER_TIME + TARRAY( 1 )
#endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   Call Gear solver for the integration interval
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         CALL SMVGEAR( IRUNC, JDATE, JTIME, CHEMSTEP, LIRRBLK, NIRRCLS, IRRCELL )

#ifdef sunws
         ERRSTAT = DTIME ( TARRAY )
         SOLVER_TIME = SOLVER_TIME + TARRAY( 1 )
#endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  If not ordering cells, save performance statistics, do debug output
C  if requested, and store updated concentrations.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( .NOT. LORDERING ) THEN
            IF ( LPERFSMRY ) THEN
               NSTPERF(    BLK ) = NSTEPS
               MXORDPERF(  BLK ) = MXORDUSED
               NITERPERF(  BLK ) = NUMNEWT
               NSUBPERF(   BLK ) = NSUBFUN
               NPDPERF(    BLK ) = NPDERIV
               NCFAILPERF( BLK ) = NCFAIL
               NEFAILPERF( BLK ) = NEFAIL
               IRSPERF(    BLK ) = IRSTART
               NBKUPS(     BLK ) = NUMBKUPS
            ENDIF


C..debug output
            IF ( LDEBUG .AND. IRUNC .EQ. IRUNBUG .AND. BLK .EQ. IBLKBUG ) THEN
               WRITE( IUNDBG, 93320 ) IRUNC, ICPR
               DO ISP = 1, ISCHANG( NCS )
                  ISPNEW = IOLD2NEW( ISP, NCS )
                  WRITE( IUNDBG, 93340 ) ISP, CHEMISTRY_SPC( ISP ),
     &                                   CNEW( ICPR, ISPNEW )
               ENDDO

               NMID = NUMCELLS / 2
               IF ( NMID .LE. 0 ) NMID = 1
               WRITE( IUNDBG, 93360 ) NMID, NUMCELLS
               DO ISP = 1, ISCHANG( NCS )
                  ISPOLD  = INEW2OLD( ISP, NCS )
                  WRITE( IUNDBG, 93380 ) ISP, CHEMISTRY_SPC( ISPOLD ),
     &                                   CNEW( 1,ISP ), 
     &                                   CNEW( NMID, ISP ),
     &                                   CNEW( NUMCELLS, ISP )
               ENDDO
            ENDIF

C..Update concentrations
           DO ISP = 1, ISCHANG( NCS )
               ISPOLD = INEW2OLD( ISP, NCS )
               SPC    = CGRID_INDEX( ISPOLD )
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  ROW = CROW( CELLNUM )
                  COL = CCOL( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  IF( CONVERT_CONC( ISPOLD ) )THEN 
                     CONC( COL,ROW,LEV,SPC ) = REAL( REVERSE_CONV( ISPOLD ) 
     &                                       *       BLKDENS( NCELL ) * CNEW( NCELL,ISP ), 4)

                  ELSE
                     CONC( COL,ROW,LEV,SPC ) = REAL( CNEW( NCELL,ISP ), 4)
                  ENDIF
               ENDDO
            ENDDO


            IF ( CALL_DEG ) THEN

C  Update degradation array with species treated by Rosenbach solver
C
C               DO ISP = 1, ISCHANG( NCS )
C                  ISPOLD  = INEW2OLD( ISP, NCS )
C                  DO NCELL = 1, NUMCELLS
C                     Y_DEGRADE( NCELL,ISPOLD ) = Y( NCELL,ISP )
C                  END DO
C               END DO


C  Update CGRID based on the degradation routines
               CALL FINAL_DEGRADE( Y_DEGRADE )
               UPDATE_DEGRADED: DO ISP = 1, N_REACT
                  VAR = RXTANT_MAP( ISP )
                  IF( VAR .LE. 0 )CYCLE UPDATE_DEGRADED
                  DO SPC = 1, NUMB_MECH_SPC
                     IF( VAR .EQ. CGRID_INDEX( SPC ) )CYCLE UPDATE_DEGRADED
                  END DO
                  DO NCELL = 1, NUMCELLS
                     CELLNUM = NORDCELL( OFFSET + NCELL )
                     COL = CCOL( CELLNUM )
                     ROW = CROW( CELLNUM )
                     LEV = CLEV( CELLNUM )
                     CONC( COL,ROW,LEV,VAR ) = REAL( Y_DEGRADE( NCELL,VAR ), 4)
                  END DO
               END DO UPDATE_DEGRADED
             ENDIF
             
             IF ( LIRRBLK ) CALL PA_IRR_BLKENDC ( OFFSET, CCOL, CROW, CLEV,
     &                                            NORDCELL, NIRRCLS, IRRCELL )
         ENDIF

500   CONTINUE

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  End of block loop; reorder cells if necessary and go back do the  
C  block loop again.  Taken from Jacobson 1994. (Heapsort on ERRMX2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LORDERING ) THEN
         LORDERING = .FALSE.     
         LVAL = NOXYZ * 0.5 + 1
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
!              if ( bingo2 ) then
!                 bingo1 = .true.
!                 bingo2 = .false.
!                 write( *,* ) 'grdriver-mype,heapsort,LORDERING: ', mype, lordering
!              end if
               GO TO 100
            ENDIF
         ENDIF
         IPAR = LVAL
         JPAR = LVAL + LVAL
650      CONTINUE
         IF ( JPAR .LE. IRVAL ) THEN
            IF ( JPAR .LT. IRVAL ) THEN
               IF ( ERRMX2( JPAR ) .LT. ERRMX2( JPAR + 1 ) ) JPAR = JPAR + 1
            ENDIF
            IF ( VALLOW .LT. ERRMX2( JPAR )) THEN
               ERRMX2( IPAR ) = ERRMX2( JPAR )
               NORDCELL( IPAR ) = NORDCELL( JPAR )
               IPAR = JPAR
               JPAR = JPAR + JPAR
            ELSE
               JPAR = IRVAL + 1
            ENDIF
            GO TO 650
         ENDIF
         ERRMX2( IPAR ) = VALLOW
         NORDCELL( IPAR ) = JREORD
         GO TO 600
      ENDIF
       
      !Update Aerosol Surface Area
      CALL HETCHEM_UPDATE_AERO( CONC )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Output performance statistics if required and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LPERFSMRY ) THEN
         WRITE( IUNPERF ) IRUNC, NBLKS, JTIME, CHEMSTEP
         WRITE( IUNPERF ) ( NSTPERF(    BLK ), BLK = 1, NBLKS )
         WRITE( IUNPERF ) ( MXORDPERF(  BLK ), BLK = 1, NBLKS )
         WRITE( IUNPERF ) ( NITERPERF(  BLK ), BLK = 1, NBLKS )
         WRITE( IUNPERF ) ( NSUBPERF(   BLK ), BLK = 1, NBLKS )
         WRITE( IUNPERF ) ( NPDPERF(    BLK ), BLK = 1, NBLKS )
         WRITE( IUNPERF ) ( NCFAILPERF( BLK ), BLK = 1, NBLKS )    
         WRITE( IUNPERF ) ( NEFAILPERF( BLK ), BLK = 1, NBLKS )
         WRITE( IUNPERF ) ( IRSPERF(    BLK ), BLK = 1, NBLKS )
         WRITE( IUNPERF ) ( NBKUPS(     BLK ), BLK = 1, NBLKS )
         DEALLOCATE ( NSTPERF )
         DEALLOCATE ( MXORDPERF )
         DEALLOCATE ( NITERPERF )
         DEALLOCATE ( NSUBPERF )
         DEALLOCATE ( NPDPERF )
         DEALLOCATE ( NCFAILPERF )
         DEALLOCATE ( NEFAILPERF )
         DEALLOCATE ( IRSPERF )
         DEALLOCATE ( NBKUPS )
      ENDIF

#ifdef sunws
      ERRSTAT = DTIME( TARRAY )
      DRIVER_TIME = DRIVER_TIME + TARRAY( 1 )

      WRITE( LOGDEV, 93400 ), DRIVER_TIME, SOLVER_TIME
93400 FORMAT( / 10X, 'Timing Statistics in Chemistry ...'
     &        / 10X, 'Driver time for this step:', 1PG13.5
     &        / 10X, 'Solver time for this step:', 1PG13.5 )
#endif

      RETURN
      
C*********************** FORMAT STATEMENTS ****************************
92000 FORMAT( / 10X, 'Emissions Processing in Chemistry ...'
     &        / 10X, 'Number of Emissions Layers:         ', I3
     &        / 10X, 'out of total Number of Model Layers:', I3 )
92020 FORMAT( / 10X, 'Chemistry Solver Blocking Parameters ... ',
     &        / 10X, 'Domain Size (number of cells):             ', I10
     &        / 10X, 'Dimensioning Block Size (number of cells): ', I10
     &        / 10X, 'Number of Blocks:        ', I10
     &        / 10X, 'Size of General Blocks:  ', I10
     &        / 10X, 'Size of Last Block:      ', I10 )
92040 FORMAT( / 10X, 'Chemistry Solver Error Control Parameters ...',
     &        / 10X, 'RTOL : ', 1PE12.3,
     &        / 10X, 'ATOL : ', 1PE12.3, ' ppm' )

93000 FORMAT(   'units' )
93020 FORMAT(   A4, 1X, 1PE15.6 )
93040 FORMAT(   'TEMP', 1X, 1PE15.6 )
93060 FORMAT(   'H2O ', 1X, 1PE15.6 )
93080 FORMAT(   'PRES', 1X, 1PE15.6 )
93085 FORMAT(   'LAND', 1X, L15 )
93100 FORMAT(   I3, 1X, 1PE15.6 )
93120 FORMAT(   1X, 'Rate constants at start of irun=', I4, 
     &              ' block=', I4 )
93140 FORMAT(   1X, 'n= ', I3, ' k=', 1PE20.8 )
93160 FORMAT(  /1X, 'Species concentrations at start of irun=', I4,
     &              ' for block=',I4)
93180 FORMAT(   1X, 'C(0)= ', I3, 2X, A4, 2x, 3( 1PE20.10 ) )
93200 FORMAT(   1X, 'C(0)= ', I3, 2X, 'M   ', 2X, 1PE20.10 )  
93220 FORMAT(   1X, 'C(0)= ', I3, 2X, 'O2  ', 2X, 1PE20.10 ) 
93240 FORMAT(   1X, 'C(0)= ', I3, 2X, 'N2  ', 2X, 1PE20.10 ) 
93260 FORMAT(   1X, 'C(0)= ', I3, 2X, 'H2O ', 2X, 3( 1PE20.10 ) )
93280 FORMAT(   1X, 'TEMP (K)         =   ', 3( 1PE20.10 ) )
93300 FORMAT(   1X, 'PRESS(ATM)       =   ', 3( 1PE20.10 ) )
93305 FORMAT(   1X, 'LAND(True/False) =   ', 3( L20 ) )
93320 FORMAT(  /1X, 'Species concentrations at end of irun=', I4,
     &              ' Cell=', I5 )
93340 FORMAT(   1x, 'C(end)= ', I3, 2x, A4, 2x, 1PE20.8)
93360 FORMAT( //1X, 'Concs for cell 1, cell ',I3,' and cell ',I3 )
93380 FORMAT(   1X, 'C(E)= ',I3, 2X, A4, 2X, 3( 1PE20.10 ) )
      END
                            

