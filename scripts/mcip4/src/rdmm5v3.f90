!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

SUBROUTINE rdmm5v3 (mcip_now)

!-------------------------------------------------------------------------------
! Name:     Read MM5 Version 3
! Purpose:  Reads incoming MM5v3-formatted output files for use in MCIP.
! Notes:    Option to use Pleim-Xiu prognostic surface fields is only
!           compatible with MM5v3.
! Revised:  20 Sep 2001  Original version.  This replaces MCIP v1 routines
!                        readmm1.F and readmm2.F.  (T. Otte)
!           18 Oct 2001  Corrected calculation of precipitation rates for
!                        MCIP runs starting after MM5 output start time, and
!                        for using MM5 variable BUFFRQ to split output files.
!                        Added error-checking on new file headers.  (T. Otte)
!           20 Nov 2001  Now allow code to run in the absence of input
!                        cloud and rain mixing ratio arrays.  (T. Otte)
!           22 Jan 2002  Changed file name to explicit file rather than
!                        Fortran unit to improve portability.  (S. Howard
!                        and T. Otte)
!                        Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           05 Jun 2003  Renamed local variable SEASON to MET_SEASON and
!                        moved it to module METINFO.  Added read of snow
!                        cover.  (T. Otte, J. Pleim, and D. Schwede)
!           09 Aug 2004  Added graupel.  Modified handling of VEGFRC to
!                        account for NCAR coding error in P-X LSM in MM5v3.5+.
!                        Corrected error-checking for restart files to ensure
!                        that there is no physical discontinuity.  Changed
!                        local array allocation to occur only on initial call
!                        to subroutine to avoid memory fragmentation.  Corrected
!                        header settings for polar stereographic and Mercator
!                        projections.  Added option to read snow cover from
!                        liquid-equivalent field (WEASD).  Added read for 2-m
!                        temperature.  (T. Otte and D. Schwede)
!           29 Nov 2004  Added call to read fractional land use categories
!                        if the data are available.  (T. Otte)
!           19 May 2005  Changed code so that meteorology arrays are loaded
!                        X-Y rather than MM5 convention Y-X.  Removed unused
!                        variables MET_IEXPAND, MET_IOFFSET, MET_JOFFSET,
!                        REGIME, and MAVAIL.  Added MET_TRU2.  Removed
!                        interface block for unused routine CRS2DOT.  Corrected
!                        setting for GOTZNT flag so that roughness length can
!                        be time-varying with Pleim-Xiu simulations.  Added
!                        reads for U10 and V10, if available.  (T. Otte)
!           15 Jul 2005  Corrected MM5 seasons for Southern Hemisphere.
!                        (T. Otte)
!           19 Aug 2005  Removed unused variable FSOIL.  Eliminated use of F90
!                        intrinsic function TRANSPOSE to avoid bounds checking
!                        issues on Linux Intel compilers.  Changed internal
!                        variable TIME to TIMEOUT to avoid confusion with F90
!                        intrinsic function.  (T. Otte)
!           10 Apr 2006  Corrected checking of I/O API variables for Mercator
!                        projection.  (T. Otte)
!           02 May 2006  Corrected setting of I/O API variables for polar
!                        stereographic projection...again.  (T. Otte)
!           17 Aug 2006  Added capability to process roughness length, albedo,
!                        and emissivity here for MM5v2 runs that are
!                        converted to MM5v3 format.  (T. Otte)
!           30 Jul 2007  Removed reads and internal variables for emissivity
!                        and net radiation.  Changed variable NAME to VNAME
!                        to avoid confusion with F90 intrinsic.  Eliminated
!                        logical variable "PX" to make code more general.
!                        Changed MET_RADIATION to MET_LW_RAD.  (T. Otte)
!           21 Apr 2008  Corrected reading snow cover field for MM5 simulations
!                        with snow effects off.  Added 2-m mixing ratio (Q2)
!                        and turbulent kinetic energy (TKE), if available.
!                        (T. Otte)
!           29 Oct 2009  Added Coriolis on cross points (CORIOLIS) for when
!                        potential vorticity is needed.  Note that MM5 outputs
!                        Coriolis on dot points.  PV calculation uses Coriolis
!                        on cross points, so it is computed here.  Added
!                        latitude, longitude, and map-scale factors on U and
!                        V faces.  Changed real number comparisons from
!                        "equivalences" to "less than tolerances".  (T. Otte)
!           12 Feb 2010  Removed unused variables HDATE, IEND, and SLMO.
!                        (T. Otte)
!           23 Dec 2010  Added sea ice.  If sea ice is not part of MM5 output
!                        file, create sea ice field from SST and LANDMASK.
!                        Changed latitude and longitude calculations for
!                        polar stereographic projection to interpolations.
!                        (T. Otte)
!           31 Aug 2011  Changed name of module FILE to FILES and DATA to
!                        DATA_IN to avoid conflicts with F90 protected
!                        intrinsics.  Improved error handling.  Changed F77
!                        character declarations to F90 standard.  Changed
!                        DATA statements to parameters.  Changed arguments
!                        to 19-character elements for GETH_IDTS.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE date_pack
  USE files
  USE metinfo, nx => met_nx, ny => met_ny, nz => met_nz
  USE metvars
  USE mcipparm

  IMPLICIT NONE

  REAL,    SAVE,      ALLOCATABLE   :: albd       ( : , : )
  INTEGER,            ALLOCATABLE   :: bhi        ( : , : )
  REAL,               ALLOCATABLE   :: bhr        ( : , : )
  REAL                              :: buffrq
  REAL,               EXTERNAL      :: cori
  CHARACTER(LEN=24)                 :: currentdate
  REAL,               ALLOCATABLE   :: data_in    ( : , : , : , : )
  CHARACTER(LEN=46)                 :: description
  REAL,    SAVE,      ALLOCATABLE   :: dum2d      ( : , : )
  INTEGER                           :: end_index  ( 4 )
  CHARACTER(LEN=19)                 :: endseas
  LOGICAL, SAVE                     :: first      = .TRUE.
  INTEGER                           :: i
  INTEGER                           :: idts_end
  INTEGER                           :: idts_start
  INTEGER                           :: idtsec
  INTEGER                           :: iflag
  LOGICAL                           :: ifmm
  CHARACTER(LEN=60)                 :: ifmt1
  CHARACTER(LEN=60)                 :: ifmt1a
  INTEGER                           :: ii
  INTEGER                           :: im1
  INTEGER                           :: istat
  INTEGER                           :: j
  INTEGER                           :: jj
  INTEGER                           :: jm1
  INTEGER                           :: k
  INTEGER                           :: k1
  INTEGER                           :: k2
  INTEGER                           :: keep
  INTEGER                           :: km1
  INTEGER                           :: lbnd
  REAL,               EXTERNAL      :: mapfac_lam
  REAL,               EXTERNAL      :: mapfac_merc
  REAL,               EXTERNAL      :: mapfac_ps
  CHARACTER(LEN=24),  INTENT(IN)    :: mcip_now
  CHARACTER(LEN=256)                :: mmfile
  INTEGER                           :: ndim
  LOGICAL                           :: newfile
  LOGICAL                           :: newtime
  INTEGER,            PARAMETER     :: numprogs   = 20
  INTEGER,            PARAMETER     :: numvalsi   = 50
  INTEGER,            PARAMETER     :: numvalsr   = 20
  INTEGER                           :: nxm
  INTEGER                           :: nym
  LOGICAL                           :: ok         = .TRUE.
  CHARACTER(LEN=4)                  :: ordering
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'RDMM5V3'
  REAL,    SAVE,      ALLOCATABLE   :: sfz0       ( : , : )
  REAL,               PARAMETER     :: smallnum   = 1.0e-7
  CHARACTER(LEN=4)                  :: staggering
  INTEGER                           :: start_index(4)
  CHARACTER(LEN=19),  SAVE          :: startdate
  CHARACTER(LEN=19)                 :: startseas
  CHARACTER(LEN=2)                  :: str1
  CHARACTER(LEN=2)                  :: str2
  REAL                              :: timeout
  INTEGER,            PARAMETER     :: ttol       = 300  ! [sec]
  INTEGER                           :: ubnd
  CHARACTER(LEN=25)                 :: units
  CHARACTER(LEN=9)                  :: var
  CHARACTER(LEN=9)                  :: vname
  REAL                              :: xoff
  REAL                              :: xxin
  REAL                              :: yoff
  REAL                              :: yyin

  ! Data capture verification flags.

  LOGICAL :: gotalbedo,  gotglw,     gotgroundt, gothfx,     gotisltyp,   &
             gotlai,     gotlanduse, gotlatcrs,  gotloncrs,  gotmapcrs,   &
             gotmapdot,  gotmol,     gotpp,      gotpsa,     gotq2,       &
             gotqca,     gotqfx,     gotqga,     gotqia,     gotqra,      &
             gotqsa,     gotqva,     gotra,      gotraincon, gotrainnon,  &
             gotrgrnd,   gotrstom,   gotseaice,  gotsigmah,  gotsnowcov,  &
             gotsoilt1,  gotsoilt2,  gott2,      gotta,      gotterrain,  &
             gottke,     gotu10,     gotua,      gotust,     gotv10,      &
             gotva,      gotveg,     gotw2,      gotwa,      gotwg,       &
             gotwr,      gotznt,     gotzpbl

  ! Define roughness length and albedo as functions of land use and season
  ! since they typically are not part of MM5v2 output.

  REAL, PARAMETER :: albd13sum ( 13 ) = &  ! summer values [dec]
    (/  0.18,  0.17,  0.19,  0.16,  0.12,  0.14,  0.08,  0.14,   &
        0.25,  0.15,  0.55,  0.12,  0.20 /)

  REAL, PARAMETER :: albd13win ( 13 ) = &  ! winter values [dec]
    (/  0.18,  0.23,  0.23,  0.17,  0.12,  0.14,  0.08,  0.14,   &
        0.25,  0.70,  0.70,  0.12,  0.20 /)

  REAL, PARAMETER :: albd24sum ( 24 ) = &  ! summer values [dec]
    (/  0.18,  0.17,  0.18,  0.18,  0.18,  0.16,  0.19,  0.22,   &
        0.20,  0.20,  0.16,  0.14,  0.12,  0.12,  0.13,  0.08,   &
        0.14,  0.14,  0.25,  0.15,  0.15,  0.15,  0.25,  0.55 /)

  REAL, PARAMETER :: albd24win ( 24 ) = &  ! winter values [dec]
    (/  0.18,  0.23,  0.23,  0.23,  0.23,  0.20,  0.23,  0.25,   &
        0.24,  0.20,  0.17,  0.15,  0.12,  0.12,  0.14,  0.08,   &
        0.14,  0.14,  0.25,  0.60,  0.50,  0.55,  0.70,  0.70 /)

  REAL, PARAMETER :: sfz013sum ( 13 ) = &  ! summer values [cm]
    (/ 50.0,  15.0,  12.0,  50.0,  50.0,  40.0,   0.01, 20.0,    &
       10.0,  10.0,   5.0,  50.0,  15.0 /)

  REAL, PARAMETER :: sfz013win ( 13 ) = &  ! winter values [cm]
    (/ 50.0,   5.0,  10.0,  50.0,  50.0,  40.0,   0.01, 20.0,    &
       10.0,  10.0,   5.0,  50.0,  15.0 /)

  REAL, PARAMETER :: sfz024sum ( 24 ) = &  ! summer values [cm]
    (/ 50.0,  15.0,  15.0,  15.0,  14.0,  20.0,  12.0,  10.0,    &
       11.0,  15.0,  50.0,  50.0,  50.0,  50.0,  50.0,   0.01,   &
       20.0,  40.0,  10.0,  10.0,  30.0,  15.0,  10.0,   5.0 /)

  REAL, PARAMETER :: sfz024win ( 24 ) = &  ! winter values [cm]
    (/ 50.0,   5.0,   5.0,   5.0,   5.0,  20.0,  10.0,  10.0,    &
       10.0,  15.0,  50.0,  50.0,  50.0,  50.0,  50.0,   0.01,   &
       20.0,  40.0,  10.0,  10.0,  30.0,  15.0,   5.0,   5.0 /)

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f6000 = "(4x, a,     f11.4, 2x, a)"
  CHARACTER(LEN=256), PARAMETER :: f6100 = "(4x, a,     i11,   2x, a)"
  CHARACTER(LEN=256), PARAMETER :: f6200 = "(4x, a, i2, f12.4, 2x, a)"

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR READING FILE, UNIT = ', i3, &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3,&
    & /, 1x, '***   IFLAG = ', i3, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3, &
    & /, 1x, '***   VARIABLE = SMALL HEADER', &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3, &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   FOUND VARIABLE ', a, &
    & /, 1x, '***   BUT ARRAY DIMENSIONS DO NOT MATCH', &
    & /, 1X, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9450 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   INVALID MM5v3 DATA FLAG', &
    & /, 1x, '***   IFLAG = ', i4, &
    & /, 1X, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9500 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a, &
    & /, 1x, '***   COULD NOT FIND INPUT MET ON UNIT ', i3, &
    & /, 1x, '***   FILE MAY NOT EXIST', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9600 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING MM5 FILE ON UNIT ', i3, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9700 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   MM5 FILES DO NOT SEEM TO BE SAME GRID', &
    & /, 1x, '***   CHECK FORTRAN UNITS ', i3, ' AND ', i3, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9750 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   MAXIMUM VALUE OF VEGOLD ARRAY IS 0.0', &
    & /, 1x, '***   NEED A NEW SOLUTION TO P-X BUG', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9800 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   MM5 FILES MAY HAVE A DISCONTINUITY', &
    & /, 1x, '***   CHECK FORTRAN UNITS ', i3, ' AND ', i3, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9900 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND ARRAY ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9925 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND ARRAY ', a, &
    & /, 1x, '***   WILL SET FROM LOOKUP TABLE', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9950 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND ARRAY ', a, &
    & /, 1x, '***   WILL SET TO 0.0 FOR ALL GRID CELLS', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9975 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND ARRAY ', a, &
    & /, 1x, '***   WILL DEFINE FROM OTHER FIELDS LATER', &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( bhi (numvalsi, numprogs) )
  ALLOCATE ( bhr (numvalsr, numprogs) )

  IF ( .NOT. ALLOCATED ( dum2d ) ) ALLOCATE ( dum2d (nx, ny) )  ! (X,Y)

!-------------------------------------------------------------------------------
! Initialize data capture flags.  Need to do this here (rather than on
! declaration line) since Sun seems to default all variables to "SAVE".
! That is, on Sun, the variables will not be re-initialized from the
! declaration line on subsequent calls; they hold the value from the last call.
!-------------------------------------------------------------------------------

  newtime = .TRUE.


  IF ( first ) THEN  ! Need to read time-invariant fields.

    gotalbedo  = .FALSE.  ;  gotlanduse = .FALSE.  ;  gotlatcrs  = .FALSE.
    gotloncrs  = .FALSE.  ;  gotmapcrs  = .FALSE.  ;  gotmapdot  = .FALSE.
    gotsigmah  = .FALSE.  ;  gotterrain = .FALSE.  ;  gotznt     = .FALSE.

  ELSE  ! Already stored time-invariant fields, and do not need to re-store.

    gotalbedo  = .TRUE.   ;  gotlanduse = .TRUE.   ;  gotlatcrs  = .TRUE.
    gotloncrs  = .TRUE.   ;  gotmapcrs  = .TRUE.   ;  gotmapdot  = .TRUE.
    gotsigmah  = .TRUE.   ;  gotterrain = .TRUE.   ;  gotznt     = .TRUE. 

  ENDIF


  ! Need to read these time-variant fields each time.

  gotglw     = .FALSE.  ;  gotgroundt = .FALSE.  ;  gothfx     = .FALSE.
  gotpp      = .FALSE.  ;  gotpsa     = .FALSE.  ;  gotqca     = .FALSE.
  gotqfx     = .FALSE.  ;  gotqga     = .FALSE.  ;  gotqia     = .FALSE.
  gotqra     = .FALSE.  ;  gotqsa     = .FALSE.  ;  gotqva     = .FALSE.
  gotraincon = .FALSE.  ;  gotrainnon = .FALSE.  ;  gotrgrnd   = .FALSE.
  gotseaice  = .FALSE.  ;  gotsnowcov = .FALSE.  ;  gotta      = .FALSE.
  gotua      = .FALSE.  ;  gotust     = .FALSE.  ;  gotva      = .FALSE.
  gotwa      = .FALSE.  ;  gotzpbl    = .FALSE.  

  IF ( ift2m ) THEN  ! expecting to read 2-m temperature
    gott2      = .FALSE.
  ELSE
    gott2      = .TRUE.
  ENDIF

  IF ( ifq2m ) THEN  ! expecting to read 2-m mixing ratio
    gotq2      = .FALSE.
  ELSE
    gotq2      = .TRUE.
  ENDIF

  IF ( ifw10m ) THEN  ! expecting to read 10-m wind components
    gotu10     = .FALSE.
    gotv10     = .FALSE.
  ELSE
    gotu10     = .TRUE.
    gotv10     = .TRUE.
  ENDIF

  IF ( iflai ) THEN  ! expecting to read leaf area index
    gotlai     = .FALSE.
  ELSE
    gotlai     = .TRUE.
  ENDIF

  IF ( ifmol ) THEN  ! expecting to read Monin-Obukhov length
    gotmol     = .FALSE.
  ELSE
    gotmol     = .TRUE.
  ENDIF

  IF ( ifresist ) THEN  ! expecting to read aerodynamic and stomatal resistances
    gotra      = .FALSE.
    gotrstom   = .FALSE.
  ELSE
    gotra      = .TRUE.
    gotrstom   = .TRUE.
  ENDIF

  IF ( ifveg ) THEN  ! expecting to read vegetation fraction
    gotveg     = .FALSE.
  ELSE
    gotveg     = .TRUE.
  ENDIF

  IF ( ifwr ) THEN  ! expecting to read canopy wetness
    gotwr      = .FALSE.
  ELSE
    gotwr      = .TRUE.
  ENDIF

  IF ( ifsoil ) THEN  ! expecting to read soil moisture, temperature, and type
    gotisltyp  = .FALSE.
    gotsoilt1  = .FALSE.
    gotsoilt2  = .FALSE.
    gotw2      = .FALSE.
    gotwg      = .FALSE.
  ELSE  ! These variables are not in output and memory was not allocated
    gotisltyp  = .TRUE.
    gotsoilt1  = .TRUE.
    gotsoilt2  = .TRUE.
    gotw2      = .TRUE.
    gotwg      = .TRUE.
  ENDIF

  IF ( met_soil_lsm == 3 ) THEN  ! Pleim-Xiu LSM; time-varying albedo and z0
    gotalbedo  = .FALSE.
    gotznt     = .FALSE.
  ELSE IF ( met_soil_lsm == 2 ) THEN  ! NOAH LSM; time-varying albedo
    gotalbedo  = .FALSE.
  ENDIF

  IF ( iftke ) THEN  ! expecting to read turbulent kinetic energy
    gottke     = .FALSE.
  ELSE
    gottke     = .TRUE.
  ENDIF

!-------------------------------------------------------------------------------
! Set up print statements.
!-------------------------------------------------------------------------------

  k1 = nz / 5
  k2 = MOD(nz, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,4x,a,5(2x,f9.4)," // str1 // "(/,13x,5(2x,f9.4)),/,13x,"   &
         &    // str2 // "(2x,f9.4))"
    ELSE
      ifmt1 = "(/,4x,a,5(2x,f9.4)," // str1 // "(/,13x,5(2x,f9.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,4x,a,5(2x,f9.4),/,13x," // str2 // "(2x,f9.4))"
    ELSE
      ifmt1 = "(/,4x,a,5(2x,f9.4))"
    ENDIF
  ENDIF

  k1 = (nz+1) / 5
  k2 = MOD(nz+1, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1a = "(/,4x,a,5(2x,f9.4)," // str1 // "(/,13x,5(2x,f9.4)),/,13x,"   &
         &     // str2 // "(2x,f9.4))"
    ELSE
      ifmt1a = "(/,4x,a,5(2x,f9.4)," // str1 // "(/,13x,5(2x,f9.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1a = "(/,4x,a,5(2x,f9.4),/,13x," // str2 // "(2x,f9.4))"
    ELSE
      ifmt1a = "(/,4x,a,5(2x,f9.4))"
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Read MM5v3 data for this domain.  Note that big header character information
! (BHIC and BHRC) is not read and not used here.  Invert vertical index so that
! k=1 at the surface and k=nz at the model top.
!-------------------------------------------------------------------------------

  keep    = 0
  newfile = .FALSE.

  findtime: DO

    var = 'IFLAG    '
    READ (iutmm, IOSTAT=istat) iflag

    IF ( istat > 0 ) THEN  ! error on read

      WRITE (*,f9000) TRIM(pname), iutmm, TRIM(var), istat
      CALL graceful_stop (pname)

    ELSE IF ( istat < 0 ) THEN  ! end-of-file reached

      ! Try to open another input file to get the time period we need.

      CLOSE (iutmm)
      PRINT*, '+++++ End-of-file marker found on unit ', iutmm

      iutmm = iutmm + 1
      PRINT*, '+++++ Attempting to open unit ', iutmm

      mmfile = TRIM( file_mm(iutmm-iutmmi+1) )
      INQUIRE ( FILE=mmfile, EXIST=ifmm )
      IF ( .NOT. ifmm ) THEN
        WRITE (*,f9500) TRIM(pname), mcip_now, iutmm
        CALL graceful_stop (pname)
      ENDIF

      OPEN (UNIT=iutmm, FILE=mmfile, FORM='UNFORMATTED', STATUS='OLD',  &
            IOSTAT=istat)

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9600) TRIM(pname), iutmm, istat
        CALL graceful_stop (pname)
      ENDIF

      newfile = .TRUE.
      CYCLE findtime

    ENDIF

    IF ( iflag == 0 ) THEN

      var = 'BHI & BHR'
      READ (iutmm, IOSTAT=istat) bhi, bhr

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutmm, TRIM(var), istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
        WRITE (*,f9100) TRIM(pname), iutmm, iflag, istat
        CALL graceful_stop (pname)
      ENDIF

      IF ( newfile ) THEN

        ! Check to see if the grids and options are the same.

        ok = ok .AND. ( ny             == bhi(16,1)  )
        ok = ok .AND. ( nx             == bhi(17,1)  )
        ok = ok .AND. ( nz             == bhi(12,11) )
        ok = ok .AND. ( met_nycoarse   == bhi(5,1)   )
        ok = ok .AND. ( met_nxcoarse   == bhi(6,1)   )
        ok = ok .AND. ( met_mapproj    == bhi(7,1)   )
        ok = ok .AND. ( met_gratio     == bhi(20,1)  )
        ok = ok .AND. ( met_y_centd    == bhr(2,1)   )
        ok = ok .AND. ( met_x_centd    == bhr(3,1)   )
        ok = ok .AND. ( met_cone_fac   == bhr(4,1)   )
        ok = ok .AND. ( met_tru1       == bhr(5,1)   )
        ok = ok .AND. ( met_tru2       == bhr(6,1)   )

        SELECT CASE ( met_mapproj )
          CASE (1)  ! Lambert conformal
            ok = ok .AND. ( met_p_alp_d == MIN(bhr(5,1),bhr(6,1)) )
            ok = ok .AND. ( met_p_bet_d == MAX(bhr(5,1),bhr(6,1)) )
            ok = ok .AND. ( met_p_gam_d == bhr(3,1) )
          CASE (2)  ! polar stereographic
            ok = ok .AND. ( met_p_bet_d == bhr(5,1) )
            ok = ok .AND. ( met_p_gam_d == bhr(3,1) )
          CASE (3)  ! Mercator
            ok = ok .AND. ( met_p_gam_d == bhr(7,1) )
        END SELECT

        ok = ok .AND. ( met_resoln     == bhr(9,1)   )
        ok = ok .AND. ( met_y_11       == bhr(10,1)  )
        ok = ok .AND. ( met_x_11       == bhr(11,1)  )
        ok = ok .AND. ( met_lu_water   == bhi(23,1)  )
        ok = ok .AND. ( met_lw_rad     == bhi(1,13)  )
        ok = ok .AND. ( met_cumulus    == bhi(2,13)  )
        ok = ok .AND. ( met_expl_moist == bhi(3,13)  )
        ok = ok .AND. ( met_pbl        == bhi(4,13)  )
        ok = ok .AND. ( met_soil_lsm   == bhi(5,13)  )
        ok = ok .AND. ( met_tapfrq     == bhr(4,12)  )
        ok = ok .AND. ( met_ptop       == bhr(2,2)   )
        ok = ok .AND. ( met_p00        == bhr(2,5)   )
        ok = ok .AND. ( met_ts0        == bhr(3,5)   )
        ok = ok .AND. ( met_tlp        == bhr(4,5)   )
        ok = ok .AND. ( met_tiso       == bhr(5,5)   )

        IF ( .NOT. ok ) THEN
          WRITE (*,f9700) TRIM(pname), iutmm-1, iutmm
          CALL graceful_stop (pname)
        ENDIF

        ! If the new file is not a continuation of a previous run
        ! (i.e., "restart" or "split"), then stop since there is
        ! a physical discontinuity.

        met_restart = bhi(1,12)
        buffrq      = bhr(5,12)
        IF ( .NOT. ( ( buffrq > 0.0 ) .AND. ( buffrq >= met_tapfrq ) ) .AND.  &
             ( met_restart /= 1 ) ) THEN
          WRITE (*,f9800) TRIM(pname), iutmm-1, iutmm
          CALL graceful_stop (pname)
        ENDIF

        newfile = .FALSE.

      ENDIF

    ELSE IF ( iflag == 1 ) THEN

      var = 'SM HEADER'
      READ (iutmm, IOSTAT=istat) ndim, start_index, end_index, timeout,  &
            staggering, ordering, currentdate, vname, units, description

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutmm, TRIM(var), istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
        WRITE (*,f9200) TRIM(pname), iutmm, istat
        CALL graceful_stop (pname)
      ENDIF

      currentdate(11:11) = "-"  ! switch "_" to "-" for consistency

      IF ( first ) THEN
        startdate = currentdate(1:19)
      ENDIF

      IF ( newtime ) THEN

        WRITE (*,'(/,a,2x,f15.5," Hours"/)') currentdate, timeout/60.0
        newtime = .FALSE.

        ! Compare CURRENTDATE to CTM_NOW to see if this meteorology output
        ! time should be processed.  Since MM5 output is processed as REAL
        ! numbers within MM5, there is occasional drift in the output times,
        ! so they often are not exactly on the hour (for example).  To overcome
        ! this, a time tolerance is set so the appropriate times can be
        ! captured.

        CALL geth_idts (currentdate(1:19), mcip_now(1:19), idtsec)
        IF ( ABS(idtsec) <= ttol ) THEN
          keep = 1
        ELSE
          keep = 0
        ENDIF

      ENDIF

      IF ( ndim == 1 ) THEN
        ALLOCATE ( data_in(end_index(1), 1, 1, 1) )
      ELSE IF ( ndim == 2 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), 1, 1) )
      ELSE IF ( ndim == 3 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), end_index(3), 1) )
      ELSE IF ( ndim == 4 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), end_index(3),  &
                           end_index(4)))
      ENDIF

      var = vname
      READ (iutmm, IOSTAT=istat) data_in

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutmm, TRIM(var), istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
        WRITE (*,f9300) TRIM(pname), iutmm, TRIM(var), istat
        CALL graceful_stop (pname)
      ENDIF

      IF ( ( vname == 'RAIN CON ' ) .AND. ( .NOT. gotraincon ) ) THEN
        IF ( ( SIZE(raincon,1) == SIZE(data_in,2) ) .AND.  &
             ( SIZE(raincon,2) == SIZE(data_in,1) ) ) THEN
          ! accumulated convective precip [cm]
          DO j = start_index(2), end_index(2)
            DO i = start_index(1), end_index(1)
              raincon(j,i) = data_in(i,j,1,1)
            ENDDO
          ENDDO
          dum2d   = raincon
          raincon = MAX(0.0, (raincon - rcold ))  ! incremental total in cm
          rcold   = dum2d  ! accumulated total to subtract from next interval
          IF ( keep == 1 ) THEN
            gotraincon = .TRUE.
            WRITE (*,f6000) 'RAINCON  ', raincon(lprt_metx, lprt_mety),  &
                            TRIM(units)
          ENDIF
        ELSE
          WRITE (*,f9400) TRIM(pname), TRIM(var)
          CALL graceful_stop (pname)
        ENDIF
      ENDIF

      IF ( ( vname == 'RAIN NON ' ) .AND. ( .NOT. gotrainnon ) ) THEN
        IF ( ( SIZE(rainnon,1) == SIZE(data_in,2) ) .AND.  &
             ( SIZE(rainnon,2) == SIZE(data_in,1) ) ) THEN
          ! accumulated nonconvective precip [cm]
          DO j = start_index(2), end_index(2)
            DO i = start_index(1), end_index(1)
              rainnon(j,i) = data_in(i,j,1,1)
            ENDDO
          ENDDO
          dum2d   = rainnon
          rainnon = MAX(0.0, (rainnon - rnold ))  ! incremental total in cm
          rnold   = dum2d  ! accumulated total to subtract from next interval
          IF ( keep == 1 ) THEN
            gotrainnon = .TRUE.
            WRITE (*,f6000) 'RAINNON  ', rainnon(lprt_metx, lprt_mety),  &
                            TRIM(units)
          ENDIF
        ELSE
          WRITE (*,f9400) TRIM(pname), TRIM(var)
          CALL graceful_stop (pname)
        ENDIF
      ENDIF

      keeptime: IF ( keep == 1 ) THEN

        IF ( ( vname == 'U        ' ) .AND. ( .NOT. gotua ) ) THEN
          IF ( ( SIZE(ua,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(ua,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(ua,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz  ! u-component of horizontal wind [m s^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  ua(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotua = .TRUE.
            WRITE (*,ifmt1) 'UA       ', ua(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'V        ' ) .AND. ( .NOT. gotva ) ) THEN
          IF ( ( SIZE(va,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(va,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(va,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz  ! v-component of horizontal wind [m s^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  va(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotva = .TRUE.
            WRITE (*,ifmt1) 'VA       ', va(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'T        ' ) .AND. ( .NOT. gotta ) ) THEN
          IF ( ( SIZE(ta,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(ta,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(ta,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz   ! temperature [K]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  ta(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotta = .TRUE.
            WRITE (*,ifmt1) 'TA       ', ta(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'Q        ' ) .AND. ( .NOT. gotqva ) ) THEN
          IF ( ( SIZE(qva,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(qva,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(qva,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz  ! mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qva(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqva = .TRUE.
            WRITE (*,ifmt1) 'QVA      ', qva(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'CLW      ' ) .AND. ( .NOT. gotqca ) ) THEN
          IF ( ( SIZE(qca,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(qca,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(qca,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1,nz  ! cloud water mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qca(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqca = .TRUE.
            WRITE (*,ifmt1) 'QCA      ', qca(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'RNW      ' ) .AND. ( .NOT. gotqra ) ) THEN
          IF ( ( SIZE(qra,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(qra,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(qra,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz  ! rain water mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qra(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqra = .TRUE.
            WRITE (*,ifmt1) 'QRA      ', qra(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'ICE      ' ) .AND. ( .NOT. gotqia ) ) THEN
          IF ( ( SIZE(qia,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(qia,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(qia,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz  ! cloud ice mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qia(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqia = .TRUE.
            WRITE (*,ifmt1) 'QIA      ', qia(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SNOW     ' ) .AND. ( .NOT. gotqsa ) ) THEN
          IF ( ( SIZE(qsa,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(qsa,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(qsa,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz  ! snow mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qsa(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqsa = .TRUE.
            WRITE (*,ifmt1) 'QSA      ', qsa(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'GRAUPEL  ' ) .AND. ( .NOT. gotqga ) ) THEN
          IF ( ( SIZE(qga,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(qga,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(qga,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz  ! graupel mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qga(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqga = .TRUE.
            WRITE (*,ifmt1) 'QGA      ', qga(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'W        ' ) .AND. ( .NOT. gotwa ) ) THEN
          IF ( ( SIZE(wa,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(wa,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(wa,3) == SIZE(data_in,3) ) ) THEN
            lbnd = LBOUND(wa,3)  ! first element is mapped correctly (0 or 1)
            ubnd = UBOUND(wa,3)  ! last element is mapped correctly (nz or nz+1)
            DO k = lbnd, ubnd  ! vertical component of wind [m s^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  wa(j,i,k) = data_in(i,j,nz+1-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotwa = .TRUE.
            WRITE (*,ifmt1a) 'WA       ', wa(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'TKE      ' ) .AND. ( .NOT. gottke ) ) THEN
          IF ( ( SIZE(tke,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(tke,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(tke,3) == SIZE(data_in,3) ) ) THEN
            lbnd = LBOUND(tke,3)  ! first element is mapped correctly (0 or 1)
            ubnd = UBOUND(tke,3)  ! last element is mapped correctly (nz or nz+1)
            IF ( iftkef ) THEN  ! TKE on full-levels
              DO k = lbnd, ubnd  ! turbulent kinetic energy [J kg^-1]
                DO j = start_index(2), end_index(2)
                  DO i = start_index(1), end_index(1)
                    tke(j,i,k) = data_in(i,j,nz+1-k+1,1)
                  ENDDO
                ENDDO
              ENDDO
            ELSE  ! TKE on half-layers
              DO k = lbnd, ubnd  ! turbulent kinetic energy [J kg^-1]
                DO j = start_index(2), end_index(2)
                  DO i = start_index(1), end_index(1)
                    tke(j,i,k) = data_in(i,j,nz-k+1,1)
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
            gottke = .TRUE.
            IF ( iftkef ) THEN  ! TKE on full-levels
              WRITE (*,ifmt1a) 'TKE      ', tke(lprt_metx,lprt_mety,:)
            ELSE  ! TKE on half-layers
              WRITE (*,ifmt1)  'TKE      ', tke(lprt_metx,lprt_mety,:)
            ENDIF
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'PP       ' ) .AND. ( .NOT. gotpp ) ) THEN
          IF ( ( SIZE(pp,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(pp,2) == SIZE(data_in,1) ) .AND.  &
               ( SIZE(pp,3) == SIZE(data_in,3) ) ) THEN
            DO k = 1, nz  ! pressure perturbation [Pa]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  pp(j,i,k) = data_in(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotpp = .TRUE.
            WRITE (*,ifmt1) 'PP       ', pp(lprt_metx,lprt_mety,:)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'PSTARCRS ' ) .AND. ( .NOT. gotpsa ) ) THEN
          IF ( ( SIZE(psa,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(psa,2) == SIZE(data_in,1) ) ) THEN
            ! (reference) surface pressure minus ptop [Pa]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                psa(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotpsa = .TRUE.
            WRITE (*,f6000) 'PSA      ', psa(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'GROUND T ' ) .AND. ( .NOT. gotgroundt ) ) THEN
          IF ( ( SIZE(groundt,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(groundt,2) == SIZE(data_in,1) ) ) THEN
            ! ground temperature [K]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                groundt(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotgroundt = .TRUE.
            WRITE (*,f6000) 'GROUNDT  ', groundt(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'TERRAIN  ' ) .AND. ( .NOT. gotterrain ) ) THEN
          IF ( ( SIZE(terrain,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(terrain,2) == SIZE(data_in,1) ) ) THEN
            ! terrain elevation [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                terrain(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotterrain = .TRUE.
            WRITE (*,f6000) 'TERRAIN  ', terrain(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'MAPFACCR ' ) .AND. ( .NOT. gotmapcrs ) ) THEN
          IF ( ( SIZE(mapcrs,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(mapcrs,2) == SIZE(data_in,1) ) ) THEN
            ! map scale factor (cross) [dim'less]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                mapcrs(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotmapcrs = .TRUE.
            WRITE (*,f6000) 'MAPCRS   ', mapcrs(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'MAPFACDT ' ) .AND. ( .NOT. gotmapdot ) ) THEN
          IF ( ( SIZE(mapdot,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(mapdot,2) == SIZE(data_in,1) ) ) THEN
            ! map scale factor (dot) [dim'less]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                mapdot(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotmapdot = .TRUE.
            WRITE (*,f6000) 'MAPDOT   ', mapdot(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'LATITCRS ' ) .AND. ( .NOT. gotlatcrs ) ) THEN
          IF ( ( SIZE(latcrs,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(latcrs,2) == SIZE(data_in,1) ) ) THEN
            ! latitude (cross) [degree]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                latcrs(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotlatcrs = .TRUE.
            WRITE (*,f6000) 'LATCRS   ', latcrs(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'LONGICRS ' ) .AND. ( .NOT. gotloncrs ) ) THEN
          IF ( ( SIZE(loncrs,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(loncrs,2) == SIZE(data_in,1) ) ) THEN
            ! longitude (cross) [degree]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                loncrs(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotloncrs = .TRUE.
            WRITE (*,f6000) 'LONCRS   ', loncrs(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'LAND USE ' ) .AND. ( .NOT. gotlanduse ) ) THEN
          IF ( ( SIZE(landuse,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(landuse,2) == SIZE(data_in,1) ) ) THEN
            ! land use [category]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                landuse(j,i) = NINT(data_in(i,j,1,1))
              ENDDO
            ENDDO
            gotlanduse = .TRUE.
            WRITE (*,f6100) 'LANDUSE  ', landuse(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SNOWCOVR ' ) .AND. ( .NOT. gotsnowcov ) .AND.  &
             ( ( met_snow_opt == 0 ) .OR. ( met_snow_opt == 1 ) ) ) THEN
          IF ( ( SIZE(snowcovr,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(snowcovr,2) == SIZE(data_in,1) ) ) THEN
            ! snow cover [1= yes, 0=no]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                snowcovr(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotsnowcov = .TRUE.
            WRITE (*,f6000) 'SNOWCOVR ', snowcovr(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SEAICE   ' ) .AND. ( .NOT. gotseaice ) ) THEN
          IF ( ( SIZE(seaice,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(seaice,2) == SIZE(data_in,1) ) ) THEN
            ! sea ice [flag]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                seaice(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotseaice = .TRUE.
            WRITE (*,f6000) 'SEAICE   ', seaice(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'WEASD    ' ) .AND. ( .NOT. gotsnowcov ) .AND.  &
             ( met_snow_opt == 2 ) ) THEN
          IF ( ( SIZE(snowcovr,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(snowcovr,2) == SIZE(data_in,1) ) ) THEN
            ! snow cover [mm]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                snowcovr(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            ! Convert liquid equiv snow depth into yes/no
            WHERE ( snowcovr >= 1.0 )  ! [mm], see MM5's rdinit.F
              snowcovr = 1.0
            ELSEWHERE
              snowcovr = 0.0
            END WHERE
            gotsnowcov = .TRUE.
            WRITE (*,f6000) 'SNOWCOVR ', snowcovr(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'PBL HGT  ' ) .AND. ( .NOT. gotzpbl ) ) THEN
          IF ( ( SIZE(zpbl,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(zpbl,2) == SIZE(data_in,1) ) ) THEN
            ! PBL height [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                zpbl(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotzpbl = .TRUE.
            WRITE (*,f6000) 'ZPBL     ', zpbl(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SHFLUX   ' ) .AND. ( .NOT. gothfx ) ) THEN
          IF ( ( SIZE(hfx,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(hfx,2) == SIZE(data_in,1) ) ) THEN
            ! sensible heat flux [W m^-2]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                hfx(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gothfx = .TRUE.
            WRITE (*,f6000) 'HFX      ', hfx(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'LHFLUX   ' ) .AND. ( .NOT. gotqfx ) ) THEN
          IF ( ( SIZE(qfx,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(qfx,2) == SIZE(data_in,1) ) ) THEN
            ! latent heat flux [W m^-2]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                qfx(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotqfx = .TRUE.
            WRITE (*,f6000) 'QFX      ', qfx(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'UST      ' ) .AND. ( .NOT. gotust ) ) THEN
          IF ( ( SIZE(ust,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(ust,2) == SIZE(data_in,1) ) ) THEN
            ! frictional velocity [m s^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                ust(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotust = .TRUE.
            WRITE (*,f6000) 'UST      ', ust(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SWDOWN   ' ) .AND. ( .NOT. gotrgrnd ) ) THEN
          IF ( ( SIZE(rgrnd,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(rgrnd,2) == SIZE(data_in,1) ) ) THEN
            ! surface downward SW radiation [W m^-2]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                rgrnd(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotrgrnd = .TRUE.
            WRITE (*,f6000) 'RGRND    ', rgrnd(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'LWDOWN   ' ) .AND. ( .NOT. gotglw ) ) THEN
          IF ( ( SIZE(glw,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(glw,2) == SIZE(data_in,1) ) ) THEN
            ! surface downward LW radiation [W m^-2]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                glw(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotglw = .TRUE.
            WRITE (*,f6000) 'GLW      ', glw(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SOIL T 1 ' ) .AND. ( .NOT. gotsoilt1 ) ) THEN
          IF ( ( SIZE(soilt1,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(soilt1,2) == SIZE(data_in,1) ) ) THEN
            ! (surface) soil temperature in layer 1 [K]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                soilt1(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotsoilt1 = .TRUE.
            WRITE (*,f6000) 'SOILT1   ', soilt1(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SOIL T 2 ' ) .AND. ( .NOT. gotsoilt2 ) ) THEN
          IF ( ( SIZE(soilt2,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(soilt2,2) == SIZE(data_in,1) ) ) THEN
            ! (deep) soil temperature in layer 2 [K]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                soilt2(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotsoilt2 = .TRUE.
            WRITE (*,f6000) 'SOILT2   ', soilt2(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SOIL M 1 ' ) .AND. ( .NOT. gotwg ) ) THEN
          IF ( ( SIZE(wg,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(wg,2) == SIZE(data_in,1) ) ) THEN
            ! (surface) soil moisture in layer 1 [m^3 m^-3]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                wg(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotwg = .TRUE.
            WRITE (*,f6000) 'WG       ', wg(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SOIL M 2 ' ) .AND. ( .NOT. gotw2 ) ) THEN
          IF ( ( SIZE(w2,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(w2,2) == SIZE(data_in,1) ) ) THEN
            ! soil moisture in layer 2 [m^3 m^-3]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                w2(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotw2 = .TRUE.
            WRITE (*,f6000) 'W2       ', w2(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'CANOPYM  ' ) .AND. ( .NOT. gotwr ) ) THEN
          IF ( ( SIZE(wr,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(wr,2) == SIZE(data_in,1) ) ) THEN
            ! canopy moisture content [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                wr(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotwr = .TRUE.
            WRITE (*,f6000) 'WR       ', wr(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'ALBD     ' ) .AND. ( .NOT. gotalbedo ) .AND.  &
             ( met_soil_lsm /= 2 ) .AND. ( met_soil_lsm /= 3 ) ) THEN
          IF ( .NOT. ALLOCATED ( albd ) )  &
            ALLOCATE ( albd (SIZE(data_in,1), SIZE(data_in,2)) )
          albd = data_in(:,:,1,1)  ! surface albedo [percent]
          gotalbedo = .TRUE.
        ENDIF

        IF ( ( vname == 'SFZ0     ' ) .AND. ( .NOT. gotznt ) .AND.  &
             ( met_soil_lsm /= 3 ) ) THEN
          IF ( .NOT. ALLOCATED ( sfz0 ) )  &
            ALLOCATE ( sfz0 (SIZE(data_in,1), SIZE(data_in,2)) )
          sfz0 = data_in(:,:,1,1) / 100.0  ! roughness length [cm --> m]
          gotznt = .TRUE.
        ENDIF

        IF ( ( vname == 'T2       ' ) .AND. ( .NOT. gott2 ) ) THEN
          IF ( ( SIZE(t2,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(t2,2) == SIZE(data_in,1) ) ) THEN
            ! 2-m temperature [K]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                t2(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gott2 = .TRUE.
            WRITE (*,f6000) 'T2       ', t2(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'Q2       ' ) .AND. ( .NOT. gotq2 ) ) THEN
          IF ( ( SIZE(q2,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(q2,2) == SIZE(data_in,1) ) ) THEN
            ! 2-m mixing ratio [kg kg^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                q2(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotq2 = .TRUE.
            WRITE (*,f6000) 'Q2       ', q2(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'U10      ' ) .AND. ( .NOT. gotu10 ) ) THEN
          IF ( ( SIZE(u10,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(u10,2) == SIZE(data_in,1) ) ) THEN
            ! 10-m u-component wind [m/s]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                u10(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotu10 = .TRUE.
            WRITE (*,f6000) 'U10      ', u10(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'V10      ' ) .AND. ( .NOT. gotv10 ) ) THEN
          IF ( ( SIZE(v10,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(v10,2) == SIZE(data_in,1) ) ) THEN
            ! 10-m v-component wind [m/s]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                v10(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotv10 = .TRUE.
            WRITE (*,f6000) 'V10      ', v10(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'M-O LENG ' ) .AND. ( .NOT. gotmol ) ) THEN
          IF ( ( SIZE(mol,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(mol,2) == SIZE(data_in,1) ) ) THEN
            ! Monin-Obukhov length [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                mol(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotmol = .TRUE.
            WRITE (*,f6000) 'MOL      ', mol(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'ALBEDO   ' ) .AND. ( .NOT. gotalbedo ) ) THEN
          IF ( ( SIZE(albedo,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(albedo,2) == SIZE(data_in,1) ) ) THEN
            ! surface albedo [fraction]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                albedo(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotalbedo = .TRUE.
            WRITE (*,f6000) 'ALBEDO   ', albedo(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'ALB      ' ) .AND. ( .NOT. gotalbedo ) ) THEN
          IF ( ( SIZE(albedo,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(albedo,2) == SIZE(data_in,1) ) ) THEN
            ! surface albedo [fraction]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                albedo(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotalbedo = .TRUE.
            WRITE (*,f6000) 'ALB      ', albedo(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'RA       ' ) .AND. ( .NOT. gotra ) ) THEN
          IF ( ( SIZE(ra,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(ra,2) == SIZE(data_in,1) ) ) THEN
            ! aerodynamic resistance [s m^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                ra(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotra = .TRUE.
            WRITE (*,f6000) 'RA       ', ra(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'RS       ' ) .AND. ( .NOT. gotrstom ) ) THEN
          IF ( ( SIZE(rstom,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(rstom,2) == SIZE(data_in,1) ) ) THEN
            ! surface resistance [s m^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                rstom(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotrstom = .TRUE.
            WRITE (*,f6000) 'RSTOM    ', rstom(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'LAI      ' ) .AND. ( .NOT. gotlai ) ) THEN
          IF ( ( SIZE(lai,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(lai,2) == SIZE(data_in,1) ) ) THEN
            ! leaf area index [area area^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                lai(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotlai = .TRUE.
            WRITE (*,f6000) 'LAI      ', lai(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'VEGFRC   ' ) .AND. ( .NOT. gotveg ) ) THEN
          IF ( ( SIZE(veg,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(veg,2) == SIZE(data_in,1) ) ) THEN
            ! vegetation coverage [fraction]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                veg(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            IF ( met_soil_lsm == 3 ) THEN  ! Pleim-Xiu LSM
              IF ( MAXVAL(veg) > 1.1 ) THEN  ! bad data; use previous hour
                IF ( ABS(MAXVAL(vegold)) < smallnum ) THEN
                  WRITE (*,f9750) TRIM(pname)
                  CALL graceful_stop (pname)
                ENDIF
                veg = vegold
              ELSE
                vegold = veg
              ENDIF
            ELSE IF ( met_soil_lsm == 2 ) THEN  ! NOAH LSM
              veg(:,:) = veg(:,:) * 0.01  ! percent -> decimal
            ENDIF
            gotveg = .TRUE.
            WRITE (*,f6000) 'VEG      ', veg(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'ZNT      ' ) .AND. ( .NOT. gotznt ) ) THEN
          IF ( ( SIZE(znt,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(znt,2) == SIZE(data_in,1) ) ) THEN
            ! roughness length [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                znt(j,i) = data_in(i,j,1,1)
              ENDDO
            ENDDO
            gotznt = .TRUE.
            WRITE (*,f6000) 'ZNT      ', znt(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'ISLTYP   ' ) .AND. ( .NOT. gotisltyp ) ) THEN
          IF ( ( SIZE(isltyp,1) == SIZE(data_in,2) ) .AND.  &
               ( SIZE(isltyp,2) == SIZE(data_in,1) ) ) THEN
            ! soil type [USDA category]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                isltyp(j,i) = NINT(data_in(i,j,1,1))
              ENDDO
            ENDDO
            gotisltyp = .TRUE.
            WRITE (*,f6100) 'ISLTYP   ', isltyp(lprt_metx,lprt_mety),  &
                            TRIM(units)
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
        ENDIF

        IF ( ( vname == 'SIGMAH   ' ) .AND. ( .NOT. gotsigmah ) ) THEN
          IF ( SIZE(sigmah) == SIZE(data_in,1) ) THEN
            sigmah = data_in(nz:1:-1,1,1,1)  ! vertical coordinate [half-sigma]
            gotsigmah = .TRUE.
          ELSE
            WRITE (*,f9400) TRIM(pname), TRIM(var)
            CALL graceful_stop (pname)
          ENDIF
          lbnd = LBOUND(sigmaf,1)  ! first element mapped correctly (0 or 1)
          ubnd = UBOUND(sigmaf,1)  ! last element mapped correctly (nz or nz+1)
          sigmaf(lbnd) = 1.0
          sigmaf(ubnd) = 0.0
          DO k = lbnd+1, ubnd-1
            km1 = k - 1
            sigmaf(k) = ( 2.0 * sigmah(km1) ) - sigmaf(km1)
          ENDDO
        ENDIF

      ENDIF keeptime

      DEALLOCATE ( data_in )

    ELSE IF ( iflag == 2 ) THEN

      newtime = .TRUE.
      IF ( keep == 1 ) EXIT findtime

    ELSE

      WRITE (*,f9450) TRIM(pname), iflag
      CALL graceful_stop (pname)

    ENDIF

  ENDDO findtime
   
!-------------------------------------------------------------------------------
! Make sure we collected the arrays we need.
!-------------------------------------------------------------------------------

  IF ( .NOT. gotua ) THEN
    WRITE (*,f9900) TRIM(pname), 'U'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotva ) THEN
    WRITE (*,f9900) TRIM(pname), 'V'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotta ) THEN
    WRITE (*,f9900) TRIM(pname), 'T'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotqva ) THEN
    WRITE (*,f9900) TRIM(pname), 'Q'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotqca ) THEN
    IF ( met_expl_moist < 4 ) THEN  ! MM5 without ice-phase scheme
      qca(:,:,:) = 0.0
      gotqca = .TRUE.
    ELSE
      WRITE (*,f9900) TRIM(pname), 'CLW'
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. gotqra ) THEN
    IF ( met_expl_moist < 4 ) THEN  ! MM5 without ice-phase scheme
      qra(:,:,:) = 0.0
      gotqra = .TRUE.
    ELSE
      WRITE (*,f9900) TRIM(pname), 'RNW'
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. gotqia ) THEN
    IF ( met_expl_moist < 5 ) THEN  ! MM5 without mixed-phase scheme
      qia(:,:,:) = 0.0
      gotqia = .TRUE.
    ELSE
      WRITE (*,f9900) TRIM(pname), 'ICE'
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. gotqsa ) THEN
    IF ( met_expl_moist < 5 ) THEN  ! MM5 without mixed-phase scheme
      qsa(:,:,:) = 0.0
      gotqsa = .TRUE.
    ELSE
      WRITE (*,f9900) TRIM(pname), 'SNOW'
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. gotqga ) THEN
    IF ( met_expl_moist < 6 ) THEN  ! MM5 without graupel
      qga(:,:,:) = 0.0
      gotqga = .TRUE.
    ELSE
      WRITE (*,f9900) TRIM(pname), 'GRAUPEL'
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. gotwa ) THEN
    WRITE (*,f9900) TRIM(pname), 'W'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gottke ) THEN
    WRITE (*,f9900) TRIM(pname), 'TKE'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotpp ) THEN
    WRITE (*,f9900) TRIM(pname), 'PP'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotpsa ) THEN
    WRITE (*,f9900) TRIM(pname), 'PSTARCRS'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotgroundt ) THEN
    WRITE (*,f9900) TRIM(pname), 'GROUND T'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotraincon ) THEN
    WRITE (*,f9900) TRIM(pname), 'RAIN CON'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotrainnon ) THEN
    WRITE (*,f9900) TRIM(pname), 'RAIN NON'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotterrain ) THEN
    WRITE (*,f9900) TRIM(pname), 'TERRAIN'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotmapcrs ) THEN
    WRITE (*,f9900) TRIM(pname), 'MAPFACCR'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotmapdot ) THEN
    WRITE (*,f9900) TRIM(pname), 'MAPFACDT'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotlatcrs ) THEN
    WRITE (*,f9900) TRIM(pname), 'LATITCRS'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotloncrs ) THEN
    WRITE (*,f9900) TRIM(pname), 'LONGICRS'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotlanduse ) THEN
    WRITE (*,f9900) TRIM(pname), 'LAND USE'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotsnowcov ) THEN
    WRITE (*,f9950) TRIM(pname), 'SNOW COVER'
    snowcovr(:,:) = 0.0
    gotsnowcov = .TRUE.
  ENDIF

  IF ( .NOT. gotseaice ) THEN
    WRITE (*,f9975) TRIM(pname), 'SEAICE'
    needseaice = .TRUE.
  ELSE
    needseaice = .FALSE.
  ENDIF

  IF ( .NOT. gotzpbl ) THEN
    WRITE (*,f9900) TRIM(pname), 'PBL HGT'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gothfx ) THEN
    WRITE (*,f9900) TRIM(pname), 'SHFLUX'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotqfx ) THEN
    WRITE (*,f9900) TRIM(pname), 'LHFLUX'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotust ) THEN
    WRITE (*,f9900) TRIM(pname), 'UST'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotrgrnd ) THEN
    WRITE (*,f9900) TRIM(pname), 'SWDOWN'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotsoilt1 ) THEN
    WRITE (*,f9900) TRIM(pname), 'SOIL T 1'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotsoilt2 ) THEN
    WRITE (*,f9900) TRIM(pname), 'SOIL T 2'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotwg ) THEN
    WRITE (*,f9900) TRIM(pname), 'SOIL M 1'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotw2 ) THEN
    WRITE (*,f9900) TRIM(pname), 'SOIL M 2'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotwr ) THEN
    WRITE (*,f9900) TRIM(pname), 'CANOPYM'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gott2 ) THEN
    WRITE (*,f9900) TRIM(pname), 'T2'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotq2 ) THEN
    WRITE (*,f9900) TRIM(pname), 'Q2'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotu10 ) THEN
    WRITE (*,f9900) TRIM(pname), 'U10'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotv10 ) THEN
    WRITE (*,f9900) TRIM(pname), 'V10'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotmol ) THEN
    WRITE (*,f9900) TRIM(pname), 'M-O LENG'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotalbedo ) THEN
    IF ( met_soil_lsm == 3 ) THEN
      WRITE (*,f9900) TRIM(pname), 'ALBEDO'
      CALL graceful_stop (pname)
    ELSE IF ( met_soil_lsm == 2 ) THEN
      WRITE (*,f9900) TRIM(pname), 'ALB'
      CALL graceful_stop (pname)
    ELSE
      WRITE (*,f9925) TRIM(pname), 'ALBD'
    ENDIF
  ENDIF

  IF ( .NOT. gotra ) THEN
    WRITE (*,f9900) TRIM(pname), 'RA'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotrstom ) THEN
    WRITE (*,f9900) TRIM(pname), 'RS'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotlai ) THEN
    WRITE (*,f9900) TRIM(pname), 'LAI'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotveg ) THEN
    WRITE (*,f9900) TRIM(pname), 'VEGFRC'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotznt ) THEN
    IF ( met_soil_lsm == 3 ) THEN
      WRITE (*,f9900) TRIM(pname), 'ZNT'
      CALL graceful_stop (pname)
    ELSE
      WRITE (*,f9925) TRIM(pname), 'SFZ0'
    ENDIF
  ENDIF

  IF ( .NOT. gotisltyp ) THEN
    WRITE (*,f9900) TRIM(pname), 'ISLTYP'
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. gotsigmah ) THEN
    WRITE (*,f9900) TRIM(pname), 'SIGMAH'
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Get latitude and longitude on dot points.  If potential vorticity is to be
! computed, then get Coriolis on cross points.  Then, if necessary, get
! roughness length and albedo from land use category.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    IF ( lpv > 0 ) THEN  ! get Coriolis on cross-points
      DO j = 1, ny
        DO i = 1, nx
          coriolis(i,j) = cori(latcrs(i,j))
        ENDDO
      ENDDO
    ENDIF

    ! Calculate latitude, longitude, and map-scale factors using offset
    ! distance of given grid point from center of domain.

    SELECT CASE ( met_mapproj )

      CASE (1)  ! Lambert conformal

        xoff = 0.0  ! dot-point grid: no offset from dot-point center value
        yoff = 0.0  ! dot-point grid: no offset from dot-point center value

        DO j = 1, ny
          DO i = 1, nx

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_lam (xxin, yyin, met_tru1, met_tru2, met_proj_clon,  &
                            met_ref_lat, latdot(i,j), londot(i,j))

            mapdot(i,j) = mapfac_lam (latdot(i,j), met_tru1, met_tru2)

          ENDDO
        ENDDO

        xoff = 0.0  ! U-face: no offset in X from dot-point center value
        yoff = 0.5  ! U-face: 0.5-cell offset in Y from dot-point center value

        DO j = 1, ny  ! do all of Y to fill array; last row is outside domain
          DO i = 1, nx

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_lam (xxin, yyin, met_tru1, met_tru2, met_proj_clon,  &
                            met_ref_lat, latu(i,j), lonu(i,j))

            mapu(i,j) = mapfac_lam (latu(i,j), met_tru1, met_tru2)

          ENDDO
        ENDDO

        xoff = 0.5  ! V-face: 0.5-cell offset in X from dot-point center value
        yoff = 0.0  ! V-face: no offset in Y from dot-point center value

        DO j = 1, ny
          DO i = 1, nx  ! do all of X to fill array; last col outside domain

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_lam (xxin, yyin, met_tru1, met_tru2, met_proj_clon,  &
                            met_ref_lat, latv(i,j), lonv(i,j))

            mapv(i,j) = mapfac_lam (latv(i,j), met_tru1, met_tru2)

          ENDDO
        ENDDO


      CASE (2)  ! polar stereographic

        nxm = nx - 1
        nym = ny - 1

        DO j = 1, ny
          DO i = 1, nx

            ii = MAX(i,nxm)
            jj = MAX(j,nym)

            im1 = MIN(i-1,1)
            jm1 = MIN(j-1,1)

            ! Use four-point interpolation here for latitude and longitude.
            ! Because CMAQ will never use outermost row and column from WRF
            ! due to location of CMAQ boundaries, inexact values in the
            ! outermost row and column will not matter.

            latdot(i,j) = ( latcrs(im1,jj)  + latcrs(ii,jj) +   &
                            latcrs(im1,jm1) + latcrs(ii,jm1) ) * 0.25

            londot(i,j) = ( loncrs(im1,jj)  + loncrs(ii,jj) +   &
                            loncrs(im1,jm1) + loncrs(ii,jm1) ) * 0.25

            mapdot(i,j) = mapfac_ps (latdot(i,j), met_tru1)

            ! Use linear interpolation here for latitude and longitude.
            ! Because CMAQ will never use outermost row and column from WRF
            ! due to location of CMAQ boundaries, inexact values in the
            ! outermost row and column will not matter.

            latu(i,j) = ( latcrs(im1,jj) + latcrs(ii,jj) ) * 0.5
            lonu(i,j) = ( loncrs(im1,jj) + loncrs(ii,jj) ) * 0.5
            mapu(i,j) = mapfac_ps (latu(i,j), met_tru1)

            latv(i,j) = ( latcrs(ii,jm1) + latcrs(ii,jj) ) * 0.5
            lonv(i,j) = ( loncrs(ii,jm1) + loncrs(ii,jj) ) * 0.5
            mapv(i,j) = mapfac_ps (latv(i,j), met_tru1)

          ENDDO
        ENDDO


      CASE (3)  ! Mercator

        xoff = 0.0  ! dot-point grid: no offset from dot-point center value
        yoff = 0.0  ! dot-point grid: no offset from dot-point center value

        DO j = 1, ny
          DO i = 1, nx

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_merc (xxin, yyin, met_proj_clon,  &
                             latdot(i,j), londot(i,j))

            mapdot(i,j) = mapfac_merc (latdot(i,j))

          ENDDO
        ENDDO

        xoff = 0.0  ! U-face: no offset in X from dot-point center value
        yoff = 0.5  ! U-face: 0.5-cell offset in Y from dot-point center value

        DO j = 1, ny  ! do all of Y to fill array; last row is outside domain
          DO i = 1, nx

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_merc (xxin, yyin, met_proj_clon,  &
                             latu(i,j), lonu(i,j))

            mapu(i,j) = mapfac_merc (latu(i,j))

          ENDDO
        ENDDO

        xoff = 0.5  ! V-face: 0.5-cell offset in X from dot-point center value
        yoff = 0.0  ! V-face: no offset in Y from dot-point center value

        DO j = 1, ny
          DO i = 1, nx  ! do all of X to fill array; last col outside domain

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_merc (xxin, yyin, met_proj_clon,  &
                             latv(i,j), lonv(i,j))

            mapv(i,j) = mapfac_merc (latv(i,j))

          ENDDO
        ENDDO


    END SELECT


    ! See which MM5 "season" we are in.
 
    startseas = startdate(1:4) // "-04-15-00:00:00"
    endseas   = startdate(1:4) // "-10-15-00:00:00"

    CALL geth_idts ( startdate, startseas, idts_start )
    CALL geth_idts ( endseas,   startdate, idts_end )

    IF ( ( idts_start < 0 ) .OR. ( idts_end < 0 ) ) THEN
      IF ( met_y_centd >= 0.0 ) THEN  ! Northern Hemisphere
        met_season = 2   ! winter
      ELSE  ! Southern Hemisphere
        met_season = 1   ! summer
      ENDIF
    ELSE
      IF ( met_y_centd >= 0.0 ) THEN  ! Northern Hemisphere
        met_season = 1   ! summer
      ELSE  ! Southern Hemisphere
        met_season = 2   ! winter
      ENDIF
    ENDIF

    IF ( met_soil_lsm /= 3 ) THEN

      IF ( gotznt ) THEN

        DO j = 1, ny-1
          DO i = 1, nx-1
            znt(i,j)    = sfz0(landuse(i,j),met_season)
          ENDDO
        ENDDO

      ELSE

        IF ( met_season == 1 ) THEN  ! summer

          DO j = 1, ny-1
            DO i = 1, nx-1
              IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
                znt(i,j) = sfz013sum(landuse(i,j)) * 0.01  ! sfz0: [cm --> m]
              ELSE IF ( met_lu_water == 16 ) THEN
                znt(i,j) = sfz024sum(landuse(i,j)) * 0.01  ! sfz0: [cm --> m]
              ENDIF
            ENDDO
          ENDDO

        ELSE IF ( met_season == 2 ) THEN  ! winter

          DO j = 1, ny-1
            DO i = 1, nx-1
              IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
                znt(i,j) = sfz013win(landuse(i,j)) * 0.01  ! sfz0: [cm --> m]
              ELSE IF ( met_lu_water == 16 ) THEN
                znt(i,j) = sfz024win(landuse(i,j)) * 0.01  ! sfz0: [cm --> m]
              ENDIF
            ENDDO
          ENDDO

        ENDIF

      ENDIF

      znt(:,ny) = znt(:,ny-1)
      znt(nx,:) = znt(nx-1,:)

      WRITE (*,f6000) 'ZNT      ', znt   (lprt_metx, lprt_mety), 'm'

      IF ( met_soil_lsm /= 2 ) THEN

        IF ( gotalbedo ) THEN

          DO j = 1, ny-1
            DO i = 1, nx-1
              albedo(i,j) = albd(landuse(i,j),met_season) * 0.01  ! albd: [% --> dec]
            ENDDO
          ENDDO

        ELSE

          IF ( met_season == 1 ) THEN  ! summer

            DO j = 1, ny-1
              DO i = 1, nx-1
                IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
                  albedo(i,j) = albd13sum(landuse(i,j))
                ELSE IF ( met_lu_water == 16 ) THEN
                  albedo(i,j) = albd24sum(landuse(i,j))
                ENDIF
              ENDDO
            ENDDO

          ELSE IF ( met_season == 2 ) THEN  ! winter

            DO j = 1, ny-1
              DO i = 1, nx-1
                IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
                  albedo(i,j) = albd13win(landuse(i,j))
                ELSE IF ( met_lu_water == 16 ) THEN
                  albedo(i,j) = albd24win(landuse(i,j))
                ENDIF
              ENDDO
            ENDDO

          ENDIF

        ENDIF

        albedo(:,ny) = albedo(:,ny-1)
        albedo(nx,:) = albedo(nx-1,:)

        WRITE (*,f6000) 'ALBEDO   ', albedo(lprt_metx, lprt_mety), 'fraction'

      ENDIF

    ENDIF

    IF ( iflufrc ) THEN
      CALL readter
      DO k = 1, nummetlu
        WRITE (*,f6200) 'VEGCAT', k, lufrac(lprt_metx,lprt_mety,k), 'fraction'
      ENDDO
    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( bhi     )
  DEALLOCATE ( bhr     )
! DEALLOCATE ( dum2d )  ! commented out to avoid memory fragmentation

! IF ( ALLOCATED (albd) ) DEALLOCATE ( albd )  ! commented out
! IF ( ALLOCATED (sfz0) ) DEALLOCATE ( sfz0 )  ! commented out
! IF ( ALLOCATED (sfem) ) DEALLOCATE ( sfem )  ! commented out

END SUBROUTINE rdmm5v3
