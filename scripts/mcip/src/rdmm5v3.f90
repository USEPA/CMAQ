
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/MCIP2/src/mcip2/rdmm5v3.F,v 1.7 2007/08/03 20:52:26 tlotte Exp $ 


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
!-------------------------------------------------------------------------------

  USE date_pack
  USE file
  USE metinfo, nx => met_nx, ny => met_ny, nz => met_nz
  USE metvars
  USE mcipparm

  IMPLICIT NONE

  REAL,    SAVE, ALLOCATABLE   :: albd       ( : , : )
  REAL                         :: albd13     ( 13, 2 )
  REAL                         :: albd24     ( 24, 2 )
  INTEGER,       ALLOCATABLE   :: bhi        ( : , : )
  REAL,          ALLOCATABLE   :: bhr        ( : , : )
  REAL                         :: buffrq
  REAL,          EXTERNAL      :: cori
  CHARACTER*24                 :: currentdate
  REAL,          ALLOCATABLE   :: data       ( : , : , : , : )
  CHARACTER*46                 :: description
  REAL,    SAVE, ALLOCATABLE   :: dum2d      ( : , : )
  INTEGER                      :: end_index  ( 4 )
  CHARACTER*24                 :: endseas
  LOGICAL,       SAVE          :: first      = .TRUE.
  INTEGER                      :: i
  INTEGER                      :: idts_end
  INTEGER                      :: idts_start
  INTEGER                      :: idtsec
  INTEGER                      :: iflag
  LOGICAL                      :: ifmm
  CHARACTER*60                 :: ifmt1
  CHARACTER*60                 :: ifmt1a
  INTEGER                      :: istat
  INTEGER                      :: j
  INTEGER                      :: k
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER                      :: keep
  INTEGER                      :: km1
  INTEGER                      :: lbnd
  REAL,          EXTERNAL      :: mapfac_lam
  REAL,          EXTERNAL      :: mapfac_merc
  REAL,          EXTERNAL      :: mapfac_ps
  CHARACTER*24,  INTENT(IN)    :: mcip_now
  CHARACTER*256                :: mmfile
  INTEGER                      :: ndim
  LOGICAL                      :: newfile
  LOGICAL                      :: newtime
  INTEGER,       PARAMETER     :: numprogs   = 20
  INTEGER,       PARAMETER     :: numvalsi   = 50
  INTEGER,       PARAMETER     :: numvalsr   = 20
  LOGICAL                      :: ok         = .TRUE.
  CHARACTER*4                  :: ordering
  CHARACTER*16,  PARAMETER     :: pname      = 'RDMM5V3'
  REAL,    SAVE, ALLOCATABLE   :: sfz0       ( : , : )
  REAL                         :: sfz013     ( 13, 2 )
  REAL                         :: sfz024     ( 24, 2 )
  REAL,          PARAMETER     :: smallnum   = 1.0e-7
  CHARACTER*4                  :: staggering
  INTEGER                      :: start_index(4)
  CHARACTER*24,  SAVE          :: startdate
  CHARACTER*24                 :: startseas
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2
  REAL                         :: timeout
  INTEGER,       PARAMETER     :: ttol       = 300  ! [sec]
  INTEGER                      :: ubnd
  CHARACTER*25                 :: units
  CHARACTER*9                  :: var
  CHARACTER*9                  :: vname
  REAL                         :: xoff
  REAL                         :: xxin
  REAL                         :: yoff
  REAL                         :: yyin

  ! Data capture verification flags.

  LOGICAL :: gotalbedo,  gotglw,     gotgroundt, gothfx,     gotisltyp,   &
             gotlai,     gotlanduse, gotlatcrs,  gotloncrs,  gotmapcrs,   &
             gotmapdot,  gotmol,     gotpp,      gotpsa,     gotq2,       &
             gotqca,     gotqfx,     gotqga,     gotqia,     gotqra,      &
             gotqsa,     gotqva,     gotra,      gotraincon, gotrainnon,  &
             gotrgrnd,   gotrstom,   gotsigmah,  gotsnowcov, gotsoilt1,   &
             gotsoilt2,  gott2,      gotta,      gotterrain, gottke,      &
             gotu10,     gotua,      gotust,     gotv10,     gotva,       &
             gotveg,     gotw2,      gotwa,      gotwg,      gotwr,       &
             gotznt,     gotzpbl

  ! Define roughness length and albedo as functions of land use and season
  ! since they typically are not part of MM5v2 output.

  DATA (sfz013(i,1),i=1,13) / 50.0,  15.0,  12.0,  50.0,  50.0,  40.0,   &
                               0.01, 20.0,  10.0,  10.0,   5.0,  50.0,   &
                              15.0 /  ! summer values [cm]

  DATA (sfz024(i,1),i=1,24) / 50.0,  15.0,  15.0,  15.0,  14.0,  20.0,   &
                              12.0,  10.0,  11.0,  15.0,  50.0,  50.0,   &
                              50.0,  50.0,  50.0,   0.01, 20.0,  40.0,   &
                              10.0,  10.0,  30.0,  15.0,  10.0,   5.0 /  ! summer values [cm]

  DATA (sfz013(i,2),i=1,13) / 50.0,   5.0,  10.0,  50.0,  50.0,  40.0,   &
                               0.01, 20.0,  10.0,  10.0,   5.0,  50.0,   &
                              15.0 /  ! winter values [cm]

  DATA (sfz024(i,2),i=1,24) / 50.0,   5.0,   5.0,   5.0,   5.0,  20.0,   &
                              10.0,  10.0,  10.0,  15.0,  50.0,  50.0,   &
                              50.0,  50.0,  50.0,   0.01, 20.0,  40.0,   &
                              10.0,  10.0,  30.0,  15.0,   5.0,   5.0 /  ! winter values [cm]

  DATA (albd13(i,1),i=1,13) /  0.18,  0.17,  0.19,  0.16,  0.12,  0.14,  &
                               0.08,  0.14,  0.25,  0.15,  0.55,  0.12,  &
                               0.20 /  ! summer values [dec]

  DATA (albd24(i,1),i=1,24) /  0.18,  0.17,  0.18,  0.18,  0.18,  0.16,  &
                               0.19,  0.22,  0.20,  0.20,  0.16,  0.14,  &
                               0.12,  0.12,  0.13,  0.08,  0.14,  0.14,  &
                               0.25,  0.15,  0.15,  0.15,  0.25,  0.55 /  ! summer values [dec]

  DATA (albd13(i,2),i=1,13) /  0.18,  0.23,  0.23,  0.17,  0.12,  0.14,  &
                               0.08,  0.14,  0.25,  0.70,  0.70,  0.12,  &
                               0.20 /  ! winter values [dec]

  DATA (albd24(i,2),i=1,24) /  0.18,  0.23,  0.23,  0.23,  0.23,  0.20,  &
                               0.23,  0.25,  0.24,  0.20,  0.17,  0.15,  &
                               0.12,  0.12,  0.14,  0.08,  0.14,  0.14,  &
                               0.25,  0.60,  0.50,  0.55,  0.70,  0.70 /  ! winter values [dec]

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
  gotsnowcov = .FALSE.  ;  gotta      = .FALSE.  ;  gotua      = .FALSE.
  gotust     = .FALSE.  ;  gotva      = .FALSE.  ;  gotwa      = .FALSE.
  gotzpbl    = .FALSE.

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
    READ (iutmm, IOSTAT=istat, ERR=8000, END=999) iflag

    IF ( iflag == 0 ) THEN

      var = 'BHI & BHR'
      READ (iutmm, IOSTAT=istat, ERR=8000, END=8100) bhi, bhr

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
          WRITE (6,9700) iutmm-1, iutmm
          GOTO 1001
        ENDIF

        ! If the new file is not a continuation of a previous run
        ! (i.e., "restart" or "split"), then stop since there is
        ! a physical discontinuity.

        met_restart = bhi(1,12)
        buffrq      = bhr(5,12)
        IF ( .NOT. ( ( buffrq > 0.0 ) .AND. ( buffrq >= met_tapfrq ) ) .AND.  &
             ( met_restart /= 1 ) ) THEN
          WRITE (6,9800) iutmm-1, iutmm
          GOTO 1001
        ENDIF

        newfile = .FALSE.

      ENDIF

    ELSE IF ( iflag == 1 ) THEN

      var = 'SM HEADER'
      READ (iutmm, IOSTAT=istat, ERR=8000, END=8200) ndim, start_index,    &
            end_index, timeout, staggering, ordering, currentdate, vname,  &
            units, description

      currentdate(11:11) = "-"  ! switch "_" to "-" for consistency

      IF ( first ) THEN
        startdate = currentdate
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

        CALL geth_idts (currentdate, mcip_now, idtsec)
        IF ( ABS(idtsec) <= ttol ) THEN
          keep = 1
        ELSE
          keep = 0
        ENDIF

      ENDIF

      IF ( ndim == 1 ) THEN
        ALLOCATE ( data(end_index(1), 1, 1, 1) )
      ELSE IF ( ndim == 2 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), 1, 1) )
      ELSE IF ( ndim == 3 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), end_index(3), 1) )
      ELSE IF ( ndim == 4 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), end_index(3), end_index(4)))
      ENDIF

      var = vname
      READ (iutmm, IOSTAT=istat, ERR=8000, END=8300) data

      IF ( ( vname == 'RAIN CON ' ) .AND. ( .NOT. gotraincon ) ) THEN
        IF ( ( SIZE(raincon,1) == SIZE(data,2) ) .AND.  &
             ( SIZE(raincon,2) == SIZE(data,1) ) ) THEN
          ! accumulated convective precip [cm]
          DO j = start_index(2), end_index(2)
            DO i = start_index(1), end_index(1)
              raincon(j,i) = data(i,j,1,1)
            ENDDO
          ENDDO
          dum2d   = raincon
          raincon = MAX(0.0, (raincon - rcold ))  ! incremental total in cm
          rcold   = dum2d  ! accumulated total to subtract from next interval
          IF ( keep == 1 ) THEN
            gotraincon = .TRUE.
            WRITE (*,6000) 'RAINCON  ', raincon(lprt_metx, lprt_mety), TRIM(units)
          ENDIF
        ELSE
          GOTO 8400
        ENDIF
      ENDIF

      IF ( ( vname == 'RAIN NON ' ) .AND. ( .NOT. gotrainnon ) ) THEN
        IF ( ( SIZE(rainnon,1) == SIZE(data,2) ) .AND.  &
             ( SIZE(rainnon,2) == SIZE(data,1) ) ) THEN
          ! accumulated nonconvective precip [cm]
          DO j = start_index(2), end_index(2)
            DO i = start_index(1), end_index(1)
              rainnon(j,i) = data(i,j,1,1)
            ENDDO
          ENDDO
          dum2d   = rainnon
          rainnon = MAX(0.0, (rainnon - rnold ))  ! incremental total in cm
          rnold   = dum2d  ! accumulated total to subtract from next interval
          IF ( keep == 1 ) THEN
            gotrainnon = .TRUE.
            WRITE (*,6000) 'RAINNON  ', rainnon(lprt_metx, lprt_mety), TRIM(units)
          ENDIF
        ELSE
          GOTO 8400
        ENDIF
      ENDIF

      keeptime: IF ( keep == 1 ) THEN

        IF ( ( vname == 'U        ' ) .AND. ( .NOT. gotua ) ) THEN
          IF ( ( SIZE(ua,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(ua,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(ua,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz  ! u-component of horizontal wind [m s^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  ua(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotua = .TRUE.
            WRITE (*,ifmt1) 'UA       ', ua(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'V        ' ) .AND. ( .NOT. gotva ) ) THEN
          IF ( ( SIZE(va,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(va,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(va,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz  ! v-component of horizontal wind [m s^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  va(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotva = .TRUE.
            WRITE (*,ifmt1) 'VA       ', va(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'T        ' ) .AND. ( .NOT. gotta ) ) THEN
          IF ( ( SIZE(ta,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(ta,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(ta,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz   ! temperature [K]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  ta(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotta = .TRUE.
            WRITE (*,ifmt1) 'TA       ', ta(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'Q        ' ) .AND. ( .NOT. gotqva ) ) THEN
          IF ( ( SIZE(qva,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(qva,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(qva,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz  ! mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qva(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqva = .TRUE.
            WRITE (*,ifmt1) 'QVA      ', qva(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'CLW      ' ) .AND. ( .NOT. gotqca ) ) THEN
          IF ( ( SIZE(qca,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(qca,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(qca,3) == SIZE(data,3) ) ) THEN
            DO k = 1,nz  ! cloud water mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qca(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqca = .TRUE.
            WRITE (*,ifmt1) 'QCA      ', qca(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'RNW      ' ) .AND. ( .NOT. gotqra ) ) THEN
          IF ( ( SIZE(qra,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(qra,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(qra,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz  ! rain water mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qra(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqra = .TRUE.
            WRITE (*,ifmt1) 'QRA      ', qra(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'ICE      ' ) .AND. ( .NOT. gotqia ) ) THEN
          IF ( ( SIZE(qia,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(qia,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(qia,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz  ! cloud ice mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qia(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqia = .TRUE.
            WRITE (*,ifmt1) 'QIA      ', qia(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SNOW     ' ) .AND. ( .NOT. gotqsa ) ) THEN
          IF ( ( SIZE(qsa,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(qsa,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(qsa,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz  ! snow mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qsa(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqsa = .TRUE.
            WRITE (*,ifmt1) 'QSA      ', qsa(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'GRAUPEL  ' ) .AND. ( .NOT. gotqga ) ) THEN
          IF ( ( SIZE(qga,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(qga,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(qga,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz  ! graupel mixing ratio [kg kg^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  qga(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotqga = .TRUE.
            WRITE (*,ifmt1) 'QGA      ', qga(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'W        ' ) .AND. ( .NOT. gotwa ) ) THEN
          IF ( ( SIZE(wa,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(wa,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(wa,3) == SIZE(data,3) ) ) THEN
            lbnd = LBOUND(wa,3)  ! first element is mapped correctly (0 or 1)
            ubnd = UBOUND(wa,3)  ! last element is mapped correctly (nz or nz+1)
            DO k = lbnd, ubnd  ! vertical component of wind [m s^-1]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  wa(j,i,k) = data(i,j,nz+1-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotwa = .TRUE.
            WRITE (*,ifmt1a) 'WA       ', wa(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'TKE      ' ) .AND. ( .NOT. gottke ) ) THEN
          IF ( ( SIZE(tke,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(tke,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(tke,3) == SIZE(data,3) ) ) THEN
            lbnd = LBOUND(tke,3)  ! first element is mapped correctly (0 or 1)
            ubnd = UBOUND(tke,3)  ! last element is mapped correctly (nz or nz+1)
            IF ( iftkef ) THEN  ! TKE on full-levels
              DO k = lbnd, ubnd  ! turbulent kinetic energy [J kg^-1]
                DO j = start_index(2), end_index(2)
                  DO i = start_index(1), end_index(1)
                    tke(j,i,k) = data(i,j,nz+1-k+1,1)
                  ENDDO
                ENDDO
              ENDDO
            ELSE  ! TKE on half-layers
              DO k = lbnd, ubnd  ! turbulent kinetic energy [J kg^-1]
                DO j = start_index(2), end_index(2)
                  DO i = start_index(1), end_index(1)
                    tke(j,i,k) = data(i,j,nz-k+1,1)
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
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'PP       ' ) .AND. ( .NOT. gotpp ) ) THEN
          IF ( ( SIZE(pp,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(pp,2) == SIZE(data,1) ) .AND.  &
               ( SIZE(pp,3) == SIZE(data,3) ) ) THEN
            DO k = 1, nz  ! pressure perturbation [Pa]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  pp(j,i,k) = data(i,j,nz-k+1,1)
                ENDDO
              ENDDO
            ENDDO
            gotpp = .TRUE.
            WRITE (*,ifmt1) 'PP       ', pp(lprt_metx,lprt_mety,:)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'PSTARCRS ' ) .AND. ( .NOT. gotpsa ) ) THEN
          IF ( ( SIZE(psa,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(psa,2) == SIZE(data,1) ) ) THEN
            ! (reference) surface pressure minus ptop [Pa]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                psa(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotpsa = .TRUE.
            WRITE (*,6000) 'PSA      ', psa(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'GROUND T ' ) .AND. ( .NOT. gotgroundt ) ) THEN
          IF ( ( SIZE(groundt,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(groundt,2) == SIZE(data,1) ) ) THEN
            ! ground temperature [K]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                groundt(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotgroundt = .TRUE.
            WRITE (*,6000) 'GROUNDT  ', groundt(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'TERRAIN  ' ) .AND. ( .NOT. gotterrain ) ) THEN
          IF ( ( SIZE(terrain,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(terrain,2) == SIZE(data,1) ) ) THEN
            ! terrain elevation [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                terrain(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotterrain = .TRUE.
            WRITE (*,6000) 'TERRAIN  ', terrain(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'MAPFACCR ' ) .AND. ( .NOT. gotmapcrs ) ) THEN
          IF ( ( SIZE(mapcrs,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(mapcrs,2) == SIZE(data,1) ) ) THEN
            ! map scale factor (cross) [dim'less]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                mapcrs(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotmapcrs = .TRUE.
            WRITE (*,6000) 'MAPCRS   ', mapcrs(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'MAPFACDT ' ) .AND. ( .NOT. gotmapdot ) ) THEN
          IF ( ( SIZE(mapdot,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(mapdot,2) == SIZE(data,1) ) ) THEN
            ! map scale factor (dot) [dim'less]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                mapdot(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotmapdot = .TRUE.
            WRITE (*,6000) 'MAPDOT   ', mapdot(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'LATITCRS ' ) .AND. ( .NOT. gotlatcrs ) ) THEN
          IF ( ( SIZE(latcrs,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(latcrs,2) == SIZE(data,1) ) ) THEN
            ! latitude (cross) [degree]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                latcrs(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotlatcrs = .TRUE.
            WRITE (*,6000) 'LATCRS   ', latcrs(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'LONGICRS ' ) .AND. ( .NOT. gotloncrs ) ) THEN
          IF ( ( SIZE(loncrs,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(loncrs,2) == SIZE(data,1) ) ) THEN
            ! longitude (cross) [degree]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                loncrs(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotloncrs = .TRUE.
            WRITE (*,6000) 'LONCRS   ', loncrs(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'LAND USE ' ) .AND. ( .NOT. gotlanduse ) ) THEN
          IF ( ( SIZE(landuse,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(landuse,2) == SIZE(data,1) ) ) THEN
            ! land use [category]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                landuse(j,i) = NINT(data(i,j,1,1))
              ENDDO
            ENDDO
            gotlanduse = .TRUE.
            WRITE (*,6100) 'LANDUSE  ', landuse(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SNOWCOVR ' ) .AND. ( .NOT. gotsnowcov ) .AND.  &
             ( ( met_snow_opt == 0 ) .OR. ( met_snow_opt == 1 ) ) ) THEN
          IF ( ( SIZE(snowcovr,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(snowcovr,2) == SIZE(data,1) ) ) THEN
            ! snow cover [1= yes, 0=no]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                snowcovr(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotsnowcov = .TRUE.
            WRITE (*,6000) 'SNOWCOVR ', snowcovr(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'WEASD    ' ) .AND. ( .NOT. gotsnowcov ) .AND.  &
             ( met_snow_opt == 2 ) ) THEN
          IF ( ( SIZE(snowcovr,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(snowcovr,2) == SIZE(data,1) ) ) THEN
            ! snow cover [mm]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                snowcovr(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            ! Convert liquid equiv snow depth into yes/no
            WHERE ( snowcovr >= 1.0 )  ! [mm], see MM5's rdinit.F
              snowcovr = 1.0
            ELSEWHERE
              snowcovr = 0.0
            END WHERE
            gotsnowcov = .TRUE.
            WRITE (*,6000) 'SNOWCOVR ', snowcovr(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'PBL HGT  ' ) .AND. ( .NOT. gotzpbl ) ) THEN
          IF ( ( SIZE(zpbl,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(zpbl,2) == SIZE(data,1) ) ) THEN
            ! PBL height [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                zpbl(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotzpbl = .TRUE.
            WRITE (*,6000) 'ZPBL     ', zpbl(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SHFLUX   ' ) .AND. ( .NOT. gothfx ) ) THEN
          IF ( ( SIZE(hfx,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(hfx,2) == SIZE(data,1) ) ) THEN
            ! sensible heat flux [W m^-2]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                hfx(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gothfx = .TRUE.
            WRITE (*,6000) 'HFX      ', hfx(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'LHFLUX   ' ) .AND. ( .NOT. gotqfx ) ) THEN
          IF ( ( SIZE(qfx,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(qfx,2) == SIZE(data,1) ) ) THEN
            ! latent heat flux [W m^-2]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                qfx(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotqfx = .TRUE.
            WRITE (*,6000) 'QFX      ', qfx(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'UST      ' ) .AND. ( .NOT. gotust ) ) THEN
          IF ( ( SIZE(ust,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(ust,2) == SIZE(data,1) ) ) THEN
            ! frictional velocity [m s^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                ust(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotust = .TRUE.
            WRITE (*,6000) 'UST      ', ust(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SWDOWN   ' ) .AND. ( .NOT. gotrgrnd ) ) THEN
          IF ( ( SIZE(rgrnd,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(rgrnd,2) == SIZE(data,1) ) ) THEN
            ! surface downward SW radiation [W m^-2]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                rgrnd(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotrgrnd = .TRUE.
            WRITE (*,6000) 'RGRND    ', rgrnd(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'LWDOWN   ' ) .AND. ( .NOT. gotglw ) ) THEN
          IF ( ( SIZE(glw,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(glw,2) == SIZE(data,1) ) ) THEN
            ! surface downward LW radiation [W m^-2]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                glw(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotglw = .TRUE.
            WRITE (*,6000) 'GLW      ', glw(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SOIL T 1 ' ) .AND. ( .NOT. gotsoilt1 ) ) THEN
          IF ( ( SIZE(soilt1,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(soilt1,2) == SIZE(data,1) ) ) THEN
            ! (surface) soil temperature in layer 1 [K]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                soilt1(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotsoilt1 = .TRUE.
            WRITE (*,6000) 'SOILT1   ', soilt1(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SOIL T 2 ' ) .AND. ( .NOT. gotsoilt2 ) ) THEN
          IF ( ( SIZE(soilt2,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(soilt2,2) == SIZE(data,1) ) ) THEN
            ! (deep) soil temperature in layer 2 [K]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                soilt2(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotsoilt2 = .TRUE.
            WRITE (*,6000) 'SOILT2   ', soilt2(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SOIL M 1 ' ) .AND. ( .NOT. gotwg ) ) THEN
          IF ( ( SIZE(wg,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(wg,2) == SIZE(data,1) ) ) THEN
            ! (surface) soil moisture in layer 1 [m^3 m^-3]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                wg(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotwg = .TRUE.
            WRITE (*,6000) 'WG       ', wg(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SOIL M 2 ' ) .AND. ( .NOT. gotw2 ) ) THEN
          IF ( ( SIZE(w2,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(w2,2) == SIZE(data,1) ) ) THEN
            ! soil moisture in layer 2 [m^3 m^-3]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                w2(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotw2 = .TRUE.
            WRITE (*,6000) 'W2       ', w2(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'CANOPYM  ' ) .AND. ( .NOT. gotwr ) ) THEN
          IF ( ( SIZE(wr,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(wr,2) == SIZE(data,1) ) ) THEN
            ! canopy moisture content [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                wr(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotwr = .TRUE.
            WRITE (*,6000) 'WR       ', wr(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'ALBD     ' ) .AND. ( .NOT. gotalbedo ) .AND.  &
             ( met_soil_lsm /= 2 ) .AND. ( met_soil_lsm /= 3 ) ) THEN
          IF ( .NOT. ALLOCATED ( albd ) )  &
            ALLOCATE ( albd (SIZE(data,1), SIZE(data,2)) )
          albd = data(:,:,1,1)  ! surface albedo [percent]
          gotalbedo = .TRUE.
        ENDIF

        IF ( ( vname == 'SFZ0     ' ) .AND. ( .NOT. gotznt ) .AND.  &
             ( met_soil_lsm /= 3 ) ) THEN
          IF ( .NOT. ALLOCATED ( sfz0 ) )  &
            ALLOCATE ( sfz0 (SIZE(data,1), SIZE(data,2)) )
          sfz0 = data(:,:,1,1) / 100.0  ! surface roughness length [cm --> m]
          gotznt = .TRUE.
        ENDIF

        IF ( ( vname == 'T2       ' ) .AND. ( .NOT. gott2 ) ) THEN
          IF ( ( SIZE(t2,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(t2,2) == SIZE(data,1) ) ) THEN
            ! 2-m temperature [K]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                t2(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gott2 = .TRUE.
            WRITE (*,6000) 'T2       ', t2(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'Q2       ' ) .AND. ( .NOT. gotq2 ) ) THEN
          IF ( ( SIZE(q2,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(q2,2) == SIZE(data,1) ) ) THEN
            ! 2-m mixing ratio [kg kg^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                q2(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotq2 = .TRUE.
            WRITE (*,6000) 'Q2       ', q2(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'U10      ' ) .AND. ( .NOT. gotu10 ) ) THEN
          IF ( ( SIZE(u10,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(u10,2) == SIZE(data,1) ) ) THEN
            ! 10-m u-component wind [m/s]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                u10(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotu10 = .TRUE.
            WRITE (*,6000) 'U10      ', u10(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'V10      ' ) .AND. ( .NOT. gotv10 ) ) THEN
          IF ( ( SIZE(v10,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(v10,2) == SIZE(data,1) ) ) THEN
            ! 10-m v-component wind [m/s]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                v10(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotv10 = .TRUE.
            WRITE (*,6000) 'V10      ', v10(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'M-O LENG ' ) .AND. ( .NOT. gotmol ) ) THEN
          IF ( ( SIZE(mol,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(mol,2) == SIZE(data,1) ) ) THEN
            ! Monin-Obukhov length [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                mol(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotmol = .TRUE.
            WRITE (*,6000) 'MOL      ', mol(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'ALBEDO   ' ) .AND. ( .NOT. gotalbedo ) ) THEN
          IF ( ( SIZE(albedo,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(albedo,2) == SIZE(data,1) ) ) THEN
            ! surface albedo [fraction]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                albedo(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotalbedo = .TRUE.
            WRITE (*,6000) 'ALBEDO   ', albedo(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'ALB      ' ) .AND. ( .NOT. gotalbedo ) ) THEN
          IF ( ( SIZE(albedo,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(albedo,2) == SIZE(data,1) ) ) THEN
            ! surface albedo [fraction]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                albedo(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotalbedo = .TRUE.
            WRITE (*,6000) 'ALB      ', albedo(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'RA       ' ) .AND. ( .NOT. gotra ) ) THEN
          IF ( ( SIZE(ra,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(ra,2) == SIZE(data,1) ) ) THEN
            ! aerodynamic resistance [s m^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                ra(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotra = .TRUE.
            WRITE (*,6000) 'RA       ', ra(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'RS       ' ) .AND. ( .NOT. gotrstom ) ) THEN
          IF ( ( SIZE(rstom,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(rstom,2) == SIZE(data,1) ) ) THEN
            ! surface resistance [s m^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                rstom(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotrstom = .TRUE.
            WRITE (*,6000) 'RSTOM    ', rstom(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'LAI      ' ) .AND. ( .NOT. gotlai ) ) THEN
          IF ( ( SIZE(lai,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(lai,2) == SIZE(data,1) ) ) THEN
            ! leaf area index [area area^-1]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                lai(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotlai = .TRUE.
            WRITE (*,6000) 'LAI      ', lai(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'VEGFRC   ' ) .AND. ( .NOT. gotveg ) ) THEN
          IF ( ( SIZE(veg,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(veg,2) == SIZE(data,1) ) ) THEN
            ! vegetation coverage [fraction]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                veg(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            IF ( met_soil_lsm == 3 ) THEN  ! Pleim-Xiu LSM
              IF ( MAXVAL(veg) > 1.1 ) THEN  ! bad data; use previous hour
                IF ( ABS(MAXVAL(vegold)) < smallnum ) THEN
                  GOTO 8750
                ENDIF
                veg = vegold
              ELSE
                vegold = veg
              ENDIF
            ELSE IF ( met_soil_lsm == 2 ) THEN  ! NOAH LSM
              veg(:,:) = veg(:,:) * 0.01  ! percent -> decimal
            ENDIF
            gotveg = .TRUE.
            WRITE (*,6000) 'VEG      ', veg(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'ZNT      ' ) .AND. ( .NOT. gotznt ) ) THEN
          IF ( ( SIZE(znt,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(znt,2) == SIZE(data,1) ) ) THEN
            ! roughness length [m]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                znt(j,i) = data(i,j,1,1)
              ENDDO
            ENDDO
            gotznt = .TRUE.
            WRITE (*,6000) 'ZNT      ', znt(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'ISLTYP   ' ) .AND. ( .NOT. gotisltyp ) ) THEN
          IF ( ( SIZE(isltyp,1) == SIZE(data,2) ) .AND.  &
               ( SIZE(isltyp,2) == SIZE(data,1) ) ) THEN
            ! soil type [USDA category]
            DO j = start_index(2), end_index(2)
              DO i = start_index(1), end_index(1)
                isltyp(j,i) = NINT(data(i,j,1,1))
              ENDDO
            ENDDO
            gotisltyp = .TRUE.
            WRITE (*,6100) 'ISLTYP   ', isltyp(lprt_metx,lprt_mety), TRIM(units)
          ELSE
            GOTO 8400
          ENDIF
        ENDIF

        IF ( ( vname == 'SIGMAH   ' ) .AND. ( .NOT. gotsigmah ) ) THEN
          IF ( SIZE(sigmah) == SIZE(data,1) ) THEN
            sigmah = data(nz:1:-1,1,1,1)  ! vertical coordinate [half-sigma]
            gotsigmah = .TRUE.
          ELSE
            GOTO 8400
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

      DEALLOCATE ( data )

    ELSE IF ( iflag == 2 ) THEN

      newtime = .TRUE.
      IF ( keep == 1 ) EXIT findtime

    ELSE

      WRITE (6,9450) iflag
      GOTO 1001

    ENDIF

    CYCLE findtime

    ! If the end-of-file marker was reached, try to open another input file to
    ! get the time period we need.

 999 CONTINUE

    CLOSE (iutmm)
    PRINT*, '+++++ End-of-file marker found on unit ', iutmm

    iutmm = iutmm + 1
    PRINT*, '+++++ Attempting to open unit ', iutmm

    mmfile = TRIM( file_mm(iutmm-iutmmi+1) )
    INQUIRE ( FILE=mmfile, EXIST=ifmm )
    IF ( .NOT. ifmm ) THEN
      WRITE (6,9500) mcip_now, iutmm
      GOTO 1001
    ENDIF

    OPEN (UNIT=iutmm,   FILE=mmfile, FORM='UNFORMATTED', STATUS='OLD',  &
          IOSTAT=istat, ERR=8600)

    newfile = .TRUE.

  ENDDO findtime
   
!-------------------------------------------------------------------------------
! Make sure we collected the arrays we need.
!-------------------------------------------------------------------------------

  IF ( .NOT. gotua ) THEN
    WRITE (6,9900) 'U'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotva ) THEN
    WRITE (6,9900) 'V'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotta ) THEN
    WRITE (6,9900) 'T'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotqva ) THEN
    WRITE (6,9900) 'Q'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotqca ) THEN
    IF ( met_expl_moist < 4 ) THEN  ! MM5 without ice-phase scheme
      qca(:,:,:) = 0.0
      gotqca = .TRUE.
    ELSE
      WRITE (6,9900) 'CLW'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqra ) THEN
    IF ( met_expl_moist < 4 ) THEN  ! MM5 without ice-phase scheme
      qra(:,:,:) = 0.0
      gotqra = .TRUE.
    ELSE
      WRITE (6,9900) 'RNW'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqia ) THEN
    IF ( met_expl_moist < 5 ) THEN  ! MM5 without mixed-phase scheme
      qia(:,:,:) = 0.0
      gotqia = .TRUE.
    ELSE
      WRITE (6,9900) 'ICE'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqsa ) THEN
    IF ( met_expl_moist < 5 ) THEN  ! MM5 without mixed-phase scheme
      qsa(:,:,:) = 0.0
      gotqsa = .TRUE.
    ELSE
      WRITE (6,9900) 'SNOW'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqga ) THEN
    IF ( met_expl_moist < 6 ) THEN  ! MM5 without graupel
      qga(:,:,:) = 0.0
      gotqga = .TRUE.
    ELSE
      WRITE (6,9900) 'GRAUPEL'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotwa ) THEN
    WRITE (6,9900) 'W'
    GOTO 1001
  ENDIF

  IF ( .NOT. gottke ) THEN
    WRITE (6,9900) 'TKE'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotpp ) THEN
    WRITE (6,9900) 'PP'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotpsa ) THEN
    WRITE (6,9900) 'PSTARCRS'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotgroundt ) THEN
    WRITE (6,9900) 'GROUND T'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotraincon ) THEN
    WRITE (6,9900) 'RAIN CON'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotrainnon ) THEN
    WRITE (6,9900) 'RAIN NON'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotterrain ) THEN
    WRITE (6,9900) 'TERRAIN'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotmapcrs ) THEN
    WRITE (6,9900) 'MAPFACCR'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotmapdot ) THEN
    WRITE (6,9900) 'MAPFACDT'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotlatcrs ) THEN
    WRITE (6,9900) 'LATITCRS'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotloncrs ) THEN
    WRITE (6,9900) 'LONGICRS'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotlanduse ) THEN
    WRITE (6,9900) 'LAND USE'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotsnowcov ) THEN
    WRITE (6,9950) 'SNOW COVER'
    snowcovr(:,:) = 0.0
    gotsnowcov = .TRUE.
  ENDIF

  IF ( .NOT. gotzpbl ) THEN
    WRITE (6,9900) 'PBL HGT'
    GOTO 1001
  ENDIF

  IF ( .NOT. gothfx ) THEN
    WRITE (6,9900) 'SHFLUX'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotqfx ) THEN
    WRITE (6,9900) 'LHFLUX'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotust ) THEN
    WRITE (6,9900) 'UST'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotrgrnd ) THEN
    WRITE (6,9900) 'SWDOWN'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotsoilt1 ) THEN
    WRITE (6,9900) 'SOIL T 1'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotsoilt2 ) THEN
    WRITE (6,9900) 'SOIL T 2'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotwg ) THEN
    WRITE (6,9900) 'SOIL M 1'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotw2 ) THEN
    WRITE (6,9900) 'SOIL M 2'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotwr ) THEN
    WRITE (6,9900) 'CANOPYM'
    GOTO 1001
  ENDIF

  IF ( .NOT. gott2 ) THEN
    WRITE (6,9900) 'T2'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotq2 ) THEN
    WRITE (6,9900) 'Q2'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotu10 ) THEN
    WRITE (6,9900) 'U10'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotv10 ) THEN
    WRITE (6,9900) 'V10'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotmol ) THEN
    WRITE (6,9900) 'M-O LENG'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotalbedo ) THEN
    IF ( met_soil_lsm == 3 ) THEN
      WRITE (6,9900) 'ALBEDO'
      GOTO 1001
    ELSE IF ( met_soil_lsm == 2 ) THEN
      WRITE (6,9900) 'ALB'
      GOTO 1001
    ELSE
      WRITE (6,9925) 'ALBD'
    ENDIF
  ENDIF

  IF ( .NOT. gotra ) THEN
    WRITE (6,9900) 'RA'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotrstom ) THEN
    WRITE (6,9900) 'RS'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotlai ) THEN
    WRITE (6,9900) 'LAI'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotveg ) THEN
    WRITE (6,9900) 'VEGFRC'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotznt ) THEN
    IF ( met_soil_lsm == 3 ) THEN
      WRITE (6,9900) 'ZNT'
      GOTO 1001
    ELSE
      WRITE (6,9925) 'SFZ0'
    ENDIF
  ENDIF

  IF ( .NOT. gotisltyp ) THEN
    WRITE (6,9900) 'ISLTYP'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotsigmah ) THEN
    WRITE (6,9900) 'SIGMAH'
    GOTO 1001
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

        xoff = 0.0  ! dot-point grid: no offset from dot-point center value
        yoff = 0.0  ! dot-point grid: no offset from dot-point center value

        DO j = 1, ny
          DO i = 1, nx

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_ps (xxin, yyin, met_tru1, met_proj_clon,  &
                           latdot(i,j), londot(i,j))

            mapdot(i,j) = mapfac_ps (latdot(i,j), met_tru1)

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

            CALL xy2ll_ps (xxin, yyin, met_tru1, met_proj_clon,  &
                           latu(i,j), lonu(i,j))

            mapu(i,j) = mapfac_ps (latu(i,j), met_tru1)

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

            CALL xy2ll_ps (xxin, yyin, met_tru1, met_proj_clon,  &
                           latv(i,j), lonv(i,j))

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
 
    startseas = startdate(1:4) // "-04-15-00:00:00.0000"
    endseas   = startdate(1:4) // "-10-15-00:00:00.0000"

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

        DO j = 1, ny-1
          DO i = 1, nx-1
            IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
              znt   (i,j) = sfz013(landuse(i,j),met_season) * 0.01  ! sfz0: [cm --> m]
            ELSE IF ( met_lu_water == 16 ) THEN
              znt   (i,j) = sfz024(landuse(i,j),met_season) * 0.01  ! sfz0: [cm --> m]
            ENDIF
          ENDDO
        ENDDO

      ENDIF

      znt   (:,ny) = znt   (:,ny-1)
      znt   (nx,:) = znt   (nx-1,:)

      WRITE (*,6000) 'ZNT      ', znt   (lprt_metx, lprt_mety), 'm'

      IF ( met_soil_lsm /= 2 ) THEN

        IF ( gotalbedo ) THEN

          DO j = 1, ny-1
            DO i = 1, nx-1
              albedo(i,j) = albd(landuse(i,j),met_season) * 0.01  ! albd: [% --> dec]
            ENDDO
          ENDDO

        ELSE

          DO j = 1, ny-1
            DO i = 1, nx-1
              IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
                albedo(i,j) = albd13(landuse(i,j),met_season)
              ELSE IF ( met_lu_water == 16 ) THEN
                albedo(i,j) = albd24(landuse(i,j),met_season)
              ENDIF
            ENDDO
          ENDDO

        ENDIF

        albedo(:,ny) = albedo(:,ny-1)
        albedo(nx,:) = albedo(nx-1,:)

        WRITE (*,6000) 'ALBEDO   ', albedo(lprt_metx, lprt_mety), 'fraction'

      ENDIF

    ENDIF

    IF ( iflufrc ) THEN
      CALL readter
      DO k = 1, nummetlu
        WRITE (*,6200) 'VEGCAT', k, lufrac(lprt_metx,lprt_mety,k), 'fraction'
      ENDDO
    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (4x, a,     f11.4, 2x, a)
 6100 FORMAT (4x, a,     i11,   2x, a)
 6200 FORMAT (4x, a, i2, f12.4, 2x, a)

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( bhi     )
  DEALLOCATE ( bhr     )
! DEALLOCATE ( dum2d )  ! commented out to avoid memory fragmentation

! IF ( ALLOCATED (albd) ) DEALLOCATE ( albd )  ! commented out
! IF ( ALLOCATED (sfz0) ) DEALLOCATE ( sfz0 )  ! commented out
! IF ( ALLOCATED (sfem) ) DEALLOCATE ( sfem )  ! commented out

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutmm, TRIM(var), istat
      GOTO 1001

 8100 WRITE (6,9100) iutmm, iflag, istat
      GOTO 1001

 8200 WRITE (6,9200) iutmm, istat
      GOTO 1001

 8300 WRITE (6,9300) iutmm, TRIM(var), istat
      GOTO 1001

 8400 WRITE (6,9400) TRIM(var)
      GOTO 1001

 8600 WRITE (6,9600) iutmm, istat
      GOTO 1001

 8750 WRITE (6,9750)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   ERROR READING FILE, UNIT = ', i3,       &
              /, 1x, '***   VARIABLE = ', a,                        &
              /, 1x, '***   IOSTAT = ', i4,                         &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3,   &
              /, 1x, '***   IFLAG = ', i3,                          &
              /, 1x, '***   IOSTAT = ', i4,                         &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3,   &
              /, 1x, '***   VARIABLE = SMALL HEADER',               &
              /, 1x, '***   IOSTAT = ', i4,                         &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3,   &
              /, 1x, '***   VARIABLE = ', a,                        &
              /, 1x, '***   IOSTAT = ', i4,                         &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   FOUND VARIABLE ', a,                    &
              /, 1x, '***   BUT ARRAY DIMENSIONS DO NOT MATCH',     &
              /, 1X, 70('*'))

 9450 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   INVALID MM5v3 DATA FLAG',               &
              /, 1x, '***   IFLAG = ', i4,                          &
              /, 1X, 70('*'))

 9500 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a,     &
              /, 1x, '***   COULD NOT FIND INPUT MET ON UNIT ', i3, &
              /, 1x, '***   FILE MAY NOT EXIST',                    &
              /, 1x, 70('*'))

 9600 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   ERROR OPENING MM5 FILE ON UNIT ', i3,   &
              /, 1x, '***   IOSTAT = ', i4,                         &
              /, 1x, 70('*'))

 9700 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   MM5 FILES DO NOT SEEM TO BE SAME GRID', &
              /, 1x, '***   CHECK FORTRAN UNITS ', i3, ' AND ', i3, &
              /, 1x, 70('*'))

 9750 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   MAXIMUM VALUE OF VEGOLD ARRAY IS 0.0',  &
              /, 1x, '***   NEED A NEW SOLUTION TO P-X BUG',        &
              /, 1x, 70('*'))

 9800 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   MM5 FILES MAY HAVE A DISCONTINUITY',    &
              /, 1x, '***   CHECK FORTRAN UNITS ', i3, ' AND ', i3, &
              /, 1x, 70('*'))

 9900 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   DID NOT FIND ARRAY ', a,                &
              /, 1x, 70('*'))

 9925 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   DID NOT FIND ARRAY ', a,                &
              /, 1x, '***   WILL SET FROM LOOKUP TABLE',            &
              /, 1x, 70('*'))

 9950 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V3',                     &
              /, 1x, '***   DID NOT FIND ARRAY ', a,                &
              /, 1x, '***   WILL SET TO 0.0 FOR ALL GRID CELLS',    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE rdmm5v3
