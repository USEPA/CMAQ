
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
! $Header: /project/work/rep/MCIP2/src/mcip2/readnml.F,v 1.5 2007/08/03 20:50:24 tlotte Exp $ 


SUBROUTINE readnml (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Read Namelist
! Purpose:  Reads input namelist to get user control variables.
! Revised:  21 Sep 2001  Original version.  (T. Otte)
!           16 Oct 2001  Added variable COORDNAM to namelist USERDEFS.
!                        Created new namelist WINDOWDEFS, and moved I0 and J0
!                        from USERDEFS to WINDOWDEFS.  Changed code related
!                        to I0 and J0 accordingly.  Changed relationship
!                        between I0 and J0 and BTRIM.  (T. Otte)
!           07 Jan 2002  Added explicit read of input meteorology files through
!                        the namelist to improved portability.  (T. Otte and
!                        S. Howard)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           27 Feb 2002  Removed minimum grid size for windows.  (T. Otte)
!           19 Mar 2002  Added error-handling on OPEN and READ statements.
!                        (T. Otte and S. Howard)
!           10 Aug 2004  Added flag to create static output (grid) files.
!                        Removed LUTYPE from USERDEFS namelist.  (T. Otte)
!           29 Nov 2004  Added optional input MM5 "TERRAIN" file to get
!                        fractional land use fields.  (T. Otte)
!           26 May 2005  Removed NDX and CTM2MET and option to interpolate
!                        to finer scale meteorology.  Removed I0LUSE, J0LUSE,
!                        and BMAX, which are no longer used.  Removed NDEP and
!                        made its usage explicit in PBLPKG and RADMDRY.
!                        Removed IWIND and made its usage explicit in VERTHYD.
!                        Changed I0 and J0 to Y0 and X0 to make code more
!                        general.  Added two new user options for LDDEP, and
!                        inserted calculation of LTOTG.  Removed "no_file"
!                        option on FILE_MM.  Added capability to use vertical
!                        structure from input meteorology file without
!                        specifying a priori via namelist.  (T. Otte)
!           24 Jul 2007  Added option LDDEP=0 to indicate that dry deposition
!                        velocities will be computed outside MCIP.  Changed
!                        code so that M3Dry with chlorine and mercury is the
!                        only option to compute dry deposition velocities in
!                        MCIP.  Removed user option variables LPBL, LRAD, LCLD,
!                        and LHYDOUT.  (T. Otte)
!           29 Apr 2008  Added earth radius in meters (ERADM) to the USERDEFS
!                        namelist.  Use ERADM to compute values in CONST_MOD
!                        that were formerly set as F90 parameters.  (T. Otte)
!                        Added user option to process satellite data for use
!                        in photolysis in CMAQ.  Added FILE_SAT to FILENAMES
!                        namelist, and added LSAT to USERDEFS namelist.
!                        Contributed by University of Alabama at Huntsville.
!                        (A. Biazar and T. Otte)
!           23 Dec 2008  Added user-definable reference latitude for WRF
!                        Lambert conformal data sets.  Best used for
!                        consistency with existing MM5 data sets.  (T. Otte)
!           23 Sep 2009  Added user option to compute and output 3D field of
!                        potential vorticity.  Added user option to output
!                        vertical velocity predicted by the meteorological
!                        model.  Added user option to output u- and v-
!                        component winds on C-staggered grid.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE const, ONLY: rearth, dg2m, pi180
  USE file
  USE sat2mcip

  IMPLICIT NONE

  INTEGER                      :: btrim
  REAL,          INTENT(OUT)   :: ctmlays    ( maxlays )
  INTEGER                      :: hh
  INTEGER                      :: istat
  INTEGER                      :: mm
  INTEGER                      :: n
  INTEGER                      :: ncolsin
  INTEGER                      :: nrowsin
  CHARACTER*16,  PARAMETER     :: pname      = 'READNML'

  NAMELIST /filenames/   file_gd, file_hdr, file_mm, file_ter, file_sat,  &
                         makegrid

  NAMELIST /userdefs/    lddep, lpv, lwout, luvcout, lsat,    &
                         eradm, mcip_start, mcip_end, intvl,  &
                         coordnam, grdnam, ctmlays,           &
                         btrim, lprt_col, lprt_row,           &
                         wrf_lc_ref_lat

  NAMELIST /windowdefs/  x0, y0, ncolsin, nrowsin

!-------------------------------------------------------------------------------
! Open namelist file.
!-------------------------------------------------------------------------------

  OPEN (iutnml, FILE=file_nml, STATUS='OLD', ERR=8000, IOSTAT=istat)

!-------------------------------------------------------------------------------
! Initialize input file names.
!-------------------------------------------------------------------------------

  file_gd     = "GRIDDESC"
  file_hdr    = "mmheader"
  file_mm(:)  = " "
  file_ter    = " "
  file_sat(:) = " "
  makegrid    = .TRUE.

!-------------------------------------------------------------------------------
! Set default value for user-selected options.
!
!   LDDEP:   0 = Do not calculate dry deposition velocities in MCIP
!            4 = Use Models-3 dry deposition with extra Cl and Hg species
!
!   LPV:     0 = Do not compute and output 3D potential vorticity
!            1 = Compute and output 3D potential vorticity
!
!   LWOUT:   0 = Do not output vertical velocity
!            1 = Output vertical velocity
!
!   LUVCOUT: 0 = Do not output u- and v-component winds on C-staggered grid
!            1 = Output u- and v-component winds on C-staggered grid
!
!   LSAT:    0 = No satellite input is available (default)
!            1 = GOES observed cloud information replaces model-derived input
!-------------------------------------------------------------------------------

  lddep      = 4
  lpv        = 0
  lwout      = 0
  luvcout    = 0
  lsat       = 0

!-------------------------------------------------------------------------------
! Set default value for earth radius in meters (ERADM).  The default value is
! consistent with the value used for a spherical earth in MM5 and in WRF-ARW.
!-------------------------------------------------------------------------------

  eradm      = 6370000.0  ! [m]

!-------------------------------------------------------------------------------
! Set default reference latitude for WRF Lambert conformal data sets.
!-------------------------------------------------------------------------------

  wrf_lc_ref_lat = -999.9

!-------------------------------------------------------------------------------
! Set default date.
!-------------------------------------------------------------------------------

  mcip_start = '0000-00-00-00:00:00.0000'
  mcip_end   = '0000-00-00-00:00:00.0000'
  intvl      =  0
 
!-------------------------------------------------------------------------------
! Set coordinates for cell to print diagnostic output.  If 0 is set,
! domain center cell will be used.
!-------------------------------------------------------------------------------

  lprt_col = 0
  lprt_row = 0

!-------------------------------------------------------------------------------
! Initialize CTM layers to an unrealistic value (-1).
!-------------------------------------------------------------------------------

  ctmlays(:) = -1.0

!-------------------------------------------------------------------------------
! Set default meteorology "boundary" point removal to 5.
!-------------------------------------------------------------------------------

  btrim   = 5

!-------------------------------------------------------------------------------
! Initialize COORDNAM and GRDNAM to missing values.
!-------------------------------------------------------------------------------

  coordnam = "????????????????"
  grdnam   = "????????????????"

!-------------------------------------------------------------------------------
! Set default values for coordinate of full MCIP "X" domain (including MCIP
! lateral boundary) lower-left corner with respect to the input meteorology
! domain.  Minimum acceptable value is 1.  X0 is the column offset, and Y0 is
! the row offset.
!-------------------------------------------------------------------------------

  x0 = 1
  y0 = 1

!-------------------------------------------------------------------------------
! Set default values for window size.  (20 is arbitrary...just want to prevent
! "very small" domains.)
!-------------------------------------------------------------------------------

  ncolsin = 20
  nrowsin = 20

!-------------------------------------------------------------------------------
! Read namelist to get user definitions.  Rewind namelist file after each
! read in case namelists are not in the correct order in the namelist.
!-------------------------------------------------------------------------------

  READ (iutnml, filenames, ERR=8025, IOSTAT=istat)
  REWIND (iutnml)

  READ (iutnml, userdefs, ERR=8050, IOSTAT=istat)
  REWIND (iutnml)

  IF ( btrim < 0 ) THEN
    READ (iutnml, windowdefs, ERR=8075, IOSTAT=istat)
    REWIND (iutnml)
  ENDIF

!-------------------------------------------------------------------------------
! Crop blank spaces off ends of file names.
!-------------------------------------------------------------------------------

  file_gd  = TRIM( ADJUSTL(file_gd)  )
  file_hdr = TRIM( ADJUSTL(file_hdr) )

  DO n = 1, SIZE(file_mm)
    file_mm(n) = TRIM( ADJUSTL( file_mm(n) ) )
  ENDDO

  file_ter = TRIM( ADJUSTL(file_ter) )
  IF ( file_ter(1:7) == "no_file" ) file_ter = " "

  DO n = 1, SIZE(file_sat)
    file_sat(n) = TRIM( ADJUSTL( file_sat(n) ) )
  ENDDO

!-------------------------------------------------------------------------------
! Verify values of user-defined options.
!-------------------------------------------------------------------------------

  IF ( lddep == 0 ) THEN
    ltotg = 0
  ELSE IF ( lddep == 4 ) THEN
    ltotg = lddgas
  ELSE
    WRITE (6,9300) "LDDEP", lddep
    GOTO 1001
  ENDIF

  IF ( ( lpv /= 0 ) .AND. ( lpv /= 1 ) ) THEN
    WRITE (6,9300) "LPV", lpv
    GOTO 1001
  ENDIF

  IF ( ( lwout /= 0 ) .AND. ( lwout /= 1 ) ) THEN
    WRITE (6,9300) "LWOUT", lwout
    GOTO 1001
  ENDIF

  IF ( ( luvcout /= 0 ) .AND. ( luvcout /= 1 ) ) THEN
    WRITE (6,9300) "LUVCOUT", luvcout
    GOTO 1001
  ENDIF

  IF ( ( lsat /= 0 ) .AND. ( lsat /= 1 ) ) THEN
    WRITE (6,9300) "LSAT", lsat
    GOTO 1001
  ENDIF

  IF ( eradm < 6000000.0 ) THEN  ! user probably input km instead of meters
    WRITE (6,9400) eradm
    GOTO 1001
  ELSE
    rearth = eradm           ! fill value in CONST_MOD
    dg2m   = rearth * pi180  ! fill value in CONST_MOD now that REARTH is known
  ENDIF

  IF ( ( ( wrf_lc_ref_lat > 90.0 ) .OR. ( wrf_lc_ref_lat < -90.0 ) ) .AND.  &
       ( wrf_lc_ref_lat > -999.0 ) ) THEN
    WRITE (6,9500) wrf_lc_ref_lat
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Set start and end dates.  Ensure that "special characters" that separate
! components of date are set correctly.  If these are not set properly, the
! lexical time comparisons in the driver (mcip.F) will not work properly.
!-------------------------------------------------------------------------------

  mcip_start( 5: 5) = "-"         ;  mcip_end  ( 5: 5) = "-"
  mcip_start( 8: 8) = "-"         ;  mcip_end  ( 8: 8) = "-"
  mcip_start(11:11) = "-"         ;  mcip_end  (11:11) = "-"
  mcip_start(14:14) = ":"         ;  mcip_end  (14:14) = ":"
  mcip_start(17:24) = ":00.0000"  ;  mcip_end  (17:24) = ":00.0000"

  IF ( mcip_start > mcip_end ) THEN
    WRITE (6,9600) mcip_start, mcip_end
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Ensure that cell coordinates for diagnostic output are not negative.
!-------------------------------------------------------------------------------

  IF ( ( lprt_col < 0 ) .OR. ( lprt_row < 0 ) ) THEN
    WRITE (6,9650) lprt_col, lprt_row
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Determine actual CTM layers and fill arrays.
! If maximum value of CTMLAYS is -1.0 (which indicates that the namelist value
! was not filled or was set with -1.0), use this as a flag to process MCIP
! using the vertical structure of the input meteorology data with no collapsing.
!-------------------------------------------------------------------------------

  IF ( MAXVAL(ctmlays) < 0.0 ) THEN

    needlayers = .TRUE.  ! read layer structure in setup.F from input met file

  ELSE

    needlayers = .FALSE.  ! using layer definition from namelist file

    DO n = 1, maxlays
      IF ( ctmlays(n) < 0.0 ) EXIT
      IF ( n == 1 ) THEN
        IF ( ctmlays(n) /= 1.0 ) THEN
          WRITE (6,9700) ctmlays(1)
          GOTO 1001
        ENDIF
        CYCLE
      ENDIF
      IF ( ctmlays(n) >= ctmlays(n-1) ) THEN
        WRITE (6,9800) ctmlays(:)
        GOTO 1001
      ENDIF
    ENDDO

    IF ( ctmlays(n-1) /= 0.0 ) THEN
      WRITE (6,9900) ctmlays(n-1)
      GOTO 1001
    ENDIF

    nlays = n - 2  ! one less than number in array, and one less than loop counter

  ENDIF

!-------------------------------------------------------------------------------
! Store meteorology boundary removal in MCIPPARM.  If not windowing, set
! coordinate of lower-left corner of output domain with respect to
! input meteorology domain.  If windowing, ensure that I0 and J0 are
! not too small.
!-------------------------------------------------------------------------------

  nbdrytrim = btrim

  IF ( nbdrytrim >= 0 ) THEN
    x0 = nbdrytrim + 1
    y0 = nbdrytrim + 1
  ENDIF

  IF ( ( x0 < 1 ) .OR. ( y0 < 1 ) ) THEN
    WRITE (6,9950) x0, y0
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Verify that user-defined number of rows and columns is not less than 1.
! If windowing, use the user-defined input to set NCOLS and NROWS.
!-------------------------------------------------------------------------------

  IF ( ( ncolsin >= 1 ) .AND. ( nrowsin >= 1 ) ) THEN
    IF ( nbdrytrim < 0 ) THEN
      ncols = ncolsin
      nrows = nrowsin
    ENDIF
  ELSE
    WRITE (6,9975) ncolsin, nrowsin
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Calculate GRSTEP from INTVL.  GRSTEP is in STIME format (HHMMSS).
! Assume SS is 0.
!-------------------------------------------------------------------------------

  hh     = INT(intvl / 60)
  mm     = MOD(intvl,  60)

  grstep = ( hh * 10000 ) + ( mm * 100 )

!-------------------------------------------------------------------------------
! Close namelist file.
!-------------------------------------------------------------------------------

  CLOSE (iutnml)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutnml, TRIM(file_nml), istat
      GOTO 1001

 8025 WRITE (6,9050) iutnml, TRIM(file_nml), "filenames", istat
      GOTO 1001

 8050 WRITE (6,9050) iutnml, TRIM(file_nml), "userdefs", istat
      GOTO 1001

 8075 WRITE (6,9050) iutnml, TRIM(file_nml), "windowdefs", istat
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   ERROR OPENING NAMELIST FILE ON UNIT ', i3,      &
              /, 1x, '***   NAMELIST FILE NAME = ', a,                      &
              /, 1x, '***   IOSTAT = ', i4,                                 &
              /, 1x, 70('*'))

 9050 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   ERROR READING NAMELIST FILE ON UNIT ', i3,      &
              /, 1x, '***   NAMELIST FILE NAME = ', a,                      &
              /, 1x, '***   NAMELIST = ', a,                                &
              /, 1x, '***   IOSTAT = ', i4,                                 &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Invalid value of ', a, ' :', i4,                &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Invalid value of ERADM:', f12.3,                &
              /, 1x, '***   Verify that input value is in meters',          &
              /, 1x, 70('*'))

 9500 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Invalid value of WRF_LC_REF_LAT',               &
              /, 1x, '***   Value from namelist is ', f12.3,                &
              /, 1x, 70('*'))

 9600 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Start date must be before end date',            &
              /, 1x, '***   Input MCIP_START = ', a,                        &
              /, 1x, '***   Input MCIP_END   = ', a,                        &
              /, 1x, 70('*'))

 9650 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Invalid coordinates for cell for diagnostics',  &
              /, 1x, '***   Input LPRT_COL and LPRT_ROW are ', i4, 2x, i4,  &
              /, 1x, 70('*'))

 9700 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   First CTM layer must be 1.0',                   &
              /, 1x, '***   First input CTM layer is ', f7.4,               &
              /, 1x, 70('*'))

 9800 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Input CTM layers seem to be out of order',      &
              /, 1x, '***     Layers must be in descending order',          &
              /, 1x, '***   Input CTM layers are ', 50f7.4,                 &
              /, 1x, 70('*'))

 9900 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Last CTM layer must be 0.0',                    &
              /, 1x, '***   Last input CTM layer is ', f7.4,                &
              /, 1x, 70('*'))

 9950 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Minimum value for X0 and Y0 is 1',              &
              /, 1x, '***   User input X0 and Y0: ', 2(1x, i4),             &
              /, 1x, 70('*'))

 9975 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: READNML',                             &
              /, 1x, '***   Minimum value for NCOLS and NROWS is 1',        &
              /, 1x, '***   User input NCOLS and NROWS: ', 2(1x, i4),       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE readnml
