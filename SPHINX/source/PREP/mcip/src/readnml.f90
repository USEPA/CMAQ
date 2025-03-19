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

SUBROUTINE readnml

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
!           09 Sep 2010  Removed option to compute dry deposition velocities
!                        in MCIP.  (T. Otte)
!           29 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Improved error
!                        handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           18 Dec 2018  Removed support for MM5v3 input.  Added runtime option
!                        to choose output format.  Removed runtime option to
!                        not output time-independent files.  (T. Spero)
!           17 Jun 2019  Removed layer collapsing.  Changed variable LUVCOUT to
!                        LUVBOUT to reflect that the default 3D wind components
!                        are on the Arakawa-C staggered grid, and the optional
!                        additional 3D winds are now on the Arakawa-B staggered
!                        grid.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE const, ONLY: rearth, dg2m, pi180
  USE files

  IMPLICIT NONE

  INTEGER                           :: btrim
  INTEGER                           :: hh
  INTEGER                           :: istat
  INTEGER                           :: mm
  INTEGER                           :: n
  INTEGER                           :: ncolsin
  INTEGER                           :: nrowsin
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'READNML'

  NAMELIST /filenames/   file_gd, file_mm, file_geo, ioform

  NAMELIST /userdefs/    lpv, lwout, luvbout,                 &
                         eradm, mcip_start, mcip_end, intvl,  &
                         coordnam, grdnam,                    &
                         btrim, lprt_col, lprt_row,           &
                         wrf_lc_ref_lat

  NAMELIST /windowdefs/  x0, y0, ncolsin, nrowsin

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING NAMELIST FILE ON UNIT ', i3, &
    & /, 1x, '***   NAMELIST FILE NAME = ', a, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9050 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR READING NAMELIST FILE ON UNIT ', i3, &
    & /, 1x, '***   NAMELIST FILE NAME = ', a, &
    & /, 1x, '***   NAMELIST = ', a, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Invalid value of ', a, ' :', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Invalid value of ERADM:', f12.3, &
    & /, 1x, '***   Verify that input value is in meters', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9500 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Invalid value of WRF_LC_REF_LAT', &
    & /, 1x, '***   Value from namelist is ', f12.3, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9600 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Start date must be before end date', &
    & /, 1x, '***   Input MCIP_START = ', a, &
    & /, 1x, '***   Input MCIP_END   = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9650 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Invalid coordinates for cell for diagnostics', &
    & /, 1x, '***   Input LPRT_COL and LPRT_ROW are ', i4, 2x, i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9950 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Minimum value for X0 and Y0 is 1', &
    & /, 1x, '***   User input X0 and Y0: ', 2(1x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9975 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Minimum value for NCOLS and NROWS is 1', &
    & /, 1x, '***   User input NCOLS and NROWS: ', 2(1x, i4), &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Open namelist file.
!-------------------------------------------------------------------------------

  OPEN (iutnml, FILE=file_nml, STATUS='OLD', IOSTAT=istat)

  IF ( istat > 0 ) THEN
    WRITE (*,f9000) TRIM(pname), iutnml, TRIM(file_nml), istat
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Initialize input file names.
!-------------------------------------------------------------------------------

  file_gd     = "GRIDDESC"
  file_mm(:)  = " "
  file_geo    = " "

!-------------------------------------------------------------------------------
! Set default value for user-selected options.
!
!   LPV:     0 = Do not compute and output 3D potential vorticity
!            1 = Compute and output 3D potential vorticity
!
!   LWOUT:   0 = Do not output vertical velocity
!            1 = Output vertical velocity
!
!   LUVBOUT: 0 = Do not output u- and v-component winds on B-staggered grid
!            1 = Output u- and v-component winds on B-staggered grid
!                in addition to the C-staggered grid
!
!   IOFORM:  1 = Models-3 I/O API
!            2 = netCDF
!-------------------------------------------------------------------------------

  lpv        = 0
  lwout      = 0
  luvbout    = 0
  ioform     = 1

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

  READ (iutnml, filenames, IOSTAT=istat)
  IF ( istat > 0 ) THEN
    WRITE (*,f9050) TRIM(pname), iutnml, TRIM(file_nml), "filenames", istat
    CALL graceful_stop (pname)
  ENDIF
  REWIND (iutnml)

  READ (iutnml, userdefs, IOSTAT=istat)
  IF ( istat > 0 ) THEN
    WRITE (*,f9050) TRIM(pname), iutnml, TRIM(file_nml), "userdefs", istat
    CALL graceful_stop (pname)
  ENDIF
  REWIND (iutnml)

  IF ( btrim < 0 ) THEN
    READ (iutnml, windowdefs, IOSTAT=istat)
    IF ( istat > 0 ) THEN
      WRITE (*,f9050) TRIM(pname), iutnml, TRIM(file_nml), "windowdefs", istat
      CALL graceful_stop (pname)
    ENDIF
    REWIND (iutnml)
  ENDIF

!-------------------------------------------------------------------------------
! Crop blank spaces off ends of file names.
!-------------------------------------------------------------------------------

  file_gd  = TRIM( ADJUSTL(file_gd)  )

  DO n = 1, SIZE(file_mm)
    file_mm(n) = TRIM( ADJUSTL( file_mm(n) ) )
  ENDDO

  file_geo = TRIM( ADJUSTL(file_geo) )
  IF ( file_geo(1:7) == "no_file" ) file_geo = " "

!-------------------------------------------------------------------------------
! Verify values of user-defined options.
!-------------------------------------------------------------------------------

  IF ( ( lpv /= 0 ) .AND. ( lpv /= 1 ) ) THEN
    WRITE (*,f9300) TRIM(pname), "LPV", lpv
    CALL graceful_stop (pname)
  ENDIF

  IF ( ( lwout /= 0 ) .AND. ( lwout /= 1 ) ) THEN
    WRITE (*,f9300) TRIM(pname), "LWOUT", lwout
    CALL graceful_stop (pname)
  ENDIF

  IF ( ( luvbout /= 0 ) .AND. ( luvbout /= 1 ) ) THEN
    WRITE (*,f9300) TRIM(pname), "LUVBOUT", luvbout
    CALL graceful_stop (pname)
  ENDIF

  IF ( ( ioform /= 1 ) .AND. ( ioform /= 2 ) ) THEN
    WRITE (*,f9300) TRIM(pname), "IOFORM", ioform
    CALL graceful_stop (pname)
  ENDIF

  IF ( eradm < 6000000.0 ) THEN  ! user probably input km instead of meters
    WRITE (*,f9400) TRIM(pname), eradm
    CALL graceful_stop (pname)
  ELSE
    rearth = eradm           ! fill value in CONST_MOD
    dg2m   = rearth * pi180  ! fill value in CONST_MOD now that REARTH is known
  ENDIF

  IF ( ( ( wrf_lc_ref_lat > 90.0 ) .OR. ( wrf_lc_ref_lat < -90.0 ) ) .AND.  &
       ( wrf_lc_ref_lat > -999.0 ) ) THEN
    WRITE (*,f9500) TRIM(pname), wrf_lc_ref_lat
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Set start and end dates.  Ensure that "special characters" that separate
! components of date are set correctly.  If these are not set properly, the
! lexical time comparisons in the driver (mcip.f90) will not work properly.
!-------------------------------------------------------------------------------

  mcip_start( 5: 5) = "-"         ;  mcip_end  ( 5: 5) = "-"
  mcip_start( 8: 8) = "-"         ;  mcip_end  ( 8: 8) = "-"
  mcip_start(11:11) = "-"         ;  mcip_end  (11:11) = "-"
  mcip_start(14:14) = ":"         ;  mcip_end  (14:14) = ":"
  mcip_start(17:24) = ":00.0000"  ;  mcip_end  (17:24) = ":00.0000"

  IF ( mcip_start > mcip_end ) THEN
    WRITE (*,f9600) TRIM(pname), mcip_start, mcip_end
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Ensure that cell coordinates for diagnostic output are not negative.
!-------------------------------------------------------------------------------

  IF ( ( lprt_col < 0 ) .OR. ( lprt_row < 0 ) ) THEN
    WRITE (*,f9650) TRIM(pname), lprt_col, lprt_row
    CALL graceful_stop (pname)
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
    WRITE (*,f9950) TRIM(pname), x0, y0
    CALL graceful_stop (pname)
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
    WRITE (*,f9975) TRIM(pname), ncolsin, nrowsin
    CALL graceful_stop (pname)
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

END SUBROUTINE readnml
