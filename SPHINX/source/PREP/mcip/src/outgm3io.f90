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

SUBROUTINE outgm3io (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Output GRID Fields in Models-3 I/O API
! Purpose:  Output time-independent fields in Models-3 I/O API.
! Revised:  17 Dec 2018  Original version in MCIPv5.0.  Subsumes part of
!                        gridout.f90 from MCIPv4.5.  (T. Spero)
!                        CCTM grid to a separate subroutine.  Subsumed output
!                        of fractional land use from lucro.f90.  (T. Spero)
!           09 Jul 2019  Corrected output time step to 0 for time-invariant
!                        file LUFRAC_CRO.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars, ONLY: xlusrc
  USE ctmvars
  USE coord
  USE files
  USE m3utilio

  IMPLICIT NONE

  INTEGER                           :: n
  INTEGER                           :: nchar
  CHARACTER(LEN=16),  PARAMETER     :: pname       = 'OUTGM3IO'
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime
  REAL(8)                           :: xorigdot
  REAL(8)                           :: yorigdot

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING FILE ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR WRITING TO FILE ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Build common header for I/O API files.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Write GRID_CRO_2D.
!-------------------------------------------------------------------------------

  DO n = 1, nfld2dxy

    vtype3d(n) = m3real

    nchar = LEN_TRIM(fld2dxy(n)%fldname)
    vname3d(n)(1:nchar)  = TRIM(fld2dxy(n)%fldname)
    vname3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld2dxy(n)%units)
    units3d(n)(1:nchar)  = TRIM(fld2dxy(n)%units)
    units3d(n)(nchar+1:) = ' '

    IF ( TRIM(vname3d(n)) == 'DLUSE' ) THEN
      nchar = LEN_TRIM(TRIM(fld2dxy(n)%long_name) // ' ' // TRIM(xlusrc))
      vdesc3d(n)(1:nchar)  = TRIM(fld2dxy(n)%long_name) // ' ' // TRIM(xlusrc)
      vdesc3d(n)(nchar+1:) = ' '
    ELSE
      nchar = LEN_TRIM(fld2dxy(n)%long_name)
      vdesc3d(n)(1:nchar)  = TRIM(fld2dxy(n)%long_name)
      vdesc3d(n)(nchar+1:) = ' '
    ENDIF

  ENDDO

  gdnam3d = TRIM(grdnam) // '_CROSS'

  xorig3d = xorig_gd
  yorig3d = yorig_gd
  ncols3d = ncols
  nrows3d = nrows
  nthik3d = nthik

  ftype3d = grdded3
  nvars3d = nfld2dxy
  nlays3d = 1
  tstep3d = 0

  IF ( .NOT. open3 (gridcro2d, fsunkn3, pname) ) THEN
    WRITE (*,f9000) TRIM(pname), TRIM(gridcro2d)
    CALL graceful_stop (pname)
  ENDIF


  IF ( .NOT. desc3 (gridcro2d) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // gridcro2d // ' file', .TRUE.)
  ENDIF

  DO n = 1, nfld2dxy
    IF ( .NOT. write3 (gridcro2d, vname3d(n), sdate, stime,  &
                       fld2dxy(n)%fld) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write GRID_BDY_2D.  Header is the same as GRID_CRO_2D except for file type.
!-------------------------------------------------------------------------------

  ftype3d = bndary3

  IF ( .NOT. open3 (gridbdy2d, fsunkn3, pname) ) THEN
    WRITE (*,f9000) TRIM(pname), TRIM(gridbdy2d)
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. desc3 (gridbdy2d) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // gridbdy2d // ' file', .TRUE.)
  ENDIF

  DO n = 1, nfld2dxy
    IF ( .NOT. write3 (gridbdy2d, vname3d(n), sdate, stime,  &
                       fld2dxy(n)%bdy) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridbdy2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write GRID_DOT_2D.
!-------------------------------------------------------------------------------

  ! Compute XORIGDOT and YORIGDOT.  Assume (XORIG_GD, YORIG_GD) are
  ! for cross cells without boundary.

  xorigdot = xorig_gd - (0.5 * xcell3d)
  yorigdot = yorig_gd - (0.5 * ycell3d)

  DO n = 1, nfld2dxy_d

    vtype3d(n) = m3real

    nchar = LEN_TRIM(fld2dxy_d(n)%fldname)
    vname3d(n)(1:nchar)  = TRIM(fld2dxy_d(n)%fldname)
    vname3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld2dxy_d(n)%units)
    units3d(n)(1:nchar)  = TRIM(fld2dxy_d(n)%units)
    units3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld2dxy_d(n)%long_name)
    vdesc3d(n)(1:nchar)  = TRIM(fld2dxy_d(n)%long_name)
    vdesc3d(n)(nchar+1:) = ' '

  ENDDO

  gdnam3d = TRIM(grdnam) // '_DOT'

  xorig3d = xorigdot
  yorig3d = yorigdot
  ncols3d = ncols + 1
  nrows3d = nrows + 1
  nthik3d = nthik

  ftype3d = grdded3
  nvars3d = nfld2dxy_d
  nlays3d = 1
  tstep3d = 0

  IF ( .NOT. open3 (griddot2d, fsunkn3, pname) ) THEN
    WRITE (*,f9000) TRIM(pname), TRIM(griddot2d)
    CALL graceful_stop (pname)
  ENDIF

  IF ( .NOT. desc3 (griddot2d) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // griddot2d // ' file', .TRUE.)
  ENDIF

  DO n = 1, nfld2dxy_d
    IF ( .NOT. write3 (griddot2d, vname3d(n), sdate, stime,  &
                       fld2dxy_d(n)%fld) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(griddot2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write LUFRAC_CRO.
!-------------------------------------------------------------------------------

  IF ( iflufrc ) THEN  ! fractional land use data are available

    CALL comheader_lufrac (sdate, stime)

    DO n = 1, nfld3dxyl

      vtype3d(n) = m3real

      nchar = LEN_TRIM(fld3dxyl(n)%fldname)
      vname3d(n)(1:nchar)  = TRIM(fld3dxyl(n)%fldname)
      vname3d(n)(nchar+1:) = ' '

      nchar = LEN_TRIM(fld3dxyl(n)%units)
      units3d(n)(1:nchar)  = fld3dxyl(n)%units
      units3d(n)(nchar+1:) = ' '

      nchar = LEN_TRIM(fld3dxyl(n)%long_name)
      vdesc3d(n)(1:nchar)  = fld3dxyl(n)%long_name
      vdesc3d(n)(nchar+1:) = ' '

    ENDDO

    gdnam3d = TRIM(grdnam) // '_CROSS'

    ftype3d = grdded3
    nvars3d = nfld3dxyl
    nlays3d = nummetlu
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik
    tstep3d = 0

    IF ( .NOT. open3 (lufraccro, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(lufraccro)
      CALL graceful_stop (pname)
    ENDIF

    IF ( .NOT. desc3 (lufraccro) ) THEN
      CALL m3err ('LUCRO', sdate, stime,  &
                  'Could not read DESC of ' // lufraccro // ' file', .TRUE.)
    ENDIF

    DO n = 1, nfld3dxyl
      IF ( .NOT. write3 (lufraccro, vname3d(n), sdate, stime,  &
                         fld3dxyl(n)%fld) ) THEN
        WRITE (*,f9100) TRIM(pname), TRIM(lufraccro)
        CALL graceful_stop (pname)
      ENDIF
    ENDDO

  ENDIF

END SUBROUTINE outgm3io
