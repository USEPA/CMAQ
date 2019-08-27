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

SUBROUTINE outcm3io (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Output CTM Fields in Models-3 I/O API
! Purpose:  Output time-varying fields in Models-3 I/O API.
! Revised:  17 Dec 2018  Original version in MCIPv5.0.  Subsumes parts of
!                        metcro.f90, metdot.f90, soilcro.f90, and moscro.f90
!                        from MCIPv4.5.  (T. Spero)
!           19 Jun 2019  Added optional variables from KF convective scheme
!                        with radiative feedbacks.  (T. Spero)
!           15 Jul 2019  Corrected error in setting units for 3D microphysics
!                        fields.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE ctmvars
  USE coord
  USE files
  USE vgrd
  USE m3utilio

  IMPLICIT NONE

  REAL,               PARAMETER     :: epsilonq    = 1.0e-30
  LOGICAL, SAVE                     :: first       = .TRUE.
  INTEGER                           :: ii
  INTEGER                           :: n
  INTEGER                           :: nchar
  CHARACTER(LEN=16),  PARAMETER     :: pname       = 'OUTCM3IO'
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime

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
! Build common header for I/O API output.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Write MET_CRO_2D.
!-------------------------------------------------------------------------------

  DO n = 1, nfld2dxyt

    vtype3d(n) = m3real

    nchar = LEN_TRIM(fld2dxyt(n)%fldname)
    vname3d(n)(1:nchar)  = TRIM(fld2dxyt(n)%fldname)
    vname3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld2dxyt(n)%units)
    units3d(n)(1:nchar)  = TRIM(fld2dxyt(n)%units)
    units3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld2dxyt(n)%long_name)
    vdesc3d(n)(1:nchar)  = TRIM(fld2dxyt(n)%long_name)
    vdesc3d(n)(nchar+1:) = ' '

  ENDDO

  gdnam3d = TRIM(grdnam) // '_CROSS'

  xorig3d = xorig_gd
  yorig3d = yorig_gd
  ncols3d = ncols
  nrows3d = nrows
  nthik3d = nthik

  ftype3d = grdded3
  nvars3d = nfld2dxyt
  nlays3d = 1
  ncols3d = ncols
  nrows3d = nrows
  nthik3d = nthik
  tstep3d = grstep

  IF ( first ) THEN
    IF ( .NOT. open3 (metcro2d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. desc3 (metcro2d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metcro2d // ' file', .TRUE.)
  ENDIF

  DO n = 1, nfld2dxyt
    IF ( .NOT. write3 (metcro2d, vname3d(n), sdate, stime,  &
                       fld2dxyt(n)%fld) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write MET_CRO_3D.
!-------------------------------------------------------------------------------

  DO n = 1, nfld3dxyzt

    vtype3d(n) = m3real

    nchar = LEN_TRIM(fld3dxyzt(n)%fldname)
    vname3d(n)(1:nchar)  = TRIM(fld3dxyzt(n)%fldname)
    vname3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld3dxyzt(n)%units)
    units3d(n)(1:nchar)  = TRIM(fld3dxyzt(n)%units)
    units3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld3dxyzt(n)%long_name)
    vdesc3d(n)(1:nchar)  = TRIM(fld3dxyzt(n)%long_name)
    vdesc3d(n)(nchar+1:) = ' '

  ENDDO

  IF ( nqspecies > 0 ) THEN

    DO ii = 1, nfld3dxyzt_q

      n = nfld3dxyzt + ii

      vtype3d(n) = m3real

      nchar = LEN_TRIM(fld3dxyzt_q(ii)%fldname)
      vname3d(n)(1:nchar)  = TRIM(fld3dxyzt_q(ii)%fldname)
      vname3d(n)(nchar+1:) = ' '

      nchar = LEN_TRIM(fld3dxyzt_q(ii)%units)
      units3d(n)(1:nchar)  = TRIM(fld3dxyzt_q(ii)%units)
      units3d(n)(nchar+1:) = ' '

      nchar = LEN_TRIM(fld3dxyzt_q(ii)%long_name)
      vdesc3d(n)(1:nchar)  = TRIM(fld3dxyzt_q(ii)%long_name)
      vdesc3d(n)(nchar+1:) = ' '

    ENDDO

  ENDIF

  gdnam3d = TRIM(grdnam) // '_CROSS'

  xorig3d = xorig_gd
  yorig3d = yorig_gd
  ncols3d = ncols
  nrows3d = nrows
  nthik3d = nthik

  ftype3d = grdded3
  nvars3d = nfld3dxyzt + nfld3dxyzt_q
  nlays3d = nlays
  ncols3d = ncols
  nrows3d = nrows
  nthik3d = nthik
  tstep3d = grstep

  IF ( first ) THEN
    IF ( .NOT. open3 (metcro3d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(metcro3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. desc3 (metcro3d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metcro3d // ' file', .TRUE.)
  ENDIF

  WHERE ( ABS(c_qv%fld(:,:,:)) < epsilonq ) c_qv%fld(:,:,:) = 0.0

  IF ( ASSOCIATED(c_qc) ) THEN
    WHERE ( ABS(c_qc%fld(:,:,:)) < epsilonq ) c_qc%fld(:,:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qr) ) THEN
    WHERE ( ABS(c_qr%fld(:,:,:)) < epsilonq ) c_qr%fld(:,:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qi) ) THEN
    WHERE ( ABS(c_qi%fld(:,:,:)) < epsilonq ) c_qi%fld(:,:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qs) ) THEN
    WHERE ( ABS(c_qs%fld(:,:,:)) < epsilonq ) c_qs%fld(:,:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qg) ) THEN
    WHERE ( ABS(c_qg%fld(:,:,:)) < epsilonq ) c_qg%fld(:,:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qc_cu) ) THEN
    WHERE ( ABS(c_qc_cu%fld(:,:,:)) < epsilonq ) c_qc_cu%fld(:,:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qi_cu) ) THEN
    WHERE ( ABS(c_qi_cu%fld(:,:,:)) < epsilonq ) c_qi_cu%fld(:,:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_cldfra_dp) ) THEN
    WHERE ( ABS(c_cldfra_dp%fld(:,:,:)) < epsilonq ) c_cldfra_dp%fld(:,:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_cldfra_sh) ) THEN
    WHERE ( ABS(c_cldfra_sh%fld(:,:,:)) < epsilonq ) c_cldfra_sh%fld(:,:,:) = 0.0
  ENDIF

  DO n = 1, nfld3dxyzt
    IF ( .NOT. write3 (metcro3d, vname3d(n), sdate, stime,  &
                       fld3dxyzt(n)%fld) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  IF ( nqspecies > 0 ) THEN
    DO n = 1, nfld3dxyzt_q
      IF ( .NOT. write3 (metcro3d, vname3d(nfld3dxyzt+n), sdate, stime,  &
                         fld3dxyzt_q(n)%fld) ) THEN
        WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
  ENDIF

!-------------------------------------------------------------------------------
! Write MET_BDY_3D.  Header is the same as MET_CRO_3D except for file type.
!-------------------------------------------------------------------------------

  ftype3d = bndary3

  IF ( first ) THEN
    IF ( .NOT. open3 (metbdy3d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(metbdy3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. desc3 (metbdy3d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metbdy3d // ' file', .TRUE.)
  ENDIF

  WHERE ( ABS(c_qv%bdy(:,:)) < epsilonq ) c_qv%bdy(:,:) = 0.0

  IF ( ASSOCIATED(c_qc) ) THEN
    WHERE ( ABS(c_qc%bdy(:,:)) < epsilonq ) c_qc%bdy(:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qr) ) THEN
    WHERE ( ABS(c_qr%bdy(:,:)) < epsilonq ) c_qr%bdy(:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qi) ) THEN
    WHERE ( ABS(c_qi%bdy(:,:)) < epsilonq ) c_qi%bdy(:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qs) ) THEN
    WHERE ( ABS(c_qs%bdy(:,:)) < epsilonq ) c_qs%bdy(:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qg) ) THEN
    WHERE ( ABS(c_qg%bdy(:,:)) < epsilonq ) c_qg%bdy(:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qc_cu) ) THEN
    WHERE ( ABS(c_qc_cu%bdy(:,:)) < epsilonq ) c_qc_cu%bdy(:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_qi_cu) ) THEN
    WHERE ( ABS(c_qi_cu%bdy(:,:)) < epsilonq ) c_qi_cu%bdy(:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_cldfra_dp) ) THEN
    WHERE ( ABS(c_cldfra_dp%bdy(:,:)) < epsilonq ) c_cldfra_dp%bdy(:,:) = 0.0
  ENDIF

  IF ( ASSOCIATED(c_cldfra_sh) ) THEN
    WHERE ( ABS(c_cldfra_sh%bdy(:,:)) < epsilonq ) c_cldfra_sh%bdy(:,:) = 0.0
  ENDIF

  DO n = 1, nfld3dxyzt
    IF ( .NOT. write3 (metbdy3d, vname3d(n), sdate, stime,  &
                       fld3dxyzt(n)%bdy) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metbdy3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  IF ( nqspecies > 0 ) THEN
    DO n = 1, nfld3dxyzt_q
      IF ( .NOT. write3 (metbdy3d, vname3d(nfld3dxyzt+n), sdate, stime,  &
                         fld3dxyzt_q(n)%bdy) ) THEN
        WRITE (*,f9100) TRIM(pname), TRIM(metbdy3d)
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
  ENDIF

!-------------------------------------------------------------------------------
! Write MET_DOT_3D.
!-------------------------------------------------------------------------------

  DO n = 1, nfld3dxyzt_d

    vtype3d(n) = m3real

    nchar = LEN_TRIM(fld3dxyzt_d(n)%fldname)
    vname3d(n)(1:nchar)  = TRIM(fld3dxyzt_d(n)%fldname)
    vname3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld3dxyzt_d(n)%units)
    units3d(n)(1:nchar)  = TRIM(fld3dxyzt_d(n)%units)
    units3d(n)(nchar+1:) = ' '

    nchar = LEN_TRIM(fld3dxyzt_d(n)%long_name)
    vdesc3d(n)(1:nchar)  = TRIM(fld3dxyzt_d(n)%long_name)
    vdesc3d(n)(nchar+1:) = ' '

  ENDDO

  gdnam3d = TRIM(grdnam) // '_DOT'

  xorig3d = xorig_gd - 0.5d0 * xcell_gd
  yorig3d = yorig_gd - 0.5d0 * ycell_gd
  ncols3d = ncols + 1
  nrows3d = nrows + 1
  nthik3d = nthik

  ftype3d = grdded3
  nvars3d = nfld3dxyzt_d
  nlays3d = nlays
  tstep3d = grstep

  IF ( first ) THEN
    IF ( .NOT. open3 (metdot3d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(metdot3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( .NOT. desc3 (metdot3d) ) THEN
    CALL m3err ('METDOT', sdate, stime,  &
                'Could not read DESC of ' // metdot3d // ' file', .TRUE.)
  ENDIF

  DO n = 1, nfld3dxyzt_d
    IF ( .NOT. write3 (metdot3d, vname3d(n), sdate, stime,  &
                       fld3dxyzt_d(n)%fld) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metdot3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write SOI_CRO.
!-------------------------------------------------------------------------------

  IF ( ifsoil ) THEN

    CALL comheader_soi (sdate, stime)

    DO n = 1, nfld3dxyst

      vtype3d(n) = m3real

      nchar = LEN_TRIM(fld3dxyst(n)%fldname)
      vname3d(n)(1:nchar)  = TRIM(fld3dxyst(n)%fldname)
      vname3d(n)(nchar+1:) = ' '

      nchar = LEN_TRIM(fld3dxyst(n)%units)
      units3d(n)(1:nchar)  = TRIM(fld3dxyst(n)%units)
      units3d(n)(nchar+1:) = ' '

      nchar = LEN_TRIM(fld3dxyst(n)%long_name)
      vdesc3d(n)(1:nchar)  = TRIM(fld3dxyst(n)%long_name)
      vdesc3d(n)(nchar+1:) = ' '

    ENDDO

    gdnam3d = TRIM(grdnam) // '_CROSS'

    ftype3d = grdded3
    nvars3d = nfld3dxyst
    nlays3d = metsoi
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik
    tstep3d = grstep

    IF ( first ) THEN
      IF ( .NOT. open3 (soicro, fsunkn3, pname) ) THEN
        WRITE (*,f9000) TRIM(pname), TRIM(soicro)
        CALL graceful_stop (pname)
      ENDIF
    ENDIF

    IF ( .NOT. desc3 (soicro) ) THEN
      CALL m3err ('SOICRO', sdate, stime,  &
                  'Could not read DESC of ' // soicro // ' file', .TRUE.)
    ENDIF

    DO n = 1, nfld3dxyst
      IF ( .NOT. write3 (soicro, vname3d(n), sdate, stime,  &
                         fld3dxyst(n)%fld) ) THEN
        WRITE (*,f9100) TRIM(pname), TRIM(soicro)
        CALL graceful_stop (pname)
      ENDIF
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Write MOSAIC_CRO.
!-------------------------------------------------------------------------------

  IF ( ifmosaic ) THEN

    CALL comheader_mos (sdate, stime)

    DO n = 1, nfld3dxymt

      vtype3d(n) = m3real

      nchar = LEN_TRIM(fld3dxymt(n)%fldname)
      vname3d(n)(1:nchar)  = TRIM(fld3dxymt(n)%fldname)
      vname3d(n)(nchar+1:) = ' '

      nchar = LEN_TRIM(fld3dxymt(n)%units)
      units3d(n)(1:nchar)  = TRIM(fld3dxymt(n)%units)
      units3d(n)(nchar+1:) = ' '

      nchar = LEN_TRIM(fld3dxymt(n)%long_name)
      vdesc3d(n)(1:nchar)  = TRIM(fld3dxymt(n)%long_name)
      vdesc3d(n)(nchar+1:) = ' '

    ENDDO

    gdnam3d = TRIM(grdnam) // '_CROSS'

    ftype3d = grdded3
    nvars3d = nfld3dxymt
    nlays3d = nummosaic
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik
    tstep3d = grstep

    IF ( first ) THEN
      IF ( .NOT. open3 (mosaiccro, fsunkn3, pname) ) THEN
        WRITE (*,f9000) TRIM(pname), TRIM(mosaiccro)
        CALL graceful_stop (pname)
      ENDIF
    ENDIF

    IF ( .NOT. desc3 (mosaiccro) ) THEN
      CALL m3err ('MOSCRO', sdate, stime,  &
                  'Could not read DESC of ' // mosaiccro // ' file', .TRUE.)
    ENDIF

    DO n = 1, nfld3dxymt
      IF ( .NOT. write3 (mosaiccro, vname3d(n), sdate, stime,  &
                         fld3dxymt(n)%fld) ) THEN
        WRITE (*,f9100) TRIM(pname), TRIM(mosaiccro)
        CALL graceful_stop (pname)
      ENDIF
    ENDDO

  ENDIF

  first = .FALSE.

END SUBROUTINE outcm3io
