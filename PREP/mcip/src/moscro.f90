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

SUBROUTINE moscro (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Mosaic Output on Cross Points
! Purpose:  Compute and output time-dependent, cross-point parameters on
!           mosaic layers.
! Revised:  16 Mar 2018  Original version based on metcro.f90 from MCIPv4.4.
!                        (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE coord
  USE m3utilio
  USE files
  USE mosoutcom

  IMPLICIT NONE

  INTEGER                           :: c
  INTEGER                           :: col
  LOGICAL, SAVE                     :: first       = .TRUE.
  INTEGER                           :: idx
  CHARACTER(LEN=63)                 :: ifmt1
  INTEGER                           :: k
  INTEGER                           :: k1
  INTEGER                           :: k2
  INTEGER                           :: l
  INTEGER                           :: lvl
  INTEGER                           :: n
  CHARACTER(LEN=16),  PARAMETER     :: pname       = 'MOSCRO'
  INTEGER                           :: r
  INTEGER                           :: row
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime
  CHARACTER(LEN=2)                  :: str1
  CHARACTER(LEN=2)                  :: str2

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f6000 = "(1x, a9, 2x, f12.4, 2x, a)"

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING FILE ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR WRITING TO FILE ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! If first time, build headers for files.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ! Build common header.

    CALL comheader_mos (sdate, stime)

    !---------------------------------------------------------------------------
    ! Build and write header for MOSAIC_CRO file.
    !---------------------------------------------------------------------------

    DO idx = 1, mos3index
      vtype3d(idx) = m3real
      vname3d(idx) = mos3vname(idx)
      units3d(idx) = mos3units(idx)
      vdesc3d(idx) = mos3vdesc(idx)
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    ftype3d = grdded3
    nvars3d = mos3index
    nlays3d = nummosaic
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik
    tstep3d = grstep

    IF ( .NOT. open3 (mosaiccro, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(mosaiccro)
      CALL graceful_stop (pname)
    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Build common header.
!-------------------------------------------------------------------------------

  CALL comheader_mos (sdate, stime)

!-------------------------------------------------------------------------------
! Assign arrays in MOSAIC_CRO.
!-------------------------------------------------------------------------------

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik
      DO lvl = 1, nummosaic

        lufrac2_c  (col,row,lvl) = xlufrac2  (c,r,lvl)
        moscatidx_c(col,row,lvl) = xmoscatidx(c,r,lvl)
        lai_mos_c  (col,row,lvl) = xlai_mos  (c,r,lvl)

        IF ( xra_mos(c,r,lvl) < amiss3 ) THEN  ! BADVAL3 < AMISS3
          rai_mos_c  (col,row,lvl) = 0.0
        ELSE
          rai_mos_c  (col,row,lvl) = 1.0 / xra_mos(c,r,lvl)
        ENDIF

        IF ( xrs_mos(c,r,lvl) < amiss3 ) THEN  ! BADVAL3 < AMISS3
          rsi_mos_c  (col,row,lvl) = 0.0
        ELSE
          rsi_mos_c  (col,row,lvl) = 1.0 / xrs_mos(c,r,lvl)
        ENDIF

        tsk_mos_c  (col,row,lvl) = xtsk_mos  (c,r,lvl)
        znt_mos_c  (col,row,lvl) = xznt_mos  (c,r,lvl)

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Write MOSAIC_CRO data.
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (mosaiccro) ) THEN
    CALL m3err ('MOSCRO', sdate, stime,  &
                'Could not read DESC of ' // mosaiccro // ' file', .TRUE.)
  ENDIF

  DO l = 1, mos3index
    IF ( .NOT. write3 (mosaiccro, vname3d(l), sdate, stime,  &
                       mos3(1,1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(mosaiccro)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  k1 = nummosaic / 5
  k2 = MOD(nummosaic, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)),/,10x," &
        & // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4),/,10x," // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4))"
    ENDIF
  ENDIF

  WRITE (*,'(/,a,/)') '- MOSCRO: Printing sample cells in output grid'

  DO n = 1, mos3index
    WRITE (*,ifmt1) TRIM(mos3vname(n)),(mos3(lprt_col,lprt_row,k,n),k=1,nummosaic)
  ENDDO

END SUBROUTINE moscro
