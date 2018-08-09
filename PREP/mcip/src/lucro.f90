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

SUBROUTINE lucro (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Land Use Fraction Output on Cross Points
! Purpose:  Compute and output time-dependent, cross-point parameters in
!           land use categories.
! Revised:  13 Feb 2018  Original version based on metcro.f90 from MCIPv4.4.
!                        (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE coord
  USE m3utilio
  USE files
  USE luoutcom

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
  CHARACTER(LEN=16),  PARAMETER     :: pname       = 'LUCRO'
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

    CALL comheader_lufrac (sdate, stime)

    !---------------------------------------------------------------------------
    ! Build and write header for MOSAIC_CRO file.
    !---------------------------------------------------------------------------

    DO idx = 1, lu3index
      vtype3d(idx) = m3real
      vname3d(idx) = lu3vname(idx)
      units3d(idx) = lu3units(idx)
      vdesc3d(idx) = lu3vdesc(idx)
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    ftype3d = grdded3
    nvars3d = lu3index
    nlays3d = nummetlu
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik
    tstep3d = grstep

    IF ( .NOT. open3 (lufraccro, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(lufraccro)
      CALL graceful_stop (pname)
    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Build common header.
!-------------------------------------------------------------------------------

  CALL comheader_lufrac (sdate, stime)

!-------------------------------------------------------------------------------
! Assign arrays in LUFRAC_CRO.
!-------------------------------------------------------------------------------

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik
      DO lvl = 1, nummetlu

        lufrac_c(col,row,lvl) = xluse(c,r,lvl)

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Write LUFRAC_CRO data.
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (lufraccro) ) THEN
    CALL m3err ('LUCRO', sdate, stime,  &
                'Could not read DESC of ' // lufraccro // ' file', .TRUE.)
  ENDIF

  DO l = 1, lu3index
    IF ( .NOT. write3 (lufraccro, vname3d(l), sdate, stime,  &
                       lu3(1,1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(lufraccro)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  k1 = nummetlu / 5
  k2 = MOD(nummetlu, 5)

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

  WRITE (*,'(/,a,/)') '- LUCRO: Printing sample cells in output grid'

  DO n = 1, lu3index
    WRITE (*,ifmt1) TRIM(lu3vname(n)),(lu3(lprt_col,lprt_row,k,n),k=1,nummetlu)
  ENDDO

END SUBROUTINE lucro
