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

SUBROUTINE outclog

!-------------------------------------------------------------------------------
! Name:     Output CTM -- Log
! Purpose:  Output sample of time-varying fields to log file.
! Revised:  17 Dec 2018  Original version in MCIPv5.0.  Subsumes parts of
!                        metcro.f90, metdot.f90, soilcro.f90, and moscro.f90
!                        from MCIPv4.5.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE ctmvars

  IMPLICIT NONE

  CHARACTER(LEN=63)                 :: ifmt1
  INTEGER                           :: k
  INTEGER                           :: k1
  INTEGER                           :: k2
  INTEGER                           :: n
  CHARACTER(LEN=2)                  :: str1
  CHARACTER(LEN=2)                  :: str2

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f6000 = "(1x, a9, 2x, f12.4, 2x, a)"

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  k1 = nlays / 5
  k2 = MOD(nlays, 5)

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

  WRITE (*,'(/,a,/)') '- METCRO: Printing sample cells in output grid'

  DO n = 1, nfld2dxyt
    WRITE (*,f6000) TRIM(fld2dxyt(n)%fldname),   &
                         fld2dxyt(n)%fld(lprt_col,lprt_row),  &
                    TRIM(fld2dxyt(n)%units)
  ENDDO

  DO n = 1, nfld3dxyzt
    WRITE (*,ifmt1) TRIM(fld3dxyzt(n)%fldname),   &
                        (fld3dxyzt(n)%fld(lprt_col,lprt_row,k),k=1,nlays)
  ENDDO

  IF ( nqspecies > 0 ) THEN
    DO n = 1, nfld3dxyzt_q
      WRITE (*,ifmt1) TRIM(fld3dxyzt_q(n)%fldname),   &
                          (fld3dxyzt_q(n)%fld(lprt_col,lprt_row,k),k=1,nlays)
    ENDDO
  ENDIF

  WRITE (*,'(/,a,/)') '- METDOT: Printing sample cells in output grid'

  DO n = 1, nfld3dxyzt_d
    WRITE (*,ifmt1) TRIM(fld3dxyzt_d(n)%fldname),   &
                        (fld3dxyzt_d(n)%fld(lprt_col,lprt_row,k),k=1,nlays)
  ENDDO

  IF ( ifsoil ) THEN

    k1 = metsoi / 5
    k2 = MOD(metsoi, 5)

    WRITE ( str1, '(i2)' ) k1 - 1
    WRITE ( str2, '(i2)' ) k2

    IF ( (k1 - 1) > 0 ) THEN
      IF ( k2 > 0 ) THEN
        ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)),/,10x,"&
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

    WRITE (*,'(/,a,/)') '- SOICRO: Printing sample cells in output grid'

    DO n = 1, nfld3dxyst
      WRITE (*,ifmt1) TRIM(fld3dxyst(n)%fldname),  &
                          (fld3dxyst(n)%fld(lprt_col,lprt_row,k),k=1,metsoi)
    ENDDO

  ENDIF  ! ifsoil

END SUBROUTINE outclog
