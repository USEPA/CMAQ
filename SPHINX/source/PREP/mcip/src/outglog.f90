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

SUBROUTINE outglog

!-------------------------------------------------------------------------------
! Name:     Output GRID -- Log
! Purpose:  Output sample of time-independent fields to log file.
! Revised:  17 Dec 2018  Original version in MCIPv5.0.  Subsumes part of
!                        gridout.f90 from MCIPv4.5.  (T. Spero)
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

  WRITE (*,'(/,a,/)') '- GRIDOUT: Printing sample cells in output grid'

  DO n = 1, nfld2dxy
    WRITE (*,f6000) TRIM(fld2dxy(n)%fldname),  &
                         fld2dxy(n)%fld(lprt_col,lprt_row),  &
                    TRIM(fld2dxy(n)%units)
  ENDDO

  DO n = 1, nfld2dxy_d
    WRITE (*,f6000) TRIM(fld2dxy_d(n)%fldname),  &
                         fld2dxy_d(n)%fld(lprt_col,lprt_row),  &
                    TRIM(fld2dxy_d(n)%units)
  ENDDO

  IF ( iflufrc ) THEN  ! fractional land use data are available

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

    DO n = 1, nfld3dxyl
      WRITE (*,ifmt1) TRIM(fld3dxyl(n)%fldname),  &
                          (fld3dxyl(n)%fld(lprt_col,lprt_row,k),k=1,nummetlu)
    ENDDO

  ENDIF

END SUBROUTINE outglog
