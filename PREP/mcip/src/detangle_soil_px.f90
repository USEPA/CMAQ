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

SUBROUTINE detangle_soil_px

!-------------------------------------------------------------------------------
! Name:     Detangle Soil Categories from Pleim-Xiu Land-Surface Model
! Purpose:  Realign soil categories from Pleim-Xiu land-surface model in WRF
!           so that they are consistent with the WRF documentation and can be
!           handled properly within CMAQ.
! Notes:    WRFv3.7 and all prior versions have soil categories within PXLSM
!           placed in an indexing that is inconsistent with the WRF
!           documentation.  See subroutine SOILPROP in WRF's module_sf_pxlsm.
! Revised:  25 Aug 2015  Original version.  (T. Spero)
!-------------------------------------------------------------------------------

  USE metinfo, ONLY: nx => met_nx, ny => met_ny
  USE metvars, ONLY: isltyp

  IMPLICIT NONE

  INTEGER                           :: i
  INTEGER                           :: j
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'DETANGLE_SOIL_PX'

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED SOIL CATEGORY FROM PXLSM: ', i3, &
    & /, 1x, '***   AT WRF GRID CELL: ', i4, 2x, i4, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Loop over grid cells to align soil type from PXLSM with WRF standard.
!
! Categories in PXLSM range from 1-11 plus 14, where categories 1-3 and 14 are
! consistent with WRF standard, category 4 is a combination of WRF categories
! 4-5, and categories 5-11 from PXLSM are the same as 6-12 from WRF,
! respectively.  WRF categories 13, 15, and 16 are not part of the PXLSM
! classification.
!
! WRF soil categories:
!    1   Sand
!    2   Loamy Sand
!    3   Sandy Loam
!    4   Silt Loam
!    5   Silt
!    6   Loam
!    7   Sandy Clay Loam
!    8   Silty Clay Loam
!    9   Clay Loam
!   10   Sandy Clay
!   11   Silty Clay
!   12   Clay
!   13   Organic Material
!   14   Water
!   15   Bedrock
!   16   Other (land-ice)
!-------------------------------------------------------------------------------

  DO i = 1, nx
    DO j = 1, ny

      SELECT CASE ( isltyp(i,j) )
        CASE ( 1:4, 14 )
          isltyp(i,j) = isltyp(i,j)
        CASE ( 5:11 )
          isltyp(i,j) = isltyp(i,j) + 1
        CASE DEFAULT
          WRITE (*,f9000) TRIM(pname), isltyp(i,j), i, j
          CALL graceful_stop (pname)
      END SELECT
    ENDDO
  ENDDO

END SUBROUTINE detangle_soil_px
