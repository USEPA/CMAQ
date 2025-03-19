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

SUBROUTINE gridproc

!-------------------------------------------------------------------------------
! Name:     Grid Processing
! Purpose:  Fill arrays for time-independent output fields.
! Revised:  14 Dec 2018  Initial version with code taken from gridout.f90 and
!                        lucro.f90.  (T. Spero)
!           18 Jun 2019  Updated comments to add clarification for the
!                        sizing of the dot-point arrays.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE ctmvars

  IMPLICIT NONE

  INTEGER                           :: c
  INTEGER                           :: col
  INTEGER                           :: idx
  INTEGER                           :: l
  INTEGER                           :: lvl
  INTEGER                           :: r
  INTEGER                           :: row
  REAL                              :: xmapmin

!-------------------------------------------------------------------------------
! Fill time-independent 2d fields at cell centers.
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapc)  ! XMAPMIN also used for XMAPD

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik
            
      g_lat%fld(col,row) = xlatc(c,r)
      g_lon%fld(col,row) = xlonc(c,r)

      IF ( xmapmin < xmissing ) THEN
        g_msfx2%fld(col,row) = fillreal
      ELSE
        g_msfx2%fld(col,row) = xmapc2(c,r)  ! already squared
      ENDIF

      g_ht%fld(col,row)     = xtopo(c,r)
      g_dluse%fld(col,row)  = xdluse(c,r)
      g_lwmask%fld(col,row) = xlwmask(c,r)

      IF ( iflufrc ) THEN
        g_purb%fld(col,row) = xpurb(c,r)
      ELSE IF ( ifrcurb ) THEN  ! include PURB from urban canopy model
        g_purb%fld(col,row) = xpurb(c,r)
      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Fill time-independent 3d fields (fractional land use) at cell centers.
!-------------------------------------------------------------------------------

  IF ( iflufrc ) THEN  ! fractional land use data are available

    DO row = 1, nrows
      r = row + nthik
      DO col = 1, ncols
        c = col + nthik
        DO lvl = 1, nummetlu

          g_lufrac%fld(col,row,lvl) = xluse(c,r,lvl)

        ENDDO
      ENDDO
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Fill time-independent 2d fields for boundaries.
!-------------------------------------------------------------------------------

  idx = 0

  ! Southern boundary moving west to east from column 1 (in output grid) to
  ! column NCOLS+NTHIK.

  DO r = 1, nthik
    DO c = 1 + nthik, ncols_x

      idx = idx + 1

      g_lat%bdy(idx) = xlatc(c,r)
      g_lon%bdy(idx) = xlonc(c,r)

      IF ( xmapmin < xmissing ) THEN
        g_msfx2%bdy(idx) = fillreal
      ELSE
        g_msfx2%bdy(idx) = xmapc2(c,r)  ! already squared
      ENDIF

      g_ht%bdy(idx)     = xtopo(c,r)
      g_dluse%bdy(idx)  = xdluse(c,r)
      g_lwmask%bdy(idx) = xlwmask(c,r)

      IF ( iflufrc ) THEN
        g_purb%bdy(idx) = xpurb(c,r)
      ELSE IF ( ifrcurb ) THEN  ! include PURB from urban canopy model
        g_purb%bdy(idx) = xpurb(c,r)
      ENDIF

    ENDDO
  ENDDO

  ! Eastern boundary moving south to north from row 1 (in output grid) to
  ! row NROWS+NTHIK.

  DO r = 1+nthik, nrows_x
    DO l = 1, nthik

      c = ncols_x - nthik + l
      idx = idx + 1

      g_lat%bdy(idx) = xlatc(c,r)
      g_lon%bdy(idx) = xlonc(c,r)

      IF ( xmapmin < xmissing ) THEN
        g_msfx2%bdy(idx) = xmissing
      ELSE
        g_msfx2%bdy(idx) = xmapc2(c,r)  ! already squared
      ENDIF

      g_ht%bdy(idx)     = xtopo(c,r)
      g_dluse%bdy(idx)  = xdluse(c,r)
      g_lwmask%bdy(idx) = xlwmask(c,r)

      IF ( iflufrc ) THEN
        g_purb%bdy(idx) = xpurb(c,r)
      ELSE IF ( ifrcurb ) THEN  ! include PURB from urban canopy model
        g_purb%bdy(idx) = xpurb(c,r)
      ENDIF

    ENDDO
  ENDDO

  ! Northern boundary moving west to east from column 1-NTHIK (in output grid)
  ! to column NCOLS.

  DO l = 1, nthik
    r = nrows_x - nthik + l
    DO c = 1, ncols_x - nthik
      idx = idx + 1

      g_lat%bdy(idx) = xlatc(c,r)
      g_lon%bdy(idx) = xlonc(c,r)

      IF ( xmapmin < xmissing ) THEN
        g_msfx2%bdy(idx) = fillreal
      ELSE
        g_msfx2%bdy(idx) = xmapc2(c,r)  ! already squared
      ENDIF

      g_ht%bdy(idx)     = xtopo(c,r)
      g_dluse%bdy(idx)  = xdluse(c,r)
      g_lwmask%bdy(idx) = xlwmask(c,r)

      IF ( iflufrc ) THEN
        g_purb%bdy(idx) = xpurb(c,r)
      ELSE IF ( ifrcurb ) THEN  ! include PURB from urban canopy model
        g_purb%bdy(idx) = xpurb(c,r)
      ENDIF

    ENDDO
  ENDDO
      
  ! Western boundary moving south to north from row 1-NTHIK (in output grid)
  ! to row NROWS.

  DO r = 1, nrows_x - nthik
    DO c = 1, nthik

      idx = idx + 1

      g_lat%bdy(idx) = xlatc(c,r)
      g_lon%bdy(idx) = xlonc(c,r)

      IF ( xmapmin < xmissing ) THEN
        g_msfx2%bdy(idx) = fillreal
      ELSE
        g_msfx2%bdy(idx) = xmapc2(c,r)  ! already squared
      ENDIF

      g_ht%bdy(idx)     = xtopo(c,r)
      g_dluse%bdy(idx)  = xdluse(c,r)
      g_lwmask%bdy(idx) = xlwmask(c,r)

      IF ( iflufrc ) THEN
        g_purb%bdy(idx) = xpurb(c,r)
      ELSE IF ( ifrcurb ) THEN  ! include PURB from urban canopy model
        g_purb%bdy(idx) = xpurb(c,r)
      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Fill time-independent 2d fields at cell corners and faces.
!
! These arrays are all set to the dot-point dimensions to accommodate the
! false dot points in the Arakawa-C staggered grid that are output in
! Models-3 I/O API "DOT" files.  When the output is written in netCDF, the
! true dimensions of the Arakawa-C staggered fields are used.
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapd)   ! XMAPMIN also used for XMAPC

  DO row = 1, nrows+1
    r = row + nthik
    DO col = 1, ncols+1
      c = col + nthik

      g_latd%fld(col,row) = xlatd(c,r)
      g_lond%fld(col,row) = xlond(c,r)

      IF ( xmapmin < xmissing ) THEN
        g_msfd2%fld(col,row) = fillreal
      ELSE
        g_msfd2%fld(col,row) = xmapd(c,r)**2
      ENDIF

      g_latu%fld(col,row) = xlatu(c,r)
      g_lonu%fld(col,row) = xlonu(c,r)

      IF ( xmapmin < xmissing ) THEN
        g_msfu2%fld(col,row) = fillreal
      ELSE
        g_msfu2%fld(col,row) = xmapu(c,r)**2
      ENDIF

      g_latv%fld(col,row) = xlatv(c,r)
      g_lonv%fld(col,row) = xlonv(c,r)

      IF ( xmapmin < xmissing ) THEN
        g_msfv2%fld(col,row) = fillreal
      ELSE
        g_msfv2%fld(col,row) = xmapv(c,r)**2
      ENDIF

    ENDDO
  ENDDO

END SUBROUTINE gridproc
