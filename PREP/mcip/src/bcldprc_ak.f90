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

SUBROUTINE bcldprc_ak

!-------------------------------------------------------------------------------
! Name:     Cloud Processor for Anthes-Kuo Scheme
! Purpose:  Determine the base and top of cloudy layers, as well as the mean
!           water content and fractional area of cloud coverage using JEP H54 
!           cloud and photolysis scheme.  (ANTHES-KUO scheme)
! Revised:  20 Mar 1991  Original version.  (CJW)
!           27 Oct 1995  Modified.  (C. Ingram)
!           05 Feb 1997  Updated for Models-3.  (D. Byun)
!           04 Feb 1998  Changed include method nonglobal includes.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  Changed CCOV to
!                        allocatable.  Changed vertical dimension of CCOV
!                        from MAXK to METLAY.  (T. Otte)
!           27 Feb 2002  Corrected bug that persisted cloud base arrays when
!                        clouds had dissipated.  (T. Otte and S. Roselle)
!           08 Jul 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  Removed XFLAGS.  (T. Otte)
!           09 Apr 2007  Combined two sets of c,r loops.  Changed CCOV from a
!                        3D to a 1D (k-only) array.  Removed unused variable
!                        PNAME.  Removed low, middle, and high cloud
!                        calculations.  (T. Otte)
!           01 Sep 2011  Replaced statement functions with external routines.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE const

  IMPLICIT NONE

  INTEGER                      :: c
  REAL                         :: cbase
  REAL                         :: ccmax
  REAL,    SAVE, ALLOCATABLE   :: ccov       ( : )
  REAL                         :: ctop
  REAL                         :: dp
  REAL                         :: dtdp
  REAL,          EXTERNAL      :: e_aerk
  REAL                         :: es
  REAL                         :: frac
  INTEGER                      :: iflag
  INTEGER                      :: itr
  INTEGER                      :: k
  INTEGER                      :: kbase
  INTEGER                      :: kct
  INTEGER                      :: kmx
  INTEGER                      :: ktop
  REAL,          PARAMETER     :: mvoma = mwwat / mwair  ! 0.622015
  REAL                         :: pbar
  REAL                         :: pbase
  REAL                         :: plcl
  REAL                         :: qlcl
  REAL                         :: qs
  REAL,          EXTERNAL      :: qsat
  REAL                         :: qwat
  REAL                         :: qwsa
  INTEGER                      :: r
  REAL                         :: rh
  REAL                         :: rhc
  REAL                         :: sg1
  REAL                         :: sumz
  REAL                         :: tad
  REAL                         :: tbar
  REAL                         :: tbase
  REAL                         :: tlcl
  REAL                         :: twc
  REAL                         :: wl
  REAL                         :: wtbar
  REAL                         :: x1

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( ccov ) ) ALLOCATE ( ccov (metlay) )

!-------------------------------------------------------------------------------
! Initialize all cloud variables.
!-------------------------------------------------------------------------------

  xwbar   (:,:)   = 0.0
  xcldbot (:,:)   = 0.0
  xcldtop (:,:)   = 0.0
  xcfract (:,:)   = 0.0
  ccov        (:) = 0.0

!-------------------------------------------------------------------------------
! Loop over grid points to compute cloud coverage, cloud base and top, and
! average liquid water content in cloud.
!-------------------------------------------------------------------------------

  DO c = 1, ncols_x
    DO r = 1, nrows_x

      !-------------------------------------------------------------------------
      ! Define cloud coverage (CCOV).
      !-------------------------------------------------------------------------

      kmx = 1

      DO k = 1, metlay

        ! Define RH and critical RH of all layers.

        es = e_aerk( xtempm(c,r,k) - stdtemp )
        qs = qsat( es, xpresm(c,r,k) )
        rh = xwvapor(c,r,k) / qs
        rh = MIN(rh,1.0)

        ! Set RHC to at least 98% in PBL - JEP 5/91

        IF ( x3htf(c,r,k-1) < xpbl(c,r) ) THEN

          rhc = 0.98
          kmx = k

          IF ( rh > rhc ) THEN

            ! CBL mixing induced clouds should not exceed the frac area of 
            ! the updrafts at top of cbl, les estimates are ~34%
            ! (Schumann 89, and Wyngaard and Brost 84)

            ccov(k) = 0.34 * ( rh - rhc ) / ( 1.0 - rhc )
          ELSE
            ccov(k) = 0.0
          ENDIF

        ELSE

          sg1 = xpresm(c,r,k) / xpresm(c,r,kmx)
          rhc = 1.0 - ( 2.0 * sg1 * (1.0-sg1) * (1.0 + 1.732*(sg1-0.5)) )

          IF ( rh > rhc ) THEN
            ccov(k) = ( (rh - rhc)/(1.0 - rhc) )**2   ! Geleyn et al., 1982
          ELSE
            ccov(k) = 0.0
          ENDIF

        ENDIF

        ccov(k) = MAX( MIN( ccov(k), 1.0 ), 0.0 )

      ENDDO

      !-------------------------------------------------------------------------
      ! Get cloud top and bottom and XWBAR.
      !-------------------------------------------------------------------------

      kct   = 0
      kbase = 0
      ktop  = 0
      ccmax = 0.0

      ! Determine level of highest rh relative to RHC.

      DO k = 2, metlay-1
        IF ( ccov(k) > ccmax ) THEN
          ccmax = ccov(k)
          kct   = k
        ENDIF
      ENDDO

      frac  = 0.0
      cbase = 0.0
      ctop  = 0.0
      wtbar = 0.0
      sumz  = 0.0

      ! Exit with no cloud values if RH < RHC at all levels.

      IF ( ccmax < 0.01 ) THEN
        xcldtop(c,r) = 0.0
        xcldbot(c,r) = 0.0
        CYCLE
      ENDIF

      ! Look for cloud top and base layer up and down from level of max RH.

      top: DO k = kct, metlay
        ktop = k - 1
        IF ( ccov(k) < 0.5*ccmax ) EXIT top
      ENDDO top

      bottom: DO k = kct, 1, -1
        kbase = k + 1
        IF ( ccov(k) < 0.5*ccmax ) EXIT bottom
      ENDDO bottom

      DO k = 1, ktop
        IF ( k < kbase ) cbase = cbase + xdx3htf(c,r,k)
        ctop = ctop + xdx3htf(c,r,k)
      ENDDO

      xcldtop(c,r) = ctop
      xcldbot(c,r) = cbase

      ! We need pres. at the bottom of the lowest layer containing clouds.
      ! Temp. and qv at cloud base are for mid layer values.

      plcl = xpresf(c,r,kbase-1)

      tlcl = ( plcl - xpresm(c,r,kbase) ) /                  &
             ( xpresm(c,r,kbase-1) - xpresm(c,r,kbase) ) *   &
             ( xtempm(c,r,kbase-1) - xtempm(c,r,kbase) ) +   &
             xtempm(c,r,kbase)

      es   = e_aerk( tlcl - stdtemp )
      qlcl = qsat( es, plcl )

      iflag = 0
      pbase = plcl
      tbase = tlcl

      ! Follow moist adiabat up.

      DO k = kbase, ktop

        dp   = pbase - xpresm(c,r,k)
        pbar = pbase - dp / 2.0
        tbar = tbase

        DO itr = 1, 5
          es   = e_aerk( tbar - stdtemp )
          qs   = qsat( es, pbar )
          x1   = lv0 * qs / ( rdgas * tbar )
          dtdp = rdgas * tbar / pbar / cpd * ( ( 1.0 + x1 ) /  &
                 ( 1.0 + mvoma * lv0 / cpd / tbar * x1 ) )
          tad  = tbase - dp * dtdp
          tbar = ( tad + tbase ) * 0.5
        ENDDO

        ! Determine water content by fraction of adiabatic.

        tad   = MAX(tad, 150.0)
        IF ( tad > xtempm(c,r,k) ) iflag = 1

        ! Pressure in Pascal = cb*1000

        wl    = 0.7 * EXP( ( xpresm(c,r,k) - plcl ) / 8000.0 ) + 0.2
        es    = e_aerk( tad - stdtemp )
        qwsa  = qsat( es, xpresm(c,r,k)  )

        qwat  = wl * ( qlcl - qwsa )
        qwat  = MAX(qwat, 0.0)

        twc   = qwat * xpresm(c,r,k) * 1.0e3 / rdgas / xtempm(c,r,k)

        wtbar = wtbar + twc * xdx3htf(c,r,k)

        frac  = frac + ccov(k) * xdx3htf(c,r,k)
        sumz  = sumz + xdx3htf(c,r,k)
        tbase = tad
        pbase = xpresm(c,r,k)
            
      ENDDO

      xcfract(c,r) = frac  / sumz
      xwbar  (c,r) = wtbar / sumz

      IF ( xcfract(c,r) < 0.001 ) THEN
        xcldtop(c,r) = 0.0
        xcldbot(c,r) = 0.0
      ENDIF

      IF ( xwbar(c,r) == 0.0 ) THEN
        xcldtop(c,r) = 0.0
        xcldbot(c,r) = 0.0
        xcfract(c,r) = 0.0
        CYCLE
      ENDIF

      ! If stable environment, use alternate water content expression.

      IF ( iflag == 0 ) THEN
        wtbar = 0.0
        DO k = kbase, ktop
          twc   = 0.05e3 * xwvapor(c,r,k) * xpresm(c,r,k) /   &
                  rdgas / xtempm(c,r,k)
          wtbar = wtbar + twc * xdx3htf(c,r,k)
        ENDDO
        xwbar(c,r) = wtbar / sumz
      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( ccov )  ! commented out to avoid memory fragmentation

END SUBROUTINE bcldprc_ak
