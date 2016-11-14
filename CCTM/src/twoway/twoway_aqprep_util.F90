SUBROUTINE bcldprc_ak (wrf_ncols, wrf_nrows, nlays,                &
                       zf, ta, pres, qv, pbl, dzf, presf,  &
                       cfrac, cldb, cldt, wbar)

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
!           04 Apr 2007  Modified for AQPREP.  Combined two sets of column and
!                        row loops into a single loop.  Eliminated low, middle,
!                        and high cloud calculation.  Converted CCOV to a 1D
!                        array.  Changed constants to WRF-based names (and
!                        values).  Changed internal variable names to match
!                        AQPREP names.  Added argument list.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE module_model_constants

  IMPLICIT NONE

  INCLUDE SUBST_CONST

  INTEGER,       INTENT(IN)    :: wrf_ncols
  INTEGER,       INTENT(IN)    :: wrf_nrows
  INTEGER,       INTENT(IN)    :: nlays
  REAL,          INTENT(IN)    :: zf         ( : , : , : )
  REAL,          INTENT(IN)    :: ta         ( : , : , : )
  REAL,          INTENT(IN)    :: pres       ( : , : , : )
  REAL,          INTENT(IN)    :: qv         ( : , : , : )
  REAL,          INTENT(IN)    :: pbl        ( : , : )
  REAL,          INTENT(IN)    :: dzf        ( : , : , : )
  REAL,          INTENT(IN)    :: presf      ( : , : , : )
  REAL,          INTENT(OUT)   :: cfrac      ( : , : )
  REAL,          INTENT(OUT)   :: cldb       ( : , : )
  REAL,          INTENT(OUT)   :: cldt       ( : , : )
  REAL,          INTENT(OUT)   :: wbar       ( : , : )

  INTEGER                      :: c
  REAL                         :: cbase
  REAL                         :: ccmax
  REAL,          ALLOCATABLE   :: ccov       ( : )
  REAL                         :: ctop
  REAL                         :: dp
  REAL                         :: dtdp
  REAL                         :: e_aerk
  REAL                         :: esat1
  REAL                         :: frac
  INTEGER                      :: iflag
  INTEGER                      :: itr
  INTEGER                      :: k
  INTEGER                      :: kbase
  INTEGER                      :: kct
  INTEGER                      :: kmx
  INTEGER                      :: ktop
  REAL,          PARAMETER     :: mvoma      = 0.622
  REAL                         :: pbar
  REAL                         :: pbase
  REAL                         :: plcl
  REAL                         :: prs
  REAL                         :: qlcl
  REAL                         :: qsat
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
  REAL                         :: tempc
  REAL                         :: tlcl
  REAL                         :: twc
! REAL,          PARAMETER     :: vp0        = svp1 * 1000.0  ! cb -> Pa
  REAL                         :: wl
  REAL                         :: wtbar
  REAL                         :: x1

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  ! Saturation vapor pressure [Pa]

  e_aerk(tempc) = vp0 * EXP( 17.625 * tempc / ( 243.04 + tempc ) )

  qsat(esat1,prs) = esat1 * mvoma / ( prs - esat1 )

!-------------------------------------------------------------------------------
! Initialize all cloud variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED (ccov) ) THEN
    ALLOCATE ( ccov ( nlays ) )
  ENDIF

  wbar  (:,:) = 0.0
  cldb  (:,:) = 0.0
  cldt  (:,:) = 0.0
  cfrac (:,:) = 0.0
  ccov  (:)   = 0.0

!-------------------------------------------------------------------------------
! Loop over grid points to compute cloud parameters.
!-------------------------------------------------------------------------------

  DO c = 1, wrf_ncols
    DO r = 1, wrf_nrows

      !-------------------------------------------------------------------------
      ! Compute cloud coverage.
      !-------------------------------------------------------------------------

      kmx = 1

      DO k = 1, nlays

        ! Define RH and critical RH of all layers.

        rh = qv(c,r,k) / qsat( e_aerk( ta(c,r,k)-svpt0 ), pres(c,r,k) )
        rh  = MIN(rh,1.0)

        ! Set RHC to at least 98% in PBL - JEP 5/91

!       IF ( zf(c,r,k-1) < pbl(c,r) ) THEN
        IF ( zf(c,r,k) < pbl(c,r) ) THEN  ! zf (1:35) due to f90 

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

          sg1 = pres(c,r,k) / pres(c,r,kmx)
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
      ! Locate cloud top and cloud bottom, and compute average liquid water
      ! content.
      !-------------------------------------------------------------------------

      kct   = 0
      kbase = 0
      ktop  = 0
      ccmax = 0.0

      ! Determine level of highest rh relative to RHC.

      DO k = 2, nlays-1
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
        cldt(c,r) = 0.0
        cldb(c,r) = 0.0
        CYCLE
      ENDIF

      ! Look for cloud top and base layer up and down from level of max RH.

      top: DO k = kct, nlays
        ktop = k - 1
        IF ( ccov(k) < 0.5*ccmax ) EXIT top
      ENDDO top

      bottom: DO k = kct, 1, -1
        kbase = k + 1
        IF ( ccov(k) < 0.5*ccmax ) EXIT bottom
      ENDDO bottom

      DO k = 1, ktop
        IF ( k < kbase ) cbase = cbase + dzf(c,r,k)  ! <-- check indexing
        ctop = ctop + dzf(c,r,k)                     !     TLO 4 Apr 07
      ENDDO

      cldt(c,r) = ctop
      cldb(c,r) = cbase

      ! We need pres. at the bottom of the lowest layer containing clouds.
      ! Temp. and qv at cloud base are for mid layer values.

      plcl = presf(c,r,kbase-1)

      tlcl = ( plcl - pres(c,r,kbase) ) /                  &
             ( pres(c,r,kbase-1) - pres(c,r,kbase) ) *   &
             ( ta  (c,r,kbase-1) - ta  (c,r,kbase) ) +   &
             ta(c,r,kbase)

      qlcl = qsat( e_aerk( tlcl - svpt0 ), plcl )

      iflag = 0
      pbase = plcl
      tbase = tlcl

      ! Follow moist adiabat up.

      DO k = kbase, ktop

        dp   = pbase - pres(c,r,k)
        pbar = pbase - dp / 2.0
        tbar = tbase

        DO itr = 1, 5
          x1   = xlv * qsat( e_aerk( tbar-svpt0 ), pbar ) / ( r_d * tbar )
          dtdp = r_d * tbar / pbar / cpd * ( ( 1.0 + x1 ) /  &
                 ( 1.0 + mvoma * xlv / cpd / tbar * x1 ) )
          tad  = tbase - dp * dtdp
          tbar = ( tad + tbase ) * 0.5
        ENDDO

        ! Determine water content by fraction of adiabatic.

        tad   = MAX(tad, 150.0)
        IF ( tad > ta(c,r,k) ) iflag = 1

        ! Pressure in Pascal = cb*1000

        wl    = 0.7 * EXP( ( pres(c,r,k) - plcl ) / 8000.0 ) + 0.2
        qwsa  = qsat( e_aerk(tad - svpt0), pres(c,r,k)  )

        qwat  = wl * ( qlcl - qwsa )
        qwat  = MAX(qwat, 0.0)

        twc   = qwat * pres(c,r,k) * 1.0e3 / r_d / ta(c,r,k)

        wtbar = wtbar + twc * dzf(c,r,k)

        frac  = frac + ccov(k) * dzf(c,r,k)
        sumz  = sumz + dzf(c,r,k)
        tbase = tad
        pbase = pres(c,r,k)
            
      ENDDO

      cfrac(c,r) = frac  / sumz
      wbar (c,r) = wtbar / sumz

      IF ( cfrac(c,r) < 0.001 ) THEN
        cldt(c,r) = 0.0
        cldb(c,r) = 0.0
      ENDIF

      IF ( wbar(c,r) == 0.0 ) THEN
        cldt (c,r) = 0.0
        cldb (c,r) = 0.0
        cfrac(c,r) = 0.0
        CYCLE
      ENDIF

      ! If stable environment, use alternate water content expression.

      IF ( iflag == 0 ) THEN
        wtbar = 0.0
        DO k = kbase, ktop
          twc   = 50.0 * qv(c,r,k) * pres(c,r,k) / r_d / ta(c,r,k)
          wtbar = wtbar + twc * dzf(c,r,k)
        ENDDO
        wbar(c,r) = wtbar / sumz
      ENDIF

    ENDDO
  ENDDO

END SUBROUTINE bcldprc_ak
