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

SUBROUTINE ctmproc

!-------------------------------------------------------------------------------
! Name:     Meteorology on Cross Points
! Purpose:  Compute and output time-dependent, cross-point parameters.
! Revised:  15 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           04 Feb 1998  LSM method for nonglobal variables changed.  (???)
!           30 Apr 1999  PSTAR was replaced with PRSFC and MAXK.  (D. Byun)
!           20 Sep 2001  Converted to free-form f90.  Added interface for
!                        COLLAPX and removed WORKC.  Added interface for
!                        SANITY and removed NCELLS.  Removed IFTYPE and
!                        dependence of TSTEP3D on this variable.  This routine
!                        always accesses time-dependent data.  Removed ISTAT
!                        from argument list.  Changed DUMARAY0 and DUMARAY1 to
!                        allocatable.  Removed NLAYS from argument lists
!                        for COMHEADER and COLLAPX.  Added QICE, QSNOW, SOIM1,
!                        SOIM2, SOIT1, SOIT2, and SLTYP to output.  Removed
!                        unused arrays for LAMDA and MCONERR.  (T. Otte)
!           11 Oct 2001  Changed lower-limit restrictions on MC3 and MB3 with
!                        epsilon.  (T. Otte)
!           20 Nov 2001  Removed "sanity" checks.  (T. Otte)
!           27 Feb 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Added logic
!                        to enable "missing" variables in output to have
!                        values of BADVAL3 < AMISS3, or 0.0, as appropriate.
!                        Removed RIB from output.  Renamed SURF2 as WIND10
!                        and SURF1 as TEMP1P5.  (T. Otte)
!           27 Mar 2003  Corrected calculation of WGHT_TOP and WGHT_BOT that
!                        is used to get WHAT_JD_C and WHAT_JD_B on full levels.
!                        Removed extraneous calculation of X3MIDL.  Changed
!                        calls to COLLAPX to be on Jacobian coupled with
!                        density instead of just Jacobian for better mass
!                        conservation reasons.  Changed subsequent calculations
!                        of output variables based on Jacobian.  Removed
!                        JDRATE.  (J. Pleim and T. Otte)
!           09 Jun 2003  Added SNOCOV to output.  (D. Schwede)
!                        Removed extraneous variables from output.  Also
!                        removed unused file METBDY2D.  (T. Otte)
!           09 Aug 2004  Modified code so that arrays are made available in
!                        output only if user options in MM5 generate those
!                        data.  Added graupel (QG), full Jacobian (JACOBS and
!                        JACOBF), 10-m wind speed and direction (WSPD10 and
!                        WDIR10), passed-through 2-m temperature (TEMP2), and
!                        leaf-area index from Pleim-Xiu LSM to output.
!                        Changed local array allocation to occur only on initial
!                        call to subroutine to avoid memory fragmentation.
!                        Added initialization of NTHIK3D for I/O API output.
!                        (T. Otte and D. Schwede)
!           31 Jan 2005  Changed output file names by removing "_G1".  (T. Otte)
!           19 Aug 2005  Added USE statement for new DEPVVARS module.  Changed
!                        internal variable EPSILON to EPSILONQ and internal
!                        variable INDEX to IDX to avoid confusion with F90
!                        intrinsic functions.  Modified call to COLLAPX to
!                        reflect that there is one option for layer collapsing.
!                        (T. Otte)
!           19 Jun 2006  Updated comment to reflect that there is only one
!                        way to collapse fields.  Corrected setting of
!                        VNAME3D for dry deposition species.  Removed unused
!                        variable JDATE.  (T. Otte)
!           26 Jul 2007  Changed XUSTAR and XRADYN to 2D arrays without a
!                        dimension for fractional land use that was required
!                        for RADMdry.  Removed 1.5-m and 10-m temperature
!                        arrays.  Fill 2-m temperature array regardless of
!                        whether it is available in input meteorology.
!                        Changed 2-m temperature from XT2 to XTEMP2.  Added
!                        VEG to output, made LAI a general output variable, and
!                        added WR to output to support inline dry deposition
!                        velocity calculations in CCTM.  Removed RBNDYI and
!                        JACOBS from output.  (T. Otte)
!           28 Apr 2008  Replaced variable NTHIKD with NTHIK and removed
!                        options for NTHIK=0 in METBDY3D.  Added Q2 to
!                        METCRO2D, and added TKE or TKEF to METCRO3D and
!                        METBDY3D if TKE is available in meteorology file.
!                        (T. Otte)
!                        Added cloud transmissivity variable to output if
!                        external satellite data are provided.  When satellite
!                        data are available and this option is invoked,
!                        photolysis rates in CMAQ are computed using cloud
!                        cover, cloud transmissivity, cloud top and bottom
!                        heights, and cloud fraction that are based on observed
!                        fields rather than model-derived estimates.  
!                        Contributed by University of Alabama at Huntsville.
!                        (A. Biazar and T. Otte)
!           29 Oct 2009  Corrected an error in the mapping of TKE to DUMARAY1
!                        and vice versa in the layer collapsing.  Added user
!                        option to output potential vorticity.  Use XMAPC2
!                        rather than squaring XMAPC each time routine is
!                        called.  Remove vertical velocity (predicted by the
!                        meteorological model, WWIND) from output by default;
!                        retain user option to output 3D field.  (T. Otte)
!           14 Dec 2010  Removed option to compute dry deposition velocities
!                        in MCIP.  Added sea ice.  (T. Otte)
!           11 Aug 2011  Replaced modules FDESC3, IODECL3, and PARMS3 with
!                        I/O API module M3UTILIO.  (T. Otte)
!           01 Sep 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Improved error
!                        handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           27 Apr 2015  Added 3D resolved cloud fraction (CFRAC_3D) to output
!                        if it is available in incoming meteorological model
!                        data  (T. Spero)
!           20 Aug 2015  Changed latent heat flux from QFX to LH.  (T. Spero)
!           30 Oct 2015  Changed WRITE statements for printing sampled data to
!                        log file to eliminate warning messages.  (T. Spero)
!           22 Nov 2017  Added SNOWH to output.  (T. Spero)
!           26 Jun 2018  Updated units to conform to CF compliance.
!                        Consolidated output variables into a single module
!                        CTMVARS.  Now use netCDF tokens for missing values.
!                        (T. Spero)
!           14 Sep 2018  Removed support for MM5v3 input.  (T. Spero)
!           13 Dec 2018  Updated to use new data structures.  Removed deprecated
!                        variables CFRAC, CLDT, CLDB, and WBAR.  (T. Spero)
!           17 Dec 2018  Moved parsing and processing of output fields on the
!                        CCTM. grid to a different routine.  Most of this
!                        is from metcro.f90, soilcro.f90, moscro.f90, and
!                        metdot.f90.  (T. Spero)
!           18 Jun 2019  Removed layer collapsing.  Added new surface variables
!                        with PX LSM that can improve dust simulation in CCTM.
!                        Simplified calculation of JACOBM and JACOBF to remove
!                        extra multiplication and division of density.  Changed
!                        variable LUVCOUT to LUVBOUT to reflect that the default
!                        3D wind components are on the Arakawa-C staggered grid,
!                        and the optional additional 3D winds are on the
!                        Arakawa-B staggered grid.  Added optional variables
!                        from KF convective scheme with radiative feedbacks.
!                        (T. Spero)
!           21 Nov 2019  Corrected error in setting the 3D fractional cloud
!                        coverage on one of the lateral boundaries.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE ctmvars
  USE vgrd
  USE coord   ! <--- fix this so that X3FACE and X3MIDL are moved  TLS 14Dec18

  IMPLICIT NONE

  INTEGER                           :: c
  INTEGER                           :: cm1
  INTEGER                           :: col
  LOGICAL, SAVE                     :: first       = .TRUE.
  INTEGER                           :: icld
  INTEGER                           :: idx
  INTEGER                           :: ii
  INTEGER                           :: ipv
  INTEGER                           :: itke
  INTEGER                           :: iwout
  REAL,    SAVE,      ALLOCATABLE   :: jdenm       ( : , : , : )
  INTEGER                           :: k
  INTEGER                           :: l
  INTEGER                           :: lvl
  INTEGER                           :: r
  REAL                              :: rhojmi
  INTEGER                           :: rm1
  INTEGER                           :: row
  REAL,    SAVE,      ALLOCATABLE   :: wght_bot    ( : )
  REAL,    SAVE,      ALLOCATABLE   :: wght_top    ( : )
  REAL                              :: x3jfmin
  REAL                              :: x3jmmin
  REAL                              :: xdnamin
  REAL                              :: xdnjmin
  REAL                              :: xmapmin
  REAL(8)                           :: xorigdot
  REAL,    SAVE,      ALLOCATABLE   :: xrhojf      ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: xrhojm      ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: xuhat_s     ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: xvhat_t     ( : , : , : )
  REAL(8)                           :: yorigdot

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( jdenm   ) )  &
    ALLOCATE ( jdenm   ( ncols_x+1, nrows_x+1,   metlay ) )
  IF ( .NOT. ALLOCATED ( xrhojf ) )  &
    ALLOCATE ( xrhojf  ( ncols_x,   nrows_x,   0:metlay ) )
  IF ( .NOT. ALLOCATED ( xrhojm ) )  &
    ALLOCATE ( xrhojm  ( ncols_x,   nrows_x,     metlay ) )
  IF ( .NOT. ALLOCATED ( xuhat_s ) )  &
    ALLOCATE ( xuhat_s ( ncols_x+1, nrows_x+1,   metlay ) )
  IF ( .NOT. ALLOCATED ( xvhat_t ) )  &
    ALLOCATE ( xvhat_t ( ncols_x+1, nrows_x+1,   metlay ) )

!-------------------------------------------------------------------------------
! Compute linear interpolation coefficients based on layer thickness.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ALLOCATE ( wght_top (nlays) )
    ALLOCATE ( wght_bot (nlays) )

    DO lvl = 1, nlays-1
      wght_top(lvl) = ( x3face_gd(lvl)   - x3midl(lvl) )  &
                      / ( x3midl (lvl+1) - x3midl(lvl) )
      wght_bot(lvl) = 1.0 - wght_top(lvl)
    ENDDO

    IF ( iftke ) THEN
      itke = 1
    ELSE
      itke = 0
    ENDIF
    IF ( lpv > 0 ) THEN
      ipv = 1
    ELSE
      ipv = 0
    ENDIF
    IF ( lwout > 0 ) THEN
      iwout = 1
    ELSE
      iwout = 0
    ENDIF
    IF ( ifcld3d ) THEN
      icld = 1
    ELSE
      icld = 0
    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Fill time-varying 2d fields at cell centers.
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapc)
  x3jfmin = MINVAL(x3jacobf)

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik

      c_prsfc%fld(col,row) = xprsfc(c,r)
      c_ustar%fld(col,row) = xustar(c,r)
      c_wstar%fld(col,row) = xwstar(c,r)
      c_pbl%fld(col,row)   = xpbl(c,r)
      c_zruf%fld(col,row)  = xzruf(c,r)

      IF ( xmol(c,r) < xmissing ) THEN
        c_moli%fld(col,row) = 0.0
      ELSE
        c_moli%fld(col,row) = 1.0 / xmol(c,r)
      ENDIF

      c_hfx%fld(col,row) = xhfx(c,r)
      c_lh%fld(col,row)  = xlh(c,r)

      IF ( xradyn(c,r) < xmissing ) THEN
        c_radyni%fld(col,row) = 0.0
      ELSE
        c_radyni%fld(col,row) = 1.0 / xradyn(c,r)
      ENDIF

      IF ( xrstom(c,r) < xmissing ) THEN
        c_rstomi%fld(col,row) = 0.0
      ELSE
        c_rstomi%fld(col,row) = 1.0 / xrstom(c,r)
      ENDIF

      c_tempg%fld(col,row)  = xtempg(c,r)
      c_temp2%fld(col,row)  = xtemp2(c,r)
      c_q2%fld(col,row)     = xq2(c,r)
      c_wspd10%fld(col,row) = xwspd10(c,r)
      c_wdir10%fld(col,row) = xwdir10(c,r)
      c_glw%fld(col,row)    = xglw(c,r)
      c_gsw%fld(col,row)    = xgsw(c,r)
      c_rgrnd%fld(col,row)  = xrgrnd(c,r)

      c_rn%fld(col,row)     = xrainn(c,r)
      c_rc%fld(col,row)     = xrainc(c,r)

      c_cfrac%fld(col,row)  = xcfract(c,r)
      c_cldt%fld(col,row)   = xcldtop(c,r)
      c_cldb%fld(col,row)   = xcldbot(c,r)
      c_wbar%fld(col,row)   = xwbar(c,r)

      c_snocov%fld(col,row) = xsnocov(c,r)
      c_veg%fld(col,row)    = xveg(c,r)
      c_lai%fld(col,row)    = xlai(c,r)
      c_seaice%fld(col,row) = xseaice(c,r)
      c_snowh%fld(col,row)  = xsnowh(c,r)

      IF ( ifwr ) THEN
        c_wr%fld(col,row) = xwr(c,r)
      ENDIF

      IF ( ifsoil ) THEN
        c_soim1%fld(col,row) = xwga(c,r)
        c_soim2%fld(col,row) = xw2a(c,r)
        c_soit1%fld(col,row) = xtga(c,r)
        c_soit2%fld(col,row) = xt2a(c,r)
        c_sltyp%fld(col,row) = xsltyp(c,r)
      ENDIF

      IF ( ifpxwrf41 ) THEN
        c_wsat_px%fld(col,row)   = xwsat_px(c,r)
        c_wfc_px%fld(col,row)    = xwfc_px(c,r)
        c_wwlt_px%fld(col,row)   = xwwlt_px(c,r)
        c_csand_px%fld(col,row)  = xcsand_px(c,r)
        c_fmsand_px%fld(col,row) = xfmsand_px(c,r)
        c_clay_px%fld(col,row)   = xclay_px(c,r)
      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Calculate density*Jacobian on mid-layers and full levels.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        xrhojm(c,r,k) = xdensam(c,r,k) * x3jacobm(c,r,k)

      ENDDO
    ENDDO
  ENDDO

  DO k = 0, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        xrhojf(c,r,k) = xdensaf(c,r,k) * x3jacobf(c,r,k)

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Scale potential vorticity by Jacobian*density/map-scale factor.
!-------------------------------------------------------------------------------

  IF ( lpv > 0 ) THEN

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x

          rhojmi      = 1.0 / xrhojm(c,r,k)
          xpvc(c,r,k) = rhojmi * xpvc(c,r,k)

        ENDDO
      ENDDO
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Compute density*Jacobian for X-domain.  Persist outermost row and column
! of cross-point JDENM field into non-physical dot-point array.  These data
! will be used to fill non-physical extra row/column of square/triangle flux
! points.  These data are just available so that large "missing" values do not
! corrupt UHAT_S and VHAT_T fields for display purposes.  Note that JDENM is
! the portion of the UHAT_JD and VHAT_JD that includes (rho*J/m^2)*m.
! Also note that this is a subtle deviation from equations 12-120a and 12-120b
! in "Science Algorithms of the EPA Models-3/CMAQ Modeling System"
! (EPA/600/R-99/030), since contravariant wind components (equations 12-119a and
! 12-119b) are no longer computed in MCIP on dot points, and they are not stored
! locally.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        jdenm(c,r,k) = xdensam(c,r,k) * x3jacobm(c,r,k) / xmapc(c,r)

      ENDDO
    ENDDO
  ENDDO

  jdenm(ncols_x+1,:,:) = jdenm(ncols_x,:,:)
  jdenm(:,nrows_x+1,:) = jdenm(:,nrows_x,:)

!-------------------------------------------------------------------------------
! Compute contra-u*density*Jacobian at flux (square) points,
! and compute contra-v*density*Jacobian at flux (triangle) points.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    DO r = 1, nrows_x+1
      rm1 = MAX( r-1, 1 )
      DO c = 1, ncols_x+1
        cm1 = MAX( c-1, 1 )

        xuhat_s(c,r,k) = 0.5 * ( jdenm(cm1,r,k) + jdenm(c,r,k) ) * xuu_s(c,r,k)
        xvhat_t(c,r,k) = 0.5 * ( jdenm(c,rm1,k) + jdenm(c,r,k) ) * xvv_t(c,r,k)

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Fill time-varying 3d fields at cell centers.
!-------------------------------------------------------------------------------

  x3jmmin = MINVAL(x3jacobm)
  xdnamin = MINVAL(xdensam)

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik
      DO lvl = 1, nlays

        ! Used in generalized vertical coordinates in CCTM.

        IF ( ( x3jfmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobf%fld(col,row,lvl) = x3jacobf(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobm%fld(col,row,lvl) = x3jacobm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin > xmissing ) .AND. ( x3jmmin > xmissing ) ) THEN
          c_densa_j%fld(col,row,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! State fields.

        c_ta%fld(col,row,lvl)   = xtempm(c,r,lvl)
        c_qv%fld(col,row,lvl)   = xwvapor(c,r,lvl)
        c_pres%fld(col,row,lvl) = xpresm(c,r,lvl)
        c_dens%fld(col,row,lvl) = xdensam(c,r,lvl)
        c_zh%fld(col,row,lvl)   = x3htm(c,r,lvl)
        c_zf%fld(col,row,lvl)   = x3htf(c,r,lvl)

        ! Moisture fields.

        IF ( nqspecies >= 2 ) THEN
          c_qc%fld(col,row,lvl) = xcldwtr(c,r,lvl)
          c_qr%fld(col,row,lvl) = xranwtr(c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            c_qi%fld(col,row,lvl) = xqice (c,r,lvl)
            c_qs%fld(col,row,lvl) = xqsnow(c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              c_qg%fld(col,row,lvl) = xqgraup(c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          c_tke%fld(col,row,lvl) = xtke(c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          c_pv%fld(col,row,lvl) = xpvc(c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          c_wwind%fld(col,row,lvl) = xwwind(c,r,lvl)
        ENDIF

        IF ( ifcld3d ) THEN
          c_cfrac_3d%fld(col,row,lvl) = xcfrac3d(c,r,lvl)
        ENDIF

        IF ( ifkfradextras ) THEN
          c_qc_cu%fld(col,row,lvl)     = xqc_cu(c,r,lvl)
          c_qi_cu%fld(col,row,lvl)     = xqi_cu(c,r,lvl)
          c_cldfra_dp%fld(col,row,lvl) = xcldfrad(c,r,lvl)
          c_cldfra_sh%fld(col,row,lvl) = xcldfras(c,r,lvl)
        ENDIF

      ENDDO
    ENDDO
  ENDDO

  ! Added for mass consistency

  xdnjmin = MINVAL(c_densa_j%fld(:,:,:))

  IF ( xdnjmin > xmissing ) THEN

    DO row = 1, nrows
      r = row + nthik
      DO col = 1, ncols
        c = col + nthik
        DO lvl = 1, nlays-1

          c_what_jd%fld(col,row,lvl) = xwhat(c,r,lvl) *                        &
                                ( wght_bot(lvl) * c_densa_j%fld(col,row,lvl)   &
                                + wght_top(lvl) * c_densa_j%fld(col,row,lvl+1) )
        ENDDO

        c_what_jd%fld(col,row,nlays) = 0.0

      ENDDO
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Fill boundaries for time-varying 3d fields at cell centers.
!-------------------------------------------------------------------------------

  idx = 0

  ! Southern boundary moving west to east from column 1 (in output grid) to
  ! column NCOLS+NTHIK.

  DO r = 1, nthik
    DO c = 1 + nthik, ncols_x
      idx = idx + 1
      DO lvl = 1, nlays

        ! Used in generalized vertical coordinates in CCTM.

        IF ( ( x3jfmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobf%bdy(idx,lvl) = x3jacobf(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobm%bdy(idx,lvl) = x3jacobm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin > xmissing ) .AND. ( x3jmmin > xmissing ) ) THEN
          c_densa_j%bdy(idx,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! State variables.

        c_ta%bdy(idx,lvl)   = xtempm(c,r,lvl)
        c_qv%bdy(idx,lvl)   = xwvapor(c,r,lvl)
        c_pres%bdy(idx,lvl) = xpresm(c,r,lvl)
        c_dens%bdy(idx,lvl) = xdensam(c,r,lvl)
        c_zh%bdy(idx,lvl)   = x3htm(c,r,lvl)
        c_zf%bdy(idx,lvl)   = x3htf(c,r,lvl)

        ! Moisture variables.

        IF ( nqspecies >= 2 ) THEN
          c_qc%bdy(idx,lvl) = xcldwtr(c,r,lvl)
          c_qr%bdy(idx,lvl) = xranwtr(c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            c_qi%bdy(idx,lvl) = xqice(c,r,lvl)
            c_qs%bdy(idx,lvl) = xqsnow(c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              c_qg%bdy(idx,lvl) = xqgraup(c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          c_tke%bdy(idx,lvl) = xtke(c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          c_pv%bdy(idx,lvl) = xpvc(c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          c_wwind%bdy(idx,lvl) = xwwind(c,r,lvl)
        ENDIF

        IF ( ifcld3d ) THEN
          c_cfrac_3d%bdy(idx,lvl) = xcfrac3d(c,r,lvl)
        ENDIF

        IF ( ifkfradextras ) THEN
          c_qc_cu%bdy(idx,lvl)     = xqc_cu(c,r,lvl)
          c_qi_cu%bdy(idx,lvl)     = xqi_cu(c,r,lvl)
          c_cldfra_dp%bdy(idx,lvl) = xcldfrad(c,r,lvl)
          c_cldfra_sh%bdy(idx,lvl) = xcldfras(c,r,lvl)
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin > xmissing ) THEN

        DO lvl = 1, nlays-1
          c_what_jd%bdy(idx,lvl) = xwhat(c,r,lvl) *                           &
                                   ( wght_bot(lvl) * c_densa_j%bdy(idx,lvl)   &
                                   + wght_top(lvl) * c_densa_j%bdy(idx,lvl+1) )
        ENDDO

        c_what_jd%bdy(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO

  ! Eastern boundary moving south to north from row 1 (in output grid) to
  ! row NROWS+NTHIK.

  DO r = 1+nthik, nrows_x
    DO l = 1, nthik
      c = ncols_x - nthik + l
      idx = idx + 1
      DO lvl = 1, nlays

        ! Used in generalized vertical coordinates in CCTM.

        IF ( ( x3jfmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobf%bdy(idx,lvl) = x3jacobf(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobm%bdy(idx,lvl) = x3jacobm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin > xmissing ) .AND. ( x3jmmin > xmissing ) ) THEN
          c_densa_j%bdy(idx,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! State variables.

        c_ta%bdy(idx,lvl)   = xtempm(c,r,lvl)
        c_qv%bdy(idx,lvl)   = xwvapor(c,r,lvl)
        c_pres%bdy(idx,lvl) = xpresm(c,r,lvl)
        c_dens%bdy(idx,lvl) = xdensam(c,r,lvl)
        c_zh%bdy(idx,lvl)   = x3htm(c,r,lvl)
        c_zf%bdy(idx,lvl)   = x3htf(c,r,lvl)

        ! Moisture variables.

        IF ( nqspecies >= 2 ) THEN
          c_qc%bdy(idx,lvl) = xcldwtr(c,r,lvl)
          c_qr%bdy(idx,lvl) = xranwtr(c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            c_qi%bdy(idx,lvl) = xqice(c,r,lvl)
            c_qs%bdy(idx,lvl) = xqsnow(c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              c_qg%bdy(idx,lvl) = xqgraup(c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          c_tke%bdy(idx,lvl) = xtke(c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          c_pv%bdy(idx,lvl) = xpvc(c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          c_wwind%bdy(idx,lvl) = xwwind(c,r,lvl)
        ENDIF

        IF ( ifcld3d ) THEN
          c_cfrac_3d%bdy(idx,lvl) = xcfrac3d(c,r,lvl)
        ENDIF

        IF ( ifkfradextras ) THEN
          c_qc_cu%bdy(idx,lvl)     = xqc_cu(c,r,lvl)
          c_qi_cu%bdy(idx,lvl)     = xqi_cu(c,r,lvl)
          c_cldfra_dp%bdy(idx,lvl) = xcldfrad(c,r,lvl)
          c_cldfra_sh%bdy(idx,lvl) = xcldfras(c,r,lvl)
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin > xmissing ) THEN

        DO lvl = 1, nlays-1
          c_what_jd%bdy(idx,lvl) = xwhat(c,r,lvl) *                           &
                                   ( wght_bot(lvl) * c_densa_j%bdy(idx,lvl)   &
                                   + wght_top(lvl) * c_densa_j%bdy(idx,lvl+1) )
        ENDDO

        c_what_jd%bdy(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO

  ! Northern boundary moving west to east from column 1-NTHIK (in output grid)
  ! to column NCOLS.

  DO l = 1, nthik
    r = nrows_x - nthik + l
    DO c = 1, ncols_x - nthik

      idx = idx + 1

      DO lvl = 1, nlays

        ! Used in generalized vertical coordinates in CCTM.

        IF ( ( x3jfmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobf%bdy(idx,lvl) = x3jacobf(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobm%bdy(idx,lvl) = x3jacobm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin > xmissing ) .AND. ( x3jmmin > xmissing ) ) THEN
          c_densa_j%bdy(idx,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! State variables.

        c_ta%bdy(idx,lvl)   = xtempm(c,r,lvl)
        c_qv%bdy(idx,lvl)   = xwvapor(c,r,lvl)
        c_pres%bdy(idx,lvl) = xpresm(c,r,lvl)
        c_dens%bdy(idx,lvl) = xdensam(c,r,lvl)
        c_zh%bdy(idx,lvl)   = x3htm(c,r,lvl)
        c_zf%bdy(idx,lvl)   = x3htf(c,r,lvl)

        ! Moisture variables.

        IF ( nqspecies >= 2 ) THEN
          c_qc%bdy(idx,lvl) = xcldwtr(c,r,lvl)
          c_qr%bdy(idx,lvl) = xranwtr(c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            c_qi%bdy(idx,lvl) = xqice(c,r,lvl)
            c_qs%bdy(idx,lvl) = xqsnow(c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              c_qg%bdy(idx,lvl) = xqgraup(c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          c_tke%bdy(idx,lvl) = xtke(c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          c_pv%bdy(idx,lvl) = xpvc(c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          c_wwind%bdy(idx,lvl) = xwwind(c,r,lvl)
        ENDIF

        IF ( ifcld3d ) THEN
          c_cfrac_3d%bdy(idx,lvl) = xcfrac3d(c,r,lvl)
        ENDIF

        IF ( ifkfradextras ) THEN
          c_qc_cu%bdy(idx,lvl)     = xqc_cu(c,r,lvl)
          c_qi_cu%bdy(idx,lvl)     = xqi_cu(c,r,lvl)
          c_cldfra_dp%bdy(idx,lvl) = xcldfrad(c,r,lvl)
          c_cldfra_sh%bdy(idx,lvl) = xcldfras(c,r,lvl)
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin > xmissing ) THEN

        DO lvl = 1, nlays-1
          c_what_jd%bdy(idx,lvl) = xwhat(c,r,lvl) *                          &
                                   ( wght_bot(lvl) * c_densa_j%bdy(idx,lvl)  & 
                                   + wght_top(lvl) * c_densa_j%bdy(idx,lvl+1) )
        ENDDO

        c_what_jd%bdy(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO
       
  ! Western boundary moving south to north from row 1-NTHIK (in output grid)
  ! to row NROWS.

  DO r = 1, nrows_x - nthik
    DO c = 1, nthik
      idx = idx + 1
      DO lvl = 1, nlays

        ! Used in generalized vertical coordinates in CCTM.

        IF ( ( x3jfmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobf%bdy(idx,lvl) = x3jacobf(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin > xmissing ) .AND. ( xmapmin > xmissing ) ) THEN
          c_jacobm%bdy(idx,lvl) = x3jacobm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin > xmissing ) .AND. ( x3jmmin > xmissing ) ) THEN
          c_densa_j%bdy(idx,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! State variables.

        c_ta%bdy(idx,lvl)   = xtempm(c,r,lvl)
        c_qv%bdy(idx,lvl)   = xwvapor(c,r,lvl)
        c_pres%bdy(idx,lvl) = xpresm(c,r,lvl)
        c_dens%bdy(idx,lvl) = xdensam(c,r,lvl)
        c_zh%bdy(idx,lvl)   = x3htm(c,r,lvl)
        c_zf%bdy(idx,lvl)   = x3htf(c,r,lvl)

        ! Moisture variables.

        IF ( nqspecies >= 2 ) THEN
          c_qc%bdy(idx,lvl) = xcldwtr(c,r,lvl)
          c_qr%bdy(idx,lvl) = xranwtr(c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            c_qi%bdy(idx,lvl) = xqice(c,r,lvl)
            c_qs%bdy(idx,lvl) = xqsnow(c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              c_qg%bdy(idx,lvl) = xqgraup(c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          c_tke%bdy(idx,lvl) = xtke(c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          c_pv%bdy(idx,lvl) = xpvc(c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          c_wwind%bdy(idx,lvl) = xwwind(c,r,lvl)
        ENDIF

        IF ( ifcld3d ) THEN
          c_cfrac_3d%bdy(idx,lvl) = xcfrac3d(c,r,lvl)
        ENDIF

        IF ( ifkfradextras ) THEN
          c_qc_cu%bdy(idx,lvl)     = xqc_cu(c,r,lvl)
          c_qi_cu%bdy(idx,lvl)     = xqi_cu(c,r,lvl)
          c_cldfra_dp%bdy(idx,lvl) = xcldfrad(c,r,lvl)
          c_cldfra_sh%bdy(idx,lvl) = xcldfras(c,r,lvl)
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin > xmissing ) THEN

        DO lvl = 1, nlays-1
          c_what_jd%bdy(idx,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * c_densa_j%bdy(idx,lvl)   &
                                 + wght_top(lvl) * c_densa_j%bdy(idx,lvl+1) )
        ENDDO

        c_what_jd%bdy(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Fill time-varying 3d fields at cell corners and cell faces.
!
! These arrays are all set to the dot-point dimensions to accommodate the
! false dot points in the Arakawa-C staggered grid that are output in
! Models-3 I/O API "DOT" files.  When the output is written in netCDF, the
! true dimensions of the Arakawa-C staggered fields are used.
!-------------------------------------------------------------------------------

  DO k = 1, nlays
    DO row = 1, nrows+1
      r = row + nthik
      DO col = 1, ncols+1
        c = col + nthik

        c_uwindc%fld(col,row,k)  = xuu_s(c,r,k)
        c_vwindc%fld(col,row,k)  = xvv_t(c,r,k)
        c_uhat_jd%fld(col,row,k) = xuhat_s(c,r,k)
        c_vhat_jd%fld(col,row,k) = xvhat_t(c,r,k)

        IF ( luvbout > 0 ) THEN
          c_uwind%fld(col,row,k) = xuu_d(c,r,k)
          c_vwind%fld(col,row,k) = xvv_d(c,r,k)
        ENDIF

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Fill time-varying 3d fields (soil layers) at cell centers.
!-------------------------------------------------------------------------------

  IF ( ifsoil ) THEN

    DO row = 1, nrows
      r = row + nthik
      DO col = 1, ncols
        c = col + nthik
        DO lvl = 1, metsoi

          c_soit3d%fld(col,row,lvl) = xsoit3d(c,r,lvl)
          c_soim3d%fld(col,row,lvl) = xsoim3d(c,r,lvl)

        ENDDO
      ENDDO
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Fill time-varying 3d fields (mosaic land use categories) at cell centers.
!-------------------------------------------------------------------------------

  IF ( ifmosaic ) THEN

    DO row = 1, nrows
      r = row + nthik
      DO col = 1, ncols
        c = col + nthik
        DO lvl = 1, nummosaic

          c_lufrac2%fld(col,row,lvl) = xlufrac2  (c,r,lvl)
          c_moscat%fld(col,row,lvl)  = xmoscatidx(c,r,lvl)
          c_lai_mos%fld(col,row,lvl) = xlai_mos  (c,r,lvl)

          IF ( xra_mos(c,r,lvl) < xmissing ) THEN
            c_rai_mos%fld(col,row,lvl) = 0.0
          ELSE
            c_rai_mos%fld(col,row,lvl) = 1.0 / xra_mos(c,r,lvl)
          ENDIF

          IF ( xrs_mos(c,r,lvl) < xmissing ) THEN
            c_rsi_mos%fld(col,row,lvl) = 0.0
          ELSE
            c_rsi_mos%fld(col,row,lvl) = 1.0 / xrs_mos(c,r,lvl)
          ENDIF

          c_tsk_mos%fld(col,row,lvl) = xtsk_mos  (c,r,lvl)
          c_znt_mos%fld(col,row,lvl) = xznt_mos  (c,r,lvl)

        ENDDO
      ENDDO
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( jdenm      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xrhojf     )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xrhojm     )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xuhat_s    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xvhat_t    )  ! commented out to avoid memory fragmentation

END SUBROUTINE ctmproc
