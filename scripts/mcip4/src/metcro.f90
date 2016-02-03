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

SUBROUTINE metcro (sdate, stime)

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
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE coord
  USE m3utilio
  USE files
  USE mcoutcom
  USE vgrd
  USE sat2mcip

  IMPLICIT NONE

  INTEGER                           :: c
  INTEGER                           :: col
  REAL,    SAVE,      ALLOCATABLE   :: dumaray0    ( : , : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dumaray1    ( : , : , : , : )
  REAL,               PARAMETER     :: epsilonq    = 1.0e-30
  LOGICAL, SAVE                     :: first       = .TRUE.
  INTEGER                           :: idx
  CHARACTER(LEN=63)                 :: ifmt1
  INTEGER                           :: ilg
  INTEGER, SAVE                     :: ipv
  INTEGER, SAVE                     :: isat
  INTEGER, SAVE                     :: isoil
  INTEGER, SAVE                     :: itke
  INTEGER, SAVE                     :: iwout
  INTEGER, SAVE                     :: iwr
  INTEGER                           :: k
  INTEGER                           :: k1
  INTEGER                           :: k2
  INTEGER                           :: l
  INTEGER                           :: lvl
  INTEGER                           :: n
  CHARACTER(LEN=16),  PARAMETER     :: pname       = 'METCRO'
  INTEGER                           :: r
  REAL                              :: rhojmi
  INTEGER                           :: row
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime
  CHARACTER(LEN=2)                  :: str1
  CHARACTER(LEN=2)                  :: str2
  REAL,    SAVE,      ALLOCATABLE   :: wght_bot    ( : )
  REAL,    SAVE,      ALLOCATABLE   :: wght_top    ( : )
  REAL                              :: x3jfmin
  REAL                              :: x3jmmin
  REAL                              :: xdnamin
  REAL                              :: xdnjmin
  REAL                              :: xmapmin
  REAL,    SAVE,      ALLOCATABLE   :: xrhojf      ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: xrhojm      ( : , : , : )

  INTERFACE

    SUBROUTINE collapx (aa, vertin, vertout)
      IMPLICIT NONE
      REAL,               INTENT(INOUT) :: aa         ( : , : , : )
      REAL,               INTENT(IN)    :: vertin     ( : )
      REAL,               INTENT(IN)    :: vertout    ( : )
    END SUBROUTINE collapx

  END INTERFACE

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
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( xrhojf ) )  &
    ALLOCATE ( xrhojf ( ncols_x, nrows_x, 0:metlay ) )
  IF ( .NOT. ALLOCATED ( xrhojm ) )  &
    ALLOCATE ( xrhojm ( ncols_x, nrows_x,   metlay ) )

  IF ( lsat == 1 ) THEN

    IF ( .NOT. ALLOCATED ( cldtr_c ) )  &
      ALLOCATE ( cldtr_c ( ncols, nrows ) )

    cldtr_c = badval3

  ENDIF

!-------------------------------------------------------------------------------
! If first time, build headers for files.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ALLOCATE ( wght_top (nlays) )
    ALLOCATE ( wght_bot (nlays) )

    ! Build common header.

    CALL comheader (sdate, stime)

    !---------------------------------------------------------------------------
    ! Build and write header for METCRO2D file.
    !---------------------------------------------------------------------------

    DO idx = 1, mc2index
      vtype3d(idx) = m3real
      vname3d(idx) = mc2vname(idx)
      units3d(idx) = mc2units(idx)
      vdesc3d(idx) = mc2vdesc(idx)
    ENDDO

    IF ( ifwr ) THEN
      iwr = 1
      idx = mc2index + iwr
      vtype3d(idx) = m3real
      vname3d(idx) = 'WR'
      units3d(idx) = 'M'
      vdesc3d(idx) = 'canopy moisture content'
    ELSE
      iwr = 0
    ENDIF

    IF ( ifsoil ) THEN
      isoil = 5
      idx = mc2index + iwr + 1
      vtype3d(idx) = m3real
      vname3d(idx) = 'SOIM1'
      units3d(idx) = 'M**3/M**3'
      vdesc3d(idx) = 'volumetric soil moisture in top cm'
      idx = mc2index + iwr + 2
      vtype3d(idx) = m3real
      vname3d(idx) = 'SOIM2'
      units3d(idx) = 'M**3/M**3'
      vdesc3d(idx) = 'volumetric soil moisture in top m'
      idx = mc2index + iwr + 3
      vtype3d(idx) = m3real
      vname3d(idx) = 'SOIT1'
      units3d(idx) = 'K'
      vdesc3d(idx) = 'soil temperature in top cm'
      idx = mc2index + iwr + 4
      vtype3d(idx) = m3real
      vname3d(idx) = 'SOIT2'
      units3d(idx) = 'K'
      vdesc3d(idx) = 'soil temperature in top m'
      idx = mc2index + iwr + 5
      vtype3d(idx) = m3real
      vname3d(idx) = 'SLTYP'
      units3d(idx) = 'CATEGORY'
      vdesc3d(idx) = 'soil texture type by USDA category'
    ELSE
      isoil = 0
    ENDIF

    IF ( lsat == 1 ) THEN  ! Set up header for cloud transmissivity
      isat = 1
      idx = mc2index + iwr + isoil + isat
      vtype3d(idx) = m3real
      vname3d(idx) = 'CLDTR'
      units3d(idx) = 'FRACTION'
      vdesc3d(idx) = 'Cloud transmissivity'
    ELSE
      isat = 0
    ENDIF

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    ftype3d = grdded3
    nvars3d = mc2index + iwr + isoil + isat
    nlays3d = 1
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik
    tstep3d = grstep

    IF ( .NOT. open3 (metcro2d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF

    !---------------------------------------------------------------------------
    ! Build and write header for METCRO3D file.
    !---------------------------------------------------------------------------

    DO idx = 1, mc3index
      vtype3d(idx) = m3real
      vname3d(idx) = mc3vname(idx)
      units3d(idx) = mc3units(idx)
      vdesc3d(idx) = mc3vdesc(idx)
    ENDDO

    DO ilg = 1, nqspecies
      idx = mc3index + ilg
      vtype3d(idx) = m3real
      vname3d(idx) = qc3vname(ilg)
      units3d(idx) = qc3units(ilg)
      vdesc3d(idx) = qc3vdesc(ilg)
    ENDDO

    IF ( iftke ) THEN
      itke = 1
      idx = mc3index + nqspecies + itke
      vtype3d(idx) = m3real
      IF ( iftkef ) THEN  ! TKE on full-levels
        vname3d(idx) = 'TKEF'
        units3d(idx) = 'J/KG'
        vdesc3d(idx) = 'turbulent kinetic energy on full-levels'
      ELSE  ! TKE on half-layers
        vname3d(idx) = 'TKE'
        units3d(idx) = 'J/KG'
        vdesc3d(idx) = 'turbulent kinetic energy on half-layers'
      ENDIF
    ELSE
      itke = 0
    ENDIF

    IF ( lpv > 0 ) THEN
      ipv = 1
      idx = mc3index + nqspecies + itke + ipv
      vtype3d(idx) = m3real
      vname3d(idx) = 'PV'
      units3d(idx) = 'M^2*K/KG/S * E-6'
      vdesc3d(idx) = 'potential vorticity'
    ELSE
      ipv = 0
    ENDIF

    IF ( lwout > 0 ) THEN
      iwout = 1
      idx = mc3index + nqspecies + itke + ipv + iwout
      vtype3d(idx) = m3real
      vname3d(idx) = 'WWIND'
      units3d(idx) = 'M/S'
      vdesc3d(idx) = 'true W component of wind'
    ELSE
      iwout = 0
    ENDIF

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    xorig3d = xorig_gd
    yorig3d = yorig_gd
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik

    ftype3d = grdded3
    nvars3d = mc3index + nqspecies + itke + ipv + iwout
    nlays3d = nlays

    IF ( .NOT. open3 (metcro3d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(metcro3d)
      CALL graceful_stop (pname)
    ENDIF

    !---------------------------------------------------------------------------
    ! Build and write header for METBDY3D file.
    ! Header information is the same as METCRO3D except for file type.
    !---------------------------------------------------------------------------

    ftype3d = bndary3
    nvars3d = mc3index + nqspecies + itke + ipv + iwout
    nlays3d = nlays

    IF ( .NOT. open3 (metbdy3d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(metbdy3d)
      CALL graceful_stop (pname)
    ENDIF

    ! Compute linear interpolation coefficients based on layer thickness.

    DO lvl = 1, nlays-1
      wght_top(lvl) = ( x3face_gd(lvl)   - x3midl(lvl) )  &
                      / ( x3midl (lvl+1) - x3midl(lvl) )
      wght_bot(lvl) = 1.0 - wght_top(lvl)
    ENDDO

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Build common header.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Assign arrays in MET_CRO_2D (time dependent).
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapc)
  x3jfmin = MINVAL(x3jacobf)

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik

      prsfc_c  (col,row) = xprsfc  (c,r)
      ustar_c  (col,row) = xustar  (c,r)
      wstar_c  (col,row) = xwstar  (c,r)
      pbl_c    (col,row) = xpbl    (c,r)
      zzero_c  (col,row) = xzruf   (c,r)

      IF ( xmol(c,r) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        moli_c (col,row) = 0.0
      ELSE
        moli_c (col,row) = 1.0 / xmol (c,r)
      ENDIF

      hfx_c    (col,row) = xhfx    (c,r)
      qfx_c    (col,row) = xqfx    (c,r)

      IF ( xradyn(c,r) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        radyni_c (col,row) = 0.0
      ELSE
        radyni_c (col,row) = 1.0 / xradyn  (c,r)
      ENDIF

      IF ( xrstom(c,r) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        rstomi_c (col,row) = 0.0
      ELSE
        rstomi_c (col,row) = 1.0 / xrstom  (c,r)
      ENDIF

      tempg_c  (col,row) = xtempg  (c,r)
      temp2_c  (col,row) = xtemp2  (c,r)
      q2_c     (col,row) = xq2     (c,r)
      wspd10_c (col,row) = xwspd10 (c,r)
      wdir10_c (col,row) = xwdir10 (c,r)
      glw_c    (col,row) = xglw    (c,r)
      gsw_c    (col,row) = xgsw    (c,r)
      rgrnd_c  (col,row) = xrgrnd  (c,r)

      rainn_c  (col,row) = xrainn  (c,r)
      rainc_c  (col,row) = xrainc  (c,r)
      cfract_c (col,row) = xcfract (c,r)
      cldtop_c (col,row) = xcldtop (c,r)
      cldbot_c (col,row) = xcldbot (c,r)
      wbar_c   (col,row) = xwbar   (c,r) 

      snocov_c (col,row) = xsnocov (c,r)
      veg_c    (col,row) = xveg    (c,r)
      lai_c    (col,row) = xlai    (c,r)
      seaice_c (col,row) = xseaice (c,r)

      IF ( ifwr ) THEN
        wr_c     (col,row) = xwr     (c,r)
      ENDIF

      IF ( ifsoil ) THEN
        soim1_c  (col,row) = xwga    (c,r)
        soim2_c  (col,row) = xw2a    (c,r)
        soit1_c  (col,row) = xtga    (c,r)
        soit2_c  (col,row) = xt2a    (c,r)
        sltyp_c  (col,row) = xsltyp  (c,r)
      ENDIF

      IF ( lsat == 1 ) THEN
        cldtr_c  (col,row) = xcldtr  (c,r)
      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Calculate density*Jacobian on mid-layers and full levels.  Use this for
! collapsing rather than Jacobian alone.
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
! Collapse 3-D 'X' arrays.  (Original values are replaced temporarily.)
! For safe collpasing, store information first in DUMARRAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    IF ( iftke ) THEN
      IF ( iftkef ) THEN  ! TKE on full-levels
        IF ( .NOT. ALLOCATED ( dumaray0 ) ) &
          ALLOCATE ( dumaray0 ( ncols_x, nrows_x, 0:metlay, 4+itke+iwout ) )
        IF ( .NOT. ALLOCATED ( dumaray1 ) ) &
          ALLOCATE ( dumaray1 ( ncols_x, nrows_x, 1:metlay, 6+nqspecies+ipv ) )
      ELSE  ! TKE on half-layers
        IF ( .NOT. ALLOCATED ( dumaray0 ) ) &
          ALLOCATE ( dumaray0 ( ncols_x, nrows_x, 0:metlay, 4+iwout ) )
        IF ( .NOT. ALLOCATED ( dumaray1 ) ) &
          ALLOCATE ( dumaray1 ( ncols_x, nrows_x, 1:metlay, 6+nqspecies+itke+ipv ) )
      ENDIF
    ELSE
      IF ( .NOT. ALLOCATED ( dumaray0 ) ) &
        ALLOCATE ( dumaray0 ( ncols_x, nrows_x, 0:metlay, 4+iwout ) )
      IF ( .NOT. ALLOCATED ( dumaray1 ) ) &
        ALLOCATE ( dumaray1 ( ncols_x, nrows_x, 1:metlay, 6+nqspecies+ipv ) )
    ENDIF

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x

          dumaray1(c,r,k, 1) = xrhojm  (c,r,k)
          dumaray1(c,r,k, 2) = xdensam (c,r,k)
          dumaray1(c,r,k, 3) = xpresm  (c,r,k)
          dumaray1(c,r,k, 4) = xtempm  (c,r,k)
          dumaray1(c,r,k, 5) = xwvapor (c,r,k)
          dumaray1(c,r,k, 6) = x3htm   (c,r,k)

          IF ( nqspecies >= 2 ) THEN
            dumaray1(c,r,k, 7) = xcldwtr (c,r,k)
            dumaray1(c,r,k, 8) = xranwtr (c,r,k)
            IF ( nqspecies >= 4 ) THEN
              dumaray1(c,r,k, 9) = xqice   (c,r,k)
              dumaray1(c,r,k,10) = xqsnow  (c,r,k)
              IF ( nqspecies == 5 ) THEN
                dumaray1(c,r,k,11) = xqgraup (c,r,k)
              ENDIF
            ENDIF
          ENDIF

        ENDDO 
      ENDDO
    ENDDO

    IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN  ! TKE on half-layers
      dumaray1(:,:,:,6+nqspecies+itke) = xtke (:,:,:)
    ENDIF

    IF ( lpv > 0 ) THEN  ! Output potential vorticity
      dumaray1(:,:,:,6+nqspecies+itke+ipv) = xpvc (:,:,:)
    ENDIF

    DO k = 0, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x

          dumaray0(c,r,k,1) = xrhojf  (c,r,k)
          dumaray0(c,r,k,2) = xwhat   (c,r,k)
          dumaray0(c,r,k,3) = x3htf   (c,r,k)
          dumaray0(c,r,k,4) = xdensaf (c,r,k)

        ENDDO 
      ENDDO
    ENDDO    

    IF ( lwout > 0 ) THEN
      DO k = 0, metlay
        DO r = 1, nrows_x
          DO c = 1, ncols_x
            dumaray0(c,r,k,5) = xwwind  (c,r,k)
          ENDDO 
        ENDDO
      ENDDO    
    ENDIF
      
    IF ( ( iftke ) .AND. ( iftkef ) ) THEN  ! TKE on full-levels
      dumaray0(:,:,0:,5+iwout) = xtke (:,:,0:)
    ENDIF

    CALL collapx (xrhojm,  xx3midl, x3midl)
    CALL collapx (xdensam, xx3midl, x3midl)
    CALL collapx (xpresm,  xx3midl, x3midl)
    CALL collapx (xtempm,  xx3midl, x3midl)
    CALL collapx (xwvapor, xx3midl, x3midl)
    CALL collapx (x3htm,   xx3midl, x3midl)
    IF ( nqspecies >= 2 ) THEN
      CALL collapx (xcldwtr, xx3midl, x3midl)
      CALL collapx (xranwtr, xx3midl, x3midl)
      IF ( nqspecies >= 4 ) THEN
        CALL collapx (xqice,   xx3midl, x3midl)
        CALL collapx (xqsnow,  xx3midl, x3midl)
        IF ( nqspecies == 5 ) THEN
          CALL collapx (xqgraup, xx3midl, x3midl)
        ENDIF
      ENDIF
    ENDIF

    IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN  ! TKE on half-layers
      CALL collapx (xtke,  xx3midl, x3midl)
    ENDIF

    IF ( lpv > 0 ) THEN  ! Output potential vorticity
      CALL collapx (xpvc,  xx3midl, x3midl)
    ENDIF

    CALL collapx (xrhojf,  xx3face, x3face)
    CALL collapx (xwhat,   xx3face, x3face)
    CALL collapx (x3htf,   xx3face, x3face)
    CALL collapx (xdensaf, xx3face, x3face)
 
    IF ( lwout > 0 ) THEN
      CALL collapx (xwwind,  xx3face, x3face)
    ENDIF

    IF ( ( iftke ) .AND. ( iftkef ) ) THEN  ! TKE on full-levels
      CALL collapx (xtke,  xx3face, x3face)
    ENDIF

  ENDIF  

!-------------------------------------------------------------------------------
! Assign arrays in MET_CRO_3D (time dependent).
!-------------------------------------------------------------------------------

  x3jmmin = MINVAL(x3jacobm)
  xdnamin = MINVAL(xdensam)

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_c (col,row,lvl) = badval3
        ELSE
          jacobf_c (col,row,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /   &
                                   xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_c (col,row,lvl) = badval3
        ELSE
          jacobm_c (col,row,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /   &
                                   xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_c(col,row,lvl) = badval3
        ELSE
          densa_j_c(col,row,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! Following for testing consistency in met. data

        tempa_c  (col,row,lvl) = xtempm  (c,r,lvl)
        wvapor_c (col,row,lvl) = xwvapor (c,r,lvl)
        press_c  (col,row,lvl) = xpresm  (c,r,lvl)
        densa_c  (col,row,lvl) = xdensam (c,r,lvl)
        x3htf_c  (col,row,lvl) = x3htf   (c,r,lvl)
        x3htm_c  (col,row,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_c (col,row,lvl) = xcldwtr (c,r,lvl)
          ranwtr_c (col,row,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_c   (col,row,lvl) = xqice   (c,r,lvl)
            qsnow_c  (col,row,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_c (col,row,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          tke_c  (col,row,lvl) = xtke  (c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          pvc_c  (col,row,lvl) = xpvc  (c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          wwind_c  (col,row,lvl) = xwwind  (c,r,lvl)
        ENDIF

      ENDDO
    ENDDO
  ENDDO

  ! Added for mass consistency

  xdnjmin = MINVAL(densa_j_c)

  IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

    what_jd_c(:,:,:) = badval3

  ELSE

    DO row = 1, nrows
      r = row + nthik
      DO col = 1, ncols
        c = col + nthik
        DO lvl = 1, nlays-1

          what_jd_c(col,row,lvl) = xwhat(c,r,lvl) *                           &
                                   ( wght_bot(lvl) * densa_j_c(col,row,lvl)   & 
                                   + wght_top(lvl) * densa_j_c(col,row,lvl+1) )
        ENDDO

        what_jd_c(col,row,nlays) = 0.0

      ENDDO
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Assign arrays in MET_BDY_3D (time dependent).
!-------------------------------------------------------------------------------

  idx = 0

  ! Southern boundary moving west to east from column 1 (in output grid) to
  ! column NCOLS+NTHIK.

  DO r = 1, nthik
    DO c = 1 + nthik, ncols_x
      idx = idx + 1
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (idx,lvl) = badval3
        ELSE
          jacobf_b (idx,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (idx,lvl) = badval3
        ELSE
          jacobm_b (idx,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(idx,lvl) = badval3
        ELSE
          densa_j_b(idx,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! Following for testing consistency in met. data

        tempa_b  (idx,lvl) = xtempm  (c,r,lvl)
        wvapor_b (idx,lvl) = xwvapor (c,r,lvl)
        press_b  (idx,lvl) = xpresm  (c,r,lvl)
        densa_b  (idx,lvl) = xdensam (c,r,lvl)
        x3htf_b  (idx,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (idx,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_b (idx,lvl) = xcldwtr (c,r,lvl)
          ranwtr_b (idx,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_b   (idx,lvl) = xqice   (c,r,lvl)
            qsnow_b  (idx,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_b (idx,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          tke_b  (idx,lvl) = xtke  (c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          pvc_b  (idx,lvl) = xpvc  (c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          wwind_b  (idx,lvl) = xwwind  (c,r,lvl)
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(idx,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(idx,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(idx,lvl)   &
                                 + wght_top(lvl) * densa_j_b(idx,lvl+1) )
        ENDDO

        what_jd_b(idx,nlays) = 0.0

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

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (idx,lvl) = badval3
        ELSE
          jacobf_b (idx,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (idx,lvl) = badval3
        ELSE
          jacobm_b (idx,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(idx,lvl) = badval3
        ELSE
          densa_j_b(idx,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! Following for testing consistency in met. data

        tempa_b  (idx,lvl) = xtempm  (c,r,lvl)
        wvapor_b (idx,lvl) = xwvapor (c,r,lvl)
        press_b  (idx,lvl) = xpresm  (c,r,lvl)
        densa_b  (idx,lvl) = xdensam (c,r,lvl)
        x3htf_b  (idx,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (idx,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_b (idx,lvl) = xcldwtr (c,r,lvl)
          ranwtr_b (idx,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_b   (idx,lvl) = xqice   (c,r,lvl)
            qsnow_b  (idx,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_b (idx,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          tke_b  (idx,lvl) = xtke  (c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          pvc_b  (idx,lvl) = xpvc  (c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          wwind_b  (idx,lvl) = xwwind  (c,r,lvl)
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(idx,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(idx,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(idx,lvl)   &
                                 + wght_top(lvl) * densa_j_b(idx,lvl+1) )
        ENDDO

        what_jd_b(idx,nlays) = 0.0

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

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (idx,lvl) = badval3
        ELSE
          jacobf_b (idx,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (idx,lvl) = badval3
        ELSE
          jacobm_b (idx,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(idx,lvl) = badval3
        ELSE
          densa_j_b(idx,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! Following for testing consistency in met. data

        tempa_b  (idx,lvl) = xtempm  (c,r,lvl)
        wvapor_b (idx,lvl) = xwvapor (c,r,lvl)
        press_b  (idx,lvl) = xpresm  (c,r,lvl)
        densa_b  (idx,lvl) = xdensam (c,r,lvl)
        x3htf_b  (idx,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (idx,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_b (idx,lvl) = xcldwtr (c,r,lvl)
          ranwtr_b (idx,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_b   (idx,lvl) = xqice   (c,r,lvl)
            qsnow_b  (idx,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_b (idx,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          tke_b  (idx,lvl) = xtke  (c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          pvc_b  (idx,lvl) = xpvc  (c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          wwind_b  (idx,lvl) = xwwind  (c,r,lvl)
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(idx,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(idx,lvl) = xwhat(c,r,lvl) *               &
                        ( wght_bot(lvl) * densa_j_b(idx,lvl)  & 
                        + wght_top(lvl) * densa_j_b(idx,lvl+1) )
        ENDDO

        what_jd_b(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO
       
  ! Western boundary moving south to north from row 1-NTHIK (in output grid)
  ! to row NROWS.

  DO r = 1, nrows_x - nthik
    DO c = 1, nthik
      idx = idx + 1
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (idx,lvl) = badval3
        ELSE
          jacobf_b (idx,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc2(c,r)
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (idx,lvl) = badval3
        ELSE
          jacobm_b (idx,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc2(c,r)
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(idx,lvl) = badval3
        ELSE
          densa_j_b(idx,lvl) = xrhojm(c,r,lvl) / xmapc2(c,r)
        ENDIF

        ! Following for testing consistency in met. data

        tempa_b  (idx,lvl) = xtempm  (c,r,lvl)
        wvapor_b (idx,lvl) = xwvapor (c,r,lvl)
        press_b  (idx,lvl) = xpresm  (c,r,lvl)
        densa_b  (idx,lvl) = xdensam (c,r,lvl)
        x3htf_b  (idx,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (idx,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_b (idx,lvl) = xcldwtr (c,r,lvl)
          ranwtr_b (idx,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_b   (idx,lvl) = xqice   (c,r,lvl)
            qsnow_b  (idx,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_b (idx,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

        IF ( iftke ) THEN
          tke_b  (idx,lvl) = xtke  (c,r,lvl)
        ENDIF

        IF ( lpv > 0 ) THEN
          pvc_b  (idx,lvl) = xpvc  (c,r,lvl)
        ENDIF

        IF ( lwout > 0 ) THEN
          wwind_b  (idx,lvl) = xwwind  (c,r,lvl)
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(idx,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(idx,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(idx,lvl)   &
                                 + wght_top(lvl) * densa_j_b(idx,lvl+1) )
        ENDDO

        what_jd_b(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Write MET_CRO_2D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metcro2d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metcro2d // ' file', .TRUE.)
  ENDIF

  DO l = 1, mc2index
    IF ( .NOT. write3 (metcro2d, vname3d(l), sdate, stime,  &
                       mc2(1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  IF ( ifwr ) THEN
    idx = mc2index + iwr
    IF ( .NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                       wr_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( ifsoil ) THEN
    idx = mc2index + iwr + 1
    IF ( .NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                       soim1_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
    idx = mc2index + iwr + 2
    IF ( .NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                       soim2_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
    idx = mc2index + iwr + 3
    IF ( .NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                       soit1_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
    idx = mc2index + iwr + 4
    IF ( .NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                       soit2_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
    idx = mc2index + iwr + 5
    IF ( .NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                       sltyp_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( lsat == 1 ) THEN
    idx = mc2index + iwr + isoil + isat
    IF (.NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                      cldtr_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Write MET_CRO_3D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metcro3d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metcro3d // ' file', .TRUE.)
  ENDIF

  WHERE ( ABS(mc3) < epsilonq ) mc3 = 0.0
  WHERE ( ABS(qc3) < epsilonq ) qc3 = 0.0

  DO l = 1, mc3index
    IF ( .NOT. write3 (metcro3d, vname3d(l), sdate, stime,  &
                       mc3(1,1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  DO l = 1, nqspecies
    IF ( .NOT. write3 (metcro3d, vname3d(mc3index+l), sdate, stime,  &
                       qc3(1,1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  IF ( iftke ) THEN
    idx = mc3index + nqspecies + itke
    IF ( .NOT. write3 (metcro3d, vname3d(idx), sdate, stime,  &
                       tke_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( lpv > 0 ) THEN
    idx = mc3index + nqspecies + itke + ipv
    IF ( .NOT. write3 (metcro3d, vname3d(idx), sdate, stime,  &
                       pvc_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( lwout > 0 ) THEN
    idx = mc3index + nqspecies + itke + ipv + iwout
    IF ( .NOT. write3 (metcro3d, vname3d(idx), sdate, stime,  &
                       wwind_c) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metcro3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Write MET_BDY_3D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metbdy3d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metbdy3d // ' file', .TRUE.)
  ENDIF

  WHERE ( ABS(mb3) < epsilonq ) mb3 = 0.0
  WHERE ( ABS(qb3) < epsilonq ) qb3 = 0.0

  DO l = 1, mc3index
    IF ( .NOT. write3 (metbdy3d, vname3d(l), sdate, stime,  &
                       mb3(1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metbdy3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  DO l = 1, nqspecies
    IF ( .NOT. write3 (metbdy3d, vname3d(mc3index+l), sdate, stime,  &
                       qb3(1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metbdy3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  IF ( iftke ) THEN
    idx = mc3index + nqspecies + itke
    IF ( .NOT. write3 (metbdy3d, vname3d(idx), sdate, stime,  &
                       tke_b) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metbdy3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( lpv > 0 ) THEN
    idx = mc3index + nqspecies + itke + ipv
    IF ( .NOT. write3 (metbdy3d, vname3d(idx), sdate, stime,  &
                       pvc_b) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metbdy3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( lwout > 0 ) THEN
    idx = mc3index + nqspecies + itke + ipv + iwout
    IF ( .NOT. write3 (metbdy3d, vname3d(idx), sdate, stime,  &
                       wwind_b) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metbdy3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! For safe collpasing, restore information from DUMARAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          xrhojm  (c,r,k) = dumaray1(c,r,k, 1)
          xdensam (c,r,k) = dumaray1(c,r,k, 2)
          xpresm  (c,r,k) = dumaray1(c,r,k, 3)
          xtempm  (c,r,k) = dumaray1(c,r,k, 4)
          xwvapor (c,r,k) = dumaray1(c,r,k, 5)
          x3htm   (c,r,k) = dumaray1(c,r,k, 6)
          IF ( nqspecies >= 2 ) THEN
            xcldwtr (c,r,k) = dumaray1(c,r,k, 7)
            xranwtr (c,r,k) = dumaray1(c,r,k, 8)
            IF ( nqspecies >= 4 ) THEN
              xqice   (c,r,k) = dumaray1(c,r,k, 9)
              xqsnow  (c,r,k) = dumaray1(c,r,k,10)
              IF ( nqspecies == 5 ) THEN
                xqgraup (c,r,k) = dumaray1(c,r,k,11)
              ENDIF
            ENDIF
          ENDIF
        ENDDO 
      ENDDO
    ENDDO

    IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN  ! TKE on half-layers
      xtke (:,:,:) = dumaray1(:,:,:,6+nqspecies+itke)
    ENDIF

    IF ( lpv > 0 ) THEN  ! Output potential vorticity
      xpvc (:,:,:) = dumaray1(:,:,:,6+nqspecies+itke+ipv)
    ENDIF

    DO k = 0, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          xrhojf  (c,r,k) = dumaray0(c,r,k,1)
          xwhat   (c,r,k) = dumaray0(c,r,k,2)
          x3htf   (c,r,k) = dumaray0(c,r,k,3)
          xdensaf (c,r,k) = dumaray0(c,r,k,4)
        ENDDO 
      ENDDO
    ENDDO 

    IF ( lwout > 0 ) THEN
      DO k = 0, metlay
        DO r = 1, nrows_x
          DO c = 1, ncols_x
            xwwind  (c,r,k) = dumaray0(c,r,k,5)
          ENDDO 
        ENDDO
      ENDDO 
    ENDIF

    IF ( ( iftke ) .AND. ( iftkef ) ) THEN  ! TKE on full-levels
      xtke (:,:,0:) = dumaray0(:,:,0:,5+iwout)
    ENDIF

!   DEALLOCATE ( dumaray0 )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( dumaray1 )  ! commented out to avoid memory fragmentation

  ENDIF

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

  DO n = 1, mc2index
    WRITE (*,f6000) TRIM(mc2vname(n)), mc2(lprt_col,lprt_row,n), mc2units(n)
  ENDDO

  IF ( ifwr ) THEN
    idx = mc2index + iwr
    WRITE (*,f6000) TRIM(vname3d(idx)), wr_c(lprt_col,lprt_row), units3d(idx)
  ENDIF

  IF ( ifsoil ) THEN
    idx = mc2index + iwr + 1
    WRITE (*,f6000) TRIM(vname3d(idx)), soim1_c(lprt_col,lprt_row), units3d(idx)
    idx = mc2index + iwr + 2
    WRITE (*,f6000) TRIM(vname3d(idx)), soim2_c(lprt_col,lprt_row), units3d(idx)
    idx = mc2index + iwr + 3
    WRITE (*,f6000) TRIM(vname3d(idx)), soit1_c(lprt_col,lprt_row), units3d(idx)
    idx = mc2index + iwr + 4
    WRITE (*,f6000) TRIM(vname3d(idx)), soit2_c(lprt_col,lprt_row), units3d(idx)
    idx = mc2index + iwr + 5
    WRITE (*,f6000) TRIM(vname3d(idx)), sltyp_c(lprt_col,lprt_row), units3d(idx)
  ENDIF

  IF ( lsat == 1 ) THEN
    idx = mc2index + iwr + isoil + isat
    WRITE (*,f6000) TRIM(vname3d(idx)), cldtr_c(lprt_col,lprt_row), units3d(idx)
  ENDIF

  DO n = 1, mc3index
    WRITE (*,ifmt1) TRIM(mc3vname(n)), mc3(lprt_col,lprt_row,:,n)
  ENDDO

  DO n = 1, nqspecies
    WRITE (*,ifmt1) TRIM(qc3vname(n)), qc3(lprt_col,lprt_row,:,n)
  ENDDO

  IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN  ! TKE on half-layers
    WRITE (*,ifmt1) 'TKE',  tke_c(lprt_col,lprt_row,:)
  ELSE IF ( ( iftke ) .AND. ( iftkef ) ) THEN   ! TKE on full-levels
    WRITE (*,ifmt1) 'TKEF', tke_c(lprt_col,lprt_row,:)
  ENDIF

  IF ( lpv > 0 ) THEN  ! Output potential vorticity
    WRITE (*,ifmt1) 'PV',  pvc_c(lprt_col,lprt_row,:)
  ENDIF

  IF ( lwout > 0 ) THEN  ! Output vertical velocity
    WRITE (*,ifmt1) 'WWIND',  wwind_c(lprt_col,lprt_row,:)
  ENDIF

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( xrhojf )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xrhojm )  ! commented out to avoid memory fragmentation

END SUBROUTINE metcro
