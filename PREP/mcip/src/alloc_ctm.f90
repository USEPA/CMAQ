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

SUBROUTINE alloc_ctm

!-------------------------------------------------------------------------------
! Name:     Allocate Arrays for CTM Dimensions
! Purpose:  Allocate arrays based on CTM grid.
! Revised:  14 Sep 2001  Original version.  (T. Otte)
!           27 Feb 2002  Renamed SURF2 as WIND10.  Removed RIB.  (T. Otte)
!           18 Mar 2003  Removed JDRATE.  (T. Otte)
!           09 Jun 2003  Added SNOCOV to METCRO2D.  (D. Schwede)
!                        Removed extraneous variables from output.  (T. Otte)
!           09 Aug 2004  Added JACOBS, WSPD10, WDIR10, SOIM1, SOIM2, SOIT1,
!                        SOIT2, SLTYP, JACOBF, QG, TEMP2, and LAI.  Restored
!                        GRIDBDY2D file to output.  Modified code so that arrays
!                        are made available in output only if user options in
!                        MM5 generate those data.  (T. Otte and D. Schwede)
!           29 Nov 2004  Added PURB.  (T. Otte)
!           25 Feb 2005  Eliminated GRIDCRO3D for hydrostatic runs.  (T. Otte)
!           19 Aug 2005  Moved VD_C from a pointer to MC2 to an individual
!                        array that is allocated in ALLOC_DEPV.  (T. Otte)
!           14 Jul 2006  Updated condition for GRID_CRO_3D fields to reflect
!                        new vertical structure indicator for WRF.  Added
!                        new output field LWMASK.  (T. Otte)
!           25 Jul 2007  Removed TEM1P5 and TEMP10.  Write TEMP2 regardless
!                        of whether or not it is part of input meteorology.
!                        Added VEG to output, made LAI a general output field,
!                        and added WR to output to support inline dry
!                        deposition velocity calculations in CCTM.  Added
!                        fractional land use.  Removed RBNDYI and JACOBS.
!                        (T. Otte)
!           21 Apr 2008  Replaced NBNDYD with NBNDY in allocation of boundary
!                        arrays.  Added 2-m mixing ratio (Q2_C) and turbulent
!                        kinetic energy (TKE_C and TKE_B) arrays.  (T. Otte)
!           29 Oct 2009  Added potential vorticity (PVC_C and PVC_B).  Added
!                        latitude, longitude, and map-scale factors squared
!                        on U and V faces to GRIDDOT2D.  Allow output variable
!                        PURB to be created with urban model in WRF.  Remove
!                        vertical velocity (predicted by the meteorological
!                        model; WWIND) from output by default; retain user
!                        option to output 3D field.  Add user option to output
!                        u- and v-component winds (UWINDC and VWINDC) on
!                        C-staggered grid.  (T. Otte)
!           14 Dec 2010  Added sea ice.  (T. Otte)
!           11 Aug 2011  Replaced module PARMS3 with I/O API module M3UTILIO.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           10 Apr 2015  Added new arrays CFRAC3D_C and CFRAC3D_B to pass 3D
!                        resolved cloud fraction to output.  (T. Spero)
!           20 Aug 2015  Changed latent heat flux from QFX to LH.  (T. Spero)
!           16 Mar 2018  Added SNOWH to METCRO2D output.  Added 3d soil, land
!                        use and mosaic arrays.  (T. Spero)
!           27 Jun 2018  Consolidated output variables into a single module
!                        CTMVARS.  Allocate data structures.  (T. Spero)
!           14 Sep 2018  Removed support for MM5v3 input.  (T. Spero)
!           18 Dec 2018  Updated to use new data structures.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Changed variable
!                        LUVCOUT to LUVBOUT to reflect that the default 3D wind
!                        components are on the Arakawa-C staggered grid, and
!                        the optional additional 3D winds are on the Arakawa-B
!                        staggered grid.  Added optional variables from KF
!                        convective scheme with radiative feedbacks.  (T. Spero)
!-------------------------------------------------------------------------------

  USE coord
  USE vgrd
  USE metinfo
  USE mcipparm
  USE ctmvars

  IMPLICIT NONE

  INTEGER                      :: nn

  INTEGER                      :: nwr
  INTEGER                      :: nsoil2d
  INTEGER                      :: npxwrf41

  INTEGER                      :: ntke
  INTEGER                      :: npv
  INTEGER                      :: nwout
  INTEGER                      :: ncld3d
  INTEGER                      :: nkfrad

!-------------------------------------------------------------------------------
! Allocate grid arrays for COORD.
!-------------------------------------------------------------------------------

  ALLOCATE ( vglvs_gd  ( nlays+1 ) )
  ALLOCATE ( x3face_gd ( 0:nlays ) )

!-------------------------------------------------------------------------------
! Allocate grid arrays for VGRD.
!-------------------------------------------------------------------------------

  ALLOCATE ( x3face ( 0:nlays ) )
  ALLOCATE ( x3midl (   nlays ) )

!-------------------------------------------------------------------------------
! Time-independent 2d fields at cell centers.
!-------------------------------------------------------------------------------

  nfld2dxy = 6
  IF ( iflufrc ) THEN
    nfld2dxy = nfld2dxy + 1  ! adds PURB
  ENDIF

  ALLOCATE ( fld2dxy ( nfld2dxy ) )

  DO nn = 1, nfld2dxy
    ALLOCATE ( fld2dxy(nn)%fld(ncols,nrows) )
    ALLOCATE ( fld2dxy(nn)%bdy(nbndy)       )
  ENDDO

  g_lat     => fld2dxy( 1)
  g_lon     => fld2dxy( 2)
  g_msfx2   => fld2dxy( 3)
  g_ht      => fld2dxy( 4)
  g_dluse   => fld2dxy( 5)
  g_lwmask  => fld2dxy( 6)

  IF ( iflufrc ) THEN
    g_purb  => fld2dxy( 7)
  ENDIF

!-------------------------------------------------------------------------------
! Time-independent 2d fields at cell corners and cell faces.
!
! These arrays are all set to the dot-point dimensions to accommodate the
! false dot points in the Arakawa-C staggered grid that are output in
! Models-3 I/O API "DOT" files.  When the output is written in netCDF, the
! true dimensions of the Arakawa-C staggered fields are used.
!-------------------------------------------------------------------------------

  nfld2dxy_d = 9

  ALLOCATE ( fld2dxy_d ( nfld2dxy_d ) )

  DO nn = 1, nfld2dxy_d
    ALLOCATE ( fld2dxy_d(nn)%fld(ncols+1,nrows+1) )
  ENDDO

  g_latd   => fld2dxy_d( 1)
  g_lond   => fld2dxy_d( 2)
  g_msfd2  => fld2dxy_d( 3)
  g_latu   => fld2dxy_d( 4)
  g_lonu   => fld2dxy_d( 5)
  g_msfu2  => fld2dxy_d( 6)
  g_latv   => fld2dxy_d( 7)
  g_lonv   => fld2dxy_d( 8)
  g_msfv2  => fld2dxy_d( 9)

!-------------------------------------------------------------------------------
! Time-independent 3d fields (fractional land use) at cell centers.
!-------------------------------------------------------------------------------

  IF ( iflufrc ) THEN

    nfld3dxyl = 1
    ALLOCATE ( fld3dxyl ( nfld3dxyl ) )

    DO nn = 1, nfld3dxyl
      ALLOCATE ( fld3dxyl(nn)%fld(ncols,nrows,nummetlu) )
    ENDDO

    g_lufrac  => fld3dxyl( 1)

  ELSE

    nfld3dxyl = 0

  ENDIF

!-------------------------------------------------------------------------------
! Time-varying 2d fields at cell centers.
!-------------------------------------------------------------------------------

  IF ( ifwr ) THEN
    nwr = 1  ! WR
  ELSE
    nwr = 0
  ENDIF

  IF ( ifsoil ) THEN
    nsoil2d = 5  ! SOIM1, SOIM2, SOIT1, SOIT2, SLTYP
  ELSE
    nsoil2d = 0
  ENDIF

  IF ( ifpxwrf41 ) THEN
    npxwrf41 = 6  ! WSAT_PX, WFC_PX, WWLT_PX, CSAND_PX, FMSAND_PX, CLAY_PX
  ELSE
    npxwrf41 = 0
  ENDIF

  nfld2dxyt = 29 + nwr + nsoil2d + npxwrf41

  ALLOCATE ( fld2dxyt ( nfld2dxyt ) )

  DO nn = 1, nfld2dxyt
    ALLOCATE ( fld2dxyt(nn)%fld(ncols,nrows) )
  ENDDO

  c_prsfc    => fld2dxyt( 1)
  c_ustar    => fld2dxyt( 2)
  c_wstar    => fld2dxyt( 3)
  c_pbl      => fld2dxyt( 4)
  c_zruf     => fld2dxyt( 5)
  c_moli     => fld2dxyt( 6)
  c_hfx      => fld2dxyt( 7)
  c_lh       => fld2dxyt( 8)
  c_radyni   => fld2dxyt( 9)
  c_rstomi   => fld2dxyt(10)
  c_tempg    => fld2dxyt(11)
  c_temp2    => fld2dxyt(12)
  c_q2       => fld2dxyt(13)
  c_wspd10   => fld2dxyt(14)
  c_wdir10   => fld2dxyt(15)
  c_glw      => fld2dxyt(16)
  c_gsw      => fld2dxyt(17)
  c_rgrnd    => fld2dxyt(18)
  c_rn       => fld2dxyt(19)
  c_rc       => fld2dxyt(20)
  c_cfrac    => fld2dxyt(21)
  c_cldt     => fld2dxyt(22)
  c_cldb     => fld2dxyt(23)
  c_wbar     => fld2dxyt(24)
  c_snocov   => fld2dxyt(25)
  c_veg      => fld2dxyt(26)
  c_lai      => fld2dxyt(27)
  c_seaice   => fld2dxyt(28)
  c_snowh    => fld2dxyt(29)

  IF ( ifwr ) THEN
    c_wr => fld2dxyt(29+nwr)
  ENDIF

  IF ( ifsoil ) THEN
    c_soim1 => fld2dxyt(29+nwr+1)
    c_soim2 => fld2dxyt(29+nwr+2)
    c_soit1 => fld2dxyt(29+nwr+3)
    c_soit2 => fld2dxyt(29+nwr+4)
    c_sltyp => fld2dxyt(29+nwr+5)
  ENDIF

  IF ( ifpxwrf41 ) THEN
    c_wsat_px   => fld2dxyt(29+nwr+nsoil2d+1)
    c_wfc_px    => fld2dxyt(29+nwr+nsoil2d+2)
    c_wwlt_px   => fld2dxyt(29+nwr+nsoil2d+3)
    c_csand_px  => fld2dxyt(29+nwr+nsoil2d+4)
    c_fmsand_px => fld2dxyt(29+nwr+nsoil2d+5)
    c_clay_px   => fld2dxyt(29+nwr+nsoil2d+6)
  ENDIF

!-------------------------------------------------------------------------------
! Time-varying 3d fields at cell centers.
!-------------------------------------------------------------------------------

  IF ( iftke ) THEN
    ntke = 1  ! TKE
  ELSE
    ntke = 0
  ENDIF

  IF ( lpv > 0 ) THEN
    npv = 1  ! PV
  ELSE
    npv = 0
  ENDIF

  IF ( lwout > 0 ) THEN
    nwout = 1  ! WWIND
  ELSE
    nwout = 0
  ENDIF

  IF ( ifcld3d ) THEN
    ncld3d = 1  ! CFRAC_3D
  ELSE
    ncld3d = 0
  ENDIF

  nfld3dxyzt = 10 + ntke + npv + nwout + ncld3d

  ALLOCATE ( fld3dxyzt ( nfld3dxyzt ) )

  DO nn = 1, nfld3dxyzt
    ALLOCATE ( fld3dxyzt(nn)%fld(ncols,nrows,nlays) )
    ALLOCATE ( fld3dxyzt(nn)%bdy(nbndy,nlays) )
  ENDDO

  c_jacobf   => fld3dxyzt( 1)
  c_jacobm   => fld3dxyzt( 2)
  c_densa_j  => fld3dxyzt( 3)
  c_what_jd  => fld3dxyzt( 4)
  c_ta       => fld3dxyzt( 5)
  c_qv       => fld3dxyzt( 6)
  c_pres     => fld3dxyzt( 7)
  c_dens     => fld3dxyzt( 8)
  c_zh       => fld3dxyzt( 9)
  c_zf       => fld3dxyzt(10)

  IF ( iftke ) THEN
    c_tke => fld3dxyzt(10+ntke)
  ENDIF

  IF ( lpv > 0 ) THEN
    c_pv => fld3dxyzt(10+ntke+npv)
  ENDIF

  IF ( lwout > 0 ) THEN
    c_wwind => fld3dxyzt(10+ntke+npv+nwout)
  ENDIF

  IF ( ifcld3d ) THEN
    c_cfrac_3d => fld3dxyzt(10+ntke+npv+nwout+ncld3d)
  ENDIF
    

  IF ( ifkfradextras ) THEN
    nkfrad = 4
  ELSE
    nkfrad = 0
  ENDIF

  nfld3dxyzt_q = nqspecies + nkfrad

  IF ( nqspecies > 0 ) THEN

    ALLOCATE ( fld3dxyzt_q ( nfld3dxyzt_q ) )

    DO nn = 1, nfld3dxyzt_q
      ALLOCATE ( fld3dxyzt_q(nn)%fld(ncols,nrows,nlays) )
      ALLOCATE ( fld3dxyzt_q(nn)%bdy(nbndy,nlays) )
    ENDDO

    c_qc => fld3dxyzt_q(1)
    c_qr => fld3dxyzt_q(2)

    IF ( nqspecies >= 4 ) THEN
      c_qi => fld3dxyzt_q(3)
      c_qs => fld3dxyzt_q(4)
      IF ( nqspecies >= 5 ) THEN
        c_qg => fld3dxyzt_q(5)
      ENDIF
    ENDIF

    IF ( ifkfradextras ) THEN
      c_qc_cu     => fld3dxyzt_q(nqspecies+1)
      c_qi_cu     => fld3dxyzt_q(nqspecies+2)
      c_cldfra_dp => fld3dxyzt_q(nqspecies+3)
      c_cldfra_sh => fld3dxyzt_q(nqspecies+4)
    ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Time-varying 3d fields at cell corners and cell faces.
!
! These arrays are all set to the dot-point dimensions to accommodate the
! false dot points in the Arakawa-C staggered grid that are output in
! Models-3 I/O API "DOT" files.  When the output is written in netCDF, the
! true dimensions of the Arakawa-C staggered fields are used.
!-------------------------------------------------------------------------------

  IF ( luvbout > 0 ) THEN
    nfld3dxyzt_d = 6
  ELSE
    nfld3dxyzt_d = 4
  ENDIF

  ALLOCATE ( fld3dxyzt_d ( nfld3dxyzt_d ) )

  DO nn = 1, nfld3dxyzt_d
    ALLOCATE ( fld3dxyzt_d(nn)%fld(ncols+1,nrows+1,nlays) )
  ENDDO

  c_uwindc  => fld3dxyzt_d( 1)
  c_vwindc  => fld3dxyzt_d( 2)
  c_uhat_jd => fld3dxyzt_d( 3)
  c_vhat_jd => fld3dxyzt_d( 4)

  IF ( luvbout > 0 ) THEN
    c_uwind   => fld3dxyzt_d( 5)
    c_vwind   => fld3dxyzt_d( 6)
  ENDIF

!-------------------------------------------------------------------------------
! Time-varying 3d fields (soil layers) at cell centers.
!-------------------------------------------------------------------------------

  IF ( ifsoil ) THEN

    nfld3dxyst = 2

    ALLOCATE ( fld3dxyst ( nfld3dxyst ) )

    DO nn = 1, nfld3dxyst
      ALLOCATE ( fld3dxyst(nn)%fld(ncols,nrows,metsoi) )
    ENDDO

    c_soit3d   => fld3dxyst( 1)
    c_soim3d   => fld3dxyst( 2)

  ELSE

    nfld3dxyst = 0

  ENDIF

!-------------------------------------------------------------------------------
! Time-varying 3d fields (mosaic land use categories) at cell centers.
!-------------------------------------------------------------------------------

  IF ( ifmosaic ) THEN

    nfld3dxymt = 7

    ALLOCATE ( fld3dxymt ( nfld3dxymt ) )

    DO nn = 1, nfld3dxymt
      ALLOCATE ( fld3dxymt(nn)%fld(ncols,nrows,nummosaic) )
    ENDDO

    c_lufrac2  => fld3dxymt( 1)
    c_moscat   => fld3dxymt( 2)
    c_lai_mos  => fld3dxymt( 3)
    c_rai_mos  => fld3dxymt( 4)
    c_rsi_mos  => fld3dxymt( 5)
    c_tsk_mos  => fld3dxymt( 6)
    c_znt_mos  => fld3dxymt( 7)

  ELSE

    nfld3dxymt = 0

  ENDIF

END SUBROUTINE alloc_ctm
