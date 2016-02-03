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
!-------------------------------------------------------------------------------

  USE coord
  USE vgrd
  USE metinfo
  USE mcipparm
  USE groutcom
  USE mcoutcom
  USE mdoutcom
  USE m3utilio

  IMPLICIT NONE

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
! Allocate grid arrays for GROUTCOM.
!-------------------------------------------------------------------------------

  ALLOCATE ( gd2 ( ncols+1, nrows+1, gd2index ) )
    glat_d     => gd2(:,:,1)
    glon_d     => gd2(:,:,2)
    gmsfsq_d   => gd2(:,:,3)
    glatu_d    => gd2(:,:,4)
    glonu_d    => gd2(:,:,5)
    gmsfusq_d  => gd2(:,:,6)
    glatv_d    => gd2(:,:,7)
    glonv_d    => gd2(:,:,8)
    gmsfvsq_d  => gd2(:,:,9)

  IF ( iflufrc ) THEN
    ALLOCATE ( gc2 ( ncols, nrows, gc2index+1+nummetlu ) )
  ELSE
    ALLOCATE ( gc2 ( ncols, nrows, gc2index ) )
  ENDIF
    glat_c     => gc2(:,:,1)
    glon_c     => gc2(:,:,2)
    gmsfsq_c   => gc2(:,:,3)
    gtopo_c    => gc2(:,:,4)
    gdluse_c   => gc2(:,:,5)
    glwmask_c  => gc2(:,:,6)
    IF ( iflufrc ) THEN
    gpurb_c    => gc2(:,:,7)
    glufrac_c  => gc2(:,:,8:7+nummetlu)
    ELSE IF ( met_urban_phys >= 1 ) THEN
    gpurb_c    => gc2(:,:,7)
    ENDIF

  IF ( iflufrc ) THEN
    ALLOCATE ( gb2 ( nbndy, gb2index+1+nummetlu ) )
  ELSE
    ALLOCATE ( gb2 ( nbndy, gb2index ) )
  ENDIF
    glat_b     => gb2(:,1)
    glon_b     => gb2(:,2)
    gmsfsq_b   => gb2(:,3)
    gtopo_b    => gb2(:,4)
    gdluse_b   => gb2(:,5)
    glwmask_b  => gb2(:,6)
    IF ( iflufrc ) THEN
    gpurb_b    => gb2(:,7)
    glufrac_b  => gb2(:,8:7+nummetlu)
    ENDIF

  IF ( ( vgtyp_gd /= vgsgph3 ) .AND. ( vgtyp_gd /= vgwrfem ) ) THEN  ! nonhydro
    ALLOCATE ( gc3 ( ncols, nrows, nlays, gc3index ) )
      gx3htf_c   => gc3(:,:,:,1)
      gx3htm_c   => gc3(:,:,:,2)
  ENDIF

!-------------------------------------------------------------------------------
! Allocate grid arrays for MCOUTCOM.
!-------------------------------------------------------------------------------

  ALLOCATE ( mc2 ( ncols, nrows, mc2index ) )
    prsfc_c    => mc2(:,:, 1)
    ustar_c    => mc2(:,:, 2)
    wstar_c    => mc2(:,:, 3)
    pbl_c      => mc2(:,:, 4)
    zzero_c    => mc2(:,:, 5)
    moli_c     => mc2(:,:, 6)
    hfx_c      => mc2(:,:, 7)
    qfx_c      => mc2(:,:, 8)
    radyni_c   => mc2(:,:, 9)
    rstomi_c   => mc2(:,:,10)
    tempg_c    => mc2(:,:,11)
    temp2_c    => mc2(:,:,12)
    q2_c       => mc2(:,:,13)
    wspd10_c   => mc2(:,:,14)
    wdir10_c   => mc2(:,:,15)
    glw_c      => mc2(:,:,16)
    gsw_c      => mc2(:,:,17)
    rgrnd_c    => mc2(:,:,18)
    rainn_c    => mc2(:,:,19)
    rainc_c    => mc2(:,:,20)
    cfract_c   => mc2(:,:,21)
    cldtop_c   => mc2(:,:,22)
    cldbot_c   => mc2(:,:,23)
    wbar_c     => mc2(:,:,24)
    snocov_c   => mc2(:,:,25)
    veg_c      => mc2(:,:,26)
    lai_c      => mc2(:,:,27)
    seaice_c   => mc2(:,:,28)

  IF ( ifwr ) THEN
    ALLOCATE ( wr_c ( ncols, nrows ) )
  ENDIF

  IF ( ifsoil ) THEN
    ALLOCATE ( soim1_c ( ncols, nrows ) )
    ALLOCATE ( soim2_c ( ncols, nrows ) )
    ALLOCATE ( soit1_c ( ncols, nrows ) )
    ALLOCATE ( soit2_c ( ncols, nrows ) )
    ALLOCATE ( sltyp_c ( ncols, nrows ) )
  ENDIF

  ALLOCATE ( mc3 ( ncols, nrows, nlays, mc3index ) )
    jacobf_c   => mc3(:,:,:, 1)
    jacobm_c   => mc3(:,:,:, 2)
    densa_j_c  => mc3(:,:,:, 3)
    what_jd_c  => mc3(:,:,:, 4)
    tempa_c    => mc3(:,:,:, 5)
    wvapor_c   => mc3(:,:,:, 6)
    press_c    => mc3(:,:,:, 7)
    densa_c    => mc3(:,:,:, 8)
    x3htm_c    => mc3(:,:,:, 9)
    x3htf_c    => mc3(:,:,:,10)

  ALLOCATE ( mb3 ( nbndy, nlays, mb3index ) )
    jacobf_b   => mb3(:,:, 1)
    jacobm_b   => mb3(:,:, 2)
    densa_j_b  => mb3(:,:, 3)
    what_jd_b  => mb3(:,:, 4)
    tempa_b    => mb3(:,:, 5)
    wvapor_b   => mb3(:,:, 6)
    press_b    => mb3(:,:, 7)
    densa_b    => mb3(:,:, 8)
    x3htm_b    => mb3(:,:, 9)
    x3htf_b    => mb3(:,:,10)

  IF ( nqspecies > 0 ) THEN

    ALLOCATE ( qc3 ( ncols, nrows, nlays, nqspecies ) )
      IF ( nqspecies >= 2 ) THEN
        cldwtr_c   => qc3(:,:,:, 1)
        ranwtr_c   => qc3(:,:,:, 2)
      ENDIF
      IF ( nqspecies >= 4 ) THEN
        qice_c     => qc3(:,:,:, 3)
        qsnow_c    => qc3(:,:,:, 4)
      ENDIF
      IF ( nqspecies == 5 ) THEN
        qgraup_c   => qc3(:,:,:, 5)
      ENDIF

    ALLOCATE ( qb3 ( nbndy, nlays, nqspecies ) )
      IF ( nqspecies >= 2 ) THEN
        cldwtr_b   => qb3(:,:, 1)
        ranwtr_b   => qb3(:,:, 2)
      ENDIF
      IF ( nqspecies >= 4 ) THEN
        qice_b     => qb3(:,:, 3)
        qsnow_b    => qb3(:,:, 4)
      ENDIF
      IF ( nqspecies == 5 ) THEN
        qgraup_b   => qb3(:,:, 5)
      ENDIF

  ENDIF

  IF ( iftke ) THEN
    ALLOCATE ( tke_c ( ncols, nrows, nlays ) )
    ALLOCATE ( tke_b ( nbndy, nlays ) )
  ENDIF

  IF ( lpv > 0 ) THEN
    ALLOCATE ( pvc_c ( ncols, nrows, nlays ) )
    ALLOCATE ( pvc_b ( nbndy, nlays ) )
  ENDIF

  IF ( lwout > 0 ) THEN
    ALLOCATE ( wwind_c ( ncols, nrows, nlays ) )
    ALLOCATE ( wwind_b ( nbndy, nlays ) )
  ENDIF

!-------------------------------------------------------------------------------
! Allocate grid arrays for MDOUTCOM.
!-------------------------------------------------------------------------------

  ALLOCATE ( md3 ( ncols+1, nrows+1, nlays, md3index ) )
    uu_d       => md3(:,:,:,1)
    vv_d       => md3(:,:,:,2)
    uhat_s     => md3(:,:,:,3)
    vhat_t     => md3(:,:,:,4)

  IF ( luvcout > 0 ) THEN
    ALLOCATE ( uu_s ( ncols+1, nrows+1, nlays ) )
    ALLOCATE ( vv_t ( ncols+1, nrows+1, nlays ) )
  ENDIF

END SUBROUTINE alloc_ctm
