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

SUBROUTINE dealloc_ctm

!-------------------------------------------------------------------------------
! Name:     Deallocate Arrays for CTM Dimensions
! Purpose:  Deallocate arrays based on CTM grid.
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
!           01 Dec 2004  Added PURB.  (T. Otte)
!           25 Feb 2005  Eliminated GRIDCRO3D for hydrostatic runs.  (T. Otte)
!           19 Aug 2005  Moved VD_C from a pointer to MC2 to an individual
!                        array that is deallocated in DEALLOC_DEPV.  (T. Otte)
!           14 Jul 2006  Added new output fields LWMASK.  (T. Otte)
!           25 Jul 2007  Removed 1.5-m and 10-m temperature arrays.  Use 2-m
!                        temperature regardless of whether or not it is in
!                        input meteorology.  Added VEG to output, made LAI a
!                        general output variable, and added WR to output to
!                        support inline dry deposition velocity calculations
!                        in CCTM.  Added fractional land use.  Removed RBNDYI
!                        and JACOBS.  (T. Otte)
!           21 Apr 2008  Added 2-m mixing ratio (Q2_C) and turbulent kinetic
!                        energy (TKE_C and TKE_B) arrays.  (T. Otte)
!           23 Sep 2009  Added potential vorticity (PVC_C and PVC_B).  Added
!                        latitude, longitude, and map-scale factors squared
!                        on U and V faces to GRIDDOT2D.  Remove vertical
!                        velocity (predicted by the meteorological model; WWIND)
!                        from output by default; retain user option to output
!                        3D field.  Add user option to output u- and v-
!                        component winds (UWINDC and VWINDC) on C-staggered
!                        grid.  (T. Otte)
!           14 Dec 2010  Added sea ice.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           10 Apr 2015  Added new arrays CFRAC3D_C and CFRAC3D_B to pass 3D
!                        resolved cloud fraction to output.  (T. Spero)
!           20 Aug 2015  Changed latent heat flux from QFX to LH.  (T. Spero)
!           16 Mar 2018  Added SNOWH to output.  Added mosaic, land use, and
!                        soil arrays.  (T. Spero)
!           27 Jun 2018  Consolidated output arrays into a single module
!                        CTMVARS.  Deallocate data structures.  (T. Spero)
!           14 Sep 2018  Removed support for MM5v3 input.  (T. Spero)
!           13 Dec 2018  Updated to use new data structures.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Added optional
!                        variables from KF convective scheme with radiative
!                        feedbacks.  (T. Spero)
!-------------------------------------------------------------------------------

  USE coord
  USE vgrd
  USE ctmvars
  USE mcipparm

  IMPLICIT NONE

  INTEGER                      :: nn

!-------------------------------------------------------------------------------
! Deallocate grid arrays for COORD.
!-------------------------------------------------------------------------------

  DEALLOCATE ( vglvs_gd   )
  DEALLOCATE ( x3face_gd  )

!-------------------------------------------------------------------------------
! Deallocate grid arrays for VGRD.
!-------------------------------------------------------------------------------

  DEALLOCATE ( x3face     )
  DEALLOCATE ( x3midl     )

!-------------------------------------------------------------------------------
! Release memory for time-independent 2d fields at cell centers.
!-------------------------------------------------------------------------------

  NULLIFY    ( g_lat    )
  NULLIFY    ( g_lon    )
  NULLIFY    ( g_msfx2  )
  NULLIFY    ( g_ht     )
  NULLIFY    ( g_dluse  )
  NULLIFY    ( g_lwmask )
  IF ( ASSOCIATED ( g_purb ) ) NULLIFY ( g_purb   )
  
  DO nn = 1, nfld2dxy
    DEALLOCATE ( fld2dxy(nn)%fld )
    DEALLOCATE ( fld2dxy(nn)%bdy )
  ENDDO

!-------------------------------------------------------------------------------
! Release memory for time-independent 2d fields at cell corners and faces.
!-------------------------------------------------------------------------------

  NULLIFY    ( g_latd  )
  NULLIFY    ( g_lond  ) 
  NULLIFY    ( g_msfv2 )
  NULLIFY    ( g_latu  )
  NULLIFY    ( g_lonu  ) 
  NULLIFY    ( g_msfv2 )
  NULLIFY    ( g_latv  )
  NULLIFY    ( g_lonv  ) 
  NULLIFY    ( g_msfv2 )

  DO nn = 1, nfld2dxy_d
    DEALLOCATE ( fld2dxy_d(nn)%fld )
  ENDDO

!-------------------------------------------------------------------------------
! Release memory for time-independent 3d fields (frac land use) at cell centers.
!-------------------------------------------------------------------------------

  IF ( ASSOCIATED ( g_lufrac ) ) NULLIFY ( g_lufrac )

  IF ( ALLOCATED ( fld3dxyl ) ) THEN
    DO nn = 1, nfld3dxyl
      DEALLOCATE ( fld3dxyl(nn)%fld )
    ENDDO
  ENDIF

!-------------------------------------------------------------------------------
! Release memory for time-varying 2d fields at cell centers.
!-------------------------------------------------------------------------------

  NULLIFY    ( c_prsfc  )
  NULLIFY    ( c_ustar  )
  NULLIFY    ( c_wstar  )
  NULLIFY    ( c_pbl    )
  NULLIFY    ( c_zruf   )
  NULLIFY    ( c_moli   )
  NULLIFY    ( c_hfx    )
  NULLIFY    ( c_lh     )
  NULLIFY    ( c_radyni )
  NULLIFY    ( c_rstomi )
  NULLIFY    ( c_tempg  )
  NULLIFY    ( c_temp2  )
  NULLIFY    ( c_q2     )
  NULLIFY    ( c_wspd10 )
  NULLIFY    ( c_wdir10 )
  NULLIFY    ( c_glw    )
  NULLIFY    ( c_gsw    )
  NULLIFY    ( c_rgrnd  )
  NULLIFY    ( c_rn     )
  NULLIFY    ( c_rc     )
  NULLIFY    ( c_cfrac  )
  NULLIFY    ( c_cldt   )
  NULLIFY    ( c_cldb   )
  NULLIFY    ( c_wbar   )
  NULLIFY    ( c_snocov )
  NULLIFY    ( c_veg    )
  NULLIFY    ( c_lai    )
  NULLIFY    ( c_seaice )
  NULLIFY    ( c_snowh  )
  IF ( ASSOCIATED ( c_wr        ) ) NULLIFY ( c_wr        )
  IF ( ASSOCIATED ( c_soim1     ) ) NULLIFY ( c_soim1     )
  IF ( ASSOCIATED ( c_soim2     ) ) NULLIFY ( c_soim2     )
  IF ( ASSOCIATED ( c_soit1     ) ) NULLIFY ( c_soit1     )
  IF ( ASSOCIATED ( c_soit2     ) ) NULLIFY ( c_soit2     )
  IF ( ASSOCIATED ( c_sltyp     ) ) NULLIFY ( c_sltyp     )
  IF ( ASSOCIATED ( c_wsat_px   ) ) NULLIFY ( c_wsat_px   )
  IF ( ASSOCIATED ( c_wfc_px    ) ) NULLIFY ( c_wfc_px    )
  IF ( ASSOCIATED ( c_wwlt_px   ) ) NULLIFY ( c_wwlt_px   )
  IF ( ASSOCIATED ( c_csand_px  ) ) NULLIFY ( c_csand_px  )
  IF ( ASSOCIATED ( c_fmsand_px ) ) NULLIFY ( c_fmsand_px )
  IF ( ASSOCIATED ( c_clay_px   ) ) NULLIFY ( c_clay_px   )

  DO nn = 1, nfld2dxyt
    DEALLOCATE ( fld2dxyt(nn)%fld )
  ENDDO

!-------------------------------------------------------------------------------
! Release memory for time-varying 3d fields at cell centers.
!-------------------------------------------------------------------------------

  NULLIFY    ( c_jacobf  )
  NULLIFY    ( c_jacobm  )
  NULLIFY    ( c_densa_j )
  NULLIFY    ( c_what_jd )
  NULLIFY    ( c_ta      )
  NULLIFY    ( c_qv      )
  NULLIFY    ( c_pres    )
  NULLIFY    ( c_dens    )
  NULLIFY    ( c_zh      )
  NULLIFY    ( c_zf      )
  IF ( ASSOCIATED ( c_tke      ) ) NULLIFY ( c_tke      )
  IF ( ASSOCIATED ( c_pv       ) ) NULLIFY ( c_pv       )
  IF ( ASSOCIATED ( c_wwind    ) ) NULLIFY ( c_wwind    )
  IF ( ASSOCIATED ( c_cfrac_3d ) ) NULLIFY ( c_cfrac_3d )

  DO nn = 1, nfld3dxyzt
    DEALLOCATE ( fld3dxyzt(nn)%fld )
    DEALLOCATE ( fld3dxyzt(nn)%bdy )
  ENDDO

  IF ( nfld3dxyzt_q > 0 ) THEN

    IF ( ASSOCIATED ( c_qc ) ) NULLIFY ( c_qc )
    IF ( ASSOCIATED ( c_qr ) ) NULLIFY ( c_qr )
    IF ( ASSOCIATED ( c_qi ) ) NULLIFY ( c_qi )
    IF ( ASSOCIATED ( c_qs ) ) NULLIFY ( c_qs )
    IF ( ASSOCIATED ( c_qg ) ) NULLIFY ( c_qg )
    IF ( ASSOCIATED ( c_qc_cu     ) ) NULLIFY ( c_qc_cu     )
    IF ( ASSOCIATED ( c_qi_cu     ) ) NULLIFY ( c_qi_cu     )
    IF ( ASSOCIATED ( c_cldfra_dp ) ) NULLIFY ( c_cldfra_dp )
    IF ( ASSOCIATED ( c_cldfra_sh ) ) NULLIFY ( c_cldfra_sh )

    DO nn = 1, nfld3dxyzt_q
      DEALLOCATE ( fld3dxyzt_q(nn)%fld )
      DEALLOCATE ( fld3dxyzt_q(nn)%bdy )
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Release memory for time-varying 3d fields at cell corners and cell faces.
!-------------------------------------------------------------------------------

  NULLIFY    ( c_uwind   )
  NULLIFY    ( c_vwind   )
  NULLIFY    ( c_uhat_jd )
  NULLIFY    ( c_vhat_jd )
  IF ( ASSOCIATED ( c_uwindc ) ) NULLIFY ( c_uwindc )
  IF ( ASSOCIATED ( c_vwindc ) ) NULLIFY ( c_vwindc )

  DO nn = 1, nfld3dxyzt_d
    DEALLOCATE ( fld3dxyzt_d(nn)%fld )
  ENDDO

!-------------------------------------------------------------------------------
! Release memory for time-varying 3d fields (soil layers) at cell centers.
!-------------------------------------------------------------------------------

  IF ( nfld3dxyst > 0 ) THEN

    NULLIFY    ( c_soit3d )
    NULLIFY    ( c_soim3d )

    DO nn = 1, nfld3dxyst
      DEALLOCATE ( fld3dxyst(nn)%fld )
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Release memory for time-varying 3d fields (mosaic land use categories) at cell
! centers.
!-------------------------------------------------------------------------------

  IF ( nfld3dxymt > 0 ) THEN

    NULLIFY    ( c_lufrac2 )
    NULLIFY    ( c_moscat  )
    NULLIFY    ( c_lai_mos )
    NULLIFY    ( c_rai_mos )
    NULLIFY    ( c_rsi_mos )
    NULLIFY    ( c_tsk_mos )
    NULLIFY    ( c_znt_mos )

    DO nn = 1, nfld3dxymt
      DEALLOCATE ( fld3dxymt(nn)%fld )
    ENDDO

  ENDIF

END SUBROUTINE dealloc_ctm
