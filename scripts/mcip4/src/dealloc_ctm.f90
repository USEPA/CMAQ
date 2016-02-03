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
!-------------------------------------------------------------------------------

  USE coord
  USE vgrd
  USE groutcom
  USE mcoutcom
  USE mdoutcom

  IMPLICIT NONE

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
! Release memory for GROUTCOM arrays.
!-------------------------------------------------------------------------------

  NULLIFY    ( glat_d     )
  NULLIFY    ( glon_d     ) 
  NULLIFY    ( gmsfsq_d   )
  NULLIFY    ( glatu_d    )
  NULLIFY    ( glonu_d    ) 
  NULLIFY    ( gmsfusq_d  )
  NULLIFY    ( glatv_d    )
  NULLIFY    ( glonv_d    ) 
  NULLIFY    ( gmsfvsq_d  )
  DEALLOCATE ( gd2        )

  NULLIFY    ( glat_c     )
  NULLIFY    ( glon_c     )
  NULLIFY    ( gmsfsq_c   )
  NULLIFY    ( gtopo_c    )
  NULLIFY    ( gdluse_c   )
  NULLIFY    ( glwmask_c  )
  IF ( ASSOCIATED ( gpurb_c   ) ) NULLIFY    ( gpurb_c    )
  IF ( ASSOCIATED ( glufrac_c ) ) NULLIFY    ( glufrac_c  )
  DEALLOCATE ( gc2        )

  NULLIFY    ( glat_b     )
  NULLIFY    ( glon_b     )
  NULLIFY    ( gmsfsq_b   )
  NULLIFY    ( gtopo_b    )
  NULLIFY    ( gdluse_b   )
  NULLIFY    ( glwmask_b  )
  IF ( ASSOCIATED ( gpurb_b   ) ) NULLIFY    ( gpurb_b    )
  IF ( ASSOCIATED ( glufrac_b ) ) NULLIFY    ( glufrac_b  )
  DEALLOCATE ( gb2        )

  IF ( ASSOCIATED ( gx3htf_c  ) ) NULLIFY    ( gx3htf_c   )
  IF ( ASSOCIATED ( gx3htm_c  ) ) NULLIFY    ( gx3htm_c   )
  IF ( ALLOCATED  ( gc3       ) ) DEALLOCATE ( gc3        )

!-------------------------------------------------------------------------------
! Release memory for MCOUTCOM arrays.
!-------------------------------------------------------------------------------

  NULLIFY    ( prsfc_c    )
  NULLIFY    ( ustar_c    )
  NULLIFY    ( wstar_c    )
  NULLIFY    ( pbl_c      )
  NULLIFY    ( zzero_c    )
  NULLIFY    ( moli_c     )
  NULLIFY    ( hfx_c      )
  NULLIFY    ( qfx_c      )
  NULLIFY    ( radyni_c   )
  NULLIFY    ( rstomi_c   )
  NULLIFY    ( tempg_c    )
  NULLIFY    ( temp2_c    )
  NULLIFY    ( q2_c       )
  NULLIFY    ( wspd10_c   )
  NULLIFY    ( wdir10_c   )
  NULLIFY    ( glw_c      )
  NULLIFY    ( gsw_c      )
  NULLIFY    ( rgrnd_c    )
  NULLIFY    ( rainn_c    )
  NULLIFY    ( rainc_c    )
  NULLIFY    ( cfract_c   )
  NULLIFY    ( cldtop_c   )
  NULLIFY    ( cldbot_c   )
  NULLIFY    ( wbar_c     )
  NULLIFY    ( snocov_c   )
  NULLIFY    ( veg_c      )
  NULLIFY    ( lai_c      )
  NULLIFY    ( seaice_c   )
  DEALLOCATE ( mc2        )

  IF ( ALLOCATED  ( wr_c      ) ) DEALLOCATE ( wr_c       )
  IF ( ALLOCATED  ( soim1_c   ) ) DEALLOCATE ( soim1_c    )
  IF ( ALLOCATED  ( soim2_c   ) ) DEALLOCATE ( soim2_c    )
  IF ( ALLOCATED  ( soit1_c   ) ) DEALLOCATE ( soit1_c    )
  IF ( ALLOCATED  ( soit2_c   ) ) DEALLOCATE ( soit2_c    )
  IF ( ALLOCATED  ( sltyp_c   ) ) DEALLOCATE ( sltyp_c    )

  NULLIFY    ( jacobf_c   )
  NULLIFY    ( jacobm_c   )
  NULLIFY    ( densa_j_c  )
  NULLIFY    ( what_jd_c  )
  NULLIFY    ( tempa_c    )
  NULLIFY    ( wvapor_c   )
  NULLIFY    ( press_c    )
  NULLIFY    ( densa_c    )
  NULLIFY    ( x3htm_c    )
  NULLIFY    ( x3htf_c    )
  DEALLOCATE ( mc3        )

  NULLIFY    ( jacobf_b   )
  NULLIFY    ( jacobm_b   )
  NULLIFY    ( densa_j_b  )
  NULLIFY    ( what_jd_b  )
  NULLIFY    ( tempa_b    )
  NULLIFY    ( wvapor_b   )
  NULLIFY    ( press_b    )
  NULLIFY    ( densa_b    )
  NULLIFY    ( x3htm_b    )
  NULLIFY    ( x3htf_b    )
  DEALLOCATE ( mb3        )

  IF ( ASSOCIATED ( cldwtr_c ) ) NULLIFY    ( cldwtr_c   )
  IF ( ASSOCIATED ( ranwtr_c ) ) NULLIFY    ( ranwtr_c   )
  IF ( ASSOCIATED ( qice_c   ) ) NULLIFY    ( qice_c     )
  IF ( ASSOCIATED ( qsnow_c  ) ) NULLIFY    ( qsnow_c    )
  IF ( ASSOCIATED ( qgraup_c ) ) NULLIFY    ( qgraup_c   )
  IF ( ALLOCATED  ( qc3      ) ) DEALLOCATE ( qc3        )

  IF ( ASSOCIATED ( cldwtr_b ) ) NULLIFY    ( cldwtr_b   )
  IF ( ASSOCIATED ( ranwtr_b ) ) NULLIFY    ( ranwtr_b   )
  IF ( ASSOCIATED ( qice_b   ) ) NULLIFY    ( qice_b     )
  IF ( ASSOCIATED ( qsnow_b  ) ) NULLIFY    ( qsnow_b    )
  IF ( ASSOCIATED ( qgraup_b ) ) NULLIFY    ( qgraup_b   )
  IF ( ALLOCATED  ( qb3      ) ) DEALLOCATE ( qb3        )

  IF ( ALLOCATED  ( tke_c    ) ) DEALLOCATE ( tke_c      )
  IF ( ALLOCATED  ( tke_b    ) ) DEALLOCATE ( tke_b      )

  IF ( ALLOCATED  ( pvc_c    ) ) DEALLOCATE ( pvc_c      )
  IF ( ALLOCATED  ( pvc_b    ) ) DEALLOCATE ( pvc_b      )

  IF ( ALLOCATED  ( wwind_c  ) ) DEALLOCATE ( wwind_c    )
  IF ( ALLOCATED  ( wwind_b  ) ) DEALLOCATE ( wwind_b    )

!-------------------------------------------------------------------------------
! Release memory for MDOUTCOM arrays.
!-------------------------------------------------------------------------------

  NULLIFY    ( uu_d       )
  NULLIFY    ( vv_d       )
  NULLIFY    ( uhat_s     )
  NULLIFY    ( vhat_t     )
  DEALLOCATE ( md3        )

  IF ( ALLOCATED  ( uu_s     ) ) DEALLOCATE ( uu_s       )
  IF ( ALLOCATED  ( vv_t     ) ) DEALLOCATE ( vv_t       )

END SUBROUTINE dealloc_ctm
