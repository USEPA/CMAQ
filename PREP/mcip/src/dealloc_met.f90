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

SUBROUTINE dealloc_met

!-------------------------------------------------------------------------------
! Name:     Deallocate Meteorology Variables
! Purpose:  Deallocate arrays for input meteorology variables.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added SNOWCOVR.  (D. Schwede)
!           09 Aug 2004  Added QGA, VEGOLD, and T2.  (D. Schwede and T. Otte)
!           29 Nov 2004  Added LUFRAC.  (T. Otte)
!           04 Apr 2005  Removed unused variables REGIME and MAVAIL.  Added PH,
!                        PHB, PB, MU, and MUB for WRF.  Added U10 and V10.
!                        (T. Otte and S.-B. Kim)
!           11 Aug 2005  Removed unused variable FSOIL.  (T. Otte)
!           25 Jul 2007  Removed internal variables for emissivity and net
!                        radiation.  Eliminated logical variable "PX" to make
!                        code more general.  (T. Otte)
!           05 May 2008  Added 2-m mixing ratio (Q2) and turbulent kinetic
!                        energy (TKE) arrays.  Added urban fraction (FRC_URB)
!                        and urban roughness length (Z0C_URB2D) for
!                        MET_UCMCALL=1.  (T. Otte)
!           29 Sep 2009  Added THETA and CORIOLIS for when potential vorticity
!                        is needed.  Added LATU, LONU, MAPU, LATV, LONV, and
!                        MAPV.  Removed Z0C_URB2D.  (T. Otte)
!           15 Dec 2010  Added sea ice.  Added tipping buckets for convective
!                        and non-convective precipitation.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           11 Sep 2012  Added LANDMASK to be read from WRF.  (T. Otte)
!           10 Apr 2015  Added new array CLDFRA to pass 3D resolved cloud
!                        fraction to output.  (T. Spero)
!           21 Aug 2015  Changed latent heat flux from QFX to LH.  Added
!                        moisture flux (QFX).  (T. Spero)
!           16 Mar 2018  Added SNOWH to output.  Added C1H, C2H, C1F, and C2F to
!                        support hybrid vertical coordinate in WRF.  Added
!                        LUFRAC2, MOSCATIDX, ZNT_MOS, TSK_MOS, RA_MOS, RS_MOS,
!                        and LAI_MOS for NOAH Mosaic land-surface model.
!                        Added DZS, SOIT3D, and SOIM3D.  Added WSPDSFC and
!                        XLAIDYN for Noah.  (T. Spero)
!           14 Sep 2018  Removed support for MM5v3 input.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Added optional
!                        variables from KF convective scheme with radiative
!                        feedbacks.  (T. Spero)
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars
  USE mcipparm

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Deallocate time-invariant fields.
!-------------------------------------------------------------------------------

  DEALLOCATE ( albedo   )  ! <--- time-variant with Pleim-Xiu and NOAH LSMs
  DEALLOCATE ( landmask )  ! <--- time-variant with NOAH LSM
  DEALLOCATE ( landuse  )
  DEALLOCATE ( latcrs   )
  DEALLOCATE ( latdot   )
  DEALLOCATE ( latu     )
  DEALLOCATE ( latv     )
  DEALLOCATE ( loncrs   )
  DEALLOCATE ( londot   )
  DEALLOCATE ( lonu     )
  DEALLOCATE ( lonv     )
  DEALLOCATE ( mapcrs   )
  DEALLOCATE ( mapdot   )
  DEALLOCATE ( sigmaf   )
  DEALLOCATE ( sigmah   )
  DEALLOCATE ( terrain  )
  DEALLOCATE ( znt      )

  IF ( ALLOCATED ( lufrac    ) )  DEALLOCATE ( lufrac    )
  IF ( ALLOCATED ( lufrac2   ) )  DEALLOCATE ( lufrac2   )
  IF ( ALLOCATED ( moscatidx ) )  DEALLOCATE ( moscatidx )
  IF ( ALLOCATED ( coriolis  ) )  DEALLOCATE ( coriolis  )

  IF ( ALLOCATED ( c1f       ) )  DEALLOCATE ( c1f       )
  IF ( ALLOCATED ( c1h       ) )  DEALLOCATE ( c1h       )
  IF ( ALLOCATED ( c2f       ) )  DEALLOCATE ( c2f       )
  IF ( ALLOCATED ( c2h       ) )  DEALLOCATE ( c2h       )

  IF ( ALLOCATED ( dzs       ) )  DEALLOCATE ( dzs       )

!-------------------------------------------------------------------------------
! Deallocate time-variant fields.
!-------------------------------------------------------------------------------

  DEALLOCATE ( glw     )
  DEALLOCATE ( groundt )
  DEALLOCATE ( hfx     )
  DEALLOCATE ( i_rainc )
  DEALLOCATE ( i_rainnc)
  DEALLOCATE ( ircold  )
  DEALLOCATE ( irnold  )
  DEALLOCATE ( lh      )
  DEALLOCATE ( pp      )
  DEALLOCATE ( psa     )
  DEALLOCATE ( qca     )
  DEALLOCATE ( qga     )
  DEALLOCATE ( qia     )
  DEALLOCATE ( qra     )
  DEALLOCATE ( qsa     )
  DEALLOCATE ( qva     )
  DEALLOCATE ( raincon )
  DEALLOCATE ( rainnon )
  DEALLOCATE ( rcold   )
  DEALLOCATE ( rgrnd   )
  DEALLOCATE ( rnold   )
  DEALLOCATE ( seaice  )
  DEALLOCATE ( snowcovr)
  DEALLOCATE ( snowh   )
  DEALLOCATE ( ta      )
  DEALLOCATE ( ua      )
  DEALLOCATE ( ust     )
  DEALLOCATE ( va      )
  DEALLOCATE ( wa      )
  DEALLOCATE ( zpbl    )

  IF ( ALLOCATED ( t2     ) )  DEALLOCATE ( t2    )
  IF ( ALLOCATED ( q2     ) )  DEALLOCATE ( q2    )
  IF ( ALLOCATED ( u10    ) )  DEALLOCATE ( u10   )
  IF ( ALLOCATED ( v10    ) )  DEALLOCATE ( v10   )
  IF ( ALLOCATED ( qfx    ) )  DEALLOCATE ( qfx   )

  IF ( met_model == 2 ) THEN  ! WRF
    DEALLOCATE ( mu    )
    DEALLOCATE ( mub   )
    DEALLOCATE ( pb    )
    DEALLOCATE ( ph    )
    DEALLOCATE ( phb   )
  ENDIF

  IF ( ALLOCATED ( isltyp    ) )  DEALLOCATE ( isltyp    )
  IF ( ALLOCATED ( lai       ) )  DEALLOCATE ( lai       )
  IF ( ALLOCATED ( mol       ) )  DEALLOCATE ( mol       )
  IF ( ALLOCATED ( ra        ) )  DEALLOCATE ( ra        )
  IF ( ALLOCATED ( rstom     ) )  DEALLOCATE ( rstom     )
  IF ( ALLOCATED ( soilt1    ) )  DEALLOCATE ( soilt1    )
  IF ( ALLOCATED ( soilt2    ) )  DEALLOCATE ( soilt2    )
  IF ( ALLOCATED ( soim3d    ) )  DEALLOCATE ( soim3d    )
  IF ( ALLOCATED ( soit3d    ) )  DEALLOCATE ( soit3d    )
  IF ( ALLOCATED ( veg       ) )  DEALLOCATE ( veg       )
  IF ( ALLOCATED ( w2        ) )  DEALLOCATE ( w2        )
  IF ( ALLOCATED ( wg        ) )  DEALLOCATE ( wg        )
  IF ( ALLOCATED ( wr        ) )  DEALLOCATE ( wr        )

  IF ( ALLOCATED ( tke       ) )  DEALLOCATE ( tke       )

  IF ( ALLOCATED ( theta     ) )  DEALLOCATE ( theta     )

  IF ( ALLOCATED ( frc_urb   ) )  DEALLOCATE ( frc_urb   )

  IF ( ALLOCATED ( cldfra    ) )  DEALLOCATE ( cldfra    )

  IF ( ALLOCATED ( lai_mos   ) )  DEALLOCATE ( lai_mos   )
  IF ( ALLOCATED ( ra_mos    ) )  DEALLOCATE ( ra_mos    )
  IF ( ALLOCATED ( rs_mos    ) )  DEALLOCATE ( rs_mos    )
  IF ( ALLOCATED ( tsk_mos   ) )  DEALLOCATE ( tsk_mos   )
  IF ( ALLOCATED ( znt_mos   ) )  DEALLOCATE ( znt_mos   )

  IF ( ALLOCATED ( wspdsfc   ) )  DEALLOCATE ( wspdsfc   )
  IF ( ALLOCATED ( xlaidyn   ) )  DEALLOCATE ( xlaidyn   )

  IF ( ALLOCATED ( lai_px    ) )  DEALLOCATE ( lai_px    )
  IF ( ALLOCATED ( wsat_px   ) )  DEALLOCATE ( wsat_px   )
  IF ( ALLOCATED ( wwlt_px   ) )  DEALLOCATE ( wwlt_px   )
  IF ( ALLOCATED ( wfc_px    ) )  DEALLOCATE ( wfc_px    )
  IF ( ALLOCATED ( csand_px  ) )  DEALLOCATE ( csand_px  )
  IF ( ALLOCATED ( fmsand_px ) )  DEALLOCATE ( fmsand_px )
  IF ( ALLOCATED ( clay_px   ) )  DEALLOCATE ( clay_px   )

  IF ( ALLOCATED ( qc_cu     ) )  DEALLOCATE ( qc_cu     )
  IF ( ALLOCATED ( qi_cu     ) )  DEALLOCATE ( qi_cu     )
  IF ( ALLOCATED ( cldfra_dp ) )  DEALLOCATE ( cldfra_dp )
  IF ( ALLOCATED ( cldfra_sh ) )  DEALLOCATE ( cldfra_sh )

END SUBROUTINE dealloc_met
