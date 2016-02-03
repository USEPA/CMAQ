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
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars
  USE mcipparm

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Deallocate time-invariant fields.
!-------------------------------------------------------------------------------

  DEALLOCATE ( albedo   )  ! <--- time-variant with Pleim-Xiu and NOAH LSMs
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
  DEALLOCATE ( sigmah   )
  DEALLOCATE ( sigmaf   )
  DEALLOCATE ( terrain  )
  DEALLOCATE ( znt      )

  IF ( ALLOCATED ( lufrac   ) )  DEALLOCATE ( lufrac   )
  IF ( ALLOCATED ( coriolis ) )  DEALLOCATE ( coriolis )

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
  DEALLOCATE ( pp      )
  DEALLOCATE ( psa     )
  DEALLOCATE ( qca     )
  DEALLOCATE ( qfx     )
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

  IF ( met_model == 2 ) THEN  ! WRF
    DEALLOCATE ( mu    )
    DEALLOCATE ( mub   )
    DEALLOCATE ( pb    )
    DEALLOCATE ( ph    )
    DEALLOCATE ( phb   )
  ENDIF

  IF ( ALLOCATED ( isltyp ) )  DEALLOCATE ( isltyp )
  IF ( ALLOCATED ( lai    ) )  DEALLOCATE ( lai    )
  IF ( ALLOCATED ( mol    ) )  DEALLOCATE ( mol    )
  IF ( ALLOCATED ( ra     ) )  DEALLOCATE ( ra     )
  IF ( ALLOCATED ( rstom  ) )  DEALLOCATE ( rstom  )
  IF ( ALLOCATED ( soilt1 ) )  DEALLOCATE ( soilt1 )
  IF ( ALLOCATED ( soilt2 ) )  DEALLOCATE ( soilt2 )
  IF ( ALLOCATED ( veg    ) )  DEALLOCATE ( veg    )
  IF ( ALLOCATED ( vegold ) )  DEALLOCATE ( vegold )
  IF ( ALLOCATED ( w2     ) )  DEALLOCATE ( w2     )
  IF ( ALLOCATED ( wg     ) )  DEALLOCATE ( wg     )
  IF ( ALLOCATED ( wr     ) )  DEALLOCATE ( wr     )

  IF ( ALLOCATED ( tke    ) )  DEALLOCATE ( tke    )

  IF ( ALLOCATED ( theta  ) )  DEALLOCATE ( theta  )

  IF ( ALLOCATED ( frc_urb   ) ) DEALLOCATE ( frc_urb   )

END SUBROUTINE dealloc_met
