
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/MCIP2/src/mcip2/dealloc_x.F,v 1.8 2007/08/03 20:51:48 tlotte Exp $ 


SUBROUTINE dealloc_x

!-------------------------------------------------------------------------------
! Name:     Deallocate X Arrays
! Purpose:  Deallocate X arrays with MCIP transfer array dimensions.
! Revised:  19 Sep 2001  Original version.  (T. Otte)
!           27 Feb 2002  Renamed XSURF1 as XTEMP1P5 and XSURF2 as XWIND10.
!                        (T. Otte)
!           18 Mar 2003  Removed XJDRATE.  (T. Otte)
!           29 May 2003  Added XF2DEF, XSNOCOV, XDELTA, XLSTWET, and XRH.
!                        (D. Schwede, T. Otte, and J. Pleim)
!           10 Aug 2004  Added XQGRAUP, XWSPD10, XWDIR10, and XT2.  Removed
!                        XFLAGS, XINDEX, XNAMES, and XLUSNAME.  Moved remaining
!                        contents of NULL_X to this routine to simplify code.
!                        (T. Otte and D. Schwede)
!           01 Dec 2004  Added XPURB.  (T. Otte)
!           04 Apr 2005  Removed unused variables XREGIME, XRTOLD, XPRSOLD, and
!                        XDENSAM_REF.  Moved XDFLUX and XPSRATE as local
!                        variables in VERTHYD.  Added XMU and XGEOF for WRF.
!                        Changed XUU and XVV to XUU_D and XVV_D, and changed
!                        XUHAT and XVHAT to XUU_S and XVV_T.  Added pointer
!                        indices for optional chlorine and mercury species.
!                        Removed XENTRP.  Added XU10 and XV10.  (T. Otte)
!           19 Aug 2005  Removed XDEPIDX and pointers to XDEPIDX.  Moved
!                        XDEPSPC and XVD to DEALLOC_DEPV.  Removed unused
!                        variables XCAPG, XMMPASS, and XFSOIL.  Removed XRH and
!                        made it a local scalar in M3DRY.  (T. Otte and
!                        W. Hutzell)
!           14 Jul 2006  Removed XDELTA and XLSTWET to be local variables in
!                        M3DRY.  Added XLWMASK.  (T. Otte)
!           30 Jul 2007  Removed XCFRACH, XCFRACM, XCFRACL, XRBNDY, XTEMP1P5,
!                        and XTEMP10.  Create 2-m temperature array even if
!                        it is not part of input meteorology.  Changed 2-m
!                        temperature from XT2 to XTEMP2.  Removed internal
!                        variables for emissivity and net radiation.  Removed
!                        XF2DEF and XRSTMIN to be local variables in
!                        RESISTCALC.  Added XPSTAR0.  (T. Otte)
!           21 Apr 2008  Added 2-m mixing ratio (XQ2) and turbulent kinetic
!                        energy (XTKE) arrays.  (T. Otte)
!           17 Sep 2009  Added land-use category description, XLUDESC.  Added
!                        potential vorticity (XPVC), Coriolis (XCORL), and
!                        potential temperature (XTHETA).  Removed dependency
!                        on module MCIPPARM.  Added map-scale factors squared
!                        (on cross points, XMAPC2).  Added XLATU, XLONU, XMAPU,
!                        XLATV, XLONV, and XMAPV.  (T. Otte)
!-------------------------------------------------------------------------------

  USE xvars

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Scalars and One-Dimensional Arrays  
!-------------------------------------------------------------------------------

  DEALLOCATE ( xx3face  )
  DEALLOCATE ( xx3midl  )
  DEALLOCATE ( xdx3     )

  DEALLOCATE ( xludesc  )

!-------------------------------------------------------------------------------
! Dot-Point and Face 2D Arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xlatd )
  DEALLOCATE ( xlatu )
  DEALLOCATE ( xlatv )
  DEALLOCATE ( xlond )
  DEALLOCATE ( xlonu )
  DEALLOCATE ( xlonv )
  DEALLOCATE ( xmapd )
  DEALLOCATE ( xmapu )
  DEALLOCATE ( xmapv )

!-------------------------------------------------------------------------------
! Cross-Point 2D Arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xlatc   )
  DEALLOCATE ( xlonc   )
  DEALLOCATE ( xmapc   )
  DEALLOCATE ( xmapc2  )
  DEALLOCATE ( xtopo   )

  DEALLOCATE ( xprsfc  )
  DEALLOCATE ( xdenss  )
  DEALLOCATE ( xtempg  )
  DEALLOCATE ( xrainn  )
  DEALLOCATE ( xrainc  )
  DEALLOCATE ( xdluse  )
  DEALLOCATE ( xlwmask )

  IF ( ALLOCATED ( xpurb ) )   DEALLOCATE ( xpurb )
  IF ( ALLOCATED ( xcorl ) )   DEALLOCATE ( xcorl )

  DEALLOCATE ( xglw    )
  DEALLOCATE ( xgsw    )
  DEALLOCATE ( xhfx    )
  DEALLOCATE ( xqfx    )
  DEALLOCATE ( xustar  )
  DEALLOCATE ( xpbl    )
  DEALLOCATE ( xzruf   )
  DEALLOCATE ( xmol    )
  DEALLOCATE ( xrgrnd  )
  DEALLOCATE ( xwr     )
  DEALLOCATE ( xlai    )
  DEALLOCATE ( xveg    )

  DEALLOCATE ( xwstar   )
  DEALLOCATE ( xrib     )
  DEALLOCATE ( xradyn   )
  DEALLOCATE ( xrstom   )
  DEALLOCATE ( xtemp2   )
  DEALLOCATE ( xq2      )
 DEALLOCATE ( xwspd10  )
  DEALLOCATE ( xwdir10  )
  DEALLOCATE ( xalbedo  )
  DEALLOCATE ( xmavail  )
  DEALLOCATE ( xcfract  )
  DEALLOCATE ( xcldtop  )
  DEALLOCATE ( xcldbot  )
  DEALLOCATE ( xwbar    )
  DEALLOCATE ( xsnocov  )

  IF ( ALLOCATED ( xu10   ) )  DEALLOCATE ( xu10   )
  IF ( ALLOCATED ( xv10   ) )  DEALLOCATE ( xv10   )

  IF ( ALLOCATED ( xtga   ) )  DEALLOCATE ( xtga   )
  IF ( ALLOCATED ( xt2a   ) )  DEALLOCATE ( xt2a   )
  IF ( ALLOCATED ( xwga   ) )  DEALLOCATE ( xwga   )
  IF ( ALLOCATED ( xw2a   ) )  DEALLOCATE ( xw2a   )
  IF ( ALLOCATED ( xsltyp ) )  DEALLOCATE ( xsltyp )

  DEALLOCATE ( xluse    )

!-------------------------------------------------------------------------------
! Cross-Point 3D Arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xtempm   )
  DEALLOCATE ( xpresm   )
  DEALLOCATE ( xdensam  )
  DEALLOCATE ( xdenswm  )
  DEALLOCATE ( x3jacobf )
  DEALLOCATE ( x3jacobm )
  DEALLOCATE ( x3htf    )
  DEALLOCATE ( x3htm    )
  DEALLOCATE ( xwhat    )
  DEALLOCATE ( xwvapor  )
  DEALLOCATE ( xwwind   )
  DEALLOCATE ( xcldwtr  )
  DEALLOCATE ( xranwtr  )
  DEALLOCATE ( xqice    )
  DEALLOCATE ( xqsnow   )
  DEALLOCATE ( xqgraup  )

  IF ( ALLOCATED (xtke)        ) DEALLOCATE ( xtke        )
  IF ( ALLOCATED (xpvc)        ) DEALLOCATE ( xpvc        )
  IF ( ALLOCATED (xtheta)      ) DEALLOCATE ( xtheta      )

!-------------------------------------------------------------------------------
! Dot-Point (and Face-Point) 3D Arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xuu_d )
  DEALLOCATE ( xvv_d )
  DEALLOCATE ( xuu_s )
  DEALLOCATE ( xvv_t )

!-------------------------------------------------------------------------------
! Variables for WRF only.
!-------------------------------------------------------------------------------

  IF ( ALLOCATED (xmu)         ) DEALLOCATE ( xmu         )
  IF ( ALLOCATED (xgeof)       ) DEALLOCATE ( xgeof       )

!-------------------------------------------------------------------------------
! Reference state variables for non-hydrostatic MM5.
!-------------------------------------------------------------------------------

  IF ( ALLOCATED (xpstar0)     ) DEALLOCATE ( xpstar0     )
  IF ( ALLOCATED (xdensaf_ref) ) DEALLOCATE ( xdensaf_ref )

!-------------------------------------------------------------------------------
! Internal arrrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xdx3htf )
  DEALLOCATE ( xdensaf )
  DEALLOCATE ( xpresf  )

END SUBROUTINE dealloc_x
