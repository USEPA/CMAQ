
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
! $Header: /project/work/rep/MCIP2/src/mcip2/init_met.F,v 1.7 2007/08/03 20:51:04 tlotte Exp $ 


SUBROUTINE init_met

!-------------------------------------------------------------------------------
! Name:     Initialize Meteorology Arrays
! Purpose:  Initializes meteorology arrays.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added SNOWCOVR.  (D. Schwede)
!           09 Aug 2004  Added QGA, VEGOLD, and T2.  (D. Schwede and T. Otte)
!           29 Nov 2004  Added LUFRAC.  (T. Otte)
!           04 Apr 2005  Removed unused variables REGIME and MAVAIL.  Added PH,
!                        PHB, PB, MU, and MUB for WRF.  Added U10 and V10.
!                        (T. Otte and S.-B. Kim)
!           11 Aug 2005  Removed unused variable FSOIL.  (T. Otte)
!           19 Jun 2006  Corrected initial values for ISLTYP and LANDUSE.
!                        (T. Otte)
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
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars
  USE mcipparm

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Initialize meteorology arrays.
!-------------------------------------------------------------------------------

  sigmah   (:)     = 0.0      ;      sigmaf   (:)     = 0.0

  albedo   (:,:)   = 0.0      ;      glw      (:,:)   = 0.0
  groundt  (:,:)   = 0.0      ;      hfx      (:,:)   = 0.0
  landuse  (:,:)   = 0        ;      latcrs   (:,:)   = 0.0
  latdot   (:,:)   = 0.0      ;      latu     (:,:)   = 0.0
  latv     (:,:)   = 0.0      ;      loncrs   (:,:)   = 0.0
  londot   (:,:)   = 0.0      ;      lonu     (:,:)   = 0.0
  lonv     (:,:)   = 0.0      ;      mapcrs   (:,:)   = 0.0
  mapdot   (:,:)   = 0.0      ;      mapu     (:,:)   = 0.0
  mapv     (:,:)   = 0.0      ;      psa      (:,:)   = 0.0
  qfx      (:,:)   = 0.0      ;      raincon  (:,:)   = 0.0
  rainnon  (:,:)   = 0.0      ;      rcold    (:,:)   = 0.0
  rgrnd    (:,:)   = 0.0      ;      rnold    (:,:)   = 0.0
  snowcovr (:,:)   = 0.0      ;      terrain  (:,:)   = 0.0
  ust      (:,:)   = 0.0      ;      znt      (:,:)   = 0.0
  zpbl     (:,:)   = 0.0

  IF ( ift2m ) THEN  ! 2-m temperature available
    t2     (:,:)   = 0.0
  ENDIF

  IF ( ifq2m ) THEN  ! 2-m mixing ratio available
    q2     (:,:)   = 0.0
  ENDIF

  IF ( ifw10m ) THEN  ! 10-m wind components available
    u10    (:,:)   = 0.0
    v10    (:,:)   = 0.0
  ENDIF

  pp       (:,:,:) = 0.0      ;      qca      (:,:,:) = 0.0
  qga      (:,:,:) = 0.0      ;      qia      (:,:,:) = 0.0
  qra      (:,:,:) = 0.0      ;      qsa      (:,:,:) = 0.0
  qva      (:,:,:) = 0.0      ;      ta       (:,:,:) = 0.0
  ua       (:,:,:) = 0.0      ;      va       (:,:,:) = 0.0
  wa       (:,:,:) = 0.0

  IF ( iflufrc ) THEN  ! fractional land use fields available
    lufrac   (:,:,:) = 0.0
  ENDIF

  IF ( met_model == 2 ) THEN  ! WRF
    mu     (:,:)   = 0.0      ;      mub      (:,:)   = 0.0
    pb     (:,:,:) = 0.0
    ph     (:,:,:) = 0.0      ;      phb      (:,:,:) = 0.0
  ENDIF

  IF ( ALLOCATED ( coriolis) )  coriolis(:,:) = 0.0
  IF ( ALLOCATED ( isltyp  ) )  isltyp  (:,:) = 0.0
  IF ( ALLOCATED ( lai     ) )  lai     (:,:) = 0.0
  IF ( ALLOCATED ( mol     ) )  mol     (:,:) = 0.0
  IF ( ALLOCATED ( ra      ) )  ra      (:,:) = 0.0
  IF ( ALLOCATED ( rstom   ) )  rstom   (:,:) = 0.0
  IF ( ALLOCATED ( soilt1  ) )  soilt1  (:,:) = 0.0
  IF ( ALLOCATED ( soilt2  ) )  soilt2  (:,:) = 0.0
  IF ( ALLOCATED ( veg     ) )  veg     (:,:) = 0.0
  IF ( ALLOCATED ( vegold  ) )  vegold  (:,:) = 0.0
  IF ( ALLOCATED ( w2      ) )  w2      (:,:) = 0.0
  IF ( ALLOCATED ( wg      ) )  wg      (:,:) = 0.0
  IF ( ALLOCATED ( wr      ) )  wr      (:,:) = 0.0

  IF ( ALLOCATED ( tke     ) )  tke     (:,:,:) = 0.0
  IF ( ALLOCATED ( theta   ) )  theta   (:,:,:) = 0.0

  IF ( ALLOCATED ( frc_urb   ) ) frc_urb  (:,:) = 0.0

END SUBROUTINE init_met
