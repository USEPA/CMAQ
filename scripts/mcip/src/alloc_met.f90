
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
! $Header: /project/work/rep/MCIP2/src/mcip2/alloc_met.F,v 1.6 2007/08/03 20:50:50 tlotte Exp $ 


SUBROUTINE alloc_met

!-------------------------------------------------------------------------------
! Name:     Allocate Meteorology Variables
! Purpose:  Allocate arrays for input meteorology variables.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added SNOWCOVR.  (D. Schwede)
!           09 Aug 2004  Added QGA, VEGOLD, and T2.  (D. Schwede and T. Otte)
!           29 Nov 2004  Added LUFRAC.  (T. Otte)
!           04 Apr 2005  Changed array dimensions from I,J to X,Y to make code
!                        more general.  Now all meteorology arrays will have
!                        the east-west index first, as opposed to the standard
!                        MM5 convention.  For MM5 input, arrays are transposed
!                        to the new convention in either RDMM5V2 or RDMM5V3.
!                        Removed unused variables REGIME and MAVAIL.  Added PH,
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
!           29 Oct 2009  Changed MET_UCMCALL to MET_URBAN_PHYS, and allowed
!                        for variable to be set to be greater than 1.  Added
!                        THETA and CORIOLIS for when potential vorticity is
!                        needed.  Added LATU, LONU, MAPU, LATV, LONV, and
!                        and MAPV.  Removed Z0C_URB2D.  (T. Otte)
!-------------------------------------------------------------------------------

  USE metinfo, nx => met_nx, ny => met_ny, nz => met_nz
  USE metvars
  USE mcipparm

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Allocate time-invariant fields.
!-------------------------------------------------------------------------------

  ALLOCATE ( albedo   (nx, ny)       )   ! <--- time-variant with Pleim-Xiu LSM
  ALLOCATE ( landuse  (nx, ny)       )
  ALLOCATE ( latcrs   (nx, ny)       )
  ALLOCATE ( latdot   (nx, ny)       )
  ALLOCATE ( latu     (nx, ny)       )
  ALLOCATE ( latv     (nx, ny)       )
  ALLOCATE ( loncrs   (nx, ny)       )
  ALLOCATE ( londot   (nx, ny)       )
  ALLOCATE ( lonu     (nx, ny)       )
  ALLOCATE ( lonv     (nx, ny)       )
  ALLOCATE ( mapcrs   (nx, ny)       )
  ALLOCATE ( mapdot   (nx, ny)       )
  ALLOCATE ( mapu     (nx, ny)       )
  ALLOCATE ( mapv     (nx, ny)       )
  ALLOCATE ( sigmah           (nz)   )
  ALLOCATE ( sigmaf           (nz+1) )
  ALLOCATE ( terrain  (nx, ny)       )
  ALLOCATE ( znt      (nx, ny)       )

  IF ( iflufrc ) THEN
    ALLOCATE ( lufrac (nx, ny, nummetlu) )
  ENDIF

  IF ( lpv > 0 ) THEN  ! potential vorticity; get Coriolis
    ALLOCATE ( coriolis (nx, ny) )
  ENDIF

!-------------------------------------------------------------------------------
! Allocate time-variant fields.
!-------------------------------------------------------------------------------

  ALLOCATE ( glw     (nx, ny)       )
  ALLOCATE ( groundt (nx, ny)       )
  ALLOCATE ( hfx     (nx, ny)       )
  ALLOCATE ( pp      (nx, ny, nz)   )
  ALLOCATE ( psa     (nx, ny)       )
  ALLOCATE ( qca     (nx, ny, nz)   )
  ALLOCATE ( qfx     (nx, ny)       )
  ALLOCATE ( qga     (nx, ny, nz)   )
  ALLOCATE ( qia     (nx, ny, nz)   )
  ALLOCATE ( qra     (nx, ny, nz)   )
  ALLOCATE ( qsa     (nx, ny, nz)   )
  ALLOCATE ( qva     (nx, ny, nz)   )
  ALLOCATE ( raincon (nx, ny)       )
  ALLOCATE ( rainnon (nx, ny)       )
  ALLOCATE ( rcold   (nx, ny)       )   ! save this variable on each call
  ALLOCATE ( rgrnd   (nx, ny)       )
  ALLOCATE ( rnold   (nx, ny)       )   ! save this variable on each call
  ALLOCATE ( snowcovr(nx, ny)       )
  ALLOCATE ( ta      (nx, ny, nz)   )
  ALLOCATE ( ua      (nx, ny, nz)   )
  ALLOCATE ( ust     (nx, ny)       )
  ALLOCATE ( va      (nx, ny, nz)   )
  ALLOCATE ( wa      (nx, ny, nz+1) )
  ALLOCATE ( zpbl    (nx, ny)       )

  IF ( ift2m ) THEN  ! 2-m temperature available
    ALLOCATE ( t2    (nx, ny)       )
  ENDIF

  IF ( ifq2m ) THEN  ! 2-m mixing ratio available
    ALLOCATE ( q2    (nx, ny)       )
  ENDIF

  IF ( ifw10m ) THEN  ! 10-m wind components available
    ALLOCATE ( u10   (nx, ny)       )
    ALLOCATE ( v10   (nx, ny)       )
  ENDIF

  IF ( met_model == 2 ) THEN  ! WRF
    ALLOCATE ( mu    (nx, ny)       )
    ALLOCATE ( mub   (nx, ny)       )
    ALLOCATE ( pb    (nx, ny, nz)   )
    ALLOCATE ( ph    (nx, ny, nz+1) )
    ALLOCATE ( phb   (nx, ny, nz+1) )
  ENDIF

  IF ( iflai ) THEN  ! leaf area index available
    ALLOCATE ( lai    (nx, ny) )
  ENDIF

  IF ( ifmol ) THEN  ! Monin-Obukhov length available
    ALLOCATE ( mol    (nx, ny) )
  ENDIF

  IF ( ifresist ) THEN  ! aerodynamic and stomatal resistances available
    ALLOCATE ( ra     (nx, ny) )
    ALLOCATE ( rstom  (nx, ny) )
  ENDIF

  IF ( ifveg ) THEN  ! vegetation fraction available
    ALLOCATE ( veg    (nx, ny) )
    IF ( ( met_model == 1 ) .AND. ( met_soil_lsm == 3 ) ) THEN
      ALLOCATE ( vegold (nx, ny) )
    ENDIF
  ENDIF

  IF ( ifwr ) THEN  ! canopy wetness available
    ALLOCATE ( wr     (nx, ny) )
  ENDIF

  IF ( ifsoil ) THEN  ! soil moisture, temperature, and type available
    ALLOCATE ( isltyp (nx, ny) )
    ALLOCATE ( soilt1 (nx, ny) )
    ALLOCATE ( soilt2 (nx, ny) )
    ALLOCATE ( w2     (nx, ny) )
    ALLOCATE ( wg     (nx, ny) )
  ENDIF

  IF ( iftke ) THEN  ! turbulent kinetic energy available
    IF ( iftkef ) THEN  ! TKE on full-levels
      ALLOCATE ( tke   (nx, ny, nz+1) )
    ELSE  ! TKE on half-levels
      ALLOCATE ( tke   (nx, ny, nz)   )
    ENDIF
  ENDIF

  IF ( lpv > 0 ) THEN  ! potential vorticity; get potential temperature
    ALLOCATE ( theta (nx, ny, nz) )
  ENDIF

  IF ( met_urban_phys >= 1 ) THEN  ! urban canopy model in WRF
    ALLOCATE ( frc_urb   (nx, ny) )
  ENDIF

END SUBROUTINE alloc_met
