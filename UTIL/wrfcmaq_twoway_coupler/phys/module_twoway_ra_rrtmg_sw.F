! FSB REvised  Mie calculations  02/09/2011

MODULE module_twoway_ra_rrtmg_sw

use module_model_constants, only : cp
USE module_wrf_error
#if (HWRF == 1)
USE module_state_description, ONLY : FER_MP_HIRES, FER_MP_HIRES_ADVECT,
ETAMP_HWRF
#else
USE module_state_description, ONLY : FER_MP_HIRES, FER_MP_HIRES_ADVECT 
#endif

!USE module_dm

use parrrsw, only : nbndsw, ngptsw, naerec
use rrtmg_sw_init, only: rrtmg_sw_ini
use rrtmg_sw_rad, only: rrtmg_sw
use mcica_subcol_gen_sw, only: mcica_subcol_sw

use module_ra_rrtmg_lw, only : inirad, o3data, relcalc, reicalc, retab

private
public :: RRTMG_SWRAD2

contains

!------------------------------------------------------------------
   SUBROUTINE RRTMG_SWRAD2(                                       &
                       rthratensw,                                &
                       swupt, swuptc, swuptcln, swdnt, swdntc, swdntcln, &
                       swupb, swupbc, swupbcln, swdnb, swdnbc, swdnbcln, &
!                      swupflx, swupflxc, swdnflx, swdnflxc,      &
                       swcf, gsw,                                 &
                       xtime, gmt, xlat, xlong,                   &
                       radt, degrad, declin,                      &
                       coszr, julday, solcon,                     &
                       albedo, t3d, t8w, tsk,                     &
                       p3d, p8w, pi3d, rho3d,                     &
                       dz8w, cldfra3d, lradius, iradius,          & 
                       is_cammgmp_used, r, g,                     &
                       re_cloud,re_ice,re_snow,                   &
                       has_reqc,has_reqi,has_reqs,                &
                       icloud, warm_rain,                         &
                       cldovrlp,                                  & ! J. Henderson AER: cldovrlp namelist value
                       f_ice_phy, f_rain_phy,                     &
                       xland, xice, snow,                         &
                       qv3d, qc3d, qr3d,                          &
                       qi3d, qs3d, qg3d,                          &
                       o3input, o33d,                             &
                       aer_opt, aerod, no_src,                    &
                       alswvisdir, alswvisdif,                    &  !Zhenxin ssib alb comp (06/20/2011)
                       alswnirdir, alswnirdif,                    &  !Zhenxin ssib alb comp (06/20/2011)
                       swvisdir, swvisdif,                        &  !Zhenxin ssib swr comp (06/20/2011)
                       swnirdir, swnirdif,                        &  !Zhenxin ssib swi comp (06/20/2011)
                       sf_surface_physics,                        &  !Zhenxin
                       f_qv, f_qc, f_qr, f_qi, f_qs, f_qg,        &
                       tauaer300,tauaer400,tauaer600,tauaer999,   & ! czhao 
                       gaer300,gaer400,gaer600,gaer999,           & ! czhao 
                       waer300,waer400,waer600,waer999,           & ! czhao 
                       aer_ra_feedback,                           &
!jdfcz                 progn,prescribe,                           &
                       progn,calc_clean_atm_diag,                 &
                       qndrop3d,f_qndrop,                         & !czhao
                       mp_physics,                                & !wang 2014/12
                       ids,ide, jds,jde, kds,kde,                 & 
                       ims,ime, jms,jme, kms,kme,                 &
                       its,ite, jts,jte, kts,kte,                 &
                       swupflx, swupflxc,                         &
                       swdnflx, swdnflxc,                         &
                       tauaer3d_sw,ssaaer3d_sw,asyaer3d_sw,       & ! jararias 2013/11
                       swddir, swddni, swddif,                    & ! jararias 2013/08
                       swdownc, swddnic, swddirc,                 & ! PAJ
                       xcoszen,julian                             & ! jararias 2013/08
! ** FSB Add aerosol input ffrom CMAQ
                       ,nmode,                                    & ! WRF-CMAQ twoway coupled model
                       mass_ws_i, mass_ws_j, mass_ws_k,           & ! WRF-CMAQ twoway coupled model
                       mass_in_i, mass_in_j, mass_in_k,           & ! WRF-CMAQ twoway coupled model
                       mass_ec_i, mass_ec_j, mass_ec_k,           & ! WRF-CMAQ twoway coupled model
                       mass_ss_i, mass_ss_j, mass_ss_k,           & ! WRF-CMAQ twoway coupled model
                       mass_h2o_i, mass_h2o_j, mass_h2o_k,        & ! WRF-CMAQ twoway coupled model
                       dgn_i, dgn_j, dgn_k,                       & ! WRF-CMAQ twoway coupled model
                       sig_i, sig_j, sig_k,                       & ! WRF-CMAQ twoway coupled model
                       gtauxar_01, gtauxar_02, gtauxar_03,        & ! WRF-CMAQ twoway coupled model
                       gtauxar_04, gtauxar_05,                    & ! WRF-CMAQ twoway coupled model
                       asy_fac_01, asy_fac_02, asy_fac_03,        & ! WRF-CMAQ twoway coupled model
                       asy_fac_04, asy_fac_05,                    & ! WRF-CMAQ twoway coupled model
                       ssa_01, ssa_02, ssa_03,                    & ! WRF-CMAQ twoway coupled model
                       ssa_04, ssa_05                             & ! WRF-CMAQ twoway coupled model
                       ,sw_zbbcddir                               & ! WRF-CMAQ twoway model
                       ,sw_dirdflux, sw_difdflux                  & ! WRF-CMAQ twoway model
                                                                  )
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   LOGICAL, INTENT(IN )      ::        warm_rain
   LOGICAL, INTENT(IN )      ::   is_CAMMGMP_used ! Added for CAM5 RRTMG<->CAMMGMP
!
   INTEGER, INTENT(IN )      ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN )      ::        ICLOUD
   INTEGER, INTENT(IN )      ::        MP_PHYSICS
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(IN   ) ::                                   dz8w, &
                                                             t3d, &
                                                             t8w, &
                                                             p3d, &
                                                             p8w, &
                                                            pi3d, &
                                                           rho3d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(INOUT)  ::                            RTHRATENSW

   REAL, DIMENSION( ims:ime, jms:jme )                          , &
         INTENT(INOUT)  ::                                   GSW, &
                                                            SWCF, &
                                                           COSZR

   INTEGER, INTENT(IN  )     ::                           JULDAY
   REAL, INTENT(IN    )      ::                      RADT,DEGRAD, &
                                         XTIME,DECLIN,SOLCON,GMT

   REAL, DIMENSION( ims:ime, jms:jme )                          , &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                           XLAND, &
                                                            XICE, &
                                                            SNOW, &
                                                             TSK, &
                                                          ALBEDO
!
!!! -------------------  Zhenxin (2011-06/20) ------------------
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL                                               , &
         INTENT(IN)     ::                            ALSWVISDIR, &     ! ssib albedo of sw and lw
                                                      ALSWVISDIF, &
                                                      ALSWNIRDIR, &
                                                      ALSWNIRDIF

   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL                                               , &
         INTENT(OUT)    ::                              SWVISDIR, &
                                                        SWVISDIF, &
                                                        SWNIRDIR, &
                                                        SWNIRDIF        ! ssib sw dir and diff rad
   INTEGER, INTENT(IN) :: sf_surface_physics                            ! ssib para

!  ----------------------- end Zhenxin --------------------------
!

! ------------------------ jararias 2013/08/10 -----------------
   real, dimension(ims:ime,jms:jme), intent(out) :: &
            swddir,  &  ! All-sky broadband surface direct horiz irradiance
            swddni,  &  ! All-sky broadband surface direct normal irradiance
            swddif,  &  ! All-sky broadband surface diffuse irradiance
            swdownc, & ! Clear sky GHI
            swddnic, & ! Clear ski DNI
            swddirc    ! Clear ski direct horizontal irradiance

   real, optional, intent(in) :: &
            julian      ! julian day (1-366)
   real, dimension(ims:ime,jms:jme), intent(in) :: &
            xcoszen     ! cosine of the solar zenith angle
   real, dimension(:,:,:,:), pointer :: tauaer3d_sw,ssaaer3d_sw,asyaer3d_sw
! ------------------------ jararias end snippet -----------------

   REAL, INTENT(IN  )   ::                                   R,G
!
! Optional
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) ::                                         &
                                                        CLDFRA3D, &
                                                         LRADIUS, &
                                                         IRADIUS, &
                                                            QV3D, &
                                                            QC3D, &
                                                            QR3D, &
                                                            QI3D, &
                                                            QS3D, &
                                                            QG3D, &
                                                        QNDROP3D

!..Added by G. Thompson to couple cloud physics effective radii.
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::       &
                                                        RE_CLOUD, &
                                                          RE_ICE, &
                                                         RE_SNOW
   INTEGER, INTENT(IN):: has_reqc, has_reqi, has_reqs

   real pi,third,relconst,lwpmin,rhoh2o

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) ::                                         &
                                                       F_ICE_PHY, &
                                                      F_RAIN_PHY

   LOGICAL, OPTIONAL, INTENT(IN)   ::                             &
                                F_QV,F_QC,F_QR,F_QI,F_QS,F_QG,F_QNDROP

! Optional
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN    ) :: tauaer300,tauaer400,tauaer600,tauaer999, & ! czhao 
                                 gaer300,gaer400,gaer600,gaer999, & ! czhao 
                                 waer300,waer400,waer600,waer999    ! czhao 

   INTEGER,    INTENT(IN  ), OPTIONAL   ::       aer_ra_feedback
!jdfcz   INTEGER,    INTENT(IN  ), OPTIONAL   ::       progn,prescribe
   INTEGER,    INTENT(IN  ), OPTIONAL   ::       progn
   INTEGER,    INTENT(IN  )             ::       calc_clean_atm_diag

!  Ozone
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) :: O33D
   INTEGER, OPTIONAL, INTENT(IN ) :: o3input
!  EC aerosol: no_src = naerec = 6
   INTEGER,           INTENT(IN ) :: no_src
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:no_src )       , &
         OPTIONAL                                               , &
         INTENT(IN   ) :: aerod
   INTEGER, OPTIONAL, INTENT(IN ) :: aer_opt

      !wavelength corresponding to wavenum1 and wavenum2 (cm-1)
      real, save :: wavemin(nbndsw) ! Min wavelength (um) of 14 intervals
      data wavemin /3.077,2.500,2.150,1.942,1.626,1.299, &
      1.242,0.778,0.625,0.442,0.345,0.263,0.200,3.846/
      real, save :: wavemax(nbndsw) ! Max wavelength (um) of interval
      data wavemax/3.846,3.077,2.500,2.150,1.942,1.626, &
      1.299,1.242,0.778,0.625,0.442,0.345,0.263,12.195/
      real wavemid(nbndsw) ! Mid wavelength (um) of interval
      real, parameter :: thresh=1.e-9
      real ang,slope
      character(len=200) :: msg

! Top of atmosphere and surface shortwave fluxes (W m-2)
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL, INTENT(INOUT) ::                               &
                    SWUPT,SWUPTC,SWUPTCLN,SWDNT,SWDNTC,SWDNTCLN,  &
                    SWUPB,SWUPBC,SWUPBCLN,SWDNB,SWDNBC,SWDNBCLN

! Layer shortwave fluxes (including extra layer above model top)
! Vertical ordering is from bottom to top (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(OUT) ::                                 &
                               SWUPFLX,SWUPFLXC,                  &
                               SWDNFLX,SWDNFLXC

! =s= WRF-CMAQ twoway coupled model
! ** FSB items needed for new aerosol code from CMAQ
   integer, intent(in)  ::  nmode  ! number of log-normal modes

! the following mass cocentrations are in [ ug/m**3]
! mass_ws_*  - water soluble species
! mass_in_*  _ isoluble species
! mass_ec_*  - elemental carbon
! mass_h2o_* - aerosol water
! mass_ss_*  - sea salt
! dgn_*      - geometric mean diameter of each mode  [ m ]
! sig_*      _ geometric standard deviation of each mode

   real, dimension(ims:ime, kms:kme, jms:jme), intent(in) :: &
           mass_ws_i, mass_ws_j, mass_ws_k,                  &
           mass_in_i, mass_in_j, mass_in_k,                  &
           mass_ec_i, mass_ec_j, mass_ec_k,                  &
           mass_ss_i, mass_ss_j, mass_ss_k,                  &
           mass_h2o_i, mass_h2o_j, mass_h2o_k,               &
           dgn_i, dgn_j, dgn_k,                              &
           sig_i, sig_j, sig_k

   real, dimension(ims:ime, kms:kme, jms:jme), intent(out) :: gtauxar_01,   &
                                                              gtauxar_02,   &
                                                              gtauxar_03,   &
                                                              gtauxar_04,   &
                                                              gtauxar_05,   &
                                                              asy_fac_01,   &
                                                              asy_fac_02,   &
                                                              asy_fac_03,   &
                                                              asy_fac_04,   &
                                                              asy_fac_05,   &
                                                              ssa_01,       &
                                                              ssa_02,       &
                                                              ssa_03,       &
                                                              ssa_04,       &
                                                              ssa_05

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)        :: sw_zbbcddir,  &
                                                              sw_dirdflux,  &
                                                              sw_difdflux

! =e= WRF-CMAQ twoway coupled model

!  LOCAL VARS
 
   REAL, DIMENSION( kts:kte+1 ) ::                          Pw1D, &
                                                            Tw1D

   REAL, DIMENSION( kts:kte ) ::                          TTEN1D, &
                                                        CLDFRA1D, &
                                                            DZ1D, &
                                                             P1D, &
                                                             T1D, &
                                                            QV1D, &
                                                            QC1D, &
                                                            QR1D, &
                                                            QI1D, &
                                                           RHO1D, &
                                                            QS1D, &
                                                            QG1D, &
                                                            O31D, &
                                                          qndrop1d 

!BSF: From eq. (5) on p. 2434 in McFarquhar & Heymsfield (1996)
      real, parameter :: re_50C=1250.0/9.917, re_40C=1250.0/9.337,     &
                         re_30C=1250.0/9.208, re_20C=1250.0/9.387

! Added local arrays for RRTMG
    integer ::                                              ncol, &
                                                            nlay, &
                                                            icld, &
                                                        cldovrlp, & ! J. Henderson AER
                                                         inflgsw, &
                                                        iceflgsw, &
                                                        liqflgsw
! Dimension with extra layer from model top to TOA
    real, dimension( 1, kts:kte+2 )  ::                     plev, &
                                                            tlev
    real, dimension( 1, kts:kte+1 )  ::                     play, &
                                                            tlay, &
                                                          h2ovmr, &
                                                           o3vmr, &
                                                          co2vmr, &
                                                           o2vmr, &
                                                          ch4vmr, &
                                                          n2ovmr
    real, dimension( kts:kte+1 )  ::                       o3mmr
! mji - Add height of each layer for exponential-random cloud overlap
! This will be derived below from the dz in each layer
    real, dimension( 1, kts:kte+1 )  ::                     hgt
    real ::                                               dzsum
! Surface albedo (for UV/visible and near-IR spectral regions,
! and for direct and diffuse radiation)
    real, dimension( 1 )  ::                               asdir, &
                                                           asdif, &
                                                           aldir, &
                                                           aldif
! Dimension with extra layer from model top to TOA, 
! though no clouds are allowed in extra layer
    real, dimension( 1, kts:kte+1 )  ::                   clwpth, &
                                                          ciwpth, &
                                                          cswpth, &
                                                             rel, &
                                                             rei, &
                                                             res, &
                                                         cldfrac, &
                                                         relqmcl, &
                                                         reicmcl, &
                                                         resnmcl
    real, dimension( nbndsw, 1, kts:kte+1 )  ::           taucld, &
                                                          ssacld, &
                                                          asmcld, &
                                                          fsfcld
    real, dimension( ngptsw, 1, kts:kte+1 )  ::          cldfmcl, &
                                                         clwpmcl, &
                                                         ciwpmcl, &
                                                         cswpmcl, &
                                                         taucmcl, &
                                                         ssacmcl, &
                                                         asmcmcl, &
                                                         fsfcmcl
    real, dimension( 1, kts:kte+1, nbndsw )  ::           tauaer, &
                                                          ssaaer, &
                                                          asmaer   
    real, dimension( 1, kts:kte+1, naerec )  ::            ecaer

! Output arrays contain extra layer from model top to TOA
    real, dimension( 1, kts:kte+2 )  ::                   swuflx, &
                                                          swdflx, &
                                                         swuflxc, &
                                                         swdflxc, &
                                                       swuflxcln, &
                                                       swdflxcln, &
                                                       sibvisdir, &  ! Zhenxin 2011-06-20
                                                       sibvisdif, &
                                                       sibnirdir, &
                                                       sibnirdif     ! Zhenxin 2011-06-20

    real, dimension( 1, kts:kte+2 ) ::                   swdkdir, &  ! jararias, 2013/08/10
                                                         swdkdif, &  ! jararias, 2013/08/10
                                                        swdkdirc     ! PAJ

    real, dimension( 1, kts:kte+1 )  ::                     swhr, &
                                                           swhrc

    real, dimension ( 1 ) ::                                tsfc, &
                                                              ps, &
                                                          coszen
    real ::                                                   ro, &
                                                              dz, &
                                                           adjes, &
                                                            scon, &  
                                                  snow_mass_factor
    integer ::                                            dyofyr

    integer:: idx_rei
    real:: corr

! Set trace gas volume mixing ratios, 2005 values, IPCC (2007)
! carbon dioxide (379 ppmv)
    real :: co2
    data co2 / 379.e-6 / 
! methane (1774 ppbv)
    real :: ch4
    data ch4 / 1774.e-9 / 
! nitrous oxide (319 ppbv)
    real :: n2o
    data n2o / 319.e-9 / 
! Set oxygen volume mixing ratio (for o2mmr=0.23143)
    real :: o2
    data o2 / 0.209488 /

    integer :: iplon, irng, permuteseed
    integer :: nb

! For old lw cloud property specification
! Cloud and precipitation absorption coefficients
!    real :: abcw,abice,abrn,absn
!    data abcw /0.144/
!    data abice /0.0735/
!    data abrn /0.330e-3/
!    data absn /2.34e-3/

! Molecular weights and ratios for converting mmr to vmr units
!    real :: amd       ! Effective molecular weight of dry air (g/mol)  
!    real :: amw       ! Molecular weight of water vapor (g/mol)        
!    real :: amo       ! Molecular weight of ozone (g/mol)              
!    real :: amo2      ! Molecular weight of oxygen (g/mol)              
! Atomic weights for conversion from mass to volume mixing ratios                
!    data amd   /  28.9660   /                                                  
!    data amw   /  18.0160   /                                                  
!    data amo   /  47.9998   /                                                  
!    data amo2  /  31.9999   /
                                                                                 
    real :: amdw     ! Molecular weight of dry air / water vapor  
    real :: amdo     ! Molecular weight of dry air / ozone
    real :: amdo2    ! Molecular weight of dry air / oxygen
    data amdw /  1.607793 /                                                    
    data amdo /  0.603461 /
    data amdo2 / 0.905190 /
    
!!
    real, dimension(1, 1:kte-kts+1 )  :: pdel          ! Layer pressure thickness (mb)

    real, dimension(1, 1:kte-kts+1) ::   cicewp, &     ! in-cloud cloud ice water path
                                         cliqwp, &     ! in-cloud cloud liquid water path
                                         csnowp, &     ! in-cloud snow water path
                                          reliq, &     ! effective drop radius (microns)
                                          reice        ! ice effective drop size (microns)
    real, dimension(1, 1:kte-kts+1):: recloud1d, &
                                        reice1d, &
                                       resnow1d
    real :: gliqwp, gicewp, gsnowp, gravmks, tem1,tem2,tem3

!
!    REAL   ::  TSFC,GLW0,OLR0,EMISS0,FP
    REAL   ::  FP

!    real, dimension(1:ite-its+1 )          ::   clat     ! latitude in radians for columns
    real :: coszrs                      ! Cosine of solar zenith angle for present latitude 
    logical :: dorrsw                   ! Flag to allow shortwave calculation

    real, dimension (1) :: landfrac, landm, snowh, icefrac

    integer :: pcols, pver

    INTEGER :: i,j,K, na
    LOGICAL :: predicate

    REAL :: da, eot ! jararias, 14/08/2013

! =s= WRF-CMAQ twoway model
! fsb local variables for setting tauaer, sssaaer, asmaer
! FSB local variabble for C0AQ aerosols
    REAL, DIMENSION (nmode) :: INMASS_ws, INMASS_in,               &
          INMASS_ec, INMASS_ss,  INMASS_h2o,  INDGN, INSIG
    REAL  xtauaer, waer, gaer
    REAL delta_z ! local layer thickness

    INTEGER modes

    character (len = 50) :: mystr
! =e= WRF-CMAQ twoway model

!------------------------------------------------------------------
#if ( WRF_CHEM == 1 )
      IF ( aer_ra_feedback == 1) then
      IF ( .NOT. &
      ( PRESENT(tauaer300) .AND. &
        PRESENT(tauaer400) .AND. &
        PRESENT(tauaer600) .AND. &
        PRESENT(tauaer999) .AND. &
        PRESENT(gaer300) .AND. &
        PRESENT(gaer400) .AND. &
        PRESENT(gaer600) .AND. &
        PRESENT(gaer999) .AND. &
        PRESENT(waer300) .AND. &
        PRESENT(waer400) .AND. &
        PRESENT(waer600) .AND. &
        PRESENT(waer999) ) ) THEN
      CALL wrf_error_fatal  &
      ('Warning: missing fields required for aerosol radiation' )
      ENDIF
      ENDIF
#endif

!-----CALCULATE SHORT WAVE RADIATION
!                                                              
! All fields are ordered vertically from bottom to top
! Pressures are in mb

! latitude loop
  j_loop: do j = jts,jte

! longitude loop
     i_loop: do i = its,ite
         rho1d(kts:kte)=rho3d(i,kts:kte,j) ! BUG FIX (SGT): this was uninitialized
!
! Do shortwave by default, deactivate below if sun below horizon
         dorrsw = .true.

! Cosine solar zenith angle for current time step
!
          ! jararias, 14/08/2013
          coszr(i,j)=xcoszen(i,j)
          coszrs=xcoszen(i,j)

! Set flag to prevent shortwave calculation when sun below horizon
         if (coszrs.le.0.0) dorrsw = .false.
! Perform shortwave calculation if sun above horizon
         if (dorrsw) then

         do k=kts,kte+1
            Pw1D(K) = p8w(I,K,J)/100.
            Tw1D(K) = t8w(I,K,J)
         enddo

         DO K=kts,kte
            QV1D(K)=0.
            QC1D(K)=0.
            QR1D(K)=0.
            QI1D(K)=0.
            QS1D(K)=0.
            CLDFRA1D(k)=0.
            QNDROP1D(k)=0.
         ENDDO

         DO K=kts,kte
            QV1D(K)=QV3D(I,K,J)
            QV1D(K)=max(0.,QV1D(K))
         ENDDO

         IF (PRESENT(O33D)) THEN
            DO K=kts,kte
               O31D(K)=O33D(I,K,J)
            ENDDO
         ELSE
            DO K=kts,kte
               O31D(K)=0.0
            ENDDO
         ENDIF

         DO K=kts,kte
            TTEN1D(K)=0.
            T1D(K)=t3d(I,K,J)
            P1D(K)=p3d(I,K,J)/100.
            DZ1D(K)=dz8w(I,K,J)
         ENDDO

! moist variables

         IF (ICLOUD .ne. 0) THEN
            IF ( PRESENT( CLDFRA3D ) ) THEN
              DO K=kts,kte
                 CLDFRA1D(k)=CLDFRA3D(I,K,J)
              ENDDO
            ENDIF

            IF (PRESENT(F_QC) .AND. PRESENT(QC3D)) THEN
              IF ( F_QC) THEN
                 DO K=kts,kte
                    QC1D(K)=QC3D(I,K,J)
                    QC1D(K)=max(0.,QC1D(K))
                 ENDDO
              ENDIF
            ENDIF

            IF (PRESENT(F_QR) .AND. PRESENT(QR3D)) THEN
              IF ( F_QR) THEN
                 DO K=kts,kte
                    QR1D(K)=QR3D(I,K,J)
                    QR1D(K)=max(0.,QR1D(K))
                 ENDDO
              ENDIF
            ENDIF

            IF ( PRESENT(F_QNDROP).AND.PRESENT(QNDROP3D)) THEN
             IF (F_QNDROP) THEN
              DO K=kts,kte
               qndrop1d(K)=qndrop3d(I,K,J)
              ENDDO
             ENDIF
            ENDIF

! This logic is tortured because cannot test F_QI unless
! it is present, and order of evaluation of expressions
! is not specified in Fortran

            IF ( PRESENT ( F_QI ) ) THEN
              predicate = F_QI
            ELSE
              predicate = .FALSE.
            ENDIF

! For MP option 3
            IF (.NOT. predicate .and. .not. warm_rain) THEN
               DO K=kts,kte
                  IF (T1D(K) .lt. 273.15) THEN
                  QI1D(K)=QC1D(K)
                  QS1D(K)=QR1D(K)
                  QC1D(K)=0.
                  QR1D(K)=0.
                  ENDIF
               ENDDO
            ENDIF

            IF (PRESENT(F_QI) .AND. PRESENT(QI3D)) THEN
               IF (F_QI) THEN
                  DO K=kts,kte
                     QI1D(K)=QI3D(I,K,J)
                     QI1D(K)=max(0.,QI1D(K))
                  ENDDO
               ENDIF
            ENDIF

            IF (PRESENT(F_QS) .AND. PRESENT(QS3D)) THEN
               IF (F_QS) THEN
                  DO K=kts,kte
                     QS1D(K)=QS3D(I,K,J)
                     QS1D(K)=max(0.,QS1D(K))
                  ENDDO
               ENDIF
            ENDIF

            IF (PRESENT(F_QG) .AND. PRESENT(QG3D)) THEN
               IF (F_QG) THEN
                  DO K=kts,kte
                     QG1D(K)=QG3D(I,K,J)
                     QG1D(K)=max(0.,QG1D(K))
                  ENDDO
               ENDIF
            ENDIF

! mji - For MP option 5
            IF ( PRESENT(F_QI) .and. PRESENT(F_QC) .and. PRESENT(F_QS) .and. PRESENT(F_ICE_PHY) ) THEN
               IF ( F_QC .and. .not. F_QI .and. F_QS ) THEN
                  DO K=kts,kte
                     qi1d(k) = 0.1*qs3d(i,k,j)
                     qs1d(k) = 0.9*qs3d(i,k,j)
                     qc1d(k) = qc3d(i,k,j)
                     qi1d(k) = max(0.,qi1d(k))
                     qc1d(k) = max(0.,qc1d(k))
                  ENDDO
               ENDIF
            ENDIF

         ENDIF
! For mp option=5 or 85  (new Ferrier- Aligo or called fer_hires
! scheme), QI3D saves all frozen water (ice+snow)
#if (HWRF == 1)
           IF ( mp_physics == FER_MP_HIRES .OR. &
                mp_physics == FER_MP_HIRES_ADVECT .OR. &
                mp_physics == ETAMP_HWRF ) THEN
#else
           IF ( mp_physics == FER_MP_HIRES .OR. &
                mp_physics == FER_MP_HIRES_ADVECT) THEN
#endif
                  DO K=kts,kte
                     qi1d(k) = qi3d(i,k,j)
                     qs1d(k) = 0.0
                     qc1d(k) = qc3d(i,k,j)
                     qi1d(k) = max(0.,qi1d(k))
                     qc1d(k) = max(0.,qc1d(k))
                  ENDDO
           ENDIF
!
!         EMISS0=EMISS(I,J)
!         GLW0=0. 
!         OLR0=0. 
!         TSFC=TSK(I,J)
         DO K=kts,kte
            QV1D(K)=AMAX1(QV1D(K),1.E-12) 
         ENDDO

! Set up input for shortwave
         ncol = 1
! Add extra layer from top of model to top of atmosphere
         nlay = (kte - kts + 1) + 1

! Select cloud overlap assumption (1 = random, 2 = maximum-random, 3 = maximum, 4 = exponential, 5 = exponential-random
         icld=cldovrlp ! J. Henderson AER assign namelist variable cldovrlp to existing icld

! Select cloud liquid and ice optics parameterization options
! For passing in cloud optical properties directly:
!         inflgsw = 0
!         iceflgsw = 0
!         liqflgsw = 0
! For passing in cloud physical properties; cloud optics parameterized in RRTMG:
         inflgsw = 2
         iceflgsw = 3
         liqflgsw = 1

!Mukul change the flags here with reference to the new effective cloud/ice/snow radius
         IF (ICLOUD .ne. 0) THEN
            IF ( has_reqc .ne. 0) THEN
               inflgsw = 3
               DO K=kts,kte
                  recloud1D(ncol,K) = MAX(2.5, re_cloud(I,K,J)*1.E6)
                  if (recloud1D(ncol,K).LE.2.5.AND.cldfra3d(i,k,j).gt.0. &
     &                            .AND. (XLAND(I,J)-1.5).GT.0.) then      !--- Ocean
                     recloud1D(ncol,K) = 10.5
                  elseif (recloud1D(ncol,K).LE.2.5.AND.cldfra3d(i,k,j).gt.0. &
     &                            .AND. (XLAND(I,J)-1.5).LT.0.) then      !--- Land
                     recloud1D(ncol,K) = 7.5
                  endif
               ENDDO
            ELSE
               DO K=kts,kte
#if (EM_CORE==1)
                  recloud1D(ncol,K) = 5.0
#else
                  recloud1D(ncol,K) = 10.0  ! was 5.0
#endif
               ENDDO
            ENDIF

            IF ( has_reqi .ne. 0) THEN
               inflgsw  = 4
               iceflgsw = 4
               DO K=kts,kte
                  reice1D(ncol,K) = MAX(5., re_ice(I,K,J)*1.E6)
                  if (reice1D(ncol,K).LE.5..AND.cldfra3d(i,k,j).gt.0.) then
                     idx_rei = int(t3d(i,k,j)-179.)
                     idx_rei = min(max(idx_rei,1),75)
                     corr = t3d(i,k,j) - int(t3d(i,k,j))
                     reice1D(ncol,K) = retab(idx_rei)*(1.-corr) +      &
     &                                 retab(idx_rei+1)*corr
                     reice1D(ncol,K) = MAX(reice1D(ncol,K), 5.0)
                  endif
               ENDDO
            ELSE
               DO K=kts,kte
                  reice1D(ncol,K) = 10.
               ENDDO
            ENDIF

            IF ( has_reqs .ne. 0) THEN
               inflgsw  = 5
               iceflgsw = 5
               DO K=kts,kte
                  resnow1D(ncol,K) = MAX(10., re_snow(I,K,J)*1.E6)
               ENDDO
            ELSE
               DO K=kts,kte
#if (EM_CORE==1) 
                  resnow1D(ncol,K) = 10.0
#else
                 tem2 = 25.0  !- was 10.0
                 tem3=1.e3*rho1d(k)*qi1d(k)  !- IWC (g m^-3)
                 if (tem3>thresh) then       !- Only when IWC>1.e-9 g m^-3
                   tem1=t1d(k)-273.15
                   if (tem1 < -50.0) then
                     tem2 = re_50C*tem3**0.109
                   elseif (tem1 < -40.0) then
                      tem2 = re_40C*tem3**0.08
                   elseif (tem1 < -30.0) then
                      tem2 = re_30C*tem3**0.055
                   else
                      tem2 = re_20C*tem3**0.031
                   endif
                    tem2 = max(25.,tem2)
                 endif
                 reice1D(ncol,K) = min(tem2, 135.72)   !- 1.0315*reice <= 140 microns 
#endif
               ENDDO
            ENDIF

! special case for P3 microphysics
! put ice into snow category for optics, then set ice to zero
            IF ( has_reqs .eq. 0 .and. has_reqi .ne. 0 .and. has_reqc .ne. 0) THEN
               inflgsw  = 5
               iceflgsw = 5
               DO K=kts,kte
                  resnow1D(ncol,K) = MAX(10., re_ice(I,K,J)*1.E6)
                  QS1D(K)=QI3D(I,K,J)
                  QI1D(K)=0.
                  reice1D(ncol,K)=10.
               END DO

            END IF

         ENDIF

! Set cosine of solar zenith angle
         coszen(ncol) = coszrs
! Set solar constant
         scon = solcon
! For Earth/Sun distance adjustment in RRTMG
!         dyofyr = julday
!         adjes = 0.0 
! For WRF, solar constant is already provided with eccentricity adjustment,
! so do not do this in RRTMG
         dyofyr = 0
         adjes = 1.0 

! Layer indexing goes bottom to top here for all fields.
! Water vapor and ozone are converted from mmr to vmr. 
! Pressures are in units of mb here. 
         plev(ncol,1) = pw1d(1)
         tlev(ncol,1) = tw1d(1)
         tsfc(ncol) = tsk(i,j)
         do k = kts, kte
            play(ncol,k) = p1d(k)
            plev(ncol,k+1) = pw1d(k+1)
            pdel(ncol,k) = plev(ncol,k) - plev(ncol,k+1)
            tlay(ncol,k) = t1d(k)
            tlev(ncol,k+1) = tw1d(k+1)
            h2ovmr(ncol,k) = qv1d(k) * amdw
            co2vmr(ncol,k) = co2
            o2vmr(ncol,k) = o2
            ch4vmr(ncol,k) = ch4
            n2ovmr(ncol,k) = n2o
         enddo

! mji - Derive height of each layer mid-point from layer thickness.
! Needed for exponential (icld=4) and exponential-random overlap option (icld=5) only.
         dzsum = 0.0
         do k = kts, kte
            dz = dz1d(k)
            hgt(ncol,k) = dzsum + 0.5*dz
            dzsum = dzsum + dz
         enddo

!  Define profile values for extra layer from model top to top of atmosphere. 
!  The top layer temperature for all gridpoints is set to the top layer-1 
!  temperature plus a constant (0 K) that represents an isothermal layer    
!  above ptop.  Top layer interface temperatures are linearly interpolated 
!  from the layer temperatures.  

         play(ncol,kte+1) = 0.5 * plev(ncol,kte+1)
         tlay(ncol,kte+1) = tlev(ncol,kte+1) + 0.0
         plev(ncol,kte+2) = 1.0e-5
         tlev(ncol,kte+2) = tlev(ncol,kte+1) + 0.0
         tlev(ncol,kte+2) = tlev(ncol,kte+1) + 0.0
         h2ovmr(ncol,kte+1) = h2ovmr(ncol,kte) 
         co2vmr(ncol,kte+1) = co2vmr(ncol,kte) 
         o2vmr(ncol,kte+1) = o2vmr(ncol,kte) 
         ch4vmr(ncol,kte+1) = ch4vmr(ncol,kte) 
         n2ovmr(ncol,kte+1) = n2ovmr(ncol,kte) 

! mji - Fill in height array above model top to top of atmosphere using
! dz from model top layer for completeness, though this information is not
! likely to be used by the exponential-random cloud overlap method.
         hgt(ncol,kte+1) = dzsum + 0.5*dz

! Get ozone profile including amount in extra layer above model top
         call inirad (o3mmr,plev,kts,kte)

        if(present(o33d)) then
         do k = kts, kte+1
            o3vmr(ncol,k) = o3mmr(k) * amdo
            IF ( PRESENT( O33D ) ) THEN
            if(o3input .eq. 2)then
               if(k.le.kte)then
                 o3vmr(ncol,k) = o31d(k)
               else
! apply shifted climatology profile above model top
                 o3vmr(ncol,k) = o31d(kte) - o3mmr(kte)*amdo + o3mmr(k)*amdo
                 if(o3vmr(ncol,k) .le. 0.)o3vmr(ncol,k) = o3mmr(k)*amdo
               endif
            endif
            ENDIF
         enddo
        else
         do k = kts, kte+1
            o3vmr(ncol,k) = o3mmr(k) * amdo
         enddo
        endif

! Set surface albedo for direct and diffuse radiation in UV/visible and
! near-IR spectral regions
! -------------- Zhenxin 2011-06-20 ----------- !

! ------- 1.  Commented by Zhenxin 2011-06-20 for SSiB coupling modified ---- !
!         asdir(ncol) = albedo(i,j)
!         asdif(ncol) = albedo(i,j)
!         aldir(ncol) = albedo(i,j)
!         aldif(ncol) = albedo(i,j)
! -------    End of Comments    ------ !

! ------- 2. New Addiation  ------ !
    IF ( sf_surface_physics .eq. 8 .AND. XLAND(i,j) .LT. 1.5) THEN
         asdir(ncol) = ALSWVISDIR(I,J)
         asdif(ncol) = ALSWVISDIF(I,J)
         aldir(ncol) = ALSWNIRDIR(I,J)
         aldif(ncol) = ALSWNIRDIF(I,J)
    ELSE
         asdir(ncol) = albedo(i,j)
         asdif(ncol) = albedo(i,j)
         aldir(ncol) = albedo(i,j)
         aldif(ncol) = albedo(i,j)
    ENDIF

! ---------- End of Addiation ------!
! ----------  End of fds_Zhenxin 2011-06-20   --------------!

! Define cloud optical properties for radiation (inflgsw = 0)
! This option is not currently active
! Cloud and precipitation paths in g/m2 
! qi=0 if no ice phase
! qs=0 if no ice phase
         if (inflgsw .eq. 0) then

! Set cloud fraction and cloud optical properties here; not yet active
            do k = kts, kte
               cldfrac(ncol,k) = cldfra1d(k)
               do nb = 1, nbndsw
                  taucld(nb,ncol,k) = 0.0
                  ssacld(nb,ncol,k) = 1.0
                  asmcld(nb,ncol,k) = 0.0
                  fsfcld(nb,ncol,k) = 0.0
               enddo
            enddo

! Zero out cloud physical property arrays; not used when passing optical properties
! into radiation
            do k = kts, kte
               clwpth(ncol,k) = 0.0
               ciwpth(ncol,k) = 0.0
               rel(ncol,k) = 10.0
               rei(ncol,k) = 10.
            enddo
         endif

! Define cloud physical properties for radiation (inflgsw = 1 or 2)
! Cloud fraction
! Set cloud arrays if passing cloud physical properties into radiation
         if (inflgsw .gt. 0) then 
            do k = kts, kte
               cldfrac(ncol,k) = cldfra1d(k)
            enddo

! Compute cloud water/ice paths and particle sizes for input to radiation (CAM method)
            pcols = ncol
            pver = kte - kts + 1
            gravmks = g
            landfrac(ncol) = 2.-XLAND(I,J)
            landm(ncol) = landfrac(ncol)
            snowh(ncol) = 0.001*SNOW(I,J)
            icefrac(ncol) = XICE(I,J)

! From module_ra_cam: Convert liquid and ice mixing ratios to water paths;
! pdel is in mb here; convert back to Pa (*100.)
! Water paths are in units of g/m2
! snow added as ice cloud (JD 091022)
            do k = kts, kte
               gicewp = (qi1d(k)+qs1d(k)) * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box ice water path.
               gliqwp = qc1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box liquid water path.
               cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))               ! In-cloud ice water path.
               cliqwp(ncol,k) = gliqwp / max(0.01,cldfrac(ncol,k))               ! In-cloud liquid water path.
            end do

! Mukul
!..The ice water path is already sum of cloud ice and snow, but when we have explicit
!.. ice effective radius, overwrite the ice path with only the cloud ice variable,
!.. leaving out the snow for its own effect.
           if(iceflgsw.ge.4)then 
              do k = kts, kte
                     gicewp = qi1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box ice water path.
                     cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))               ! In-cloud ice water path.
              end do
           end if

!..Here the snow path is adjusted if (radiation) effective radius of snow is
!.. larger than what we currently have in the lookup tables.  Since mass goes
!.. rather close to diameter squared, adjust the mixing ratio of snow used
!.. to compute its water path in combination with the max diameter.  Not a
!.. perfect fix, but certainly better than using all snow mass when diameter is
!.. far larger than table currently contains and crystal sizes much larger than
!.. about 140 microns have lesser impact than those much smaller sizes.

           if(iceflgsw.eq.5)then
              do k = kts, kte
                 snow_mass_factor = 1.0                 
                 if (resnow1d(ncol,k) .gt. 130.)then 
                     snow_mass_factor = (130.0/resnow1d(ncol,k))*(130.0/resnow1d(ncol,k))
                     resnow1d(ncol,k)   = 130.0
                 endif
                 gsnowp = qs1d(k) * snow_mass_factor * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box snow water path.
                 csnowp(ncol,k) = gsnowp / max(0.01,cldfrac(ncol,k))
              end do
           end if


!link the aerosol feedback to cloud  -czhao
  if( PRESENT( progn ) ) then
    if (progn == 1) then
!jdfcz     if(prescribe==0) then

      pi = 4.*atan(1.0)
      third=1./3.
      rhoh2o=1.e3
      relconst=3/(4.*pi*rhoh2o)
!     minimun liquid water path to calculate rel
!     corresponds to optical depth of 1.e-3 for radius 4 microns.
      lwpmin=3.e-5
      do k = kts, kte
         reliq(ncol,k) = 10.
         if( PRESENT( F_QNDROP ) ) then
            if( F_QNDROP ) then
              if ( qc1d(k)*pdel(ncol,k).gt.lwpmin.and. &
                   qndrop1d(k).gt.1000. ) then
               reliq(ncol,k)=(relconst*qc1d(k)/qndrop1d(k))**third ! effective radius in m
!           apply scaling from Martin et al., JAS 51, 1830.
               reliq(ncol,k)=1.1*reliq(ncol,k)
               reliq(ncol,k)=reliq(ncol,k)*1.e6 ! convert from m to microns
               reliq(ncol,k)=max(reliq(ncol,k),4.)
               reliq(ncol,k)=min(reliq(ncol,k),20.)
              end if
            end if
         end if
      end do
!jdfcz     else ! prescribe 
! following Kiehl
!     call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
!      write(0,*) 'sw prescribe aerosol',maxval(qndrop3d)
!jdfcz     endif
    else  ! progn   (progn=1)
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
    endif
  else   !progn   (PRESENT)
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
  endif

! following Kristjansson and Mitchell
      call reicalc(ncol, pcols, pver, tlay, reice)



!..If we already have effective radius of cloud and ice, then just overwrite what
!.. was computed in the relcalc and reicalc subroutines above.

      if (inflgsw .ge. 3) then
         do k = kts, kte
            reliq(ncol,k) = recloud1d(ncol,k)
         end do
      endif
#if (EM_CORE==1) 
      if (iceflgsw .ge. 4) then
#else
      if (iceflgsw .ge. 3) then   !BSF: was .ge. 4
#endif
         do k = kts, kte
            reice(ncol,k) = reice1d(ncol,k)
         end do
      endif


#if 0
      if (i==80.and.j==30) then
#if defined( DM_PARALLEL ) && ! defined( STUBMPI) 
      if( PRESENT( progn ) ) write(0,*) 'aerosol indirect',progn
      write(0,*)'sw water eff radius',reliq(ncol,10),reliq(ncol,20),reliq(ncol,25)
      write(0,*)'sw ice eff radius',reice(ncol,10),reice(ncol,20),reice(ncol,25)
#endif
      endif
#endif


! Limit upper bound of reice for Fu ice parameterization and convert
! from effective radius to generalized effective size (*1.0315; Fu, 1996)
            if (iceflgsw .eq. 3) then
               do k = kts, kte
                  reice(ncol,k) = reice(ncol,k) * 1.0315
                  reice(ncol,k) = min(140.0,reice(ncol,k))
               end do
            endif
            
!if CAMMGMP is used, use output from CAMMGMP            
!PMA
            if(is_CAMMGMP_used) then
               do k = kts, kte
                  if ( qi1d(k) .gt. 1.e-20 .or. qs1d(k) .gt. 1.e-20) then
                     reice(ncol,k) = iradius(i,k,j)
                  else
                     reice(ncol,k) = 25.
                  end if
                  reice(ncol,k) = max(5., min(140.0,reice(ncol,k)))
                  if ( qc1d(k) .gt. 1.e-20) then
                     reliq(ncol,k) = lradius(i,k,j)
                  else
                     reliq(ncol,k) = 10.
                  end if
                  reliq(ncol,k) = max(2.5, min(60.0,reliq(ncol,k)))
               enddo
            endif

! Set cloud physical property arrays
            do k = kts, kte
               clwpth(ncol,k) = cliqwp(ncol,k)
               ciwpth(ncol,k) = cicewp(ncol,k)
               rel(ncol,k) = reliq(ncol,k)
               rei(ncol,k) = reice(ncol,k)
            enddo

!Mukul
            if (inflgsw .eq. 5) then
               do k = kts, kte
                  cswpth(ncol,k) = csnowp(ncol,k)
                  res(ncol,k) = resnow1d(ncol,k)
               end do
            else
               do k = kts, kte
                  cswpth(ncol,k) = 0.0
                  res(ncol,k) = 10.0
               end do
            endif

! Zero out cloud optical properties here, calculated in radiation 
            do k = kts, kte
               do nb = 1, nbndsw
                  taucld(nb,ncol,k) = 0.0
                  ssacld(nb,ncol,k) = 1.0
                  asmcld(nb,ncol,k) = 0.0
                  fsfcld(nb,ncol,k) = 0.0
               enddo
            enddo
         endif

! No clouds are allowed in the extra layer from model top to TOA
         clwpth(ncol,kte+1) = 0.
         ciwpth(ncol,kte+1) = 0.
         cswpth(ncol,kte+1) = 0.
         rel(ncol,kte+1) = 10.
         rei(ncol,kte+1) = 10.
         res(ncol,kte+1) = 10.
         cldfrac(ncol,kte+1) = 0.
         do nb = 1, nbndsw
            taucld(nb,ncol,kte+1) = 0.
            ssacld(nb,ncol,kte+1) = 1.
            asmcld(nb,ncol,kte+1) = 0.
            fsfcld(nb,ncol,kte+1) = 0.
         enddo

         iplon = 1
         irng = 0
         permuteseed = 1

! Sub-column generator for McICA
! mji - Add layer height needed for exponential (icld=4) and exponential-random (icld=5) overlap options
         call mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, play, hgt, &
                       cldfrac, ciwpth, clwpth, cswpth, rei, rel, res, taucld, ssacld, asmcld, fsfcld, &
                       cldfmcl, ciwpmcl, clwpmcl, cswpmcl, reicmcl, relqmcl, resnmcl, &
                       taucmcl, ssacmcl, asmcmcl, fsfcmcl)


!--------------------------------------------------------------------------
! Aerosol optical depth, single scattering albedo and asymmetry parameter -czhao 03/2010
!--------------------------------------------------------------------------
! by layer for each RRTMG shortwave band
! No aerosols in top layer above model top (kte+1).
!cz        do nb = 1, nbndsw
!cz           do k = kts, kte+1
!cz              tauaer(ncol,k,nb) = 0.
!cz              ssaaer(ncol,k,nb) = 1.
!cz              asmaer(ncol,k,nb) = 0.
!cz           enddo
!cz        enddo

! ... Aerosol effects. Added aerosol feedbacks from Chem , 03/2010 -czhao
!
      do nb = 1, nbndsw
      do k = kts,kte+1
         tauaer(ncol,k,nb) = 0.
         ssaaer(ncol,k,nb) = 1.
         asmaer(ncol,k,nb) = 0.

         INMASS_ws(1)  = mass_ws_i(i,k,j)
         INMASS_ws(2)  = mass_ws_j(i,k,j)
         INMASS_ws(3)  = mass_ws_k(i,k,j)
         INMASS_in(1)  = mass_in_i(i,k,j)
         INMASS_in(2)  = mass_in_j(i,k,j)
         INMASS_in(3)  = mass_in_k(i,k,j)
         INMASS_ec(1)  = mass_ec_i(i,k,j)
         INMASS_ec(2)  = mass_ec_j(i,k,j)
         INMASS_ec(3)  = mass_ec_k(i,k,j)
         INMASS_ss(1)  = mass_ss_i(i,k,j)
         INMASS_ss(2)  = mass_ss_j(i,k,j)
         INMASS_ss(3)  = mass_ss_k(i,k,j)
         INMASS_h2o(1) = mass_h2o_i(i,k,j)
         INMASS_h2o(2) = mass_h2o_j(i,k,j)
         INMASS_h2o(3) = mass_h2o_k(i,k,j)
         INDGN(1)      = dgn_i(i,k,j)
         INDGN(2)      = dgn_j(i,k,j)
         INDGN(3)      = dgn_k(i,k,j)
         INSIG(1)      = sig_i(i,k,j)
         INSIG(2)      = sig_j(i,k,j)
         INSIG(3)      = sig_k(i,k,j)

         delta_z    = dz8w(i,k,j)

         call get_aerosol_Optics_RRTMG_SW( nb,nmode,delta_z,    &
               INMASS_ws, INMASS_in, INMASS_ec, INMASS_ss,      &
               INMASS_h2o,  INDGN, INSIG,                       &
               xtauaer, waer, gaer  )

         write (mystr, *) xtauaer
         if (trim(mystr) == ' NaN') then
            write (6, '(a13, 2i5)')     ' ==d==       ', nb, nmode
            write (6, '(a13, 5e18.10)') ' ==d== delta ', delta_z
            write (6, '(a13, 5e18.10)') ' ==d== ws    ', INMASS_ws
            write (6, '(a13, 5e18.10)') ' ==d== in    ', INMASS_in
            write (6, '(a13, 5e18.10)') ' ==d== ec    ', INMASS_ec
            write (6, '(a13, 5e18.10)') ' ==d== ss    ', INMASS_ss
            write (6, '(a13, 5e18.10)') ' ==d== h2o   ', INMASS_h2o
            write (6, '(a13, 5e18.10)') ' ==d== indgn ', INDGN
            write (6, '(a13, 5e18.10)') ' ==d== insig ', INSIG
         end if

         if (nb == 11) then
            gtauxar_01 (i,k,j) = xtauaer
            asy_fac_01 (i,k,j) = gaer
            ssa_01 (i,k,j)     = waer
         else if (nb == 10) then
            gtauxar_02 (i,k,j) = xtauaer
            asy_fac_02 (i,k,j) = gaer
            ssa_02 (i,k,j)     = waer
         else if (nb == 9) then
            gtauxar_03 (i,k,j) = xtauaer
            asy_fac_03 (i,k,j) = gaer
            ssa_03 (i,k,j)     = waer
         else if (nb == 8) then
            gtauxar_04 (i,k,j) = xtauaer
            asy_fac_04 (i,k,j) = gaer
            ssa_04 (i,k,j)     = waer
         else if (nb == 7) then
            gtauxar_05 (i,k,j) = xtauaer
            asy_fac_05 (i,k,j) = gaer
            ssa_05 (i,k,j)     = waer
         end if

         tauaer(ncol,k,nb) = xtauaer
         ssaaer(ncol,k,nb) = waer
         asmaer(ncol,k,nb) = gaer

      enddo ! loop over layers
! No aerosols in top layer above model top (kte+1).
      tauaer(ncol, kte+1 ,nb) = 0.
      ssaaer(ncol, kte+1 ,nb) = 1.
      asmaer(ncol, kte+1 ,nb) = 0.
      enddo ! loop over wavelengths

      if ( associated (tauaer3d_sw) ) then
! ---- jararias 11/2012
            do nb=1,nbndsw
               do k=kts,kte
                  tauaer(ncol,k,nb)=tauaer3d_sw(i,k,j,nb)
                  ssaaer(ncol,k,nb)=ssaaer3d_sw(i,k,j,nb)
                  asmaer(ncol,k,nb)=asyaer3d_sw(i,k,j,nb)
               end do
            end do
      end if

#if ( WRF_CHEM == 1 )
   IF ( AER_RA_FEEDBACK == 1) then
      do nb = 1, nbndsw
         wavemid(nb)=0.5*(wavemin(nb)+wavemax(nb))  ! um
      do k = kts,kte      !wig

! convert optical properties at 300,400,600, and 999 to conform to the band wavelengths
! tauaer - use angstrom exponent
        if(tauaer300(i,k,j).gt.thresh .and. tauaer999(i,k,j).gt.thresh) then
           ang=alog(tauaer300(i,k,j)/tauaer999(i,k,j))/alog(999./300.)
           tauaer(ncol,k,nb)=tauaer400(i,k,j)*(0.4/wavemid(nb))**ang
           !tauaer(ncol,k,nb)=tauaer600(i,k,j)*(0.6/wavemid(nb))**ang 
           if (i==30.and.j==49.and.k==2.and.nb==12) then
            write(0,*) 'TAU from 600 vs 400 in RRTMG',tauaer600(i,k,j),tauaer400(i,k,j)
            print*, 'TAU from 600 vs 400 in RRTMG',tauaer600(i,k,j),tauaer400(i,k,j)
            write(0,*) tauaer600(i,k,j)*(0.6/wavemid(nb))**ang,tauaer400(i,k,j)*(0.4/wavemid(nb))**ang
            print*, tauaer600(i,k,j)*(0.6/wavemid(nb))**ang,tauaer400(i,k,j)*(0.4/wavemid(nb))**ang
           endif
! ssa - linear interpolation; extrapolation
           slope=(waer600(i,k,j)-waer400(i,k,j))/.2
           ssaaer(ncol,k,nb) = slope*(wavemid(nb)-.6)+waer600(i,k,j)
           if(ssaaer(ncol,k,nb).lt.0.4) ssaaer(ncol,k,nb)=0.4
           if(ssaaer(ncol,k,nb).ge.1.0) ssaaer(ncol,k,nb)=1.0
! g - linear interpolation;extrapolation
           slope=(gaer600(i,k,j)-gaer400(i,k,j))/.2
           asmaer(ncol,k,nb) = slope*(wavemid(nb)-.6)+gaer600(i,k,j) ! notice reversed varaibles
           if(asmaer(ncol,k,nb).lt.0.5) asmaer(ncol,k,nb)=0.5
           if(asmaer(ncol,k,nb).ge.1.0) asmaer(ncol,k,nb)=1.0
        endif
      end do ! k
      end do ! nb

!wig beg
      do nb = 1, nbndsw
         slope = 0.  !use slope as a sum holder
         do k = kts,kte
            slope = slope + tauaer(ncol,k,nb)
         end do
         if( slope < 0. ) then
            write(msg,'("ERROR: Negative total optical depth of ",f8.2," at point i,j,nb=",3i5)') slope,i,j,nb
            call wrf_error_fatal(msg)
         else if( slope > 6. ) then
            call wrf_message("-------------------------")
            write(msg,'("WARNING: Large total sw optical depth of ",f8.2," at point i,j,nb=",3i5)') slope,i,j,nb
            call wrf_message(msg)

            call wrf_message("Diagnostics 1: k, tauaer300, tauaer400, tauaer600, tauaer999, tauaer")
            do k=kts,kte
               write(msg,'(i4,5f8.2)') k, tauaer300(i,k,j), tauaer400(i,k,j), &
                    tauaer600(i,k,j), tauaer999(i,k,j),tauaer(ncol,k,nb)
               call wrf_message(msg)
               !czhao set an up-limit here to avoid segmentation fault 
               !from extreme AOD
               tauaer(ncol,k,nb)=tauaer(ncol,k,nb)*6.0/slope 
            end do

            call wrf_message("Diagnostics 2: k, gaer300, gaer400, gaer600, gaer999")
            do k=kts,kte
               write(msg,'(i4,4f8.2)') k, gaer300(i,k,j), gaer400(i,k,j), &
                    gaer600(i,k,j), gaer999(i,k,j)
               call wrf_message(msg)
            end do

            call wrf_message("Diagnostics 3: k, waer300, waer400, waer600, waer999")
            do k=kts,kte
               write(msg,'(i4,4f8.2)') k, waer300(i,k,j), waer400(i,k,j), &
                    waer600(i,k,j), waer999(i,k,j)
               call wrf_message(msg)
            end do

            call wrf_message("Diagnostics 4: k, ssaal, asyal, taual")
            do k=kts-1,kte
               write(msg,'(i4,3f8.2)') k, ssaaer(i,k,nb), asmaer(i,k,nb), tauaer(i,k,nb)
               call wrf_message(msg)
            end do
            call wrf_message("-------------------------")
         endif
      enddo  ! nb
      endif  ! aer_ra_feedback
#endif


! Zero array for input of aerosol optical thickness for use with
! ECMWF aerosol types (not used)
         do na = 1, naerec
            do k = kts, kte+1
               ecaer(ncol,k,na) = 0.
            enddo
         enddo

      IF ( PRESENT( aerod ) ) THEN
      if ( aer_opt .eq. 0 ) then
         do na = 1, naerec
            do k = kts, kte+1
               ecaer(ncol,k,na) = 0.
            enddo
         enddo
      else if ( aer_opt .eq. 1 ) then
         do na = 1, naerec
            do k = kts, kte
               ecaer(ncol,k,na) = aerod(i,k,j,na)
            enddo
! assuming 0 or same value at the top?
!           ecaer(ncol,kte+1,na) = ecaer(ncol,kte,na)
            ecaer(ncol,kte+1,na) = 0.
         enddo
      endif
      ENDIF

! Call RRTMG shortwave radiation model

         call rrtmg_sw &
            (ncol    ,nlay    ,icld    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc   , &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,cldfmcl , &
             taucmcl ,ssacmcl ,asmcmcl ,fsfcmcl , &
             ciwpmcl ,clwpmcl ,cswpmcl, reicmcl ,relqmcl ,resnmcl, &
             tauaer  ,ssaaer  ,asmaer  ,ecaer   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, swuflxcln, swdflxcln, aer_opt, &
! -----      Zhenxin added for ssib coupiling 2011-06-20 --------!
             sibvisdir, sibvisdif, sibnirdir, sibnirdif,         &
! --------------------   End of addiation by Zhenxin 2011-06-20 ------!
             swdkdir, swdkdif,                     &  ! jararias, 2012/08/10
             swdkdirc                              &  ! PAJ
             ,calc_clean_atm_diag                 &
                                                   )


! Output net absorbed shortwave surface flux and shortwave cloud forcing
! at the top of atmosphere (W/m2)
         gsw(i,j) = swdflx(1,1) - swuflx(1,1)
         swcf(i,j) = (swdflx(1,kte+2) - swuflx(1,kte+2)) - (swdflxc(1,kte+2) - swuflxc(1,kte+2))

         if (present(swupt)) then 
! Output up and down toa fluxes for total and clear sky
            swupt(i,j)     = swuflx(1,kte+2)
            swuptc(i,j)    = swuflxc(1,kte+2)
            swdnt(i,j)     = swdflx(1,kte+2)
            swdntc(i,j)    = swdflxc(1,kte+2)
! Output up and down surface fluxes for total and clear sky
            swupb(i,j)     = swuflx(1,1)
            swupbc(i,j)    = swuflxc(1,1)
            swdnb(i,j)     = swdflx(1,1)
! Added by Zhenxin for 4 compenants of swdown radiation
            swvisdir(i,j)  = sibvisdir(1,1)
            swvisdif(i,j)  = sibvisdif(1,1)
            swnirdir(i,j)  = sibnirdir(1,1)
            swnirdif(i,j)  = sibnirdif(1,1)
!  Ended, Zhenxin (2011/06/20)
            swdnbc(i,j)    = swdflxc(1,1)
            if(calc_clean_atm_diag .gt. 0)then
            	swuptcln(i,j)  = swuflxcln(1,kte+2)
            	swdntcln(i,j)  = swdflxcln(1,kte+2)
            	swupbcln(i,j)  = swuflxcln(1,1)
            	swdnbcln(i,j)  = swdflxcln(1,1)
            end if
         endif
            swddir(i,j)    = swdkdir(1,1)          ! jararias 2013/08/10
            swddni(i,j)    = swddir(i,j) / coszrs  ! jararias 2013/08/10
            swddif(i,j)    = swdkdif(1,1)          ! jararias 2013/08/10
            swdownc(i, j)  = swdflxc(1,1)          ! PAJ: clear-sky GHI
            swddirc(i,j)   = swdkdirc(1,1)         ! PAJ: clear-sky direct normal irradiance
            swddnic(i,j)   = swddirc(i,j) / coszrs ! PAJ: clear-sky direct normal irradiance

! Output up and down layer fluxes for total and clear sky.
! Vertical ordering is from bottom to top in units of W m-2. 
         if ( present (swupflx) ) then
         do k=kts,kte+2
            swupflx(i,k,j)  = swuflx(1,k)
            swupflxc(i,k,j) = swuflxc(1,k)
            swdnflx(i,k,j)  = swdflx(1,k)
            swdnflxc(i,k,j) = swdflxc(1,k)
         enddo
         endif

! Output heating rate tendency; convert heating rate from K/d to K/s
! Heating rate arrays are ordered vertically from bottom to top here. 
         do k=kts,kte 
            tten1d(k) = swhr(ncol,k)/86400.
            rthratensw(i,k,j) = tten1d(k)/pi3d(i,k,j)
         enddo
      else

! =s= WRF-CMAQ twoway coupled model
         gtauxar_01 (i,:,j) = 0.0
         gtauxar_02 (i,:,j) = 0.0
         gtauxar_03 (i,:,j) = 0.0
         gtauxar_04 (i,:,j) = 0.0
         gtauxar_05 (i,:,j) = 0.0
         asy_fac_01 (i,:,j) = 0.0
         asy_fac_02 (i,:,j) = 0.0
         asy_fac_03 (i,:,j) = 0.0
         asy_fac_04 (i,:,j) = 0.0
         asy_fac_05 (i,:,j) = 0.0
         ssa_01 (i,:,j)     = 0.0
         ssa_02 (i,:,j)     = 0.0
         ssa_04 (i,:,j)     = 0.0
         ssa_04 (i,:,j)     = 0.0
         ssa_05 (i,:,j)     = 0.0
! =e= WRF-CMAQ twoway coupled model

         if (present(swupt)) then 
! Output up and down toa fluxes for total and clear sky
            swupt(i,j)     = 0.
            swuptc(i,j)    = 0.
            swdnt(i,j)     = 0.
            swdntc(i,j)    = 0.
! Output up and down surface fluxes for total and clear sky
            swupb(i,j)     = 0.
            swupbc(i,j)    = 0.
            swdnb(i,j)     = 0.
            swdnbc(i,j)    = 0.
            swvisdir(i,j)  = 0.  ! Add by Zhenxin (2011/06/20)
            swvisdif(i,j)  = 0.
            swnirdir(i,j)  = 0.
            swnirdif(i,j)  = 0.  ! Add by Zhenxin (2011/06/20)
            if(calc_clean_atm_diag .gt. 0)then
				swuptcln(i,j)  = 0.
				swdntcln(i,j)  = 0.
				swupbcln(i,j)  = 0.
				swdnbcln(i,j)  = 0.
            end if
         endif
            swddir(i,j)    = 0.  ! jararias 2013/08/10
            swddni(i,j)    = 0.  ! jararias 2013/08/10
            swddif(i,j)    = 0.  ! jararias 2013/08/10
            swdownc(i, j)  = 0.0 ! PAJ
            swddnic(i,j)   = 0.0 ! PAJ
            swddirc(i,j)   = 0.0 ! PAJ
            swcf(i,j)      = 0.

      endif
!
      end do i_loop
   end do j_loop                                           

   END SUBROUTINE RRTMG_SWRAD2

!------------------------------------------------------------------
      Subroutine get_aerosol_Optics_RRTMG_SW ( ns, nmode,delta_z, INMASS_ws,     &
                                               INMASS_in, INMASS_ec, INMASS_ss,  &
                                               INMASS_h2o,  INDGN, INSIG,        &
                                               tauaer, waer, gaer )

!FSB This version switches between BHCOAT to BHMIE depending upon whether
!    EC is present or not. 04/15/2012.

!FSB this version does a core-shell calculation with BHCOAT 04/11/2012    
! This version is set up to be used with RRTMG_SW <<<<<<<<
!     wavelenght is calculated internally
! FSB This routine calculates the aerosol information ( tauaer, waer, 
!     gaer, needed to calculate the solar radiation)  The calling 
!     program specifies the location ( row, column, layer, 
!     layer thicknes, and wave length for the calculation.   
! FSB 02/09/2011 Modifications made to subroutine ghintBH.
! FSB 04/14/2012 REmoved MODULUS, made changes to ghintBH.
!     Put in option for core-shell (coated-sphere). 2

! FSB Input variables:

      use rrtmg_aero_optical_util_module

      implicit none     

      integer,intent(in) ::  ns      ! index for wavelength should be 
                                     ! between 1 and 14. <<< RRTMG_SW
      integer,intent(in) ::  nmode   ! should be 3 for WRF/CMAQ calculation
      real,intent(in)    ::  delta_z ! layer thickness [m]
! FSB mode types for WRF/CMAQ 
!     nmode = 1 Aitken
!     nmode = 2 accumulation
!     nmode = 3 coarse 
! FSB modal mass concentration by species  [ ug / m**3]  NOTE:  MKS
      real, intent(in) ::  INMASS_ws(nmode)   ! water soluble
      real, intent(in) ::  INMASS_in(nmode)   ! insolugle
      real, intent(in) ::  INMASS_ec(nmode)   ! elemental carbon or soot like
      real, intent(in) ::  INMASS_ss(nmode)   ! sea salt
      real, intent(in) ::  INMASS_h2o(nmode)  ! water
! FSB particle size-distribution information      
      real, intent(in) ::  INDGN( nmode)      ! geometric mean diameter [ m ] NOTE: MKS      
      real, intent(in) ::  INSIG( nmode)      ! geometric standard deviation

!FSB output aerosol radiative properties  [dimensionless]
      real, intent(out) ::  tauaer   ! aerosol extinction optical depth
      real, intent(out) ::  waer     ! aerosol single scattering albedo
      real, intent(out) ::  gaer     ! aerosol assymetry parameter

! FSB Internal variables

      real    :: NR(nmode), NI(nmode)           ! refractive indices 
      complex :: refcor(nmode), refshell(nmode) ! complex refracive indices
      complex :: crefin(nmode)                  ! complex refractive index

! FSB special values for EC CORE-shell calculation
      real :: DGNSHELL(nmode)    ! modal geometric mean diameter [m]
      real :: DGNCORE (nmode)    ! modal geometric mean diameter [m] 
       
! FSB Modal volumes [ m**3 / m**3 ]
      real :: MVOL_ws(nmode)   ! water soluble
      real :: MVOL_in(nmode)   ! insolugle
      real :: MVOL_ec(nmode)   ! soot like
      real :: MVOL_ss(nmode)   !sea salt
      real :: MVOL_h2o(nmode)  ! water
!     real :: VOL(nmode)       ! total modal volume [m** 3 / m**3]
! FSB special values for EC CORE-shell calculation      
      real :: VOLCOR(nmode)    ! volume of EC core  [m** 3 / m**3]
      real :: VOLSHELL(nmode)  ! volume of shell    [m** 3 / m**3]
                      
      integer :: m             ! loop index
      real    :: bext          ! extinction coefficient [1 / m]
      real    :: bscat         ! scattering coefficient [1 / m]
      real    :: gfac          ! asymmetry factor

      real    :: bextsum, bscatsum, bsgsum
 
! FSB History variables by wavelength and mode     
!     real    :: bext_wm(ns,nmode) 
!     real    :: bscat_wm(ns,nmode)
!     real    :: gfac_wm(ns,nmode)

      real, parameter ::  one3rd = 1.0 / 3.0 
      real  :: dfac      ! ratio of (volcor/vol) ** one3rd
                         ! used for calculating the diameter
                         ! of the EC core

      logical :: succesS

!...component densities [ g/ cm**3 ] <<<<< cgs

      real, parameter :: rhows = 1.8   !  bulk density of water soluble aerosol

      real, parameter :: rhoin = 2.2   ! bulk density forinsoluble aerosol 

!     real, parameter :: rhoec = 1.7   ! bulk density for soot aerosol
      real, parameter :: rhoec = 1.8   ! new value

      real, parameter :: rhoh2o = 1.0  !  bulk density of aerosol water
       
      real, parameter :: rhoss = 2.2   ! bulk density of seasalt

! FSB scale factor for volume calculation
!            1.0d-12 * [ cm**3 / g] -> [ m** 3 / ug ]
      real, parameter :: scalefactor = 1.0e-12

! FSB scale factor for [1/g] to [1/ug]
      real, parameter ::  cug2g = 1.0e-06

! FSB reciprocal component densities[ m ** 3 / ug ] 
 
      real, parameter :: rhows1 = scalefactor / rhows    !   water soluble aerosol

      real, parameter :: rhoin1 = scalefactor /  rhoin   ! insoluble aerosol 

      real, parameter :: rhoec1 = scalefactor / rhoec    !  soot aerosol

      real, parameter :: rhoh2o1 = scalefactor / rhoh2o  !  aerosol water
       
      real, parameter :: rhoss1 = scalefactor / rhoss    !  seasalt

      integer,parameter ::  nspint_sw = 14 ! number of spectral intervals for RRTMG_SW

! FSB Band numbers and wavelengths for RRTMG_SW   
      integer, parameter :: Band(nspint_sw) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 /)
      
      real, parameter :: LAMDA_SW(nspint_sw) = (/ 3.4615,  2.7885, 2.325,  2.046,  1.784,         &
                                                  1.4625,  1.2705, 1.0101, 0.7016, 0.53325,       &
                                                  0.38815, 0.299,  0.2316, 8.24 /) ! wavelength [ um ]      
    
! *** refractive indices

! *** Except as otherwise noted reference values of refractive 
!     indices  for aerosol particles are from the OPAC Data base. 
!     Hess,  Koepke, and  Schult, Optical properties of
!     aerosols and clouds: The software package OPAC, Bulletan of
!     the American Meteorological Society, Vol 79, No 5, 
!     pp 831 - 844, May 1998. 
!     OPAC is a downloadable data set of optical properties of 
!     10 aerosol components, 6 water clouds and 3 cirrus clouds 
!     at UV, visible and IR wavelengths
!     www.lrz-muenchen.de/~uh234an/www/radaer/opac.htm


! FSB water soluble
      real, parameter :: xnreal_ws(nspint_sw) = (/ 1.443, 1.420, 1.420, 1.420, 1.463, 1.510, 1.510,   &
                                                   1.520, 1.530, 1.530, 1.530, 1.530, 1.530, 1.710 /)
      real, parameter :: xnimag_ws(nspint_sw) = (/ 5.718E-3, 1.777E-2, 1.060E-2, 8.368E-3, 1.621E-2,  &
                                                   2.198E-2, 1.929E-2, 1.564E-2, 7.000E-3, 5.666E-3,  &
                                                   5.000E-3, 8.440E-3, 3.000E-2, 1.100E-1 /)

! FSB sea salt      
      real, parameter :: xnreal_ss(nspint_sw) = (/ 1.480, 1.534, 1.437, 1.448, 1.450, 1.462, 1.469,   &
                                                   1.470, 1.490, 1.500, 1.502, 1.510, 1.510, 1.510 /)

      real, parameter :: xnimag_ss(nspint_sw) = (/ 1.758E-3, 7.462E-3, 2.950E-3, 1.276E-3, 7.944E-4,  &
                                                   5.382E-4, 3.754E-4, 1.498E-4, 2.050E-7, 1.184E-8,  &
                                                   9.938E-8, 2.060E-6, 5.000E-6, 1.000E-2 /)

! FSB insoluble      
      real, parameter :: xnreal_in(nspint_sw) = (/ 1.272, 1.168, 1.208, 1.253, 1.329, 1.418, 1.456,   &
                                                   1.518, 1.530, 1.530, 1.530, 1.530, 1.530, 1.470 /)
      real, parameter :: xnimag_in(nspint_sw) = (/ 1.165E-2, 1.073E-2, 8.650E-3, 8.092E-3, 8.000E-3,  &
                                                   8.000E-3, 8.000E-3, 8.000E-3, 8.000E-3, 8.000E-3,  &
                                                   8.000E-3, 8.440E-3, 3.000E-2,9.000E-2 /)

! FSB 02/11/2012 These values are replaced.
!      data xnreal_ec /1.877, 1.832, 1.813, 1.802, 1.791, 1.769, 1.761,   &
!                      1.760, 1.750, 1.740, 1.750, 1.738, 1.620, 2.120/
!      data xnimag_ec/ 5.563E-1, 5.273E-1, 5.030E-1, 4.918E-1, 4.814E-1,  &
!                      4.585E-1, 4.508E-1, 4.404E-1, 4.300E-1, 4.400E-1,  &
!                      4.600E-1, 4.696E-1, 4.500E-1, 5.700E-1/

!    New  Refractive indices for EC at RRTMG Wavelengths
!    	Source	lamda	xnreal_ec	xnimag_ec
!   	C&C Values
!          3.4615	2.089	1.070
!    		2.7885	2.014	0.939
!   		2.325	1.962	0.843
!    		2.046	1.950	0.784
!   	Bond values	
!           1.784	1.940	0.760
!     		1.4625	1.930	0.749
!		    1.2705	1.905	0.737
!		    1.0101	1.870	0.726
!  B&B  Values	
!          0.7016	1.85	0.71
!   	   0.53325  1.85    0.71
!		   0.38815  1.85	0.71
!		   0.299	1.85	0.71
!		   0.2316	1.85	0.71
!	 C & C values	
!     	   8.24	    2.589	1.771
!References:	
! Bond  Personal Communication from Tami Bond
! B&B Bond, T.C. & R.W. Bergstrom (2006) Light absorption by 
! Carbonaceous Particles: An investigative review,
! Aerosol Science and Technology. Vol. 40. pp 27-67
!
! C&C Chang,H and T.T. Charalmpopoulos (1990) Determination of the
! wavelength dependence of refractive indices of flame soot, 
! Proceeding of the Royal Society of London A, Vol. 430, pp 577-591.
! FSB new values

! FSB elemental carbon - soot like   

      real, parameter :: xnreal_ec(nspint_sw) = (/ 2.089, 2.014, 1.962, 1.950, 1.940, 1.930, 1.905,   &
                                                   1.870, 1.85,  1.85,  1.85,  1.85,  1.85,  2.589 /)
      real, parameter :: xnimag_ec(nspint_sw) = (/ 1.070, 0.939, 0.843, 0.784, 0.760, 0.749, 0.737,   &
                                                   0.726, 0.71,  0.71,  0.71,  0.71,  0.71,  1.771 /)	

! FSB water
      real, save :: xnreal_h2o(nspint_sw) = (/ 1.408, 1.324, 1.277, 1.302, 1.312, 1.321, 1.323,       &
                                               1.327, 1.331, 1.334, 1.340, 1.349, 1.362, 1.260 /)
      real, save :: xnimag_h2o(nspint_sw) = (/ 1.420E-2, 1.577E-1, 1.516E-3, 1.159E-3, 2.360E-4,      &
                                               1.713E-4, 2.425E-5, 3.125E-6, 3.405E-8, 1.639E-9,      &
                                               2.955E-9, 1.635E-8, 3.350E-8, 6.220E-2 /)
      

! FSB Begin code ======================================================
      
      bextsum  = 0.0
      bscatsum = 0.0
      bsgsum   = 0.0
      do m = 1, nmode
! FSB calculate volumes [ m**3 / m**3 ]
! FSB the reciprocal densities have been scaled to [ m**3 / ug ]      

         MVOL_ws(m)  = rhows1  * INMASS_ws(m)
         MVOL_in(m)  = rhoin1  * INMASS_in(m)
         MVOL_ec(m)  = rhoec1  * INMASS_ec(m)
         MVOL_ss(m)  = rhoss1  * INMASS_ss(m)
         MVOL_h2o(m) = rhoh2o1 * INMASS_h2o(m)
      
         VOLSHELL(m) = MVOL_ws(m) + MVOL_in(m) + MVOL_ss(m) + MVOL_h2o(m)
         VOLCOR(m)   = MVOL_ec(m)
!        VOL(m)      = VOLSHELL(m) + VOLCOR(m) ! VOL is total volume

         if ( VOLCOR(m) .gt. 0.0 ) then
! FSB EC is present  
!       calculate the ratio of core to shell volume
!       take cube root for scaling the diameter of 
!       the core to that of the shell.
       
!           dfac        = ( VOLCOR(m) / VOL(m) ) ** one3rd
            dfac        = ( VOLCOR(m) / ( VOLSHELL(m) + VOLCOR(m) ) ) ** one3rd
!           dfac        = ( VOLCOR(m) / ( VOLSHELL(m) + VOLCOR(m) ) )
! FSB Set shell and core diameters        
            DGNSHELL(m) = INDGN(m)
            DGNCORE(M)  = dfac * INDGN(m)
! FSB note that VOLSHELL(m) is the total volume when EC is not present
         end if
        
! internal mixture of non-EC species.

! modal real refractive index No EC 
         nr(m) = (MVOL_ws(m)  * xnreal_ws(ns) +              &
                  MVOL_in(m)  * xnreal_in(ns) +              &
                  MVOL_ss(m)  * xnreal_ss(ns) +              &
!                 MVOL_h2o(m) * xnreal_h2o(ns)) / VOL(m)
                  MVOL_h2o(m) * xnreal_h2o(ns)) / VOLSHELL(m)

! modal imaginary refractive index no EC
         ni(m) = (MVOL_ws(m)  * xnimag_ws(ns) +              &
                  MVOL_in(m)  * xnimag_in(ns) +              &
                  MVOL_ss(m)  * xnimag_ss(ns) +              &
!                 MVOL_h2o(m) * xnimag_h2o(ns)) / VOL(m)
                  MVOL_h2o(m) * xnimag_h2o(ns)) / VOLSHELL(m)

         if ( VOLCOR(m) .gt. 0.0) then

! FSB calculate the complex refractive indices for the CORE and
!     the SHELL for case when and EC core is assumed to exist.

            refcor(m)   = cmplx( xnreal_ec(ns), xnimag_ec(ns) ) 
            refshell(m) = cmplx(nr(m), ni(m) )
! FSB do BHCOAT case        
            CALL aero_optical_CS( LAMDA_SW(ns), refcor(m), refshell(m),   &
                                  VOLCOR(m),VOLSHELL(m), DGNCORE(m),      &
                                  DGNSHELL(m), INSIG(m),                  &                            
                                  bext, bscat, gfac, succesS )
!        else if ( VOL(m) .gt. 0.0) then
         else if ( VOLSHELL(m) .gt. 0.0) then
! FSB do BHMIE case for the case when EC is not present.
            crefin(m) = cmplx(nr(m), ni(m) )
!           CALL aero_optical2( LAMDA_SW(ns), crefin(m), VOL(m),          &
            CALL aero_optical2( LAMDA_SW(ns), crefin(m), VOLSHELL(m),     &
                                INDGN(m), INSIG(m),                       &
                                bext, bscat, gfac, success )
         else
            bext = 0.0
            bscat = 0.0
            gfac = 0.0
         end if       

! FSB sum for total values
         bextsum  = bextsum + bext
         bscatsum = bscatsum +bscat              
         bsgsum   = bsgsum + bscat * gfac
! FSB get history 
!        bext_wm(ns,m)  = bext
!        bscat_wm(ns,m) = bscat
!        gfac_wm(ns,m)  = gfac          
      end do ! loop over modes
           
! FSB construct output variables      
      tauaer = bextsum * delta_z
      waer   = bscatsum / bextsum      
      gaer   = bsgsum / bscatsum

!     Write out modal values.

!     write(100,*) ' lamda mode bext bscat gaer '
!     m=1
!     write(100,*) lamda(ns), m, bext_wm(ns,m),bscat_wm(ns,m), gfac_wm(ns,m) 
!     m=2
!     write(100,*) lamda(ns), m, bext_wm(ns,m),bscat_wm(ns,m), gfac_wm(ns,m) 
!     m=3
!     write(100,*) lamda(ns), m, bext_wm(ns,m),bscat_wm(ns,m), gfac_wm(ns,m) 

      end subroutine get_aerosol_Optics_RRTMG_SW

END MODULE module_twoway_ra_rrtmg_sw
