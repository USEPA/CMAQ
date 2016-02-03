! FSB REvised  Mie calculations  02/09/2011

MODULE module_twoway_ra_rrtmg_sw

use parrrsw, only : nbndsw, naerec, ngptsw
use mcica_subcol_gen_sw, only : mcica_subcol_sw
use module_ra_rrtmg_lw, only : inirad, relcalc, reicalc
use rrtmg_sw_rad, only: rrtmg_sw

private
public :: RRTMG_SWRAD2

contains

!------------------------------------------------------------------
   SUBROUTINE RRTMG_SWRAD2(                                       &
                       rthratensw,                                &
                       swupt, swuptc, swdnt, swdntc,              &
                       swupb, swupbc, swdnb, swdnbc,              &
!                      swupflx, swupflxc, swdnflx, swdnflxc,      &
                       swcf, gsw,                                 &
                       xtime, gmt, xlat, xlong,                   &
                       radt, degrad, declin,                      &
                       coszr, julday, solcon,                     &
                       albedo, t3d, t8w, tsk,                     &
                       p3d, p8w, pi3d, rho3d,                     &
                       dz8w, cldfra3d, r, g,                      &
                       icloud, warm_rain,                         &
                       f_ice_phy, f_rain_phy,                     &
                       xland, xice, snow,                         &
                       qv3d, qc3d, qr3d,                          &
                       qi3d, qs3d, qg3d,                          &
                       f_qv, f_qc, f_qr, f_qi, f_qs, f_qg,        &
                       tauaer300,tauaer400,tauaer600,tauaer999,   & ! czhao 
                       gaer300,gaer400,gaer600,gaer999,           & ! czhao 
                       waer300,waer400,waer600,waer999,           & ! czhao 
                       aer_ra_feedback,                           &
!jdfcz                 progn,prescribe,                           &
                       progn,                                     &
                       qndrop3d,f_qndrop,                         & !czhao
                       ids,ide, jds,jde, kds,kde,                 & 
                       ims,ime, jms,jme, kms,kme,                 &
                       its,ite, jts,jte, kts,kte,                 &
                       swupflx, swupflxc, swdnflx, swdnflxc,      &
! ** FSB Add aerosol input ffrom CMAQ
                       nmode,                                     &
                       mass_ws_i, mass_ws_j, mass_ws_k,           &
                       mass_in_i, mass_in_j, mass_in_k,           &
                       mass_ec_i, mass_ec_j, mass_ec_k,           &
                       mass_ss_i, mass_ss_j, mass_ss_k,           &
                       mass_h2o_i, mass_h2o_j, mass_h2o_k,        &
                       dgn_i, dgn_j, dgn_k,                       &
                       sig_i, sig_j, sig_k,                       &
                       gtauxar                                    &
                                                                  )
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   LOGICAL, INTENT(IN )      ::        warm_rain
!
   INTEGER, INTENT(IN )      ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN )      ::        ICLOUD
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
   REAL, INTENT(IN  )   ::                                   R,G
!
! Optional
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) ::                                         &
                                                        CLDFRA3D, &
                                                            QV3D, &
                                                            QC3D, &
                                                            QR3D, &
                                                            QI3D, &
                                                            QS3D, &
                                                            QG3D, &
                                                        QNDROP3D

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
                                       SWUPT,SWUPTC,SWDNT,SWDNTC, &
                                       SWUPB,SWUPBC,SWDNB,SWDNBC

! Layer shortwave fluxes (including extra layer above model top)
! Vertical ordering is from bottom to top (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(OUT) ::                                 &
                               SWUPFLX,SWUPFLXC,SWDNFLX,SWDNFLXC

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

   real, dimension(ims:ime, kms:kme, jms:jme, 9 ), intent(out) :: gtauxar


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
                                                            QS1D, &
                                                            QG1D, &
                                                          qndrop1d 

! Added local arrays for RRTMG
    integer ::                                              ncol, &
                                                            nlay, &
                                                            icld, &
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
                                                             rel, &
                                                             rei, &
                                                         cldfrac, &
                                                         relqmcl, &
                                                         reicmcl
    real, dimension( nbndsw, 1, kts:kte+1 )  ::           taucld, &
                                                          ssacld, &
                                                          asmcld, &
                                                          fsfcld
    real, dimension( ngptsw, 1, kts:kte+1 )  ::          cldfmcl, &
                                                         clwpmcl, &
                                                         ciwpmcl, &
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
                                                         swdflxc
    real, dimension( 1, kts:kte+1 )  ::                     swhr, &
                                                           swhrc

    real, dimension ( 1 ) ::                                tsfc, &
                                                              ps, &
                                                          coszen
    real ::                                                   ro, &
                                                              dz, &
                                                           adjes, &
                                                            scon
    integer ::                                            dyofyr

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
                                          reliq, &     ! effective drop radius (microns)
                                          reice        ! ice effective drop size (microns)
    real :: gliqwp, gicewp, gravmks

!
!    REAL   ::  TSFC,GLW0,OLR0,EMISS0,FP
    REAL   ::  FP

!    real, dimension(1:ite-its+1 )          ::   clat     ! latitude in radians for columns
    real :: coszrs                      ! Cosine of solar zenith angle for present latitude 
    logical :: dorrsw                   ! Flag to allow shortwave calculation

    real, dimension (1) :: landfrac, landm, snowh, icefrac

    integer :: pcols, pver

    REAL :: XT24, TLOCTM, HRANG, XXLAT

! fsb local variables for setting tauaer, sssaaer, asmaer
! FSB local variabble for C0AQ aerosols
   REAL, DIMENSION (nmode) :: INMASS_ws, INMASS_in,               &
         INMASS_ec, INMASS_ss,  INMASS_h2o,  INDGN, INSIG
   REAL  xtauaer, waer, gaer
   REAL delta_z ! local layer thickness

    INTEGER modes
    INTEGER :: i,j,K, na
    LOGICAL :: predicate

!------------------------------------------------------------------
#ifdef WRF_CHEM
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
!
! Do shortwave by default, deactivate below if sun below horizon
         dorrsw = .true.

! Cosine solar zenith angle for current time step
!
! xt24 is the fractional part of simulation days plus half of radt expressed in 
! units of minutes
! julian is in days
! radt is in minutes
         xt24 = mod(xtime+radt*0.5,1440.)
         tloctm = gmt + xt24/60. + xlong(i,j)/15.
         hrang = 15. * (tloctm-12.) * degrad
         xxlat = xlat(i,j) * degrad
!         clat(i) = xxlat
         coszrs = sin(xxlat) * sin(declin) + cos(xxlat) * cos(declin) * cos(hrang)
         coszr(i,j) = coszrs

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
                     qi1d(k) = qs3d(i,k,j)
                     qc1d(k) = qc3d(i,k,j)
                     qi1d(k) = max(0.,qi1d(k))
                     qc1d(k) = max(0.,qc1d(k))
                  ENDDO
               ENDIF
            ENDIF

         ENDIF

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

! Select cloud liquid and ice optics parameterization options
! For passing in cloud optical properties directly:
!         icld = 2
!         inflgsw = 0
!         iceflgsw = 0
!         liqflgsw = 0
! For passing in cloud physical properties; cloud optics parameterized in RRTMG:
         icld = 2
         inflgsw = 2
         iceflgsw = 3
         liqflgsw = 1

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

! Get ozone profile including amount in extra layer above model top
         call inirad (o3mmr,plev,kts,kte)

         do k = kts, kte+1
            o3vmr(ncol,k) = o3mmr(k) * amdo
         enddo

! Set surface albedo for direct and diffuse radiation in UV/visible and
! near-IR spectral regions
         asdir(ncol) = albedo(i,j)
         asdif(ncol) = albedo(i,j)
         aldir(ncol) = albedo(i,j)
         aldif(ncol) = albedo(i,j)

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
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
!      write(0,*) 'sw prescribe aerosol',maxval(qndrop3d)
!jdfcz     endif
    else  ! progn   
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
    endif
  else   !progn 
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
  endif

! following Kristjansson and Mitchell
      call reicalc(ncol, pcols, pver, tlay, reice)

      if (i==80.and.j==30) then
#if defined( DM_PARALLEL ) && ! defined( STUBMPI) 
      if( PRESENT( progn ) ) write(0,*) 'aerosol indirect',progn
      write(0,*)'sw water eff radius',reliq(ncol,10),reliq(ncol,20)   ! ,reliq(ncol,25)
      write(0,*)'sw ice eff radius',reice(ncol,10),reice(ncol,20)   ! ,reice(ncol,25)
#endif
      endif

! Limit upper bound of reice for Fu ice parameterization and convert
! from effective radius to generalized effective size (*1.0315; Fu, 1996)
            if (iceflgsw .eq. 3) then
               do k = kts, kte
                  reice(ncol,k) = reice(ncol,k) * 1.0315
                  reice(ncol,k) = min(140.0,reice(ncol,k))
               end do
            endif

! Set cloud physical property arrays
            do k = kts, kte
               clwpth(ncol,k) = cliqwp(ncol,k)
               ciwpth(ncol,k) = cicewp(ncol,k)
               rel(ncol,k) = reliq(ncol,k)
               rei(ncol,k) = reice(ncol,k)
            enddo

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
         rel(ncol,kte+1) = 10.
         rei(ncol,kte+1) = 10.
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

         call mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, play, &
                       cldfrac, ciwpth, clwpth, rei, rel, taucld, ssacld, asmcld, fsfcld, &
                       cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, &
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

         if ((5 <= nb) .and.(nb <= 13)) then
            gtauxar (i,k,j,nbndsw-nb) = xtauaer
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

#ifdef WRF_CHEM
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

! Call RRTMG shortwave radiation model

         call rrtmg_sw &
            (ncol    ,nlay    ,icld    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc   , &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,cldfmcl , &
             taucmcl ,ssacmcl ,asmcmcl ,fsfcmcl , &
             ciwpmcl ,clwpmcl ,reicmcl ,relqmcl , &
             tauaer  ,ssaaer  ,asmaer  ,ecaer   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc)

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
            swdnbc(i,j)    = swdflxc(1,1)
         endif

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
         gtauxar (i,:,j,:) = 0.0
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
         endif

      endif
!
      end do i_loop
   end do j_loop                                           


!-------------------------------------------------------------------

   END SUBROUTINE RRTMG_SWRAD2

!------------------------------------------------------------------
      Subroutine get_aerosol_Optics_RRTMG_SW( ns,nmode,delta_z,    &
            INMASS_ws, INMASS_in, INMASS_ec, INMASS_ss,             &
            INMASS_h2o,  INDGN, INSIG,                              &
                  tauaer, waer, gaer                          )
! This version is set up to be used with RRTMG_SW <<<<<<<<
!     wavelenght is calculated internally
! FSB This routine calculates the aerosol information ( tauaer, waer, 
!     gaer, needed to calculate the solar radiation)  The calling 
!     program specifies the location ( row, column, layer, 
!     layer thicknes, and wave length for the calculation.   
! FSB 02/09/2011 Modifications made to subroutine ghintBH.
! FSB Input variables:
      
      integer, intent(in) ::  ns      ! index for wavelength should be 
                                      ! between 1 and 14. <<< RRTMG_SW
      integer, intent(in) ::  nmode   ! should be 3
       real, intent(in)   ::  delta_z ! layer thickness [m]
! FSB mode types 
!     nmode = 1 Aitken
!     nmode = 2 accumulation
!     nmode = 3 coarse 
! FSB modal mass concentration by species  [ ug / m**3]  NOTE:  MKS
      real, intent(in) ::  INMASS_ws(nmode)   ! water soluble
      real, intent(in) ::  INMASS_in(nmode)   ! insolugle
      real, intent(in) ::  INMASS_ec(nmode)   ! elemental carbon or soot like
      real, intent(in) ::  INMASS_ss(nmode)   ! sea salt
      real, intent(in) ::  INMASS_h2o(nmode)  ! water
      
      real, intent(in) ::  INDGN( nmode)      ! geometric mean diameter [ m ] NOTE: MKS      
      real, intent(in) ::  INSIG( nmode)      ! geometric standard deviation
!... output aerosol radiative property  [dimensionless]
!
      real, intent(out) ::  tauaer   ! aerosol extinction optical depth
      real, intent(out) ::  waer     ! aerosol single scattering albedo
      real, intent(out) ::  gaer     ! aerosol assymetry parameter

! FSB Internal variables

! FSB total modal mass concentration [ g/ cm **3 ]     
! FSB in this RRTMG version MMASS is in [ ug / m**3 ]
      real :: MMASS(nmode)
!
      integer nspint ! number of spectral intervals
       parameter( nspint = 14)


      real NR(nmode), NI(nmode) ! refractive indices       
!      real Mdens1(nmode) ! reciprocal density concentration [ cm**3 / g ]
      real DGN(nmode)    ! modal geometric mean diameter [cm]
      real SIG(nmode)    ! modal geometric standard deviation
       
! FSB modal mass concentration by species  [ ug / m**3]  NOTE:  MKS
      real MASS_ws(nmode)   ! water soluble
      real MASS_in(nmode)   ! insolugle
      real MASS_ec(nmode)   ! elemental carbon or soot like
      real MASS_ss(nmode)    ! sea salt
      real MASS_h2o(nmode)  ! water

! FSB Modal volumes [ m**3 / cm **3 ]
      real MVOL_ws(nmode)  ! water soluble
      real MVOL_in(nmode)   ! insolugle
      real MVOL_ec(nmode)   ! soot like
      real MVOL_ss(nmode)   !sea salt
      real MVOL_h2o(nmode)  ! water

      real VOL(nmode)  ! total modal volume[ m** 3 / m**3]
      real MODULUS(nmode)
!      real mdgn(nmode)
!      real frac_aec(nmode)    ! volume fraction of soot like aerosol
! FSB additions for external mixtures    
      real sig_scat, sig_abs  ! specific scattering and absorption cross sections
                              ! for diesel soot
                              ! reference schnaiter et al. jour. of
                              ! aerosol science vol. 34, pp1421-1444, 2003
       parameter(sig_scat = 1.6e-6, sig_abs = 6.6e-6) ! [ m**2 / ug ] values at 550 [nm]
!!       parameter(sig_scat = 1.6d-2, sig_abs = 6.6d-2) ! [ cm**2 / ug ] values at 550 [nm]
      
      
!      real Tot_mass_ec ! total mass of elemental carbon [ ug / m **3 ] 
      real Tot_mass  ! total aerosol mass [ ug / m **3 ]
      real angstrm_scat, angstrm_abs
       parameter( angstrm_scat = 1.1d0, angstrm_abs = 1.9d0) ! angstrom exponents
!      real wavescale_scat, wavescale_abs ! scaling for scattering 
                                                         ! and absorption
      real l550                                          ! 550 [micro-m]
       parameter( l550 = 0.550 )
!      real rat550                                        ! l550 / lamda

      real dsootdens                     ! diesel soot density 
       parameter( dsootdens = 1.7 )      ! [ g /cm ** 3] schnaiter et al. (2003)
                                         ! page 1430 table 2
!      real qqscat, qqabs ! specific cross sections at  various
                                         ! spectral intervals [ m**2 / g]
!      real ndqscat                       ! non dimensionsal qqscat 
!     real asmfac_soot           ! asymmetry factor for soot
!     real*8 asmfac_tot
   
!      real    bscat_s ! scattering coefficient for soot [1/m]
!      real    babs_s  ! absorption ceofficient for soot [1/m]
!      real    bext_s  ! extinction coefficient for soot [1/m
                      
      integer m ! loop index

!      real    aermmr   ! mass mixing ratio of aerosols
!      real    aerMMr_ec ! mass mixing ratio for ec

      real    bext                ! extinction coefficient [cm**2 / g]
      real    bscat               ! scattering coefficient [cm**2 / g]
      real    g_bar              ! asymmetry factor    
!      logical do_external(nmode)

!...component densities [ g/ cm**3 ]

       real        rhows   !  bulk density of water soluble aerosol
       parameter (   rhows = 1.8 )

       real        rhoin  ! bulk density forinsoluble aerosol 
       parameter (   rhoin = 2.2 )

       real        rhoec  ! bulk density for soot aerosol
       parameter (   rhoec = 1.7 )

       real        rhoh2o   !  bulk density of aerosol water
       parameter (   rhoh2o = 1.0 )
       
       real        rhoss    ! bulk density of seasalt
       parameter(    rhoss = 2.2 )

! FSB scale factor for volume calculation
!            1.0d-12 * [ cm**3 / g] -> [ m** 3 / ug ]
      real    scalefactor  
      parameter  ( scalefactor = 1.0e-12 )

! FSB reciprocal component densities[ m ** 3 / ug ] 
 
       real         rhows1    !   water soluble aerosol
       parameter (   rhows1 = scalefactor / rhows )

       real        rhoin1    ! insoluble aerosol 
       parameter (   rhoin1 = scalefactor /  rhoin )

       real        rhoec1    !  soot aerosol
       parameter (   rhoec1 = scalefactor / rhoec )

       real         rhoh2o1   !  aerosol water
       parameter (   rhoh2o1 = scalefactor / rhoh2o  )
       
       real        rhoss1    !  seasalt
       parameter(    rhoss1 = scalefactor / rhoss )      

! FSB Band numbers and wavelengths for RRTMG_SW   
      integer, save :: Band(nspint)
      data Band /1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14/ 
      
      real, save :: LAMDA(nspint) ! wavelength [ um ]      
      data LAMDA /3.4615, 2.7885, 2.325, 2.046, 1.784, 1.4625, 1.2705,    &
                  1.0101, 0.7016, 0.53325, 0.38815, 0.299, 0.2316, 8.24/
    
! *** refractive indices

! *** Except as otherwise noted reference values of refractive 
!     indices  for aerosol particles are from the OPAC Data base. 
!     Hess, M. P. Koepke, and I. Schult, Optical properties of
!     aerosols and clouds: The software package OPAC, Bulletan of
!     the American Meteorological Society, Vol 79, No 5, 
!     pp 831 - 844, May 1998. 
!     OPAC is a downloadable data set of optical properties of 
!     10 aerosol components, 6 water clouds and 3 cirrus clouds 
!     at UV, visible and IR wavelengths
!     www.lrz-muenchen.de/~uh234an/www/radaer/opac.htm


! FSB water soluble
      real, save ::  xnreal_ws(nspint), xnimag_ws(nspint)  ! water soluble    

      data xnreal_ws  /	1.443, 1.420, 1.420, 1.420, 1.463, 1.510, 1.510,  &
                        1.520, 1.530, 1.530, 1.530, 1.530, 1.530, 1.710/ 
      data xnimag_ws /5.718E-3, 1.777E-2, 1.060E-2, 8.368E-3, 1.621E-2,   &
                      2.198E-2, 1.929E-2, 1.564E-2, 7.000E-3, 5.666E-3,   &
                      5.000E-3, 8.440E-3, 3.000E-2, 1.100E-1/

! FSB sea salt      
      real, save ::   xnreal_ss(nspint), xnimag_ss(nspint) ! sea salt

      data xnreal_ss /1.480, 1.534, 1.437, 1.448, 1.450, 1.462, 1.469,    &
                      1.470, 1.490, 1.500, 1.502, 1.510, 1.510, 1.510/
      data xnimag_ss /1.758E-3, 7.462E-3, 2.950E-3, 1.276E-3, 7.944E-4,   &
                      5.382E-4, 3.754E-4, 1.498E-4, 2.050E-7, 1.184E-8,   &
                      9.938E-8, 2.060E-6, 5.000E-6, 1.000E-2/

! FSB insoluble      
      real, save ::  xnreal_in(nspint), xnimag_in(nspint) ! dust like 

      data xnreal_in /1.272, 1.168, 1.208, 1.253, 1.329, 1.418, 1.456,    &
                      1.518, 1.530, 1.530, 1.530, 1.530, 1.530, 1.470/
      data xnimag_in /1.165E-2, 1.073E-2, 8.650E-3, 8.092E-3, 8.000E-3,   &
                      8.000E-3, 8.000E-3, 8.000E-3, 8.000E-3, 8.000E-3,   &
                      8.000E-3, 8.440E-3, 3.000E-2,9.000E-2/

! FSB elemental carbon - soot like      
      real, save ::  xnreal_ec(nspint), xnimag_ec(nspint) ! soot like

      data xnreal_ec /1.877, 1.832, 1.813, 1.802, 1.791, 1.769, 1.761,    &
                      1.760, 1.750, 1.740, 1.750, 1.738, 1.620, 2.120/
      data xnimag_ec/ 5.563E-1, 5.273E-1, 5.030E-1, 4.918E-1, 4.814E-1,   &
                      4.585E-1, 4.508E-1, 4.404E-1, 4.300E-1, 4.400E-1,   &
                      4.600E-1, 4.696E-1, 4.500E-1, 5.700E-1/

! FSB water
      real, save ::  xnreal_h2o(nspint), xnimag_h2o(nspint)  ! water

      data xnreal_h2o/ 1.408, 1.324, 1.277, 1.302, 1.312, 1.321, 1.323,   &
                       1.327, 1.331, 1.334, 1.340, 1.349, 1.362, 1.260/
      data xnimag_h2o/ 1.420E-2, 1.577E-1, 1.516E-3, 1.159E-3, 2.360E-4,  &
                       1.713E-4, 2.425E-5, 3.125E-6, 3.405E-8, 1.639E-9,  &
                       2.955E-9, 1.635E-8, 3.350E-8, 6.220E-2/
      
      
      
      


!
! FSB Begin code ======================================================
       
      
       do m = 1, nmode
 ! FSB set local variables      
        MASS_ws(m)  = INMASS_ws(m)
        MASS_in(m)  = INMASS_in(m)
        MASS_ec(m)  = INMASS_ec(m)
        MASS_ss(m)  = INMASS_ss(m)
        MASS_h2o(m) = INMASS_h2o(m)
        DGN(m)      = INDGN(m)
        SIG(m)      = INSIG(m)
! FSB calculate volumes [ m**3 / m**3 ]
! FSB the reciprocal densities have been scaled to [ m**3 / ug ]      

        MVOL_ws(m)  = rhows1  * MASS_ws(m)
        MVOL_in(m)  = rhoin1  * MASS_in(m)
        MVOL_ec(m)  = rhoec1  * MASS_ec(m)
        MVOL_ss(m)  = rhoss1  * MASS_ss(m)
        MVOL_h2o(m) = rhoh2o1 * MASS_h2o(m)
      end do 
      
     Tot_mass = 0.0
      
! FSB sum to get total modal volumes and total mass
      do m = 1, nmode
      
        VOL(m) = MVOL_ws(m) + MVOL_in(m) + MVOL_ss(m) +  &
                 MVOL_ec(m) + MVOL_h2o(m)
       
        MMASS(m) = MASS_ws(m) + MASS_in(m) + MASS_ss(m)  &
                  + MASS_ec(m)+ MASS_h2o(m)     
        
        Tot_mass = Tot_mass + MMASS(m)
       
      end do ! loop on modes
      
! FSB determine fraction of elemental carbon      
!      do m = 1, nmode
!        do_external(m) = .false.      
!        frac_aec(m) = MVOL_ec(m) / VOL(m)
!        if( frac_aec(m) .ge. 0.60 ) do_external(m) = .true.
!      end do  ! loop on modes


! FSB now calculate the refractive indices 
     
      do m = 1, nmode
     
        
       ! all internal mixture
       
       ! modal real refractive index
        nr(m) = (MVOL_ws(m) * xnreal_ws(ns) +                             &
           MVOL_in(m) * xnreal_in(ns) + MVOL_ss(m) * xnreal_ss(ns)        &
          + MVOL_h2o(m) * xnreal_h2o(ns) + MVOL_ec(m) * xnreal_ec(ns)     &
          ) / VOL(m)

       ! modal imaginary refractive index 
        ni(m) = (MVOL_ws(m) * xnimag_ws(ns) +                             &
         MVOL_in(m) * xnimag_in(ns)   + MVOL_ss(m) * xnimag_ss(ns)        &
          + MVOL_h2o(m) * xnimag_h2o(ns) +  MVOL_ec(m) * xnimag_ec(ns)    &
         ) / VOL(m)

       ! modulus of modal refracive index
        MODULUS(m) = sqrt( nr(m) * nr(m) + ni(m) * ni(m)  ) 

     end do ! loop over modes 

           CALL aero_optical( LAMDA(ns),  &
              NMODE,                      &
              NR, NI,                     &
              MODULUS,                    &
              VOL, DGN, SIG,              &
              bext, bscat, g_bar)
     
        tauaer = bext  * delta_z  ! contributionto aerosol optical depth

        waer   = bscat / bext 
        gaer   = g_bar
        faer   = gaer * gaer
     
      return
      end subroutine get_aerosol_Optics_RRTMG_SW

! --------------------------------------------------------------------------

! ------------------------------------------------------------------------------
!     /////////////////////////////////////////////////////////////////
      
       subroutine aero_optical(  lamda_in,  &
                  nmode,                    &
                  nr, ni,                   &
                  modulus,                  &
                  Vol,  dgn, sig,           &
                  bext, bscat, g_bar )
     
! *** calculate the extinction and scattering coefficients and
!     assymetry factors for each wavelength as a sum over the 
!     individual lognormal modes. Each mode may have a different 
!     set of refractive indices.

      IMPLICIT NONE
! *** input variables
      real lamda_in      ! wavelengths  [micro-m]
                      
      INTEGER nmode  ! number of lognormal modes
      real nr( nmode), ni(nmode) ! real(r8) and imaginary 
                                ! refractive indices
      real modulus(nmode) ! modulus of refracive index                          
      real Vol(nmode) ! modal aerosol volumes [m**3 /m**3]
      real dgn(nmode) ! geometric mean diameters 
                      ! for number distribution [ m]
      real sig(nmode) ! geometric standard deviation 
      
      
! *** output variables 
      real bext  ! extinction coefficient [ 1 / m ]
      real bscat ! scattering coefficient [ 1 / m ]
      real g_bar  ! assymetry factor for Mie and molecular scattering
      
      
! *** internal variables
      INTEGER  j  ! loop index
      real lamda  ! wavelength [ m]
!      real xlnsig(nmode) ! natural log of geometric standard deviations      
      real beta_Sc, bsc  !aerosol scattering coefficient 
 
      real beta_Ex       ! aerosol extinction coefficients       
      real G             ! modal aerosol assymetry factors
      real sum_g
      real VLX, DGX, SIGX, NRX, NIX, LSIGX
      real lamdam1 ! 1/ lamda
      real alphav ! Mie size parameter
      real vfac
      real modalph
      real pi
      PARAMETER( pi = 3.14159265359)
       
       
! *** coded 09/08/2004 by Dr. Francis S. Binkowski
! FSB Modified for RRTMG version December 2009.
! FSB modified 10/06/2004, 10/12/2004, 10/18/2005
! FSB 01/12/2006
!     Formerly Carolina Environmental Program
! FSB now the Institute for the Environment
!     University of North Carolina at Chapel Hill
!     email: frank_binkowski@unc.edu


! *** initialize variables
       lamda = 1.0e-6 * lamda_in ! lamda now in [ m ]
       bext  = 0.0
       bscat = 0.0
       sum_g = 0.0
       lamdam1 = 1.0 / lamda
        
!     write(20,*) ' inside aero_optical', ' lamda = ', lamda
      
       DO j = 1, nmode
!    calculate the extinction and scattering coefficients
!    for each mode 
!        write(20,*) ' j = ', j
        VLX   = Vol(j)
        DGX   = dgn(j)
        SIGX  = sig(j) 
        NRX   = nr(j)
        NIX   = ni(j)
        LSIGX = log(SIGX)
       
!     calculate Mie size parameter for volume distribution
!     exp(3.0 * xlnsig*xlnsig)  converts dgn to dgv (volume diameter)
          alphav =  pi * DGX *  &
                           exp(3.0 * LSIGX * LSIGX) * lamdam1
       modalph = alphav * modulus(j)   
       
!      write(20,*) j, alphav, modalph, modulus(j)
!       write(20,*) j, alphav, modalph, NRX, NIX
      

        CALL ghintBH(NRX, NIX, alphav, LSIGX, beta_EX, beta_Sc, G)            

! *** fast_optics  returns the normalized values
!     Calculate the actual extinction and scattering coefficients 
!     by multplying by the modal volume and dividing by the wavelength
         
         
         vfac  =  VLX * lamdam1

!        write(20,*)' vfac = ', vfac
!        write(20,*)' G = ', G
!        write(20,*) ' beta_Ex = ', beta_Ex
!        write(20,*) ' beta_Sc = ', beta_Sc
        
! *** sum to get total extinction and scattering 
!     and contribution to the overal assymetry factor

          bext    = bext  + vfac * beta_Ex
          bsc     = vfac * beta_Sc
          bscat   = bscat + bsc          
          sum_g   = sum_g + bsc * G


       END DO  ! loop on modes  
       
! *** calculate combined assymetry factor for all modes  

       g_bar = sum_g / bscat ! changed to divide by bscat
!        write(30,*) ' inside aero_optical '       
!        write(30,*) ' g_bar = ', g_bar
!       write(20,*) ' bext = ', bext
!       write(20,*) ' bscat = ', bscat
!       write(20,*) ' gbar = ', g_bar 
      RETURN
      
      END SUBROUTINE ! aero_optical



! /////////////////////////////////////////////////////////////////////


! ***************
       subroutine ghintBH(nr, ni, alfv_in, xlnsig,  bext, bscat, g) 

! FSB This version includes a call to Wiscombe (1980) for values
!     of modulus * alfv less than or equal to 0.1
!
! FSB This version uses the large limit code for alfv .gt. 30.0

! FSB For  certian ranges of nr and ni / (nr -1) the EF method is used
!     see subroutines  getqext_integ and getqext_EF below.

!       calculates the extinction and scattering coefficients 
!       normalized by wavelength and total particle volume
!       concentration for a log normal particle distribution 
!       with the logarithm of the geometri!  standard deviation
!       given by xlnsig. The integral of the
!       asymmetry factor g is also calculated. 

!  *** Does Gauss-Hermite quadrature of Qext / alfa & Qscat / alfa
!      over log normal distribution using  symmetric  points
!
!      Reference:  Abramowitz, M & I.A. Stegun,  Handbook of Mathematical
!      Functions with Formulas, Graphs and Mathematical Tables,
!      Nat. Bur. Stand. Appl. Math. Ser. Vol 55, U.S. Gov't. 
!      Printing Office, Washington, D.C., 1965.
!

      implicit none

      real, intent(in)    :: nr, ni  ! indices of refraction
      real, intent(in)    :: alfv_in ! Mie parameter for dgv
      real, intent(in)    :: xlnsig  ! log of geometri!  standard deviation
!      integer, intent(in) :: IGH ! index for GH quadrature      
      real, intent(out)   :: bext    ! normalized extinction coefficient
      real, intent(out)   :: bscat   ! normalized scattering coefficient
      real, intent(out)   :: g       ! asymmetry factor
      
      complex :: crefin ! complex refractive index      
      real    :: modulus ! modulus of refractive index
      real    :: modalph ! modulus * alfv
      real    :: tau     ! ni / ( nr - 1)
      real    :: aa1     ! see below for definition

      real    :: alfaip, alfaim   ! Mie parameters at abscissas
      
      real    :: alfv
!  *** these are Qext/alfa and Qscat/alfv at the abscissas
      real    :: qalfip_e, qalfim_e ! extinction  
      real    :: qalfip_s, qalfim_s ! scattering
      real    :: gsalfp, gsalfm     ! scattering times asymmetry factor

! FSB define parameters 
      real, parameter :: pi = 3.14159265

      real, parameter :: sqrtpi = 1.772454 

      real, parameter :: sqrtpi1 = 1.0 / sqrtpi 

      real, parameter :: sqrt2 = 1.414214 

      real, parameter :: three_pi_two = 3.0 * pi / 2.0 

      real, parameter ::  const = three_pi_two * sqrtpi1 
      
      integer ::  i
      
 
 
! FSB variables neded for Gauss-Hermite numerical quadrature. 
      real    ::  sum_e,sum_s, sum_sg, xi,wxi,xf

! FSB full precision from Table 25.10 
! Gauss-Hermite abscissas and weights are from Abramowitz &
! Stegun, Table 25.10 page 924

      real,save :: ghxi_6(3),   ghwi_6(3)

! FSB six-point 
       data ghxi_6 / 0.436077411927617,                                  &
     &               1.335849074013597,                                  &
     &               2.350604973674492/
          
       data ghwi_6 / 7.246295952244e-01,                                 &
     &               1.570673203229e-01,                                 &
     &               4.530009905509e-03/



! FSB begin code

       alfv = alfv_in
       modulus = sqrt( nr*nr + ni*ni ) ! nr, ni are assumed to be well behaved.
       modalph = modulus * alfv
       crefin = cmplx(nr,ni)
       tau = ni / ( nr - 1.0 )

! FSB check for small and large limits

      if( modalph .lt. 0.1 ) then
      
 !  FSB use Wiscombe code for small modalph note change of call.       
        call SMALL4( alfv, crefin, xlnsig, bext, bscat, g )
        return
      end if ! small test

      if( alfv .gt. 30.0 ) then
      
! FSB use geometric optics for large alfv      
        call large_limit( nr, ni, alfv, xlnsig, bext, bscat, g)
        
        return
      end if ! large test

! FSB do Gauss-Hermite numerical quadrature 

!     Zero out  summation variables.     
      sum_e  = 0.0
      sum_s  = 0.0
      sum_sg = 0.0
      aa1 = sqrt2 * xlnsig ! This 1.0 / Sqrt( A ) in derivation
                           ! of the integral
                           ! where A = 1.0 / ( 2.0 * xlnsg**2 ) 
! Then alpha = alfv * exp[ u / sqrt(A) ]
! For Gauss-Hermite Quadrature u = xi 
! Therefore, xf = exp( xi / sqrt(A) ),
!  or xf = exp( xi * aa1 ) 


! use six point       
        do i= 1,3
          xi      = ghxi_6(i)
          wxi     = ghwi_6(i)
          xf      = exp( xi * aa1 )
       
         alfaip  = alfv * xf
         alfaim  = alfv / xf ! division cheaper than another exp()

! *** call suboutine fetch the efficiencies

         call getqext_integ(alfaip,nr, ni, modulus, tau,                 &
     &                      qalfip_e,qalfip_s, gsalfp)
         call getqext_integ(alfaim,nr, ni, modulus, tau,                 &
     &                       qalfim_e,qalfim_s, gsalfm)
       

         sum_e  = sum_e + wxi  * ( qalfip_e + qalfim_e )
         sum_s  = sum_s + wxi  * ( qalfip_s + qalfim_s )
           
        end do
! FSB calculate extinction and scattering coefficients.  
        bext  = const * sum_e  
        bscat = const * sum_s   
        
! FSB  get g at alfv after  using the mean values theorem for integrals.
         call getqesg(alfv, crefin, qalfip_e, qalfip_s, gsalfp)        
        g = gsalfp / qalfip_s 
      return
      end subroutine ghintBH

! FSB /////////////////////////////////////////////////////////////////      
      subroutine getqesg(xx, crefin, qextalf, qscatalf, gscatalfg)
      implicit none
      real :: xx, qextalf, qscatalf, gscatalfg
      COMPLEX   CREFIN
      
      real :: QEXT, QSCA, QBACK, G_MIE
      INTEGER ::    NANG, MXNANG, maxang
      
      PARAMETER  ( MXNANG = 10, NANG = 2 )
      PARAMETER( maxang = 2* MXNANG-1)
      
          CALL BHMIE(xx,CREFIN,QEXT,QSCA,QBACK,G_MIE)
            qextalf    = QEXT / xx
            qscatalf   = QSCA / xx
            gscatalfg  = qscatalf * G_MIE 
       
      RETURN
      END subroutine getqesg
 
! FSB /////////////////////////////////////////////////////////////////      
      subroutine getqext_integ(xx, nr, ni, modulus, tau,                 &
     & qextalf, qscatalf, gscatalfg)
      implicit none
      real :: xx, nr, ni, modulus, tau, qextalf, qscatalf, gscatalfg
      real :: qext_ef,qabs_ef,qscat_ef,g_ef, QABS
      real :: QEXT, QSCA, QBACK, G_MIE
      INTEGER ::    NANG, MXNANG, maxang
      
      PARAMETER  ( MXNANG = 10, NANG = 2 )
      PARAMETER( maxang = 2* MXNANG-1)
      COMPLEX   CREFIN
       
      
        if ( modulus .lt. 1.71 .and. tau .lt. 0.1) then

! FSB Use Evans & Fournier method here.       
          call getqext_EF(xx,nr,ni,qext_ef,qabs_ef,qscat_ef,g_ef)
        
          qextalf      = qext_ef/xx
          QABS         = qabs_ef/xx
          qscatalf     = qscat_ef/xx
          gscatalfg    = qscatalf * g_ef
       
         else
         
! FSB Use BHMIE method here.

          CREFIN = cmplx(nr,ni)
          CALL BHMIE(xx,CREFIN,QEXT,QSCA,QBACK,G_MIE)
            qextalf    = QEXT / xx
            qscatalf   = QSCA / xx
            gscatalfg  =  qscatalf * G_MIE 
         end if        
        
      RETURN
      END subroutine getqext_integ
!______________________________________________________________________
      SUBROUTINE BHMIE(X,REFREL,QEXT,QSCA,QBACK,GSCA)
      IMPLICIT NONE   
! Declare parameters:
! Note: important that MXNANG be consistent with dimension of S1 and S2
!       in calling routine!
      INTEGER MXNANG,NMXX
!      PARAMETER(MXNANG=1000,NMXX=15000)
      PARAMETER(MXNANG=10,NMXX=150000)
! Arguments:
      INTEGER NANG
      REAL GSCA,QBACK,QEXT,QSCA,X
      COMPLEX REFREL
      COMPLEX S1(2*MXNANG-1),S2(2*MXNANG-1)
! Local variables:
      INTEGER J,JJ,N,NSTOP,NMX,NN
      DOUBLE PRECISION CHI,CHI0,CHI1,DANG,DX,EN,FN,P,PII,PSI,PSI0,PSI1, &
                       THETA,XSTOP,YMOD
      DOUBLE PRECISION AMU(MXNANG),PI(MXNANG),PI0(MXNANG),PI1(MXNANG),  &
                       TAU(MXNANG)
      DOUBLE COMPLEX AN,AN1,BN,BN1,DREFRL,XI,XI1,Y
      DOUBLE COMPLEX D(NMXX)
! ***********************************************************************
! Subroutine BHMIE is the Bohren-Huffman Mie scattering subroutine
!    to calculate scattering and absorption by a homogenous isotropic
!    sphere.
! Given:
!    X = 2*pi*a/lambda
!    REFREL = (complex refr. index of sphere)/(real index of medium)
!    NANG = number of angles between 0 and 90 degrees
!           (will calculate 2*NANG-1 directions from 0 to 180 deg.)
!           if called with NANG<2, will set NANG=2 and will compute
!           scattering for theta=0,90,180.
! Returns:
!    S1(1 - 2*NANG-1) = -i*f_22 (incid. E perp. to scatt. plane,
!                                scatt. E perp. to scatt. plane)
!    S2(1 - 2*NANG-1) = -i*f_11 (incid. E parr. to scatt. plane,
!                                scatt. E parr. to scatt. plane)
!    QEXT = C_ext/pi*a**2 = efficiency factor for extinction
!    QSCA = C_sca/pi*a**2 = efficiency factor for scattering
!    QBACK = (dC_sca/domega)/pi*a**2
!          = backscattering efficiency [NB: this is (1/4*pi) smaller
!            than the "radar backscattering efficiency"; see Bohren &
!            Huffman 1983 pp. 120-123]
!    GSCA = <cos(theta)> for scattering
!
! Original program taken from Bohren and Huffman (1983), Appendix A
! Modified by B.T.Draine, Princeton Univ. Obs., 90/10/26
! in order to compute <cos(theta)>
! 91/05/07 (BTD): Modified to allow NANG=1
! 91/08/15 (BTD): Corrected error (failure to initialize P)
! 91/08/15 (BTD): Modified to enhance vectorizability.
! 91/08/15 (BTD): Modified to make NANG=2 if called with NANG=1
! 91/08/15 (BTD): Changed definition of QBACK.
! 92/01/08 (BTD): Converted to full double precision and double complex
!                 eliminated 2 unneed lines of code
!                 eliminated redundant variables (e.g. APSI,APSI0)
!                 renamed RN -> EN = double precision N
!                 Note that DOUBLE COMPLEX and DCMPLX are not part
!                 of f77 standard, so this version may not be fully
!                 portable.  In event that portable version is
!                 needed, use src/bhmie_f77.f
! 93/06/01 (BTD): Changed AMAX1 to generic function MAX
! ***********************************************************************
! *** Safety checks
      NANG = 2
      IF(NANG.GT.MXNANG)STOP'***Error: NANG > MXNANG in bhmie'
      IF(NANG.LT.2)NANG=2
! *** Obtain pi:
      PII=4.*ATAN(1.D0)
      DX=X
      DREFRL=REFREL
      Y=X*DREFRL
      YMOD=ABS(Y)
!
! *** Series expansion terminated after NSTOP terms
!    Logarithmic derivatives calculated from NMX on down
      XSTOP=X+4.*X**0.3333+2.
      NMX=MAX(XSTOP,YMOD)+15
! BTD experiment 91/1/15: add one more term to series and compare results
!      NMX=AMAX1(XSTOP,YMOD)+16
! test: compute 7001 wavelengths between .0001 and 1000 micron
! for a=1.0micron SiC grain.  When NMX increased by 1, only a single
! computed number changed (out of 4*7001) and it only changed by 1/8387
! conclusion: we are indeed retaining enough terms in series!
      NSTOP=XSTOP
!
      IF(NMX.GT.NMXX)THEN
          WRITE(0,*)'Error: NMX > NMXX=',NMXX,' for |m|x=',YMOD
          STOP
      ENDIF
!*** Require NANG.GE.1 in order to calculate scattering intensities
      DANG=0.
      IF(NANG.GT.1)DANG=.5*PII/DBLE(NANG-1)
      DO 1000 J=1,NANG
          THETA=DBLE(J-1)*DANG
          AMU(J)=COS(THETA)
 1000 CONTINUE
      DO 1100 J=1,NANG
          PI0(J)=0.
          PI1(J)=1.
 1100 CONTINUE
      NN=2*NANG-1
      DO 1200 J=1,NN
          S1(J)=(0.,0.)
          S2(J)=(0.,0.)
 1200 CONTINUE
!
! *** Logarithmic derivative D(J) calculated by downward recurrence
!    beginning with initial value (0.,0.) at J=NMX
!
!     D(NMX)=(0.,0.)
      D(NMX)=0.0
      NN=NMX-1
      DO 2000 N=1,NN
          EN=NMX-N+1
          D(NMX-N)=(EN/Y)-(1./(D(NMX-N+1)+EN/Y))
 2000 CONTINUE
!
! *** Riccati-Bessel functions with real argument X
!    calculated by upward recurrence
!
      PSI0=COS(DX)
      PSI1=SIN(DX)
      CHI0=-SIN(DX)
      CHI1=COS(DX)
      XI1=DCMPLX(PSI1,-CHI1)
      QSCA=0.E0
      GSCA=0.E0
      P=-1.
      DO 3000 N=1,NSTOP
          EN=N
          FN=(2.E0*EN+1.)/(EN*(EN+1.))
! for given N, PSI  = psi_n        CHI  = chi_n
!              PSI1 = psi_{n-1}    CHI1 = chi_{n-1}
!              PSI0 = psi_{n-2}    CHI0 = chi_{n-2}
! Calculate psi_n and chi_n
          PSI=(2.E0*EN-1.)*PSI1/DX-PSI0
          CHI=(2.E0*EN-1.)*CHI1/DX-CHI0
          XI=DCMPLX(PSI,-CHI)
!
! *** Store previous values of AN and BN for use
!    in computation of g=<cos(theta)>
          IF(N.GT.1)THEN
              AN1=AN
              BN1=BN
          ENDIF
!
! *** Compute AN and BN:
          AN=(D(N)/DREFRL+EN/DX)*PSI-PSI1
          AN=AN/((D(N)/DREFRL+EN/DX)*XI-XI1)
          BN=(DREFRL*D(N)+EN/DX)*PSI-PSI1
          BN=BN/((DREFRL*D(N)+EN/DX)*XI-XI1)
!
! *** Augment sums for Qsca and g=<cos(theta)>
          QSCA=QSCA+(2.*EN+1.)*(ABS(AN)**2+ABS(BN)**2)
          GSCA=GSCA+((2.*EN+1.)/(EN*(EN+1.)))*                          &
               (REAL(AN)*REAL(BN)+IMAG(AN)*IMAG(BN))
          IF(N.GT.1)THEN
              GSCA=GSCA+((EN-1.)*(EN+1.)/EN)*                           &
              (REAL(AN1)*REAL(AN)+IMAG(AN1)*IMAG(AN)+                   &
               REAL(BN1)*REAL(BN)+IMAG(BN1)*IMAG(BN))
          ENDIF
!
! *** Now calculate scattering intensity pattern
!    First do angles from 0 to 90
          DO 2500 J=1,NANG
              JJ=2*NANG-J
              PI(J)=PI1(J)
              TAU(J)=EN*AMU(J)*PI(J)-(EN+1.)*PI0(J)
              S1(J)=S1(J)+FN*(AN*PI(J)+BN*TAU(J))
              S2(J)=S2(J)+FN*(AN*TAU(J)+BN*PI(J))
 2500     CONTINUE
!
! *** Now do angles greater than 90 using PI and TAU from
!    angles less than 90.
!    P=1 for N=1,3,...; P=-1 for N=2,4,...
          P=-P
          DO 2600 J=1,NANG-1
              JJ=2*NANG-J
              S1(JJ)=S1(JJ)+FN*P*(AN*PI(J)-BN*TAU(J))
              S2(JJ)=S2(JJ)+FN*P*(BN*PI(J)-AN*TAU(J))
 2600     CONTINUE
          PSI0=PSI1
          PSI1=PSI
          CHI0=CHI1
          CHI1=CHI
          XI1=DCMPLX(PSI1,-CHI1)
!
! *** Compute pi_n for next value of n
!    For each angle J, compute pi_n+1
!    from PI = pi_n , PI0 = pi_n-1
          DO 2800 J=1,NANG
              PI1(J)=((2.*EN+1.)*AMU(J)*PI(J)-(EN+1.)*PI0(J))/EN
              PI0(J)=PI(J)
 2800     CONTINUE
 3000 CONTINUE
! 
! *** Have summed sufficient terms.
!    Now compute QSCA,QEXT,QBACK,and GSCA
      GSCA=2.*GSCA/QSCA
      QSCA=(2./(DX*DX))*QSCA
      QEXT=(4./(DX*DX))*REAL(S1(1))
      QBACK=(ABS(S1(2*NANG-1))/DX)**2/PII
      RETURN
      END SUBROUTINE BHMIE


!     /////////////////////////////////////////////////////////////////
      SUBROUTINE SMALL4( XX, CREFIN, XLNSIG, BEXT, BSCAT, G )
! FSB This is a modification of Wiscombe's SMALL2 subroutine from MIEV0
! Using the analytic character of QEXT / XX, QSCA / XX  GQSC, to calculate
! the integral over a log normal distribution characterized by XX and XLNSIG. 
!  
!     The presence of XX  within the calculations of the A and B 
!     coefficients is ignored in order to facilitate the analytical 
!     integration. These terms have the same dominant powers in the
!     numerators and denomenators, and are thus held constant for the
!     integration of QSCAT and QEXT over XX. 

! FSB code revised 02/09/2011 to have MIMCUT as a local parameter,
!     and to set the calculations of BSCAT and BEXT immediately 
!     after the calculationof QSCAT and QEXT. 

!       Small-particle limit of Mie quantities for general refractive
!       index ( Mie series truncated after 2 terms )

!        A,B       First two Mie coefficients, with numerator and
!                  denominator expanded in powers of XX ( a factor
!                  of XX**3 is missing but is restored before return
!                  to Wiscombe's calling program MIEV0)

!        CIORSQ    Square of refractive index

!    References:
!     Wiscombe, W. J., Improves Mie Scattering Algorithms, Applied 
!       Optics, Vol. 19,No. 9, PP 1505-1509, May 1, 1980. 

!     Wiscombe, W. J. Mie Scattering Calculations: Advances in 
!      Techniques and Fast Vector-Speed Computer Codes, 
!      NCAR Technical NCAR/TN 140+STR, June 1979, revised August 1996.
!        Equation citations below are to the NCAR document.

      IMPLICIT  NONE
       
       REAL, INTENT(IN)  :: XX        ! PI * diameter / wavelength
       COMPLEX, INTENT( IN) :: CREFIN ! complex refractive index
       REAL, INTENT(IN)  :: XLNSIG    ! Logrithm of geometric standart deviation.
       REAL, INTENT(OUT) :: BEXT      ! Extinction coeffcient
       REAL, INTENT(OUT) :: BSCAT     ! Scattering coefficient
       REAL, INTENT(OUT) :: G         ! asymmetry factor 


      REAL :: QEXT    ! Extinction efficiency factor
      REAL :: QSCA    ! Scattering efficiency factor
      

!     .. Parameters ..
       REAL, PARAMETER :: MIMCUT = 1.0E-05 ! the cutoff value of imaginary 
                                  ! part of the refractive index.
                                  ! values smaller than the cutoff are 
                                  ! treated as zero.

      REAL, PARAMETER :: PI = 3.14159265358979324 
      REAL, PARAMETER :: THREE_PI_TWO = 1.5 * PI

      REAL   :: TWOTHR, FIVTHR
      PARAMETER ( TWOTHR = 2.0/3.0, FIVTHR = 5.0/3.0)
      REAL  ::    FOUR5, F49_2
      PARAMETER ( FOUR5 = 4.50 , F49_2 = 0.5 * 49.0 / 2.0 )
      REAL  ::  T
!     .. Scalar Arguments ..

      REAL ::  MRE, MIM, XLNSIG2, EXPFAC3, EXPFACM1 
      REAL ::  XX_2, XX_3, XX_4
      REAL ::  GG
!     .. Array Arguments ..

      COMPLEX ::  A( 2 ), B( 2 )
!     ..
!     .. Local Scalars ..

      COMPLEX :: CIOR, CIORSQ, CTMP
!     ..
!     .. Intrinsic Functions ..

      INTRINSIC :: AIMAG, CMPLX, CONJG, REAL
!     ..
!     .. Statement Functions ..

      REAL ::     SQ
!     ..
!     .. Statement Function definitions ..

      SQ( CTMP ) = REAL( CTMP )**2 + AIMAG( CTMP )**2

!     Begin Code..
      
      CIOR = CREFIN
      XLNSIG2  = XLNSIG * XLNSIG
      EXPFAC3  = EXP( FOUR5 * XLNSIG2 )
      EXPFACM1 = EXP( 0.5 * XLNSIG2 )
      XX_2 = XX * XX
      XX_3 = XX * XX * XX
      XX_4 = XX_2 * XX_2 
      
! FSB MIEV0 wants the refractive index with a negative value for the
!     imaginary part.        
      IF( AIMAG(CIOR) .GT. 0.0 ) CIOR = CONJG( CIOR )

      MRE    = REAL( CIOR )
      MIM    = -AIMAG( CIOR )

      CIORSQ = CIOR**2
      CTMP   = CMPLX( 0.0, TWOTHR )*( CIORSQ - 1.0 )

!                                           ** Eq. R42a
      A( 1 ) = CTMP*( 1.0- 0.1*XX_2 +                                     &
               ( CIORSQ / 350.0 + 1.0/280.)*XX_4 ) /                      &
               ( CIORSQ + 2.0 + ( 1.0- 0.7 * CIORSQ )*XX_2 -              &
               ( CIORSQ**2 / 175.0- 0.275 * CIORSQ + 0.25 )*XX_4 +        &
               XX**3 * CTMP * ( 1.- 0.1*XX_2 ) )

!                                           ** Eq. R42b
      B( 1 ) = ( XX**2 / 30.0 )*CTMP*( 1.0 +                              &
               ( CIORSQ / 35.0 - 1.0/ 14.0)*XX_2 ) /                      &
               ( 1.0- ( CIORSQ / 15.0 - 1.0/6.0)*XX_2 )                      

!                                           ** Eq. R42c

      A( 2 ) = ( 0.1*XX_2 )*CTMP*( 1.0 - XX_2 / 14.0 ) /                  &
               ( 2.0*CIORSQ + 3.0- ( CIORSQ / 7.0- 0.5 ) * XX_2 )

!                                           ** Eq. R40a

!      QSCA = (6.*XX_4) * ( SQ( A(1) ) + SQ( B(1) ) +                      &
!     &                     FIVTHR * SQ( A(2) ) )
     
      T   =  ( SQ( A(1) ) + SQ( B(1) ) + FIVTHR * SQ( A(2) )  )
      
      QSCA = (6.0*XX_4) * T
! Analytic integrals   
        BSCAT  = THREE_PI_TWO *  QSCA * EXPFAC3 / XX
        BEXT   = BSCAT
! FSB  If the imaginary part of CREFIN is larger than MIMCUT
!      then calculate the extinction efficiency and the
!      extinction coefficient. 
!                                          ** Eq. R40b
      IF( MIM .GT. MIMCUT )  THEN                                            
        QEXT = 6.0*XX * REAL( A(1) + B(1) + FIVTHR*A(2) )
! FSB  Form analytical integral       
        BEXT   = THREE_PI_TWO  * QEXT / XX
       END IF

!                                           ** Eq. R40c

!      GQSC = (6.*XX_4) * REAL( A(1)*CONJG( A(2) + B(1) ) )

!      G = GQSC / QSCA 

! This is  the form given in Wiscombe (1980) 

       GG =  REAL( A(1)*CONJG( A(2) + B(1) ) ) / T
                     
       G  =  GG
      
      RETURN
      END SUBROUTINE SMALL4
! /////////////////////////////////////////////////////////////////////

      subroutine large_limit( nr, ni, xx, xlnsig, bext, bscat, g)
! FSB calculate bext, bscat, g from asymptotic limits.
!     The values of bext, and bscat are dimensionless and have been
!     integrated over a log normal distribution. 
!     References:
!     Irvine, W.M. Light scattering by spherica particles: radiation 
!       pressre, asymmetry factor, and extinction cross section, Jour.
!       of the Optical Society of America, Vol. 55, No. 1,
!       pp. 16-21.January, 1965.
!
!     Kokhanovsky, A.A. and E.F. Zege, Otical properties of aerosol 
!      particles: a revies of approximate analytical solutions, Jour.
!      of Aerosol  Science, Vol. 28, Number 1, pp. 1-21, 1997. (K&Z)

!     We fit values in Table 6 of Kokhanovsy and Zege with
!     polynomials. Edge effects are ignored.

!     coded April 26, 2010, by Dr. Francis S. Binkowski
!     Center for Environmental Modeling for Policy Development, 
!     The Institute for the Environment, The University of 
!     North Carolina at Chapel Hill. 

      implicit none
      real, intent(in)  :: nr, ni ! refractive indices
      real, intent(in)  :: xx     ! pi * dg_v / lamda
      real, intent(in)  :: xlnsig  ! ln( sigma_g )
      real, intent(out) :: bext   ! extinction coefficient       
      real, intent(out) :: bscat  ! scatering coefficient
      real, intent(out) :: g      ! asymmetry factor.
      
      real  :: ginf  ! asymmetry factor for an absorbing sphere
      real  :: delta_g ! difference between g0 and ginf
      real  :: Y     ! variable from Kokhanovsky and Zege
      real  :: QSCA  ! Scattering eficiency
      real  :: QEXT  ! extinction efficiency
      real  :: expfac1, expfac2
      real  :: c
      real  :: nr2, nr3, nr4
      real  :: y1, y2,y3, y4
      real  :: g1, g2, dg1, dg2
      real  :: qs1,qs2
      real, parameter :: pi = 3.1415926525
      real, parameter :: three_pi_two = 1.5 * pi
! FSB begin code 
      expfac1 = exp( 0.5 * xlnsig * xlnsig ) / xx 
      c = 4.0 * ni * xx
      
      nr2 = nr * nr      
      nr3 = nr * nr * nr
      nr4 = nr2 * nr2

! assemble components of polynomials      
      y4  = -1.27579 * nr4
      y3  = 8.845570 * nr3
      y2  = - 2.299855e01 * nr2
      y1  = 2.68117e01 * nr -1.104857e01       
      g1  = -9.418646e-02 * nr + 1.107985
      g2  = -6.558408e-03 * nr2
      dg1 = 5.874452e-01 * nr - 4.614125e-01
      dg2 = -1.329323e-01 * nr2
      qs1 = 2.344471e-01 * nr + 8.025779e-01
      qs2 = -2.772065e-02 * nr2 

! calculate variables

! For scattering, use Irvine equation (11): QSCA = 1.0 + w
! The variable S in (K&Z) Table 6 is the same as w in
! Table 1 of Irvine. (K&Z) have a larger range of nr in 
! their Table 6. 
! We fit 1.0 + S  with a quadratic
      QSCA  = qs1 + qs2
! Equation (10 ) from Irvine.      
      QEXT  = 2.0 
! Integrate QSCA / xx and QEXT / xx over a lognormal
! distribution 
      bext  = three_pi_two * QEXT * EXPFAC1 
      bscat = bext ! FSB 02/09/2011 Ensure that bscat = bext when 
                   ! ni is zero. 
      if( ni .ge. 1.0e-5) then ! same value as in SMALL4.
        bscat = three_pi_two * QSCA * EXPFAC1 
      end if 
! For asymmetry factor, use  (K&Z) equation (49), g0
! is a geometric optics approximation. 
! for nearly non-absorbing spheres, the (K&Z) values
! g0 match Irvine falues at nr = 1.2 an 1.5 to 4 decimal 
! places. The (K&Z) version adjusts for absorbing spheres
! We fit the varaible y in (K&Z) Table 6 with a quartic. 
      y         = y4 + y3 + y2 + y1
      ginf      = g1 + g2     ! quadratic fit 
      delta_g   = dg1 + dg2   ! quadratic fit 
      expfac2   = exp( - c * y )       
      g         = ginf - delta_g * expfac2 ! Eq (49) 
! The expfac1 and xx terms cancel out when g is integrated
! over a lognormal distribution. This follows from
! equation (7) of (K&Z). This step is not an approximation.

      return
      end subroutine large_limit
! ______________________________________

!//////////////////////////////////////////////////////////////////////


       subroutine getqext_EF(alfa, nr, ni, qext, qabs, qscat, g ) 
! FSB  compute the extinction and scattering efficienies and the
!      asymmetry factor. 

!      where:

!      alfa is Mie parameter
!      nr is the real part ot the refractive index
!      ni is the imaginary part of the refractive index
!      qext is extinction efficiency
!      qabs is the absorption efficiency
!      qscat is the scattering efficiency
!      g is the asymmetry factor <cos>
!      all input and output variables are REAL*4

!      references:
!      For the main approximation to extinction
 
!      Evans, B.T.N.  and G.R. Fournier, Simple approximations to
!      extinction efficiency valid over all size parameters,
!      Applied Optics, 29, 4666 - 4670. 1990.
!      
!      For ADT algorithm
!      van de Hulst- Light Scattering by Small Particles,
!      Dover,1981 page 179. Original edition was Wiley, 1957.

!      For the small particle approximation

!      Penndorf, R., Scattering and extinction coefficients for small
!      aerosols, J. Atmos. Sci., 19, p 193, 1962.

!     For the absorption approximation

!     Bohren and Nevitt, Absorption by a sphere: a simple approximation
!     Applied Optics, vol 22, no 6, pp 774-775, 15 March 1983.

!     For an algebrai! simplification is due to Kokhanovsky and Zege,

!     Kokhanovsky and Zege, Optical properties of aerosol particles: 
!     a review of approximate analytical solutions, J. Aerosol Science, 
!     vol 28, No 1, pp 1-21, 1997.

!     Asymmetry factor is calculated by an algorithm due to 
!     Adel Hanna and Rohit Mathur (personal communication)

!     This version coded June 10, 2003, and modified for inline
!     documentation September 02, 2003 by Dr. Francis S. Binkowski
!     Carolina Environmental Program, University of North Carolina
!     modified by FSB to report only qscaef 05/10/2005
!     modified by FSB 12/21/2010 to report qext,qabs,qscat, and g

!     Email: frank_binkowski@unc.edu

       implicit none

       real :: nr, ni  ! real and imaginary parts of index of refraction
       real :: qextadt
       real :: qext, qabs, qscat
       real :: QABS_PENN, QEXT_PENN, QSCAT_PENN, QABS_BN 


       real :: tt        !  edge effect factor
       real :: mu, mum1  !  exponents in formula
       real :: aa        !  first coefficient in mu
       real :: gg        !  second coefficient in mu
       real :: nrm1, sqrtni
       real :: alfa
       real :: alfm23   ! functions of alfa (mie parameter)
       real :: three5, three4, two3
        parameter( three5    = 3.0 / 5.0   ,   &
                   three4    = 3.0 / 4.0   ,   &
                   two3      = 2.0 / 3.0   )
       
! FSB added for asymmetry factor calculation
      real :: aa0, aa1,aa2,aa3,aa4
       parameter( aa0 =  88.466,                                        &
                  aa1 = -61.628,                                        &
                  aa2 =  34.483,                                        &
                  aa3 = -8.543,                                         &
                  aa4 =  0.77435 )
      real :: qangle,g 
      real :: qq, qf1,qf2,qf3
      real :: degtorad 
      real :: pi 
       parameter( pi = 3.141592654)
       parameter( degtorad = pi / 180.0 )
       
       nrm1   = nr - 1.0
       sqrtni = sqrt (ni )
       call adtqext(alfa,nr,ni, qextadt)
       call penndorf(alfa,nr,ni,QEXT_PENN, QABS_PENN, QSCAT_PENN)
       
! FSB Evens and Fournier approximation

         alfm23  = ( 1.0 / alfa ) ** two3

         tt      = 2.0 - exp( -alfm23 )

         aa      = 0.5 + ( nrm1 - two3 * sqrtni - 0.5 * ni ) +          &
                 ( nrm1 + two3 * ( sqrtni - 5.0  * ni )  ) ** 2

         gg      = three5 - three4 * sqrt ( nrm1) + 3.0 * nrm1 ** 4 +   &
                  25.0 * ni / ( 6.0 * ni + 5.0 * nrm1 )

         mu      = aa + gg / alfa
         mum1    = - 1.0 / mu

         qext = ( QEXT_PENN *                                           &
                ( 1.0 + (QEXT_PENN /( qextadt * tt)) ** mu ) ** mum1 ) 
       

         qabs = 0.0

! FSB test imaginary part of the refractive index
!     if it is nonzero, calculate absorption efficiency
         if( ni .gt. 0.0 ) then
 
          call abs_bn(nr,ni,alfa, QABS_BN) ! Bohren & Nevitt

! *** use the same  Evans & Fournier algorithm for absorption.
          
             qabs  = ( QABS_PENN *                                      &
                 ( 1.0 + (QABS_PENN /( QABS_BN * tt)) ** mu ) ** mum1) 
     
         
 
         end if 
         
         qscat = qext - qabs

! FSB Set up for Mathur & Hanna method for asymmetry factor
         
       
       qangle = aa0
      if( qscat .gt. 1.0e-4 ) then
! FSB evaluate quartic equation for qangle      
        qq  = qscat * qscat
        qf1 = aa0 + aa1 * qscat
        qf2 = aa2 + aa3 * qscat
        qf3 = aa4 * qq
        qangle = qf1 + qf2 * qq + qf3 * qq
      end if 
      qangle = degtorad * qangle 
      g = cos( qangle)
!       write(35,*) 'alfa=',alfa, 'qangle = ', qangle,'g=',g
       return
       end subroutine getqext_EF
! ______________________________________
      subroutine adtqext(alfa, nr, ni, QEXT)

! *** van de Hulst approximation for QEXT.
! ***  This approximation is known as Anomalous Diffraction Theory (ADT)

! *** originally coded by Dr Francis  S. Binkowski,
!        AMAB/MD/ESRL RTP,N.C. 27711 28 July 1977.
!       corrected 7/19/90 by fsb.
!       revised 1/8/98 by FSB


! *** reference:
!         van de Hulst- Light Scattering by Small Particles,
!         Dover,1981 page 179. Original edition was Wiley, 1957.

        implicit none
        real :: alfa   ! Mie parameter
        real :: nr, ni ! real and imaginary parts of the index of refraction
        real :: QEXT   ! extinction efficiency for a sphere
        real :: z, tanb, b, cos2b,  x, expmx, cs1, cs2
        real :: twob, cosb, sinzmb, coszm2b
        real :: nr1
        real :: xxalf
        
        xxalf   = max( alfa, 1.0) ! limiting range of ADT 
        
        nr1     = nr - 1.0
        z       = 2.0 * xxalf * nr1
        tanb    = ni/nr1
        b       = atan(tanb)
        cosb    = cos(b)
        twob    = 2.0 * b        
        sinzmb  = sin(z-b)
        coszm2b = cos(z-twob)
        cos2b   = cos(twob)
        x       = z * tanb
        expmx   = exp( -x )
        cs1     = cosb / z
        cs2     = cs1 * cs1

        QEXT    = 2.0 - 4.0 * cs1 * expmx * sinzmb + 4.0 * cs2 *         &
                    ( cos2b - expmx * coszm2b )
        
        return
        end subroutine adtqext
! __________________________________________________
      subroutine penndorf(alfa, nr, ni, QEXT, QABS, QSCA)
! *** calculates the Mie efficiencies for extinction, scattering and
!     absorption using Penndorf's approximations for small
!     values of alfa.
!
!  input:
!       nr        the real part of the refractive index.
!       ni        the imaginary part of the refractive index
!       alfa      mie parameter

!
!  output:
!       QEXT      extinction efficiency
!       QABS      absorption efficeincy
!       QSCA      scattereing efficiency
!
! *** reference:
!       Penndorf, r., Scattering and extinction coefficients for small
!       aerosols, J. Atmos. Sci., 19, p 193, 1962.
!
! *** originally coded by Dr Francis  S.  Binkowski,
!       AMAB/MD/ESRL RTP,N.C. 27711 28 July 1977.
!       corrected 7/19/90 by FSB
!       modified 30 september 1992 by FSB
!       modified 1/6/98 by FSB
!       modified to include QSCAT, QABS June 9, 2003 by FSB

        implicit none

        real :: alfa, nr, ni
        real :: QEXT, QABS, QSCA
        real :: alf2,alf3,alf4

        real :: a1,a2,a3
        real :: xnr,xni,xnr2,xni2,xnri,xnri2,xnrmi
        real :: xri,xri2,xri36,xnx,xnx2
	    real :: z1,z12,z2,xc1

        xnr   = nr
        xni   = ni
        xnr2  = xnr   * xnr
        xni2  = xni   * xni
        xnri  = xnr2  + xni2
        xnri2 = xnri  * xnri
        xnrmi = xnr2  - xni2
        xri   = xnr   * xni
        xri2  = xri   * xri
        xri36 = 36.0  * xri2
        xnx   = xnri2 + xnrmi - 2.0
        xnx2  = xnx   * xnx

        z1    = xnri2 + 4.0 * xnrmi + 4.0
        z12   = z1    * z1
        z2    = 4.0   * xnri2 + 12.0 * xnrmi + 9.0
        xc1   = 8.0   / (3.0*z12)

        alf2  = alfa  * alfa
        alf3  = alfa  * alf2
        alf4  = alfa  * alf3

        a1    = 24.0  * xri / z1

        a2    = 4.0   * xri / 15.0 + 20.0 * xri / (3.0 * z2) +           &
                    4.8   * xri * (  7.0 * xnri2 +                       &
                    4.0   * ( xnrmi - 5.0) ) / z12

        a3    = xc1 * (xnx2 - xri36)

	    QEXT  = a1 * alfa + a2 * alf3 + a3 * alf4
        QSCA  = a3 * alf4
        QABS  = QEXT - QSCA 
        return
        end subroutine penndorf
!______________________________________________________________________

      subroutine abs_bn(nr, ni,x, QABS)
! FSB calculate the absorption efficiency using the algorithm of
!     Bohren and Nevitt.
!     The algebrai! simplification is due to Kokhanovsky and Zege,

!     Reference:
!     Bohren and Nevitt, Absorption by a sphere: a simple approximation
!     Applied Optics, vol22, no 6, pp 774-775, 15 March 1983.

!     Kokhanovsky and Zege
!     Optical properties of aerosol particles: a review of approximate
!     analytical solutions, J. Aerosol Science, 
!     vol 28, No 1, pp 1-21, 1997.

! FSB coded June 9, 2003, by Dr. Francis S. Binkowski
! FSB modified June 27,20033 for case M > 1.0
!     use Kokhanovsky & Zege ( J. aerosol Science Vol. 38, No.1,
!     pp1-21, 1996 ) recommendation that M can be approximated by
!     Mst = exp( -2 * ! *( nr##2 - b**3) / 3 ). They cite Shifrin &
!     Tonna with a claim that the differenced is about 1%. for nr
!     in the range 1.1 to 1.5.  
      implicit none
      real :: nr    ! real refractive index
      real :: ni    ! imaginary refractive index
      real :: x     ! Mie parameter
      real :: QABS  ! absorption efficiency
      real :: nrm1, nrp1,nr2, b,c,c2,cb,D, expmc, Mst, psi 
      real :: two3
       parameter( two3 = 2.0 / 3.0 )
           
! *** If ni is zero, so is QABS.       
      if( ni .eq. 0.0) then
        QABS = 0.0
        return
      end if ! check on ni
      
      c     = 4.0 * x * ni
      c2    = c * c
      expmc = exp(-c)
      nr2   = nr * nr
      nrp1  = nr + 1.0
      nrm1  = nr - 1.0      
      D     = nrp1 * nrp1 - ( nrm1 * nrm1) * expmc
      b     = sqrt( ( nr2 - 1.0 ) / nr2 )
      psi   = nr2 * ( 1.0 - b * b * b)      
      cb    = c * b
      
!      Mbn = 2.0 * nr2 * 
!     &     ( exp( - cb) * ( 1.0 + cb) - (1.0 + c ) * expmc ) / c2 

! *** algebraic simplification:

      Mst  = exp( - two3 * c * psi ) 
      
!      if ( Mbn .ge. 1.0 ) then
!       M = Mst
!      else
!       M = Mbn
!      end if
      
      QABS = 4.0 * nr * ( 1.0 - Mst ) / D
      
      return
      end subroutine abs_bn


!     /////////////////////////////////////////////////////////////////



!      /////////////////////////////////////////////////////////////////
             
END MODULE module_twoway_ra_rrtmg_sw
