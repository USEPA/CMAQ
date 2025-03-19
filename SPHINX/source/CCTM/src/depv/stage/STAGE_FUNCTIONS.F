
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!
      Module Resist_Funcs
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
! This module contains functions and subroutines to calculate variables
! used to estimate the air-surface exchange using the STAGE deposition option
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Use ASX_DATA_MOD, only:pr, twothirds
      
      Implicit None
      Real, Parameter, Private :: a0         = 8.0        ! [dim'less]

      Contains 
!--------------------------------------------------------------------------------------------------
! Calculate soil dry diffusive length until saturation
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_ldry( sm_v1cm, sm_v5cm, sm_vsat, zsoil1 ) Result( ldry )

         Implicit None        

         Real, Intent( IN ) :: sm_v1cm ! Volumetric soil moisture at -1 cm (m3/m3)
         Real, Intent( IN ) :: sm_v5cm ! Volumetric soil moisture at -5 cm (m3/m3)
         Real, Intent( IN ) :: sm_vsat ! Volumetric soil moisture saturation point (m3/m3)
         Real, Intent( IN ) :: zsoil1  ! Model depth of soil layer 1
         Real, Parameter    :: ldry_max = 0.02 ! maximum dry diffusion length based on measurements of Kondo et al 1990
         Real               :: ldry    ! Length of dry layer soil diffusion distance (m)
    
! The following resistance parameterization is derived from measurements with soil samples of 2 cm thick (Kondo et al 1990)
! https://doi.org/10.1175/1520-0450(1990)029<0385:APOEFB>2.0.CO;2 as discussed in Sakaguchi and Zeng 2009 JGR 
! https://doi.org/10.1029/2008JD010834 According to Swenson and Lawrence 2014 (https://doi.org/10.1002/2014JD022314) and the 
! references therein the dry layer thickness varies from 1 to 3 cm. 
! From Sakaguchi and Zeng 2009 JGR Equation 10
         ldry = ldry_max * ( Exp( ( 1.0 - sm_v1cm / sm_vsat ) ** 5 ) - 1.0 ) / 1.718

         If(ldry .gt. zsoil1) Then
            ldry = zsoil1 + ( ldry_max - zsoil1 ) * ( Exp( ( 1.0 - sm_v5cm / sm_vsat ) ** 5 ) - 1.0 ) / 1.718
         End If

         End Function Calc_ldry
!--------------------------------------------------------------------------------------------------
! Calculate Quasi-laminar boundary layer resistance to deposition to water surfaces
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_Rbw( ustar, scc_pr_23 ) Result( Rbw )
! Returns the quasi-laminar boundary layer resistance for a flat surface based on 
! Wesely and Hicks 1977  https://doi.org/10.1080/00022470.1977.10470534 equation 13

         Implicit None
         
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: scc_pr_23
         Real               :: Rbw

         Rbw = 5.0 / ustar * scc_pr_23
         End Function Calc_Rbw

!--------------------------------------------------------------------------------------------------
! Calculate Resistance to deposition to surface waters
! 
! References: 
! Chang et al., Ozone deposition to the sea surface: chemical enhancement and wind speed 
!    dependence, Atmos. Environ., 38(7), 1053-1059, https://doi.org/10.1016/j.atmosenv.2003.10.050, 
!    2004
! Garland et al., The mechanism for dry deposition of ozone to seawater surface, J. Geophys. Res,
!    85, 7488-7492, https://doi.org/10.1029/JC085iC12p07488, 1980
! Hayduk, W., and H. Laudie, Prediction of diffusion coefficients for nonelectrolytes indilute 
!    aqueous solutions, AIChE. J., 20, 611-615, https://doi.org/10.1002/aic.690200329, 1974
! Rebello et al., The Cycling of Iodine as Iodate and Iodide in a Tropical Estuarine System, 
!    Marine Chem. 29, 77-93, https://doi.org/10.1016/0304-4203(90)90007-Y, 1990
! 
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_Rwater( ustar, q_2m, temp_2m, temp_g, tw, MVol, H, O3_Hit, 
     &                         Hg_Hit, sea_ice  ) Result( Rwater )         
         Implicit None

         Include SUBST_CONST     ! constants
         
         Real, Intent( IN ) :: ustar   ! Surface friction velocity (m/s)
         Real, Intent( IN ) :: q_2m    ! temp water vapor mixing ratio
         Real, Intent( IN ) :: temp_2m ! 2m temp (K)
         Real, Intent( IN ) :: temp_g  ! surface temp (K)
         Real, Intent( IN ) :: tw      ! water skin temp (K)
         Real, Intent( IN ) :: H       ! Effective Henry's Constant (unitless)
         Real, Intent( IN ) :: MVol    ! Molar volume (LeBas) (L/mol)
         logical, Intent( IN ) :: O3_Hit  ! O3 index
         logical, Intent( IN ) :: Hg_Hit  ! Hg index
         logical, Intent( IN ) :: sea_ice ! sea ice coverage (ratio)
         Real, Parameter :: rt25inK    = 1.0/(stdtemp + 25.0) ! 1/298.15K (1/K)
         Real, Parameter :: d3         = 1.38564e-2           ! Empirical coefficient (unitless)
         Real            :: pChang, kwChang  ! Air-water partitioning coefficients (unitless)
         Real            :: ciodide, qiodide ! Iodide in sea-water based on SST  (mol / L)
         Real            :: dw25, dw, kvisw  ! Trace gase diffusivity (at 25C and tw) in water and kinematic viscosity of water (cm2/s)
         Real            :: scw_pr_23    ! (SCC/PR)**2/3, fn of DIF0
         Real            :: Rwater       ! Resistance of deposition to sea surfaces (s/m)

! from Hayduk and Laudie
         dw25 = 13.26e-5 / ( 0.8904**1.14 * MVol**0.589 )
         kvisw = 0.017 * EXP( -0.025 * ( tw - stdtemp ) )
         dw    = dw25 * ( tw * rt25inK ) * ( 0.009025 / kvisw )
         scw_pr_23 = ( ( kvisw / dw ) / pr ) ** twothirds
! All species but Hg and O3
         Rwater = scw_pr_23 / ( H * d3 * ustar )

         IF ( O3_Hit ) THEN   !implement Chang et al(2004)
c        pChang is a/H or alpha/H which would be 1/H in current model
c        note that in Chang et al (2004) and Garland et al (1980) their H is Cair/Cwater which is
c        the inverse of heff
            pChang = 1.75
            kwChang = (d3*ustar)/scw_pr_23

c        If a file of chlorophyll concentrations is provided, Iodide concentrations are estimated from
c        a fit to the Rebello et al 1990 data. The slope and correlation are given in the paper
c        but not the intercept, so the data in Tables 3 & 4 were fit to get the relationship below.
c        The regression gives the concentration in umol/L and is converted to mol/L for use in Chang et al eq.
c        The slope and correlation are slightly different than in Table 5.
c        If chlorophyll concs are not available, a constant value for [I-] of 100e-9 mol/l is used
c        Use ocean file variables to determine if the water cell is ocean or lake; method is only for ocean cells

            IF ( sea_ice ) THEN
! O3 over sea ice
               Rwater   = scw_pr_23 / ( H * d3 * ustar )
            ELSE                  
c        Iodide in sea-water based on SST  (mol /dm-3)
               ciodide = 1.46E6 * EXP( -9134.0 / temp_g)
               qiodide = sqrt( 2.0e9 * ciodide * dw * 1.0e-4 ) * H
               Rwater = 1.0 / ( pChang * kwchang + qiodide )
            END IF
         End IF 
         IF( Hg_Hit ) THEN
            Rwater = 1.0e6 ! surface waters are typically enriched in Hg(0) and act as an emission source
         END IF

         End Function Calc_Rwater
!--------------------------------------------------------------------------------------------------
! Calculate Resistance to deposition to ice surfaces
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_Rice( Rx ) Result( Rice )

         Implicit None
         
         Real, Intent( IN ) :: Rx   ! Relative reactivivity (unitless)
         Real               :: Rice ! Resistance of deposition to ice surfaces (s/m)
         Real, Parameter    :: rsnow0     = 10000.0    ! Empirical scaling coefficient set to
                                                       ! approximately match D Helmig et al. 2007 for O3
                                                    
         Rice = rsnow0 * a0 / Rx                                                   

         End Function Calc_Rice

!--------------------------------------------------------------------------------------------------
! Calculate resistance to snow-covered surfaces
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_Rsnow( temp_g, snow, molwt, M_ac, heff, dif_T, rel_rx ) Result( Rsnow )

         Implicit None

         Include SUBST_CONST     ! constants
         
         Real, Intent( IN ) :: temp_g ! surface temperature (K) 
         Real, Intent( IN ) :: snow   ! snow coverage (ratio)
         Real, Intent( IN ) :: molwt  ! molar mass (g/mol)
         Real, Intent( IN ) :: M_ac   ! Mass accommodation coefficient (ratio)
         Real, Intent( IN ) :: heff   ! Effective henry's constant (unitless)
         Real, Intent( IN ) :: dif_T  ! Mass diffusivity of the trace gas (m2/s) 
         Real, Intent( IN ) :: rel_rx ! Relative reactivivity (unitless)
         Real               :: Rwet   ! Resistance of deposition to water surfaces (s/m)
         Real               :: Rice   ! Resistance of deposition to ice surfaces (s/m)
         Real               :: Rsnow  ! Resistance of deposition to snow surfaces (s/m)
         Real               :: melt_snow, ice_snow ! Ration of snow with semi-solid melted and ice surfaces (ratio)
         Real, Parameter    :: rsndiff    = 10.0   ! snow diffusivity fac (s/m)

         Rwet = Calc_Rwet( temp_g, molwt, M_ac, heff, dif_T )
         Rice = Calc_Rice( rel_rx )
! Liquid snow fraction modeled as a system dominated by van der Waals forces following Dash et al. 1999 S. Rep. Prog. Phys. 
! with a maximum fraction of the disordered interface acting as an aqueous solution as 20% following Conklin et al 1993 with
! the negligible impact of the disordered interface depth of 2 nm following Roth et al 2004. The 2 nm depth was approximated 
! to be around 263 degrees Celsius interpolated from figure 3 in Huthwelker et al 2006 doi:10.1021/cr020506v
         IF( snow .GT. 0.0 ) Then
            IF( stdtemp-temp_g .GT. 0.002 ) THEN
               melt_snow = 0.025 / (stdtemp-temp_g)**(1.0/3.0)
               melt_snow = MIN (melt_snow, 0.2)
               melt_snow = MAX (melt_snow, 0.01)
            ELSE
               melt_snow = 0.2
            ENDIF
         Else
            melt_snow = 0.0
         End IF
! frozen snow fraction
         ice_snow  = 1.0 - melt_snow               

         Rsnow = 1.0 / ( ice_snow / Rice + melt_snow / ( rsndiff + rwet ) )                                 

         End Function Calc_Rsnow

!-------------------------------------------------------------------------------------------------
! Resistance to air-wet surface exchange
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rwet( temp_g, MW, ac, H, dif_T ) Result( Rwet )

         Implicit None

         Include SUBST_CONST     ! constants
         
         Real,    Intent( IN ) :: temp_g
         Real,    Intent( IN ) :: H
         Real,    Intent( IN ) :: dif_T
         Real,    Intent( IN ) :: MW
         Real,    Intent( IN ) :: ac
         Real, Parameter :: rad_wat     = 1.9e-4    ! water droplet radius (m)
         Real            :: Rwet
         Real            :: rmsv
         Real            :: rawmt

         rmsv    = sqrt( 3.0 * RGASUNIV * 1.0e3 * temp_g / MW) 
         rawmt   = rad_wat / dif_T  + 4.0 / ( rmsv  * ac )
         Rwet   = rawmt + rawmt/( H * rad_wat )

         End Function Calc_Rwet

!-------------------------------------------------------------------------------------------------
! Quasi-Laminar Resistance to leaf following Jensen and Hummelshoj 1995/1997 doi:10.1016/0168-1923(94)05083-I
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rb_leaf( k_vis, dif_T, ustar, l_leaf, lai ) Result( Rb_leaf )

         Implicit None

         Real, Intent( IN ) :: k_vis
         Real, Intent( IN ) :: dif_T
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: l_leaf
         Real, Intent( IN ) :: lai
         Real               :: Rb_leaf
         Real, Parameter    :: C0 = 100.0 ! From Jensen and Hummelshoj 1997 errata

! Note that LAI**2 comes from the drag coefficient in Jensen and Hummelshoj 1995 and the 
! parameterization should be divided by LAI for a bulk canopy resistance         
         Rb_leaf = k_vis / ( dif_T * ustar * max( lai, 1.0 ) ) * 
     &           ( C0 * l_leaf * ustar / (k_vis * max(lai,1.0)**2 ) )**(1.0/3.0)

         End Function Calc_Rb_leaf

!-------------------------------------------------------------------------------------------------
! Resistance to air-stomatal exchange
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rst( Rstw, dwat_T, dif_T, H, f_mes, lai ) Result( Rst )

         Implicit None
   
         Real, Intent( IN ) :: Rstw
         Real, Intent( IN ) :: dwat_T
         Real, Intent( IN ) :: dif_T
         Real, Intent( IN ) :: H
         Real, Intent( IN ) :: f_mes
         Real, Intent( IN ) :: lai
         Real               :: Rst

         Rst = Rstw * dwat_T / dif_T + 1.0 / ( H / 3000.0 + 100.0 * f_mes ) / lai

         End Function Calc_Rst
!-------------------------------------------------------------------------------------------------
! Resistance to air-cuticular exchange
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rcut( temp_g, Rx, molwt, M_ac, heff, dif_T, a_cut,  
     &                            snow, no_snow, dry, wet, rh, lai, O3_hit, NH3_hit, ABFLUX ) Result( Rcut )

         Implicit None

         Real, Intent( IN ) :: temp_g
         Real, Intent( IN ) :: a_cut
         Real, Intent( IN ) :: snow
         Real, Intent( IN ) :: no_snow
         Real, Intent( IN ) :: dry
         Real, Intent( IN ) :: wet
         Real, Intent( IN ) :: rh
         Real, Intent( IN ) :: lai
         Real, Intent( IN ) :: Rx
         Real, Intent( IN ) :: molwt 
         Real, Intent( IN ) :: M_ac 
         Real, Intent( IN ) :: heff 
         Real, Intent( IN ) :: dif_T
!         Real, Intent( IN ) :: Rice
!         Real, Intent( IN ) :: Rsnow
         Logical, Intent( IN ) :: O3_hit
         Logical, Intent( IN ) :: NH3_hit
         Logical, Intent( IN ) :: ABFLUX

         Real            :: Rcut
         Real            :: Rsnow
         Real            :: rcdry
         Real            :: rwet
         Real            :: rh_func
         Real, Parameter :: rcut0   = 3000.0     ! [s/m]
         Real, Parameter :: rwm     = 31.5              ! Minimum NH3 cuticle resistance [s/m] from Massad et al. 2010

         Include SUBST_CONST     ! constants
C Calculate Rcut
         ! wet Cuticle
         ! If the surface is cold and wet, use dry snow.                   
         rcdry = rcut0 * a0 / Rx
         Rsnow = Calc_Rsnow( temp_g, snow, molwt, M_ac, heff, dif_T, Rx )
         IF ( temp_g .GE. stdtemp ) THEN 
!            Rwet= Rwet0         
            Rwet = Calc_Rwet( temp_g, molwt, M_ac, heff, dif_T ) 
         ELSE ! temp_g .Lt. stdtemp
!            rwet = Rice
            rwet = Calc_Rice( Rx )
         END IF ! temp 
         IF ( O3_hit ) THEN 
         ! Canopy level wet resistence Rwet to ozone was found to be about 200 s/m on basis of Keysburg exp
         ! Using LAI(1-sided) of about 6.25 measured at Keysburg gives leaf level rwet about 1250 s/m
            rwet = 1250.0    ! s/m
         ! Leaf level rwet estimated from Altimir et al 2006 gives about 1350 s/m  
         ! Dry cuticle
            rh_func = max( 0.0,( rh - 70.0 )/30.0 )
            rcdry   = 1.0 / ( ( 1.0 -rh_func) / ( rcut0 * a0 / Rx )  + rh_func / rwet )               
         End If
         If ( NH3_hit .And. ABFLUX ) Then
         ! Massad et al. 2010 Cuticular resistance
            rcdry = rwm * EXP( a_cut * ( 100.0 - rh ) )    
         End If ! O3

         Rcut = no_snow / ( lai * ( dry / rcdry     +  ! Dry Cuticle 
     &                               wet / rwet ) ) +  ! Wet Cuticle 
     &          snow * Rsnow                           ! Snow

         End Function Calc_Rcut
!-------------------------------------------------------------------------------------------------
! In-Canopy Aerodynamics Resistance
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rinc( Ra, lai ) Result( Rinc )

         Implicit None

         Real, Intent( IN ) :: Ra
         Real, Intent( IN ) :: lai
         Real               :: Rinc

! Calculate in canopy aerodynamic resistance based on the momentum attenuation coefficient derived 
! by Yi 2008 https://doi.org/10.1175/2007JAMC1667.1
         Rinc =  Ra * ( Exp( lai / 2.0 ) - 1.0 )

         End Function Calc_Rinc
!-------------------------------------------------------------------------------------------------
! Soil Quasi-laminar boundary layer Resistance
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rbg( k_vis, dif_T, lai, ustar ) Result( Rbg )

         Implicit None

         Real, Intent( IN ) :: k_vis
         Real, Intent( IN ) :: dif_T
         Real, Intent( IN ) :: lai
         Real, Intent( IN ) :: ustar
         Real               :: Rbg
         Real               :: scn
         Real               :: del0
         Real               :: ustg
         Real, Parameter    :: karman = 0.4 ! add to stage data and move functions to new module STAGE_OPS

C Calculate Canopy Covered Soil Resistance Nemitz et al 2000 https://doi.org/10.1016/S0168-1923(00)00206-9
         ! Soil quazi laminar boundary layer resistance with canopy 
         scn    = k_vis / dif_T
         ! ustar at the soil surface following Yi 2008 https://doi.org/10.1175/2007JAMC1667.1
         ustg   = max( ustar * EXP( -lai / 2.0 ), 0.001 )         
         del0   = dif_T / ( karman * ustg )
         Rbg    = ( scn - LOG( del0 / 0.10 ) ) / ( karman * ustg )
         End Function Calc_Rbg

!-------------------------------------------------------------------------------------------------
! Net resistance for deposition to soil surfaces
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rg( dif_T, k_vis, Rx, Ra, ustar, lai, temp_g, molwt, M_ac, heff, sm_v1cm,
     &                          sm_v5cm, sm_vsat, sm_vfc, sm_vwlt, sm_vres, sm_bslp, zsoil, frac_ir,
     &                          dry, wet, snow, no_snow, O3_Hit, NH3_Hit, ABFLUX ) Result( Rgc )

         Implicit None

         Include SUBST_CONST     ! constants

         Real, Intent   ( IN ) :: dif_T
         Real, Intent   ( IN ) :: k_vis
         Real, Intent   ( IN ) :: Rx
         Real, Intent   ( IN ) :: Ra
         Real, Intent   ( IN ) :: ustar
         Real, Intent   ( IN ) :: lai
         Real, Intent   ( IN ) :: temp_g
         Real, Intent   ( IN ) :: molwt 
         Real, Intent   ( IN ) :: M_ac 
         Real, Intent   ( IN ) :: heff
         Real, Intent   ( IN ) :: sm_v1cm
         Real, Intent   ( IN ) :: sm_v5cm
         Real, Intent   ( IN ) :: sm_vsat
         Real, Intent   ( IN ) :: sm_vfc
         Real, Intent   ( IN ) :: sm_vwlt
         Real, Intent   ( IN ) :: sm_vres
         Real, Intent   ( IN ) :: sm_bslp
         Real, Intent   ( IN ) :: zsoil
         Real, Intent   ( IN ) :: frac_ir
         Real, Intent   ( IN ) :: dry, wet, snow, no_snow
         Logical, Intent( IN ) :: O3_Hit, NH3_Hit, ABFLUX
         
         Real               :: Rg
         Real               :: Rgc
         Real               :: Rbgc
         Real               :: Rinc
         Real               :: Rwet
         Real               :: Rsnow
         Real               :: sm_v1cm_ir, sm_v5cm_ir, sm_v10cm
         Real               :: sm_func
         Real               :: ldry
         Real               :: dp, rgdry
         Real               :: p_wet ! wet soil surface
         Real               :: p_dry ! dry soil surface
         Real, Parameter    :: ldry_max = 0.02
         Real, Parameter    :: rg0      = 1000.0     ! [s/m]

         Rwet  = Calc_Rwet( temp_g, molwt, M_ac, heff, dif_T )
         Rsnow = Calc_Rsnow( temp_g, snow, molwt, M_ac, heff, dif_T, Rx )

         If(ABFLUX .And. NH3_Hit) Then
! Updated based on EPIC 5cm soil moisture estimates where the 25% percentile of the irrigated crop fractional soil moisture was 
! approximately equal to 60% of the field capacity.  
            If ( frac_ir .Gt. 0.0 .And. sm_v5cm .LE.  0.60 * sm_vfc ) Then            
               sm_v1cm_ir = ( 1.0 - frac_ir ) * sm_v1cm + frac_ir * 0.60 * sm_vfc
               sm_v5cm_ir = ( 1.0 - frac_ir ) * sm_v5cm + frac_ir * 0.60 * sm_vfc
            Else
               sm_v1cm_ir = sm_v1cm
               sm_v5cm_ir = sm_v5cm
            End If
            
! The following resistance parameterization is derived from measurements with soil samples of 2 cm thick (Kondo et al 1990)
! https://doi.org/10.1175/1520-0450(1990)029<0385:APOEFB>2.0.CO;2 as discussed in Sakaguchi and Zeng 2009 JGR 
! https://doi.org/10.1029/2008JD010834 According to Swenson and Lawrence 2014 (https://doi.org/10.1002/2014JD022314) and the 
! references therein the dry layer thickness varies from 1 to 3 cm. 
! From Sakaguchi and Zeng 2009 JGR Equation 10
            ldry = Calc_ldry( sm_v1cm_ir, sm_v5cm_ir, sm_vsat, zsoil )         
            dp  = dif_T * sm_vsat**2 * ( 1.0 - sm_vres / sm_vsat ) ** ( 2.0 + 3.0 / sm_bslp )         
            rgdry = max(ldry / dp,1.0e-6)
! Assumes that the soil water is an emission source. Air-soil water gradient replaces the empirical resistance to water
            Rg = no_snow * rgdry + snow * Rsnow 

         Else If ( O3_Hit ) Then
! Following based on measurements Fares et al 2014 https://doi.org/10.1016/j.agrformet.2014.08.014 for sandy soil 
! forests at 10cm measured soil moisture and Fumagalli et al. 20016 https://doi.org/10.1016/j.agrformet.2016.07.011 for sandy loam soils
! Here an asymptotic function was applied to set lower and upper bounds in the resistance as reported by Fumagalli et al. 2016
            sm_v10cm = min(sm_v1cm * exp( 0.09 * GRAV )**(1.0/sm_bslp), sm_vsat )
            sm_func = max( tiny(0.0)**(1.0/sm_bslp) * PI , ( sm_v10cm-sm_vwlt ) / sm_vfc )
            rgdry   = 250.0 + 2000.0 * atan( sm_func**sm_bslp ) /PI  

            Rg = no_snow * ( 1.0/ ( dry / rgdry + wet / rwet ) ) + snow * Rsnow   
         Else 
            rgdry  = rg0 * a0 / Rx
            Rg = no_snow * ( 1.0/ ( dry / rgdry + wet / rwet ) ) + snow * Rsnow  
         End If
         Rinc =  Calc_Rinc( Ra, lai )
         Rbgc = Calc_Rbg( k_vis, dif_T, lai, ustar )

         Rgc = Rg + Rbgc + Rinc 

         End Function Calc_Rg

!-------------------------------------------------------------------------------------------------
! CMAQ v5.3 Aerosol Deposition Velocity parameterization. Note that this is the v5.3 implementation
! in the STAGE deposition option, evaluated in Appel et al. 2021 and Benish et al. 2022, and differs 
! from Shu et al. in the land use tiling and implementaiton of the vegetation factor
! 
! References: 
! Appel, K.W., et. al., The Community Multiscale Air Quality (CMAQ) model versions 5.3 and 5.3. 1: 
!    system updates and evaluation. Geosci. Model Dev. 14, 2867-2897, 
!    https://doi.org/10.5194/gmd-14-2867-2021, 2021
! Benish, S.E., et al., Long-term Regional Trends of Nitrogen and Sulfur Deposition in the United 
!    States from 2002 to 2017, Atmos. Phys. Chem. Discussions, https://doi.org/10.5194/acp-2022-201,
!    2022
! Shu et al., Particle dry deposition algorithms in CMAQ version 5.3: characterization of critical 
!    parameters and land use dependence using DepoBoxTool version 1.0, Geosci. Model Dev. Discussions, 
!    https://doi.org/10.5194/gmd-2021-129, 2021
! 
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
! Aerosol surface resistance for vegetative surfaces
!-------------------------------------------------------------------------------------------------
         Pure Function RD_Veg(Vghat, ustar, SC,l_width, lai) Result( RD )
                  
         Implicit None

         Include SUBST_CONST     ! constants

         Real, Intent( IN ) :: Vghat
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: SC
         Real, Intent( IN ) :: l_width
         Real, Intent( IN ) :: lai
         Real               :: RD
         Real               :: V_fac
         Real               :: ST
         Real               :: EIM
         Real               :: EIB

         V_fac   = max( lai, 1.0 )
         ST      = Vghat * ustar / ( GRAV * l_width )
         EIB     = Calc_EIB( SC )
         EIM     = EIM_Veg( ST )
         RD      = 1.0 / ( V_fac * ustar * ( EIB + EIM ) )

         End Function RD_Veg
!-------------------------------------------------------------------------------------------------
! Aerosol surface resistance for water and bare soil surfaces
!-------------------------------------------------------------------------------------------------
         Pure Function RD_Smooth(Vghat, ustar, SC, nu ) Result( RD )
                  
         Implicit None

         Include SUBST_CONST     ! constants

         Real, Intent( IN ) :: Vghat
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: SC
         Real, Intent( IN ) :: nu
         Real               :: RD
         Real               :: ST
         Real               :: EIM
         Real               :: EIB

         ST      = Vghat * ustar**2 / ( GRAV * nu )
         EIB     = Calc_EIB( SC )
         EIM     = EIM_Smooth( ST )
         RD      = 1.0 / ( ustar * ( EIB + EIM ) )

         End Function RD_Smooth
!-------------------------------------------------------------------------------------------------
! CMAQ v5.3 Aerosol Deposition Velocity parameterization. 
! 
! Reference:
! Venkatram, A. and Pleim, J.: The electrical analogy does not apply to modeling dry deposition of 
!    particles, Atmos. Environ., 33, 30753076, https://doi.org/10.1016/S1352-2310(99)00094-1, 1999.
! 
!-------------------------------------------------------------------------------------------------
         Pure Function aero_depv(veg, Ra, Vghat, ustar, SC, NU, l_aero, lai ) Result( depv )
                  
         Implicit None

         Real, Intent( IN ) :: veg
         Real, Intent( IN ) :: Ra
         Real, Intent( IN ) :: Vghat
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: SC
         Real, Intent( IN ) :: NU
         Real, Intent( IN ) :: l_aero
         Real, Intent( IN ) :: lai
         Real               :: RD_Can
         Real               :: RD_Smth
         Real               :: depv


         If( veg .Gt. 0.0  ) Then
            RD_Can = RD_Veg( Vghat, ustar, SC,l_aero, lai)
         Else
            RD_Can = 1.0e6          
         End If
         RD_Smth = RD_Smooth(Vghat, ustar, SC, NU )

         depv =         veg   * Vghat / ( 1.0 - EXP( -Vghat * ( Ra + RD_Can ) ) ) + 
     &          ( 1.0 - veg ) * Vghat / ( 1.0 - EXP( -Vghat * ( Ra + RD_Smth ) ) )

         End Function aero_depv
!-------------------------------------------------------------------------------------------------
! Brownian Diffusion Collection Efficiency
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_EIB( SC ) Result( EIB )

         Implicit None

         Real, Intent( IN ) :: SC
         Real               :: EIB

         EIB = SC ** (-twothirds)

         End Function Calc_EIB
!-------------------------------------------------------------------------------------------------
! Impaction Collection Efficiency for vegetation following Slinn 1982
!-------------------------------------------------------------------------------------------------
         Pure Function EIM_Veg( ST ) Result( EIM )

         Implicit None

         Real, Intent( IN ) :: ST
         Real               :: EIM

         EIM     = ST**2 / ( 1.0 + ST**2 ) ! Slinn 1982 equation 28           

         End Function EIM_Veg
!-------------------------------------------------------------------------------------------------
! Impaction Collection Efficiency for water and soil surfaces following Giorgi 1986
!-------------------------------------------------------------------------------------------------
         Pure Function EIM_Smooth( ST ) Result( EIM )

         Implicit None

         Real, Intent( IN ) :: ST
         Real               :: EIM

         EIM     = ST**2 / ( 400.0 + ST**2 ) ! Giorgi 1986 Equation 17     

         End Function EIM_Smooth
!-------------------------------------------------------------------------------------------------
!                           Aerosol Dry Deposition Code Following
!         Emerson et al 2020 PNAS https://www.pnas.org/cgi/doi/10.1073/pnas.2014761117
! 
! This code follows the Emerson et al 2020 parameterization with modifications to account for
! seasonality due to changing LAI and continuous scaling from vegetated to non-vegetated surfaces
! This parmaterization matches Emerson et al 2020 when LAI is approximately 5 for vegetation 
! categories and for non-vegitated surfaces.  
!
! Refernces: 
! Emerson et al., Revisiting particle dry deposition and its role in radiative effect estimates, 
!    Proc. Natl. Acad. Sci., 117, 26076-26082, https://www.pnas.org/cgi/doi/10.1073/pnas.2014761117, 
!    2020
! Zhang et al. A size-segregated particle dry depositionscheme for an atmospheric aerosol module. 
!    Atmos. Environ.35, 549560, https://doi.org/10.1016/S1352-2310(00)00326-5, 2001 
!-------------------------------------------------------------------------------------------------
! Aerosol surface resistance for vegitative surfaces
!-------------------------------------------------------------------------------------------------
         Pure Function RD_Veg_E20(Vghat, ustar, SC,l_width, lai, Dp, wet, Alpha, EINHAT) Result( RD )
                  
         Implicit None

         Include SUBST_CONST     ! constants

         Real, Intent( IN ) :: Vghat
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: SC
         Real, Intent( IN ) :: l_width
         Real, Intent( IN ) :: lai
         Real, Intent( IN ) :: alpha
         Real, Intent( IN ) :: Dp
         Real, Intent( IN ) :: wet
         Real, Intent( IN ) :: EINHAT
         Real               :: RD
         Real               :: veg_ustar ! intgrated lai * ustar through the canopy is equal to 3 * ustar at lai = 5.5
         Real               :: ST
         Real               :: EIM ! Impaction Collection Efficiency for vegetation following Emerson et al. 2020
         Real               :: EIN ! Interception Collection Efficiency for vegetation following Emerson et al 2020
         Real               :: EIB ! Brownian Diffusion Collection Efficiency Following Emerson et al. 2020
         Real               :: R1  ! collection efficiency following Slinn 1982

! Integration of lai(z)/hc u*(z) from 0 to hc.   
         veg_ustar = max((4.0 - (2.0 * lai + 4.0) * exp( -lai/2.0 )) * ustar, 1.0e-3) ! Replaces the constant 3 in Zhang et 2001
         ST        = Vghat * ustar / ( GRAV * l_width )
         R1        = max( Exp( -SQRT( ST ) ), 1.0e-20) ! only apply to particles with diameter larger than 5 um Zhang et al 2001
         EIB       = Calc_EIB_E20( SC )
         EIN       = EIN_Veg_E20( Dp, l_width ) * EINHAT
         EIM       = EIM_Veg_E20( ST, alpha )
         RD        = 1.0 / (      wet  * ( veg_ustar * ( EIB + EIN + EIM ) ) +      ! Wet surface
     &                     (1.0 - wet) * ( veg_ustar * ( EIB + EIN + EIM ) * R1 ) ) ! Dry surface

         End Function RD_Veg_E20
!-------------------------------------------------------------------------------------------------
! Aerosol surface resistance for water and bare soil surfaces
!-------------------------------------------------------------------------------------------------
         Pure Function RD_Smooth_E20(Vghat, ustar, lai, SC, nu, wet ) Result( RD )
                  
         Implicit None

         Include SUBST_CONST     ! constants

         Real, Intent( IN ) :: Vghat
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: lai
         Real, Intent( IN ) :: SC
         Real, Intent( IN ) :: nu
         Real, Intent( IN ) :: wet
         Real               :: ustg ! ustar at ground = ustar if lai=0
         Real               :: RD
         Real               :: ST
         Real               :: EIM
         Real               :: EIB
         Real               :: R1

         ustg    = max( ustar * exp( -lai/2.0 ), 0.001 )
         ST      = Vghat * ustg**2 / ( GRAV * nu )
         R1      = max( Exp( -SQRT( ST ) ), 1.0e-20) ! only apply to particles with diameter larger than 5 um Zhang et al 2001
         EIB     = Calc_EIB_E20( SC )
         EIM     = EIM_SMOOTH_E20( ST )
         RD      = 1.0 / (        wet  * ( 3.0 * ustg * ( EIB + EIM ) ) +      ! Wet surface
     &                     (1.0 - wet) * ( 3.0 * ustg * ( EIB + EIM ) * R1 ) ) ! Dry Surface

         End Function RD_Smooth_E20
!-------------------------------------------------------------------------------------------------
! Aerosol Deposition Velocity
!-------------------------------------------------------------------------------------------------
         Pure Function aero_depv_E20(veg, lai, Ustar, Ra, wet, l_aero, Alpha, DG, SC, NU, VGHAT, EINHAT)
     &                               Result( depv )
                  
         Implicit None

         Real, Intent( IN ) :: veg
         Real, Intent( IN ) :: lai
         Real, Intent( IN ) :: Ustar
         Real, Intent( IN ) :: Ra
         Real, Intent( IN ) :: wet
         Real, Intent( IN ) :: l_aero
         Real, Intent( IN ) :: Alpha
         Real, Intent( IN ) :: DG
         Real, Intent( IN ) :: SC
         Real, Intent( IN ) :: NU
         Real, Intent( IN ) :: Vghat
         Real, Intent( IN ) :: EINHAT ! interception modal integration term
         Real               :: depv
         Real               :: RD_VegL   ! Resistance to vegetation leaves
         Real               :: RD_VegG   ! Resistance to vegetation ground
         Real               :: RD_GW  ! Resistance to non-vegetated ground or water
         Real               :: Rc     ! Two layer canopy resitance
         Real               :: Rinc   ! In canopy resistance for two layer model 

         RD_GW = RD_Smooth_E20(VGHAT, Ustar, 0.0, SC, NU, wet )
         If( lai .gt. 0.0 .and. veg .gt. 0.0 ) Then
            Rinc      = Calc_Rinc( Ra, lai )
            RD_VegL   = RD_Veg_E20( Vghat, Ustar, SC,l_aero, lai, DG, wet, Alpha, EINHAT )
            RD_VegG   = RD_Smooth_E20(VGHAT, Ustar, lai, SC, NU, wet )
! Resistance to:                  Soil              Canopy
            Rc   = 1.0 /( 1.0/(Rinc + RD_VegG) + 1.0/RD_VegL )  
            depv =       veg   * Vghat / ( 1.0 - EXP( -Vghat * ( Ra + Rc ) ) ) +  
     &           ( 1.0 - veg ) * Vghat / ( 1.0 - EXP( -Vghat * ( Ra + RD_GW ) ) )
         Else
            depv = Vghat / ( 1.0 - EXP( -Vghat * ( Ra + RD_GW ) ) )
         End If
         End Function aero_depv_E20
!-------------------------------------------------------------------------------------------------
! Brownian Diffusion Collection Efficiency 
! Following Emerson et al 2020 PNAS https://www.pnas.org/cgi/doi/10.1073/pnas.2014761117
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_EIB_E20( SC ) Result( EIB )

         Implicit None

         Real, Intent( IN ) :: SC
         Real               :: EIB
         Real, Parameter    :: CIB = 0.2

         EIB = CIB * SC ** (-twothirds)

         End Function Calc_EIB_E20
!-------------------------------------------------------------------------------------------------
! Impaction Collection Efficiency for vegetation
! Following Emerson et al 2020 PNAS https://www.pnas.org/cgi/doi/10.1073/pnas.2014761117
!-------------------------------------------------------------------------------------------------
         Pure Function EIM_Veg_E20( ST, Alpha ) Result( EIM )

         Implicit None

         Real, Intent( IN ) :: ST
         Real, Intent( IN ) :: Alpha        ! Zhang et al 2010 Table 3
         Real               :: EIM
         Real, Parameter    :: CIM = 0.4    ! Emerson et al. 2020 (Table S1)
         Real, Parameter    :: Beta = 1.7

         EIM     = CIM * ( ST /( Alpha + ST ) ) ** Beta ! Emerson et al 2020 equation 4           

         End Function EIM_Veg_E20
!-------------------------------------------------------------------------------------------------
! Impaction Collection Efficiency for water and soil surfaces following Giorgi 1986
!-------------------------------------------------------------------------------------------------
         Pure Function EIM_Smooth_E20( ST ) Result( EIM )

         Implicit None

         Real, Intent( IN ) :: ST
         Real               :: EIM
         Real, Parameter    :: CIM = 0.4     ! Emerson et al 2020 (Table S1)
         Real, Parameter    :: Alpha = 100.0 ! Zhang et al. 2001 Table 3
         Real, Parameter    :: Beta = 1.7

         EIM     = CIM * ( ST /( Alpha + ST ) ) ** Beta ! Emerson et al 2020 equation 4           

         End Function EIM_Smooth_E20

!-------------------------------------------------------------------------------------------------
! Interception Collection Efficiency for vegetation following Emerson et al 2020
!-------------------------------------------------------------------------------------------------
         Pure Function EIN_Veg_E20( Dp,l_aero ) Result( EIN )

         Implicit None

         Real, Intent( IN ) :: Dp
         Real, Intent( IN ) :: l_aero
         Real               :: EIN
         Real, Parameter    :: CIN = 2.5

         EIN     = CIN * ( Dp / l_aero ) ** 0.8 ! Emerson et al 2020 equation            

         End Function EIN_Veg_E20

!-------------------------------------------------------------------------------------------------
!                           Aerosol Dry Deposition Code Following
!         Pleim et al 2022 
! 
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
! Aerosol Deposition Velocity
!-------------------------------------------------------------------------------------------------
         Pure Function aero_depv_P22(veg, lai, bai, Ustar, Ubar, Ra, Water, aleaf, ahair, fhair, SC,
     &                               NU, VGHAT, SeaIce, SST) Result( depv )
                  
         Implicit None

         Real,    Intent( IN ) :: veg
         Real,    Intent( IN ) :: lai
         Real,    Intent( IN ) :: bai
         Real,    Intent( IN ) :: Ustar
         Real,    Intent( IN ) :: Ubar
         Real,    Intent( IN ) :: Ra
         Logical, Intent( IN ) :: Water
         Real,    Intent( IN ) :: aleaf
         Real,    Intent( IN ) :: ahair
         Real,    Intent( IN ) :: fhair
         Real,    Intent( IN ) :: SC
         Real,    Intent( IN ) :: NU
         Real,    Intent( IN ) :: Vghat
         Real,    Intent( IN ) :: SeaIce
         Real,    Intent( IN ) :: SST
         Real                  :: depv
         Real                  :: RD_Can   ! Resistance to vegetation leaves
         Real                  :: RD_Smth   ! Resistance to vegetation ground
         Real                  :: Vdv    ! Vegetation deposition velocity
         Real                  :: Vdnv   ! Non-Vegetation depositon velocity
         
         Vdv  = 0.0
         Vdnv = 0.0
         If( lai .gt. 0.0 .and. veg .gt. 0.0 ) Then
            RD_Can    = RD_Veg_P22( Vghat, Ustar, SC, aleaf, ahair, fhair, lai )
            Vdv = Vghat  / ( 1.0 - EXP( -Vghat * ( Ra + RD_Can ) ) )
         End If
         If( 1.0 - veg .gt. 0.001 ) Then
            RD_Smth = RD_Smooth_P22(VGHAT, Ustar, Ubar, BAI, SC, NU, SST, SeaIce, Water )
            Vdnv      = Vghat / ( 1.0 - EXP( -Vghat * ( Ra + RD_Smth ) ) )
         End If
         depv = veg * Vdv + (1.0-veg)*Vdnv
         End Function aero_depv_P22

         Pure Function RD_Veg_P22(Vghat, ustar, SC, aleaf, ahair, fhair, lai ) Result( RD )
                  
         Implicit None

         Include SUBST_CONST     ! constants

         Real, Intent( IN ) :: Vghat
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: SC
         Real, Intent( IN ) :: aleaf
         Real, Intent( IN ) :: ahair
         Real, Intent( IN ) :: fhair
         Real, Intent( IN ) :: lai
         Real               :: RD
         Real               :: veg_ustar 
         Real               :: ST1
         Real               :: ST2
         Real               :: EIM 
         Real               :: EIB 

! Integration of lai(z)/hc u*(z) from 0 to hc.   
         veg_ustar = max(1.0,lai) * ustar ! Replaces the constant 3 in Zhang et 2001
         ST1       = Vghat * ustar / ( GRAV * aleaf )
         ST2       = Vghat * ustar / ( GRAV * ahair )
         EIB       = Calc_EIB_Land_P22( SC )
         EIM       = EIM_Veg_P22( ST1, ST2, fhair )
         RD        = 1.0 / ( veg_ustar * ( EIB + EIM ) )

         End Function RD_Veg_P22

!-------------------------------------------------------------------------------------------------
! Aerosol surface resistance for water and bare soil surfaces
!-------------------------------------------------------------------------------------------------
         Pure Function RD_Smooth_P22(Vghat, ustar, Ubar, BAI, SC, nu, SST, SeaIce, water ) Result( RD )
                  
         Implicit None

         Include SUBST_CONST     ! constants

         Real,    Intent( IN ) :: Vghat
         Real,    Intent( IN ) :: ustar
         Real,    Intent( IN ) :: Ubar
         Real,    Intent( IN ) :: BAI
         Real,    Intent( IN ) :: SC
         Real,    Intent( IN ) :: nu
         Real,    Intent( IN ) :: SST
         Real,    Intent( IN ) :: SeaIce
         Logical, Intent( IN ) :: Water
         Real                  :: RD
         Real                  :: ST
         Real                  :: EIM
         Real                  :: EIB

         ST      = Vghat * ustar**2 / ( GRAV * nu )
         If( Water .and. SST .gt. -31.0 .And. SeaIce .lt. 0.5 ) Then
            EIB  = Calc_EIB_Water_P22( SC, Ubar, ustar, SST )
         Else 
            EIB  = Calc_EIB_Land_P22( SC )
         End If
         EIM     = EIM_SMOOTH_P22( ST )
         RD      = 1.0 / ( ustar * BAI * ( EIB + EIM ) )

         End Function RD_Smooth_P22
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_EIB_Land_P22( SC ) Result( EIB )

         Implicit None

         Real, Intent( IN ) :: SC
         Real               :: EIB
         Real, Parameter    :: CIB = 1.0/3.0

         EIB = CIB * SC ** (-twothirds)

         End Function Calc_EIB_Land_P22

! For water include effects of whitecaps - Hummelshoj et al. (1992)
         Pure Function Calc_EIB_Water_P22( SC, Ubar, ustar, SST ) Result( EIB )

         Implicit None

         Real, Intent( IN ) :: SC
         Real, Intent( IN ) :: Ubar
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: SST
         Real               :: EIB
         Real               :: alfbob
         Real               :: awc
         Real               :: bwc
         Real               :: Ewc 
         Real, Parameter    :: CIB = 1.0/3.0
         
         awc    = 8.46e-5 + 1.63e-6 * SST - 3.35e-8 * SST**2
         bwc    = 3.354 - 0.062 * SST
         alfbob = awc * ( Ubar + bwc )**2   ! Albert 2016 with SST deg-C
         Ewc    = alfbob * ustar/Ubar
         EIB    = CIB * (1.0-alfbob) * SC ** (-twothirds) + Ewc

         End Function Calc_EIB_Water_P22
!-------------------------------------------------------------------------------------------------
         Pure Function EIM_Veg_P22( ST1, ST2, fhair ) Result( EIM )

         Implicit None

         Real, Intent( IN ) :: ST1
         Real, Intent( IN ) :: ST2
         Real, Intent( IN ) :: fhair 
         Real               :: EIM

         EIM     = ( 1.0 - fhair ) * ST1**2 /( 1.0 + ST1**2 ) + 
     &                     fhair   * ST2**2 /( 1.0 + ST2**2 )

         End Function EIM_Veg_P22

!-------------------------------------------------------------------------------------------------
         Pure Function EIM_SMOOTH_P22( ST ) Result( EIM )

         Implicit None

         Real, Intent( IN ) :: ST
         Real               :: EIM

         EIM     = 10.0**(-3.0/ST)

         End Function EIM_SMOOTH_P22

      End Module Resist_Funcs
