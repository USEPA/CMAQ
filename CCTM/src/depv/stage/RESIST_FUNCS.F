
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

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/arc/CCTM/src/depv/stage/RESIST_FUNCS.F,v 1.11 2012/04/19 17:03:23 yoj Exp $

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module Resist_Funcs
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
! This module contains functions and subroutines to calculate variables
! used to estimate the air-surface exchange using the STAGE deposition option
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!      Use STAGE_Data
      Use ASX_DATA_MOD, only:pr, twothirds
      
      Implicit None
      Real, Parameter, Private :: a0         = 8.0        ! [dim'less]

      Contains 
!--------------------------------------------------------------------------------------------------
! Calcuate soil dry diffusive length until saturation
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_ldry( sm_v1cm, sm_v5cm, sm_vsat, zsoil1 ) Result( ldry )

         Implicit None        

         Real, Intent( IN ) :: sm_v1cm
         Real, Intent( IN ) :: sm_v5cm
         Real, Intent( IN ) :: sm_vsat
         Real, Intent( IN ) :: zsoil1
         Real, Parameter    :: ldry_max = 0.02
         Real               :: ldry
    
! The following resistance parameterization is derived from measurements with soil samples of 2 cm thick (Kondo et al 1990)
! https://doi.org/10.1175/1520-0450(1990)029<0385:APOEFB>2.0.CO;2 as discussed in Sakaguchi and Zeng 2009 JGR 
! https://doi.org/10.1029/2008JD010834 According to Swenson and Lawrence 2014 (https://doi.org/10.1002/2014JD022314) and the 
! references therin the dry layer thickness varies from 1 to 3 cm. 
! From Sakaguchi and Zeng 2009 JGR Equation 10
         ldry = ldry_max * ( Exp( ( 1.0 - sm_v1cm / sm_vsat ) ** 5 ) - 1.0 ) / 1.718

         If(ldry .gt. zsoil1) Then
            ldry = zsoil1 + ( ldry_max - zsoil1 ) * ( Exp( ( 1.0 - sm_v5cm / sm_vsat ) ** 5 ) - 1.0 ) / 1.718
         End If

         End Function Calc_ldry
!--------------------------------------------------------------------------------------------------
! Calcuate Quasi-laminar boundary layer resistance to deposition to water surfaces
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
! Calcuate Resistance to deposition to surface waters
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_Rwater( ustar, q_2m, temp_2m, temp_g, tw, MVol, H, O3_Hit, 
     &                         Hg_Hit, sea_ice  ) Result( Rwater )         
         Implicit None

         Include SUBST_CONST     ! constants
         
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: q_2m   ! 2m temp
         Real, Intent( IN ) :: temp_2m   ! 2m temp
         Real, Intent( IN ) :: temp_g   ! 2m temp
         Real, Intent( IN ) :: tw   ! water skin temp
         Real, Intent( IN ) :: H    ! Molar volume (LeBas)
         Real, Intent( IN ) :: MVol    ! Molar volume (LeBas)
         logical, Intent( IN ) :: O3_Hit
         logical, Intent( IN ) :: Hg_Hit
         logical, Intent( IN ) :: sea_ice
         Real, Parameter :: rt25inK    = 1.0/(stdtemp + 25.0) ! 298.15K = 25C
         Real, Parameter :: d3         = 1.38564e-2 ! [dim'less]
         Real            :: pChang, kwChang, ciodide, qiodide
         Real            :: dw25, dw, kvisw, scw_pr_23
         Real            :: Rwater

! from Hayduk and Laudie
         dw25 = 13.26e-5 / ( 0.8904**1.14 * MVol**0.589 )
         kvisw = 0.017 * EXP( -0.025 * ( tw - stdtemp ) )
         dw    = dw25 * ( tw * rt25inK ) * ( 0.009025 / kvisw )
         scw_pr_23 = ( ( kvisw / dw ) / pr ) ** twothirds
! All species but Hg and O3
         Rwater = scw_pr_23 / ( H * d3 * ustar )

         IF ( O3_Hit ) THEN   !implement Chang et al(2004)
c        pChang is a/H or alpha/H which would be 1/H in current model
c        note that in Chang et al (2004) and Garland et al (1980) their H is Cair/Cwater with is
c        the inverse of heff
            pChang = 1.75
            kwChang = (d3*ustar)/scw_pr_23

c        If a file of chlorophyll concentrations is provided, Iodide concentration are estimated from
c        a fit to the Rebello et al 1990 data. The slope and correlation are given in the paper
c        but not the intercept, so the data in Tables 3 & 4 were fit to get the relationship below.
c        The regression gives the concentration in umol/L and is converted to mol/L for use in Chang et al eq.
c        The slope and correlation are a slightly different than in Table 5.
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
! Calcuate Resistance for deposition to ice surfaces
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_Rice( Rx ) Result( Rice )

         Implicit None
         
         Real, Intent( IN ) :: Rx
         Real            :: Rice
         Real, Parameter :: rsnow0     = 10000.0    ! to match the maximum snow deposition (0.01 cm/s) of D Helmig et al. 2007 
                                                    ! this should be ~14000 for stage and 15000 for m3dry due to the relative reactivity adjustment
         Rice = rsnow0 * a0 / Rx                                                   

         End Function Calc_Rice

!--------------------------------------------------------------------------------------------------
! Calcuate Resistance for deposition to ice surfaces
!--------------------------------------------------------------------------------------------------
         Pure Function Calc_Rsnow( temp_g, Rwet, Rice, snow ) Result( Rsnow )

         Implicit None

         Include SUBST_CONST     ! constants
         
         Real, Intent( IN ) :: temp_g
         Real, Intent( IN ) :: snow
         Real, Intent( IN ) :: Rwet
         Real, Intent( IN ) :: Rice
         Real               :: Rsnow
         Real               :: melt_snow, ice_snow
         Real, Parameter    :: rsndiff    = 10.0       ! snow diffusivity fac s/m

! Liquid snow fraction modeled as a system dominated by van der Walls forces following Dash et al. 1999 S. Rep. Prog. Phys. 
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
! Quazi Laminar Resistance to leaf following Jensen and Hummelshoj 1995/1997 doi:10.1016/0168-1923(94)05083-I
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rb_leaf( k_vis, dif_T, ustar, l_leaf, lai ) Result( Rb_leaf )

         Implicit None

         Real, Intent( IN ) :: k_vis
         Real, Intent( IN ) :: dif_T
         Real, Intent( IN ) :: ustar
         Real, Intent( IN ) :: l_leaf
         Real, Intent( IN ) :: lai
         Real               :: Rb_leaf
         
         Rb_leaf = k_vis / ( dif_T * ustar ) * 
     &           ( l_leaf * ustar / (k_vis * max(lai,1.0)**2 ) )**(1.0/3.0)

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
         Pure Function Calc_Rcut( temp_g, Rx, Rwet0, Rice, Rsnow, a_cut, snow, no_snow, dry, wet, rh, lai, 
     &                                 O3_hit, NH3_hit, ABFLUX ) Result( Rcut )

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
         Real, Intent( IN ) :: Rwet0
         Real, Intent( IN ) :: Rice
         Real, Intent( IN ) :: Rsnow
         Logical, Intent( IN ) :: O3_hit
         Logical, Intent( IN ) :: NH3_hit
         Logical, Intent( IN ) :: ABFLUX

         Real            :: Rcut
         Real            :: rcdry
         Real            :: rwet
         Real            :: rh_func
         Real, Parameter :: rcut0   = 3000.0     ! [s/m]
         Real, Parameter :: rwm     = 31.5              ! Minimum NH3 cuticle resistance [s/m] from Massad et al. 2010

         Include SUBST_CONST     ! constants
C Calcuate Rcut
         ! wet Cuticle
         ! If the surface is cold and wet, use dry snow.                   
         rcdry = rcut0 * a0 / Rx
         IF ( O3_hit ) THEN 
         ! Canopy level wet resistence Rwet to ozone was found to be about 200 s/m on basis of Keysburg exp
         ! Using LAI(1-sided) of about 6.25 measured at Keysburg gives leaf level rwet about 1250 s/m
            rwet = 1250.0    ! s/m
         ! Leaf level rwet estimated from Altimir et al 2006 gives about 1350 s/m  
         ! Dry cuticule
            rh_func = max( 0.0,( rh - 70.0 )/30.0 )
            rcdry   = 1.0 / ( ( 1.0 -rh_func) / ( rcut0 * a0 / Rx )  + rh_func / rwet )               
         End If
         If ( NH3_hit .And. ABFLUX ) Then
         ! Massad et al. 2010 Cuticular resistance
            rcdry = rwm * EXP( a_cut * ( 100.0 - rh ) )    
         End If ! O3
         IF ( temp_g .GE. stdtemp ) THEN 
            rwet= Rwet0          
         ELSE ! temp_g .Lt. stdtemp
            rwet = Rice
         END IF ! temp 
! this should be Calc_Rsnow

         Rcut = no_snow / ( lai * ( dry / rcdry     +  ! Dry Cuticle 
     &                               wet / rwet ) ) +  ! Wet Cuticle 
     &           snow * Rsnow                          ! Snow

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

C Calcuate Canopy Covered Soil Resistance Nemitz et al 2000 https://doi.org/10.1016/S0168-1923(00)00206-9
         ! Soil quazi laminar boundary layer resistance with canopy 
         scn    = k_vis / dif_T
         ! ustar at the soil surface following Yi 2008 https://doi.org/10.1175/2007JAMC1667.1
         ustg   = max( ustar * EXP( -lai / 2.0 ), 0.001 )         
         del0   = dif_T / ( karman * ustg )
         Rbg    = ( scn - LOG( del0 / 0.10 ) ) / ( karman * ustg )
         End Function Calc_Rbg

!-------------------------------------------------------------------------------------------------
! Net resistance for depostioin to soil surfaces
!-------------------------------------------------------------------------------------------------
         Pure Function Calc_Rg( dif_T, Rx, Rsnow, Rwet, sm_v1cm,
     &                          sm_v5cm, sm_vsat, sm_vfc, sm_vwlt, sm_vres, sm_bslp, zsoil, frac_ir,
     &                          dry, wet, snow, no_snow, O3_Hit, NH3_Hit, ABFLUX ) Result( Rg )

         Implicit None

         Include SUBST_CONST     ! constants

         Real, Intent   ( IN ) :: dif_T
         Real, Intent   ( IN ) :: Rx
         Real, Intent   ( IN ) :: Rsnow
         Real, Intent   ( IN ) :: Rwet
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
         Real               :: sm_v1cm_ir, sm_v5cm_ir, sm_v10cm
         Real               :: sm_func
         Real               :: ldry
         Real               :: dp, rgdry
         Real               :: p_wet ! wet soil surface
         Real               :: p_dry ! dry soil surface
         Real, Parameter    :: ldry_max = 0.02
         Real, Parameter    :: rg0      = 1000.0     ! [s/m]

         If(ABFLUX .And. NH3_Hit) Then
! Updated based on EPIC 5cm soil moisture estimates where the 25% percentile of the irrigated crop fractional soil moisture was 
! approximately equal approximately equal to 60% of the field capacity.  
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
! references therin the dry layer thickness varies from 1 to 3 cm. 
! From Sakaguchi and Zeng 2009 JGR Equation 10
            ldry = Calc_ldry( sm_v1cm_ir, sm_v5cm_ir, sm_vsat, zsoil )         
            dp  = dif_T * sm_vsat**2 * ( 1.0 - sm_vres / sm_vsat ) ** ( 2.0 + 3.0 / sm_bslp )         
            rgdry = max(ldry / dp,1.0e-6)
! Assumes that the soil water is an emission source. Air-soil water gradient repalces the emperical resistance to water
            Rg = no_snow * rgdry + snow * Rsnow 

         Else If ( O3_Hit ) Then
! Following based on measurements Fares et al 2014 https://doi.org/10.1016/j.agrformet.2014.08.014 for sandy soil 
! forests at 10cm measured soil moisture and Fumagalli et al. 20016 https://doi.org/10.1016/j.agrformet.2016.07.011 for sandy loam soils
! Here an asymptotic function was applied to set lower and upper bounds in the resistance as repoerted by Fumagalli et al. 2016
            sm_v10cm = min(sm_v1cm * exp( 0.09 * GRAV )**(1.0/sm_bslp), sm_vsat )
            sm_func = max( tiny(0.0)**(1.0/sm_bslp) * PI , ( sm_v10cm-sm_vwlt ) / sm_vfc )
            rgdry   = 250.0 + 2000.0 * atan( sm_func**sm_bslp ) /PI  

            Rg = no_snow * ( 1.0/ ( dry / rgdry + wet / rwet ) ) + snow * Rsnow   
         Else 
            rgdry  = rg0 * a0 / Rx
            Rg = no_snow * ( 1.0/ ( dry / rgdry + wet / rwet ) ) + snow * Rsnow  
         End If

         End Function Calc_Rg
      End Module Resist_Funcs
