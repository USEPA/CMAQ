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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/arc/CCTM/src/depv/m3dry/m3dry.F,v 1.12 2012/01/19 14:19:43 yoj Exp $

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE STAGE_MOD 

C-------------------------------------------------------------------------------
C Name:     Surface Tiled Aerosol and Gaseous Exchange (STAGE)
C Purpose:  Computes aerosol and gaseous air surface exchange for sub-
C           grid land use categories. All gaseous species are treated 
C           as having bidirectional exchange following the two layer 
C           model from Nemitz et al. 2001 and aerosol species deposition 
C           follows Binkowski and Shankar 1995. Note that the Nemitz et al. 
C           2001 parameterization reduces to a standard deposition velocity
C           if the pollutant concentration on the leaf, in the leaf stomata, 
C           in the soil are zero.
C 
C References:
C          Nemitz et al. 2001 Q. J. R. Meteorol. Soc DOI: 10.1002/qj.49712757306
C          Binkowski and Shankar 1995 JGR  DOI: 10.1029/95JD02093
C 
C Default variables output area weighted Vd
C Optional variables output land use specific LAI, RA, U*, Z0, and Vd
C
C Revised:  1 Dec 2017  Original version.  (J. Bash)
C-------------------------------------------------------------------------------

      Use GRID_CONF           ! horizontal & vertical domain specifications
      Use LSM_MOD             ! Land surface data
      Use ASX_DATA_MOD
      USE UTILIO_DEFN      
      USE CGRID_SPCS          ! CGRID mechanism species
      USE STAGE_DATA
      USE AERO_DATA, Only: N_MODE

      IMPLICIT NONE

C shared variables 
      INTEGER, ALLOCATABLE, SAVE :: DEPV_SUR( : )   ! pointer to surrogate
      REAL, ALLOCATABLE, SAVE :: VDEP( : )    ! deposition  velocity [ m/s ]
      REAL, ALLOCATABLE, SAVE :: VDEPJ( :,: ) ! deposition  velocity [ m/s ]
      REAL,    SAVE           :: xcent
      REAL,    SAVE           :: ycent
      REAL, SAVE                 :: scc_pr_23( dep_gas_all )        ! (SCC/PR)**2/3, fn of DIF0
      Real, Save                 :: molwt( dep_gas_all )
      INTEGER, PARAMETER         :: N_AE_DEP_SPC = 9 
C land use indexes
      Logical, Allocatable, Save :: Water( : )
      Logical, Allocatable, Save :: Ag( : )
      Logical, Allocatable, Save :: Forest( : )
      Real,    Allocatable, Save :: l_leaf( : )    ! characteristic leaf width from Massad et al. 2010 Table 6
      Real,    Allocatable, Save :: l_aero( : )    ! characteristic leaf radius from Zhang et al. 2001 Table 3
      Real,    Allocatable, Save :: a_cut( : )     ! NH3 cuticular resistance exponential term Massad et al. 2010 Table 8
C gas phase species indexes
      Integer, Save              :: n_HONO
      Integer, Save              :: s_HONO         ! m3dry map
      Integer, Save              :: n_NO2          ! CGRID map
      Integer, Save              :: s_NO2
      Integer, Save              :: n_O3          ! CGRID map
      Integer, Save              :: s_O3
      Integer, Save              :: n_NH3          ! CGRID map
      Integer, Save              :: s_NH3
      Integer, Save              :: n_HG          ! CGRID map
      Integer, Save              :: s_HG
      Integer, Save              :: n_HGII          ! CGRID map
      Integer, Save              :: s_HGII
      Integer, PRIVATE           :: ALLOCSTAT
      CHARACTER( 96 )            :: xmsg = ' '
C Aerosol deposition arrays
      REAL, ALLOCATABLE, SAVE  :: XXLSG( : ) ! log of standard deviation
      REAL, ALLOCATABLE, SAVE  :: DG( : )    ! geometric mean diameter
      REAL, ALLOCATABLE, SAVE  :: PDENS( : ) ! particle density         


      Contains
         SUBROUTINE INIT_STAGE ( JDATE, JTIME )

C-----------------------------------------------------------------------
C  This subroutine sets up the mapping and options for the TAGX gaseous 
C  and aerosol exchange subroutines. 
C-----------------------------------------------------------------------
         USE NH3_BIDI_MOD

         Implicit None        
 
         Include SUBST_FILES_ID  ! file name parameters
         Include SUBST_CONST     ! constants

C Arguments:
         Integer, Intent( IN ) :: JDATE, JTIME      ! internal simulation date&time
         integer               :: c, r, l, n, s, v
         CHARACTER( 16 ), PARAMETER :: pname      = 'INIT_STAGE'        
         CHARACTER( 16 )       :: gc_depv_name( dep_gas_all )
         CHARACTER( 16 )       :: gc_scav_name( dep_gas_all )
         Logical               :: unique_gc_depv( n_gc_depv )
C Local variables:

         CHARACTER( 16 ) :: VDAE_NAME( N_AE_DEP_SPC )! dep vel surrogate name table
         DATA         VDAE_NAME( 1 ) / 'VNUMATKN' /
         DATA         VDAE_NAME( 2 ) / 'VNUMACC ' /
         DATA         VDAE_NAME( 3 ) / 'VNUMCOR ' /
         DATA         VDAE_NAME( 4 ) / 'VMASSI  ' /
         DATA         VDAE_NAME( 5 ) / 'VMASSJ  ' /
         DATA         VDAE_NAME( 6 ) / 'VMASSC  ' /
         DATA         VDAE_NAME( 7 ) / 'VSRFATKN' /
         DATA         VDAE_NAME( 8 ) / 'VSRFACC ' /
         DATA         VDAE_NAME( 9 ) / 'VSRFCOR ' /

         Call Map_Stage

         IF ( .NOT. desc3( met_cro_2d ) ) THEN
            xmsg = 'Could not get  met_cro_2d  file description'
            CALL m3exit( pname, jdate, jtime, xmsg, xstat2 )
         END IF

         xcent = real( xcent3d, 4 )
         ycent = real( ycent3d, 4 )

         IF ( abflux ) THEN
            CALL Init_NH3_Bidi( jdate, jtime )
         END IF

         Allocate ( Water  ( n_lufrac ),
     &              Ag     ( n_lufrac ),
     &              Forest ( n_lufrac ),
     &              a_cut  ( n_lufrac ),
     &              l_leaf ( n_lufrac ), 
     &              l_aero ( n_lufrac ), STAT = ALLOCSTAT )

         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating Water, Ag, Forest, l_leaf, or a_cut'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
                         
C  Allocate arrays
         ALLOCATE ( VDEP( N_AE_DEP_SPC ), 
     &              VDEPJ( N_LUFRAC,N_AE_DEP_SPC ), 
     &              DEPV_SUR( N_AE_DEPV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VDEP, VDEPJ, DEPV_SUR'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( DG( N_MODE ), XXLSG( N_MODE ), PDENS( N_MODE), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DG, XXLSG, or PDENS'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Set the dep vel surrogate pointers
         DO S = 1, N_AE_DEPV
            N = INDEX1( AE_DEPV( S ), N_AE_DEP_SPC, VDAE_NAME )
            IF ( N .NE. 0 ) THEN
               DEPV_SUR( S ) = N
            ELSE
               XMSG = 'Could not find ' // AE_DEPV( V ) // ' in aerosol' //
     &                ' surrogate table. >>> Dep vel set to zero <<< '
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               DEPV_SUR( S ) = 0
            END IF
         END DO                               

C-----------------------------------------------------------------
C        Species maps
C-----------------------------------------------------------------
         s_HONO = 0
         n_HONO = 0
         s_NO2  = 0
         n_NO2  = 0
         s_O3   = 0
         n_O3   = 0
         s_NH3  = 0
         n_NH3  = 0
         s_HG   = 0
         n_HG   = 0
         s_HGII = 0
         n_HGII = 0

         n = 0
         maploop: DO s = 1, dep_gas_all
            IF ( .NOT. asx_run_map( s ) ) CYCLE maploop
            n = n + 1

            If ( vd_name( s ) .EQ. 'NO2' ) THEN
               s_NO2 = s
               n_NO2 = n
            End If
            If ( vd_name( s ) .EQ. 'HONO' ) THEN
               s_HONO = s
               n_HONO = n
            End If
            If ( vd_name( s ) .EQ. 'O3' ) THEN
               s_O3 = s
               n_O3 = n
            End If
            If ( vd_name( s ) .EQ. 'NH3' ) THEN
               s_NH3 = s
               n_NH3 = n
            End If
            If ( vd_name( s ) .EQ. 'HG' ) THEN
               s_HG = s
               n_HG = n
            End If
            If ( vd_name( s ) .EQ. 'HGIIGAS' ) THEN
               s_HGII = s
               n_HGII = n
            End If
         END DO maploop

         Water  = .FALSE.
         Ag     = .FALSE.
         Forest = .FALSE.
         DO l = 1, n_lufrac
            Select Case( cat_lu( l ) )
               Case( 'WATER' )
                  Water( l ) = .TRUE.
                  l_leaf( l ) = 0.0
                  l_aero( l ) = 0.0
                  a_cut( l )  = 0.0
               Case( 'AG'    )
                  ag( l )    = .TRUE.
                  l_leaf( l ) = 0.02
                  l_aero( l ) = 0.005
                  a_cut( l )  = 0.148
               Case( 'AGMOS' )
                  ag( l )    = .TRUE.
                  l_leaf( l ) = 0.035
                  l_aero( l ) = 0.0035
                  a_cut( l )  = 0.148
               Case( 'HAY'   )
                  ag( l )    = .TRUE.
                  l_leaf( l ) = 0.01
                  l_aero( l ) = 0.005
                  a_cut( l )  = 0.148
               Case( 'URBAN' )
                  l_leaf( l ) = 0.05
                  l_aero( l ) = 0.01
                  a_cut( l )  = 0.120
               Case( 'DECFOR' )
                  Forest( l ) = .TRUE.
                  l_leaf( l ) = 0.05
                  l_aero( l ) = 0.01
                  a_cut( l )  = 0.0318
               Case( 'EVEFOR' )
                  Forest( l ) = .TRUE.
                  l_leaf( l ) = 0.005
                  l_aero( l ) = 0.002
                  a_cut( l )  = 0.0318
               Case( 'MIXFOR' )
                  Forest( l ) = .TRUE.
                  l_leaf( l ) = 0.028
                  l_aero( l ) = 0.005
                  a_cut( l )  = 0.0318
               Case( 'HERB','SHRUB' )
                  l_leaf( l ) = 0.02
                  l_aero( l ) = 0.01
                  a_cut( l )  = 0.120
               Case( 'GRASS' )
                  l_leaf( l ) = 0.01
                  l_aero( l ) = 0.005
                  a_cut( l )  = 0.176
               Case( 'WETLAND' )
                  l_leaf( l ) = 0.01
                  l_aero( l ) = 0.01
                  a_cut( l )  = 0.0318
               Case Default
                  l_leaf( l ) = 0.01
                  l_aero( l ) = 0.002
                  a_cut( l )  = 0.120
            End Select
         END DO

         END SUBROUTINE INIT_STAGE

         SUBROUTINE GAS_X( JDate, JTime, TStep, c, r, cgridl1, pvd, cmp, depvel_gas )    
        
         Use NH3_BIDI_MOD
         Use MOSAIC_MOD, Only: Tile_Data 
         Use HGSIM

         Implicit None

         Integer, Intent( IN )  :: JDate, JTime, c, r      ! internal simulation date&time
         REAL,    Intent( IN )  :: cgridl1( : )    ! layer 1 concentrations
         REAL,    Intent( IN )  :: TStep           ! Time step in seconds
         Real,    Intent( OUT ) :: pvd( : )        ! model emissions term
         Real,    Intent( OUT ) :: cmp( : )        ! diagnostic component fluxes 
         Real,    Intent( OUT ) :: depvel_gas( : ) ! instantanious deposition velocity 
C Parameters specific to gas_x. Currently based on m3dry but subject to change
         Real, Parameter :: a0         = 8.0        ! [dim'less]
         Real, Parameter :: d3         = 1.38564e-2 ! [dim'less]
         Real, Parameter :: hplus_ap   = 1.0e-6     ! pH=6.0 leaf apoplast solution Ph (Massad et al 2008)      
         Real, Parameter :: hplus_def  = 1.0e-5     ! pH=5.0
         Real, Parameter :: hplus_east = 1.0e-5     ! pH=5.0
         Real, Parameter :: hplus_h2o  = 7.94328e-9 ! 10.0**(-8.1)
         Real, Parameter :: hplus_west = 3.16228e-6 ! 10.0**(-5.5)
         Real, Parameter :: rcut0      = 3000.0     ! [s/m]
         Real, Parameter :: rcw0       = 125000.0   ! acc'd'g to Padro and
         Real, Parameter :: rg0        = 1000.0     ! [s/m]
         Real, Parameter :: rgwet0     = 25000.0    ! [s/m]
         Real, Parameter :: rsnow0     = 1000.0
         Real, Parameter :: rt25inK    = 1.0/(stdtemp + 25.0) ! 298.15K = 25C
         Real, Parameter :: rwm = 31.5              ! Minimum NH3 cuticle resistance [s/m] from Massad et al. 2010
         Real            :: rh_func                 ! functions of humidity
         Real            :: surf_bldg, surf_leaf, kno2, conc_no2
         Real            :: heff_ap, heff_wat, heff
         Real            :: hplus
         Real            :: w10cm
         Real            :: rcwet, rcdry
         Real            :: rawmt, rad_wat, rmsv, dif_T, k_vis, dwat_T ! mass transport between the air-water droplet interface
         Real            :: rgwet
         Real            :: rgdry, sm_func  
         Real            :: temp_g, temp_2m, q_2m, rh, ustar, lai ! column met data
         Real            :: Ra, Rb, Rb_leaf, Rbgc, Rst, Rcut, Rgc, Rg, Rsnow, Rinc, Rwat ! land use and species specific bulk resistances
         Real            :: snow, no_snow, wet, dry, veg, no_veg, ice_snow, melt_snow    ! fractional land coverage
         Real            :: frac_lu                                                      ! fractional land use
         Real            :: hcan, ustg, del0
         Real            :: Flux
         Real            :: c_atm, c_z0, c_leaf, c_stom, c_cut, c_grnd, c_wat ! Compensation points  
         Real            :: cs0_z0, cs0_leaf                                  ! Compensation points with zero surface concentration
         Real            :: ca0_z0, ca0_leaf                                  ! Compensation points with zero atmospheric concentration
         Real            :: soil_emis, soil_dep, soil_flux                    ! soil emission and deposition for soil biogeochem
         Real            :: f_emis, f_dep, f_stom, f_cut, f_soil, flux_ag, f_nat, f_wat ! component fluxes
         Real            :: ctemp2, lv, cp_air, tw, dw25, kvisw, dw, scw_pr_23, scn
         Real            :: pChang, kwChang, ciodide, qiodide
         integer         :: l, n, s
         LOGICAL         :: effective  ! true=compute effective Henry's Law const
         Real, External  :: hlconst
! Hg bidi variables 
         Real            :: Hg_st, Hg_cut, Hg_grnd, Hg_wat, flux_hgII
   
         effective = .TRUE.

! column met variables 
         temp_2m = Met_Data%TEMP2(c,r)
         q_2m    = MET_DATA%Q2( c,r )
         temp_g  = MET_DATA%TEMPG( c,r )
         rh      = MET_DATA%RH( c,r )
         snow    = max( 0.0, MET_DATA%SNOCOV( c,r ) )
! total snow fraction
         no_snow = 1.0 - snow
! Liquid snow fraction
         melt_snow = 0.02 * ( temp_2m - ( stdtemp - 1.0 ) )**2
         IF ( temp_2m .Lt. ( stdtemp - 1.0 ) ) melt_snow = 0.0
         melt_snow = MIN (melt_snow, 0.5)
         melt_snow = MAX (melt_snow, 0.0)
! frozen snow fraction
         ice_snow  = 1.0 - melt_snow
         rh_func = max( 0.0,( rh - 70.0 )/30.0 )
         IF ( ( ycent .GE.   30.0 ) .AND. ( ycent .LE.  45.0 ) .AND.
     &        ( xcent .GE. -120.0 ) .AND. ( xcent .LE. -70.0 ) ) THEN
            IF ( GRID_DATA%LON( c,r ) .GT. -100.0 ) THEN
               hplus = hplus_east
            ELSE
               hplus = hplus_west
            END IF
         ELSE
            hplus = hplus_def
         END IF
! moved water temperature here to reduce redundancy and facilitate moving the hlconst 
! subroutine out of the land use loop for better vectorization.
         ctemp2 = temp_2m - stdtemp
         lv     = lv0 - dlvdt * ctemp2
         cp_air = CPD * ( 1.0 + 0.84 * q_2m )               ! [J/kg/K]
         tw     = ( ( 4.71e4 * cp_air / lv ) - 0.870 ) + stdtemp  ! [K]
         k_vis  = kvis*1.0e-4 * ( temp_2m/STDTEMP )**1.81 ! Following Massman 1999
!         dwat_T    = 1.0e-7*temp_2m**1.75 * sqrt( 1.0/MWAIR + 1.0/molwt_all( s ) ) / 
!     &             ( MOLVOL**(1.0/3.0) + LeBasM( s )**(1.0/3.0) )**2 ! Following Fuller et al 1966.
         dwat_T    = dwat*1.0e-4 * ( temp_2m/STDTEMP )**1.81 ! Following Massman 1999
         n = 0
         cmp = 0.0
         pvd = 0.0
         depvel_gas = 0.0
         spc_loop: Do s = 1, dep_gas_all
            IF ( .NOT. asx_run_map( s ) ) CYCLE spc_loop
            n = n + 1
! Following Fuller et al 1966. Here we use the LeBas molar volume which is similar to Fuller's diffusive volume
            dif_T   = 1.0e-7*temp_2m**1.75 * sqrt( 1.0/MWAIR + 1.0/molwt_all( s ) ) / 
     &                ( MOLVOL**(1.0/3.0) + LeBasM( s )**(1.0/3.0) )**2 
            scc_pr_23( s ) = ( ( k_vis / dif_T ) / pr ) ** twothirds

            c_atm = max( cgridl1( n ), 1.0e-30 )
            f_emis = 0.0
            f_dep  = 0.0
            f_stom = 0.0
            f_cut  = 0.0
            f_soil = 0.0
            flux_ag   = 0.0
            f_nat     = 0.0
            f_wat     = 0.0
            soil_dep  = 0.0
            soil_flux = 0.0
            soil_emis = 0.0
            heff_wat  = hlconst( H_name_all( s ), tw, effective, hplus_h2o )* 0.08205 * tw
            heff_ap   = hlconst( H_name_all( s ), temp_2m, effective, hplus_ap )
            heff      = hlconst( H_name_all( s ), temp_2m, effective, hplus )* 0.08205 * temp_2m
            If ( HGBIDI .And. s .Eq. s_Hg ) Then
               Call  Get_Hg_Comp( Hg_st, Hg_cut, Hg_grnd, Hg_wat, c_atm, heff_wat, heff, r, c )
            End If
            lu_loop: Do l = 1, n_lufrac
               c_z0   = 0.0
               c_leaf = 0.0
               If( HGBIDI .And. s .Eq. s_Hg) Then
                  c_stom = Hg_st
                  c_cut  = Hg_cut
                  c_grnd = Hg_grnd
                  c_wat  = Hg_wat
               Else
                  c_stom = 0.0
                  c_cut  = 0.0
                  c_grnd = 0.0
                  c_wat  = 0.0
               End If
               Rb     = 0.0
               Rb_leaf= 0.0
               Rst    = 0.0
               Rcut   = 0.0
               Rinc   = 0.0
               Rg     = 0.0
               Rgc    = 0.0                  
               Rsnow  = 0.0       
C land use specific area fraction 
               frac_lu = GRID_DATA%LUFRAC( c,r,l )  
               If( frac_lu .Eq. 0.0 ) Cycle lu_loop
C land use specific column met data
               ustar   = MOSAIC_DATA%USTAR( c,r,l )
               lai     = MOSAIC_DATA%LAI( c,r,l )
C land use specific land cover data
               wet     = Mosaic_Data%DELTA( c,r,l )
               dry     = 1.0 - wet
               veg     = MOSAIC_DATA%VEG( c,r,l ) 
               no_veg  = 1.0 - veg
C Get Ra
               Ra = MOSAIC_DATA%RA( c,r,l )
C Calculate Rb
               Rb  = 5.0 / ustar * scc_pr_23( s )
!-------------------------------------------------------------------------------------------------
! Resistance to air-water exchange
!-------------------------------------------------------------------------------------------------
               If( Water( l ) ) Then
!                  heff_wat  = heff_wat * 0.08205 * tw
         ! from Hayduk and Laudie
                  dw25 = 13.26e-5 / ( 0.8904**1.14 * LeBasM( s )**0.589 )
                  kvisw = 0.017 * EXP( -0.025 * ( tw - stdtemp ) )
                  dw    = dw25 * ( tw * rt25inK ) * ( 0.009025 / kvisw )
                  scw_pr_23 = ( ( kvisw / dw ) / pr ) ** twothirds
                  IF ( s .EQ. s_O3 ) THEN   !implement Chang et al(2004)
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

                     IF (((GRID_DATA%OCEAN(c,r) + GRID_DATA%SZONE(c,r)) .GT. 0) .AND. (MET_DATA%SEAICE(c,r) .LE. 0)) THEN
c        Iodide in sea-water based on SST  (mol /dm-3)
                        ciodide = 1.46E6 * EXP( -9134.0 / temp_g)
                        qiodide = ( ( 2.0e9 * ciodide * dw * 1e-4 ) ** 0.5 ) * heff_wat
                        Rwat = 1.0 / ( pChang * kwchang + qiodide )
                     ELSE                  ! O3 over sea ice
                        Rwat   = scw_pr_23 / ( heff_wat * d3 * ustar )
                     END IF
                  ELSE                     ! other chems
                     Rwat   = scw_pr_23 / ( heff_wat * d3 * ustar )
                  END IF
                  Flux     = (c_atm-c_wat)/( Ra + Rb + Rwat )                         ! positive values for deposition
                  If( c_wat .Gt. 0.0 ) Then
                     pvd( n ) =  pvd( n ) + frac_lu *  c_wat / ( Ra + Rb + Rwat ) 
                     depvel_gas( n ) = depvel_gas( n ) + frac_lu / ( Ra + Rb + Rwat )
                     f_emis = f_emis + frac_lu *  c_wat / ( Ra + Rb + Rwat ) 
                     f_dep  = f_dep  + frac_lu *  c_atm / ( Ra + Rb + Rwat ) 
                     f_wat  = f_wat  + frac_lu * ( c_wat - c_atm ) / ( Ra + Rb + Rwat )   
                  Else
                     depvel_gas( n ) = depvel_gas( n ) + frac_lu / ( Ra + Rb + Rwat )
                  End If
                  Tile_Data%pol( l,s,c,r ) = c_wat / ( Ra + Rb + Rwat )
                  Tile_Data%depvel_gasj( l,s,c,r ) = Flux / c_atm
               Else ! Land
!-------------------------------------------------------------------------------------------------
! Resistance to air-snow exchange
!-------------------------------------------------------------------------------------------------
C Calcuate Rsnow
                  Rsnow = rsnow0 * a0 / rel_rx( s )
!-------------------------------------------------------------------------------------------------
! Resistance to air-canopy exchange
!-------------------------------------------------------------------------------------------------
C Calculate Rst
                  If( lai .Gt. 0.0 ) Then

!-------------------------------------------------------------------------------------------------
! Quazi Laminar Resistance to leaf following Jensen and Hummelshoj 1995/1997 doi:10.1016/0168-1923(94)05083-I
!-------------------------------------------------------------------------------------------------
                     Rb_leaf = k_vis / ( dif_T * ustar ) * 
     &                       ( l_leaf( l ) * ustar / (k_vis * max(lai,1.0)**2 ) )**(1.0/3.0)
!-------------------------------------------------------------------------------------------------
! Resistance to air-stomatal exchange
!-------------------------------------------------------------------------------------------------
                     Rst = Mosaic_Data%RSTW( c,r,l ) * dwat_T / dif_T
     &                        + 1.0 / ( heff_ap / 3000.0 + 100.0 * f0( s ) ) / lai
!-------------------------------------------------------------------------------------------------
! Resistance to air-cuticle exchange
!-------------------------------------------------------------------------------------------------
C Calcuate Rcut
         ! wet Cuticle
         ! If the surface is cold and wet, use dry snow.
                     IF ( temp_g .LT. stdtemp ) THEN
                        rcwet = Rsnow
                     ELSE
                        IF ( s .Ne. s_O3 ) THEN 
!                           rcwet = rcw0 / heff
                           If ( Met_Data%WR( C, R ) .Gt. 0.0 ) Then
                              rad_wat = 1.9e-4
                              rmsv    = sqrt( 3.0 * RGASUNIV * 1.0e3 * temp_2m / molwt_all( s ) ) 
                              rawmt   = rad_wat / dif_T  + 4.0 / ( rmsv  * M_ac( s ) )
                              rcwet   = rawmt + rawmt/( heff * rad_wat )
                           Else 
                              rcwet = 1.0e6
                           End If
                        Else
         ! Canopy level wet resistence Rwet to ozone was found to be about 200 s/m on basis of Keysburg exp
         ! Using LAI(1-sided) of about 6.25 measured at Keysburg gives leaf level rwet about 1250 s/m
                           rcwet = 1250.0    ! s/m
         ! Leaf level rwet estimated from Altimir et al 2006 gives about 1350 s/m                           
                        End If ! O3
                     END IF ! temp 
         ! Dry Cuticle
                     IF( s .Eq. s_O3 ) THEN
         ! Leaf level rwet estimated from Altimir et al 2006 gives about 1350 s/m
                        rcdry = 1.0 / ( ( 1.0 -rh_func) / ( rcut0 * a0 / rel_rx( s ) )  + rh_func / rcwet )
                     ELSE IF ( s .Eq. s_NH3 .And. ABFLUX ) THEN  
         ! Massad et al. 2010 Cuticular resistance
                        rcdry    = rwm * EXP( a_cut( l ) * ( 100.0 - rh ) )
                     ELSE 
                        rcdry = rcut0 * a0 / rel_rx( s )
                     END IF ! O3 or NH3

                     Rcut = no_snow / ( lai * ( dry / rcdry +  ! Dry Cuticle 
     &                                       wet / rcwet ) ) +                      ! Wet Cuticle 
     &                                       snow * Rsnow                           ! Snow
!-------------------------------------------------------------------------------------------------
! Resistance to in-canopy exchange
!-------------------------------------------------------------------------------------------------
! Calculate in canopy aerodynamic resistance based on the momentum attenuation coefficient derived 
! by Yi 2008 https://doi.org/10.1175/2007JAMC1667.1
                     Rinc =  Ra * ( Exp( lai / 2.0 ) - 1.0 )
!-------------------------------------------------------------------------------------------------
! Resistance to air-canopy covered soil exchange
!-------------------------------------------------------------------------------------------------
C Calcuate Canopy Covered Soil Resistance Nemitz et al 2000 https://doi.org/10.1016/S0168-1923(00)00206-9
         ! Soil quazi laminar boundary layer resistance with canopy 
                     scn    = k_vis / dif_T
         ! ustar at the soil surface following Yi 2008 https://doi.org/10.1175/2007JAMC1667.1
                     ustg   = max( ustar * EXP( -lai / 2.0 ), 0.001 )         
                     del0   = dif_T / ( karman * ustg )
                     Rbgc   = ( scn - LOG( del0 / 0.10 ) ) / ( karman * ustg )
                  Else ! LAI = 0.0
                     Rst     = 1.0e6
                     Rcut    = 1.0e6
                     Rinc    = 1.0e6
                     Rbgc    = 1.0e6
                     Rb_leaf = Rb
                  End If ! LAI    
!-------------------------------------------------------------------------------------------------
! Resistance to air-base soil exchange
!-------------------------------------------------------------------------------------------------
C Calcuate Bare Soil Resistance
! Soil quazi laminar boundary layer resistance with out canopy Nemitz et al 2000 https://doi.org/10.1016/S0168-1923(00)00206-9
                  scn    = k_vis / dif_T
                  del0   = dif_T / ( karman * ustar )
                  Rb   = ( scn - LOG( del0 / 0.10 ) ) / ( karman * ustar )
         ! Wet Ground
                  rgwet  = rgwet0 / heff   
                  If(ABFLUX .And. s .Eq. s_NH3) Then
                     Call Get_NH3_Comp( c_stom, c_grnd, rgdry, dif_T, r, c, l, s )
!                     rgdry  = rgdry
                     Rgc = no_snow * rgdry +                                                   ! Snow free soil 
     &                     snow * ( 1.0 / ( ice_snow / Rsnow + melt_snow / ( rsndiff + rgwet ) ) ) +    ! Snow covered soil
     &                     Rbgc + Rinc                                                                ! Incanopy resistances

                     Rg  = no_snow * rgdry +                                                   ! Snow free soil 
     &                     snow * ( 1.0 / ( ice_snow / Rsnow + melt_snow / ( rsndiff+rgwet ) ) ) +     ! Snow covered soil
     &                     Rb                                                                         ! Qauzi Laminar Boundary Layer resistance
                  Else ! not ABFLUX and NH3       
                     IF( s .Eq. s_O3 ) THEN  
! Following based on measurements Fares et al 2014 https://doi.org/10.1016/j.agrformet.2014.08.014 for sandy soil 
! forests at 10cm measured soil moisture and Fumagalli et al. 20016 https://doi.org/10.1016/j.agrformet.2016.07.011 for sandy loam soils
! Here an asymptotic function was applied to set lower and upper bounds in the resistance as repoerted by Fumagalli et al. 2016
                           w10cm = min(MET_DATA%SOIM1( c,r ) * exp( 0.09 * GRAV )**(1.0/GRID_DATA%BSLP( c,r )),
     &                                 GRID_DATA%WSAT( c,r ) )
                           sm_func = max(0.0,(w10cm-GRID_DATA%WWLT( c,r ) ) / GRID_DATA%WFC( c,r ) )
                           rgdry   = 250.0 + 2000.0 * atan( sm_func**GRID_DATA%BSLP( c,r ) ) /PI  
                     Else                   
                        rgdry  = rg0 * a0 / rel_rx( s )     
                     End If
                     Rgc = no_snow * ( 1.0/ ( dry / rgdry +                                       ! Snow free dry soil 
     &                     wet / rgwet ) ) +                                                          ! Snow free wet soil 
     &                     snow * ( 1.0 / ( ice_snow / Rsnow + melt_snow / ( rsndiff + rgwet ) ) ) +  ! Snow covered soil
     &                     Rbgc + Rinc                                                                ! Incanopy resistances

                     Rg  = no_snow * ( 1.0/ ( dry / rgdry +                                       ! Snow free dry soil 
     &                     wet / rgwet ) ) +                                                          ! Snow free wet soil 
     &                     snow * ( 1.0 / ( ice_snow / Rsnow + melt_snow / ( rsndiff+rgwet ) ) ) +    ! Snow covered soil
     &                     Rb                                                                         ! Qauzi Laminar Boundary Layer resistance
                  End If ! ABFLUX and NH3
!-------------------------------------------------------------------------------------------------
! Calcuate the compensation points follwing Nimitz et al 2001
!-------------------------------------------------------------------------------------------------
! Leaf compensation point
!-------------------------------------------------------------------------------------------------
                  c_leaf = (c_atm/(Ra*Rb_leaf)+                                                                                      ! Atmospheric Component
     &                      c_stom*(1.0/(Ra*Rst)+1.0/(Rb_leaf*Rst)+1.0/(Rgc*Rst))+                                                   ! Stomatal Component
     &                      c_cut*(1.0/(Ra*Rcut)+1.0/(Rb_leaf*Rcut)+1.0/(Rgc*Rcut))+                                                 ! Cuticular Component
     &                      c_grnd/(Rb_leaf*Rgc))/                                                                                   ! Soil Component
     &            (1.0/(Ra*Rb_leaf) +1.0/(Ra*Rst) +1.0/(Ra*Rcut)+1.0/(Rb_leaf*Rgc)+1.0/(Rb_leaf*Rcut)+
     &             1.0/(Rb_leaf*Rst)+1.0/(Rgc*Rst)+1.0/(Rgc*Rcut)) ! Least common denominator
!-------------------------------------------------------------------------------------------------
! Compensation point at z0
!-------------------------------------------------------------------------------------------------
                  c_z0     = (c_atm/Ra+c_leaf/Rb_leaf+c_grnd/Rgc)/(1.0/Ra+1.0/Rb_leaf+1.0/Rgc)           
!-------------------------------------------------------------------------------------------------
! Estimate air-surface flux
!-------------------------------------------------------------------------------------------------         
! positive values for deposition                 
                  Flux     = veg    * (c_atm-c_z0)/Ra +                 ! air-vegetation flux
     &                       no_veg * (c_atm-c_grnd)/( Ra + Rg )        ! air-soil flux
!-------------------------------------------------------------------------------------------------
! NH3 bidirectional exchange diagnostic fluxes
!-------------------------------------------------------------------------------------------------                                                               
                  If(ABFLUX .And. s .Eq. s_NH3 ) Then     
! Leaf compensation point if atmospheric conc were 0 for diagnostic emission and soil budget estiamtes
                     ca0_leaf = (c_stom*(1.0/(Ra*Rst)+1.0/(Rb_leaf*Rst)+1.0/(Rgc*Rst))+                                                 
     &                           c_cut*(1.0/(Ra*Rcut)+1.0/(Rb_leaf*Rcut)+1.0/(Rgc*Rcut))+                                                
     &                           c_grnd/(Rb_leaf*Rgc))/                                                                                  
     &                 (1.0/(Ra*Rb_leaf)+1.0/(Ra*Rst)+1.0/(Ra*Rcut)+1.0/(Rb_leaf*Rgc)+1.0/(Rb_leaf*Rcut)+
     &                  1.0/(Rb_leaf*Rst)+1.0/(Rgc*Rst)+1.0/(Rgc*Rcut)) 
                     ca0_z0     = (ca0_leaf/Rb_leaf+c_grnd/Rgc)/(1.0/Ra+1.0/Rb_leaf+1.0/Rgc)
! Leaf compensation point if soil and vegetation  conc were 0 for diagnostic deposition and soil budget estiamtes
                     cs0_leaf = c_atm/(Ra*Rb_leaf)/                                                                                     
     &                 (1.0/(Ra*Rb_leaf)+1.0/(Ra*Rst)+1.0/(Ra*Rcut)+1.0/(Rb_leaf*Rgc)+1.0/(Rb_leaf*Rcut)+
     &                  1.0/(Rb_leaf*Rst)+1.0/(Rgc*Rst)+1.0/(Rgc*Rcut)) 
                     cs0_z0     = (c_atm/Ra+cs0_leaf/Rb_leaf)/(1.0/Ra+1.0/Rb_leaf+1.0/Rgc)
                    If( Ag( l ) ) Then
                        flux_ag = flux_ag + frac_lu * ( veg * ( c_z0 - c_atm ) / Ra + 
     &                                        no_veg * ( c_grnd - c_atm ) / Rg )
                        soil_dep  = soil_dep + frac_lu * ( veg * cs0_z0 / Rgc
     &                            + no_veg / ( Ra + Rg ) * c_atm )  ! soil layer
                        soil_emis = soil_emis + max( frac_lu * ( veg * (c_grnd-ca0_z0) / Rgc  
     &                            + no_veg / (Ra + Rg) * c_grnd ),0.0)             ! soil layer
                        soil_flux  = soil_flux + veg * ( c_grnd - c_z0 ) / Rgc
     &                             + no_veg * ( c_grnd  - c_atm ) / ( Ra + Rg )  ! soil layer
                    Else
                       f_nat = f_nat + frac_lu * ( veg * ( c_z0 - c_atm ) / Ra - 
     &                                          no_veg * c_atm / Rg )          
                    End If
                    f_dep = f_dep + frac_lu * ( veg * ( c_atm - cs0_z0 ) / Ra + 
     &                      no_veg * c_atm / ( Ra + Rg ) )
                    f_emis = f_emis + frac_lu * ( veg * ca0_z0 / Ra +
     &                       no_veg * c_grnd / ( Ra + Rg ) )
                  End If ! ABFLUX and NH3
                  If( c_stom .Gt. 0.0 .Or. c_cut .Gt. 0.0 .Or. c_grnd .Gt. 0.0 ) Then
!-------------------------------------------------------------------------------------------------
! Bidirectional exchange
!-------------------------------------------------------------------------------------------------                                                               
                     pvd( n ) =  pvd( n ) + frac_lu * ( veg * c_z0 / Ra                  !Vegetation flux
     &                       + no_snow * no_veg * c_grnd / ( Ra + Rg ) )                 !Bare soil flux w/ no snow emissions
                     Tile_Data%pol( l,s,c,r ) = ( veg * c_z0 / Ra                        !Vegetation flux
     &                       + no_snow * no_veg * c_grnd / ( Ra + Rg ) )                 !Bare soil flux w/ no snow emissions
                     Tile_Data%depvel_gasj( l,s,c,r ) = veg / Ra + no_veg / ( Ra + Rg )  !Exchange velocity with compensation point
                     depvel_gas( n ) = depvel_gas( n ) + frac_lu * Tile_Data%depvel_gasj( l,s,c,r )
                    f_soil = f_soil + frac_lu * ( veg * ( ( c_grnd - c_z0 ) / Rgc ) + 
     &                                         no_veg * (c_grnd - c_atm)/( Ra + Rg ) )            ! air-soil flux
                    f_stom = f_stom + frac_lu * veg * ( c_stom - c_leaf ) / Rst 
                    f_cut  = f_cut  + frac_lu * veg * ( c_cut  - c_leaf ) / Rcut  

                  Else
!-------------------------------------------------------------------------------------------------
! Non bidirectional exchange
!-------------------------------------------------------------------------------------------------   
                     Tile_Data%depvel_gasj( l,s,c,r ) =  veg * (1.0-c_z0/c_atm)/Ra + no_veg /( Ra + Rg )                                                            
                     depvel_gas( n ) = depvel_gas( n ) + frac_lu * Tile_Data%depvel_gasj( l,s,c,r )                  
                  End If ! compensation points greater than 0
               End If ! Water      
            End Do lu_loop
            If(ABFLUX .And. s .Eq. s_NH3 ) Then
               cmp( 1 ) = max( f_emis, 0.0 )
               cmp( 2 ) = max( f_dep, 0.0 )
               cmp( 3 ) = f_stom
               cmp( 4 ) = f_cut
               cmp( 5 ) = f_soil
               cmp( 6 ) = flux_ag
               cmp( 7 ) = f_nat
               cmp( 8 ) = f_wat
               If( sum(GRID_DATA%LUFRAC(c,r,:),mask=Ag) .Gt. 0.0 ) Then 
                  Call Calc_Nitrif ( TStep, C, R, soil_flux )
               End If
            End If
            If(HGBIDI .And. s .Eq. s_HG ) Then
               flux_hgII = 0.0 
! negative values are deposition fluxes
               flux_hgII = -Sum( Tile_Data%depvel_gasj( :,s_HgII,c,r ) * GRID_DATA%LUFRAC( c,r,: ), mask = WATER)

               Call Hg_Surf_Update ( f_stom, f_cut, f_soil, f_wat, flux_hgII, 
     &                               Heff_wat, Heff, TStep, c, r, Jdate, Jtime )
            End If
C--------------------------------------------------------------------------
            IF ( sfc_hono ) THEN

C HONO production via heterogeneous reaction on ground surfaces,
C 2NO2 = HONO + HNO3
C Rate constant for the reaction = (3.0E-3/60)* (A/V),
C where A/V is surface area/volume ratio
C HONO is produced and released into the atmosphere
C NO2 is lost via chemical reaction
C HNO3 is sticky and stays on the surfaces

C Calculate A/V for leaves.
C LAI was multiplied by 2 to account for the fact that surface area
C is provided by both sides of the leaves.
C Matthews Jones, Ammonia deposition to semi-natural vegetation,
C PhD dissertation, University of Dundee, Scotland, 2006

               surf_leaf = 2.0 * MET_DATA%LAI( c,r ) / MET_DATA%ZF( c,r,1 )

C Calculate A/V for buildings and other structures.
C Buildings and other structures can provide additional surfaces in
C urban areas for the heterogeneous reaction to occur. However, such
C information is not readily available; in the absence of such information,
C it is scaled to purb(c,r). Svensson et al., (1987) suggests a typical value
C of 0.2 for A/V for buildings in urban environments. A maximum value of 0.2
C for A/V for buildings is assigned to the grid cell containing the highest
C purb(c,r) i.e., 100.0. A/V for buildings for other grid-cell is calculated
C as purb(c,r)*(0.2/100.0); Cai et al. (2006) used a value of 1.0 for their
C study at New York (total A/V)

               surf_bldg = GRID_DATA%PURB( c,r ) * 0.002

C Calculate rate constant for the reaction (psudeo-first order reaction,
C unit per second). Calculate pseudo-first order rate constant using Eq 1
C of Vogel et al. (2003).  Unit of KNO2 is in 1/min in the paper; divide it
C by 60 to convert it into 1/sec.

!                 kno2 = MAX( 0.0, 3.0E-3 * (surf_leaf + surf_bldg) / 60.0 )
               kno2 = MAX( 0.0, 5.0E-5 * (surf_leaf + surf_bldg) )

C Determine NO2 concentration needed for HONO production term.

               IF ( s .EQ. s_NO2 ) THEN
                  conc_no2 = cgridl1( n )

C Loss of NO2 via the heterogeneous reaction is accounted as additional
C depositional loss. Add the loss of NO2 via the heterogeneous reaction
C to the regular deposition velocity (increased dep. vel.).  This will
C reduce the NO2 conc. in the atmosphere. Dep vel is adjusted back to the
C original value in vdiffacm2 after NO2 conc is reduced but before calculating
C depositional loss.

!                    depvel_gas( n,c,r ) = depvel_gas( n,c,r ) + 2.0 * kno2 * zf( c,r )

               END IF
C Calculate production (pvd) for HONO; unit = ppm * m/s
               IF ( s .EQ. s_HONO ) pvd( n ) = kno2 * conc_no2 * MET_DATA%ZF( c,r,1 )
            END IF

            ! Check for negative values or NaN's                  
            if(isnan(pvd(n))) write(logdev,*) 'NaN in ',vd_name( s ),' production term'
            if(isnan(depvel_gas(n))) write(logdev,*) 'NaN in ',vd_name( s ),' Vd term'
         End Do spc_loop

         Return         
         END SUBROUTINE GAS_X

         SUBROUTINE AERO_X(CGRID, JDATE, JTIME, TSTEP, VDEP_AE, C, R )

C *** Calculate deposition velocity for Aitken, accumulation, and
C     coarse modes.
C     Reference:
C     Binkowski F. S., and U. Shankar, The regional particulate
C     model 1. Model description and preliminary results.
C     J. Geophys. Res., 100, D12, 26191-26209, 1995.
 
C    May 05 D.Schwede: added impaction term to coarse mode dry deposition
C 25 May 05 J.Pleim:  Updated dry dep velocity calculation for aerosols
C                     to Venkatram and Pleim (1999)
C 20 Jul 05 J.Pleim:  Changed impaction term using modal integration of
C                     Stokes**2 / 400 (Giorgi, 1986, JGR)
C 14 Apr 08 J.Kelly:  Added code to calculate deposition velocity of
C                     coarse surface area and to account for variable
C                     standard deviation of the coarse mode.
C 08 Sep 08 P.Bhave:  Backward compatibility with AE4 mechanisms
C                     standardized names of all coarse-mode variables
C-----------------------------------------------------------------------

         USE AERO_DATA           ! aero variable data   
         USE AEROMET_DATA        ! Includes CONST.EXT
         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE RXNS_DATA           ! chemical mechanism data
         Use MOSAIC_MOD, Only: Tile_Data 

         IMPLICIT NONE

C Includes:

         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments
         REAL,    POINTER       :: CGRID( :,:,:,: )
         INTEGER, INTENT( IN )  :: JDATE               ! current model date , coded YYYYDDD
         INTEGER, INTENT( IN )  :: JTIME               ! current model time , coded HHMMSS
         INTEGER, INTENT( IN )  :: TSTEP               ! model time step, coded HHMMSS
         INTEGER, INTENT( IN )  :: C,R                 ! Column and Row
         REAL,    INTENT( OUT ) :: VDEP_AE( : )        ! surrogate deposition velocities [ m s**-1 ]



C *** array indices hardcoded to match SUBROUTINE AERO_DEPV
      INTEGER, PARAMETER, DIMENSION( 3 ) :: 
     &                      VDN = (/ 1,2,3 /) , 
     &                      VDM = (/ 4,5,6 /) , 
     &                      VDS = (/ 7,8,9 /)  

C Meteorological variables

         CHARACTER( 16 ), SAVE :: AE_VRSN ! Aerosol version name

         INTEGER, SAVE :: NCELLS              ! number of cells per layer

         REAL, SAVE  :: XLM        ! mean free path [ m ]
         REAL, SAVE  :: AMU        ! dynamic viscosity [ kg m**-1 s**-1 ]

         REAL M3_WET, M3SUBT, M3_DRY
         REAL M2_WET, M2_DRY

         CHARACTER( 16 ), SAVE :: PNAME = 'AERO_X'
         CHARACTER( 16 ) :: VNAME            ! variable name
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER  V, N, L               ! loop counters
         INTEGER  SPC, S                ! species loop counter
         INTEGER  ALLOCSTAT

C modal Knudsen numbers
         REAL KN

C modal particle diffusivities for number, 2nd, and 3rd moment, or mass:
         REAL DCHAT0
         REAL DCHAT2
         REAL DCHAT3

C modal sedimentation velocities for number, 2nd, and 3rd moment, or mass:
         REAL VGHAT0
         REAL VGHAT2
         REAL VGHAT3

         INTEGER NCELL, J, IM

         REAL DCONST,  DCONST1
         REAL DCONST2, DCONST3
         REAL SC0                   ! Schmidt numbers for number
         REAL SC2                   ! Schmidt numbers for 2ND MOMENT
         REAL SC3                   ! Schmidt numbers for 3rd moment
         REAL ST_VEG                ! Stokes numbers for vegetation 
         REAL ST_SMOOTH             ! Stokes numbers for each mode following Pleim
         REAL RD0_VEG,RD0_SMOOTH    ! canopy resistance for number
         REAL RD2_VEG,RD2_SMOOTH    ! canopy resistance for 2nd moment
         REAL RD3_VEG,RD3_SMOOTH    ! canopy resisteance for 3rd moment
         REAL UTSCALE               ! scratch function of USTAR and WSTAR
         REAL NU                    ! kinematic viscosity [ m**2 s**-1 ]
         REAL TWOXLM                ! 2 X atmospheric mean free path
         REAL Ra, Ustar, Wstar, lai ! Land use specific environmental variables
         REAL Lu_frac, Veg          ! Land use and vegetation coverage fraction
         REAL V_fac                 ! Vegetation factor for deposition 

C Parameters
         REAL, PARAMETER :: BHAT    = 1.246   ! Constant from Cunningham slip correction
         REAL, PARAMETER :: T0      = 288.15  ! [ K ] ! starting standard surface temp.
         REAL, PARAMETER :: THREEPI = 3.0 * PI
         REAL, PARAMETER :: TWO3    = 2.0 / 3.0

C Scalar variables for VARIABLE standard deviations.

         REAL    L2SG

         REAL    E1                  ! mode exp( log^2( sigmag )/8 )
         REAL    ES04                !        " **4
         REAL    ES08                !        " **8
         REAL    ES12                !        " **12
         REAL    ES16                !        " **16
         REAL    ES20                !        " **20
         REAL    ES28                !        " **28
         REAL    ES32                !        " **32
         REAL    ES36                !        " **36
         REAL    ES48                !        " **48
         REAL    ES64                !        " **64
         REAL    ESM12               !        " **(-12)
         REAL    ESM16               !        " **(-16)
         REAL    ESM20               !        " **(-20)
         REAL    ESM32               !        " **(-32)
         REAL    EIM_VEG, EIM_SMOOTH ! Impaction efficiency

C-----------------------------------------------------------------------

         VDEP  = 0.0   ! array assignment
         VDEPJ = 0.0   ! array assignment

C ***    Set meteorological data for the grid cell.
         AIRDENS = Met_Data%DENS1( C,R )
         AIRTEMP = Met_Data%TEMP2( C,R )
         AIRPRES = Met_Data%PRSFC( C,R )

C ***    extract grid cell concentrations of aero species from CGRID
C        into aerospc_conc in aero_data module
C        Also determines second moment from surface area and adds wet
C        species
         CALL EXTRACT_AERO( CGRID( C,R,1,: ), .TRUE. )

C ***    Calculate geometric mean diameters and standard deviations of the
C        "wet" size distribution
         CALL GETPAR( .FALSE. )     

C        Save getpar values to arrays
         DO IM = 1,N_MODE
            XXLSG( IM ) = AEROMODE_LNSG( IM )   
            DG( IM )    = AEROMODE_DIAM( IM )
            PDENS( IM ) = AEROMODE_DENS( IM )
         END DO
 
C        Calculate mean free path [ m ]:
         XLM = 6.6328E-8 * STDATMPA * AIRTEMP / ( T0 * AIRPRES )

C ***    Calcualte dynamic and kinimatic viscosity [ kg m**-1 s**-1 ]:
         AMU = 1.458E-6 * AIRTEMP * SQRT( AIRTEMP )
     &                 / ( AIRTEMP + 110.4 )
         NU = AMU / Met_Data%DENS1( C,R )   

C *** Calculate Knudsen numbers
         TWOXLM = XLM + XLM
         DO IM = 1, N_MODE
            KN = TWOXLM / DG( IM )

C *** Calculate functions of variable standard deviation.

            L2SG = XXLSG( IM ) ** 2
            
            E1   = EXP( 0.125 * L2SG )
            ES04 = E1 ** 4
            ES08 = ES04 ** 2
            ES12 = ES04 * ES08
            ES16 = ES08 ** 2
            ES20 = ES16 * ES04
            ES28 = ES20 * ES08
            ES32 = ES16 ** 2
            ES36 = ES16 * ES20
            ES48 = ES36 * ES12
            ES64 = ES32 ** 2

C *** calculate inverses:

            ESM12 = 1.0 / ES12
            ESM16 = 1.0 / ES16
            ESM20 = 1.0 / ES20
            ESM32 = 1.0 / ES32

            DCONST  = BOLTZMANN * Met_Data%TEMP2( C,R ) / ( THREEPI * AMU )
            DCONST1 = DCONST / DG( IM )

            DCONST2 = GRAV / ( 18.0 * AMU )
            DCONST3 = DCONST2 * PDENS( IM ) * DG( IM ) ** 2 ! Gravitational settling 
C Calculate characteristic parameters
            DCHAT0  = DCONST1 * ( ES04  + BHAT * KN * ES16 )
            DCHAT2  = DCONST1 * ( ESM12 + BHAT * KN * ESM16 )
            DCHAT3  = DCONST1 * ( ESM20 + BHAT * KN * ESM32 )
            VGHAT0  = DCONST3 * ( ES16  + BHAT * KN * ES04 )
            VGHAT2  = DCONST3 * ( ES48  + BHAT * KN * ES20 )
            VGHAT3  = DCONST3 * ( ES64  + BHAT * KN * ES28 )  

            SC0 = NU / DCHAT0
            SC2 = NU / DCHAT2   
            SC3 = NU / DCHAT3


            lu_loop: DO L = 1, N_LUFRAC
C ***    Land use paramters
               Veg     = Mosaic_Data%VEG( C,R,L )
               Ustar   = Mosaic_Data%USTAR( C,R,L )
               Wstar   = Mosaic_Data%WSTAR( C,R,L )
               lai     = Mosaic_Data%LAI( C,R,L )
               Ra      = Mosaic_Data%RA( C,R,L )
               lu_frac = Grid_Data%LUFRAC( C,R,L )

               RD0_Veg = 0.0
               RD2_Veg = 0.0
               RD3_Veg = 0.0
               IF ( lu_frac .Eq. 0.0 ) Cycle lu_loop

C now calculate the deposition velocities
               UTSCALE = Ustar + 0.24 * Wstar**2 / Ustar

C Estimate the vegetation deposition parameters for the 0th, 2nd, and 3rd moments
               IF( veg .gt. 0.0 ) THEN ! EIM_Veg following Slinn 1982 equation 28
                  V_fac   = max( lai, 1.0 )
                  ST_VEG  = VGHAT0 * Ustar / ( GRAV * l_aero( l ) )
                  EIM_Veg = ST_VEG**2 / ( 1.0 + ST_VEG**2 ) ! Slinn 1982 equation 28           
                  RD0_Veg = 1.0 / ( V_fac * UTSCALE * ( SC0 ** ( -TWO3 ) + EIM_Veg ) )

                  ST_VEG  = VGHAT2 * Ustar / ( GRAV * l_aero( l ) )
                  EIM_Veg = ST_VEG**2 / ( 1.0 + ST_VEG**2 ) ! Slinn 1982 equation 28           
                  RD2_Veg = 1.0 / ( V_fac * UTSCALE * ( SC2 ** ( -TWO3 ) + EIM_Veg ) )

                  ST_VEG  = VGHAT3 * Ustar / ( GRAV * l_aero( l ) ) 
                  EIM_Veg = ST_VEG**2 / ( 1.0 + ST_VEG**2 ) ! Slinn 1982 equation 28             
                  RD3_Veg = 1.0 / ( V_fac * UTSCALE * ( SC3 ** ( -TWO3 ) + EIM_Veg ) )
               END IF
C EIM_SMOOTH following Giorgi 1986 Equation 17
               ST_SMOOTH  = VGHAT0 * Ustar**2 / ( GRAV * NU )
               EIM_SMOOTH = ST_SMOOTH**2 / ( 400.0 + ST_SMOOTH**2 )   ! Giorgi 1986 Equation 17  
               RD0_SMOOTH = 1.0 / ( UTSCALE * ( SC0 ** ( -TWO3 ) + EIM_SMOOTH ) )

               ST_SMOOTH  = VGHAT2 * Ustar**2 / ( GRAV * NU )
               EIM_SMOOTH = ST_SMOOTH**2 / ( 400.0 + ST_SMOOTH**2 )   ! Giorgi 1986 Equation 17
               RD2_SMOOTH = 1.0 / ( UTSCALE * ( SC2 ** ( -TWO3 ) + EIM_SMOOTH ) )

               ST_SMOOTH  = VGHAT3 * Ustar**2 / ( GRAV * NU )
               EIM_SMOOTH = ST_SMOOTH**2 / ( 400.0 + ST_SMOOTH**2 )   ! Giorgi 1986 Equation 17
               RD3_SMOOTH = 1.0 / ( UTSCALE * ( SC3 ** ( -TWO3 ) + EIM_SMOOTH ) )
C Parallel conductances are additive. Thus, the vegetated and non-vegetated deposition velocities are added. 
C first do 0th moment for the deposition of number
               VDEPJ( L,VDN( IM ) ) = veg   * VGHAT0 / ( 1.0 - EXP( -VGHAT0 * ( Ra + RD0_Veg ) ) ) + 
     &                        ( 1.0 - veg ) * VGHAT0 / ( 1.0 - EXP( -VGHAT0 * ( Ra + RD0_Smooth ) ) )
               VDEP( VDN( IM ) ) =  VDEP( VDN( IM ) ) + Lu_Frac * VDEPJ( L,VDN( IM ) )

C now do 2nd moment for the deposition of surface area
               VDEPJ( L,VDS( IM ) ) = veg   * VGHAT2 / ( 1.0 - EXP( -VGHAT2 * ( Ra + RD2_Veg ) ) ) + 
     &                        ( 1.0 - veg ) * VGHAT2 / ( 1.0 - EXP( -VGHAT2 * ( Ra + RD2_Smooth ) ) )
               VDEP( VDS( IM ) ) =  VDEP( VDS( IM ) ) + Lu_Frac * VDEPJ( L,VDS( IM ) )

C now do 3rd moment for the deposition of mass
               VDEPJ( L,VDM( IM ) ) = veg   * VGHAT3 / ( 1.0 - EXP( -VGHAT3 * ( Ra + RD3_Veg ) ) ) + 
     &                        ( 1.0 - veg ) * VGHAT3 / ( 1.0 - EXP( -VGHAT3 * ( Ra + RD3_Smooth ) ) )
               VDEP( VDM( IM ) ) =  VDEP( VDM( IM ) ) + Lu_Frac * VDEPJ( L,VDM( IM ) )

            END DO lu_loop ! n_lufrac
         END DO ! aerosol mode

C Return dry deposition velocities for aerosols (first layer only).

         DO V = 1, N_AE_DEPV
            IF ( DEPV_SUR( V ) .GT. 0 ) THEN
               VDEP_AE( V ) = VDEP( DEPV_SUR( V ) )
               Tile_Data%ADEPVJ( :,V,C,R ) = VDEPJ( :,DEPV_SUR( V ) )
            ELSE
               VDEP_AE( V ) = 0.0
               Tile_Data%ADEPVJ( :,V,C,R ) = 0.0 
            END IF
         END DO
                    
         Return
         END SUBROUTINE AERO_X
      END MODULE STAGE_MOD
