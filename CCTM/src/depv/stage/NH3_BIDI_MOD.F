
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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module NH3_Bidi_Mod

! Contains the shared variables and subroutines needed for the bidirectional 
! NH3 flux model in CMAQ
!
! INIT_NH3_BiDi - Initializes the NH3 flux routines, allocates arrays, reads in
!                 initial soil NH3 & H concentrations, and fertilizer application
!                 amounts and timing for the model run
! 
! Revision History: J. Bash Dec 08 10:      Created
!                   J. Bash May 11 11:      Updated for CMAQ 5.0
!                   J.Young Oct 26 11:      KIND=16->KIND=8 for Portland Group compiler (pgi)
!                                           IsNaN function   "    "        "      "
!                                           (This Module must be compiled w/ -Kieee if pgi)
!                   J. Bash Jan 31 12:      New daily EPIC output now includes soil NH3 from 
!                                           mineralization of organic and no longer includes 
!                                           the monthly fertilizer totals. The initialization 
!                                           of soil NH3 was rewritten to reflect this.
!                   J. Bash Apr 19 12:      Set bounds on the soil moisture from the met. model
!                                           to be between saturation and residual soil moisture
!                                           to avoid errors in the soil resistance from rounding
!                                           errors. Corrected a units conversion error in the 
!                                           coupling of the soil NH4 to the atmospheric NH3.
!                                           This will maintain a better mass balance and have
!                                           a small impact on the model results ~ 1% of the
!                                           ambient NH3 concentrations.
!                   J. Bash Apr 19 12:      The apoplast compensation point for agricultural land use 
!                                           is now a function of the soil ammonium concentration 
!                                           following Massad et al. 2010 doi:10.5194/acp-10-10359-2010
!                   J. Bash Aug 29 12:      The subroutine was modified to utilize new EPIC output that 
!                                           estimates the ammonium content of fertilizer applied to the 
!                                           1cm and 5cm soil layers. 
!                   D. Schwede Sept 12 12:  Added code for NLCD40 land use classification
!                   J. Bash    Apr   4 13:  Brought in new water, agriculture and snow land use 
!                                           classification in LSM_MOD to simplify the case structures
C                   J. Bash:   Nov   7 14:  Modified for the restructuring of vidff.
!                   D. Wong:   Feb  10 19:  Implemented centralized I/O approach, removed all MY_N clauses
C-------------------------------------------------------------------------------
      Use, intrinsic :: ieee_arithmetic, only: isnan => ieee_is_nan

      Implicit None
! shared variables
      Real, Save, Allocatable :: frac_ir( :,: ) ! irrigated fraction of ag 
! Private variables used in this module and subroutines     
      Real, Save,              Private :: C_gam           ! Canadian fertilizer facter   
      Real, Parameter,         Private :: maxgam   = 2.0e5 ! maximum soil gamm. It is assumed that any excess NH4 would from salts
! variables for STAGE bidi NH3
      Real, Save, Allocatable, Private :: frac_ag( :,: ) ! fraction of ag    
      Real, Save, Allocatable, Private :: knit1  ( :,: ) ! EPIC Grid nitrificaiton rate
      Real, Save, Allocatable, Private :: knit2  ( :,: ) ! EPIC Grid nitrification rate
      Real, Save, Allocatable, Private :: NH4_G1 ( :,: ) ! EPIC Grid soil ammonium
      Real, Save, Allocatable, Private :: NH4_G2 ( :,: ) ! EPIC Grid soil ammonium
      Real, Save, Allocatable, Private :: BDs1   ( :,: ) ! mean ag soil bulk density kg/ha
      Real, Save, Allocatable, Private :: BDs2   ( :,: ) ! mean ag soil bulk density kg/ha
      Real, Save,              Private :: wg             ! Soil moisture for biogeochemical calcs
      Real, Save,              Private :: zsoil          ! Soil depth for biogeochemical calcs
     
      Contains 

!------------------------------------------------------------------------------
! STAGE option NH3 bidi initialization
!------------------------------------------------------------------------------

         Subroutine Init_NH3_Bidi( jdate, jtime)
 
         Use HGRD_DEFN           ! horizontal grid specifications
         Use UTILIO_DEFN         
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2
         Use ASX_DATA_MOD, Only: zsoil1, zsoil2, Grid_Data
         Use LSM_MOD
         Use depv_data_module
         Use centralized_io_module

         Implicit None 
! Includes
         Include SUBST_CONST     ! constants
         Include SUBST_FILES_ID  ! file name parameters

! Local Variables

         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime   
         Integer                :: c,r,l,k
         Integer            :: ALLOCSTAT
         Real,    Parameter :: convl1 = 7.142857e-04   ! ha/m**2 * mol/g N * 1/z_soil_l1 
         Real,    Parameter :: convl2 = 7.142857e-05   ! ha/m**2 * mol/g N * 1/z_soil_l2 
         Real               :: pHfac1
         Real               :: pHfac2
! STAGE bidi variables
         Real,       Allocatable :: L1_MIN     ( :,:,: ) ! Epic Layer 1 organic N mineralization
         Real,       Allocatable :: L2_MIN     ( :,:,: ) ! Epic Layer 2 organic N mineralization 
         Real,       Allocatable :: C_knit1    ( :,:,: ) ! EPIC Crop nitrificaiton rate
         Real,       Allocatable :: C_knit2    ( :,:,: ) ! EPIC Crop nitrification rate
         Real,       Allocatable :: fagam1     ( :,: )   ! delta gamma due to fertilizer app and mineralization
         Real,       Allocatable :: fagam2     ( :,: )   ! delta gamma due to fertilizer app and mineralization           
 
         Character( 16 ), Parameter :: pname = 'Init_NH3_Bidi' 
         Character( 96 )            :: xmsg = ' '

! Find the Canadian fertilizer factor based off of Sheppard et al 2010 Canadian J. Soil Sci. & 
! Zhang et al. 2010 JGR 
         Select Case( jdate )
            Case(  60:90 )
               C_gam = 811.5
            Case(  91:120 )
               C_gam = 3447.3
            Case( 121:151 )
               C_gam = 8702.8
            Case( 152:181 )
               C_gam = 1269.3
            Case( 182:212 )
               C_gam = 667.1
            Case( 213:243 )
               C_gam = 704.2
            Case( 244:273 )
               C_gam = 811.5
            Case( 274:304 )
               C_gam = 1376.7
            Case( 305:334 )
               C_gam = 1079.6
            Case Default
               C_gam = 630.0
         End Select           
                  
         Allocate( L1_MIN     ( ncols,nrows,e2c_cats ),
     &             L2_MIN     ( ncols,nrows,e2c_cats ),    
     &             C_Knit1    ( ncols,nrows,e2c_cats ),
     &             C_Knit2    ( ncols,nrows,e2c_cats ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating EPIC vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         L1_MIN  = 0.0
         L2_MIN  = 0.0
         C_Knit1 = 0.0
         C_Knit2 = 0.0

         Beld_ag = 0.01 * Beld_ag   ! convert to fraction

! Allocate variable needed soil processes and fertilization
         Allocate( fagam1     ( ncols,nrows ),
     &             fagam2     ( ncols,nrows ), 
     &             MHp1       ( ncols,nrows ),
     &             MHp2       ( ncols,nrows ),
     &             BDs1       ( ncols,nrows ),
     &             BDs2       ( ncols,nrows ),
     &             Knit1      ( ncols,nrows ),
     &             Knit2      ( ncols,nrows ),
     &             NH4_G1     ( ncols,nrows ),
     &             NH4_G2     ( ncols,nrows ),
     &             frac_ir    ( ncols,nrows ), 
     &             frac_ag    ( ncols,nrows ), 
     &              STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating soil vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         fagam1  = 0.0
         fagam2  = 0.0
         MHp1    = 0.0
         MHp2    = 0.0
         BDs1    = 0.0
         BDs2    = 0.0
         Knit1   = 0.0
         Knit2   = 0.0
         NH4_G1  = 0.0
         NH4_G2  = 0.0
         frac_ir = 0.0
         frac_ag = 0.0    

! EPIC v1.4 does not always correctly write out m3badval in areas where there is no data. In these cases, 
! EPIC writes out large negative values which can cause issues with the bidirectional exchange numerics. 
! The following variables should always be positive and a floor of 0.0 is now set to mitigate numerical issues. 
         Where( isnan( Nit1   ) ) Nit1   = 0.0
         Where( isnan( Nit2   ) ) Nit2   = 0.0
         Where( isnan( F1_NH4 ) ) F1_NH4 = 0.0
         Where( isnan( F2_NH4 ) ) F2_NH4 = 0.0
         Where( isnan( NH4ps1 ) ) NH4ps1 = 0.0
         Where( isnan( NH4ps2 ) ) NH4ps2 = 0.0
         Where( isnan( pHs1   ) ) pHs1   = 0.0
         Where( isnan( pHs2   ) ) pHs2   = 0.0
         Where( Nit1   .Lt. 0.0 ) Nit1   = 0.0
         Where( Nit2   .Lt. 0.0 ) Nit2   = 0.0
         Where( F1_NH4 .Lt. 0.0 ) F1_NH4 = 0.0
         Where( F2_NH4 .Lt. 0.0 ) F2_NH4 = 0.0
         Where( NH4ps1 .Lt. 0.0 ) NH4ps1 = 0.0
         Where( NH4ps2 .Lt. 0.0 ) NH4ps2 = 0.0

! get fertilizer from the previous month to estimate soil NH4+        
! time in the files is not the same
         If ( .Not. MEDC_AVAIL ) Then
            Write(Logdev,*) 'Estimating soil NHx from EPIC output'
            gamma1  = 0.0
            gamma2  = 0.0

            If ( .Not. E2C_CHEM_AVAIL ) Then
               xmsg = E2C_CHEM // ' file not available'
               Call M3exit ( pname, jdate, jtime, xmsg, xstat1 )
            End If

            If( GMN_AVAIL ) Then
! NH4 from organic N mineralization is not layer specific in Fest-C v1.4 therefore it is allocated proportionally
! to the organic N from which it was mineralized.
               Where( GMN .Gt. 0.0 .And. L1_ON .Gt. 0.0 )
                  L1_MIN = GMN * L1_ON / ( L1_ON + L2_ON )
               End Where
               Where( GMN .Gt. 0.0 .And. L2_ON .Gt. 0.0 )
                  L2_MIN = GMN * L2_ON / ( L1_ON + L2_ON )
               End Where

            End iF
! EPIC data as simulated in FEST-C 1.3 is for the end of the day. Here we 
! want to get the values at the beginning of the day.
! EPIC ammonia evasion is estimated as 5% of nitrification. We add the 
! evaded ammonia and nitrification back to the soil NH3 to account for losses.
            F1_NH4 = Nit1 / 0.95 + L1_MIN
            F2_NH4 = Nit2 / 0.95 + L2_MIN
! Calculate grid cell values from the EPIC crop specific data            
            frac_ag  = sum(Beld_ag,DIM=3)
            Do l = 1, e2c_cats
               Where( pHs1(:,:,l) .Gt. 4.0 .And. pHs1(:,:,l) .Lt. 9.0 .And. Beld_ag(:,:,l) .Gt. 0.0 )
                  MHp1   = MHp1   + Beld_ag(:,:,l) * 10.0 ** (-pHs1(:,:,l) )
                  MHp2   = MHp2   + Beld_ag(:,:,l) * 10.0 ** (-pHs2(:,:,l) )
                  BDs1   = BDs1   + Beld_ag(:,:,l) * BDc1(:,:,l)
                  BDs2   = BDs2   + Beld_ag(:,:,l) * BDc2(:,:,l)
                  Knit1  = Knit1  + Beld_ag(:,:,l) * Nit1(:,:,l)
                  Knit2  = Knit2  + Beld_ag(:,:,l) * Nit2(:,:,l)
                  NH4_G1 = NH4_G1 + Beld_ag(:,:,l) * NH4ps1(:,:,l)
                  NH4_G2 = NH4_G2 + Beld_ag(:,:,l) * NH4ps2(:,:,l)
                  gamma1 = gamma1 + convl1 * NH4ps1(:,:,l) / 10.0 ** (-pHs1(:,:,l) ) * Beld_ag(:,:,l)
                  gamma2 = gamma2 + convl2 * NH4ps2(:,:,l) / 10.0 ** (-pHs2(:,:,l) ) * Beld_ag(:,:,l)
                  fagam1 = fagam1 + convl1 * F1_NH4(:,:,l) / 10.0 ** (-pHs1(:,:,l) ) * Beld_ag(:,:,l)
                  fagam2 = fagam2 + convl2 * F2_NH4(:,:,l) / 10.0 ** (-pHs2(:,:,l) ) * Beld_ag(:,:,l)
               End Where
               If ( Index( Beld_Names( l ), '_ir' ) .Gt. 0 ) Then
                  frac_ir = frac_ir + Beld_ag( :,:,l )
               End If                    
            End Do
            Where( frac_ag .gt. 0.0 )
               MHp1 = MHp1 / frac_ag
               MHp2 = MHp2 / frac_ag
               gamma1 = max(( gamma1 + fagam1 ) / frac_ag, 630.0 )
               gamma2 = max(( gamma2 + fagam2 ) / frac_ag, 630.0 )
               BDs1   = BDs1 / frac_ag
               BDs2   = BDs2 / frac_ag
               frac_ir = frac_ir / frac_ag
            End Where
! Eliminate unrealistically low bulk density 
            Where( BDs1 .Lt. 1.0 ) BDs1 = 1.6
            Where( BDs2 .Lt. 1.0 ) BDs2 = 1.6
! prevent missmatched data between soil and daily files
            Where( MHp1 .Eq. 0.0 .Or. MHp2 .Eq. 0.0 )
               gamma1 = 0.0
               gamma2 = 0.0  
            End Where

            Where( NH4_G1 .Gt. 0.0 )
               Knit1 = min(-log(NH4_G1 / (NH4_G1 + Knit1)),-log(0.5))
            Elsewhere
               Knit1 = 0.0
            End Where
            Where( NH4_G2 .Gt. 0.0 )
               Knit2 = min(-log(NH4_G2 / (NH4_G2 + Knit2)),-log(0.5))
            Elsewhere
               Knit2 = 0.0
            End Where

         Else ! read

            If( GMN_AVAIL ) Then
! NH4 from organic N mineralization is not layer specific in Fest-C v1.4 therefore it is allocated proportionally
! to the organic N from which it was mineralized.
               Where( GMN .Gt. 0.0 .And. L1_ON .Gt. 0.0 .And. L2_ON .Gt. 0.0 )
                  L1_MIN = GMN * L1_ON / ( L1_ON + L2_ON )
                  L2_MIN = GMN * L2_ON / ( L1_ON + L2_ON )
               End Where

            Else
! The organic soil change from the previous day is equal to the fertilization - mineralization 
! Here we read in the previous days concentration to calculate the mineralization and add it to the NH4 pool.
! In EPIC run for CMAQ organic N mineralized goes to the NH4 pool and if mineralization is negative it is 
! taken from the soil NO3 pool. A cap was placed on the maximum mineralization based on the 0.999 percentile 
! from multiple years of EPIC simulations. 
               L1_MIN = L1_ON_Yest - L1_ON - F1_ON
               L2_MIN = L2_ON_Yest - L2_ON - F2_ON
               Where( L1_MIN .Lt. 0.0 )
                  L1_MIN = 0.0
               Else Where( L1_MIN .Gt. 0.3 )
                  L1_MIN = 0.3
               End Where
               Where( L2_MIN .Lt. 0.0 )
                  L2_MIN = 0.0
               Else Where( L2_MIN .Gt. 0.3 )
                  L2_MIN = 0.3
               End Where
            End If
            F1_NH4  = F1_NH4 + L1_MIN
            F2_NH4  = F2_NH4 + L2_MIN
! Calculate grid cell values from the EPIC crop specific data            
            frac_ag  = sum(Beld_ag,DIM=3)
            Do l = 1, e2c_cats
               Where( pHs1(:,:,l) .Gt. 4.0 .And. pHs1(:,:,l) .Lt. 9.0 .And. Beld_ag(:,:,l) .Gt. 0.0 )
                  MHp1   = MHp1   + Beld_ag(:,:,l) * 10.0 ** (-pHs1(:,:,l) )
                  MHp2   = MHp2   + Beld_ag(:,:,l) * 10.0 ** (-pHs2(:,:,l) )
                  BDs1   = BDs1   + Beld_ag(:,:,l) * BDc1(:,:,l)
                  BDs2   = BDs2   + Beld_ag(:,:,l) * BDc2(:,:,l)
                  Knit1  = Knit1  + Beld_ag(:,:,l) * Nit1(:,:,l)
                  Knit2  = Knit2  + Beld_ag(:,:,l) * Nit2(:,:,l)
                  NH4_G1 = NH4_G1 + Beld_ag(:,:,l) * NH4ps1(:,:,l)
                  NH4_G2 = NH4_G2 + Beld_ag(:,:,l) * NH4ps2(:,:,l)
                  fagam1 = fagam1 + convl1 * F1_NH4(:,:,l) / 10.0 ** (-pHs1(:,:,l) ) * Beld_ag(:,:,l)
                  fagam2 = fagam2 + convl2 * F2_NH4(:,:,l) / 10.0 ** (-pHs2(:,:,l) ) * Beld_ag(:,:,l)
               End Where
               If ( Index( Beld_Names( l ), '_ir' ) .Gt. 0 ) Then
                  frac_ir = frac_ir + Beld_ag( :,:,l )
               End If                    
            End Do

            Where( frac_ag .gt. 0.0 )
               MHp1 = MHp1 / frac_ag
               MHp2 = MHp2 / frac_ag
               gamma1 = max( gamma1 + fagam1 / frac_ag, 630.0 )
               gamma2 = max( gamma2 + fagam2 / frac_ag, 630.0 )
               BDs1   = BDs1 / frac_ag
               BDs2   = BDs2 / frac_ag
               frac_ir = frac_ir / frac_ag
            End Where

! Eliminate unrealistically low bulk density 
            Where( BDs1 .Lt. 1.0 ) BDs1 = 1.6
            Where( BDs2 .Lt. 1.0 ) BDs2 = 1.6
! prevent missmatched data between soil and daily files
            Where( MHp1 .Eq. 0.0 .Or. MHp2 .Eq. 0.0 )
               gamma1 = 0.0
               gamma2 = 0.0  
            End Where

            Where( NH4_G1 .Gt. 0.0 )
               Knit1 = min(-log(NH4_G1 / (NH4_G1 + Knit1)),-log(0.5))
            Elsewhere
               Knit1 = 0.0
            End Where
            Where( NH4_G2 .Gt. 0.0 )
               Knit2 = min(-log(NH4_G2 / (NH4_G2 + Knit2)),-log(0.5))
            Elsewhere
               Knit2 = 0.0
            End Where

         End If ! INIT_MEDC_1
         
         Return
!------------------------------------------------------------------------------
! Error handling section
!------------------------------------------------------------------------------
1001     Continue
         Call M3exit( pname, jdate, jtime, xmsg, xstat1 )

C-------------------------------------------------------------------------------
C Format statements.
C-------------------------------------------------------------------------------

9001     Format( 'Failure reading ', a, 1x, 'from ', a )

         Return
         
         End Subroutine Init_NH3_Bidi
                  
!------------------------------------------------------------------------------
! Subroutine to get the soil and canopy compensation point for STAGE
!------------------------------------------------------------------------------          
         Subroutine Get_NH3_Comp( NH3_st, NH3_ll, NH3_g, diff, r, c, l, l_ag )
         
         Use UTILIO_DEFN
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2
         Use ASX_DATA_MOD
         Use LSM_MOD
         Use MOSAIC_MOD, Only: Tile_Data 
         
         Implicit None
         
         Include SUBST_FILES_ID  ! file name parameters

         Integer, Intent( IN )  :: r, c, l  ! Row, Column, Land use indices
         Integer, Intent( IN )  :: l_ag     ! Ag index  
         Real,    Intent( IN )  :: diff     ! NH3 diffusion in air m2/s
         Real,    Intent( OUT ) :: NH3_st   ! Stomatal Compensation point
         Real,    Intent( OUT ) :: NH3_ll   ! Leaf litter/under canopy Compensation point
         Real,    Intent( OUT ) :: NH3_g    ! Soil Compensation point
         
         Real                   :: cnh3g1, cnh3g2   ! NH3 compensation concentration for ground [ppm]
         Real                   :: cnh3ll1      ! NH3 compensation concentration for ground [ppm]
         Real                   :: mNH4         ! Soil total NH4+ mg N / Kg
         Real                   :: Ka           ! NH4 acid dissociation constant
         Real                   :: coef_a       ! intermediate variable to solve for soil solution NHx
         Real                   :: coef_b       ! intermediate variable to solve for soil solution NHx
         Real                   :: coef_c       ! intermediate variable to solve for soil solution NHx
         Real                   :: NH4_sol      ! NH4+ in soil water solution mg N / kg
         Real                   :: NH3_sol      ! NH3 in soil water solution mg N / kg
         Real                   :: NH3_gam_ll   ! Under canopy leaf litter molar emission potential [NH3]/[H+]
         Real                   :: frac_sol     ! Fraction of NHx in solution
         Real                   :: ldry1
         Real                   :: ldry2
         Real                   :: ldry_max
         Real( 8 )              :: dp
         Real( 8 )              :: rsoil1
         Real( 8 )              :: rsoil2
         Real( 8 )              :: a1
         Real,        Parameter :: twothree = 2.0/3.0
         Real,        Parameter :: onethree = 1.0/3.0
         Real,        Parameter :: MolN     = 14.007  ! g/mol N      
         Real                   :: wg_ir        ! 1 cm soil moisture  
         Real                   :: w5cm, cg         ! soil moisture in top 5 cm (vol frc)

         Integer                :: j
! Point were soil solution NH4 equals half the maximum sorption capacity Alnosour 2020  Bi-directional exchange of
!  ammonia from soils in row crop agro-ecosystems. Dissertation. North Carolina State University 
!  http://www.lib.ncsu.edu/resolver/1840.20/37245
         Real, Parameter        :: half_sol = 124.0 ! update from NC State Alnosour Thesis
! Maximum NH4 soil sorption capacity 
         Real, Parameter        :: max_sorp = 426.0 ! update from NC State Alnosour Thesis 
         Real, Parameter        :: NH3_gam_soil = 20.0 ! bare soil water solution emission potetnial [NH3]/[H+]
 
         If( PX_LSM ) Then
! simplified from Darcy's law assuming stationarity and only gravitational draining with the Campbell hydrological functions applied
! 0.4 is the difference in the layer depths of 1cm and 5 cm
            w5cm = MET_DATA%SOIM1( c,r ) * exp( 0.04 * GRAV )**(1.0/GRID_DATA%BSLP( c,r ))
            w5cm = Min( w5cm, GRID_DATA%WSAT( c,r ) )
            w5cm = Max( w5cm, GRID_DATA%WRES( c,r ) )
         Else If( CLM_LSM .OR. NOAH_LSM ) Then
            w5cm = MET_DATA%SOIM2( c,r )
            w5cm = Min( w5cm, GRID_DATA%WSAT( c,r ) )
            w5cm = Max( w5cm, GRID_DATA%WRES( c,r ) )
         End If

! Updated based on EPIC 5cm soil moisture estimates where the 25% percentile of the irrigated crop fractional soil moisture was 
! approximately equal approximately equal to 60% of the field capacity.  
         If ( frac_ir( c,r ) .Gt. 0.0 .And. MET_DATA%SOIM2( c,r ) .LE.  0.60 * GRID_DATA%WFC( c,r ) ) Then            
            wg_ir = ( 1.0 - frac_ir( c,r ) ) * MET_DATA%SOIM1( c,r ) + frac_ir( c,r ) * 0.60 * GRID_DATA%WFC( c,r )
            w5cm  = ( 1.0 - frac_ir( c,r ) ) * w5cm + frac_ir( c,r ) * 0.60 * GRID_DATA%WFC( c,r )
         Else
            wg_ir = MET_DATA%SOIM1( c,r )
         End If
 
         wg_ir = Min( wg_ir,GRID_DATA%WSAT( c,r ) )
         wg_ir = Max( wg_ir,GRID_DATA%WRES( c,r ) )

! The following resistance parameterization is derived from measurements with soil samples of 2 cm thick (Kondo et al 1990)
! https://doi.org/10.1175/1520-0450(1990)029<0385:APOEFB>2.0.CO;2 as discussed in Sakaguchi and Zeng 2009 JGR 
! https://doi.org/10.1029/2008JD010834 According to Swenson and Lawrence 2014 (https://doi.org/10.1002/2014JD022314) and the 
! references therin the dry layer thickness varies from 1 to 3 cm. 
         ldry_max = 0.02
! From Sakaguchi and Zeng 2009 JGR Equation 10
         ldry1 = ldry_max * ( Exp( ( 1.0 - wg_ir / GRID_DATA%WSAT( c,r ) ) ** 5 ) - 1.0 ) / 1.718

!> Compute compensation point. gamma is specified according to the amount of 
!> cultivated vegetation               
         a1     = 161512.0d0 / real( MET_DATA%SoiT1( c,r ), 8 )
     &          * 10.0d0 ** ( -4507.11d0 / real( MET_DATA%SoiT1( c,r ), 8 ) )
         a1     = a1 * 24.5d0 * 1.0d6  ! ppm
         NH3_st       = a1 * Tile_Data%NH3_gam_st( l )
         NH3_gam_ll = Tile_Data%NH3_gam_grnd( l ) 
         
!> Set a maximum [NH4]/[H+] ratio at 200,000 based on output from the AIM aerosol
!> model any [NH4] in excess of this ratio is assumed to partition into the solid
!> phase. Canada soil gamma taken from Zhang et al 2010 JGR Table 5
         cnh3ll1 = 0.0
         If( l .Eq. l_ag ) Then
! Use the soil ammonium sorption model of Venteria et al. Sci. Rep. doi:10.1038/srep12153 to estimate the NHx 
! available for volatilization 
           If( gamma1( c,r ) .eq. 0.0 ) Then 
               cg = wg_ir/1.6 ! L water / kg soil
               mNH4    = C_gam*1.0e-7/1.6*MolN*1.0e3 ! assume a bulk density of 1.6 kg/l untill Canadian data is available
               Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
               coef_a  = cg * (1.0+Ka/1.0e-7) ! assume a pH of 7
               coef_b  = max_sorp+cg * half_sol * (1.0+Ka/1.0e-7) - mNH4
               coef_c  = -half_sol * mNH4
               NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
               NH3_sol = NH4_sol*Ka/1.0e-7 ! mg N/l
               cnh3g1  = (NH4_sol+NH3_sol)*1.0e-3/MolN/1.0e-7
               cnh3ll1 = cnh3g1
            Else
               cg = wg_ir/BDs1( c,r ) ! L water / kg soil
               mNH4 = gamma1( c,r )*MHp1( c,r )/BDs1( c,r )*MolN*1.0e3
               Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
               coef_a  = cg*(1.0+Ka/MHp1( c,r ))
               coef_b  = max_sorp+cg*half_sol*(1.0+Ka/MHp1(c,r)) - mNH4
               coef_c  = -half_sol*mNH4
               NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
               NH3_sol = NH4_sol*Ka/MHp1( c,r ) ! mg N/l
               cnh3g1  = (NH4_sol+NH3_sol)*1.0e-3/MolN/MHp1( c,r )
               cnh3ll1 = cnh3g1
            End If
            If( gamma2( c,r ) .eq. 0.0 ) Then 
               cg = w5cm/1.6 ! L water / kg soil
               mNH4    = C_gam*1.0e-7/1.6*MolN*1.0e3 ! assume a bulk density of 1.6 kg/l untill Canadian data is available
               Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
               coef_a  = cg*(1.0+Ka/1.0e-7)
               coef_b  = max_sorp+cg*half_sol*(1.0+Ka/1.0e-7) - mNH4
               coef_c  = -half_sol*mNH4
               NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
               NH3_sol = NH4_sol*Ka/1.0e-7 ! mg N/l
               cnh3g2  = (NH4_sol+NH3_sol)*1.0e-3/MolN/1.0e-7!/wg_ir
               NH3_st   = Max( NH3_st, Real( a1 ) *( C_gam * 1.0e-7
     &                  * w5cm * MolN * zsoil2 * 1.0e4 * 12.3 + 20.3 ) )
             Else 
               cg = w5cm/BDs2( c,r ) ! L water / kg soil
               mNH4 = gamma2( c,r )*MHp2( c,r )/BDs2( c,r )*MolN*1.0e3
               Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
               coef_a  = cg*(1.0+Ka/MHp2( c,r ))
               coef_b  = max_sorp+cg*half_sol*(1.0+Ka/MHp2(c,r)) - mNH4
               coef_c  = -half_sol*mNH4
               NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
               NH3_sol = NH4_sol*Ka/MHp2( c,r ) ! mg N/l
               cnh3g2  = (NH4_sol+NH3_sol)*1.0e-3/MolN/MHp2( c,r )
               NH3_st   = Max( NH3_st, Real( a1 ) * ( gamma2(c,r) * 1.0e-7
     &                  * w5cm * MolN * zsoil2 * 1.0e4 * 12.3 + 20.3 ) )
            End If
         Else 
            cnh3ll1 = Min( NH3_gam_ll / wg_ir, maxgam )
            cnh3g1  = Min( NH3_gam_soil / wg_ir, maxgam )
            cnh3g2  = Min( NH3_gam_soil / w5cm, maxgam ) ! if the leaf litter is too dry use the soil value
         End If
         cnh3ll1 = a1 * Max( cnh3ll1, 0.01 )
         cnh3g1  = a1 * Max( cnh3g1, 0.01 )
         cnh3g2  = a1 * Max( cnh3g2, 0.01 )

         If( ldry1 .Le. zsoil1 ) Then
            NH3_g  = cnh3g1
            NH3_ll = cnh3ll1
            wg     = wg_ir
            zsoil  = zsoil1
         Else
            NH3_g  = cnh3g2
            NH3_ll = cnh3g2
            wg     = w5cm
            zsoil  = zsoil2
         End If
         Return         
         End Subroutine Get_NH3_Comp
!------------------------------------------------------------------------------
! Subroutine to update the soil ammonia and pH due to evasion, deposition, 
! nitrification, leaching, and run off
! Soil ammonium, pH, leaching, and run off are defined in the module
!------------------------------------------------------------------------------
         Subroutine Calc_Nitrif ( dt, C, R, l_ag, flux_ag )
         
         Use UTILIO_DEFN
         Use ASX_DATA_MOD
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2
         Use LSM_MOD
         Use MOSAIC_MOD, Only: Tile_Data 
         
         Implicit None                
         
         Real,      Intent( IN )    :: dt     ! time step in s
         Integer,   Intent( IN )    :: C      ! Column
         Integer,   Intent( IN )    :: R      ! Row
         Integer,   Intent( IN )    :: L_ag   ! LU Agricuture index
         Real,      Intent( IN )    :: flux_ag ! ag emissions ppm*m/s

         Real    :: Kn     ! nitrification rate 1/s
         Real    :: Knit   ! EPIC combined nitrification rate
         Real    :: kn_max ! EPIC maximum nitrification rate
         Real    :: Kvs    ! Air-soil exchange rate for aqueous NH4+
         Real    :: MNHx   ! molar soil water NH4+ + NH3 conc.
         Real    :: pHsl   ! Soil pH
         Real    :: T_Soil ! Soil T
         Real    :: NH3_flux  ! mol/l/s emissions are positive
         Real    :: MHp       ! molar H concentration
         Real    :: gam       ! updated land use specific gamma
         Real    :: MV_air    ! Molar volume of air L/mol

         CHARACTER( 96 )            :: xmsg = ' '
         Character( 16 ), Parameter :: pname = 'Calc_Nitrif' 

! get the correct soil temp
         If(zsoil .Eq. zsoil1 ) Then
            T_Soil = MET_DATA%SoiT1( C,R )
            gam    = max( gamma1( C,R ), 630.0 )
            MHp    = MHp1( C,R )  
            Knit   = Knit1( C,R )
            gamma2( C,R ) = gamma2( C,R ) * exp( -Knit2( c,r ) * dt )
         Else
            T_Soil = MET_DATA%SoiT2( C,R )
            gam    = max( gamma2( C,R ), 630.0 )
            MHp    = MHp2( C,R ) 
            Knit   = Knit2( C,R ) 
            gamma1( C,R ) = gamma1( C,R ) * exp( -Knit1( c,r ) * dt ) 
         End If

! if MHp = 0.0 then there is no EPIC data for the grid cell or is not agricultural
         If( MHp .Eq. 0.0 .Or. frac_ag( C,R ) .Lt. 1.0e-3 ) Return
! Note that the kg to g and m3 to L units cancel 
         MV_air       = MWAIR / MET_DATA%DENS1( C,R )
! convert deposition from ppmv m/s to mol/l/s
         NH3_flux     = flux_ag * 1.0e-6 / MV_air / zsoil
! Get the soil pH
         pHsl = -log10( MHp )

! get NH4+ from gamma ([NH4+]/[H+] with units in mol/l)
         MNHx = gam * MHp
! Estimate the soil evasion rate assuming that the flux = MNHx*(1-exp(-kvs*dt)) where dt = 1 s and the flux is negative for deposition to the
! soil and positive for evasion from the soil. 
         If( NH3_flux .lt. 0.0 ) Then
            Kvs    = 0.0
! In epic the nitrification and evasion rates are added in a combined nitrification/evasion rate. 
! Then the emissions are assumed to be 5% of the total combined loss. Here we calcuate the nitrification/evasion rate as in EPIC and then the
! maximum rate is adjusted for the EPIC evasion because we will calculate the evasion seperately and add the rate to 
! EPIC nitrification. 
! EPIC estimates a combined evasion and nitrification rate with a maximum user specified rate. Here we subtract the CMAQ evasion rate from the 
! EPIC nitrification rate to ensure that the maximum rate is retained. 
            Kn     =  Knit/24.0/3600.0    
            If ( Frac_ag( c,r ) .Gt. 0.0 ) Then 
               If ( Kn .Gt. 0.0 ) Then
                  MNHx = -NH3_flux / Kn + ( MNHx + NH3_flux / Kn ) * exp( -Kn * dt )
               End If
               gam = MNHx / MHp 
            Else
               gam = Tile_Data%NH3_gam_grnd( L_Ag )
            End If
          Else ! evasion
            Kvs    = -log(1.0-NH3_flux/MNHx)
! In epic the nitrification and evasion rates are added in a combined nitrification/evasion rate. 
! Then the emissions are assumed to be 5% of the total combined loss. Here we calcuate the nitrification/evasion rate as in EPIC and then the
! maximum rate is adjusted for the EPIC evasion because we will calculate the evasion seperately and add the rate to 
! EPIC nitrification. 
            Kn     = Knit/24.0/3600.0 + Kvs 
            If ( Frac_ag( c,r ) .Gt. 0.0 ) Then 
               If ( Kn .Gt. 0.0 ) Then
                  MNHx = MNHx * exp( -Kn * dt )
               End If
               gam = MNHx / MHp 
            Else
               gam = Tile_Data%NH3_gam_grnd( L_Ag )
            End If
          End If
! Update soil concentrations
         If(zsoil .Eq. zsoil1 ) Then
            gamma1( C,R ) = gam 
         Else
            gamma2( C,R ) = gam
         End If

         If ( isnan( gam ) ) Then
            xmsg = 'NaN in Gamma Calculation'
            Write( logdev,* ) 'MNHx   :', MNHx
            Write( logdev,* ) 'gam    :', gam
            Write( logdev,* ) 'MHp    :', MHp
            Write( logdev,* ) 'wg     :', wg
            Write( logdev,* ) 'wres   :', GRID_DATA%WRES( c,r )
            Write( logdev,* ) 'wsat   :', GRID_DATA%WSAT( c,r )
            Write( logdev,* ) 'wfc    :', GRID_DATA%WFC( c,r )
            Write( logdev,* ) 'wwlt   :', GRID_DATA%WWLT( c,r )
            Write( logdev,* ) 'kvs    :', kvs
            Write( logdev,* ) 'kn     :', kn
            Write( logdev,* ) 'zsoil  :', zsoil
            Write( logdev,* ) 'frac_ag:', frac_ag( c,r )
            Call M3exit( pname, 0, 0, xmsg, xstat1)
         End If

         Return
         End Subroutine Calc_Nitrif

      End Module NH3_Bidi_Mod
