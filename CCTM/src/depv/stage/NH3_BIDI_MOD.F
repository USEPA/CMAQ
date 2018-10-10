
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
! $Header: /project/work/rep/arc/CCTM/src/depv/m3dry/ABFLUX_MOD.F,v 1.11 2012/04/19 17:03:23 yoj Exp $

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module NH3_Bidi_Mod

! Contains the shared variables and subroutines needed for the bidirectional 
! NH3 flux model in CMAQ
!
! INIT_ABFLUX - Intitializes the NH3 flux routines, allocates arrays, reads in
!               intial soil NH3 & H concentrations, and fertilizer application
!               amounts and timing for the model run
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
C-------------------------------------------------------------------------------

      Implicit None
! shared variables

! Private variables used in this module and subroutines  
      Real, Save, Allocatable, Private :: frac_ir( :,: ) ! irrigated fraction of ag    
      Real, Save, Allocatable, Private :: NH4ps1 ( :,:,: ) ! Initial Soil NH4
      Real, Save, Allocatable, Private :: NH4ps2 ( :,:,: ) ! Initial Soil NH4
      Real, Save, Allocatable, Private :: pHs1   ( :,:,: )
      Real, Save, Allocatable, Private :: pHs2   ( :,:,: )
      Real, Save,              Private :: C_gam           ! Canadian fertilizer facter   
      Real, Parameter,         Private :: maxgam   = 2.0e5 ! maximum soil gamm. It is assumed that any excess NH4 would from salts
      Real, Parameter,         Private :: natgam   = 20.0  ! Background soil gamma
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

         Implicit None 
! Includes
         Include SUBST_CONST     ! constants
         Include SUBST_FILES_ID  ! file name parameters

! Local Variables

         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime   
         Integer                :: jdate_yest
         Integer                :: c,r,l,k
         Integer, Parameter :: e2c_cats = 42   ! number of crop catigories
         Integer            :: gxoff, gyoff    ! global origin offset from file
         integer            :: strtcol, endcol, strtrow, endrow
         integer            :: strtcol_medi,  endcol_medi,  strtrow_medi,  endrow_medi
         integer            :: strtcol_fert,  endcol_fert,  strtrow_fert,  endrow_fert
         integer            :: strtcol_beld,  endcol_beld,  strtrow_beld,  endrow_beld
         Integer            :: ALLOCSTAT, N_GMN
         Real,  Allocatable :: Beld_ag ( :,:,: )        
         Real               :: Tot_Ag          ! total ag in the grid cell    
         Real,    Parameter :: convl1 = 7.142857e-04   ! ha/m**2 * mol/g N * 1/z_soil_l1 
         Real,    Parameter :: convl2 = 7.142857e-05   ! ha/m**2 * mol/g N * 1/z_soil_l2 
         Real               :: pHfac1
         Real               :: pHfac2
! STAGE bidi variables
         Real,       Allocatable :: F1_NH4     ( :,:,: ) ! fertlizer application to layer 1 
         Real,       Allocatable :: F2_NH4     ( :,:,: ) ! fertlizer application to layer 2
         Real,       Allocatable :: Nit1       ( :,:,: ) ! Epic Nitrification layer 1
         Real,       Allocatable :: Nit2       ( :,:,: ) ! Epic Nitrification layer 2
         Real,       Allocatable :: L1_MIN     ( :,:,: ) ! Epic Layer 1 organic N mineralization
         Real,       Allocatable :: L2_MIN     ( :,:,: ) ! Epic Layer 2 organic N mineralization 
         Real,       Allocatable :: GMN        ( :,:,: ) ! Epic Layer 1 and 2 N mineralized in Fest-C 1.4
         Real,       Allocatable :: L1_ON      ( :,:,: ) ! Epic Layer 1 organic N
         Real,       Allocatable :: L2_ON      ( :,:,: ) ! Epic Layer 2 organic N
         Real,       Allocatable :: L1_ON_Yest ( :,:,: ) ! Epic Layer 1 organic N
         Real,       Allocatable :: L2_ON_Yest ( :,:,: ) ! Epic Layer 2 organic N
         Real,       Allocatable :: F1_ON      ( :,:,: ) ! Epic Layer 1 organic N Fertilization 
         Real,       Allocatable :: F2_ON      ( :,:,: ) ! Epic Layer 2 organic N Fertilization 
         Real,       Allocatable :: C_knit1    ( :,:,: ) ! EPIC Crop nitrificaiton rate
         Real,       Allocatable :: C_knit2    ( :,:,: ) ! EPIC Crop nitrification rate
         Real,       Allocatable :: BDc1       ( :,:,: ) ! crop soil bulk density kg/ha
         Real,       Allocatable :: BDc2       ( :,:,: ) ! crop soil bulk density kg/ha   
         Real,       Allocatable :: fagam1     ( :,: )   ! delta gamma due to fertilizer app and mineralization
         Real,       Allocatable :: fagam2     ( :,: )   ! delta gamma due to fertilizer app and mineralization           
 
         Character( 16 ), Parameter :: pname = 'Init_ABFlux' 
         Character( 16 )            :: vname
         Character( 96 )            :: xmsg = ' '
         Character( 20 )            :: Beld_Names( e2c_cats )

C Some compilers do not pad with blank characters in data initialization
C                          '12345678901234567890'
         Data Beld_Names / 'Hay                 ',  !  1
     &                     'Hay_ir              ',  !  2
     &                     'Alfalfa             ',  !  3
     &                     'Alfalfa_ir          ',  !  4
     &                     'Other_Grass         ',  !  5
     &                     'Other_Grass_ir      ',  !  6
     &                     'Barley              ',  !  7
     &                     'Barley_ir           ',  !  8
     &                     'BeansEdible         ',  !  9
     &                     'BeansEdible_ir      ',  ! 10
     &                     'CornGrain           ',  ! 11
     &                     'CornGrain_ir        ',  ! 12
     &                     'CornSilage          ',  ! 13
     &                     'CornSilage_ir       ',  ! 14
     &                     'Cotton              ',  ! 15
     &                     'Cotton_ir           ',  ! 16
     &                     'Oats                ',  ! 17
     &                     'Oats_ir             ',  ! 18
     &                     'Peanuts             ',  ! 19
     &                     'Peanuts_ir          ',  ! 20
     &                     'Potatoes            ',  ! 21
     &                     'Potatoes_ir         ',  ! 22
     &                     'Rice                ',  ! 23
     &                     'Rice_ir             ',  ! 24
     &                     'Rye                 ',  ! 25
     &                     'Rye_ir              ',  ! 26
     &                     'SorghumGrain        ',  ! 27
     &                     'SorghumGrain_ir     ',  ! 28
     &                     'SorghumSilage       ',  ! 29
     &                     'SorghumSilage_ir    ',  ! 30
     &                     'Soybeans            ',  ! 31
     &                     'Soybeans_ir         ',  ! 32
     &                     'Wheat_Spring        ',  ! 33
     &                     'Wheat_Spring_ir     ',  ! 34
     &                     'Wheat_Winter        ',  ! 35
     &                     'Wheat_Winter_ir     ',  ! 36
     &                     'Other_Crop          ',  ! 37
     &                     'Other_Crop_ir       ',  ! 38
     &                     'Canola              ',  ! 39
     &                     'Canola_ir           ',  ! 40
     &                     'Beans               ',  ! 41
     &                     'Beans_ir            ' / ! 42
 

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
                  
! get soil pH to estimate the emissions potential
! time in the files is not the same    
         If ( .Not. Open3( E2C_SOIL, fsread3, pname ) ) Then
           xmsg = 'Could not open '// E2C_SOIL // ' file'
           Call M3exit ( pname, jdate, jtime, xmsg, xstat1 )
         End If

         Call Subhfile ( E2C_SOIL, gxoff, gyoff, strtcol, endcol, strtrow, endrow )

         If ( .Not. Allocated ( pHs1 ) ) Then
            Allocate ( pHs1 ( my_ncols,my_nrows,e2c_cats ) )
         End If

         vname = 'L1_PH'
         If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, strtrow, endrow,
     &                        strtcol, endcol, jdate, 0, pHs1 ) ) Then
            Write( xmsg,9001 ) vname, E2C_SOIL
            Go To 1001
         End If
         If ( .Not. Allocated ( pHs2 ) ) Then
            Allocate ( pHs2 ( my_ncols,my_nrows,e2c_cats ) )
         End If
         
         vname = 'L2_PH'
         If ( .Not. Xtract3 ( E2C_SOIL, vname, 1, e2c_cats, strtrow, endrow,
     &                        strtcol, endcol, jdate, 0, pHs2 ) ) Then
            Write( xmsg,9001 ) vname, E2C_SOIL
            Go To 1001
         End If
! get soil fertilizer applicaiton rates and dates
! time in the files is not the same    
         If ( .Not. Open3( E2C_FERT, fsread3, pname ) ) Then
           xmsg = 'Could not open '// E2C_FERT // ' file'
           Call M3exit ( pname, 0, 0, xmsg, xstat1 )
         End If
         Allocate( F1_NH4     ( my_ncols,my_nrows,e2c_cats ), 
     &             F2_NH4     ( my_ncols,my_nrows,e2c_cats ), 
     &             Nit1       ( my_ncols,my_nrows,e2c_cats ),
     &             Nit2       ( my_ncols,my_nrows,e2c_cats ),
     &             L1_MIN     ( my_ncols,my_nrows,e2c_cats ),
     &             L2_MIN     ( my_ncols,my_nrows,e2c_cats ),    
     &             L1_ON      ( my_ncols,my_nrows,e2c_cats ),
     &             L2_ON      ( my_ncols,my_nrows,e2c_cats ), 
     &             C_Knit1    ( my_ncols,my_nrows,e2c_cats ),
     &             C_Knit2    ( my_ncols,my_nrows,e2c_cats ),
     &             BDc1       ( my_ncols,my_nrows,e2c_cats ),
     &             BDc2       ( my_ncols,my_nrows,e2c_cats ),
     &             Beld_ag    ( my_ncols,my_nrows,e2c_cats ),
     &             NH4ps1     ( my_ncols,my_nrows,e2c_cats ),
     &             NH4ps2     ( my_ncols,my_nrows,e2c_cats ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating EPIC vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         L1_MIN  = 0.0
         L2_MIN  = 0.0
         C_Knit1 = 0.0
         C_Knit2 = 0.0

         If ( .Not. desc3( E2C_FERT ) ) Then
            xmsg = 'Could not get ' // E2C_FERT // ' file description'
            Call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         End If                                   

         N_GMN = INDEX1( 'GMN', NVARS3D, VNAME3D )
         
         Call Subhfile ( E2C_FERT, gxoff, gyoff, strtcol_fert, endcol_fert, 
     &                   strtrow_fert, endrow_fert )

         vname = 'L1_BD'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                       strtcol_fert, endcol_fert, jdate, 0, BDc1 ) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If

         vname = 'L2_BD'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                       strtcol_fert, endcol_fert, jdate, 0, BDc2 ) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If       

         vname = 'L1_NH3'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                       strtcol_fert, endcol_fert, jdate, 0, NH4ps1) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If
         vname = 'L2_NH3'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                    strtcol_fert, endcol_fert, jdate, 0, NH4ps2) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If
         vname = 'L1_NITR'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                       strtcol_fert, endcol_fert, jdate, 0, Nit1 ) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If
         vname = 'L2_NITR'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                       strtcol_fert, endcol_fert, jdate, 0, Nit2 ) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If       
         vname = 'L1_ON'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                       strtcol_fert, endcol_fert, jdate, 0, L1_ON ) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If
         vname = 'L2_ON'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                    strtcol_fert, endcol_fert, jdate, 0, L2_ON ) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If      

         If( N_GMN .Gt. 0 ) Then ! Using Fest-C 1.4 output
            Allocate( GMN     ( my_ncols,my_nrows,e2c_cats ), STAT = ALLOCSTAT )
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating GMN'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If

            vname = 'GMN'
            If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                    strtcol_fert, endcol_fert, jdate, 0, GMN ) ) Then
               Write( xmsg,9001 ) vname, E2C_FERT
               Go To 1001
            End If        
           
         End If

! get BELD4 land use types time independent files. 
         If ( .Not. Open3( BELD4_LU, fsread3, pname ) ) Then
           xmsg = 'Could not open '// BELD4_LU // ' file'
           Call M3exit ( pname, jdate, jtime, xmsg, xstat1 )
         End If                
         Call Subhfile ( BELD4_LU, gxoff, gyoff, strtcol_beld, endcol_beld, 
     &                   strtrow_beld, endrow_beld )

         Do k = 1, e2c_cats
            vname = BELD_Names(k)
            If ( .Not. Interpx ( BELD4_LU, vname, pname, strtcol_beld, endcol_beld,
     &                           strtrow_beld, endrow_beld, 1, 1, jdate, jtime,
     &                           Beld_ag( :,:,k ) ) ) Then   
               Write( xmsg,9001 ) vname, BELD4_LU
               Go To 1001
            End If
         End Do
         Beld_ag = 0.01 * Beld_ag   ! convert to fraction

! Allocate variable needed soil processes and fertilization
         Allocate( gamma1     ( my_ncols,my_nrows ),
     &             gamma2     ( my_ncols,my_nrows ),
     &             fagam1     ( my_ncols,my_nrows ),
     &             fagam2     ( my_ncols,my_nrows ), 
     &             MHp1       ( my_ncols,my_nrows ),
     &             MHp2       ( my_ncols,my_nrows ),
     &             BDs1       ( my_ncols,my_nrows ),
     &             BDs2       ( my_ncols,my_nrows ),
     &             Knit1      ( my_ncols,my_nrows ),
     &             Knit2      ( my_ncols,my_nrows ),
     &             NH4_G1     ( my_ncols,my_nrows ),
     &             NH4_G2     ( my_ncols,my_nrows ),
     &             frac_ir    ( my_ncols,my_nrows ), 
     &             frac_ag    ( my_ncols,my_nrows ), 
     &              STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating soil vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         gamma1  = 0.0
         gamma2  = 0.0
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

! get fertilizer from the previous month to estimate soil NH4+        
! time in the files is not the same
         If ( .Not. Open3( INIT_MEDC_1, fsread3, pname ) ) Then
            Write(Logdev,*) 'Estimating soil NHx from EPIC output'
            If ( .Not. Open3( E2C_FERT, fsread3, pname ) ) Then
               xmsg = 'Could not open '// E2C_FERT // ' file'
               Call M3exit ( pname, jdate, jtime, xmsg, xstat1 )
            End If

            If( N_GMN .Gt. 0 ) Then
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

         ! parameters
            Do r = 1, my_nrows
               Do c = 1, my_ncols
                  Tot_Ag = 0.0                  
                  Do l = 1, e2c_cats
                     Tot_Ag = Tot_Ag + Beld_ag( c,r,l )
                     If ( Index( Beld_Names( l ), '_ir' ) .Gt. 0 ) Then
                        frac_ir( c,r ) = frac_ir( c,r ) + Beld_ag( c,r,l )
                     End If                    
                     If( .Not. IsNaN( pHs1( c,r,l ) )   .And.
     &                   .Not. IsNaN( pHs2( c,r,l ) )   .And.
     &                   .Not. IsNaN( NH4ps1( c,r,l ) ) .And.
     &                   .Not. IsNaN( NH4ps2( c,r,l ) ) .And.
     &                    Beld_ag( c,r,l ) .Gt. 0.0 ) Then
                        If( pHs1( c,r,l ) .Gt. 0.0 ) Then
                           pHfac1 = 10.0 ** (-pHs1( c,r,l ) )
                           pHfac2 = 10.0 ** (-pHs2( c,r,l ) )
                        Else
                           pHfac1 = 0.0
                           pHfac2 = 0.0
                        End If
! convert units from kg/ha N to mol/l
                        If ( pHs1( c,r,l )   .Gt. 4.0       .And.
     &                       pHs2( c,r,l )   .Gt. 4.0       .And.
     &                       pHs1( c,r,l )   .Lt. 9.0       .And.
     &                       pHs2( c,r,l )   .Lt. 9.0       .And.
     &                       NH4ps1( c,r,l ) .Gt. 0.0       .And.
     &                       NH4ps2( c,r,l ) .Gt. 0.0     ) Then
                           gamma1( c,r ) = gamma1( c,r ) + convl1 * NH4ps1( c,r,l )
     &                                   / pHfac1 * Beld_ag( c,r,l )
                           MHp1( c,r )   = MHp1( c,r ) + Beld_ag( c,r,l ) * pHfac1
                           BDs1( c,r )   = BDs1( c,r ) + Beld_ag( c,r,l ) * BDc1( c,r,l )
                           Knit1( c,r )  = Knit1( c,r ) + Beld_ag( c,r,l ) * Nit1( c,r,l )
                           Knit2( c,r )  = Knit2( c,r ) + Beld_ag( c,r,l ) * Nit2( c,r,l )
                           NH4_G1( c,r ) = NH4_G1( c,r ) + Beld_ag( c,r,l ) * NH4ps1( c,r,l )
                           NH4_G2( c,r ) = NH4_G2( c,r ) + Beld_ag( c,r,l ) * NH4ps2( c,r,l )
                           gamma2( c,r ) = gamma2( c,r ) + convl2 * NH4ps2( c,r,l )
     &                                   / pHfac2 * Beld_ag( c,r,l )
                           MHp2( c,r )   = MHp2( c,r ) + Beld_ag( c,r,l ) * pHfac2
                           BDs2( c,r )   = BDs2( c,r ) + Beld_ag( c,r,l ) * BDc2( c,r,l )
                           If ( .Not. IsNaN( F1_NH4( c,r,l ) ) .And.
     &                                       F1_NH4( c,r,l ) .Gt. 0.0 ) Then
! Surface applied fertlizer
                              fagam1( c,r ) = fagam1( c,r ) + F1_NH4( c,r,l ) * Beld_ag( c,r,l )
     &                                      * convl1 / pHfac1
                           End If
                           If ( .Not. IsNaN( F2_NH4( c,r,l ) ) .And.
     &                                       F2_NH4( c,r,l ) .Gt. 0.0 ) Then
! Injected fertilizer
                              fagam2( c,r ) = fagam2( c,r ) + F2_NH4( c,r,l ) * Beld_ag( c,r,l )
     &                                      * convl2 / pHfac2
                           End If
                           If ( IsNaN( fagam1( c,r ) ) .Or. IsNaN( fagam2( c,r ) ) ) Then
                               xmsg = 'NaN in fagam Calculation'
                               Call M3exit( pname, jdate, jtime, xmsg, xstat1 )
                           End If
                        End If
                     End If
                  End Do ! e2c_cats
                  frac_ag( c,r ) = Tot_Ag
                  If ( Tot_Ag .ge. 1.0e-6 .And. ! ~ 1.5 ha per grid cell
     &                 MHp1( c,r ) / Tot_Ag .Gt. 1.0e-9 .And.
     &                 MHp1( c,r ) / Tot_Ag .Lt. 1.0e-4 .And.
     &                 MHp2( c,r ) / Tot_Ag .Gt. 1.0e-9 .And.
     &                 MHp2( c,r ) / Tot_Ag .Lt. 1.0e-4 ) Then
! get the agricultural only relevant number
                     MHp1( c,r ) = MHp1( c,r ) / Tot_Ag
                     MHp2( c,r ) = MHp2( c,r ) / Tot_Ag
                     gamma1( c,r ) = ( gamma1( c,r ) + fagam1( c,r ) ) / Tot_Ag
                     gamma2( c,r ) = ( gamma2( c,r ) + fagam2( c,r ) ) / Tot_Ag
                     BDs1( c,r )    = BDs1( c,r ) / Tot_Ag
                     BDs2( c,r )    = BDs2( c,r ) / Tot_Ag 
                     frac_ir( c,r ) = frac_ir( c,r ) / Tot_Ag
! Set a minimum Gamma for Ag based off of Zhang et al. 2010
                     gamma1( c,r ) = max( gamma1( c,r ), 630.0 )
                     gamma2( c,r ) = max( gamma2( c,r ), 630.0 )
                     If ( IsNaN( gamma1( c,r ) ) .Or. IsNaN( gamma2( c,r ) ) .Or.
     &                    IsNaN( MHp2( c,r ) ) .Or. IsNaN( MHp2( c,r ) ) ) Then
                        xmsg = 'NaN in grid cell Gamma Calculation'
                        Call M3exit( pname, jdate, jtime, xmsg, xstat1 )
                     End If
                  Else
                     MHp1( c,r ) = 0.0
                     MHp2( c,r ) = 0.0
                     gamma1( c,r ) = 0.0
                     gamma2( c,r ) = 0.0
                  End If
               End Do ! c
            End Do ! r

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
            Call Subhfile ( INIT_MEDC_1, gxoff, gyoff, strtcol_medi, endcol_medi, 
     &                      strtrow_medi, endrow_medi )

! for whatever reason the jdate and jtime get messed up by the e2c files
            Write( Logdev,* ) 'Reading soil NHx from Previous CMAQ run'

            vname = 'Gamma1'
            If ( .Not. Xtract3 ( INIT_MEDC_1, vname, 1, 1, strtrow_medi, endrow_medi,
     &                           strtcol_medi, endcol_medi, jdate, 000000, gamma1 ) ) Then
               Write( xmsg,9001 ) vname, INIT_MEDC_1
               Go To 1001
            End If
            vname = 'Gamma2'
            If ( .Not. Xtract3 ( INIT_MEDC_1, vname, 1, 1, strtrow_medi, endrow_medi,
     &                           strtcol_medi, endcol_medi, jdate, 000000, gamma2 ) ) Then
               Write( xmsg,9001 ) vname, INIT_MEDC_1
               Go To 1001
            End If
            vname = 'L1_ANH3'
            If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                          strtcol_fert, endcol_fert, jdate, 0, F1_NH4 ) ) Then
               Write( xmsg,9001 ) vname, E2C_FERT
               Go To 1001
            End If
            vname = 'L2_ANH3'
            If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                          strtcol_fert, endcol_fert, jdate, 0, F2_NH4 ) ) Then
               Write( xmsg,9001 ) vname, E2C_FERT
               Go To 1001
            End If

            If( N_GMN .Le. 0 ) Then

               If( MOD(jdate,1000) .Eq. 1 ) Then
                  If( MOD(jdate,4000) .Eq. 0 .And. MOD(jdate,100000) .Eq. 0 .And. MOD(jdate,400000) .Eq. 0) Then
                     jdate_yest = (INT(jdate/1000)-1)*1000+366
                  Else ! not a leap year
                     jdate_yest = (INT(jdate/1000)-1)*1000+365
                  End If
               Else 
                  jdate_yest = jdate-1
               End If

               Allocate( L1_ON_Yest ( my_ncols,my_nrows,e2c_cats ),
     &                   L2_ON_Yest ( my_ncols,my_nrows,e2c_cats ),
     &                   F1_ON      ( my_ncols,my_nrows,e2c_cats ),
     &                   F2_ON      ( my_ncols,my_nrows,e2c_cats ),
     &                   STAT = ALLOCSTAT )
               If ( ALLOCSTAT .Ne. 0 ) Then
                  XMSG = 'Failure allocating organic N vars'
                  Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               End If

               vname = 'L1_AON'
               If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                          strtcol_fert, endcol_fert, jdate, 0, F1_ON ) ) Then
                  Write( xmsg,9001 ) vname, E2C_FERT
                  Go To 1001
               End If
               vname = 'L2_AON'
               If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                             strtcol_fert, endcol_fert, jdate, 0, F2_ON ) ) Then
                  Write( xmsg,9001 ) vname, E2C_FERT
                  Go To 1001
               End If   

               vname = 'L1_ON'
               If ( .Not. Xtract3 (E2C_FERT_YEST, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                       strtcol_fert, endcol_fert, jdate_yest, 0, L1_ON_Yest ) ) Then
                  Write( xmsg,9001 ) vname, E2C_FERT
                  Go To 1001
               End If
               vname = 'L2_ON'
               If ( .Not. Xtract3 (E2C_FERT_YEST, vname, 1, e2c_cats, strtrow_fert, endrow_fert,
     &                       strtcol_fert, endcol_fert, jdate_yest, 0, L2_ON_Yest ) ) Then
                  Write( xmsg,9001 ) vname, E2C_FERT
                  Go To 1001
               End If 
            End If
            If( N_GMN .Gt. 0 ) Then
! NH4 from organic N mineralization is not layer specific in Fest-C v1.4 therefore it is allocated proportionally
! to the organic N from which it was mineralized.
               Where( GMN .Gt. 0.0 .And. L1_ON .Gt. 0.0 )
                  L1_MIN = GMN * L1_ON / ( L1_ON + L2_ON )
               End Where
               Where( GMN .Gt. 0.0 .And. L2_ON .Gt. 0.0 )
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

            do r = 1, my_nrows
               Do c = 1, my_ncols
                  Tot_Ag = 0.0 
                  Do l = 1, e2c_cats
                     Tot_Ag = Tot_Ag + Beld_ag( c,r,l )
                     If ( Index( Beld_Names( l ), '_ir' ) .Gt. 0 ) Then
                           frac_ir( c,r ) = frac_ir( c,r ) + Beld_ag( c,r,l )
                     End If
                     If( .Not. IsNaN( pHs1( c,r,l ) )   .And.
     &                   .Not. IsNaN( pHs2( c,r,l ) )   .And.
     &                    Beld_ag( c,r,l ) .Gt. 0.0 ) Then
                        If( pHs1( c,r,l ) .Gt. 0.0 ) Then
                           pHfac1 = 10.0 ** (-pHs1( c,r,l ) )
                           pHfac2 = 10.0 ** (-pHs2( c,r,l ) )
                        Else
                           pHfac1 = 0.0
                           pHfac2 = 0.0
                        End If
! convert units from kg/ha N to mol/l 
                        If ( pHs1( c,r,l )   .Gt. 4.0       .And.
     &                       pHs2( c,r,l )   .Gt. 4.0       .And.
     &                       pHs1( c,r,l )   .Lt. 9.0       .And.
     &                       pHs2( c,r,l )   .Lt. 9.0 ) Then
                           MHp1( c,r )   = MHp1( c,r ) + Beld_ag( c,r,l ) * pHfac1
                           MHp2( c,r )   = MHp2( c,r ) + Beld_ag( c,r,l ) * pHfac2
                           BDs1( c,r )   = BDs1( c,r ) + Beld_ag( c,r,l ) * BDc1( c,r,l )
                           BDs2( c,r )   = BDs2( c,r ) + Beld_ag( c,r,l ) * BDc2( c,r,l )
                           Knit1( c,r )  = Knit1( c,r ) + Beld_ag( c,r,l ) * Nit1( c,r,l )
                           Knit2( c,r )  = Knit2( c,r ) + Beld_ag( c,r,l ) * Nit2( c,r,l )
                           NH4_G1( c,r ) = NH4_G1( c,r ) + Beld_ag( c,r,l ) * NH4ps1( c,r,l )
                           NH4_G2( c,r ) = NH4_G2( c,r ) + Beld_ag( c,r,l ) * NH4ps2( c,r,l )
                           If ( .Not. IsNaN( F1_NH4( c,r,l ) ) .And. 
     &                           F1_NH4( c,r,l ) .Gt. 0.0    ) Then
! surface applied fertilizer                        
                                fagam1( c,r ) = fagam1( c,r ) + F1_NH4( c,r,l ) * Beld_ag( c,r,l )
     &                                       * convl1 / pHfac1
                           End If
                           If ( .Not. IsNaN( F2_NH4( c,r,l ) ) .And. 
     &                           F2_NH4( c,r,l ) .Gt. 0.0    ) Then
! Injected fertilizer                        
                                fagam2( c,r ) = fagam2( c,r ) + F2_NH4( c,r,l )* Beld_ag( c,r,l )
     &                                       * convl2 / pHfac2
                           End If
                           If ( IsNaN( fagam1( c,r ) ) .Or. IsNaN( fagam2( c,r ) ) ) Then
                              xmsg = 'NaN in fagam Calculation'
                              Call M3exit( pname, jdate, jtime, xmsg, xstat1)
                        End If   
                        End If
                     End If                      
                  End Do ! e2c_cats
                  frac_ag( c,r ) = Tot_Ag 
! Agricultural only numbers were written to file                  
                  If ( Tot_Ag .Ge. 1.0e-6  .And.
     &                 MHp1( c,r ) .Gt. 1.0e-9 .And.
     &                 MHp1( c,r ) .Lt. 1.0e-4 .And. 
     &                 MHp2( c,r ) .Gt. 1.0e-9 .And.
     &                 MHp2( c,r ) .Lt. 1.0e-4 ) Then
! get the agricultural only relevant number                                 
                     gamma1( c,r )  = ( gamma1( c,r )*Tot_Ag + fagam1( c,r ) ) / Tot_Ag
                     gamma2( c,r )  = ( gamma2( c,r )*Tot_Ag + fagam2( c,r ) ) / Tot_Ag   
                     BDs1( c,r )    = BDs1( c,r ) / Tot_Ag
                     BDs2( c,r )    = BDs2( c,r ) / Tot_Ag
                     frac_ir( c,r ) = frac_ir( c,r ) / Tot_Ag
! Set a minimum Gamma for Ag based off of Zhang et al. 2010
                     gamma1( c,r ) = max( gamma1( c,r ), 630.0 )
                     gamma2( c,r ) = max( gamma2( c,r ), 630.0 )
                     If ( IsNaN( gamma1( c,r ) ) .Or. IsNaN( gamma2( c,r ) ) .Or. 
     &                    IsNaN( MHp2( c,r ) ) .Or. IsNaN( MHp2( c,r ) )) Then
                        xmsg = 'NaN in grid cell Gamma Calculation'
                        Call M3exit( pname, jdate, jtime, xmsg, xstat1 )
                     End If   
                  End If
               End Do ! c
            End Do ! r

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
         Subroutine Get_NH3_Comp( NH3_st, NH3_g, Rg, diff, r, c, l, s )
         
         Use UTILIO_DEFN
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2
         Use ASX_DATA_MOD
         Use LSM_MOD
         
         Implicit None
         
         Include SUBST_FILES_ID  ! file name parameters

         Integer, Intent( IN )  :: r, c, l, s ! row
         Real,    Intent( IN )  :: diff     ! NH3 diffusion in air m2/s
         Real,    Intent( OUT ) :: NH3_st   ! Stomatal Compensation point
         Real,    Intent( OUT ) :: NH3_g    ! ground Compensation point
         Real,    Intent( OUT ) :: Rg       ! Soil diffusive resistance to soil water
         
         Real                   :: cnh3g1, cnh3g2   ! NH3 compensation concentration for ground [ppm]
         Real                   :: gammas       ! [NH4+]/[H+]
         Real                   :: mNH4         ! Soil total NH4+ mg N / Kg
         Real                   :: Ka           ! NH4 acid dissociation constant
         Real                   :: coef_a       ! intermediate variable to solve for soil solution NHx
         Real                   :: coef_b       ! intermediate variable to solve for soil solution NHx
         Real                   :: coef_c       ! intermediate variable to solve for soil solution NHx
         Real                   :: NH4_sol      ! NH4+ in soil water solution mg N / kg
         Real                   :: NH3_sol      ! NH3 in soil water solution mg N / kg
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
         Real,        Parameter :: MolNH3   = 17.01  ! g/mol NH3         
         Real( 8 )              :: agfrac
         Real                   :: wg_ir        ! 1 cm soil moisture  
         Real                   :: w5cm, cg         ! soil moisture in top 5 cm (vol frc)
         Real( 8 )              :: watfrac ! water fraction

         Integer                :: j
! Point were soil solution NH4 equals half the maximum sorption capacity Venteria et al. Sci. Rep. doi:10.1038/srep12153
         Real, Parameter        :: half_sol = 345.0 
! Maximum NH4 soil sorption capacity Venteria et al. Sci. Rep. doi:10.1038/srep12153
         Real, Parameter        :: max_sorp = 550.0 

 
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
         ldry1 = min(ldry1,zsoil1)

         ldry2 = ( ldry_max - zsoil1 ) * ( Exp( ( 1.0 - w5cm / GRID_DATA%WSAT( c,r ) ) ** 5 ) - 1.0 ) / 1.718
         
         dp  = diff * GRID_DATA%WSAT( c,r )**2
     &       * ( 1.0 - GRID_DATA%WRES( c,r ) / GRID_DATA%WSAT( c,r ) ) ** ( 2.0 + 3.0 / GRID_DATA%BSLP( c,r ) )         

         rsoil1 = ldry1 / dp
         rsoil2 = ldry2 / dp

!> Compute compensation point. gamma is specified according to the amount of 
!> cultivated vegetation               
         a1     = 161512.0d0 / real( MET_DATA%SoiT1( c,r ), 8 )
     &          * 10.0d0 ** ( -4507.11d0 / real( MET_DATA%SoiT1( c,r ), 8 ) )
         a1     = a1 * 24.5d0 * 1.0d6  ! ppm
         NH3_st = a1 * luf_fac( l )       
         
!> Set a maximum [NH4]/[H+] ratio at 200,000 based on output from the AIM aerosol
!> model any [NH4] in excess of this ratio is assumed to partition into the solid
!> phase. Canada soil gamma taken from Zhang et al 2010 JGR Table 5

         Select Case( cat_lu( l ) )
            Case( 'WATER','SNOWICE' ) ! water, ice or snow
               cnh3g1 = 0.0
               cnh3g2 = 0.0
               NH3_st = 0.0
            Case( 'AG','HAY' ) ! Ag
               if( gamma1( c,r ) .eq. 0.0 ) Then 
                  cg = wg_ir/1.6 ! L water / kg soil
                  mNH4    = C_gam*1.0e-7/1.6*14.0*1.0e3 ! assume a bulk density of 1.6 kg/l untill Canadian data is available
                  Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
                  coef_a  = cg * (1.0+Ka/1.0e-7) ! assume a pH of 7
                  coef_b  = max_sorp+cg * half_sol * (1.0+Ka/1.0e-7) - mNH4
                  coef_c  = -half_sol * mNH4
                  NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
                  NH3_sol = NH4_sol*Ka/1.0e-7 ! mg N/l
                  cnh3g1  = (NH4_sol+NH3_sol)*1.0e-3/14.0/1.0e-7
               Else 
                  cg = wg_ir/BDs1( c,r ) ! L water / kg soil
                  mNH4 = gamma1( c,r )*MHp1( c,r )/BDs1( c,r )*14.0*1.0e3
                  Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
                  coef_a  = cg*(1.0+Ka/MHp1( c,r ))
                  coef_b  = max_sorp+cg*half_sol*(1.0+Ka/MHp1(c,r)) - mNH4
                  coef_c  = -half_sol*mNH4
                  NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
                  NH3_sol = NH4_sol*Ka/MHp1( c,r ) ! mg N/l
                  cnh3g1  = (NH4_sol+NH3_sol)*1.0e-3/14.0/MHp1( c,r )
               End If
               If( gamma2( c,r ) .eq. 0.0 ) Then 
                  cg = w5cm/1.6 ! L water / kg soil
                  mNH4    = C_gam*1.0e-7/1.6*14.0*1.0e3 ! assume a bulk density of 1.6 kg/l untill Canadian data is available
                  Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
                  coef_a  = cg*(1.0+Ka/1.0e-7)
                  coef_b  = max_sorp+cg*half_sol*(1.0+Ka/1.0e-7) - mNH4
                  coef_c  = -half_sol*mNH4
                  NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
                  NH3_sol = NH4_sol*Ka/1.0e-7 ! mg N/l
                  cnh3g2  = (NH4_sol+NH3_sol)*1.0e-3/14.0/1.0e-7!/wg_ir
                  NH3_st   = Max( NH3_st, Real( a1 ) *( C_gam * 1.0e-7
     &                    * w5cm * MolN * zsoil2 * 1.0e4 * 12.3 + 20.3 ) )
                Else 
                  cg = w5cm/BDs2( c,r ) ! L water / kg soil
                  mNH4 = gamma2( c,r )*MHp2( c,r )/BDs2( c,r )*14.0*1.0e3
                  Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
                  coef_a  = cg*(1.0+Ka/MHp2( c,r ))
                  coef_b  = max_sorp+cg*half_sol*(1.0+Ka/MHp2(c,r)) - mNH4
                  coef_c  = -half_sol*mNH4
                  NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
                  NH3_sol = NH4_sol*Ka/MHp2( c,r ) ! mg N/l
                  cnh3g2  = (NH4_sol+NH3_sol)*1.0e-3/14.0/MHp2( c,r )
                  NH3_st   = Max( NH3_st, Real( a1 ) * ( gamma2(c,r) * 1.0e-7
     &                    * w5cm * MolN * zsoil2 * 1.0e4 * 12.3 + 20.3 ) )
               End If
            Case( 'AGMOS' ) ! Ag mosaic 67% ag 33% mosaic 
               If( gamma1( c,r ) .eq. 0.0 ) Then 
                  cg = wg_ir/1.6 ! L water / kg soil
                  mNH4    = (C_gam*twothree+natgam*onethree)*1.0e-7/1.6*14.0*1.0e3 ! assume a bulk density of 1.6 kg/l untill Canadian data is available
                  Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
                  coef_a  = cg*(1.0+Ka/1.0e-7)
                  coef_b  = max_sorp+cg*half_sol*(1.0+Ka/1.0e-7) - mNH4
                  coef_c  = -half_sol*mNH4
                  NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
                  NH3_sol = NH4_sol*Ka/1.0e-7 ! mg N/l
                  cnh3g1  = (NH4_sol+NH3_sol)*1.0e-3/14.0/1.0e-7!/wg_ir
               Else  
                  cg = wg_ir/BDs1( c,r ) ! L water / kg soil
                  mNH4 = (gamma1( c,r )*MHp1( c,r )*twothree + natgam*1.0e-7*onethree)/BDs1( c,r )*14.0*1.0e3
                  Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
                  coef_a  =cg*(1+Ka/MHp1( c,r ))
                  coef_b  = max_sorp+cg*half_sol*(1.0+Ka/MHp1(c,r)) - mNH4
                  coef_c  = -half_sol*mNH4
                  NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
                  NH3_sol = NH4_sol*Ka/MHp1( c,r ) ! mg N/l
                  cnh3g1  = (NH4_sol+NH3_sol)*1.0e-3/14.0/MHp1(c,r)!/wg_ir
               End If
               If( gamma2( c,r ) .eq. 0.0 ) Then 
                  cg = w5cm/1.6 ! L water / kg soil
                  mNH4    = (C_gam*twothree + natgam*onethree)*1.0e-7/1.6*14.0*1.0e3 ! assume a bulk density of 1.6 kg/l untill Canadian data is available
                  Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
                  coef_a  = cg*(1+Ka/1.0e-7)
                  coef_b  = max_sorp+cg*half_sol*(1.0+Ka/1.0e-7) - mNH4
                  coef_c  = -half_sol*mNH4
                  NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
                  NH3_sol = NH4_sol*Ka/1.0e-7 ! mg N/l
                  cnh3g2  = (NH4_sol+NH3_sol)*1.0e-3/14.0/1.0e-7!/w5cm
                  NH3_st   = Max( NH3_st, Real( a1 ) * ( C_gam * 1.0e-7
     &                    * w5cm * MolN * zsoil2 *1.0e4 * 12.3 + 20.3 ) )
     &                    * twothree + onethree * NH3_st
               Else 
                  cg = w5cm/BDs2( c,r ) ! L water / kg soil
                  mNH4    = (gamma2( c,r )*MHp2( c,r )/BDs2( c,r )*twothree+natgam/3.0/1.6)*14.0*1.0e3
                  Ka      = 5.68853e-10*exp(-6248.151*(1.0/MET_DATA%SoiT1( c,r )-1.0/298.15))
                  coef_a  = cg*(1+Ka/MHp2( c,r ))
                  coef_b  = max_sorp+cg*half_sol*(1.0+Ka/MHp2(c,r)) - mNH4
                  coef_c  = -half_sol*mNH4
                  NH4_sol = (-coef_b+sqrt(coef_b**2.0-4.0*coef_a*coef_c))/(2.0*coef_a)
                  NH3_sol = NH4_sol*Ka/MHp2( c,r ) ! mg N/l
                  cnh3g2  = (NH4_sol+NH3_sol)*1.0e-3/14.0/MHp2( c,r )!/w5cm
                  NH3_st   = Max( NH3_st, Real( a1 ) * ( gamma2(c,r) * 1.0e-7
     &                    * w5cm * MolN * zsoil2 *1.0e4 * 12.3 + 20.3 ) )
     &                    * twothree + onethree * NH3_st
               End If     
            Case Default ! not ag
               cnh3g1 = Min( natgam / wg_ir, maxgam )
               cnh3g2 = Min( natgam / w5cm, maxgam )
         End Select
         cnh3g1 = a1 * Max( cnh3g1, 0.01 )
         cnh3g2 = a1 * Max( cnh3g2, 0.01 )

         If( ldry1 .Le. zsoil1 ) Then
            NH3_g = cnh3g1
            Rg = rsoil1
            wg = wg_ir
            zsoil = zsoil1
         Else
            NH3_g = cnh3g2
            Rg = rsoil1 + rsoil2
            wg = w5cm
            zsoil = zsoil2
         End If
         Return         
         End Subroutine Get_NH3_Comp
!------------------------------------------------------------------------------
! Subroutine to update the soil ammonia and pH due to evasion, deposition, 
! nitrification, leaching, and run off
! Soil ammonium, pH, leaching, and run off are defined in the module
!------------------------------------------------------------------------------
         Subroutine Calc_Nitrif ( dt, C, R, flux_ag )
         
         Use UTILIO_DEFN
         Use ASX_DATA_MOD
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2
         Use LSM_MOD
         
         Implicit None                
         
         Real,      Intent( IN )    :: dt     ! time step in s
         Integer,   Intent( IN )    :: C      ! Column
         Integer,   Intent( IN )    :: R      ! Row
         Real,      Intent( IN )    :: flux_ag ! ag emissions ppm*m/s

         Real    :: ag_tot ! total agriculture
         Real    :: fTg    ! nitrification temperature factor
         Real    :: fSWg   ! nitrification Soil Water factor
         Real    :: fpHg   ! nitrification pH factor 
         Real    :: fU10   ! EPIC wind speed volitization factor
         Real    :: Kn     ! nitrification rate 1/s
         Real    :: Knit   ! EPIC combined nitrification rate
         Real    :: kn_max ! EPIC maximum nitrification rate
         Real    :: Kvs    ! Air-soil exchange rate for aqueous NH4+
         Real    :: kvg    ! Air-soil exchange rate for gas phase NH3
         Real    :: MNHx   ! molar soil water NH4+ + NH3 conc.
         Real    :: wg25   ! wilting point +0.25*(field capacity-wilting point)
         Real    :: SWF    ! Soil water nutreint cycling factor
         Real    :: pHsl   ! Soil pH
         Real    :: T_Soil ! Soil T
         Real    :: NH3_flux  ! mol/l/s emissions are positive
         Real    :: MHp       ! molar H concentration
         Real    :: gam       ! updated land use specific gamma
         Real    :: mean_gam  ! Grid cell mean gamma
         Real    :: MV_air    ! Molar volume of air L/mol


         Integer :: i   ! loop counter
         CHARACTER( 96 )            :: xmsg = ' '
         Character( 16 ), Parameter :: pname = 'Calc_Nitrif' 

!         If( soil_dep .Eq. 0.0 .And. soil_emis .Eq. 0.0 ) Return
! get the correct soil temp
         If(zsoil .Eq. zsoil1 ) Then
            T_Soil = MET_DATA%SoiT1( C,R )
            gam    = gamma1( C,R )
            MHp    = MHp1( C,R )  
            Knit   = Knit1( C,R )
            gamma2( C,R ) = gamma2( C,R ) * exp( -Knit2( c,r ) * dt )
         Else
            T_Soil = MET_DATA%SoiT2( C,R )
            gam    = gamma2( C,R )
            MHp    = MHp2( C,R ) 
            Knit   = Knit2( C,R ) 
            gamma1( C,R ) = gamma1( C,R ) * exp( -Knit1( c,r ) * dt ) 
         End If

! if MHp = 0.0 then there is no EPIC data for the grid cell or is not agricultural
         If( MHp .Eq. 0.0 .Or. frac_ag( C,R ) .Lt. 1.0e-3 ) Return
! Note that the kg to g and m3 to L units cancel 
         MV_air       = MWAIR / MET_DATA%DENS1( C,R )
! convert deposition from ppmv/s to mol/l/s
         NH3_flux     = flux_ag * 1.0e-6 / MV_air
!         Write(logdev,*) 'MV_air', MV_air
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
               gam = natgam
            End If
          Else ! evasion
            Kvs    = -log(1.0-NH3_flux/MNHx)
!            Write(logdev,*) 'Flux/soil NH4', NH3_flux/MNHx
! In epic the nitrification and evasion rates are added in a combined nitrification/evasion rate. 
! Then the emissions are assumed to be 5% of the total combined loss. Here we calcuate the nitrification/evasion rate as in EPIC and then the
! maximum rate is adjusted for the EPIC evasion because we will calculate the evasion seperately and add the rate to 
! EPIC nitrification. 
            Kn     = Knit/24.0/3600.0 + Kvs 
!            Write(logdev,*) 'Knit: ', Knit, 'Kvs: ', Kvs*24.0*3600.0
            If ( Frac_ag( c,r ) .Gt. 0.0 ) Then 
               If ( Kn .Gt. 0.0 ) Then
                  MNHx = MNHx * exp( -Kn * dt )
               End If
               gam = MNHx / MHp 
            Else
               gam = natgam
            End If
          End If
! Update soil concentrations
         If(zsoil .Eq. zsoil1 ) Then
            gamma1( C,R ) = gam 
         Else
            gamma2( C,R ) = gam
         End If

         If ( IsNaN( gam ) ) Then
            xmsg = 'NaN in Gamma Calculation'
            Write( logdev,* ) 'MNHx   :', MNHx
            Write( logdev,* ) 'gam    :', gam
            Write( logdev,* ) 'MHp    :', MHp
            Write( logdev,* ) 'wg     :', wg
            Write( logdev,* ) 'wres   :', GRID_DATA%WRES( c,r )
            Write( logdev,* ) 'wsat   :', GRID_DATA%WSAT( c,r )
            Write( logdev,* ) 'wfc    :', GRID_DATA%WFC( c,r )
            Write( logdev,* ) 'wwlt   :', GRID_DATA%WWLT( c,r )
            Write( logdev,* ) 'kvg    :', kvg
            Write( logdev,* ) 'kvs    :', kvs
            Write( logdev,* ) 'kn     :', kn
            Write( logdev,* ) 'zsoil  :', zsoil
            Write( logdev,* ) 'ag_tot :', ag_tot
            Call M3exit( pname, 0, 0, xmsg, xstat1)
         End If

         Return
         End Subroutine Calc_Nitrif

         Logical Function IsNaN( a ) ! required for Portland Group compiler (pgi)
            Real :: a
            If ( a .Eq. a ) Then   ! requires -Kieee flag for pgi
               IsNaN = .false.
            Else
               IsNaN = .true.
            End If
            Return
         End Function IsNaN  ! error, if pgf90

      End Module NH3_Bidi_Mod
