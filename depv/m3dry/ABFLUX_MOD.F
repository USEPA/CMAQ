
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
      Module ABFlux_Mod

! Contains the shared variables and subrountes needed for the bidirectional 
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
!---------------------------------------------------------------------------------------------

      Implicit None
! shared variables

! Private variables used in this module and subroutines  
      Real, Save, Allocatable, Private :: frac_ir( :,: ) ! irrigated fraction of ag    
      Real, Save, Allocatable, Private :: fagam1 ( :,: ) ! due to fertilizer app
      Real, Save, Allocatable, Private :: fagam2 ( :,: ) ! due to fertilizer app
      Real, Save, Allocatable, Private :: NH4ps1 ( :,:,: ) ! Initial Soil NH4
      Real, Save, Allocatable, Private :: NH4ps2 ( :,:,: ) ! Initial Soil NH4
      Real, Save, Allocatable, Private :: pHs1   ( :,:,: )
      Real, Save, Allocatable, Private :: pHs2   ( :,:,: )                 
      Real, Save,              Private :: C_gam           ! Canadian fertilizer facter      
      Real, Parameter,         Private :: maxgam   = 2.0e5 ! maximum soil gamm. It is assumed that any excess NH4 would from salts
      Real, Parameter,         Private :: natgam   = 20.0  ! Background soil gamma
      Integer, Save, Private      :: logdev       
     
      Contains 
         Subroutine Init_ABFlux( jdate, jtime)
 
         Use HGRD_DEFN           ! horizontal grid specifications
         Use UTILIO_DEFN         
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2

         Implicit None 
! Includes
         Include SUBST_CONST     ! constants
         Include SUBST_FILES_ID  ! file name parameters

! Local Variables

         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime   
         Integer                :: c,r,l,k
         Integer, Parameter :: e2c_cats = 42   ! number of crop catigories
         Integer            :: gxoff, gyoff    ! global origin offset from file
         integer, save      :: strtcol, endcol, strtrow, endrow
  
         Real, Save, Allocatable :: F_rate1 ( :,:,: ) ! fertlizer application to layer 1 
         Real, Save, Allocatable :: F_rate2 ( :,:,: ) ! fertlizer application to layer 2
         Real, Save, Allocatable :: Beld_ag ( :,:,: )        
         Real                    :: Tot_Ag          ! total ag in the grid cell    
	 Real, Parameter ::  convl1 = 7.142857e-04   ! ha/m**2 * mol/g N * 1/z_soil_l1 	 
         Real, Parameter ::  convl2 = 1.428571e-04   ! ha/m**2 * mol/g N * 1/z_soil_l2 
         Real            :: pHfac1
         Real            :: pHfac2
 
         Character( 16 ), Parameter :: pname = 'Init_ABFlux     ' 
         Character( 16 )            :: vname
         Character( 96 )            :: xmsg = ' '
         Character( 20 ), Save      :: Beld_Names(e2c_cats)
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
 
         logdev =  init3() 
 
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
         If ( .Not. Allocated ( F_rate1 ) ) Then
            Allocate ( F_rate1 ( my_ncols,my_nrows,e2c_cats ) )
         End If
         If ( .Not. Allocated ( F_rate2 ) ) Then
            Allocate ( F_rate2 ( my_ncols,my_nrows,e2c_cats ) )
         End If
         
         vname = 'L1_ANH3'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow, endrow,
     &                       strtcol, endcol, jdate, 0, F_rate1 ) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If
         vname = 'L2_ANH3'
         If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow, endrow,
     &                       strtcol, endcol, jdate, 0, F_rate2 ) ) Then
            Write( xmsg,9001 ) vname, E2C_FERT
            Go To 1001
         End If                 
! get BELD4 land use types time independent files. 
         If ( .Not. Open3( BELD4_LU, fsread3, pname ) ) Then
           xmsg = 'Could not open '// BELD4_LU // ' file'
           Call M3exit ( pname, jdate, jtime, xmsg, xstat1 )
         End If                
         If ( .Not. Allocated ( Beld_ag ) ) Then
            Allocate ( Beld_ag ( my_ncols,my_nrows,e2c_cats ) )
         End If
         Do k = 1, e2c_cats
            vname = BELD_Names(k)
            If ( .Not. Interpx ( BELD4_LU, vname, pname, strtcol, endcol,
     &                           strtrow, endrow, 1, 1, jdate, jtime,
     &                           Beld_ag( :,:,k ) ) ) Then   
               Write( xmsg,9001 ) vname, BELD4_LU
               Go To 1001
            End If
         End Do
         Beld_ag = 0.01 * Beld_ag   ! convert to fraction

! Allocate variable needed soil processes and fertilization
         If ( .Not. Allocated ( gamma1 ) ) Then
            Allocate ( gamma1 ( my_ncols,my_nrows ) )
         End If
         gamma1 = 0.0
         If ( .Not. Allocated ( gamma2 ) ) Then
            Allocate ( gamma2 ( my_ncols,my_nrows ) )
         End If
         gamma2 = 0.0
         If ( .Not. Allocated ( fagam1 ) ) Then
            Allocate ( fagam1 ( my_ncols,my_nrows ) )
         End If
         fagam1 = 0.0
         If ( .Not. Allocated ( fagam2 ) ) Then
            Allocate ( fagam2 ( my_ncols,my_nrows ) )
         End If
         fagam2 = 0.0
         If ( .Not. Allocated ( MHp1 ) ) Then
            Allocate ( MHp1 ( my_ncols,my_nrows ) )
         End If         
         MHp1 = 0.0
         If ( .Not. Allocated ( MHp2 ) ) Then
            Allocate ( MHp2 ( my_ncols,my_nrows ) )
         End If
         MHp2 = 0.0
         If ( .Not. Allocated ( frac_ir ) ) Then
            Allocate ( frac_ir ( my_ncols,my_nrows ) )
         End If
         frac_ir = 0.0

! get fertilizer from the previous month to estimate soil NH4+        
! time in the files is not the same
         If ( .Not. Open3( INIT_MEDC_1, fsread3, pname ) ) Then
            Write(Logdev,*) 'Estimating soil NHx from EPIC output'
            If ( .Not. Open3( E2C_FERT, fsread3, pname ) ) Then
               xmsg = 'Could not open '// E2C_FERT // ' file'
               Call M3exit ( pname, jdate, jtime, xmsg, xstat1 )
            End If
            If ( .Not. Allocated ( NH4ps1 ) ) Then
               Allocate ( NH4ps1 ( my_ncols,my_nrows,e2c_cats ) )
            End If
            NH4ps1 = 0.0
            If ( .Not. Allocated ( NH4ps2 ) ) Then
               Allocate ( NH4ps2 ( my_ncols,my_nrows,e2c_cats ) )
            End If
            NH4ps2 = 0.0  
            vname = 'L1_NH3'
            If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow, endrow,
     &                       strtcol, endcol, jdate, 0, NH4ps1) ) Then
               Write( xmsg,9001 ) vname, E2C_FERT
               Go To 1001
            End If
            vname = 'L2_NH3'
            If ( .Not. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow, endrow,
     &                       strtcol, endcol, jdate, 0, NH4ps2) ) Then
               Write( xmsg,9001 ) vname, E2C_FERT
               Go To 1001
            End If

         ! parameters            
            Do r = 1, my_nrows
               Do c = 1, my_ncols
                  Tot_Ag = 0.0                   
                  Do l = 1, e2c_cats
                     pHfac1 = 10.0 ** (-pHs1( c,r,l ) )
                     pHfac2 = 10.0 ** (-pHs2( c,r,l ) )
! convert units from kg/ha N to mol/l
                     If ( pHs1( c,r,l )   .Gt. 4.0       .And.
     &                    pHs2( c,r,l )   .Gt. 4.0       .And.
     &                    pHs1( c,r,l )   .Lt. 9.0       .And.
     &                    pHs2( c,r,l )   .Lt. 9.0       .And.
     &                    NH4ps1( c,r,l ) .Gt. 0.0       .And.
     &                    NH4ps2( c,r,l ) .Gt. 0.0       .And.
     &                    .Not. IsNaN( pHs1( c,r,l ) )   .And. 
     &                    .Not. IsNaN( pHs2( c,r,l ) )   .And.
     &                    .Not. IsNaN( NH4ps1( c,r,l ) ) .And.
     &                    .Not. IsNaN( NH4ps2( c,r,l ) ) .And.
     &                    Beld_ag( c,r,l ) .Gt. 0.0 ) Then
                        If ( Index( Beld_Names( l ), '_ir' ) .Gt. 0 ) Then
                           frac_ir( c,r ) = frac_ir( c,r ) + Beld_ag( c,r,l )
                        End If                    
                        Tot_Ag      = Tot_Ag + Beld_ag( c,r,l )
                        gamma1( c,r ) = gamma1( c,r ) + convl1 * NH4ps1( c,r,l )
     &                                / pHfac1 * Beld_ag( c,r,l )
                        MHp1( c,r )   = MHp1( c,r ) + Beld_ag( c,r,l ) * pHfac1
                        gamma2( c,r ) = gamma2( c,r ) + convl2 * NH4ps2( c,r,l )
     &                                / pHfac2 * Beld_ag( c,r,l )
                        MHp2( c,r )   = MHp2( c,r ) + Beld_ag( c,r,l ) * pHfac2
                        If ( .Not. IsNaN( F_rate1( c,r,l ) ) .And. 
     &                                    F_rate1( c,r,l ) .Gt. 0.0 ) Then
! Surface applied fertlizer                                        
                           fagam1( c,r ) = fagam1( c,r ) + F_rate1( c,r,l ) * Beld_ag( c,r,l )
     &                                   * convl1 / pHfac1
                        End If                                                                 
                        If ( .Not. IsNaN( F_rate2( c,r,l ) ) .And.
     &                                    F_rate2( c,r,l ) .Gt. 0.0 ) Then
! Injected fertilizer
                           fagam2( c,r ) = fagam2( c,r ) + F_rate2( c,r,l ) * Beld_ag( c,r,l )
     &                                   * convl2 / pHfac2
                        End If
                        If ( IsNaN( fagam1( c,r ) ) .Or. IsNaN( fagam2( c,r ) ) ) Then
                            xmsg = 'NaN in fagam Calculation'
                            Call M3exit( pname, jdate, jtime, xmsg, xstat1 )
                        End If     
                     End If                                
                  End Do ! e2c_cats
                  If ( Tot_Ag .ge. 1.0e-6 .And. ! ~ 1.5 ha per grid cell
     &                 MHp1( c,r ) / Tot_Ag .Gt. 1.0e-9 .And.
     &                 MHp1( c,r ) / Tot_Ag .Lt. 1.0e-4 .And. 
     &                 MHp2( c,r ) / Tot_Ag .Gt. 1.0e-9 .And.
     &                 MHp2( c,r ) / Tot_Ag .Lt. 1.0e-4 ) Then
! get the agricultrual only relevant number                                 
                     MHp1( c,r ) = MHp1( c,r ) / Tot_Ag
                     MHp2( c,r ) = MHp2( c,r ) / Tot_Ag         
                     gamma1( c,r ) = ( gamma1( c,r ) + fagam1( c,r ) ) / Tot_Ag
                     gamma2( c,r ) = ( gamma2( c,r ) + fagam2( c,r ) ) / Tot_Ag  
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
!           xmsg = 'NaN in fagam Calculation' ! JOB debugging
!           Call M3exit( pname, jdate, jtime, xmsg, xstat1) ! JOB debuggin

         Else ! read          
! for whatever reason the jdate and jtime get messed up by the e2c files
            Write( Logdev,* ) 'Reading soil NHx from Previous CMAQ run'         

            vname = 'Gamma1'
            If ( .Not. Xtract3 ( INIT_MEDC_1, vname, 1, 1, strtrow, endrow,
     &                           strtcol, endcol, jdate, 000000, gamma1 ) ) Then
               Write( xmsg,9001 ) vname, INIT_MEDC_1
               Go To 1001
            End If
            vname = 'Gamma2'
            If ( .Not. Xtract3 ( INIT_MEDC_1, vname, 1, 1, strtrow, endrow,
     &                           strtcol, endcol, jdate, 000000, gamma2 ) ) Then
               Write( xmsg,9001 ) vname, INIT_MEDC_1
               Go To 1001
            End If
            vname = 'MHpsl1'
            If ( .Not. Xtract3 ( INIT_MEDC_1, vname, 1, 1, strtrow, endrow,
     &                           strtcol, endcol, jdate, 000000, MHp1 ) ) Then
               Write( xmsg,9001 ) vname, INIT_MEDC_1
               Go To 1001
            End If
            vname = 'MHpsl2'
            If ( .Not. Xtract3 ( INIT_MEDC_1, vname, 1, 1, strtrow, endrow,
     &                           strtcol, endcol, jdate, 000000, MHp2 ) ) Then
               Write( xmsg,9001 ) vname, INIT_MEDC_1
               Go To 1001
            End If

            do r = 1, my_nrows
               Do c = 1, my_ncols
                  Tot_Ag = 0.0 
                  Do l = 1, e2c_cats
                     pHfac1 = 10.0 ** (-pHs1( c,r,l ) )
                     pHfac2 = 10.0 ** (-pHs2( c,r,l ) )
! convert units from kg/ha N to mol/l 
                     If ( Index( Beld_Names( l ), '_ir' ) .Gt. 0 ) Then
                        frac_ir( c,r ) = frac_ir( c,r ) + Beld_ag( c,r,l )
                     End If
                     Tot_Ag = Tot_Ag + Beld_ag( c,r,l )
                     If ( .Not. IsNaN( F_rate1( c,r,l ) ) .And. 
     &                     F_rate1( c,r,l ) .Gt. 0.0      .And.
     &                    .Not. IsNaN( pHs1( c,r,l ) )    .And.  
     &                    pHs1( c,r,l ) .Gt. 4.0          .And.
     &                    pHs1( c,r,l ) .Lt. 9.0 ) Then
! surface applied fertilizer                        
                         fagam1( c,r ) = fagam1( c,r ) + F_rate1( c,r,l ) * Beld_ag( c,r,l )
     &                                 * convl1 / pHfac1
                     End If
                     If ( .Not. IsNaN( F_rate2( c,r,l ) ) .And. 
     &                     F_rate2( c,r,l ) .Gt. 0.0      .And. 
     &                    .Not. IsNaN( pHs2( c,r,l ) )    .And. 
     &                    pHs2( c,r,l ) .Gt. 4.0          .And. 
     &                    pHs2( c,r,l ) .Lt. 9.0 ) Then
! Injected fertilizer                        
                         fagam2( c,r ) = fagam2( c,r ) + F_rate2( c,r,l ) * Beld_ag( c,r,l )
     &                                 * convl2 / pHfac2
                     End If
                     If ( IsNaN( fagam1( c,r ) ) .Or. IsNaN( fagam2( c,r ) ) ) Then
                        xmsg = 'NaN in fagam Calculation'
                        Call M3exit( pname, jdate, jtime, xmsg, xstat1)
                     End If                         
                  End Do ! e2c_cats 
! Agricultural only numbers were written to file                  
                  If ( Tot_Ag .Gt. 1.0e-6  .And.
     &                 MHp1( c,r ) .Gt. 1.0e-9 .And.
     &                 MHp1( c,r ) .Lt. 1.0e-5 .And. 
     &                 MHp2( c,r ) .Gt. 1.0e-9 .And.
     &                 MHp2( c,r ) .Lt. 1.0e-5 ) Then
! get the agricultrual only relevant number                                 
                     gamma1( c,r ) = gamma1( c,r ) + fagam1( c,r ) / Tot_Ag
                     gamma2( c,r ) = gamma2( c,r ) + fagam2( c,r ) / Tot_Ag   
                     If ( IsNaN( gamma1( c,r ) ) .Or. IsNaN( gamma2( c,r ) ) .Or. 
     &                    IsNaN( MHp2( c,r ) ) .Or. IsNaN( MHp2( c,r ) )) Then
                        xmsg = 'NaN in grid cell Gamma Calculation'
                        Call M3exit( pname, jdate, jtime, xmsg, xstat1 )
                     End If   
                  End If
               End Do ! c
            End Do ! r
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
         
         End Subroutine Init_ABFlux
                  
!------------------------------------------------------------------------------
! Subroutine to get the soil and canopy compensation point
!------------------------------------------------------------------------------          
         Subroutine Get_Flux( tempg, rh_air, cNH3, rwetsfc, rgw, wg, w2, Sltype, dif, 
     &                        row, col, sp, pvd, lnh3, f_stom, f_cut, f_soil, f_emis,
     &                        f_dep, f_ag, f_nag, f_wat, dt, jtime )
         
         Use UTILIO_DEFN
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2, lufrac
         Use Mosaic_Mod
         Use LSM_MOD
         
         Implicit None
         
         Include SUBST_CONST     ! constants
         Include SUBST_FILES_ID  ! file name parameters
         
         Real,    Intent( IN )  :: tempg        
         Real,    Intent( IN )  :: rh_air         
         Real,    Intent( IN )  :: cNH3
         Real,    Intent( IN )  :: rwetsfc        
         Real,    Intent( IN )  :: rgw
         Real,    Intent( IN )  :: wg
         Real,    Intent( IN )  :: w2
         Real,    Intent( IN )  :: Sltype                
         Real,    Intent( IN )  :: dif        

         Integer, Intent( IN )  :: row
         Integer, Intent( IN )  :: col
         Integer, Intent( IN )  :: sp ! species index
         
         Real,    Intent( OUT ) :: pvd
         Real,    Intent( OUT ) :: lnh3
         Real,    Intent( OUT ) :: f_stom
         Real,    Intent( OUT ) :: f_cut
         Real,    Intent( OUT ) :: f_soil
         Real,    Intent( OUT ) :: f_emis
         Real,    Intent( OUT ) :: f_dep
         Real,    Intent( OUT ) :: f_ag
         Real,    Intent( OUT ) :: f_nag
         Real,    Intent( OUT ) :: f_wat         

         Integer, Intent( IN )  :: dt
         Integer, Intent( IN )  :: jtime
         
         Real,        Parameter :: kvis = 0.132   ! [cm**2/s] at 273.15K
         Real( 8 )              :: aq           ! Quadradic equation variable
         Real( 8 )              :: bq           ! Quadradic equation variable
         Real                   :: cnh3c        ! In canopy NH3 concentration [ppm]
         Real                   :: cnh3g1, cnh3g2   ! NH3 compensation concentration for ground [ppm]
         Real                   :: cnh3s        ! NH3 compensation concentration for stomatal [ppm]
         Real( 8 )              :: cq           ! Quadradic equation variable
         Real                   :: del0         ! for Rbg
         Real( 8 )              :: ga           ! Ga = 1/Ra [m/s]
         Real                   :: gammas       ! [NH4+]/[H+]
         Real( 8 )              :: gcw
         Real( 8 )              :: gg1( n_lufrac ) ! Gg = 1/(Rgnd(nh3)+Rinc) [m/s]  
         Real( 8 )              :: gg2( n_lufrac ) ! Gg = 1/(Rgnd(nh3)+Rinc) [m/s]
         Real( 8 )              :: gsb          ! Gsb = 1/(Rstom(nh3)+Rb(nh3)) [m/s]
         Real( 8 )              :: gt
         Real,        Parameter :: karman = 0.4 ! von Karman constant
         Real( 8 )              :: qq           ! intermediate variable
         Real                   :: rbgj( n_lufrac )
!        Real,        Parameter :: rwm = 35.0   ! Minimum NH3 cuticle resistance [s/m]
         Real,        Parameter :: rwm = 20.0   ! Minimum NH3 cuticle resistance [s/m]
         Real                   :: rwmb         ! Rwmb = Rwm + Rb
         Real                   :: rwx          ! Rw = Rwm + Rwx * CNH3C [s/m]
         Real                   :: scn          ! for Rbg
         Real                   :: ustg         ! for Rbg
         Real                   :: vdg          ! Vd(nh3) to non-veg part [m/s]
         Real                   :: wg_ir        ! 1 cm soil moisture  
         Real                   :: w5cm         ! soil moisture in top 5 cm (vol frc)
         Real,        Parameter :: d1 = 0.01    ! Top soil layer depth (1 cm)
         Real,        Parameter :: d2 = 0.05    ! Bottom soil layer mid depth (5 cm)
	 Real,        Parameter :: twothree = 2.0/3.0
	 Real,        Parameter :: onethree = 1.0/3.0
	 Real,        Parameter :: MolN     = 14.007  ! g/mol N
	 Real,        Parameter :: MolNH3   = 17.01  ! g/mol NH3
         Real( 8 )              :: ldry
         Real( 8 )              :: dp
         Real( 8 )              :: rsoil1
         Real( 8 )              :: rsoil2
         Real( 8 )              :: a1
         Real( 8 )              :: agfrac
         Real( 8 )              :: sqrr
         Real( 8 )              :: ddd
         Real( 8 )              :: watfrac ! water fraction
         Real                   :: xxx          ! lumped variable
         Real,        Parameter :: f_emis_lim = -1.0e-07 ! limit to report to log: if below, probably due to numerical precision error
         Integer                :: k,j
         Integer                :: ist

         logdev = init3()
!> Compute quasi-laminar boundary layer resistance at the soil surface

         scn  = kvis / dif
         Do j = 1, n_lufrac
            ustg = max( ustarj( j,col,row ) * EXP( -laij( j,col,row ) ), 0.001 )         
            del0 = 1.0E-4 * kvis / ( karman * ustg )
            rbgj( j ) = ( scn - LOG( 10.0 * del0 ) ) / ( karman * ustg )
         End Do
                        
!> Compute soil resistance
                     
         ist = nint( Sltype )
         
         If ( ist .lt. 1 .or. ist .gt. 11 ) Then
            ist = 4
         End If
         
         If ( frac_ir( col,row ) .Gt. 0.0 .And. wg .LT. 0.75 * wfc( ist ) ) Then            
            wg_ir = ( 1.0 - frac_ir( col,row ) ) * wg + frac_ir( col,row ) * 0.75 * wfc( ist )
         Else
            wg_ir = wg
         End If
 
         wg_ir = Min( wg_ir,wsat( ist ) )
         wg_ir = Max( wg_ir,wres( ist ) )
         
         ldry= d1 * ( Exp( ( 1.0 - wg_ir / wsat( ist ) ) ** 5 ) - 1.0 ) / 1.718
         
         dp  = dif * 1.0E-4 * wsat( ist ) * wsat( ist )
     &       * ( 1.0 - wres( ist ) / wsat( ist ) ) ** ( 2.0 + 3.0 / bslp( ist ) )
         
         rsoil1=ldry/dp
         
         w5cm = 0.2 * wg_ir + 0.8 * w2
         w5cm = Min( w5cm, wsat( ist ) )
         w5cm = Max( w5cm, wres( ist ) )
         
         If ( frac_ir( col,row ) .Gt. 0.0 .And. w5cm .LT. 0.75 * wfc( ist ) ) Then
            w5cm = ( 1.0 - frac_ir( col,row ) ) * w5cm + frac_ir( col,row ) * 0.75 * wfc( ist )
         End If
         
         ldry = d2 * ( Exp( ( 1.0 - w5cm / wsat( ist ) ) ** 5 ) - 1.0 ) / 1.718
         rsoil2 = ldry / dp
         
         If ( Land_Scheme .Eq. 'USGS24' ) Then
            agfrac  = lufrac(  2,col,row ) +      lufrac(  3,col,row )
     &              + lufrac(  4,col,row ) +
     &                twothree * lufrac(  5,col,row ) + twothree * lufrac(  6,col,row )
            watfrac = lufrac( 16,col,row )  
         End If
         
         If ( Land_Scheme .Eq. 'NLCD50' ) Then
            agfrac  = lufrac( 19,col,row ) +            lufrac( 20,col,row )
     &              + lufrac( 43,col,row ) + twothree * lufrac( 45,col,row )
            watfrac = lufrac(  1,col,row ) +            lufrac( 31,col,row )
     &              + lufrac( 48,col,row )         
         End If

         If ( Land_Scheme .Eq. 'NLCD40' ) Then
            agfrac  = lufrac( 12,col,row ) +            lufrac( 37,col,row )
     &              + lufrac( 38,col,row ) + twothree * lufrac( 14,col,row )
            watfrac = lufrac( 17,col,row ) +            lufrac( 21,col,row )
         End If
         
         If ( Land_Scheme .Eq. 'MODIS' ) Then
            agfrac  = lufrac( 12,col,row ) + twothree * lufrac( 14,col,row )
            watfrac = lufrac( 17,col,row )  
         End If
         
         pvd    = 0.0
         lnh3   = 0.0
         f_emis = 0.0
         f_dep  = 0.0
         f_stom = 0.0
         f_cut  = 0.0
         f_soil = 0.0
         f_ag   = 0.0
         f_nag  = 0.0
         f_wat  = 0.0
         
         Do j = 1, n_lufrac
            If ( lufrac( j,col,row ) .Gt. 0.0 ) Then

!> If the soil is frozen assume not evasive flux and skip calculation of comp. points         
               If ( Tempg .Le. 273.15 ) Then
                  cnh3s  = 0.0
                  cnh3g1 = 0.0
                  cnh3g2 = 0.0
                  Go To 101
               End If

!> Compute compensation point. gamma is specified according to the amount of 
!> cultivated vegetation               
               gammas = luf_fac_p( j )
               a1    = 161512.0d0 / real( tempg, 8 )
     &               * 10.0d0 ** ( -4507.11d0 / real( tempg, 8 ) )
               a1    = a1 * 24.5d0 * 1.0d6  ! ppm
               cnh3s = a1 * gammas       
         
!> Set a maximum [NH4]/[H+] ratio at 200,000 based on output from the AIM aerosol
!> model any [NH4] in excess of this ratio is assumed to partition into the solid
!> phase. Canada soil gamma taken from Zhang et al 2010 JGR Table 5
               Select Case( cat_lu( j ) )

                  Case( 'WATER','SNOWICE' ) ! water, ice or snow

                     cnh3g1 = 0.0

                     cnh3g2 = 0.0
     
                  Case( 'AG' ) ! Ag                

                     cnh3g1 = Min( Max( gamma1( col,row ) / wg_ir, C_gam / wg_ir ),
     &                                  maxgam)

                     cnh3g2 = Min( Max( gamma2( col,row ) / w5cm, C_gam / w5cm), 
     &                                  maxgam )                     

                     cnh3s  = Max( cnh3s, a1 *( Max(gamma2(col,row),C_gam) * 1.0e-7 * w5cm * MolN * d2 *
     &                             1.0e4 * 12.3 + 20.3 ) )
     
                  Case( 'AGMOS' ) ! Ag mosaic 67% ag 33% mosaic		  

                     cnh3g1 = Min( Max( gamma1( col,row ) / wg_ir, C_gam / wg_ir) *
     &                        twothree + onethree * natgam / wg_ir , maxgam )

                     cnh3g2 = Min( Max( gamma2( col,row ) / w5cm, C_gam / w5cm ) * 
     &                       twothree + onethree * natgam / w5cm, maxgam )

                     cnh3s  = Max( cnh3s,a1*( Max(gamma2(col,row),C_gam) * 1.0e-7 * 
     &                             w5cm * MolN * d2 *1.0e4 * 12.3 + 20.3 ) ) * 
     &                             twothree + onethree * cnh3s

                  Case Default ! not ag

                     cnh3g1 = Min( natgam / wg_ir, maxgam )

                     cnh3g2 = Min( natgam / w5cm, maxgam )

               End Select                      

               cnh3g1 = a1 * Max(cnh3g1,0.01)         
               cnh3g2 = a1 * Max(cnh3g2,0.01)

101            Continue

!> Cuticle resistance : rw = rwx * cnh3c + rwm
               If ( laij( j,col,row ) .Gt. 0.0 ) Then

                  rwx   = rwetsfc * MolNH3 * 1000.0 / 24.5  ! in ppb/ cso2c 		  
                  rwmb  = rwm * Max( 1.0, 0.5 * ( 100.0-rh_air ) ) + rbcj( j )
                  ga    = 1.0 / ( raj( j,col,row ) + 0.5 * rincj( j,col,row ) ) 
                  gsb   = 1.0 / ( rstomj( j,sp,col,row ) + rbcj( j ) )
                  gg1( j )= 1.0 / ( rbgj( j ) + 0.5 * rincj( j,col,row ) + rsoil1 )
                  gg2( j )= 1.0 / ( rbgj( j ) + 0.5 * rincj( j,col,row ) + rsoil2 )
                  gcw   = laij( j,col,row ) / ( rbcj( j ) + rwetsfc )
                  gt    = gsb + gg1( j ) + gg2( j ) + ga + deltaj( j,col,row ) * gcw
                  qq    = ga * cnh3 + gsb * cnh3s + gg1( j ) * cnh3g1 + gg2( j ) * cnh3g2
                  aq    = rwx * gt
                  bq    = rwmb * gt + laij( j,col,row ) * ( 1.0 - deltaj( j,col,row ) ) - rwx * qq
                  cq    = -rwmb * qq
                  sqrr = -0.5d0 * ( bq + sign( 1.0d0,bq ) * sqrt( bq * bq - 4.0d0 * aq * cq ) )
!> find the positive root
                  If ( sqrr .Ge. 0.0 ) Then
                     cnh3c = sqrr/aq
                  Else ! because cq is always negative
                     cnh3c = cq/sqrr
                  End If
! if the compensation point less than zero reset it to zero
                  If ( cnh3c .Lt. 0.0 ) Then
                     cnh3c = 0.0
                  End If

                  ddd = ( 1.0 - deltaj( j,col,row ) ) / rgndcj( j,col,row ) + 
     &                  deltaj( j,col,row ) / rgw 

                  vdg = 1.0 / ( raj( j,col,row ) + rbcj( j ) + 1.0 / ddd )
               End If ! laij > 0.0            

!> Integrate over the land uses        
               Select Case( cat_lu( j ) )
                  Case( 'WATER','SNOWICE' ) ! no vegetation
                     lnh3 = lnh3 + lufrac( j,col,row ) / ( rsurfj( j,sp,col,row ) + 
     &                                                     raj( j,col,row ) )
                  Case Default     ! vegetation
                     pvd  = pvd + lufrac( j,col,row ) * cnh3c * ga * vegj( j,col,row )
                     lnh3 = lnh3 + lufrac( j,col,row ) * ( ( 1.0 - vegj( j,col,row ) ) * 
     &                      vdg + vegj( j,col,row ) * ga )
               End Select

!> -------Compute Fluxes  and integrate over LU for output JOB -------------------
               Select Case( cat_lu( j ) )
                  Case( 'WATER' ) ! water
                     f_dep = f_dep + lufrac( j,col,row ) / ( rsurfj( j,sp,col,row )+raj( j,col,row ) ) * cnh3
                     f_wat = f_wat + lufrac( j,col,row ) / ( rsurfj( j,sp,col,row )+raj( j,col,row ) ) * cnh3 
     &                     / watfrac
                  Case( 'SNOWICE' ) ! no veg
!> Zero out the compensation points for deposition
                     xxx = rwx * ga * cnh3 - rwmb * gt - laij( j,col,row ) * ( 1.0 - deltaj( j,col,row ) )
                     f_dep = f_dep + lufrac( j,col,row ) * ( vegj( j,col,row ) * ga
     &                     * ( cnh3 - ( xxx + Sqrt( xxx**2 + 4.0 * aq * rwmb * ga * cnh3 ) )
     &                     / ( 2.0 * aq ) ) + ( 1.0 - vegj( j,col,row ) ) * vdg * cnh3 )
                  Case( 'AG','AGMOS' ) ! agriculture
!> Zero out the atmospheric NH3 concentration for emissions
                     xxx = rwx * ( qq - ga*cnh3 ) - rwmb * gt - laij( j,col,row ) * ( 1.0 - deltaj( j,col,row ) )
                     f_emis = f_emis + lufrac( j,col,row ) * vegj( j,col,row ) * ga * ( xxx
     &                   + Sqrt( xxx**2 + 4.0 * aq * rwmb * ( qq - ga * cnh3 ) ) ) / ( 2.0 * aq )
!> Zero out the compensation points for deposition
                     xxx = rwx * ga * cnh3 - rwmb * gt - laij( j,col,row ) * ( 1.0 - deltaj( j,col,row ) )
                     f_dep = f_dep + lufrac( j,col,row ) * ( vegj( j,col,row ) * ga
     &                     * ( cnh3 - ( xxx + Sqrt( xxx**2 + 4.0 * aq * rwmb * ga * cnh3 ) )
     &                     / ( 2.0 * aq ) ) + ( 1.0 - vegj( j,col,row ) ) * vdg * cnh3 )
!> Components of the flux for stomatal, cuticular and soil surfaces
                     f_stom = f_stom + lufrac( j,col,row ) * vegj( j,col,row ) * ( cnh3s - cnh3c ) * gsb ! component 1 stomatal
                     f_cut = f_cut + lufrac( j,col,row ) * ( - cnh3c * vegj( j,col,row ) * laij( j,col,row )
     &                     * ( deltaj( j,col,row ) / ( rbcj( j ) + rwetsfc )    ! wet cuticle
     &                     + ( 1.0 - deltaj( j,col,row ) ) / ( rwmb + rwx*cnh3c ) ) )  ! dry cuticle
                     f_soil = f_soil + lufrac( j,col,row ) * ( vegj( j,col,row ) * ( (cnh3g1 - cnh3c) * gg1( j )  ! soil layer 1
     &                      + ( cnh3g2 - cnh3c ) * gg2( j ) ) )    ! soil layer 2                                                  
                     f_ag = f_ag + lufrac( j,col,row ) * ( vegj( j,col,row ) * ga * ( cnh3c - cnh3 )
     &                    -( 1.0 - vegj( j,col,row ) ) * vdg * cnh3 ) / agfrac
                     If ( f_emis .lt. f_emis_lim ) Then
                        Write(logdev,*) 'ag f_emis: ', f_emis
                        Write(logdev,*) 'ag f_ag  : ', f_ag
                        Write(logdev,*) 'ag f_dep : ', f_dep
                        Write(logdev,*) 'ag f_stom: ', f_stom
                        Write(logdev,*) 'ag f_cut : ', f_cut
                        Write(logdev,*) 'ag f_soil: ', f_soil
                        Write(logdev,*) 'ag lufrac: ', lufrac(j,col,row)
                        Write(logdev,*) 'ag vegj  : ', vegj(j,col,row)
                        Write(logdev,*) 'ag ga    : ', ga
                        Write(logdev,*) 'ag vdg   : ', vdg
                        Write(logdev,*) 'ag agfrac: ', agfrac
                     End If
                  Case Default ! natural 
!> Zero out the atmospheric NH3 concentration for emissions
                     xxx = rwx * ( qq - ga * cnh3 ) - rwmb * gt - laij( j,col,row ) * ( 1.0 - deltaj( j,col,row ) )
                     f_emis = f_emis + lufrac( j,col,row ) * vegj( j,col,row ) * ga * ( xxx
     &                      + Sqrt( xxx**2 + 4.0 * aq * rwmb * ( qq - ga * cnh3 ) ) ) / ( 2.0 * aq )
!> Zero out the compensation points for deposition
                     xxx = rwx * ga * cnh3 - rwmb * gt - laij( j,col,row ) * ( 1.0 - deltaj( j,col,row ) )
                     f_dep = f_dep + lufrac( j,col,row ) * ( vegj( j,col,row ) * ga
     &                     * ( cnh3 - ( xxx + Sqrt( xxx**2 + 4.0 * aq * rwmb * ga * cnh3 ) )
     &                     / ( 2.0 * aq ) ) + ( 1.0 - vegj( j,col,row ) ) * vdg * cnh3 )
!> Components of the flux for stomatal, cuticular and soil surfaces
                     f_stom = f_stom + lufrac( j,col,row ) * vegj( j,col,row ) * ( cnh3s - cnh3c ) * gsb ! component 1 stomatal
                     f_cut  = f_cut + lufrac( j,col,row ) * ( - cnh3c * vegj( j,col,row ) * laij( j,col,row )
     &                      * ( deltaj( j,col,row ) / ( rbcj( j ) + rwetsfc )    ! wet cuticle
     &                      + ( 1.0 - deltaj( j,col,row ) ) / ( rwmb + rwx * cnh3c ) ) ) ! dry cuticle
                     f_soil = f_soil + lufrac( j,col,row ) * ( vegj( j,col,row ) * ( ( cnh3g1 - cnh3c ) * gg1( j ) +  ! soil layer 1
     &                        ( cnh3g2 - cnh3c ) * gg2( j )) )  ! soil layer 2                        
                     f_nag = f_nag + lufrac( j,col,row ) * ( vegj( j,col,row ) * ga * ( cnh3c- cnh3 ) - 
     &                     ( 1.0 - vegj( j,col,row ) ) * vdg * cnh3 ) / (1.0-agfrac-watfrac)
                     If ( f_emis .lt. f_emis_lim ) Then
                        Write(logdev,*) 'f_emis: ', f_emis
                        Write(logdev,*) 'f_nag : ', f_nag
                        Write(logdev,*) 'f_dep : ', f_dep
                        Write(logdev,*) 'f_stom: ', f_stom
                        Write(logdev,*) 'f_cut : ', f_cut
                        Write(logdev,*) 'f_soil: ', f_soil
                        Write(logdev,*) 'lufrac: ', lufrac(j,col,row)
                        Write(logdev,*) 'vegj  : ', vegj(j,col,row)
                        Write(logdev,*) 'ga    : ', ga
                        Write(logdev,*) 'vdg   : ', vdg
                        Write(logdev,*) 'agfrac: ', agfrac
                     End If
               End Select
            End If ! Lufrac > 0.0
         End Do ! n_lufrac         
         
!> -----Compute nitrification rates for agricultural areas only where there is NHx and H+   
         If ( gamma1( col,row ) .Gt. 0.0 .And. MHp1( col,row ) .Gt. 1.0e-9 .And.
     &        MHp1( col,row ) .Lt. 1.0e-5 ) Then  
            Call Calc_Nitrif ( Tempg,wg_ir,Cnh3c,gg1,gamma1(col,row),ist,dt, MHp1(col,row),col,row,d1 )
         End If
         If ( gamma2( col,row ) .Gt. 0.0 .And. MHp2( col,row ) .Gt. 1.0e-9 .And.
     &        MHp2( col,row ) .Lt. 1.0e-5 ) Then  
            Call Calc_Nitrif ( Tempg,w5cm,Cnh3c,gg2,gamma2(col,row),ist,dt,MHp2(col,row),col,row,d2 )
         End If

         Return         
         End Subroutine Get_Flux
!------------------------------------------------------------------------------
! Subroutine to update the soil ammonia and pH due to evasion, deposition, 
! nitrification, leaching, and run off
! Soil ammonium, pH, leaching, and run off are defined in the module
!------------------------------------------------------------------------------
         Subroutine Calc_Nitrif ( Tempg, wg, CNH3c, gg, gam, ist, dt, MHp, C, R, ds )  
         
         Use UTILIO_DEFN
         Use Bidi_Mod, Only: lufrac
         Use LSM_MOD
         
         Implicit None                
         
         Real,      Intent( IN )    :: Tempg     ! Soil Temperature
         Real,      Intent( IN )    :: wg        ! Soil Moisture
         Real,      Intent( IN )    :: CNH3c     ! Canopy compensation point
         Real,      Intent( IN )    :: MHp       ! molar H concentration
         Real( 8 ), Intent( IN )    :: gg(n_lufrac) ! soil-air conductance
         Real,      Intent( IN )    :: ds          ! soil depth in m
         Real,      Intent( INOUT ) :: gam        ! updated gamma
         Integer,   Intent( IN )    :: ist    ! Soil Type
         Integer,   Intent( IN )    :: dt     ! time step in s
         Integer,   Intent( IN )    :: C      ! Column
         Integer,   Intent( IN )    :: R      ! Row

         Real    :: fTg    ! nitrification temperature factor
         Real    :: fSWg   ! nitrification Soil Water factor
         Real    :: fpHg   ! nitrification pH factor 
         Real    :: Kn     ! nitrification rate 1/s
         Real    :: Kvs    ! Air-soil exchange rate for aqueous NH4+
         Real    :: kvg    ! Air-soil exchange rate for gas phase NH3
         Real    :: MNH3   ! molar atmospheric NH3 conc.
         Real    :: MNHx   ! molar soil water NH4+ + NH3 conc.
         Real    :: wg25   ! wilting point +0.25*(field capacity-wilting point)
         Real    :: pHsl   ! Soil pH
         Real    :: ag_tot ! total 
         Integer :: i   ! loop counter
         CHARACTER( 96 )            :: xmsg = ' '
         Character( 16 ), Parameter :: pname = 'Calc_Nitrif' 

! convert the ambient concentrations from ppm to mol/l
         MNH3 = CNH3c / 24.45 * 1.0e-6

! Get the soil pH         
         pHsl = -log10(MHp)

! get NH4+ from gamma ([NH4+]/[H+] with units in mol/l)         
         MNHx = min(gam/wg,2.0e5)*MHp

! Estimate the soil nitrification rate following Reddy et al 1979 starting with
! the temperature parameter         
         fTg = max( 0.041 * ( Tempg - 278.15 ), 0.0)
 
! Estimate the soil water parameter
         wg25 = wwlt( ist ) + 0.25 * ( wfc( ist ) - wwlt( ist ) )
         If ( wg .Le. wwlt( ist ) ) fSWg = 0.1
         If ( wg .Lt. wg25 .And. wg .Gt. wwlt( ist ) ) Then
            fSWg = max( 0.1, 0.1 + 0.9 * Sqrt( ( wg - wwlt( ist ) ) / 
     &                ( wfc( ist ) - wwlt( ist ) ) ), ( wg - wwlt( ist ) ) / 
     &                ( 0.25 * ( wfc( ist ) - wwlt( ist ) ) ) )
         End If
         If ( wg .Ge. wg25 .And. wg .Lt. wfc( ist ) ) fSWg = 1.0

         If ( wg .Ge. wfc( ist ) ) Then
            fSWg = max( 0.1, 1.0 - ( wg - wfc( ist ) ) / ( wsat( ist )-wfc( ist ) ) )
         End If
! Estimate the pH factor
         If ( pHsl .Lt. 7.0 ) fpHg = 0.307 * pHsl - 1.269
         If ( pHsl .Ge. 7.0 .And. pHsl .Lt. 7.4 ) fpHg = 1.0
         If ( pHsl .Ge. 7.4) fpHg = 5.367 - 0.599 * pHsl

         Kn = fTg * fSWg * fpHg / 24.0 / 3600.0  ! Kn is in 1/d in EPIC
! Estimate the soil evasion rate
         kvg    = 0.0
         Kvs    = 0.0
         ag_tot = 0.0
         Do i = 1, n_lufrac
            If ( lufrac(i,c,r) .Gt. 0.0 ) Then
               Select Case( cat_lu( i ) )
                  Case('AG','AGMOS' )
                     kvg = kvg + gg( i ) / ( wg * real( ds, 8 ) ) * lufrac( i,c,r )
                     Kvs = Kvs + ( 161512.0d0 / real( tempg, 8 )
     &                         * 10.0d0**( -4507.11d0 / real( tempg, 8 ) ) )
     &                         * gg( i ) / ( wg * real( ds, 8 ) ) / mhp * lufrac( i,c,r )
                     ag_tot = ag_tot + lufrac( i,c,r )
               End Select
            End If
         End Do
         If ( ag_tot .Gt. 0.0 ) Then
            kvg = kvg / ag_tot
            Kvs = Kvs / ag_tot
            MNHx = kvg * MNH3 / ( Kvs + Kn ) + ( MNHx - kvg * MNH3 / ( Kvs + Kn ) ) * 
     &             exp( - ( Kvs + Kn ) * dt )
            gam = MNHx / MHp * wg + max( gam / wg - maxgam , 0.0 ) * wg
         Else
            gam = natgam
         End If

         If ( IsNaN(gam) ) Then
            xmsg = 'NaN in Gamma Calculation'
            Write(logdev,*) 'MNHx   :', MNHx
            Write(logdev,*) 'gam    :', gam
            Write(logdev,*) 'MHp    :', MHp
            Write(logdev,*) 'ag_tot :', ag_tot
            Call M3exit( pname, 0, 0, xmsg, xstat1)
         End If

         Return
         End Subroutine Calc_Nitrif

         Logical Function IsNaN( a ) ! required for Portland Group compiler (pgi)
            Real :: a
            If ( a .Eq. a ) Then   ! requires -Kieee flag for pgi
!               write( *,* ) '==IsNaN false: ', a
               IsNaN = .false.
            Else
!               write( *,* ) '==IsNaN true: ', a
               IsNaN = .true.
            End If
            Return
         End Function IsNaN  ! error, if pgf90
!        End  ! error, if ifort

      End Module ABFlux_Mod
