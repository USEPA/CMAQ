
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
      Module ABFlux_Mod

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
!                   J. Pleim   Dec   2016:  Revised bidi using gamma from EPIC every day with no mosaic
!                   J. Pleim   April 2018:  Estimate available NH3 frac according to EPIC
!                   D. Wong    Feb.  1 19:  Implemented centralized I/O approach, removed all MY_N
!                                           clauses, and created a new module depv_data_module 
!                                           (model_data_module.F) to hold some of information
!                                           originally stored here to avoid cyclic dependence
C-------------------------------------------------------------------------------

      Implicit None
! shared variables

      Real, Save, Allocatable :: fcec1  ( :,: ) ! Volatility function of CEC from EPIC (Williams 1995) in L1
      Real, Save, Allocatable :: fcec2  ( :,: ) ! Volatility function of CEC from EPIC (Williams 1995) in L2
      
      Real, Save,              Private :: C_gam           ! Canadian fertilizer facter      
      Real, Parameter,         Private :: maxgam   = 1.0e6 ! removed 200000 limit since I can't find any justification
      Real, Parameter,         Private :: natgam   = 20.0  ! Background soil gamma
     
      Contains 
         Subroutine Init_ABFlux( jdate, jtime)
 
         Use HGRD_DEFN           ! horizontal grid specifications
         Use UTILIO_DEFN         
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2
         Use depv_data_module
         Use, intrinsic :: ieee_arithmetic, only: isnan => ieee_is_nan

         Implicit None 
! Includes
         Include SUBST_CONST     ! constants
         Include SUBST_FILES_ID  ! file name parameters

! Local Variables

         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime   
         Integer                :: c,r,l,k
         Integer            :: gxoff, gyoff    ! global origin offset from file
         integer            :: strtcol, endcol, strtrow, endrow
         integer            :: strtcol_medi,  endcol_medi,  strtrow_medi,  endrow_medi
         integer            :: strtcol_fert,  endcol_fert,  strtrow_fert,  endrow_fert
         integer            :: strtcol_beld,  endcol_beld,  strtrow_beld,  endrow_beld
  
         Real                    :: Tot_Ag          ! total ag in the grid cell    
         Real, Parameter :: conv   = 7.142857e-06   ! ha/m**2 * mol/g N 
         Real            :: pHfac1
         Real            :: pHfac2
         Real, Parameter :: d1 = 0.01    ! Top soil layer depth (1 cm); Assume constant for all grid cells
         Real wres1, wres2
         Real tcec1, tcec2

         Character( 16 ), Parameter :: pname = 'Init_ABFlux' 
         Character( 16 )            :: vname
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
                  
         Beld_ag = 0.01 * Beld_ag   ! convert to fraction

! Allocate variable needed soil processes 
         If ( .Not. Allocated ( gamma1 ) ) Then
            Allocate ( gamma1 ( ncols,nrows ) )
         End If
         gamma1 = 0.0
         If ( .Not. Allocated ( gamma2 ) ) Then
            Allocate ( gamma2 ( ncols,nrows ) )
         End If
         gamma2 = 0.0
         If ( .Not. Allocated ( MHp1 ) ) Then
            Allocate ( MHp1 ( ncols,nrows ) )
         End If         
         MHp1 = 0.0
         If ( .Not. Allocated ( MHp2 ) ) Then
            Allocate ( MHp2 ( ncols,nrows ) )
         End If
         MHp2 = 0.0
         If ( .Not. Allocated ( fcec1 ) ) Then
            Allocate ( fcec1 ( ncols,nrows ) )
         End If
         fcec1 = 0.0
         If ( .Not. Allocated ( fcec2 ) ) Then
            Allocate ( fcec2 ( ncols,nrows ) )
         End If
         fcec2 = 0.0

         ! parameters
            Do r = 1, nrows
               Do c = 1, ncols
                  Tot_Ag = 0.0      
                  tcec1   = 0.0    
                  tcec2   = 0.0
                  Do l = 1, e2c_cats
                     If( .Not. IsNaN( pHs1( c,r,l ) )   .And.
     &                   .Not. IsNaN( pHs2( c,r,l ) )   .And.
     &                   .Not. IsNaN( NH4ps1( c,r,l ) ) .And.
     &                   .Not. IsNaN( NH4ps2( c,r,l ) ) .And.
     &                    Beld_ag( c,r,l ) .Gt. 0.0 ) Then
                        If( pHs1( c,r,l ) .Gt. 0.0 ) Then
                           pHfac1 = 10.0 ** (-pHs1( c,r,l ) )
                           pHfac2 = 10.0 ** (-pHs2( c,r,l ) )
                        Else
                           pHfac1 = 1.0
                           pHfac2 = 1.0
                        End If
! convert units from kg/ha N to mol/l
                        If ( pHs1( c,r,l )   .Gt. 4.0       .And.
     &                       pHs2( c,r,l )   .Gt. 4.0       .And.
     &                       pHs1( c,r,l )   .Lt. 9.0       .And.
     &                       pHs2( c,r,l )   .Lt. 9.0       .And.
     &                       NH4ps1( c,r,l ) .Gt. 0.0       .And.
     &                       NH4ps2( c,r,l ) .Gt. 0.0     ) Then
   
                           wep1(c,r,l) = min (wep1(c,r,l),d1*1000.*por1(c,r,l)) ! don't let SM exceed saturation  
                           wep2(c,r,l) = min (wep2(c,r,l),dep2(c,r,l)*1000.*por2(c,r,l)) 
                           wres1       = 0.0065757 + 0.24909 * wp1(c,r,l) 
                           wres2       = 0.0065757 + 0.24909 * wp2(c,r,l) 
                           wep1(c,r,l) = max (wep1(c,r,l),d1*1000.*wres1)        ! don't let SM below Wres  
                           wep2(c,r,l) = max (wep2(c,r,l),dep2(c,r,l)*1000.*wres2)              
                           Tot_Ag        = Tot_Ag + Beld_ag( c,r,l )                        
                           gamma1( c,r ) = gamma1( c,r ) + conv * NH4ps1( c,r,l )
     &                                   * 1000.0 / (wep1(c,r,l) * pHfac1) * Beld_ag( c,r,l )
                           MHp1( c,r )   = MHp1( c,r ) + Beld_ag( c,r,l ) * pHfac1
                           gamma2( c,r ) = gamma2( c,r ) + conv * NH4ps2( c,r,l )
     &                                   * 1000.0 / (wep2(c,r,l) * pHfac2) * Beld_ag( c,r,l )
                           MHp2( c,r )   = MHp2( c,r ) + Beld_ag( c,r,l ) * pHfac2
                           tcec1         = tcec1        + Beld_ag( c,r,l ) * cec1(c,r,l)
                           tcec2         = tcec2        + Beld_ag( c,r,l ) * cec2(c,r,l)
                        End If
                     End If
                  End Do ! e2c_cats
                  If ( Tot_Ag .ge. 1.0e-6 ) then
                     If ( MHp1( c,r ) / Tot_Ag .Gt. 1.0e-9 .And.
     &                    MHp1( c,r ) / Tot_Ag .Lt. 1.0e-4 .And.
     &                    MHp2( c,r ) / Tot_Ag .Gt. 1.0e-9 .And.
     &                    MHp2( c,r ) / Tot_Ag .Lt. 1.0e-4 ) Then
! get the agricultrual only relevant number
                        MHp1( c,r ) = MHp1( c,r ) / Tot_Ag
                        MHp2( c,r ) = MHp2( c,r ) / Tot_Ag
                        gamma1( c,r ) = gamma1( c,r ) / Tot_Ag
                        gamma2( c,r ) = gamma2( c,r ) / Tot_Ag
! Set a minimum Gamma for Ag based off of Zhang et al. 2010
!                     gamma1( c,r ) = max( gamma1( c,r ), 630.0 )
!                     gamma2( c,r ) = max( gamma2( c,r ), 630.0 )
! NaN trap for debugging
!                        If ( IsNaN( gamma1( c,r ) ) .Or. IsNaN( gamma2( c,r ) ) .Or.
!     &                       IsNaN( MHp2( c,r ) ) .Or. IsNaN( MHp2( c,r ) ) ) Then
!                           xmsg = 'NaN in grid cell Gamma Calculation'
!                           Call M3exit( pname, jdate, jtime, xmsg, xstat1 )
!                        End If
                        tcec1         = tcec1 / Tot_Ag
                        tcec2         = tcec2 / Tot_Ag
                        fcec1(c,r)    = max(0.3, 1.0-0.038*tcec1) ! from EPIC (Wiliams 1995)
                        fcec2(c,r)    = max(0.3, 1.0-0.038*tcec2)
!                        if(r.eq.18.and.c.eq.27) then
!                            Write(Logdev,*) ' Bidi-Epic soil data'
!                            Write(Logdev,*) gamma1(c,r),gamma2(c,r),fcec1(c,r),tcec1
!                        Endif
                     Else
                        MHp1( c,r ) = 0.0
                        MHp2( c,r ) = 0.0
                        gamma1( c,r ) = 0.0
                        gamma2( c,r ) = 0.0
                     Endif
                  Else
                     MHp1( c,r ) = 0.0
                     MHp2( c,r ) = 0.0
                     gamma1( c,r ) = 0.0
                     gamma2( c,r ) = 0.0
                  End If
               End Do ! c
            End Do ! r
         
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

         Subroutine Get_Flux( cNH3, rwetsfc, rgw, r, c, l, pvd, lnh3,
     &                         rb,rinc,rstom, delta,rgnd,femis,fdep)
         
         Use UTILIO_DEFN
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2
         Use ASX_DATA_MOD
         Use LSM_MOD
         
         Implicit None
         
         Include SUBST_FILES_ID  ! file name parameters

         Real,    Intent( IN )  :: cNH3
         Real,    Intent( IN )  :: rwetsfc        
         Real,    Intent( IN )  :: rgw
         Real,    Intent( IN )  :: rb
         Real,    Intent( IN )  :: rinc
         Real,    Intent( IN )  :: rstom
         Real,    Intent( IN )  :: delta
         Real,    Intent( IN )  :: rgnd

         Integer, Intent( IN )  :: r
         Integer, Intent( IN )  :: c
         Integer, Intent( IN )  :: l ! species index
         
         Real,    Intent( OUT ) :: pvd
         Real,    Intent( OUT ) :: lnh3
         Real,    Intent( OUT ) :: femis
         Real,    Intent( OUT ) :: fdep
         
         Real( 8 )              :: aq           ! Quadradic equation variable
         Real( 8 )              :: bq           ! Quadradic equation variable
         Real                   :: cnh3c        ! In canopy NH3 concentration [ppm]
         Real                   :: cnh3g1, cnh3g2, cnh3g   ! NH3 compensation concentration for ground [ppm]
         Real                   :: cnh3s        ! NH3 compensation concentration for stomatal [ppm]
         Real                   :: cnh3g1j, cnh3g2j, cnh3sj
         Real( 8 )              :: cq           ! Quadradic equation variable
         Real                   :: del0         ! for Rbg
         Real( 8 )              :: ga           ! Ga = 1/Ra [m/s]
         Real                   :: gammas       ! [NH4+]/[H+]
         Real( 8 )              :: gcw
         Real( 8 )              :: gg1 !( n_lufrac ) ! Gg = 1/(Rgnd(nh3)+Rinc) [m/s]  
         Real( 8 )              :: gsb          ! Gsb = 1/(Rstom(nh3)+Rb(nh3)) [m/s]
         Real( 8 )              :: gt
         Real( 8 )              :: qq           ! intermediate variable
         Real                   :: rbg  !j( n_lufrac )
!        Real,        Parameter :: rwm = 35.0   ! Minimum NH3 cuticle resistance [s/m]
         Real,        Parameter :: rwm = 20.0   ! Minimum NH3 cuticle resistance [s/m]
         Real                   :: rwmb         ! Rwmb = Rwm + Rb
         Real                   :: rwx          ! Rw = Rwm + Rwx * CNH3C [s/m]
         Real                   :: scn          ! for Rbg
         Real                   :: ustg         ! for Rbg
         Real                   :: vdg          ! Vd(nh3) to non-veg part [m/s]
         Real                   :: w5cm         ! soil moisture in top 5 cm (vol frc)
         Real,        Parameter :: d1 = 0.01    ! Top soil layer depth (1 cm)
         Real,        Parameter :: twothree = 2.0/3.0
         Real,        Parameter :: onethree = 1.0/3.0
         Real,        Parameter :: MolN     = 14.007  ! g/mol N
!         Real,        Parameter :: MolNH3   = 17.01  ! g/mol NH3
         Real( 8 )              :: ldry
         Real( 8 )              :: dp
         Real( 8 )              :: rsoil1
         Real( 8 )              :: rsoil2
         Real( 8 )              :: a1
         Real                   :: agfrac
         Real( 8 )              :: ddd
         Real                   :: watfrac ! water fraction
         Integer                :: k,j
         Real                   :: canfrac
         Real                   :: frac_sol1    ! Fraction of NHx in solution layer 1
         Real                   :: frac_sol2    ! Fraction of NHx in solution layer 2
         Real                   :: cnh3cdep
         Real                   :: cnh3cemis, rcut, gwd

         canfrac = 0.5    !exp(-1.)   

!> Compute quasi-laminar boundary layer resistance at the soil surface

         scn  = kvis / dif0( l )
         ustg = max( MET_DATA%USTAR( c,r ) * EXP( -MET_DATA%LAI( c,r ) ), 0.001 )         
         del0 = 1.0E-4 * kvis / ( karman * ustg )
         rbg = ( scn - LOG( 10.0 * del0 ) ) / ( karman * ustg )


!-- Compute soil resitance using soil moitsure in soil layer 1 (1 cm) from WRF
         ldry= d1 * ( Exp( ( 1.0 - MET_DATA%SOIM1( c,r ) / GRID_DATA%WSAT( c,r ) ) ** 5 ) - 1.0 ) / 1.718
         
         dp  = dif0( l ) * 1.0E-4 * GRID_DATA%WSAT( c,r )**2
     &       * ( 1.0 - GRID_DATA%WRES(c,r) / GRID_DATA%WSAT(c,r) ) ** ( 2.0 + 3.0 / GRID_DATA%BSLP(c,r) )
         
         rsoil1 = ldry / dp
         
         w5cm = 0.2 * MET_DATA%SOIM1( c,r ) + 0.8 * MET_DATA%SOIM2( c,r )
         w5cm = Min( w5cm, GRID_DATA%WSAT( c,r ) )
         w5cm = Max( w5cm, GRID_DATA%WRES( c,r ) )
          


         agfrac  = 0.0
         watfrac = 0.0
         pvd     = 0.0
         lnh3    = 0.0

!> If the soil is frozen assume not evasive flux and skip calculation of comp. points         
         If ( MET_DATA%Tempg( c,r ) .Le. 273.15 ) Then
            cnh3s  = 0.0
            cnh3g = 0.0
            Go To 101
         End If

!> Compute compensation point. gamma is specified according to the amount of 
!> cultivated vegetation  
             
         a1    = 161512.0d0 / real( MET_DATA%tempg( c,r ), 8 )
     &         * 10.0d0 ** ( -4507.11d0 / real( MET_DATA%tempg( c,r ), 8 ) )
         a1    = a1 * 17.0d9    ! microgram/m3  24.5d0 * 1.0d6  ! ppm    
         
!> Set a maximum [NH4]/[H+] ratio at 200,000 based on output from the AIM aerosol
!> model any [NH4] in excess of this ratio is assumed to partition into the solid
!> phase. Canada soil gamma taken from Zhang et al 2010 JGR Table 5

         cnh3g1 = 0.0
         cnh3g2 = 0.0
         cnh3s = 0.0
!---- removed division of gamma by soil moisture -- jep 12/16
         Do j = 1, n_lufrac
            If ( GRID_DATA%LUFRAC( c,r,j ) .Gt. 0.0 ) Then
               cnh3sj = a1 * luf_fac( j )
               Select Case( cat_lu( j ) )
                  Case( 'WATER' ) ! water
                     cnh3g1j = 0.0
                     cnh3g2j = 0.0
                     cnh3sj  =0.0
                     watfrac = watfrac + GRID_DATA%LUFRAC( c,r,j )
                  Case( 'SNOWICE' ) ! ice or snow
                     cnh3g1j = 0.0
                     cnh3g2j = 0.0
                     cnh3sj  =0.0
!---- Gamma soil 
                  Case( 'AG','AGMOS' ) ! Ag
                      if (fcec1(c,r).gt. 0.299 ) Then                
                        frac_sol1= fcec1(c,r) 
                        frac_sol2= fcec2(c,r)
                      else
                        frac_sol1= 0.55
                        frac_sol2= 0.55
                      endif
                     
                    if( cat_lu( j ).eq.'AG' ) then   ! Ag
                        cnh3g1j = Min( Max( frac_sol1*gamma1( c,r ), C_gam / MET_DATA%SOIM1( c,r ) ),
     &                                     maxgam)

                        cnh3g2j = Min( Max( frac_sol2*gamma2( c,r ), C_gam / w5cm), 
     &                                     maxgam )
                        agfrac  = agfrac  + GRID_DATA%LUFRAC( c,r,j )
                     else                             ! Ag mosaic 67% ag 33% mosaic 
                        cnh3g1j = Min( Max( frac_sol1*gamma1( c,r ), C_gam / MET_DATA%SOIM1( c,r ))
     &                                    * twothree + onethree * natgam , maxgam )
                        cnh3g2j = Min( Max( frac_sol2*gamma2( c,r ), C_gam / w5cm )
     &                                    * twothree + onethree * natgam , maxgam )
                        agfrac  = agfrac  + twothree * GRID_DATA%LUFRAC( c,r,j )
                     endif

                  Case Default ! not ag
                     cnh3g1j = Min( natgam , maxgam )
                     cnh3g2j = Min( natgam , maxgam )
               End Select
               cnh3g1  = cnh3g1 + GRID_DATA%LUFRAC( c,r,j ) * cnh3g1j             
               cnh3g2  = cnh3g2 + GRID_DATA%LUFRAC( c,r,j ) * cnh3g2j
               cnh3s   = cnh3s  + GRID_DATA%LUFRAC( c,r,j ) * cnh3sj
            End If
         End Do
         cnh3g1 = a1 * Max( cnh3g1, 0.01 ) / (1.0 - watfrac)   ! Land only (microgram/m3)
         cnh3g2 = a1 * Max( cnh3g2, 0.01 ) / (1.0 - watfrac)   ! Land only (microgram/m3)
         cnh3g  = Max (cnh3g1, cnh3g2)   ! use greater of layer concs
         cnh3s = cnh3s / (1.0 - watfrac)   ! Land only (microgram/m3)
!         if(agfrac.GT.0.5.and.gamma1( c,r ).gt.50000.) then
!            write(logdev,*) ' frac_sol1',frac_sol1,' cnh3g1',cnh3g1,' gamma1',gamma1( c,r )
!            write(logdev,*) ' c,r=',c,r,' gamma2',gamma2( c,r )
!         endif
101      Continue

!> Cuticle resistance : rw = rwx * cnh3c + rwm
         If ( MET_DATA%LAI( c,r ) .Gt. 0.0 ) Then

            rwx   = rwetsfc   
            rwmb  = rwm + rwx + (100.0-max(MET_DATA%RH2( c,r ),60.0) + Rb) * MET_DATA%LAI( c,r )  ! from Pleim et al 2013 JGR
            ga    = 1.0 / ( MET_DATA%RA( c,r ) + canfrac * rinc ) !
            gsb   = 1.0 / ( rstom + rb )
            gg1   = 1.0 / ( rbg + (1.-canfrac) * rinc + rsoil1 )
            gcw   = 1.0 / ( rb + rwetsfc/MET_DATA%LAI( c,r ) )
            gt    = gsb + gg1  + ga + delta * gcw   ! single soil layer
            qq    = ga * cnh3 + gsb * cnh3s + gg1 * cnh3g 
            aq    = rwx * gt
            bq    = rwmb * gt + MET_DATA%LAI( c,r ) * ( 1.0 - delta ) - rwx * qq
            cq    = -rwmb * qq
! Microg/m3
            cnh3c = (-bq + sqrt( bq**2 - 4.0d0 * aq * cq))/(2.0d0 * aq)  ! this should always yield non-negative result
           
! if the compensation point less than zero reset it to zero
            cnh3c = max( cnh3c, 0.0 )   
!-- Compute deposition part assuming cnh3g and cnh3s = 0
            qq    = ga * cnh3
            bq    = rwmb * gt + MET_DATA%LAI( c,r ) * ( 1.0 - delta ) - rwx * qq
            cq    = -rwmb * qq
            cnh3cdep = (-bq + sqrt( bq**2 - 4.0d0 * aq * cq))/(2.0d0 * aq) 

!-- compute emission component using cnh3c from bidi flux for Rcut
!         rcut = rwm + rwx*(cnh3c+1.) + (100.0-max(MET_DATA%RH( c,r ),60.0) + Rb) * MET_DATA%LAI( c,r )
            rcut = rwmb + rwx* cnh3c
            gwd = ( 1.0 - delta )* MET_DATA%LAI( c,r )/rcut
            cnh3cemis = (gsb * cnh3s + gg1 * cnh3g)/( gt + gwd)
         else
            cnh3c = 0.0
            cnh3cemis = 0.0

         End If  ! lai > 0.0

         ddd = ( 1.0 - delta ) / rgnd
     &       + delta / rgw

         vdg = 1.0 / ( MET_DATA%RA( c,r ) + rb + 1.0 / ddd )

         pvd  = cnh3c * ga * MET_DATA%VEG( c,r )                  ! microg/m2/s
         lnh3  = vdg + MET_DATA%VEG( c,r ) * ( ga - vdg )
         femis  = ga*(cnh3cemis)*MET_DATA%VEG( c,r ) 
         fdep = lnh3*cnh3 - ga*cnh3cdep*MET_DATA%VEG( c,r ) 

!         if(r.eq.18.and.c.eq.27) then
!            Write(Logdev,*) ' Bidi-Epic conc'
!            Write(Logdev,*) cnh3g1,cnh3g2,cnh3g,cnh3s,cnh3c,cnh3
!            Write(Logdev,*) agfrac,rsoil1,MET_DATA%VEG( c,r ),ga,rbg
!          Endif
 

         Return        
         End Subroutine Get_Flux

      End Module ABFlux_Mod
