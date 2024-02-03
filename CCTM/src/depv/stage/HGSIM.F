
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
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry_mp/HGSIM.F,v 1.3 2012/01/24 21:15:18 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE HGSIM

C-----------------------------------------------------------------------
C Function: This module contains the code to predict bidirectional
C          exchanges between the atmosphere and surface media using a two
C          layer resistance-capacitance model. Fluxes are parameterized by
C          applying Fick's law  across the atmospheric surface media
C          concentration gradient.
C
C Revision History:
C      12 Aug  2008  J. Bash initial implementation
C       2 Apr  2009  J. Bash for solar irradation on the order of 1e-3 w/m2
C                           the mercury surface water photo redox scheme
C                           became unstable. A conditional statement was
C                           added to correct this instability.
C       4 June 2009 J. Bash Corrected the time stamp on WRASX_MEDIA to be
C                           consistant with other CMAQ modules reported by
C                          (T.Myers)
C     22 Oct   2009 J. Bash Corrected a units conversion error in ASWX and ATX
C                           reported by (P. Pongprueksa) and added a more
C                           robust soil diffusion model adapted from the
C                           Community Land Model 3.5.
C     13 Sept 2011  J. Bash Updated the Hg bidi model to share data with the 
C                           NH3 bidirectional exchange model in a more general
C                           framework using BIDI_MOD.F and LSM_MOD.F modules. 
C                           Hg bidirectional exchange is now a run time option. 
C     17 Jan  2012  J. Bash Removed the dependence on the LAPACK libraries
C                           and found analytical solutions to all the Hg
C                           exchange equations.
C     14 Feb 2013   J. Bash Added support for the NLCD 40 (2006) land use data
C     10 Feb 2019   D. Wong Implemented centralized I/O approach, removed all MY_N clauses
C
C  References:
C
C  Bash, J.O. 2010, Description and initial simulaiton of a dynamic bi-directional 
C     surface exchange model for mercury in CMAQ, J. Geophys.
C     Res., 115, D06305
C  Mason, R.P., J.R. Reinfelder, F.M.M. Morel, 1996, Uptake, toxicity, and
C     trophic transfer of mercury in a coastal diatom, Environ. Sci. Technol.
C     30, 1835-1845
C  Scholtz, M.T., B.J. Van Heyst, W.H. Schroeder, 2003, Modelling of mercury
C     emissions from background soils, Sci. Tot. Environ. 304, 185-207
C  Trapp S. and Matthies, 1995, Generic one-compartment model for uptake of
C     organic chemicals by foliar vegetations. Environ. Sci. Technol. 29,
C     2333-2338
C  Trapp, S., 2004, Plant uptake and transport for netural and ionic chemicals,
C     Environ. Sci. Pollut. Res. 11, 33-39
C  Whalin, L., E.-H. Kim, R. Mason, 2007, Factors influencing the oxidation,
C     reduciton, methylation and demethylation of mercury species in costal
C     water, Marine Chem. 107, 278-294
C-----------------------------------------------------------------------
      IMPLICIT NONE

!    Shared variables

!     Private variables used in this routine and
      REAL, ALLOCATABLE, SAVE, PRIVATE :: fevgrn(:,:)  ! fraction of evergreen land use
      REAL, ALLOCATABLE, SAVE, PRIVATE :: f_wat(:,:)   ! fraction of water land use

      REAL, PARAMETER, PRIVATE :: zsurf  = 1.0 ! ocean slab depth (m)
      REAL, PARAMETER, PRIVATE :: ZG = 1.0e-2
      REAL, PARAMETER, PRIVATE :: MWHG   = 200.6
      REAL, PARAMETER, PRIVATE :: MWHGII = 271.5
      CHARACTER( 96 ), PRIVATE :: XMSG = ' '

      CHARACTER( 80 ), SAVE, Private   :: LAND_SCHEME
      REAL, SAVE, PRIVATE  :: ZC, ZM   ! g/m2 of model surface media
      REAL, SAVE, PRIVATE  :: kam      ! Leaf Water partitioning coef mol water g-1 leaf dry mass
      REAL, SAVE, PRIVATE  :: kac      ! mol air g-1 leaf dry mass
      REAL, SAVE, PRIVATE  :: MV_air

! variable needed for analytical solutions of exchange equations
      REAL, PRIVATE :: KO(2,2)
      REAL, PRIVATE :: EIVAL(2)
      REAL, PRIVATE :: VR(2,2)
      REAL, PRIVATE :: ax     ! coefficients used of the  
      REAL, PRIVATE :: bx     ! quadratic and cubic equations
      REAL, PRIVATE :: cx     ! ATX and ASWX
      REAL, PRIVATE :: Qx     ! coefficients used to solve for 
      REAL, PRIVATE :: Rx     ! the roots of the cubic equation
      REAL, PRIVATE :: ev1    ! Temporary variables used to 
      REAL, PRIVATE :: ev2    ! calculate the eigen vectors 
      REAL, PRIVATE :: ev3    ! in ATX and ASWX
      REAL, PRIVATE :: evmax  !
      REAL, PRIVATE :: DetKO  ! Variables used to solve for the 
      REAL, PRIVATE :: DetK1  ! non-homogeneous part of the solution
      REAL, PRIVATE :: DetK2  ! a system of equations 
      REAL, PRIVATE :: DetEV  ! Variables used to solve for the 
      REAL, PRIVATE :: DetE1  ! integration constants in the 
      REAL, PRIVATE :: DetE2  ! system of equations using Cramer's Rule
      
      INTEGER, PRIVATE :: NC
      INTEGER, PRIVATE :: i
      INTEGER, PRIVATE :: j
      
      REAL, PRIVATE :: B( 2 )   ! Surface media concentration vector
      REAL, PRIVATE :: NHS( 2 ) ! non-homogenious solution

C input/output parameters

      INTEGER, PRIVATE :: N_AQ_CONC  ! aqueous media concentrations
      INTEGER, PRIVATE :: N_GAS_CONC ! gaseous media concentrations
      INTEGER, PRIVATE :: N_SOL_CONC ! solid media concentrations

      CHARACTER( 16 ), ALLOCATABLE, PRIVATE :: MEDIA_NAMES( : )

      CONTAINS

         SUBROUTINE INIT_HGSIM( JDATE, JTIME )

         USE HGRD_DEFN           ! horizontal grid specifications
         USE UTILIO_DEFN
         USE ASX_DATA_MOD
         USE LSM_MOD
         USE Bidi_Mod
         Use MOSAIC_MOD, Only: Tile_Data 

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER, INTENT( IN ) :: JDATE
         INTEGER, INTENT( IN ) :: JTIME

         CHARACTER( 16 ) :: PNAME = 'INIT_HGSIM'
         CHARACTER( 96 ) :: MSG = ' '

         INTEGER  V, L, C, R

C--------------------------------------------------------------------------

         IF ( .NOT. ALLOCATED ( fevgrn ) ) THEN
            ALLOCATE ( fevgrn( NCOLS,NROWS ) )
            fevgrn( :,: ) = 0.0
         END IF
         IF ( .NOT. ALLOCATED ( f_wat ) ) THEN
            ALLOCATE ( f_wat( NCOLS,NROWS ) )
            f_wat( :,: ) = 0.0
         END IF
 
         DO C = 1, NCOLS
            DO R = 1, NROWS
               DO L = 1, Tile_data%N_LUFRAC
                  IF(Tile_data%CAT_LU(L) .EQ. 'EVEFORN' .Or. 
     &               Tile_data%CAT_LU(L) .EQ. 'EVEFORB' ) THEN
                     fevgrn(c,r) = fevgrn(c,r) + Tile_data%lufrac(c,r,l)
                  End IF
                  IF(Tile_data%CAT_LU(L) .EQ. 'MIXFOR') THEN
                     fevgrn(c,r) = fevgrn(c,r) + Tile_data%lufrac(c,r,l)
                  End IF                                    
                  IF(Tile_data%CAT_LU(L) .EQ. 'WATER') THEN
                     f_wat(c,r) = f_wat(c,r) + Tile_data%lufrac(c,r,l)
                  END IF
               END DO
            END DO
         END DO

         RETURN

!------------------------------------------------------------------------------
! Error handling section
!------------------------------------------------------------------------------
1001     CONTINUE
         CALL M3EXIT( pname, jdate, jtime, xmsg, xstat1 )
C-------------------------------------------------------------------------------
C Format statements.
C-------------------------------------------------------------------------------

9001     FORMAT( 'Failure reading ', a, 1x, 'from ', a )

         RETURN

         END SUBROUTINE INIT_HGSIM
!------------------------------------------------------------------------------
! Gets compensation points for STAGE
!------------------------------------------------------------------------------
         Subroutine Get_Hg_Comp( Hg_st, Hg_cut, Hg_grnd, Hg_wat, Hg_atm, H_wat, H_soil, r, c )
 
         USE BIDI_MOD
         USE ASX_DATA_MOD

         IMPLICIT NONE

         REAL,    INTENT( IN )   :: H_wat, H_soil                      ! Effective H for Hg(0)
         REAL,    INTENT( IN )   :: Hg_atm                             ! atm conc and land use fractions
         INTEGER, INTENT( IN )   :: c,r                                ! column and row
         REAL,    INTENT( OUT )  :: Hg_st, Hg_cut, Hg_grnd, Hg_wat     ! compensation points     
!********* reduction and partioning terms *******************************
         REAL :: Kow     ! Hg(0) Octanol water partioning coefficient
         REAL :: Kpwc    ! Hg(0) air-vegetation surface partitioning coefficient
         REAL :: Kpwm    ! Hg(0) air-mesophyll partitioning coefficient
!********* vegetation poperties *****************************************
         REAL :: lm   ! leaf mesophyll lipid fraction
         REAL :: lc   ! cuticular wax mesophyll lipid fraction
         REAL :: Wp   ! water content fraction of the leaf
         REAL :: bc   ! Emprical coefficeint to describe differences in plant lipids
!************************************************************************
         Real    :: del      
         If ( Met_Data%WR( c,r ) .LE. 0.0 ) Then
               del = 0.0
         Else 
               del = Met_Data%WR( c,r ) / ( 0.2e-3 * Met_Data%Veg( c,r ) * Met_Data%LAI( c,r ) )   ! refer to SiB model
               del = min(del,1.0)
         End If
!***************** canopy parameters *********************************
         Kow  = 4.15    ! For Hg, Mason 1996
         lm   = 2.0e-2  ! From Trapp and Matthis 1995
         lc   = 2.0e-2  ! Assumed cuticular wax lipid content
         Wp   = 0.80    ! leaf water fraction, Trapp and Mathis 1996
         bc   = 0.95    ! For barley, Trapp and Mathis 1996
         Kpwc = (Wp+lc*1.0/0.822*Kow**bc)                 ! unitless
         Kpwm = (Wp+lm*1.0/0.822*Kow**bc)                 ! unitless
! Partitioning coeficients following the methodology of the PEM model
         kac = Kpwc*(1.0-del) +  ! evasion from dry cuticles
     &         Kpwc*del*H_soil   ! unitless cuticle surface
         kam = Kpwm*H_soil       ! unitless apoplast solution
C Flux unit conversions
         ZM     = 71.0 * Met_Data%LAI(c,r) / ( 10.0 * Met_Data%Z0(c,r) )! measurements at UCONN's experimental
         ZC     = 71.0 * Met_Data%LAI(c,r) / ( 10.0 * Met_Data%Z0(c,r) )! g/m**3 based off of leaf litter fall
         MV_air = MWAIR / MET_DATA%DENS1( C,R ) / 1.0e3 ! m3/mol
!**************** soil parameters ************************************

         IF( INIT_COMP ) THEN
! Equilibrium Hg(0) mesophyll concentration in a 5 month box model simulation
            If( f_wat( c,r ) .eq. 1.0 ) Then
               CMEDIA( c,r,5 ) = 0.0 ! umol/g bulk leaf concentration
               CMEDIA( c,r,6 ) = 0.0 ! umol/g bulk leaf concentration
               CMEDIA( c,r,3 ) = 0.0 ! umol/g bulk soil concentration
               CMEDIA( c,r,2 ) = 3.57e-6      ! from Whalin et al 2007
               CMEDIA( c,r,1 ) = Hg_atm * 3.0 ! assume 3x eq con.
            Else if( f_wat( c,r ) .gt. 0.0 ) Then
               CMEDIA( c,r,5 ) = fevgrn(c,r)*6.0e-6 + (1.0-fevgrn(c,r))*CMEDIA(c,r,5) ! umol/g bulk leaf concentration
               CMEDIA( c,r,6 ) = fevgrn(c,r)*6.0e-7 + (1.0-fevgrn(c,r))*CMEDIA(c,r,6) ! umol/g bulk leaf concentration
               CMEDIA( c,r,3 ) = Hg_atm                                              ! umol/g bulk soil concentration
               CMEDIA( c,r,2 ) = 3.57e-6      ! from Whalin et al 2007
               CMEDIA( c,r,1 ) = Hg_atm * 3.0 ! assume 3x eq con.
            Else 
               CMEDIA( c,r,5 ) = fevgrn(c,r)*6.0e-6 + (1.0-fevgrn(c,r))*CMEDIA(c,r,5) ! umol g-1 bulk leaf concentration
               CMEDIA( c,r,6 ) = fevgrn(c,r)*6.0e-7 + (1.0-fevgrn(c,r))*CMEDIA(c,r,6) ! umol g-1 bulk leaf concentration
               CMEDIA( c,r,3 ) = Hg_atm                                               ! ppm bulk soil concentration
               CMEDIA( c,r,2 ) = 0.0
               CMEDIA( c,r,1 ) = 0.0
            End If
         End If

         Hg_st     = CMEDIA( c,r,5 ) / kam * ZM * MV_air ! umol g-1 bulk leaf concentration * g m-3 leaf * m3 mol-1
         Hg_cut    = CMEDIA( c,r,6 ) / kac * ZC * MV_air ! umol g-1 bulk leaf concentration * g m-3 leaf * m3 mol-1
         Hg_grnd   = CMEDIA( c,r,3 ) / H_soil         ! ppm bulk soil concentration
         Hg_wat    = CMEDIA( c,r,1 ) / H_wat          ! ppm

         Return

         End Subroutine Get_Hg_Comp

!------------------------------------------------------------------------------
! Updates Hg surface concentrations in STAGE
!------------------------------------------------------------------------------
         SUBROUTINE Hg_Surf_Update (flx_stom, flx_cut, flx_grnd, flx_wat, flx_hgII,
     &                              H_wat, H_soil, dt, c, r, Jdate, Jtime )

         USE BIDI_MOD
         USE ASX_DATA_MOD
         USE UTILIO_DEFN
         Use MOSAIC_MOD, Only: Tile_Data 

         IMPLICIT NONE

         REAL,    INTENT( IN )   :: flx_stom, flx_cut, flx_grnd, flx_wat, flx_hgII         ! fluxes ppm*m/s
         REAL,    INTENT( IN )   :: H_wat, H_soil                                          ! Soil and surface water H
         REAL,    INTENT( IN )   :: dt                                                     ! Intigration time step
         INTEGER, INTENT( IN )   :: c,r                                                    ! column and row
         INTEGER, INTENT( IN )   :: Jdate, Jtime                                           ! Time info

         REAL    :: flux_st, flux_cut, flux_grnd, flux_wat, flux_hgII      ! fluxes ppm*m/s
         REAL    :: Hg_st, Hg_cut, Hg_grnd, Hg_wat     ! compensation points     
         REAL, SAVE :: HgII_grnd, HgII_wat    ! Soil and water Hg(II) concentrations
!********* reduction and partioning terms *******************************
         REAL :: kr         ! soil divalent mercury reduction term
         REAL :: Prod       ! Production umol/s
         REAL :: K_loss     ! Loss Rate 1/s
         REAL :: K_Prod, K_Prod_HgII     ! Relative Production Rate 1/s
!********* Unit conversions *********************
!         REAL, Parameter :: M3MOLVOL = MOLVOL/1.0e3 ! molar volume of air at stp m3/mol      
C*************************** Ocean box parameters ***********************
         REAL, PARAMETER :: satten = 7.58-1
C***** reduction and partioning terms from Whalin et al 2007 ************
         REAL, PARAMETER :: rref  = 240.0  ! referance incoming radiation
                                           ! for redox measurements (w/m2)
         REAL, PARAMETER :: kphot = 6.5-4 ! drm photoreduction rate 1/s
         REAL, PARAMETER :: kox   = 7.2-4 ! dgm photo-oxidation rate 1/s
         CHARACTER( 16 ), PARAMETER :: pname      = 'Hg_Surf_Update'

         Hg_st     = CMEDIA( c,r,5 ) ! umol/g bulk leaf concentration
         Hg_cut    = CMEDIA( c,r,6 ) ! umol/g bulk leaf concentration
         Hg_grnd   = CMEDIA( c,r,3 ) ! umol/g bulk soil concentration
         Hg_wat    = CMEDIA( c,r,1 ) ! ppm
         HgII_wat  = CMEDIA( c,r,2 ) ! ppm
         HgII_grnd = 0.0
         DO i = 1, Tile_Data%n_lufrac
            HgII_grnd = HgII_grnd+Tile_Data%Hg_grnd(i)*Tile_Data%lufrac(c,r,i)
         END DO
C Set floor to smallest terrestrial value
         HgII_grnd = MAX( HgII_grnd, 1.8e1 )                   

! ppm m / s to umol/g/s
         If(ZM .gt. 0.0 )Then
            flux_st  = flx_stom / MV_air / ZM
            flux_cut = flx_cut  / MV_air / ZM 
         Else
            flux_st  = 0.0
            flux_cut = 0.0
         End If
! ppm m/s to ppm/s
         flux_grnd  = flx_grnd / ZG 
         flux_wat   = flx_wat  / zsurf 
         flux_hgII  = flx_HgII / zsurf 

         If( f_wat( c,r ) .gt. 0.0 ) Then ! Water
C in the absence of photo-redox reactions divalent Hg accumulates
            IF ( Met_Data%RGRND(c,r) .LT. 1e-3 ) THEN
               HgII_wat = HgII_wat - flux_hgII / ( f_wat( c,r ) ) * dt
C Find a simple one box solution for elemental Hg
               If( flux_wat .le. 0.0 ) Then ! Deposition
                  Prod = -flux_wat / ( f_wat( c,r ) )
                  Hg_wat = Hg_wat + Prod * dt
               Else
                  K_loss = min(flux_wat / ( CMEDIA( c,r, 1 ) * f_wat( c,r ) ), 0.99)                   
                  K_loss = -log(1.0 - K_loss )
                  Hg_wat = Hg_wat * exp( -K_loss * dt )
               End If
            ELSE If( CMEDIA( c,r,2 ) .Gt. 0.0 ) Then ! photo-redox reactions
               K_Prod_HgII = -flux_hgII / ( f_wat( c,r ) * CMEDIA( c,r,2 ) )
               K_Prod_HgII = log(1.0+K_Prod_HgII)
               If( flux_wat .le. 0.0 ) Then ! Deposition
                  Prod   = -flux_wat/ ( f_wat( c,r ) )
                  K_Prod = log(1.0+Prod/CMEDIA( c,r, 1 ))
                  K_loss = 0.0
               Else
                  Prod = 0.0
                  K_Prod = 0.0
                  K_loss = min( flux_wat / ( CMEDIA( c,r, 1 ) * f_wat( c,r ) ), 0.99 ) 
                  K_loss = -log(1.0-K_loss)
               End If 

               KO = 0.0 

C 240 w/m**2 is the 'typical light spectrum' from Whalin et al 2007 Marine Chem.
C attenuation at 1 m = 1/K (1-exp(-K Z)) = 0.758 using a K of 0.58


               KO( 1,1 )   = -K_loss - kox * satten * Met_Data%RGRND(c,r)/( rref ) + K_prod
               KO( 1,2 )   =  kphot * satten * Met_Data%RGRND(c,r)/( rref )
               KO( 2,1 )   =  kox *   satten * Met_Data%RGRND(c,r)/( rref )
               KO( 2,2 )   = -kphot * satten * Met_Data%RGRND(c,r)/( rref ) + K_Prod_HgII

               NHS = 0.0

               NHS(1) = 0.0!-Prod
               NHS(2) = 0.0!flux_hgII/( f_wat( c,r ) )

               B = 0.0

               B( 1 ) = Hg_wat   
               B( 2 ) = HgII_wat


C*****************************************************************************
! Get eigen values and vectors where the cubic equation is:
! ax*lambda**2+bx*lambda+cx = 0
! and is solved following Numerical recipies for Fortran equations 5.6.2-5.6.5
C*****************************************************************************

               ax = 1.0
               bx = -(KO(1,1)+KO(2,2))
               cx = KO(1,1)*KO(2,2)-KO(1,2)*KO(2,1)
    
               Qx = -0.5*(bx+SIGN(1.0,bx)*SQRT(bx**2-4.0*ax*cx))
    
               EIVAL(1) = Qx/ax
               EIVAL(2) = cx/Qx
    
! Solve for the eigenvectors
               DO i = 1, NC
                  ev1   = 1.0
                  ev2   = -(KO(2,1)*ev1)/(KO(2,2)-EIVAL(i))
                  evmax = max(abs(ev1),abs(ev2))
! scale the eigenvector
                  VR(1,i) = ev1/evmax
                  VR(2,i) = ev2/evmax
               END DO                  
! update the surface array
               Hg_wat   = 0.0
               HgII_wat = 0.0

               DO i = 1, NC
                  Hg_wat   = Hg_wat   + B(i) * VR(1,i) * EXP( EIVAL(i) * dt )
                  HgII_wat = HgII_wat + B(i) * VR(2,i) * EXP( EIVAL(i) * dt )
               END DO

               Hg_wat     = Hg_wat   + NHS(1)
               HgII_wat   = HgII_wat + NHS(2)

            END IF         

            IF ( HgII_wat .LT. 0.0 .OR. Hg_wat .LT. 0.0 ) THEN
            
               XMSG = '*** Negative concentration ***'   
               WRITE(LOGDEV,*) 'Hg_wat',Hg_wat,'HgII_wat',HgII_wat    
               Write(logdev,*) 'K_loss', K_loss
               Write(logdev,*) 'EIVAL', EIVAL(1), EIVAL(2)
               Write(logdev,*) 'EIVEC', VR(1,1), VR(2,2)
               Write(logdev,*) 'NHS', NHS(1), NHS(2)
         
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
   
            END IF       
            CMEDIA( c,r,1 ) = Hg_wat    ! ppm
            CMEDIA( c,r,2 ) = HgII_wat  ! ppm                                                          
         End If
         If( f_wat( c,r ) .lt. 1.0 ) Then ! land
! Soil divalent mercury reduction rate following Scholz et al 2003
            IF(Met_Data%SOIT1(c,r) .GT. 273.15) THEN
               kr     = 8.0e-11
            ELSE ! if the soil is frozen limit diffusion and reduction
               kr     = 0.0
            END IF
            If( Met_Data%LAI(c,r) .Gt. 0.0 ) Then
! Stomatal flux
               If( flux_st .le. 0.0 ) Then ! deposition 
                  Prod = -flux_st / ( (1.0 - f_wat( c,r ) ) )
                  Hg_st = Hg_st + Prod * dt
               Else
                  K_loss  = min( flux_st / ( Hg_st * (1.0 - f_wat( c,r ) ) ), 0.99 ) 
                  K_loss  = -log(1.0 - K_loss)
                  Hg_st = Hg_st * exp( - K_loss * dt ) 
               End If
               CMEDIA( c,r,5 ) = Hg_st
! Cuticular flux
               If( flux_cut .le. 0.0 ) Then ! deposition 
                  Prod = -flux_cut / ( (1.0 - f_wat( c,r ) ) )
                  Hg_cut = Hg_cut + Prod * dt
               Else
                  K_loss  = min(flux_cut / ( Hg_cut * (1.0 - f_wat( c,r ) ) ), 0.99 ) 
                  K_loss  = -log(1.0 - K_loss )
                  Hg_cut = Hg_cut * exp( - K_loss * dt ) 
               End If
               CMEDIA( c,r,6 ) = Hg_cut
            End If
! soil flux
            If( flux_grnd .le. 0.0 ) Then ! deposition 
               Prod = kr * Grid_Data%RHOB( c,r ) * HgII_grnd * ZG /( 1.0e3 * 200.59 ) -
     &                flux_grnd / max((1.0 - f_wat( c,r )),0.001 )
               Hg_grnd = Hg_grnd + Prod * dt
            Else 
               Prod    = kr * Grid_Data%RHOB( c,r ) * HgII_grnd * ZG /( 1.0e3 * 200.59 )
               K_loss  = min( flux_grnd / ( Hg_grnd * max((1.0 - f_wat( c,r )),0.001 ) ), 0.99 ) 
               K_loss  = -log(1.0 - K_loss)
               If( K_loss .eq. 0.0 ) Then ! due to numerical rounding
                  Hg_grnd = Hg_grnd + Prod * dt
               Else
                  Hg_grnd = Prod / K_loss + ( Hg_grnd - Prod / K_loss ) * exp( -K_loss * dt )
               End If
            End If
            CMEDIA( c,r,3 ) = Hg_grnd
! Model layer depths

         End If

         Return
         End Subroutine Hg_Surf_Update

C------------------------------------------------------------------------------

         SUBROUTINE GET_WDEP( CSE, WDEP, C, R )

         Use ASX_DATA_MOD, Only: Grid_Data
         USE BIDI_MOD, Only: CMedia

         IMPLICIT NONE

         INCLUDE SUBST_CONST     ! constants

         CHARACTER( 8 ), INTENT( IN ) :: CSE  ! wet dep sepcies
         REAL,      INTENT( IN ) :: WDEP ! wet deposition in kg/ha
         INTEGER,   INTENT( IN ) :: C
         INTEGER,   INTENT( IN ) :: R
         REAL, PARAMETER :: HAOM2   = 1.0e-4 ! ha/m^2 conversion
         REAL, PARAMETER :: MWHG    = 200.59 ! molecular weight of Hg
         REAL, PARAMETER :: UGOKG   = 1.0e9  ! ug/kg conversion
         REAL, PARAMETER :: GH2ONM3 = 1.0e6  ! g H2O in M^3 H2O
         REAL  WDEP_LOAD   ! loading due to wet deposition


         IF ( f_wat( c,r )  .Gt. 0 ) THEN ! water

         ! convert to umol/m2 pulse input
            WDEP_LOAD = WDEP*HAOM2*UGOKG/MWHG*f_wat( c,r )
         ! convert to added concentration in ppm assuming it remains at the surface
            WDEP_LOAD = WDEP_LOAD/ZSURF/GH2ONM3*MWWAT

            IF( CSE .EQ. 'HG      ' ) THEN

               CMEDIA( C,R,1 ) = CMEDIA( C,R,1 ) + WDEP_LOAD

            END IF

            IF( CSE .EQ. 'HGIIGAS ' ) THEN

               CMEDIA( C,R,2 ) = CMEDIA( C,R,2 ) + WDEP_LOAD

            END IF

         END IF ! water

         RETURN

         END SUBROUTINE GET_WDEP
      END MODULE HGSIM
