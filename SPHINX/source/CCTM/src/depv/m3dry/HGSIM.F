
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
C     15 Oct 2018   D. wong Moved INIT_MEDC_1 data extraction code to centralized_io_module.F
C     01 Feb 2019 David Wong: Implemented centralized I/O approach, removed all MY_N clauses
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
      REAL(8), ALLOCATABLE,       PRIVATE :: HgLU_Fac(:)
      REAL(8), ALLOCATABLE, SAVE, PRIVATE :: fevgrn(:,:)  ! fraction of evergreen land use

      REAL(8), PARAMETER, PRIVATE :: zsurf  = 1d+0 ! ocean slab depth (m)
      REAL(8), PARAMETER, PRIVATE :: ZG = 5d-2

      CHARACTER( 96 ), PRIVATE :: XMSG = ' '
      CHARACTER( 80 ), SAVE, Private   :: LAND_SCHEME

! variable needed for analytical solutions of exchange equations
      REAL(8), ALLOCATABLE, PRIVATE :: KO(:,:)
      REAL(8), ALLOCATABLE, PRIVATE :: EIVAL(:)
      REAL(8), ALLOCATABLE, PRIVATE :: VR(:,:)
      REAL(8), PRIVATE :: ax     ! coefficients used of the  
      REAL(8), PRIVATE :: bx     ! quadratic and cubic equations
      REAL(8), PRIVATE :: cx     ! ATX and ASWX
      REAL(8), PRIVATE :: Qx     ! coefficients used to solve for 
      REAL(8), PRIVATE :: Rx     ! the roots of the cubic equation
      REAL(8), PRIVATE :: ThetaX ! in ATX
      REAL(8), PRIVATE :: ev1    ! Temporary variables used to 
      REAL(8), PRIVATE :: ev2    ! calculate the eigen vectors 
      REAL(8), PRIVATE :: ev3    ! in ATX and ASWX
      REAL(8), PRIVATE :: evmax  !
      REAL(8), PRIVATE :: DetKO  ! Variables used to solve for the 
      REAL(8), PRIVATE :: DetK1  ! non-homogeneous part of the solution
      REAL(8), PRIVATE :: DetK2  ! a system of equations in ATX and
      REAL(8), PRIVATE :: DetK3  ! ASWX using Cramer's Rule
      REAL(8), PRIVATE :: DetEV  ! Variables used to solve for the 
      REAL(8), PRIVATE :: DetE1  ! integration constants in the 
      REAL(8), PRIVATE :: DetE2  ! system of equations in ATX and
      REAL(8), PRIVATE :: DetE3  ! ASWX using Cramer's Rule
      
      INTEGER, PRIVATE :: NC
      INTEGER, PRIVATE :: i
      INTEGER, PRIVATE :: j
      
      REAL(8), ALLOCATABLE, PRIVATE :: B( : )   ! Surface media concentration vector
      REAL(8), ALLOCATABLE, PRIVATE :: NHS( : ) ! non-homogenious solution

      INTEGER, SAVE, PRIVATE :: dt  ! internal model time step

C input/output parameters

      INTEGER, PRIVATE :: N_AQ_CONC  ! aqueous media concentrations
      INTEGER, PRIVATE :: N_GAS_CONC ! gaseous media concentrations
      INTEGER, PRIVATE :: N_SOL_CONC ! solid media concentrations

      CHARACTER( 16 ), ALLOCATABLE, PRIVATE :: MEDIA_NAMES( : )

      REAL(8), ALLOCATABLE, PRIVATE :: MLAI( :,: ) ! used to track change in LAI

      CONTAINS

         SUBROUTINE INIT_HGSIM( JDATE, JTIME )

         USE HGRD_DEFN           ! horizontal grid specifications
         USE UTILIO_DEFN
         USE ASX_DATA_MOD
         USE LSM_MOD
         USE Bidi_Mod

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER, INTENT( IN ) :: JDATE
         INTEGER, INTENT( IN ) :: JTIME

         INTEGER      GXOFF, GYOFF              ! global origin offset from file
         integer, save :: loc_strtcol, loc_endcol, loc_strtrow, loc_endrow
         INTEGER, SAVE :: loc_STRTCOLGC2, loc_ENDCOLGC2, loc_STRTROWGC2, loc_ENDROWGC2

         CHARACTER( 16 ) :: PNAME = 'INIT_HGSIM'
         CHARACTER( 96 ) :: MSG = ' '

         INTEGER  V, L, C, R

C--------------------------------------------------------------------------
         INIT_LAI = .TRUE.

         IF( .NOT. ALLOCATED (MLAI) ) THEN
            ALLOCATE( MLAI(NCOLS,NROWS) )
            MLAI( :,: ) = 0.0
         END IF
         IF ( .NOT. ALLOCATED ( fevgrn ) ) THEN
            ALLOCATE ( fevgrn( NCOLS,NROWS ) )
            fevgrn( :,: ) = 0.0
         END IF

!         SELECT CASE( LAND_SCHEME )
!            CASE( 'USGS24' )
!               ALLOCATE (  Hglu_fac( n_lufrac ) )
!               Hglu_fac = HGLU_FAC_USGS            
         DO C = 1, NCOLS
            DO R = 1, NROWS
               DO L = 1, N_LUFRAC
                  IF(CAT_LU(L) .EQ. 'EVEFOR') THEN
                     fevgrn(c,r) = fevgrn(c,r) + Grid_Data%lufrac(c,r,l)
                  End IF
                  IF(CAT_LU(L) .EQ. 'MIXFOR') THEN
                     fevgrn(c,r) = fevgrn(c,r) + Grid_Data%lufrac(c,r,l)
                  End IF                                    
               END DO
            END DO
         END DO
!            CASE( 'MODIS' )
!               ALLOCATE (  Hglu_fac( n_lufrac ) )
!               Hglu_fac = HGLU_FAC_MODIS
!               DO C = 1, NCOLS
!                   DO R = 1, NROWS
!                     fevgrn(c,r) = lufrac(1,c,r)+lufrac(2,c,r)+0.5*lufrac(5,c,r)
!                  END DO
!               END DO
!             CASE( 'NLCD40' )
!               ALLOCATE (  Hglu_fac( n_lufrac ) )
!               Hglu_fac = HGLU_FAC_NLCD40
!               DO C = 1, NCOLS
!                   DO R = 1, NROWS
!                      fevgrn(c,r) = lufrac(1,c,r) + lufrac(2,c,r) +
!     &                              0.5*lufrac(5,c,r) + lufrac(29,c,r) +
!     &                              0.5*lufrac(30,c,r)
!                  END DO
!               END DO
!             CASE( 'NLCD50' )
!               ALLOCATE (  Hglu_fac( n_lufrac ) )
!               Hglu_fac = HGLU_FAC_NLCD50
!               DO C = 1, NCOLS
!                   DO R = 1, NROWS
!                      fevgrn(c,r) = lufrac(10,c,r) + 0.5*lufrac(11,c,r) +
!     &                              lufrac(32,c,r) + lufrac(32,c,r) + 
!     &                              0.5*lufrac(36,c,r)
!                  END DO
!               END DO
!            CASE DEFAULT
!               xmsg = 'Land use scheme not supported'
!               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
!         END SELECT

C **** Read in saved surface layer concentrations
#ifdef mpas
         INIT_ASX = .FALSE.
         INIT_ATX = .FALSE.
#else
         IF ( .NOT. MEDC_AVAIL ) THEN
            INIT_ASX = .TRUE.
            INIT_ATX = .TRUE.

         ELSE

            INIT_ASX = .FALSE.
            INIT_ATX = .FALSE.

         END IF ! load surface arrays
#endif

         RETURN

!------------------------------------------------------------------------------
! Error handeling section
!------------------------------------------------------------------------------
1001     CONTINUE
         CALL M3EXIT( pname, jdate, jtime, xmsg, xstat1 )
C-------------------------------------------------------------------------------
C Format statements.
C-------------------------------------------------------------------------------

9001     FORMAT( 'Failure reading ', a, 1x, 'from ', a )

         RETURN

         END SUBROUTINE INIT_HGSIM

         SUBROUTINE ATX (rbc, rcut, rwetsfc, rinc, rsnow, rgw, ifsnow, xm, 
     &                   dvel, HG, H, dpvd, del, tstep, c, r, l, jdate, jtime )

! test program to find and return eigenvalues and eigenvectors for a coupled
! land - surface echange model using Intels math kernel library (MKL) linear
! algebra functions

         USE HGRD_DEFN           ! horizontal grid specifications
         USE DEPVVARS
         USE LSM_MOD
         USE UTILIO_DEFN
         USE BIDI_MOD
         USE ASX_DATA_MOD

C Includes:

!         INCLUDE SUBST_CONST     ! constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments passed to and from m3dry

         REAL, INTENT( IN )   :: rbc     ! laminar boundary layer resistance
         REAL, INTENT( IN )   :: rcut    ! cuticle resistance
         REAL, INTENT( IN )   :: rwetsfc ! wet surface resistance
         REAL, INTENT( IN )   :: rinc    ! in canopy resistance
         REAL, INTENT( IN )   :: rsnow
         REAL, INTENT( IN )   :: rgw     ! wet soil resistance
         REAL, INTENT( IN )   :: xm
         REAL, INTENT( IN )   :: HG
         REAL, INTENT( IN )   :: H
         REAL, INTENT( IN )   :: del

         REAL, INTENT( OUT )  :: dpvd     ! evasion
         REAL, INTENT( OUT )  :: dvel     ! deposition velocity

         INTEGER, INTENT( IN ) :: c
         INTEGER, INTENT( IN ) :: r
         INTEGER, INTENT( IN ) :: l
         INTEGER, INTENT( IN ) :: jdate
         INTEGER, INTENT( IN ) :: jtime
         INTEGER, INTENT( IN ) :: tstep
         INTEGER, INTENT( IN ) :: ifsnow

         CHARACTER( 96 ) :: XMSG = ' '

         CHARACTER( 16 ), PARAMETER :: pname      = 'ATX'

         REAL(8)   :: vdHG   ! Elemental Hg deposition velocity
         REAL(8)   :: vdHGct   ! Hg cuticular transfer velocity
         REAL(8)   :: vdHGst  ! stomatal transfer velocity
         REAL(8)   :: vdHGsl
         REAL(8)   :: pdHgsl
         REAL(8)   :: rgnd

         REAL(8) :: ZC, ZM ! depth of model surface media

!Model concentrations

         REAL(8), SAVE :: Cc         ! cuticular Hg(0) concentrations
         REAL(8), SAVE :: Cm         ! mesophyll Hg(0) concentrations
         REAL(8), SAVE :: CgHg       ! Soil Hg(0) concentrations
         REAL(8), SAVE :: CHgzo      ! Hg(0) concentrations at z = zo
         REAL(8), SAVE :: CgHgII0    ! Soil Hg(II) concentrations
         REAL(8) :: vdHgt   ! sum of Hg deposition velocities
!********* reduction and partioning terms *******************************
         REAL(8) :: kr      ! soil divalent mercury reduction term
         REAL(8) :: Kam     ! air mesophyll partitioning coefficient for Hg(II)
         REAL(8) :: Kac     ! air cuticule partitioning coeffiecient for Hg(II)
         REAL(8) :: Kow     ! HgCl2 Octanol water partioning coefficient
         REAL(8) :: Kpwc    ! Hg(0) air-vegetation surface partitioning coefficient
         REAL(8) :: Kpwm    ! Hg(0) air-mesophyll partitioning coefficient
!********* vegetation poperties *****************************************
         REAL(8) :: lm   ! leaf mesophyll lipid fraction
         REAL(8) :: lc   ! cuticular wax mesophyll lipid fraction
         REAL(8) :: Wp   ! water content fraction of the leaf
         REAL(8) :: bc   ! Emprical coefficeint to describe differences in plant lipids
         REAL(8) :: flai ! Factor in mercury accumulation due to biodilution or scenescence
!********* Intermediate concentration variables *********************
         REAL, SAVE :: Hgm
         REAL, SAVE :: Hgc
         REAL, SAVE :: Hgs
!******** soil properties ***********************************************
         REAL(8), PARAMETER :: kvis_d = REAL(kvis,8) ! [cm^2 / s] at 273.15K
         REAL(8)            :: wg_min         ! minimum soil moisture content
         REAL(8)            :: ldry           ! diffusion length
         Real(8)            :: rbg            ! soil boundary layer resistance
         REAL(8)            :: scn            ! for Rbg
         REAL(8)            :: ustg           ! for Rbg
         REAL(8)            :: del0           ! for Rbg
         Real(8)            :: dp             ! for Rsoil
         INTEGER            :: ist            ! soil type
!********* Unit conversions *********************
         REAL(8) :: M3MOLVOL ! molar volume of air at stp m3/mol
!********* Variables used to handle an over determined system
         LOGICAL :: EV23 ! eigen values 1 and 3 are the same 

         M3MOLVOL = MOLVOL/1.0D3
         
         IF( INIT_LAI ) THEN
            MLAI( c,r ) = Met_Data%LAI(c,r)
            flai = 1.0D0
         END IF         
         IF( INIT_ATX ) THEN
! Equilibrium Hg(0) mesophyll concentration in a 5 month box model simulation
! umol/g leaf dry matter
            Cm     = fevgrn(c,r)*6.0D-6 + (1.0D0-fevgrn(c,r))*CMEDIA(c,r,5)
! Cuticular Hg(0) concentration in a 5 month box model simulation
            Cc     = fevgrn(c,r)*6.0D-7 + (1.0D0-fevgrn(c,r))*CMEDIA(c,r,6)
! Initialize at ambient concentration (zero flux condition)
            CgHg   = HG  ! ng/g bulk soil concentration
            CHgzo  = HG  ! ppm compensation point
         ELSE
            IF( MLAI(c,r) .EQ. 0.0 ) THEN
               flai = 1.0D0
            ELSE
               flai = max(Met_Data%LAI(c,r)/MLAI( c,r ),1.0D0) ! bio dilution
            END IF
            IF(flai .GT. 3.0) WRITE(Logdev,*) 'LAI factor: ', flai
            MLAI( c,r ) = Met_Data%LAI(c,r)
            Cm     = CMEDIA( c,r,5 )/flai  ! ng/g bulk leaf concentration
            Cc     = CMEDIA( c,r,6 )/flai  ! ng/g bulk leaf concentration
            CgHg   = CMEDIA( c,r,3 )  ! ng/g bulk soil concentration
         END IF

         dt     = tstep

! Model layer depths

         ZC     = 7.1D1* REAL(Met_Data%LAI(c,r),8) ! g/m**2 based off of leaf litter fall
         ZM     = 7.1D1* REAL(Met_Data%LAI(c,r),8) ! measurements at UCONN's experimental
! forest Bash and Miller 2009 Atmos. Environ.

!***************** canopy parameters *********************************
         Kow  = 4.15D0    ! For Hg, Mason 1996
         lm   = 2.0D-2    ! From Trapp and Matthis 1995
         lc   = 2.0D-2    ! Assumed cuticular wax lipid content
         Wp   = 8.0D-1     ! leaf water fraction, Trapp and Mathis 1996
         bc   = 9.5D-1   ! For barley, Trapp and Mathis 1996
         Kpwc = (Wp+lc*1.0D0/8.22D-1*Kow**bc)*MWWAT                 ! g/umol
         Kpwm = (Wp+lm*1.0D0/8.22D-1*Kow**bc)*MWWAT                 ! g/umol
! Partitioning coeficients following the methodology of the PEM model
         kac = (Kpwc*(1.0D0-del)) +  ! evasion from dry cuticles
     &         Kpwc*del*H        ! g/mol cuticle surface
         kam = Kpwm*H            ! g/mol apoplast solution
!**************** soil parameters ************************************
! Compute quasi-laminar boundary layer resistance at the soil surface
         scn  = kvis / dif0(l)
         ustg = max(Met_Data%Ustar(c,r) * EXP(-REAL(Met_Data%LAI(c,r),8)), 1.0D-3)
         del0 = 1.0D-4 * kvis / ( karman * ustg )
         rbg  = ( scn - LOG( 1.0D1 * del0 ) ) / ( karman * ustg )
! Compute soil resistance
         ist = Grid_Data%SLTYP(c,r)
         wg_min = REAL(MAX(Met_Data%SOIM1(c,r),Grid_Data%Wres(c,r)),8)
         ldry= MAX(ZG*(EXP((1.0D0-wg_min/Grid_Data%Wsat(c,r))**5)-1.0D0)/1.718D0,1.0D-12)
         dp  = dif0(l)*1.D-4 * Grid_Data%Wsat(c,r)**2 
     &       *(1.0D0-Grid_Data%Wres(c,r)/Grid_Data%Wsat(c,r))**(2.0D0+3.0D0/Grid_Data%Bslp(c,r))
! Soil divalent mercury reduction rate following Scholz et al 2003
         IF(Met_Data%SOIT1(c,r) .GT. 273.15) THEN
            kr     = 8.0D-11
            rgnd   = ldry/dp
         ELSE ! if the soil is frozen limit diffusion and reduction
            kr     = 0.0D0
            rgnd   = 1.0D6
         END IF

         cgHgII0 = 0.0D0

         DO i = 1, n_lufrac
            cgHgII0 = cgHgII0+hglu_fac(i)*Grid_data%lufrac(c,r,i)
         END DO
C Set floor to smallest terrestrial value
         cgHgII0 = MAX( cgHgII0, 1.8D1 )          

C diffusion through soil from Scholtz et al. 2003
         vdHg   = 1/(REAL(Met_Data%RA(c,r),8)+5.0D-1*rinc)
         vdHgst = 1.0D0/( rbc + REAL(Met_Data%RS(c,r),8) )
         vdHgct = REAL(Met_Data%LAI(c,r),8)*(( 1.0D0 - del  )/( rbc + rcut )
     &            + ( del )/(rbc + rwetsfc ))
         vdHgsl = REAL(Met_Data%VEG(c,r),8)/( rbg + rgnd + 5.0D-1*rinc)
     &            + ((1-ifsnow) *( 1.0D0 - REAL(Met_Data%VEG(c,r),8) )*( 1.0D0-del ))/(rbg + rgnd )
     &            + (del * (1.0D0-ifsnow))/(rbg + rgw )
     &            + (ifsnow*(1.0D0 - xm))/( rbg + rsnow )
     &            + (xm*ifsnow)/(rbc + rsndiff + rgw)
         pdHgsl = 1.0D0/(REAL(Met_Data%RA(c,r),8) + 1.0D0/vdHgsl )     ! production term with no canopy

         vdHgt = vdHg + vdHgst + vdHgct + vdHgsl

         CHgzo = (vdHg*Hg+vdHgst/kam*Cm+vdHgct/kac*Cc+vdHgsl/H*cgHg)
     &                    /vdHgt

         dpvd = Met_Data%veg(c,r) * vdHg * CHgzo + (1.0D0-REAL(Met_Data%VEG(c,r),8)) * pdHgsl/H * CgHg

         dvel = pdHgsl + REAL(Met_Data%VEG(c,r),8) *(vdhg-pdHgsl)

! Load array A
         NC = 3

         ALLOCATE ( KO(NC,NC), VR(NC,NC), EIVAL(NC))

         KO = 0.0D0

         KO(1,1) = -vdHgst/(ZM*kam*M3MOLVOL)*(1.0D0-vdHgst/(kam*vdHgt))
         KO(1,2) =  vdHgst/(ZM*M3MOLVOL)*vdHgct/(kac*vdHgt)
         KO(1,3) =  vdHgst/(ZM*M3MOLVOL)*vdHgsl/(H*vdHgt)
         KO(2,1) =  vdHgct/(ZC*M3MOLVOL)*vdHgst/(kam*vdHgt)
         KO(2,2) = -vdHgct/(ZC*kac*M3MOLVOL)*(1.0D0-vdHgct/(kac*vdHgt))
         KO(2,3) =  vdHgct/(ZC*M3MOLVOL)*vdHgsl/(H*vdHgt)
         KO(3,1) =  vdHgsl/(ZG*wg_min)*vdHgst/(kam*vdHgt)
         KO(3,2) =  vdHgsl/(ZG*wg_min)*vdHgct/(kac*vdHgt)
         KO(3,3) = -vdHgsl/(ZG*wg_min*H)*(1.0D0-vdHgsl/(H*vdHgt))         

         ALLOCATE( NHS(NC))

         NHS = 0.0D0

!        load the non-homogenious part of the system of equations
         NHS(1) = -vdHgst/ZM*(vdHg*HG/M3MOLVOL)/vdHgt
         NHS(2) = -vdHgct/ZC*(vdHg*HG/M3MOLVOL)/vdHgt
         NHS(3) = -vdHgsl/(ZG*wg_min)*(vdHg*HG)/vdHgt 
     &            -kr*rhob(ist)*cgHgII0*(ZG)/(1.0D3*2.0059D2)     
         
         ALLOCATE( B(NC))

         B = 0.0D0

!        Load the initial conditions

         B(1) = real( Cm,   8 )
         B(2) = real( Cc,   8 )
         B(3) = real( CgHg, 8 )

!*****************************************************************************
! Get eigen values and vectors where the cubic equation is:
! lambda**3+ax*lambda**2+bx*lambda+cx = 0
! and is solved following Numerical recipies for Fortran equations 5.6.10-5.6.12
!*****************************************************************************

         ax = -(KO(1,1)+KO(2,2)+KO(3,3))
 
         bx = -(KO(2,3)*KO(3,2)+KO(2,1)*KO(1,2)+
     &          KO(3,1)*KO(1,3)-KO(1,1)*KO(2,2)-
     &          KO(1,1)*KO(3,3)-KO(2,2)*KO(3,3))
         
         cx = (KO(1,1)*KO(2,3)*KO(3,2)+
     &         KO(3,1)*KO(1,3)*KO(2,2)+
     &         KO(2,1)*KO(1,2)*KO(3,3)-
     &         KO(3,1)*KO(1,2)*KO(2,3)-
     &         KO(2,1)*KO(1,3)*KO(3,2)-
     &         KO(1,1)*KO(2,2)*KO(3,3))
     
         Qx = (ax**2.0D0-3.0D0*bx)/9.0D0
         Rx = (2.0D0*ax**3.0D0-9.0D0*ax*bx+27.0D0*cx)/54.0D0 

! There will always be three real roots in this system
! so we can use the simple geometric solution for a 
! cubic equation.
         IF( Rx/DSQRT(Qx**3) .LT. 1.0D0 ) THEN
            ThetaX   =  DACOS(Rx/DSQRT(Qx**3))
            EIVAL(1) = -2.0D0*sqrt(Qx)*DCOS(ThetaX/3.0D0)-ax/3.0D0
            EIVAL(2) = -2.0D0*sqrt(Qx)*DCOS((ThetaX+2.0D0*Pi)/3.0D0)-ax/3.0D0
            EIVAL(3) = -2.0D0*sqrt(Qx)*DCOS((ThetaX-2.0D0*Pi)/3.0D0)-ax/3.0D0
! Solve for the eigenvectors by setting the first element to 1 and using
! Cramer's rule to solve the second and third elements using the first
! two equations          
            DO i = 1, NC
               ev1 =   1.0D0
               ev2 = (KO(2,1)*KO(1,3)-(KO(1,1)-EIVAL(i))*KO(2,3))/
     &               (KO(1,2)*KO(2,3)-(KO(2,2)-EIVAL(i))*KO(1,3))  
               ev3 = ((KO(1,1)-EIVAL(i))*(KO(2,2)-EIVAL(i))-KO(2,1)*KO(1,2))/
     &               (KO(1,2)*KO(2,3)-(KO(2,2)-EIVAL(i))*KO(1,3))
               evmax = max(abs(ev1),abs(ev2),abs(ev3))
! scale the eigenvector
               VR(1,i) = ev1/evmax
               VR(2,i) = ev2/evmax
               VR(3,i) = ev3/evmax         
            END DO 
! Rounding error can lead to Rx**2 > Qx**2 this usually indicates that two of the Eigen values are 
! equivalent Rx**2 = Qx**3 or there are complex roots in which the model will crash
         ELSE
            ThetaX   =  0.0D0
            EIVAL(1) = -2.0D0*sqrt(Qx)*DCOS(ThetaX/3.0D0)-ax/3.0D0
            EIVAL(2) = -2.0D0*sqrt(Qx)*DCOS((ThetaX+2.0D0*Pi)/3.0D0)-ax/3.0D0
            EIVAL(3) = -2.0D0*sqrt(Qx)*DCOS((ThetaX-2.0D0*Pi)/3.0D0)-ax/3.0D0
            DO i = 1, NC
               ev1 =   1.0D0
               ev2 = (KO(2,1)*KO(1,3)-(KO(1,1)-EIVAL(i))*KO(2,3))/
     &               (KO(1,2)*KO(2,3)-(KO(2,2)-EIVAL(i))*KO(1,3))  
               ev3 = ((KO(1,1)-EIVAL(i))*(KO(2,2)-EIVAL(i))-KO(2,1)*KO(1,2))/
     &               (KO(1,2)*KO(2,3)-(KO(2,2)-EIVAL(i))*KO(1,3))
               evmax = max(abs(ev1),abs(ev2),abs(ev3))
! scale the eigenvector
               VR(1,i) = ev1/evmax
               VR(2,i) = ev2/evmax
               VR(3,i) = ev3/evmax        
            END DO   
! Two roots are the same and independent eigenvectors need to be found 
! simply select a different element of the vector to be unity
            ev1 = ( KO(1,3)*(         KO(2,2)-EIVAL(3))-KO(1,2)*KO(2,3))/
     &            ((KO(1,1)-EIVAL(3))*KO(2,3)-          KO(1,3)*KO(2,1))
            ev2 = -1.0D0  
            ev3 = ( KO(2,1)*          KO(1,2)-(KO(1,1)-EIVAL(3))*(KO(2,2)-EIVAL(3)))/
     &            ((KO(1,1)-EIVAL(3))*KO(2,3)- KO(1,3)*           KO(2,1))
            evmax = max(abs(ev1),abs(ev2),abs(ev3))
! scale the eigenvector
            VR(1,3) = ev1/evmax
            VR(2,3) = ev2/evmax
            VR(3,3) = ev3/evmax
         END IF

C******************************************************************************
C******* Find the non homogenious solution ************************************
C******************************************************************************

! solve for KO*x = NHS using Cramer's Rule

         DetKO = KO(1,1)*KO(2,2)*KO(3,3)-KO(1,1)*KO(3,2)*KO(2,3)+
     &           KO(2,1)*KO(3,2)*KO(1,3)-KO(2,1)*KO(1,2)*KO(3,3)+
     &           KO(3,1)*KO(1,2)*KO(2,3)-KO(3,1)*KO(2,2)*KO(1,3)     

         DetK1 = NHS(1)*KO(2,2)*KO(3,3)-NHS(1)*KO(3,2)*KO(2,3)+
     &           NHS(2)*KO(3,2)*KO(1,3)-NHS(2)*KO(1,2)*KO(3,3)+
     &           NHS(3)*KO(1,2)*KO(2,3)-NHS(3)*KO(2,2)*KO(1,3)    
     
         DetK2 = KO(1,1)*NHS(2)*KO(3,3)-KO(1,1)*NHS(3)*KO(2,3)+
     &           KO(2,1)*NHS(3)*KO(1,3)-KO(2,1)*NHS(1)*KO(3,3)+
     &           KO(3,1)*NHS(1)*KO(2,3)-KO(3,1)*NHS(2)*KO(1,3)  
     
         DetK3 = KO(1,1)*KO(2,2)*NHS(3)-KO(1,1)*KO(3,2)*NHS(2)+
     &           KO(2,1)*KO(3,2)*NHS(1)-KO(2,1)*KO(1,2)*NHS(3)+
     &           KO(3,1)*KO(1,2)*NHS(2)-KO(3,1)*KO(2,2)*NHS(1)   
     
         NHS(1) = DetK1/DetKO
         NHS(2) = DetK2/DetKO
         NHS(3) = DetK3/DetKO

C******************************************************************************
C*** Update the IC's for the Non-homogenious solutions and solve the system ***
C******************************************************************************

!        Subtract the non-homogenious solution from the 
         DO i = 1, NC
            B(i) = B(i)-NHS(i)
         END DO
!        Solve x for VR*x=B using Cramer's Rule

         DetEV = VR(1,1)*VR(2,2)*VR(3,3)-VR(1,1)*VR(3,2)*VR(2,3)+
     &           VR(2,1)*VR(3,2)*VR(1,3)-VR(2,1)*VR(1,2)*VR(3,3)+
     &           VR(3,1)*VR(1,2)*VR(2,3)-VR(3,1)*VR(2,2)*VR(1,3)  
 
         DetE1 = B(1)*VR(2,2)*VR(3,3)-B(1)*VR(3,2)*VR(2,3)+
     &           B(2)*VR(3,2)*VR(1,3)-B(2)*VR(1,2)*VR(3,3)+
     &           B(3)*VR(1,2)*VR(2,3)-B(3)*VR(2,2)*VR(1,3) 
     
         DetE2 = VR(1,1)*B(2)*VR(3,3)-VR(1,1)*B(3)*VR(2,3)+
     &           VR(2,1)*B(3)*VR(1,3)-VR(2,1)*B(1)*VR(3,3)+
     &           VR(3,1)*B(1)*VR(2,3)-VR(3,1)*B(2)*VR(1,3)
     
         DetE3 = VR(1,1)*VR(2,2)*B(3)-VR(1,1)*VR(3,2)*B(2)+
     &           VR(2,1)*VR(3,2)*B(1)-VR(2,1)*VR(1,2)*B(3)+
     &           VR(3,1)*VR(1,2)*B(2)-VR(3,1)*VR(2,2)*B(1)  
     
         B(1) = DetE1/DetEV        
         B(2) = DetE2/DetEV        
         B(3) = DetE3/DetEV   

! update the surface array
         Hgm = 0.0
         Hgc = 0.0
         Hgs = 0.0

         DO i = 1, NC
            Hgm = Hgm + B(i) * VR(1,i) * DEXP( EIVAL(i) * dt )
            Hgc = Hgc + B(i) * VR(2,i) * DEXP( EIVAL(i) * dt )
            Hgs = Hgs + B(i) * VR(3,i) * DEXP( EIVAL(i) * dt )
         END DO

         Hgm   = Hgm + NHS(1)
         Hgc   = Hgc + NHS(2)
         Hgs   = Hgs + NHS(3) 
 
         IF ( Hgm .LT. 0.0 ) THEN

            XMSG = '*** Negative concentration in Hgm resetting it to zero ***'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            Hgm = max(Hgm,0.0)

         END IF

         IF ( Hgc .LT. 0.0 ) THEN

            XMSG = '*** Negative concentration in Hgc resetting it to zero ***'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            Hgc = max(Hgc, 0.0)

         END IF

         IF ( Hgs .LT. 0.0 ) THEN
! This can happen when the soil moisture approaches zero limiting the Hg that 
! can evade from this source.
            XMSG = '*** Negative concentration in Hgs resetting it to zero flux condition ***'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            WRITE(LOGDEV,*) 'wg  :',wg_min, 'Hgs :', Hgs, 'H :',H
            WRITE(LOGDEV,*) 'Col :',c, 'Row :', r
            Hgs = CHgzo*H 

         END IF

         IF ( Hgm .NE. Hgm .OR. Hgc .NE. Hgc .OR. Hgs .NE. Hgs ) THEN
            XMSG = '*** NaN in Hgs, Hgc, or Hgm ***'
            WRITE(LOGDEV,*) 'Col',c,'Row',r
            WRITE(LOGDEV,*) 'Hgm',Hgm,'Hgc',Hgc,'Hgs',Hgs
            WRITE(LOGDEV,*) 'B      :',B
            WRITE(LOGDEV,*) 'NHS    :',NHS
            WRITE(LOGDEV,*) 'KO     :',KO
            WRITE(LOGDEV,*) 'cgHgII0: ',cgHgII0
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1)
         END IF

         CMEDIA( c,r,1 ) = 0.0 ! water
         CMEDIA( c,r,2 ) = 0.0 ! water
         CMEDIA( c,r,3 ) = Hgs
         CMEDIA( c,r,4 ) = CHgzo
         CMEDIA( c,r,5 ) = Hgm
         CMEDIA( c,r,6 ) = Hgc

         DEALLOCATE( KO, VR, EIVAL )

         DEALLOCATE( B, NHS )

         RETURN

         END SUBROUTINE ATX

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C::::::: Air surface water exchange subroutine :::::::::::::::::::::::::::
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

         SUBROUTINE ASWX( Hg, HgIIgas, vdHg, vdHgII, awhg,
     &                    dpvd, c, r, JDATE, JTIME, TSTEP )

         USE HGRD_DEFN           ! horizontal grid specifications
         USE DEPVVARS
         USE UTILIO_DEFN
         USE BIDI_MOD
         USE ASX_DATA_MOD

C Includes:

!         INCLUDE SUBST_CONST     ! constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER, INTENT( IN )   :: JDATE
         INTEGER, INTENT( IN )   :: JTIME
         INTEGER, INTENT( IN )   :: TSTEP
         REAL,    INTENT( IN )   :: Hg
         REAL,    INTENT( IN )   :: HgIIgas
         REAL,    INTENT( IN )   :: vdHg
         REAL,    INTENT( IN )   :: vdHgII
         REAL,    INTENT( IN )   :: awhg
         REAL,    INTENT( OUT )  :: dpvd
         INTEGER, INTENT( IN )   :: c
         INTEGER, INTENT( IN )   :: r

         CHARACTER( 16 ), PARAMETER :: pname      = 'ASWX'

C*************************** Ocean box parameters ***********************
         REAL(8), PARAMETER :: satten = 7.58d-1
C*************************** Model concentrations ***********************
         REAL(8), SAVE :: cDGM
         REAL(8), SAVE :: cHgIIaq
C***** reduction and partioning terms from Whalin et al 2007 ************
         REAL(8), PARAMETER :: rref  = 240d+0    ! referance incoming radiation
                                           ! for redox measurements (w/m2)
         REAL(8), PARAMETER :: kphot = 6.5d-4 ! drm photoreduction rate 1/s
         REAL(8), PARAMETER :: kox   = 7.2d-4 ! dgm photo-oxidation rate 1/s
C********* Intermediate concentration variables *********************
         REAL(8), SAVE :: DGM
         REAL(8), SAVE :: DRM


         IF (INIT_ASX ) THEN
            cDGM    = Hg*awhg*3 ! assume 3x eq con.
            cHgIIaq = 3.57e-6   ! from Whalin et al 2007
         ELSE
            cDGM    = CMEDIA( c,r,1 )
            cHgIIaq = CMEDIA( c,r,2 )
         END IF

         dt   = TSTEP

         dpvd = cDGM * vdHg/awhg

         IF ( Met_Data%RGRND(c,r) .LT. 1e-3 ) THEN
C the aqueous elemental and divalent Hg pools become decoupled and the
C matrices become singular warrenting an alternative solution

C Find a simple one box solution for elemental Hg

            DGM = Hg*awhg + (cDGM - Hg*awhg)*DEXP(-vdHg/(ZSURF*awhg)*dt)

C in the absence of photo-redox reactions divalent Hg accumulates
            DRM = cHgIIaq + vdHgII/ZSURF*HgIIgas*dt

         ELSE

            NC    = 2

            ALLOCATE ( KO(NC,NC), VR(NC,NC), EIVAL(NC) )

            KO = 0.0D0

C 240 w/m**2 is the 'typical light spectrum' from Whalin et al 2007 Marine Chem.
C attenuation at 1 m = 1/K (1-exp(-K Z)) = 0.758 using a K of 0.58


            KO( 1,1 )   = -vdhg / ( ZSURF * awhg )
     &                    -kox *   satten * Met_Data%RGRND(c,r)/rref
            KO( 1,2 )   =  kphot * satten * Met_Data%RGRND(c,r)/rref
            KO( 2,1 )   =  kox *   satten * Met_Data%RGRND(c,r)/rref
            KO( 2,2 )   = -kphot * satten * Met_Data%RGRND(c,r)/rref

            ALLOCATE( NHS(NC))

            NHS = 0.0

            NHS(1) = -vdHg/ZSURF*HG
            NHS(2) = -vdHgII/ZSURF*HgIIgas

            ALLOCATE( B(NC))

            B = 0.0

            B( 1 ) = REAL( cDGM,    8)   
            B( 2 ) = REAL( cHgIIaq, 8)


C*****************************************************************************
! Get eigen values and vectors where the cubic equation is:
! ax*lambda**2+bx*lambda+cx = 0
! and is solved following Numerical recipies for Fortran equations 5.6.2-5.6.5
C*****************************************************************************

            ax = 1.0D0
            bx = -(KO(1,1)+KO(2,2))
            cx = KO(1,1)*KO(2,2)-KO(1,2)*KO(2,1)
    
            Qx = -5.0D-1*(bx+SIGN(1.0D0,bx)*DSQRT(bx**2-4.0D0*ax*cx))
    
            EIVAL(1) = Qx/ax
            EIVAL(2) = cx/Qx
    
! Solve for the eigenvectors
         DO i = 1, NC
            ev1   = 1.0D0
            ev2   = -(KO(2,1)*ev1)/(KO(2,2)-EIVAL(i))
            evmax = max(abs(ev1),abs(ev2))
! scale the eigenvector
            VR(1,i) = ev1/evmax
            VR(2,i) = ev2/evmax
         END DO

C******************************************************************************
C******* Do the non homogenious part ******************************************
C******************************************************************************

! solve for x in  KO*x = NHS using Cramer's Rule

         DetKO  = KO(1,1)*KO(2,2)-KO(1,2)*KO(2,1)  
 
         DetK1 = NHS(1)*KO(2,2)-NHS(2)*KO(1,2)
         DetK2 = NHS(2)*KO(1,1)-NHS(1)*KO(2,1)
 
         NHS(1) = DetK1/DetKO
         NHS(2) = DetK2/DetKO 

C******************************************************************************
C*** Update the IC's for the Non-homogenious solutions and solve the system ***
C******************************************************************************

            DO i = 1, NC
               B(i) = B(i) - NHS(i)
            END DO

!        Solve for x in VR*x=B using Cramer's Rule

         DetEV  = VR(1,1)*VR(2,2)-VR(1,2)*VR(2,1)
 
         DetE1  = B(1)*VR(2,2)-B(2)*VR(1,2) 
         DetE2  = B(2)*VR(1,1)-B(1)*VR(2,1)
 
         B(1) = DetE1/DetEV
         B(2) = DetE2/DetEV

! update the surface array
            DGM = 0.0D0
            DRM = 0.0D0

            DO i = 1, NC
               DGM = DGM + B(i) * VR(1,i) * DEXP( EIVAL(i) * dt )
               DRM = DRM + B(i) * VR(2,i) * DEXP( EIVAL(i) * dt )
            END DO

            DGM     = DGM   + NHS(1)
            DRM     = DRM   + NHS(2)
    
            DEALLOCATE( KO, VR, EIVAL )

            DEALLOCATE( B, NHS )

         END IF         
    
         IF ( DGM .LT. 0.0 .OR. DRM .LT. 0.0 ) THEN
            
            XMSG = '*** Negative concentration ***'   
            WRITE(LOGDEV,*) 'awhg',awhg,'HG',HG
            WRITE(LOGDEV,*) 'DGM',DGM,'DRM',DRM             
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )

         END IF 

         CMEDIA( c,r,1 ) = DGM
         CMEDIA( c,r,2 ) = DRM
         CMEDIA( c,r,3 ) = 0.0 ! land
         CMEDIA( c,r,4 ) = 0.0 ! land
         CMEDIA( c,r,5 ) = 0.0 ! land
         CMEDIA( c,r,6 ) = 0.0 ! land

         RETURN

         END SUBROUTINE ASWX

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


         IF ( NINT( Grid_Data%lwmask( c,r ) ) .EQ. 0 ) THEN ! water

         ! convert to umol/m2 pulse input
            WDEP_LOAD = WDEP*HAOM2*UGOKG/MWHG
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
