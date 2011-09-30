C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry_mp/HGSIM.F,v 1.1 2011/09/30 19:10:29 sjr Exp $

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
C      12 Aug 2008 J. Bash initial implementation
C       2 Apr 2009 J. Bash for solar irradation on the order of 1e-3 w/m2
C                          the mercury surface water photo redox scheme
C                          became unstable. A conditional statement was
C                          added to correct this instability.
C       4 June 2009 J. Bash Corrected the time stamp on WRASX_MEDIA to be
C                          consistant with other CMAQ modules reported by
C                         (T.Myers)
C     22 Oct  2009 J. Bash Corrected a units conversion error in ASWX and ATX
C                          reported by (P. Pongprueksa) and added a more
C                          robust soil diffusion model adapted from the
C                          Community Land Model 3.5.
C
C  References:
C
C  Bash, J.O. (in press) Description and initial simulaiton of a dynamic
C     bi-directional surface exchange model for mercury in CMAQ, J. Geophys.
C     Res.
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

C    Shared variables

C     Private variables used in this routine and
      REAL, ALLOCATABLE,       PRIVATE :: HgLU_Fac(:)
      Real, ALLOCATABLE, Save, Private :: fevgrn(:,:)  ! fraction of evergreen land use

      REAL, PARAMETER, PRIVATE :: zsurf  = 1 ! ocean slab depth (m)
      REAL, PARAMETER, PRIVATE :: ZG = 0.05

      CHARACTER( 96 ), PRIVATE :: XMSG = ' '
      INTEGER, SAVE,PRIVATE :: LOGDEV
      Character( 80 ), Save, Private   :: LAND_SCHEME

C variable needed for SGEEV eigen value funciton
      REAL, ALLOCATABLE, PRIVATE :: KO(:,:)
      REAL, ALLOCATABLE, PRIVATE :: COEFS(:,:)
      REAL, ALLOCATABLE, PRIVATE :: EIVAL(:)
      REAL, ALLOCATABLE, PRIVATE :: WI(:)
      REAL, ALLOCATABLE, PRIVATE :: WORK(:)
      REAL, ALLOCATABLE, PRIVATE :: VR(:,:)
      REAL, ALLOCATABLE, PRIVATE :: VL(:,:)
      INTEGER, PRIVATE :: NC
      INTEGER, PRIVATE :: LDA
      INTEGER, PRIVATE :: LDVR
      INTEGER, PRIVATE :: LDVL
      INTEGER, PRIVATE :: i
      INTEGER, PRIVATE :: j
      INTEGER, PRIVATE :: LWORK
      INTEGER, PRIVATE :: INFO

C variable needed for SGETRF LU matrix factorization
      REAL,    ALLOCATABLE, PRIVATE :: EVECTS(:,:) ! eigen vectors
      INTEGER, ALLOCATABLE, PRIVATE :: IPIV(:)     ! pivot locations for LU decomp

C variable needed for SGETRS linear system solver function
      REAL, ALLOCATABLE, PRIVATE :: B( :,: )   ! Surface media concentration vector
      REAL, ALLOCATABLE, PRIVATE :: NHS( :,: ) ! non-homogenious solution
      INTEGER, PRIVATE :: NRHS
      INTEGER, PRIVATE :: LDB

      INTEGER, SAVE, PRIVATE :: dt  ! internal model time step

C input/output parameters

      INTEGER, PRIVATE :: N_AQ_CONC  ! aqueous media concentrations
      INTEGER, PRIVATE :: N_GAS_CONC ! gaseous media concentrations
      INTEGER, PRIVATE :: N_SOL_CONC ! solid media concentrations
!      INTEGER, PRIVATE :: N_TOT
!      Integer, Private :: n_lufrac ! number of land use categories

      CHARACTER( 16 ), ALLOCATABLE, PRIVATE :: MEDIA_NAMES( : )

      REAL, ALLOCATABLE, PRIVATE ::  WRMC( :,: ) ! media read/write buffer
!      REAL, ALLOCATABLE, PRIVATE :: CMEDIA( :,:,: ) ! surface layer concs
      REAL, ALLOCATABLE, PRIVATE :: MLAI( :,: ) ! used to track change in LAI
!      Real, Save, Allocatable :: lufrac( :,:,: ) ! land use fraction

      CONTAINS

         SUBROUTINE INIT_HGSIM( JDATE, JTIME )

         USE HGRD_DEFN           ! horizontal grid specifications
!         USE SUBST_MODULES       ! stenex
         USE UTILIO_DEFN
 !        USE GRID_CONF
         USE LSM_MOD
         USE Bidi_Mod!, Only: Media_Names, CMedia, Hg_Tot, INIT_LAI, lufrac

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER, INTENT( IN ) :: JDATE
         INTEGER, INTENT( IN ) :: JTIME
!         INTEGER, INTENT( IN ) :: STRTCOL
!         INTEGER, INTENT( IN ) :: STRTROW
!         INTEGER, INTENT( IN ) :: ENDCOL
!         INTEGER, INTENT( IN ) :: ENDROW

         INTEGER      GXOFF, GYOFF              ! global origin offset from file
         integer, save :: strtcol, endcol, strtrow, endrow
         INTEGER, SAVE :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
!         INTEGER, EXTERNAL  :: TRIMLEN
!         INTEGER, EXTERNAL  :: ENVINT

         CHARACTER( 16 ) :: PNAME = 'INIT_HGSIM'
         CHARACTER( 96 ) :: MSG = ' '

         INTEGER  V, L, C, R

C--------------------------------------------------------------------------
         LOGDEV = INIT3()

         INIT_LAI = .TRUE.

         If( .NOT. ALLOCATED (WRMC) ) Then
            ALLOCATE( WRMC(MY_NCOLS,MY_NROWS) )
            WRMC( :,: ) = 0.0
         End If
         If( .NOT. ALLOCATED (MLAI) ) Then
            ALLOCATE( MLAI(MY_NCOLS,MY_NROWS) )
            MLAI( :,: ) = 0.0
         End If
         IF ( .NOT. ALLOCATED ( fevgrn ) ) THEN
            ALLOCATE ( fevgrn( MY_NCOLS,MY_NROWS ) )
            fevgrn( :,: ) = 0.0
         END IF

         SELECT CASE( LAND_SCHEME )
            CASE( 'USGS24' )
!               n_lufrac = n_usgs
               ALLOCATE (  Hglu_fac( n_lufrac ) )
               Hglu_fac = HGLU_FAC_USGS
               DO C = 1, MY_NCOLS
                  DO R = 1, MY_NROWS
                     fevgrn(c,r) = lufrac(13,c,r)+lufrac(14,c,r)+0.5*lufrac(15,c,r)
                  END DO
               END DO
            CASE( 'MODIS' )
!               n_lufrac = n_modis
               ALLOCATE (  Hglu_fac( n_lufrac ) )
               Hglu_fac = HGLU_FAC_MODIS
               DO C = 1, MY_NCOLS
                   DO R = 1, MY_NROWS
                     fevgrn(c,r) = lufrac(1,c,r)+lufrac(2,c,r)+0.5*lufrac(5,c,r)
                  END DO
               END DO
            CASE( 'NLCD-MODIS' )
!               n_lufrac = n_nlcd
               ALLOCATE (  Hglu_fac( n_lufrac ) )
               Hglu_fac = HGLU_FAC_NLCD
               DO C = 1, MY_NCOLS
                   DO R = 1, MY_NROWS
                      fevgrn(c,r) = lufrac(10,c,r) + 0.5*lufrac(11,c,r) +
     &                              lufrac(32,c,r) + 0.5*lufrac(36,c,r)
                  END DO
               END DO
            CASE DEFAULT
               xmsg = 'Land use scheme not supported'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END SELECT

C **** Read in saved surface layer concentrations
         IF ( .NOT. OPEN3( INIT_MEDC_1, FSREAD3, PNAME ) ) THEN
            INIT_ASX = .TRUE.
            INIT_ATX = .TRUE.

         ELSE

            INIT_ASX = .FALSE.
            INIT_ATX = .FALSE.

            IF ( .NOT. DESC3( INIT_MEDC_1 ) ) THEN
               XMSG = 'Could not get ' // TRIM( INIT_MEDC_1 ) // ' file description'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            CALL SUBHFILE ( INIT_MEDC_1, GXOFF, GYOFF, STRTCOL, ENDCOL, STRTROW, ENDROW )

            DO V = 1, Hg_TOT
               IF( .NOT. Xtract3( INIT_MEDC_1, MEDIA_NAMES( V ), 1, 1, STRTROW,
     &                   ENDROW, STRTCOL, ENDCOL, JDATE, 0, WRMC ) )THEN
                  Write(xmsg,9001) Media_Names( V ), INIT_MEDC_1
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               WRITE(LOGDEV,*) MEDIA_NAMES( V ),' read into surface array'

               DO R = 1, MY_NROWS
                  DO C = 1,MY_NCOLS
                     CMEDIA( V,C,R ) = WRMC( C,R )
                  END DO
               END DO
            END DO

         END IF ! load surface arrays

         DEALLOCATE( WRMC )

         RETURN

!------------------------------------------------------------------------------
! Error handeling section
!------------------------------------------------------------------------------
1001     Continue
         Call m3exit( pname, jdate, jtime, xmsg, xstat1 )
C-------------------------------------------------------------------------------
C Format statements.
C-------------------------------------------------------------------------------

9001     Format( 'Failure reading ', a, 1x, 'from ', a )

         Return

         END SUBROUTINE INIT_HGSIM

         SUBROUTINE ATX (diff, ra, rbc, rstom, rcut, rwetsfc, rinc, rsnow, rsndiff,
     &                   rgw, ifsnow, xm, dvel, HG, H, LAI, dpvd, del, TS1CM,
     &                   wg, tstep, c, r, veg, sltype, jdate, jtime, ustar )

! test program to find and return eigenvalues and eigenvectors for a coupled
! land - surface echange model using Intels math kernel library (MKL) linear
! algebra functions

         USE HGRD_DEFN           ! horizontal grid specifications
         USE DEPVVARS
         USE LSM_MOD
         USE UTILIO_DEFN
         USE BIDI_MOD

C Includes:

         INCLUDE SUBST_CONST     ! constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments passed to and from m3dry

         REAL, INTENT( IN )   :: diff    ! Elemental Hg diffusivity
         REAL, INTENT( IN )   :: ra      ! Aerodynamic resistance
         REAL, INTENT( IN )   :: rbc     ! laminar boundary layer resistance
         REAL, INTENT( IN )   :: rstom   ! stomatal resistance
         REAL, INTENT( IN )   :: rcut    ! cuticle resistance
         REAL, INTENT( IN )   :: rwetsfc ! wet surface resistance
         REAL, INTENT( IN )   :: rinc    ! in canopy resistance
         REAL, INTENT( IN )   :: rsnow
         REAL, INTENT( IN )   :: rsndiff ! diffusivity through snow pack
         REAL, INTENT( IN )   :: rgw     ! wet soil resistance
         REAL, INTENT( IN )   :: xm
         REAL, INTENT( IN )   :: HG
         REAL, INTENT( IN )   :: H
         REAL, INTENT( IN )   :: del
         REAL, INTENT( IN )   :: LAI
         REAL, INTENT( IN )   :: wg ! 1 cm soil moisture
         REAL, INTENT( IN )   :: TS1CM
         REAL, INTENT( IN )   :: veg
         REAL, INTENT( IN )   :: sltype
         REAL, INTENT( IN )   :: ustar

         REAL, INTENT( OUT )  :: dpvd     ! evasion
         REAL, INTENT( OUT )  :: dvel     ! deposition velocity

         INTEGER, INTENT( IN ) :: c
         INTEGER, INTENT( IN ) :: r
         INTEGER, INTENT( IN ) :: jdate
         INTEGER, INTENT( IN ) :: jtime
         INTEGER, INTENT( IN ) :: tstep
         INTEGER, INTENT( IN ) :: ifsnow

         CHARACTER( 96 ) :: XMSG = ' '

         CHARACTER( 16 ), PARAMETER :: pname      = 'ATX'

         REAL   :: vdHG   ! Elemental Hg deposition velocity
         REAL   :: vdHGct   ! Hg cuticular transfer velocity
         REAL   :: vdHGst  ! stomatal transfer velocity
         REAL   :: vdHGsl
         REAL   :: pdHgsl
         REAL   :: rgnd
         REAL   :: rgndc

         REAL :: ZC, ZM ! depth of model surface media

!Model concentrations

         REAL, SAVE :: Cc         ! cuticular Hg(0) concentrations
         REAL, SAVE :: Cm         ! mesophyll Hg(0) concentrations
         REAL, SAVE :: CgHg       ! Soil Hg(0) concentrations
         REAL, SAVE :: CHgzo      ! Hg(0) concentrations at z = zo
         REAL, SAVE :: CgHgII0    ! Soil Hg(II) concentrations
         REAL :: vdHgt   ! sum of Hg deposition velocities
!********* reduction and partioning terms *******************************
         REAL :: kr      ! soil divalent mercury reduction term
         REAL :: Kam     ! air mesophyll partitioning coefficient for Hg(II)
         REAL :: Kac     ! air cuticule partitioning coeffiecient for Hg(II)
         REAL :: Kow     ! HgCl2 Octanol water partioning coefficient
         REAL :: Kpwc    ! Hg(0) air-vegetation surface partitioning coefficient
         REAL :: Kpwm    ! Hg(0) air-mesophyll partitioning coefficient
         REAL :: Kas     ! Hg(0) air-soil partitioning coefficient
!********* vegetation poperties *****************************************
         REAL :: lm   ! leaf mesophyll lipid fraction
         REAL :: lc   ! cuticular wax mesophyll lipid fraction
         REAL :: Wp   ! water content fraction of the leaf
         REAL :: bc   ! Emprical coefficeint to describe differences in plant lipids
         REAL :: flai ! Factor in mercury accumulation due to biodilution or scenescence
!********* Intermediate concentration variables *********************
         REAL, SAVE :: Hgm
         REAL, SAVE :: Hgc
         REAL, SAVE :: Hgs
!******** soil properties ***********************************************
         REAL, PARAMETER :: kvis   = 0.132 ! [cm^2 / s] at 273.15K
         REAL, PARAMETER :: karman = 0.4   ! von Karman constant
         REAL            :: ldry           ! diffusion length
         Real            :: rbg            ! soil boundary layer resistance
         REAL            :: scn            ! for Rbg
         REAL            :: ustg           ! for Rbg
         REAL            :: del0           ! for Rbg
          Real            :: dp                    ! for Rsoil
         Integer         :: ist            ! soil type
!********* Unit conversions *********************
         REAL :: M3MOLVOL ! molar volume of air at stp m3/mol

         M3MOLVOL = MOLVOL/1.0e3
!         Write(logdev,*) 'Before INIT_LAI'
         If( INIT_LAI ) Then
            MLAI( c,r ) = lai
            flai = 1.0
         EndIf
!         Write(logdev,*) 'Before INIT_ATX'
         If( INIT_ATX ) Then
! Equilibrium Hg(0) mesophyll concentration in a 5 month box model simulation
! umol/g leaf dry matter
            Cm     = fevgrn(c,r)*6.0e-6 + (1.0-fevgrn(c,r))*CMEDIA(5,c,r)
! Cuticular Hg(0) concentration in a 5 month box model simulation
            Cc     = fevgrn(c,r)*6.0e-7 + (1.0-fevgrn(c,r))*CMEDIA(6,c,r)
            CgHg   = CMEDIA( 3,c,r )  ! ng/g bulk soil concentration
            CHgzo  = CMEDIA( 4,c,r )  ! ng/g bulk soil concentration
         Else
            If( MLAI(c,r) .EQ. 0.0 ) Then
               flai = 1.0
            Else
               flai = max(lai/MLAI( c,r ),1.0) ! bio dilution
            End If
            if(flai .GT. 3.0) Write(Logdev,*) 'LAI factor: ', flai
            MLAI( c,r ) = lai
            Cm     = CMEDIA( 5,c,r )/flai  ! ng/g bulk leaf concentration
            Cc     = CMEDIA( 6,c,r )/flai  ! ng/g bulk leaf concentration
            CgHg   = CMEDIA( 3,c,r )  ! ng/g bulk soil concentration
            CHgzo  = CMEDIA( 4,c,r )  ! ng/g bulk soil concentration
         EndIf
!         Write(logdev,*) 'After INIT_ATX'
!         Write(logdev,*) 'flai, MLAI', flai, MLAI(c,r)

         dt     = tstep

! Model layer depths

         ZC     = 71*LAI ! g/m**2 based off of leaf litter fall
         ZM     = 71*LAI ! measurements at UCONN's experimental
! forest Bash and Miller 2009 Atmos. Environ.

!***************** canopy parameters *********************************
         Kow  = 4.15    ! For Hg, Mason 1996
         lm   = 0.02    ! From Trapp and Matthis 1995
         lc   = 0.02    ! Assumed cuticular wax lipid content
         Wp   = 0.8     ! leaf water fraction, Trapp and Mathis 1996
         bc   = 0.95   ! For barley, Trapp and Mathis 1996
         Kpwc = (Wp+lc*1.0/0.822*Kow**bc)*MWWAT                 ! g/umol
         Kpwm = (Wp+lm*1.0/0.822*Kow**bc)*MWWAT                 ! g/umol
! Partitioning coeficients following the methodology of the PEM model
         kac = (Kpwc*(1-del)) +  ! evasion from dry cuticles
     &         Kpwc*del*H        ! g/mol cuticle surface
         kam = Kpwm*H            ! g/mol apoplast solution
!**************** soil parameters ************************************
! Compute quasi-laminar boundary layer resistance at the soil surface
         scn  = kvis / diff
         ustg = max(ustar * EXP(-lai), 0.001)
         del0 = 1.0E-4 * kvis / ( karman * ustg )
         rbg  = ( scn - LOG( 10.0 * del0 ) ) / ( karman * ustg )
! Compute soil resistance
         ist = nint(Sltype)
         if (ist.lt.1.or.ist.gt.11) then
            WRITE(logdev,*) ' ist=',ist
            ist = 4
         endif
!         Write(logdev,*) 'before rgnd'
         ldry= ZG*(EXP((1-wg/wsat(ist))**5)-1)/1.718
         dp  = diff*1.E-4 * wsat(ist)**2 *
     &         (1.-wres(ist)/wsat(ist))**(2.+3./bslp(ist))
! Soil divalent mercury reduction rate following Scholz et al 2003
         If(TS1CM .GT. 273.15) Then
            kr     = 8e-11
            rgnd   = ldry/dp
         Else ! if the soil is frozen limit diffusion and reduction
            kr     = 0.0
            rgnd   = 1.0e6
         EndIf
!         Write(logdev,*) 'After rgnd, rgnd:', rgnd


         Kas = H
         cgHgII0 = 0.0
!         Write(logdev,*) 'Before cgHgII0'
!         Write(logdev,*) 'Size hglu_fac     ',size(hglu_fac)
!         Write(logdev,*) 'Size lufrac(i,c,r)',size(lufrac,1),size(lufrac,2),size(lufrac,3)
!         Write(logdev,*) '(n_lufrac,c,r)',n_lufrac,',',c,',',r
         Do i = 1, n_lufrac
            cgHgII0 = cgHgII0+hglu_fac(i)*lufrac(i,c,r)
         EndDo
!         Write(logdev,*) 'cgHgII0', cgHgII0
!         Write(logdev,*) 'H', H


C diffusion through soil from Scholtz et al. 2003
         rgndc  = rgnd + 0.5*rinc ! new soil diffusion model accounts for wet surfaces
         vdHg   = 1/(ra+0.5*rinc)
         vdHgst = 1/( rbc + rstom )
         vdHgct = lai*(( 1.0 - del  )/( rbc + rcut )
     &            + ( del )/(rbc + rwetsfc ))
         vdHgsl = veg/( rbg + rgndc + 0.5*rinc)
     &            + ((1-ifsnow) *( 1.0 - veg )*( 1.0-del ))/(rbg + rgnd )
     &            + (del * (1-ifsnow))/(rbg + rgw )
     &            + (ifsnow*(1.0 - xm))/( rbg + rsnow )
     &            + (xm*ifsnow)/(rbc + rsndiff + rgw)
         pdHgsl = 1/(ra + 1/vdHgsl )     ! production term with no canopy

         vdHgt = vdHg + vdHgst + vdHgct + vdHgsl

         CHgzo = (vdHg*Hg+vdHgst/kam*Cm+vdHgct/kac*Cc+vdHgsl/Kas*cgHg)
     &                    /vdHgt

         dpvd = veg * vdHg * CHgzo + (1-veg) * pdHgsl/Kas * CgHg

         dvel = pdHgsl + veg *(vdhg-pdHgsl)


! Load array A
         NC = 3
         LDA = 3
         LDVR = 3
         LDVL = 3
         LWORK = 204

         ALLOCATE ( KO(NC,NC), VR(NC,NC), VL(NC,NC), EIVAL(NC),
     &              WI(NC), WORK(LWORK), COEFS(NC,NC) )


         KO = 0.0
!         Write(logdev,*) 'Before load KO'
!         Write(logdev,*) 'H', H
!         Write(logdev,*) 'vdHgst', vdHgst
!         Write(logdev,*) 'vdHgct', vdHgct
!         Write(logdev,*) 'vdHgsl', vdHgsl
!         Write(logdev,*) 'ZM', ZM
!         Write(logdev,*) 'ZC', ZC
!         Write(logdev,*) 'ZG', ZG
!         Write(logdev,*) 'kam', kam
!         Write(logdev,*) 'kac', kac
!         Write(logdev,*) 'kas', kas
!         Write(logdev,*) 'wg', wg
!         Write(logdev,*) 'M3MOLVOL', M3MOLVOL
 !        Write(logdev,*) 'vdHgt', vdHgt

         KO(1,1) = -vdHgst/(ZM*kam*M3MOLVOL)*(1-vdHgst/(kam*vdHgt))
         KO(1,2) =  vdHgst/(ZM*M3MOLVOL)*vdHgct/(kac*vdHgt)
         KO(1,3) =  vdHgst/(ZM*M3MOLVOL)*vdHgsl/(H*vdHgt)
         KO(2,1) =  vdHgct/(ZC*M3MOLVOL)*vdHgst/(kam*vdHgt)
         KO(2,2) = -vdHgct/(ZC*kac*M3MOLVOL)*(1-vdHgct/(kac*vdHgt))
         KO(2,3) =  vdHgct/(ZC*M3MOLVOL)*vdHgsl/(H*vdHgt)
         KO(3,1) =  vdHgsl/(ZG*wg)*vdHgst/(kam*vdHgt)
         KO(3,2) =  vdHgsl/(ZG*wg)*vdHgct/(kac*vdHgt)
         KO(3,3) = -vdHgsl/(ZG*wg*Kas)*(1-vdHgsl/(H*vdHgt))
!         Write(logdev,*) 'KO: ', KO

!         Write(logdev,*) 'Before load COEFS'
         DO i = 1, NC
            DO j = 1, NC
               COEFS(i,j) = KO(i,j)
            END DO
         END DO
!         Write(logdev,*) 'COEFS: ', COEFS

         NRHS = 1
         LDB = 3

         ALLOCATE( NHS(LDB,NRHS))

         NHS = 0.0

!         Write(logdev,*) 'Before load NHS'
         NHS(1,1) = -vdHgst/ZM*(vdHg*HG/M3MOLVOL)/vdHgt
         NHS(2,1) = -vdHgct/ZC*(vdHg*HG/M3MOLVOL)/vdHgt
         NHS(3,1) = -vdHgsl/(ZG*wg)*(vdHg*HG)/vdHgt -
     &               kr*rhob(ist)*cgHgII0*(ZG)/(1.0e3*200.59)

!         Write(logdev,*) 'NHS: ', NHS
         ALLOCATE( B(LDB,NRHS))

         B = 0.0

!         Write(logdev,*) 'Before load B'
!         Write(logdev,*) 'Size B(LBD,NRHS) ',size(B,1), size(B,2)
!         Write(logdev,*) 'Cm,Cc,CgHg',Cm,',',Cc,',',CgHg
!         Write(logdev,*) 'B ', B
         B(1,1) = Cm
         B(2,1) = Cc
         B(3,1) = CgHg
!         Write(logdev,*) 'B ', B


!         Write(logdev,*) 'Before allocate EVECTS and IPIV'
         ALLOCATE( EVECTS(NC, NC ), IPIV(LDA))

         EVECTS = 0.0

C*****************************************************************************
C********** call LAPACK DGEEV to get eigen values and vectors ****************
C*****************************************************************************

!         Write(logdev,*) 'Before call SGEEV'
         CALL SGEEV('N','V',NC,KO,LDA,EIVAL,WI,VR, LDVR,VR,LDVR,WORK,LWORK,INFO)

         IF ( INFO .NE. 0 ) THEN
            IF ( INFO .LT. 0 ) THEN
               WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGEEV is incorrect'
            ELSE
               WRITE( LOGDEV,* ) 'SGEEV failed to compute all eigenvalues and vectors'
            END IF
               XMSG = '*** SGEEV fails ***'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

!         Write(logdev,*) 'Before load EVECTS'
         DO i = 1, NC
            DO j = 1, NC
               EVECTS(i,j) = VR(i,j)
            END DO
         END DO
!         Write(logdev,*) 'EVECTS'

C******************************************************************************
C******* Do the non homogenious part ******************************************
C******************************************************************************


! call LAPACK SGETRF to calculate the LU array factorization
         CALL SGETRF(NC, NC, COEFS, LDA, IPIV, INFO)

         IF ( INFO .NE. 0 ) THEN
            IF ( INFO .LT. 0 ) THEN
               WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGEEV is incorrect'
            ELSE
               WRITE( LOGDEV,* ) 'The upper triangular matrix is singular'
            END IF
            XMSG = '*** SGETRF fails ***'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

! call LAPACK SGETRS linear system solver for initial conditions
         CALL SGETRS('N', NC, NRHS, COEFS, LDA, IPIV, NHS, LDB, INFO )
!
         IF ( INFO .NE. 0 ) THEN
            WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGEEV is incorrect'
            XMSG = '*** SGETRS fails ***'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

C******************************************************************************
C*** Update the IC's for the Non-homogenious solutions and solve the system ***
C******************************************************************************

!         Write(logdev,*) 'Before load B'
         DO i = 1, NC
            B(i,1) = B(i,1) - NHS(i,1)
         ENDDO
!         Write(logdev,*) 'B: ', B

! call LAPACK SGETRF to calculate the LU array factorization
         CALL SGETRF(NC, NC, EVECTS, LDA, IPIV, INFO)

         IF ( INFO .NE. 0 ) THEN
            WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGETRF is incorrect'
            XMSG = '*** SGETRS fails ***'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF


! call LAPACK SGETRS linear system solver for initial conditions
         CALL SGETRS('N', NC, NRHS, EVECTS, LDA, IPIV, B, LDB, INFO )

         IF( INFO .NE. 0 ) THEN
            WRITE(*,*) 'Error in non homogenious SGETRS!!'
            WRITE(*,*) 'INFO: ', INFO
         END IF

!         WRITE(LOGDEV,*) 'Hg: ', HG, 'Hgm: ', Cm, 'Hgc: ', Cc, 'Hgs: ', CgHg

! update the surface array
         Hgm = 0.0
         Hgc = 0.0
         Hgs = 0.0

!        Write(logdev,*) 'Before calc Hg conc line 645'
         DO i = 1, NC
            Hgm = Hgm + B(i,1) * VR(1,i) * EXP( EIVAL(i) * dt )
            Hgc = Hgc + B(i,1) * VR(2,i) * EXP( EIVAL(i) * dt )
            Hgs = Hgs + B(i,1) * VR(3,i) * EXP( EIVAL(i) * dt )
         END DO

         Hgm   = Hgm + NHS(1,1)
         Hgc   = Hgc + NHS(2,1)
         Hgs   = Hgs + NHS(3,1)

!               WRITE(LOGDEV,*) 'Hg: ', HG, 'Hgm: ', Cm, 'Hgc: ', Cc, 'Hgs: ', CgHg
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

            XMSG = '*** Negative concentration in Hgs resetting it to zero ***'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            Hgs = max(Hgs, 0.0)

         END IF

!         Write(logdev,*) 'Before load CMEDIA line 680'
         CMEDIA( 1,c,r ) = 0.0 ! water
         CMEDIA( 2,c,r ) = 0.0 ! water
         CMEDIA( 3,c,r ) = Hgs
         CMEDIA( 4,c,r ) = CHgzo
         CMEDIA( 5,c,r ) = Hgm
         CMEDIA( 6,c,r ) = Hgc

         DEALLOCATE( KO, VR, VL, EIVAL, WI, WORK, COEFS )

         DEALLOCATE( B )

         DEALLOCATE( NHS )

         DEALLOCATE( EVECTS, IPIV )

         RETURN

         END SUBROUTINE ATX

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C::::::: Air surface water exchange subroutine :::::::::::::::::::::::::::
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

         SUBROUTINE ASWX( Hg, HgIIgas, vdHg, vdHgII, awhg, rgrnd,
     &                    dpvd, c, r, JDATE, JTIME, TSTEP )

         USE HGRD_DEFN           ! horizontal grid specifications
         USE DEPVVARS
         USE UTILIO_DEFN
         USE BIDI_MOD

C Includes:

         INCLUDE SUBST_CONST     ! constants
         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER, INTENT( IN )   :: JDATE
         INTEGER, INTENT( IN )   :: JTIME
         INTEGER, INTENT( IN )   :: TSTEP
         REAL,    INTENT( IN )   :: Hg
         REAL,    INTENT( IN )   :: HgIIgas
         REAL,    INTENT( IN )   :: vdHg
         REAL,    INTENT( IN )   :: vdHgII
         REAL,    INTENT( IN )   :: awhg
         REAL,    INTENT( IN )   :: rgrnd
         REAL,    INTENT( OUT )  :: dpvd
         INTEGER, INTENT( IN )   :: c
         INTEGER, INTENT( IN )   :: r

         CHARACTER( 16 ), PARAMETER :: pname      = 'ASWX'

C*************************** Ocean box parameters ***********************
         REAL, PARAMETER :: satten = 0.758
C*************************** Model concentrations ***********************
         REAL, SAVE :: cDGM
         REAL, SAVE :: cHgIIaq
C***** reduction and partioning terms from Whalin et al 2007 ************
         REAL, PARAMETER :: rref  = 240    ! referance incoming radiation
                                           ! for redox measurements (w/m2)
         REAL, PARAMETER :: kphot = 6.5e-4 ! drm photoreduction rate 1/s
         REAL, PARAMETER :: kox   = 7.2e-4 ! dgm photo-oxidation rate 1/s
C********* Intermediate concentration variables *********************
         REAL, SAVE :: DGM
         REAL, SAVE :: DRM


         IF (INIT_ASX ) THEN
            cDGM    = Hg*awhg*3 ! assume 3x eq con.
            cHgIIaq = 3.57e-6   ! from Whalin et al 2007
         ELSE
            cDGM    = CMEDIA( 1,c,r )
            cHgIIaq = CMEDIA( 2,c,r )
         END IF

         dt   = TSTEP

         dpvd = cDGM * vdHg/awhg

         IF ( rgrnd .LT. 1e-3 ) THEN
C the aqueous elemental and divalent Hg pools become decoupled and the
C matrices become singular warrenting an alternative solution

C Find a simple one box solution for elemental Hg

            DGM = Hg*awhg + (cDGM - Hg*awhg)*exp(-vdHg/(ZSURF*awhg)*dt)

C in the absence of photo-redox reactions divalent Hg accumulates
            DRM = cHgIIaq + vdHgII/ZSURF*HgIIgas*dt

         ELSE

            NC    = 2
            LDA   = 2
            LDVR  = 2
            LDVL  = 2
            LWORK = 16 ! use the box model to calculate optimal size of the
                     ! work array

            ALLOCATE ( KO(NC,NC), COEFS(NC,NC), VR(NC,NC), VL(NC,NC), EIVAL(NC),
     &              WI(NC), WORK(LWORK) )

            KO = 0.0

C 240 w/m**2 is the 'typical light spectrum' from Whalin et al 2007 Marine Chem.
C attenuation at 1 m = 1/K (1-exp(-K Z)) = 0.758 using a K of 0.58


            KO( 1,1 )   = -vdhg / ( ZSURF * awhg )
     &                    -kox * satten *rgrnd/rref
            KO( 1,2 )   = kphot * satten*rgrnd/rref
            KO( 2,1 )   = kox * satten * rgrnd/rref
            KO( 2,2 )   = -kphot * satten * rgrnd/rref

            COEFS = 0.0

            DO i = 1, NC
               DO j = 1, NC
                  COEFS(i,j) = KO(i,j)
               END DO
            END DO

            NRHS = 1
            LDB = 2

            ALLOCATE( NHS(LDB,NRHS))

            NHS = 0.0

            NHS(1,1) = -vdHg/ZSURF*HG
            NHS(2,1) = -vdHgII/ZSURF*HgIIgas

            ALLOCATE( B(LDB,NRHS))

            B = 0.0

            B( 1,1 ) = cDGM
            B( 2,1 ) = cHgIIaq

            ALLOCATE( EVECTS(NC, NC ), IPIV(LDA))

            EVECTS = 0.0

C*****************************************************************************
C********** call LAPACK DGEEV to get eigen values and vectors ****************
C*****************************************************************************

         CALL SGEEV('N','V',NC,KO,LDA,EIVAL,WI,VR, LDVR,VR,LDVR,WORK,LWORK,INFO)

            IF ( INFO .NE. 0 ) THEN
               IF ( INFO .LT. 0 ) THEN
                  WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGEEV is incorrect'
               ELSE
                  WRITE( LOGDEV,* ) 'SGEEV failed to compute all eigenvalues and vectors'
               END IF
               XMSG = '*** SGEEV fails ***'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            DO i = 1, NC
               DO j = 1, NC
                  EVECTS(i,j) = VR(i,j)
               END DO
            END DO

C******************************************************************************
C******* Do the non homogenious part ******************************************
C******************************************************************************
! call LAPACK SGETRF to calculate the LU array factorization
            CALL SGETRF(NC, NC, COEFS, LDA, IPIV, INFO)

            IF ( INFO .NE. 0 ) THEN
               IF ( INFO .LT. 0 ) THEN
                  WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGEEV is incorrect'
               ELSE
                  WRITE( LOGDEV,* ) 'The upper triangular matrix is singular'
               END IF
               XMSG = '*** SGETRF fails ***'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

! call LAPACK SGETRS linear system solver for initial conditions
            CALL SGETRS('N', NC, NRHS, COEFS, LDA, IPIV, NHS, LDB, INFO )
!
            IF ( INFO .NE. 0 ) THEN
               WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGETRS is incorrect'
               XMSG = '*** SGETRS fails ***'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

C******************************************************************************
C*** Update the IC's for the Non-homogenious solutions and solve the system ***
C******************************************************************************

            DO i = 1, NC
               B(i,1) = B(i,1) - NHS(i,1)
            ENDDO

! call LAPACK SGETRF to calculate the LU array factorization
            CALL SGETRF(NC, NC, EVECTS, LDA, IPIV, INFO)

            IF ( INFO .NE. 0 ) THEN
               WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGETRF is incorrect'
               XMSG = '*** SGETRF fails ***'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF


! call LAPACK SGETRS linear system solver for initial conditions
            CALL SGETRS('N', NC, NRHS, EVECTS, LDA, IPIV, B, LDB, INFO )

            IF( INFO .NE. 0 ) THEN
               WRITE( LOGDEV,* ) 'The ', INFO, ' var in SGETRS is incorrect'
               XMSG = '*** SGETRS fails ***'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF


! update the surface array
            DGM = 0.0
            DRM = 0.0

            DO i = 1, NC
               DGM = DGM + B(i,1) * VR(1,i) * EXP( EIVAL(i) * dt )
               DRM = DRM + B(i,1) * VR(2,i) * EXP( EIVAL(i) * dt )
            END DO

            DGM     = DGM   + NHS(1,1)
            DRM     = DRM   + NHS(2,1)

            DEALLOCATE( KO, VR, VL, EIVAL, WI, WORK, COEFS )

            DEALLOCATE( B )

            DEALLOCATE( NHS )

            DEALLOCATE( EVECTS, IPIV )

         END IF

         IF ( DGM .LT. 0.0 .OR. DRM .LT. 0.0 ) THEN

            XMSG = '*** Negative concentration ***'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )

         END IF

         CMEDIA( 1,c,r ) = DGM
         CMEDIA( 2,c,r ) = DRM
         CMEDIA( 3,c,r ) = 0.0 ! land
         CMEDIA( 4,c,r ) = 0.0 ! land
         CMEDIA( 5,c,r ) = 0.0 ! land
         CMEDIA( 6,c,r ) = 0.0 ! land

         RETURN

         END SUBROUTINE ASWX

C------------------------------------------------------------------------------

         SUBROUTINE GET_WDEP( CSE, WDEP, C, R )

         Use LSM_MOD, Only: lwmask
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
         REAL, PARAMETER :: MWH2O   = 18.02  ! molecular weight of H2O
         REAL  WDEP_LOAD   ! loading due to wet deposition


         IF ( NINT( lwmask( c,r ) ) .EQ. 0 ) THEN ! water

         ! convert to umol/m2 pulse input
            WDEP_LOAD = WDEP*HAOM2*UGOKG/MWHG
         ! convert to added concentration in ppm assuming it remains at the surface
            WDEP_LOAD = WDEP_LOAD/ZSURF/GH2ONM3*MWWAT

            IF( TRIM(CSE) .EQ. 'HG      ' ) THEN

               CMEDIA( 1,C,R ) = CMEDIA( 1,C,R ) + WDEP_LOAD

            END IF

            IF( TRIM(CSE) .EQ. 'HGIIGAS ' ) THEN

               CMEDIA( 2,C,R ) = CMEDIA( 2,C,R ) + WDEP_LOAD

            END IF

         END IF ! water

         RETURN

         END SUBROUTINE GET_WDEP
      END MODULE HGSIM

