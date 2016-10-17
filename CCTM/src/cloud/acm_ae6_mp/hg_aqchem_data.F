
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
C $Header: /project/yoj/arc/CCTM/src/cloud/cloud_acm_ae6_mp/hg_aqchem_data.F,v 1.3 2011/10/21 16:10:27 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      MODULE HG_AQCHEM_DATA
C-----------------------------------------------------------------------
C Function: Sets up Data for mercury cloud chemistry and contain subroutine to
C           calculate rates and minimum time steps of its reactions.

C Revision History:
C     27 Aug 2008 B.Hutzell: initial implementation
C     06 Jul 2009 J.Bash:    Replaced the Hg(II) reduction by HO2 with the 
C                            reduction mechanism by dicaboxylic acids of 
C                            Si and Ariya 2008 ES&T 
C     10 Sep 2011 B.Hutzell: updated for CMAQ version 5.0
C-----------------------------------------------------------------------


      IMPLICIT NONE

      INTEGER, PARAMETER  :: NPR    = 10  ! number of Hg reactions exclude adsorb/absorb

      INTEGER, PARAMETER  :: NUMOX_v46  = 5  ! number of oxidizing reactions

      INTEGER, PARAMETER  :: NHgRXN = 24 ! number of Hg reactions

      INTEGER, PARAMETER  :: NRXN   = NUMOX_v46 + NHgRXN     ! number of all reactions


C... INDICES FOR MERCURIC REDOX REACTIONS

      INTEGER          :: IHG        ! counter index over Hg rxns
      INTEGER, SAVE    :: IHG_OX = 1 ! Hg(aq) + O3(aq) => HgO(aq)    '
      INTEGER, SAVE    :: IHGSO3 = 2 ! HgSO3 => Hg(aq) + S(IV) van Loon et al.      '
      INTEGER, SAVE    :: IHGHY  = 3 ! Hg(OH)2 => Hg + products      '
      INTEGER, SAVE    :: IOHRAD = 4 ! Hg(aq) + OHRAD(aq) => Hg(II)  '
      INTEGER, SAVE    :: ICLI   = 5 ! oxidation by CL2 dissocations products (HOCL and OCL)
      INTEGER, SAVE    :: IORGC  = 6 ! Reduction of all Hg(II) species by oxalate RXN 6
C                                      Hg(II) + R(CO2)2 =>  Hg+ + products
C                                      Hg+    + HORCO2  =>  Hg(aq) + products
      INTEGER, SAVE    :: IHGDISULF  = 7        ! Reduction of Hg(SO3)2 by oxalate RXN 7
      INTEGER, SAVE    :: IHGOHP     = 8        ! Reduction of HgOHp    by oxalate RXN 8
      INTEGER, SAVE    :: IHGOHCL    = 9        ! Reduction of HgOHCL   by oxalate RXN 9
      INTEGER, SAVE    :: IHGCL2     = 10       ! Reduction of HgCL2   by oxalate RXN 9
      INTEGER, SAVE    :: ISHGCL2    = NPR + 1  ! index for HgCl2    sorption
      INTEGER, SAVE    :: ISHGSO3    = NPR + 3  ! index for HgSO3    sorption
      INTEGER, SAVE    :: ISHGHY     = NPR + 5  ! index for HgHY     sorption
      INTEGER, SAVE    :: ISHGDISULF = NPR + 7  ! index for Hg(SO3)2-- sorption
      INTEGER, SAVE    :: ISHGOHP    = NPR + 9  ! index for HgOH     sorption
      INTEGER, SAVE    :: ISHGOHCL   = NPR + 11 ! index for HgOHCl   sorption
      INTEGER, SAVE    :: ISHGII     = NPR + 13 ! index for Hg(II)   sorption
      INTEGER, SAVE    :: IDHGCL2    = NPR + 2  ! index for HgCl2    desorption
      INTEGER, SAVE    :: IDHGSO3    = NPR + 4  ! index for HgSO3    desorption
      INTEGER, SAVE    :: IDHGHY     = NPR + 6  ! index for HgHY     desorption
      INTEGER, SAVE    :: IDHGDISULF = NPR + 8  ! index for Hg(SO3)2-- desorption
      INTEGER, SAVE    :: IDHGOHP    = NPR + 10 ! index for HgOH     desorption
      INTEGER, SAVE    :: IDHGOHCL   = NPR + 12 ! index for HgOHCl   desorption
      INTEGER, SAVE    :: IDHGII     = NPR + 14 ! index for Hg(II)   desorption
      
      REAL( 8 )         :: COSINE_ZENITH   ! solar zenith anagle
      REAL( 8 )         :: PHGCL20         ! total HgCl2 partial pressure (atm)
      REAL( 8 )         :: PHGCL2F         ! gas only HgCl2 partial pressure (atm)
C...Aqueous Species
      REAL( 8 )         :: CLI             ! Cl(I) conc in cloudwater (mol/liter), from Cl2(aq)
      REAL( 8 )         :: HGII            ! Hg(II) conc in cloudwater (mol/liter)
      REAL( 8 )         :: HGH             ! Henry's Law Constant for Hg
      REAL( 8 )         :: HGL             ! Hg conc in cloudwater (mol/liter)
      REAL( 8 )         :: HGOHP           ! HgOH+ conc in cloudwater (mol/liter)
      REAL( 8 )         :: PHGAKNA         ! aitken mercury aerosol in water (moles/L)
      REAL( 8 )         :: PHGACCA         ! accum  mercury aerosol in water (moles/L)
      REAL( 8 )         :: PHG_AEROSOL_BAK ! previous sorbed Hg(II) in liquid phase
      REAL( 8 )         :: PHG_AEROSOL_NOW ! current  sorbed Hg(II) in liquid phase
      REAL( 8 )         :: PHG_AEROSOL_DEL ! PHG_AEROSOL_NOW - PHG_AEROSOL_BAK
      REAL( 8 )         :: SHGCL2          ! Sorbed HgCl2 conc in cloudwater (mol/liter)
      REAL( 8 )         :: SHGSO3          ! Sorbed HgSO3 conc in cloudwater (mol/liter)
      REAL( 8 )         :: SHGDISULF       ! Sorbed Hg(SO3)2-- conc in cloudwater (mol/liter)
      REAL( 8 )         :: SHGOHP          ! Sorbed HgOHp conc in cloudwater (mol/liter)
      REAL( 8 )         :: SHGHY           ! Sorbed Hg(OH)2 conc in cloudwater (mol/liter)
      REAL( 8 )         :: SHGOHCL         ! Sorbed HgOHCl conc in cloudwater (mol/liter)
      REAL( 8 )         :: SHGII           ! Sorbed HgII conc in cloudwater (mol/liter)
      REAL( 8 )         :: SORBED_HG_INIT  ! Initial total sorbed Hg in cloudwater (mol/liter)
      REAL( 8 )         :: AHGCL2          ! additional mass from RGMS to add to SHgCl2
      REAL( 8 )         :: AHGSO3          ! additional mass from RGMS to add to SHgSO3
      REAL( 8 )         :: AHGDISULF       ! additional mass from RGMS to add to SHgdisulf
      REAL( 8 )         :: AHGOHP          ! additional mass from RGMS to add to SHgOHp
      REAL( 8 )         :: AHGHY           ! additional mass from RGMS to add to SHgHY
      REAL( 8 )         :: AHGOHCL         ! additional mass from RGMS to add to SHgOHCl
      REAL( 8 )         :: AHGII           ! additional mass from RGMS to add to SHgII

C...Henry's Law and Dissociation Constant
      REAL( 8 )         :: HGOHP1          ! First dissociation constant for HgOH+
      REAL( 8 )         :: HGOHP1I         ! Inverse HgOHp1
      REAL( 8 )         :: HGOHCL          ! HgOHCl conc in cloudwater (mol/liter)
      REAL( 8 )         :: HGOHCL1         ! First dissociation constant for HgOHCl
      REAL( 8 )         :: HGOHCL1I        ! Inverse HgOHCl1
      REAL( 8 )         :: HGCL21          ! First dissociation constant for HgCl2
      REAL( 8 )         :: HGCL21I         ! Inverse HgCl21
      REAL( 8 )         :: HGCL2H          ! Henry's Law Constant for HgCl2
      REAL( 8 )         :: HEFFHGCL2       ! Effective Henry's Law Constant for HgCl2
      REAL( 8 )         :: HGCL21H         ! HgCl21 * HgCl2H
      REAL( 8 )         :: HGCL2L          ! HgCl2(aq) conc in cloudwater (mol/liter)
      REAL( 8 )         :: HGHY            ! Hg(OH)2 conc in cloudwater (mol/liter)
      REAL( 8 )         :: HGHY1           ! Equilibrium constant for Hg(OH)2
      REAL( 8 )         :: HGHY1I          ! Inverse HgHY1
      REAL( 8 )         :: HGSO3           ! HgSO3 conc in cloudwater (mol/liter)
      REAL( 8 )         :: HGSO31          ! Equilibrium constant for HgSO3
      REAL( 8 )         :: HGSO31I         ! Inverse HgSO31
      REAL( 8 )         :: HGDISULF        ! Hg(SO3)2-- conc in cloudwater (mol/liter)
      REAL( 8 )         :: HGDISULF1       ! Equilibrium constant for Hg(SO3)2--
      REAL( 8 )         :: HGDISULF1I      ! Inverse Hgdisulf1
C...reaction rates
      REAL( 8 )         :: K1K2I           ! HgSO31I * Hgdisulf1I
      REAL( 8 )         :: K4K5I           ! HgOHp1I * HgHY1I
      REAL( 8 )         :: K4K6I           ! HgOHp1I * HgOHCl1I
      REAL( 8 )         :: K6A             ! Rate constant used in ox of Hg by chlorine
      REAL( 8 )         :: K6B             ! Rate constant used in ox of Hg by chlorine
      REAL( 8 )         :: KORGC            ! Rate constant used in redux of all HgII by R(CO2)2
      REAL( 8 )         :: RHG6            !     ''        Hg with chlorine
      REAL( 8 )         :: RHG_OX          !     ''        Hg ox by O3
      REAL( 8 )         :: RHGSO3          !     ''
      REAL( 8 )         :: RHGSO3I         !     ''        , inverse
      REAL( 8 )         :: RHGHY           !     ''
      REAL( 8 )         :: RHGHYI          !     ''        , inverse
      REAL( 8 )         :: ROHRAD          !     ''
      REAL( 8 )         :: SOVERL          ! Ratio of sorbed to dissolved Hg(II)
      REAL( 8 )         :: XLHG            !
      REAL( 8 )         :: XLHGCL2         !
C...derivatives, differential, and timesteps
      REAL( 8 )         :: DHGDT( NHGRXN ) ! rate of Hg spcs prod in cld (mol/liter/sec)
      REAL( 8 )         :: DHG  ( NHGRXN ) ! Hg species product produced over tstep DTWHG(0)
      REAL( 8 )         :: DTWHG_MIN       ! safe timestep for mercury chemistry
      REAL( 8 )         :: DTWHG( NRXN )   ! timesteps for mercury chemistry

C...data used for using chemistry diagnositic file
      INTEGER,   SAVE   :: HG_AQCHEM_LOG
      

      CHARACTER( 30 )   ::  HG_ACHEM_RXN ( NHgRXN ) ! Hg reaction description
C...........INSORB is a logical variable used to initialize the fraction
C           of aqueous Hg(II) sorbed to suspended carbon

      LOGICAL      :: INSORB

c these vars based on jproc method of zenith calculation

      CONTAINS

      SUBROUTINE INIT_AQCHEM_HG(TEMP, WCAVG, JULIAN_DATE, ITIME, DARK)

!     USE AQ_DATA,    ONLY : AQCHEM_LAT, AQCHEM_LON
      USE UTILIO_DEFN

      IMPLICIT NONE

      INCLUDE SUBST_CONST        ! commonly used constants

      REAL( 8 ), PARAMETER :: H2ODENS    = 1000.0D+0  ! density of water (kg/m3) at 20 C and 1 ATM

C...Arguments
      REAL,    INTENT( IN )  :: TEMP         ! AIR TEMP (K)
      REAL,    INTENT( IN )  :: WCAVG        ! liquid water content (kg/m3)
      INTEGER, INTENT( IN )  :: JULIAN_DATE  ! Julian data, YYYYMMM
      INTEGER, INTENT( IN )  :: ITIME        ! time, HHMMSS (GMT)
      LOGICAL, INTENT( IN )  :: DARK         ! DARK = TRUE is night,  DARK = FALSE is day
      
C...Local      
      REAL( 8 )       :: XL        ! conversion factor (liter-atm/mol)
      REAL( 8 )       :: DBLE_TEMP ! TEMP converted to double precision
      REAL            :: COSZEN    ! cosine of solar zenith angle (dimensionaless)
      REAL            :: GMT       ! Greenwich mean time (dec.milt)

      CHARACTER( 4 )  :: PE_STRING 
      CHARACTER( 80 ) :: HG_AQCHEM_LOGFILE 
      LOGICAL, SAVE   :: FIRSTCALL = .TRUE.

C.. EXTERNAL FUNCTIONS and their descriptions:

      REAL,    EXTERNAL :: HLCONST


      IF( FIRSTCALL )THEN
      
          FIRSTCALL = .FALSE.
          
      END IF
      
      GMT  = REAL( ITIME, 4 )/8.64E+4
     
      
      HGH    = REAL( HLCONST( 'HG              ', TEMP, .FALSE., 0.0 ), 8)
      HGCL2H = REAL( HLCONST( 'HGIIGAS         ', TEMP, .FALSE., 0.0 ), 8)

!      XL = REAL( MOLVOL*(WCAVG/H2ODENS)*(TEMP/STDTEMP), 8)   ! conversion factor (l-atm/mol)
      XL = REAL( (MOLVOL*WCAVG*TEMP/STDTEMP), 8)  / H2ODENS
 
      XLHG    = HGH    * XL
      XLHGCL2 = HGCL2H * XL

C...dissociation constant for dissolved mercury species

C     K1:
      HGSO31 = 2.0D-13      ! (M)
      HGSO31I= 1.0D+0/HGSO31   !  (M**-1)
C     K2:
      HGDISULF1 = 4.D-12   ! (M)
      HGDISULF1I= 1.0D+0/HGDISULF1
C     K3:
      HGCL21  = 1.D-14     ! (M**2)
      HGCL21I = 1.0D+0/HGCL21  ! (M**-2)
      HGCL21H = HGCL21 * HGCL2H   !RB: HGCL21H IS NEVER USED
C     K4:
      HGOHP1   =  2.510D-11 ! (M)
      HGOHP1I   =  1.0D+0/HGOHP1
C     K5:
      HGHY1  = 6.310D-12    ! (M)
      HGHY1I = 1.0D+0/HGHY1
C     K6:
      HGOHCL1 = 3.720D-8    ! (M)
      HGOHCL1I = 1.0D+0/3.720D-8

      K1K2I = HGSO31I * HGDISULF1I
      K4K5I = HGOHP1I * HGHY1I
      K4K6I = HGOHP1I * HGOHCL1I

C...Hg reaction rates  RXN
C  Hg(aq) + O3(aq) => HgO(aq)     RXN 1  4.7E7
      RHG_OX  = 4.7D+7

      DBLE_TEMP = REAL( TEMP, 8)
C  HgSO3 => Hg(aq) + S(IV)        RXN 2 from Van Loon et al.
      RHGSO3  =  DBLE_TEMP * DEXP( (31.971D+0*DBLE_TEMP - 12595.0D+0)/DBLE_TEMP )
      RHGSO3I = 1.0D+0/RHGSO3
      IHGSO3  = 2

C  Hg(OH)2 => Hg + products        RXN 3   3.00D-7
      IF ( .NOT. DARK ) THEN
        RHGHY  = 6.00D-7 * COSINE_ZENITH   ! RATE NORMALIZED TO SOLAR FLUX 
        RHGHYI = 1.0D+0/RHGHY
      ELSE
        RHGHY  = 0.0D+0
        RHGHYI = 0.0D+0               ! NOT USED IF RHGHY = 0
      END IF

C  Hg(aq) + OHRAD(aq) => Hg(II)     RXN 4  2.0E9
      ROHRAD = 2.0D+9

C  OXIDATION OF Hg(aq) BY CHLORINE (HOCl and OCl-)  RXN 5

C  Hg(aq) + HOCl(aq) => Hg(II) + products
C  Hg(aq) + OCl-   --(H+)-->  Hg(II) + products
C  HOCl <=> H+ + OCl-                   K = 10**-7.5

      K6A     = 2.09D+6
      K6B     = 1.99D+6

C Reduction of all Hg(II) species by R(CO2)2      RXN 6
C  Hg(II) + R(CO2)2 =>  Hg+ + products
C  Hg+    + HORCO2 =>  Hg(aq) + products
C  Overall: Hg(II) + R(CO2)2 => Hg(aq) + products
      IF ( COSINE_ZENITH .GT. 0.0D+0 ) THEN
         KORGC   = 1.2D+4 * COSINE_ZENITH        ! 1/(M S)
      ELSE 
         KORGC   = 0.0D+0
      END IF

C ADSORPTION AND DESORPTION:  RXN 7-20
C
C All Hg(II) species sorb/desorb at the same time rate
C
C HgCl2L => SHgCl2                 RXN 7  adsorb = RHgad
C SHgCl2 => HgCl2L                 RXN 8  desorb = RHgde
C HgSO3  => SHgSO3                 RXN 9  adsorb = RHgad
C SHgSO3 => HgSO3                  RXN 10 desorb = RHgde
C HgHY   => SHgHY                  RXN 11 adsorb = RHgad
C SHgHY  => HgHY                   RXN 12 desorb = RHgde
C Hgdisulf  => SHgdisulf           RXN 13 adsorb = RHgad
C SHgdisulf => Hgdisulf            RXN 14 desorb = RHgde
C HgOHp     => SHgOHp              RXN 15 adsorb = RHgad
C SHgOHp    => HgOHp               RXN 16 desorb = RHgad
C HgOHCl    => SHgOHCl             RXN 17 adsorb = RHgad
C SHgOHCl   => HgOHCl              RXN 18 desorb = RHgad
C HgII      => SHgII               RXN 19 adsorb = RHgad
C SHgII     => HgII                RXN 20 desorb = RHgad


       DO IHG = 1, NHGRXN

         DHGDT( IHG ) = 0.0D+0
         DHG  ( IHG ) = 0.0D+0

       END DO

       SORBED_HG_INIT  = 0.0D+0
c...set history rgms to 0 hg before the time loop begins.
       PHG_AEROSOL_BAK  = 0.0D+0
 
1001   FORMAT(65(1x, A16))

       RETURN
       END SUBROUTINE
       REAL( 8 ) FUNCTION HGCL2_FACTOR_HLCONST( SO3, OH, CL, ACT_SQU) RESULT (HLCONST_FACTOR)
 
         IMPLICIT NONE
         
C        Inputs:
         REAL( 8 ), INTENT ( IN ) :: SO3     ! SO3= conc in cloudwater (mol/liter)
         REAL( 8 ), INTENT ( IN ) :: OH      ! OH conc in cloudwater (mol/liter)
         REAL( 8 ), INTENT ( IN ) :: CL      ! total Cl-  conc in cloudwater (mol/liter)
         REAL( 8 ), INTENT ( IN ) :: ACT_SQU ! activity factor correction for squared ions conc. ( dimensionaless )
         
C        Result:
         REAL( 8 )    HLCONST_EFECTIVE ! Effective Henry's Law Constant for Mercuric Chloride       
 
C        Local: 
         REAL( 8 ) CL_SAFE  !  CL ion filtered by MIN test
         REAL( 8 ) RECIPCL2 !  reciprocal of CL ion times ACT2

         CL_SAFE  = MAX( CL, 1.0D-10)

         RECIPCL2 = 1.0D+0 / (CL_SAFE*CL_SAFE*ACT_SQU)

         HLCONST_FACTOR = (1.0D+0 + HGCL21*RECIPCl2
     &                  * (1.0D+0 + HGSO31I*SO3 + K1K2I*SO3*SO3
     &                  +  HGOHP1I*OH +K4K5I*OH*OH 
     &                  +  K4K6I*OH*CL_SAFE )) 
     
                   
          RETURN
          
       END FUNCTION HGCL2_FACTOR_HLCONST
C
       SUBROUTINE MERCURY_RATES(WCAVG, DTRMV, EC, O3L, HPLUS, OHRAD, ORGC,
     &                          HOCL, OCL)

       IMPLICIT NONE

        INCLUDE SUBST_CONST        ! commonly used constants


        REAL,      INTENT( IN ) :: WCAVG  ! Liquid water content   (kg/m3)
        REAL( 8 ), INTENT( IN ) :: EC     ! elemental carbon acc+akn aerosol in cloudwater (mol/liter)
        REAL( 8 ), INTENT( IN ) :: DTRMV  ! Minimum time step required
        REAL( 8 ), INTENT( IN ) :: O3L    ! ozone dissolved in cloud water  (mol/liter)
        REAL( 8 ), INTENT( IN ) :: HPLUS  ! hydrogen ion concentration (mol/liter)
        REAL( 8 ), INTENT( IN ) :: OHRAD  ! OH ion concentration (mol/liter)
        REAL( 8 ), INTENT( IN ) :: ORGC   ! Assumed Oxalic acid concentration (mol/liter)
        REAL( 8 ), INTENT( IN ) :: HOCL   ! HOCL in cloud water  (mol/liter)
        REAL( 8 ), INTENT( IN ) :: OCL    ! OCL in cloud water  (mol/liter)
 

        REAL( 8 ) SOVD   ! sorbed Hg(II) / desorbed Hg(II)
        REAL( 8 ) SF     ! fraction of aqueous Hg(II) sorbed to E.C.
        REAL( 8 ) ECW    ! elem. carbon suspended in water (g / liter)
        REAL( 8 ) EC5    ! EC * 5
                            
        REAL( 8 ) RHGAD    ! HG overall adsorption
        REAL( 8 ) RHGADI   !     ''        , inverse
        REAL( 8 ) RHGDE    ! HG overall desorption
        REAL( 8 ) RHGDEI   !     ''        , inverse
        REAL( 8 ) KEQHOCL  ! equilib constant used in ox of Hg by chlorine
        REAL( 8 ) MINCARB  ! 1.00D-18/WCAVG in g/Liter
                            
        REAL( 8 ) DHGPROLOSS       ! sum of mercury species production and loss terms over dt
        REAL( 8 ) DHG_SCALE        ! Rate scale to preserve mass under S controled time step
        REAL( 8 ) LOSS_HG_SPECIES  ! sum of reaction rates controling an aqueous species 
     
        REAL( 8 ), PARAMETER :: KP      = 900.0D+0  ! sorption coeff. from Seigneur et al.(1998)
        REAL( 8 ), PARAMETER :: TFOLD   = 3600.0D+0 ! e-folding time for adsorption equilibrium (sec)
        REAL( 8 ), PARAMETER :: FAC_ADS =    1.0D+0 ! toggle factor on adsportion


       DTWHG( 1:NRXN ) = DTRMV
       DTWHG_MIN       = DTRMV
     
C... first Hg reaction:  Hg(aq) + O3(aq) => HgO(aq)

         DHGDT(IHG_OX) = RHG_OX * O3L * HGL
c        IF ((DHgDT(IHg_ox) .EQ. 0.0D+0).OR.( HgL .LE. 1.00D-30 ) ) THEN
c          DTWHG(IHg_ox + NUMOX_v46) = DTRMV
c        ELSE
cC         at completion, dtw*dhgdt=min(O3L, HgL) (limiting reagent)
c          DTWHG(IHg_ox + NUMOX_v46) = 0.1D+0 * (MIN(O3L, HgL))/DHgDT(IHg_ox )
c        END IF

C...second Hg reaction:  HgSO3 => Hg(aq) + S(IV)
!WTH:(08/14/09): includes ORGC reduction reaction
         DHGDT(IHGSO3) = (RHGSO3 + KORGC * ORGC) * HGSO3
         IF ( ( DHGDT(IHGSO3) .EQ. 0.0D+0 ).OR.(HGSO3 .LE. 1.00D-30) ) THEN
           DTWHG(IHGSO3 + NUMOX_V46 ) = DTRMV
         ELSE
           DTWHG(IHGSO3 + NUMOX_V46 ) = 0.1D+0 * RHGSO3I
         END IF
        

C...third Hg reaction:  Hg(OH)2 => Hg + products
!WTH:(08/14/09): includes ORGC reduction reaction
         DHGDT(IHGHY) = (RHGHY + KORGC * ORGC) * HGHY
         IF ( ( DHGDT(IHGHY) .EQ. 0.0D+0 ) .OR. ( HGHY .LE. 1.00D-30 ) ) THEN
           DTWHG(IHGHY + NUMOX_V46 ) = DTRMV
         ELSE
           DTWHG(IHGHY + NUMOX_V46 ) = 0.1D+0 * RHGHYI
         END IF
 
C...fourth Hg reaction:  Hg(aq) + OHRAD(aq) => Hg(II)
         DHGDT(IOHRAD) = ROHRAD * OHRAD * HGL

c        IF ( ( DHgDT(IOHRAD) .EQ. 0.0D+0 ) .OR. ( HgL .LE. 1.00D-30 ) .OR.
c     &      ( OHRAD .LE. 1.00D-30 ) ) THEN
c          DTWHG(IOHRAD + NUMOX_v46 ) = DTRMV
c        ELSE
c          at completion, dtw*dhgdt=min(OHRAD, HgL) (limiting reagent)
c          DTWHG(IOHRAD + NUMOX_v46 ) = 0.1D+0 * (MIN(OHRAD, HgL))/DHgDT(IOHRAD)
c        END IF

C...fifth Hg reaction:  OXIDATION OF Hg(aq) BY CHLORINE (HOCl and OCl-)
C
C  Hg(aq) + HOCl(aq) => Hg(II) + products
C  Hg(aq) + OCl-   --(H+)-->  Hg(II) + products
C  HOCl <=> H+ + OCl-                   K = 10**-7.5
C
         KEQHOCL = 3.16230D-8    ! EQUALS 10**(-7.5)
   
         RHG6 =  K6A*HOCL + K6B*OCL
         DHGDT(ICLI) = RHG6 *  HGL 
c         IF ( ( DHgDT(IClI) .EQ. 0.0D+0 ) .OR. ( HgL .LE. 1.00D-30 ) ) THEN
c          DTWHG(IClI + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(IClI+ NUMOX_v46 ) = 0.1D+0 * (MIN(HgL, ClI))/DHgDT(IClI)
c        END IF
c        if(DTWHG(IClI+ NUMOX_v46 ) .lt. 0.1D+0 ) then  !temporary check
c           DTWHG(IClI+ NUMOX_v46 ) = DTRMV
c           print*,"Warning: DTW too short for Chlorine ox. of Hg(0)"
c        end if


C...sixth Hg reaction:  REDUCTION OF Hg(II) by R(CO2)2
C
C  Hg(II) + R(CO2)2 =>  Hg+ + products
C  Hg+    + HORCO2 =>  Hg(aq) + products
C  Overall: Hg(II) + R(CO2)2 => Hg(aq) + products
C

         DHGDT(IORGC) = KORGC * ORGC *  HGII 
         IF ( ( DHGDT(IORGC) .EQ. 0.0D+0 ) .OR.( HGII .LE. 1.00D-30 ) ) THEN
           DTWHG(IORGC + NUMOX_V46 ) = DTRMV
         ELSE
           DTWHG(IORGC+ NUMOX_V46 ) = 0.1D+0 * (MIN(ORGC, HGII ))/DHGDT(IORGC)
         END IF

         DHGDT(IHGDISULF) = KORGC * ORGC * HGDISULF 
         IF ( ( DHGDT(IHGDISULF) .EQ. 0.0D+0 ) .OR.( HGDISULF .LE. 1.00D-30 ) ) THEN
           DTWHG(IHGDISULF + NUMOX_V46 ) = DTRMV
         ELSE
           DTWHG(IHGDISULF+ NUMOX_V46 ) = 0.1D+0 * (MIN(ORGC, HGDISULF ))/DHGDT(IHGDISULF)
         END IF
      
         DHGDT(IHGOHP) = KORGC * ORGC * HGOHP
         IF ( ( DHGDT(IHGOHP) .EQ. 0.0D+0 ) .OR.( HGOHP .LE. 1.00D-30 ) ) THEN
           DTWHG(IHGOHP + NUMOX_V46 ) = DTRMV
         ELSE
           DTWHG(IHGOHP+ NUMOX_V46 ) = 0.1D+0 * (MIN(ORGC, HGOHP ))/DHGDT(IHGOHP)
         END IF
       
         DHGDT(IHGOHCL) = KORGC * ORGC * HGOHCL
         IF ( ( DHGDT(IHGOHCL) .EQ. 0.0D+0 ) .OR.( HGOHCL .LE. 1.00D-30 ) ) THEN
           DTWHG(IHGOHCL + NUMOX_V46 ) = DTRMV
         ELSE
           DTWHG(IHGOHCL+ NUMOX_V46 ) = 0.1D+0 * (MIN(ORGC, HGOHCL ))/DHGDT(IHGOHCL)
         END IF
        
         DHGDT(IHGCL2) = KORGC * ORGC * HGCL2L
         IF ( ( DHGDT(IHGCL2) .EQ. 0.0D+0 ) .OR.( HGCL2L .LE. 1.00D-30 ) ) THEN
           DTWHG(IHGCL2 + NUMOX_V46 ) = DTRMV
         ELSE
           DTWHG(IHGCL2+ NUMOX_V46 ) = 0.1D+0 * (MIN(ORGC, HGCL2L ))/DHGDT(IHGCL2)
         END IF
 
C...new sorption/desorption code to allow adjustable elemental carbon
C   aerosol air concentrations (Russ Bullock, 09/19/2000)
c   using carbon aerosol within water

c EC here in moles/liter of water
c
c times 5 to estimate amount of elem carbon avail if 5% of pm2.5 rather
c than 1% had been set to elem carbon originally in aero_driver.F of

c         EC5 = max(0.0, (EC * 5.0)) ! moles/L
         EC5 = MAX(0.0D+0, EC) ! moles/l

c set min carb as equivilant to 1.0D-12 microg/m**3, and convert this
c to grams carbon/Liter of water.  1.0D-12 microg/m**3 is
c 1.0D-18g/m**3, which is (1.0D-18g/m**3)/WCAVG grams/Liter,
c where WCAVG is liquid water content (kg/m3)
c (1.0D-18g carb)/m**3 / [(?Kg H20)/m**3] = (1.0D-18g carb) /(?Kg H20)
c = (1.0D-18g carb) /(?Liters H20)
c (density of H20 is assumed to be close to 1kg/Liter)


          ECW     = EC5 * 12.01D+0    ! MOLES/LITER * 12G PER MOLE = G/LITER
          MINCARB = 1.00D-18 / WCAVG  ! IN G/L
          ECW     = MAX(ECW, MINCARB) ! FORCED MIN OF 1.0D-12UG/M**3 AS G/LITER

          SOVD = KP * ECW                ! SORBED HG(II) / DESORBED HG(II)
          SF   = SOVD / (1.0D+0 + SOVD)  ! FRACTION SORBED AT EQUILIBRIUM
    
          RHGAD  = SF / TFOLD        ! ADSORB RATE FOR E-FOLDING TIME
          RHGADI = 1.0D+0 / RHGAD
          RHGDE  = RHGAD / SOVD      ! DESORB RATE TO YIELD SF AT EQUILIBRIUM
          RHGDEI = 1.0D+0 / RHGDE
          
          RHGDE  = FAC_ADS * RHGDE
          RHGDEI = FAC_ADS * RHGDEI
          RHGAD  = FAC_ADS * RHGAD
          RHGADI = FAC_ADS * RHGADI
          
C Adsorption of Hg Species HgSO3 -> SHgSO3
          DHGDT(ISHGSO3) = RHGAD * HGSO3
c        IF ((DHgDT(ISHgSO3) .EQ. 0.0 ).OR.(HgSO3  .LE. 1.00D-30 ))THEN
c          DTWHG(ISHgSO3 + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(ISHgSO3 + NUMOX_v46 ) = .1* RHgadI
c        END IF

C Desorption of Hg Species SHgSO3 -> HgSO3
          DHGDT(IDHGSO3) = RHGDE * SHGSO3
c        IF ( ( DHgDT(IDHgSO3).EQ.0.0).OR.(SHgSO3.LE.1.00D-30 ) ) THEN
c          DTWHG(IDHgSO3 + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(IDHgSO3 + NUMOX_v46 ) = 0.1D+0* RHgdeI
c        END IF

C Adsorption of Hg Species HgCl2L -> SHgCl2
          DHGDT(ISHGCL2) = RHGAD * HGCL2L
c        IF ((DHgDT(ISHgCl2) .EQ. 0.0D+0) .OR. (HgCl2L .LE. 1.00D-30)) THEN
c          DTWHG(ISHgCl2 + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(ISHgCl2 + NUMOX_v46 ) = 0.1D+0 * RHgadI
c        END IF

C Desorption of Hg Species SHgCl2 -> HgCl2L
          DHGDT(IDHGCL2) = RHGDE * SHGCL2
c        IF ((DHgDT(IDHgCl2) .EQ. 0.0D+0 ).OR.( SHgCl2 .LE. 1.00D-30 ))THEN
c          DTWHG(IDHgCl2 + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(IDHgCl2 + NUMOX_v46 ) = 0.1D+0* RHgdeI
c        END IF

C Adsorption and Loss of Hg Species Hgdisulf -> SHgdisulf
          DHGDT(ISHGDISULF) = RHGAD * HGDISULF
c        IF ((DHgDT(ISHgdisulf).EQ.0.0D+0 ).OR.(Hgdisulf.LE.1.00D-30))THEN
c          DTWHG(ISHgdisulf + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(ISHgdisulf + NUMOX_v46 ) = .1* RHgadI
c        END IF

C Desorption of Hg Species SHgdisulf -> Hgdisulf
          DHGDT(IDHGDISULF) = RHGDE * SHGDISULF
c        IF ((DHgDT(IDHgdisulf).EQ.0.0D+0).OR.(SHgdisulf.LE.1.00D-30))THEN
c          DTWHG(IDHgdisulf + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(IDHgdisulf + NUMOX_v46 ) = .1* RHgdeI
c        END IF

C Adsorption and Loss of Hg Species HgOHp    -> SHgOHp
          DHGDT(ISHGOHP) = RHGAD * HGOHP
c        IF ( (DHgDT(ISHgOHp) .EQ. 0.0D+0 ).OR.( HgOHp .LE. 1.00D-30))THEN
c          DTWHG(ISHgOHp + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(ISHgOHp + NUMOX_v46 ) = .1* RHgadI
c        END IF

C Desorption of Hg Species SHgOHp    -> HgOHp
          DHGDT(IDHGOHP) = RHGDE * SHGOHP
c        IF (( DHgDT(IDHgOHp) .EQ. 0.0 ).OR.( SHgOHp .LE.1.00D-30) )THEN
c          DTWHG(IDHgOHp + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(IDHgOHp + NUMOX_v46 ) = .1* RHgdeI
c        END IF

C Adsorption and Loss of Hg Species HgOHCl   -> SHgOHCl
          DHGDT(ISHGOHCL) = RHGAD * HGOHCL
c        IF ((DHgDT(ISHgOHCl).EQ.0.0D+0 ).OR.( HgOHCl .LE. 1.00D-30 ) )THEN
c          DTWHG(ISHgOHCl + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(ISHgOHCl + NUMOX_v46 ) = .1* RHgadI
c        END IF

C Desorption of Hg Species SHgOHCl   -> HgOHCl
          DHGDT(IDHGOHCL) = RHGDE * SHGOHCL
c        IF (( DHgDT(IDHgOHCl).EQ.0.0).OR.(SHgOHCl.LE. 1.00D-30 ) )THEN
c          DTWHG(IDHgOHCl + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(IDHgOHCl + NUMOX_v46 ) = .1* RHgdeI
c        END IF

C Adsorption of Hg Species HgHY -> SHgHY
          DHGDT(ISHGHY) = RHGAD * HGHY
c        IF ((DHgDT(ISHgHY).EQ.0.0D+0 ) .OR. ( HgHY .LE. 1.00D-30 ) ) THEN
c          DTWHG(ISHgHY + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(ISHgHY + NUMOX_v46 ) = .1* RHgadI
c        END IF

C Desorption of Hg Species SHgHY -> HgHY
          DHGDT(IDHGHY) = RHGDE * SHGHY
c        IF ((DHgDT(IDHgHY) .EQ. 0.0 ).OR.( SHgHY .LE. 1.00D-30 ) ) THEN
c          DTWHG(IDHgHY + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(IDHgHY + NUMOX_v46 ) = .1* RHgdeI
c        END IF

C Adsorption of Hg Species HgII -> SHGII
          DHgDT(ISHgII) = RHgad * HgII
c        IF (( DHgDT(ISHgII).EQ.0.0 ) .OR. ( HgII .LE. 1.00D-30 ) ) THEN
c          DTWHG(ISHgII + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(ISHgII + NUMOX_v46 ) = .1* RHgadI
c        END IF

C Desorption of Hg Species SHgII -> HgII
          DHGDT(IDHGII) = RHGDE * SHGII
c        IF ((DHgDT(IDHgII) .EQ. 0.0 ).OR.( SHgII .LE. 1.00D-30 ) ) THEN
c          DTWHG(IDHgII + NUMOX_v46 ) = DTRMV
c        ELSE
c          DTWHG(IDHgII + NUMOX_v46 ) = .1* RHgdeI
c        END IF

C Don't allow any aqeuous mercury species concentrations to become negative
      ! If the losses of HgII are greater than the produciton terms scale
      ! the losses to zero out the HgII concentration in the time step
      
          DTWHG_MIN = MINVAL(DTWHG)

          LOSS_HG_SPECIES = DHGDT(IHG_OX) + DHGDT(IOHRAD) + DHGDT(ICLI)
          IF ( HGL .LT.  LOSS_HG_SPECIES * DTRMV ) THEN
              DHG_SCALE = HGL / ( LOSS_HG_SPECIES * DTRMV )
              DHGDT(IHG_OX) = DHGDT(IHG_OX) * DHG_SCALE 
              DHGDT(IOHRAD) = DHGDT(IOHRAD) * DHG_SCALE  
              DHGDT(ICLI)   = DHGDT(ICLI)   * DHG_SCALE 
          END IF
  
          LOSS_HG_SPECIES = DHGDT(IHGSO3) + DHGDT(ISHGSO3)
          IF ( HGSO3 .LT.  LOSS_HG_SPECIES * DTRMV ) THEN
              DHG_SCALE = HGSO3 / ( LOSS_HG_SPECIES * DTRMV )
              DHGDT(IHGSO3)  = DHGDT(IHGSO3)  * DHG_SCALE 
              DHGDT(ISHGSO3) = DHGDT(ISHGSO3) * DHG_SCALE 
          END IF
          
          LOSS_HG_SPECIES = DHGDT(IHGHY) + DHGDT(ISHGHY)
          IF ( HGHY .LT.  LOSS_HG_SPECIES * DTRMV ) THEN
              DHG_SCALE = HGHY / ( LOSS_HG_SPECIES * DTRMV )
              DHGDT(IHGHY)  = DHGDT(IHGHY)  * DHG_SCALE 
              DHGDT(ISHGHY) = DHGDT(ISHGHY) * DHG_SCALE 
          END IF
  
          LOSS_HG_SPECIES = DHGDT(IORGC) + DHGDT(ISHGII)
          IF ( HGII .LT.  LOSS_HG_SPECIES * DTRMV ) THEN
              DHG_SCALE = HGII / ( LOSS_HG_SPECIES * DTRMV )
              DHGDT(IORGC)   = DHGDT(IORGC) * DHG_SCALE 
              DHGDT(ISHGII) = DHGDT(ISHGII) * DHG_SCALE 
          END IF

          LOSS_HG_SPECIES = DHGDT(IHGDISULF) + DHGDT(ISHGDISULF)
          IF ( HGDISULF .LT.  LOSS_HG_SPECIES * DTRMV ) THEN
              DHG_SCALE = HGDISULF / ( LOSS_HG_SPECIES * DTRMV )
              DHGDT(IHGDISULF)   = DHGDT(IHGDISULF)  * DHG_SCALE 
              DHGDT(ISHGDISULF)  = DHGDT(ISHGDISULF) * DHG_SCALE 
          END IF

          LOSS_HG_SPECIES = DHGDT(IHGOHP) + DHGDT(ISHGOHP)
          IF ( HGOHP .LT.  LOSS_HG_SPECIES * DTRMV ) THEN
              DHG_SCALE = HGOHP / ( LOSS_HG_SPECIES * DTRMV )
              DHGDT(IHGOHP)   = DHGDT(IHGOHP)  * DHG_SCALE 
              DHGDT(ISHGOHP)  = DHGDT(ISHGOHP) * DHG_SCALE 
          END IF

          LOSS_HG_SPECIES = DHGDT(IHGOHCL) + DHGDT(ISHGOHCL)
          IF ( HGOHCL .LT.  LOSS_HG_SPECIES * DTRMV ) THEN
              DHG_SCALE = HGOHCL / ( LOSS_HG_SPECIES * DTRMV )
              DHGDT(IHGOHCL)   = DHGDT(IHGOHCL)  * DHG_SCALE 
              DHGDT(ISHGOHCL)  = DHGDT(ISHGOHCL) * DHG_SCALE 
          END IF

          LOSS_HG_SPECIES = DHGDT(IHGCL2) + DHGDT(ISHGCL2)
          IF ( HGCL2L .LT.  LOSS_HG_SPECIES * DTRMV ) THEN
              DHG_SCALE = HGCL2L / ( LOSS_HG_SPECIES * DTRMV )
              DHGDT(IHGCL2)   = DHGDT(IHGCL2)   * DHG_SCALE 
              DHGDT(ISHGCL2)  = DHGDT(ISHGCL2) * DHG_SCALE 
          END IF
  
           DHGDT(IDHGSO3)    = MIN( SHGSO3    / DTRMV, DHGDT(IDHGSO3) )
           DHGDT(IDHGHY)     = MIN( SHGHY     / DTRMV, DHGDT(IDHGHY) )
           DHGDT(IDHGII)     = MIN( SHGII     / DTRMV, DHGDT(IDHGII) )
           DHGDT(IDHGCL2)    = MIN( SHGCL2    / DTRMV, DHGDT(IDHGCL2) )
           DHGDT(IDHGDISULF) = MIN( SHGDISULF / DTRMV, DHGDT(IDHGDISULF) )
           DHGDT(IDHGOHP)    = MIN( SHGOHP    / DTRMV, DHGDT(IDHGOHP) )
           DHGDT(IDHGOHCL)   = MIN( SHGOHCL   / DTRMV, DHGDT(IDHGOHCL) )
  
           DHGDT(ISHGCL2)    = MIN( HGCL2L    / DTRMV, DHGDT(ISHGCL2) )
           DHGDT(ISHGDISULF) = MIN( HGDISULF  / DTRMV, DHGDT(ISHGDISULF) )
           DHGDT(ISHGOHP)    = MIN( HGOHP     / DTRMV, DHGDT(ISHGOHP) )
           DHGDT(ISHGOHCL)   = MIN( HGOHCL    / DTRMV, DHGDT(ISHGOHCL) )
  
        END SUBROUTINE MERCURY_RATES

      END MODULE HG_AQCHEM_DATA
