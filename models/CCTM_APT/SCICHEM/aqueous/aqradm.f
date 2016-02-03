      subroutine aqradm ( temp, pres_atm, taucld, wcavg, 
     &                    alfa0, alfa2, alfa3,
     &                    gas, aerosol, rhoair, hplus )

C-----------------------------------------------------------------------
C
C  DESCRIPTION:
C    Compute concentration changes in cloud due to aqueous chemistry
C    Adapted from RADM Cloud implementation in CMAQ for SCICHEM
C    by PK, AER, Feb 2005 for Southern Company and EPRI,
C    EPRI Agreement EP-P14638/C7185
C
C  Reference: 
C     Walcek & Taylor, 1986, A theoretical Method for computing
C      vertical distributions of acidity and sulfate within cumulus
C      clouds, J. Atmos Sci.,  Vol. 43, no. 4 pp 339 - 355
C
C  Called by:  STEP_AQUEOUS
C
C  Based on aqchem.F in CMAQ
C
C  Calls the following functions:  HLCONST
C
C  ARGUMENTS     TYPE      I/O       DESCRIPTION
C  ---------     ----  ------------  --------------------------------
C  GAS(ngas)     real  input&output  Concentration for species i=1,15
C
C  AEROSOL(naer,nmodes) real input&output   Concentration for species i=1,42
C
C Updated to CMAQ 4.7.1, PK, ENVIRON, Nov 2010
C
C Updates for CMAQ 5.0 beta, PK, ENVIRON, Aug 2011
C Updates for CMAQ 5.0 final, PK, ENVIRON, Mar 2012
C Updates for CMAQ 5.0.1, PK, ENVIRON, May 2012
C-----------------------------------------------------------------------
      use error_inc
      use aqueous_consts_inc
      use aqueous_species_inc

!debug
!     use files_inc
!debug

      implicit none

C...........PARAMETERS

      INTEGER, PARAMETER :: NUMOX =  5   ! number of oxidizing reactions

! minimum concentration
!     REAL, PARAMETER :: CONMIN = 1.0E-30  ! Now comes from AERO_DATA via
!                                           ! AQ_DATA

C...........ARGUMENTS and their descriptions

      REAL,      INTENT( IN ) ::  TEMP         ! temperature (K)
      REAL,      INTENT( IN ) ::  PRES_ATM     ! pressure (atm)
      REAL,      INTENT( IN ) ::  TAUCLD       ! timestep for cloud (s)
      REAL,      INTENT( IN ) ::  WCAVG        ! liquid water content (kg/m3)
      REAL,      INTENT( IN ) ::  ALFA0        ! scav coef for aitken aerosol number
      REAL,      INTENT( IN ) ::  ALFA2        ! scav coef for aitken aerosol sfc area
      REAL,      INTENT( IN ) ::  ALFA3        ! scav coef for aitken aerosol mass

      REAL, INTENT( INOUT ) :: GAS    ( NGAS )         ! gas phase concentrations (mol/molV)
      REAL, INTENT( INOUT ) :: AEROSOL( NAER, NMODES ) ! aerosol concentrations (mol/molV)

      real,      INTENT( IN )  ::  rhoair  ! air density, moles/m3
      real,      INTENT( OUT ) ::  hplus   ! hydrogen ion concentration (mol/l)

C...........LOCAL VARIABLES and their descriptions:
      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru

! Henry's law surrogate for MGLY
      CHARACTER(16), SAVE :: MGLYSUR = 'METHYL_GLYOXAL  '

!debug
!     integer, save :: counter = 0
!debug
      INTEGER      ISPC            ! loop counter for species
      INTEGER      I20C            ! loop counter for do loop 20
      INTEGER      I30C            ! loop counter for do loop 30
      INTEGER      ITERAT          ! # iterations of aqueous chemistry solver
      INTEGER      I7777C          ! aqueous chem iteration counter
      INTEGER      ICNTAQ          ! aqueous chem iteration counter
      INTEGER      LIQ             ! loop counter for liquid species
      INTEGER      IGAS            ! loop counter for gas species
      INTEGER      IOX             ! index over oxidation reactions

      REAL         A               ! iron's anion concentration
      REAL         AC              ! H+ concentration in cloudwater (mol/liter)
      REAL         ACT1            ! activity correction factor!single ions
      REAL         ACT2            ! activity factor correction!double ions
      REAL         ACTB            ! 
      REAL         AE              ! guess for H+ conc in cloudwater (mol/l)

      REAL         B               ! manganese's anion concentration
      REAL         BB              ! lower limit guess of cloudwater pH
      REAL         CA              ! Calcium conc in cloudwater (mol/liter)
      REAL         CL              ! total Cl-  conc in cloudwater (mol/liter)
      REAL         CLACC           ! fine Cl- in cloudwater (mol/liter)
      REAL         CLCOR           ! coarse Cl-  conc in cloudwater (mol/liter)
      REAL         CO2H            ! Henry's Law constant for CO2
      REAL         CO21            ! First dissociation constant for CO2
      REAL         CO22            ! Second dissociation constant for CO2
      REAL         CO212           ! CO21*CO22
      REAL         CO212H          ! CO2H*CO21*CO22
      REAL         CO21H           ! CO2H*CO21
      REAL         CO2L            ! CO2 conc in cloudwater (mol/liter)
      REAL         CO3             ! CO3= conc in cloudwater (mol/liter)
      REAL         DSIV_SCALE      ! mass conservation scale factor for S(IV)
      REAL         DTS6            !                  

      REAL      DGLYDT             ! change in GLY (mol/liter/sec)
      REAL      DMGLYDT            ! change in MGLY (mol/liter/sec)
      REAL      DGLY1              ! change due to Rxn. in GLY for DTW(0) time step
      REAL      DMGLY1             ! change due to Rxn. in MGLY for DTW(0) time step
      REAL      DORGC              ! change in ORGC for DTW(0) time step (mol/liter)

      REAL         EALFA0T         ! EXP( -ALFA0 * TAUCLD )
      REAL         EALFA2T         ! EXP( -ALFA2 * TAUCLD )
      REAL         EALFA3T         ! EXP( -ALFA3 * TAUCLD )
      REAL         OMEALFAT        ! 1 - e(-alfa*t)

      REAL         EC              ! elemental carbon acc+akn aerosol in
                                   ! cloudwater (mol/liter)
      REAL         FA              ! functional value ??
      REAL         FB              ! functional value ??
      REAL         FE              ! Fe+++ conc in cloudwater (mol/liter)

      REAL         FNH3            ! frac weight of NH3 to total ammonia
      REAL         FNH4ACC         ! frac weight of NH4 acc to total ammonia
      REAL         FHNO3           ! frac weight of HNO3 to total NO3
      REAL         FNO3ACC         ! frac weight of NO3 acc to total NO3

      REAL         FOA1            ! First dissociation constant for FOA (Formic Acid)
      REAL         FOAH            ! Henry's Law constant for FOA
      REAL         FOA1H           ! FOAH*FOA1
      REAL         FOAL            ! FOA conc in cloudwater (mol/liter)
      REAL         FTST            !
      REAL         GLYH            ! Henry's Law constant for glyoxal
      REAL         GLYL            ! glyoxal conc in cloud water (mol/liter)

      REAL         GM              !
      REAL         GM1             !
      REAL         GM1LOG          !      
      REAL         GM2             ! activity correction factor
      REAL         GM2LOG          !

      REAL         HA              !
      REAL         HB              !
      REAL         H2OW            !
      REAL         H2O2H           ! Henry's Law Constant for H2O2
      REAL         H2O2L           ! H2O2 conc in cloudwater (mol/liter)
      REAL         HCLH            ! Henry's Law constant for HCL 
      REAL         HCL1            ! First dissociation constant for HCL
      REAL         HCL1H           ! HCLH*HCL1
      REAL         HCLL            ! HCL conc in cloudwater (mol/liter)
      REAL         HCO2            ! HCO2 conc in cloudwater (mol/liter)
      REAL         HCO3            ! HCO3 conc in cloudwater (mol/liter)
      REAL         HNO3H           ! Henry's Law Constant for HNO3
      REAL         HNO31           ! First dissociation constant for HNO3
      REAL         HNO31H          ! HNO31*HNO3H
      REAL         HNO3L           ! HNO3 conc in cloudwater (mol/liter)
      REAL         HOH             ! Henry's Law Constant for HO
      REAL         HSO3            ! HSO3 conc in cloudwater (mol/liter)
      REAL         HSO4            ! HSO4 concn in cloudwater (mol/liter)
      REAL         HSO4ACC         ! accumulation mode HSO4 concn in cloudwater (mol/liter)
      REAL         HSO4COR         ! coarse HSO4 concn in cloudwater (mol/liter)

      REAL         HTST            !

      REAL         K               ! K conc in cloudwater (mol/liter)
      REAL         MG              ! Mg conc in cloudwater (mol/liter)
      REAL         MGLYH           ! Henry's Law Constant for MGLY (M/atm)
      REAL         MGLYL           ! MGLY conc in cloudwater (mol/liter)
      REAL         MHPH            ! Henry's Law Constant for MHP
      REAL         MHPL            ! MHP conc in cloudwater (mol/liter)
      REAL         MN              ! Mn++ conc in cloudwater (mol/liter)
      REAL         NA              ! Na conc in cloudwater (mol/liter)
      REAL         NAACC           ! Na in cloudwater (mol/liter)
      REAL         NACOR           ! coarse Na in cloudwater (mol/liter)
      REAL         NH31            ! First dissociation constant for NH3
      REAL         NH3H            ! Henry's Law Constant for NH3
      REAL         NH3DH2O         ! 
      REAL         NH31HDH         !       
      REAL         NH3L            ! NH3 conc in cloudwater (mol/liter)
      REAL         NH4             ! NH4+ conc in cloudwater (mol/liter)
      REAL         NH4ACC          ! NH4 acc conc in cloudwater (mol/liter)
      REAL         NH4COR          ! NH4 coarse conc in cloudwater (mol/liter)
      REAL         NITAER          ! total aerosol nitrate
      REAL         NO3             ! NO3- conc in cloudwater (mol/liter)
      REAL         NO3ACC          ! NO3 acc conc in cloudwater (mol/liter)
      REAL         NO3COR          ! NO3 coarse conc in cloudwater (mol/liter)
!     REAL( 8 ) :: NUMCOR          ! coarse aerosol number in cloudwater (mol/liter)
      REAL         O3H             ! Henry's Law Constant for O3
      REAL         O3L             ! O3 conc in cloudwater (mol/liter)
      REAL         OH              ! OH- conc in cloudwater (mol/liter)
      REAL         OHL             ! OH radical conc in cloudwater (mol/liter)
      REAL      :: SOA             ! secondary organic aerosol in cloudwater (mol/liter)
      REAL         ORGC            ! cloud-produced SOA in cloudwater (treated as primary) (mol/liter)
      REAL      :: POA             ! primary organic aerosol in cloudwater (mol/liter)
      REAL         PAAH            ! Henry's Law Constant for PAA
      REAL         PAAL            ! PAA conc in cloudwater (mol/liter)
      REAL         PCO2F           ! gas only CO2 partial pressure (atm)
      REAL         PFOAF           ! gas only ORGANIC ACID partial press (atm)
      REAL         PGLYF           ! gas only GLY partial pressure (atm)
      REAL         PH2O2F          ! gas only H2O2 partial pressure (atm)
      REAL         PHCLF           ! gas only HCL partial pressure (atm)
      REAL         PHNO3F          ! gas only HNO3 partial pressure (atm)
      REAL         PHOF            ! gas only HO partial pressure (atm)
      REAL         PMGLYF          ! gas only MGLY partial pressure (atm)
      REAL         PMHPF           ! gas only MHP partial pressure (atm)
      REAL         PNH3F           ! gas only NH3 partial pressure (atm)
      REAL         PO3F            ! gas only O3 partial pressure (atm)
      REAL         PPAAF           ! gas only PAA partial pressure (atm)      
      REAL         PRIM            ! PRIMARY acc+akn aerosol in cloudwater (mol/liter)
      REAL         PSO2F           ! gas only SO2 partial pressure (atm)

      REAL         RECIPA1         !                   
      REAL         RECIPA2         ! 
      REAL         RECIPAP1        ! one over pressure (/atm)

      REAL         RGLY3           ! liter/(mol sec)
      REAL         RH2O2           !
      REAL         RMGLY3          ! liter/(mol sec)
      REAL         RMHP            !
      REAL         RPAA            ! 

      REAL         RT              ! gas const * temperature (liter atm/mol)

      REAL         SIV             ! dissolved so2 in cloudwater (mol/liter)
      REAL         SK6             !  
      REAL         SK6TS6          !
      REAL         SO21            ! First dissociation constant for SO2
      REAL         SO22            ! Second dissociation constant for SO2
      REAL         SO2H            ! Henry's Law Constant for SO2
      REAL         SO212           ! SO21*SO22
      REAL         SO212H          ! SO21*SO22*SO2H
      REAL         SO21H           ! SO21*SO2H
      REAL         SO2L            ! SO2 conc in cloudwater (mol/liter)
      REAL         SO3             ! SO3= conc in cloudwater (mol/liter)
      REAL         SO4             ! SO4= conc in cloudwater (mol/liter)
      REAL         SO4ACC          ! accumulation mode SO4= conc in cloudwater (mol/liter)
      REAL         SO4COR          ! coarse SO4= conc in cloudwater (mol/liter)

      REAL         STION           ! ionic strength  
      REAL         TAC             ! 
      REAL         TEMP1           ! (1/T) - (1/298) (1/K)
      REAL         TIMEW           ! cloud chemistry clock (sec)

      REAL         TGLY            ! total glyoxal available for oxidation
      REAL         TMGLY           ! total methylglyoxal available for oxidation
      REAL         TOTOX           !      
      REAL         TH2O2
      REAL         TO3
      REAL         TMHP
      REAL         TPAA
      REAL         TOTAMM          ! total ammonium
      REAL         TOTNIT          ! total nitrate (excluding coarse mode)
      REAL         TS6             ! SO4 conc in cloudwater (mol/liter)
      REAL         TS6ACC          ! SO4 acc conc in cloudwater (mol/liter)
      REAL         TS6COR          ! coarse SO4 conc in cloudwater   (mol/liter) 
      REAL         TSIV            ! total S(iv) available for oxidation

      REAL         TST             ! 

      REAL         XL              ! conversion factor (liter-atm/mol)
      REAL         ONE_OVER_XL     ! 1.0 / XL
      REAL         PRES_ATM_OVER_XL     ! PRES_ATM / XL

      REAL         XLCO2           !
      REAL         XLH2O2          !
      REAL         XLHCL           ! const in calc of HCL final partial pres
      REAL         XLHCHO          !
      REAL         XLHNO3          !
      REAL         XLMHP           ! 
      REAL         XLNH3           !
      REAL         XLO3            ! 
      REAL         XLPAA           ! 
      REAL         XLSO2           ! 

      REAL      :: CAACC           ! accumulation mode Calcium (AE6) SLN 16March2011      
      REAL      :: MGACC           ! accumulation mode Magnesium (AE6) SLN 16March2011     
      REAL      :: KACC            ! accumulation mode Potassium (AE6) SLN 16March2011     
      REAL      :: CACOR           ! coarse mode Calcium (AE6) SLN 16March2011
      REAL      :: MGCOR           ! coarse mode Magnesium (AE6) SLN 16March2011
      REAL      :: KCOR            ! coarse mode Potassium (AE6) SLN 16March2011
      REAL      :: SOILCOR         ! coarse mode SOIL (AE6) SLN 16March2011
      REAL      :: ANTHCOR         ! coarse mode CORS (AE6) SLN 16March2011
      REAL      :: SEASCOR         ! coarse mode SEAS (AE6) SLN 16March2011
      REAL      :: FEACC           ! accumulation mode Fe (AE6) SLN 22March2011
      REAL      :: MNACC           ! accumulation mode Fe (AE6) SLN 22March2011
      REAL      :: FECOR           ! coarse mode Mn (AE6) SLN 22March2011
      REAL      :: MNCOR           ! coarse mode Mn (AE6) SLN 22March2011
      REAL      :: FE_OX           ! Fe(III) available for sulfate oxidation
      REAL      :: MN_OX           ! Mn(II) available for sulfate oxidation

      REAL      :: FE_III          ! Fractional Fe(III) partitioning, GS - July 1, 2011
      REAL      :: MN_II           ! Fractional Mn(II) partitioning, GS - July 1, 2011
      
      REAL      :: FE_SOL          ! Fractional Fe solubility, GS - July 1, 2011
      REAL      :: MN_SOL          ! Fractional Mn solubility, GS - July 1, 2011

C...........Local Variables (arrays):

      REAL      :: LIQUID( NLIQS ) ! wet deposition array (mm mol/liter)
      REAL      :: LOADING( NAER, NMODES ) ! aerosol loading (mol/liter)
      REAL      :: INITGAS( NGAS ) ! initial gas partial pressure (atm)      
      REAL      :: DSIVDT( 0:NUMOX ) ! rate of so2 oxid incloud (mol/liter/sec)
      REAL      :: DS4   ( 0:NUMOX ) ! S(IV) oxidized over timestep DTW(0)
      REAL      :: DTW   ( 0:NUMOX ) ! cloud chemistry timestep (sec)

      REAL      :: ONE_OVER_TEMP     ! 1.0 / TEMP


C...........EXTERNAL FUNCTIONS and their descriptions:

      REAL, EXTERNAL :: HLCONST
      LOGICAL, EXTERNAL :: LDARK
C*********************************************************************
C     begin body of subroutine AQRADM

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

C...special treatment of MGLY for CB05 mechanism:
C...  use Henry's law constant for glyoxal as a surrogate for methyl glyoxal

        MGLYSUR = 'GLYOXAL         '

      END IF    ! FIRSTIME

!debug
!     counter = counter + 1
!     write(lun_log,*)'counter: ',counter
!debug
C...Check for bad temperature or pressure

      if ( temp <= 0.0 .or. pres_atm <= 0.0 ) then
         nError   = IV_ERROR
         eRoutine = 'aqradm'
         eMessage = 'Invalid temp and/or pressure'
         eInform  = 'Need positive values'
         eAction  = 'Check met file'
         call WarningMessage(0,.false.)
         return
      end if

      one_over_temp = 1.0 / temp

C...Compute several conversion factors

      icntaq = 0
      iterat = 0
      DSIV_SCALE = 1.0
      rt = ( MOLVOL / STDTEMP ) * temp    ! r * t (liter atm / mol)
      xl   = wcavg * rt / H2ODENS         ! conversion factor (l-atm/mol)
      ONE_OVER_XL = 1.0 / XL
      pres_atm_over_xl = pres_atm / xl
      tst  = 0.999
      act1 = 1.0
      act2 = 1.0
      gm2  = 1.0
      timew = 0.0
      recipap1 = 1.0 / pres_atm

C...set equilibrium constants as a function of temperature
C...Henry's law constants

      so2h  = HLCONST( 'SO2             ', temp, .FALSE., 0.0 )
      co2h  = HLCONST( 'CO2             ', temp, .FALSE., 0.0 )
      nh3h  = HLCONST( 'NH3             ', temp, .FALSE., 0.0 )
      h2o2h = HLCONST( 'H2O2            ', temp, .FALSE., 0.0 )
      o3h   = HLCONST( 'O3              ', temp, .FALSE., 0.0 )
      hclh  = HLCONST( 'HCL             ', temp, .false., 0.0 )
      hno3h = HLCONST( 'HNO3            ', temp, .FALSE., 0.0 )
      mhph  = HLCONST( 'METHYLHYDROPEROX', temp, .FALSE., 0.0 )
      paah  = HLCONST( 'PEROXYACETIC_ACI', temp, .FALSE., 0.0 )
      foah  = HLCONST( 'FORMIC_ACID     ', temp, .FALSE., 0.0 )
      GLYH  = HLCONST( 'GLYOXAL         ', TEMP, .FALSE., 0.0 )
      MGLYH = HLCONST( MGLYSUR,            TEMP, .FALSE., 0.0 )
      hoh   = HLCONST( 'OH              ', temp, .false., 0.0 )

C...Dissociation constants

      temp1 = one_over_temp - 1.0 / 298.0

      foa1  = 1.80e-04 * EXP( -2.00e+01 * temp1 )    ! Martell and Smith (1977)
      sk6   = 1.02e-02 * EXP(  2.72e+03 * temp1 )    ! Smith and Martell (1976)
      so21  = 1.30e-02 * EXP(  1.96e+03 * temp1 )    ! Smith and Martell (1976)
      so22  = 6.60e-08 * EXP(  1.50e+03 * temp1 )    ! Smith and Martell (1976)
      co21  = 4.30e-07 * EXP( -1.00e+03 * temp1 )    ! Smith and Martell (1976)
      co22  = 4.68e-11 * EXP( -1.76e+03 * temp1 )    ! Smith and Martell (1976)
      h2ow  = 1.00e-14 * EXP( -6.71e+03 * temp1 )    ! Smith and Martell (1976)
      nh31  = 1.70e-05 * EXP( -4.50e+02 * temp1 )    ! Smith and Martell (1976)
      hcl1  = 1.74e+06 * EXP(  6.90e+03 * temp1 )    ! Marsh & McElroy (1985)
      hno31 = 1.54e+01 * EXP(  8.70e+03 * temp1 )    ! Schwartz (1984)

C...Kinetic oxidation rates
C...   From Jacobson  (1997)

      rh2o2 = 7.45E+07 * EXP( -15.96 * ( ( 298.0 / TEMP )  - 1.0 ) )

C...   From Jacobson, 1997

      rmhp = 1.90E+07 * EXP( -12.75 * ( ( 298.0 / TEMP )  - 1.0 ) )
      rpaa = 3.60E+07 * EXP( -13.42 * ( ( 298.0 / TEMP )  - 1.0 ) )

C...From Carlton et al. (2007)

      RGLY3  = 3.0E+10   ! rate constant measured at 298K
      RMGLY3 = 3.0E+10   ! assumed to be the same as GLY

C...Make initializations

      LOADING = 0.0
      INITGAS = 0.0

      dsivdt = 0.0
      dtw    = 0.0
      ds4    = 0.0

      dgly1  = 0.0
      dmgly1 = 0.0
      dorgc  = 0.0

C...compute fractional weights for several species
      
      totnit = gas( lhno3 ) + aerosol( lno3, ACC )
      if ( totnit > 0.0 ) then
        fhno3   = gas( lhno3 ) / totnit
        fno3acc = 1. - fhno3
      else
        fhno3   = 1.0
        fno3acc = 0.0
      end if

      totamm = gas( lnh3 ) + aerosol( lnh4, ACC )
      if ( totamm > 0.0 ) then
        fnh3    = gas( lnh3 ) / totamm
        fnh4acc = 1. - fnh3
      else
        fnh3    = 1.0
        fnh4acc = 0.0
      end if

C...Assign fraction partitioning of FE(III) and MN(II)

      IF ( LDARK() ) THEN
        FE_III = 0.9  ! Night time, GS 01July2011
      ELSE
        FE_III = 0.1  ! Day time, GS 01July2011
      END IF

      MN_II = 1.0                     ! Same for day and night, GS  01July2011

C...Assign solubility of Fe and Mn

      FE_SOL = 0.1                    ! GS 01July2011
      MN_SOL = 0.5                    ! GS 28July2011

C...initial concentration from accumulation-mode aerosol loading (mol/liter)
C...  an assumption is made that all of the accumulation-mode
C...  aerosol mass is incorporated into the cloud droplets

      do ispc = 1, NAER
        loading( ispc, ACC ) = aerosol( ispc, ACC ) * pres_atm_over_xl
      end do

      loading( lso4, ACC ) = ( aerosol( lso4, ACC ) + gas( lh2so4 ) ) * pres_atm_over_xl

C...initial concentration from coarse-mode aerosol loading (mol/liter)
C...  an assumption is made that all of the coarse-mode
C...  aerosol mass is incorporated into the cloud droplets

      do ispc = 1, NAER
        loading( ispc, COR ) = aerosol( ispc, COR ) * pres_atm_over_xl
      end do

C...Set constant factors that will be used in later multiplications (moles/atm)

      xlh2o2  = h2o2h * xl
      xlo3    = o3h   * xl
      xlmhp   = mhph  * xl
      xlpaa   = paah  * xl
      xlso2   = so2h  * xl
      xlnh3   = nh3h  * xl
      xlhcl   = hclh  * xl
      xlhno3  = hno3h * xl
      xlco2   = co2h  * xl

      so212   = so21  * so22
      so21h   = so21  * so2h
      so212h  = so212 * so2h
      co212   = co21  * co22
      co21h   = co21  * co2h
      co212h  = co22  * co21h
      nh3dh2o = nh31  / h2ow
      nh31hdh = nh3h  * nh3dh2o
      foa1h   = foa1  * foah
      hcl1h   = hcl1  * hclh
      hno31h  = hno31 * hno3h

C...loop if kinetic calculations are made, return to this point

      DO I20C = 1, 10001

        if ( i20c >= 10000 ) then 
          nError   = AB_ERROR
          eRoutine = 'aqradm'
          eMessage = 'Excessive looping at I20C'
          eInform  = ''
          eAction  = ''
          call WarningMessage(0,.false.)
          return
        end if

C...set aitken-mode aerosol loading (mol/liter)

        omealfat = 1. - EXP( -alfa3 * timew )

        do ispc = 1, NAER
          loading( ispc, AKN ) = aerosol( ispc, AKN ) * pres_atm_over_xl
     &                         * omealfat
        end do

C...Initial gas phase partial pressures (atm)
        initgas( lso2 )  = gas( lso2  ) * pres_atm
     &                   + ds4( 0 ) * xl
        initgas( lnh3 )  = gas( lnh3  ) * pres_atm
     &                   + ( loading( lnh4, ACC ) + loading( lnh4, COR ) + loading( lnh4, AKN ) ) * xl
        initgas( lhno3 ) = ( gas( lhno3 ) + 2.0 * gas( ln2o5 ) ) * pres_atm
     &                   + ( loading( lno3, ACC ) + loading( lno3, COR ) + loading( lno3, AKN ) ) * xl
        initgas( lhcl )  = gas(  lhcl ) * pres_atm
     &                   + ( loading( lcl, ACC ) + loading( lcl, COR ) + loading( lcl, AKN ) ) * xl ! new for sea salt
        initgas( lh2o2 ) = gas( lh2o2 ) * pres_atm
        initgas( lo3 )   = gas( lo3   ) * pres_atm
        initgas( lfoa )  = gas( lfoa  ) * pres_atm
        initgas( lmhp )  = gas( lmhp  ) * pres_atm
        initgas( lpaa )  = gas( lpaa  ) * pres_atm
        initgas( lco2 )  = gas( lco2  ) * pres_atm
        initgas( lgly )  = gas( lgly  ) * pres_atm
     &                   + dgly1 * xl
        initgas( lmgly ) = gas( lmgly  ) * pres_atm
     &                   + dmgly1 * xl
        initgas( lho )   = gas( lho ) * pres_atm

C...Don't allow gas concentrations to go below zero

        do igas = 1, NGAS
          initgas( igas ) = MAX( initgas( igas ), 0.0 )
        end do

C...Molar concentrations of soluble aerosols
        ts6cor  = MAX( loading( lso4,  COR ), 0.0 )
        no3cor  = MAX( loading( lno3,  COR ), 0.0 )
        clcor   = MAX( loading( lcl,   COR ), 0.0 )
        nh4cor  = MAX( loading( lnh4,  COR ), 0.0 )
        soilcor = MAX( loading( lsoilc,COR ), 0.0 ) ! SLN 16March2011
        anthcor = MAX( loading( lanthc,COR ), 0.0 ) ! SLN 16March2011
        seascor = MAX( loading( lseasc,COR ), 0.0 ) ! SLN 16March2011

        fecor   = 0.0281  * (100.0/55.8) * soilcor / ( 1.0 - 0.04642 )     ! SLN 22March2011
     &          + 0.0467  * (100.0/55.8) * anthcor / ( 1.0 - 0.00325 )
        mncor   = 0.00078 * (100.0/54.9) * soilcor / ( 1.0 - 0.04642 )     ! SLN 22March2011
     &          + 0.0011  * (100.0/54.9) * anthcor / ( 1.0 - 0.00325 )
!debug
!        write(lun_log,*)'soilcor,anthcor,fecor,mncor: ',soilcor,anthcor,fecor,mncor
!        call flush(lun_log)
!debug
        nacor   = 0.8373  * ( 23.0/23.0) * seascor                         ! SLN 29March2011
     &          + 0.0652  * (100.0/23.0) * soilcor / ( 1.0 - 0.04642 )
     &          + 0.0023  * (100.0/23.0) * anthcor / ( 1.0 - 0.00325 )
        mgcor   = 0.0997  * ( 23.0/24.3) * seascor                         ! SLN 16March2011
     &          + 0.0000  * (100.0/24.3) * soilcor / ( 1.0 - 0.04642 )
     &          + 0.0032  * (100.0/24.3) * anthcor / ( 1.0 - 0.00325 )
        cacor   = 0.0320  * ( 23.0/40.1) * seascor                         ! SLN 16March2011
     &          + 0.0872  * (100.0/40.1) * soilcor / ( 1.0 - 0.04642 )
     &          + 0.0562  * (100.0/40.1) * anthcor / ( 1.0 - 0.00325 )
        kcor    = 0.0310  * ( 23.0/39.1) * seascor                         ! SLN 16March2011
     &          + 0.0252  * (100.0/39.1) * soilcor / ( 1.0 - 0.04642 )
     &          + 0.0176  * (100.0/39.1) * anthcor / ( 1.0 - 0.00325 )

        ts6     = loading( lso4,  AKN ) + loading( lso4, ACC ) + TS6COR
     &          - DS4( 0 )

        na      = loading( lna,   ACC ) + loading( lna, AKN ) + nacor
        ca      = loading( lcaacc, ACC) + cacor
        mg      = loading( lmgacc, ACC) + mgcor 
        k       = loading( lkacc,  ACC) + kcor 
        fe      = loading( lfeacc, ACC) + fecor
        mn      = loading( lmnacc, ACC) + mncor
!debug
!        write(lun_log,*)'fecor,fe: ',fecor,fe
!        write(lun_log,*)'mncor,mn: ',mncor,mn
!        call flush(lun_log)
!debug
        soa     = loading( lsoa,  ACC )
        orgc    = loading( lorgc, ACC ) + dorgc        ! new in-cloud organic
        poa     = loading( lpoa,  ACC ) + loading( lpoa, AKN )
        ec      = loading( lec,   ACC ) + loading( lec,   AKN )
        prim    = loading( lpri,  ACC ) + loading( lpri,  AKN )

!       numcor  = loading( lnum,  COR )

!       a       = 3.0 * fe
!       b       = 2.0 * mn

C...Don't allow aerosol concentrations to go below zero

        ts6     = MAX( ts6,     0.0 )
        na      = MAX( na,      0.0 )
        ca      = MAX( ca,      0.0 )
        mg      = MAX( mg,      0.0 )
        k       = MAX( k,       0.0 )
        fe      = MAX( fe,      0.0 )
        mn      = MAX( mn,      0.0 )
        soa     = MAX( soa,     0.0 )
        orgc    = MAX( ORGC,    0.0 )
        poa     = MAX( poa,     0.0 )
        prim    = MAX( prim,    0.0 )
!       numcor  = MAX( numcor,  0.0 )

        FE_OX = FE_III * FE_SOL * FE     ! GS 01July2011
        MN_OX = MN_II  * MN_SOL * MN     ! GS 01July2011

!debug
!        write(lun_log,*)'fe_iii,fe_sol,fe,fe_ox: ',fe_iii,fe_sol,fe,fe_ox
!        write(lun_log,*)'mn_ii,mn_sol,mn,mn_ox: ',mn_ii,mn_sol,mn,mn_ox
!        call flush(lun_log)
!debug
        A = 3.0 * FE_OX
        B = 2.0 * MN_OX

        sk6ts6 = sk6 * ts6

C...Find solution of the equation using a method of reiterative 
C...bisections Make initial guesses for pH:   between .01  to  10. 

        ha = PHMIN
        hb = PHMAX

        DO I7777C = 1, 10001
          if ( i7777c >= 10000 ) then
!debug
!        write(lun_log,*)'error: i7777c,ha,hb,fa,fb: ',i7777c,ha,hb,fa,fb
!debug
            nError   = AB_ERROR
            eRoutine = 'aqradm'
            eMessage = 'Excessive looping at I7777C'
            eInform  = ''
            eAction  = ''
            call WarningMessage(0,.false.)
            return
          end if

!     ha = MAX( ha - 0.8, 0.1 )
!     hb = MIN( hb + 0.8, 9.9 )
          ha = MAX( ha - 0.8, PHMIN )
          hb = MIN( hb + 0.8, PHMAX )
          ae = 10.0**( -ha )

!debug
!          write(lun_log,*)'ha,hb,ae: ',ha,hb,ae
!debug
          recipa1 = 1.0 / ( ae * act1 )
          recipa2 = 1.0 / ( ae * ae * act2 )

C...Calculate final gas phase partial pressure of SO2, NH3, HNO3
C...HCOOH, and CO2 (atm)

          pso2f = initgas( lso2 ) / ( 1.0 + xlso2 * ( 1.0 + so21 * recipa1
     &          + so212 * recipa2 ) )
          pnh3f = initgas( lnh3 ) / ( 1.0 + xlnh3 * ( 1.0 + nh3dh2o * ae ) )
          phclf = initgas( lhcl ) / ( 1.0 + xlhcl *  ( 1.0 + hcl1 * recipa1 ) )

          pfoaf = initgas( lfoa ) / ( 1.0 + xl * ( foah + foa1h * recipa1 ) )

          phno3f = initgas( lhno3 ) / ( 1.0 + xlhno3 * ( 1.0 + hno31 * recipa1 ) )

          pco2f = initgas( lco2 ) / ( 1.0 + xlco2 * ( 1.0 + co21 * recipa1
     &          + co212 * recipa2 ) )

C...Calculate liquid phase concentrations (moles/liter) 

          so4  = sk6ts6 / ( ae * gm2 + sk6 )
          hso4 = ts6 - so4
          so3  = so212h  * pso2f  * recipa2
          hso3 = so21h   * pso2f  * recipa1
          co3  = co212h  * pco2f  * recipa2
          hco3 = co21h   * pco2f  * recipa1
          oh   = h2ow    * recipa1
          nh4  = nh31hdh * pnh3f  * ae
          hco2 = foa1h   * pfoaf  * recipa1
          no3  = hno31h  * phno3f * recipa1
          cl   = hcl1h   * phclf  * recipa1 ! new for sea salt

C...Compute functional value

          fa = ae + nh4 + na + k + 2.0 * ( ca + mg - co3 - so3 - so4 )  ! SLN 16March2011
     &       - oh - hco3 - hso3 - no3 - hso4 - hco2 - cl

!debug
!            write(lun_log,*)'fa:' ,fa
!debug
C...Start iteration and bisection ****************<<<<<<<
!debug
!        write(lun_log,*)'before i30c,ha,hb,fa: ',ha,hb,fa
!debug
          DO I30C = 1, 10000
            if ( i30c >= 10000 ) then
              nError   = AB_ERROR
              eRoutine = 'aqradm'
              eMessage = 'Excessive looping at I30C'
              eInform  = ''
              eAction  = ''
              call WarningMessage(0,.false.)
              return
            end if

!           bb = 0.5 * ( ha + hb )
            bb = -LOG10( 0.5 * ( 10.0 **( -ha ) + 10.0 ** (-hb ) ) )
            ae = 10.0**( -bb )

!debug
!         write(lun_log,*)'ae,bb,fa: ',ae,bb,fa
!debug
! --- don't solve for H+ if fa < 0 at first try
            if ( i7777c == 1 .and. fa < 0. ) then

              bb = ha
              hb = ha
              ae = 10.0**( -bb )

            end if

            recipa1 = 1.0 / ( ae * act1 )
            recipa2 = 1.0 / ( ae * ae * act2 )

C...Calculate final gas phase partial pressure of SO2, NH3, HNO3
C...HCOOH, CO2, HCL, HO2, HNO2, and HCHO (atm)

            pso2f = initgas( lso2 ) / ( 1.0 + xlso2
     &            * ( 1.0 + so21 * recipa1 + so212 * recipa2 ) )

            pnh3f = initgas( lnh3 ) / ( 1.0 + xlnh3 * ( 1.0 + nh3dh2o * ae ) )

            phclf = initgas( lhcl ) / ( 1.0 + xlhcl *  ( 1.0 + hcl1 * recipa1 ) )

            phno3f = initgas( lhno3 ) / ( 1.0 + xlhno3 * ( 1.0 + hno31 * recipa1 ) )

            pfoaf = initgas( lfoa ) / ( 1.0 + xl * ( foah + foa1h * recipa1 ) )

            pco2f = initgas( lco2 ) / ( 1.0 + xlco2 * ( 1.0 + co21 * recipa1
     &            + CO212 * RECIPA2 ) )

C...Calculate liquid phase concentrations (moles/liter)

            so4  = sk6ts6 / ( ae * gm2 + sk6 )
            hso4 = ts6 - so4
            so3  = so212h  * pso2f  * recipa2
            hso3 = so21h   * pso2f  * recipa1
            co3  = co212h  * pco2f  * recipa2
            hco3 = co21h   * pco2f  * recipa1
            oh   = h2ow    * recipa1
            nh4  = nh31hdh * pnh3f  * ae
            hco2 = foa1h   * pfoaf  * recipa1
            no3  = hno31h  * phno3f * recipa1
            cl   = hcl1h   * phclf  * recipa1 ! new for sea salt

C...compute functional value

            fb = ae + nh4 + na + k + 2.0 * ( ca + mg - co3 - so3 - so4 )  ! SLN 16March2011
     &         - oh - hco3 - hso3 - no3 - hso4 - hco2 - cl

C...Calculate and check the sign of the product of the two functional values

            ftst = fa * fb
!debug
!         write(lun_log,*)'fa,fb,ftst: ',fa,fb,ftst
!debug
            if ( ftst <= 0.0 ) then 
              hb = bb
            else
              ha = bb
              fa = fb
            end if

C...Check convergence of solutions 

!            htst = ABS(10.0**(-hb)-10.0**(-ha))/0.5*(10.0**(-ha)+10.0**(-hb))
             htst = ABS(10.0**(-hb)-10.0**(-ha))/10.0**(-hb)
             if ( htst < 1.0E-3 ) EXIT    ! exit loop I30C
!            htst = ha / hb
!            if ( htst >= tst ) EXIT  ! exit loop I30C
          end do   ! I30C
!debug
!        write(lun_log,*)'after i30c,ha,hb,fa,fb,htst: ',i30c,ha,hb,fa,fb,htst
!debug

C...end of zero-finding routine ****************<<<<<<<<<<<< 

C...compute Ionic strength and activity coefficient by the Davies equation

          stion = 0.5
     &          * ( ae + nh4 + oh + hco3 + hso3
     &              + 4.0 * ( so4 + co3 + so3 + ca + mg + mn_ox )
     &              + no3 + hso4 + 9.0 * fe_ox + na + k + cl + a + b + hco2 ) ! KMF 08September2011
          gm1log = -0.509 * ( SQRT( stion )
     &           / ( 1.0 + SQRT( stion ) ) - 0.2 * stion )
          gm2log = gm1log * 4.0
          gm1  = 10.0**gm1log
          gm2  = MAX( 10.0**gm2log, 1.0e-30 )
          actb = act1
          act1 = MAX( gm1 * gm1, 1.0e-30 )
          act2 = MAX( gm1 * gm1 * gm2, 1.0e-30 )

C...check for convergence and possibly go to I7777C, to recompute
C...  Gas and liquid phase concentrations

! --- don't solve for H+ if fa < 0 at first try
          if ( i7777c == 1 .and. fa < 0. ) then
            actb = act1
          end if

! --- if both ha and hb are less than -1, set them to -1 and
! --- finish iteration
          if (ha <= -1.0 .and. hb <= -1.0) then
             ha = -1.0
             hb = -1.0
             bb = ha
             actb = act1
          end if

          tac = ABS( actb - act1 ) / actb
!debug
!       write(lun_log,*)'stion,tac,act1,actb: ',stion,tac,act1,actb
!debug
          if ( tac < 1.0E-2 ) EXIT    ! exit loop I7777C

          icntaq = icntaq + 1
          if ( icntaq >= 60000 ) then
            nError   = WN_ERROR
            eRoutine = 'aqradm'
            eMessage = 'Maximum iterations for pH calculation exceeded'
            eInform  = 'Using last pH value'
            eAction  = ''
            icntaq = 0
            call WarningMessage(0,.true.)
            EXIT    ! exit loop I7777C
          end if
!debug
!        write(lun_log,*)'i7777c,ha,hb,fa,fb: ',i7777c,ha,hb,fa,fb
!debug

        end do     ! end of do loop I7777C

C...return an error if the pH is not in range 

ccc      if ( ( ha .lt. 0.02 ) .or. ( ha .gt. 9.49 ) ) then 
        if ( ( ha < PHMIN ) .or. ( ha > PHMAX ) ) then 
          print *, ha 
          nError   = AB_ERROR
          eRoutine = 'aqradm'
          eMessage = 'pH value out of range'
          eInform  = ''
          eAction  = ''
          call WarningMessage(0,.false.)
          return
        end if

C...Make those concentration calculations which can be made outside
C...  of the function.

        so2l = so2h * pso2f
        ac = 10.0**( -bb )
        siv = so3 + hso3 + so2l

C...Calculate final gas phase concentrations of oxidants (atm) 

        ph2o2f = ( initgas( lh2o2 ) + xl * ds4( 1 ) ) / ( 1.0 + xlh2o2 )
        po3f   = ( initgas( lo3 )   + xl * ds4( 2 ) ) / ( 1.0 + xlo3   )
        pmhpf  = ( initgas( lmhp )  + xl * ds4( 4 ) ) / ( 1.0 + xlmhp  )
        ppaaf  = ( initgas( lpaa )  + xl * ds4( 5 ) ) / ( 1.0 + xlpaa  )
        pglyf  = ( initgas( lgly )                  ) / ( 1.0 + glyh * xl )
        pmglyf = ( initgas( lmgly )                 ) / ( 1.0 + mglyh * xl )
        phof   = ( initgas( lho )                   ) / ( 1.0 + hoh * xl)

!debug
!        write(lun_log,*)'before adjustment; ph2o2f = ',ph2o2f
!        call flush(lun_log)
!debug
        ph2o2f = MAX( ph2o2f, 0.0 )
        po3f   = MAX( po3f,   0.0 )
        pmhpf  = MAX( pmhpf,  0.0 )
        ppaaf  = MAX( ppaaf,  0.0 )

C...Calculate liquid phase concentrations of oxidants (moles/liter) 

        h2o2l = ph2o2f * h2o2h
        o3l   = po3f   * o3h
        mhpl  = pmhpf  * mhph
        paal  = ppaaf  * paah
        foal  = pfoaf  * foah
        nh3l  = pnh3f  * nh3h
        co2l  = pco2f  * co2h
        hcll  = phclf  * hclh
        hno3l = phno3f * hno3h
        glyl  = pglyf  * glyh
        mglyl = pmglyf * mglyh
        ohl   = phof   * hoh

C...compute modal concentrations

        so4cor  = sk6 * ts6cor / ( ae * gm2 + sk6 )
        hso4cor = MAX( ts6cor - so4cor, 0.0 )

        ts6acc  = MAX( ts6  - ts6cor,   0.0 )
        so4acc  = MAX( so4  - so4cor,   0.0 )
        hso4acc = MAX( hso4 - hso4cor,  0.0 )
        no3acc  = MAX( no3 - no3cor,   0.0 )
        naacc   = MAX( na   - nacor,    0.0 )
        clacc   = MAX( cl   - clcor,    0.0 )
        nh4acc  = MAX( nh4  - nh4cor,   0.0 )
        caacc   = MAX( ca   - cacor,    0.0 ) ! AE6       
        mgacc   = MAX( mg   - mgcor,    0.0 ) ! AE6      
        kacc    = MAX( k    - kcor,     0.0 ) ! AE6      
        feacc   = MAX( fe   - fecor,    0.0 ) ! AE6
        mnacc   = MAX( mn   - mncor,    0.0 ) ! AE6

C...load the liquid concentration array with current values

        liquid( lacl      ) = ac
        liquid( lnh4accl  ) = nh4acc
        liquid( lcacorl   ) = cacor
        liquid( lnaaccl   ) = naacc
        liquid( lohl      ) = ohl
        liquid( lso4accl  ) = so4acc
        liquid( lhso4accl ) = hso4acc
        liquid( lso3l     ) = so3
        liquid( lhso3l    ) = hso3
        liquid( lso2l     ) = so2l
        liquid( lco3l     ) = co3
        liquid( lhco3l    ) = hco3
        liquid( lco2l     ) = co2l
        liquid( lno3accl  ) = no3acc
        liquid( lnh3l     ) = nh3l
        liquid( lclaccl   ) = clacc
        liquid( lh2o2l    ) = h2o2l
        liquid( lo3l      ) = o3l
        liquid( lfecorl   ) = fecor
        liquid( lmncorl   ) = mncor
        liquid( lal       ) = a
        liquid( lfoal     ) = foal
        liquid( lhco2l    ) = hco2
        liquid( lmhpl     ) = mhpl
        liquid( lpaal     ) = paal
        liquid( lhcll     ) = hcll
        liquid( lpriml    ) = prim
        liquid( lmgcorl   ) = mgcor
        liquid( lkcorl    ) = kcor
        liquid( lbl       ) = b
        liquid( lhno3l    ) = hno3l
!       liquid( lnumcorl  ) = numcor
        liquid( lts6corl  ) = ts6cor
        liquid( lnacorl   ) = nacor
        liquid( lclcorl   ) = clcor
        liquid( lno3corl  ) = no3cor
        liquid( lnh4corl  ) = nh4cor
        liquid( lpoal     ) = poa
        liquid( lecl      ) = ec
        liquid( lsoal     ) = soa
        liquid( lorgcl    ) = orgc  
        liquid( lglyl     ) = glyl    
        liquid( lmglyl    ) = mglyl   
        liquid( lcaaccl   ) = caacc   ! AE6 - SLN 16March2011      
        liquid( lmgaccl   ) = mgacc   ! AE6 - SLN 16March2011     
        liquid( lkaccl    ) = kacc    ! AE6 - SLN 16March2011     
        liquid( lsoilcl   ) = soilcor ! AE6 - SLN 16March2011
        liquid( lanthcl   ) = anthcor ! AE6 - SLN 16March2011
        liquid( lseascl   ) = seascor ! AE6 - SLN 16March2011
        liquid( lfeaccl   ) = feacc   ! AE6 - SLN 22March2011
        liquid( lmnaccl   ) = mnacc   ! AE6 - SLN 22March2011

C...if the maximum cloud lifetime has not been reached, then compute
C...the next timestep.

        if ( timew >= taucld ) EXIT   ! exit 20 loop

C...make kinetics calculations
C...  note: DS4(i) and DSIV(I) are negative numbers!

        iterat = iterat + 1

C...Define the total S(iv) available for oxidation

        tsiv = initgas( lso2 ) * one_over_xl

C...Calculate sulfur iv oxidation rate due to H2O2 (Jacobson, 1997)

        dsivdt( 1 ) = -rh2o2 * h2o2l * hso3 * ac / ( 1.0 + 13.0 * AC )
        th2o2 = initgas( lh2o2 ) * one_over_xl
!debug
!        write(lun_log,*)'dsivdt(1),tsiv,th2o2: ',dsivdt(1),tsiv,th2o2
!        call flush(lun_log)
!debug
        if ( ( dsivdt( 1 ) == 0.0 ) .or.
     &       ( tsiv  <= CONMIN ) .or.
     &       ( th2o2 <= CONMIN ) ) then
          dtw(1) = taucld
        else
          dtw( 1 ) = -0.05 * MIN( th2o2, tsiv ) / dsivdt( 1 )
        end if
!debug
!        write(lun_log,*)'taucld,dtw(1): ',taucld,dtw(1)
!        call flush(lun_log)
!debug

C...Calculate sulfur iv oxidation rate due to O3 (Jacobson, 1997)

        dsivdt( 2 ) = -( 2.4E4 * so2l +
     &                   3.7E5 * EXP( -18.56 * ( ( 298.0 / temp ) - 1.0 ) ) * hso3 + 
     &                   1.5E9 * EXP( -17.72 * ( ( 298.0 / temp ) - 1.0 ) ) * so3 ) * o3l 

        to3 = initgas( lo3 ) * one_over_xl
!debug
!        write(lun_log,*)'dsivdt(2),tsiv,to3: ',dsivdt(2),tsiv,to3
!        call flush(lun_log)
!debug
        if ( ( dsivdt( 2 ) == 0.0 ) .or.
     &       ( tsiv  <= CONMIN ) .or.
     &       ( to3 <= CONMIN ) ) then
          dtw( 2 ) = taucld
        else
          dtw( 2 ) = -0.01 * MIN( to3, tsiv ) / dsivdt( 2 )
        end if
!debug
!        write(lun_log,*)'taucld,dtw(2): ',taucld,dtw(2)
!        call flush(lun_log)
!debug

C...Calculate sulfur iv oxidation rate due to O2 catalyzed by Mn++ 
C...(Martin and Goodman, 1991)
!debug
!        write(lun_log,*)'mn_ox, fe_ox, siv: ',mn_ox,fe_ox,siv
!        call flush(lun_log)
!debug
!pk, ENVIRON, August 2012: Martin and Good for Fe only and Mn only terms
        dsivdt( 3 ) = - ( 750.0  * mn_ox * siv +                 ! GS 4May2011
     &                    2600.0 * fe_ox * siv )                 ! GS 4May2011
!debug
!        write(lun_log,*)'dsivdt(3) for Fe and Mn only: ',dsivdt(3)
!debug
!pk, ENVIRON, August 2012: use IT87 rate expression for synergistic term
        if ( bb >= 4.2 )  then  ! 4.2  <= ph
           dsivdt( 3 ) = dsivdt( 3 ) - ( 2.51E13 * mn_ox * fe_ox * siv ) * ( ac ** 0.67)
        else
           dsivdt( 3 ) = dsivdt( 3 ) - ( 3.72E7 * mn_ox * fe_ox * siv ) / ( ac ** 0.74)
        end if
!debug
!        write(lun_log,*)'Total dsivdt(3): ',dsivdt(3)
!debug
!pk, ENVIRON, August 2012: Apply MG91 correction for sulfate inhibition
        dsivdt( 3 ) = dsivdt( 3 ) / ( 1. + 75.0 * ( TS6 ** 0.67 ) )
!debug
!        write(lun_log,*)'ts6,dsivdt(3): ',ts6,dsivdt(3)
!debug

!debug
!        write(lun_log,*)'dsivdt(3),tsiv: ',dsivdt(3),tsiv
!        call flush(lun_log)
!debug
        if ( ( dsivdt( 3 ) == 0.0 ) .or. ( tsiv <= CONMIN ) ) then
          dtw( 3 ) = taucld
        else
          dtw( 3 ) = -0.1 * tsiv / dsivdt( 3 )
        end if
!debug
!        write(lun_log,*)'taucld,dtw(3): ',taucld,dtw(3)
!        call flush(lun_log)
!debug

C...Calculate sulfur oxidation rate due to MHP (Jacobson,  1997)

        dsivdt( 4 ) = -rmhp * ac * mhpl * hso3 
        tmhp = initgas( lmhp ) * one_over_xl
!debug
!        write(lun_log,*)'dsivdt(4),tsiv,tmhp: ',dsivdt(4),tsiv,tmhp
!        call flush(lun_log)
!debug
        if ( ( dsivdt( 4 ) == 0.0 ) .or.
     &       ( tsiv  <= CONMIN ) .or.
     &       ( tmhp <= CONMIN ) ) then
          dtw( 4 ) = taucld
        else
          dtw( 4 ) = -0.1 * MIN( tmhp, tsiv ) / dsivdt( 4 )
        end if
!debug
!        write(lun_log,*)'taucld,dtw(4): ',taucld,dtw(4)
!        call flush(lun_log)
!debug

C...Calculate sulfur oxidation due to PAA (Jacobson,  1997)

        dsivdt( 5 ) = -( rpaa * ac + 7.00E2 ) * hso3 * paal
        tpaa = initgas( lpaa ) * one_over_xl
!debug
!        write(lun_log,*)'dsivdt(5),tsiv,tpaa: ',dsivdt(5),tsiv,tpaa
!        call flush(lun_log)
!debug
        if ( ( dsivdt( 5 ) == 0.0 ) .or.
     &       ( tsiv  <= CONMIN ) .or.
     &       ( tpaa <= CONMIN ) ) then
          dtw( 5 ) = taucld
        else
          dtw( 5 ) = -0.1 * MIN( tpaa, tsiv ) / dsivdt( 5 )
        end if
!debug
!        write(lun_log,*)'taucld,dtw(5): ',taucld,dtw(5)
!        call flush(lun_log)
!debug

C...Calculate total sulfur iv oxidation rate

        dsivdt( 0 ) = 0.0
        do iox = 1, NUMOX
          dsivdt( 0 ) = dsivdt( 0 ) + dsivdt( iox )
        end do

C...Calculate a minimum time step required

        dtw( 0 ) = MIN( dtw( 1 ), dtw( 2 ), dtw( 3 ),
     &                  dtw( 4 ), dtw( 5 ) )

!debug
!      write(lun_log,*)'dsivdt(0),dtw(0): ',dsivdt(0),dtw(0)
!      call flush(lun_log)
!debug
C...check for large time step

        if ( dtw( 0 ) > 8.0e+37 ) then
          write(6,1001) dsivdt(0), ts6, dtw(0)
        else

C...CALCULATE IN-CLOUD SOA PRODUCTION
C...  Reference:  Carlton, A.G., B.J. Turpin, K.E. Altieri, A. Reff,
C...  S. Seitzinger, H.J. Lim, and B. Ervens (2007), Atmospheric Oxalic
C...  Acid and SOA Production from Glyoxal: Results of Aqueous
C...  Photooxidation Experiments, Atmos. Environ., 41(35), 7588-7602.

C...Define the total glyoxal available for oxidation

          tgly = initgas( lgly ) * one_over_xl

C...Calculate GLY oxidation due to OH

          dglydt = -rgly3 * glyl * ohl

C...Define the total methylglyoxal available for oxidation

          tmgly = initgas( lmgly ) * one_over_xl

C...Calculate MGLY oxidation due to OH

          dmglydt = -rmgly3 * mglyl * ohl
        
!ccC...Define the total OH available for oxidation
!cc
!cc          THO = PHO0 * ONE_OVER_XL

C...Calculate OH consumption
        
!steadystate          DOHDT = -( RGLY3 * GLYL + RMGLY3 * MGLYL ) * OHL

C...calculate the change in sulfur iv for this time step

60        CONTINUE
          dts6 = ABS( dtw( 0 ) * dsivdt( 0 ) )

!debug
!          write(lun_log,*)'60 loop; dtw(0),ts6,dts6: ',dtw(0),ts6,dts6
!          call flush(lun_log)
!debug
C...If DSIV(0), sulfur iv oxidized during this time step would be 
C...less than 5% of sulfur oxidized since time 0, then double DT 

          if ( dtw( 0 ) <= taucld ) then
            if ( dts6 < 0.05 * ts6 ) then 
              dtw( 0 ) = dtw( 0 ) * 2.0 
	      go to 60
            end if
          end if
        end if
        dtw( 0 ) = MIN( dtw( 0 ), taucld )

C...Limit the timestep to prevent negative SO2 concentrations and mass creation
C...  for sulfate (suggested by Bonyoung Koo)

        if ( dsivdt( 0 ) < 0.0 ) then
!         DTW( 0 ) = MIN( DTW( 0 ), -TSIV * 1.00001 / DSIVDT( 0 ) )
          dtw( 0 ) = MIN( dtw( 0 ), -tsiv / dsivdt( 0 ) )
        end if
!debug
!        write(lun_log,*)'dtw0 after checking for SO2-Koo correction: ',dtw(0)
!        call flush(lun_log)
!debug
C...If the total time after this time increment will be greater than 
C...  TAUCLD sec., then set DTW(0) so that total time will be TAUCLD

        if ( timew + dtw( 0 ) > taucld ) dtw( 0 ) = taucld - timew
!       if ( ts6 < 1.0e-11 ) dtw( 0 ) = taucld - timew
!       if ( iterat > 100 ) dtw( 0 ) = taucld - timew 
        if ( iterat > 100 ) dtw( 0 ) = MAX( 1.0, dtw( 0 ) )

C...force mass balance for the specified timestep
C...  for GLY and MGLY, assume that OH is in steady state

        dglydt  = MAX( dglydt,  -tgly  / dtw( 0 ) )
        dmglydt = MAX( dmglydt, -tmgly / dtw( 0 ) )

C...  for S(IV), also limit by oxidants (except assume O2 in steady state)

!debug
!        write(lun_log,*)'dsivdt(1),dtw(0): ',dsivdt(1),dtw(0)
!        write(lun_log,*)'tsiv,th2o2: ',tsiv,th2o2
!        write(lun_log,*)'-MIN(tsiv,th2o2)/dtw(0): ',-MIN(tsiv,th2o2)/dtw(0)
!        call flush(lun_log)
!debug
        dsivdt( 1 ) = MAX( dsivdt( 1 ), -MIN( tsiv, th2o2 ) / dtw( 0 ) )
!debug
!        write(lun_log,*)'dsivdt(1) after: ',dsivdt(1)
!        write(lun_log,*)'dsivdt(2): ',dsivdt(2)
!        call flush(lun_log)
!debug
        dsivdt( 2 ) = MAX( dsivdt( 2 ), -MIN( tsiv, to3   ) / dtw( 0 ) )
!debug
!        write(lun_log,*)'dsivdt(2) after: ',dsivdt(2)
!        write(lun_log,*)'dsivdt(3): ',dsivdt(3)
!        call flush(lun_log)
!debug
        dsivdt( 3 ) = MAX( dsivdt( 3 ), -tsiv / dtw( 0 ) )
!debug
!        write(lun_log,*)'dsivdt(3) after: ',dsivdt(3)
!        write(lun_log,*)'dsivdt(4): ',dsivdt(4)
!        call flush(lun_log)
!debug
        dsivdt( 4 ) = MAX( dsivdt( 4 ), -MIN( tsiv, tmhp  ) / dtw( 0 ) )
!debug
!        write(lun_log,*)'dsivdt(4) after: ',dsivdt(4)
!        write(lun_log,*)'dsivdt(5): ',dsivdt(5)
!        call flush(lun_log)
!debug
        dsivdt( 5 ) = MAX( dsivdt( 5 ), -MIN( tsiv, tpaa  ) / dtw( 0 ) )
!debug
!        write(lun_log,*)'dsivdt(5) after: ',dsivdt(5)
!        call flush(lun_log)
!debug

C...  recalculate the total S(iv) oxidation rate

        dsivdt( 0 ) = 0.0
        do iox = 1, NUMOX
          dsivdt( 0 ) = dsivdt( 0 ) + dsivdt( iox )
        end do

!debug
!      write(lun_log,*)'dsivdt(0) recalculated: ',dsivdt(0)
!      call flush(lun_log)
!debug
C...  if the total S(iv) oxidized over the timestep exceeds the amount of
C...    S(iv) available then scale the rates to conserve mass

        if (-dsivdt( 0 ) * dtw( 0 ) > tsiv ) then
          dsiv_scale = tsiv / ( -dsivdt( 0 ) * dtw( 0 ) )
          dsivdt( 0 ) = dsivdt( 0 ) * dsiv_scale
          dsivdt( 1 ) = dsivdt( 1 ) * dsiv_scale
          dsivdt( 2 ) = dsivdt( 2 ) * dsiv_scale
          dsivdt( 3 ) = dsivdt( 3 ) * dsiv_scale
          dsivdt( 4 ) = dsivdt( 4 ) * dsiv_scale
          dsivdt( 5 ) = dsivdt( 5 ) * dsiv_scale       	
        end if
!debug
!      write(lun_log,*)'dsivdt(0) after checking for siv mass: ',dsivdt(0)
!      call flush(lun_log)
!debug

C...Set DSIV(I), I = 0,NUMOX, the amount of S(IV) oxidized by each 
C... individual oxidizing agent, as well as the total.

        do iox = 0, NUMOX
          ds4( iox ) = ds4( iox ) + dtw( 0 ) * dsivdt( iox )
!debug
!          write(lun_log,*)'iox,ds4: ',iox,ds4(iox)
!          call flush(lun_log)
!debug
        end do

        dgly1  = dgly1  + dtw( 0 ) * dglydt

        dmgly1 = dmgly1 + dtw( 0 ) * dmglydt

csteadystate        DOH1   = DOH1   + DTW( 0 ) * DOHDT

C...Calculate AORGC Production:  4% SOAcld (ORGC) yield from glyoxal
C...  and methylglyoxal is assumed

        dorgc = dorgc - ( 0.04 * ( dglydt + dmglydt ) * dtw( 0 ) )

        timew = timew + dtw( 0 )

      end do     ! I20C loop

C...Compute the output concentrations

C...gas concentrations (mol/molV)

      totamm = ( pnh3f  + ( nh4acc + nh3l  ) * xl ) * recipap1
      totnit = ( phno3f + ( no3acc + hno3l ) * xl ) * recipap1

      gas( LSO2   ) = ( pso2f   + xl *  siv )   * recipap1
      gas( LH2O2  ) = ( ph2o2f  + xl *  h2o2l ) * recipap1
      gas( LO3    ) = ( po3f    + xl *  o3l )   * recipap1
      gas( LCO2   ) = ( pco2f  + xl *  co2l )  * recipap1
      gas( LFOA   ) = ( pfoaf  + xl * ( foal + hco2 ) ) * recipap1
      gas( LMHP   ) = ( pmhpf   + xl *  mhpl )  * recipap1
      gas( LPAA   ) = ( ppaaf   + xl *  paal )  * recipap1
      gas( LHCL   ) = ( phclf   + xl *  hcll )  * recipap1
      gas( LGLY   ) = ( pglyf   + xl *  glyl )   * recipap1
      GAS( LMGLY  ) = ( PMGLYF  + XL *  MGLYL)   * RECIPAP1
!     gas( LHO    ) = ( phof   + xl *  ohl  )  * recipap1

      gas( LNH3   ) = fnh3  * totamm
      gas( LHNO3  ) = fhno3 * totnit
      gas( LN2O5  ) = 0.0 ! assume all into aerosol
      gas( LH2SO4 ) = 0.0 ! assume all into aerosol

C...aerosol concentrations (mol/molV)
      ealfa0t   = EXP( -alfa0 * taucld )
      ealfa2t   = EXP( -alfa2 * taucld )
      ealfa3t   = EXP( -alfa3 * taucld )

      aerosol( LSO4, AKN ) = aerosol( LSO4, AKN ) * ealfa3t
      aerosol( LNH4, AKN ) = aerosol( LNH4, AKN ) * ealfa3t
      aerosol( LNO3, AKN ) = aerosol( LNO3, AKN ) * ealfa3t
      aerosol( LEC,  AKN ) = aerosol( LEC,  AKN ) * ealfa3t
      aerosol( LPRI, AKN ) = aerosol( LPRI, AKN ) * ealfa3t

      aerosol( LPOA, AKN ) = aerosol( LPOA, AKN ) * ealfa3t

      aerosol( LSO4, ACC ) = ts6acc * xl * recipap1
      aerosol( LEC,  ACC ) = ec     * xl * recipap1
      aerosol( LPRI, ACC ) = prim   * xl * recipap1

      aerosol( LSOA,  ACC ) = soa  * xl * recipap1
      aerosol( LORGC, ACC ) = orgc * xl * recipap1
      aerosol( LPOA,  ACC ) = poa  * xl * recipap1

      aerosol( LNH4, ACC ) = fnh4acc * totamm
      aerosol( LNO3, ACC ) = fno3acc * totnit

      aerosol( LSO4, COR ) = ts6cor * xl * recipap1
      aerosol( LNO3, COR ) = no3cor * xl * recipap1
      aerosol( LNH4, COR ) = nh4cor * xl * recipap1

      aerosol( LNA, AKN  ) = aerosol( LNA, AKN ) * ealfa3t
      aerosol( LCL, AKN  ) = aerosol( LCL, AKN ) * ealfa3t
      aerosol( LNA, ACC  ) = naacc * xl * recipap1
      aerosol( LCL, ACC  ) = clacc * xl * recipap1

      aerosol( LNUM, AKN ) = aerosol( LNUM, AKN ) * ealfa0t
      aerosol( LSRF, AKN ) = aerosol( LSRF, AKN ) * ealfa2t

!     aerosol( LNUM, COR ) = numcor * xl * recipap1

      aerosol( LCAACC, ACC ) = caacc   * xl * recipap1 ! AE6 - SLN 16March2011
      aerosol( LMGACC, ACC ) = mgacc   * xl * recipap1 ! AE6 - SLN 16March2011
      aerosol( LKACC,  ACC ) = kacc    * xl * recipap1 ! AE6 - SLN 16March2011
      aerosol( LSOILC, COR ) = soilcor * xl * recipap1 ! AE6 - SLN 16March2011
      aerosol( LANTHC, COR ) = anthcor * xl * recipap1 ! AE6 - SLN 16March2011
      aerosol( LSEASC, COR ) = seascor * xl * recipap1 ! AE6 - SLN 16March2011
      aerosol( LFEACC, ACC ) = feacc   * xl * recipap1 ! AE6 - SLN 22March2011
      aerosol( LMNACC, ACC ) = mnacc   * xl * recipap1 ! AE6 - SLN 22March2011

! hydrogen ion concentrations (for scavenging coefficient calculations in
! driver routine)

      hplus = liquid( lacl      )

      return

C...formats

1001  format (1X,'DSIVDT(0) =', F10.5,  
     &       'TS6=', F10.5, 'DTW(0)=', F10.5)

      end
