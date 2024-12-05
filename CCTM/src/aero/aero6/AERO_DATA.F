
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
      Module aero_data

C  Defines aerosol species arrays and the parameters required in aerosol
C  processing.

C  Contains:
C     Subroutine map_aero
C     Subroutine extract_aero
C     Subroutine update_aero
C     Function findAero

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.

C HS = Heather Simon; GS = Golam Sarwar; SH = Steve Howard; SR = Shawn Roselle;
C JY = Jeff Young; HOTP = Havala Pye; KF = Kathleen Fahey, CGN = Chris Nolte;
C PL = Peng Liu; BM = Ben Murphy; BH = Bill Hutzell, NS = Nash Skipper

C HS  01/24/11 Updated to initial AERO6; PMOTHR -> 9 species
C GS  03/02/11 Revised parameters for Mg, K, and Ca
C SH  03/10/11 Inserted functionality of PMEM_DEFN
C    -new subroutine map_pmemis
C HS  03/10/11 Made PNCOM a required species
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C SH  04/04/11 Added sea-salt speciation factors
C GS  04/09/11 Updated sea-salt speciation; replaced ANAK with ASEACAT;
C              made MG, K, CA, SEACAT required species;
C JY  04/21/11 Added optional log messages (verbose_aero)
C JY  05/02/11 Added Reshape to aerospc_ssf initialization for pgf90 compiler
C BH  08/31/11 Adapted for mercury and HAP mechanisms
C JY  06/08/12 remove full character blank padding put in for GNU Fortran (GCC) 4.1.2
C HOTP 01/16/13 Removed isoprene acid enhanced aerosol and added new isoprene aerosol
C HOTP 01/18/13 Added information for particle phase reaction of isoprene products
C               based on Eddingsaas et al. 2010
C KF  09/17/14 Changed emitted modal PM mass fractions and geometric mean diameter and
C              geometric standard deviation of emitted particles according to
C              Elleman and Covert (2010)
C HOTP 09/27/14 Added alkane and PAH SOA species. PAH SOA densities follow Chan et al.
C JY  08/24/15 Changed visual index factors
C HOTP 2/17/2016 Updated SOA densities. BTX follows Ng et al. 2007.
C                1.4 g/cm3 used by default.
C JY  02/17/16 Created named constants for speciation and other factors for AERO_EMIS,
C              DUST_EMIS, aero_subs:VOLINORG, and aqchem
C CGN 04/14/16 Changed AMGJ speciation factor from 0.0 to 0.019 following Upadhyay et al.
C HOTP,BM  5/20/16 Updated to work with all ae6 mechs (6, 6i, 6mp)
C PL 05/15/16 Update visual_idx, and add visual_idx_large in spcs_type, due to ammonium sulfate, nitrate and OM
C             are split into small and large modes.
C             visual_idx for ammonium is zero, because it will be treated together with sulfate and nitrate
C             see Pitchford et al., Journal of the Air & Waste Management Association (57)(2007), pp 1326
C PL 05/15/16 Add "om" in spcs_type, to flag organic aerosols, which
C             will be used to calculate total mass of organic aerosls for estimating
C             aerosol extinction efficiency
C HOTP 7/17/2018 Added kappaorg hygroscopicity parameter (Petters and
C             Kreidenweis 2007 ACP) for organic aerosols. kappa should be zero for
C             species that are not organic. Water uptake onto inorganics calculated using ISORROPIA.
C             POC+NCOM kappa is set to zero and calculated in SOA_DEFN if those species are in use.
C             Current kappaorg values are based on species specific
C             OM/OC ratios (Pye et al. 2017 ACP).
C GS  08/17/81 Added bromide in seasalt
C SR 12/14/2018 Added sulfur tracking option
C SLN 12/30/2019 ddm-3d implementaiton for v 5.3.1
C     10 Mar 2021 G. Sarwar: Changed CB6R3M_AE7_KMTBR to CB6R3M_AE7_AQ
C     10 June 2021 G. Sarwar: Added CB6R5M_AE7_AQ
C 23 Jun 21 G. Sarwar: Replaced CB6R3M with CB6R5M
C NS 11/28/2023 CRACMM2 updates

C References:
C 1. Eddingsaas, N. C., Vandervelde, D. G., Wennberg, P. O., "Kinetics and
C    products of the acid-catalyzed ring-opening of atmospherically relevant
C    butyl epoxy alcohols," J. Phys. Chem. A., 2010, 114, 8106-8113.
C 2. Chan, et al., "Secondary organic aerosol formation from photooxidation
C    of naphthalene and alkylnaphthalenes: implications for oxidation of
C    intermediate volatility organic compounds (IVOCs)", Atmos. Chem. Phys.,
C    2009, 9, 3049-3060.
C 3. Ng, N. L., Kroll, J. H., Chan, A. W. H., Chhabra, P. S., Flagan, R.
C    C., and Seinfeld, J. H.: Secondary organic aerosol formation from
C    m-xylene, toluene, and benzene, Atmos. Chem. Phys., 7, 3909-3922,
C    doi:10.5194/acp-7-3909-2007, 2007.
C 4. Elleman and Covert, "Aerosol size distribution modeling with the Community Multiscale
C    Air Quality modeling system in the Pacific Northwest: 3. Size distribution of
C    particles emitted into a mesoscale model", JGR, 115, D03204, 2010
C 5. Simon, et al., "The development and uses of EPA's SPECIATE database",
C    Atmos. Poll. Res., 1, 196-206, 2010
C 6. Upadhyay, et al., "Size-Differentiated Chemical Composition of Re-Suspended Soil
C    Dust from the Desert Southwest United States," Aero. and AQ Res., 2015, 387-398

C JY: From Christian Hogrefe...
C Based on Malm and Hand (Atmos. Env. 41, 3407-3427, 2007), the revised
C IMPROVE extinction calculation includes coarse particles, sea salt, and
C a relative humidity correction for sea salt. Also, the factor for "LAC"
C (light absorbing carbon, i.e. AECI and AECJ) should be 10, not 0 since
C both scattering and absorption contribute to total extinction.
C ASEACAT includes all sea-salt cations in coarse mode (Na, Ca, K, and Mg)
C Also note...
C In the Fortran user-derived spcs_type, below, visual_idx is an optimal dry mass
C extinction efficiency [m^2/g], see White, Atmos.Env., 294(10)(1990), pp 2673-1672
C and Malm, et al., JGR, 99(D1)(1994), pp 1347-1370
C----------------------------------------------------------------------
      USE RUNTIME_VARS
      Use utilio_defn
#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
      Use aero_ddm3d, ONLY : s_aerospc_conc, ae_ddm3d_ready,
     &                       init_aero_ddm3d
#endif 


      Implicit None

C Define Logical values as T and F for the aerospc table
      Logical, Parameter, Private :: T = .true.
      Logical, Parameter, Private :: F = .false.

C Number of aerosol species and modes

      Integer, Parameter :: n_aerolist = 139    ! number of aero species
      Integer, Parameter :: n_mode = 3          ! number of modes:
      Integer, Parameter :: iait = 1            ! 1 = Aitken       
      Integer, Parameter :: iacc = 2            ! 2 = accumulation 
      Integer, Parameter :: icor = 3            ! 3 = coarse       
      
      Logical, Parameter :: reqd_modes( n_mode ) = (/ T,T,T /)
      Character(1), Parameter :: modesuff( n_mode ) = (/ 'I','J','K' /)

      Integer, Save :: n_aerospc ! number of aero species

C Default minimum concentration
      Real,    Parameter :: conmin  = 1.0E-23    ! [ ug m-3 ]
      Real(8), Parameter :: conminD = 1.0D-23    ! [ ug m-3 ]
      Real,    Parameter :: evapmin = 1.0E-20    ! [ ug m-3 ]
      Real(8), Parameter :: evapminD= 1.0D-20    ! [ ug m-3 ]
      Real,    Parameter :: cm_set( n_mode ) = (/conmin, conmin, conmin/)
      Real,    Parameter :: cm_so4( n_mode ) = (/conmin, conmin, conmin/)
      Real,    Parameter :: cm_cor( n_mode ) = (/conmin, conmin, conmin/)

      Real,    Parameter :: def_diam( n_mode )   = (/ 15.0E-9, 80.0E-9,  600.0E-9 /)  ! default background mean diameter for each mode
      Real,    Parameter :: min_dg_dry( n_mode ) = (/ 1.0E-9,  30.0E-9,  120.0E-9 /)
      Real,    Parameter :: max_dg_dry( n_mode ) = (/ 80.0E-9, 500.0E-9, 100.0E-6 /)
      Real,    Parameter :: min_dg_wet( n_mode ) = (/ 1.0E-9,  30.0E-9,  120.0E-9 /)
      Real,    Parameter :: max_dg_wet( n_mode ) = (/ 160.0E-9,1500.0E-9,300.0E-6 /)

      Real,    Parameter :: def_sigma_g( n_mode ) = (/ 1.70, 2.0, 2.2 /)       ! default background sigma-g for each mode
      Real,    Parameter :: min_sigma_g = 1.05
      Real,    Parameter :: max_sigma_g = 2.5001
      Real,    Save      :: def_l2sg( n_mode ), max_l2sg, min_l2sg   

C If FIXED_sg = T, atkn & accum std. dev. are not changed by GETPAR
C If FIXED_sg = F, the second moment will be adjusted if the standard
C                  deviation (sigma_g) is outside of the bounds specified 
C                  by min_sigma_g and max_sigma_g.
      LOGICAL, PARAMETER :: FIXED_sg = .FALSE.

C Flag to obtain coagulation coefficients
C by analytical approximation (True) or by Gauss-Hermite quadrature (False)
      Logical, Parameter :: fastcoag_flag = .True.
      Integer, Parameter :: coag_moments = 2      ! Number of moments to consider 
                                                  ! for coagulation 
                                                  ! 2 = number and volume
                                                  ! 3 = number, volume, and surface area

C-------------------------------------------------------------------------------------------------------

      Type spcs_type
         Character( 16 ) :: name( n_mode )       ! mode-dependent names of aerosol species
         Character( 16 ) :: bulkname             ! mode-independent names of aerosol species
         Logical         :: lait                 ! Available in Aitken Mode
         Logical         :: lacc                 ! Available in Accumulation Mode
         Logical         :: lcor                 ! Available in Coarse Mode
         Real            :: min_conc( n_mode )   ! minimum concentration values for each mode
         Real            :: density              ! density [ kg m-3 ]
         Character( 16 ) :: gasname              ! Gas species in equilibrium with this aerosol component
         Character( 16 ) :: ctrname              ! Rxn Counter Species used to calculate production of 
                                                 !   this component if irreversible partitioning is assumed
         Real            :: ctr_yield            ! Yield of Reaction Counter when forming condensed mass
         Character( 3  ) :: voltype              ! Type of production to be implemented for this species. 
                                                 !   Options are: REV = Reversible, IRV = Irreversible, 
                                                 !                NVL = Nonvolatile
         Logical         :: no_M2Wet             ! flag to exclude from 2nd moment during transport
         Logical         :: tracer               ! tracer flag; does have not mass
         Integer         :: charge               ! electroneutrality charge
         Real            :: visual_idx           ! visual index factor [ m2 g-1 ]
         Real            :: visual_idx_large     ! visual index factor [ m2 g-1 ] for large mode, if not applicable, same value as visual_idx
         Logical         :: om                   ! flag for organic aerosols
         Character( 16 ) :: optic_surr           ! optical surrogate name
         Real            :: kappaorg             ! hygroscopicity parameter for organic aerosol (excluding POC+NCOM which must be calculated)
      End Type spcs_type
      Type( spcs_type ), Allocatable, Save :: aerospc ( : )
 

      ! Master List of Aerosol Species and Properties
      Type spcs_list_type
         Character( 16 ) :: bulkname             ! mode-independent names of aerosol species
         Logical         :: lait                 ! Available in Aitken Mode
         Logical         :: lacc                 ! Available in Accumulation Mode
         Logical         :: lcor                 ! Available in Coarse Mode
         Real            :: min_conc( n_mode )   ! minimum concentration values for each mode
         Real            :: density              ! density [ kg m-3 ]
         Character( 16 ) :: gasname              ! Gas species in equilibrium with this aerosol component
         Character( 16 ) :: ctrname              ! Rxn Counter Species used to calculate production of 
                                                 !   this component if irreversible partitioning is assumed
         Real            :: ctr_yield            ! Yield of Reaction Counter when forming condensed mass
         Character( 3  ) :: voltype              ! Type of production to be implemented for this species. 
                                                 !   Options are: REV = Reversible, IRV = Irreversible, 
                                                 !                NVL = Nonvolatile
         Logical         :: no_M2Wet             ! flag to exclude from 2nd moment during transport
         Logical         :: tracer               ! tracer flag; does have not mass
         Integer         :: charge               ! electroneutrality charge
         Real            :: visual_idx           ! visual index factor [ m2 g-1 ]
         Real            :: visual_idx_large     ! visual index factor [ m2 g-1 ] for large mode, if not applicable, same value as visual_idx
         Logical         :: om                   ! flag for organic aerosols
         Character( 16 ) :: optic_surr           ! optical surrogate name
         Real            :: kappaorg             ! hygroscopicity parameter for organic aerosol (excluding POC+NCOM which must be calculated)
      End Type spcs_list_type

      Type( spcs_list_type ), Parameter :: aerolist( n_aerolist ) = (/
C                                                                                           Visidx_Large
C                                                                               no_M2Wet Tracer        |
C                                                                                       | | Charge      | OM           
C                      Name      T A C Min_Con Density Gas-Name   CTR-Name  Yield Vol   | |   | Visidx  |  |  OptSurr   korg  
C                     ---------- - - - ------- ------- ---------- --------- ----- ----- + +   + ------ --- +  -------- ----- 
     & spcs_list_type('ASO4    ',T,T,T, cm_so4, 1800.0, 'SULF   ','SULRXN ',1.0  ,'IRV',F,F, -2,  2.2, 4.8,F, 'SOLUTE', 0.00), ! Sulfate
     & spcs_list_type('ANO3    ',T,T,T, cm_set, 1800.0, 'HNO3   ','       ',0.0  ,'REV',F,F, -1,  2.4, 5.1,F, 'SOLUTE', 0.00), ! Nitrate
     & spcs_list_type('ACL     ',T,T,T, cm_set, 2200.0, 'HCL    ','       ',0.0  ,'REV',F,F, -1,  1.7, 1.7,F, 'SOLUTE', 0.00), ! Chloride
     & spcs_list_type('ANH4    ',T,T,T, cm_set, 1800.0, 'NH3    ','       ',0.0  ,'REV',F,F,  1,  0.0, 0.0,F, 'SOLUTE', 0.00), ! Ammonium
     & spcs_list_type('ANA     ',T,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  1,  1.7, 1.7,F, 'SOLUTE', 0.00), ! Sodium
     & spcs_list_type('AMG     ',F,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  2,  1.0, 1.0,F, 'DUST  ', 0.00), ! Magnesium
     & spcs_list_type('AK      ',F,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  1,  1.0, 1.0,F, 'DUST  ', 0.00), ! Potassium
     & spcs_list_type('ACA     ',F,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  2,  1.0, 1.0,F, 'DUST  ', 0.00), ! Calcium
     & spcs_list_type('APOC    ',T,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.00), ! POA Carbon
     & spcs_list_type('APNCOM  ',T,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.00), ! POA Non-Carbon          #10
     & spcs_list_type('AEC     ',T,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  0, 10.0,10.0,F, 'SOOT  ', 0.00), ! Elemental (Black) Carbon
     & spcs_list_type('AFE     ',F,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Iron
     & spcs_list_type('AOTHR   ',T,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Other 
     & spcs_list_type('AAL     ',F,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Aluminum
     & spcs_list_type('ASI     ',F,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Silicon
     & spcs_list_type('ATI     ',F,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Titanium
     & spcs_list_type('AMN     ',F,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Manganese
     & spcs_list_type('AH2O    ',T,T,T, cm_set, 1000.0, '       ','       ',0.0  ,'H2O',T,F,  0,  0.0, 0.0,F, 'WATER ', 0.00), ! Water
     & spcs_list_type('AORGH2O ',F,T,F, cm_set, 1000.0, '       ','       ',0.0  ,'H2O',T,F,  0,  0.0, 0.0,F, 'WATER ', 0.00), ! Organic Water
     & spcs_list_type('AH3OP   ',T,T,T, cm_set, 1000.0, '       ','       ',0.0  ,'H2O',T,T,  0,  0.0, 0.0,F, 'WATER ', 0.00), ! Hydronium               #20
     & spcs_list_type('AALK1   ',F,T,F, cm_set, 1400.0, 'SVALK1 ','ALKRXN ',.0334,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), ! Alkane SOA 1            
     & spcs_list_type('AALK2   ',F,T,F, cm_set, 1400.0, 'SVALK2 ','ALKRXN ',.2164,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.06), ! Alkane SOA 2
     & spcs_list_type('AXYL1   ',F,T,F, cm_set, 1480.0, 'SVXYL1 ','XYLNRXN',.0310,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.17), ! Xylene SOA 1
     & spcs_list_type('AXYL2   ',F,T,F, cm_set, 1480.0, 'SVXYL2 ','XYLNRXN',.0900,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.11), ! Xylene SOA 2
     & spcs_list_type('AXYL3   ',F,T,F, cm_set, 1330.0, '       ','XYLHRXN',.3600,'IRV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Xylene SOA 3
     & spcs_list_type('ATOL1   ',F,T,F, cm_set, 1240.0, 'SVTOL1 ','TOLNRXN',.0580,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Toluene SOA 1
     & spcs_list_type('ATOL2   ',F,T,F, cm_set, 1240.0, 'SVTOL2 ','TOLNRXN',.1130,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.10), ! Toluene SOA 2
     & spcs_list_type('ATOL3   ',F,T,F, cm_set, 1450.0, '       ','TOLHRXN',.3000,'IRV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.20), ! Toluene SOA 3
     & spcs_list_type('ABNZ1   ',F,T,F, cm_set, 1400.0, 'SVBNZ1 ','BNZNRXN',.0720,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.19), ! Benzene SOA 1
     & spcs_list_type('ABNZ2   ',F,T,F, cm_set, 1400.0, 'SVBNZ2 ','BNZNRXN',.8880,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Benzene SOA 2           #30
     & spcs_list_type('ABNZ3   ',F,T,F, cm_set, 1400.0, '       ','BNZHRXN',.3700,'IRV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.23), ! Benzene SOA 3           
     & spcs_list_type('ATRP1   ',F,T,F, cm_set, 1400.0, 'SVTRP1 ','TRPRXN ',.1393,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.10), ! Terpene SOA 1
     & spcs_list_type('ATRP2   ',F,T,F, cm_set, 1400.0, 'SVTRP2 ','TRPRXN ',.4542,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.10), ! Terpene SOA 2
     & spcs_list_type('AISO1   ',F,T,F, cm_set, 1400.0, 'SVISO1 ','ISOPRXN',.2320,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.14), ! Isoprene SOA 1
     & spcs_list_type('AISO2   ',F,T,F, cm_set, 1400.0, 'SVISO2 ','ISOPRXN',.0288,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Isoprene SOA 2
     & spcs_list_type('AISO3   ',F,T,F, cm_set, 1400.0, '       ','       ',.0000,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.21), ! Isoprene SOA 3
     & spcs_list_type('ASQT    ',F,T,F, cm_set, 1400.0, 'SVSQT  ','SESQRXN',1.537,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), ! Sesquiterpene SOA
     & spcs_list_type('AHOM    ',F,T,F, cm_set, 1400.0, 'HOM    ','       ',0.0 ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.13), ! HOM SOA
     & spcs_list_type('AELHOM  ',F,T,F, cm_set, 1400.0, 'ELHOM  ','       ',0.0 ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.09), ! ELHOM SOA
     & spcs_list_type('APAH1   ',F,T,F, cm_set, 1480.0, 'SVPAH1 ','PAHNRXN',0.210,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.08), ! PAH SOA 1
     & spcs_list_type('APAH2   ',F,T,F, cm_set, 1480.0, 'SVPAH2 ','PAHNRXN',1.070,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.06), ! PAH SOA 2
     & spcs_list_type('APAH3   ',F,T,F, cm_set, 1550.0, '       ','PAHHRXN',0.730,'IRV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.09), ! PAH SOA 3               #40
     & spcs_list_type('AOLGA   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.18), ! Anth. Oligomer SOA      
     & spcs_list_type('AOLGB   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.13), ! Biogenic Oligomer SOA
     & spcs_list_type('AORGC   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.12), ! Cloud-Processed SOA
     & spcs_list_type('ASOIL   ',F,F,T, cm_set, 2600.0, '       ','       ',0.0  ,'NVL',F,F,  0,  0.6, 0.6,F, 'DUST  ', 0.00), ! Soil
     & spcs_list_type('ACORS   ',F,F,T, cm_cor, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  0,  0.6, 0.6,F, 'DUST  ', 0.00), ! Coarse PM
     & spcs_list_type('ASEACAT ',F,F,T, cm_cor, 2200.0, '       ','       ',0.0  ,'NVL',F,F,  1,  1.7, 1.7,F, 'SOLUTE', 0.00), ! SeaSpray Cations        
                                                                                         
       ! Associated with ae6i                                                                
     & spcs_list_type('AMTNO3  ',F,T,F, cm_set, 1400.0, 'MTNO3  ','       ',0.0  ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.11), ! Monoterpene Nitrate SOA
     & spcs_list_type('AISOPNN ',F,T,F, cm_set, 1400.0, 'ISOPNN ','       ',0.0  ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.32), ! Isoprene Nitrate SOA
     & spcs_list_type('AMTHYD  ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), ! 
     & spcs_list_type('AIETET  ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Iso. Tetrol SOA         #50
     & spcs_list_type('AIEOS   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.30), ! Iso. Organosulfate SOA  
     & spcs_list_type('ADIM    ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.13), !
     & spcs_list_type('AIMGA   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.18), !
     & spcs_list_type('AIMOS   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.36), !

       ! Associated with cracmm1
     & spcs_list_type('AISO3NOS',F,T,F,cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Iso. nonsulfated SOA
     & spcs_list_type('AISO3OS ',F,T,F,cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.30), ! Iso. Organosulfate SOA

       ! Updated monoterpene SOA following Saha and Grieshop ES&T 2016                  
     & spcs_list_type('AMT1    ',F,T,F, cm_set, 1400.0, 'SVMT1  ','TRPRXN ',0.040,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.08), ! MT SOA Lowest Volatility
     & spcs_list_type('AMT2    ',F,T,F, cm_set, 1400.0, 'SVMT2  ','TRPRXN ',0.032,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.08), ! 
     & spcs_list_type('AMT3    ',F,T,F, cm_set, 1400.0, 'SVMT3  ','TRPRXN ',0.032,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.09), !
     & spcs_list_type('AMT4    ',F,T,F, cm_set, 1400.0, 'SVMT4  ','TRPRXN ',0.103,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), !
     & spcs_list_type('AMT5    ',F,T,F, cm_set, 1400.0, 'SVMT5  ','TRPRXN ',0.143,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), !
     & spcs_list_type('AMT6    ',F,T,F, cm_set, 1400.0, 'SVMT6  ','TRPRXN ',0.285,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.05), !                         #60
     & spcs_list_type('AMT7    ',F,T,F, cm_set, 1400.0, 'SVMT7  ','TRPRXN ',0.160,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.06), ! MT SOA Highest Volatility
                                                                                       
       ! ae6i and cb6                                                                 
     & spcs_list_type('AGLY    ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.13), ! Glyoxal SOA
                                                                                      
       ! Semivolatile POA                                                             
     & spcs_list_type('ALVPO1  ',T,T,F, cm_set, 1400.0, 'VLVPO1 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.05), ! LV-POA
     & spcs_list_type('ASVPO1  ',T,T,F, cm_set, 1400.0, 'VSVPO1 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.05), ! SV-POA 1
     & spcs_list_type('ASVPO2  ',T,T,F, cm_set, 1400.0, 'VSVPO2 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.04), ! SV-POA 2
     & spcs_list_type('ASVPO3  ',F,T,F, cm_set, 1400.0, 'VSVPO3 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.03), ! SV-POA 3
     & spcs_list_type('AIVPO1  ',F,T,F, cm_set, 1400.0, 'VIVPO1 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.03), ! IV-POA 
     & spcs_list_type('ALVOO1  ',T,T,F, cm_set, 1400.0, 'VLVOO1 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.15), ! LV-SOA 1                
     & spcs_list_type('ALVOO2  ',T,T,F, cm_set, 1400.0, 'VLVOO2 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.13), ! LV-SOA 2
     & spcs_list_type('ASVOO1  ',T,T,F, cm_set, 1400.0, 'VSVOO1 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.11), ! SV-SOA 1                #70
     & spcs_list_type('ASVOO2  ',T,T,F, cm_set, 1400.0, 'VSVOO2 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.09), ! SV-SOA 2
     & spcs_list_type('ASVOO3  ',F,T,F, cm_set, 1400.0, 'VSVOO3 ','       ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.08), ! SV-SOA 3
     & spcs_list_type('APCSO   ',F,T,F, cm_set, 1400.0, 'LVPCSOG','PCSOARXN',1.0 ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.12), ! pcSOA
                                                                                  
       ! Lumped anthropogenic SOA (introduced aero7, M. Qin, 8/2018)
     & spcs_list_type('AAVB1   ',F,T,F, cm_set, 1400.0, 'SVAVB1 ','       ',0.0  ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.20),
     & spcs_list_type('AAVB2   ',F,T,F, cm_set, 1400.0, 'SVAVB2 ','       ',0.0  ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15),
     & spcs_list_type('AAVB3   ',F,T,F, cm_set, 1400.0, 'SVAVB3 ','       ',0.0  ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.14),
     & spcs_list_type('AAVB4   ',F,T,F, cm_set, 1400.0, 'SVAVB4 ','       ',0.0  ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.12),
                                                          
       ! CRACMM SVOCs and LVOCs                                                             
     & spcs_list_type('AOP3      ',F,T,F,cm_set,1400.0,'OP3      ','      ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.05), ! 
     & spcs_list_type('AROCN2ALK ',T,T,F,cm_set,1400.0,'VROCN2ALK','      ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.05), ! 
     & spcs_list_type('AROCN1ALK ',T,T,F,cm_set,1400.0,'VROCN1ALK','      ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.05), ! 
     & spcs_list_type('AROCP0ALK ',T,T,F,cm_set,1400.0,'VROCP0ALK','      ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.04), !   
     & spcs_list_type('AROCP1ALK ',T,T,F,cm_set,1400.0,'VROCP1ALK','      ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.03), ! 
     & spcs_list_type('AROCP2ALK ',F,T,F,cm_set,1400.0,'VROCP2ALK','      ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.03), ! 
     & spcs_list_type('AROCP3ALK ',F,T,F,cm_set,1400.0,'VROCP3ALK','      ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.03), ! 
     & spcs_list_type('AROCN2OXY2',T,T,F,cm_set,1400.0,'VROCN2OXY2','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.15), ! 
     & spcs_list_type('AROCN2OXY4',T,T,F,cm_set,1400.0,'VROCN2OXY4','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.15), ! 
     & spcs_list_type('AROCN2OXY8',T,T,F,cm_set,1400.0,'VROCN2OXY8','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.15), ! 
     & spcs_list_type('AROCN1OXY1',T,T,F,cm_set,1400.0,'VROCN1OXY1','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.13), ! 
     & spcs_list_type('AROCN1OXY3',T,T,F,cm_set,1400.0,'VROCN1OXY3','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.13), ! 
     & spcs_list_type('AROCN1OXY6',T,T,F,cm_set,1400.0,'VROCN1OXY6','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.13), ! 
     & spcs_list_type('AROCP0OXY2',T,T,F,cm_set,1400.0,'VROCP0OXY2','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.11), !   
     & spcs_list_type('AROCP0OXY4',T,T,F,cm_set,1400.0,'VROCP0OXY4','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.11), ! 
     & spcs_list_type('AROCP1OXY1',T,T,F,cm_set,1400.0,'VROCP1OXY1','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.09), ! 
     & spcs_list_type('AROCP1OXY3',T,T,F,cm_set,1400.0,'VROCP1OXY3','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.09), ! 
     & spcs_list_type('AROCP2OXY2',F,T,F,cm_set,1400.0,'VROCP2OXY2','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.08), ! 
     & spcs_list_type('AROCP3OXY2',F,T,F,cm_set,1400.0,'VROCP3OXY2','     ',0.0  ,'REV',F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.08), ! 
              
       ! The following species are associated with the Multi-Pollutant code
     & spcs_list_type('ANI     ',T,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Nickel
     & spcs_list_type('ACR_VI  ',T,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Chromium 6
     & spcs_list_type('ACR_III ',T,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Chromium 3              #80
     & spcs_list_type('ABE     ',T,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Beryllium
     & spcs_list_type('APB     ',T,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Lead                     
     & spcs_list_type('ADE_OTHR',T,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Fine PM          
     & spcs_list_type('ADE_EC  ',T,T,F, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Black Carbon
     & spcs_list_type('ADE_OC  ',T,T,F, cm_set, 2000.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Organic Carbon
     & spcs_list_type('ADE_NO3 ',F,T,F, cm_set, 1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Nitrate
     & spcs_list_type('ADE_SO4 ',F,T,F, cm_set, 1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Sulfate
     & spcs_list_type('ADE_CORS',F,F,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Coarse PM
     & spcs_list_type('ACD     ',T,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Cadmium
     & spcs_list_type('AMN_HAPS',T,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Manganese               #90
     & spcs_list_type('APHG    ',T,T,T, cm_set, 2200.0, 'HGIIAER','PHGRXN ',1.0  ,'IRV',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Mercury
     & spcs_list_type('AAS     ',T,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Arsenic                 #92
     & spcs_list_type('ABENAPY ',T,T,F, cm_set, 1400.0, 'BENAPY ','       ',1.0  ,'REV',F,T,  0,  0.0, 0.0,T, 'DUST  ', 0.00), ! Benzo[a]pyrene          #93
                                                          
      ! The following species are associated with the marine chemistry code
     & spcs_list_type('ABR     ',F,T,T, cm_set, 2200.0, '       ','       ',0.0  ,'NVL',F,F, -1,  1.7, 1.7,F, 'SOLUTE', 0.00), ! Bromide 
      ! Species created for CRACMM
     & spcs_list_type('ASOAT   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.13), ! Lumped SOA not otherwise specified, OM/OC=2.1

      ! The following species are associated with the sulfur tracking model
     & spcs_list_type('ASO4AQH2O2',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous H2O2 rxn
     & spcs_list_type('ASO4AQO3  ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous O3 rxn
     & spcs_list_type('ASO4AQFEMN',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous FEMN cat rxn
     & spcs_list_type('ASO4AQMHP ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous MHP rxn
     & spcs_list_type('ASO4AQPAA ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous PAA rxn
     & spcs_list_type('ASO4GAS   ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from gas rxn
     & spcs_list_type('ASO4EMIS  ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! emitted SO4
     & spcs_list_type('ASO4ICBC  ',T,T,T,cm_so4,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from ICBCs
                                                          
      ! The following species are associated with the sulfur tracking model, representing loss of tracked species to organosulfate
     & spcs_list_type('OSO4      ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 loss to organosulfate
     & spcs_list_type('OSO4AQH2O2',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous H2O2 rxn loss to organosulfate
     & spcs_list_type('OSO4AQO3  ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous O3 rxn loss to organosulfate
     & spcs_list_type('OSO4AQFEMN',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous FEMN cat rxn loss to organosulfate
     & spcs_list_type('OSO4AQMHP ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous MHP rxn loss to organosulfate
     & spcs_list_type('OSO4AQPAA ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from aqueous PAA rxn loss to organosulfate
     & spcs_list_type('OSO4GAS   ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from gas rxn loss to organosulfate
     & spcs_list_type('OSO4EMIS  ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from emitted SO4 loss to organosulfate
     & spcs_list_type('OSO4ICBC  ',T,T,T,cm_set,1800.0, '       ','       ',0.0  ,'NVL',F,T,  0,  0.0, 0.0,F, 'SOLUTE', 0.00), ! SO4 from ICBCs loss to organosulfate
     
      ! The following species are associated with CRACMM2
     & spcs_list_type('AISO4   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.21), ! from heterogeneous uptake of IPX
     & spcs_list_type('AISO5   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'NVL',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.18), ! from heterogeneous uptake of INALD
     & spcs_list_type('ATRPN   ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.10), ! SOA from first generation monoterpene nitrate
     & spcs_list_type('AHONIT  ',F,T,F, cm_set, 1400.0, '       ','       ',0.0  ,'REV',F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.14)  ! SOA from second generation monoterpene nitrate
     & /)                                                 
 
! Define Reference Emissions Size Distributions
!   Geometric mean (or median) diameter by volume (or mass) of emitted particles in
!   each mode [ m ] and geometric standard deviation of emitted particles.  
!   See paragraph #14 of Binkowski & Roselle (2003).
!   09/17/14 change by Kathleen Fahey - see Revision History, above.
      TYPE em_aero
          Character( 20 ) :: name
          Real            :: split( n_mode )  ! dimensionless
          Real            :: dgvem( n_mode )  ! meters
          Real            :: sgem ( n_mode )  ! dimensionless
      END TYPE em_aero
      INTEGER, PARAMETER  :: desid_n_aero_ref = 9

      TYPE( em_aero ), Parameter :: desid_aero_ref( desid_n_aero_ref ) = (/

!              ----Name----     -----Split-----    ---Geo. Mean Diameter---   ---Stnd Dev.---
     & em_aero('FINE_REF       ',(/0.1,0.9,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Default Accum and Aitken Mode
     & em_aero('ACC_REF        ',(/0.0,1.0,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Just Accumulation Mode
     & em_aero('COARSE_REF     ',(/0.0,0.0,1.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Just Coarse Mode
     & em_aero('UNITY_REF      ',(/1.0,1.0,1.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Used for online sectors (e.g. SeaSpray)
     & em_aero('ZERO_REF       ',(/0.0,0.0,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Zero out the emissions
     & em_aero('FINE_WBDUST    ',(/0.0,1.0,0.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Default Fine Wind-Blown Dust Parameterization
     & em_aero('COARSE_WBDUST  ',(/0.0,0.0,1.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Default Coarse Wind-Blown Dust Param.
     & em_aero('FINE_SEASPRAY  ',(/0.0,1.0,0.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Fine Sea Spray Parameterization is Dynamic. 
     & em_aero('COARSE_SEASPRAY',(/0.0,0.0,1.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/))  ! Coarse Sea Spray Parameterization is Dynamic. 
                                                                                                !  The values here are not actually used but
                                                                                                !  are replaced in SSEMIS when FACNUM and FACSRF
                                                                                                !  are calculated online.
     & /)

! Primary Organic Aerosol Volatility Distributions
      Integer, Parameter :: n_vbs_bin = 5
      Character( 10 ) :: poa_name( n_vbs_bin ) = (/ 'LVPO1', 'SVPO1', 'SVPO2', 'SVPO3', 'IVPO1' /)
      Real, Parameter :: poa_op_vf( n_vbs_bin ) = (/ 0.09,  0.09,  0.14,  0.18,  0.5 /)  ! Aggregated

! The Following Volatility Distributions are alternative options
! but at this point can only be implemented indivdually for the
! entire POA suite of compounds.
!     Real, Parameter :: poa_gv_vf(n_vbs_bin) = (/0.27,  0.15,   0.26,   0.15,   0.17/) ! Gasoline
!     Real, Parameter :: poa_dv_vf(n_vbs_bin) = (/0.03,  0.25,   0.37,   0.24,   0.11/) ! Diesel
!     Real, Parameter :: poa_bb_vf(n_vbs_bin) = (/0.2,   0.1,    0.1,    0.2,    0.4/)  ! Biomass Burning
!     Real, Parameter :: poa_nv_vf(n_vbs_bin) = (/1.0,   0.0,    0.0,    0.0,    0.0/)  ! Nonvolatile
!     Real, Parameter :: poa_mc_vf(n_vbs_bin) = (/0.35,  0.35,   0.1,    0.1,    0.1/)  ! Meat Cooking

! POA_AMF is the fraction of total emissions in the particle phase.
! This parameter is for distributing the total emissions between
! gas and particle BEFORE the aerosol size distribution is applied.
! This helps prevent numerical issues with shrinking the particles
! instantaneously.
      Real, Parameter :: poa_amf( n_vbs_bin ) = (/ 1.0, 0.5, 0.0, 0.0, 0.0 /)
      Real, Parameter :: pog_amf( n_vbs_bin ) = (/ 0.0, 0.5, 1.0, 1.0, 1.0 /)
 
! PCVOC_FAC is the scale factor for deriving PCVOC emissions from POA
! emissions. PCVOC is the vapor precursor to pcSOA formation.
      Real, Parameter :: PCVOC_FAC = 6.579  ! Scale factor for PCVOC/POA
                                            ! Murphy et al., 2017

C number of lognormal modes in windblown dust aerosol = n_mode
C - but only accumulation and coarse modes used
      Type emis_table
         Character( 16 ) :: description       ! Species Long Name
         Character( 16 ) :: name              ! Species Name
         Real            :: mw                ! Molecular weight (g/mol)
         Real            :: spcfac( 2 )       ! Speciation Factor for fine and coarse modes (g/g)
      End type emis_table

C For the wind-blown dust speciation factors, we used the median of the Desert Soil
C profiles 3398, 3403, 3408, and 3413 for the J mode from the SPECIATE database and
C profiles 3399, 3404, 3409, and 3414 for coarse PM. (G. Pouliot - private communication)
C See Ref. (4), above and https://cfpub.epa.gov/si/speciate/
! For toxic metal species, more recent sources were consulted because the above profiles give large uncertainties in their 
! speciation factors
! For the air toxics version of manganese, nickel, chromium(III), arsenic, and lead:
!    Table 2 in Y. Su, and R. Yang., Background concentrations of elements in surface soils and 
!    their changes as affected by agriculture use in the desert-oasis ecotone in the middle of Heihe
!    River Basin, North-west China, Journal of Geochemical Exploration, 98, 2008, 57–64 was used.
!    The table represents natural desert soils prior to cultivation. Note that all chromium is 
!    to be trivalent based on assuming low pH or anoxics conditions. 
! For cadmium:
!    The background mixing ratio was used from Table 3 in Su, C., Jiang, L.,and Zhang, W., 
!    A review on heavy metal contamination in the soil worldwide: Situation, impact and 
!    remediation techniques, Environmental Skeptics and Critics, 2014, 3(2): 24-38. 
!    The table reviews worldwide cultivation soils.
! For mercury:
!    Table 2 in D. Obrist et al., A synthesis of terrestrial mercury in the western United States: 
!    Spatial distribution defined by land cover and plant productivity, Science of the Total Environment, 
!    568, 2016, 522–535.
!    Factors were based on mixing ratios for barren and cultivated land cover type
! Speciation factors between j and k modes were assumed constant because no data was found.

C maximum number of chemical species in windblown dust aerosol
      Integer, Parameter :: ndust_spc = 28

       
      Type( emis_table ) :: dust_spc( ndust_spc ) = (/
C                    --description-- -- Species -- --MW--   ------ spcfac ------
C                                                             fine    coarse
     &   emis_table( 'Sulfate       ',  'SO4    ',  96.0, (/ 0.02250, 0.02655/) ),   ! Sulfate
     &   emis_table( 'Nitrate       ',  'NO3    ',  62.0, (/ 0.00020, 0.00160/) ),   ! Nitrate
     &   emis_table( 'Chlorine      ',  'CL     ',  35.5, (/ 0.00945, 0.01190/) ),   ! Chlorine
     &   emis_table( 'Ammonium      ',  'NH4    ',  18.0, (/ 0.00005, 0.0    /) ),   ! Ammonium
     &   emis_table( 'Sodium        ',  'NA     ',  23.0, (/ 0.03935, 0.0    /) ),   ! Sodium
     &   emis_table( 'Calcium       ',  'CA     ',  40.1, (/ 0.07940, 0.0    /) ),   ! Calcium
     &   emis_table( 'Magnesium     ',  'MG     ',  24.3, (/ 0.01900, 0.0    /) ),   ! Magnesium
     &   emis_table( 'Potassium     ',  'K      ',  39.1, (/ 0.03770, 0.0    /) ),   ! Potassium
     &   emis_table( 'Org. Carbon   ',  'POC    ', 220.0, (/ 0.01075, 0.0    /) ),   ! Organic Carbon
     &   emis_table( 'NonCarbon Org.',  'PNCOM  ', 220.0, (/ 0.00430, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   emis_table( 'Low-Vol. POA  ',  'LVPO1  ', 218.0, (/ 0.01501, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   emis_table( 'Low-Vol. OOA  ',  'LVOO1  ', 136.0, (/ 2.23E-5, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   emis_table( 'Black Carbon  ',  'EC     ',  12.0, (/ 0.0,     0.0    /) ),   ! Black or Elemental Carbon
     &   emis_table( 'Iron          ',  'FE     ',  55.8, (/ 0.03355, 0.0    /) ),   ! Iron
     &   emis_table( 'Aluminum      ',  'AL     ',  27.0, (/ 0.05695, 0.0    /) ),   ! Aluminum
     &   emis_table( 'Silicon       ',  'SI     ',  28.1, (/ 0.19425, 0.0    /) ),   ! Silicon
     &   emis_table( 'Titanium      ',  'TI     ',  47.9, (/ 0.00280, 0.0    /) ),   ! Titanium
     &   emis_table( 'Manganese     ',  'MN     ',  54.9, (/ 0.00115, 0.0    /) ),   ! Manganese
     &   emis_table( 'Water         ',  'H2O    ',  18.0, (/ 0.00541, 0.00637/) ),   ! Water
     &   emis_table( 'Undefined Mass',  'OTHR   ', 200.0, (/ 0.48319, 0.0    /) ),   ! Other
     &   emis_table( 'Non-Anion Dust',  'SOIL   ', 100.0, (/ 0.0,     0.95358/) ),   ! Non-Anion Dust
     &   emis_table( 'Air Toxics Mn ',  'MN_HAPS',  54.9, (/ 0.00041, 0.00041/) ),   ! Air toxics Manganese J and K mode
     &   emis_table( 'Nickel        ',  'NI     ',  58.7, (/ 2.0E-05, 2.0E-05/) ),   ! Nickel J and K mode
     &   emis_table( 'Chromium III  ',  'CR_III ',  52.0, (/ 5.6E-05, 5.6E-05/) ),   ! Trivalent Chromium J and K mode
     &   emis_table( 'Arsenic       ',  'AS     ',  74.92,(/ 5.5E-06, 5.5E-06/) ),   ! Arsenic J and K mode
     &   emis_table( 'Lead          ',  'PB     ', 207.2, (/ 1.6E-05, 1.6E-05/) ),   ! Lead J and K mode
     &   emis_table( 'Cadmium       ',  'CD     ', 112.4, (/ 9.7E-08, 9.7E-08/) ),   ! Cadmium J and K mode
     &   emis_table( 'Mercury       ',  'PHG    ', 200.5, (/ 1.4E-08, 1.4E-08/) ) /) ! Mercury J and K mode

      ! Manually Enter the mode-dependent Density of the Dust Particles using the mass fractions in 
      ! the dust_spc table and user-defined densities for each component. You may reference the aerolist 
      ! for densities as well. Units are [kg/m3]. The appropriate calculation is:
      !    dust_dens = sum( frac_i ) / sum( frac_i / dens_i )
      ! Make sure to ignore any tracer species when performing this calculation.
      Real, Parameter :: dust_dens( n_mode ) = (/ 2200.0, 2156.55, 2536.92 /)

      Real, ALLOCATABLE, SAVE    :: DUSTOUTM( :,:,: ) ! Wind-Blown Dust Mass Emiss Rate [ug/m3/s]
      Real, ALLOCATABLE, SAVE    :: DUSTOUTN( :,:,: )   ! Wind-Blown Dust Number Emiss Rate [1/m3/s]
      Real, ALLOCATABLE, SAVE    :: DUSTOUTS( :,:,: )   ! Wind-Blown Dust Surface Area Emiss Rate [m2/m3/s]

C Sea-Spray Aerosol Speciation factors based on seawater composition:
! For toxic metal species, Use Table 1 in K.W. Bruland and M.C. Lohan, 6.02 - Controls of Trace Metals in Seawater, 
! In Treatise on Geochemistry, edited by Heinrich D. Holland and Karl K. Turekian, Pergamon, Oxford, 2003, Pages 23-47,
! ISBN 9780080437514, http://dx.doi.org/10.1016/B0-08-043751-6/06105-3.

C number of chemical species in seawater aerosol composition
      integer, parameter :: nsea_spc = 17

      Type( emis_table ), Parameter :: sea_spc( nsea_spc ) = (/
C                     -description--  -- Species -- --MW--  ------ spcfac ------
C                                                              fine    coarse
     &   emis_table( 'Sulfate       ',   'SO4    ',  96.0, (/ 0.07760, 0.07760/) ),   ! Sulfate
     &   emis_table( 'Chlorine      ',   'CL     ',  35.5, (/ 0.55380, 0.55380/) ),   ! Chlorine
     &   emis_table( 'Sodium        ',   'NA     ',  23.0, (/ 0.30860, 0.0    /) ),   ! Sodium
     &   emis_table( 'Calcium       ',   'CA     ',  40.1, (/ 0.01180, 0.0    /) ),   ! Calcium
     &   emis_table( 'Magnesium     ',   'MG     ',  24.3, (/ 0.03680, 0.0    /) ),   ! Magnesium
     &   emis_table( 'Potassium     ',   'K      ',  39.1, (/ 0.01140, 0.0    /) ),   ! Potassium
     &   emis_table( 'SeaSalt_Cation',   'SEACAT ',  23.75,(/ 0.0    , 0.36860/) ),   ! Sea-Salt Cations
     &   emis_table( 'Chromium VI   ',   'CR_VI  ',  52.0, (/ 5.95E-9, 5.95E-9/) ),   ! Hexavalent Chromium
     &   emis_table( 'Nickel        ',   'NI     ',  58.7, (/ 1.34E-8, 1.34E-8/) ),   ! Nickle
     &   emis_table( 'Arsenic       ',   'AS     ',  74.92,(/ 4.93E-8, 4.93E-8/) ),   ! Arsenic
     &   emis_table( 'Beryllium     ',   'BE     ',   9.0, (/ 5.2E-11, 5.2E-11/) ),   ! Beryllium
     &   emis_table( 'Mercury       ',   'PHG    ', 200.5, (/ 5.8E-11, 5.8E-11/) ),   ! Mercury
     &   emis_table( 'Lead          ',   'PB     ', 207.2, (/ 6.0E-11, 6.0E-11/) ),   ! Lead
     &   emis_table( 'Cadmium       ',   'CD     ', 112.4, (/ 1.93E-9, 1.93E-9/) ),   ! cadmium
     &   emis_table( 'Air Toxics Mn ',   'MN_HAPS',  54.9, (/ 4.7E-11, 4.7E-11/) ),   ! Air toxics Manganese
     &   emis_table( 'Bromine       ',   'BR     ',  79.9, (/ 0.00190, 0.00190/) ),   ! Bromine 
     &   emis_table( 'Water         ',   'H2O    ',  18.0, (/ 0.0    , 0.0    /) ) /) ! Water (uptake is calculated online)
 
      ! MAKE SURE WATER IS THE LAST COMPONENT IN THE TABLE ABOVE OR YOU
      ! WILL ENCOUNTER ISSUES WITH THE SEA SPRAY EMISSIONS MODULE.

      ! Manually Enter the mode-dependent density of the Sea Spray Particles using the mass fractions in 
      ! the sea_spc table and user-defined densities for each component. You may reference the aerolist 
      ! for densities as well. Units are [kg m-3]. The appropriate calculation is:
      !    seaspray_dens = sum( frac_i ) / sum( frac_i / dens_i )
      ! Make sure to ignore any tracer species when performing this calculation.
      Real, Parameter :: seaspray_dens( n_mode ) = (/ 2162.7, 2162.7, 2162.7 /)
      Real, Parameter :: specific_vol_h2o = 0.001  !Inverse Density of Particulate Water
 
C Constants used for simulating the ionic effects of sea-spray,
C windblown dust and anthropogenic dust in the coarse mode. These cation
C species are not transported individually in the domain but their
C relative abundance is assumed to be constant and scaled to the total
C SeaSalt_Cation, ASOIL, or Coarse-Mode Dust present.

      Real( 8 ), Parameter :: asoil_renorm = 1.0D0 - 0.04642D0  ! = 0.95358, same as ASOIL speciation factor in the dust_spc table above

      Real( 8 ), Parameter :: ascat_na_fac = 0.8373D0    ! for NA in coarse sea-spray aerosol
      Real( 8 ), Parameter :: asoil_na_fac = 0.0626D0    ! for NA in windblown dust
      Real( 8 ), Parameter :: acors_na_fac = 0.0023D0    ! for NA in anthropogenic coarse

      Real( 8 ), Parameter :: ascat_mg_fac = 0.0997D0    ! for MG in coarse sea-spray aerosol
      Real( 8 ), Parameter :: asoil_mg_fac = 0.0170D0    ! for MG in windblown dust
      Real( 8 ), Parameter :: acors_mg_fac = 0.0032D0    ! for MG in anthropogenic coarse

      Real( 8 ), Parameter :: ascat_k_fac =  0.0310D0    ! for K in coarse sea-spray aerosol
      Real( 8 ), Parameter :: asoil_k_fac =  0.0242D0    ! for K in windblown dust
      Real( 8 ), Parameter :: acors_k_fac =  0.0176D0    ! for K in anthropogenic coarse

      Real( 8 ), Parameter :: ascat_ca_fac = 0.0320D0    ! for CA in coarse sea spray aerosol
      Real( 8 ), Parameter :: asoil_ca_fac = 0.0838D0    ! for CA in windblown dust
      Real( 8 ), Parameter :: acors_ca_fac = 0.0562D0    ! for CA in anthropogenic coarse


      Real( 8 ), Parameter :: asoil_fe_fac = 0.02695D0   ! for FE in windblown dust
      Real( 8 ), Parameter :: acors_fe_fac = 0.0467D0    ! for FE in anthropogenic coarse

      Real( 8 ), Parameter :: asoil_mn_fac = 0.00075D0   ! for MN in windblown dust
      Real( 8 ), Parameter :: acors_mn_fac = 0.0011D0    ! for MN in anthropogenic coarse
 
C Coarse mode PMC speciation based on anthropogenic inventory composite from various
C sources (G. Pouliot - private communication):

      Real( 8 ), Parameter :: acorsem_aso4_fac = 0.00100D0
      Real( 8 ), Parameter :: acorsem_ano3_fac = 0.00048D0
      Real( 8 ), Parameter :: acorsem_acl_fac  = 0.00145D0
      Real( 8 ), Parameter :: acorsem_ah2o_fac = 0.00032D0
      Real( 8 ), Parameter :: acorsem_renorm   = 1.0D0
     &                                     - acorsem_aso4_fac
     &                                     - acorsem_ano3_fac
     &                                     - acorsem_acl_fac
     &                                     - acorsem_ah2o_fac
 
C Required species
      Character( 16 ), Private, Parameter :: req_so4    = 'ASO4'
      Character( 16 ), Private, Parameter :: req_no3    = 'ANO3'
      Character( 16 ), Private, Parameter :: req_cl     = 'ACL'
      Character( 16 ), Private, Parameter :: req_nh4    = 'ANH4'
      Character( 16 ), Private, Parameter :: req_na     = 'ANA'
      Character( 16 ), Private, Parameter :: req_mg     = 'AMG'
      Character( 16 ), Private, Parameter :: req_k      = 'AK'
      Character( 16 ), Private, Parameter :: req_ca     = 'ACA'
      Character( 16 ), Private, Parameter :: req_fe     = 'AFE'
      Character( 16 ), Private, Parameter :: req_mn     = 'AMN'
      Character( 16 ), Private, Parameter :: req_poc    = 'APOC'
      Character( 16 ), Private, Parameter :: req_ncom   = 'APNCOM'
      Character( 16 ), Private, Parameter :: req_h2o    = 'AH2O'
      Character( 16 ), Private, Parameter :: req_h3op   = 'AH3OP'
      Character( 16 ), Private, Parameter :: req_soil   = 'ASOIL'
      Character( 16 ), Private, Parameter :: req_cors   = 'ACORS'
      Character( 16 ), Private, Parameter :: req_seacat = 'ASEACAT'

C Indices of required species
      Integer :: aso4_idx
      Integer :: ano3_idx
      Integer :: acl_idx
      Integer :: anh4_idx
      Integer :: ana_idx
      Integer :: amg_idx
      Integer :: ak_idx
      Integer :: aca_idx
      Integer :: afe_idx
      Integer :: amn_idx
      Integer :: apoc_idx
      Integer :: apncom_idx
      Integer :: ah2o_idx
      Integer :: ah3op_idx
      Integer :: asoil_idx
      Integer :: acors_idx
      Integer :: aseacat_idx

C Flag if optional species present
      Logical :: ae6isoa = .False.
      Logical :: ae6hg   = .False.
      Logical :: ae7orgh2o = .False.
      Logical :: marine  = .False.

C Optional Species
      Character( 16 ), Private, Parameter :: req_ietet  = 'AIETET'
      Character( 16 ), Private, Parameter :: req_ieos   = 'AIEOS'
      Character( 16 ), Private, Parameter :: req_dim    = 'ADIM'
      Character( 16 ), Private, Parameter :: req_imga   = 'AIMGA'
      Character( 16 ), Private, Parameter :: req_imos   = 'AIMOS'
      Character( 16 ), Private, Parameter :: req_phgj   = 'APHGJ'
      Character( 16 ), Private, Parameter :: req_orgh2o = 'AORGH2O'
      Character( 16 ), Private, Parameter :: req_br     = 'ABR' 
      Character( 16 ), Private, Parameter :: req_so4aqh2o2 = 'ASO4AQH2O2'
      Character( 16 ), Private, Parameter :: req_so4aqo3   = 'ASO4AQO3'
      Character( 16 ), Private, Parameter :: req_so4aqfemn = 'ASO4AQFEMN'
      Character( 16 ), Private, Parameter :: req_so4aqmhp  = 'ASO4AQMHP'
      Character( 16 ), Private, Parameter :: req_so4aqpaa  = 'ASO4AQPAA'
      Character( 16 ), Private, Parameter :: req_so4gas    = 'ASO4GAS'
      Character( 16 ), Private, Parameter :: req_so4emis   = 'ASO4EMIS'
      Character( 16 ), Private, Parameter :: req_so4icbc   = 'ASO4ICBC'
      Character( 16 ), Private, Parameter :: req_oso4       = 'OSO4'
      Character( 16 ), Private, Parameter :: req_oso4aqh2o2 = 'OSO4AQH2O2'
      Character( 16 ), Private, Parameter :: req_oso4aqo3   = 'OSO4AQO3'
      Character( 16 ), Private, Parameter :: req_oso4aqfemn = 'OSO4AQFEMN'
      Character( 16 ), Private, Parameter :: req_oso4aqmhp  = 'OSO4AQMHP'
      Character( 16 ), Private, Parameter :: req_oso4aqpaa  = 'OSO4AQPAA'
      Character( 16 ), Private, Parameter :: req_oso4gas    = 'OSO4GAS'
      Character( 16 ), Private, Parameter :: req_oso4emis   = 'OSO4EMIS'
      Character( 16 ), Private, Parameter :: req_oso4icbc   = 'OSO4ICBC'

C Indices of Optional species
      Integer :: aietet_idx = 0
      Integer :: aieos_idx  = 0
      Integer :: adim_idx   = 0
      Integer :: aimga_idx  = 0
      Integer :: aimos_idx  = 0
      Integer :: aphgj_idx  = 0
      Integer :: aorgh2o_idx= 0
      Integer :: abr_idx    = 0
C Indices of Optional sulfur tracking species
      Integer :: aso4aqh2o2_idx = 0
      Integer :: aso4aqo3_idx   = 0
      Integer :: aso4aqfemn_idx = 0
      Integer :: aso4aqmhp_idx  = 0
      Integer :: aso4aqpaa_idx  = 0
      Integer :: aso4gas_idx    = 0
      Integer :: aso4emis_idx   = 0
      Integer :: aso4icbc_idx   = 0
      Integer :: oso4_idx       = 0
      Integer :: oso4aqh2o2_idx = 0
      Integer :: oso4aqo3_idx   = 0
      Integer :: oso4aqfemn_idx = 0
      Integer :: oso4aqmhp_idx  = 0
      Integer :: oso4aqpaa_idx  = 0
      Integer :: oso4gas_idx    = 0
      Integer :: oso4emis_idx   = 0
      Integer :: oso4icbc_idx   = 0

C Common Arrays for Aerosol Data
      Real, Allocatable :: aerospc_mw( : )     ! molecular weights (from AE_SPC Namelist) [ g/mol ]
      Real, Allocatable :: aerospc_mwinv( : )  ! reciprocal MWs (from AE_SPC Namelist) [ mol/g ]
      Real, Allocatable :: aerospc_conc( :,: ) ! aero species concentration [ ug/m^3 ]

C Common factors
      Real( 8 ) :: h2ofac                      ! converts mass concentrations [ug/m3] to 3rd moment concentrations [m3/m3]

C Variables for converting emission rates into molar-mixing-ratio units
      REAL,    PARAMETER :: GPKG = 1.0E+03     ! kg -> g
      REAL,    PARAMETER :: MGPG = 1.0E+06     ! g -> ug

C-------------------------------------------------------------------------------------------------------
      Type mode_type
         Character( 3  ) :: suff         ! Suffix
         Character( 16 ) :: num_name     ! name of aerosol number variable
         Character( 16 ) :: srf_name     ! name of aerosol surface area variable
         Logical         :: ultrafine_mask ! is this mode ultrafine PM?
         Logical         :: fine_mask    ! is this mode fine PM?
         Logical         :: coarse_mask  ! is this mode coarse PM?
         Logical         :: accum_mask   ! is this accumulation-mode PM?
         Logical         :: aitken_mask  ! is this aitken-mode PM?
         Logical         :: nuc_mask     ! is this nucleation-mode PM?
         Real            :: min_numconc  ! minimum number concentration
         Real            :: min_m2conc   ! minimum 2nd moment concentration
         Real            :: min_m3conc   ! minimum 3rd moment concentration
      End Type mode_type

      Type ( mode_type ), Parameter  :: aeromode( n_mode ) = (/
C                  suffix   number     surface         Masks       minimum minimum minimum
C                            name       name     U  F  C AC AI NU  numconc  m2conc  m3conc
C                  ------- ----------  -------   -  -  - -- -- --  -------- -------  ------
     &   mode_type('AIT'  ,'NUMATKN', 'SRFATKN', T, T, F, F, T, F, conmin,  conmin, conmin),
     &   mode_type('ACC'  ,'NUMACC ', 'SRFACC ', F, T, F, T, F, F, conmin,  conmin, conmin),
     &   mode_type('COR'  ,'NUMCOR ', 'SRFCOR ', F, F, T, F, F, F, conmin,  conmin, conmin)/)


      Real          :: moment0_conc( n_mode )     ! 0th moment concentration
      Real          :: moment2_conc( n_mode )     ! 2nd moment concentration
      Real          :: moment3_conc( n_mode )     ! 3rd moment concentration
      Logical, save :: wet_moments_flag           ! T if M2 and M3 are wet, F otherwise

C Mass concentration (calculated by GETPAR)
      Real :: aeromode_mass( n_mode )   ! [ ug/m^3 ]

C Particle density (calculated by GETPAR)
      Real :: aeromode_dens( n_mode )   ! [ kg/m^3 ]

C Geometric mean diameter (calculated by GETPAR) for the NUMBER
C distribution. Remember that for log-normal distributions the geometric
C mean and the median are identical!
      Real :: aeromode_diam( n_mode )   ! [ m ]

C Log of geometric standard deviation (calculated by GETPAR ) for the
C NUMBER distribution.
      Real :: aeromode_lnsg( n_mode )

C Minimum number (calculated in map_aero routine)
      Real :: aeromode_minNum( n_mode )

C Minimum 2nd moment (calculated in map_aero routine)
      Real :: aeromode_minM2( n_mode )

C Mapping for loading from and unloading to CGRID array
      Integer, Allocatable :: aerospc_map( :,: )     ! indices of aero species to CGRID
      Integer              :: aeronum_map( n_mode )  ! indices of aero number variable to CGRID
      Integer              :: aerosrf_map( n_mode )  ! indices of aero surf area variable to CGRID

! Diagnostic Aerosol Distribution Parameters 
      Real :: wet_aero_diam( n_mode )
      Real :: dry_aero_diam( n_mode )
      Real :: wet_aero_m2  ( n_mode )
      Real :: dry_aero_m2  ( n_mode )
      Real :: wet_aero_m3  ( n_mode )
      Real :: dry_aero_m3  ( n_mode )
      Real :: wet_aero_dens( n_mode )
      Real :: dry_aero_dens( n_mode )

C Missing aerosol species map
      Logical, Allocatable :: aero_missing( :,: )  
      Logical, Save :: AE_eflag            = .False. ! error flag for AERO_DATA

C IC/BC Correction Mapping
      !The following vectors store masks of the aerosol species
      !identities for the AE_SPC and AE_TRNS vectors. For example, 
      !L_AESP_NUM stores 1's for every species on AE_SPC that
      !represents an aerosol number concentration and 0's otherwise.
      !   L_AESP_NUM - Number Concentration
      !   L_AESP_SURF - Surface Area Concentration
      !   L_AESP_MASS - Mass Concentration
      !   L_AESP_MODE - Mask for each aerosol mode
      LOGICAL, ALLOCATABLE, SAVE :: L_AESP_NUM(:),    L_AESP_SRF(:), L_AESP_MASS(:),
     &                              L_AESP_MODE(:,:), L_AESP_DRY(:)
      LOGICAL, ALLOCATABLE, SAVE :: L_AETR_NUM(:),    L_AETR_SRF(:), L_AETR_MASS(:),
     &                              L_AETR_MODE(:,:), L_AETR_DRY(:)
      REAL, ALLOCATABLE, SAVE :: AEROCGRID_RHOINV( : ) !
      
C Private variables for loading from and unloading to CGRID array
      Logical, Private, Save :: mapped    = .False.
      Character( 16 ), Private, Save :: pname = 'Aero_Data'

! Variables for collecting tendencies for aerosol sub-processes and
! passing back to modules like Process analysis and ISAM. These vectors
! should be sized to the same length as the CGRID species dimension and
! be mapped to CGRID species order. Units for each are total
! concentration or mixing ratio gained or lost. These are not normalized
! by the time step. Ex (ppmv, ug m-3, N m-3, or m2 m-3)
      REAL, ALLOCATABLE, SAVE :: COND_BUDGET( : )
      REAL, ALLOCATABLE, SAVE :: COAG_BUDGET( :,: )
      REAL, ALLOCATABLE, SAVE :: NPF_BUDGET( : )
      REAL, ALLOCATABLE, SAVE :: GROWTH_BUDGET( : )

! Initial Reference Values for M2 and M3 used for Heterogenous Chemistry
! Module.
      REAL( 8 ), ALLOCATABLE, SAVE :: CHEM_M2DRY_INIT( :,:,:,: ),
     &                          CHEM_M3DRY_INIT( :,:,:,: )

      Contains


C-----------------------------------------------------------------------
      Subroutine map_aero()

C  Defines aerosol mapping from CGRID for species concentration and moments.
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.

C HS  01/24/11 Renamed AORGPA as POC for AERO6
C GS  03/02/11 Find new req`d species for AERO6 (Mg, K, Ca)
C HS  03/10/11 Get index for new required species, PNCOM
C JY  03/22/16 Get index for new required species: Fe, Mn (aqchem)
C HOTP 05/11/16 Add IEPOX derived species
C-----------------------------------------------------------------------

      Use rxns_data       ! chemical mechanism data
      Use cgrid_spcs      ! CGRID mechanism species
      Use aeromet_data
      Use vdiff_data, only : n_spc_diff, diff_spc
      Implicit None

C Local Variables:
      Character( 256 ) :: xmsg
      Character( 256 ) :: xmsg2
      Real    :: mole_weight( n_aerolist )
      Integer :: map_listtogrid( n_aerolist,n_mode )
      Integer :: map_aerotolist( n_aerolist )

      Integer,       Allocatable :: aerospc_in_dustlist( : )
      Character(16), Allocatable :: refractive_index( : )

      Integer l, m, n, spc, isea, idust, p
      Real    so4fac
      Real    anthfac

      Real    POA_OC    ! ratio of Elemental Oxygen to Carbon in POA
      Real    OOA_OC    ! ratio of Elemental Oxygen to Carbon in Oxygenated OA

      Integer dust_org1, dust_org2
      Logical ae6isoa
      Logical ae6hg
      Logical dust_match
      Logical new_bulk_species
      Logical replace_optics_surr

      Character(16) sn

      If ( mapped ) Return

      Call LOG_SUBHEADING( LOGDEV, "Map Aerosol Species" )

      mole_weight = 0.0

      n_aerospc      = 0   ! Number of Used Aerosol Chemical Species
      map_listtogrid = 0   ! Pointer from aerolist to CGRID
      map_aerotolist = 0   ! Pointer from aerospc to aerolist
      AE_eflag       = .False.
      max_l2sg       = ( LOG( max_sigma_g ) ) ** 2
      min_l2sg       = ( LOG( min_sigma_g ) ) ** 2
      def_l2sg( : )  = ( LOG( def_sigma_g( : ) ) ) ** 2
      

      allocate( aerocgrid_rhoinv( n_ae_trns ),
     &          l_aesp_num( n_ae_spc ),
     &          l_aesp_srf( n_ae_spc ), l_aesp_mass( n_ae_spc ),
     &          l_aesp_mode( n_mode,n_ae_spc ), l_aesp_dry( n_ae_spc ),
     &          l_aetr_num( n_ae_trns ),
     &          l_aetr_srf( n_ae_trns ), l_aetr_mass( n_ae_trns ),
     &          l_aetr_mode( n_mode,n_ae_trns ), l_aetr_dry( n_ae_trns ) )

      Allocate( refractive_index( n_aerolist ) )
      refractive_index( : ) = aerolist( : )%optic_surr

      aerocgrid_rhoinv = 0.
      l_aesp_num = .False.
      l_aesp_srf = .False.
      l_aesp_mass= .False.
      l_aesp_mode= .False.
      l_aesp_dry = .False.
      l_aetr_num = .False.
      l_aetr_srf = .False.
      l_aetr_mass= .False.
      l_aetr_mode= .False.
      l_aetr_dry = .False.

C Build mapping to CGRID for each species in the master AeroList
      Do spc = 1, n_aerolist
         ! Check to see if this species is in the namelist
         replace_optics_surr = .False.
         do m = 1,n_mode
            if ( m .eq. n_mode .and. 
     &           (aerolist( spc )%bulkname .eq. 'ACORS'    .or.
     &            aerolist( spc )%bulkname .eq. 'ASOIL'    .or.
     &            aerolist( spc )%bulkname .eq. 'ASEACAT'  .or.
     &            aerolist( spc )%bulkname .eq. 'ADE_CORS' )    ) then
              sn = aerolist( spc )%bulkname
            else
              sn = trim( aerolist( spc )%bulkname ) // modesuff( m )
            end if

            n = index1( sn, n_ae_spc, ae_spc )
            p = index1( sn, n_ae_trns, ae_trns )

            If ( n .Ne. 0 ) Then
               If( Len_Trim( ae_optics( n ) ) .gt. 0 )Then
                  replace_optics_surr  = .True.
               End If   
               ! Species is on the NameList
               new_bulk_species = ( .Not. Any( map_listtogrid( spc,: ) .NE. 0 ) )
               If ( new_bulk_species ) Then
                  ! Add this species to the map if it does not exist already
                  n_aerospc = n_aerospc + 1
                  map_aerotolist( n_aerospc ) = spc
                  ! Set the Molecular Weight
                  mole_weight( spc ) = ae_molwt( n )
                  If( replace_optics_surr )refractive_index( spc ) = ae_optics( n )                      
               Else If ( mole_weight( spc ) .Ne. ae_molwt( n ) ) Then
                  ! If the species already exists and the
                  ! molecular weight from this mode on the
                  ! namelist is not matching the standing
                  ! molecular weight, throw an error
                  xmsg = 'The molecular weight of ' // Trim( sn )
     &                  // ' is different from that of the same species'
     &                  // ' in the same or another mode.'
                  Call m3warn( pname, 0, 0, xmsg )
                  Write( xmsg,* ) 'New Value(', n, ') = ', ae_molwt( n ),
     &                            'Expected value(', spc, ')= ', mole_weight( spc )
                  Call m3warn( pname, 0, 0, xmsg )
                  AE_eflag  = .True.
               Else If ( replace_optics_surr ) Then 
                  If ( refractive_index( spc ) .Ne. ae_optics( n ) ) Then
                     ! If the species already exists and the
                     ! namelist's refractive index for the mode
                     ! does not match the new value,
                     ! write message and set error flag
                     xmsg = 'FATAL ERROR: In AErosol namelist, OPTICS value ' 
     &                    // 'for bulk ' // Trim( aerolist( spc )%bulkname )
     &                    // ' is inconsistent across modes. CORRECT the AE namelist'
                     Call m3mesg( xmsg )
                     If( Len_Trim( ae_optics( n ) ) .lt. 1 ) Then
                        xmsg2 = 'blank'
                     Else
                        xmsg2 = Trim( ae_optics( n ) )
                     End If
                     Write( xmsg,* ) 'Bad Value is ', Trim( xmsg2 ),
     &               '; Expected value is ', Trim( refractive_index( spc ) )
                     Call m3mesg( xmsg )
                     AE_eflag  = .True.
                  End If   
               End If
               ! Update the map from CGRID to AeroList for this mode
               map_listtogrid( spc,m ) = ae_strt - 1 + n
               l_aesp_mode( m,n ) = .True.
               l_aetr_mode( m,p ) = .True.
               If ( .Not. aerolist( spc)%tracer ) then
                   l_aesp_mass( n ) = .True.
                   if ( p .gt. 0 ) l_aetr_mass( p ) = .True.
               End If
               If ( .Not. aerolist( spc)%no_m2wet ) then
                   l_aesp_dry( n ) = .True.
                   if ( p .gt. 0 ) l_aetr_dry( p ) = .True.
               End If
               !Map the CGRID Aerosol Species to their Densities
               aerocgrid_rhoinv( n ) = 1.0 / aerolist( spc )%density

            End If
         End Do
      End Do
      Write(logdev,*)' '

      ! Migrate all of the user-requested aerosols from AeroList to
      ! AeroSpc. Begin by allocating all of the new arrays now that we
      ! know the value of n_aerospc
      Allocate ( aerospc       ( n_aerospc ) )
      Allocate ( aerospc_mw    ( n_aerospc ) )
      Allocate ( aerospc_mwinv ( n_aerospc ) )
      Allocate ( aerospc_map   ( n_aerospc, n_mode ) )
      Allocate ( aerospc_conc  ( n_aerospc, n_mode ) )
      Allocate ( aero_missing  ( n_aerospc, n_mode ) )

#ifdef sens
      Allocate ( s_aerospc_conc( n_aerospc,n_mode,npmax ) )
#endif

      aero_missing( :,: ) = .True.
      aerospc_conc( :,: ) = conmin

      Do spc = 1, n_aerospc ! Loop through user-requested chemical species
         l = map_aerotolist( spc )

         ! Map AeroList Contents to AeroSpc
         aerospc( spc )%bulkname   = aerolist( l )%bulkname   
         aerospc( spc )%lait       = aerolist( l )%lait
         aerospc( spc )%lacc       = aerolist( l )%lacc
         aerospc( spc )%lcor       = aerolist( l )%lcor
         aerospc( spc )%min_conc   = aerolist( l )%min_conc
         aerospc( spc )%density    = aerolist( l )%density   
         aerospc( spc )%gasname    = aerolist( l )%gasname   
         aerospc( spc )%ctrname    = aerolist( l )%ctrname   
         aerospc( spc )%ctr_yield  = aerolist( l )%ctr_yield
         aerospc( spc )%voltype    = aerolist( l )%voltype
         aerospc( spc )%no_M2wet   = aerolist( l )%no_M2Wet   
         aerospc( spc )%tracer     = aerolist( l )%tracer     
         aerospc( spc )%charge     = aerolist( l )%charge     
         aerospc( spc )%visual_idx = aerolist( l )%visual_idx 
         aerospc( spc )%visual_idx_large = aerolist( l )%visual_idx_large
         aerospc( spc )%om         = aerolist( l )%om
         aerospc( spc )%optic_surr = refractive_index( l ) 
         aerospc( spc )%kappaorg   = aerolist( l )%kappaorg
         aerospc( spc )%name( : )  = ''

         If ( aerospc( spc )%optic_surr .Ne. aerolist( l )%optic_surr ) Then
            xmsg  = 'For aerosol species, ' // Trim( aerospc( spc )%bulkname )
     &            // ', setting refactive index to '
     &            // Trim( aerospc( spc )%optic_surr ) // ' instead of '
     &            // Trim(  aerolist( l )%optic_surr )
            write(logdev,'(a)')Trim( xmsg )
         End If   

         ! Map mode-independent properties
         aerospc_mw( spc ) = mole_weight( l )
         aerospc_mwinv( spc ) = 1.0E0 / aerospc_mw( spc )

         Do n = 1,n_mode
            ! Map the Modal-Dependent CGRID Pointers from AeroSpc to AeroList
            aerospc_map( spc, n ) = map_listtogrid( l, n )

            ! Remove this Chemical/Mode Combination from the "missing" list 
            ! if it can be mapped to the cgrid list of variables
            if ( aerospc_map( spc,n ) .ne. 0 ) then
                aero_missing( spc,n ) = .False.
                ! Create Mode-Dependent Names
                if ( aerospc( spc )%bulkname .eq. 'ACORS'    .or.
     &               aerospc( spc )%bulkname .eq. 'ASOIL'    .or.
     &               aerospc( spc )%bulkname .eq. 'ASEACAT'  .or.
     &               aerospc( spc )%bulkname .eq. 'ADE_CORS'     ) then
                   aerospc( spc )%name( n ) = trim( aerospc( spc )%bulkname )
                else
                   aerospc( spc )%name( n ) = 
     &                 trim( aerospc( spc )%bulkname ) // modesuff( n )
                end if
            end if
         End Do

      End Do
      Write(logdev,*)' '

C Build mapping to CGRID for aero # and surf area variables
      aeronum_map = 0
      aerosrf_map = 0

      Do m = 1, n_mode
         n = index1( aeromode( m )%num_name , n_ae_spc, ae_spc )
         p = index1( aeromode( m )%num_name , n_ae_trns, ae_trns )
         If ( n .Eq. 0 ) Then
            xmsg = 'Species ' // Trim( aeromode( m )%num_name )
     &           //' is not in AE namelist'
            AE_eflag  = .True.
            Call m3warn( pname, 0, 0, xmsg )
!            Call m3exit( pname, 0, 0, xmsg, xstat3 )
         Else
            aeronum_map( m ) = ae_strt - 1 + n
            aerocgrid_rhoinv( n ) = 1.0
            l_aesp_num( n ) = .True.
            l_aesp_mode( m,n ) = .True.
            if ( p .gt. 0 ) then 
               l_aetr_num( p ) = .True.
               l_aetr_mode( m,p ) = .True.
            end if
         End If

         n = index1( aeromode( m )%srf_name , n_ae_spc, ae_spc )
         p = index1( aeromode( m )%srf_name , n_ae_trns, ae_trns )
         If ( n .Eq. 0 ) Then
            xmsg = 'species ' // Trim( aeromode( m )%srf_name )
     &           // ' is not in AE namelist'
            AE_eflag  = .True.
            Call m3warn( pname, 0, 0, xmsg )
!            Call m3exit( pname, 0, 0, xmsg, xstat3 )
         Else
            aerosrf_map( m ) = ae_strt - 1 + n
            aerocgrid_rhoinv( n ) = 1.0
            l_aesp_srf( n ) = .True.
            l_aesp_mode( m,n ) = .True.
            if ( p .gt. 0 ) then 
               l_aetr_srf( p ) = .True.
               l_aetr_mode( m,p ) = .True.
            end if
         End If
      End Do

C Find indices of required species
      aso4_idx    = findAero( req_so4,    .True. )
      ano3_idx    = findAero( req_no3,    .True. )
      acl_idx     = findAero( req_cl,     .True. )
      anh4_idx    = findAero( req_nh4,    .True. )
      ana_idx     = findAero( req_na,     .True. )
      amg_idx     = findAero( req_mg,     .True. )
      ak_idx      = findAero( req_k,      .True. )
      aca_idx     = findAero( req_ca,     .True. )
      afe_idx     = findAero( req_fe,     .True. )
      amn_idx     = findAero( req_mn,     .True. )
      ah2o_idx    = findAero( req_h2o,    .True. )
      ah3op_idx   = findAero( req_h3op,   .True. )
      asoil_idx   = findAero( req_soil,   .True. )
      acors_idx   = findAero( req_cors,   .True. )
      aseacat_idx = findAero( req_seacat, .True. )
      apoc_idx    = findAero( req_poc,    .True. )
      apncom_idx  = findAero( req_ncom,   .True. )

      If ( ( Index( mechname, 'SAPRC07TIC_AE6I' ) .Gt. 0 ) .OR.
     &     ( Index( mechname, 'SAPRC07TIC_AE7I' ) .Gt. 0 )  ) Then
         ae6isoa     = .True.
         aietet_idx  = findAero( req_ietet, ae6isoa )
         aieos_idx   = findAero( req_ieos,  ae6isoa )
         adim_idx    = findAero( req_dim,   ae6isoa )
         aimga_idx   = findAero( req_imga,  ae6isoa )
         aimos_idx   = findAero( req_imos,  ae6isoa )
      Else
         aietet_idx  = 0
         aieos_idx   = 0
         adim_idx    = 0
         aimga_idx   = 0
         aimos_idx   = 0
         ae6isoa     = .False.
      End If

       If ( Index( mechname, 'CB6R5M_AE7_AQ' ) .Gt. 0 ) Then     
         abr_idx     = findAero( req_br,     .True. )
         marine = .True.
      Else
         abr_idx     = 0
         marine = .False.
      End If

      aphgj_idx = findAero( req_phgj,  .False. )
      If ( aphgj_idx .Gt. 0 ) Then
         ae6hg     = .True.
      Else
         aphgj_idx = 0 
         ae6hg     = .False.
      End If

C AORGH2O is not required, but highly recommended for aero7
      aorgh2o_idx = findAero( req_orgh2o,  .False. )
      If ( aorgh2o_idx .Gt. 0 ) Then
         ae7orgh2o   = .True.
      Else
         aorgh2o_idx = 0
         ae7orgh2o   = .False.
      End If

      If ( stm ) Then
         aso4aqh2o2_idx = findAero( req_so4aqh2o2, .True. )
         aso4aqo3_idx   = findAero( req_so4aqo3,   .True. )
         aso4aqfemn_idx = findAero( req_so4aqfemn, .True. )
         aso4aqmhp_idx  = findAero( req_so4aqmhp,  .True. )
         aso4aqpaa_idx  = findAero( req_so4aqpaa,  .True. )
         aso4gas_idx    = findAero( req_so4gas,    .True. )
         aso4emis_idx   = findAero( req_so4emis,   .True. )
         aso4icbc_idx   = findAero( req_so4icbc,   .True. )
         If ( ( ae6isoa ) .OR.
     &        ( Index( mechname, 'CRACMM1_'        ) .Gt. 0 ) .OR.
     &        ( Index( mechname, 'CRACMM2'         ) .Gt. 0 ) .OR.
     &        ( Index( mechname, 'CRACMM1AMORE_'   ) .Gt. 0 ) .OR.
     &        ( Index( mechname, 'CB6R3_AE7'       ) .Gt. 0 ) .OR.
     &        ( Index( mechname, 'CB6R5_AE7'       ) .Gt. 0 ) .OR.
     &        ( Index( mechname, 'CB6R5M_AE7'      ) .Gt. 0 ) ) Then
            oso4_idx       = findAero( req_oso4,       .True. )
            oso4aqh2o2_idx = findAero( req_oso4aqh2o2, .True. )
            oso4aqo3_idx   = findAero( req_oso4aqo3,   .True. )
            oso4aqfemn_idx = findAero( req_oso4aqfemn, .True. )
            oso4aqmhp_idx  = findAero( req_oso4aqmhp,  .True. )
            oso4aqpaa_idx  = findAero( req_oso4aqpaa,  .True. )
            oso4gas_idx    = findAero( req_oso4gas,    .True. )
            oso4emis_idx   = findAero( req_oso4emis,   .True. )
            oso4icbc_idx   = findAero( req_oso4icbc,   .True. )
         EndIf
      EndIf

C Compute common factors
      h2ofac = 1.0D-9 * f6dpi / Real( aerospc( ah2o_idx )%density, 8 )

C compute aeromode_minNum and aeromode_minM2
      so4fac  = 1.0E-9 * Real( f6dpi, 4 ) / aerospc( aso4_idx )%density
      anthfac = 1.0E-9 * Real( f6dpi, 4 ) / aerospc( acors_idx )%density

      Do m = 1, n_mode
         If ( m .Lt. n_mode ) Then
            aeromode_minNum( m ) = aerospc( aso4_idx )%min_conc( m ) /
     &           ( def_diam( m )**3 * Exp( 4.5 * Log( def_sigma_g( m ) )**2 ) )
            aeromode_minNum( m ) = aeromode_minNum( m ) * so4fac 
            aeromode_minNum( m ) = Max( aeromode_minNum( m ), conmin )
         Else
            aeromode_minNum( m ) = aerospc( acors_idx )%min_conc( m ) /
     &           ( def_diam( m )**3 * Exp( 4.5 * Log( def_sigma_g( m ) )**2 ) )
            aeromode_minNum( m ) = aeromode_minNum( m ) * anthfac 
            aeromode_minNum( m ) = Max( aeromode_minNum( m ), conmin )
         End If
         aeromode_minM2( m ) = aeromode_minNum( m ) *
     &             def_diam( m )**2 * Exp( 2.0 * Log( def_sigma_g( m ) )**2 )
      End do

      ! Substitute Gas and Rxn Counter Names for CGRID names
      Do spc = 1,n_aerospc
         If ( aerospc( spc )%gasname .ne. '' ) Then
            Do l = 1,n_gc_g2ae
                If ( aerospc( spc )%gasname .eq. gc_g2ae( l ) )
     &               aerospc( spc )%gasname = gc_spc( gc_g2ae_map(l) )                
            End Do
            Do l = 1,n_nr_n2ae
                If ( aerospc( spc )%gasname .eq. nr_n2ae( l ) )
     &               aerospc( spc )%gasname = nr_spc( nr_n2ae_map(l) )                
            End Do
         End If
         If ( aerospc( spc )%ctrname .ne. '' ) Then
            Do l = 1,n_gc_g2ae
                If ( aerospc( spc )%ctrname .eq. gc_g2ae( l ) )
     &               aerospc( spc )%ctrname = gc_spc( gc_g2ae_map(l) )                
            End Do
            Do l = 1,n_nr_n2ae
                If ( aerospc( spc )%ctrname .eq. nr_n2ae( l ) )
     &               aerospc( spc )%ctrname = nr_spc( nr_n2ae_map(l) )                
            End Do
         End If
      End Do

      ! Exit if Errors are Detected
      if( AE_eflag )Then
         Write(logdev,99901) Trim( mechname )
          xmsg = 'The FATAL errors found in namelist used. Check '
     &    //  'the log of exiting processor if more details are needed.'
         Call m3exit( pname, 0, 0, xmsg, xstat3 )
      End If 

      mapped = .True.


99901 Format( 'FATAL Error(s) found in the AE namelist used. Check that' 
     &  /' this AE namelist contains the above required data '
     &  / 'as the file in '
     &  /'the respository version of the mechanism: ', a )
      Return
      End Subroutine map_aero

C-----------------------------------------------------------------------
#ifdef sens
      Subroutine extract_aero( conc, minchk, s_conc, schk )
#else
      Subroutine extract_aero( conc, minchk )
#endif

C  Extracts aerosol data into the AERO_DATA:aerospc_conc array
C  The original idea is that the data for conc comes from CGRID
C  Also transfers dry surface area to wet 2nd moment.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C     4/2016: Updated for 2nd moment by H. Pye and B. Murphy
C-----------------------------------------------------------------------

      Use aeromet_data, only : pi, f6pi  ! fundamental constants, data type definitions, etc.

      Implicit None

C Arguments:
      Real,    Intent( In ) :: conc( : )
      Logical, Intent( In ) :: minchk
#ifdef sens
      Real,    Intent( In ) :: s_conc( :,: )
      Logical, Intent( In ) :: schk  ! necessary because some routines don't require sensitivity updates
#endif

C Local Variables:
      Integer m, n, spc

      If ( .Not. mapped ) Then
         Call map_aero()
      End If

#ifdef sens
      If ( schk ) Then
         If ( .Not. ae_ddm3d_ready ) Then
            Call init_aero_ddm3d()
         End If
      End If
#endif

C Copy grid cell concentrations of aero species to aerospc_conc
      aerospc_conc = 0.0
#ifdef sens
      If ( schk ) Then
         s_aerospc_conc = 0.0D0
      End If
#endif
      If ( minchk ) Then
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                  aerospc_conc( spc,m ) = Max( conc( n ), aerospc( spc )%min_conc( m ) ) ! [ug/m^3]
#ifdef sens
                  If ( schk ) Then
                     Do np = 1, npmax
                        If ( aerospc_conc( spc,m ) .Eq. aerospc( spc )%min_conc( m ) ) Then
                           s_aerospc_conc( spc,m,np ) = 0.0D0
                        Else
                           s_aerospc_conc( spc,m,np ) = Real( s_conc( np,n ), 8 )
                        End If
                     End Do
                  End If
#endif
               End If
            End Do
         End Do
      Else
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                  aerospc_conc( spc,m ) = conc( n )   ! [ug/m^3]
#ifdef sens
                  If ( schk ) Then
                     Do np = 1, npmax
                        s_aerospc_conc( spc,m,np ) = Real( s_conc( np,n ), 8 )
                     End Do
                  End If
#endif
               End If
            End Do
         End Do
      End If

      ! Calculate Dry Third Moment [ m3 / m3 ]
      Do m = 1, n_mode
         moment3_conc( m ) = sum( aerospc_conc(:,m) / aerospc(:)%density,
     &      MASK= ( ( .NOT. aerospc%no_M2wet ) .AND.
     &              ( .NOT. aerospc%Tracer ) ) )
         moment3_conc( m ) = max( moment3_conc( m ) * 1.0E-9 * f6pi,
     &                            aeromode( m )%min_m3conc )
      End Do

C Copy grid cell concentrations of aero # and surf area
C Convert surface area to M2 and set wet_moments_flag

      moment0_conc = 0.0
      moment2_conc = 0.0

      If ( minchk ) Then
         Do m = 1, n_mode
            n = aeronum_map( m )
            moment0_conc( m ) = Max( conc( n ), aeromode( m )%min_numconc )
            n = aerosrf_map( m )
            moment2_conc( m ) = Max( conc( n ) / pi, aeromode( m )%min_m2conc )
         End Do
      Else
         Do m = 1, n_mode
            n = aeronum_map( m )
            moment0_conc( m ) = conc( n )
            n = aerosrf_map( m )
            moment2_conc( m ) = conc( n ) / pi
         End Do
      End If
      wet_moments_flag = .false.

C Convert dry 2,3 moment to wet 2,3 moment
C flag will be set to .true.
      Call calcmoments( .true. )

      Return
      End Subroutine extract_aero

C-----------------------------------------------------------------------
#ifdef sens
      Subroutine update_aero( conc, minchk, s_conc )
#else
      Subroutine update_aero( conc, minchk )
#endif

C  Updates conc from the AERO_DATA:aerospc_conc array.
C  The original idea is that the data in conc updates CGRID
C  Update_aero now also saves the updated surface area back to CGRID as
C  well. Moment2 will be dried if necessary and flag reset.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C     4/2016: Updated for 2nd moment by H. Pye and B. Murphy
C-----------------------------------------------------------------------

      Use aeromet_data, only : pi     ! fundamental constants, data type definitions, etc.

      Implicit None

C Arguments:
      Real, Intent( Out ) :: conc( : )
      Logical, Intent( In ) :: minchk
#ifdef sens 
      Real,    Intent( Out ) :: s_conc( :,: )
#endif

C Local variables:
      Character( 80 ) :: xmsg
      Integer m, n, spc

      If ( .Not. mapped ) Then
         xmsg = 'CGRID Species has not been mapped'
         Call m3exit( pname, 0, 0, xmsg, xstat3 )
      End If

C Copy aerospc_conc back to grid cell concentrations

      If ( minchk ) Then
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                    conc( n ) = Max( aerospc_conc( spc,m ), aerospc( spc )%min_conc( m ) )
#ifdef sens
                    Do np = 1, npmax
                       If ( conc( n ) .Eq. aerospc( spc )%min_conc( m ) ) Then
                          s_conc( np,n ) = 0.0
                       Else
                          s_conc( np,n ) = Real( s_aerospc_conc( spc,m,np ), 4 )
                       End If
                    End Do
#endif
               End If
            End Do
         End Do
      Else
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                    conc( n ) = aerospc_conc( spc,m )
#ifdef sens
                    Do np = 1, npmax
                       s_conc( np,n ) = Real( s_aerospc_conc( spc,m,np ), 4 )
                    End Do
#endif
               End If
            End Do
         End Do
      End If

C Copy aero number and surface area back to grid cell concentrations

      If ( minchk ) Then
         Do m = 1, n_mode
            n = aeronum_map( m )
            conc( n ) = Max( moment0_conc( m ), aeromode( m )%min_numconc )
         End Do
      Else
         Do m = 1, n_mode
            n = aeronum_map( m )
            conc( n ) = moment0_conc( m )
         End Do
      End If

C Save dry second moment to surface area (with pi conversion)
      If ( wet_moments_flag ) Then
         Call calcmoments( .False. ) ! called with the F flag, returns dry moments
      End If

      Do m = 1, n_mode
         n = aerosrf_map( m )
         conc( n ) = Real( pi, 4 ) * moment2_conc( m )
      End Do

      Return
      End Subroutine update_aero

C-----------------------------------------------------------------------
      Function findAero( vname, required ) Result ( idx )

C  Finds the index of 'required' aerosol species in the aerospc list

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Implicit None

C Arguments:
      Character( 16 ) :: vname
      Logical :: required
      Integer :: idx

C Local Variables:
      Character( 80 ) :: xmsg
      Integer spc, n

      idx = 0
C Find the substring vname in aerospc( spc )%name( n )
      Do spc = 1, n_aerospc
         If ( aerospc( spc )%bulkname .eq. vname ) Then
            idx = spc
            Return
         End If
         Do n = 1, n_mode
            If ( aerospc( spc )%name( n ) .eq. vname ) Then
               idx = spc
               Return
            End If
         End Do
      End Do

      If ( .Not. required ) Then
         xmsg = 'Optional Species '
     &       // Trim( vname ) // ' Not found in AE namelist.'
         write(logdev,'(5x,a)') xmsg
         Return
      End If

      xmsg = 'Required Species ' // Trim( vname ) // 
     &       ' Not found in AE namelist'
      AE_eflag  = .True.
      Call m3warn( pname, 0, 0, xmsg )

      Return
      End Function findAero

C-----------------------------------------------------------------------
      Subroutine calcmoments( addwet )

C Subroutine calculates wet (addwet=T) or dry (addwet=F) aerosol third
C and second moment and stores them in moment_conc arrays and
C updates wet_moments_flag.
C Note that third moment information will be overwritten no matter what
C the wet_moments_flag indicates. M2 will depend on the history
C of the moment (wet_moments_flag). This routine will not update
C M2 in the event of added mass due to processes other than
C wetting/drying.
C
C Notes:
C wet_moments_flag is obtained from AERO_DATA ! true =  H2O and SOA included in 2,3 moment
C                                             ! false = H2O and SOA excluded from 2,3 moment
C wet_moments_flag reflects the current state of the moment2,3_conc arrays.
C addwet will results in wet (T) or dry (F) moments.
C
C History:
C 4/2016: HOT Pye Created routine
C
C-----------------------------------------------------------------------

      Use aeromet_data, only: f6dpi, f6pi

      Implicit None

C Arguments:
      Logical :: addwet ! T: result in wet m3 and m2, F: result in dry m3 and m2

C Parameters:
      Real( 8 ), Parameter :: two3rds = 2.0D0 / 3.0D0

C Local variables:
      Integer :: spc, n ! loop variable
      Real( 4 ) :: m3( n_mode )       ! wet or dry M3
      Real( 4 ) :: m2( n_mode )       ! wet or dry M2
      Real( 8 ) :: drysumM3  ! dry M3 [ m**3 / m**3 ]
      Real( 8 ) :: wetsumM3  ! wet M3 [ m**3 / m**3 ]
      Real( 8 ) :: factor
      Real( 4 ) :: initialM3 ! initial M3 from moment3_conc [ m**3 /m**3 ]
      Real( 4 ) :: initialM2 ! initial M2 from moment2_conc [ m**2 /m**3 ]

      Character( 16 )  :: pname_loc = 'CalcMoments'
      Character( 100 ) :: xmsg

C *** Calculate aerosol 3rd moment concentrations [ m**3 / m**3 ], 2nd
C     moment [ m**2/m**3 ]

      If( addwet ) then

         Do n = 1, n_mode
            initialM2 = moment2_conc( n )
            If ( initialM2 .Eq. 0.0 ) Then
                write( xmsg,'(A32,I1,A42)') "Warning: Second Moment for Mode ",
     &                 n," is 0.0. This will cause numerical issues."
                Call m3warn( pname_loc, 0, 0, xmsg )
            End If

            initialM3 = max( moment3_conc( n ), aeromode( n )%min_m3conc )
            If ( initialM3 .Eq. 0.0 ) Then
                write( xmsg,'(A31,I1,A42)') "Warning: Third Moment for Mode ",
     &                 n," is 0.0. This will cause numerical issues."
                call m3warn( pname_loc, 0, 0, xmsg )
            End If

            wetsumM3 = 0.0d0
            Do spc = 1, n_aerospc
               If ( aerospc( spc )%tracer .Or. aero_missing(spc,n) ) Cycle
               factor = Real( 1.0E-9 * f6pi / aerospc( spc )%density, 8 )
               wetsumM3  = wetsumM3 + factor * Real( aerospc_conc( spc,n ), 8 )
            End Do
            m3( n ) = Max ( Real( wetsumM3, 4 ), aeromode( n )%min_m3conc )
            If ( wet_moments_flag ) Then
               m2( n ) = initialM2
            Else
               m2( n ) = initialM2 * ( Real( wetsumM3, 4 ) / initialM3 ) ** Real( two3rds, 4 )
            End if
         End Do

         ! Save back to aero_data variables
         moment2_conc( : ) = m2( : )
         moment3_conc( : ) = m3( : )
         wet_moments_flag = .True.

      Else ! produce dry moments

         Do n = 1, n_mode
            initialM2 = moment2_conc( n )
            If ( initialM2 .Eq. 0.0 )  Then
                write( xmsg,'(A32,I1,A42)') "Warning: Second Moment for Mode ",
     &                 n," is 0.0. This will cause numerical issues."
                Call m3warn( pname_loc, 0, 0, xmsg )
            End If

            initialM3 = max( moment3_conc( n ), aeromode( n )%min_m3conc )
            If ( initialM3 .Eq. 0.0 )  Then
                write( xmsg,'(A31,I1,A42)') "Warning: Third Moment for Mode ",
     &                 n," is 0.0. This will cause numerical issues."
                Call m3warn( pname_loc, 0, 0, xmsg )
            End If

            drysumM3 = 0.0d0
            Do spc = 1, n_aerospc
               If ( aerospc( spc )%tracer .Or. aero_missing(spc,n) .Or.
     &              aerospc( spc )%no_M2Wet  ) Cycle
               factor = Real( 1.0E-9 * f6pi /  aerospc( spc )%density, 8 )
               drysumM3  = drysumM3 + factor * Real( aerospc_conc( spc,n ), 8 )
            End Do
            m3( n ) = Max ( Real( drysumM3, 4 ), aeromode( n )%min_m3conc )
            If ( wet_moments_flag) Then
               m2( n ) = initialM2 * ( Real( drysumM3, 4 ) / initialM3 ) ** Real( two3rds, 4 )
            Else ! already dry
               m2( n ) = initialM2
            End If
         End Do

         ! Save back to aero_data variables
         moment2_conc( : ) = m2( : )
         moment3_conc( : ) = m3( : )
         wet_moments_flag = .False.

      End If

      Return
      End Subroutine calcmoments

C
C-----------------------------------------------------------------------
      Subroutine CHECK_AERO_ICBC( IBCON0, LM2WET, USE_M2, IS_BC, 
     &                            L_WRITE_WARNING, COL, ROW, LAY )

C Subroutine checks that all size distributions from boundary and
C initial conditions are within tolerances for diameter and mode
C width. If any parameters are out of range or particle number or mass
C are neglgible, this routine will enforce default parameters and
C recalculate all three moments so that calculations in subsequent 
C modules are stable.
C
C Inputs:  IBCON - the actual initial or boundary condition vector of 
C                  concentrations from the calling routine
C          LM2WET - Logical identifying if the second moment of the 
C                   distribution is dry or wet. False = Dry. 'Wet'
C                   aerosol includes species for which the variable
C                   no_M2Wet is set to True in the aerolist.
C          USE_M2 - Logical prescribing whether or not to use
C                       the second moment from the input IC or BC file. 
C                       True = use the 2nd moment input from the file.
C          IS_BC - TRUE if this is for BCs, FALSE for ICs
C          L_WRITE_WARNING - Should a warning be printed if a size
C                            distribution fails
C
C 11 May 16  B.Murphy  Program Written
C
C-----------------------------------------------------------------------
      Use AEROMET_DATA, only : pi, f6pi
      Use CGRID_SPCS, only : N_AE_TRNS, N_AE_SPC
      Use HGRD_DEFN, ONLY : NROWS, NCOLS

#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_COMM_MODULE, SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_COMM_MODULE, NOOP_UTIL_MODULE)
#endif
#endif


      Implicit None

      REAL, INTENT(INOUT)   :: IBCON0( : )
      LOGICAL, INTENT( IN ) :: LM2WET          ! FALSE (Default) for Dry distribution parameters
                                               ! TRUE  for Wet distribution parameters
      LOGICAL, INTENT( IN ) :: USE_M2          ! TRUE (Default) to use second moment from input file
                                               ! FALSE  to ignore and overwrite M2 from input
                                                  ! file
      LOGICAL, INTENT( IN ) :: IS_BC           ! TRUE if BCON called this routine. 
                                               ! FALSE if ICON called it
      LOGICAL, INTENT( INOUT ) :: L_WRITE_WARNING ! TRUE if warning should be printed 
                                               ! when size distribution fails tests
      INTEGER, INTENT( IN ) :: COL, ROW, LAY

      INTEGER :: LSTAT( N_MODE ) ! 0 - Distribution is ok
                                 ! Nonzero values indicate problem with
                                 ! modal parameters. See below 
                                                                    ! | Before  After
      REAL(8) :: AER_PAR( 2, N_MODE, 8 ) !Track the modal parameters! |    N,    N
                                         !before and after the IC/BC! |  M2WET, M2WET
                                         !check routine             ! |  M2DRY, M2DRY
                                                                    ! |  M3WET, M3WET
                                                                    ! |  M3DRY, M3DRY
                                                                    ! |  dgwet, dgwet
                                                                    ! |  dgdry, dgdry
                                                                    ! |  sigma, sigma
      INTEGER, SAVE    :: J, SFX, EFX, NFX, WFX
      LOGICAL, SAVE    :: BNDY_PE_LOY, BNDY_PE_HIY,
     &                    BNDY_PE_LOX, BNDY_PE_HIX
      LOGICAL, SAVE    :: FIRST_TIME = .TRUE.
      LOGICAL          :: LBC

      REAL(8), PARAMETER  :: F1PI = 1.d0 / REAL(pi,8)
      REAL(8), PARAMETER  :: ONE_THIRD  = 1.0d0 / 3.0d0
      REAL(8), PARAMETER  :: TWO_THIRDS = 2.0d0 / 3.0d0
      CHARACTER( 199 ) :: XMSG2 = ' '
      
      ! Definition of Distribution Error Statuses (LSTAT):
      !    0 = All Parameters within Limits
      !    4 = Standard Deviation is just barely invalid. 
      !         Reset Surface Area but don't warn. (i.e. 1.04999 vs. 1.05)
      !    5 = Diameter is just barely invalid but Standard Deviation 
      !          is valid. Reset Number and Surface Area but don't warn.
      !    6 = Diameter and Standard Deviation are barely invalid. 
      !          Reset Number and Surface Area but don't warn.
      !    11 = Mass is below limit. Reset distribution to minimum valid
      !           concentration (i.e. conmin).
      !    12 = Number is below limit. Reset number and Surface Area
      !    13 = Surface Area is below limit but diameter is valid.
      !          Set standard deviation to default (def_l2sg) and reset 
      !          Surface Area.
      !    14 = Standard Deviation is invalid. Reset Surface Area.
      !    15 = Diameter is invalid but Standard Deviation is valid.
      !          Reset Number and Surface Area.
      !    16 = Diameter and Standard Deviation are invalid. Reset
      !          Number and Surface Area.


C Local variables:

      INTEGER   :: IMODE, IT, IS
      REAL(8), ALLOCATABLE :: IBCON(:)
      REAL(8)   :: NUM, M2, M3, M3DRY, M3WET, M2WET, M2DRY, 
     &             l2sg, dg, dgdry, dgwet, sg, l2sg_new
      REAL, Parameter :: KGPMG = 1.0E-9 !Kilogram per microgram m-3

      IF ( FIRST_TIME ) THEN
         FIRST_TIME = .FALSE.

#ifndef mpas
         ! Retrieve Info about boundary cells
         CALL SUBST_HI_LO_BND_PE ( 'R', BNDY_PE_LOY, BNDY_PE_HIY )
         CALL SUBST_HI_LO_BND_PE ( 'C', BNDY_PE_LOX, BNDY_PE_HIX )
#endif
         SFX = 0
         EFX = NCOLS + 1
         NFX = NCOLS + NROWS + 3
         WFX = 2 * NCOLS + NROWS + 4
      END IF

      ! First Determine Whether or Not this is a true Boundary
      ! Condition Processor and GridCell
      IF ( IS_BC ) THEN
          LBC = .FALSE.
          IF ( BNDY_PE_LOY .AND. ROW .GT. SFX .AND.
     &         ROW .LE. SFX+NCOLS ) LBC = .TRUE.
          
          IF ( BNDY_PE_HIX .AND. ROW .GT. EFX .AND.
     &         ROW .LE. EFX+NROWS ) LBC = .TRUE.

          IF ( BNDY_PE_HIY .AND. ROW .GT. NFX .AND.
     &         ROW .LE. NFX+NCOLS ) LBC = .TRUE.
          
          IF ( BNDY_PE_LOX .AND. ROW .GT. WFX .AND. 
     &         ROW .LE. WFX+NROWS ) LBC = .TRUE.
          
          IF ( .NOT. LBC ) RETURN

      END IF

      ! Initialize Aerosol ICBC Check Routine
      LSTAT    = 0
      AER_PAR = 0.0d0
      dg      = 0.0d0
      sg      = 0.0d0
       
      CALL MAP_AERO()
      ALLOCATE( IBCON( SIZE(IBCON0) ) )
      IBCON = REAL( IBCON0, 8 )

      !Loop Through Each Aerosol Mode. Sum up the third moment, then calculate 
      !the Dg and Sg of the mode and check to make sure they are valid. When 
      !checking Dg, use wet or dry diameter limits depending on the state of
      !the incoming (initial or boundary) size distribution.
      DO IMODE = 1,N_MODE

         IF ( IS_BC ) THEN
            ! IBCON is stored in order of Transported Aerosols
            NUM = SUM( IBCON( : ), MASK = ( L_AETR_MODE( IMODE,: ) .AND. L_AETR_NUM ) ) 
            M2  = SUM( IBCON( : ), MASK = ( L_AETR_MODE( IMODE,: ) .AND. L_AETR_SRF ) ) * F1PI
           
            M3DRY = SUM( IBCON( : ) * REAL( AEROCGRID_RHOINV * F6PI, 8), 
     &                   MASK = ( L_AETR_MASS .AND. L_AETR_MODE( IMODE,: )  
     &                            .AND. L_AETR_DRY ) ) 
            M3WET = SUM( IBCON( : ) * REAL( AEROCGRID_RHOINV * F6PI, 8), 
     &                   MASK = ( L_AETR_MASS .AND. L_AETR_MODE( IMODE,: ) ) )
         ELSE
            ! IBCON is stored in order of CGRID
            NUM = SUM( IBCON( : ), MASK = ( L_AESP_MODE( IMODE,: ) .AND. L_AESP_NUM ) )
            M2  = SUM( IBCON( : ), MASK = ( L_AESP_MODE( IMODE,: ) .AND. L_AESP_SRF ) ) * F1PI
           
            M3DRY = SUM( IBCON( : ) * REAL( AEROCGRID_RHOINV * F6PI, 8), 
     &                   MASK = ( L_AESP_MASS .AND. L_AESP_MODE( IMODE,: )  
     &                            .AND. L_AESP_DRY ) ) 
            M3WET = SUM( IBCON( : ) * REAL( AEROCGRID_RHOINV * F6PI, 8), 
     &                   MASK = ( L_AESP_MASS .AND. L_AESP_MODE( IMODE,: ) ) ) 
         END IF
         
         ! Store M2 as Wet or Dry
         IF ( LM2WET ) THEN
             M2WET = M2
             M2DRY = 0. 
         ELSE
             M2WET = 0.
             M2DRY = M2
         END IF
                
         ! If checking Boundary Conditions, the concentrations are
         ! already in kilograms, thus we do not need to convert them
         ! before applying aerosol density (kg m-3). If checking Initial
         ! Conditions, we do need to convert micrograms to kilograms.
         IF ( .NOT. IS_BC ) THEN 
             M3WET = M3WET * REAL( KGPMG,8)
             M3DRY = M3DRY * REAL( KGPMG,8)
         END IF

         AER_PAR ( 1, IMODE, 1 ) = NUM
         AER_PAR ( 1, IMODE, 2 ) = M2WET
         AER_PAR ( 1, IMODE, 3 ) = M2DRY
         AER_PAR ( 1, IMODE, 4 ) = M3WET
         AER_PAR ( 1, IMODE, 5 ) = M3DRY
         AER_PAR ( 1, IMODE, 6 ) = 0.    
         AER_PAR ( 1, IMODE, 7 ) = 0.    
         AER_PAR ( 1, IMODE, 8 ) = 0.    

         ! Begin Checking Aerosol Parameters. 
         IF ( M3DRY .LT. 1.1e-30 .OR. M3WET .LT. 1.1e-30 ) THEN
             ! Dry or Wet Mass is below limit -> reset distribution
             LSTAT( IMODE ) = 11

             ! Set Problematic Aerosol Mass to Minimum valid
             ! concentration (i.e. conmin) in Output Vector
             IF ( IS_BC ) THEN
                DO IT = 1,N_AE_TRNS
                  IF ( L_AETR_MODE( IMODE,IT ) .AND. L_AETR_MASS( IT ) ) 
     &               IBCON(IT) = MAX( IBCON(IT), CONMIND )
                END DO
                M3DRY = SUM( IBCON( : ) * REAL(AEROCGRID_RHOINV * F6PI,8), 
     &                       MASK = ( L_AETR_MASS .AND. L_AETR_MODE( IMODE,: )  
     &                                .AND. L_AETR_DRY ) ) 
                M3WET = SUM( IBCON( : ) * REAL(AEROCGRID_RHOINV * F6PI,8), 
     &                       MASK = ( L_AETR_MASS .AND. L_AETR_MODE( IMODE,: ) ) )
             ELSE
                DO IS = 1,N_AE_SPC
                  IF ( L_AESP_MODE( IMODE,IS ) .AND. L_AESP_MASS( IS ) ) 
     &               IBCON(IS) = MAX( IBCON(IS), CONMIND )
                END DO
                M3DRY = SUM( IBCON( : ) * REAL(AEROCGRID_RHOINV * F6PI,8), 
     &                       MASK = ( L_AESP_MASS .AND. L_AESP_MODE( IMODE,: )  
     &                                .AND. L_AESP_DRY ) ) 
                M3WET = SUM( IBCON( : ) * REAL(AEROCGRID_RHOINV * F6PI,8), 
     &                       MASK = ( L_AESP_MASS .AND. L_AESP_MODE( IMODE,: ) ) ) 
             END IF

             L2SG  = REAL( DEF_L2SG( IMODE ),8)
             SG    = EXP( SQRT( L2SG ) )
             
             DGDRY = REAL( DEF_DIAM( IMODE ),8)  ! Dry Diameter Default
             NUM   = M3DRY * ( EXP( -4.5 * L2SG ) ) / ( DGDRY ) ** 3
             M2DRY = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3DRY ) - L2SG )
             
             DGWET = ( M3WET / NUM * EXP( -4.5 * L2SG ) )  ** ( ONE_THIRD )
             M2WET = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3WET ) - L2SG )

         ELSE IF ( NUM .LT. 1.1e-30 ) THEN
             ! Number is below limit -> reset Num and Surf with
             !  default dry diameter and default standard deviation
             LSTAT( IMODE ) = 12

             L2SG  = REAL( DEF_L2SG( IMODE ), 8)
             SG    = EXP( SQRT( L2SG ) )

             DGDRY = REAL( DEF_DIAM( IMODE ), 8)  ! Dry Diameter Default
             NUM   = M3DRY * ( EXP( -4.5 * L2SG ) ) / ( DGDRY ) ** 3
             M2DRY = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3DRY ) - L2SG )

             DGWET = ( M3WET / NUM * EXP( -4.5 * L2SG ) )  ** ( ONE_THIRD )
             M2WET = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3WET ) - L2SG )

         ELSE IF ( M2 .LT. 1.1e-30 .OR. .NOT. USE_M2 ) THEN                
             ! Mass and Number are valid. Surface area is either invalid
             ! or the user has issued an override. The mass and number
             ! concentrations specify the dry and wet diameters, and the
             ! second moment will be generated by assuming a
             ! representative sigma.
             LSTAT( IMODE ) = 13

             L2SG  = REAL( DEF_L2SG( IMODE ), 8)
             SG    = EXP( SQRT( L2SG ) )

             DGDRY = ( M3DRY / NUM * EXP( -4.5 * L2SG ) )  ** ( ONE_THIRD )
             M2DRY = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3DRY ) - L2SG )

             DGWET = ( M3WET / NUM * EXP( -4.5 * L2SG ) )  ** ( ONE_THIRD )
             M2WET = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3WET ) - L2SG )

         ELSE
             ! All three moments are possibly valid. 
             ! Diagnose and Check Standard Deviation
             IF ( LM2WET ) THEN 
                L2SG = ( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3WET ) - LOG( M2WET ))
             ELSE
                L2SG = ( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3DRY ) - LOG( M2DRY ))
             END IF

             sg = 1.0d0
             IF ( L2SG .gt. 0.0d0 ) SG = EXP( SQRT( L2SG ) )
             AER_PAR( 1, IMODE, 8 ) = SG
             IF ( (L2SG .LT. REAL( MIN_L2SG,8) .AND. L2SG .GT. 0.95d0 * REAL(MIN_L2SG,8)) .OR.
     &            (L2SG .GT. REAL( MAX_L2SG,8) .AND. L2SG .LT. 1.05d0 * REAL(MAX_L2SG,8)) ) THEN
                  ! Standard deviation is barely invalid. Don't trigger warning
                  LSTAT( IMODE ) = 4
             ELSE IF ( L2SG .LT. REAL( 0.95*MIN_L2SG,8) .OR. L2SG .GT. REAL(1.05*MAX_L2SG,8) ) THEN 
                  ! Standard deviation is invalid. Reset Standard Deviation
                  LSTAT( IMODE ) = 14
             END IF
             L2SG  = MIN( MAX( L2SG, REAL(MIN_L2SG,8) ), REAL(MAX_L2SG,8) )
             SG   = EXP( SQRT( L2SG ) )

             ! Diagnose and Check Dry Diameter against Limits
             DGDRY = ( M3DRY / NUM * EXP( -4.5 * L2SG ) )  ** ( ONE_THIRD )
             AER_PAR( 1, IMODE, 7 ) = DGDRY
             DGWET = ( M3WET / NUM * EXP( -4.5 * L2SG ) )  ** ( ONE_THIRD )
             AER_PAR( 1, IMODE, 6 ) = DGWET

             IF ( (DGDRY .GT. REAL(0.95*MIN_DG_DRY(IMODE),8) .AND. DGDRY .LT. REAL(MIN_DG_DRY(IMODE),8)) .OR.
     &            (DGDRY .GT. REAL(MAX_DG_DRY(IMODE),8) .AND. DGDRY .LT. REAL(1.05*MAX_DG_DRY(IMODE),8)) ) THEN
                  ! Diameter is barely invalid. Don't trigger warning
                  IF ( LSTAT( IMODE ) .EQ. 4 ) THEN
                     LSTAT( IMODE ) = 6  ! Both sg and dg are barely invalid
                  ELSE IF (LSTAT( IMODE ) .EQ. 0 ) THEN
                     LSTAT( IMODE ) = 5  ! Just dg is barely invalid
                  END IF
             
             ELSE IF ( DGDRY .LT. REAL(0.95*MIN_DG_DRY( IMODE ),8)  .OR. 
     &                 DGDRY .GT. REAL(1.05*MAX_DG_DRY(IMODE) ,8 ) ) THEN
                 ! Diameter is invalid. Reset Diameter
                 IF ( LSTAT( IMODE ) .EQ. 4 .OR. LSTAT( IMODE ) .EQ. 12 ) THEN
                    LSTAT( IMODE ) = 16  ! Both dg and sg are invalid
                ELSE IF ( LSTAT( IMODE ) .EQ. 0 ) THEN
                    LSTAT( IMODE ) = 15  ! Just dg is invalid
                 END IF
             ENDIF
             DGDRY = MIN( MAX( DGDRY, MIN_DG_DRY(IMODE) ), MAX_DG_DRY(IMODE) )

             ! Recalculate Number
             IF ( LSTAT( IMODE ) .GT. 0 ) 
     &            NUM   = M3DRY * ( EXP( -4.5 * L2SG ) ) / ( DGDRY ) ** 3

             ! Recalculate Dry Second Moment
             M2DRY = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3DRY ) - L2SG )

             ! Recalculate Wet Distribution Parameters
             DGWET = ( M3WET / NUM * EXP( -4.5 * L2SG ) )  ** ( ONE_THIRD )
             M2WET = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3WET ) - L2SG )
 
         ENDIF

         ! Export Number Concentration and Surface Area Concentration
         IF ( LM2WET ) THEN
             M2 = M2WET
         ELSE
             M2 = M2DRY
         END IF

         IF ( IS_BC ) THEN
            DO IT = 1,N_AE_TRNS
              IF ( L_AETR_MODE( IMODE,IT ) .AND. L_AETR_NUM( IT ) ) IBCON0(IT) = REAL(NUM,4)
              IF ( L_AETR_MODE( IMODE,IT ) .AND. L_AETR_SRF( IT ) ) IBCON0(IT) = REAL(M2,4) * PI
            END DO
         ELSE
            DO IS = 1,N_AE_SPC
              IF ( L_AESP_MODE( IMODE,IS ) .AND. L_AESP_NUM( IS ) ) IBCON0(IS) = REAL(NUM,4)
              IF ( L_AESP_MODE( IMODE,IS ) .AND. L_AESP_SRF( IS ) ) IBCON0(IS) = REAL(M2,4) * PI
            END DO
         END IF

         !Save Modal Properties After the Check
         AER_PAR( 2, IMODE, 1 ) = NUM
         AER_PAR( 2, IMODE, 2 ) = M2WET
         AER_PAR( 2, IMODE, 3 ) = M2DRY
         AER_PAR( 2, IMODE, 4 ) = M3WET
         AER_PAR( 2, IMODE, 5 ) = M3DRY
         AER_PAR( 2, IMODE, 6 ) = DGWET
         AER_PAR( 2, IMODE, 7 ) = DGDRY
         AER_PAR( 2, IMODE, 8 ) = SG
          
         !Print warning if any aerosol BC or IC violated the size 
         !distribution parameters
         IF ( LSTAT( IMODE ) .GT. 10 ) THEN
           IF ( IS_BC ) THEN
              IF ( L_WRITE_WARNING ) THEN
                 L_WRITE_WARNING = .FALSE.
                 WRITE( XMSG2, '(A)' ),
     &              'ATTENTION: Applying fix to aerosol Boundary' //
     &              ' Conditions for aerosol modes.' //
     &              ' Set verbose_rdbcon preprocessor flag to' //
     &              ' learn more.'
                 WRITE( LOGDEV, * )
                 CALL LOG_MESSAGE( LOGDEV, XMSG2 ) 
                 WRITE( LOGDEV, * )
              END IF
#ifdef verbose_rdbcon
              WRITE( LOGDEV,* )
              WRITE ( LOGDEV,'(7x,A55,I1,/,9x,A10,I4,A7,I3,A10,I2,/,7x,A20,I1,A42,/,9x,A51,/,
     &              9x,A19,1x,A3,8x,A5,8x,A5,8x,A5,8x,A5,8x,A5,8x,A5,8x,A2,/,
     &              27x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,4x,F7.3,/,
     &              9x,A19,1x,A3,8x,A5,8x,A5,8x,A5,8x,A5,8x,A5,8x,A5,8x,A2,/,
     &              27x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,4x,F7.3)'),
     &            'Warning: Applying Aerosol Boundary Conditions for mode ',IMODE,
     &            'BC Index: ',ROW,'  Lay: ',LAY,'  Status: ',LSTAT(IMODE),
     &            'The Offending Mode (',IMODE,') had diameter and/or sigma out of bounds.',
     &            'It was overwritten by changing the Num and SrfArea.',
     &            'Modal Props Before:','Num','M2WET','M2DRY','M3WET','M3DRY','DGWET','DGDRY','Sg',(AER_PAR(1,IMODE,j),j=1,8),
     &            'Modal Props After: ','Num','M2WET','M2DRY','M3WET','M3DRY','DGWET','DGDRY','Sg',(AER_PAR(2,IMODE,j),j=1,8)
              WRITE( LOGDEV,* )
#endif
           ELSE
              IF ( L_WRITE_WARNING ) THEN
                 L_WRITE_WARNING = .FALSE.
                 WRITE( XMSG2, '(A)' ),
     &              'ATTENTION: Applying fix to aerosol Initial' //
     &              ' Conditions for aerosol modes.' //
     &              ' Set verbose_loadcgrid preprocessor flag to' //
     &              ' learn more.'
                 WRITE( LOGDEV, * )
                 CALL LOG_MESSAGE( LOGDEV, XMSG2 ) 
                 WRITE( LOGDEV, * )
              END IF
#ifdef verbose_loadcgrid 
              WRITE( LOGDEV, * )
              WRITE ( logdev, '(7x,A55,I1,/,9x,A5,I3,A7,I3,A7,I3,A10,I2,/,7x,A20,I1,A42,/,9x,A51,/,
     &              9x,A19,1x,A3,8x,A5,8x,A5,8x,A5,8x,A5,8x,A5,8x,A5,8x,A2,/,
     &              27x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,4x,F7.3,/,
     &              9x,A19,1x,A3,8x,A5,8x,A5,8x,A5,8x,A5,8x,A5,8x,A5,8x,A2,/,
     &              27x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,1x,E11.3,4x,F7.3)'),
     &            'Warning: Applying Aerosol Initial Conditions for mode ',IMODE,
     &            'Col: ',COL,'  Row: ',ROW,'  Lay: ',LAY,'  Status: ',LSTAT(IMODE),
     &            'The Offending Mode (',IMODE,') had diameter and/or sigma out of bounds.',
     &            'It was overwritten by changing the Num and SrfArea.',
     &            'Modal Props Before:','Num','M2WET','M2DRY','M3WET','M3DRY','DGWET','DGDRY','Sg',(AER_PAR(1,IMODE,j),j=1,8),
     &            'Modal Props After: ','Num','M2WET','M2DRY','M3WET','M3DRY','DGWET','DGDRY','Sg',(AER_PAR(2,IMODE,j),j=1,8)
              WRITE( LOGDEV,* )
#endif
           END IF    
         ENDIF
      ENDDO
      
      End Subroutine CHECK_AERO_ICBC

!-----------------------------------------------------------------------
      Subroutine CALC_AERODIST_PARAMS( INIT_TIME )

! Subroutine calculates the wet and then dry aerosol distribution parameters
!     and stores them in public arrays to be passed to the diagnostic 
!     array.
!-----------------------------------------------------------------------
 
      IMPLICIT NONE

      LOGICAL, INTENT( IN ) :: INIT_TIME

      CALL CALCMOMENTS( .TRUE. )
      CALL GETPAR( FIXED_sg )

      WET_AERO_DIAM( : ) = AEROMODE_DIAM( : ) * 1.0E6 ! um
      WET_AERO_M2  ( : ) = MOMENT2_CONC( : )
      WET_AERO_M3  ( : ) = MOMENT3_CONC( : )
      WET_AERO_DENS( : ) = AEROMODE_DENS( : )

      CALL CALCMOMENTS( .FALSE. )
      CALL GETPAR( FIXED_sg )

      DRY_AERO_DIAM( : ) = AEROMODE_DIAM( : ) * 1.0E6 ! um
      DRY_AERO_M2  ( : ) = MOMENT2_CONC( : )
      DRY_AERO_M3  ( : ) = MOMENT3_CONC( : )
      DRY_AERO_DENS( : ) = AEROMODE_DENS( : )

      END SUBROUTINE CALC_AERODIST_PARAMS

      End Module aero_data
 
