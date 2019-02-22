
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
C PL = Peng Liu; BM = Ben Murphy; BH = Bill Hutzell

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

      Implicit None

C Number of aerosol species and modes

      Integer, Parameter :: n_aerolist = 93     ! number of aero species
      Integer, Parameter :: n_mode = 3          ! number of modes:
                                                ! 1 = Aitken
                                                ! 2 = accumulation
                                                ! 3 = coarse
      Character(1), Parameter :: modesuff( n_mode ) = (/ 'I','J','K' /)

      Integer, Save :: n_aerospc ! number of aero species

C Default minimum concentration
      Real,    Parameter :: conmin  = 1.0E-30    ! [ ug m-3 ]
      Real(8), Parameter :: conminD = 1.0D-30    ! [ ug m-3 ]
      Real,    Parameter :: evapmin = 1.0E-20    ! [ ug m-3 ]
      Real(8), Parameter :: evapminD= 1.0D-20    ! [ ug m-3 ]
      Real,    Parameter :: cm_set( n_mode ) = (/conmin, conmin, conmin/)
      Real,    Parameter :: cm_so4( n_mode ) = (/conmin, conmin, conmin/)
      Real,    Parameter :: cm_cor( n_mode ) = (/conmin, conmin, conmin/)

      Real,    Parameter :: def_diam( n_mode )   = (/ 15.0E-9, 80.0E-9,  600.0E-9 /)  ! default background mean diameter for each mode
      Real,    Parameter :: min_diam_g( n_mode ) = (/ 1.0E-9,  30.0E-9,  120.0E-9 /)
      Real,    Parameter :: max_diam_g( n_mode ) = (/ 80.0E-9, 500.0E-9, 100.0E-6 /)

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

C Define Logical values as T and F for the aerospc table
      Logical, Parameter, Private :: T = .true.
      Logical, Parameter, Private :: F = .false.

C-------------------------------------------------------------------------------------------------------

      Type spcs_type
         Character( 16 ) :: name( n_mode )       ! mode-dependent names of aerosol species
         Character( 16 ) :: bulkname             ! mode-independent names of aerosol species
         Real            :: min_conc( n_mode )   ! minimum concentration values for each mode
         Real            :: density              ! density [ kg m-3 ]
         Logical         :: no_M2Wet             ! flag to exclude from 2nd moment during transport
         Logical         :: nonVol_soa           ! non-volatile SOA flag
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
         Real            :: min_conc( n_mode )   ! minimum concentration values for each mode
         Real            :: density              ! density [ kg m-3 ]
         Logical         :: no_M2Wet             ! flag to exclude from 2nd moment during transport
         Logical         :: nonVol_soa           ! non-volatile SOA flag
         Logical         :: tracer               ! tracer flag; does have not mass
         Integer         :: charge               ! electroneutrality charge
         Real            :: visual_idx           ! visual index factor [ m2 g-1 ]
         Real            :: visual_idx_large     ! visual index factor [ m2 g-1 ] for large mode, if not applicable, same value as visual_idx
         Logical         :: om                   ! flag for organic aerosols
         Character( 16 ) :: optic_surr           ! optical surrogate name
         Real            :: kappaorg             ! hygroscopicity parameter for organic aerosol (excluding POC+NCOM which must be calculated)
      End Type spcs_list_type

      Type( spcs_list_type ), Parameter :: aerolist( n_aerolist ) = (/
C                                              nonVolSOA       Visidx_Large
C                                           no_M2Wet|Tracer         |
C                     ---Name---                  | | | Charge      | OM           
C                               Min_Concs Density | | |   | Visidx  |  |  OptSurr  korg  
C                     ---------- -------- ------- + + +   + ------ --- +  -------- ----- 
     & spcs_list_type('ASO4    ', cm_so4, 1800.0, F,F,F, -2,  2.2, 4.8,F, 'SOLUTE', 0.00), ! Sulfate
     & spcs_list_type('ANO3    ', cm_set, 1800.0, F,F,F, -1,  2.4, 5.1,F, 'SOLUTE', 0.00), ! Nitrate
     & spcs_list_type('ACL     ', cm_set, 2200.0, F,F,F, -1,  1.7, 1.7,F, 'SOLUTE', 0.00), ! Chloride
     & spcs_list_type('ANH4    ', cm_set, 1800.0, F,F,F,  1,  0.0, 0.0,F, 'SOLUTE', 0.00), ! Ammonium
     & spcs_list_type('ANA     ', cm_set, 2200.0, F,F,F,  1,  1.7, 1.7,F, 'SOLUTE', 0.00), ! Sodium
     & spcs_list_type('AMG     ', cm_set, 2200.0, F,F,F,  2,  1.0, 1.0,F, 'DUST  ', 0.00), ! Magnesium
     & spcs_list_type('AK      ', cm_set, 2200.0, F,F,F,  1,  1.0, 1.0,F, 'DUST  ', 0.00), ! Potassium
     & spcs_list_type('ACA     ', cm_set, 2200.0, F,F,F,  2,  1.0, 1.0,F, 'DUST  ', 0.00), ! Calcium
     & spcs_list_type('APOC    ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.00), ! POA Carbon
     & spcs_list_type('APNCOM  ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.00), ! POA Non-Carbon          #10
     & spcs_list_type('AEC     ', cm_set, 2200.0, F,F,F,  0, 10.0,10.0,F, 'SOOT  ', 0.00), ! Elemental (Black) Carbon
     & spcs_list_type('AFE     ', cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Iron
     & spcs_list_type('AAL     ', cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Aluminum
     & spcs_list_type('ASI     ', cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Silicon
     & spcs_list_type('ATI     ', cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Titanium
     & spcs_list_type('AMN     ', cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Manganese
     & spcs_list_type('AH2O    ', cm_set, 1000.0, T,F,F,  0,  0.0, 0.0,F, 'WATER ', 0.00), ! Water
     & spcs_list_type('AORGH2O ', cm_set, 1000.0, T,F,F,  0,  0.0, 0.0,F, 'WATER ', 0.00), ! Organic Water
     & spcs_list_type('AH3OP   ', cm_set, 1000.0, T,F,T,  0,  0.0, 0.0,F, 'WATER ', 0.00), ! Hydronium
     & spcs_list_type('AOTHR   ', cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ', 0.00), ! Other                   #20
     & spcs_list_type('AALK1   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), ! Alkane SOA 1            
     & spcs_list_type('AALK2   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.06), ! Alkane SOA 2
     & spcs_list_type('AXYL1   ', cm_set, 1480.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.17), ! Xylene SOA 1
     & spcs_list_type('AXYL2   ', cm_set, 1480.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.11), ! Xylene SOA 2
     & spcs_list_type('AXYL3   ', cm_set, 1330.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Xylene SOA 3
     & spcs_list_type('ATOL1   ', cm_set, 1240.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Toluene SOA 1
     & spcs_list_type('ATOL2   ', cm_set, 1240.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.10), ! Toluene SOA 2
     & spcs_list_type('ATOL3   ', cm_set, 1450.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.20), ! Toluene SOA 3
     & spcs_list_type('ABNZ1   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.19), ! Benzene SOA 1
     & spcs_list_type('ABNZ2   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Benzene SOA 2           #30
     & spcs_list_type('ABNZ3   ', cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.23), ! Benzene SOA 3           
     & spcs_list_type('ATRP1   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.10), ! Terpene SOA 1
     & spcs_list_type('ATRP2   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.10), ! Terpene SOA 2
     & spcs_list_type('AISO1   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.14), ! Isoprene SOA 1
     & spcs_list_type('AISO2   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Isoprene SOA 2
     & spcs_list_type('AISO3   ', cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.21), ! Isoprene SOA 3
     & spcs_list_type('ASQT    ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), ! Sesquiterpene SOA
     & spcs_list_type('APAH1   ', cm_set, 1480.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.08), ! PAH SOA 1
     & spcs_list_type('APAH2   ', cm_set, 1480.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.06), ! PAH SOA 2
     & spcs_list_type('APAH3   ', cm_set, 1550.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.09), ! PAH SOA 3               #40
     & spcs_list_type('AOLGA   ', cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.18), ! Anth. Oligomer SOA      
     & spcs_list_type('AOLGB   ', cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.13), ! Biogenic Oligomer SOA
     & spcs_list_type('AORGC   ', cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.12), ! Cloud-Processed SOA
     & spcs_list_type('ASOIL   ', cm_set, 2600.0, F,F,F,  0,  0.6, 0.6,F, 'DUST  ', 0.00), ! Soil
     & spcs_list_type('ACORS   ', cm_cor, 2200.0, F,F,F,  0,  0.6, 0.6,F, 'DUST  ', 0.00), ! Coarse PM
     & spcs_list_type('ASEACAT ', cm_cor, 2200.0, F,F,F,  1,  1.7, 1.7,F, 'SOLUTE', 0.00), ! SeaSpray Cations        
                                                                                   
       ! Associated with ae6i                                                           
     & spcs_list_type('AMTNO3  ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.11), ! Monoterpene Nitrate SOA
     & spcs_list_type('AISOPNN ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.32), ! Isoprene Nitrate SOA
     & spcs_list_type('AMTHYD  ', cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), ! 
     & spcs_list_type('AIETET  ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15), ! Iso. Tetrol SOA         #50
     & spcs_list_type('AIEOS   ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.30), ! Iso. Organosulfate SOA  
     & spcs_list_type('ADIM    ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.13), !
     & spcs_list_type('AIMGA   ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.18), !
     & spcs_list_type('AIMOS   ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.36), !
                                                                                        
       ! Updated monoterpene SOA following Saha and Grieshop ES&T 2016                  
     & spcs_list_type('AMT1    ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.08), ! MT SOA Lowest Volatility
     & spcs_list_type('AMT2    ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.08), ! 
     & spcs_list_type('AMT3    ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.09), !
     & spcs_list_type('AMT4    ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), !
     & spcs_list_type('AMT5    ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07), !
     & spcs_list_type('AMT6    ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.05), !                         #60
     & spcs_list_type('AMT7    ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.06), ! MT SOA Highest Volatility
                                                                                        
       ! ae6i and cb6                                                                   
     & spcs_list_type('AGLY    ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.13), ! Glyoxal SOA
                                                                                        
       ! Semivolatile POA                                                               
     & spcs_list_type('ALVPO1  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.05), ! LV-POA
     & spcs_list_type('ASVPO1  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.05), ! SV-POA 1
     & spcs_list_type('ASVPO2  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.04), ! SV-POA 2
     & spcs_list_type('ASVPO3  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.03), ! SV-POA 3
     & spcs_list_type('AIVPO1  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.03), ! IV-POA 
     & spcs_list_type('ALVOO1  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.15), ! LV-SOA 1                
     & spcs_list_type('ALVOO2  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.13), ! LV-SOA 2
     & spcs_list_type('ASVOO1  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.11), ! SV-SOA 1                #70
     & spcs_list_type('ASVOO2  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.09), ! SV-SOA 2
     & spcs_list_type('ASVOO3  ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.08), ! SV-SOA 3
     & spcs_list_type('APCSO   ', cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ', 0.12), ! pcSOA
                                                                                  
       ! Lumped anthropogenic SOA (introduced aero7, M. Qin, 8/2018)
     & spcs_list_type('AAVB1   ', cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.20),
     & spcs_list_type('AAVB2   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.15),
     & spcs_list_type('AAVB3   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.14),
     & spcs_list_type('AAVB4   ', cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.12),

       ! The following species are associated with the Multi-Pollutant code
     & spcs_list_type('ANI     ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Nickel
     & spcs_list_type('ACR_VI  ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Chromium 6
     & spcs_list_type('ACR_III ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Chromium 3              #80
     & spcs_list_type('ABE     ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Beryllium
     & spcs_list_type('APB     ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Lead                     
     & spcs_list_type('ADE_OTHR', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Fine PM          
     & spcs_list_type('ADE_EC  ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Black Carbon
     & spcs_list_type('ADE_OC  ', cm_set, 2000.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Organic Carbon
     & spcs_list_type('ADE_NO3 ', cm_set, 1800.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Nitrate
     & spcs_list_type('ADE_SO4 ', cm_set, 1800.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Sulfate
     & spcs_list_type('ADE_CORS', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Diesel Coarse PM
     & spcs_list_type('ACD     ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Cadmium
     & spcs_list_type('AMN_HAPS', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Manganese               #90
     & spcs_list_type('APHG    ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Mercury
     & spcs_list_type('AAS     ', cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ', 0.00), ! Arsenic                 #92

      ! The following species are associated with the marine chemistry code
     & spcs_list_type('ABR     ', cm_set, 2200.0, F,F,F, -1,  1.7, 1.7,F, 'SOLUTE', 0.00)  ! Bromide 
     & /)
 
! Define Reference Emissions Size Distributions
!   Geometric mean diameter by volume (or mass) of emitted particles in
!   each mode [ m ] and geometric standard deviation of emitted particles.  
!   See paragraph #14 of Binkowski & Roselle (2003).
!   09/17/14 change by Kathleen Fahey - see Revision History, above.
      TYPE em_aero
          Character( 20 ) :: name
          Real            :: split( n_mode )  ! dimensionless
          Real            :: dgvem( n_mode )  ! meters
          Real            :: sgem ( n_mode )  ! dimensionless
      END TYPE em_aero
      INTEGER, PARAMETER  :: n_em_aero_ref = 7

      TYPE( em_aero ), Parameter :: em_aero_ref( n_em_aero_ref ) = (/

!              ----Name---- -----Split-----    ---Geo. Mean Diameter---    ---Stnd Dev.---
     & em_aero('FINE_REF  ',(/0.1,0.9,0.0/)  ,(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Default Accum and Aitken Mode
     & em_aero('ACC_REF   ',(/0.0,1.0,0.0/)  ,(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Just Accumulation Mode
     & em_aero('COARSE_REF',(/0.0,0.0,1.0/)  ,(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Just Coarse Mode
     & em_aero('UNITY_REF ',(/1.0,1.0,1.0/)  ,(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Used for online sectors (e.g. SeaSpray)
     & em_aero('ZERO_REF  ',(/0.0,0.0,0.0/)  ,(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Zero out the emissions
     & em_aero('WB_DUST   ',(/0.0,0.07,0.93/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Default Dust Parameterization. Currently,
                                                                                             !  these values are hard-coded in the dust 
                                                                                             !  routine.
     & em_aero('SEASPRAY  ',(/0.0,0.05,0.05/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/))  ! Default Sea Spray Parameterization is Dynamic. 
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
         Character( 16 ) :: description
         Character( 16 ) :: name  
         Real            :: spcfac( n_mode )
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
C                    --description-- -- Species --  -------- spcfac --------
C                                                    Aitken  accum   coarse
     &   emis_table( 'Sulfate       ', 'ASO4    ', (/ 0.0, 0.02250, 0.02655/) ),   ! Sulfate
     &   emis_table( 'Nitrate       ', 'ANO3    ', (/ 0.0, 0.00020, 0.00160/) ),   ! Nitrate
     &   emis_table( 'Chlorine      ', 'ACL     ', (/ 0.0, 0.00945, 0.01190/) ),   ! Chlorine
     &   emis_table( 'Ammonium      ', 'ANH4    ', (/ 0.0, 0.00005, 0.0    /) ),   ! Ammonium
     &   emis_table( 'Sodium        ', 'ANA     ', (/ 0.0, 0.03935, 0.0    /) ),   ! Sodium
     &   emis_table( 'Calcium       ', 'ACA     ', (/ 0.0, 0.07940, 0.0    /) ),   ! Calcium
     &   emis_table( 'Magnesium     ', 'AMG     ', (/ 0.0, 0.01900, 0.0    /) ),   ! Magnesium
     &   emis_table( 'Potassium     ', 'AK      ', (/ 0.0, 0.03770, 0.0    /) ),   ! Potassium
     &   emis_table( 'Org. Carbon   ', 'APOC    ', (/ 0.0, 0.01075, 0.0    /) ),   ! Organic Carbon
     &   emis_table( 'NonCarbon Org.', 'APNCOM  ', (/ 0.0, 0.00430, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   emis_table( 'Low-Vol. POA  ', 'ALVPO1  ', (/ 0.0, 0.01501, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   emis_table( 'Low-Vol. OOA  ', 'ALVOO1  ', (/ 0.0, 2.23E-5, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   emis_table( 'Black Carbon  ', 'AEC     ', (/ 0.0, 0.0,     0.0    /) ),   ! Black or Elemental Carbon
     &   emis_table( 'Iron          ', 'AFE     ', (/ 0.0, 0.03355, 0.0    /) ),   ! Iron
     &   emis_table( 'Aluminum      ', 'AAL     ', (/ 0.0, 0.05695, 0.0    /) ),   ! Aluminum
     &   emis_table( 'Silicon       ', 'ASI     ', (/ 0.0, 0.19425, 0.0    /) ),   ! Silicon
     &   emis_table( 'Titanium      ', 'ATI     ', (/ 0.0, 0.00280, 0.0    /) ),   ! Titanium
     &   emis_table( 'Manganese     ', 'AMN     ', (/ 0.0, 0.00115, 0.0    /) ),   ! Manganese
     &   emis_table( 'Water         ', 'AH2O    ', (/ 0.0, 0.00541, 0.00637/) ),   ! Water
     &   emis_table( 'Undefined Mass', 'AOTHR   ', (/ 0.0, 0.48319, 0.0    /) ),   ! Other
     &   emis_table( 'Non-Anion Dust', 'ASOIL   ', (/ 0.0, 0.0,     0.95358/) ),   ! Non-Anion Dust
     &   emis_table( 'Air Toxics Mn ', 'AMN_HAPS', (/ 0.0, 0.00041, 0.00041/) ),   ! Air toxics Manganese J and K mode
     &   emis_table( 'Nickel        ', 'ANI     ', (/ 0.0, 2.0E-05, 2.0E-05/) ),   ! Nickel J and K mode
     &   emis_table( 'Chromium III  ', 'ACR_III ', (/ 0.0, 5.6E-05, 5.6E-05/) ),   ! Trivalent Chromium J and K mode
     &   emis_table( 'Arsenic       ', 'AAS     ', (/ 0.0, 5.5E-06, 5.5E-06/) ),   ! Arsenic J and K mode
     &   emis_table( 'Lead          ', 'APB     ', (/ 0.0, 1.6E-05, 1.6E-05/) ),   ! Lead J and K mode
     &   emis_table( 'Cadmium       ', 'ACD     ', (/ 0.0, 9.7E-08, 9.7E-08/) ),   ! Cadmium J and K mode
     &   emis_table( 'Mercury       ', 'APHG    ', (/ 0.0, 1.4E-08, 1.4E-08/) ) /) ! Mercury J and K mode

      ! Manually Enter the mode-dependent Density of the Dust Particles using the mass fractions in 
      ! the dust_spc table and user-defined densities for each component. You may reference the aerolist 
      ! for densities as well. Units are [kg/m3]. The appropriate calculation is:
      !    dust_dens = sum( frac_i ) / sum( frac_i / dens_i )
      ! Make sure to ignore any tracer species when performing this calculation.
      Real, Parameter :: dust_dens( n_mode ) = (/ 2200.0, 2156.55, 2536.92 /)

      INTEGER, ALLOCATABLE, SAVE :: MAP_DUSTtoAERO( : ) 
      Real, ALLOCATABLE, SAVE    :: DUSTOUTM( :,:,:,: ) ! Wind-Blown Dust Mass Emiss Rate [ug/m3/s]
      Real, ALLOCATABLE, SAVE    :: DUSTOUTN( :,:,: )   ! Wind-Blown Dust Number Emiss Rate [1/m3/s]
      Real, ALLOCATABLE, SAVE    :: DUSTOUTS( :,:,: )   ! Wind-Blown Dust Surface Area Emiss Rate [m2/m3/s]

C Sea-Spray Aerosol Speciation factors based on seawater composition:
! For toxic metal species, Use Table 1 in K.W. Bruland and M.C. Lohan, 6.02 - Controls of Trace Metals in Seawater, 
! In Treatise on Geochemistry, edited by Heinrich D. Holland and Karl K. Turekian, Pergamon, Oxford, 2003, Pages 23-47,
! ISBN 9780080437514, http://dx.doi.org/10.1016/B0-08-043751-6/06105-3.

C number of chemical species in seawater aerosol composition
      integer, parameter :: nsea_spc = 17

      Type( emis_table ), Parameter :: sea_spc( nsea_spc ) = (/
C                     -description--  -- Species --  -------- spcfac --------
C                                                     Aitken  accum   coarse
     &   emis_table( 'Sulfate       ', 'ASO4    ', (/ 0.0, 0.07760, 0.07760/) ),   ! Sulfate
     &   emis_table( 'Chlorine      ', 'ACL     ', (/ 0.0, 0.55380, 0.55380/) ),   ! Chlorine
     &   emis_table( 'Sodium        ', 'ANA     ', (/ 0.0, 0.30860, 0.0    /) ),   ! Sodium
     &   emis_table( 'Calcium       ', 'ACA     ', (/ 0.0, 0.01180, 0.0    /) ),   ! Calcium
     &   emis_table( 'Magnesium     ', 'AMG     ', (/ 0.0, 0.03680, 0.0    /) ),   ! Magnesium
     &   emis_table( 'Potassium     ', 'AK      ', (/ 0.0, 0.01140, 0.0    /) ),   ! Potassium
     &   emis_table( 'SeaSalt_Cation', 'ASEACAT ', (/ 0.0, 0.0    , 0.36860/) ),   ! Sea-Salt Cations
     &   emis_table( 'Chromium IV   ', 'ACR_VI  ', (/ 0.0, 5.95E-9, 5.95E-9/) ),   ! accumulation and coarse mode Hexavalent Chromium
     &   emis_table( 'Nickel        ', 'ANI     ', (/ 0.0, 1.34E-8, 1.34E-8/) ),   ! accumulation and coarse mode Nickle
     &   emis_table( 'Arsenic       ', 'AAS     ', (/ 0.0, 4.93E-8, 4.93E-8/) ),   ! accumulation and coarse mode Arsenic
     &   emis_table( 'Beryllium     ', 'ABE     ', (/ 0.0, 5.2E-11, 5.2E-11/) ),   ! accumulation and coarse mode Beryllium
     &   emis_table( 'Mercury       ', 'APHG    ', (/ 0.0, 5.8E-11, 5.8E-11/) ),   ! accumulation and coarse mode Mercury
     &   emis_table( 'Lead          ', 'APB     ', (/ 0.0, 6.0E-11, 6.0E-11/) ),   ! accumulation and coarse mode Lead
     &   emis_table( 'Cadmium       ', 'ACD     ', (/ 0.0, 1.93E-9, 1.93E-9/) ),   ! accumulation and coarse mode cadmium
     &   emis_table( 'Air Toxics Mn ', 'AMN_HAPS', (/ 0.0, 4.7E-11, 4.7E-11/) ),   ! accumulation and coarse mode Air toxics Manganese
     &   emis_table( 'bromine       ', 'ABR     ', (/ 0.0, 0.00190, 0.00190/) ),   ! Bromine 
     &   emis_table( 'Water         ', 'AH2O    ', (/ 0.0, 0.0    , 0.0    /) ) /) ! Water (uptake is calculated online)
 
      ! MAKE SURE WATER IS THE LAST COMPONENT IN THE TABLE ABOVE OR YOU
      ! WILL ENCOUNTER ISSUES WITH THE SEA SPRAY EMISSIONS MODULE.

      ! Manually Enter the mode-dependent density of the Sea Spray Particles using the mass fractions in 
      ! the sea_spc table and user-defined densities for each component. You may reference the aerolist 
      ! for densities as well. Units are [kg m-3]. The appropriate calculation is:
      !    seaspray_dens = sum( frac_i ) / sum( frac_i / dens_i )
      ! Make sure to ignore any tracer species when performing this calculation.
      Real, Parameter :: seaspray_dens( n_mode ) = (/ 2162.7, 2162.7, 2162.7 /)
      Real, Parameter :: specific_vol_h2o = 0.001  !Inverse Density of Particulate Water
 
      INTEGER, ALLOCATABLE, SAVE :: MAP_SEAtoAERO( : )  !

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

C Indices of Optional species
      Integer :: aietet_idx = 0
      Integer :: aieos_idx  = 0
      Integer :: adim_idx   = 0
      Integer :: aimga_idx  = 0
      Integer :: aimos_idx  = 0
      Integer :: aphgj_idx  = 0
      Integer :: aorgh2o_idx= 0
      Integer :: abr_idx    = 0

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
         Character( 16 ) :: num_name     ! name of aerosol number variable
         Character( 16 ) :: srf_name     ! name of aerosol surface area variable
         Real            :: min_numconc  ! minimum number concentration
         Real            :: min_m2conc   ! minimum 2nd moment concentration
         Real            :: min_m3conc   ! minimum 3rd moment concentration
      End Type mode_type

      Type ( mode_type ), Parameter  :: aeromode( n_mode ) = (/
C                   number     surface   minimum minimum minimum
C                    name       name     numconc  m2conc  m3conc
C                  ----------  -------  -------- -------  ------
     &   mode_type('NUMATKN', 'SRFATKN', conmin,  conmin, conmin),
     &   mode_type('NUMACC ', 'SRFACC ', conmin,  conmin, conmin),
     &   mode_type('NUMCOR ', 'SRFCOR ', conmin,  conmin, conmin)/)


      Real          :: moment0_conc( n_mode )     ! 0th moment concentration
      Real          :: moment2_conc( n_mode )     ! 2nd moment concentration
      Real          :: moment3_conc( n_mode )     ! 3rd moment concentration
      Logical, save :: wet_moments_flag           ! T if M2 and M3 are wet, F otherwise

C Mass concentration (calculated by GETPAR)
      Real :: aeromode_mass( n_mode )   ! [ ug/m^3 ]

C Particle density (calculated by GETPAR)
      Real :: aeromode_dens( n_mode )   ! [ kg/m^3 ]

C Geometric mean diameter (calculated by GETPAR)
      Real :: aeromode_diam( n_mode )   ! [ m ]

C Log of geometric standard deviation (calculated by GETPAR )
      Real :: aeromode_lnsg( n_mode )

C Minimum number (calculated in map_aero routine)
      Real :: aeromode_minNum( n_mode )

C Minimum 2nd moment (calculated in map_aero routine)
      Real :: aeromode_minM2( n_mode )

C Mapping for loading from and unloading to CGRID array
      Integer, Allocatable :: aerospc_map( :,: )     ! indices of aero species to CGRID
      Integer              :: aeronum_map( n_mode )  ! indices of aero number variable to CGRID
      Integer              :: aerosrf_map( n_mode )  ! indices of aero surf area variable to CGRID

C Missing aerosol species map
      Logical, Allocatable :: aero_missing( :,: )  

      Logical, Save :: V51_SOA_mechanism   = .False. !
      Logical, Save :: AE_eflag            = .False. ! error flag for AERO_DATA
C IC/BC Correction Mapping
      !The following vectors store masks of the aerosol species
      !identities. For example, LBCNUM stores 1's for every species that
      !represents an aerosol number concentration and 0's otherwise.
      !   LBCNUM - Number Concentration
      !   LBCSURF - Surface Area Concentration
      !   LBCMASS - Mass Concentration
      !   LBCMODE - Mask for each aerosol mode
      LOGICAL, ALLOCATABLE, SAVE :: LBCNUM(:), LBCSRF(:), LBCMASS(:),
     &                        LBCMODE(:,:), LBCDRY(:)
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
      REAL, ALLOCATABLE :: COND_BUDGET( : )
      REAL, ALLOCATABLE :: COAG_BUDGET( :,: )
      REAL, ALLOCATABLE :: NPF_BUDGET( : )
      REAL, ALLOCATABLE :: GROWTH_BUDGET( : )

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
      Use utilio_defn
      Use vdiff_data, only : n_spc_diff, diff_spc
      Implicit None

C Local Variables:
      Character( 256 ) :: xmsg
      Character( 256 ) :: xmsg2
      Real    :: mole_weight( n_aerolist )
      Integer :: map_listtogrid( n_aerolist,n_mode )
      Integer :: map_aerotolist( n_aerolist )

      Integer, Allocatable :: aerospc_in_dustlist( : )

      Integer l, m, n, spc, isea, idust
      Real    so4fac
      Real    anthfac

      Real    POA_OC    ! ratio of Elemental Oxygen to Carbon in POA
      Real    OOA_OC    ! ratio of Elemental Oxygen to Carbon in Oxygenated OA

      Integer dust_org1, dust_org2
      Logical ae6isoa
      Logical ae6hg
      Logical dust_match

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
      

      allocate( aerocgrid_rhoinv( n_ae_trns ), lbcnum( n_ae_trns ),
     &          lbcsrf( n_ae_trns ), lbcmass( n_ae_trns ),
     &          lbcmode( n_mode,n_ae_trns ), lbcdry( n_ae_trns ) )
      aerocgrid_rhoinv = 0.
      lbcnum = .False.
      lbcsrf = .False.
      lbcmass= .False.
      lbcmode= .False.
      lbcdry = .False.

C Build mapping to CGRID for each species in the master AeroList
      Do spc = 1, n_aerolist
         ! Check to see if this species is in the namelist
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

            If ( n .Ne. 0 ) Then
               ! Species is on the NameList
               If ( .Not. ( Any( map_listtogrid( spc,: ) .NE. 0 ) ) ) Then
                  ! Add this species to the map if it does not exist already
                  n_aerospc = n_aerospc + 1
                  map_aerotolist( n_aerospc ) = spc
                  ! Set the Molecular Weight
                  mole_weight( spc ) = ae_molwt( n )
               Else If ( mole_weight( spc ) .Ne. ae_molwt( n ) ) Then
                  ! If the species already exists and the
                  ! molecular weight from this mode on the
                  ! namelist is not matching the standing
                  ! molecular weight, throw an error
                  xmsg = 'The molecular weight of ' // Trim( sn )
     &                   // ' is different from that of the same species'
     &                   // ' in the same or another mode.'
                  Call m3warn( pname, 0, 0, xmsg )
                  Write( xmsg,* ) 'New Value(', n, ') = ', ae_molwt( n ),
     &                            'Expected value(', spc, ')= ', mole_weight( spc )
                  Call m3warn( pname, 0, 0, xmsg )
                  AE_eflag  = .True.
               End If
               ! Update the map from CGRID to AeroList for this mode
               map_listtogrid( spc,m ) = ae_strt - 1 + n
               lbcmode( m,n ) = .True.
               If ( .Not. aerolist( spc)%tracer )  lbcmass( n ) = .True.
               If ( .Not. aerolist( spc)%no_m2wet ) lbcdry( n ) = .True.
               !Map the CGRID Aerosol Species to their Densities
               aerocgrid_rhoinv( n ) = 1.0 / aerolist( spc )%density

            End If
         End Do
      End Do

      ! Migrate all of the user-requested aerosols from AeroList to
      ! AeroSpc. Begin by allocating all of the new arrays now that we
      ! know the value of n_aerospc
      Allocate ( aerospc       ( n_aerospc ) )
      Allocate ( aerospc_mw    ( n_aerospc ) )
      Allocate ( aerospc_mwinv ( n_aerospc ) )
      Allocate ( aerospc_map   ( n_aerospc, n_mode ) )
      Allocate ( aerospc_conc  ( n_aerospc, n_mode ) )
      Allocate ( aero_missing  ( n_aerospc, n_mode ) )

      aero_missing( :,: ) = .True.

      Do spc = 1, n_aerospc ! Loop through user-requested chemical species
         l = map_aerotolist( spc )

         ! Map AeroList Contents to AeroSpc
         aerospc( spc )%bulkname   = aerolist( l )%bulkname   
         aerospc( spc )%min_conc   = aerolist( l )%min_conc
         aerospc( spc )%density    = aerolist( l )%density   
         aerospc( spc )%no_M2wet   = aerolist( l )%no_M2Wet   
         aerospc( spc )%nonVol_soa = aerolist( l )%nonVol_soa 
         aerospc( spc )%tracer     = aerolist( l )%tracer     
         aerospc( spc )%charge     = aerolist( l )%charge     
         aerospc( spc )%visual_idx = aerolist( l )%visual_idx 
         aerospc( spc )%visual_idx_large = aerolist( l )%visual_idx_large
         aerospc( spc )%om         = aerolist( l )%om         
         aerospc( spc )%optic_surr = aerolist( l )%optic_surr 
         aerospc( spc )%name( : )  = ''

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

C Build mapping to CGRID for aero # and surf area variables
      aeronum_map = 0
      aerosrf_map = 0

      Do m = 1, n_mode
         n = index1( aeromode( m )%num_name , n_ae_spc, ae_spc )
         If ( n .Eq. 0 ) Then
            xmsg = 'Species ' // Trim( aeromode( m )%num_name )
     &           //' is not in AE namelist'
            AE_eflag  = .True.
            Call m3warn( pname, 0, 0, xmsg )
!            Call m3exit( pname, 0, 0, xmsg, xstat3 )
         Else
            aeronum_map( m ) = ae_strt - 1 + n
            aerocgrid_rhoinv( n ) = 1.0
            lbcnum( n ) = .True.
            lbcmode( m,n ) = .True.
         End If

         n = index1( aeromode( m )%srf_name , n_ae_spc, ae_spc )
         If ( n .Eq. 0 ) Then
            xmsg = 'species ' // Trim( aeromode( m )%srf_name )
     &           // ' is not in AE namelist'
            AE_eflag  = .True.
            Call m3warn( pname, 0, 0, xmsg )
!            Call m3exit( pname, 0, 0, xmsg, xstat3 )
         Else
            aerosrf_map( m ) = ae_strt - 1 + n
            aerocgrid_rhoinv( n ) = 1.0
            lbcsrf( n ) = .True.
            lbcmode( m,n ) = .True.
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

      If ( Index( mechname, 'NVPOA' ) .Eq. 0 ) Then
         apoc_idx    = findAero( 'ALVPO1          ', .True. )
         apncom_idx  = 0
      Else
         apoc_idx    = findAero( req_poc,  .True. )
         apncom_idx  = findAero( req_ncom, .True. )
      End If

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

      If ( Index( mechname, 'CB6R3M_AE7_KMTBR' ) .Gt. 0 ) Then
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

C Compute common factors
      h2ofac = 1.0D-9 * f6dpi / Real( aerospc( ah2o_idx )%density, 8 )

C compute aeromode_minNum and aeromode_minM2
      so4fac  = 1.0E-9 * Real( f6dpi, 4 ) / aerospc( aso4_idx )%density
      anthfac = 1.0E-9 * Real( f6dpi, 4 ) / aerospc( acors_idx )%density

      Do m = 1, n_mode
         If ( m .Lt. n_mode ) Then
            aeromode_minNum( m ) = so4fac * aerospc( aso4_idx )%min_conc( m ) /
     &           ( def_diam( m )**3 * Exp( 4.5 * Log( def_sigma_g( m ) )**2 ) )
         Else
            aeromode_minNum( m ) = anthfac * aerospc( acors_idx )%min_conc( m ) /
     &           ( def_diam( m )**3 * Exp( 4.5 * Log( def_sigma_g( m ) )**2 ) )
         End If
         aeromode_minM2( m ) = aeromode_minNum( m ) *
     &             def_diam( m )**2 * Exp( 2.0 * Log( def_sigma_g( m ) )**2 )
      End do

      if( AE_eflag )Then
         Write(logdev,99901) Trim( mechname )
             xmsg = 'The FATAL errors found in namelist used. Check '
     &         //  'the log of exiting processor if more details are needed.'
         Call m3exit( pname, 0, 0, xmsg, xstat3 )
      End If 

      mapped = .True.


99901 Format( 'FATAL Error(s) found in the AE namelist used. Check that ' 
     &     /  'this AE namelist contains the above required data as the file in '
     &     /  'the respository version of the mechanism: ', a )
      Return
      End Subroutine map_aero

C-----------------------------------------------------------------------
      Subroutine extract_aero( conc, minchk )

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

C Local Variables:
      Integer m, n, spc

      If ( .Not. mapped ) Then
         Call map_aero()
      End If

C Copy grid cell concentrations of aero species to aerospc_conc
      aerospc_conc = 0.0
      If ( minchk ) Then
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                  aerospc_conc( spc,m ) = Max( conc( n ), aerospc( spc )%min_conc( m ) ) ! [ug/m^3]
               End If
            End Do
         End Do
      Else
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                  aerospc_conc( spc,m ) = conc( n )   ! [ug/m^3]
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
      Subroutine update_aero( conc, minchk )

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
      Use utilio_defn

      Implicit None

C Arguments:
      Real, Intent( Out ) :: conc( : )
      Logical, Intent( In ) :: minchk

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
               End If
            End Do
         End Do
      Else
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                    conc( n ) = aerospc_conc( spc,m )
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

      Use utilio_defn

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
      Do n = 1, n_mode
         Do spc = 1, n_aerospc
            If ( Index( aerospc( spc )%name( n ), trim(vname) ) .Gt. 0 ) Then
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

      xmsg = 'Required Species ' // Trim( vname ) // ' Not found in AE namelist'
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

            initialM3 = moment3_conc( n )
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

            initialM3 = moment3_conc( n )
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
      Subroutine CHECK_AERO_ICBC( IBCON, AE_MASS_UNIT, STAT, AER_PAR, LMODE )

C Subroutine corrects the scale factors for number, surface area when a 
C change has happened to the mass of any boundary condition species.
C Eventually this code should perform a similar operation for the
C initial conditions, but this is somewhat complicated and needs to be
C thought through more.
C
C Inputs:  IBCON - the actual initial or boundary conditions from the
C                  calling routine
C          ICBC_FAC - The scale factor used to modify IBCON right before
C                     this routine is called
C
C 11 May 16  B.Murphy  Program Written
C
C-----------------------------------------------------------------------
      Use AEROMET_DATA, only : pi, f6pi

      Implicit None

      REAL, INTENT(INOUT)        :: IBCON( : )
      LOGICAL, INTENT( IN ) :: AE_MASS_UNIT  ! FALSE for KGM3 input
                                             ! TRUE  for UGM3 input
      INTEGER, INTENT(OUT)       :: STAT     ! 0 - Distribution is ok
                                             ! 1 - Distribution is problematic
      REAL, INTENT(OUT) :: AER_PAR( 2, N_MODE, 5 ) !Track the modal parameters   (N, M2, M3, dg, sg) -Before
                                                   !before and after the BC      (N, M2, M3, dg, sg) -After
                                                   !check routine               
      INTEGER, INTENT(OUT) :: LMODE  !Identifies the problematic mode

      INTEGER          :: LSTAT

      REAL, PARAMETER  :: F1PI = 1.0 / pi
      REAL, PARAMETER  :: ONE_THIRD  = 1.0 / 3.0
      REAL, PARAMETER  :: TWO_THIRDS = 2.0 / 3.0


C Local variables:

      INTEGER   :: IMODE
      REAL      :: NUM, M2, M3, l2sg, dg, sg
      REAL, Parameter :: KGPMG = 1.0E-9 !Kilogram per microgram m-3

      LMODE   = 0
      STAT    = 0
      AER_PAR = 0.0
      dg      = 0.0
      sg      = 0.0
       
      CALL MAP_AERO()

      !Loop Through Each Aerosol Mode. Sum up the third moment, then
      !calculate the Dg and Sg of the mode and check to make sure they
      !are valid
      DO IMODE = 1,N_MODE
         LSTAT = 0

         NUM = SUM( IBCON( : ), MASK = ( LBCMODE( IMODE,: ) .AND. LBCNUM ) )
         M2  = SUM( IBCON( : ), MASK = ( LBCMODE( IMODE,: ) .AND. LBCSRF ) ) * F1PI

         M3 = SUM( IBCON( : ) * AEROCGRID_RHOINV * F6PI, 
     &             MASK = ( LBCMASS .AND. LBCMODE( IMODE,: ) .AND. LBCDRY( : ) )  )

         IF ( AE_MASS_UNIT ) M3 = M3 * KGPMG

         AER_PAR ( 1, IMODE, 1 ) = NUM
         AER_PAR ( 1, IMODE, 2 ) = M2
         AER_PAR ( 1, IMODE, 3 ) = M3
         AER_PAR ( 1, IMODE, 4 ) = 0.    
         AER_PAR ( 1, IMODE, 5 ) = 0.    

         IF ( M3 .LT. 1.0e-30 ) THEN
             LSTAT = 4
             LMODE = IMODE
         ELSE IF ( NUM .LT. 1.0e-30 .OR. M2 .LT. 1.0e-30 ) THEN
             LSTAT = 3
             LMODE = IMODE
         ELSE                  
             l2sg = ( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3 ) - LOG( M2 ))

             IF ( l2sg .LT. MIN_L2SG .OR. l2sg .GT. MAX_L2SG ) THEN 
                  LSTAT = 2
                  LMODE = IMODE
             ELSE

                  dg   = ( M3 / NUM * EXP( -4.5 * l2sg ) )  ** ( ONE_THIRD )
                  sg   = EXP( SQRT( l2sg ) )
                  AER_PAR( 1, IMODE, 4 ) = dg
                  AER_PAR( 1, IMODE, 5 ) = sg

                  IF ( dg .LT. MIN_DIAM_G( IMODE )  .OR. dg .GT. MAX_DIAM_G( IMODE ) ) THEN
                      LSTAT = 1
                      LMODE = IMODE
                  ENDIF
              ENDIF     
         ENDIF

         !Save Modal Properties After the Check
         AER_PAR( 2, IMODE, 1 ) = NUM
         AER_PAR( 2, IMODE, 2 ) = M2
         AER_PAR( 2, IMODE, 3 ) = M3
         AER_PAR( 2, IMODE, 4 ) = dg
         AER_PAR( 2, IMODE, 5 ) = sg

         IF ( LSTAT .GT. 0 .AND. LSTAT .LT. 4 ) THEN
           STAT = LSTAT

           !Rewrite Boundary Condition Number and Surface Area to Satisfy
           !Default Size Parameters
           l2sg = DEF_L2SG( IMODE )
           NUM = M3 * ( EXP( -4.5 * l2sg ) ) / ( DEF_DIAM( IMODE ) ) ** 3
           dg   = ( M3 / NUM * EXP( -4.5 * l2sg ) ) ** ( ONE_THIRD )
           sg   = EXP( SQRT( l2sg ) )
           
           !First Number Concentration
           WHERE( LBCMODE( IMODE,: ) .AND. LBCNUM ) IBCON = NUM
           
           !Then Surface Area Concentration
           M2 = EXP( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3 ) - l2sg )
           WHERE( LBCMODE( IMODE,: ) .AND. LBCSRF ) IBCON = M2 * F1PI
         
           !Save Modal Properties After the Check
           AER_PAR( 2, IMODE, 1 ) = NUM
           AER_PAR( 2, IMODE, 2 ) = M2
           AER_PAR( 2, IMODE, 3 ) = M3
           AER_PAR( 2, IMODE, 4 ) = dg
           AER_PAR( 2, IMODE, 5 ) = sg

         ELSEIF ( LSTAT .EQ. 4 ) THEN
           STAT = LSTAT

           WHERE( LBCMODE( IMODE,: ) .AND. LBCNUM ) IBCON = conmin  !Number
           WHERE( LBCMODE( IMODE,: ) .AND. LBCSRF ) IBCON = conmin  !Second Moment
           WHERE( LBCMODE( IMODE,: ) .AND. LBCMASS) IBCON = conmin  !Third Moment
           !Save Modal Properties After the Check
           AER_PAR( 2, IMODE, 1 ) = conmin
           AER_PAR( 2, IMODE, 2 ) = conmin
           AER_PAR( 2, IMODE, 3 ) = conmin
           AER_PAR( 2, IMODE, 4 ) = 0.
           AER_PAR( 2, IMODE, 5 ) = 0.
         ENDIF

      ENDDO

      End Subroutine CHECK_AERO_ICBC
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      End Module aero_data
