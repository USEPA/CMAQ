
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero6/AERO_DATA.F,v 1.10 2012/01/19 13:18:37 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module aero_data

C  Defines aerosol species arrays and the parameters required in aerosol
C  processing.

C  Contains:
C     Subroutine map_aero
C     Subroutine map_pmemis
C     Subroutine extract_aero
C     Subroutine update_aero
C     Function findAero

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.

C HS = Heather Simon; GS = Golam Sarwar; SH = Steve Howard; SR = Shawn Roselle;
C JY = Jeff Young; HOTP = Havala Pye; KF = Kathleen Fahey, CGN = Chris Nolte;
C PL = Peng Liu;

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
C             are splitted into small and large modes.
C             visual_idx for ammominm is zero, because it will be treated together with sulfate and nitrate
C             see Pitchford et al., Journal of the Air & Waste Management Association (57)(2007), pp 1326
C PL 05/15/16 Add "om" in spcs_type, to flag organic aerosols, which
C             will be used to calculate total mass of organic aerosls for estimating
C             aerosol extinction efficiency

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

      Implicit None

C Number of aerosol species and modes

      Integer, Parameter :: n_aerolist = 89     ! number of aero species
      Integer, Parameter :: n_mode = 3          ! number of modes:
                                                ! 1 = Aitken
                                                ! 2 = accumulation
                                                ! 3 = coarse

      Integer, Save :: n_aerospc ! number of aero species

C Default minimum concentration
      Real,    Parameter :: conmin  = 1.0E-30    ! [ ug/m^3 ]
      Real(8), Parameter :: conminD = 1.0D-30    ! [ ug/m^3 ]
      Real,    Parameter :: evapmin = 1.0E-20    ! [ ug/m^3 ]
      Real(8), Parameter :: evapminD= 1.0D-20    ! [ ug/m^3 ]
      Real,    Parameter :: cm_set( n_mode ) = (/conmin, conmin, conmin/)
      Real,    Parameter :: cm_so4( n_mode ) = (/1.0E-12,1.0E-6, conmin/)
      Real,    Parameter :: cm_cor( n_mode ) = (/conmin, conmin, 1.889544E-05/)

C Emissions splits
      Real,    Parameter :: es_fin( n_mode ) = (/ 0.100, 0.900, 0.000 /)
      Real,    Parameter :: es_acc( n_mode ) = (/ 0.000, 1.000, 0.000 /)
      Real,    Parameter :: es_cor( n_mode ) = (/ 0.000, 0.000, 1.000 /)
      Real,    Parameter :: es_0  ( n_mode ) = (/ 0.000, 0.000, 0.000 /)

C Geometric mean diameter by volume (or mass) of emitted particles in
C each mode [ m ].  See paragraph #14 of Binkowski & Roselle (2003)
C 09/17/14 change by Kathleen Fahey - see Revision History, above
      Real,    Parameter :: dgvem( n_mode ) = (/ 0.06E-6, 0.28E-6, 6.0E-6 /)
      Real,    Parameter :: def_diam( n_mode )   = (/ 15.0E-9, 80.0E-9,  600.0E-9 /)  ! default background mean diameter for each mode
      Real,    Parameter :: min_diam_g( n_mode ) = (/ 1.0E-9,  30.0E-9,  120.0E-9 /)
      Real,    Parameter :: max_diam_g( n_mode ) = (/ 30.0E-9, 120.0E-9, 100.0E-6 /)

C Geometric standard deviation of emitted particles in each mode, as
C described in paragraph #14 of Binkowski & Roselle (2003)
C 09/17/14 change by Kathleen Fahey - see Revision History, above
      Real,    Parameter :: sgem( n_mode )        = (/ 1.7, 1.7, 2.2 /)
      Real,    Parameter :: def_sigma_g( n_mode ) = (/ 1.70, 2.0, 2.2 /)       ! default background sigma-g for each mode
      Real,    Parameter :: min_sigma_g = 1.05
      Real,    Parameter :: max_sigma_g = 2.5001
      Real,    Save      :: def_l2sg( n_mode ), max_l2sg, min_l2sg   

C Flag to obtain coagulation coefficients
C by analytical approximation (True) or by Gauss-Hermite quadrature (False)
      Logical, Parameter :: fastcoag_flag = .True.

C Define Logical values as T and F for the aerospc table
      Logical, Parameter, Private :: T = .true.
      Logical, Parameter, Private :: F = .false.

      Integer, Private, Save :: logdev
      Integer, Private, External :: setup_logdev

C-------------------------------------------------------------------------------------------------------

      Type spcs_type
         Character( 16 ) :: name( n_mode )       ! names of aerosol species for each mode
         Real            :: min_conc( n_mode )   ! minimum concentration values for each mode
         Real            :: density              ! density [ kg/m^3 ]
         Logical         :: no_M2Wet             ! flag to exclude from 2nd moment during transport
         Logical         :: nonVol_soa           ! non-volatile SOA flag
         Logical         :: tracer               ! tracer flag; does have not mass
         Integer         :: charge               ! electroneutrality charge
         Real            :: visual_idx           ! visual index factor [ m^2/g ]
         Real            :: visual_idx_large     ! visual index factor [m^2/g ] for large mode, if not applicable, same value as visual_idx
         Logical         :: om                   ! flag for organic aerosols
         Character( 16 ) :: optic_surr           ! optical surrogate name
         Character( 16 ) :: emis                 ! file emissions names
         Real            :: emis_split( n_mode ) ! minimum concentration values for each mode
      End Type spcs_type

      ! Create Array of Aerosol Species to Be Filled with User Preferences
      Type( spcs_type ), Allocatable, Save :: aerospc ( : )

      ! Master List of Aerosol Species and Properties
      Type( spcs_type ), Parameter :: aerolist( n_aerolist ) = (/
C                                                                       nonVolSOA       Visidx_Large
C                                                                   no_M2Wet|Tracer         |
C                  -------------Name--------------                        | | | Charge      | OM                  Emissions
C                   Aitken      Accum       Coarse      Min_Concs Density | | |   | Visidx  |  |  OptSurr       Emis       Splits
C                  ---------   ---------   ---------    --------- ------- + + +   + ------ --- +  -------- --------------- ------
     & spcs_type((/'ASO4I    ','ASO4J    ','ASO4K    '/), cm_so4, 1800.0, F,F,F, -2,  2.2, 4.8,F, 'SOLUTE','PSO4         ',es_fin),
     & spcs_type((/'ANO3I    ','ANO3J    ','ANO3K    '/), cm_set, 1800.0, F,F,F, -1,  2.4, 5.1,F, 'SOLUTE','PNO3         ',es_fin),
     & spcs_type((/'ACLI     ','ACLJ     ','ACLK     '/), cm_set, 2200.0, F,F,F, -1,  1.7, 1.7,F, 'SOLUTE','PCL          ',es_fin),
     & spcs_type((/'ANH4I    ','ANH4J    ','ANH4K    '/), cm_set, 1800.0, F,F,F,  1,  0.0, 0.0,F, 'SOLUTE','PNH4         ',es_fin),
     & spcs_type((/'ANAI     ','ANAJ     ','         '/), cm_set, 2200.0, F,F,F,  1,  1.7, 1.7,F, 'SOLUTE','PNA          ',es_fin),
     & spcs_type((/'         ','AMGJ     ','         '/), cm_set, 2200.0, F,F,F,  2,  1.0, 1.0,F, 'DUST  ','PMG          ',es_acc),
     & spcs_type((/'         ','AKJ      ','         '/), cm_set, 2200.0, F,F,F,  1,  1.0, 1.0,F, 'DUST  ','PK           ',es_acc),
     & spcs_type((/'         ','ACAJ     ','         '/), cm_set, 2200.0, F,F,F,  2,  1.0, 1.0,F, 'DUST  ','PCA          ',es_acc),
     & spcs_type((/'APOCI    ','APOCJ    ','         '/), cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ','POC          ',es_fin),
     & spcs_type((/'APNCOMI  ','APNCOMJ  ','         '/), cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ','PNCOM        ',es_fin),
     & spcs_type((/'AECI     ','AECJ     ','         '/), cm_set, 2200.0, F,F,F,  0, 10.0,10.0,F, 'SOOT  ','PEC          ',es_fin),
     & spcs_type((/'         ','AFEJ     ','         '/), cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ','PFE          ',es_acc),
     & spcs_type((/'         ','AALJ     ','         '/), cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ','PAL          ',es_acc),
     & spcs_type((/'         ','ASIJ     ','         '/), cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ','PSI          ',es_acc),
     & spcs_type((/'         ','ATIJ     ','         '/), cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ','PTI          ',es_acc),
     & spcs_type((/'         ','AMNJ     ','         '/), cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ','PMN          ',es_acc),
     & spcs_type((/'AH2OI    ','AH2OJ    ','AH2OK    '/), cm_set, 1000.0, T,F,F,  0,  0.0, 0.0,F, 'WATER ','PH2O         ',es_fin),
     & spcs_type((/'AH3OPI   ','AH3OPJ   ','AH3OPK   '/), cm_set, 1000.0, T,F,T,  0,  0.0, 0.0,F, 'WATER ','             ',es_0),
     & spcs_type((/'AOTHRI   ','AOTHRJ   ','         '/), cm_set, 2200.0, F,F,F,  0,  1.0, 1.0,F, 'DUST  ','PMOTHR       ',es_fin),
     & spcs_type((/'         ','AALK1J   ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AALK2J   ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AXYL1J   ','         '/), cm_set, 1480.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AXYL2J   ','         '/), cm_set, 1480.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AXYL3J   ','         '/), cm_set, 1330.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ATOL1J   ','         '/), cm_set, 1240.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ATOL2J   ','         '/), cm_set, 1240.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ATOL3J   ','         '/), cm_set, 1450.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ABNZ1J   ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ABNZ2J   ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ABNZ3J   ','         '/), cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ATRP1J   ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ATRP2J   ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AISO1J   ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AISO2J   ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AISO3J   ','         '/), cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ASQTJ    ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','APAH1J   ','         '/), cm_set, 1480.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','APAH2J   ','         '/), cm_set, 1480.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','APAH3J   ','         '/), cm_set, 1550.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AOLGAJ   ','         '/), cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AOLGBJ   ','         '/), cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AORGCJ   ','         '/), cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','         ','ASOIL    '/), cm_set, 2600.0, F,F,F,  0,  0.6, 0.6,F, 'DUST  ','             ',es_cor),
     & spcs_type((/'         ','         ','ACORS    '/), cm_cor, 2200.0, F,F,F,  0,  0.6, 0.6,F, 'DUST  ','PMC          ',es_cor),
     & spcs_type((/'         ','         ','ASEACAT  '/), cm_cor, 2200.0, F,F,F,  1,  1.7, 1.7,F, 'SOLUTE','             ',es_cor),

       ! Associated with ae6i
     & spcs_type((/'         ','AMTNO3J  ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AISOPNNJ ','         '/), cm_set, 1400.0, T,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AMTHYDJ  ','         '/), cm_set, 1400.0, F,T,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AIETETJ  ','         '/), cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AIEOSJ   ','         '/), cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ADIMJ    ','         '/), cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AIMGAJ   ','         '/), cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','AIMOSJ   ','         '/), cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),

       ! ae6i and cb6
     & spcs_type((/'         ','AGLYJ    ','         '/), cm_set, 1400.0, F,F,F,  0,  2.8, 6.1,T, 'DUST  ','             ',es_0),
 
       ! Semivolatile POA
     & spcs_type((/'ALVPO1I  ','ALVPO1J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','POC          ',es_fin),
     & spcs_type((/'ASVPO1I  ','ASVPO1J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','POC          ',es_fin),
     & spcs_type((/'ASVPO2I  ','ASVPO2J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','POC          ',es_fin),
     & spcs_type((/'         ','ASVPO3J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','POC          ',es_acc),
     & spcs_type((/'         ','AIVPO1J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','POC          ',es_acc),
     & spcs_type((/'ALVOO1I  ','ALVOO1J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'ALVOO2I  ','ALVOO2J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'ASVOO1I  ','ASVOO1J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'ASVOO2I  ','ASVOO2J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','             ',es_0),
     & spcs_type((/'         ','ASVOO3J  ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','             ',es_0), ! pcSOA
     & spcs_type((/'         ','APCSOJ   ','         '/), cm_set, 1400.0, T,F,F,  0,  4.0, 6.1,T, 'DUST  ','             ',es_0),

       ! The following species are associated with the Multi-Pollutant code
     & spcs_type((/'ANII     ','ANIJ     ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','NICKEL_F     ',es_fin),
     & spcs_type((/'         ','         ','ANIK     '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','NICKEL_C     ',es_cor),
     & spcs_type((/'ACR_VII  ','ACR_VIJ  ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','CHROMHEX_F   ',es_fin),
     & spcs_type((/'         ','         ','ACR_VIK  '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','CHROMHEX_C   ',es_cor),
     & spcs_type((/'ACR_IIII ','ACR_IIIJ ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','CHROMTRI_F   ',es_fin),
     & spcs_type((/'         ','         ','ACR_IIIK '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','CHROMTRI_C   ',es_cor),
     & spcs_type((/'ABEI     ','ABEJ     ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','BERYLLIUM_F  ',es_fin),
     & spcs_type((/'         ','         ','ABEK     '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','BERYLLIUM_C  ',es_cor),
     & spcs_type((/'APBI     ','APBJ     ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','LEAD_F       ',es_fin),
     & spcs_type((/'         ','         ','APBK     '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','LEAD_C       ',es_cor),
     & spcs_type((/'ADE_OTHRI','ADE_OTHRJ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','DIESEL_PMFINE',es_fin),
     & spcs_type((/'ADE_ECI  ','ADE_ECJ  ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','DIESEL_PMEC  ',es_fin),
     & spcs_type((/'ADE_OCI  ','ADE_OCJ  ','         '/), cm_set, 2000.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','DIESEL_PMOC  ',es_fin),
     & spcs_type((/'         ','ADE_NO3J ','         '/), cm_set, 1800.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','DIESEL_PMNO3 ',es_acc),
     & spcs_type((/'         ','ADE_SO4J ','         '/), cm_set, 1800.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','DIESEL_PMSO4 ',es_acc),
     & spcs_type((/'         ','         ','ADE_K    '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','DIESEL_PMC   ',es_cor),
     & spcs_type((/'ACDI     ','ACDJ     ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','CADMIUM_F    ',es_fin),
     & spcs_type((/'         ','         ','ACDK     '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','CADMIUM_C    ',es_cor),
     & spcs_type((/'AMN_HAPSI','AMN_HAPSJ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','MANGANESE_F  ',es_fin),
     & spcs_type((/'         ','         ','AMN_HAPSK'/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','MANGANESE_C  ',es_cor),
     & spcs_type((/'APHGI    ','APHGJ    ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','PHGI         ',es_fin),
     & spcs_type((/'         ','         ','APHGK    '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','             ',es_cor),
     & spcs_type((/'AASI     ','AASJ     ','         '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','ARSENIC_F    ',es_fin),
     & spcs_type((/'         ','         ','AASK     '/), cm_set, 2200.0, F,F,T,  0,  0.0, 0.0,F, 'DUST  ','ARSENIC_C    ',es_cor)
     & /)
 
C Primary Organic Aerosol Volatility Distributions
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
 
C number of lognormal modes in windblown dust aerosol = n_mode
C - but only accumulation and coarse modes used
      Type emis_table
         Character( 16 ) :: description
         Character( 16 ) :: name  ( n_mode )
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
      Integer, Parameter :: ndust_list = 26
      Integer            :: ndust_spc

       
      Type( emis_table ) :: dust_spc_list( ndust_list ) = (/
C                    --description--           -------- name --------             -------- spcfac --------
C                                           Aitken     accum       coarse          Aitken  accum   coarse
     &   emis_table( 'Sulfate       ', (/'         ','ASO4J    ','ASO4K    '/), (/ 0.0, 0.02250, 0.02655/) ),   ! Sulfate
     &   emis_table( 'Nitrate       ', (/'         ','ANO3J    ','ANO3K    '/), (/ 0.0, 0.00020, 0.00160/) ),   ! Nitrate
     &   emis_table( 'Chlorine      ', (/'         ','ACLJ     ','ACLK     '/), (/ 0.0, 0.00945, 0.01190/) ),   ! Chlorine
     &   emis_table( 'Ammonium      ', (/'         ','ANH4J    ','         '/), (/ 0.0, 0.00005, 0.0    /) ),   ! Ammonium
     &   emis_table( 'Sodium        ', (/'         ','ANAJ     ','         '/), (/ 0.0, 0.03935, 0.0    /) ),   ! Sodium
     &   emis_table( 'Calcium       ', (/'         ','ACAJ     ','         '/), (/ 0.0, 0.07940, 0.0    /) ),   ! Calcium
     &   emis_table( 'Magnesium     ', (/'         ','AMGJ     ','         '/), (/ 0.0, 0.01900, 0.0    /) ),   ! Magnesium
     &   emis_table( 'Potassium     ', (/'         ','AKJ      ','         '/), (/ 0.0, 0.03770, 0.0    /) ),   ! Potassium
     &   emis_table( 'Org. Carbon   ', (/'         ','APOCJ    ','         '/), (/ 0.0, 0.01075, 0.0    /) ),   ! Organic Carbon
     &   emis_table( 'NonCarbon Org.', (/'         ','APNCOMJ  ','         '/), (/ 0.0, 0.00430, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   emis_table( 'Black Carbon  ', (/'         ','AECJ     ','         '/), (/ 0.0, 0.0,     0.0    /) ),   ! Black or Elemental Carbon
     &   emis_table( 'Iron          ', (/'         ','AFEJ     ','         '/), (/ 0.0, 0.03355, 0.0    /) ),   ! Iron
     &   emis_table( 'Aluminum      ', (/'         ','AALJ     ','         '/), (/ 0.0, 0.05695, 0.0    /) ),   ! Aluminum
     &   emis_table( 'Silcon        ', (/'         ','ASIJ     ','         '/), (/ 0.0, 0.19425, 0.0    /) ),   ! Silicon
     &   emis_table( 'Titanum       ', (/'         ','ATIJ     ','         '/), (/ 0.0, 0.00280, 0.0    /) ),   ! Titanium
     &   emis_table( 'Manganese     ', (/'         ','AMNJ     ','         '/), (/ 0.0, 0.00115, 0.0    /) ),   ! Manganese
     &   emis_table( 'Water         ', (/'         ','AH2OJ    ','AH2OK    '/), (/ 0.0, 0.00541, 0.00637/) ),   ! Water
     &   emis_table( 'Undefined Mass', (/'         ','AOTHRJ   ','         '/), (/ 0.0, 0.48319, 0.0    /) ),   ! Other
     &   emis_table( 'Non-Anion Dust', (/'         ','         ','ASOIL    '/), (/ 0.0, 0.0,     0.95358/) ),   ! Non-Anion Dust
     &   emis_table( 'Air Toxics Mn ', (/'         ','AMN_HAPSJ','AMN_HAPSK'/), (/ 0.0, 0.00041, 0.00041/) ),   ! Air toxics Manganese J and K mode
     &   emis_table( 'Nickel        ', (/'         ','ANIJ     ','ANIK     '/), (/ 0.0, 2.0E-05, 2.0E-05/) ),   ! Nickel J and K mode
     &   emis_table( 'Chromium III  ', (/'         ','ACR_IIIJ ','ACR_IIIK '/), (/ 0.0, 5.6E-05, 5.6E-05/) ),   ! Trivalent Chromium J and K mode
     &   emis_table( 'Arsenic       ', (/'         ','AASJ     ','AASK     '/), (/ 0.0, 5.5E-06, 5.5E-06/) ),   ! Arsenic J and K mode
     &   emis_table( 'Lead          ', (/'         ','APBJ     ','APBK     '/), (/ 0.0, 1.6E-05, 1.6E-05/) ),   ! Lead J and K mode
     &   emis_table( 'Cadmium       ', (/'         ','ACDJ     ','ACDK     '/), (/ 0.0, 9.7E-08, 9.7E-08/) ),   ! Cadmium J and K mode
     &   emis_table( 'Mercury       ', (/'         ','APHGJ    ','APHGK    '/), (/ 0.0, 1.4E-08, 1.4E-08/) ) /) ! Mercury J and K mode

      Type( emis_table ), Allocatable :: dust_spc( : )

C Sea-Salt Speciation factors based on seawater composition:
! For toxic metal species, Use Table 1 in K.W. Bruland and M.C. Lohan, 6.02 - Controls of Trace Metals in Seawater, 
! In Treatise on Geochemistry, edited by Heinrich D. Holland and Karl K. Turekian, Pergamon, Oxford, 2003, Pages 23-47,
! ISBN 9780080437514, http://dx.doi.org/10.1016/B0-08-043751-6/06105-3.

C number of chemical species in seawater aerosol composition
      integer, parameter :: nsea_spc = 15

      Type( emis_table ), Parameter :: sea_spc( nsea_spc ) = (/
C                     -description--            -------- name --------                -------- spcfac --------
C                                           Aitken    accum     coarse             Aitken  accum   coarse
     &   emis_table( 'Sulfate       ', (/'         ','ASO4J    ','ASO4K    '/), (/ 0.0, 0.07760, 0.07760/) ),   ! Sulfate
     &   emis_table( 'Chlorine      ', (/'         ','ACLJ     ','ACLK     '/), (/ 0.0, 0.55380, 0.55380/) ),   ! Chlorine
     &   emis_table( 'Sodium        ', (/'         ','ANAJ     ','         '/), (/ 0.0, 0.30860, 0.0    /) ),   ! Sodium
     &   emis_table( 'Calcium       ', (/'         ','ACAJ     ','         '/), (/ 0.0, 0.01180, 0.0    /) ),   ! Calcium
     &   emis_table( 'Magnesium     ', (/'         ','AMGJ     ','         '/), (/ 0.0, 0.03680, 0.0    /) ),   ! Magnesium
     &   emis_table( 'Potassium     ', (/'         ','AKJ      ','         '/), (/ 0.0, 0.01140, 0.0    /) ),   ! Potassium
     &   emis_table( 'SeaSalt_Cation', (/'         ','         ','ASEACAT  '/), (/ 0.0, 0.0    , 0.36860/) ),   ! Sea-Salt Cations
     &   emis_table( 'Chromium IV   ', (/'         ','ACR_VIJ  ','ACR_VIK  '/), (/ 0.0, 5.95E-9, 5.95E-9/) ),   ! accumaltion and coarse mode Hexavalent Chromium
     &   emis_table( 'Nickel        ', (/'         ','ANIJ     ','ANIK     '/), (/ 0.0, 1.34E-8, 1.34E-8/) ),   ! accumaltion and coarse mode Nickle
     &   emis_table( 'Arsenic       ', (/'         ','AASJ     ','AASK     '/), (/ 0.0, 4.93E-8, 4.93E-8/) ),   ! accumaltion and coarse mode Arsenic
     &   emis_table( 'Beryllium     ', (/'         ','ABEJ     ','ABEK     '/), (/ 0.0, 5.2E-11, 5.2E-11/) ),   ! accumaltion and coarse mode Beryllium
     &   emis_table( 'Mercury       ', (/'         ','APHGJ    ','APHGK    '/), (/ 0.0, 5.8E-11, 5.8E-11/) ),   ! accumaltion and coarse mode Mercury
     &   emis_table( 'Lead          ', (/'         ','APBJ     ','APBK     '/), (/ 0.0, 6.0E-11, 6.0E-11/) ),   ! accumaltion and coarse mode Lead
     &   emis_table( 'Cadmium       ', (/'         ','ACDJ     ','ACDK     '/), (/ 0.0, 1.93E-9, 1.93E-9/) ),   ! accumaltion and coarse mode cadmium
     &   emis_table( 'Air Toxics Mn ', (/'         ','AMN_HAPSJ','AMN_HAPSK'/), (/ 0.0, 4.7E-11, 4.7E-11/) ) /) ! accumaltion and coarse mode Air toxics Manganese
 
      Real, Allocatable, Save :: aerospc_ssf( :,: )

C Constants used in other routines:

      Real( 8 ), Parameter :: asoil_renorm = 1.0D0 - 0.04642D0  ! = 0.95358, same as ASOIL speciation factor in the dust_spc table above

      Real( 8 ), Parameter :: ascat_na_fac = 0.8373D0    ! coarse sea-salt NA cation
      Real( 8 ), Parameter :: asoil_na_fac = 0.0626D0    ! for NA in windblown dust
      Real( 8 ), Parameter :: acors_na_fac = 0.0023D0    ! for NA in anthropogenic coarse

      Real( 8 ), Parameter :: ascat_mg_fac = 0.0997D0    ! coarse sea-salt MG cation
      Real( 8 ), Parameter :: asoil_mg_fac = 0.0170D0    ! for MG in windblown dust
      Real( 8 ), Parameter :: acors_mg_fac = 0.0032D0    ! for MG in anthropogenic coarse

      Real( 8 ), Parameter :: ascat_k_fac =  0.0310D0    ! coarse sea-salt K cation
      Real( 8 ), Parameter :: asoil_k_fac =  0.0242D0    ! for K in windblown dust
      Real( 8 ), Parameter :: acors_k_fac =  0.0176D0    ! for K in anthropogenic coarse

      Real( 8 ), Parameter :: ascat_ca_fac = 0.0320D0    ! coarse sea-salt CA cation
      Real( 8 ), Parameter :: asoil_ca_fac = 0.0838D0    ! for CA in windblown dust
      Real( 8 ), Parameter :: acors_ca_fac = 0.0562D0    ! for CA in anthropogenic coarse


      Real( 8 ), Parameter :: asoil_fe_fac = 0.02695D0   ! for FE in dust*
      Real( 8 ), Parameter :: acors_fe_fac = 0.0467D0    ! for FE in anthropogenic coarse

      Real( 8 ), Parameter :: asoil_mn_fac = 0.00075D0   ! for MN in dust*
      Real( 8 ), Parameter :: acors_mn_fac = 0.0011D0    ! for MN in anthropogenic coarse

C Coarse mode PMC speciation based on anthopogenic inventory composite from various
C sources (G. Pouliot - private communication):

      Real( 8 ), Parameter :: acorsem_aso4_fac = 0.00100D0
      Real( 8 ), Parameter :: acorsem_ano3_fac = 0.00048D0
      Real( 8 ), Parameter :: acorsem_acl_fac  = 0.00145D0
      Real( 8 ), Parameter :: acorsem_ah2o_fac = 0.00032D0
      Real( 8 ), Parameter :: acors_renorm = 1.0D0
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

C Optional Species
      Character( 16 ), Private, Parameter :: req_ietet  = 'AIETET'
      Character( 16 ), Private, Parameter :: req_ieos   = 'AIEOS'
      Character( 16 ), Private, Parameter :: req_dim    = 'ADIM'
      Character( 16 ), Private, Parameter :: req_imga   = 'AIMGA'
      Character( 16 ), Private, Parameter :: req_imos   = 'AIMOS'
      Character( 16 ), Private, Parameter :: req_phgj   = 'APHGJ'

C Indices of Optional species
      Integer :: aietet_idx = 0
      Integer :: aieos_idx  = 0
      Integer :: adim_idx   = 0
      Integer :: aimga_idx  = 0
      Integer :: aimos_idx  = 0
      Integer :: aphgj_idx  = 0

      Real, Allocatable :: aerospc_mw( : )     ! aero species M.W. (from AE_SPC Namelist) [ g/mol ]
      Real, Allocatable :: aerospc_conc( :,: ) ! aero species concentration [ ug/m^3 ]

C Common factors
      Real( 8 ) :: h2ofac                      ! converts mass concentrations [ug/m3] to 3rd moment concentrations [m3/m3]

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
      Integer, Allocatable :: aerospc_map( :,: )  ! indices of aero species to CGRID
      Integer, Allocatable :: aeronum_map( : )      ! indices of aero number variable to CGRID
      Integer, Allocatable :: aerosrf_map( : )      ! indices of aero surf area variable to CGRID

C Missing aerosol species map
      Logical, Allocatable :: aero_missing( :,: )  ! indices of aero species to CGRID

C Emissions mapping
      Integer         :: n_emis_pm              ! number of aerospc with emissions
      Integer, Allocatable :: pmem_map( : )     ! mapping to aerospc array for PM emissions
      Character( 16 ), Allocatable :: pmem_map_name( : )! mapping to aerospc array for PM emissions
      Character( 16 ) :: pmem_units             ! units for PM emissions for all species

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
     &                        LBCMODE(:,:)
      REAL, ALLOCATABLE, SAVE :: AEROCGRID_RHOINV( : ) !
      
C Private variables for loading from and unloading to CGRID array
      Logical, Private, Save :: mapped    = .False.
      Character( 16 ), Private, Save :: pname = 'Aero_Data'

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

      Use rxns_data             ! chemical mechanism data
      Use cgrid_spcs            ! CGRID mechanism species
      Use aeromet_data
      Use utilio_defn
      Implicit None

C Local Variables:
      Character( 256 ) :: xmsg
      Character( 256 ) :: xmsg2
      Real    :: mole_weight( n_aerolist )
      Integer :: aerolist_map  ( n_aerolist,n_mode )
      Integer :: aerospc_tolist( n_aerolist )

      Integer, Allocatable :: aerospc_in_dustlist( : )

      Integer l, m, n, spc, isea, idust
      Real    so4fac
      Real    anthfac

      Real    dust_POC  ! dust scale factor for Primary Organic Carbon
      Real    dust_PNC  ! dust scale factor for Primary Non-Carbon aerosol
      Real    dust_POA  ! scale factor for Semivolatile POA in dust emissions
      Real    dust_OOA  ! scale factor for Semivolatile OOA in dust emissions
      Real    dust_OMOC ! ratio of Organic Matter to Organic Carbon in dust emissions
      Real    dust_OC   ! ratio of Elmental Oxygen to Carbon in dust emissions
      Real    POA_OC    ! ratio of Elemental Oxygen to Carbon in POA
      Real    OOA_OC    ! ratio of Elemental Oxygen to Carbon in Oxygenated OA

      Integer dust_org1, dust_org2
      Logical ae6isoa
      Logical ae6hg
      Logical dust_match

      If ( mapped ) Return

      logdev      = setup_logdev()
      mole_weight = 0.0

      n_aerospc      = 0   ! Number of Used Aerosol Chemical Species
      aerolist_map   = 0   ! Pointer from aerolist to CGRID
      aerospc_tolist = 0   ! Pointer from aerospc to aerolist
      AE_eflag       = .False.
      max_l2sg       = ( LOG( max_sigma_g ) ) ** 2
      min_l2sg       = ( LOG( min_sigma_g ) ) ** 2
      def_l2sg( : )  = ( LOG( def_sigma_g( : ) ) ) ** 2
      

      allocate( aerocgrid_rhoinv( n_ae_trns ), lbcnum( n_ae_trns ),
     &          lbcsrf( n_ae_trns ), lbcmass( n_ae_trns ),
     &          lbcmode( n_mode,n_ae_trns ) )
      aerocgrid_rhoinv = 0.
      lbcnum = .False.
      lbcsrf = .False.
      lbcmass= .False.
      lbcmode= .False.

C Build mapping to CGRID for each species in the master AeroList
      Do spc = 1, n_aerolist
         Do m = 1, n_mode   ! Loop through modes

            If ( aerolist( spc )%name( m )(1:1) .Ne. ' ' ) Then
               ! This place on the AeroList has a potential species in
               ! it. Check to see if it is in the namelist
               n = index1( aerolist( spc )%name( m ) , n_ae_spc, ae_spc )

               If ( n .Ne. 0 ) Then
                  ! Species is in the Master List and on the NameList
                  If ( .Not. ( Any( aerolist_map( spc,: ) .NE. 0 ) ) ) Then
                     ! Add this species to the map if it does not exist already
                     n_aerospc = n_aerospc + 1
                     aerospc_tolist( n_aerospc ) = spc
                     ! Set the Molecular Weight
                     mole_weight( spc ) = ae_molwt( n )
                  Else If ( mole_weight( spc ) .Ne. ae_molwt( n ) ) Then
                     ! If the species already exists and the
                     ! molecular weight from this mode on the
                     ! namelist is not matching the standing
                     ! molecular weight, throw an error
                     xmsg = 'molecular weight of ' // Trim( aerolist(spc )%name( m ) )
     &                      // ' is different from that of the same species'
     &                      // ' in the same or another mode.'
                     Call m3warn( pname, 0, 0, xmsg )
                     Write( xmsg,* ) 'New Value(', n, ') = ', ae_molwt( n ),
     &                               'Expected value(', spc, ')= ', mole_weight( spc )
                     Call m3warn( pname, 0, 0, xmsg )
                     AE_eflag  = .True.
                  End If
                  ! Add the map from CGRID to AeroList
                  aerolist_map( spc,m ) = ae_strt - 1 + n
                  lbcmode( m,n ) = .True.
                  If ( .Not. aerolist( spc)%tracer ) lbcmass( n ) = .True.
                  !Map the CGRID Aerosol Species to their Densities
                  aerocgrid_rhoinv( n ) = 1.0 / aerolist( spc )%density

               End If
            End If
         End Do
      End Do

      ! Migrate all of the user-requested aerosols from AeroList to
      ! AeroSpc. Begin by allocating all of the new arrays now that we
      ! know the value of n_aerospc
      Allocate ( aerospc     ( n_aerospc ) )
      Allocate ( aerospc_mw  ( n_aerospc ) )
      Allocate ( aerospc_map ( n_aerospc, n_mode ) )
      Allocate ( aerospc_conc( n_aerospc, n_mode ) )
      Allocate ( aerospc_ssf ( n_mode, n_aerospc ) )
      Allocate ( aero_missing( n_aerospc, n_mode ) )
      Allocate ( aeronum_map ( n_aerospc ) )
      Allocate ( aerosrf_map ( n_aerospc ) )

      aerospc_ssf = 0.0

      Do spc = 1, n_aerospc ! Loop through user-requested chemical species
         l = aerospc_tolist( spc )
         aerospc( spc ) = aerolist( l )   ! Map AeroList to AeroSpc
         aerospc_mw( spc ) = mole_weight( l )

#ifdef verbose_aero
         If( spc .eq. 1 )Write(logdev,99948)
#endif
         Do n = 1,n_mode
            ! Map the Modal-Dependent CGRID Pointers from AeroSpc to AeroList
            aerospc_map( spc, n ) = aerolist_map( l, n )

            ! Map The Sea-Salt Species to the aerospc_ssf Array
            Do isea = 1, nsea_spc
               Do m = 1, n_mode
                  If ( sea_spc( isea )%name( m ) .Eq. aerospc( spc )%name( n ) ) Then
                     aerospc_ssf( n, spc ) = sea_spc( isea )%spcfac( m )
#ifdef verbose_aero
                      If ( sea_spc( isea )%spcfac( m ) .Lt. conmin )Cycle
                      Write(logdev,99950)spc,sea_spc( isea )%name( m ),sea_spc( isea )%spcfac( m )
#endif
                  End If
               End Do
            End Do


            ! If a user invokes a chemical species, they should have the
            ! same modes present as is indicated in the table above.
            ! Check that this is true.
            If ( aerospc( spc )%name( n )(1:) .Ne. ' ' .And.
     &           aerolist_map( aerospc_tolist( spc ),n ) .Eq. 0 ) Then
               AE_eflag  = .True.
               Write( xmsg,* ) 'FATAL ERROR: Linked set of tracers, ',
     &                    Trim( xmsg2 ),', is missing species, ',
     &                    Trim( aerolist( spc )%name( n ) )
               Call m3warn( pname, 0, 0, xmsg )
            End If

            ! Set the Aero_Missing Flag
            If ( aerospc( spc )%name( n ) .Ne. ' ' ) Then
                aero_missing( spc,n ) = .False.
            Else
                aero_missing( spc,n ) = .True.
            End If

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

      If ( Index( mechname, 'SAPRC07TIC_AE6I' ) .Gt. 0 ) Then
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

      aphgj_idx = findAero( req_phgj,  .False. )
      If ( aphgj_idx .Gt. 0 ) Then
         ae6hg     = .True.
      Else
         aphgj_idx = 0 
         ae6hg     = .False.
      End If

C Adjust Dust Emissions list for Organic Species, if running with semivolatile POA
      If ( Index( mechname, 'NVPOA' ) .Eq. 0 ) Then
         ! Find Indices for Nonvolatile Organics
         Do idust = 1, ndust_list
            If ( Index( dust_spc_list( idust )%name( 2 ), 'APOCJ' ) .Gt. 0 ) dust_org1 = idust
            If ( Index( dust_spc_list( idust )%name( 2 ), 'APNCOMJ') .Gt. 0 ) dust_org2 = idust
         End Do

         ! Compute Total Organic Dust Scale Factor and O:C Ratio
         ! The dust OM:OC should equal 1.4 if it consistent with Simon and Bhave (2011).
         dust_POC = dust_spc_list( dust_org1 )%spcfac( 2 )
         dust_PNC = dust_spc_list( dust_org2 )%spcfac( 2 )
         dust_OMOC = ( dust_POC + dust_PNC ) / dust_POC
         dust_OC = 12.0 / 15.0 * dust_OMOC - 14.0 / 15.0

         ! Distribute Dust Organic Speciation between Volatility-Based Surrogates.
         ! These hard-coded values can be generalized once O:C information is
         ! incoporated directly into the internal OA species table.
         POA_OC = 0.1853  ! O:C Ratio of LVPOA1
         OOA_OC = 0.7106  ! O:C Ratio of LVOOA2
         dust_OC = Max( dust_OC, POA_OC )
         dust_POA = 1.0 / ( 1.0 + ( dust_OC - POA_OC ) / ( OOA_OC - dust_OC ) )
         dust_OOA = 1.0 / ( 1.0 + ( dust_OC - OOA_OC ) / ( POA_OC - dust_OC ) )

         ! Assign new names and speciation to dust table
         dust_spc_list( dust_org1 )%name( 2 ) = 'ALVPO1J'
         dust_spc_list( dust_org2 )%name( 2 ) = 'ALVOO2J'
         dust_spc_list( dust_org1 )%spcfac( 2 ) = dust_POA * ( dust_POC + dust_PNC )
         dust_spc_list( dust_org2 )%spcfac( 2 ) = dust_OOA * ( dust_POC + dust_PNC )
      End If

! Determine map between aero_spc dust emissions and the dust emission listndust_spc
      Allocate( aerospc_in_dustlist( n_aerolist ) )
      aerospc_in_dustlist   = 0   ! Pointer identifies to use dust emission table

      ndust_spc = 0
      Do spc = 1, n_aerospc ! Loop through user-requested chemical species
         Do idust = 1, ndust_list ! loop through dust list
#ifdef verbose_aero
            xmsg = ' '
#endif
            dust_match = .False.
            Do m = 1, n_mode
               If( aero_missing(spc,m) )Cycle
               If ( dust_spc_list( idust )%name( m ) .Eq. aerospc( spc )%name( m ) ) Then
#ifdef verbose_aero
                   Select Case( m )
                     Case( 1 )
                        xmsg = 'Aitken'
                     Case( 2 )
                        xmsg = Trim( xmsg ) // ' Accum.'
                     Case( 3 ) 
                       xmsg = Trim( xmsg ) // ' Coarse'
                   End Select
#endif
                   dust_match = .True.
               End If
            End Do
            If( dust_match )Then
                ndust_spc = ndust_spc + 1
                aerospc_in_dustlist( ndust_spc ) = idust
#ifdef verbose_aero
                Write( logdev,'(a,1x,a)')'Dust Emissions Added to ',
     &          Trim( dust_spc_list( ndust_spc )%description )  // ' '
     &          // Trim( xmsg ) // ' mode(s)'
#endif
            End If
         End Do
      End Do

      If( ndust_spc .Gt. 0 )Then
! build dust_spc type from selected list elements
         Allocate( dust_spc( ndust_spc ) )
         n = 0
#ifdef verbose_aero
         Write(logdev,99949)
#endif
         Do spc = 1, ndust_spc
            idust = aerospc_in_dustlist( spc )
            dust_spc( spc ) = dust_spc_list( idust )
            Do m = 1, n_mode
               If( len_trim( dust_spc( spc )%name( m ) ) .lt. 1 )cycle
               n = n + 1
#ifdef verbose_aero
               Write(logdev,99950)n,dust_spc( spc )%name( m ),dust_spc( spc )%spcfac( m )
#endif
            End Do
         End Do
      End If

#ifdef verbose_aero
99948 Format(/ '_I_',1X,'Aero_Species____','SeaSalt_Emiss_Speciation')
99949 Format(/ '_I_',1X,'Aero_Species____','Dust_Emiss_Speciation')
99950 Format(i3,1X,a16,11X,es12.4)
#endif

      Deallocate( aerospc_in_dustlist )

#ifdef verbose_aero
      ! Mandatory Species
      Write( logdev,'( /5x, a )' ) 'map_aero required species'
      Write( logdev,'( 5x, a, i4 )' ) 'aso4_idx:   ', aso4_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ano3_idx:   ', ano3_idx
      Write( logdev,'( 5x, a, i4 )' ) 'acl_idx:    ', acl_idx
      Write( logdev,'( 5x, a, i4 )' ) 'anh4_idx:   ', anh4_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ana_idx:    ', ana_idx
      Write( logdev,'( 5x, a, i4 )' ) 'amg_idx:    ', amg_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ak_idx:     ', ak_idx
      Write( logdev,'( 5x, a, i4 )' ) 'aca_idx:    ', aca_idx
      Write( logdev,'( 5x, a, i4 )' ) 'afe_idx:    ', afe_idx
      Write( logdev,'( 5x, a, i4 )' ) 'amn_idx:    ', amn_idx
      Write( logdev,'( 5x, a, i4 )' ) 'apoc_idx:   ', apoc_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ah2o_idx:   ', ah2o_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ah3op_idx:  ', ah3op_idx
      Write( logdev,'( 5x, a, i4 )' ) 'asoil_idx:  ', asoil_idx
      Write( logdev,'( 5x, a, i4 )' ) 'acors_idx:  ', acors_idx
      Write( logdev,'( 5x, a, i4 )' ) 'apncom_idx: ', apncom_idx
      Write( logdev,'( 5x, a, i4 )' ) 'aseacat_idx:', aseacat_idx
      If( ae6isoa )Then
          Write( logdev,'( 5x, a, i4 )' ) 'aietet_idx: ', aietet_idx
          Write( logdev,'( 5x, a, i4 )' ) 'aieos_idx:  ', aieos_idx
          Write( logdev,'( 5x, a, i4 )' ) 'adim_idx:   ', adim_idx
          Write( logdev,'( 5x, a, i4 )' ) 'aimga_idx:  ', aimga_idx
          Write( logdev,'( 5x, a, i4 )' ) 'aimos_idx:  ', aimos_idx
      End If      
      If ( ae6hg ) Then
          Write( logdev,'( 5x, a, i4 )' ) 'aphgj_idx:  ', aphgj_idx
      Else 
          Write( logdev,'( 5x, a, i4 )' ) 'PHGJ not found so aphgj_idx: ', aphgj_idx
      End If
#endif
 
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

      Write( logdev,'( 5x, a )' ) ' --- Aero Species Mapped ---'

99901 Format( 'FATAL Error(s) found in the AE namelist used. Check that ' 
     &     /  'this AE namelist contains the above required data as the file in '
     &     /  'the respository version of the mechanism: ', a )
      Return
      End Subroutine map_aero

C-----------------------------------------------------------------------
      Subroutine map_pmemis ( )
 
C  Set the emissions units from the header of EMIS_1 file
C  Verify that all the units on the file are consistent

      Use utilio_defn
      Use rxns_data, only : mechname

      Implicit None

      Include SUBST_FILES_ID  ! file name parameters

C Parameters:
      Character( 10 ), Parameter :: blank10 = ' '

C Local Variables:
      Character( 16 ), Save :: pname = 'map_pmemis'
      Character( 512 )      :: xmsg1
      Character( 1024 )     :: xmsg2
      Character( 16 )       :: units

      Integer :: indx
      Integer :: v
      Logical :: found, match

      Logical, Save :: pm_mapped = .False.

      logdev = setup_logdev()

C Create mapping only if first call
      If ( pm_mapped ) Return

C Call routine to map aerosol species array
      If ( .Not. mapped ) Call map_aero ( )

C Open the gridded emissions file, which contains gas, aerosol, and non-reactive
C species

      If ( .Not. open3( emis_1, fsread3, pname ) ) Then
         xmsg1 = 'Could not open '// emis_1 // ' file'
         Call m3exit( pname, 0, 0, Trim( xmsg1 ), xstat1 )
      End If

      If ( .Not. desc3( emis_1 ) ) Then
         xmsg1 = 'Could not get '// 'EMIS_1' // ' file description'
         Call m3exit( pname, 0, 0, Trim( xmsg1 ), xstat2 )
      End If

C Search emissions file for emission species names. Verify that their units
C are the same and set pmem_units

      Allocate( pmem_map      ( n_aerospc ) )
      Allocate( pmem_map_name ( n_aerospc ) )
      n_emis_pm = 0
      pmem_units = 'null'
      found = .True.
      match = .True.
      xmsg1 = 'Could not find the following species in emissions file'
      xmsg2 = 'PM Units not uniform in EMIS_1 file.'

      Do v = 1, n_aerospc
         If ( aerospc( v )%emis( 1:1 ) .Ne. ' ' ) Then
            indx = index1( aerospc( v )%emis, nvars3d, vname3d )
            If ( indx .Le. 0 ) Then
               xmsg1 = Trim( xmsg1 ) // crlf() // blank10
     &               // Trim( aerospc( v )%emis )
               found = .False.
               Cycle
            End If
            n_emis_pm = n_emis_pm + 1
            pmem_map( n_emis_pm ) = v
            pmem_map_name (n_emis_pm ) = aerospc( v )%emis

            ! Add Non-Carbon Primary Organic Emissions Surrogate and Map
            ! it to the Carbon Primary Organic Emissions Surrogate
            If ( Index( mechname,"NVPOA") .EQ. 0 .AND. aerospc( v )%emis .EQ. 'POC' ) Then
              indx = index1( 'PNCOM', nvars3d, vname3d )
              If ( indx .Le. 0 ) Then
                 xmsg1 = Trim( xmsg1 ) // crlf() // blank10
     &               // Trim( aerospc( v )%emis )
                 found = .False.
                 Cycle
              End If
              n_emis_pm = n_emis_pm + 1
              pmem_map( n_emis_pm ) = v
              pmem_map_name (n_emis_pm ) = 'PNCOM'
            End If  

C Change UNITS to upper case
            units = units3d( indx )
            Call upcase( units )

C Save units on first emissions
            If ( pmem_units .Eq. 'null' ) pmem_units = units

C Check that all emissions units match
            If ( pmem_units .Ne. units ) Then
               xmsg2 = Trim( xmsg2 ) // crlf() // blank10
     &               // Trim( aerospc( v )%emis )
     &               // '  [' // Trim( units3d( indx ) ) // ']'
               match = .False.
            End If
         End If
      End Do

      If ( .Not. found ) Then
         Call m3warn( pname, 0, 0, Trim( xmsg1 ) )
      End If

      If ( .Not. match ) Then
         Call m3exit( pname, 0, 0, Trim( xmsg2 ), xstat2 )
      End If

#ifdef verbose_aero
      Write( logdev,'( /5x, a )' ) 'pmem_map to aerospc'
         Do v = 1, n_emis_pm
         Write( logdev,'( 5x, a, 2i4, 2x, a )' ) 'pmem_map:', v, pmem_map( v ), pmem_map_name( v )
      End Do
#endif

      pm_mapped = .True.

      Write( logdev,'( 5x, a )' ) ' --- PM Emis Species Mapped ---'

      Return

      End Subroutine map_pmemis

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
!    &      MASK= (aerospc%no_M2wet==.false. .AND. aerospc%Tracer==.false.) )
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
         xmsg = ' NOT FATAL: Optional Species '
     &       // Trim( vname ) // ' Not found in AE namelist.'
         write(logdev,'(/ a / )')xmsg
         Return
      End If

      xmsg = 'FATAL: Required Species ' // Trim( vname ) // ' Not found in AE namelist'
      AE_eflag  = .True.
      Call m3warn( pname, 0, 0, xmsg )
!     Call m3exit( pname, 0, 0, xmsg, xstat3 )

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
         NUM = SUM( IBCON( : ), MASK = ( LBCMODE( IMODE,: ) .AND. LBCNUM ) )
         M2  = SUM( IBCON( : ), MASK = ( LBCMODE( IMODE,: ) .AND. LBCSRF ) ) * F1PI

         M3 = SUM( IBCON( : ) * AEROCGRID_RHOINV * F6PI, MASK = ( LBCMASS .AND. LBCMODE( IMODE,: ) )  )

         IF ( AE_MASS_UNIT ) M3 = M3 * KGPMG

         AER_PAR ( 1, IMODE, 1 ) = NUM
         AER_PAR ( 1, IMODE, 2 ) = M2
         AER_PAR ( 1, IMODE, 3 ) = M3
         AER_PAR ( 1, IMODE, 4 ) = 0.    
         AER_PAR ( 1, IMODE, 5 ) = 0.    

         IF ( M3 .LT. 1.0e-30 ) THEN
             STAT = 4
             LMODE = IMODE
         ELSE IF ( NUM .LT. 1.0e-30 .OR. M2 .LT. 1.0e-30 ) THEN
             STAT = 3
             LMODE = IMODE
         ELSE                  
             l2sg = ( ONE_THIRD * LOG( NUM ) + TWO_THIRDS * LOG( M3 ) - LOG( M2 ))

             IF ( l2sg .LT. MIN_L2SG .OR. l2sg .GT. MAX_L2SG ) THEN 
                  STAT = 2
                  LMODE = IMODE
             ELSE

                  dg   = ( M3 / NUM * EXP( -4.5 * l2sg ) )  ** ( ONE_THIRD )
                  sg   = EXP( SQRT( l2sg ) )
                  AER_PAR( 1, IMODE, 4 ) = dg
                  AER_PAR( 1, IMODE, 5 ) = sg

                  IF ( dg .LT. MIN_DIAM_G( IMODE )  .OR. dg .GT. MAX_DIAM_G( IMODE ) ) THEN
                      STAT = 1
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

         IF ( STAT .GT. 0 .AND. STAT .LT. 4 ) THEN
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

         ELSEIF ( STAT .EQ. 4 ) THEN
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
