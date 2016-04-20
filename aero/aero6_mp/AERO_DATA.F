!
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero6_mp/AERO_DATA.F,v 1.3 2012/01/26 18:44:38 sjr Exp $

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
C JY = Jeff Young; HOTP = Havala Pye; KF = Kathleen Fahey, CGN = Chris Nolte

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
C KF  09/23/14 Changed emitted modal PM mass fractions according to  
C              Elleman and Covert (2010)
C JY  08/24/15 Changed visual index factors
C JY  02/17/16 Created named constants for speciation and other factors for AERO_EMIS,
C              DUST_EMIS, aero_subs:VOLINORG, and aqchem
C CGN 04/14/16 Changed AMGJ speciation factor from 0.0 to 0.019 following Upadhyay et al.

C References:
C 1. Upadhyay, et al., Size-Differentiated Chemical Composition of Re-Suspended Soil
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
C extinction efficiency [m^2/g], see White, Atmos.Env.,294(10)(1990),pp 2673-1672
C and Malm, et al., JGR,99(D1)(1994),pp 1347-1370
C----------------------------------------------------------------------

      Implicit None

C Number of aerosol species and modes

!     Integer, Parameter :: n_aerospc = 40      ! number of aero species
      Integer, Parameter :: n_aerolist = 69     ! number of aero species
      Integer, Parameter :: n_mode = 3          ! number of modes:
                                                ! 1 = Aitken 
                                                ! 2 = accumulation 
                                                ! 3 = coarse 

      Integer, Save :: n_aerospc ! number of aero species

C Default minimum concentration
      Real,    Parameter :: conmin = 1.0E-30    ! [ ug/m^3 ]
      Real,    Parameter :: cm_set( n_mode ) = (/ conmin, conmin, conmin /)
      Real,    Parameter :: cm_so4( n_mode ) = (/ 1.0E-12,1.0E-6, conmin /)
      Real,    Parameter :: cm_cor( n_mode ) = (/ conmin, conmin, 1.889544E-05 /)

C Emissions splits
      Real,    Parameter :: es_fin( n_mode ) = (/ 0.100, 0.900, 0.000 /)
      Real,    Parameter :: es_acc( n_mode ) = (/ 0.000, 1.000, 0.000 /)
      Real,    Parameter :: es_cor( n_mode ) = (/ 0.000, 0.000, 1.000 /)
      Real,    Parameter :: es_0  ( n_mode ) = (/ 0.000, 0.000, 0.000 /)

C Sea salt factors
      Real,    Parameter :: ss_so4( n_mode ) = (/ 0.0,    0.0776, 0.0776 /)
      Real,    Parameter :: ss_na ( n_mode ) = (/ 0.0,    0.3086, 0.0    /)
      Real,    Parameter :: ss_cl ( n_mode ) = (/ 0.0,    0.5338, 0.5338 /)
      Real,    Parameter :: ss_mg ( n_mode ) = (/ 0.0,    0.0368, 0.0    /)
      Real,    Parameter :: ss_ca ( n_mode ) = (/ 0.0,    0.0114, 0.0    /)
      Real,    Parameter :: ss_k  ( n_mode ) = (/ 0.0,    0.0118, 0.0    /)
      Real,    Parameter :: ss_sea( n_mode ) = (/ 0.0,    0.0,    0.3686 /)
      Real,    Parameter :: ss_0  ( n_mode ) = (/ 0.0,    0.0,    0.0    /)

C Geometric mean diameter by volume (or mass) of emitted particles in
C each mode [ m ].  See paragraph #14 of Binkowski & Roselle (2003)
!     Real,    Parameter :: dgvem( n_mode ) = (/ 0.03E-6, 0.3E-6, 6.0E-6 /)
      Real,    Parameter :: dgvem( n_mode ) = (/ 0.06E-6, 0.28E-6, 6.0E-6 /) ! KF 05/06/14

C Geometric standard deviation of emitted particles in each mode, as
C described in paragraph #14 of Binkowski & Roselle (2003)
!     Real,    Parameter :: sgem( n_mode ) = (/ 1.7, 2.0, 2.2 /)
      Real,    Parameter :: sgem( n_mode ) = (/ 1.7, 1.7, 2.2 /)  ! KF 05/06/14


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
         Real            :: visual_idx           ! visual index factor
         Character( 16 ) :: optic_surr           ! optical surrogate name
         Character( 16 ) :: emis                 ! file emissions names
         Real            :: emis_split( n_mode ) ! minimum concentration values for each mode
         Real            :: ssf_emis( n_mode )   ! fraction of sea salt emission in each mode
      End Type spcs_type

      Type( spcs_type ), Allocatable, Save :: aerospc ( : )

      Type( spcs_type ), Parameter   :: aerolist( n_aerolist ) = (/
C                                                                        nonVolSOA
C                                                                            |   Charge                                                      
C                    ------------Name--------------                  NoM2Wet |Tracer|       OptSurr               Emissions Sea Salt
C                    Aitken      Accum       Coarse     Min_Concs Density |  |  |   | Visidx    |      Emis          Splits  factors
C                   ---------   ---------   ---------     ------  ------  +  +  +   + ------  ------   -------------  ------ ------
     & spcs_type((/'ASO4I    ','ASO4J    ','ASO4K    '/), cm_so4, 1800.0, F, F, F, -2, 3.0, 'SOLUTE','PSO4         ',es_fin,ss_so4),
     & spcs_type((/'ANO3I    ','ANO3J    ','ANO3K    '/), cm_set, 1800.0, F, F, F, -1, 3.0, 'SOLUTE','PNO3         ',es_fin,  ss_0),
     & spcs_type((/'ACLI     ','ACLJ     ','ACLK     '/), cm_set, 2200.0, F, F, F, -1, 1.37,'SOLUTE','PCL          ',es_fin, ss_cl),
     & spcs_type((/'ANH4I    ','ANH4J    ','ANH4K    '/), cm_set, 1800.0, F, F, F,  1, 3.0, 'SOLUTE','PNH4         ',es_fin,  ss_0),
     & spcs_type((/'ANAI     ','ANAJ     ','         '/), cm_set, 2200.0, F, F, F,  1, 1.37,'SOLUTE','PNA          ',es_fin, ss_na),
     & spcs_type((/'         ','AMGJ     ','         '/), cm_set, 2200.0, F, F, F,  2, 1.0, 'DUST  ','PMG          ',es_acc, ss_mg),
     & spcs_type((/'         ','AKJ      ','         '/), cm_set, 2200.0, F, F, F,  1, 1.0, 'DUST  ','PK           ',es_acc,  ss_k),
     & spcs_type((/'         ','ACAJ     ','         '/), cm_set, 2200.0, F, F, F,  2, 1.0, 'DUST  ','PCA          ',es_acc, ss_ca),
     & spcs_type((/'APOCI    ','APOCJ    ','         '/), cm_set, 2000.0, F, F, F,  0, 4.0, 'DUST  ','POC          ',es_fin,  ss_0),
     & spcs_type((/'APNCOMI  ','APNCOMJ  ','         '/), cm_set, 2000.0, F, F, F,  0, 4.0, 'DUST  ','PNCOM        ',es_fin,  ss_0),
     & spcs_type((/'AECI     ','AECJ     ','         '/), cm_set, 2200.0, F, F, F,  0,10.0, 'SOOT  ','PEC          ',es_fin,  ss_0),
     & spcs_type((/'         ','AFEJ     ','         '/), cm_set, 2200.0, F, F, F,  0, 1.0, 'DUST  ','PFE          ',es_acc,  ss_0),
     & spcs_type((/'         ','AALJ     ','         '/), cm_set, 2200.0, F, F, F,  0, 1.0, 'DUST  ','PAL          ',es_acc,  ss_0),
     & spcs_type((/'         ','ASIJ     ','         '/), cm_set, 2200.0, F, F, F,  0, 1.0, 'DUST  ','PSI          ',es_acc,  ss_0),
     & spcs_type((/'         ','ATIJ     ','         '/), cm_set, 2200.0, F, F, F,  0, 1.0, 'DUST  ','PTI          ',es_acc,  ss_0),
     & spcs_type((/'         ','AMNJ     ','         '/), cm_set, 2200.0, F, F, F,  0, 1.0, 'DUST  ','PMN          ',es_acc,  ss_0),
     & spcs_type((/'AH2OI    ','AH2OJ    ','AH2OK    '/), cm_set, 1000.0, T, F, F,  0, 0.0, 'WATER ','PH2O         ',es_fin,  ss_0),
     & spcs_type((/'AH3OPI   ','AH3OPJ   ','AH3OPK   '/), cm_set, 1000.0, T, F, T,  0, 0.0, 'WATER ','             ',es_0,    ss_0),
     & spcs_type((/'AOTHRI   ','AOTHRJ   ','         '/), cm_set, 2200.0, F, F, F,  0, 1.0, 'DUST  ','PMOTHR       ',es_fin,  ss_0),
     & spcs_type((/'         ','AALK1J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AALK2J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AXYL1J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AXYL2J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AXYL3J   ','         '/), cm_set, 2000.0, F, T, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ATOL1J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ATOL2J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ATOL3J   ','         '/), cm_set, 2000.0, F, T, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ABNZ1J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ABNZ2J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ABNZ3J   ','         '/), cm_set, 2000.0, F, T, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ATRP1J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ATRP2J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AISO1J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AISO2J   ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AISO3J   ','         '/), cm_set, 2000.0, F, T, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','ASQTJ    ','         '/), cm_set, 2000.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','APAH1J   ','         '/), cm_set, 1480.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','APAH2J   ','         '/), cm_set, 1480.0, T, F, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','APAH3J   ','         '/), cm_set, 1550.0, F, T, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AOLGAJ   ','         '/), cm_set, 2000.0, F, T, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AOLGBJ   ','         '/), cm_set, 2000.0, F, T, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','AORGCJ   ','         '/), cm_set, 2000.0, F, T, F,  0, 4.0, 'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'         ','         ','ASOIL    '/), cm_set, 2600.0, F, F, F,  0, 0.6, 'DUST  ','             ',es_cor,  ss_0),
     & spcs_type((/'         ','         ','ACORS    '/), cm_cor, 2200.0, F, F, F,  0, 0.6, 'DUST  ','PMC          ',es_cor,  ss_0),
     & spcs_type((/'         ','         ','ASEACAT  '/), cm_cor, 2200.0, F, F, F,  1,1.37, 'SOLUTE','             ',es_cor,ss_sea),
C the below tracer are optional and their usage depend on the AE namelist for the mechanism
C                                                                        nonVolSOA
C                                                                            |   Charge                                                      
C                    ------------Name--------------                  NoM2Wet |Tracer|       OptSurr               Emissions Sea Salt
C                    Aitken      Accum       Coarse     Min_Concs Density |  |  |   | Visidx   |      Emis          Splits  factors
C                   ---------   ---------   ---------     ------- ------  +  +  +   +  ----  ------   -------------  ------ ------
     & spcs_type((/'ANII     ','ANIJ     ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','NICKEL_F     ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','ANIK     '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','NICKEL_C     ',es_cor,  ss_0),
     & spcs_type((/'ACR_VII  ','ACR_VIJ  ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','CHROMHEX_F   ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','ACR_VIK  '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','CHROMHEX_C   ',es_cor,  ss_0),
     & spcs_type((/'ACR_IIII ','ACR_IIIJ ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','CHROMTRI_F   ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','ACR_IIIK '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','CHROMTRI_C   ',es_cor,  ss_0),
     & spcs_type((/'ABEI     ','ABEJ     ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','BERYLLIUM_F  ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','ABEK     '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','BERYLLIUM_C  ',es_cor,  ss_0),
     & spcs_type((/'APBI     ','APBJ     ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','LEAD_F       ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','APBK     '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','LEAD_C       ',es_cor,  ss_0),
     & spcs_type((/'ADE_OTHRI','ADE_OTHRJ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','DIESEL_PMFINE',es_fin,  ss_0),
     & spcs_type((/'ADE_ECI  ','ADE_ECJ  ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','DIESEL_PMEC  ',es_fin,  ss_0),
     & spcs_type((/'ADE_OCI  ','ADE_OCJ  ','         '/), cm_set, 2000.0, F, F, T,  0,  0.0,'DUST  ','DIESEL_PMOC  ',es_fin,  ss_0),
     & spcs_type((/'         ','ADE_NO3J ','         '/), cm_set, 1800.0, F, F, T,  0,  0.0,'DUST  ','DIESEL_PMNO3 ',es_acc,  ss_0),
     & spcs_type((/'         ','ADE_SO4J ','         '/), cm_set, 1800.0, F, F, T,  0,  0.0,'DUST  ','DIESEL_PMSO4 ',es_acc,  ss_0),
     & spcs_type((/'         ','         ','ADE_K    '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','DIESEL_PMC   ',es_cor,  ss_0),
     & spcs_type((/'ACDI     ','ACDJ     ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','CADMIUM_F    ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','ACDK     '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','CADMIUM_C    ',es_cor,  ss_0),
     & spcs_type((/'AMN_HAPSI','AMN_HAPSJ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','MANGANESE_F  ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','AMN_HAPSK'/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','MANGANESE_C  ',es_cor,  ss_0),
     & spcs_type((/'APHGI    ','APHGJ    ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','PHGI         ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','APHGK    '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','             ',es_0,    ss_0),
     & spcs_type((/'AASI     ','AASJ     ','         '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','ARSENIC_F    ',es_fin,  ss_0),
     & spcs_type((/'         ','         ','AASK     '/), cm_set, 2200.0, F, F, T,  0,  0.0,'DUST  ','ARSENIC_C    ',es_cor,  ss_0)
     & /)                                                                                                                          
C number of chemical species in windblown dust aerosol
      integer, parameter :: ndust_spc = 19

C number of lognormal modes in windblown dust aerosol = n_mode
C - but only accumulation and coarse modes used
      Type dust_type
         Character( 16 ) :: name  ( n_mode )
         Real            :: spcfac( n_mode )
      End type dust_type

C speciation factors from SPECIATE composite of 4 desert soil profiles (Pouliot):
      Type( dust_type ), Parameter :: dust_spc( ndust_spc ) = (/
C                        -------- name --------         -------- spcfac --------
C                      Aitken    accum     coarse        Aitken  accum   coarse
     &   dust_type( (/'       ','ASO4J  ','ASO4K  '/), (/ 0.0, 0.02250, 0.02655/) ),   ! Sulfate
     &   dust_type( (/'       ','ANO3J  ','ANO3K  '/), (/ 0.0, 0.00020, 0.00160/) ),   ! Nitrate
     &   dust_type( (/'       ','ACLJ   ','ACLK   '/), (/ 0.0, 0.00945, 0.01190/) ),   ! Chlorine
     &   dust_type( (/'       ','ANH4J  ','       '/), (/ 0.0, 0.00005, 0.0    /) ),   ! Ammonium
     &   dust_type( (/'       ','ANAJ   ','       '/), (/ 0.0, 0.03935, 0.0    /) ),   ! Sodium
     &   dust_type( (/'       ','ACAJ   ','       '/), (/ 0.0, 0.07940, 0.0    /) ),   ! Calcium
     &   dust_type( (/'       ','AMGJ   ','       '/), (/ 0.0, 0.01900, 0.0    /) ),   ! Magnesium
     &   dust_type( (/'       ','AKJ    ','       '/), (/ 0.0, 0.03770, 0.0    /) ),   ! Potassium
     &   dust_type( (/'       ','APOCJ  ','       '/), (/ 0.0, 0.01075, 0.0    /) ),   ! Organic Carbon
     &   dust_type( (/'       ','APNCOMJ','       '/), (/ 0.0, 0.00430, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   dust_type( (/'       ','AECJ   ','       '/), (/ 0.0, 0.0,     0.0    /) ),   ! Elemental Carbon
     &   dust_type( (/'       ','AFEJ   ','       '/), (/ 0.0, 0.03355, 0.0    /) ),   ! Iron
     &   dust_type( (/'       ','AALJ   ','       '/), (/ 0.0, 0.05695, 0.0    /) ),   ! Aluminum
     &   dust_type( (/'       ','ASIJ   ','       '/), (/ 0.0, 0.19425, 0.0    /) ),   ! Silicon
     &   dust_type( (/'       ','ATIJ   ','       '/), (/ 0.0, 0.00280, 0.0    /) ),   ! Titanium
     &   dust_type( (/'       ','AMNJ   ','       '/), (/ 0.0, 0.00115, 0.0    /) ),   ! Manganese
     &   dust_type( (/'       ','AH2OJ  ','       '/), (/ 0.0, 0.00541, 0.0    /) ),   ! Water
     &   dust_type( (/'       ','AOTHRJ ','       '/), (/ 0.0, 0.48319, 0.0    /) ),   ! Other
     &   dust_type( (/'       ','       ','ASOIL  '/), (/ 0.0, 0.0, 0.95995    /) ) /) ! Non-Anion Dust

C Sea-Salt Speciation factors
       Real, Allocatable, Save  :: aerospc_ssf( :, : )

C aero_subs:VOLINORG parameters
C what is 0.04642D0 and 0.00325D0?
C     SOILwVOLS = REAL( AEROSPC_CONC( ASOIL_IDX,IMODE ), 8 ) / ( 1.0D0 - 0.04642D0 )  ! ASOIL_IDX = 38
C     PMCwVOLS  = REAL( AEROSPC_CONC( ACORS_IDX,IMODE ), 8 ) / ( 1.0D0 - 0.00325D0 )  ! ACORS_IDX = 39
C From Heather Simon:
C “PMC profiles for CMAQ” tab, the PMC speciation was listed as follows:
C    PCL       0.01190
C    PNO3      0.00160
C    PSO4      0.02655
C    PH2O      0.00637
C    REMAINDER 0.95358
C If you add up PCL + PNO3 + PSO4 + PH2O you get the 0.04642 number.  I didn’t
C double check, but I assume that the 0.00325 matches with the mass fraction of
C volatile anthropogenic PMC components (i.e. sulfate, nitrate, chlorine).  So it
C appears that Prakash (Bhave) did indeed renormalize the metal concentration to take
C into account that fact the ACORS and ASOIL only made up a fraction of the emitted
C PMC mass (dust and anthropogenic).  Also note, that PH2O may have been dropped in
C the dust emissions because that species is not transported in the model, rather
C H2O is recalculated at every time step in VOLINORG and is dependent on RH and PM
C composition so emitted mass doesn’t matter.

      Real( 8 ), Parameter :: asoil_renorm = 1.0D0 - 0.04642D0

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


      Real( 8 ), Parameter :: asoil_fe_fac = 0.0281D0    ! for FE in dust
      Real( 8 ), Parameter :: acors_fe_fac = 0.0467D0    ! for FE in anthropogenic coarse

      Real( 8 ), Parameter :: asoil_mn_fac = 0.00078D0   ! for MN in dust
      Real( 8 ), Parameter :: acors_mn_fac = 0.0011D0    ! for MN in anthropogenic coarse


C AERO_EMIS - Prakash Bhave noted Coarse mode PMC speciation:
C *** PVB: temporary hack to speciate PMC
C        SPFC_ACORS = ( 1.0 - 0.00325 ) * AEROSPC( ACORS_IDX )%EMIS_SPLIT( N_MODE )
C        SPFC_ASO4  = 0.00100 * AEROSPC( ASO4_IDX )%EMIS_SPLIT( N_MODE )
C        SPFC_ANO3  = 0.00048 * AEROSPC( ANO3_IDX )%EMIS_SPLIT( N_MODE )
C        SPFC_ACL   = 0.00145 * AEROSPC( ACL_IDX )%EMIS_SPLIT( N_MODE )
C        SPFC_AH2O  = 0.00032 * AEROSPC( AH2O_IDX )%EMIS_SPLIT( N_MODE )
C If you add up SPFC_ASO4 + SPFC_ANO3 + SPFC_ACL + SPFC_AH2O you get the 0.00325 number.

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
      Character( 16 ), Private, Parameter :: req_h2o    = 'AH2O'
      Character( 16 ), Private, Parameter :: req_h3op   = 'AH3OP'
      Character( 16 ), Private, Parameter :: req_soil   = 'ASOIL'
      Character( 16 ), Private, Parameter :: req_cors   = 'ACORS'
      Character( 16 ), Private, Parameter :: req_ncom   = 'APNCOM'
      Character( 16 ), Private, Parameter :: req_seacat = 'ASEACAT'
      Character( 16 ), Private, Parameter :: req_phgj   = 'APHGJ'
    
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
      Integer :: ah2o_idx 
      Integer :: ah3op_idx 
      Integer :: asoil_idx
      Integer :: acors_idx
      Integer :: apncom_idx
      Integer :: aseacat_idx
      Integer :: aphgj_idx

      Real, Allocatable :: aerospc_mw( : )     ! aero species M.W. (from AE_SPC.EXT) [ g/mol ]
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
C                    number    surface  minimum minimum minimum
C                     name      name    numconc  m2conc  m3conc
C                   --------   -------   ------  ------  ------
     &   mode_type('NUMATKN', 'SRFATKN', conmin, conmin, conmin),
     &   mode_type('NUMACC ', 'SRFACC ', conmin, conmin, conmin),
     &   mode_type('NUMCOR ', 'SRFCOR ', conmin, conmin, conmin)/)

      Real, Parameter :: min_sigma_g = 1.05
      Real, Parameter :: max_sigma_g = 2.50
      Real, Parameter :: def_sigma_g( n_mode ) = (/ 1.70, 2.0, 2.2 /)       ! default background sigma-g for each mode
      Real, Parameter :: def_diam( n_mode ) = (/ 1.0E-8, 7.0E-8, 1.0E-6 /)  ! default background mean diameter for each mode

      Real            :: moment0_conc( n_mode )     ! 0th moment concentration
      Real            :: moment2_conc( n_mode )     ! 2nd moment concentration
      Real            :: moment3_conc( n_mode )     ! 3rd moment concentration

C Mass concentration (calculated by GETPAR)
      Real :: aeromode_mass( n_mode )   ! [ ug/m^3 ]

C Particle density (calculated by GETPAR)
      Real :: aeromode_dens( n_mode )   ! [ kg/m^3 ]

C Geometric mean diameter (calculated by GETPAR)
      Real :: aeromode_diam( n_mode )   ! [ m ]

C Log of geometric standard deviation (calculated by GETPAR )
      Real :: aeromode_sdev( n_mode )

C Minimum number (calculated in map_aero routine)
      Real :: aeromode_minNum( n_mode )

C Minimum 2nd moment (calculated in map_aero routine)
      Real :: aeromode_minM2( n_mode )

C Mapping for loading from and unloading to CGRID array
      Integer, Allocatable :: aerospc_map( :, :)  ! indices of aero species to CGRID
      Integer :: aeronum_map( n_mode )      ! indices of aero number variable to CGRID
      Integer :: aerosrf_map( n_mode )      ! indices of aero surf area variable to CGRID

C Missing aerosol species map
      Logical, Allocatable :: aero_missing( :,: )  ! indices of aero species to CGRID

C Emissions mapping
      Integer              :: n_emis_pm               ! number of aerospc with emissions
      Integer, Allocatable :: pmem_map( : )           ! mapping to aerospc array for PM emissions
      Character( 16 )      :: pmem_units              ! units for PM emissions for all species

      Logical, Save :: V51_SOA_mechanism   = .False. !

C Private variables for loading from and unloading to CGRID array
      Logical, Private, Save :: mapped = .False.
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
C-----------------------------------------------------------------------

      Use rxns_data             ! chemical mechanism data
      Use cgrid_spcs            ! CGRID mechanism species
      Use aeromet_data
      Use utilio_defn

      Implicit None

C Local Variables:
      Character( 120 ) ::  msg
      Character( 120 )  :: xmsg
      Integer l, m, n, spc
      Real  so4fac
      Real  anthfac

      Real    :: mole_weight( n_aerolist )
      Integer :: spc_map( n_aerolist, n_mode )
      Integer :: aerolist_map( n_aerolist )
      Logical :: tracer_present, error_flag
      Character(16) :: tracer_names( n_mode )

      logdev = setup_logdev()
      mole_weight = 0.0

      if ( mapped ) Return

         n_aerospc    = 0 
         aerolist_map = 0
         error_flag   = .False.
         spc_map      = 0

C build mapping to CGRID for each aero spc
         Do spc = 1, n_aerolist
            tracer_present = .false.
            tracer_names   = '                '

!           Write( logdev,9997 ) spc, ( m, aerolist( spc )%name( m ), m= 1, n_mode )

            Do m = 1, n_mode
               If ( aerolist( spc )%name( m )(1:1) .Ne. ' ' ) Then
                  n = index1( aerolist( spc )%name( m ), n_ae_spc, ae_spc )
                  If ( n .Eq. 0 ) Then
                     If ( .Not. aerolist( spc )%tracer ) Then
                        xmsg = 'FATAL ERROR: Required Species '
     &                       // aerolist( spc )%name( m ) 
     &                       // ' in aerospc name is not in AE_SPC'
                        Call m3warn( pname, 0, 0, xmsg)
                        error_flag = .true.
                     Else
                        xmsg =  'Optional Tracer Species '
     &                       // aerolist( spc )%name( m ) 
     &                       // ' in aerolist name is not in AE_SPC. '
                        Call m3mesg( xmsg )
                     End If
                  Else
                     spc_map( spc,m ) = ae_strt - 1 + n
                     If ( aerolist( spc )%tracer ) Then
                        tracer_present = .true.
                        tracer_names( m ) = aerolist( spc )%name( m )
                     End If
                     If ( mole_weight( spc ) .Lt. 0.5 ) Then  ! mw=0 means a new species
                        mole_weight( spc ) = ae_molwt( n )
                     Else If ( mole_weight( spc ) .Ne. ae_molwt( n ) ) Then
                        xmsg = 'molecular weight of ' // Trim( aerolist( spc )%name( m ) )
     &                       // ' is different from that of the same species'
     &                       // ' in the same or another mode.'
!                       Write( logdev,* ) mole_weight( spc ), ae_molwt( n )
                        Call m3warn( pname, 0, 0, xmsg )
                        Write( xmsg,* ) 'New Value(', n, ') = ', ae_molwt( n ),
     &                                  'Expected value(', spc, ')= ', mole_weight( spc )
                        Call m3warn( pname, 0, 0, xmsg )
                        error_flag = .true.
                     End If
                  End If
               End If
            End Do
!           Write( logdev,* ) pname,': error flag = ',error_flag
            If ( .Not. aerolist( spc )%tracer ) Then
               n_aerospc = n_aerospc + 1
               aerolist_map( n_aerospc ) = spc
!              l = aerolist_map( n_aerospc )
!              Write( logdev,* ) n_aerospc, l, aerolist( l )
            Else If ( .Not. tracer_present ) Then
               xmsg = 'The above linked set of tracers will not be simulated.'
!              Call m3mesg( xmsg )
               Cycle
            Else If ( tracer_present ) Then
               Write( msg,* ) (' ', TRIM( aerolist( spc )%name( m ) ), ' ', m = 1, n_mode )
               Do m = 1, n_mode
                  If ( aerolist( spc )%name( m ) .Ne. tracer_names( m ) ) then
                     error_flag = .true.
                     Write( xmsg,* ) 'FATAL ERROR: Linked set of tracers, ',
     &                    TRIM( msg ),', is missing species, ',
     &                    TRIM( aerolist( spc )%name( m ) )
                     call  m3mesg( xmsg ) 
                     CYCLE
                  End if
               End do
               If ( .Not. error_flag ) then
                  n_aerospc = n_aerospc + 1
                  aerolist_map( n_aerospc ) = spc
!                 l = aerolist_map( n_aerospc )
!                 Write( logdev,* ) n_aerospc, l, aerolist( l )
               End If
            End If
!           Write( logdev,'( 2(i3,1X) )' ) n_aerospc, aerolist_map( n_aerospc )
         End Do
         
!         Write( logdev,'(i3)' ) aerolist_map( 1:n_aerospc )
         
         xmsg = ' Above Fatal Error(s) found '
         If ( error_flag ) Call m3exit( pname, 0, 0, xmsg, xstat3 )

         Allocate( aerospc     ( n_aerospc ) )
         Allocate( aerospc_mw  ( n_aerospc ) )
         Allocate( pmem_map    ( n_aerospc ) )
         Allocate( aerospc_map ( n_aerospc, n_mode ) )
         Allocate( aerospc_conc( n_aerospc, n_mode ) )
         Allocate( aerospc_ssf ( n_mode, n_aerospc ) )
         Allocate( aero_missing( n_aerospc, n_mode ) )
         
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               If ( aerospc( spc )%name( m ) .Ne. ' ' ) Then
                  aero_missing( spc,m ) = .False.
               Else
                  aero_missing( spc,m ) = .True.
               End If
            End Do
         End Do
         
#ifdef verbose_aero         
         Write( logdev,9998 ) "SPC", "TOT", "LST", "  AITKEN  ", "  ACUMM   ",
     &                        "   COAR   ", "  MW  ", "MAP", "ESPLIT", "SSFACT"
#endif
     
         Do spc = 1, n_aerospc
            l = aerolist_map( spc )
            aerospc( spc ) = aerolist( l )
!           Write( logdev,* ) n_aerospc, spc, l, aerolist( l )
            aerospc_mw( spc ) = mole_weight( l )
            Do n = 1, n_mode
               aerospc_map( spc, n ) = spc_map( l, n )
               aerospc_ssf( n, spc ) = aerospc( spc )%ssf_emis( n )
            End do
#ifdef verbose_aero
            Write( logdev,9999 )spc, n_aerospc, l,
     &           ( aerospc( spc )%name( n ), n = 1, n_mode ),
     &             aerospc_mw( spc ),
     &           ( aerospc_map( spc,n ), n = 1, n_mode ),
     &           ( aerospc( spc )%emis_split( n ), n = 1, n_mode ),
     &           ( aerospc_ssf( n,spc ), n = 1, n_mode )
#endif
         End do

C Build mapping to CGRID for aero # and surf area variables
      aeronum_map = 0
      aerosrf_map = 0

      Do m = 1, n_mode
         n = index1( trim( aeromode( m )%num_name ), n_ae_spc, ae_spc )
         If ( n .Eq. 0 ) Then
            xmsg = 'Species ' // Trim( aeromode( m )%num_name )
     &           //' in aeronum name is not in AE_SPC'
            Call m3exit( pname, 0, 0, xmsg, xstat3 )
         Else
            aeronum_map( m ) = ae_strt - 1 + n
         End If

         n = index1( trim( aeromode( m )%srf_name ), n_ae_spc, ae_spc )
         If ( n .Eq. 0 ) Then
            xmsg = 'species ' // Trim( aeromode( m )%srf_name )
     &           // ' in aerosrf name is not in AE_SPC'
            Call m3exit( pname, 0, 0, xmsg, xstat3 )
         Else
            aerosrf_map( m ) = ae_strt - 1 + n
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
      apoc_idx    = findAero( req_poc,    .True. )
      ah2o_idx    = findAero( req_h2o,    .True. )
      ah3op_idx   = findAero( req_h3op,   .True. )
      asoil_idx   = findAero( req_soil,   .True. )
      acors_idx   = findAero( req_cors,   .True. )
      apncom_idx  = findAero( req_ncom,   .True. )
      aseacat_idx = findAero( req_seacat, .True. )
      aphgj_idx   = findAero( req_phgj,  .False. )


#ifdef verbose_aero
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
      Write( logdev,'( 5x, a, i4 )' ) 'aphgj_idx:  ', aphgj_idx
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
         aeromode_minM2( m ) = aeromode_minNum( m ) * def_diam( m )**2 *
     &            Exp( 2.0 * Log( def_sigma_g( m ) )**2 )
      End do
 
      mapped = .True.

      Write( logdev,'( 5x, a )' ) ' --- Aero Species Mapped ---' 

9997  Format( i3, 1x, 3( i1, 1x, a16, 1x) )
9998  Format( 3a4, 3a17, a7, "    ", a4, "     ", 2("        ",a7,"        ") )
9999  Format( 3(i3, 1x), 3(a16, 1x), f6.2, 1x, 3(i3, 1x), 6(f7.3, 1x) )

      Return
      End Subroutine map_aero

C-----------------------------------------------------------------------
      Subroutine map_pmemis ( )
C
C  Set the emissions units from the header of EMIS_1 file
C  Verify that all the units on the file are consistent

      Use utilio_defn

      Implicit None

      Include SUBST_FILES_ID  ! file name parameters

C Parameters:
      Character( 10 ), Parameter :: blank10 = ' '

C Local Variables:
      Character( 16 ), Save :: pname = 'map_pmemis'
      Character( 512 )      :: xmsg1
      Character( 1024 )     :: xmsg2
      Character( 10 )       :: units

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

      n_emis_pm = 0
      pmem_units = 'null'
      found = .True.
      match = .True.
      xmsg1 = 'Could not find the following species in emissions file'
      xmsg2 = 'PM Units not uniform in EMIS_1 file.'

      Do v = 1, n_aerospc
         If ( aerospc( v )%emis( 1:1 ) .Ne. ' ' ) Then
            indx = index1( trim(aerospc( v )%emis ), nvars3d, vname3d )
            If ( indx .Le. 0 ) Then
               xmsg1 = Trim( xmsg1 ) // crlf() // blank10
     &               // Trim( aerospc( v )%emis )
               found = .False.
               Cycle
            End If

            n_emis_pm = n_emis_pm + 1
            pmem_map( n_emis_pm ) = v

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
         Call m3exit( pname, 0, 0, Trim( xmsg1 ), xstat2 )
      End If

      If ( .Not. match ) Then
         Call m3exit( pname, 0, 0, Trim( xmsg2 ), xstat2 )
      End If

#ifdef verbose_aero
      Write( logdev,'( /5x, a )' ) 'pmem_map to aerospc' 
         Do v = 1, n_emis_pm
         Write( logdev,'( 5x, a, 2i4, 2x, a )' ) 'pmem_map:', v, pmem_map( v ), aerospc( pmem_map( v ) )%emis
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
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

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

C Copy grid cell concentrations of aero # and surf area
C Convert and assign to moment0_conc and moment2_conc

      moment0_conc = 0.0
      moment2_conc = 0.0

      If ( minchk ) Then
         Do m = 1, n_mode
            n = aeronum_map( m )
            moment0_conc( m ) = Max( conc( n ), aeromode( m )%min_numconc )
            n = aerosrf_map( m )
            moment2_conc( m ) = Max( conc( n ), aeromode( m )%min_m2conc )
         End Do
      Else
         Do m = 1, n_mode
            n = aeronum_map( m )
            moment0_conc( m ) = conc( n )
            n = aerosrf_map( m )
            moment2_conc( m ) = conc( n )
         End Do
      End If

      Return
      End Subroutine extract_aero

C-----------------------------------------------------------------------
      Subroutine update_aero( conc, minchk )

C  Updates conc from the AERO_DATA:aerospc_conc array.
C  The original idea is that the data in conc updates CGRID
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Use aeromet_data      ! fundamental constants, data type definitions, etc.
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

C Find the substring vname in aerospc( spc )%name( n )
      Do n = 1, n_mode
         Do spc = 1, n_aerospc
            If ( Index( aerospc( spc )%name( n ), Trim( vname ) ) .Gt. 0 ) Then
               idx = spc
               Return
            End If
         End Do
      End Do

      If ( .Not. required ) Then
         idx = 0
         xmsg =  pname // 'Optional Species '
     &       // Trim( vname ) // ' Not found in aerospc names array'
         call m3mesg( xmsg )
         Return
      End If

      xmsg = 'Required Species ' // Trim( vname ) // ' Not found in aerospc names array'
      Call m3exit( pname, 0, 0, xmsg, xstat3 )

      Return
      End Function findAero

      End Module aero_data

