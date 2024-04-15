
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
      Module soa_defn

C  Defines aerosol species arrays and parameters required in SOA processing.

C  Contains:
C     Subroutine extract_soa
C     Subroutine update_orgvapor
C     Function findVapor
C     Function findOrgprod
C     Subroutine orgaer

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C HP  03/11/11 Updated monoterpene SOA alphas and Cstars to Carlton et al. 2010 values
C HP  07/24/11 Changed aromatic SOA alphas for consistency with updated reaction counters
C               BNZ, TOL, XYL numbers now match Ng et al. 2007 Atmos. Chem. Phys.
C 08 Jun 12 J.Young: remove full character blank padding for GNU Fortran (GCC) 4.1.2
C 13 Aug 13 H. Pye: Xylene and toluene low-NOx yields switched. Values now
C               follow experimental data of Ng et al. 2007 ACP as shown in Table 3.
C               Values in Table 6 of Ng et al. (previously used) are incorrect.
C 18 Dec 13 G.Sarwar: added orgprod parent names based on RACM2
C 07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C 21 Jul 14 B.Hutzell: used ifdef statement to make oligomerization
C                      optional because process represented in chemical
C                      mechanism
C 26 Sep 14 H. Pye: Added isoprene + NO3 SOA (see mech.def, no changes
C                   here). When IEPOX uptake present in gas phase for
C                   cb05e51, replace preivous acid enhanced isoprene SOA
C                   with IEPOX uptake SOA now handled as a heterogeneous
C                   rxn. For saprc07tic_ae6i, perform more detailed
C                   IEPOX and MAE uptake and do not do Carlton et al. 2010
C                   acid enhancement. Note that saprc07tic_ae6i is a research
C                   version and it is unclear how duplicative AISO1+AISO2+their oligomers
C                   are with  IEPOX+MAE uptake and their oligomers.
C                   Both pathways occur with sarpc07tic_ae6i. To turn AISO1+AISO2
C                   oligomers off, set "Decay" in oaspc to 0.0 for SV_ISO1/2.
C 27 Sep 14 H. Pye: Added alkane and PAH SOA (Pye and Pouliot 2012 ES&T)
C 15 Jul 15 G. Sarwar: updated SOA from alkane, PAH, and isoprene for RACM2
C 03/03/16  D. Luecken: added capability for CB6
C 24 Mar 16 G. Sarwar: updated for CB05EH51
C    May 16 B. Murphy, H. Pye: updated treatment of aerosol moments
C    Jan 18 H. Pye: updated monoterpene photoxidation SOA: Xu et al. 2018 ACPD
C                   AMT1-7 will replace ATRP1-2 when fully implemented
C                   across mechanisms. 
C    Aug 18 M. Qin, H. Pye: Removed oligomerization option here. Oligomerization
C                   must be done in gas chemistry (mech.def). Added anthropogenic
C                   SOA condensation for aero7.
C   29 Aug 18 G. Sarwar: updated for CB6R3M
C 17 Jan 20 B.Hutzell -Added algorithm to handle tracer species and
C                     -added benzo[a]pyrene to oa_list. Its cstar is based
C                      on the subcooled vapor pressure in Lei et. al (2002)
C   15 Sep 23 G. Sarwar: updated for CRACMM2
C 28 Nov 23 N. Skipper: added new species for CRACMM2
C-----------------------------------------------------------------------
#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
      Use aero_ddm3d, ONLY : init_aero_ddm3d, ae_ddm3d_ready, og_sens, rog_dsens
#endif 
      Implicit None

      ! Define Logical values as T and F for the OA table
      Logical, Parameter, Private :: T = .true.
      Logical, Parameter, Private :: F = .false.

      Integer, Parameter :: n_oa_list = 85   ! # of potential partitioning SVOCs
      Integer, Save      :: n_oa             ! Total # of simulated SOA vapors
      Integer, Save      :: n_oa_NotTracers  ! # of simulated SOA vapors that are not tracers
      Integer, Save      :: n_oa_tracers      ! # of simulated SOA vapors that are tracers

      Type oa_type
         Character( 16 ) :: name        ! Organic Aerosol Species Name
         Character( 16 ) :: gas_name    ! Condensable Vapor Species Name
         Character( 16 ) :: ctr_name    ! Reaction Counter Name
         Real            :: alpha       ! Mass-based stoichiometric coefficients [ug/m^3]/[ug/m^3]
         Real            :: cstar       ! Effective saturation concentrations [ug/m^3] at 298 K
         Real            :: enth        ! Enthalphy of Vaporization [J/mol]
         Real            :: otoc        ! O:C - Ratio of Oxygen and to Carbon
         Real            :: OMtoOC      ! OM:OC - Ratio of Total Organic Mass to Organic Carbon Mass
         Logical         :: primary     ! Flag identifying primary species
         Logical         :: anthro      ! Flag identifying anthropogenic species
         Logical         :: biog        ! Flag identifying biogenic species
         Logical         :: nonvol      ! Flag identifying nonvolatile species
      End Type oa_type

      Type( oa_type ), Allocatable, Save :: oaspc( : )

      Type( oa_type ), Save :: oa_list( n_oa_list ) = (/
      !                                                                                  ANTHROPOGENIC
      !                                                                                      | BIOGENIC
      !         PM          Vapor       Rxn Cntr     Alpha   CStar^    Enth-   O:C OM:OC     |   |   Nonvol-     
      !         Name        Name        Name*                           alpy             POA |   |   atile        
      !        ----------  ----------  ----------   ------  --------  -------  --- ----- --- --- --- ---    
     & oa_type('AALK1  ', 'SVALK1  ', 'ALKRXN  ',  0.0334,   0.1472,  53.0E3, .315,1.56, F,  T,  F,  F ),
     & oa_type('AALK2  ', 'SVALK2  ', 'ALKRXN  ',  0.2164,  51.8775,  53.0E3, .203,1.42, F,  T,  F,  F ),
     & oa_type('AXYL1  ', 'SVXYL1  ', 'XYLNRXN ',  0.0310,   1.3140,  32.0E3,1.002,2.42, F,  T,  F,  F ),
     & oa_type('AXYL2  ', 'SVXYL2  ', 'XYLNRXN ',  0.0900,  34.4830,  32.0E3, .611,1.93, F,  T,  F,  F ),
     & oa_type('ATOL1  ', 'SVTOL1  ', 'TOLNRXN ',  0.0580,   2.3260,  18.0E3, .875,2.26, F,  T,  F,  F ),
     & oa_type('ATOL2  ', 'SVTOL2  ', 'TOLNRXN ',  0.1130,  21.2770,  18.0E3, .523,1.82, F,  T,  F,  F ),
     & oa_type('ABNZ1  ', 'SVBNZ1  ', 'BNZNRXN ',  0.0720,   0.3020,  18.0E3,1.211,2.68, F,  T,  F,  F ),
     & oa_type('ABNZ2  ', 'SVBNZ2  ', 'BNZNRXN ',  0.8880, 111.1100,  18.0E3, .851,2.23, F,  T,  F,  F ),
     & oa_type('APAH1  ', 'SVPAH1  ', 'PAHNRXN ',  0.2100,   1.6598,  18.0E3, .371,1.63, F,  T,  F,  F ),
     & oa_type('APAH2  ', 'SVPAH2  ', 'PAHNRXN ',  1.0700, 264.6675,  18.0E3, .259,1.49, F,  T,  F,  F ),
     & oa_type('ATRP1  ', 'SVTRP1  ', 'TRPRXN  ',  0.1393,  14.7920,  40.0E3, .539,1.84, F,  F,  T,  F ),
     & oa_type('ATRP2  ', 'SVTRP2  ', 'TRPRXN  ',  0.4542, 133.7297,  40.0E3, .531,1.83, F,  F,  T,  F ),
     & oa_type('AMT1   ', 'SVMT1   ', 'TRPRXN  ',   0.040,    0.010, 102.0E3, .400,1.67, F,  F,  T,  F ),
     & oa_type('AMT2   ', 'SVMT2   ', 'TRPRXN  ',   0.032,    0.100,  91.0E3, .400,1.67, F,  F,  T,  F ),
     & oa_type('AMT3   ', 'SVMT3   ', 'TRPRXN  ',   0.032,    1.000,  80.0E3, .444,1.72, F,  F,  T,  F ),
     & oa_type('AMT4   ', 'SVMT4   ', 'TRPRXN  ',   0.103,   10.000,  69.0E3, .300,1.53, F,  F,  T,  F ),
     & oa_type('AMT5   ', 'SVMT5   ', 'TRPRXN  ',   0.143,  100.000,  58.0E3, .333,1.57, F,  F,  T,  F ),
     & oa_type('AMT6   ', 'SVMT6   ', 'TRPRXN  ',   0.285, 1000.000,  47.0E3, .200,1.40, F,  F,  T,  F ),
     & oa_type('AMT7   ', 'SVMT7   ', 'TRPRXN  ',   0.160,10000.000,  36.0E3, .222,1.43, F,  F,  T,  F ),
     & oa_type('AHOM   ', 'HOM     ', '        ',     0.0,  6.3E-03, 130.0E3, .700,2.08, F,  F,  T,  F ),
     & oa_type('AELHOM ', 'ELHOM   ', '        ',     0.0,  1.0E-05, 160.0E3, .400,1.67, F,  F,  T,  F ),
     & oa_type('AMTNO3 ', 'MTNO3   ', '        ',     0.0,     12.0,  40.0E3, .587,1.90, F,  F,  T,  F ),
     & oa_type('AISOPNN', 'ISOPNN  ', '        ',     0.0,     8.9,   40.0E3,2.107,3.80, F,  F,  T,  F ),
     & oa_type('AMTHYD ', '        ', '        ',     0.0,   1.E-10,   1.0E0, .299,1.54, F,  F,  T,  T ),
     & oa_type('AIETET ', '        ', '        ',     0.0,   1.E-10,   1.0E0, .883,2.27, F,  F,  T,  T ),
     & oa_type('AIEOS  ', '        ', '        ',     0.0,   1.E-10,   1.0E0,1.947,3.60, F,  F,  T,  T ),
     & oa_type('ADIM   ', '        ', '        ',     0.0,   1.E-10,   1.0E0, .723,2.07, F,  F,  T,  T ),
     & oa_type('AIMGA  ', '        ', '        ',     0.0,   1.E-10,   1.0E0,1.067,2.50, F,  F,  T,  T ),
     & oa_type('AIMOS  ', '        ', '        ',     0.0,   1.E-10,   1.0E0,2.403,4.17, F,  F,  T,  T ),
     & oa_type('AISO3NOS','        ','         ',     0.0,   1.e-10,   1.0E0,  0.8,2.27, F,  F,  T,  T ),
     & oa_type('AISO3OS ','        ','         ',     0.0,   1.e-10,   1.0E0,  1.4,3.60, F,  F,  T,  T ),
     & oa_type('AISO1  ', 'SVISO1  ', 'ISOPRXN ',  0.2320, 116.0100,  40.0E3, .827,2.20, F,  F,  T,  F ),
     & oa_type('AISO2  ', 'SVISO2  ', 'ISOPRXN ',  0.0288,   0.6170,  40.0E3, .851,2.23, F,  F,  T,  F ),
     & oa_type('AISO3  ', '        ', '        ',     0.0,   1.e-10,   1.0E0,1.307,2.80, F,  F,  T,  T ),
     & oa_type('ASQT   ', 'SVSQT   ', 'SESQRXN ',  1.5370,  24.9840,  40.0E3, .283,1.52, F,  F,  T,  F ),
     & oa_type('AGLY   ', '        ', '        ',     0.0,   1.E-10,   1.0E0, .771,2.13, F,  F,  F,  T ),
     & oa_type('AORGC  ', '        ', '        ',     0.0,   1.E-10,   1.0E0, .667,2.00, F,  F,  F,  T ),
     & oa_type('AXYL3  ', '        ', 'XYLHRXN ',  0.3600,   1.e-10,   1.0E0, .907,2.30, F,  T,  F,  T ),
     & oa_type('ATOL3  ', '        ', 'TOLHRXN ',  0.3000,   1.e-10,   1.0E0,1.227,2.70, F,  T,  F,  T ),
     & oa_type('ABNZ3  ', '        ', 'BNZHRXN ',  0.3700,   1.e-10,   1.0E0,1.467,3.00, F,  T,  F,  T ),
     & oa_type('APAH3  ', '        ', 'PAHHRXN ',  0.7300,   1.e-10,   1.0E0, .483,1.77, F,  T,  F,  T ),
     & oa_type('APOC   ', '        ', '        ',  0.0   ,   1.e-10,   1.0E0, .000,1.00, T,  T,  F,  T ),
     & oa_type('APNCOM ', '        ', '        ',  0.0   ,   1.e-10,   1.0E0, -1.0,-1.0, T,  T,  F,  T ),
     & oa_type('APCSO  ', 'LVPCSOG ', 'PCSOARXN',  1.0   ,   1.e-05,  40.0E3, .667,2.00, F,  T,  F,  F ),
     & oa_type('ALVPO1 ', 'VLVPO1  ', '        ',  0.0000,    1.e-1,  96.0E3, .185,1.39, T,  T,  F,  F ),
     & oa_type('ASVPO1 ', 'VSVPO1  ', '        ',  0.0000,    1.e+0,  85.0E3, .123,1.32, T,  T,  F,  F ),
     & oa_type('ASVPO2 ', 'VSVPO2  ', '        ',  0.0000,    1.e+1,  74.0E3, .073,1.26, T,  T,  F,  F ),
     & oa_type('ASVPO3 ', 'VSVPO3  ', '        ',  0.0000,    1.e+2,  63.0E3, .032,1.21, T,  T,  F,  F ),
     & oa_type('AIVPO1 ', 'VIVPO1  ', '        ',  0.0000,    1.e+3,  52.0E3, .000,1.17, T,  T,  F,  F ),
     & oa_type('ALVOO1 ', 'VLVOO1  ', '        ',  0.0000,    1.e-2, 107.0E3, .886,2.27, F,  T,  F,  F ),
     & oa_type('ALVOO2 ', 'VLVOO2  ', '        ',  0.0000,    1.e-1,  96.0E3, .711,2.06, F,  T,  F,  F ),
     & oa_type('ASVOO1 ', 'VSVOO1  ', '        ',  0.0000,    1.e+0,  85.0E3, .567,1.88, F,  T,  F,  F ),
     & oa_type('ASVOO2 ', 'VSVOO2  ', '        ',  0.0000,    1.e+1,  74.0E3, .447,1.73, F,  T,  F,  F ),
     & oa_type('ASVOO3 ', 'VSVOO3  ', '        ',  0.0000,    1.e+2,  63.0E3, .345,1.60, F,  T,  F,  F ),
     & oa_type('AAVB1  ', 'SVAVB1  ', '        ',  0.0000,    0.010,  18.0E3,1.227,2.70, F,  T,  F,  F ),
     & oa_type('AAVB2  ', 'SVAVB2  ', '        ',  0.0000,    1.000,  18.0E3, .947,2.35, F,  T,  F,  F ),
     & oa_type('AAVB3  ', 'SVAVB3  ', '        ',  0.0000,   10.000,  18.0E3, .803,2.17, F,  T,  F,  F ),
     & oa_type('AAVB4  ', 'SVAVB4  ', '        ',  0.0000,  100.000,  18.0E3, .659,1.99, F,  T,  F,  F ),
     & oa_type('AOLGA  ', '        ', '        ',  0.0000,   1.e-10,   1.0E0,1.067,2.50, F,  T,  F,  T ),
     & oa_type('AOLGB  ', '        ', '        ',  0.0000,   1.e-10,   1.0E0, .747,2.10, F,  F,  T,  T ),
     & oa_type('AOP3   ', 'OP3     ', '        ',  0.0000,    1.e+1,  81.0E3, .600,1.92, F,  T,  F,  F ),
     & oa_type('ASOAT  ', '        ', '        ',  0.0000,  1.0E-10,   1.0E0,0.857,2.31, F,  T,  F,  T ),
     & oa_type('AROCN2ALK','VROCN2ALK','        ', 0.0000,    1.e-2, 104.0E3, .000,1.39, T,  T,  F,  F ),
     & oa_type('AROCN1ALK','VROCN1ALK','        ', 0.0000,    1.e-1,  96.0E3, .000,1.32, T,  T,  F,  F ),
     & oa_type('AROCP0ALK','VROCP0ALK','        ', 0.0000,    1.e+0,  85.0E3, .000,1.17, T,  T,  F,  F ),
     & oa_type('AROCP1ALK','VROCP1ALK','        ', 0.0000,    1.e+1,  81.0E3, .000,1.17, T,  T,  F,  F ),
     & oa_type('AROCP2ALK','VROCP2ALK','        ', 0.0000,    1.e+2,  77.0E3, .000,1.17, T,  T,  F,  F ),
     & oa_type('AROCP3ALK','VROCP3ALK','        ', 0.0000,    1.e+3,  73.0E3, .000,1.17, T,  T,  F,  F ),
     & oa_type('AROCN2OXY2','VROCN2OXY2','        ',0.0000,   1.e-2,  93.0E3, .200,1.42, F,  T,  F,  F ),
     & oa_type('AROCN2OXY4','VROCN2OXY4','        ',0.0000,   1.e-2,  93.0E3, .400,1.67, F,  T,  F,  F ),
     & oa_type('AROCN2OXY8','VROCN2OXY8','        ',0.0000,   1.e-2,  93.0E3, .800,2.17, F,  T,  F,  F ),
     & oa_type('AROCN1OXY1','VROCN1OXY1','        ',0.0000,   1.e-1,  89.0E3, .100,1.29, F,  T,  F,  F ),
     & oa_type('AROCN1OXY3','VROCN1OXY3','        ',0.0000,   1.e-1,  89.0E3, .300,1.54, F,  T,  F,  F ),
     & oa_type('AROCN1OXY6','VROCN1OXY6','        ',0.0000,   1.e-1,  89.0E3, .600,1.92, F,  T,  F,  F ),
     & oa_type('AROCP0OXY2','VROCP0OXY2','        ',0.0000,   1.e+0,  85.0E3, .200,1.42, F,  T,  F,  F ),
     & oa_type('AROCP0OXY4','VROCP0OXY4','        ',0.0000,   1.e+0,  85.0E3, .400,1.67, F,  T,  F,  F ),
     & oa_type('AROCP1OXY1','VROCP1OXY1','        ',0.0000,   1.e+1,  81.0E3, .100,1.29, F,  T,  F,  F ),
     & oa_type('AROCP1OXY3','VROCP1OXY3','        ',0.0000,   1.e+1,  81.0E3, .300,1.54, F,  T,  F,  F ),
     & oa_type('AROCP2OXY2','VROCP2OXY2','        ',0.0000,   1.e+2,  77.0E3, .200,1.42, F,  T,  F,  F ),
     & oa_type('AROCP3OXY2','VROCP3OXY2','        ',0.0000,   1.e+3,  73.0E3, .200,1.42, F,  T,  F,  F ),
     & oa_type('ABENAPY', 'BENAPY  ', '        ',  0.0000,   0.0396, 105.0E3, .000,1.00, F,  T,  F,  F ),
     & oa_type('AISO4  ','         ','         ',     0.0,   1.e-10,   1.0E0,  1.2,2.80, F,  F,  T,  T ),
     & oa_type('AISO5  ','         ','         ',     0.0,   1.e-10,   1.0E0,  1.0,2.50, F,  F,  T,  T ),
     & oa_type('ATRPN  ','TRPN     ','         ',     0.0,   1410.0,  89.1E3, .400,1.79, F,  F,  T,  F ),
     & oa_type('AHONIT ','HONIT    ','         ',     0.0,   0.0413, 121.0E3, .700,2.21, F,  F,  T,  F ) /)
      ! Table Comments:
      ! *Reaction Counters are only needed if the vapor species is not formed
      !  directly in the gas-phase chemistry mechanism.
      ! ^Nonvolatile species are given a default C* of 1e-10 ug/m3 and
      !  enthalpy of vaporization equal to 1 kJ/mol.
      ! Cstar for BENAPY or benzo[a]pyrene was based on the DMSO partitioning
      ! coefficient in Shrivastave et al. (2017) PNAS, vol 114, 1246-1251. All
      ! OA corresponds to DMSO but its density corresponds to average density
      ! of CMAQ OA, 1.4 g/cm3

      ! Identify Species Required for Isoprene and Monoterpene Chemistry
      Character( 16 ), Private, Parameter :: req_sviso1  = 'SVISO1'
      Character( 16 ), Private, Parameter :: req_sviso2  = 'SVISO2'
      Character( 16 ), Private, Parameter :: req_aeiso1  = 'AISO1'
      Character( 16 ), Private, Parameter :: req_aeiso2  = 'AISO2'
      Character( 16 ), Private, Parameter :: req_aeiso3  = 'AISO3'
      Character( 16 ), Private, Parameter :: req_aeorgc  = 'AORGC'

      ! Variables for Carrying Indices of required species
      Integer :: iso1_idx
      Integer :: iso2_idx
      Integer :: aiso1_idx
      Integer :: aiso2_idx
      Integer :: aiso3_idx
      Integer :: aorgc_idx

      ! Variables for saving properties and concentrations of organic
      ! compounds
      !   Molecular Weights
          Real, Allocatable    :: op_mw    ( : )   ! PM species
          Real, Allocatable    :: og_mw    ( : )   ! Vapor Species
          Real, Allocatable    :: rog_mw   ( : )   ! Precursor Species
                                                   ! that are linked to yields
      !   Concentrations
          Real, Allocatable    :: og_conc  ( : )   ! Vapors
          Real, Allocatable    :: rog_dconc( : )   ! Reaction counters

      ! Variables for mapping OA to aerosol and CGRID arrays
      Integer, Allocatable :: map_OAtoAERO( : )    ! organic aerosol pointers to aerospc
      Integer, Allocatable :: map_OGtoCGRID( : )   ! pointers of vapor species to CGRID
      Integer, Allocatable :: map_ROGtoCGRID( : )  ! pointers of SOA parent species to CGRID
      Logical, Allocatable :: lnonvol_oa( : )      ! flag identifying non-volatile OA species 
                                                   ! in aerosol array that are not tracers
      Logical, Allocatable :: l_oa( : )            ! flag identifying OA species 
                                                   ! that are not tracers
      ! Variables for controlling behavior of OA module
      Logical, Private, Save :: mapped              = .False.
      Logical, Private, Save :: RXNS_eflag          = .False. ! error flag for RXNS modules used
      Logical, Private, Save :: OA_eflag            = .False. ! error flag for soa_defn
      Character( 16 ), Private, Save :: pname = 'SOA_DEFN        '

      Contains

C-----------------------------------------------------------------------
#ifdef sens
      Subroutine extract_soa( conc, s_conc, schk )
#else
      Subroutine extract_soa( conc )
#endif

C  Extracts the required soa data from CGRID into the conc array.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C SH  03/10/11 Renamed met_data to aeromet_data
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C HP  09/27/14 alk_factor removed, updated for alkane/PAH SOA.
C              Conversion of reacted alkane to dodecane equivalent
C              is handled in mech.def. A factor of 0.47 is in use and
C              reflects the fact that alkane SOA precursor
C              emissions are dominated by compounds smaller than dodecane.
C BM  02/19/19 Major Revision to simplify the mapping procedure and
C              remove redundat variables.
C-----------------------------------------------------------------------

      Use rxns_data, only: mechname
      Use aero_data, only: AE_eflag, findAero, aerospc_mw, n_aerospc, aerospc
      Use aeromet_data, only: airdens, inv_mwair, min_gasconc
      Use cgrid_spcs, only: n_gc_spc, gc_spc, n_gc_g2ae, gc_g2ae, gc_strt, gc_molwt, gc_g2ae_map, 
     &                      n_nr_spc, nr_spc, n_nr_n2ae, nr_n2ae, nr_strt, nr_molwt, nr_n2ae_map,
     &                      n_ae_spc, ae_spc
      Use runtime_vars, only: logdev
      Use utilio_defn, only: index1, xstat3

      Implicit None

      ! Arguments:
      Real, Intent( In ) :: conc( : )
#ifdef sens
      Real, Intent( In ) :: s_conc( :,: )
      Logical, Intent( In ) :: schk  ! necessary because some routines don't require sensitivity updates
#endif


      ! Local Variables:
      Character( 300 ):: xmsg
      Real            :: gasconv
      Real            :: vtmp
      Integer         :: n, a, g
      Integer         :: spc
      Integer         :: map_OAtoOAlist( 200 )

      ! Map All OA Species and Load Properties
      If ( .Not. mapped ) Then
          mapped = .TRUE.

          ! First, the number and names of present and nontracer OA species must be
          ! determined by checking the AEROSOL Table with the species names
          ! available from the OA_LIST table. 
          n_oa = 0
          Do spc = 1,n_oa_list
             n = findAero( oa_list( spc )%name, .False. )
             If ( n .Gt. 0 ) Then
               If ( aerospc( n )%om .And. .Not. aerospc( n )%tracer ) Then
                  n_oa = n_oa + 1
                  map_OAtoOAlist( n_oa ) = spc
               End If
             End If
          End Do

          ! Place tracers at the end of the list map
          n_oa_tracers = 0
          Do spc = 1,n_oa_list
             n = findAero( oa_list( spc )%name, .False. )
             If ( n .Gt. 0 ) Then
               If ( aerospc( n )%om .And. aerospc( n )%tracer ) Then
                  n_oa = n_oa + 1
                  n_oa_tracers = n_oa_tracers + 1
                  map_OAtoOAlist( n_oa ) = spc
               End If
             End If
          End Do
          
          If ( n_oa .Eq. n_oa_tracers ) Then
              OA_eflag  = .True.
              xmsg =  'FATAL: All SOA species are tracers at least '
     &            //  'one SOA species must not be a tracer.'
              write(logdev,'(a)')Trim( xmsg )           
          End If
          
          n_oa_NotTracers = n_oa - n_oa_tracers

          ! Allocate the OA property and mapping variables now that the
          ! number of active OA species has been determined.
          Allocate ( oaspc  ( n_oa ))
          Allocate ( op_mw  ( n_oa ))
          Allocate ( og_mw  ( n_oa ))
          Allocate ( rog_mw ( n_oa ))
          Allocate ( og_conc( n_oa ))
          Allocate ( rog_dconc( n_oa ))
          Allocate ( map_ROGtoCGRID( n_oa ))
          Allocate ( map_OAtoAERO( n_oa ))
          Allocate ( map_OGtoCGRID( n_oa ))
          Allocate ( lnonvol_oa( n_aerospc ))
          Allocate ( l_oa( n_aerospc ))

#ifdef sens
          Allocate ( og_sens( n_oa,npmax ) )
          Allocate ( rog_dsens( n_oa,npmax ) )
#endif

          ! Initialize and populate key OA variables
          map_OAtoAERO = 0
          op_mw = 200.0
          lnonvol_oa = .false.
          l_oa       = .false.
          Do spc = 1,n_oa
             oaspc( spc ) = oa_list( map_OAtoOAlist( spc ) )
             map_OAtoAERO( spc ) = findAero( oaspc( spc )%name, .False. )
             If( map_OAtoAERO( spc ) .Lt. 1 ) 
     &           write(logdev,99904) Trim( oaspc( spc )%name )
             op_mw( spc ) = aerospc_mw( map_OAtoAERO( spc ) )
             lnonvol_oa( map_OAtoAERo( spc ) ) = ( oaspc( spc )%nonvol .and.
     &                                             .Not. aerospc( map_OAtoAERo( spc ) )%tracer )
             l_oa( map_OAtoAERo( spc ) )       = (  aerospc( map_OAtoAERo( spc ) )%om .and.
     &                                             .Not. aerospc( map_OAtoAERo( spc ) )%tracer )
          End Do

          ! Determine the location of any reaction counter species on the 
          ! GC Namelist. When a match is found, populate the properties
          ! of that counter species.
          map_ROGtoCGRID = 0
          ROG_mw = 200.0
          Do spc = 1, n_oa
            If( oaspc( spc )%ctr_name == '' ) Cycle
            g = index1( oaspc( spc )%ctr_name, n_gc_spc, gc_spc )
            if ( g .ge. 1 ) then
              ! Populate Mapping Vector for the Precursor Species
              map_ROGtoCGRID( spc ) = gc_strt - 1 + g
              ROG_mw( spc ) = gc_molwt( g )
            else
              ! Log an Error
              OA_eflag  = .True.
              write(logdev,99903)Trim( oaspc( spc )%ctr_name ),
     &        Trim( oaspc( spc )%gas_name )
            End If
         End Do

         ! Determine the location on CGRID of condensable gases in 
         ! equilibrium with OA particle species. When a match is found, 
         ! populate the properties of those gas species.
         map_OGtoCGRID = 0
         og_mw = 200.0 ! Default Molecular Weight for all Organic Vapor Species
         Do spc = 1,n_oa
           If( oaspc( spc )%gas_name == '' ) Cycle

           n = index1( oaspc( spc )%gas_name, n_nr_spc, nr_spc )
           If ( n .ge. 1 ) Then
               map_OGtoCGRID( spc ) = nr_strt - 1 + n
               og_mw( spc ) = nr_molwt( n )
           End If

           g = index1( oaspc( spc )%gas_name, n_gc_spc, gc_spc )
           If ( g .ge. 1 ) Then
               map_OGtoCGRID( spc ) = gc_strt - 1 + g
               og_mw( spc ) = gc_molwt( g )
           End If

           If( n .lt. 1 .and. g .lt. 1 ) Then
               OA_eflag  = .True.
               write(logdev,99902)Trim( oaspc( spc )%gas_name ), 
     &         Trim ( oaspc( spc )%name )
           End If
         End Do

         ! Find indices of required species based on mechname
         If( index( mechname,'CRACMM1_AQ' ) .eq. 0 
     &          .and. index( mechname,'CRACMM2') .eq. 0 
     &          .and. index( mechname,'CRACMM1AMORE_AQ') .eq. 0 ) then
             iso1_idx   = findVapor( req_sviso1, .True. )
             iso2_idx   = findVapor( req_sviso2, .True. )
             aiso1_idx  = findAero( req_aeiso1, .True. )
             aiso2_idx  = findAero( req_aeiso2, .True. )
             aiso3_idx  = findAero( req_aeiso3, .True. )
         Else
             iso1_idx   = findVapor( req_sviso1, .False. )
             iso2_idx   = findVapor( req_sviso2, .False. )
             aiso1_idx  = findAero( req_aeiso1, .False. )
             aiso2_idx  = findAero( req_aeiso2, .False. )
             aiso3_idx  = findAero( req_aeiso3, .False. )
         End If

         aorgc_idx  = findAero( req_aeorgc, .True. )

      End If     ! mapping condition

#ifdef sens
      If ( .Not. ae_ddm3d_ready ) Then
         Call init_aero_ddm3d()
      End If
#endif

      ! Copy grid cell concentrations of vapor species
      og_conc   = 0.0
      rog_dconc = 0.0
      gasconv = airdens * inv_mwair ! Compute gas conversion constant
#ifdef sens      
      og_sens   = 0.0
      rog_dsens = 0.0
#endif

      Do spc = 1, n_oa
         n = map_OGtoCGRID( spc )
         If ( n .Ne. 0 ) Then
            vtmp = gasconv * og_mw( spc )
            og_conc( spc ) = Max( conc( n ) * vtmp, min_gasconc )
#ifdef sens 
            If (schk ) Then
               Do np = 1, npmax
                  If ( og_conc( spc ) .eq. min_gasconc ) Then
                     og_sens( spc,np ) = 0.0
                  Else
                     og_sens( spc,np ) = s_conc( np,n ) * vtmp
                  Endif
               End Do
            End If
#endif
         End If
         
         n = map_ROGtoCGRID( spc )
         If ( n .Ne. 0 ) Then
            rog_dconc( spc ) = Max( conc( n ), min_gasconc )
#ifdef sens 
            If (schk ) Then
               Do np = 1, npmax
                  If ( rog_dconc( spc ) .eq. min_gasconc ) Then
                     rog_dsens( spc,np ) = 0.0
                  Else
                     rog_dsens( spc,np ) = s_conc( np,n )
                  Endif
               End Do
            End If
#endif
         End If
      End Do

99902 Format('FATAL: SOA Vapor, ', a, 
     &       ', is not found among the species in '
     &     / 'the NR or GC namelist used. Required for modeling ', a / )
99903 Format( 'FATAL: SOA Reaction Counter, ', a,  
     &        '. is not found among the species',
     &     / 'in GC namelist used. Required for modeling vapor, ', 
     &       a / )
99904 Format(1X,'aerosol product from namelist species: ', a /)
  
      Return
      End Subroutine extract_soa

C-----------------------------------------------------------------------
#ifdef sens
      Subroutine update_orgvapor( conc, s_conc )
#else
      Subroutine update_orgvapor( conc )
#endif

C  Populates CGRID from the conc array with updated SOA values.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C SH  03/10/11 Renamed met_data to aeromet_data
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------

      Use aero_data, only : cond_budget, npf_budget
      Use aeromet_data, only: airdens, inv_mwair, min_gasconc
      Use utilio_defn, only: xstat3, m3exit

      Implicit None

      ! Arguments:
      Real, Intent( Out ) :: conc( : )
#ifdef sens
      Real, Intent( Out ) :: s_conc( :,: )
#endif

      ! Local Variables:
      Character( 80 ) :: xmsg
      Real            :: gasconv
      Real            :: vtmp
      Integer         :: n
      Integer         :: spc

      If ( .Not. mapped ) Then
         xmsg = 'CGRID Species has not been mapped'
         Call m3exit( pname, 0, 0, xmsg, xstat3 )
      End If

      ! Compute gas conversion constant
      gasconv = airdens * inv_mwair

      ! Copy og_conc back to grid cell concentrations
      Do spc = 1, n_oa
         n = map_OGtoCGRID( spc )
         If ( n .Ne. 0 ) Then
            vtmp = og_mw( spc ) * gasconv
            conc( n ) = Max ( og_conc( spc ) / vtmp, min_gasconc )
#ifdef sens
            Do np = 1, npmax
               If ( conc( n ) .Eq. min_gasconc ) Then
                  s_conc( np,n ) = 0.0
               Else
                  s_conc( np,n ) = og_sens( spc,np ) / vtmp
               End If
            End Do
#endif
                  
            ! Convert Budget Process Numbers from ug m-3 to ppmv
            ! for application to process analysis and ISAM
            COND_BUDGET( n )   = COND_BUDGET( n )   / vtmp
            NPF_BUDGET( n )    = NPF_BUDGET( n )    / vtmp
         End If
      End Do

      Return
      End Subroutine update_orgvapor

C-----------------------------------------------------------------------
      Function findVapor( vname, required ) Result ( ndx )

C  Finds the index of 'required' semivolatile species in the oaspc list

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------
      Use RUNTIME_VARS, only : logdev
      Use utilio_defn

      Implicit None

      ! Arguments:
      Character( 16 ) :: vname
      Integer ndx
      Logical required

      ! Local Variables:
      Character( 80 ) :: xmsg
      Integer         :: spc

      ndx = 0
      Do spc = 1, n_oa
         If ( oaspc( spc )%gas_name .Eq. vname ) Then
            ndx = spc
            Return
         End If
      End Do

      If ( .Not. required ) Then
         xmsg = 'Optional Species '
     &       // Trim( vname ) // 
     &       ' is not found in the G2AE or N2AE values of GC or NR namelists.'
         write(logdev,'(5x,a)') xmsg
         Return
      End If

      xmsg = 'FATAL:' // Trim( vname )
     &  // ' is not found in the G2AE or N2AE values of GC or NR namelists.'
      OA_eflag  = .True.
      Call m3warn( pname, 0, 0, xmsg )
 

      Return
      End Function findVapor

C-----------------------------------------------------------------------
      Subroutine orgaer( dt, layer )

C Updates CGRID via several pathways for secondary organic aerosol (SOA)
C formation, as recommended by Edney et al. (2007).  These include SOA
C formation from isoprene, monoterpenes, sesquiterpenes, long alkanes, and
C aromatics (incl. benzene).

C Input includes the concentrations of reactive organic gases (ROG)
C that were oxidized during the time step (ORGPROD), the vapor-phase
C concentration of each semi-volatile organic compound, the
C concentration of each SOA species, and the concentration of primary
C organic aerosol (all concentrations are stored in the CBLK array).
C Output includes updated concentrations of SOA species, vapor-phase
C semi-volatile organic compounds, and moments of the accumulation
C mode.  The geometric mean diameter of the accumulation mode is also
C updated.  All SOA formation is restricted to the accumulation mode.

C This code relies on 12 counter species to be incorporated in the
C gas-phase chemical mechanisms to track the amounts of individual
C ROG that reacted during the current time step (i.e., NPREC=10).
C The arrays of length = NPREC include:
C       (1) "long" alkanes  (ALKRXN)
C       (2) low-yield aromatics, high-NOx pathway (XYLNRXN)
C       (3) low-yield aromatics, low-NOx pathway (XYLHRXN)
C       (4) high-yield aromatics, high-NOx pathway (TOLNRXN)
C       (5) high-yield aromatics, low-NOx pathway (TOLHRXN)
C       (6) benzene, high-NOx pathway (BNZNRXN)
C       (7) benzene, low-NOx pathway (BNZHRXN)
C       (8) monoterpenes (TRPRXN)
C       (9) isoprene (ISOPRXN)
C      (10) sesquiterpenes (SESQRXN)
C      (11) PAHs/naphthalene, high-NOx pathway (PAHNRXN)
C      (12) PAHs/naphthalene, low-NOx pathway (PAHHRXN)

C In total, 15 organic species are allowed to partition between the
C vapor and particulate phases (i.e., NCVAP=12). The arrays of
C length = NCVAP include:
C           alkane (2 semi-volatile products)
C           low-yield aromatics, high-NOx pathway (2 products)
C           high-yield aromatics, high-NOx pathway (2 products)
C           benzene, high-NOx pathway (2 products)
C           monoterpenes (2 products)
C           isoprene (2 products)
C           sesquiterpenes (1 product)
C           PAHs/naphthalene, high-NOx pathway (2 products)

C Equilibrium partitioning calculations are based on the absorptive
C partitioning model of Pankow (1994) that was extended by Odum et
C al. (1996).  Saturation vapor pressures (cstar) and mass-based
C stoichiometric yield coefficients (alpha) are obtained from smog-
C chamber studies.  Saturation vapor pressures are modified as a
C function of temperature using eqn 6 of Sheehan & Bowman (2001).

C If the pre-existing organic aerosol concentration is zero,
C gas/particle equilibrium is established only after the organic gas
C concentration reaches the threshold value defined in eqn 9 of
C Schell et al. (2001).  Until this threshold value is reached,
C organic vapors do not partition to the particle phase.  Once the
C organic gas/particle equilibrium has been established, gas and
C particle-phase concentrations of each condensible species are
C calculated iteratively using a globally convergent variation of
C Newton's method (SUBROUTINE NEWT), as described in eqn 8 of Schell
C et al. (2001).

C In addition to the various pathways of semi-volatile SOA formation
C treated in previous versions of the model, four types of non-
C volatile SOA are considered here:
C   (1) aromatic and PAH-derived SOA under low-NOx conditions
C   (2) oligomerization of all particle-phase semi-volatile material
C   (3) SOA formed by in-cloud oxidation  (SUBROUTINE AQCHEM)
C   (4) isoprene IEPOX-derived SOA under acidic conditions (AEROSOL_CHEMISTRY)

C Previous code revision history:
C   Originally coded August 1, 2001 by Dr. Francis S. Binkowski

C   Revised April 4, 2003 by Gerald Gipson to allow for evaporation
C   of organics from aerosols. Now total vapor + aerosol phase is
C   repartitioned at each time step and totorgnv ( Mo ) does not
C   include oldsoa.

C   Revised July 14, 2003 by Dr. Prakash V. Bhave
C   - changed cstar(2,3) from 10.103 & 90.925 to 111.11 & 1000.0
C     because smog chamber data of Kalberer et al. were collected
C     at 298 K (not 310 K, as was previously assumed)
C   - changed mw_vap(9,10) from 184 g/mol to 177 g/mol, to be
C     consistent with mwsoa_b
C   - modified threshold criteria for establishing gas/particle
C     equilibrium by removing the loose criterion involving "mtot"
C   - changed variable names to reflect that the combined vapor +
C     aerosol concentrations are now being repartitioned during
C     each time step (not just the newly formed SVOC's)
C   - added documentation and removed extraneous lines of code

C   Revised December 4, 2003 by Dr. Francis S. Binkowski
C   - output variables ORGRATE and ORGBRATE removed and replaced
C     by SOA_A and SOA_B, the newly equilibrated values of
C     Anthropogenic and Biogenic SOA, respectively.  These are non-
C     negative values.
C   - variable jj also removed

C   Revised January 8, 2004 by Dr. Prakash V. Bhave
C   - removed the output variable YIELD.  It has no physical meaning
C     after the 12/04/2003 revisions.

C   Revised January 12, 2004 by Dr. Chris G. Nolte
C   - for computational efficiency, modified the initial caer guess
C     used as input to NEWT.  If NEWT returns check .eq. true, then
C     NEWT is called again with a guess of caer = 0.5*ctot
C   - removed ITS parameter from NEWT call vector
C   - fixed bug where concentrations less than TOLMIN (i.e., 1.0E-12)
C     were reset to 1.0e-30
C   - removed extraneous code related to "Pandis method" of SVOC
C     partitioning when threshold criterion is not met (i.e.,
C     insufficient organic matter to establish gas/particle
C     equilibrium)  ** results unaffected by this change
C
C   Revised September 7, 2007 by Dr. Sergey L. Napelenok
C   - Replaced old SOA species (SOA_A, SOA_B) with an array of
C     precursor-specific SOA species.  Replaced OLDSOA_A and OLDSOA_B
C     with an array (OLDSOA).  Updated call vector accordingly.
C   - Deleted nole* and nbio* variables (now obsolete)
C   - Increased the dimension of several arrays to accommodate new
C     SOA precursors (benzene, sesquiterpenes) and pathways (low-NOx,
C     acid-catalyzed, oligomers, in-cloud)
C
C   Revised November 29, 2007 by Dr. Prakash V. Bhave
C   - Renamed subroutine from ORGAER3 to ORGAER5
C   - Modified M2 and M3 calculations to account for the updated
C     definition of DRY aerosol (which now includes non-volatile SOA)
C   - Updated Hvap and corresponding cstar values
C   - Added parameters for SOA from isoprene and sesquiterpenes
C   - Updated aromatic SOA scheme to include semi-volatile and non-
C     volatile products that form under high-NOx and low-NOx
C     conditions, respectively
C   - Added oligomerization process
C   - Added enhancement of isoprene SOA under acidic conditions
C
C   Revised June 2, 2008 by Dr. Prakash V. Bhave
C   - Changed h_vap of benzene SOA to match that of toluene SOA, based
C     on consultation with Dr. Ed Edney and Dr. Tad Kleindienst.

C   Revised June 5, 2008 by Drs. Prakash Bhave and Sergey Napelenok
C   - Simplified the code for conserving low-volatility isoprene
C     oxidation products and removed a minor bug in the acid-induced
C     isoprene SOA calculation.
C
C   Revised September 9, 2008 by Dr. Prakash V. Bhave
C   - Increased alpha values for SV_TRP1, SV_TRP2, and SV_SQT by a
C     factor of 1.3 to correct for the implicit assumption of unit
C     density in those SOA yield parameters.
C   - Reduced SOA/SOC ratio of AISO1 and AISO2 from 2.5 to 1.6, and
C     increased SOA/SOC ratio of AISO3 from 2.5 to 2.7.  Accordingly,
C     the molar masses of AISO1 and AISO2 were decreased to 96 g/mol
C     and the molar mass of AISO3 was increased to 162.
C
C   Revised September 26, 2014 by Dr. Havala Pye
C   - Removed previous acid enhanced isoprene SOA. Acid catalyzed
C     isoprene SOA now follows Pye et al. 2013 ES&T uptake of IEPOX.
C     See AEROSOL_CHEMISTRY and mech.def for IEPOX SOA.
C   - Allowed for alternate method to NEWT for solving partitioning
C     equations

C References:
C   1. Edney, E.O., T.E. Kleindienst, M. Lewandowski, and J.H.
C      Offenberg, Updated SOA chemical mechanism for the Community
C      Multi-Scale Air Quality model, EPA 600/X-07/025, U.S. EPA,
C      Research Triangle Park, NC, 2007.

C   2. Pankow, J. F., An absorption model of gas/particle partitioning
C      of organic compounds in the atmosphere, Atmos. Environ., Vol 28,
C      No 2, 185-188, 1994.

C   3. Odum, J. R., T. Hoffmann, F. Bowman, D. Collins, R. C. Flagan,
C      and J. H. Seinfeld, Gas/particle partitioning and secondary
C      organic aerosol yields, Environ. Sci. Technol., Vol 30, No 8,
C      2580-2585, 1996.

C   4. Sheehan, P. E. and F. M. Bowman, Estimated effects of temperature
C      on secondary organic aerosol concentrations, Environ. Sci.
C      Technol., Vol 35, No 11, 2129-2135, 2001.

C   5. Schell, B., I. J. Ackermann, H. Hass, F. S. Binkowski, and
C      A. Abel, Modeling the formation of secondary organic aerosol
C      within a comprehensive air quality modeling system, J. Geophys.
C      Res., Vol 106, No D22, 28275-28293, 2001.

C   6. Strader, R., F. Lurmann, and S. N. Pandis, Evaluation of
C      secondary organic aerosol formation in winter, Atmos. Environ.,
C      Vol 33, 4849-4863, 1999.

C   7. Ng, N. L., J. H. Kroll, A. W. H. Chan, P. S. Chhabra, R. C.
C      Flagan, and J. H. Seinfeld, Secondary organic aerosol formation
C      from m-xylene, toluene, and benzene, Atmos. Chem. Phys., Vol 7,
C      3909-3922, 2007a.

C   8. Griffin, R. J., D. R. Cocker III, R. C. Flagan, and J. H.
C      Seinfeld, Organic aerosol formation from the oxidation of
C      biogenic hydrocarbons, J. Geophys. Res., Vol 104, No D3,
C      3555-3567, 1999.

C   9. Bian, F. and F. M. Bowman, Theoretical method for lumping
C      multicomponent secondary organic aerosol mixtures, Environ.
C      Sci. Technol., Vol 36, No 11, 2491-2497, 2002.

C  10. Offenberg, J. H., T. E. Kleindienst, M. Jaoui, M. Lewandowski,
C      and E. O. Edney, Thermal properties of secondary organic
C      aerosols, Geophys. Res. Lett., Vol 33, L03816, doi:10.1029/
C      2005GL024623, 2006.

C  11. Bahreini, R., M. D. Keywood, N. L. Ng, V. Varutbangkul, S. Gao,
C      R. C. Flagan, J. H. Seinfeld, D. R. Worsnop, and J. L. Jimenez,
C      Measurements of secondary organic aerosol from oxidation of
C      cycloalkenes, terpenes, and m-xylene using an Aerodyne aerosol
C      mass spectrometer, Environ. Sci. Technol., Vol 39, 5674-5688,
C      2005.

C  12. Alfarra, M. R., D. Paulsen, M. Gysel, A. A. Gaforth, J. Dommen,
C      A. S. H. Prevot, D. R. Worsnop, U. Baltensperger, and H. Coe,
C      A mass spectrometric study of secondary organic aerosols formed
C      from the photooxidation of anthropogenic and biogenic precursors
C      in a reaction chamber, Atmos. Chem. Phys., Vol 6, 5279-5293,
C      2006.

C  13. Ng, N. L., P. S. Chhabra, A. W. H. Chan, J. D. Surratt, J. H.
C      Kroll, A. J. Kwan, D. C. McCabe, P. O. Wennberg, A. Sorooshian,
C      S. M. Murphy, N. F. Dalleska, R. C. Flagan, and J. H. Seinfeld,
C      Effect of NOx level on secondary organic aerosol (SOA) formation
C      from the photooxidation of terpenes, Atmos. Chem. Phys., Vol 7,
C      5159-5174, 2007b.

C  14. Kostenidou, E., R. K. Pathak, and S. N. Pandis, An algorithm for
C      the calculation of secondary organic aerosol density combining
C      AMS and SMPS data, Aerosol Sci. Technol., Vol 41, 1002-1010,
C      2007.

C  15. Offenberg, J. H., C. W. Lewis, M. Lewandowski, M. Jaoui, T. E.
C      Kleindienst, and E. O. Edney, Contributions of toluene and
C      alpha-pinene to SOA formed in an irradiated toluene/alpha-pinene/
C      NOx/air mixture: comparison of results using 14C content and SOA
C      organic tracer methods, Environ. Sci. Technol., Vol 41, 3972-
C      3976, 2007.

C  16. Henze, D. K. and J. H. Seinfeld, Global secondary organic aerosol
C      from isoprene oxidation, Geophys. Res. Lett., Vol 33, L09812,
C      doi:10.1029/2006GL025976, 2006.

C  17. Kleindienst, T. E., M. Jaoui, M. Lewandowski, J. H. Offenberg,
C      C. W. Lewis, P. V. Bhave, and E. O. Edney, Estimates of the
C      contributions of biogenic and anthropogenic hydrocarbons to
C      secondary organic aerosol at a southeastern US location, Atmos.
C      Environ., Vol 41, 8288-8300, 2007.

C  18. Kalberer, M., D. Paulsen, M. Sax, M. Steinbacher, J. Dommen,
C      A. S. H. Prevot, R. Fisseha, E. Weingartner, V. Frankevich,
C      R. Zenobi, and U. Baltensperger, Identification of polymers as
C      major components of atmospheric organic aerosols, Science, Vol
C      303, 1659-1662, 2004.

C  19. Turpin, B. J. and H.-J. Lim, Species contributions to PM2.5 mass
C      concentrations: revisiting common assumptions for estimating
C      organic mass, Aero. Sci. Technol., Vol 35, 602-610, 2001.

C  20. Surratt, J. D., M. Lewandowski, J. H. Offenberg, M. Jaoui, T. E.
C      Kleindienst, E. O. Edney, and J. H. Seinfeld, Effect of acidity
C      on secondary organic aerosol formation from isoprene, Environ.
C      Sci. Technol., Vol 41, 5363-5369, 2007.

C  21. Pye et al., Epoxide pathways improve model prediction of isoprene
C      markers and reveal key role of acidity in aerosol formation,
C      Environ. Sci. Technol., 2013.


C Revision History:
C    First orgaer version was coded in April 2010 by Steve Howard with
C    Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C SH  03/10/11 Renamed met_data to aeromet_data
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C HOTP 05Aug15 Made the calculation for total number of organic moles more robust
C BNM 11/09/15 Added Some Comments to the SOA Scheme
C HOTP 7/17/18 Added uptake of water onto hydrophillic organics
C-----------------------------------------------------------------------

C Key Subroutines/Functions called:  newt, soabisection

      Use aero_data
      Use aeromet_data
      Use utilio_defn

      Implicit None

      ! Arguments:
      Real    :: dt            ! Synchronization time step [ s ]
      Integer :: layer         ! model layer number


      ! Local variables:
      Logical, Save :: first_time     = .True.
      Logical, Save :: solve_orgmoles = .True.
      
      Character( 300 ) :: xmsg
      Integer       :: i, im, indx, ispc, n, nsvol
      Integer, Save :: nsvol_tracers
      Integer, Save :: nsvol_Nottracers
      
      Real, Allocatable, Save ::  mw_inv( : )     ! Inverse MW of SVOCs [ mol/g ]

      Real, Allocatable, Save ::  rog_ppm2ug( : ) ! [ ppm per ug/m3 ] for ORGPROD at
                                        ! reference temperature and pressure

      ! Parameters & variables for adjusting cstar to ambient conditions
      Real, Parameter :: tref   = 298.0          ! reference temperature [ K ]
      Real, Parameter :: trefm1 = 1.0 / tref     ! inverse of reference temperature
      Real, Parameter :: prefm1 = 1.0 / stdatmpa ! inverse of reference pressure
      Real, Parameter :: rgas1  = 1.0 / rgasuniv ! reciprocal of universal gas constant
      Real, Parameter :: kolig  = 0.69314718 / 72000.0  ! 20h half-life of oligomerization rate [ 1/s ]
      Real, Parameter :: olgrat = 2.1            ! SOA/SOC ratio for oligomers
      Real, Parameter :: kacid  = 0.00361        ! acid-induced enhancement factor
      Real, Parameter :: threshmin = 1.0E-19     ! small positive number
      Real, Parameter :: ctolmin = 1.0E-06
      Real, Parameter :: convfac_298 = 101325.0 * rgas1 * trefm1  ! P/RT at 1 atm and 298 K [ mole/m**3 ]
      Real, Parameter :: difforg = 9.36e-6  ! Diffusivity of organics [m2 s-1]
      Real, Parameter :: alphorg = 1.0      ! accomodation coefficient

      Real convfac
      Real tt1, tt2      ! temperature-related factors
      Real tempcorr      ! temperature correction factor for cstar

      ! Variables used in oligomerization calculations
      Real expdt         ! non-dimensional loss coefficient
      Real nsvpa         ! particle-phase anthropogenic SVOC [ umolC/m3 ]
      Real nsvpb         ! particle-phase biogenic SVOC [ umolC/m3 ]

      ! Variables used in acid-enhanced isoprene SOA calculations
      Real hplus         ! accumulation-mode H+ conc [ nmol/m3 ]
      Real aiso12        ! particle-phase isoprene SVOC [ ug/m3 ]
      Real vviso         ! vapor-phase isoprene SVOC [ ug/m3 ]
      Real xiso3         ! newly produced AISO3J [ ug/m3 ]
      Real isofrac       ! ratio for depletion of vapor-phase products

      ! Variables used in equilibrium partitioning calculations
      Real totrog( n_oa ) ! drog conc mapped to each SVOC [ ug/m3 ]
      Real(8) GRtmp( n_mode )   ! Dummy variable for accurate treatment of growth to specific moment
      Real GR3( n_oa,n_mode ) ! 3rd Moment Growth for each mode and compound
      Real GR3FRAC( n_oa,n_mode ) ! Fraction of each mode growing/shrinking
      Real dcaer          ! Change in SVOC conc after partitioning happens[ ug/m3 ]
      Real cbar_org(n_oa) ! On-line molecular speed of each organic
      Real dv_org            ! On-line gas-phase diffusivity of each organic
      Real totorgnv          ! Non-volatile OA [ umole/m3 ]
      Real totorg            ! SOA + POA before time step [ umole/m3 ]
      Real threshold         ! criterion for establishing gas/part equil.
      Real faer              ! fraction of total in aerosol, intermediate value
      Logical check          ! flag to indicate if NEWT subroutine
                             ! converged to a spurious root
      Real totaer            ! total aerosol-phase mass of each semivolaitle component
      Real Phi               ! mass fraction of a semivolatile component
                             ! in each mode
      Character(16), Allocatable, Save :: svname( : )
      Real, Allocatable, Save          :: svmw( : ), svmw_inv( : )
      Real, Allocatable, Save          :: c0    ( : ) ! cstar at AIRTEMP [ ug/m3 ]
      Real, Allocatable, Save          :: caer0 ( : ) ! Particle conc before current time step [ ug/m3 ]
      Real, Allocatable, Save          :: ctoti ( : ) ! Total (g+p) conc before current time step [ ug/m3 ]
      Real, Allocatable, Save          :: prod  ( : ) ! Total G+P produced during current step [ ug/m3 ]
      Real, Allocatable, Save          :: ctotf ( : ) ! Total conc after current time step [ ug/m3 ]
      Real, Allocatable, Save          :: caer  ( : ) ! Particle conc in aerosol phase after current 
                                                      !  step [ ug/m3 ]

      ! Variables for computing the budget
      REAL :: CBUDGET0_NUM ( N_MODE )
      REAL :: CBUDGET0_SRF ( N_MODE )
      REAL :: CBUDGET0_MASS( N_AEROSPC,N_MODE )
      REAL :: CBUDGET0_VAP ( N_OA )


      ! Variables for water uptake onto organics
      Real kappaVorg        ! sum_i(kappa_i*Vorganic_i) [m3 species/m3 air]
      Real overallkappa     ! sum_i(kappa_i*Vorganic_i)/vtot [NA units]
      Real totvol           ! total aerosol volume [m3 species/m3 air]
      Real poa              ! nonvolatile poa concentration [ug/m3]
      Real numparticles     ! number of acc mode particles [#/m3]
      Real diam             ! volume equivalent particle diameter [m]
      Real aw               ! activity of water [fraction]
      Real relhumid         ! relative humidity capped at 95% [fraction]
                            ! Cap represents conservative est of water uptake
                            ! and is consistent with IMPROVE visibility methods
      
      ! Variables for updating 2nd and 3rd moments
      Real(8) m0_init( 2 )  ! initial 0 moment, wet [ mom/m3 ]
      Real(8) m1wet_init( 2 )  ! initial 1st moment, wet [ mom/m3 ]
      Real(8) m2wet_initD( 2 )  ! initial 2nd moment, wet [ mom/m3 ]
      Real m3wet_init( 2 )  ! initial 3rd moment, wet [ mom/m3 ]
      Real m2wet_init( 2 )  ! initial 2nd moment, wet [ mom/m3 ]
      Real m3wet_final( 2 )  ! final 3rd moment with updated SOA [ mom/m3 ]
      Real m2wet_final( 2 )  ! final 2nd moment with updated SOA [ mom/m3 ]

      ! Added for new SOA bisection (hotp 7/6/11)
      Real               :: lowb, upb, orgmoles      ! lower bound, upper bound, total aerosol moles
      Logical, Parameter :: newtpartition = .False.   ! true to use original method, false to use new bisection

#ifdef sens
C Sensitivity variables
      Real( 8 ), Allocatable, Save :: s_totrog( :,: ) ! drog conc mapped to each SVOC [ ug/m3 ]
      Real( 8 ), Allocatable, Save :: s_ctoti ( :,: ) ! Total (g+p) conc before current time step [ ug/m3 ]
      Real( 8 ), Allocatable, Save :: s_prod  ( :,: ) ! Total G+P produced during current step [ ug/m3 ]
      Real( 8 ), Allocatable, Save :: s_ctotf ( :,: ) ! Total sens after current time step [ ug/m3 ]
      Real( 8 ), Allocatable, Save :: s_caer  ( :,: ) ! Particle sens in aerosol phase after current step [ ug/m3 ]
      Real( 4 ), Allocatable, Save :: kpart( : )      ! partitioning factor caer/(ctotf)
      Real( 8 ), Allocatable, Save :: s_dcaer ( : )   ! Change in SVOC sens after partitioning happens[ ug/m3 ]
      Real( 8 ), Allocatable, Save :: s_caer0 ( :,: ) ! Particle sens before current time step [ ug/m3 ]
      Integer allocstat
#endif

C-----------------------------------------------------------------------

      If ( first_time )  Then
         first_time = .False.

         ! Set unit conversion and inverse mw constants. Allocate space
         ! for semivolatile species partitioning variables.
         Allocate( rog_ppm2ug( n_oa ), mw_inv( n_oa ) )
         rog_ppm2ug( : ) = ROG_mw( : ) * convfac_298
         mw_inv( : ) = 1.0 / op_mw( : )

         nsvol         = count( .not.oaspc( : )%nonvol )
         If ( n_oa_tracers .gt. 0 )then
            nsvol_tracers = count( .not.oaspc( (n_oa_NotTracers+1):n_oa )%nonvol )
         Else
            nsvol_tracers = 0
         End If   
         
         nsvol_NotTracers = nsvol - nsvol_tracers
         
         
     
         If ( nsvol_NotTracers .Le. 0 ) Then
            xmsg = 'All semivolatile SOA species are tracers. Not realistic model setup.'
            solve_orgmoles = .False.
            Call m3warn( pname, 0, 0, xmsg )
         End If
         
         Allocate( svname( nsvol ), svmw( nsvol ), svmw_inv( nsvol ),
     &             c0( nsvol ), caer0( nsvol ), ctoti( nsvol ),
     &             prod( nsvol ), ctotf( nsvol ), caer( nsvol ) )

#ifdef sens
         Allocate (
     &              s_totrog( n_oa,npmax ),
     &              s_ctoti( nsvol,npmax ),
     &              s_ctotf( nsvol,npmax ),
     &              s_caer( nsvol,npmax ),    
     &              kpart( nsvol ),
     &              s_prod( nsvol,npmax ),    
     &              s_dcaer( npmax ),
     &              s_caer0( nsvol,npmax ),
     &              stat = allocstat )
         If ( allocstat .Ne. 0 ) Then
            xmsg = 'Failure allocating sensitivity variables in ORGAER'
            Call m3exit ( 'ORGAER', 0, 0, xmsg, xstat1 )
         End If
#endif
      End If ! first_time

      ! Set temperature factors
      tt1 = tref / airtemp
      tt2 = trefm1 - 1.0 / airtemp
      convfac = tt1 * airpres * prefm1

      ! Set Mass Transfer Properties (although each condensing organic
      ! will have its own diffusivity and molecular speed, just assume
      ! as a first guess that they are all uniform and given by a rep-
      ! resentative compound.
      Dv_org = difforg * ( STDATMPA / AIRPRES ) * ( AIRTEMP / 273.16 ) ** 1.75
      cbar_org( : ) = SQRT(8.0 * RGASUNIV * AIRTEMP / ( PI * og_mw(:) * 1.0E-3 ) )
 
      ! Compute 3rd moment, 2nd moment. moment_conc arrays are wet
      Call calcmoments ( .true. )
      Call getpar( FIXED_sg )


      ! Initialize Budget Variables
      CBUDGET0_NUM  = MOMENT0_CONC
      CBUDGET0_SRF  = MOMENT2_CONC
      CBUDGET0_MASS = AEROSPC_CONC
      CBUDGET0_VAP  = OG_CONC

      ! Initialize Local Moment Variables
      m3wet_init( : ) = moment3_conc( 1:2 )
      m2wet_init( : ) = moment2_conc( 1:2 )
        
      ! Calculate total flux associated with each indivual mode 
      ! for the third moment.
      GR3( :,: ) = 0.0
      m0_init( : )    = Real( moment0_conc( 1:2 ), 8 )
      m2wet_initD( : ) = Real( m2wet_init( 1:2 ), 8 )

      Do im = 1,2
        m1wet_init( im ) = m0_init( im ) * aeromode_diam( im ) * 
     &                     exp( 0.5d0 * aeromode_lnsg( im ) ** 2.0d0 )

        Do i = 1, n_oa
          If ( aero_missing( map_OAtoAERO( i ),im ) .or.
     &         oaspc(i)%nonvol ) Cycle
          
          Call HCOND3( m0_init( im ), m1wet_init( im ),
     &                 m2wet_initD( im ), Dv_org, alphorg, 
     &                 cbar_org(i), GRtmp )
          GR3( i,im ) = Real( GRtmp( 2 ), 4 )
        End Do
      End Do
      
      ! Calculate fraction of mass transfer to/from each mode
      GR3FRAC( :,: ) = 0.0
      Where ( GR3( :,1 ) .gt. 0. ) 
     &    GR3FRAC( :,1 ) = GR3( :,1 ) / ( GR3(:,1) + GR3(:,2) ) ! Aitken Growth
      Where ( GR3( :,2 ) .gt. 0. )
     &    GR3FRAC( :,2 ) = GR3( :,2 ) / ( GR3(:,1) + GR3(:,2) ) ! Accumulation Growth

      ! Initialize drog from ORGPROD and change units to [ ug / m**3 ]
      ! Assignment of drog to totrog. This moving mass from the reactive
      ! gas tracer species to the array that will be multiplied by alpha
      ! to give newly formed semivolatile vapors.
      totrog( : ) = rog_dconc( : ) * rog_ppm2ug( : ) * convfac
#ifdef sens
      Do np = 1, npmax
         Do i = 1, n_oa
            s_totrog( i, np ) = rog_dsens( i, np )* rog_ppm2ug( i ) * convfac
         End Do
      End Do
#endif

      ! Transfer non-volatile products directly to aerospc_conc array (2nd mode)
      Do i = 1, n_oa_NotTracers
         If ( oaspc( i )%nonvol .and. oaspc( i )%alpha .gt. 0. ) Then
            aerospc_conc( map_OAtoAERO( i ),2 ) =
     &      aerospc_conc( map_OAtoAERO( i ),2 )
     &      + oaspc( i )%alpha * totrog( i )
#ifdef sens
            Do np = 1, npmax
               s_aerospc_conc( map_OAtoAERO( i ),2,np ) =
     &         s_aerospc_conc( map_OAtoAERO( i ),2,np )
     &         + oaspc( i )%alpha * s_totrog( i,np )
            End Do
#endif
         End If
      End Do

      ! Equilibrium Partitioning Calculations

      ! Initial guess of caer is computed as follows:
      !    From eqn (8) of Schell et al. (2001)
      !    caer = ctotf - c0 * (caer/MW) / totorg
      !    Assuming totorg doesn't change during the timestep,
      !    caer * (1 + c0/MW / totorg) = ctotf
      !    caer = ctotf / ( 1 + c0/MW / totorg )

      threshold = 0.0  ! This threshold will be compared to the
                       ! saturation concentration to determine whether
                       ! or not OA partitioning is even likely.
      upb = 0.0  ! Upper bound for bisection method

      ! Compute the number of nonvolatile and total organic compound
      ! moles in the system
      nsvol    = 0
      totorgnv = sum( sum( aerospc_conc( :,1:2 ),2 ) / aerospc_mw( : ),
     &                     mask=lnonvol_oa( : ) )
      totorg   = sum( sum( aerospc_conc( :,1:2 ),2 ) * aerospc_mwinv( : ),
     &                     mask=(aerospc(:)%om .and. .Not. aerospc(:)%tracer) )

      
     
      ! Calculate the Initial Partitioning of Each Semivolatile
      ! Compound
      Do i = 1, n_oa
        n = map_OAtoAERO( i )
        if ( .not.oaspc( i )%nonvol ) Then
          nsvol = nsvol + 1
          svname( nsvol ) = oaspc( i )%name

          ! Sum particle-phase and total organic mass for this
          ! semivolatile species
          caer0( nsvol ) = aerospc_conc( n,2 ) + aerospc_conc( n,1 )
          ctoti( nsvol ) = og_conc( i )
     &                   + aerospc_conc( n,2 ) + aerospc_conc( n,1 )

#ifdef sens
          Do np = 1, npmax
             s_caer0( nsvol,np ) = s_aerospc_conc( n,2,np ) 
     &                           + s_aerospc_conc( n,1,np )
             s_ctoti( nsvol,np ) = og_sens( i,np )
     &                           + s_aerospc_conc( n,2,np ) 
     &                           + s_aerospc_conc( n,1,np )
          End Do
#endif
          ! Calculate C* at current temperature
          tempcorr      = tt1 * Exp( oaspc( i )%enth * rgas1 * tt2 )
          c0( nsvol )   = oaspc( i )%cstar * tempcorr      ! Satn Conc. [ug/m3]
          prod( nsvol ) = oaspc( i )%alpha * totrog( i )   ! Total Vapor Produced [ug/m3]
          ctotf( nsvol )= ctoti( nsvol ) + prod( nsvol )   ! Vapor + Particle [ug/m3]
#ifdef sens
          Do np = 1, npmax
             s_prod( nsvol,np )  = oaspc( i )%alpha * s_totrog( i,np ) 
             s_ctotf( nsvol,np ) = s_ctoti( nsvol,np ) + s_prod( nsvol,np ) 
          End Do
#endif

          If ( .Not. aerospc( n )%tracer ) Then ! partition only based on NotTracer mass
              threshold  = threshold +  ctotf( nsvol ) / c0( nsvol )
              ! Partition Nontracer semivolatile species accordingly
              faer   = totorg                                  ! initial fraction in aerosol
     &               / (totorg + c0( nsvol ) * mw_inv( i ) ) 
              caer( nsvol )  = ctotf( nsvol ) * faer           ! initial amount in aerosol
#ifdef sens
              Do np = 1, npmax
              s_caer( nsvol,np ) = s_ctotf( nsvol,np ) * faer
              End Do
#endif if
              upb        = upb + ctotf( nsvol ) * mw_inv( i )  ! upper bound: just semivolatile
                                                               !  moles for now. Nonvolatile moles 
                                                               !  will be added later.
          End If
                                                               
          svmw_inv( nsvol ) = mw_inv( i )
          svmw( nsvol ) = op_mw( i )
          
        end if
      End Do

      ! Check If gas/particle equilibrium can be established
      If ( ( threshold .Gt. 1.0 ) .Or. ( totorgnv .Gt. threshmin ) ) Then

           ! Perform one of two methods for partitioning
           If ( newtpartition ) Then

          ! METHOD1
          ! Calculate new SOA by partitioning. This method uses a globally convergent
          ! Newton-Raphson method coded by Dr Benedikt Schell to solve the nonlinear
          ! quadratic system shown in eqn 8 of Schell et al:
          !    A(i)  * caer(i) ** 2 + B * caer(i) + C(i) = 0.0,
          !    where B(i) contains the sum of all caer(j), for j not equal to i.

             Call newt( layer, caer, nsvol_NotTracers, check,
     &                  ctotf, c0, svmw_inv, totorgnv )
             If ( check ) Then
                ! Try again with initial guess of 50/50 gas/aerosol split.
                Do i = 1, nsvol_NotTracers
                   caer( i ) = 0.5 * ctotf( i )
                End Do
                Call newt( layer, caer, nsvol_NotTracers, check,
     &                     ctotf, c0, svmw_inv, totorgnv )
                If ( check ) Then
                   Write( xmsg,'( A,I4 )' ) 
     &                    ' *** Problem in NEWT at Layer = ', layer
                   Call m3exit( pname, 0, 0, xmsg, xstat3 )
                End If
             End If

             If ( nsvol_tracers .Gt. 0 ) Then
! partition tracer semivolatiles based on Schell equation (8)
! assuming:
!   1) the orgmoles from tracer species are already accounted in the orgmoles
!   from non-tracers species.
!   2) tracer is divided between volatile and nonvolatile components.
!   the nonvolatile part can not partition. The division is given by the
!   variable, frac_volatile.
!      shell's visocity
                orgmoles = totorgnv
                Do i = 1, nsvol_NotTracers
                   orgmoles = orgmoles + caer( i ) * mw_inv( i )
                End Do
!                phi = aerospc_conc( aorgh2o_idx, 2 ) * aerospc_mwinv( aorgh2o_idx ) / orgmoles
                Do i = nsvol_NotTracers+1, nsvol
                   faer = svmw(i) * orgmoles 
     &                  / ( c0( i ) + svmw(i) * orgmoles )
                   caer( i )  = ctotf( i ) * faer
! update final concentration with nonvolatile fraction
                   ctotf( i ) = ctotf( i )
                End Do  
             End If   
 
           Else
            
          ! METHOD2
          !      Calculate new SOA by partitioning.
          !      Method uses bisection method to solve for total number of moles
          !      of orgaic aerosol. Caer is then calculated based on that number.
          !         Solve for total number of organic moles in aerosol (hotp 7/5/11)
             lowb      = totorgnv   ! lower: moles of nonvolatile
             upb       = lowb + upb ! upper: all moles (semi and nonvolatile)
             If ( abs( upb - lowb ) .Lt. threshmin ) Then
                ! no SOA to partition
                orgmoles = ( lowb + upb ) / 2.0
             Else
                upb       = upb * ( 2.0 + 1.0e-7 * real(n_oa_NotTracers) ) ! prevent numerical issues
                orgmoles = soabisect( lowb, upb, totorgnv, nsvol_NotTracers, c0, ctotf, svmw )
             End If

             ! Determine concentration of each semivoltile (hotp 7/5/11)
             Do i = 1, nsvol_NotTracers 
               faer = svmw(i) * orgmoles / 
     &                ( c0( i ) + svmw(i) * orgmoles )
               caer( i ) = ctotf( i ) * faer
             End Do
! partition tracer semivolatiles based on Schell equation (8)
! assuming:
!   1) orgmoles already include moles from tracers
              Do i = nsvol_NotTracers+1, nsvol
                 faer = svmw(i) * orgmoles 
     &                / ( c0( i ) + svmw(i) * orgmoles )
                 caer( i ) = ctotf( i ) * faer
! update final concentration with nonvolatile fraction
                 ctotf( i ) = ctotf( i ) 
              End Do  
           End If  ! End OA paritioning solver METHOD selection

#ifdef sens 
c approximate partioning for sensitivities
           kpart = 0.0
           Do i = nsvol_NotTracers+1, nsvol
              If ( ctotf( i ) .gt. 0.0 ) Then
                 kpart( i ) = caer( i ) / ctotf( i )
              Else
                 kpart( i ) = 0.5
              Endif
              kpart( i ) = max( min(kpart( i ),1.0), 0.0 )
           End Do

           Do np = 1, npmax
              Do i = nsvol_NotTracers+1, nsvol
                 s_caer( i,np ) = s_ctotf( i,np ) * Real( kpart( i ),8 )
              End Do
           End Do
#endif

        ! Constrain caer to values between conmin and ctotf
        nsvol = 0
        Do i = 1, n_oa
           If ( oaspc( i )%nonvol ) Cycle
           nsvol = nsvol + 1

           ! Check that the mass of aerosol left has not gone below a
           ! minimum value
           If ( caer( nsvol ) .Lt. tiny(0.0) ) Then
              Write( logdev,* ) 'caer less than zero for OA species: ', 
     &                              oaspc( i )%name, ' ', caer( nsvol ),
     &                              ' reset to evapmin.'
              ctotf( nsvol ) = ctotf( nsvol ) - caer( nsvol ) + evapmin
              caer( nsvol ) = evapmin
#ifdef sens
              Do np = 1, npmax
                 s_ctotf( nsvol,np ) = s_ctotf( nsvol,np ) - s_caer( nsvol,np) 
c                s_ctotf( nsvol,np ) = s_ctotf( nsvol,np ) 
c                                    * ( 1.0 - kpart( nsvol) ) 
                 s_caer( nsvol,np )  = 0.0
              End Do
#endif              
           End If

           ! Check that the mass of aerosol condensed does not exceed
           ! the total
           If ( caer( nsvol ) - ctotf( nsvol ) .Gt. tiny(0.0) ) Then
               Write( logdev,* ) 'caer exceeds ctotf at i = ', i
               Write( logdev,* ) 'caer: ', caer( nsvol ), ' ctotf: ', ctotf( nsvol )
               Write( logdev,* ) 'caer reset to ctotf'
               caer( nsvol ) = ctotf( nsvol )
#ifdef sens
               Do np = 1, npmax
                 s_caer( nsvol,np )  = s_ctotf( nsvol,np ) 
               End Do
#endif                   
           End If

           ! Calculate total change in OA Species Concentration
           dcaer = caer( nsvol ) - caer0( nsvol )  
#ifdef sens
           Do np = 1, npmax
              s_dcaer( np ) = s_caer( nsvol,np ) - s_caer0( nsvol,np )
           End Do
#endif           

           ! Sum the total aerosol mass of this compound across all
           ! aerosol modes
           indx = map_OAtoAERO( i )
           totaer = max( sum( aerospc_conc( indx,: ) , mask = 
     &                 .NOT.aero_missing( indx,: ) ), conmin )
    
           ! Transfer the semivolatile mass
           Do im = 1,2
              If ( aero_missing( indx,im ) ) Cycle
              If ( dcaer .lt. 0.0 ) Then
                 ! Evaporate using mode-dependent mass fraction
                 Phi = aerospc_conc( indx,im ) / totaer
                 aerospc_conc( indx,im ) = 
     &                aerospc_conc( indx,im ) + dcaer * Phi
#ifdef sens
                 Do np = 1, npmax
                    s_aerospc_conc( indx,im,np ) = 
     &                   s_aerospc_conc( indx,im,np ) + s_dcaer( np ) * Phi
                 End Do
#endif
              Else
                 ! Condense using mode-dependent condensaiton flux 
                 ! fraction
                 aerospc_conc( indx,im ) = aerospc_conc( indx,im ) 
     &                + dcaer * GR3FRAC( i,im )
#ifdef sens
                 Do np = 1, npmax
                    s_aerospc_conc( indx,im,np ) = 
     &                   s_aerospc_conc( indx,im,np )
     &                 + s_dcaer( np ) * GR3FRAC( i,im )  
                 End Do
#endif
              End If
           End Do

           og_conc( i ) = ctotf( nsvol ) - caer( nsvol )

#ifdef sens
           Do np = 1, npmax
              og_sens( i,np ) = s_ctotf( nsvol,np ) - s_caer( nsvol,np )
           End Do
#endif
        End Do  ! Partitioning Vapor Loop
 
      Else   ! threshold not exceeded; no material transferred to aerosol phase
        
        nsvol = 0
        Do i = 1, n_oa
            if ( oaspc( i )%nonvol ) Cycle
            nsvol = nsvol + 1
            indx = map_OAtoAERO( i )

            ! Set all semivolatile aerosols to minimum concentration
            aerospc_conc( indx, 1:n_mode ) = conmin
            ! Sum up particle-phase mass to subtract it from available
            ! vapor
            og_conc( i ) = ctotf( nsvol ) -
     &                       sum( aerospc_conc( indx,: ), 
     &                            mask=.not.aero_missing( indx,: ) )
#ifdef sens
            Do np = 1, npmax
               s_aerospc_conc( indx, 1:n_mode,np ) = 0.0
               og_sens( i,np ) = s_ctotf( nsvol,np ) -
     &                       sum( s_aerospc_conc( indx,:,np ), 
     &                            mask=.not.aero_missing( indx,: ) )

            End Do
#endif
        End Do

      End If    ! check on equilibrium threshold


      ! Add uptake of water onto hydrophillic organics (see Petters and
      ! Kreidenweis 2007 ACP and Pye et al. 2017 ACP for method).
      ! Uptake of water onto inorganic species is calculated elsewhere
      ! using ISORROPIA.
      If ( aorgh2o_idx > 0 ) Then

         kappaVorg = 0.0e0 ! units: m3.spec/m3.air
         totvol    = 0.0e0 ! units: m3.particle/m3.air
         aerospc_conc( aorgh2o_idx,: ) = conmin

         im = 2 ! only compute water for accumulation mode OM

         ! Compute sum_i(Kappa_i*Vorg_i) for all OM species except POC 
         ! and PNCOM, which will be handled next.
         Do i = 1, n_aerospc
           If(  aerospc( i )%tracer )cycle
              totvol = totvol + aerospc_conc(i, im ) /
     &                 aerospc( i )%density * 1.0e-9
           If( aerospc( i )%OM .and. 
     &         i .ne. apoc_idx .and. i .ne. apncom_idx ) Then
              kappaVorg = kappaVorg + aerospc( i )%kappaorg *
     &                    aerospc_conc( i, im ) / aerospc( i )%density*1.0e-9
           End If
         End Do

         ! Account for Kappa of Nonvolatile POC and PNCOM species. This
         ! must be done separately because PNCOM has infinite OM:OC
         poa = aerospc_conc( apoc_idx, im ) + aerospc_conc( apncom_idx, im )
         kappaVorg = kappaVorg + 
     &               (0.11*poa / ( aerospc_conc(apoc_idx, im ) + tiny(0.0) ) -0.10)
     &               * (aerospc_conc( apncom_idx, im ) / aerospc( apncom_idx )%density +
     &                  aerospc_conc( apoc_idx,   im ) / aerospc( apoc_idx )%density )*1.0e-9

         ! Calculate Aggregate Kappa and volume Equivalent Diameter
         overallkappa = kappaVorg/totvol
         numparticles = moment0_conc( im )
         diam         = (6.0*totvol/numparticles/ pi ) ** (1.0/3.0)

         ! Compute aerosol water (Eq 3 of Petters and Kreidenweis)
         relhumid = min( airRH, 0.95 ) ! Cap RH at 95% to be conservative regarding uptake
         aw = activityw( relhumid, airtemp, diam ) ! activity of water
         If( overallkappa .gt. 1e-6 ) then ! prevents situation of little uptake
            aerospc_conc( aorgh2o_idx, im ) = aerospc( aorgh2o_idx )%density
     &       * aw/(1.0-aw) * kappaVorg * 1.0e9
         End If

      End If ! End organic water uptake

      ! Update 3rd moment, 2nd moment, and Dg in CBLK array by assuming that SOA
      ! condensation/evaporation does not affect the geometric standard deviation.

      ! Get new third moment of all species, do not get second moment
      ! directly from the calcmoments diagnostic. Instead, update it
      ! manually to account for condensation in ORGAER.
      Call calcmoments( .true. )
      m3wet_final( : ) = moment3_conc( 1:2 )
      m2wet_final( 1 ) = m2wet_init( 1 ) * ( m3wet_final( 1 ) /
     &                     m3wet_init( 1 ) ) ** ( 2.0 / 3.0 )
      m2wet_final( 2 ) = m2wet_init( 2 ) * ( m3wet_final( 2 ) / 
     &                     m3wet_init( 2 ) ) ** ( 2.0 / 3.0 )
      moment2_conc( 1:2 )  = m2wet_final( : )

      ! Update Budget Variables
      DO IM = 1,N_MODE
        ! Update Number and Surface Area
        COND_BUDGET( AERONUM_MAP( IM ) )  = MOMENT0_CONC( IM ) - CBUDGET0_NUM( IM )
        COND_BUDGET( AEROSRF_MAP( IM ) )  = MOMENT2_CONC( IM ) - CBUDGET0_SRF( IM )

        ! Update Organic Particles
        DO ISPC = 1,N_AEROSPC
          IF ( AEROSPC_MAP( ISPC,IM ) .NE. 0 ) 
     &         COND_BUDGET( AEROSPC_MAP( ISPC,IM ) ) = 
     &              AEROSPC_CONC( ISPC,IM ) - CBUDGET0_MASS( ISPC,IM )
        END DO

        ! Update Organic Vapors
        DO ISPC = 1,N_OA
          IF ( MAP_OGtoCGRID( ISPC ) .NE. 0 ) 
     &         COND_BUDGET( MAP_OGtoCGRID( ISPC ) ) = 
     &                 OG_CONC( ISPC ) - CBUDGET0_VAP( ISPC )
        END DO
      END DO

      Return
      End Subroutine orgaer

C-----------------------------------------------------------------------
      SUBROUTINE NEWT( LAYER, X, N, CHECK,
     &                 CTOT, CSAT, IMWCV, MINITW )

C  Description:
C   This subroutine and the underlying subprograms constitute Dr.
C   Benedikt Schell's SOA model.
C
C  Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.
C
C  Given an initial guess X(1:N) for a root in N dimensions, find
C  the root by a globally convergent Newton's method. The vector of
C  functions to be zeroed, called FVEC(1:N) in the routine below, is
C  returned by a user-supplied subroutine that must be called FUNCV
C  and have the declaration SUBROUTINE FUNCV(N,X,FVEC). The output
C  quantity CHECK is false on a normal return and true if the
C  routine has converged to a local minimum of the function FMINV
C  defined below. In this case, user should try restarting from a
C  different initial guess.
C
C  Key Subroutines Called: FDJAC, FMINV, LNSRCH, LUBKSB, LUDCMP
C
C  Revision History:
C     In CMAQ v4.3 - v4.7.1, this subroutine was embedded in a separate
C     Fortran module entitled SOA_NEWT.  Below, the Revision History from
C     the SOA_NEWT module has been merged with the Revision History from
C     this specific subroutine.
C
C CGN 01/12/04 removed ITS from call vector, added documentation, and
C     removed extraneous lines of code
C
C SLN 09/18/07 updated NP and NPREC for compatibility with new SOA module
C
C PVB 11/19/07 renamed NP to NCVAP for consistency with ORGAER5 subroutine
C
C SH  02/10/10 embedded the old Fortran module, SOA_NEWT, into a new module,
C     SOA_DEFN, so all SOA-related code can be found in one file.  Renamed
C     NCVAP to n_oa.  Its value is now set in SOA_DEFN (instead of inside
C     this subroutine).  Added TOLX to the call vector of Subroutine LNSRCH,
C     since that parameter is now set in this subroutine (instead of in the
C     Fortran module).
C
C  References:
C   1. Schell, B., I. J. Ackermann, H. Hass, F. S. Binkowski, and
C      A. Abel, Modeling the formation of secondary organic aerosol
C      within a comprehensive air quality modeling system, J. Geophys.
C      Res., Vol 106, No D22, 28275-28293, 2001.

      IMPLICIT NONE

C  Arguments

      INTEGER LAYER      ! model layer
      INTEGER N          ! dimension of problem
      REAL X( N )        ! initial guess of CAER
      LOGICAL CHECK
      REAL CTOT( N )     ! total concentration GAS + AER + PROD
      REAL CSAT( N )     ! saturation conc. of cond. vapor [ug/m^3]
      REAL IMWCV( N )    ! inverse molecular weights
      REAL MINITW        ! weighted initial mass

C  Following Numerical recipes

      Integer NN
      REAL :: FVEC( N )               ! vector of functions to be zeroed
      ! COMMON /NEWTV/ FVEC(n_oa), NN
      ! SAVE /NEWTV/

C  Parameters
      INTEGER, PARAMETER :: MAXITS = 100  ! maximum number of iterations
      REAL, PARAMETER :: TOLF = 1.0E-09   ! convergence criterion on fxn values
      REAL, PARAMETER :: TOLMIN = 1.0E-12 ! criterion whether spurious conver-
                                          ! gence to a minimum has occurred
      REAL, PARAMETER :: TOLX = 1.0E-10   ! convergence criterion on delta_x
      REAL, PARAMETER :: STPMX = 100.0    ! scaled maximum step length allowed

C  Local variables
      REAL :: CT( N ), CS( N ), IMW( N ), M
      ! REAL CS
      ! REAL IMW
      ! REAL M
      ! COMMON /NEWTINP/ CT( n_oa ), CS( n_oa ), IMW( n_oa ), M
      ! SAVE /NEWTINP/

      INTEGER I, ITS, J, INDX( N_OA )
      REAL D, DEN, F, FOLD, STPMAX, SUM, TEMP, TEST
      REAL FJAC( N_OA,N_OA )
      REAL G( N_OA), P( N_OA ), XOLD( N_OA )
!     EXTERNAL FDJAC

C-----------------------------------------------------------------------

      CHECK = .FALSE.
      M = MINITW
      DO I = 1, N
         CT( I ) = CTOT( I )
         CS( I ) = CSAT( I )
         IMW( I ) = IMWCV( I )
      END DO

      NN = N
      CALL FMINV( X,F,NN,FVEC,CT,CS,IMW,M )  ! The vector FVEC is also computed by this call
      TEST = 0.0              ! Test for initial guess being a root. Use more
                              ! stringent test than simply TOLF.
      DO I = 1, N
         IF ( ABS( FVEC( I ) ) .GT. TEST ) TEST = ABS( FVEC( I ))
      END DO

      IF ( TEST .LT. 0.01 * TOLF ) RETURN  ! initial guess is a root
      SUM = 0.0                    ! Calculate STPMAX for line searches
      DO I = 1, N
         SUM = SUM + X( I ) ** 2
      END DO
      STPMAX = STPMX * MAX( SQRT( SUM ), FLOAT( N ) )
      DO ITS = 1, MAXITS           ! start of iteration loop
         CALL FDJAC( N, X, FJAC, CT, CS, IMW, M )  ! get Jacobian
         DO I = 1, N               ! compute Delta f for line search
            SUM = 0.0
            DO J = 1, N
               SUM = SUM + FJAC( J,I ) * FVEC( J )
            END DO
            G( I ) = SUM
         END DO
         DO I = 1, N               ! store X
            XOLD( I ) = X( I )
         END DO
         FOLD = F                  ! store F
         DO I = 1, N               ! right-hand side for linear equations
            P( I ) = -FVEC( I )
         END DO
         CALL LUDCMP( FJAC, N, INDX, D ) ! solve linear equations by LU decomposition
         CALL LUBKSB( FJAC, N, INDX, P )
         CALL LNSRCH( CTOT,
     &                N, XOLD, FOLD, G,  ! LNSRCH returns new X and F. It also
     &                P, X, F, STPMAX,   ! calculates FVEC at the new X when it
     &                TOLX, CHECK, FVEC,
     &                CT, CS, IMW, M)      ! calls FMINV
         TEST = 0.0
         DO I = 1, N
            IF ( ABS( FVEC( I ) ) .GT. TEST ) TEST = ABS( FVEC( I ) )
         END DO
         IF ( TEST .LT. TOLF ) THEN
            CHECK = .FALSE.
            RETURN
         END IF
         IF ( CHECK ) THEN        ! Check for gradient of F zero,
            TEST = 0.0            ! i.e., spurious convergence.
            DEN = MAX( F, 0.5 * N )
            DO I = 1, N
               TEMP = ABS( G( I ) ) * MAX( ABS( X( I ) ), 1.0 ) / DEN
               IF ( TEMP .GT. TEST ) TEST = TEMP
            END DO
            IF ( TEST .LT. TOLMIN ) THEN
               CHECK = .TRUE.
            ELSE
               CHECK = .FALSE.
            END IF
            RETURN
         END IF
         TEST = 0.0             ! Test for convergence on delta_x
         DO I = 1, N
            TEMP = ( ABS( X( I ) - XOLD( I ) ) ) / MAX( ABS( X( I ) ), 1.0 )
            IF ( TEMP .GT. TEST ) TEST = TEMP
         END DO
         IF ( TEST .LT. TOLX ) RETURN
      END DO
      WRITE( *,'(a,i2)' ) 'MAXITS exceeded in NEWT ! Layer: ', LAYER
      END SUBROUTINE NEWT

C-----------------------------------------------------------------------
      SUBROUTINE FDJAC( N, X, FJAC, CT, CS, IMW, M )

C  Description:
C    Get the Jacobian of the function

C          ( a1 * X1^2 + b1 * X1 + c1 )
C          ( a2 * X2^2 + b2 * X2 + c2 )
C          ( a3 * X3^2 + b3 * X3 + c3 )
C   F(X) = ( a4 * X4^2 + b4 * X4 + c4 ) = 0.0
C          ( ........................ )
C          ( aN * XN^2 + bN * XN + cN )
C
C    a_i = IMW_i
C    b_i = SUM(X_j * IMW_j)_j.NE.i + CSAT_i * IMW_i  + M
C          - CTOT_i * IMW_i
C
C    c_i = - CTOT_i * [ SUM(X_j * IMW_j)_j.NE.i + M ]
C
C           delta F_i    ( 2. * a_i * X_i + b_i          If i .EQ. j
C   J_ij = ----------- = (
C           delta X_j    ( ( X_i  - CTOT_i ) * IMW_j     If i .NE. j

C Revision History:
C   CGN 01/12/04 changed B1 & B2 to scalars
C   SH  02/10/10 renamed NCVAP to n_oa. Value is inherited from SOA_DEFN.

      IMPLICIT NONE

      INTEGER N                 ! dimension of problem
      REAL X( N )               ! initial guess of CAER

      REAL CT( N )
      REAL CS( N )
      REAL IMW( N )
      REAL M

      REAL FJAC( N,N )

      INTEGER I, J              ! loop index
      REAL A( N_OA )
      REAL B( N_OA )
      REAL B1
      REAL B2
      REAL SUM_JNEI

      DO I = 1, N
         A( I ) = IMW( I )
         SUM_JNEI = 0.0
         DO J = 1, N
            SUM_JNEI = SUM_JNEI + X( J ) * IMW( J )
         END DO
         B1 = SUM_JNEI - ( X( I ) * IMW( I ) )
         B2 = ( CS( I ) - CT( I ) ) * IMW( I ) + M
         B( I ) = B1 + B2
      END DO
      DO J = 1, N
         DO I = 1, N
            IF ( I .EQ. J ) THEN
               FJAC( I,J ) = 2.0 * A( I ) * X( I ) + B( I )
            ELSE
               FJAC( I,J ) = ( X( I ) - CT( I ) ) * IMW( J )
            END IF
         END DO
      END DO

      RETURN
      END SUBROUTINE FDJAC

C-----------------------------------------------------------------------
      SUBROUTINE FMINV( X,F,N,FVEC, CT, CS, IMW, M )

C Description:
C    Returns f = 0.5 * F*F at X. SR FUNCV(N,X,F) is a fixed-name,
C    user-supplied routine that returns the vector of functions at X.
C    The common block NEWTV communicates the function values back to
C    NEWT.

C Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.

C Key Subroutines Called: FUNCV

C Revision History:
C   YOJ 07/31/02 changed FUNCTION FMIN to SUBROUTINE FMINV to avoid errors
C       with (some) compilers
C   SH  02/10/10 renamed NCVAP to n_oa. Value is inherited from SOA_DEFN.

      IMPLICIT NONE

      INTEGER N

      REAL X( * ), F
      REAL :: FVEC( N ), CT( N ), CS( N ), IMW( N ), M

      INTEGER I
      REAL SUM
      CALL FUNCV( N, X, FVEC, CT, CS, IMW, M )
      SUM = 0.0
      DO I = 1, N
         SUM = SUM + FVEC( I ) ** 2
      END DO
      F = 0.5 * SUM
      RETURN
      END SUBROUTINE FMINV

C-----------------------------------------------------------------------
      SUBROUTINE FUNCV( N, X, FVEC, CT, CS, IMW, M )

C Description:
C   From Equation (8) of Schell et al., 2001:
C     Caer,i = Ctot,i - Csat,i * (Caer,i/MWi) /
C                             ( sum_j (Caer,j/MWj) + Cinit/MWinit)
C   Let Xi  = Caer,i
C       a_i = 1 / MWi
C       M   = Cinit/MWinit
C       CTi = Ctot,i
C       CSi = Csat,i
C   Then,
C       Xi  = CTi - CSi * (a_i * Xi) / ( sum_j (a_j * Xj) + M )
C
C   Multiply above equation by sum_j(a_j*Xj) + M and group terms
C       a_i Xi^2 + ( sum_jnei (a_j*Xj) + M + CSi*a_i - CTi*a_i ) Xi
C                - CTi * ( sum_jnei (a_j*Xj) + M ) = 0
C
C   This equation is of the form F(X) = a_i*Xi^2 + b_i*Xi + c_i = 0.
C     F(X) is stored as FVEC in this subroutine.
C
C   See also FDJAC.

C Key Subroutines Called: none

C Revision History:
C CGN 01/12/04  Added documentation, removed extraneous lines of code
C SH  02/10/10 renamed NCVAP to n_oa.  Value is inherited from SOA_DEFN

C References:
C   1. Schell, B., I. J. Ackermann, H. Hass, F. S. Binkowski, and
C      A. Abel, Modeling the formation of secondary organic aerosol
C      within a comprehensive air quality modeling system, J. Geophys.
C      Res., Vol 106, No D22, 28275-28293, 2001.

      IMPLICIT NONE

      INTEGER N
      REAL X( * )
      REAL FVEC( N )

      REAL CT( N )
      REAL CS( N )
      REAL IMW( N )
      REAL M

      INTEGER I, J
      REAL SUM_JNEI
      REAL A( N )
      REAL B( N )
      REAL C( N )

      DO I = 1, N
         A( I ) = IMW( I )
         SUM_JNEI = 0.0
         DO J  = 1, N
            SUM_JNEI = SUM_JNEI + X( J ) * IMW( J )
         END DO
         SUM_JNEI = SUM_JNEI - ( X( I ) * IMW( I ) )
         B( I ) = SUM_JNEI + M + ( CS( I ) - CT( I ) ) * IMW( I )
         C( I ) = -CT( I ) * ( SUM_JNEI + M )
         FVEC( I ) = X( I ) * ( A( I ) * X( I ) + B( I ) ) + C( I )
      END DO

      RETURN
      END SUBROUTINE FUNCV

C-----------------------------------------------------------------------
      SUBROUTINE LNSRCH( CTOT,
     &                   N, XOLD, FOLD, G, P,
     &                   X, F, STPMAX, TOLX, CHECK, FVEC,
     &                   CT, CS, IMW, M )

C Description:
C   Given an n-dimensional point XOLD(1:N), the value of the function
C   and gradient there, FOLD and G(1:N), and a direction P(1:N),
C   finds a new point X(1:N) along the direction P from XOLD where
C   the function FUNC has decreased 'sufficiently'. The new function
C   value is returned in F. STPMAX is an input quantity that limits
C   the length of the steps so that you do not try to evaluate the
C   function in regions where it is undefined or subject to overflow.
C   P is usually the Newton direction. The output quantity CHECK is
C   false on a normal exit. It is true when X is too close to XOLD.
C   In a minimization algorithm, this usually signals convergence and
C   can be ignored. However, in a zero-finding algorithm the calling
C   program should check whether the convergence is spurious.
C
C  Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.

C Key Subroutines Called: FUNCV

C Revision History:
C   SH  02/10/10 added TOLX to the call vector.  In previous versions, this
C       parameter was declared in the Module SOA_NEWT (which contained this
C       subroutine).

      IMPLICIT NONE

      INTEGER N
      REAL TOLX
      LOGICAL CHECK
      REAL F, FOLD, STPMAX
      REAL G( N ), P( N ), X( N ), XOLD( N )
      REAL CTOT( N )
      REAL, PARAMETER :: ALF = 1.E-04
      REAL, PARAMETER :: CONMIN = 1.E-30
      REAL :: FVEC( N ), CT( N ), CS( N ), IMW( N ), M

      INTEGER I
      REAL A, ALAM, ALAM2, ALAMIN, B, DISC
      REAL F2, FOLD2, RHS1, RHS2, SLOPE
      REAL SUM, TEMP, TEST, TMPLAM

      CHECK = .FALSE.
      SUM = 0.0
      DO I = 1, N
         SUM = SUM + P( I ) * P( I )
      END DO
      SUM = SQRT( SUM )
      IF ( SUM .GT. STPMAX ) THEN
         DO I = 1, N
            P( I ) = P( I ) * STPMAX / SUM
         END DO
      END IF
      SLOPE = 0.0
      DO I = 1, N
         SLOPE = SLOPE + G( I ) * P( I )
      END DO
      TEST = 0.0
      DO I = 1, N
         TEMP = ABS( P( I ) ) / MAX( ABS( XOLD( I ) ), 1.0 )
         IF ( TEMP .GT. TEST ) TEST = TEMP
      END DO
      ALAMIN = TOLX / TEST
      ALAM = 1.0

101   CONTINUE

C  avoid negative concentrations and set upper limit given by CTOT.

      DO I = 1, N
         X( I ) = XOLD( I ) + ALAM * P( I )
         IF ( X( I ) .LE. 0.0 )       X( I ) = CONMIN
         IF ( X( I ) .GT. CTOT( I ) ) X( I ) = CTOT( I )
      END DO
      CALL FMINV( X,F,N,FVEC,CT,CS,IMW,M )
      IF ( ALAM .LT. ALAMIN ) THEN
         DO I = 1, N
            X( I ) = XOLD( I )
         END DO
         CHECK = .TRUE.
         RETURN
      ELSE IF ( F .LE. FOLD + ALF * ALAM * SLOPE ) THEN
         RETURN
      ELSE
         IF ( ALAM .EQ. 1.0 ) THEN
            TMPLAM = -SLOPE / ( 2.0 * ( F - FOLD - SLOPE ) )
         ELSE
            RHS1 = F - FOLD - ALAM * SLOPE
            RHS2 = F2 - FOLD2 - ALAM2 * SLOPE
            A = ( RHS1 / ALAM ** 2 - RHS2 / ALAM2 ** 2 ) / ( ALAM - ALAM2 )
            B = ( -ALAM2 * RHS1 / ALAM ** 2 + ALAM * RHS2 / ALAM2 ** 2 )
     &        / ( ALAM - ALAM2 )
            IF ( A .EQ. 0.0 ) THEN
               TMPLAM = -SLOPE / ( 2.0 * B )
            ELSE
               DISC  = B * B - 3.0 * A * SLOPE
               TMPLAM = ( -B + SQRT( DISC ) ) / ( 3.0 * A )
            END IF
            IF ( TMPLAM .GT. 0.5 * ALAM ) TMPLAM = 0.5 * ALAM
         END IF
      END IF
      ALAM2 = ALAM
      F2 = F
      FOLD2 = FOLD
      ALAM = MAX( TMPLAM, 0.1 * ALAM )
      GO TO 101

      END SUBROUTINE LNSRCH

C-----------------------------------------------------------------------
      SUBROUTINE LUBKSB( A, N, INDX, B )

C Description:
C   Solves the set of N linear equations A * X = B. Here A is input,
C   not as the matrix A but rather as its LU decomposition,
C   determined by the routine LUDCMP. B(1:N) is input as the right-
C   hand side vector B, and returns with the solution vector X. A, N,
C   and INDX are not modified by this routine and can be left in
C   place for successive calls with different right-hand sides B.
C   This routine takes into account the possibility that B will begin
C   with many zero elements, so it is efficient for use in matrix
C   inversion.
C
C  Adopted from Numerical Recipes in FORTRAN, Chapter 2.3, 2nd ed.
C
C Key Subroutines Called: none
C
C Revision History:
C    call vector modified to remove NCVAP and set dimensions to N.

      IMPLICIT NONE

      INTEGER N, INDX( N )
      REAL A( N,N ), B( N ) ! A now has dimension NxN.

      INTEGER I, II, J, LL
      REAL SUM

      II = 0
      DO I = 1, N
         LL = INDX( I )
         SUM = B( LL )
         B( LL ) = B( I )
         IF ( II .NE. 0 ) THEN
            DO J = II, I-1
               SUM = SUM - A( I,J ) * B( J )
            END DO
         ELSE IF ( SUM .NE. 0 ) THEN
            II = I
         END IF
         B( I ) = SUM
      END DO
      DO I = N, 1, -1
         SUM = B( I )
         DO J = I+1, N
            SUM = SUM - A( I,J ) * B( J )
         END DO
         B( I ) = SUM / A( I,I )
      END DO

      RETURN
      END SUBROUTINE LUBKSB

C-----------------------------------------------------------------------
      SUBROUTINE LUDCMP( A, N, INDX, D )

C Description:
C   Given a matrix A(1:N,1:N), with physical dimension N by N, this
C   routine replaces it by the LU decomposition of a rowwise
C   permutation of itself. A and N are input. A is output arranged as
C   in equation (2.3.14) above; INDX(1:N) is an output vector that
C   records vector that records the row permutation effected by the
C   partial pivoting; D is output as +-1 depending on whether the
C   number of row interchanges was even or odd, respectively. This
C   routine is used in combination with SR LUBKSB to solve linear
C   equations or invert a matrix.
C
C  Adopted from Numerical Recipes in FORTRAN, Chapter 2.3, 2nd ed.

C  Equation (2.3.14) Numerical Recipes, p 36:
C   | b_11 b_12 b_13 b_14 |
C   | a_21 b_22 b_23 b_24 |
C   | a_31 a_32 b_33 b_34 |
C   | a_41 a_42 a_43 b_44 |

C Key Subroutines Called: None

C Revision History:
C    call vector modified to remove NCVAP
C    all dimensions now depend upon N only

      IMPLICIT NONE

      INTEGER N, INDX( N )
!     INTEGER NMAX
!     PARAMETER ( NMAX = 10 )   ! largest expected N
      REAL D, A( N,N )     ! note that A now has dimension NxN
                           ! NCVAP is ignored
      REAL, PARAMETER :: TINY = 1.0E-20

      INTEGER I, IMAX, J, K
      REAL AAMAX, DUM, SUM, VV( N )

      D = 1.0
      DO I = 1, N
         AAMAX = 0.0
         DO J = 1, N
            IF ( ABS(A( I,J ) ) .GT. AAMAX ) AAMAX = ABS( A( I,J ) )
         END DO
         IF ( AAMAX .EQ. 0.0 ) THEN
            WRITE( *,'(a)' ) '*** Singular matrix in ludcmp!'
         END IF
         VV( I ) = 1.0 / AAMAX
      END DO
      DO J = 1, N
         DO I = 1, J-1
            SUM = A( I,J )
            DO K = 1, I-1
               SUM = SUM - A( I,K ) * A( K,J )
            END DO
            A( I,J ) = SUM
         END DO
         AAMAX = 0.0
         DO I = J, N
            SUM = A( I,J )
            DO K = 1, J-1
               SUM = SUM - A( I,K ) * A( K,J )
            END DO
            A( I,J ) = SUM
            DUM = VV( I ) * ABS( SUM )
            IF ( DUM .GE. AAMAX ) THEN
               IMAX = I
               AAMAX = DUM
            END IF
         END DO
         IF ( J .NE. IMAX ) THEN
            DO K = 1, N
               DUM = A( IMAX,K )
               A( IMAX,K ) = A( J,K )
               A( J,K ) = DUM
            END DO
            D = -D
            VV( IMAX ) = VV( J )
         END IF
         INDX( J ) = IMAX
         IF ( A( J,J ) .EQ. 0.0 ) A( J,J ) = TINY
         IF ( J .NE. N ) THEN
            DUM = 1.0 / A( J,J )
            DO I = J+1, N
               A( I,J ) = A( I,J ) * DUM
            END DO
         END IF
      END DO

      RETURN
      END SUBROUTINE LUDCMP

C-----------------------------------------------------------------------
      Function soabisect( lowerb, upperb, nonvolmol, n, cstaratt, totsemivol,
     &                    mlwt) RESULT ( nroot )

C     Determines the root of an equation, nroot, that is located
C     between the lowerb and upperb. The equation of interest is defined
C     in the function soaequation. Before calling soabisect, the threshold 
C     for SOA should have already been checked which should ensure a solution.
C     The recommended lower bound is the number of moles of nonvolatile aerosol
C     and the recommended upper bound is the total number of organic moles
C
C     History
C     Created 7/2011 by HOT Pye

      Use utilio_defn
      Use runtime_vars, only: logdev


      Implicit None

C     Function arguments
      Real, Intent(IN) :: lowerb, upperb      ! lower bound, upper bound 
      Real, Intent(IN) :: nonvolmol           ! nonvolatile aerosol in moles = POA + non-voltile SOA
      Integer, Intent(IN) :: n
      Real, Intent(IN) :: cstaratt(n)   ! Cstars at T of interest
      Real, Intent(IN) :: totsemivol(n) ! total semivolatile to partition in ug/m3 (gas+aer+newlyformed)
      Real, Intent(IN) :: mlwt(n)       ! molecular weight of semivolatiles 

C     Parameters for solution convergenc
      Real             :: FRACTOL = 1.0e-6    ! Solution converged if abs(old-new)/new < FRACTOL
!     Real             :: ABSTOL  = 1.0e-10   ! Solution converged if there are less than ABSTOL umol/m3 in aerosol
      Real             :: ABSTOL  = 1.0e-08   ! Solution converged if there are less than ABSTOL umol/m3 in aerosol

C     Result
      Real             :: nroot               ! solution (final nguess)

C     Local variables
      Real             :: nguess              ! current guess for total moles organic aerosol
      Real             :: oldn, lower, upper, flower, fnguess ! intermediate values
      Real             :: fupper              ! function evaluated at upper bound
      Real             :: ea                  ! difference b/w old and new guesses (umol/m3)
      Real             :: test                ! to detect sign change
      Integer          :: iter                ! counter to prevent infinite loops
      Character( 120 ) :: xmsg

C     Store current lower and upper bound, evaluate at lower bound 
      lower  = lowerb
      upper  = upperb
      flower = soaequation( nonvolmol, n, cstaratt, totsemivol, mlwt, lowerb )
      fupper = soaequation( nonvolmol, n, cstaratt, totsemivol, mlwt, upperb )

C     First guess: solution at upperb
      nguess = upperb

C     Counter of iterations to prevent infinite loops
      iter = 0

C     Iterate until the number of moles changes by less than fractol (percent) or
C     abstol (absolute) amount
      Do

        If ( flower * fupper .gt. 0.0 ) Then
           ! function does not change sign between bounds
#ifdef verbose_soa
           nroot = soabisect_debug(lowerb, upperb, nonvolmol, cstaratt, totsemivol, mlwt)
#endif
           Write( xmsg,'(a,2(1pe15.5))' )
     &           'Error: no solution between bounds in soa bisection:', flower, fupper
           Call m3exit( pname, 0, 0, xmsg, xstat3)
        End If

        iter   = iter + 1
        oldn   = nguess                     ! store last guess
        nguess = ( lower + upper ) / 2.0e0  ! new guess is halfway b/w old lower and upper
        fnguess = soaequation( nonvolmol, n, cstaratt, totsemivol, mlwt, nguess ) ! function evaluated at new guess
        If ( nguess .ne. 0.0 ) Then            ! safe division
           ea = Abs( nguess - oldn ) / nguess  ! fractional diff b/w 2 guesses
        Else
#ifdef verbose_soa
          nroot = soabisect_debug(lowerb, upperb, nonvolmol, cstaratt, totsemivol, mlwt)
          Write( xmsg,'(a,1pe15.5)' )
     &       'Warning: number of organic aerosol moles going to zero in soa bisection:',
     &        nroot
#else
          xmsg = 'Warning: number of organic aerosol moles going to zero in soa bisection'
#endif
          Call m3exit( pname, 0, 0, xmsg, xstat3 )
        End If

        test = fnguess * flower
        If ( test .Lt. 0.0 ) Then         ! if function changes sign b/w guess and lower, guess becomes upper
          upper  = nguess
          fupper = fnguess
        Else If ( test .Gt. 0.0 ) Then    ! if function does not change sign, guess becomes lowerbound
          lower  = nguess
          flower = fnguess
        Else If ( flower .Eq. 0.0 ) Then  ! if the lower bound is the root
          ea      = 0.0
          nguess  = lower
          fnguess = flower
        Else If ( fnguess .Eq. 0.0 ) Then ! guess is the root
          ea      = 0.0
        End If

        ! solution found if error is less than ABSTOL/100 % or N bounded by values less than ABSTOL
        If ( ( ea .Lt. FRACTOL ) .Or. ( ( upper + lower ) .Lt. ABSTOL ) ) Then
          nroot = nguess
          ! double check that the function is approximately zero
          fnguess = soaequation( nonvolmol, n, cstaratt, totsemivol, mlwt, nguess ) ! function evaluated at new guess
          If ( Abs( fnguess ) .Gt. 1.0e-03 ) Then
!           pfc = pfc + 1
#ifdef verbose_soa
            nroot = soabisect_debug(lowerb, upperb, nonvolmol, cstaratt, totsemivol, mlwt)
!           Write( xmsg,'(a,2(1pe15.5,i8))' )
!    &      'Warning: possible false convergence in soa bisection:', fnguess, nroot, pfc
            Write( xmsg,'(a,2(1pe15.5))' )
     &      'Warning: possible false convergence in soa bisection:', fnguess, nroot
            Call m3warn( pname, 0, 0, xmsg )
#else
            xmsg = 'Warning: possible false convergence in soa bisection'
#endif
!           Call m3warn( pname, 0, 0, xmsg )
          End If

          Return
        End If

        If ( iter .Gt. 1000000 ) Then ! stop infinite loop
#ifdef verbose_soa
          nroot = soabisect_debug(lowerb, upperb, nonvolmol, cstaratt, totsemivol, mlwt)
          Write( xmsg,'(a,i9,1pe15.5)' )
     &      'Error: too many iterations in soa bisection', iter, nroot
#else
          xmsg = 'Error: too many iterations in soa bisection'
#endif
          Call m3exit( pname, 0, 0, xmsg, xstat3 )
        End If
      End Do

      End Function soabisect

C-----------------------------------------------------------------------
      Function soaequation( nonvmol, n, cstar, semivol, mlwt, currentN ) Result ( eqnerror )

C     Evaluates the following function of total aerosol moles, N
C     Function evaluates to zero at equilibrium 
C     
C                      totalsemivol_i        mols nonvolatile POA + SOA
C     f(N) = sum_i (  ----------------   ) + ---------------------------  - 1.0 
C                     cstar_i + mw_i*N                  N
C
C     History
C     Created 7/2011 HOT Pye
      Use runtime_vars, only: logdev

      Implicit None

C     Function inputs and output
      Real, INTENT(IN) :: nonvmol, currentN             ! nonvolatile moles, total moles
      Integer, INTENT(IN) :: n                          ! number of semivolatile species
      Real, INTENT(IN) :: mlwt(:)                    ! molecular weights of semivolatiles
      Real, INTENT(IN) :: semivol(:), cstar(:) ! semivolatile mass, sat conc in ug/m3
      Real             :: eqnerror                         ! result, deviation from 0.0 in equation

C     Local variables
      Real             :: temptot                          ! temporary total
      Integer          :: i                                ! counter

C     Compute function value for current value of N 
      temptot = 0.0e0
      Do i = 1, n
         temptot = temptot + semivol(i) / ( cstar(i) + mlwt(i) * currentN )
      End Do
      eqnerror = temptot + nonvmol / currentN - 1.0e0

      End function soaequation

C-----------------------------------------------------------------------
      Function soabisect_debug( lowerb, upperb, nonvolmol, n, cstaratt, totsemivol,
     &                          mlwt) RESULT ( nroot )

      Use utilio_defn
      Use runtime_vars

      Implicit None

C     Function arguments
      Real, Intent(IN) :: lowerb, upperb      ! lower bound, upper bound
      Real, Intent(IN) :: nonvolmol           ! nonvolatile aerosol in moles = POA + non-voltile SOA
      Integer, Intent(IN) :: n                ! Number of semivolatile species
      Real, Intent(IN) :: cstaratt(:)   ! Cstars at T of interest
      Real, Intent(IN) :: totsemivol(:) ! total semivolatile to partition in ug/m3 (gas+aer+newlyformed)
      Real, Intent(IN) :: mlwt(:)       ! molecular weight of semivolatiles

C     Parameters for solution convergenc
      Real             :: FRACTOL = 1.0e-6    ! Solution converged if abs(old-new)/new < FRACTOL
      Real             :: ABSTOL  = 1.0e-10   ! Solution converged if there are less than ABSTOL umol/m3 in aerosol

C     Result
      Real             :: nroot               ! solution (final nguess)

C External functions:

C     Local variables
      Logical, Save    :: first_time = .True.

      Real             :: nguess              ! current guess for total moles organic aerosol
      Real             :: oldn, lower, upper, flower, fnguess ! intermediate values
      Real             :: fupper              ! function evaluated at upper bound
      Real             :: ea                  ! difference b/w old and new guesses (umol/m3)
      Real             :: test                ! to detect sign change
      Integer          :: iter                ! counter to preventinfinite loops
      Character( 120 ) :: xmsg

      If ( first_time )  Then
        first_time = .False.
      End If

C     Store current lower and upper bound, evaluate at lower bound
      lower  = lowerb
      upper  = upperb
      flower = soaequation( nonvolmol, n, cstaratt, totsemivol, mlwt, lowerb )
      fupper = soaequation( nonvolmol, n, cstaratt, totsemivol, mlwt, upperb )

C     First guess: solution at upperb
      nguess = upperb

C     Counter of iterations to prevent infinite loops
      iter = 0

C     Iterate until the number of moles changes by less than fractol
C     (percent) or abstol (absolute) amount
      Do

        If ( flower * fupper .Gt. 0.0 ) Then
          ! function does not change sign between bounds
          xmsg = 'Error: no solution between bounds in soa bisection'
          Write( logdev,* ) xmsg
        End If

        iter    = iter + 1
        oldn    = nguess                    ! store last guess
        nguess  = ( lower + upper ) / 2.0e0 ! new guess is halfway b/w old lower and upper
        fnguess = soaequation( nonvolmol, n, cstaratt, totsemivol, mlwt, nguess ) ! function evaluated at new guess

        Write( logdev,* ), 'iter   ', iter
        Write( logdev,* ), 'Lower= ', lower, flower
        Write( logdev,* ), 'Upper= ', upper, fupper
        Write( logdev,* ), 'Guess= ', nguess, fnguess

        If ( nguess .Ne. 0.0 ) Then           ! safe division
          ea = ABS( nguess - oldn )/nguess   ! fractional diff b/w 2guesses
        Else
          xmsg = 'Error: number of organic aerosol moles going to zero in soa bisection'
          Write( logdev,* ) xmsg
        End If

        Write( logdev,* ) 'error', ea
        test = fnguess * flower
        If ( test .Lt. 0.0 ) Then           ! if function changes sign b/w guess and lower, guess becomes upper
          upper  = nguess
          fupper = fnguess
        Else If ( test .Gt. 0.0 ) Then      ! if function does not change sign, guess becomes lowerbound
          lower  = nguess
          flower = fnguess
        Else If ( flower .Eq. 0.0 ) Then    ! if the lower bound is the root
          ea      = 0.0
          nguess  = lower
          fnguess = flower
        Else If ( fnguess .Eq. 0.0 ) Then   ! guess is the root
          ea      = 0.0
        End If

        ! solution found if error is less than ABSTOL/100 % or N bounded
        ! by values less than 1e-08
        If ( ( ea .Lt. FRACTOL ) .Or. ( ( upper + lower ) .Lt. ABSTOL ) ) then
          nroot = nguess
          ! double check that the function is approximately zero
          fnguess = soaequation( nonvolmol, n, cstaratt, totsemivol, mlwt, nguess ) ! function evaluated at new guess
          If ( Abs( fnguess ) .Gt. 1.0e-03 ) Then
             xmsg = 'Warning: possible false convergence in soa bisection'
             Write( logdev,* ) xmsg
          End If

          Return
        End If

        If ( iter .Gt. 1000000 ) Then ! stop infinite loops
          !print*,'current lower, upper, and N: ',lower, upper, nguess
          xmsg = 'Error: too many iterations in soa bisection'
          Write( logdev,* ) xmsg
        End If
      End Do

      End Function soabisect_debug

!-----------------------------------------------

      Function activityw( rh, temp, dtot ) result( aw )

C Function calculates the activity of water
C over an aqueous solution droplet of specified diameter
C Equation 1 of Petters and Kreidenweis 2007 ACP:
C
C                   -4*surftens*Mw
C   aw  = RH * exp( ---------------)
C                     R*T*dens*D
C
C The diameter is the wet, volume equivalent, diameter of the particle.

      Implicit None
                                       
      Include SUBST_CONST    ! common constants

C     Function arguments
      Real rh      ! RH between 0.005, 0.99 [fraction]
      Real temp    ! temperature [K]
      Real dtot    ! volume equivalent diameter of entire particle [m]

C     Function return value
      Real aw      ! activity of water [fraction]

C     Local variables
      Real partialaw ! intermediate value

C     Parameters
      Real, Parameter :: surftens = 0.072e0  ! surface tension of water [J/m2]
      Real, Parameter :: mwtwater = 0.018e0  ! molecular weight of water [kg/mol]
      Real, Parameter :: denswater = 1000e0  ! density of water [kg/m3]

C     Calculations
      partialaw = 4.0*surftens*mwtwater/(rgasuniv*temp*denswater)
      aw = rh / exp( partialaw/dtot )

      End Function activityw

!-----------------------------------------------

      End Module soa_defn
