
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
      Module aq_data

C  Defines gas and aerosol surrogate data.
C  this module replace the AQ_PARAMS.EXT include file
C
C  Revision History:
C     First version was coded in May 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C     Feb 2011: Steve Howard, Shawn Roselle, Prakash Bhave, Bill Hutzell -
C     Mar 2011: Sergey Napelenok - AE6 update for Ca/Mg/K/Fe/Mn 
C               indexing, density, variable names
C     Mar 2011: Shawn Roselle, replaced I/O API include files with UTILIO_DEFN
C     Sep 2011: B.Hutzell: adapted for mercury and HAP mechanisms
C     Jun 2012: J.Young: remove full character blank padding for GNU Fortran (GCC) 4.1.2
C     Oct 2014: B.Hutzell:forcing to skip mapping if aerosol species
C     index equals the values for H3OP species as for aerosol water
C     species 
C-----------------------------------------------------------------------
      Use aero_data, Only: n_aerolist, n_aerospc, conmin
      Use runtime_vars
#ifdef mpas
      Use util_module, only : index1
#endif

      Implicit None

      Integer, Parameter :: ngas = 15   ! number of gas-phase species for AQCHEM
!     Integer, Parameter :: naddaer = 7 ! number of additional aerosol surrogate species for AQCHEM
      Integer, Parameter :: naddaer = 2 ! number of additional aerosol surrogate species for AQCHEM
      Integer, Parameter :: nadd_tx = 6 ! number of surrogates for HAPS aerosol species for AQCHEM

      Integer, Parameter :: max_naer = n_aerolist + naddaer +  nadd_tx ! max number aero surrogates

      Integer, Save      :: naer        ! number of aerosol surrogate species for AQCHEM
      Integer, Parameter :: nliqs = 64  ! number of liquid-phase species in AQCHEM
      Integer, Parameter :: nmodes = 3  ! number of aerosol modes
      Integer, Parameter :: mxsrg = 70  ! maximum number of species for surrogate

      integer, parameter :: akn = 1     ! number for Aitken mode
      Integer, Parameter :: acc = 2     ! number for Accumulation mode
      Integer, Parameter :: cor = 3     ! number for Coarse mode

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C...Gas surrogates species 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      Type sur_gas_type
         Character( 16 ) :: name               ! surrogate name
         Real            :: backgnd            ! background value
      End Type sur_gas_type

      Type ( sur_gas_type ), Parameter :: srggas( ngas ) = (/
C                          Name       Background
C                       ----------    ----------
     & sur_gas_type( 'SO2       ',        0.0 ),
     & sur_gas_type( 'HNO3      ',        0.0 ),
     & sur_gas_type( 'N2O5      ',        0.0 ),
     & sur_gas_type( 'CO2       ',      340.0 ),
     & sur_gas_type( 'NH3       ',        0.0 ),
     & sur_gas_type( 'H2O2      ',        0.0 ),
     & sur_gas_type( 'O3        ',        0.0 ),
     & sur_gas_type( 'FOA       ',        0.0 ),
     & sur_gas_type( 'MHP       ',        0.0 ),
     & sur_gas_type( 'PAA       ',        0.0 ),
     & sur_gas_type( 'H2SO4     ',        0.0 ),
     & sur_gas_type( 'HCL       ',        0.0 ),
     & sur_gas_type( 'GLY       ',        0.0 ),
     & sur_gas_type( 'MGLY      ',        0.0 ),
     & sur_gas_type( 'HO        ',        0.0 ) / )

C...Required gas species
      Character( 16 ), Parameter, Private :: req_so2   = 'SO2'
      Character( 16 ), Parameter, Private :: req_hno3  = 'HNO3'
      Character( 16 ), Parameter, Private :: req_n2o5  = 'N2O5'
      Character( 16 ), Parameter, Private :: req_co2   = 'CO2'
      Character( 16 ), Parameter, Private :: req_nh3   = 'NH3'
      Character( 16 ), Parameter, Private :: req_h2o2  = 'H2O2'
      Character( 16 ), Parameter, Private :: req_o3    = 'O3'
      Character( 16 ), Parameter, Private :: req_foa   = 'FOA'
      Character( 16 ), Parameter, Private :: req_mhp   = 'MHP'
      Character( 16 ), Parameter, Private :: req_paa   = 'PAA'
      Character( 16 ), Parameter, Private :: req_h2so4 = 'H2SO4'
      Character( 16 ), Parameter, Private :: req_hcl   = 'HCL'
      Character( 16 ), Parameter, Private :: req_gly   = 'GLY'
      Character( 16 ), Parameter, Private :: req_mgly  = 'MGLY'
      Character( 16 ), Parameter, Private :: req_ho    = 'HO'

C...Pointers for the AQCHEM array GAS

      Integer :: lso2     ! Sulfur Dioxide
      Integer :: lhno3    ! Nitric Acid
      Integer :: ln2o5    ! Dinitrogen Pentoxide
      Integer :: lco2     ! Carbon Dioxide
      Integer :: lnh3     ! Ammonia
      Integer :: lh2o2    ! Hydrogen Peroxide
      Integer :: lo3      ! Ozone
      Integer :: lfoa     ! Formic Acid
      Integer :: lmhp     ! Methyl Hydrogen Peroxide
      Integer :: lpaa     ! Peroxyacetic Acid
      Integer :: lh2so4   ! Sulfuric Acid
      Integer :: lhcl     ! Hydrogen Chloride
      Integer :: lgly     ! Glyoxal
      Integer :: lmgly    ! Methylglyoxal
      Integer :: lho      ! OH, hydroxyl radical

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C...Aerosol surrogates species 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      Type sur_aer_type
         Character( 16 ) :: name( nmodes )     ! surrogate name
         Real            :: backgnd            ! background value
         Real            :: molwt              ! molecular weight
         Real            :: density            ! density [kg m-3] 
      End Type sur_aer_type

      Type ( sur_aer_type ), Allocatable, Save  :: srgaer( : )

C... Additional Aerosol surrogate species to add to aerosol list
      Type ( sur_aer_type ), private :: addsrgaer( naddaer ) = (/
C                         Aitken           Accum            Coarse        Background MolWt  Density
C                      -------------    -------------    -------------    ---------- -----  ------
!    & sur_aer_type((/'             ', '             ', 'CACO3        '/),   0.0,    100.1, 2200.0),
!    & sur_aer_type((/'             ', '             ', 'MGCO3        '/),   0.0,     84.3, 2200.0),
!    & sur_aer_type((/'             ', '             ', 'A3FE         '/),   0.0,     55.8, 2000.0),
!    & sur_aer_type((/'             ', '             ', 'B2MN         '/),   0.0,     54.9, 2000.0),
!    & sur_aer_type((/'             ', '             ', 'K            '/),   0.0,     39.1, 2200.0),
     & sur_aer_type((/'NUM_AITKEN    ', 'NUM_ACCUM     ', 'NUM_COARSE    '/),   0.0,      0.0,    1.0),
     & sur_aer_type((/'SRF_AITKEN    ', 'SRF_ACCUM     ', 'SRF_COARSE    '/),   0.0,      0.0,    1.0)/)

C...TX and other species added to Aerosol surrogate if not found in AE namelist file      
      Type ( sur_aer_type ), private :: srgaer_addtx( nadd_tx ) = (/
C                         Aitken           Accum            Coarse        Background MolWt  Density
C                      -------------    -------------    -------------    ---------- -----  ------
     & sur_aer_type((/'TRACER_AITKEN ', '              ', '              '/),   0.0,    100.0, 2200.0),
     & sur_aer_type((/'              ', 'TRACER_ACCUM  ', '              '/),   0.0,    100.0, 2200.0),
     & sur_aer_type((/'              ', '              ', 'TRACER_COARSE '/),   0.0,    100.0, 2200.0),
     & sur_aer_type((/'PHG_AITKEN    ', '              ', '              '/),   0.0,    200.5, 2200.0),
     & sur_aer_type((/'              ', 'PHG_ACCUM     ', '              '/),   0.0,    200.5, 2200.0),
     & sur_aer_type((/'              ', '              ', 'PHG_COARSE    '/),   0.0,    200.5, 2200.0)/)

C...Required aerosol species
      Character( 16 ), Parameter, Private :: req_aso4   = 'SO4_ACCUM'
      Character( 16 ), Parameter, Private :: req_anh4   = 'NH4_ACCUM'
      Character( 16 ), Parameter, Private :: req_ano3   = 'NO3_ACCUM'
      Character( 16 ), Parameter, Private :: req_asoa   = 'SOA_ACCUM'
      Character( 16 ), Parameter, Private :: req_aorgc  = 'AORGC_ACCUM'
      Character( 16 ), Parameter, Private :: req_apoa   = 'POA_ACCUM'
      Character( 16 ), Parameter, Private :: req_aec    = 'EC_ACCUM'
      Character( 16 ), Parameter, Private :: req_apri   = 'PRI_ACCUM'
!     Character( 16 ), Parameter, Private :: req_apric  = 'PRI_COARSE'  ! SLN 16March2011
      Character( 16 ), Parameter, Private :: req_ana    = 'NA_ACCUM'
      Character( 16 ), Parameter, Private :: req_acl    = 'CL_ACCUM'
      Character( 16 ), Parameter, Private :: req_acaco3 = 'CACO3'
      Character( 16 ), Parameter, Private :: req_amgco3 = 'MGCO3'
      Character( 16 ), Parameter, Private :: req_aa3fe  = 'A3FE'
      Character( 16 ), Parameter, Private :: req_ab2mn  = 'B2MN'
      Character( 16 ), Parameter, Private :: req_ak     = 'K'
      Character( 16 ), Parameter, Private :: req_num    = 'NUM_ACCUM'
      Character( 16 ), Parameter, Private :: req_srf    = 'SRF_ACCUM'

      Character( 16 ), Parameter, Private :: req_caacc  = 'CA_ACCUM'    ! SLN 16March2011
      Character( 16 ), Parameter, Private :: req_mgacc  = 'MG_ACCUM'    ! SLN 16March2011
      Character( 16 ), Parameter, Private :: req_kacc   = 'K_ACCUM'     ! SLN 16March2011
      Character( 16 ), Parameter, Private :: req_feacc  = 'FE_ACCUM'    ! SLN 22March2011
      Character( 16 ), Parameter, Private :: req_mnacc  = 'MN_ACCUM'    ! SLN 22March2011
      Character( 16 ), Parameter, Private :: req_soilc  = 'SOIL_COARSE' ! SLN 16March2011
      Character( 16 ), Parameter, Private :: req_anthc  = 'ANTH_COARSE' ! SLN 16March2011
      Character( 16 ), Parameter, Private :: req_seasc  = 'SEAS_COARSE' ! SLN 16March2011
C...For TX version      
      Character( 16 ), Parameter, Private :: req_traceri = 'TRACER_AITKEN'
      Character( 16 ), Parameter, Private :: req_tracerj = 'TRACER_ACCUM'
      Character( 16 ), Parameter, Private :: req_tracerk = 'TRACER_COARSE'
      Character( 16 ), Parameter, Private :: req_phgi    = 'PHG_AITKEN'
      Character( 16 ), Parameter, Private :: req_phgj    = 'PHG_ACCUM'
      Character( 16 ), Parameter, Private :: req_phgk    = 'PHG_COARSE'
C...For Sulfur tracking
      Character( 16 ), Parameter, Private :: req_aso4aqh2o2 = 'SO4_AQH2O2_ACCUM'
      Character( 16 ), Parameter, Private :: req_aso4aqo3   = 'SO4_AQO3_ACCUM'
      Character( 16 ), Parameter, Private :: req_aso4aqfemn = 'SO4_AQFEMN_ACCUM'
      Character( 16 ), Parameter, Private :: req_aso4aqmhp  = 'SO4_AQMHP_ACCUM'
      Character( 16 ), Parameter, Private :: req_aso4aqpaa  = 'SO4_AQPAA_ACCUM'


C...Pointers for the AQCHEM array AEROSOL
      Integer :: lso4     !  Sulfate
      Integer :: lnh4     !  Ammonium
      Integer :: lno3     !  Nitrate
      Integer :: lsoa     !  SOA
      Integer :: lorgc    !  SOA (anthropogenic organic oligomers)
      Integer :: lpoa     !  Primary organic aerosol
      Integer :: lec      !  Elemental carbon
      Integer :: lpri     !  Primary aerosol (Aitken and Accumulation)
!     Integer :: lpricor  !  Primary aerosol (coarse)
      Integer :: lna      !  Sodium
      Integer :: lcl      !  Chloride ion
!     Integer :: lcaco3   !  Calcium Carbonate aerosol (place holder)
!     Integer :: lmgco3   !  Magnesium Carbonate aerosol (place holder)
!     Integer :: la3fe    !  Iron aerosol (place holder)
!     Integer :: lb2mn    !  Manganese aerosol (place holder)
!     Integer :: lk       !  Potassium aerosol (Cl- tracked separately) (place holder)
      Integer :: lnum     !  Number
      Integer :: lsrf     !  Surface area
      
C...TXHG aersosols      
      Integer :: ltracer_akn  !  Tracer aerosols (Aitken)
      Integer :: ltracer_acc  !  Tracer aerosols (Accumulation)
      Integer :: ltracer_cor  !  Tracer aerosols (Coarse)
      Integer :: lphg_akn     !  Mercuric aerosols (Aitken and Accumulation)
      Integer :: lphg_acc     !  Mercuric aerosols (Aitken and Accumulation)
      Integer :: lphg_cor     !  Mercuric aerosols (Coarse)
C...Sulfur Tracking Model      
      Integer :: lso4aqh2o2 ! Sulfate from aq H2O2 rxn
      Integer :: lso4aqo3   ! Sulfate from aq O3 rxn
      Integer :: lso4aqfemn ! Sulfate from aq rxn catalyzed by FE&MN
      Integer :: lso4aqmhp  ! Sulfate from aq MHP rxn
      Integer :: lso4aqpaa  ! Sulfate from aq PAA rxn

      Integer :: lcaacc   !  Ca in Accumulation mode (AE6) SLN 16March2011
      Integer :: lmgacc   !  Mg in Accumulation mode (AE6) SLN 16March2011
      Integer :: lkacc    !  K in Accumulation mode (AE6)  SLN 16March2011
      Integer :: lfeacc   !  Fe in Accumulation mode (AE6) SLN 22March2011
      Integer :: lmnacc   !  Mn in Accumulation mode (AE6) SLN 22March2011
      Integer :: lsoilc   !  SOIL in Coarse mode (AE6)     SLN 16March2011
      Integer :: lanthc   !  CORS in Coarse mode (AE6)     SLN 16March2011
      Integer :: lseasc   !  SEAS in Coarse mode (AE6)     SLN 16March2011

C...Pointers for the AQCHEM arrays LIQUID and WETDEP
      Integer, Parameter :: lacl        = 01  ! Hydrogen ion
      Integer, Parameter :: lnh4accl    = 02  ! Ammonium(attributed to accum. mode)
      Integer, Parameter :: lcacorl     = 03  ! coarse mode Calcium 
      Integer, Parameter :: lnaaccl     = 04  ! Sodium
      Integer, Parameter :: lohl        = 05  ! Hydroxyl radical
      Integer, Parameter :: lso4accl    = 06  ! Sulfate (attributed to accumulation mode)
      Integer, Parameter :: lhso4accl   = 07  ! bisulfate (attributed to accumulation mode)
      Integer, Parameter :: lso3l       = 08  ! sulfite
      Integer, Parameter :: lhso3l      = 09  ! bisulfite
      Integer, Parameter :: lso2l       = 10  ! sulfur dioxide
      Integer, Parameter :: lco3l       = 11  ! carbonate
      Integer, Parameter :: lhco3l      = 12  ! bicarbonate
      Integer, Parameter :: lco2l       = 13  ! carbon dioxide
      Integer, Parameter :: lno3accl    = 14  ! nitrate(attributed to accumulation mode)
      Integer, Parameter :: lnh3l       = 15  ! ammonia
      Integer, Parameter :: lclaccl     = 16  ! chloride ion (attributed to accumulation mode)
      Integer, Parameter :: lh2o2l      = 17  ! hydrogen peroxide
      Integer, Parameter :: lo3l        = 18  ! ozone
      Integer, Parameter :: lfecorl     = 19  ! iron
      Integer, Parameter :: lmncorl     = 20  ! Manganese
      Integer, Parameter :: lal         = 21  ! generalized anion associated with iron
      Integer, Parameter :: lfoal       = 22  ! Formic acid
      Integer, Parameter :: lhco2l      = 23  ! HCOO- ion
      Integer, Parameter :: lmhpl       = 24  ! Methyl hydrogen peroxide
      Integer, Parameter :: lpaal       = 25  ! Peroxyacidic acid
      Integer, Parameter :: lhcll       = 26  ! Hydrogen chloride
      Integer, Parameter :: lpriml      = 27  ! primary aerosol
      Integer, Parameter :: lmgcorl     = 28  ! coarse mode Magnesium
      Integer, Parameter :: lkcorl      = 29  ! coarse mode Potassium
      Integer, Parameter :: lbl         = 30  ! generalized anion associated with manganese
      Integer, Parameter :: lhno3l      = 31  ! nitric acid
!     Integer, Parameter :: lprimcorl   = 32  ! coarse-mode primary aerosol
      Integer, Parameter :: lnumcorl    = 32  ! coarse-mode number
      Integer, Parameter :: lts6corl    = 33  ! sulfate (attributed to coarse mode)
      Integer, Parameter :: lnacorl     = 34  ! sodium (attributed to coarse mode)
      Integer, Parameter :: lclcorl     = 35  ! chloride ion (attributed to coarse mode)
      Integer, Parameter :: lno3corl    = 36  ! nitrate (attributed to coarse mode)
      Integer, Parameter :: lnh4corl    = 37  ! ammonium (attributed to coarse mode)
      Integer, Parameter :: lsoal       = 38  ! secondary organic aerosols
      Integer, Parameter :: lorgcl      = 39  ! in-cloud soa
      integer, parameter :: lpoal       = 40  ! primary organic aerosols
      Integer, Parameter :: lecl        = 41  ! elemental carbon
      Integer, Parameter :: lglyl       = 42  ! glyoxal
      Integer, Parameter :: lmglyl      = 43  ! methylglyoxal
      Integer, Parameter :: lcaaccl     = 44  ! accumulation mode Calcium (AE6)   SLN 16March2011
      Integer, Parameter :: lmgaccl     = 45  ! accumulation mode Magnesium (AE6) SLN 16March2011
      Integer, Parameter :: lkaccl      = 46  ! accumulation mode Potassium (AE6) SLN 16March2011
      Integer, Parameter :: lsoilcl     = 47  ! coarse mode SOIL (AE6)   SLN 16March2011
      Integer, Parameter :: lanthcl     = 48  ! coarse mode CORS (lol) (AE6)   SLN 16March2011
      Integer, Parameter :: lseascl     = 49  ! coarse mode SEAS (AE6) SLN 16March2011
      Integer, Parameter :: lfeaccl     = 50  ! accumulation mode Iron (AE6)      SLN 22March2011
      Integer, Parameter :: lmnaccl     = 51  ! accumulation mode Manganese (AE6) SLN 22March2011
C for TX version
      integer, parameter :: ltracerl    = 52 ! TRACER
      integer, parameter :: ltracercorl = 53 ! Coarse-mode TRACER
      integer, parameter :: lphgfinel   = 54 ! Fine-mode Mercury Tracer
      integer, parameter :: lphgcorl    = 55 ! Coarse-mode Mercury Tracer
C for sulfur tracking model
      Integer, Parameter :: lts6aqh2o2l = 56 ! total S(VI) from aq H2O2 rxn
      Integer, Parameter :: lts6aqo3l   = 57 ! total S(VI) from aq O3 rxn
      Integer, Parameter :: lts6aqfemnl = 58 ! total S(VI) from aq FE&MN catalyzed rxn
      Integer, Parameter :: lts6aqmhpl  = 59 ! total S(VI) from aq MHP rxn
      Integer, Parameter :: lts6aqpaal  = 60 ! total S(VI) from aq PAA rxn
      
      Integer, Parameter :: ltracil = 61
      Integer, Parameter :: lpoail = 62
      Integer, Parameter :: lpriil = 63
      Integer, Parameter :: lsoail = 64

      Integer    :: nsrggas( ngas )           ! # surrogates in CGRID for each gas
      Integer    :: lsrggas( ngas,mxsrg )

      Integer, Allocatable, Save :: nsrgaer( :,: )    ! # surrogates in CGRID for each aerosol
      Integer, Allocatable, Save :: lsrgaer( :,:,: )
      Integer, Allocatable, Save :: aknsrg( :,: )

      Integer, Allocatable, Save :: cgrid2aq_map( : ) ! allocated in AQ_DATA_INIT
      Character( 16 ), Allocatable, Save :: cgrid2aq_name( : ) ! allocated in AQ_DATA_INIT

      Real( 8 )   :: jh2o2_hydrometeors    ! H2O2 photolysis rate in condensed water [sec-1]
      Real( 8 )   :: jhno3_hydrometeors    ! HNO3 photolysis rate in condensed water [sec-1]

      Logical, Private, Save :: mapped = .False.

      Contains
 
C------------------------------------------------------------------------
      Subroutine aq_data_init()

C  Defines mapping from CGRID for species concentration and moments.
 
C  Revision History:
C     First version was coded in May 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C     Jan 2017: Additional error check added to avoid array out of
C     bounds when additional aerosol species are added. HOT Pye.
C-----------------------------------------------------------------------

      Use cgrid_spcs          ! CGRID mechanism species
      Use utilio_defn
      Use aero_data 
      Use phot_mod, Only: init_phot_shared  ! set-up shared photolysis arrays
      
      Implicit None

C Functions:
      Integer, External :: indexn

C Local Variables:
      Character( 100 ) :: xmsg
      Character( 16 ), Allocatable, Save :: aename( : )
      Character( 16 ), Allocatable, Save :: cgrid2aq( : )
      Character( 10 ) :: bkground
      Integer :: igas
      Integer :: iaer
      Integer :: isrg
      Integer :: spc
      Integer :: mode
      Integer :: i
      Integer :: n
      Integer :: var
      Logical :: new, Jexist
      Integer :: lsrg( mxsrg )
      Integer :: n_cgrid2aq
      Integer :: ios
C Local Variable used to determine what optional TX to add to surrogates
      !Logical :: found_all_modes
      !Integer :: n_missing_tx
      !Integer :: n_which_txadd( nadd_tx )
      CHARACTER(16) :: NAMEI, NAMEJ
      INTEGER LS2, LS1, LS3, LS4, ISRG2, ISRGI, IAERI, SPCI      

      Logical, Save :: Firstcall = .True.
      
C-----------------------------------------------------------------------


      If ( Firstcall ) Then
         Firstcall = .False.
      Else
         Return
      End if
 
      Call init_phot_shared()
      
      Allocate ( aename( n_ae_a2aq ), stat = ios )
      If ( ios .Ne. 0 ) Then
         xmsg = '*** Error allocating aename'
         Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
      End If

C...Build array of AE names that are mapped to AE_A2AQ
      Do spc = 1, n_ae_a2aq
         aename( spc ) = ae_spc( ae_a2aq_map( spc ) )
      End Do

C...Set number of species in CGRID and used in Aqueous module
      n_cgrid2aq = n_gc_g2aq + n_ae_a2aq + n_nr_n2aq + n_tr_t2aq

!     Allocate ( cgrid2aq( n_cgrid2aq ),
!    &           cgrid2aq_map( n_cgrid2aq ), stat = ios )
!     If ( ios .Ne. 0 ) Then
!        xmsg = '*** Error allocating cgrid2aq or cgrid2aq_map'
!        Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
!     End If

      Allocate ( cgrid2aq( n_cgrid2aq ), stat = ios )
      If ( ios .Ne. 0 ) Then
         xmsg = '*** Error allocating cgrid2aq'
         Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
      End If

      Allocate ( cgrid2aq_map( n_cgrid2aq ), stat = ios )
      If ( ios .Ne. 0 ) Then
         xmsg = '*** Error allocating cgrid2aq_map'
         Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
      End If

      Allocate ( cgrid2aq_name( n_cgrid2aq ), stat = ios )
      If ( ios .Ne. 0 ) Then
         xmsg = '*** Error allocating cgrid2aq_name'
         Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
      End If

C...Map aero species
      Call map_aero()

      naer = n_aerospc + naddaer + nadd_tx

      Allocate ( srgaer( naer ), stat = ios )
      If ( ios .Ne. 0 ) Then
         xmsg = '*** Error allocating srgaer'
         Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
      End If

      Allocate ( nsrgaer( naer,nmodes ), stat = ios )
      If ( ios .Ne. 0 ) Then
         xmsg = '*** Error allocating nsrgaer'
         Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
      End If

      Allocate ( lsrgaer( naer,nmodes,mxsrg ), stat = ios )
      If ( ios .Ne. 0 ) Then
         xmsg = '*** Error allocating lsrgaer'
         Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
      End If
      
      Allocate ( aknsrg( naer,mxsrg ), stat = ios )
      If ( ios .Ne. 0 ) Then
         xmsg = '*** Error allocating aknsrg'
         Call m3exit( 'aq_data_init', 0, 0, xmsg, xstat3 )
      End If

C...Populate optional aerosol surrogate species (e.g.
C   TRACER_AITKEN, TRACER_ACCUM, TRACER_COARSE, 
C   PHG_AITKEN,    PHG_ACCUM,    PHG_COARSE     )
      Do spc = 1, nadd_tx
         srgaer( spc ) = srgaer_addtx( spc )
      End Do

C...Initialize aerosol surrogate species from AERO_DATA table
      Do var = 1, n_aerospc
         spc = nadd_tx + var
         srgaer( spc ) = sur_aer_type( (/' ',' ',' '/), 0.0, 0.0, 0.0 )

         ! Skip H2O, H2OP, and SOIL species
         If ( var .Eq. ah2o_idx .Or. var .Eq. ah3op_idx ) Then
              write(xmsg,'(4(A,1x))')'Skipping AQ surrogate for AEROSPC ',
     &                               aerospc( spc-nadd_tx )%name( 1:n_mode )
              Call m3warn( 'aq_data_init', 0, 0, xmsg )
              Cycle
         End if

         Do mode = 1, n_mode
            n = index1( aerospc( var )%name( mode ), n_ae_a2aq, aename )
    
            If ( n .Gt. 0 ) Then
               ! Verify that surrogate is new
               new = .True. 
               Do i = 1, spc
                  If ( ae_a2aq( n ) .Eq. srgaer( i )%name( mode ) ) Then
                     new = .False.; Exit
                  End If
                 
		  Jexist = .false.            ! Does J mode exist for a new I mode species?	  
		  If ( mode .eq. 1) then
		     spci = spc
		     ls1 = len_trim(ae_a2aq(n))
		     ls2 = len_trim(srgaer(i)%name(2))
		     namei = ae_a2aq(n)
		     namei = namei(1:ls1-7)   ! Remove "_AITKEN" to get base name for I mode
		     namej = srgaer(i)%name(2)
		     namej = namej(1:ls2-6)   ! Remove "_ACCUM" to get base name for J mode
		     If (trim(namei) .eq. trim(namej)) then
		        spci = i
		        Jexist = .true.; Exit
		     End If
		  End If	     
		  
               End Do	  
	       
	       If ( new ) Then
	          
		  If (Jexist) then 
	             srgaer( spci )%name( mode ) = ae_a2aq( n )
                     srgaer( spci )%density = aerospc( var )%density
                     srgaer( spci )%molwt = aerospc_mw( var )
		  Else
                     srgaer( spc )%name( mode ) = ae_a2aq( n )
                     srgaer( spc )%density = aerospc( var )%density
                     srgaer( spc )%molwt = aerospc_mw( var ) 
		  End If

               End If
           End If
         End Do   ! mode
      End Do   ! spc
           

C...Initialize aerosol moment surrogates (i.e.
C   NUM_AITKEN, NUM_ACCUM, NUM_COARSE,
C   SRF_AITKEB, SRF_ACCUM, SRF_COARSE )
      Do n = 1, naddaer
         spc = n_aerospc + nadd_tx + n
         srgaer( spc ) = addsrgaer( n )
      End Do
   

#ifdef verbose_cloud
C...write out SRGGAS and SRGAER to verify
      Write( logdev, 1005 )ngas
      Do spc = 1, ngas
         Write( logdev,1007 ) srggas( spc )%name, srggas( spc )%backgnd
      End Do
      Write( logdev,1009 ) naer
      Do spc = 1, naer
         Write( logdev,1111 ) srgaer( spc )%name, srgaer( spc )%density,
     &                        srgaer( spc )%molwt, srgaer( spc )%backgnd
      End Do
#endif      

C...Prepare indices for aqueous species

C...CGRID map to aqueous species pointers for the gases
      spc = 0
      Do var = 1, n_gc_g2aq
         spc = spc + 1
         cgrid2aq( spc ) = gc_g2aq( var )
         cgrid2aq_map( spc ) = gc_g2aq_map( var ) + gc_strt - 1
         cgrid2aq_name( spc ) = gc_spc( gc_g2aq_map( var ) )
      End Do

C...CGRID map to aqueous species pointers for the aerosols
      Do var = 1, n_ae_a2aq
         spc = spc + 1
         cgrid2aq( spc ) = ae_a2aq( var )
         cgrid2aq_map( spc ) = ae_a2aq_map( var ) + ae_strt - 1
         cgrid2aq_name( spc ) = ae_spc( ae_a2aq_map( var ) )
      End Do

C...CGRID map to aqueous species pointers for the non-reactives
      Do var = 1, n_nr_n2aq
         spc = spc + 1
         cgrid2aq( spc ) = nr_n2aq( var )
         cgrid2aq_map( spc ) = nr_n2aq_map( var ) + nr_strt - 1
         cgrid2aq_name( spc ) = nr_spc( nr_n2aq_map( var ) )
      End Do

C...CGRID map to aqueous species pointers for the tracers
      Do var = 1, n_tr_t2aq
         spc = spc + 1
         cgrid2aq( spc ) = tr_t2aq( var )
         cgrid2aq_map( spc ) = tr_t2aq_map( var ) + tr_strt - 1
         cgrid2aq_name( spc ) = tr_spc( tr_t2aq_map( var ) )
      End Do

      write(logdev, 1114)
C...Create the pointers from CGRID to the gaseous species needed by AQCHEM
      nsrggas = 0
      Do igas = 1, ngas
         nsrggas( igas ) = indexn( srggas( igas )%name, n_cgrid2aq, cgrid2aq, lsrg )
         If ( nsrggas( igas ) .Eq. 0 ) Then
            Write ( bkground, '(F8.4)' ) srggas( igas )%backgnd
            xmsg = trim( srggas( igas )%name )
     &           // ' is not in GC or NR namelist so using background value of '
     &           // trim( bkground )
            Call m3mesg( xmsg )
         Else
            Do isrg = 1, nsrggas( igas )
               lsrggas( igas,isrg ) = lsrg( isrg )
#ifdef verbose_cloud
               write( logdev,1113 ) cgrid2aq_name( lsrggas( igas,isrg ) ),
     &         cgrid2aq_map( lsrggas( igas,isrg ) ), srggas( igas )%name,
     &         igas,lsrggas( igas,isrg )
#endif                  
            End Do
         End If
      End Do

C...Create the pointers from CGRID to the aerosol species needed by AQCHEM
      nsrgaer = 0
      aknsrg = 0
      write(logdev, 1114)
      Do iaer = 1, naer
         Do n = 1, nmodes
            If ( srgaer( iaer )%name( n ) .Ne. ' ' ) Then
               nsrgaer( iaer,n ) = indexn( srgaer( iaer )%name( n ), n_cgrid2aq,
     &                                     cgrid2aq, lsrg )
               If ( nsrgaer( iaer,n ) .Eq. 0 ) Then
                  Write ( bkground, '(f8.4)' ) srgaer( iaer )%backgnd
                  xmsg = trim( srgaer( iaer )%name( n ) )
     &           // ' is not in AE namelist so using background value of '
     &           // trim( bkground )
                  Call m3mesg( xmsg )
               Else
                  Do isrg = 1, nsrgaer( iaer,n )
                     If ( isrg .Gt. mxsrg ) Then
                        xmsg = 'lsrg will index out of bounds. Increase mxsrg in AQ_DATA.F'
                        Call m3exit( 'AQ_DATA', 0, 0, xmsg, xstat3 )
                     End If
                     lsrgaer( iaer,n,isrg ) = lsrg( isrg )
			
#ifdef verbose_cloud
                     write( logdev,1113 ) cgrid2aq_name( lsrgaer( iaer,n,isrg ) ),
     &               cgrid2aq_map( lsrgaer( iaer,n,isrg ) ), srgaer( iaer )%name( n ),
     &               iaer, lsrgaer( iaer,n,isrg )
#endif          		     
                  
                  End Do
               End If
            End If
         End Do
      End Do
            
C...Create the pointers for required gas surrogate species needed by AQCHEM
      lso2   = findSgrGas( req_so2 )    ! Sulfur Dioxide
      lhno3  = findSgrGas( req_hno3 )   ! Nitric Acid
      ln2o5  = findSgrGas( req_n2o5 )   ! Dinitrogen Pentoxide
      lco2   = findSgrGas( req_co2 )    ! Carbon Dioxide
      lnh3   = findSgrGas( req_nh3 )    ! Ammonia
      lh2o2  = findSgrGas( req_h2o2 )   ! Hydrogen Perioxide
      lo3    = findSgrGas( req_o3 )     ! Ozone
      lfoa   = findSgrGas( req_foa )    ! Formic Acid
      lmhp   = findSgrGas( req_mhp )    ! Methyl Hydrogen Peroxide
      lpaa   = findSgrGas( req_paa )    ! Peroxyacidic Acid
      lh2so4 = findSgrGas( req_h2so4 )  ! Sulfuric Acid
      lhcl   = findSgrGas( req_hcl )    ! Hydrogen Chloride
      lgly   = findSgrGas( req_gly )    ! Glyoxal
      lmgly  = findSgrGas( req_mgly )   ! Methylglyoxal
      lho    = findSgrGas( req_ho )     ! OH, hydroxyl radical

C...Create the pointers for required aerosol surrogate species needed by AQCHEM
      lso4   = findSgrAer( req_aso4 )    !  Sulfate
      lnh4   = findSgrAer( req_anh4 )    !  Ammonium
      lno3   = findSgrAer( req_ano3 )    !  Nitrate
      lsoa   = findSgrAer( req_asoa )    !  SOA
      lorgc  = findSgrAer( req_aorgc )   !  SOA (anthropogenic organic oligomers)
      lpoa   = findSgrAer( req_apoa )    !  Primary organic aerosol
      lec    = findSgrAer( req_aec )     !  Elemental carbon
      lpri   = findSgrAer( req_apri )    !  Primary aerosol (Aitken and Accumulation)
!     lpricor= findSgrAer( req_apric )   !  Primary aerosol (coarse)
      lna    = findSgrAer( req_ana )     !  Sodium
      lcl    = findSgrAer( req_acl )     !  Chloride ion
!     lcaco3 = findSgrAer( req_acaco3 )  !  Calcium Carbonate aerosol (place holder)
!     lmgco3 = findSgrAer( req_amgco3 )  !  Magnesium Carbonate aerosol (place holder)
!     la3fe  = findSgrAer( req_aa3fe )   !  Iron aerosol (place holder)
!     lb2mn  = findSgrAer( req_ab2mn )   !  Manganese aerosol (place holder)
!     lk     = findSgrAer( req_ak )      !  Potassium aerosol (Cl- tracked separately)
      lnum   = findSgrAer( req_num )     !  Number
      lsrf   = findSgrAer( req_srf )     !  Surface area
      lcaacc = findSgrAer( req_caacc )   !  Ca in Accumulation mode (AE6) SLN 16March2011
      lmgacc = findSgrAer( req_mgacc )   !  Mg in Accumulation mode (AE6) SLN 16March2011
      lkacc  = findSgrAer( req_kacc )    !  K in Accumulation mode (AE6)  SLN 16March2011
      lfeacc = findSgrAer( req_feacc )   !  Fe in Accumulation mode (AE6) SLN 22March2011
      lmnacc = findSgrAer( req_mnacc )   !  Mn in Accumulation mode (AE6) SLN 22March2011
      lsoilc = findSgrAer( req_soilc )   !  SOIL in Coarse mode  (AE6)    SLN 16March2011
      lanthc = findSgrAer( req_anthc )   !  CORS in Coarse mode  (AE6)    SLN 16March2011
      lseasc = findSgrAer( req_seasc )   !  SEAS in Coarse mode  (AE6)    SLN 16March2011
      
      
C...For TXHG version
      ltracer_akn    = findSgrAer( req_traceri  )   !  Tracer aitken mode 
      ltracer_acc    = findSgrAer( req_tracerj  )   !  Tracer accumation mode 
      ltracer_cor    = findSgrAer( req_tracerk  )   !  Tracer coarse mode 
      lphg_akn       = findSgrAer( req_phgi )       !  Mercuric aitken and accumulation modes
      lphg_acc       = findSgrAer( req_phgj )       !  Mercuric aitken and accumulation modes
      lphg_cor       = findSgrAer( req_phgk )       !  Mercuric coarse mode
C...For sulfur tracking model
      If ( stm ) Then
         lso4aqh2o2 = findSgrAer( req_aso4aqh2o2 ) ! Sulfate from aq H2O2 rxn
         lso4aqo3   = findSgrAer( req_aso4aqo3 )   ! Sulfate from aq O3 rxn
         lso4aqfemn = findSgrAer( req_aso4aqfemn ) ! Sulfate from aq rxn catalyzed by FE&MN
         lso4aqmhp  = findSgrAer( req_aso4aqmhp )  ! Sulfate from aq MHP rxn
         lso4aqpaa  = findSgrAer( req_aso4aqpaa )  ! Sulfate from aq PAA rxn
      End If

      mapped = .True.
      
C...Match ACC mode surrogate indices to their AKN mode counterparts

     
      DO IAER = 1, NAER
	 DO ISRG = 1, NSRGAER( IAER,2 )		
		        NAMEJ = CGRID2AQ_NAME( LSRGAER( IAER,2,ISRG ) )
		        LS1 = LEN_TRIM(NAMEJ)
		        LS2 = 0
		        DO I = 1, LS1
		           IF(NAMEJ(I:I) .NE. ' ') THEN
			      LS2 = LS2+1
		           END IF
		        END DO
		        ISRGI = 0
			
                        IF(IAER .EQ. LTRACER_ACC) THEN
		          IAERI = LTRACER_AKN 
		        ELSE
		          IAERI = IAER
		        END IF
			
		    LOOP_SRGI: DO ISRG2 = 1, NSRGAER( IAERI, 1 )
			          NAMEI = CGRID2AQ_NAME( LSRGAER( IAERI,1,ISRG2 ) )
			          LS3 = LEN_TRIM(NAMEI)
		                  LS4 = 0
		                  DO I = 1, LS3
		                     IF(NAMEI(I:I) .NE. ' ') THEN
			                LS4 = LS4+1
			             END IF
			          END DO
			
			          IF( TRIM(NAMEJ(1:LS2-1)) .EQ. TRIM(NAMEI(1:LS4-1)) ) THEN
			             ISRGI = ISRG2
			             EXIT LOOP_SRGI
			          END IF
			       END DO LOOP_SRGI	
			       
			
			AKNSRG(IAER, ISRG) = ISRGI		

	 END DO
      END DO


#ifdef verbose_cloud
      Write( logdev,'( /5x, a )' ) 'AQCHEM gas surrogate species'
      Write( logdev,'( 5x, a, i4 )' ) 'lso2:   ', lso2
      Write( logdev,'( 5x, a, i4 )' ) 'lhno3:  ', lhno3
      Write( logdev,'( 5x, a, i4 )' ) 'ln2o5:  ', ln2o5
      Write( logdev,'( 5x, a, i4 )' ) 'lco2:   ', lco2
      Write( logdev,'( 5x, a, i4 )' ) 'lnh3:   ', lnh3
      Write( logdev,'( 5x, a, i4 )' ) 'lh2o2:  ', lh2o2
      Write( logdev,'( 5x, a, i4 )' ) 'lo3:    ', lo3
      Write( logdev,'( 5x, a, i4 )' ) 'lfoa:   ', lfoa
      Write( logdev,'( 5x, a, i4 )' ) 'lmhp:   ', lmhp
      Write( logdev,'( 5x, a, i4 )' ) 'lpaa:   ', lpaa
      Write( logdev,'( 5x, a, i4 )' ) 'lh2so4: ', lh2so4
      Write( logdev,'( 5x, a, i4 )' ) 'lhcl:   ', lhcl
      Write( logdev,'( 5x, a, i4 )' ) 'lgly:   ', lgly
      Write( logdev,'( 5x, a, i4 )' ) 'lmgly:  ', lmgly
      Write( logdev,'( 5x, a, i4 )' ) 'lho:    ', lho

      Write( logdev,'( /5x, a )' ) 'AQCHEM aer  osol surrogate species'
      Write( logdev,'( 5x, a, i4 )' ) 'lso4:     ', lso4
      Write( logdev,'( 5x, a, i4 )' ) 'lnh4:     ', lnh4
      Write( logdev,'( 5x, a, i4 )' ) 'lno3:     ', lno3
      Write( logdev,'( 5x, a, i4 )' ) 'lsoa:     ', lsoa
      Write( logdev,'( 5x, a, i4 )' ) 'lorgc:    ', lorgc
      Write( logdev,'( 5x, a, i4 )' ) 'lpoa:     ', lpoa
      Write( logdev,'( 5x, a, i4 )' ) 'lec:      ', lec
      Write( logdev,'( 5x, a, i4 )' ) 'lpri:     ', lpri
      Write( logdev,'( 5x, a, i4 )' ) 'lna:      ', lna
      Write( logdev,'( 5x, a, i4 )' ) 'lcl:      ', lcl
      Write( logdev,'( 5x, a, i4 )' ) 'lnum:     ', lnum
      Write( logdev,'( 5x, a, i4 )' ) 'lsrf:     ', lsrf
      Write( logdev,'( 5x, a, i4 )' ) 'lcaacc:   ', lcaacc
      Write( logdev,'( 5x, a, i4 )' ) 'lmgacc:   ', lmgacc
      Write( logdev,'( 5x, a, i4 )' ) 'lkacc:    ', lkacc
      Write( logdev,'( 5x, a, i4 )' ) 'lfeacc:   ', lfeacc
      Write( logdev,'( 5x, a, i4 )' ) 'lmnacc:   ', lmnacc
      Write( logdev,'( 5x, a, i4 )' ) 'lsoilc:   ', lsoilc
      Write( logdev,'( 5x, a, i4 )' ) 'lanthc:   ', lanthc
      Write( logdev,'( 5x, a, i4 )' ) 'lseasc:   ', lseasc
      Write( logdev,'( 5x, a, i4 )' ) 'ltracer_akn:  ', ltracer_akn
      Write( logdev,'( 5x, a, i4 )' ) 'ltracer_acc:  ', ltracer_acc
      Write( logdev,'( 5x, a, i4 )' ) 'ltracer_cor:  ', ltracer_cor
      Write( logdev,'( 5x, a, i4 )' ) 'lphg_akn:     ', lphg_akn
      Write( logdev,'( 5x, a, i4 )' ) 'lphg_acc:     ', lphg_acc
      Write( logdev,'( 5x, a, i4 )' ) 'lphg_cor:     ', lphg_cor
      
      If ( stm ) Then
         Write( logdev,'( 5x, a, i4 )' ) 'lso4aqh2o2:', lso4aqh2o2
         Write( logdev,'( 5x, a, i4 )' ) 'lso4aqo3:  ', lso4aqo3
         Write( logdev,'( 5x, a, i4 )' ) 'lso4aqfemn:', lso4aqfemn
         Write( logdev,'( 5x, a, i4 )' ) 'lso4aqmhp: ', lso4aqmhp
         Write( logdev,'( 5x, a, i4 )' ) 'lso4aqpaa: ', lso4aqpaa
      End If
#endif

      Write( logdev,'( / 5x, a / )' ) ' --- AQCHEM Gas and Aerosol Species Mapped ---'

C1005  Format( / "GASES" / "NGAS = ", i3 / "    name   "," backgrd" // )
C1007  Format( a16, 1x, f8.2 )
C1009  Format( / "AEROSOLS" / "NAER = ", i3 / "    name(1)   ", "    name(2)   ",
C     &        "    name(3)   ", " dens ", " backgrd" // )
C1111  Format( 3(a16,1x), f6.0, 2(1x, f8.2) )
C1113  Format( a16, 1x, i4, 1x, a16, 1x, i4, 1x, i4)     
1114  Format( / "   CGRID_SPC    ", 1x, "SPC ", 1x, "   SURROGATE   ", 1x, "ISRG", 1x, "IDX " )

      Return
      End Subroutine aq_data_init

C-----------------------------------------------------------------------
      Function findSgrGas( vname ) Result ( idx )

C  Finds the index of 'required' gas surrogate species in the srggas list

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Use utilio_defn

      Implicit None

C Arguments:
      Character( 16 ) :: vname
      Integer idx

C Local Variables:
      Character( 80 ) :: xmsg
      Integer spc

      idx = 0
      Do spc = 1, ngas      
         If ( srggas( spc )%name .Eq. vname ) Then
            idx = spc
            ! Write( *,'(a,i5)' ) vname, idx
            Return
         End If
      End Do

      xmsg = 'Required Species ' // Trim( vname ) // ' Not found in SgrGas names array'
      Call m3exit( 'AQ_DATA', 0, 0, xmsg, xstat3 )

      Return
      End Function findSgrGas

C-----------------------------------------------------------------------
      Function findSgrAer( vname ) Result ( idx )

C  Finds the index of 'required' Aerosol surrogate species in the srgaer list

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Use utilio_defn

      Implicit None

C Arguments:
      Character( 16 ) :: vname
      Integer idx

C Local Variables:
      Character( 80 ) :: xmsg
      Integer spc, n

      idx = 0
      Do spc = 1, naer
         Do n = 1, nmodes
            ! Write( *,'(a,i5)' ) vname, spc
            If ( srgaer( spc )%name( n ) .Eq. vname ) Then
               idx = spc
               ! Write( *,'(a,i5)' ) vname, idx
               Return
            End If
         End Do
      End Do

      xmsg = 'Required Species ' // Trim( vname ) // ' Not found in SgrAer names array'
      Call m3exit( 'AQ_DATA', 0, 0, xmsg, xstat3 )

      Return
      End Function findSgrAer

C-----------------------------------------------------------------------
      Subroutine set_aeconcmin( aeconcmin )
C-----------------------------------------------------------------------
C
C  Function:
C       set minimum concentrations for aerosol species
C       routine called from convcld_acm
C
C  Preconditions:
C       Dates and times represented YYYYDDD:HHMMSS.
C
C  Revision History:
C       5/05 copied code from aero_depv to begin subroutine
C       4/08 J.Kelly: added code to set min for coarse surface area
C       21 Jun 10 J.Young: convert for Namelist redesign
C-----------------------------------------------------------------------

      Use cgrid_spcs            ! CGRID mechanism species
      Use aero_data
      Use utilio_defn

      Implicit None

C..Includes:
      Include SUBST_CONST               ! constants

C..Functions:

C..Arguments:
      Real, Intent( Out ) :: aeconcmin( : ) ! array of minimum concentrations

C..Local Variables:
      !Character( 16 ) :: pname = 'SET_AECONCMIN'   ! program name

      Integer m        ! mode loop counter
      Integer n        ! aerospc loop counter
      Integer spc      ! species loop counter


      ! compute minimums for numbers and surfaces

      Do m = 1, n_mode
         spc = index1( aeromode( m )%num_name, n_ae_spc, ae_spc )
         aeconcmin( spc ) = aeromode_minNum( m ) * mwair * 1.0E-3

         spc = index1( aeromode( m )%srf_name, n_ae_spc, ae_spc )
         aeconcmin( spc ) = aeromode_minM2( m ) * mwair * 1.0E-3
      End Do

      ! compute minimums for aerosol species
      Do m = 1, n_mode
         Do n = 1, n_aerospc
            If ( .NOT. aero_missing(n,m) ) Then
               spc = index1( aerospc( n )%name( m ), n_ae_spc, ae_spc )
               If ( spc .Ne. 0 ) Then
                  aeconcmin( spc ) = aerospc( n )%min_conc( m ) * mwair * 1.0E-9 / ae_molwt( spc )
               End If               
            End If
         End Do
      End Do 

      Return
      End Subroutine set_aeconcmin

      End Module aq_data
