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
!  but usersrel_rxe requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module STAGE_DATA
C-----------------------------------------------------------------------
C Function: User-defined types

C Revision History:
C     26 June 2014 J.Bash: initial implementation

      Implicit None
      
      Type :: Map_Type
         Integer                    :: n_gas_dep         ! number of gas deposited species
         Integer                    :: n_aero_dep        ! number of aerosol deposited species
         Integer,       Allocatable :: gas_index  ( : )  ! Index of transported gaseous species
         Integer,       Allocatable :: dep_in_gas ( : )  ! Location of the deposition species in gas vector 
         Real,          Allocatable :: MolWt      ( : )  ! Molecular Weight of deposited species
         Real,          Allocatable :: LeBasM     ( : )  ! Molar volume of deposited species
         Real,          Allocatable :: f0         ( : )  ! mesophyll resistance overide flag 
         Real,          Allocatable :: ApRx       ( : )  ! Apparent reactivity
         Real,          Allocatable :: MassAC     ( : )  ! Mass accommodation coefficient 
         Character(16), Allocatable :: dep_name   ( : )  ! Dep Species Name
         Character(16), Allocatable :: gas_name   ( : )  ! Gas Species Name
         Character(16), Allocatable :: H_name     ( : )  ! Henry's surrogate name from the name
         Real,          Allocatable :: dep_fac    ( : )  ! deposition factor from the namelist
      End Type Map_Type      

      Type :: STAGE_Diag
         Integer                    :: n_spc                    ! number of diagnostic species
         Real,          Allocatable :: NH3_Diag_Temp ( :,:,: )  ! Instentanious NH3 Diagnostic output
         Real,          Allocatable :: NH3_Diag_Out  ( :,:,: )  ! Accumulated NH3 Diagnostic output
         Real                       :: dep_fac                  ! deposition factor from the namelist
         Character(16), Allocatable :: dep_name  ( : )          ! Dep Species name
      End Type STAGE_Diag

      Type( Map_Type ), Save :: Map_Data 

      Type( STAGE_Diag ),    Save :: STAGE_Diag_Data 

      INTEGER, PARAMETER :: dep_gas_all = 177
      Character( 16 ), Save :: vd_name     ( dep_gas_all )       ! Deposition species name
      Real,            Save :: rel_rx      ( dep_gas_all )       ! reactivity relative to HNO3
      Real,            Save :: LeBasM      ( dep_gas_all )       ! Le Bas molar volume [cm3/mol ]
      Real,            Save :: f0          ( dep_gas_all )       ! Exception for species that
      Real,            Save :: M_ac        ( dep_gas_all )       ! Mass accommodation Coeff
      Real,            Save :: molwt_all   ( dep_gas_all )       ! Molar Weight from the namelist
      CHARACTER( 16 ), SAVE :: H_name_all  ( dep_gas_all )       ! Henry's constant name for all species
      INTEGER, SAVE         :: N_USE_DEPSPC        
      LOGICAL, SAVE         :: USE_DEPSPC  ( dep_gas_all )
      Integer, Save         :: n_gas_asx                         ! number of gas species used in this simulation 
      Logical, Save         :: asx_run_map( dep_gas_all )

      CHARACTER( 16 ), Parameter    :: DEPV_METHOD = 'STAGE'

      DATA vd_name(  1), rel_rx(  1), f0(  1), LeBasM(  1), M_ac(  1) / 'SO2          ',   32.8, 0.0,  35.0, 0.11 / ! apperent reactivity derrived from Duke Forest fluxes measurments (Bash et al in prep)
      DATA vd_name(  2), rel_rx(  2), f0(  2), LeBasM(  2), M_ac(  2) / 'SULF         ', 8000.0, 0.0,  49.0, 0.12 /
      DATA vd_name(  3), rel_rx(  3), f0(  3), LeBasM(  3), M_ac(  3) / 'NO2          ',    2.0, 0.1,  21.0,1.5e-3/
      DATA vd_name(  4), rel_rx(  4), f0(  4), LeBasM(  4), M_ac(  4) / 'NO           ',    2.0, 0.0,  14.0,1.0e-3/
      DATA vd_name(  5), rel_rx(  5), f0(  5), LeBasM(  5), M_ac(  5) / 'O3           ',   12.0, 1.0,  21.0, 0.10 /
      DATA vd_name(  6), rel_rx(  6), f0(  6), LeBasM(  6), M_ac(  6) / 'HNO3         ', 8000.0, 0.0,  35.0, 0.087/ 
      DATA vd_name(  7), rel_rx(  7), f0(  7), LeBasM(  7), M_ac(  7) / 'H2O2         ',34000.0, 1.0,  28.0, 0.10 /   !ar=34,000 such that r_cut=0.7 s/m as in Nguyen et al. 2015
      DATA vd_name(  8), rel_rx(  8), f0(  8), LeBasM(  8), M_ac(  8) / 'ALD          ',   10.0, 0.0,  56.0, 0.03 /
      DATA vd_name(  9), rel_rx(  9), f0(  9), LeBasM(  9), M_ac(  9) / 'HCHO         ',   10.0, 0.0,  35.0, 0.02 /
      DATA vd_name( 10), rel_rx( 10), f0( 10), LeBasM( 10), M_ac( 10) / 'OP           ',   10.0, 0.3,  49.0,6.8e-3/   !meso change from 0.1 to 0.3, Wolfe and Thornton 2011 M_acP per J. Bash
      DATA vd_name( 11), rel_rx( 11), f0( 11), LeBasM( 11), M_ac( 11) / 'PAA          ',   20.0, 0.1,  70.0, 0.019/
      DATA vd_name( 12), rel_rx( 12), f0( 12), LeBasM( 12), M_ac( 12) / 'ORA          ',   20.0, 0.0,  63.0, 0.032/
      DATA vd_name( 13), rel_rx( 13), f0( 13), LeBasM( 13), M_ac( 13) / 'NH3          ',   20.0, 0.0,  28.0, 0.091/
      DATA vd_name( 14), rel_rx( 14), f0( 14), LeBasM( 14), M_ac( 14) / 'PAN          ',   16.0, 0.1,  91.0, 0.019/
      DATA vd_name( 15), rel_rx( 15), f0( 15), LeBasM( 15), M_ac( 15) / 'HONO         ',   20.0, 0.1,  28.0, 0.500/
      DATA vd_name( 16), rel_rx( 16), f0( 16), LeBasM( 16), M_ac( 16) / 'CO           ',    5.0, 0.0,  14.0,1.5e-4/
      DATA vd_name( 17), rel_rx( 17), f0( 17), LeBasM( 17), M_ac( 17) / 'METHANOL     ',    2.0, 0.0,  42.0, 0.027/
      DATA vd_name( 18), rel_rx( 18), f0( 18), LeBasM( 18), M_ac( 18) / 'N2O5         ', 5000.0, 0.0,  49.0, 0.018/
      DATA vd_name( 19), rel_rx( 19), f0( 19), LeBasM( 19), M_ac( 19) / 'NO3          ', 5000.0, 0.0,  28.0, 4.0e-3/
      DATA vd_name( 20), rel_rx( 20), f0( 20), LeBasM( 20), M_ac( 20) / 'GEN_ALD      ',   10.0, 0.0,  56.0, 0.012/
      DATA vd_name( 21), rel_rx( 21), f0( 21), LeBasM( 21), M_ac( 21) / 'CL2          ',   10.0, 0.0,  49.0, 0.030/
      DATA vd_name( 22), rel_rx( 22), f0( 22), LeBasM( 22), M_ac( 22) / 'HOCL         ',   10.0, 0.0,  38.5, 0.116/ ! used value of HCL
      DATA vd_name( 23), rel_rx( 23), f0( 23), LeBasM( 23), M_ac( 23) / 'HCL          ', 8000.0, 0.0,  31.5, 0.116/
      DATA vd_name( 24), rel_rx( 24), f0( 24), LeBasM( 24), M_ac( 24) / 'FMCL         ',   10.0, 0.0,  45.5, 0.420/ ! default value
      DATA vd_name( 25), rel_rx( 25), f0( 25), LeBasM( 25), M_ac( 25) / 'HGIIGAS      ', 8000.0, 0.0,  95.0, 0.420/ ! estimation from back calculating to get dw25 = 1.04e-5 (Garland et al, 1965)
      DATA vd_name( 26), rel_rx( 26), f0( 26), LeBasM( 26), M_ac( 26) / 'HG           ',    0.1, 0.0,  14.8, 0.420/ ! LeBasM not used
      DATA vd_name( 27), rel_rx( 27), f0( 27), LeBasM( 27), M_ac( 27) / 'TECDD_2378   ',    2.0, 0.0, 217.0, 0.420/
      DATA vd_name( 28), rel_rx( 28), f0( 28), LeBasM( 28), M_ac( 28) / 'PECDD_12378  ',    2.0, 0.0, 234.5, 0.420/
      DATA vd_name( 29), rel_rx( 29), f0( 29), LeBasM( 29), M_ac( 29) / 'HXCDD_123478 ',    2.0, 0.0, 252.0, 0.420/
      DATA vd_name( 30), rel_rx( 30), f0( 30), LeBasM( 30), M_ac( 30) / 'HXCDD_123678 ',    2.0, 0.0, 252.0, 0.420/
      DATA vd_name( 31), rel_rx( 31), f0( 31), LeBasM( 31), M_ac( 31) / 'HXCDD_123478 ',    2.0, 0.0, 252.0, 0.420/
      DATA vd_name( 32), rel_rx( 32), f0( 32), LeBasM( 32), M_ac( 32) / 'HPCDD_1234678',    2.0, 0.0, 269.5, 0.420/
      DATA vd_name( 33), rel_rx( 33), f0( 33), LeBasM( 33), M_ac( 33) / 'OTCDD        ',    2.0, 0.0, 287.0, 0.420/
      DATA vd_name( 34), rel_rx( 34), f0( 34), LeBasM( 34), M_ac( 34) / 'TECDF_2378   ',    2.0, 0.0, 210.0, 0.420/
      DATA vd_name( 35), rel_rx( 35), f0( 35), LeBasM( 35), M_ac( 35) / 'PECDF_12378  ',    2.0, 0.0, 227.5, 0.420/
      DATA vd_name( 36), rel_rx( 36), f0( 36), LeBasM( 36), M_ac( 36) / 'PECDF_23478  ',    2.0, 0.0, 227.5, 0.420/
      DATA vd_name( 37), rel_rx( 37), f0( 37), LeBasM( 37), M_ac( 37) / 'HXCDF_123478 ',    2.0, 0.0, 245.0, 0.420/
      DATA vd_name( 38), rel_rx( 38), f0( 38), LeBasM( 38), M_ac( 38) / 'HXCDF_123678 ',    2.0, 0.0, 245.0, 0.420/
      DATA vd_name( 39), rel_rx( 39), f0( 39), LeBasM( 39), M_ac( 39) / 'HXCDF_234678 ',    2.0, 0.0, 245.0, 0.420/
      DATA vd_name( 40), rel_rx( 40), f0( 40), LeBasM( 40), M_ac( 40) / 'HXCDF_123789 ',    2.0, 0.0, 245.0, 0.420/
      DATA vd_name( 41), rel_rx( 41), f0( 41), LeBasM( 41), M_ac( 41) / 'HPCDF_1234678',    2.0, 0.0, 262.5, 0.420/
      DATA vd_name( 42), rel_rx( 42), f0( 42), LeBasM( 42), M_ac( 42) / 'HPCDF_1234789',    2.0, 0.0, 262.5, 0.420/
      DATA vd_name( 43), rel_rx( 43), f0( 43), LeBasM( 43), M_ac( 43) / 'OTCDF        ',    2.0, 0.0, 280.0, 0.420/
      DATA vd_name( 44), rel_rx( 44), f0( 44), LeBasM( 44), M_ac( 44) / 'NAPHTHALENE  ',    4.0, 0.0, 119.0, 0.960/ !Julin et al. 2014 doi:10.1021/es501816h 
      DATA vd_name( 45), rel_rx( 45), f0( 45), LeBasM( 45), M_ac( 45) / '1NITRONAPHTHA',    4.0, 0.0, 133.0, 0.960/
      DATA vd_name( 46), rel_rx( 46), f0( 46), LeBasM( 46), M_ac( 46) / '2NITRONAPHTHA',    4.0, 0.0, 133.0, 0.960/
      DATA vd_name( 47), rel_rx( 47), f0( 47), LeBasM( 47), M_ac( 47) / '14NAPHTHOQUIN',    4.0, 0.0, 119.0, 0.960/
      DATA vd_name( 48), rel_rx( 48), f0( 48), LeBasM( 48), M_ac( 48) / 'HEXMETH_DIIS ',    10.0, 0.0, 196.0, 0.960/
      DATA vd_name( 49), rel_rx( 49), f0( 49), LeBasM( 49), M_ac( 49) / 'HYDRAZINE    ',    20.0, 0.0,  42.0, 0.420/
      DATA vd_name( 50), rel_rx( 50), f0( 50), LeBasM( 50), M_ac( 50) / 'MAL_ANHYDRIDE',    10.0, 0.0,  70.0, 0.420/
      DATA vd_name( 51), rel_rx( 51), f0( 51), LeBasM( 51), M_ac( 51) / 'TOLUENE_DIIS ',    10.0, 0.0, 154.0, 0.420/
      DATA vd_name( 52), rel_rx( 52), f0( 52), LeBasM( 52), M_ac( 52) / 'TRIETHYLAMINE',    20.0, 0.0, 154.0, 0.420/
      DATA vd_name( 53), rel_rx( 53), f0( 53), LeBasM( 53), M_ac( 53) / 'NTR          ',    16.0, 0.0, 160.0, 0.420/  ! assumes 58.2% C5H11O4N and 41.8% C5H11O3N
      DATA vd_name( 54), rel_rx( 54), f0( 54), LeBasM( 54), M_ac( 54) / 'NTRM         ',    16.0, 0.0, 156.1, 0.420/
      DATA vd_name( 55), rel_rx( 55), f0( 55), LeBasM( 55), M_ac( 55) / 'MPAN         ',    16.0, 0.1, 133.0, 0.019/
      DATA vd_name( 56), rel_rx( 56), f0( 56), LeBasM( 56), M_ac( 56) / 'PPN          ',    16.0, 0.1, 118.2, 0.019/
      DATA vd_name( 57), rel_rx( 57), f0( 57), LeBasM( 57), M_ac( 57) / 'ISPD         ',     8.0, 1.0,  88.8, 0.420/
      DATA vd_name( 58), rel_rx( 58), f0( 58), LeBasM( 58), M_ac( 58) / 'NTRDN        ',    16.0, 0.1, 169.8, 0.420/
      DATA vd_name( 59), rel_rx( 59), f0( 59), LeBasM( 59), M_ac( 59) / 'NTRALK       ',    16.0, 0.1, 133.0, 0.420/
      DATA vd_name( 60), rel_rx( 60), f0( 60), LeBasM( 60), M_ac( 60) / 'NTROH        ',    16.0, 0.1, 140.4, 0.420/
      DATA vd_name( 61), rel_rx( 61), f0( 61), LeBasM( 61), M_ac( 61) / 'NTRPX        ',    16.0, 0.0, 147.8, 0.420/
      DATA vd_name( 62), rel_rx( 62), f0( 62), LeBasM( 62), M_ac( 62) / 'PROPNN       ',    16.0, 0.0, 133.0, 0.420/
      DATA vd_name( 63), rel_rx( 63), f0( 63), LeBasM( 63), M_ac( 63) / 'CLNO2        ',     8.0, 0.0,  45.5, 0.420/  ! dif0 estimated following Erickson III et al., JGR, 104, D7, 8347-8372, 1999
      DATA vd_name( 64), rel_rx( 64), f0( 64), LeBasM( 64), M_ac( 64) / 'ISOPNN       ',   275.9, 0.0, 206.8, 0.795/
      DATA vd_name( 65), rel_rx( 65), f0( 65), LeBasM( 65), M_ac( 65) / 'MTNO3        ',    96.2, 0.0, 251.2, 0.762/
      DATA vd_name( 66), rel_rx( 66), f0( 66), LeBasM( 66), M_ac( 66) / 'IEPOX        ',     8.0, 0.0,  110.8, 0.420/
      DATA vd_name( 67), rel_rx( 67), f0( 67), LeBasM( 67), M_ac( 67) / 'HACET        ',     8.0, 0.0,   72.6, 0.420/  ! dif0 from Nguyen 2015 PNAS
      DATA vd_name( 68), rel_rx( 68), f0( 68), LeBasM( 68), M_ac( 68) / 'SVALK1       ',  4572.8, 0.0,  280.5, 0.908/
      DATA vd_name( 69), rel_rx( 69), f0( 69), LeBasM( 69), M_ac( 69) / 'SVALK2       ',    12.9, 0.0,  275.6, 0.523/
      DATA vd_name( 70), rel_rx( 70), f0( 70), LeBasM( 70), M_ac( 70) / 'SVBNZ1       ', 20671.2, 0.0,  134.1, 0.906/
      DATA vd_name( 71), rel_rx( 71), f0( 71), LeBasM( 71), M_ac( 71) / 'SVBNZ2       ',    52.5, 0.0,  127.5, 0.438/
      DATA vd_name( 72), rel_rx( 72), f0( 72), LeBasM( 72), M_ac( 72) / 'SVISO1       ',    50.6, 0.0,  126.3, 0.434/
      DATA vd_name( 73), rel_rx( 73), f0( 73), LeBasM( 73), M_ac( 73) / 'SVISO2       ', 10009.0, 0.0,  123.8, 0.901/
      DATA vd_name( 74), rel_rx( 74), f0( 74), LeBasM( 74), M_ac( 74) / 'SVPAH1       ',   772.1, 0.0,  235.7, 0.886/
      DATA vd_name( 75), rel_rx( 75), f0( 75), LeBasM( 75), M_ac( 75) / 'SVPAH2       ',     4.8, 0.0,  231.5, 0.420/
      DATA vd_name( 76), rel_rx( 76), f0( 76), LeBasM( 76), M_ac( 76) / 'SVSQT        ',    10.3, 0.0,  346.5, 0.652/
      DATA vd_name( 77), rel_rx( 77), f0( 77), LeBasM( 77), M_ac( 77) / 'SVTOL1       ',  1928.3, 0.0,  153.7, 0.877/
      DATA vd_name( 78), rel_rx( 78), f0( 78), LeBasM( 78), M_ac( 78) / 'SVTOL2       ',   111.6, 0.0,  194.1, 0.679/
      DATA vd_name( 79), rel_rx( 79), f0( 79), LeBasM( 79), M_ac( 79) / 'SVTRP1       ',   160.1, 0.0,  194.9, 0.735/
      DATA vd_name( 80), rel_rx( 80), f0( 80), LeBasM( 80), M_ac( 80) / 'SVTRP2       ',    13.0, 0.0,  218.8, 0.429/
      DATA vd_name( 81), rel_rx( 81), f0( 81), LeBasM( 81), M_ac( 81) / 'SVXYL1       ',  3586.7, 0.0,  154.6, 0.891/
      DATA vd_name( 82), rel_rx( 82), f0( 82), LeBasM( 82), M_ac( 82) / 'SVXYL2       ',    72.2, 0.0,  194.6, 0.594/
      DATA vd_name( 83), rel_rx( 83), f0( 83), LeBasM( 83), M_ac( 83) / 'IO           ',     8.0, 0.0,   44.4, 0.420/
      DATA vd_name( 84), rel_rx( 84), f0( 84), LeBasM( 84), M_ac( 84) / 'OIO          ',     8.0, 0.0,   51.8, 0.420/
      DATA vd_name( 85), rel_rx( 85), f0( 85), LeBasM( 85), M_ac( 85) / 'I2O2         ',     8.0, 0.0,   88.8, 0.420/
      DATA vd_name( 86), rel_rx( 86), f0( 86), LeBasM( 86), M_ac( 86) / 'I2O3         ',     8.0, 0.0,   96.2, 0.420/
      DATA vd_name( 87), rel_rx( 87), f0( 87), LeBasM( 87), M_ac( 87) / 'I2O4         ',     8.0, 0.0,  103.6, 0.420/
      DATA vd_name( 88), rel_rx( 88), f0( 88), LeBasM( 88), M_ac( 88) / 'HI           ',     8.0, 0.0,   40.7, 0.420/
      DATA vd_name( 89), rel_rx( 89), f0( 89), LeBasM( 89), M_ac( 89) / 'HOI          ',     8.0, 0.0,   48.1, 0.420/
      DATA vd_name( 90), rel_rx( 90), f0( 90), LeBasM( 90), M_ac( 90) / 'INO          ',     8.0, 0.0,   60.9, 0.420/
      DATA vd_name( 91), rel_rx( 91), f0( 91), LeBasM( 91), M_ac( 91) / 'INO2         ',    20.0, 0.0,   69.2, 0.420/
      DATA vd_name( 92), rel_rx( 92), f0( 92), LeBasM( 92), M_ac( 92) / 'INO3         ',     8.0, 0.0,   77.5, 0.420/
      DATA vd_name( 93), rel_rx( 93), f0( 93), LeBasM( 93), M_ac( 93) / 'BRO          ',     1.0, 0.0,   34.4, 0.420/
      DATA vd_name( 94), rel_rx( 94), f0( 94), LeBasM( 94), M_ac( 94) / 'HOBR         ',     1.0, 0.0,   38.1, 0.420/
      DATA vd_name( 95), rel_rx( 95), f0( 95), LeBasM( 95), M_ac( 95) / 'HBR          ',     2.0, 0.0,   30.7, 0.420/
      DATA vd_name( 96), rel_rx( 96), f0( 96), LeBasM( 96), M_ac( 96) / 'BRNO3        ',     1.0, 0.0,   67.5, 0.420/
      DATA vd_name( 97), rel_rx( 97), f0( 97), LeBasM( 97), M_ac( 97) / 'BRNO2        ',     1.0, 0.0,   59.2, 0.420/
      DATA vd_name( 98), rel_rx( 98), f0( 98), LeBasM( 98), M_ac( 98) / 'BRCL         ',     1.0, 0.0,   51.6, 0.420/
      DATA vd_name( 99), rel_rx( 99), f0( 99), LeBasM( 99), M_ac( 99) / 'DMS          ',     2.0, 0.0,   77.4, 0.420/
      DATA vd_name(100), rel_rx(100), f0(100), LeBasM(100), M_ac(100) / 'MSA          ',     2.0, 0.0,   77.4, 0.420/
      DATA vd_name(101), rel_rx(101), f0(101), LeBasM(101), M_ac(101) / 'METHANE      ',     2.0, 0.0,   29.6,5.0e-5/ ! dif0, equation 9-22. Scwarzenbach et. (1993) Env. Org. Chem.
      DATA vd_name(102), rel_rx(102), f0(102), LeBasM(102), M_ac(102) / 'ACRYACID     ',     2.0, 0.0,   63.2, 0.420/ 
      DATA vd_name(103), rel_rx(103), f0(103), LeBasM(103), M_ac(103) / 'CARBSULFIDE  ',     5.0, 0.0,   51.5, 0.420/ 
      DATA vd_name(104), rel_rx(104), f0(104), LeBasM(104), M_ac(104) / 'ACETONITRILE ',     5.0, 0.0,   52.3, 0.420/ 
      DATA vd_name(105), rel_rx(105), f0(105), LeBasM(105), M_ac(105) / 'METH_NIT_PHEN',    16.0, 0.0,  155.0, 0.420/ ! dif0, equation 9-22. Scwarzenbach et. (1993) Env. Org. Chem.
      DATA vd_name(106), rel_rx(106), f0(106), LeBasM(106), M_ac(106) / 'PCVOC        ',    10.0, 0.0,   56.0, 0.420/ ! PCVOC
      DATA vd_name(107), rel_rx(107), f0(107), LeBasM(107), M_ac(107) / 'INTR         ',    16.0, 0.1,  140.4, 0.420/ ! INTR
      DATA vd_name(108), rel_rx(108), f0(108), LeBasM(108), M_ac(108) / 'ISPX         ',    10.0, 0.3,   49.0, 0.420/ ! ISPX diffusion should be ~ 0.0710 according to Wolfe and thornton 2011 M_acP
      DATA vd_name(109), rel_rx(109), f0(109), LeBasM(109), M_ac(109) / 'ROOH         ',    10.0, 0.3,   49.0, 0.420/ ! ROOH diffusion should be ~ 0.0710 according to Wolfe and thornton 2011 M_acP
      DATA vd_name(110), rel_rx(110), f0(110), LeBasM(110), M_ac(110) / 'LVPCSOG      ', 90000.0, 0.0,   63.0, 0.910/ ! LVPCSOG
      DATA vd_name(111), rel_rx(111), f0(111), LeBasM(111), M_ac(111) / 'VIVPO1       ',     4.2, 0.0,   63.0, 0.909/ ! VIVPO1
      DATA vd_name(112), rel_rx(112), f0(112), LeBasM(112), M_ac(112) / 'VLVOO1       ', 71624.8, 0.0,   63.0, 0.910/ ! VLVOO1
      DATA vd_name(113), rel_rx(113), f0(113), LeBasM(113), M_ac(113) / 'VLVOO2       ',  9042.0, 0.0,   63.0, 0.909/ ! VLVOO2
      DATA vd_name(114), rel_rx(114), f0(114), LeBasM(114), M_ac(114) / 'VLVPO1       ', 13818.0, 0.0,   63.0, 0.420/ ! VLVPO1
      DATA vd_name(115), rel_rx(115), f0(115), LeBasM(115), M_ac(115) / 'VSVOO1       ',  1133.9, 0.0,   63.0, 0.896/ ! VSVOO1
      DATA vd_name(116), rel_rx(116), f0(116), LeBasM(116), M_ac(116) / 'VSVOO2       ',    18.1, 0.0,   63.0, 0.444/ ! VSVOO2
      DATA vd_name(117), rel_rx(117), f0(117), LeBasM(117), M_ac(117) / 'VSVOO3       ',     2.3, 0.0,   63.0, 0.420/ ! VSVOO3
      DATA vd_name(118), rel_rx(118), f0(118), LeBasM(118), M_ac(118) / 'VSVPO1       ',  1830.5, 0.0,   63.0, 0.896/ ! VSVPO1
      DATA vd_name(119), rel_rx(119), f0(119), LeBasM(119), M_ac(119) / 'VSVPO2       ',   241.0, 0.0,   63.0, 0.783/ ! VSVPO2
      DATA vd_name(120), rel_rx(120), f0(120), LeBasM(120), M_ac(120) / 'VSVPO3       ',    31.8, 0.0,   63.0, 0.444/ ! VSVPO3
      DATA vd_name(121), rel_rx(121), f0(121), LeBasM(121), M_ac(121) / 'FACD         ',    20.0, 0.0,   63.0, 0.023/ ! FACD
      DATA vd_name(122), rel_rx(122), f0(122), LeBasM(122), M_ac(122) / 'KET          ',     1.0, 0.0,  108.2, 0.420/ ! KET different in differnt mechanisms
      DATA vd_name(123), rel_rx(123), f0(123), LeBasM(123), M_ac(123) / 'ETH          ',     1.0, 0.0,   58.1,1.0e-4/ ! ETH
      DATA vd_name(124), rel_rx(124), f0(124), LeBasM(124), M_ac(124) / 'PNA          ',     1.0, 0.0,   45.2, 0.100/ ! PNA
      DATA vd_name(125), rel_rx(125), f0(125), LeBasM(125), M_ac(125) / 'GLY          ',     1.0, 0.0,   56.2, 0.023/ ! GLY
      DATA vd_name(126), rel_rx(126), f0(126), LeBasM(126), M_ac(126) / 'GLYD         ',     1.0, 0.0,   56.4, 0.023/ ! GLYD
      DATA vd_name(127), rel_rx(127), f0(127), LeBasM(127), M_ac(127) / 'MGLY         ',     1.0, 0.0,   72.5, 0.023/ ! MGLY
      DATA vd_name(128), rel_rx(128), f0(128), LeBasM(128), M_ac(128) / 'ETHA         ',     1.0, 0.0,   61.5,1.0e-4/ ! ETHA
      DATA vd_name(129), rel_rx(129), f0(129), LeBasM(129), M_ac(129) / 'ETOH         ',     1.0, 0.0,   59.1, 0.018/ ! ETOH
      DATA vd_name(130), rel_rx(130), f0(130), LeBasM(130), M_ac(130) / 'PAR          ',     1.0, 0.0,  111.1, 0.420/ ! PAR as Pentane
      DATA vd_name(131), rel_rx(131), f0(131), LeBasM(131), M_ac(131) / 'ACET         ',     1.0, 0.0,   75.2,5.4e-3/ ! M_acET
      DATA vd_name(132), rel_rx(132), f0(132), LeBasM(132), M_ac(132) / 'PRPA         ',     1.0, 0.0,   78.1, 0.420/ ! PRPA
      DATA vd_name(133), rel_rx(133), f0(133), LeBasM(133), M_ac(133) / 'ETHY         ',     1.0, 0.0,   45.8, 0.420/ ! ETHY
      DATA vd_name(134), rel_rx(134), f0(134), LeBasM(134), M_ac(134) / 'OLE          ',     1.0, 0.0,   73.1, 0.420/ ! OLE as Propene
      DATA vd_name(135), rel_rx(135), f0(135), LeBasM(135), M_ac(135) / 'IOLE         ',     1.0, 0.0,   89.5, 0.420/ ! IOLE as Isobutene
      DATA vd_name(136), rel_rx(136), f0(136), LeBasM(136), M_ac(136) / 'BENZ         ',     1.0, 0.0,   89.4, 0.420/ ! BENZENE
      DATA vd_name(137), rel_rx(137), f0(137), LeBasM(137), M_ac(137) / 'CRES         ',     1.0, 0.0,  108.1, 0.420/ ! CRES
      DATA vd_name(138), rel_rx(138), f0(138), LeBasM(138), M_ac(138) / 'TOL          ',     1.0, 0.0,  105.7, 0.420/ ! TOL
      DATA vd_name(139), rel_rx(139), f0(139), LeBasM(139), M_ac(139) / 'XYLMN        ',     1.0, 0.0,  122.0, 0.420/ ! XYLMN
      DATA vd_name(140), rel_rx(140), f0(140), LeBasM(140), M_ac(140) / 'NAPH         ',     1.0, 0.0,  123.5, 0.420/ ! NAPH
      DATA vd_name(141), rel_rx(141), f0(141), LeBasM(141), M_ac(141) / 'CAT1         ',     1.0, 0.0,  102.6, 0.420/ ! CAT1
      DATA vd_name(142), rel_rx(142), f0(142), LeBasM(142), M_ac(142) / 'SESQ         ',     1.0, 0.0,  251.5, 0.420/ ! SESQ
      DATA vd_name(143), rel_rx(143), f0(143), LeBasM(143), M_ac(143) / 'TERP         ',     1.0, 0.0,  136.2, 0.420/ ! TERP
      DATA vd_name(144), rel_rx(144), f0(144), LeBasM(144), M_ac(144) / 'ISOP         ',     1.0, 0.0,  136.2, 0.420/ ! ISOP
      DATA vd_name(145), rel_rx(145), f0(145), LeBasM(145), M_ac(145) / 'OPEN         ',     1.0, 0.0,   69.6, 0.420/ ! OPEN C4H4O2
      DATA vd_name(146), rel_rx(146), f0(146), LeBasM(146), M_ac(146) / 'XOPN         ',     1.0, 0.0,   81.7, 0.420/ ! XOPN C5H6O2
      DATA vd_name(147), rel_rx(147), f0(147), LeBasM(147), M_ac(147) / 'SOAALK       ',     1.0, 0.0,  142.8, 0.420/ ! SOAALK as Propylcyclopentane
      DATA vd_name(148), rel_rx(148), f0(148), LeBasM(148), M_ac(148) / 'BUTADIENE13  ',     1.0, 0.0,   84.8, 0.420/ ! BUTADIENE13
      DATA vd_name(149), rel_rx(149), f0(149), LeBasM(149), M_ac(149) / 'ACROLEIN     ',     1.0, 0.0,   70.5, 0.420/ ! ACROLEIN
      DATA vd_name(150), rel_rx(150), f0(150), LeBasM(150), M_ac(150) / 'SVMT1        ',145839.6, 0.0,  355.2, 0.910/ ! SVMT1 Xu et al., 2018 ACPD: doi:10.5194/acp-2017-1109
      DATA vd_name(151), rel_rx(151), f0(151), LeBasM(151), M_ac(151) / 'SVMT2        ', 12788.1, 0.0,  236.8, 0.909/ ! SVMT2
      DATA vd_name(152), rel_rx(152), f0(152), LeBasM(152), M_ac(152) / 'SVMT3        ',  1512.4, 0.0,  214.6, 0.896/ ! SVMT3
      DATA vd_name(153), rel_rx(153), f0(153), LeBasM(153), M_ac(153) / 'SVMT4        ',   189.1, 0.0,  229.4, 0.783/ ! SVMT4
      DATA vd_name(154), rel_rx(154), f0(154), LeBasM(154), M_ac(154) / 'SVMT5        ',    22.2, 0.0,  207.2, 0.444/ ! SVMT5
      DATA vd_name(155), rel_rx(155), f0(155), LeBasM(155), M_ac(155) / 'SVMT6        ',     2.8, 0.0,  222.0, 0.420/ ! SVMT6
      DATA vd_name(156), rel_rx(156), f0(156), LeBasM(156), M_ac(156) / 'SVMT7        ',     1.0, 0.0,  199.8, 0.420/ ! SVMT7
      DATA vd_name(157), rel_rx(157), f0(157), LeBasM(157), M_ac(157) / 'SVAVB1       ',100388.0, 0.0,  163.1, 0.910/ ! SVAVB1
      DATA vd_name(158), rel_rx(158), f0(158), LeBasM(158), M_ac(158) / 'SVAVB2       ',  1461.2, 0.0,  163.2, 0.896/ ! SVAVB2
      DATA vd_name(159), rel_rx(159), f0(159), LeBasM(159), M_ac(159) / 'SVAVB3       ',   175.2, 0.0,  163.0, 0.783/ ! SVAVB3
      DATA vd_name(160), rel_rx(160), f0(160), LeBasM(160), M_ac(160) / 'SVAVB4       ',    20.8, 0.0,  162.7, 0.444/ ! SVAVB4
      DATA vd_name(161), rel_rx(161), f0(161), LeBasM(161), M_ac(161) / 'CLNO3        ',     8.0, 0.0,   65.1, 0.420/ ! CLNO3
      DATA vd_name(162), rel_rx(162), f0(162), LeBasM(162), M_ac(162) / 'FMBR         ',    10.0, 0.0,   52.9, 0.420/ ! FMBR
      DATA vd_name(163), rel_rx(163), f0(163), LeBasM(163), M_ac(163) / 'I2           ',     4.0, 0.0,   77.0, 0.420/ ! I2
      DATA vd_name(164), rel_rx(164), f0(164), LeBasM(164), M_ac(164) / 'CH3I         ',     2.0, 0.0,   66.5, 0.420/ ! CH3I
      DATA vd_name(165), rel_rx(165), f0(165), LeBasM(165), M_ac(165) / 'ICL          ',     4.0, 0.0,   63.0, 0.420/ ! ICL
      DATA vd_name(166), rel_rx(166), f0(166), LeBasM(166), M_ac(166) / 'IBR          ',     4.0, 0.0,   70.0, 0.420/ ! IBR
      DATA vd_name(167), rel_rx(167), f0(167), LeBasM(167), M_ac(167) / 'MI2          ',     2.0, 0.0,   98.0, 0.420/ ! MI2
      DATA vd_name(168), rel_rx(168), f0(168), LeBasM(168), M_ac(168) / 'MIB          ',     2.0, 0.0,   91.0, 0.420/ ! MIB
      DATA vd_name(169), rel_rx(169), f0(169), LeBasM(169), M_ac(169) / 'MIC          ',     2.0, 0.0,   84.0, 0.420/ ! MIC
      DATA vd_name(170), rel_rx(170), f0(170), LeBasM(170), M_ac(170) / 'BR2          ',     2.0, 0.0,   63.0, 0.420/ ! BR2
      DATA vd_name(171), rel_rx(171), f0(171), LeBasM(171), M_ac(171) / 'BR3          ',     2.0, 0.0,  108.5, 0.420/ ! BR3
      DATA vd_name(172), rel_rx(172), f0(172), LeBasM(172), M_ac(172) / 'CH3BR        ',     2.0, 0.0,   59.5, 0.420/ ! CH3BR
      DATA vd_name(173), rel_rx(173), f0(173), LeBasM(173), M_ac(173) / 'MB2          ',     2.0, 0.0,   84.0, 0.420/ ! MB2
      DATA vd_name(174), rel_rx(174), f0(174), LeBasM(174), M_ac(174) / 'MB2C         ',     2.0, 0.0,  101.5, 0.420/ ! MB2C
      DATA vd_name(175), rel_rx(175), f0(175), LeBasM(175), M_ac(175) / 'MBC2         ',     2.0, 0.0,   94.5, 0.420/ ! MBC2
      DATA vd_name(176), rel_rx(176), f0(176), LeBasM(176), M_ac(176) / 'MBC          ',     2.0, 0.0,   77.0, 0.420/ ! MBC
      DATA vd_name(177), rel_rx(177), f0(177), LeBasM(177), M_ac(177) / 'CLO          ',     8.0, 0.0,   31.5, 0.420/ ! CLO

      Contains
         SUBROUTINE Map_STAGE 

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN 

         IMPLICIT NONE

         Integer               :: c, r, l, n, s, v
         CHARACTER( 16 )       :: gc_depv_name( dep_gas_all )
         CHARACTER( 16 )  :: PNAME = 'Map_STAGE'
         CHARACTER( 120 ) :: XMSG = ' '

         molwt_all    = 0.0
         asx_run_map  = .FALSE.
         gc_depv_name = 'None'
         H_name_all   = 'None'

         n_gas_asx   = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV
         
         Do v = 1, N_GC_DEPV
            s = index1( gc_depv( v ),dep_gas_all,'VD_'//vd_name ) 
            If( s .Gt. 0.0 ) Then
               molwt_all( s ) = gc_molwt( gc_depv_map( v ) )
               gc_depv_name( s ) = gc_spc( gc_depv_map( v ) )
               asx_run_map( s ) = .TRUE.
            End If
         End Do 
         Do v = 1, N_GC_SCAV
            s = index1(gc_spc(gc_scav_map(v)),dep_gas_all,gc_depv_name)
            if( s .Gt. 0.0 ) Then
               H_name_all( s ) = gc_scav(v)
            End if
         End Do
         Do v = 1, N_NR_DEPV
            s = index1( nr_depv( v ),dep_gas_all,'VD_'//vd_name ) 
            If( s .Gt. 0.0 ) Then
               molwt_all( s ) = nr_molwt( nr_depv_map( v ) )
               gc_depv_name( s ) = nr_spc( nr_depv_map( v ) )
               asx_run_map( s ) = .TRUE.
            End If
         End Do 
         Do v = 1, N_NR_SCAV
            s = index1(nr_spc(nr_scav_map(v)),dep_gas_all,gc_depv_name)
            if( s .Gt. 0.0 ) Then
               H_name_all( s ) = nr_scav(v)
            End if
         End Do
         Do v = 1, N_TR_DEPV
            s = index1( tr_depv( v ),dep_gas_all,'VD_'//vd_name ) 
            If( s .Gt. 0.0 ) Then
               molwt_all( s ) = tr_molwt( tr_depv_map( v ) )
               gc_depv_name( s ) = tr_spc( tr_depv_map( v ) )
               asx_run_map( s ) = .TRUE.
            End If
         End Do 
         Do v = 1, N_TR_SCAV
            s = index1(tr_spc(tr_scav_map(v)),dep_gas_all,gc_depv_name)
            if( s .Gt. 0.0 ) Then
               H_name_all( s ) = tr_scav(v)
            End if
         End Do

         Write(Logdev,*) '*********************** STAGE Deposition Map ***********************'
         Write(Logdev,99901) 'Dep. Species', 'H Surrogate', 'Mol Weight'
         Do v = 1, dep_gas_all
           If( asx_run_map (v) ) Write(Logdev,99902) gc_depv_name(v), H_name_all(v), molwt_all(v)
         End Do       
99901    Format(A16,1x,A16,1x,A16)
99902    Format(A16,1x,A16,10x,F6.2)

         End Subroutine Map_Stage
      End Module Stage_Data
