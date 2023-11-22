
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

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE ELMO_DATA

      USE AERO_DATA, ONLY : N_MODE
      USE UTILIO_DEFN

      IMPLICIT NONE

#ifdef mpas
      INTEGER, PARAMETER :: M3REAL  =   5
#endif

      ! Define Logical values as T and F for the aerospc table
      Logical, Parameter, Private :: T = .true.
      Logical, Parameter, Private :: F = .false.

      LOGICAL, SAVE :: L_ELMO
      REAL,    SAVE :: ELMO_NSTEP = 0.
      INTEGER, SAVE :: NLAY_ELMO_INST
      INTEGER, SAVE :: NLAY_ELMO_AVRG

      ! Define Indices for All ELMO Parameters
      ! The idea here is that parameters will be organized and
      ! calculated in the order of this list, not the ELMO_LIST. The
      ! ELMO_LIST is searchable via the Parameter names but they
      ! link to this list of integers so that each parameter can be
      ! easily referenced during the run without having to do string
      ! lookups. Please see the table defining the ELMO_LIST below 
      ! for a description of each of these indices.
      INTEGER, PARAMETER :: ID_STDEV =        1
      INTEGER, PARAMETER :: ID_DRY_DG =       2
      INTEGER, PARAMETER :: ID_WET_DG =       3 
      INTEGER, PARAMETER :: ID_WET_M2 =       4
      INTEGER, PARAMETER :: ID_DRY_M3 =       5
      INTEGER, PARAMETER :: ID_WET_M3 =       6
      INTEGER, PARAMETER :: ID_PM_NUM =       7
      INTEGER, PARAMETER :: ID_PMU_NUM =      8
      INTEGER, PARAMETER :: ID_PMF_NUM =      9
      INTEGER, PARAMETER :: ID_PMC_NUM =     10
      INTEGER, PARAMETER :: ID_N10 =         11
      INTEGER, PARAMETER :: ID_N20 =         12
      INTEGER, PARAMETER :: ID_N40 =         13
      INTEGER, PARAMETER :: ID_N100 =        14
      INTEGER, PARAMETER :: ID_PM_SRF =      15
      INTEGER, PARAMETER :: ID_PMU_SRF =     16
      INTEGER, PARAMETER :: ID_PMF_SRF =     17
      INTEGER, PARAMETER :: ID_PMC_SRF =     18
      INTEGER, PARAMETER :: ID_PM_MASS =     19
      INTEGER, PARAMETER :: ID_PMU_MASS =    20
      INTEGER, PARAMETER :: ID_PMF_MASS =    21
      INTEGER, PARAMETER :: ID_PMC_MASS =    22
      INTEGER, PARAMETER :: ID_PMNUC_MASS =  23
      INTEGER, PARAMETER :: ID_PMAIT_MASS =  24
      INTEGER, PARAMETER :: ID_PMACC_MASS =  25
      INTEGER, PARAMETER :: ID_DRY_DENS =    26
      INTEGER, PARAMETER :: ID_WET_DENS =    27
      INTEGER, PARAMETER :: ID_FPM01 =       28
      INTEGER, PARAMETER :: ID_FPM1 =        29
      INTEGER, PARAMETER :: ID_FPM25 =       30
      INTEGER, PARAMETER :: ID_FPM10 =       31
      INTEGER, PARAMETER :: ID_FPM25to10 =   32
      INTEGER, PARAMETER :: ID_FAMS =        33
      INTEGER, PARAMETER :: ID_PM01 =        34
      INTEGER, PARAMETER :: ID_PM1 =         35
      INTEGER, PARAMETER :: ID_PM25 =        36
      INTEGER, PARAMETER :: ID_PM10 =        37
      INTEGER, PARAMETER :: ID_PM25to10 =    38
      INTEGER, PARAMETER :: ID_PMAMS =       39
      INTEGER, PARAMETER :: ID_PMC_SO4 =     40
      INTEGER, PARAMETER :: ID_PMC_NO3 =     41
      INTEGER, PARAMETER :: ID_PMC_NH4 =     42
      INTEGER, PARAMETER :: ID_PMC_CL =      43
      INTEGER, PARAMETER :: ID_PMC_NA =      44
      INTEGER, PARAMETER :: ID_PMC_MG =      45
      INTEGER, PARAMETER :: ID_PMC_K =       46
      INTEGER, PARAMETER :: ID_PMC_CA =      47
      INTEGER, PARAMETER :: ID_PMF_SO4 =     48
      INTEGER, PARAMETER :: ID_PMF_NO3 =     49
      INTEGER, PARAMETER :: ID_PMF_NH4 =     50
      INTEGER, PARAMETER :: ID_PMF_CL =      51
      INTEGER, PARAMETER :: ID_PMF_NA =      52
      INTEGER, PARAMETER :: ID_PMF_EC =      53
      INTEGER, PARAMETER :: ID_PMF_OC =      54
      INTEGER, PARAMETER :: ID_PMF_OA =      55
      INTEGER, PARAMETER :: ID_PMF_FE =      56
      INTEGER, PARAMETER :: ID_PMF_AL =      57
      INTEGER, PARAMETER :: ID_PMF_SI =      58
      INTEGER, PARAMETER :: ID_PMF_TI =      59
      INTEGER, PARAMETER :: ID_PMF_CA =      60
      INTEGER, PARAMETER :: ID_PMF_MG =      61
      INTEGER, PARAMETER :: ID_PMF_K  =      62
      INTEGER, PARAMETER :: ID_PMF_MN =      63
      INTEGER, PARAMETER :: ID_PMF_H2O =     64                
      INTEGER, PARAMETER :: ID_SOIL_IMPV =   65                
      INTEGER, PARAMETER :: ID_PMF_UNSP1 =   66                
      INTEGER, PARAMETER :: ID_PMF_UNSP2 =   67                
      INTEGER, PARAMETER :: ID_PMF_HP =      68                
      INTEGER, PARAMETER :: ID_PMF_HPM =     69                
      INTEGER, PARAMETER :: ID_PMF_PH =      70                
      INTEGER, PARAMETER :: ID_PMF_POC =     71                
      INTEGER, PARAMETER :: ID_PMF_SOC =     72
      INTEGER, PARAMETER :: ID_PMF_POA =     73
      INTEGER, PARAMETER :: ID_PMF_SOA =     74
      INTEGER, PARAMETER :: ID_PMF_NCOM =    75
      INTEGER, PARAMETER :: ID_PMF_OMOC =    76
      INTEGER, PARAMETER :: ID_PMF_OtoC =    77
      INTEGER, PARAMETER :: ID_PMF_ASOA =    78
      INTEGER, PARAMETER :: ID_PMF_BSOA =    79
      INTEGER, PARAMETER :: ID_PMF_CLDGLY =  80
      INTEGER, PARAMETER :: ID_PMF_ISOP =    81
      INTEGER, PARAMETER :: ID_PMF_IEPOX =   82
      INTEGER, PARAMETER :: ID_PMF_MTNSOA =  83
      INTEGER, PARAMETER :: ID_PMF_MTSOA =   84
      INTEGER, PARAMETER :: ID_AMS_SO4 =     85
      INTEGER, PARAMETER :: ID_AMS_NO3 =     86
      INTEGER, PARAMETER :: ID_AMS_NH4 =     87
      INTEGER, PARAMETER :: ID_AMS_CL =      88
      INTEGER, PARAMETER :: ID_AMS_OA =      89
      INTEGER, PARAMETER :: ID_AMS_OTOC =    90
      INTEGER, PARAMETER :: ID_PM1_SO4 =     91
      INTEGER, PARAMETER :: ID_PM1_NO3 =     92
      INTEGER, PARAMETER :: ID_PM1_NH4 =     93
      INTEGER, PARAMETER :: ID_PM1_CL =      94
      INTEGER, PARAMETER :: ID_PM1_NA =      95
      INTEGER, PARAMETER :: ID_PM1_EC =      96
      INTEGER, PARAMETER :: ID_PM1_OC =      97
      INTEGER, PARAMETER :: ID_PM1_OA =      98
      INTEGER, PARAMETER :: ID_PM1_MG =      99
      INTEGER, PARAMETER :: ID_PM1_K =      100
      INTEGER, PARAMETER :: ID_PM1_CA =     101
      INTEGER, PARAMETER :: ID_PM1_OT =     102
      INTEGER, PARAMETER :: ID_PM1_FE =     103
      INTEGER, PARAMETER :: ID_PM1_SI =     104
      INTEGER, PARAMETER :: ID_PM1_TI =     105
      INTEGER, PARAMETER :: ID_PM1_MN =     106
      INTEGER, PARAMETER :: ID_PM1_AL =     107
      INTEGER, PARAMETER :: ID_PM1_SOIL =   108
      INTEGER, PARAMETER :: ID_PM1_UNSP1 =  109
      INTEGER, PARAMETER :: ID_PM1_UN =     110
      INTEGER, PARAMETER :: ID_PM1_HP =     111
      INTEGER, PARAMETER :: ID_PM25_SO4 =   112
      INTEGER, PARAMETER :: ID_PM25_NO3 =   113
      INTEGER, PARAMETER :: ID_PM25_NH4 =   114
      INTEGER, PARAMETER :: ID_PM25_CL =    115
      INTEGER, PARAMETER :: ID_PM25_NA =    116
      INTEGER, PARAMETER :: ID_PM25_EC =    117
      INTEGER, PARAMETER :: ID_PM25_OC =    118
      INTEGER, PARAMETER :: ID_PM25_OA =    119
      INTEGER, PARAMETER :: ID_PM25_MG =    120
      INTEGER, PARAMETER :: ID_PM25_K =     121
      INTEGER, PARAMETER :: ID_PM25_CA =    122
      INTEGER, PARAMETER :: ID_PM25_OT =    123
      INTEGER, PARAMETER :: ID_PM25_FE =    124
      INTEGER, PARAMETER :: ID_PM25_SI =    125
      INTEGER, PARAMETER :: ID_PM25_TI =    126
      INTEGER, PARAMETER :: ID_PM25_MN =    127
      INTEGER, PARAMETER :: ID_PM25_AL =    128
      INTEGER, PARAMETER :: ID_PM25_SOIL =  129
      INTEGER, PARAMETER :: ID_PM25_UNSP1 = 130
      INTEGER, PARAMETER :: ID_PM25_UN =    131
      INTEGER, PARAMETER :: ID_PM25_HP =    132
      INTEGER, PARAMETER :: ID_PM25to10_SO4=133
      INTEGER, PARAMETER :: ID_PM25to10_NO3=134
      INTEGER, PARAMETER :: ID_PM25to10_NH4=135
      INTEGER, PARAMETER :: ID_PM25to10_CL =136
      INTEGER, PARAMETER :: ID_PM25to10_NA =137
      INTEGER, PARAMETER :: ID_PM25_HDSL   =138
      INTEGER, PARAMETER :: ID_PM25_HBE    =139
      INTEGER, PARAMETER :: ID_PM25_HCD    =140
      INTEGER, PARAMETER :: ID_PM25_HCR3   =141
      INTEGER, PARAMETER :: ID_PM25_HCR6   =142
      INTEGER, PARAMETER :: ID_PM25_HCR    =143
      INTEGER, PARAMETER :: ID_PM25_HPB    =144
      INTEGER, PARAMETER :: ID_PM25_HMN    =145
      INTEGER, PARAMETER :: ID_PM25_HNI    =146
      INTEGER, PARAMETER :: ID_PM25_HAS    =147
      INTEGER, PARAMETER :: ID_PM25_HG     =148
      INTEGER, PARAMETER :: ID_PM10_HDSL   =149
      INTEGER, PARAMETER :: ID_PM10_HBE    =150
      INTEGER, PARAMETER :: ID_PM10_HCD    =151
      INTEGER, PARAMETER :: ID_PM10_HCR3   =152
      INTEGER, PARAMETER :: ID_PM10_HCR6   =153
      INTEGER, PARAMETER :: ID_PM10_HCR    =154
      INTEGER, PARAMETER :: ID_PM10_HPB    =155
      INTEGER, PARAMETER :: ID_PM10_HMN    =156
      INTEGER, PARAMETER :: ID_PM10_HNI    =157
      INTEGER, PARAMETER :: ID_PM10_HAS    =158
      INTEGER, PARAMETER :: ID_PM10_HG     =159
      INTEGER, PARAMETER :: ID_PM25_BENAPY =160
      INTEGER, PARAMETER :: ID_PM10_BENAPY =161
      INTEGER, PARAMETER :: ID_PMF_BENAPY  =162
      INTEGER, PARAMETER :: ID_GAS_BENAPY  =163
      INTEGER, PARAMETER :: ID_BENAPY_FAERO=164
      INTEGER, PARAMETER :: ID_TNO3 =       165                     
      INTEGER, PARAMETER :: ID_TNO3TOT =    166                     
      INTEGER, PARAMETER :: ID_PM25_FRM =   167                     
      INTEGER, PARAMETER :: ID_PMF_FRM =    168                     
      INTEGER, PARAMETER :: ID_AOD550 =     169                     
      INTEGER, PARAMETER :: ID_PMEXT550 =   170
      INTEGER, PARAMETER :: ID_RH =         171
      INTEGER, PARAMETER :: ID_TEMP =       172
      INTEGER, PARAMETER :: ID_SRFTEMP =    173
      INTEGER, PARAMETER :: ID_PRES =       174
      INTEGER, PARAMETER :: ID_GN2O5F =     175
      INTEGER, PARAMETER :: ID_GN2O5C =     176
      INTEGER, PARAMETER :: ID_YCLNO2F =    177
      INTEGER, PARAMETER :: ID_YCLNO2C =    178
      INTEGER, PARAMETER :: ID_GIEPOX =     179
      INTEGER, PARAMETER :: ID_KIEPOX =     180
      INTEGER, PARAMETER :: ID_GIMAE =      181
      INTEGER, PARAMETER :: ID_DZ    =      182
      INTEGER, PARAMETER :: ID_ZH    =      183
      INTEGER, PARAMETER :: ID_CFRAC =      184
      INTEGER, PARAMETER :: ID_PV    =      185
      INTEGER, PARAMETER :: ID_DENS  =      186
      INTEGER, PARAMETER :: ID_RHOJ  =      187
      INTEGER, PARAMETER :: ID_GAS_HNO3 =   188
      INTEGER, PARAMETER :: ID_GAS_NH3  =   189

      TYPE ELDTP
        CHARACTER (LEN = 16) :: NAME      !Parameter Name
        INTEGER              :: ID        !ID Number in Case Statement
        INTEGER              :: VAR_TYPE  !IOAPI Variable Type
        LOGICAL              :: MODE      !Is this property mode-dependent?
        CHARACTER (LEN = 16) :: UNIT      !Units for Parameter
        CHARACTER (LEN = 80) :: DESC      !Description
      END TYPE ELDTP

      INTEGER, PARAMETER :: N_ELMO_LIST = 189

      TYPE (ELDTP), PARAMETER :: ELMO_LIST( N_ELMO_LIST ) = (/
     &ELDTP( 'STDEV        ',ID_STDEV,      M3REAL, T, '      ', 'Mode standard deviation (dry and wet)' ),
     &ELDTP( 'DRY_DG       ',ID_DRY_DG,     M3REAL, T, 'um    ', 'Mode mean diameter (dry)' ),
     &ELDTP( 'WET_DG       ',ID_WET_DG,     M3REAL, T, 'um    ', 'Mode mean diameter (wet)' ),
     &ELDTP( 'WET_M2       ',ID_WET_M2,     M3REAL, T, 'm2 m-3', 'Mode 2nd moment (wet)' ),
     &ELDTP( 'DRY_M3       ',ID_DRY_M3,     M3REAL, T, 'm3 m-3', 'Mode 3rd moment (dry)' ),
     &ELDTP( 'WET_M3       ',ID_WET_M3,     M3REAL, T, 'm3 m-3', 'Mode 3rd moment (wet)' ),
     &ELDTP( 'PM_NUM       ',ID_PM_NUM,     M3REAL, F, 'N m-3' , 'Total Particle Number' ),
     &ELDTP( 'PMU_NUM      ',ID_PMU_NUM,    M3REAL, F, 'N m-3' , 'Ultrafine-Mode Particle Number' ),
     &ELDTP( 'PMF_NUM      ',ID_PMF_NUM,    M3REAL, F, 'N m-3' , 'Fine-Mode Particle Number' ),
     &ELDTP( 'PMC_NUM      ',ID_PMC_NUM,    M3REAL, F, 'N m-3' , 'Coarse-Mode Particle Number' ),
     &ELDTP( 'N10          ',ID_N10,        M3REAL, F, 'N m-3' , 'Total Particle Number greater than 10 nm' ),
     &ELDTP( 'N20          ',ID_N20,        M3REAL, F, 'N m-3' , 'Total Particle Number greater than 20 nm' ),
     &ELDTP( 'N40          ',ID_N40,        M3REAL, F, 'N m-3' , 'Total Particle Number greater than 40 nm' ),
     &ELDTP( 'N100         ',ID_N100,       M3REAL, F, 'N m-3' , 'Total Particle Number greater than 100 nm' ),
     &ELDTP( 'PM_SRF       ',ID_PM_SRF,     M3REAL, F, 'm2 m-3', 'Particle Surface Area' ),
     &ELDTP( 'PMU_SRF      ',ID_PMU_SRF,    M3REAL, F, 'm2 m-3', 'Ultrafine-Mode Surface Area' ),
     &ELDTP( 'PMF_SRF      ',ID_PMF_SRF,    M3REAL, F, 'm2 m-3', 'Fine-Mode Surface Area' ),
     &ELDTP( 'PMC_SRF      ',ID_PMC_SRF,    M3REAL, F, 'm2 m-3', 'Coarse-Mode Surface Area' ),
     &ELDTP( 'PM_MASS      ',ID_PM_MASS,    M3REAL, F, 'ug m-3', 'Total Particle Mass' ),
     &ELDTP( 'PMU_MASS     ',ID_PMU_MASS,   M3REAL, F, 'ug m-3', 'Ultrafine Particle Mass' ),
     &ELDTP( 'PMF_MASS     ',ID_PMF_MASS,   M3REAL, F, 'ug m-3', 'Fine Particle Mass' ),
     &ELDTP( 'PMC_MASS     ',ID_PMC_MASS,   M3REAL, F, 'ug m-3', 'Coarse Particle Mass' ),
     &ELDTP( 'PMNUC_MASS   ',ID_PMNUC_MASS, M3REAL, F, 'ug m-3', 'Nucleation Mode Total PM' ),
     &ELDTP( 'PMAIT_MASS   ',ID_PMAIT_MASS, M3REAL, F, 'ug m-3', 'Aitken Mode Total PM' ),
     &ELDTP( 'PMACC_MASS   ',ID_PMACC_MASS, M3REAL, F, 'ug m-3', 'Accumulation Mode Total PM' ),
     &ELDTP( 'DRY_DENS     ',ID_DRY_DENS,   M3REAL, T, 'kg m-3', 'Bulk Density of Particles excluding particle water' ),
     &ELDTP( 'WET_DENS     ',ID_WET_DENS,   M3REAL, T, 'kg m-3', 'Bulk Density of Particles including particle water' ),
     &ELDTP( 'FPM01        ',ID_FPM01,      M3REAL, T, '1     ', 'PM01 fraction of mode' ),
     &ELDTP( 'FPM1         ',ID_FPM1,       M3REAL, T, '1     ', 'PM1 fraction of mode' ),
     &ELDTP( 'FPM25        ',ID_FPM25,      M3REAL, T, '1     ', 'PM2.5 fraction of mode' ),
     &ELDTP( 'FPM10        ',ID_FPM10,      M3REAL, T, '1     ', 'PM10 fraction of mode' ),
     &ELDTP( 'FPM25TO10    ',ID_FPM25to10,  M3REAL, T, '1     ', 'PM2.5-10 fraction of mode' ),
     &ELDTP( 'FAMS         ',ID_FAMS,       M3REAL, T, '1     ', 'AMS Transmission Fraction of mode' ),
     &ELDTP( 'PM01         ',ID_PM01,       M3REAL, F, 'ug m-3', 'Bulk PM0.1 Concentration' ), 
     &ELDTP( 'PM1          ',ID_PM1,        M3REAL, F, 'ug m-3', 'Bulk PM1.0 Concentration' ), 
     &ELDTP( 'PM25         ',ID_PM25,       M3REAL, F, 'ug m-3', 'Bulk PM2.5 Concentration' ), 
     &ELDTP( 'PM10         ',ID_PM10,       M3REAL, F, 'ug m-3', 'Bulk PM10.0 Concentration' ),
     &ELDTP( 'PM25TO10     ',ID_PM25to10,   M3REAL, F, 'ug m-3', 'Coarse-Mode Total' ),
     &ELDTP( 'PMAMS        ',ID_PMAMS,      M3REAL, F, 'ug m-3', 'Bulk PM Concentration in AMS' ),
                                            
      ! Coarse-Mode PM Species                             
     &ELDTP( 'PMC_SO4      ',ID_PMC_SO4,    M3REAL, F, 'ug m-3', 'Coarse-Mode Sulfate' ),
     &ELDTP( 'PMC_NO3      ',ID_PMC_NO3,    M3REAL, F, 'ug m-3', 'Coarse-Mode Nitrate' ),
     &ELDTP( 'PMC_NH4      ',ID_PMC_NH4,    M3REAL, F, 'ug m-3', 'Coarse-Mode Ammonium' ),
     &ELDTP( 'PMC_CL       ',ID_PMC_CL,     M3REAL, F, 'ug m-3', 'Coarse-Mode Chloride' ),
     &ELDTP( 'PMC_NA       ',ID_PMC_NA,     M3REAL, F, 'ug m-3', 'Recon Crs Sodium (.8373*ASEACAT+.0626*ASOIL+.0023*ACORS)' ),
     &ELDTP( 'PMC_MG       ',ID_PMC_MG,     M3REAL, F, 'ug m-3', 'Recon Crs Magnesium (.0997*ASEACAT+.0170*ASOIL+.0032*ACORS)'),
     &ELDTP( 'PMC_K        ',ID_PMC_K,      M3REAL, F, 'ug m-3', 'Recon Crs Potasium (.0310*ASEACAT+.0242*ASOIL+.0176*ACORS)' ),
     &ELDTP( 'PMC_CA       ',ID_PMC_CA,     M3REAL, F, 'ug m-3', 'Recon Crs Calcium (.0320*ASEACAT+.0838*ASOIL+.0562*ACORS)' ),
                                            
      ! Fine-Mode PM Species                               
     &ELDTP( 'PMF_SO4      ',ID_PMF_SO4,    M3REAL, F, 'ug m-3', 'Fine-Mode Sulfate' ),
     &ELDTP( 'PMF_NO3      ',ID_PMF_NO3,    M3REAL, F, 'ug m-3', 'Fine-Mode Nitrate' ),
     &ELDTP( 'PMF_NH4      ',ID_PMF_NH4,    M3REAL, F, 'ug m-3', 'Fine-Mode Ammonium' ),
     &ELDTP( 'PMF_CL       ',ID_PMF_CL,     M3REAL, F, 'ug m-3', 'Fine-Mode Chloride' ),
     &ELDTP( 'PMF_NA       ',ID_PMF_NA,     M3REAL, F, 'ug m-3', 'Fine-Mode Sodium' ),
     &ELDTP( 'PMF_EC       ',ID_PMF_EC,     M3REAL, F, 'ug m-3', 'Fine-Mode Black Carbon' ),
     &ELDTP( 'PMF_OC       ',ID_PMF_OC,     M3REAL, F, 'ugC m-3','Fine-Mode Organic Carbon' ),
     &ELDTP( 'PMF_OA       ',ID_PMF_OA,     M3REAL, F, 'ug m-3', 'Fine-Mode Organic Aerosol' ),
     &ELDTP( 'PMF_FE       ',ID_PMF_FE,     M3REAL, F, 'ug m-3', 'Fine-Mode Iron' ),
     &ELDTP( 'PMF_AL       ',ID_PMF_AL,     M3REAL, F, 'ug m-3', 'Fine-Mode Aluminum' ),
     &ELDTP( 'PMF_SI       ',ID_PMF_SI,     M3REAL, F, 'ug m-3', 'Fine-Mode Silicon' ),
     &ELDTP( 'PMF_TI       ',ID_PMF_TI,     M3REAL, F, 'ug m-3', 'Fine-Mode Titanium' ),
     &ELDTP( 'PMF_CA       ',ID_PMF_CA,     M3REAL, F, 'ug m-3', 'Fine-Mode Calcium' ),
     &ELDTP( 'PMF_MG       ',ID_PMF_MG,     M3REAL, F, 'ug m-3', 'Fine-Mode Magnesium' ),
     &ELDTP( 'PMF_K        ',ID_PMF_K,      M3REAL, F, 'ug m-3', 'Fine-Mode Potassium' ),
     &ELDTP( 'PMF_MN       ',ID_PMF_MN,     M3REAL, F, 'ug m-3', 'Fine-Mode Manganese' ),
     &ELDTP( 'PMF_H2O      ',ID_PMF_H2O,    M3REAL, F, 'ug m-3', 'Fine-Mode Particle Water' ),
     &ELDTP( 'PMF_SOILIMPV ',ID_SOIL_IMPV,  M3REAL, F, 'ug m-3', 'Recon of Soil PM from correlations at IMPROVE sites' ),
     &ELDTP( 'PMF_UN_IMPV1 ',ID_PMF_UNSP1,  M3REAL, F, 'ug m-3', 'Unspeciated PM including non-carbon organic mass' ),
     &ELDTP( 'PMF_UN_IMPV2 ',ID_PMF_UNSP2,  M3REAL, F, 'ug m-3', 'Unspeciated PM excluding non-carbon organic mass' ),
     &ELDTP( 'PMF_HP       ',ID_PMF_HP,     M3REAL, F, 'umol m-3','Fine-Mode H+ Ion' ),
     &ELDTP( 'PMF_HPMOLAL  ',ID_PMF_HPM,    M3REAL, F, 'mol L-1','Fine-Mode Concentration of H+ in Particle Water' ),
     &ELDTP( 'PMF_PH       ',ID_PMF_PH,     M3REAL, F, '1'     , 'Fine-Mode pH' ),
                                            
      ! Organic Aerosol Relevant Aggregates                
     &ELDTP( 'PMF_POC      ',ID_PMF_POC,    M3REAL, F, 'ugC m-3','Fine-Mode Primary Organic Carbon' ),
     &ELDTP( 'PMF_SOC      ',ID_PMF_SOC,    M3REAL, F, 'ugC m-3','Fine-Mode Secondary Organic Carbon' ),
     &ELDTP( 'PMF_POA      ',ID_PMF_POA,    M3REAL, F, 'ug m-3', 'Fine-Mode Primary Organic Aerosol' ),
     &ELDTP( 'PMF_SOA      ',ID_PMF_SOA,    M3REAL, F, 'ug m-3', 'Fine-Mode Secondary Organic Aerosol' ),
     &ELDTP( 'PMF_NCOM     ',ID_PMF_NCOM,   M3REAL, F, 'ug m-3', 'Fine-Mode Non-Carbon Organic Matter in POA' ),
     &ELDTP( 'PMF_OMOC     ',ID_PMF_OMOC,   M3REAL, F, '1     ','Fine-Mode OM/OC' ),
     &ELDTP( 'PMF_OTOC     ',ID_PMF_OtoC,   M3REAL, F, '1     ','Fine-Mode O:C' ),
     &ELDTP( 'PMF_ASOA     ',ID_PMF_ASOA,   M3REAL, F, 'ug m-3', 'Fine-Mode Anthropogenic-VOC Derived OA' ),
     &ELDTP( 'PMF_BSOA     ',ID_PMF_BSOA,   M3REAL, F, 'ug m-3', 'Fine-Mode Biogenic-VOC Derived OA' ),
     &ELDTP( 'PMF_CLDGLY   ',ID_PMF_CLDGLY, M3REAL, F, 'ug m-3', 'Fine-Mode SOA from glyoxal and methylglyoxal' ),
     &ELDTP( 'PMF_ISOPSOA  ',ID_PMF_ISOP,   M3REAL, F, 'ug m-3', 'Fine-Mode Isoprene SOA excl IEPOX SOA' ),
     &ELDTP( 'PMF_IEPOXSOA ',ID_PMF_IEPOX,  M3REAL, F, 'ug m-3', 'Fine-Mode IEPOX SOA' ),
     &ELDTP( 'PMF_MTNSOA   ',ID_PMF_MTNSOA, M3REAL, F, 'ug m-3', 'Fine-Mode Monoterpene Nitrate SOA' ),
     &ELDTP( 'PMF_MTSOA    ',ID_PMF_MTSOA,  M3REAL, F, 'ug m-3', 'Fine-Mode Monoterpene SOA excl Nitrates' ),
                                            
      ! AMS Speciation                      
     &ELDTP( 'PMAMS_SO4    ',ID_AMS_SO4,    M3REAL, F, 'ug m-3', 'Sulfate Collected by the AMS' ),
     &ELDTP( 'PMAMS_NO3    ',ID_AMS_NO3,    M3REAL, F, 'ug m-3', 'Nitrate Collected by the AMS' ),
     &ELDTP( 'PMAMS_NH4    ',ID_AMS_NH4,    M3REAL, F, 'ug m-3', 'Ammonium Collected by the AMS' ),
     &ELDTP( 'PMAMS_CL     ',ID_AMS_CL,     M3REAL, F, 'ug m-3', 'Chloride Collected by the AMS' ),
     &ELDTP( 'PMAMS_OA     ',ID_AMS_OA,     M3REAL, F, 'ug m-3', 'Organic Aerosol Collected by the AMS' ),
     &ELDTP( 'PMAMS_OTOC   ',ID_AMS_OTOC,   M3REAL, F, 'ug ug-1','O:C Collected and Measured by the AMS' ),
                                            
      ! PM1 Speciation                      
     &ELDTP( 'PM1_SO4      ',ID_PM1_SO4,    M3REAL, F, 'ug m-3', 'PM1 Sulfate' ),
     &ELDTP( 'PM1_NO3      ',ID_PM1_NO3,    M3REAL, F, 'ug m-3', 'PM1 Nitrate' ),
     &ELDTP( 'PM1_NH4      ',ID_PM1_NH4,    M3REAL, F, 'ug m-3', 'PM1 Ammonium' ),
     &ELDTP( 'PM1_CL       ',ID_PM1_CL,     M3REAL, F, 'ug m-3', 'PM1 Chloride' ),
     &ELDTP( 'PM1_NA       ',ID_PM1_NA,     M3REAL, F, 'ug m-3', 'PM1 Sodium' ),
     &ELDTP( 'PM1_EC       ',ID_PM1_EC,     M3REAL, F, 'ug m-3', 'PM1 Black Carbon' ),
     &ELDTP( 'PM1_OC       ',ID_PM1_OC,     M3REAL, F, 'ugC m-3','PM1 Organic Carbon' ),
     &ELDTP( 'PM1_OA       ',ID_PM1_OA,     M3REAL, F, 'ug m-3', 'PM1 Organic Aerosol' ),
     &ELDTP( 'PM1_MG       ',ID_PM1_MG,     M3REAL, F, 'ug m-3', 'PM1 Magnesium' ),
     &ELDTP( 'PM1_K        ',ID_PM1_K,      M3REAL, F, 'ug m-3', 'PM1 Potassium' ),
     &ELDTP( 'PM1_CA       ',ID_PM1_CA,     M3REAL, F, 'ug m-3', 'PM1 Calcium' ),
     &ELDTP( 'PM1_OTHER    ',ID_PM1_OT,     M3REAL, F, 'ug m-3', 'PM1 Other Mass' ),
     &ELDTP( 'PM1_FE       ',ID_PM1_FE,     M3REAL, F, 'ug m-3', 'PM1 Iron' ),
     &ELDTP( 'PM1_SI       ',ID_PM1_SI,     M3REAL, F, 'ug m-3', 'PM1 Silicon' ),
     &ELDTP( 'PM1_TI       ',ID_PM1_TI,     M3REAL, F, 'ug m-3', 'PM1 Titanium' ),
     &ELDTP( 'PM1_MN       ',ID_PM1_MN,     M3REAL, F, 'ug m-3', 'PM1 Manganese' ),
     &ELDTP( 'PM1_AL       ',ID_PM1_AL,     M3REAL, F, 'ug m-3', 'PM1 Aluminum' ),
     &ELDTP( 'PM1_SOIL     ',ID_PM1_SOIL,   M3REAL, F, 'ug m-3', 'PM1 Recon Soil (Fine) and Explicit (Coarse)' ),
     &ELDTP( 'PM1_UNSP1    ',ID_PM1_UNSP1,  M3REAL, F, 'ug m-3', 'PM1 Unsp. Coarse Mass for the IMPROVE Method' ),
     &ELDTP( 'PM1_UNSPCRS  ',ID_PM1_UN,     M3REAL, F, 'ug m-3', 'PM1 Unspeciated Coarse Mass' ),
     &ELDTP( 'PM1_HP       ',ID_PM1_HP,     M3REAL, F, 'ug m-3', 'PM1 H+ Ion' ),
                                            
      ! PM2.5 Speciation                    
     &ELDTP( 'PM25_SO4     ',ID_PM25_SO4,   M3REAL, F, 'ug m-3', 'PM2.5 Sulfate' ),
     &ELDTP( 'PM25_NO3     ',ID_PM25_NO3,   M3REAL, F, 'ug m-3', 'PM2.5 Nitrate' ),
     &ELDTP( 'PM25_NH4     ',ID_PM25_NH4,   M3REAL, F, 'ug m-3', 'PM2.5 Ammonium' ),
     &ELDTP( 'PM25_CL      ',ID_PM25_CL,    M3REAL, F, 'ug m-3', 'PM2.5 Chloride' ),
     &ELDTP( 'PM25_NA      ',ID_PM25_NA,    M3REAL, F, 'ug m-3', 'PM2.5 Sodium' ),
     &ELDTP( 'PM25_EC      ',ID_PM25_EC,    M3REAL, F, 'ug m-3', 'PM2.5 Black Carbon' ),
     &ELDTP( 'PM25_OC      ',ID_PM25_OC,    M3REAL, F, 'ugC m-3','PM2.5 Organic Carbon' ),
     &ELDTP( 'PM25_OA      ',ID_PM25_OA,    M3REAL, F, 'ug m-3', 'PM2.5 Organic Aerosol' ),
     &ELDTP( 'PM25_MG      ',ID_PM25_MG,    M3REAL, F, 'ug m-3', 'PM2.5 Magnesium' ),
     &ELDTP( 'PM25_K       ',ID_PM25_K,     M3REAL, F, 'ug m-3', 'PM2.5 Potassium' ),
     &ELDTP( 'PM25_CA      ',ID_PM25_CA,    M3REAL, F, 'ug m-3', 'PM2.5 Calcium' ),
     &ELDTP( 'PM25_OTHER   ',ID_PM25_OT,    M3REAL, F, 'ug m-3', 'PM2.5 Other Mass' ),
     &ELDTP( 'PM25_FE      ',ID_PM25_FE,    M3REAL, F, 'ug m-3', 'PM2.5 Iron' ),
     &ELDTP( 'PM25_SI      ',ID_PM25_SI,    M3REAL, F, 'ug m-3', 'PM2.5 Silicon' ),
     &ELDTP( 'PM25_TI      ',ID_PM25_TI,    M3REAL, F, 'ug m-3', 'PM2.5 Titanium' ),
     &ELDTP( 'PM25_MN      ',ID_PM25_MN,    M3REAL, F, 'ug m-3', 'PM2.5 Manganese' ),
     &ELDTP( 'PM25_AL      ',ID_PM25_AL,    M3REAL, F, 'ug m-3', 'PM2.5 Aluminum' ),
     &ELDTP( 'PM25_SOIL    ',ID_PM25_SOIL,  M3REAL, F, 'ug m-3', 'PM2.5 Recon Soil (Fine) and Explicit (Coarse)' ),
     &ELDTP( 'PM25_UNSP1   ',ID_PM25_UNSP1, M3REAL, F, 'ug m-3', 'PM2.5 Unsp. Coarse Mass for the IMPROVE method' ),
     &ELDTP( 'PM25_UNSPCRS ',ID_PM25_UN,    M3REAL, F, 'ug m-3', 'PM2.5 Unspeciated Coarse Mass' ),
     &ELDTP( 'PM25_HP      ',ID_PM25_HP,    M3REAL, F, 'ug m-3', 'PM2.5 H+ Ion' ),

      ! PM2.5-10.0 Speciation
     &ELDTP( 'PM25TO10_SO4 ',ID_PM25to10_SO4,M3REAL,F, 'ug m-3', 'Coarse-Mode Sulfate' ),
     &ELDTP( 'PM25TO10_NO3 ',ID_PM25to10_NO3,M3REAL,F, 'ug m-3', 'Coarse-Mode Nitrate' ),
     &ELDTP( 'PM25TO10_NH4 ',ID_PM25to10_NH4,M3REAL,F, 'ug m-3', 'Coarse-Mode Ammonium' ),
     &ELDTP( 'PM25TO10_CL  ',ID_PM25to10_CL, M3REAL,F, 'ug m-3', 'Coarse-Mode Chloride' ),
     &ELDTP( 'PM25TO10_NA  ',ID_PM25to10_NA, M3REAL,F, 'ug m-3', 'Coarse-Mode Sodium' ),

      ! PM Toxics that are part of the Multipollutant (mp) Mechanism
     &ELDTP( 'PM25_HDIESEL ',ID_PM25_HDSL,   M3REAL,F, 'ug m-3', 'PM2.5 Diesel Total from HAP inventory' ),
     &ELDTP( 'PM25_HBE     ',ID_PM25_HBE,    M3REAL,F, 'ug m-3', 'PM2.5 Beryllium from HAP inventory' ),
     &ELDTP( 'PM25_HCD     ',ID_PM25_HCD,    M3REAL,F, 'ug m-3', 'PM2.5 Cadmium from HAP inventory' ),
     &ELDTP( 'PM25_HCR3    ',ID_PM25_HCR3,   M3REAL,F, 'ug m-3', 'PM2.5 Chromium-III from HAP inventory' ),
     &ELDTP( 'PM25_HCR6    ',ID_PM25_HCR6,   M3REAL,F, 'ug m-3', 'PM2.5 Chromium-VI from HAP inventory' ),
     &ELDTP( 'PM25_HCR     ',ID_PM25_HCR,    M3REAL,F, 'ug m-3', 'PM2.5 Chromium Total from HAP inventory' ),
     &ELDTP( 'PM25_HPB     ',ID_PM25_HPB,    M3REAL,F, 'ug m-3', 'PM2.5 Lead from HAP inventory' ),
     &ELDTP( 'PM25_HMN     ',ID_PM25_HMN,    M3REAL,F, 'ug m-3', 'PM2.5 Manganese from HAP inventory' ),
     &ELDTP( 'PM25_HNI     ',ID_PM25_HNI,    M3REAL,F, 'ug m-3', 'PM2.5 Nickel from HAP inventory' ),
     &ELDTP( 'PM25_HAS     ',ID_PM25_HAS,    M3REAL,F, 'ug m-3', 'PM2.5 Arsenic from HAP inventory' ),
     &ELDTP( 'PM25_HG      ',ID_PM25_HG,     M3REAL,F, 'ug m-3', 'PM2.5 Mercury from HAP inventory' ),
     &ELDTP( 'PM10_HDIESEL ',ID_PM10_HDSL,   M3REAL,F, 'ug m-3', 'PM10.0 Diesel Total from HAP inventory' ),
     &ELDTP( 'PM10_HBE     ',ID_PM10_HBE,    M3REAL,F, 'ug m-3', 'PM10.0 Beryllium from HAP inventory' ),
     &ELDTP( 'PM10_HCD     ',ID_PM10_HCD,    M3REAL,F, 'ug m-3', 'PM10.0 Cadmium from HAP inventory' ),
     &ELDTP( 'PM10_HCR3    ',ID_PM10_HCR3,   M3REAL,F, 'ug m-3', 'PM10.0 Chromium-III from HAP inventory' ),
     &ELDTP( 'PM10_HCR6    ',ID_PM10_HCR6,   M3REAL,F, 'ug m-3', 'PM10.0 Chromium-VI from HAP inventory' ),
     &ELDTP( 'PM10_HCR     ',ID_PM10_HCR,    M3REAL,F, 'ug m-3', 'PM10.0 Chromium Total from HAP inventory' ),
     &ELDTP( 'PM10_HPB     ',ID_PM10_HPB,    M3REAL,F, 'ug m-3', 'PM10.0 Lead from HAP inventory' ),
     &ELDTP( 'PM10_HMN     ',ID_PM10_HMN,    M3REAL,F, 'ug m-3', 'PM10.0 Manganese from HAP inventory' ),
     &ELDTP( 'PM10_HNI     ',ID_PM10_HNI,    M3REAL,F, 'ug m-3', 'PM10.0 Nickel from HAP inventory' ),
     &ELDTP( 'PM10_HAS     ',ID_PM10_HAS,    M3REAL,F, 'ug m-3', 'PM10.0 Arsenic from HAP inventory' ),
     &ELDTP( 'PM10_HG      ',ID_PM10_HG,     M3REAL,F, 'ug m-3', 'PM10.0 Mercury from HAP inventory' ),
     &ELDTP( 'PM25_BENAPY  ',ID_PM25_BENAPY, M3REAL,F, 'ug m-3', 'PM2.5 Benzo-a-Pyrene' ),
     &ELDTP( 'PM10_BENAPY  ',ID_PM10_BENAPY, M3REAL,F, 'ug m-3', 'PM10.0 Benzo-a-Pyrene' ),
     &ELDTP( 'PMF_BENAPY   ',ID_PMF_BENAPY,  M3REAL,F, 'ug m-3', 'Fine-Mode Benzo-a-Pyrene' ),
     &ELDTP( 'GAS_BENAPY   ',ID_GAS_BENAPY,  M3REAL,F, 'ug m-3', 'Gas-Phase Benzo-a-Pyrene' ),
     &ELDTP( 'BENAPY_FAERO ',ID_BENAPY_FAERO,M3REAL,F, '',       'Benzo-a-Pyrene Aerosol-Phase Fraction' ),
     &ELDTP( 'GAS_HNO3     ',ID_GAS_HNO3,    M3REAL,F, 'ug m-3', 'Gas-Phase Nitric Acid' ),
     &ELDTP( 'GAS_NH3      ',ID_GAS_NH3,     M3REAL,F, 'ug m-3', 'Gas-Phase Ammonia' ),

      ! Other Special Variables and Properties
     &ELDTP( 'TNO3         ',ID_TNO3,       M3REAL, F, 'ug m-3', 'Total Nitrate only Including Inorganics' ),
     &ELDTP( 'TNO3TOT      ',ID_TNO3TOT,    M3REAL, F, 'ug m-3', 'Total Nitrate including Inorganics and Organics' ),
     &ELDTP( 'PM25_FRM     ',ID_PM25_FRM,   M3REAL, F, 'ug m-3', 'Federal Reference Method PM2.5' ),
     &ELDTP( 'PMF_FRM      ',ID_PMF_FRM,    M3REAL, F, 'ug m-3', 'Federal Reference Method PMIJ' ),
     &ELDTP( 'AOD_550      ',ID_AOD550,     M3REAL, F, '1     ', 'Aerosol Optical Depth at 550 nm -Angstrom interp' ),
     &ELDTP( 'PM_EXT_550   ',ID_PMEXT550,   M3REAL, F, 'Mm-1  ', 'Aerosol Extinction at 550 nm -Angstrom interp' ),
     &ELDTP( 'RH           ',ID_RH,         M3REAL, F, '1     ', 'Relative humidity' ),
     &ELDTP( 'TA           ',ID_TEMP,       M3REAL, F, 'K     ', 'Temperature' ),
     &ELDTP( 'TSURF        ',ID_SRFTEMP,    M3REAL, F, 'K     ', '2-meter Surface Temperature' ),
     &ELDTP( 'PRES         ',ID_PRES,       M3REAL, F, 'Pa    ', 'Pressure' ),
     &ELDTP( 'GAMMA_N2O5   ',ID_GN2O5F,     M3REAL, F, '1     ', 'Fine Mode N2O5 Heterogeneous rxn probability' ),
     &ELDTP( 'GAMMA_N2O5K  ',ID_GN2O5C,     M3REAL, F, '1     ', 'Coarse Mode N2O5 Heterogeneous rxn probability' ),
     &ELDTP( 'YIELD_CLNO2  ',ID_YCLNO2F,    M3REAL, F, '1     ', 'Fine Mode CLNO2 Heterogeneous reaction yield' ),
     &ELDTP( 'YIELD_CLNO2K ',ID_YCLNO2C,    M3REAL, F, '1     ', 'Coarse Mode CLNO2 Heterogeneous reaction yield' ),
     &ELDTP( 'GAMMA_IEPOX  ',ID_GIEPOX,     M3REAL, F, '1     ', 'IEPOX heterogeneous uptake coefficient' ),
     &ELDTP( 'K_IEPOX      ',ID_KIEPOX,     M3REAL, F, 's-1   ', 'IEPOX 1st order particle phase reaction rate const' ),
     &ELDTP( 'GAMMA_IMAE   ',ID_GIMAE,      M3REAL, F, '1     ', 'IMAE+HMML heterogeneous uptake coefficient' ),
     &ELDTP( 'DZ           ',ID_DZ,         M3REAL, F, 'm     ', 'Height of each grid cell computed from top layer height' ),
     &ELDTP( 'ZH           ',ID_ZH,         M3REAL, F, 'm     ', 'Height of grid cell midpoints (mass-weighted)' ),
     &ELDTP( 'CFRAC        ',ID_CFRAC,      M3REAL, F, '      ', 'Cloud Fraction' ),
     &ELDTP( 'PV           ',ID_PV,         M3REAL, F, '      ', 'Potential Vorticity' ),
     &ELDTP( 'DENS         ',ID_DENS,       M3REAL, F, 'kg m-3', 'Advected Density' ),
     &ELDTP( 'RHOJ         ',ID_RHOJ,       M3REAL, F, 'kg m-3', 'Advected Density x Jacobian/MSFX^2' ) /)
     

        TYPE ELMO_KEY_TYPE
            CHARACTER( 24 )             :: WORD    ! Keyword for selecting mulitple ELMO parameters
            INTEGER                     :: N       ! Number of Parameters associated with the Keyword
            CHARACTER( 16 ),ALLOCATABLE :: LIST(:) ! List of parameters
        END TYPE ELMO_KEY_TYPE

        TYPE (ELMO_KEY_TYPE), ALLOCATABLE, SAVE :: ELMO_KEY( : )
        INTEGER, SAVE :: N_KEY

        INTEGER, SAVE :: MAP_ID2LIST( N_ELMO_LIST ) 

        ! Variables for Storing Values of ELMO Output      
        REAL, ALLOCATABLE, SAVE :: ELMO_INST( :,:,:,: )
        REAL, ALLOCATABLE, SAVE :: ELMO_AVRG( :,:,:,: )
        REAL, ALLOCATABLE, SAVE :: ELMO_LOCAL( : )
        LOGICAL, ALLOCATABLE, SAVE :: L_CALC( : )
        REAL, ALLOCATABLE, SAVE :: CONC_VEC( : )
        INTEGER, SAVE           :: C1, R1, L1

        ! Variables for Aerosol Chemistry
        REAL, SAVE, ALLOCATABLE :: GAMMA_N2O5IJ( :,:,:  ) ! Fine modes N2O5 rx effic.
        REAL, SAVE, ALLOCATABLE :: GAMMA_N2O5K(  :,:,:  ) ! Coarse mode N2O5 rx effic.
        REAL, SAVE, ALLOCATABLE :: KPARTIEPOX (  :,:,:  ) ! IEPOX particle-phase reaction rate constant (sec-1)
        REAL, SAVE, ALLOCATABLE :: GAMMA_IEPOX(  :,:,:  ) ! IEPOX uptake coeff
        REAL, SAVE, ALLOCATABLE :: GAMMA_IMAE (  :,:,:  ) ! IMAE uptake coeff
        REAL, SAVE, ALLOCATABLE :: YCLNO2IJ   (  :,:,:  ) ! Yield CLNO2 in Fine modes 
        REAL, SAVE, ALLOCATABLE :: YCLNO2K    (  :,:,:  ) ! Yield CLNO2 in coarse mode 
  
        LOGICAL, SAVE           :: AERO_CHEM_SET = .FALSE.
 
        ! Define type for storing coeffieicnets and variable indices for
        ! aggregating PM diagnostics like ATOTIJ, ACLK, etc.
        TYPE ELMO_COEFFS_TYPE
            LOGICAL L_MAPPED                 ! Record whether or not this aggregate has been mapped
            INTEGER N_VARS                   ! Number of variables to store coefficients for. 
                                             !   This should not account for modes. For example, 
                                             !   to sum ASO4IJK, N_VARS would be 1 since only 
                                             !   ASO4 is being used.
            LOGICAL, ALLOCATABLE :: L_GAS(:)    ! Is this variable a gas?
            LOGICAL, ALLOCATABLE :: L_SPC(:)    ! Is this variable a CMAQ species?
            LOGICAL, ALLOCATABLE :: L_AGG(:)    ! Is this variable another aggregated diagnostic parameter?
            LOGICAL, ALLOCATABLE :: L_MODE(:)   ! For which modes does this aggregate apply
            INTEGER, ALLOCATABLE :: I_SPEC(:)   ! Index of this Variable in Aerosol/Gas/Aggregate Array
            REAL,    ALLOCATABLE :: COEFF(:)       ! Coefficient for each variable
            INTEGER INLET                    ! Integer pointing to the corresponding inlet of choice 
                                             !   in the ELMO_INLET structure
            INTEGER WETNESS                  ! Integer prescribing wet or dry aerosol (1 = wet, 2 = dry)
                                             !   Default is wet
        END TYPE ELMO_COEFFS_TYPE

        TYPE (ELMO_COEFFS_TYPE), ALLOCATABLE, SAVE :: ELMO_COEFFS( : ) 


        TYPE ELMO_INLET_TYPE
            CHARACTER( 16 ) CINLET            ! Inlet Name
            REAL            DIAM_LO           ! Lower Diameter of this Inlet [um]
            REAL            DIAM_HI           ! Upper Diameter of this inlet [um]
        END TYPE ELMO_INLET_TYPE

        INTEGER, PARAMETER :: N_INLET = 10
        INTEGER, PARAMETER :: IPM01 = 1, IPM1 = 2, IPM25 = 3, IPM10 = 4, 
     &                        IPM25to10 = 5, IPMAMS = 6, INUM10 = 7,
     &                        INUM20 = 8, INUM40 = 9, INUM100 = 10
        INTEGER, PARAMETER :: IWET = 1, IDRY = 2

        TYPE (ELMO_INLET_TYPE), PARAMETER :: ELMO_INLET( N_INLET ) = (/
       !                          Inlet Name D_Lo  D_Hi
       !                          ---------- ----  ----
     &        ELMO_INLET_TYPE( 'PM01    ', 0.0,  0.1 ),
     &        ELMO_INLET_TYPE( 'PM1     ', 0.0,  1.0 ),
     &        ELMO_INLET_TYPE( 'PM25    ', 0.0,  2.5 ),
     &        ELMO_INLET_TYPE( 'PM10    ', 0.0, 10.0 ),
     &        ELMO_INLET_TYPE( 'PM25TO10', 2.5, 10.0 ),
     &        ELMO_INLET_TYPE( 'PMAMS   ', 0.0,  0.0 ),
     &        ELMO_INLET_TYPE( 'INUM10  ', 0.01,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM20  ', 0.02,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM40  ', 0.04,1.0e3),
     &        ELMO_INLET_TYPE( 'INUM100 ', 0.1, 1.0e3) /)

        REAL, SAVE :: INLET_FRAC( N_INLET,N_MODE,2 )  ! Fraction of each aerosol mode 
                                                      !   collected by each inlet
                                                      !   Third dimension is 1
                                                      !     for wet and 2 for dry

        REAL, ALLOCATABLE, SAVE :: ELMO_AOD_550(:,:), ! total aerosol optical depth at 
                                                      !   550 nm (Angstrom Interpolation) 
     &                             ELMO_EXT_550(:,:,:)! aerosol extinction at 550 nm for layer [1/m]

        CONTAINS

!------------------------------------------------------------------------------------------            
        subroutine elmo_init_shared
!------------------------------------------------------------------------------------------            
        use grid_conf, only : ncols, nrows, nlays

        implicit none

        integer ios
        character(32) :: pname = 'ELMO_INIT_SHARED'

        allocate( elmo_aod_550(ncols, nrows ), stat=ios)
        call checkmem( ios, 'ELMO_AOD_550',pname )
        elmo_aod_550 = 0.0

        allocate( elmo_ext_550(ncols, nrows, nlays ), stat=ios)
        call checkmem( ios, 'ELMO_EXT_550',pname )
        elmo_ext_550 = 0.0

        end subroutine elmo_init_shared
!------------------------------------------------------------------------------------------            

      END MODULE ELMO_DATA
