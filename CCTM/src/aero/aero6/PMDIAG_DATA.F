
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
      MODULE PMDIAG_DATA

c-----------------------------------------------------------------------
c Revision History:
c David Wong 11 May 16: created
c-----------------------------------------------------------------------

        USE AERO_DATA, ONLY : N_MODE

        IMPLICIT NONE
         
        ! Define Logical values as T and F for the aerospc table
        Logical, Parameter, Private :: T = .true.
        Logical, Parameter, Private :: F = .false.

        LOGICAL, SAVE :: L_PMDIAG
        INTEGER, SAVE :: PMDIAG_AVRG_STEP = 0
        INTEGER, SAVE :: NLAY_PMDIAG_INST
        INTEGER, SAVE :: NLAY_PMDIAG_AVRG

        ! Define Indices for All PMDIAG Parameters
        ! The idea here is that parameters will be organized and
        ! calculated in the order of this list, not the PMDIAG_LIST. The
        ! PMDIAG_LIST is searchable via the Parameter names but they
        ! link to this list of integers so that each parameter can be
        ! easily referenced during the run without having to do string
        ! lookups.
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
        INTEGER, PARAMETER :: ID_TNO3 =       160
        INTEGER, PARAMETER :: ID_TNO3TOT =    161
        INTEGER, PARAMETER :: ID_PM25_FRM =   162
        INTEGER, PARAMETER :: ID_PMF_FRM =    163
        INTEGER, PARAMETER :: ID_AOD550 =     164
        INTEGER, PARAMETER :: ID_PMEXT550 =   165
        INTEGER, PARAMETER :: ID_RH =         166
        INTEGER, PARAMETER :: ID_TEMP =       167
        INTEGER, PARAMETER :: ID_PRES =       168
        INTEGER, PARAMETER :: ID_GN2O5F =     169
        INTEGER, PARAMETER :: ID_GN2O5C =     170
        INTEGER, PARAMETER :: ID_YCLNO2F =    171
        INTEGER, PARAMETER :: ID_YCLNO2C =    172
        INTEGER, PARAMETER :: ID_GIEPOX =     173
        INTEGER, PARAMETER :: ID_KIEPOX =     174
        INTEGER, PARAMETER :: ID_GIMAE =      175

        TYPE PMDTP
          CHARACTER (LEN = 16) :: NAME      !Parameter Name
          INTEGER              :: ID        !ID Number in Case Statement
          INTEGER              :: VAR_TYPE  !IOAPI Variable Type
          LOGICAL              :: MODE      !Is this property mode-dependent?
          CHARACTER (LEN = 16) :: UNIT      !Units for Parameter
          CHARACTER (LEN = 80) :: DESC      !Description
        END TYPE PMDTP

        INTEGER, PARAMETER :: N_PMDIAG_LIST = 175

        TYPE (PMDTP), PARAMETER :: PMDIAG_LIST( N_PMDIAG_LIST ) = (/
     &PMDTP( 'STDEV        ',ID_STDEV,      5, T, '      ', 'Mode standard deviation (dry and wet)' ),
     &PMDTP( 'DRY_DG       ',ID_DRY_DG,     5, T, 'um    ', 'Mode mean diameter (dry)' ),
     &PMDTP( 'WET_DG       ',ID_WET_DG,     5, T, 'um    ', 'Mode mean diameter (wet)' ),
     &PMDTP( 'WET_M2       ',ID_WET_M2,     5, T, 'm2 m-3', 'Mode 2nd moment (wet)' ),
     &PMDTP( 'DRY_M3       ',ID_DRY_M3,     5, T, 'm3 m-3', 'Mode 3rd moment (dry)' ),
     &PMDTP( 'WET_M3       ',ID_WET_M3,     5, T, 'm3 m-3', 'Mode 3rd moment (wet)' ),
     &PMDTP( 'PM_NUM       ',ID_PM_NUM,     5, F, 'N m-3' , 'Total Particle Number' ),
     &PMDTP( 'PMU_NUM      ',ID_PMU_NUM,    5, F, 'N m-3' , 'Ultrafine-Mode Particle Number' ),
     &PMDTP( 'PMF_NUM      ',ID_PMF_NUM,    5, F, 'N m-3' , 'Fine-Mode Particle Number' ),
     &PMDTP( 'PMC_NUM      ',ID_PMC_NUM,    5, F, 'N m-3' , 'Coarse-Mode Particle Number' ),
     &PMDTP( 'N10          ',ID_N10,        5, F, 'N m-3' , 'Total Particle Number' ),
     &PMDTP( 'N20          ',ID_N20,        5, F, 'N m-3' , 'Total Particle Number' ),
     &PMDTP( 'N40          ',ID_N40,        5, F, 'N m-3' , 'Total Particle Number' ),
     &PMDTP( 'N100         ',ID_N100,       5, F, 'N m-3' , 'Total Particle Number' ),
     &PMDTP( 'PM_SRF       ',ID_PM_SRF,     5, F, 'm2 m-3', 'Particle Surface Area' ),
     &PMDTP( 'PMU_SRF      ',ID_PMU_SRF,    5, F, 'm2 m-3', 'Ultrafine-Mode Surface Area' ),
     &PMDTP( 'PMF_SRF      ',ID_PMF_SRF,    5, F, 'm2 m-3', 'Fine-Mode Surface Area' ),
     &PMDTP( 'PMC_SRF      ',ID_PMC_SRF,    5, F, 'm2 m-3', 'Coarse-Mode Surface Area' ),
     &PMDTP( 'PM_MASS      ',ID_PM_MASS,    5, F, 'ug m-3', 'Total Particle Mass' ),
     &PMDTP( 'PMU_MASS     ',ID_PMU_MASS,   5, F, 'ug m-3', 'Ultrafine Particle Mass' ),
     &PMDTP( 'PMF_MASS     ',ID_PMF_MASS,   5, F, 'ug m-3', 'Fine Particle Mass' ),
     &PMDTP( 'PMC_MASS     ',ID_PMC_MASS,   5, F, 'ug m-3', 'Coarse Particle Mass' ),
     &PMDTP( 'PMNUC_MASS   ',ID_PMNUC_MASS, 5, F, 'ug m-3', 'Nucleation Mode Total PM' ),
     &PMDTP( 'PMAIT_MASS   ',ID_PMAIT_MASS, 5, F, 'ug m-3', 'Aitken Mode Total PM' ),
     &PMDTP( 'PMACC_MASS   ',ID_PMACC_MASS, 5, F, 'ug m-3', 'Accumulation Mode Total PM' ),
     &PMDTP( 'DRY_DENS     ',ID_DRY_DENS,   5, T, 'kg m-3', 'Bulk Density of Particles excluding particle water' ),
     &PMDTP( 'WET_DENS     ',ID_WET_DENS,   5, T, 'kg m-3', 'Bulk Density of Particles including particle water' ),
     &PMDTP( 'FPM01        ',ID_FPM01,      5, T, '1     ', 'PM01 fraction of mode' ),
     &PMDTP( 'FPM1         ',ID_FPM1,       5, T, '1     ', 'PM1 fraction of mode' ),
     &PMDTP( 'FPM25        ',ID_FPM25,      5, T, '1     ', 'PM2.5 fraction of mode' ),
     &PMDTP( 'FPM10        ',ID_FPM10,      5, T, '1     ', 'PM10 fraction of mode' ),
     &PMDTP( 'FPM25TO10    ',ID_FPM25to10,  5, T, '1     ', 'PM2.5-10 fraction of mode' ),
     &PMDTP( 'FAMS         ',ID_FAMS,       5, T, '1     ', 'AMS Transmission Fraction of mode' ),
     &PMDTP( 'PM01         ',ID_PM01,       5, F, 'ug m-3', 'Bulk PM0.1 Concentration' ), 
     &PMDTP( 'PM1          ',ID_PM1,        5, F, 'ug m-3', 'Bulk PM1.0 Concentration' ), 
     &PMDTP( 'PM25         ',ID_PM25,       5, F, 'ug m-3', 'Bulk PM2.5 Concentration' ), 
     &PMDTP( 'PM10         ',ID_PM10,       5, F, 'ug m-3', 'Bulk PM10.0 Concentration' ),
     &PMDTP( 'PM25TO10     ',ID_PM25to10,   5, F, 'ug m-3', 'Coarse-Mode Total' ),
     &PMDTP( 'PMAMS        ',ID_PMAMS,      5, F, 'ug m-3', 'Bulk PM Concentration in AMS' ),
                                                            
      ! Coarse-Mode PM Species                              
     &PMDTP( 'PMC_SO4      ',ID_PMC_SO4,    5, F, 'ug m-3', 'Coarse-Mode Sulfate' ),
     &PMDTP( 'PMC_NO3      ',ID_PMC_NO3,    5, F, 'ug m-3', 'Coarse-Mode Nitrate' ),
     &PMDTP( 'PMC_NH4      ',ID_PMC_NH4,    5, F, 'ug m-3', 'Coarse-Mode Ammonium' ),
     &PMDTP( 'PMC_NH4      ',ID_PMC_CL,     5, F, 'ug m-3', 'Coarse-Mode Chloride' ),
     &PMDTP( 'PMC_NA       ',ID_PMC_NA,     5, F, 'ug m-3', 'Recon Coarse-Mode Sodium (.8373*ASEACAT+.0626*ASOIL+.0023*ACORS)' ),
     &PMDTP( 'PMC_MG       ',ID_PMC_MG,     5, F, 'ug m-3', 'Recon Coarse-Mode Magnesium (.0997*ASEACAT+.0170*ASOIL+.0032*ACORS)' ),
     &PMDTP( 'PMC_K        ',ID_PMC_K,      5, F, 'ug m-3', 'Recon Coarse-Mode Potasium (.0310*ASEACAT+.0242*ASOIL+.0176*ACORS)' ),
     &PMDTP( 'PMC_CA       ',ID_PMC_CA,     5, F, 'ug m-3', 'Recon Coarse-Mode Calcium (.0320*ASEACAT+.0838*ASOIL+.0562*ACORS)' ),
                                                            
      ! Fine-Mode PM Species                                
     &PMDTP( 'PMF_SO4      ',ID_PMF_SO4,    5, F, 'ug m-3', 'Fine-Mode Sulfate' ),
     &PMDTP( 'PMF_NO3      ',ID_PMF_NO3,    5, F, 'ug m-3', 'Fine-Mode Nitrate' ),
     &PMDTP( 'PMF_NH4      ',ID_PMF_NH4,    5, F, 'ug m-3', 'Fine-Mode Ammonium' ),
     &PMDTP( 'PMF_CL       ',ID_PMF_CL,     5, F, 'ug m-3', 'Fine-Mode Chloride' ),
     &PMDTP( 'PMF_NA       ',ID_PMF_NA,     5, F, 'ug m-3', 'Fine-Mode Sodium' ),
     &PMDTP( 'PMF_EC       ',ID_PMF_EC,     5, F, 'ug m-3', 'Fine-Mode Black Carbon' ),
     &PMDTP( 'PMF_OC       ',ID_PMF_OC,     5, F, 'ugC m-3','Fine-Mode Organic Carbon' ),
     &PMDTP( 'PMF_OA       ',ID_PMF_OA,     5, F, 'ug m-3', 'Fine-Mode Organic Aerosol' ),
     &PMDTP( 'PMF_FE       ',ID_PMF_FE,     5, F, 'ug m-3', 'Fine-Mode Iron' ),
     &PMDTP( 'PMF_AL       ',ID_PMF_AL,     5, F, 'ug m-3', 'Fine-Mode Aluminum' ),
     &PMDTP( 'PMF_SI       ',ID_PMF_SI,     5, F, 'ug m-3', 'Fine-Mode Silicon' ),
     &PMDTP( 'PMF_TI       ',ID_PMF_TI,     5, F, 'ug m-3', 'Fine-Mode Titanium' ),
     &PMDTP( 'PMF_CA       ',ID_PMF_CA,     5, F, 'ug m-3', 'Fine-Mode Calcium' ),
     &PMDTP( 'PMF_MG       ',ID_PMF_MG,     5, F, 'ug m-3', 'Fine-Mode Magnesium' ),
     &PMDTP( 'PMF_K        ',ID_PMF_K,      5, F, 'ug m-3', 'Fine-Mode Potassium' ),
     &PMDTP( 'PMF_MN       ',ID_PMF_MN,     5, F, 'ug m-3', 'Fine-Mode Manganese' ),
     &PMDTP( 'PMF_H2O      ',ID_PMF_H2O,    5, F, 'ug m-3', 'Fine-Mode Particle Water' ),
     &PMDTP( 'PMF_SOILIMPV ',ID_SOIL_IMPV,  5, F, 'ug m-3', 'Recon of Soil PM from correlations at IMPROVE sites' ),
     &PMDTP( 'PMF_UN_IMPV1 ',ID_PMF_UNSP1,  5, F, 'ug m-3', 'Unspeciated PM including non-carbon organic mass' ),
     &PMDTP( 'PMF_UN_IMPV2 ',ID_PMF_UNSP2,  5, F, 'ug m-3', 'Unspeciated PM excluding non-carbon organic mass' ),
     &PMDTP( 'PMF_HP       ',ID_PMF_HP,     5, F, 'umol m-3','Fine-Mode H+ Ion' ),
     &PMDTP( 'PMF_HPMOLAL  ',ID_PMF_HPM,    5, F, 'mol L-1','Fine-Mode Concentration of H+ in Particle Water' ),
     &PMDTP( 'PMF_PH       ',ID_PMF_PH,     5, F, '1'     , 'Fine-Mode pH' ),
                                                            
      ! Organic Aerosol Relevant Aggregates                 
     &PMDTP( 'PMF_POC      ',ID_PMF_POC,    5, F, 'ugC m-3','Fine-Mode Primary Organic Carbon' ),
     &PMDTP( 'PMF_SOC      ',ID_PMF_SOC,    5, F, 'ugC m-3','Fine-Mode Secondary Organic Carbon' ),
     &PMDTP( 'PMF_POA      ',ID_PMF_POA,    5, F, 'ug m-3', 'Fine-Mode Primary Organic Aerosol' ),
     &PMDTP( 'PMF_SOA      ',ID_PMF_SOA,    5, F, 'ug m-3', 'Fine-Mode Secondary Organic Aerosol' ),
     &PMDTP( 'PMF_NCOM     ',ID_PMF_NCOM,   5, F, 'ug m-3', 'Fine-Mode Non-Carbon Organic Matter' ),
     &PMDTP( 'PMF_OMOC     ',ID_PMF_OMOC,   5, F, '1     ','Fine-Mode OM/OC' ),
     &PMDTP( 'PMF_OTOC     ',ID_PMF_OtoC,   5, F, '1     ','Fine-Mode O:C' ),
     &PMDTP( 'PMF_ASOA     ',ID_PMF_ASOA,   5, F, 'ug m-3', 'Fine-Mode Anthropogenic-VOC Derived OA' ),
     &PMDTP( 'PMF_BSOA     ',ID_PMF_BSOA,   5, F, 'ug m-3', 'Fine-Mode Biogenic-VOC Derived OA' ),
     &PMDTP( 'PMF_CLDGLY   ',ID_PMF_CLDGLY, 5, F, 'ug m-3', 'Fine-Mode Cloud-Processed OA + Glyoxal SOA' ),
     &PMDTP( 'PMF_ISOPSOA  ',ID_PMF_ISOP,   5, F, 'ug m-3', 'Fine-Mode Isoprene SOA' ),
     &PMDTP( 'PMF_IEPOXSOA ',ID_PMF_IEPOX,  5, F, 'ug m-3', 'Fine-Mode IEPOX SOA' ),
     &PMDTP( 'PMF_MTNSOA   ',ID_PMF_MTNSOA, 5, F, 'ug m-3', 'Fine-Mode Monoterpene Nitrate SOA' ),
     &PMDTP( 'PMF_MTSOA    ',ID_PMF_MTSOA,  5, F, 'ug m-3', 'Fine-Mode Monoterpene SOA' ),
                                                            
      ! AMS Speciation                                      
     &PMDTP( 'PMAMS_SO4    ',ID_AMS_SO4,    5, F, 'ug m-3', 'Sulfate Collected by the AMS' ),
     &PMDTP( 'PMAMS_NO3    ',ID_AMS_NO3,    5, F, 'ug m-3', 'Nitrate Collected by the AMS' ),
     &PMDTP( 'PMAMS_NH4    ',ID_AMS_NH4,    5, F, 'ug m-3', 'Ammonium Collected by the AMS' ),
     &PMDTP( 'PMAMS_CL     ',ID_AMS_CL,     5, F, 'ug m-3', 'Chloride Collected by the AMS' ),
     &PMDTP( 'PMAMS_OA     ',ID_AMS_OA,     5, F, 'ug m-3', 'Organic Aerosol Collected by the AMS' ),
     &PMDTP( 'PMAMS_OTOC   ',ID_AMS_OTOC,   5, F, 'ug ug-1','O:C Collected and Measured by the AMS' ),
                                                            
      ! PM1 Speciation                                      
     &PMDTP( 'PM1_SO4      ',ID_PM1_SO4,    5, F, 'ug m-3', 'PM1 Sulfate' ),
     &PMDTP( 'PM1_NO3      ',ID_PM1_NO3,    5, F, 'ug m-3', 'PM1 Nitrate' ),
     &PMDTP( 'PM1_NH4      ',ID_PM1_NH4,    5, F, 'ug m-3', 'PM1 Ammonium' ),
     &PMDTP( 'PM1_CL       ',ID_PM1_CL,     5, F, 'ug m-3', 'PM1 Chloride' ),
     &PMDTP( 'PM1_NA       ',ID_PM1_NA,     5, F, 'ug m-3', 'PM1 Sodium' ),
     &PMDTP( 'PM1_EC       ',ID_PM1_EC,     5, F, 'ug m-3', 'PM1 Black Carbon' ),
     &PMDTP( 'PM1_OC       ',ID_PM1_OC,     5, F, 'ugC m-3','PM1 Organic Carbon' ),
     &PMDTP( 'PM1_OA       ',ID_PM1_OA,     5, F, 'ug m-3', 'PM1 Organic Aerosol' ),
     &PMDTP( 'PM1_MG       ',ID_PM1_MG,     5, F, 'ug m-3', 'PM1 Magnesium' ),
     &PMDTP( 'PM1_K        ',ID_PM1_K,      5, F, 'ug m-3', 'PM1 Potassium' ),
     &PMDTP( 'PM1_CA       ',ID_PM1_CA,     5, F, 'ug m-3', 'PM1 Calcium' ),
     &PMDTP( 'PM1_OTHER    ',ID_PM1_OT,     5, F, 'ug m-3', 'PM1 Other Mass' ),
     &PMDTP( 'PM1_FE       ',ID_PM1_FE,     5, F, 'ug m-3', 'PM1 Iron' ),
     &PMDTP( 'PM1_SI       ',ID_PM1_SI,     5, F, 'ug m-3', 'PM1 Silicon' ),
     &PMDTP( 'PM1_TI       ',ID_PM1_TI,     5, F, 'ug m-3', 'PM1 Titanium' ),
     &PMDTP( 'PM1_MN       ',ID_PM1_MN,     5, F, 'ug m-3', 'PM1 Manganese' ),
     &PMDTP( 'PM1_AL       ',ID_PM1_AL,     5, F, 'ug m-3', 'PM1 Aluminum' ),
     &PMDTP( 'PM1_SOIL     ',ID_PM1_SOIL,   5, F, 'ug m-3', 'PM1 Recon Soil (Fine) and Explicit (Coarse)' ),
     &PMDTP( 'PM1_UNSP1    ',ID_PM1_UNSP1,  5, F, 'ug m-3', 'PM1 Unspeciated Coarse Mass' ),
     &PMDTP( 'PM1_UNSPCRS  ',ID_PM1_UN,     5, F, 'ug m-3', 'PM1 Unspeciated Coarse Mass' ),
     &PMDTP( 'PM1_HP       ',ID_PM1_HP,     5, F, 'ug m-3', 'PM1 H+ Ion' ),
                                                            
      ! PM2.5 Speciation                                    
     &PMDTP( 'PM25_SO4     ',ID_PM25_SO4,   5, F, 'ug m-3', 'PM2.5 Sulfate' ),
     &PMDTP( 'PM25_NO3     ',ID_PM25_NO3,   5, F, 'ug m-3', 'PM2.5 Nitrate' ),
     &PMDTP( 'PM25_NH4     ',ID_PM25_NH4,   5, F, 'ug m-3', 'PM2.5 Ammonium' ),
     &PMDTP( 'PM25_CL      ',ID_PM25_CL,    5, F, 'ug m-3', 'PM2.5 Chloride' ),
     &PMDTP( 'PM25_NA      ',ID_PM25_NA,    5, F, 'ug m-3', 'PM2.5 Sodium' ),
     &PMDTP( 'PM25_EC      ',ID_PM25_EC,    5, F, 'ug m-3', 'PM2.5 Black Carbon' ),
     &PMDTP( 'PM25_OC      ',ID_PM25_OC,    5, F, 'ugC m-3','PM2.5 Organic Carbon' ),
     &PMDTP( 'PM25_OA      ',ID_PM25_OA,    5, F, 'ug m-3', 'PM2.5 Organic Aerosol' ),
     &PMDTP( 'PM25_MG      ',ID_PM25_MG,    5, F, 'ug m-3', 'PM2.5 Magnesium' ),
     &PMDTP( 'PM25_K       ',ID_PM25_K,     5, F, 'ug m-3', 'PM2.5 Potassium' ),
     &PMDTP( 'PM25_CA      ',ID_PM25_CA,    5, F, 'ug m-3', 'PM2.5 Calcium' ),
     &PMDTP( 'PM25_OTHER   ',ID_PM25_OT,    5, F, 'ug m-3', 'PM2.5 Other Mass' ),
     &PMDTP( 'PM25_FE      ',ID_PM25_FE,    5, F, 'ug m-3', 'PM2.5 Iron' ),
     &PMDTP( 'PM25_SI      ',ID_PM25_SI,    5, F, 'ug m-3', 'PM2.5 Silicon' ),
     &PMDTP( 'PM25_TI      ',ID_PM25_TI,    5, F, 'ug m-3', 'PM2.5 Titanium' ),
     &PMDTP( 'PM25_MN      ',ID_PM25_MN,    5, F, 'ug m-3', 'PM2.5 Manganese' ),
     &PMDTP( 'PM25_AL      ',ID_PM25_AL,    5, F, 'ug m-3', 'PM2.5 Aluminum' ),
     &PMDTP( 'PM25_SOIL    ',ID_PM25_SOIL,  5, F, 'ug m-3', 'PM2.5 Recon Soil (Fine) and Explicit (Coarse)' ),
     &PMDTP( 'PM25_UNSP1   ',ID_PM25_UNSP1, 5, F, 'ug m-3', 'PM2.5 Unspeciated Coarse Mass' ),
     &PMDTP( 'PM25_UNSPCRS ',ID_PM25_UN,    5, F, 'ug m-3', 'PM2.5 Unspeciated Coarse Mass' ),
     &PMDTP( 'PM25_HP      ',ID_PM25_HP,    5, F, 'ug m-3', 'PM2.5 H+ Ion' ),

      ! PM2.5-10.0 Speciation
     &PMDTP( 'PM25TO10_SO4 ',ID_PM25to10_SO4,5,F, 'ug m-3', 'Coarse-Mode Sulfate' ),
     &PMDTP( 'PM25TO10_NO3 ',ID_PM25to10_NO3,5,F, 'ug m-3', 'Coarse-Mode Nitrate' ),
     &PMDTP( 'PM25TO10_NH4 ',ID_PM25to10_NH4,5,F, 'ug m-3', 'Coarse-Mode Ammonium' ),
     &PMDTP( 'PM25TO10_CL  ',ID_PM25to10_CL, 5,F, 'ug m-3', 'Coarse-Mode Chloride' ),
     &PMDTP( 'PM25TO10_NA  ',ID_PM25to10_NA, 5,F, 'ug m-3', 'Coarse-Mode Sodium' ),

      ! PM Toxics that are part of the Multipollutant (mp) Mechanism
     &PMDTP( 'PM25_HDIESEL ',ID_PM25_HDSL,   5,F, 'ug m-3', 'PM2.5 Diesel Total from HAP inventory' ),
     &PMDTP( 'PM25_HBE     ',ID_PM25_HBE,    5,F, 'ug m-3', 'PM2.5 Beryllium from HAP inventory' ),
     &PMDTP( 'PM25_HCD     ',ID_PM25_HCD,    5,F, 'ug m-3', 'PM2.5 Cadmium from HAP inventory' ),
     &PMDTP( 'PM25_HCR3    ',ID_PM25_HCR3,   5,F, 'ug m-3', 'PM2.5 Chromium-III from HAP inventory' ),
     &PMDTP( 'PM25_HCR6    ',ID_PM25_HCR6,   5,F, 'ug m-3', 'PM2.5 Chromium-VI from HAP inventory' ),
     &PMDTP( 'PM25_HCR     ',ID_PM25_HCR,    5,F, 'ug m-3', 'PM2.5 Chromium Total from HAP inventory' ),
     &PMDTP( 'PM25_HPB     ',ID_PM25_HPB,    5,F, 'ug m-3', 'PM2.5 Lead from HAP inventory' ),
     &PMDTP( 'PM25_HMN     ',ID_PM25_HMN,    5,F, 'ug m-3', 'PM2.5 Manganese from HAP inventory' ),
     &PMDTP( 'PM25_HNI     ',ID_PM25_HNI,    5,F, 'ug m-3', 'PM2.5 Nickel from HAP inventory' ),
     &PMDTP( 'PM25_HAS     ',ID_PM25_HAS,    5,F, 'ug m-3', 'PM2.5 Arsenic from HAP inventory' ),
     &PMDTP( 'PM25_HG      ',ID_PM25_HG,     5,F, 'ug m-3', 'PM2.5 Mercury from HAP inventory' ),
     &PMDTP( 'PM10_HDIESEL ',ID_PM10_HDSL,   5,F, 'ug m-3', 'PM10.0 Diesel Total from HAP inventory' ),
     &PMDTP( 'PM10_HBE     ',ID_PM10_HBE,    5,F, 'ug m-3', 'PM10.0 Beryllium from HAP inventory' ),
     &PMDTP( 'PM10_HCD     ',ID_PM10_HCD,    5,F, 'ug m-3', 'PM10.0 Cadmium from HAP inventory' ),
     &PMDTP( 'PM10_HCR3    ',ID_PM10_HCR3,   5,F, 'ug m-3', 'PM10.0 Chromium-III from HAP inventory' ),
     &PMDTP( 'PM10_HCR6    ',ID_PM10_HCR6,   5,F, 'ug m-3', 'PM10.0 Chromium-VI from HAP inventory' ),
     &PMDTP( 'PM10_HCR     ',ID_PM10_HCR,    5,F, 'ug m-3', 'PM10.0 Chromium Total from HAP inventory' ),
     &PMDTP( 'PM10_HPB     ',ID_PM10_HPB,    5,F, 'ug m-3', 'PM10.0 Lead from HAP inventory' ),
     &PMDTP( 'PM10_HMN     ',ID_PM10_HMN,    5,F, 'ug m-3', 'PM10.0 Manganese from HAP inventory' ),
     &PMDTP( 'PM10_HNI     ',ID_PM10_HNI,    5,F, 'ug m-3', 'PM10.0 Nickel from HAP inventory' ),
     &PMDTP( 'PM10_HAS     ',ID_PM10_HAS,    5,F, 'ug m-3', 'PM10.0 Arsenic from HAP inventory' ),
     &PMDTP( 'PM10_HG      ',ID_PM10_HG,     5,F, 'ug m-3', 'PM10.0 Mercury from HAP inventory' ),

      ! Other Special Variables and Properties
     &PMDTP( 'TNO3         ',ID_TNO3,       5, F, 'ug m-3', 'Total Nitrate only Including Inorganics' ),
     &PMDTP( 'TNO3TOT      ',ID_TNO3TOT,    5, F, 'ug m-3', 'Total Nitrate including Inorganics and Organics' ),
     &PMDTP( 'PM25_FRM     ',ID_PM25_FRM,   5, F, 'ug m-3', 'Federal Reference Method PM2.5' ),
     &PMDTP( 'PMF_FRM      ',ID_PMF_FRM,    5, F, 'ug m-3', 'Federal Reference Method PMIJ' ),
     &PMDTP( 'AOD_550      ',ID_AOD550,     5, F, '1     ', 'Aerosol Optical Depth at 550 nm (Angstrom interp)' ),
     &PMDTP( 'PM_EXT_550   ',ID_PMEXT550,   5, F, 'Mm-1  ', 'Aerosol Extinction at 550 nm (Angstrom interp)' ),
     &PMDTP( 'RH           ',ID_RH,         5, F, '1     ', 'Relative humidity' ),
     &PMDTP( 'T            ',ID_TEMP,       5, F, 'K     ', 'Temperature' ),
     &PMDTP( 'P            ',ID_PRES,       5, F, 'Pa    ', 'Pressure' ),
     &PMDTP( 'GAMMA_N2O5   ',ID_GN2O5F,     5, F, '1     ', 'Fine Mode N2O5 Heterogeneous rxn probability' ),
     &PMDTP( 'GAMMA_N2O5K  ',ID_GN2O5C,     5, F, '1     ', 'Coarse Mode N2O5 Heterogeneous rxn probability' ),
     &PMDTP( 'YIELD_CLNO2  ',ID_YCLNO2F,    5, F, '1     ', 'Fine Mode CLNO2 Heterogeneous reaction yield' ),
     &PMDTP( 'YIELD_CLNO2K ',ID_YCLNO2C,    5, F, '1     ', 'Coarse Mode CLNO2 Heterogeneous reaction yield' ),
     &PMDTP( 'GAMMA_IEPOX  ',ID_GIEPOX,     5, F, '1     ', 'IEPOX heterogeneous uptake coefficient' ),
     &PMDTP( 'K_IEPOX      ',ID_KIEPOX,     5, F, 's-1   ', 'IEPOX pseudo particle phase reaction rate const' ),
     &PMDTP( 'GAMMA_IMAE   ',ID_GIMAE,      5, F, '1     ', 'IMAE heterogeneous uptake coefficient' ) /)
     

        TYPE PMDIAG_KEY_TYPE
            CHARACTER( 24 )             :: WORD    ! Keyword for selecting mulitple PMDIAG parameters
            INTEGER                     :: N       ! Number of Parameters associated with the Keyword
            CHARACTER( 16 ),ALLOCATABLE :: LIST(:) ! List of parameters
        END TYPE PMDIAG_KEY_TYPE

        TYPE (PMDIAG_KEY_TYPE), ALLOCATABLE, SAVE :: PMDIAG_KEY( : )
        INTEGER, SAVE :: N_KEY

        INTEGER, SAVE :: MAP_ID2LIST( N_PMDIAG_LIST ) 

        ! Variables for Storing Values of PMDIAG Output      
        REAL, ALLOCATABLE, SAVE :: PMDIAG_INST( :,:,:,: )
        REAL, ALLOCATABLE, SAVE :: PMDIAG_AVRG( :,:,:,: )
        REAL, ALLOCATABLE, SAVE :: PMDIAG_LOCAL( : )
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
        TYPE PMDIAG_COEFFS_TYPE
            LOGICAL L_MAPPED                 ! Record whether or not this aggregate has been mapped
            INTEGER N_VARS                   ! Number of variables to store coefficients for. 
                                             !   This should not account for modes. For example, 
                                             !   to sum ASO4IJK, N_VARS would be 1 since only 
                                             !   ASO4 is being used.
            LOGICAL, ALLOCATABLE :: L_GAS(:)    ! Is this variable a gas?
            LOGICAL, ALLOCATABLE :: L_PREC(:)   ! Is this variable a gas-phase precursor already in the aerosol module?
            LOGICAL, ALLOCATABLE :: L_AGG(:)    ! Is this variable another aggregated diagnostic parameter?
            LOGICAL, ALLOCATABLE :: L_MODE(:)   ! For which modes does this aggregate apply
            INTEGER, ALLOCATABLE :: I_SPEC(:)   ! Index of this Variable in Aerosol/Gas/Aggregate Array
            REAL,    ALLOCATABLE :: COEFF(:)       ! Coefficient for each variable
            INTEGER INLET                    ! Integer pointing to the corresponding inlet of choice 
                                             !   in the PMDIAG_INLET structure
            INTEGER WETNESS                  ! Integer prescribing wet or dry aerosol (1 = wet, 2 = dry)
                                             !   Default is wet
        END TYPE PMDIAG_COEFFS_TYPE

        TYPE (PMDIAG_COEFFS_TYPE), SAVE :: PMDIAG_COEFFS( N_PMDIAG_LIST ) 


        TYPE PMDIAG_INLET_TYPE
            CHARACTER( 16 ) CINLET            ! Inlet Name
            REAL            DIAM_LO           ! Lower Diameter of this Inlet [um]
            REAL            DIAM_HI           ! Upper Diameter of this inlet [um]
        END TYPE PMDIAG_INLET_TYPE

        INTEGER, PARAMETER :: N_INLET = 10
        INTEGER, PARAMETER :: IPM01 = 1, IPM1 = 2, IPM25 = 3, IPM10 = 4, 
     &                        IPM25to10 = 5, IPMAMS = 6, INUM10 = 7,
     &                        INUM20 = 8, INUM40 = 9, INUM100 = 10
        INTEGER, PARAMETER :: IWET = 1, IDRY = 2

        TYPE (PMDIAG_INLET_TYPE), PARAMETER :: PMDIAG_INLET( N_INLET ) = (/
       !                          Inlet Name D_Lo  D_Hi
       !                          ---------- ----  ----
     &        PMDIAG_INLET_TYPE( 'PM01    ', 0.0,  0.1 ),
     &        PMDIAG_INLET_TYPE( 'PM1     ', 0.0,  1.0 ),
     &        PMDIAG_INLET_TYPE( 'PM25    ', 0.0,  2.5 ),
     &        PMDIAG_INLET_TYPE( 'PM10    ', 0.0, 10.0 ),
     &        PMDIAG_INLET_TYPE( 'PM25to10', 2.5, 10.0 ),
     &        PMDIAG_INLET_TYPE( 'PMAMS   ', 0.0,  0.0 ),
     &        PMDIAG_INLET_TYPE( 'INUM10  ', 0.01,1.0e3),
     &        PMDIAG_INLET_TYPE( 'INUM20  ', 0.02,1.0e3),
     &        PMDIAG_INLET_TYPE( 'INUM40  ', 0.04,1.0e3),
     &        PMDIAG_INLET_TYPE( 'INUM100 ', 0.1, 1.0e3) /)

        REAL, SAVE :: INLET_FRAC( N_INLET,N_MODE,2 )  ! Fraction of each aerosol mode 
                                                      !   collected by each inlet
                                                      !   Third dimension is 1
                                                      !     for wet and 2 for dry
      END MODULE PMDIAG_DATA
