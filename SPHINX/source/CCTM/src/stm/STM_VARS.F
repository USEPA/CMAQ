
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
      Module stm_vars

C  Contains:
C
C  Revision History:
C SR 07/2/2019 Initial version

C----------------------------------------------------------------------

      Implicit None

      Integer, Parameter :: N_STM_AE_SPC  = 14   ! number of stm ae species
      Integer, Parameter :: N_STM_OAE_SPC =  9   ! number of stm oae species
      Integer, Parameter :: N_STM_NR_SPC  =  1   ! number of stm nr species

      Logical, Save :: organosulf = .False.

C Define value as Y and N for the spc tables
      CHARACTER( 16 ), Parameter, Private :: Y = 'Yes'
      CHARACTER( 16 ), Parameter, Private :: N = 'No'

      ! Equivalent to the Namelist Registry Format for Gases and Nonreactives
      TYPE STM_SPC_DATA_1
          CHARACTER( 16 ) :: SPECIES_NAME  ! CMAQ Species Name
          REAL            :: MOLWT         ! g mol-1
          CHARACTER( 16 ) :: IC_SURR       ! IC Surrogate
          REAL            :: IC_FAC        ! IC Scale Factor
          CHARACTER( 16 ) :: BC_SURR       ! BC Surrogate
          REAL            :: BC_FAC        ! BC Scale Factor
          CHARACTER( 16 ) :: DEPV_SURR     ! Dry Deposition Surrogate
          REAL            :: DEPV_FAC      ! Dry Deposition Scale Factor
          CHARACTER( 16 ) :: SCAV_SURR     ! Wet Scavenging Surrogate
          REAL            :: SCAV_FAC      ! Wet Scavenging Scale Factor
          CHARACTER( 16 ) :: AERO_SURR     ! Aerosol Module Surrogate
          CHARACTER( 16 ) :: CLOUD_SURR    ! Cloud Chemistry Surrogate
          CHARACTER( 16 ) :: TRNS_FLAG     ! Do Transport (ADV + DIFF)?
          CHARACTER( 16 ) :: DDEP_FLAG     ! Output Dry Deposition Velocities
          CHARACTER( 16 ) :: WDEP_FLAG     ! Output Wet Scavenging Fluxes
          CHARACTER( 16 ) :: CONC_FLAG     ! Output Concentration
      END TYPE STM_SPC_DATA_1

      ! Equivalent to the Namelist Registry Format for Aerosols
      TYPE STM_SPC_DATA_2
          CHARACTER( 16 ) :: SPECIES_NAME  ! CMAQ Species Name
          REAL            :: MOLWT         ! g mol-1
          CHARACTER( 16 ) :: IC_SURR       ! IC Surrogate
          REAL            :: IC_FAC        ! IC Scale Factor
          CHARACTER( 16 ) :: BC_SURR       ! BC Surrogate
          REAL            :: BC_FAC        ! BC Scale Factor
          CHARACTER( 16 ) :: DEPV_SURR     ! Dry Deposition Surrogate
          REAL            :: DEPV_FAC      ! Dry Deposition Scale Factor
          CHARACTER( 16 ) :: SCAV_SURR     ! Wet Scavenging Surrogate
          REAL            :: SCAV_FAC      ! Wet Scavenging Scale Factor
          CHARACTER( 16 ) :: CLOUD_SURR    ! Cloud Chemistry Surrogate
          CHARACTER( 16 ) :: TRNS_FLAG     ! Do Transport?
          CHARACTER( 16 ) :: DDEP_FLAG     ! Output Dry Deposition Velocities
          CHARACTER( 16 ) :: WDEP_FLAG     ! Output Wet Scavenging Fluxes
          CHARACTER( 16 ) :: CONC_FLAG     ! Output Concentration
      END TYPE STM_SPC_DATA_2

      TYPE (STM_SPC_DATA_2), Parameter :: STM_AE_SPECIES_DATA(N_STM_AE_SPC) = (/
C                                                                 DRYDEP SURR                                            DDEP
C                                               ICFAC       BCFAC    |                                            TRNS | WDEP
C                      SPECIES      MW   IC       |  BC       |      |    FAC  WET-SCAV SURR  FAC AE2AQ SURR         | | | CONC  
C                      ------------ ---- ------- -+- ------- -+- ----+--- --- --------------- --- ------------------ + + + =
     & STM_SPC_DATA_2('ASO4AQH2O2J',96.0,''     ,0.0,''     ,0.0,'VMASSJ',1.0,'TRACER_ACCUM' ,1.0,'SO4_AQH2O2_ACCUM',Y,Y,Y,Y), 
     & STM_SPC_DATA_2('ASO4AQO3J'  ,96.0,''     ,0.0,''     ,0.0,'VMASSJ',1.0,'TRACER_ACCUM' ,1.0,'SO4_AQO3_ACCUM'  ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4AQFEMNJ',96.0,''     ,0.0,''     ,0.0,'VMASSJ',1.0,'TRACER_ACCUM' ,1.0,'SO4_AQFEMN_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4AQMHPJ' ,96.0,''     ,0.0,''     ,0.0,'VMASSJ',1.0,'TRACER_ACCUM' ,1.0,'SO4_AQMHP_ACCUM' ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4AQPAAJ' ,96.0,''     ,0.0,''     ,0.0,'VMASSJ',1.0,'TRACER_ACCUM' ,1.0,'SO4_AQPAA_ACCUM' ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4GASJ'   ,96.0,''     ,0.0,''     ,0.0,'VMASSJ',1.0,'TRACER_ACCUM' ,1.0,'TRACER_ACCUM'    ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4EMISJ'  ,96.0,''     ,0.0,''     ,0.0,'VMASSJ',1.0,'TRACER_ACCUM' ,1.0,'TRACER_ACCUM'    ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4ICBCJ'  ,96.0,'ASO4J',1.0,'ASO4J',1.0,'VMASSJ',1.0,'TRACER_ACCUM' ,1.0,'TRACER_ACCUM'    ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4GASI'   ,96.0,''     ,0.0,''     ,0.0,'VMASSI',1.0,'TRACER_AITKEN',1.0,'TRACER_AITKEN'   ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4EMISI'  ,96.0,''     ,0.0,''     ,0.0,'VMASSI',1.0,'TRACER_AITKEN',1.0,'TRACER_AITKEN'   ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4ICBCI'  ,96.0,'ASO4I',1.0,'ASO4I',1.0,'VMASSI',1.0,'TRACER_AITKEN',1.0,'TRACER_AITKEN'   ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4GASK'   ,96.0,''     ,0.0,''     ,0.0,'VMASSC',1.0,'TRACER_COARSE',1.0,'TRACER_COARSE'   ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4EMISK'  ,96.0,''     ,0.0,''     ,0.0,'VMASSC',1.0,'TRACER_COARSE',1.0,'TRACER_COARSE'   ,Y,Y,Y,Y),
     & STM_SPC_DATA_2('ASO4ICBCK'  ,96.0,'ASO4K',1.0,'ASO4K',1.0,'VMASSC',1.0,'TRACER_COARSE',1.0,'TRACER_COARSE'   ,Y,Y,Y,Y)
     & /)

      TYPE (STM_SPC_DATA_2), Parameter :: STM_OAE_SPECIES_DATA(N_STM_OAE_SPC) = ( /
C                                                       DRYDEP SURR                                       DDEP
C                                          ICFAC  BCFAC    |                                         TRNS | WDEP
C                      SPECIES      MW   IC  |  BC  |      |    FAC WET-SCAV SURR  FAC AE2AQ SURR     | | | CONC  
C                      ------------ ---- -- -+- -- -+- ----+--- --- -------------- --- -------------- + + + +
     & STM_SPC_DATA_2('OSO4J'      ,96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('OSO4AQH2O2J',96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('OSO4AQO3J'  ,96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('OSO4AQFEMNJ',96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('OSO4AQMHPJ' ,96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('OSO4AQPAAJ' ,96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('OSO4GASJ'   ,96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('OSO4EMISJ'  ,96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y),
     & STM_SPC_DATA_2('OSO4ICBCJ'  ,96.0,'',0.0,'',0.0,'VMASSJ',1.0,'TRACER_ACCUM',1.0,'TRACER_ACCUM',Y,Y,Y,Y)
     & /)

      TYPE (STM_SPC_DATA_1), Parameter :: STM_NR_SPECIES_DATA(N_STM_NR_SPC) = (/
C                                                             DRYDEP                                             DDEP
C                                            ICFAC      BCFAC  SURR        WET-SCAV                          TRNS | WDEP
C                      SPECIES    MW   IC      |  BC      |      |     FAC   SURR  FAC AE2AQ SURR  NR2AE SURR   | | | CONC  
C                      ---------- ---- ------ -+- ------ -+- ----+---- --- ------- --- ----------- ------------ + + + +
     & STM_SPC_DATA_1('SULF_ICBC',98.0,'SULF',1.0,'SULF',1.0,'VD_SULF',1.0,'H2SO4',1.0,'SULF_ICBC','H2SO4_ICBC',Y,Y,Y,Y)
     & /)

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      End Module stm_vars
