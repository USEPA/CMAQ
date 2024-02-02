
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
      module cgrid_spcs_types

      implicit none

      save

      ! Namelist Registry Format for Gases and Nonreactives
      TYPE SPECIES_DATA_REG_1
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
      END TYPE SPECIES_DATA_REG_1
 
      ! Namelist Registry Format for Aerosols
      TYPE SPECIES_DATA_REG_2
          CHARACTER( 16 ) :: SPECIES_NAME  ! CMAQ Species Name
          REAL            :: MOLWT         ! g mol-1
          LOGICAL         :: LAIT          ! Aitken-Mode Presence
          LOGICAL         :: LACC          ! Accumulation -Mode Presence
          LOGICAL         :: LCOR          ! Coarse-Mode Presence
          CHARACTER( 16 ) :: OPTICS_SURR   ! material's refractive index surrogate
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
      END TYPE SPECIES_DATA_REG_2

      ! Namelist Registry Format for Tracers
      TYPE SPECIES_DATA_REG_3
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
          CHARACTER( 16 ) :: ADVC_FLAG     ! Do Advection?
          CHARACTER( 16 ) :: DIFF_FLAG     ! Do Dispersion (i.e. Diffusion)?
          CHARACTER( 16 ) :: DDEP_FLAG     ! Output Dry Deposition Velocities
          CHARACTER( 16 ) :: WDEP_FLAG     ! Output Wet Scavenging Fluxes
          CHARACTER( 16 ) :: CONC_FLAG     ! Output Concentration
      END TYPE SPECIES_DATA_REG_3

      TYPE (SPECIES_DATA_REG_2), ALLOCATABLE :: AE_SPECIES_DATA(:)

      END MODULE CGRID_SPCS_TYPES
