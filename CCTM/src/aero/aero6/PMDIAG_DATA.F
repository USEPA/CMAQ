
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

        IMPLICIT NONE
         
        ! Define Logical values as T and F for the aerospc table
        Logical, Parameter, Private :: T = .true.
        Logical, Parameter, Private :: F = .false.

        LOGICAL, SAVE :: L_PMDIAG
        INTEGER, SAVE :: PMDIAG_AVRG_STEP = 0

        TYPE PMDIAG_TYPE
          CHARACTER (LEN = 16) :: NAME   !Parameter Name
          INTEGER              :: FXN       !Function Number in Case Statement
          INTEGER              :: VAR_TYPE  !IOAPI Variable Type
          LOGICAL              :: MODE      !Is this property mode-dependent?
          CHARACTER (LEN = 16) :: UNIT      !Units for Parameter
          CHARACTER (LEN = 80) :: DESC      !Description
        END TYPE PMDIAG_TYPE

        INTEGER, PARAMETER :: N_PMDIAG_LIST = 20

        TYPE (PMDIAG_TYPE), PARAMETER :: PMDIAG_LIST( N_PMDIAG_LIST ) = (/
     &    PMDIAG_TYPE( 'STDEV        ',  1, 5, T, '      ', 'Mode standard deviation (dry and wet)' ),
     &    PMDIAG_TYPE( 'DRY_DG       ',  2, 5, T, 'um    ', 'Mode mean diameter (dry)' ),
     &    PMDIAG_TYPE( 'WET_DG       ',  3, 5, T, 'um    ', 'Mode mean diameter (wet)' ),
     &    PMDIAG_TYPE( 'WET_M2       ',  4, 5, T, 'm2 m-3', 'Mode 2nd moment (wet)' ),
     &    PMDIAG_TYPE( 'DRY_M3       ',  5, 5, T, 'm3 m-3', 'Mode 3rd moment (dry)' ),
     &    PMDIAG_TYPE( 'WET_M3       ',  6, 5, T, 'm3 m-3', 'Mode 3rd moment (wet)' ),
     &    PMDIAG_TYPE( 'RH           ',  7, 5, F, '1     ', 'Relative humidity' ),
     &    PMDIAG_TYPE( 'FPM1         ',  8, 5, T, '1     ', 'PM1 fraction of mode' ),
     &    PMDIAG_TYPE( 'FPM25        ',  9, 5, T, '1     ', 'PM2.5 fraction of mode' ),
     &    PMDIAG_TYPE( 'FPM10        ', 10, 5, T, '1     ', 'PM10 fraction of mode' ),
     &    PMDIAG_TYPE( 'FAMS         ', 11, 5, T, '1     ', 'AMS Transmission Fraction of mode' ),
     &    PMDIAG_TYPE( 'GAMMA_N2O5   ', 12, 5, F, '1     ', 'Fine Mode N2O5 Heterogeneous rxn probability' ),
     &    PMDIAG_TYPE( 'GAMMA_N2O5K  ', 13, 5, F, '1     ', 'Coarse Mode N2O5 Heterogeneous rxn probability' ),
     &    PMDIAG_TYPE( 'YIELD_CLNO2  ', 14, 5, F, '1     ', 'Fine Mode CLNO2 Heterogeneous reaction yield' ),
     &    PMDIAG_TYPE( 'YIELD_CLNO2K ', 15, 5, F, '1     ', 'Coarse Mode CLNO2 Heterogeneous reaction yield' ),
     &    PMDIAG_TYPE( 'GAMMA_IEPOX  ', 16, 5, F, '1     ', 'IEPOX heterogeneous uptake coefficient' ),
     &    PMDIAG_TYPE( 'DENS_        ', 17, 5, T, 'kg m-3', 'Bulk Density of Particles' ),
     &    PMDIAG_TYPE( 'K_IEPOX      ', 18, 5, F, 's-1   ', 'IEPOX pseudo particle phase reaction rate const' ),
     &    PMDIAG_TYPE( 'GAMMA_IMAE   ', 19, 5, F, '1     ', 'IMAE heterogeneous uptake coefficient' ),
     &    PMDIAG_TYPE( 'PM25         ', 20, 5, F, 'ug m-3', 'Bulk PM2.5 Concentration' ) /)


        ! Variables for Storing Values of PMDIAG Output
        REAL, ALLOCATABLE, SAVE :: PMDIAG_INST( :,:,:,: )
        REAL, ALLOCATABLE, SAVE :: PMDIAG_AVRG( :,:,:,: )
        REAL, ALLOCATABLE, SAVE :: PMDIAG_LOCAL( : )
       
        ! Variables for Aerosol Chemistry
        REAL, SAVE, ALLOCATABLE :: GAMMA_N2O5IJ( :,:,:  ) ! Fine modes N2O5 rx effic.
        REAL, SAVE, ALLOCATABLE :: GAMMA_N2O5K(  :,:,:  ) ! Coarse mode N2O5 rx effic.
        REAL, SAVE, ALLOCATABLE :: KPARTIEPOX (  :,:,:  ) ! IEPOX particle-phase reaction rate constant (sec-1)
        REAL, SAVE, ALLOCATABLE :: GAMMA_IEPOX(  :,:,:  ) ! IEPOX uptake coeff
        REAL, SAVE, ALLOCATABLE :: GAMMA_IMAE (  :,:,:  ) ! IMAE uptake coeff
        REAL, SAVE, ALLOCATABLE :: YCLNO2IJ   (  :,:,:  ) ! Yield CLNO2 in Fine modes 
        REAL, SAVE, ALLOCATABLE :: YCLNO2K    (  :,:,:  ) ! Yield CLNO2 in coarse mode 
  
        LOGICAL, SAVE           :: AERO_CHEM_SET = .FALSE.
 
      END MODULE PMDIAG_DATA
