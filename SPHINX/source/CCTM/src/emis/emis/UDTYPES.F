
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
      MODULE UDTYPES

C-----------------------------------------------------------------------
C Function: User-defined types

C Revision History:
C     21 Nov 2007 J.Young: initial implementation
C     22 Dec 2023 C.Nolte: changed from pointers to allocatable arrays,
C                          removed unused types
C-----------------------------------------------------------------------

      IMPLICIT NONE

      TYPE :: IARRY1
         INTEGER, ALLOCATABLE :: ARRY( : )
         INTEGER              :: LEN
      END TYPE IARRY1

      TYPE :: RARRY1
         REAL, ALLOCATABLE  :: ARRY( : )
         INTEGER            :: LEN
      END TYPE RARRY1

      TYPE :: CARRY1
         CHARACTER( 16 ), ALLOCATABLE :: ARRY( : )
         CHARACTER( 16 ), ALLOCATABLE :: UNITS( : )
         INTEGER                      :: LEN
      END TYPE CARRY1

      TYPE :: LARRY1
         LOGICAL, ALLOCATABLE  :: ARRY( : )
         INTEGER               :: LEN
      END TYPE LARRY1

      TYPE :: PTMET_TYPE
         INTEGER          :: LEN1, LEN2
C Allocatable per-source meteorology variables:
         REAL, ALLOCATABLE :: HFX( : )        ! sensible heat flux [watts/m**2]
         REAL, ALLOCATABLE :: HMIX( : )       ! mixing height [m]
         REAL, ALLOCATABLE :: TSFC( : )       ! surface temperature [degK]
         REAL, ALLOCATABLE :: USTAR( : )      ! friction velocity [m/s]
         REAL, ALLOCATABLE :: PRSFC( : )      ! surface pressure [Pa]
C Allocatable temporary per-layer variables from 1:EMLAYS:
         REAL, ALLOCATABLE   :: WSPD( : )       ! wind speed [m/s]
         REAL, ALLOCATABLE   :: DTHDZ( : )      ! virtual pot temp gradient [degK/m]
C Allocatable temporary per-layer variables from 0:EMLAYS:
         REAL, ALLOCATABLE :: PRESF( : )      ! pressure at full-levels
         REAL, ALLOCATABLE :: ZZF( : )        ! per src elevn at full-levels
C Allocatable per-source and per-layer meteorology variables:
C (Dimensioned by layers, then number of sources)
         REAL, ALLOCATABLE :: DDZF ( :,: )    ! 1/( ZF(L) - ZF(L-1) )
         REAL, ALLOCATABLE :: PRES ( :,: )    ! pressure [Pa]
         REAL, ALLOCATABLE :: DENS ( :,: )    ! air density [kg/m**3]
         REAL, ALLOCATABLE :: QV   ( :,: )    ! moisture mixing ratio [kg/kg]
         REAL, ALLOCATABLE :: TA   ( :,: )    ! temperature [degK]
         REAL, ALLOCATABLE :: UWIND( :,: )    ! x-component wind speed [m/s]
         REAL, ALLOCATABLE :: VWIND( :,: )    ! y-component wind speed [m/s]
         REAL, ALLOCATABLE :: ZF   ( :,: )    ! full layer height [m]
         REAL, ALLOCATABLE :: ZH   ( :,: )    ! mid layer height [m]
         REAL, ALLOCATABLE :: ZSTK ( :,: )    ! ZF( L,S ) - STKHT(S) [m]
      END TYPE PTMET_TYPE

      END MODULE UDTYPES

