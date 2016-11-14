
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/UDTYPES.F,v 1.2 2011/10/21 16:10:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE UDTYPES

C-----------------------------------------------------------------------
C Function: User-defined types

C Revision History:
C     21 Nov 2007 J.Young: initial implementation

C-----------------------------------------------------------------------

      IMPLICIT NONE

      TYPE :: IARRY1
         INTEGER, POINTER :: ARRY( : )
         INTEGER          :: LEN
      END TYPE IARRY1

      TYPE :: IARRY2
         INTEGER, POINTER :: ARRY( :,: )
         INTEGER          :: LEN1, LEN2
      END TYPE IARRY2

      TYPE :: RARRY1
         REAL,    POINTER :: ARRY( : )
         INTEGER          :: LEN
      END TYPE RARRY1

      TYPE :: RARRY2
         REAL,    POINTER :: ARRY( :,: )
         INTEGER          :: LEN1, LEN2
      END TYPE RARRY2

      TYPE :: CARRY1
         CHARACTER( 16 ), POINTER :: ARRY( : )
         INTEGER                  :: LEN
      END TYPE CARRY1

      TYPE :: BILIN_TYPE
C Allocatable un-gridding matrix variables (uses BMATVEC bilinear interpolation):
C (Dimensioned 4 by number of pt sources)
         INTEGER          :: LEN1, LEN2
         INTEGER, POINTER :: ND( :,: )    ! dot-point, cell indexes
         INTEGER, POINTER :: NX( :,: )    ! cross-point, cell indexes
         REAL,    POINTER :: CD( :,: )    ! dot-point, coefficients
         REAL,    POINTER :: CX( :,: )    ! cross-point, coefficients
      END TYPE BILIN_TYPE

      TYPE :: PTMET_TYPE
         INTEGER          :: LEN1, LEN2
C Allocatable per-source meteorology variables:
         REAL,    POINTER :: HFX( : )        ! sensible heat flux [watts/m**2]
         REAL,    POINTER :: HMIX( : )       ! mixing height [m]
         REAL,    POINTER :: TSFC( : )       ! surface temperature [degK]
         REAL,    POINTER :: USTAR( : )      ! friction velocity [m/s]
         REAL,    POINTER :: PRSFC( : )      ! surface pressure [Pa]
C Allocatable temporary per-layer variables from 1:EMLAYS:
         REAL,    POINTER :: WSPD( : )       ! wind speed [m/s]
         REAL,    POINTER :: DTHDZ( : )      ! virtual pot temp gradient [degK/m]
C Allocatable temporary per-layer variables from 0:EMLAYS:
         REAL,    POINTER :: PRESF( : )      ! pressure at full-levels
         REAL,    POINTER :: ZZF( : )        ! per src elevn at full-levels
C Allocatable per-source and per-layer meteorology variables:
C (Dimensioned by layers, then number of sources)
!        REAL,    POINTER :: DDZH ( :,: )    ! 1/( ZH(L) - ZH(L-1) )
         REAL,    POINTER :: DDZF ( :,: )    ! 1/( ZF(L) - ZF(L-1) )
         REAL,    POINTER :: PRES ( :,: )    ! pressure [Pa]
         REAL,    POINTER :: DENS ( :,: )    ! air density [kg/m**3]
         REAL,    POINTER :: QV   ( :,: )    ! moisture mixing ratio [kg/kg]
         REAL,    POINTER :: TA   ( :,: )    ! temperature [degK]
         REAL,    POINTER :: UWIND( :,: )    ! x-component wind speed [m/s]
         REAL,    POINTER :: VWIND( :,: )    ! y-component wind speed [m/s]
         REAL,    POINTER :: ZF   ( :,: )    ! full layer height [m]
         REAL,    POINTER :: ZH   ( :,: )    ! mid layer height [m]
         REAL,    POINTER :: ZSTK ( :,: )    ! ZF( L,S ) - STKHT(S) [m]
      END TYPE PTMET_TYPE

      END MODULE UDTYPES

