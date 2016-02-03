
!----------------------------------------------------------------------
! Version 1, January 2005, PK, AER, Inc
! Updated for CMAQ 5, PK, ENVIRON, August 2011
!----------------------------------------------------------------------

module aero_consts_inc

! Geometric Constants
REAL, PARAMETER :: PI = 3.14159265358979324
REAL, PARAMETER :: F6DPI = 6.0 / PI

! *** dry moment factor
REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0

! *** flag to include water in the 3rd moment calculation
LOGICAL, PARAMETER :: M3_WET_FLAG = .FALSE.

! *** if LIMIT_Sg = T, atkn & accum std. dev. are not changed by GETPAR
LOGICAL, PARAMETER :: LIMIT_Sg = .FALSE.

! gas constant in L-atm/mol-K
REAL, PARAMETER :: RGAS = 0.0820567

REAL, PARAMETER :: STDATMPA = 101325.0 ! standard atmosphere  [ Pa ]

REAL, PARAMETER :: MWAIR = 28.9628  ! mean molec. weight for dry air [ g/mol ]
                                    ! 78.06%  N2, 21% O2 and 0.943% A on a mole
                                    ! fraction basis.
                                    ! ( Source : Hobbs, 1995) pp 69-70.
     
end module aero_consts_inc
