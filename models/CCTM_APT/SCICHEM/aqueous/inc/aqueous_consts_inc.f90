!----------------------------------------------------------------------
! Version 1, for CMAQ 5, PK, ENVIRON, August 2011
!----------------------------------------------------------------------

module aqueous_consts_inc

use aero_consts_inc
!
! Define additional constants for aqueous-phase module
!
!
! Molar volume at STP [ L/mol ] Non MKS units
      REAL, PARAMETER :: MOLVOL = 22.41410

! Standard Temperature [ K ]
      REAL, PARAMETER :: STDTEMP = 273.15

! density of water at 20 C and 1 ATM (kg/m3)
      REAL, PARAMETER :: H2ODENS = 1000.0

! minimum and maximum pH
!     REAL, PARAMETER :: PHMIN = 0.0000001
      REAL, PARAMETER :: PHMIN = -2.0
      REAL, PARAMETER :: PHMAX = 10.0

end module aqueous_consts_inc
