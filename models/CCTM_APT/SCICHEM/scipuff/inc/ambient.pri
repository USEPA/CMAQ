!----------------------------------------------------------------------
! --- common block /ambient/ -- selected met. data at one         prime
!                               grid cell;  used in numerical
!                               plume rise computation
!----------------------------------------------------------------------

INTEGER                 :: nza
REAL, DIMENSION(mxnz)   :: uamb,ramb,tamb,zgpta
REAL, DIMENSION(mxnzp1) :: dedz,zfacea

common/ambient/nza,uamb,ramb,dedz,tamb, &
               zfacea,zgpta,tamb0,ramb0,adia,ptgrad0

! --- common block /ambient/ variables:

!                    nza - integer - number of layers
!             uamb(mxnz) - real    - wind speed profile (m/s) - winds
!                                    defined at cell centers
!             ramb(mxnz) - real    - ambient air density profile
!                                    (kg/m**3) - defined at cell centers
!           dedz(mxnzp1) - real    - pot. temperature gradient profile
!                                    (deg. k/m) - defined at cell faces
!             tamb(mxnz) - real    - temperature profile (deg %k) -
!                                    defined at cell centers
!         zfacea(mxnzp1) - real    - heights of layer faces (m)
!            zgpta(mxnz) - real    - heights of layer centers (m)
!                  tamb0 - real    - surface air temperature (deg. k)
!                  ramb0 - real    - surface air density (kg/m**3)
!                   adia - real    - dry adiabatic lapse rate (deg. k/m)
!                ptgrad0 - real    - minimum potential temperature lapse
!----------------------------------------------------------------------
