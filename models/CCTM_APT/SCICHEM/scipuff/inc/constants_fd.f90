module constants_fd

!------ Mathematical constants

  REAL, PARAMETER :: PI        = 3.141593
  REAL, PARAMETER :: PI2       = 6.283185      !2.*PI
  REAL, PARAMETER :: PI3       = 15.74961      !SQRT(2*PI)**3
  REAL, PARAMETER :: PI180     = 1.7453292e-02 ! PI/180.

!------ Earth constants

  REAL, PARAMETER :: Rearth  = 6371.2E3          !m - Mean radius
  REAL, PARAMETER :: SPHFAC  = PI180*Rearth      !m/deg
  REAL, PARAMETER :: SPHFACR = 1.0/SPHFAC
  REAL, PARAMETER :: G0      = 9.81              !m/s/s
  REAL, PARAMETER :: GT      = 0.0327            !Gravity and g/T0
  REAL, PARAMETER :: FCOR0   = 4.*PI/(24.*3600.) !Coriolis param at pole 1/s

!------ Properties of Air & Atmosphere

  REAL  rhoair ,rmuair ,rnu                  !Air properties at STP

  REAL, PARAMETER :: RHOCP   = 1206.         !rho*Cp for air

!------ Turbulence Model Constants

  REAL, PARAMETER :: VONK  = 0.4         !von Karman constant
  REAL, PARAMETER :: A     = 0.75        !ARAP closure model constants 
  REAL, PARAMETER :: B     = 0.125
  REAL, PARAMETER :: BS    = 1.8*B
  REAL, PARAMETER :: CVRTX = 3.0         !Plume rise model constants
  REAL, PARAMETER :: CQB   = 0.4
  REAL, PARAMETER :: CSI1  = 0.25        !Internal scale growth: BL(3d)
  REAL, PARAMETER :: CSI2  = 2.0*BS/4.6  !Internal scale growth: Large-scale(2D)
  REAL, PARAMETER :: EQF   = GT/(2.0*A*BS)

end module constants_fd
