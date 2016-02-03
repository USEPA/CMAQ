module met_param_inc

! November 2005, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
! Host model grid
! use host_grid_inc

!------ met background common

!------ dimension parameters

!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!    Minor F90 updates, February 2004 (PKK, AER)
!
  integer, save :: MAXXB,MAXYB
  integer, save :: MAX3D,MAX2D

! November 2005, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
! integer, parameter :: MAXZB  = NLAYS     ! max z-grid size (ua)
! integer, parameter :: MAXZBP = MAXZB + 1 ! max z-grid size plus 1 (ua)
  integer, save :: MAXZB,MAXZBP

  integer, parameter :: MAXZP  = 100       ! for plume rise calculations

  integer, parameter :: MAX1D  = 101       ! max z-grid of 1d profiles
  integer, parameter :: MAX3DB = 300000    ! max 3d boundary layer field size
  integer, parameter :: MAX3C  = 6000      ! max z-grid size (bl)
  integer, parameter :: MAXXC  = 20        ! max x,y-grid size (climatology)
  integer, parameter :: MAXZC  = 20        ! max z-grid size (climatology)
  integer, parameter :: MAX_CLD = 1        ! max no. of cloud droplet sizes

!------ limits on met data

  real, parameter :: LIM_CLD   = 10.  ! max allowable cloud content (g/m3)
  real, parameter :: LIM_PRATE = 100. ! max allowable precip rate (mm/hr)

!------ obs_type parameters (for met uncertainty)

  integer, parameter :: I_OBS   = 1
  integer, parameter :: I_ANLYS = 2
  integer, parameter :: I_FCST  = 3

end module met_param_inc
