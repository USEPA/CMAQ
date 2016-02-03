!*******************************************************************************
!$RCSfile: common_grd.f90,v $
!$Revision: 1.3 $
!$Date: 2003/11/25 22:45:17 $
!*******************************************************************************
module common_grd

!
!    Updated March 2005 for Oct. 2004 CMAQ release (PKK, AER)
!

!------ common for generic gridded met file reading routine

  use common_met

  save
  integer N2, N3, MAXGRD, I_SWIFT, I_McWif

  parameter (N2 = 8) !max no. of 2d fields
  parameter (N3 = 10) !max no. of 3d fields (used for name array)

  parameter (I_SWIFT = 1) !indicates SWIFT vertical staggered grid
  parameter (I_McWif = 2) !indicates McWif vertical staggered grid

  logical lfirst_grid, lz0, lheader, taflag

  integer iyear, imonth, iday, ihour, imin, isec
  integer imax, jmax, kmax, nvar3d, nvar2d, jul0, iyy0
  integer iskip, jskip, kskip, lmap_met, istagger
  integer nxgmc, nygmc, nzgmc, nxygmc

  real dx, dy, x0, y0, xlon0, xlat0
  real, dimension(:),allocatable :: xbs, ybs, wrk
  real, dimension(:,:),allocatable :: wrk1
  real zbtop_med
  real time_gridded, time_gridded_old

  character*8 name3d(N3), name2d(N2)

end module common_grd
