module amb_data  !this will be replaced by the host model common
  implicit none
  integer nxa, nya, nza   !# of grid points in the x, y & z dir
  real x0a, y0a           !Origin of grid (lower left)
  real dxa, dya           !grid spacing in x,y and z
  real xmaxa,ymaxa,zmaxa  !max x,y and z grid points
  logical lwrt_amb        !whether or not to write out 3D ambient 
end module amb_data
