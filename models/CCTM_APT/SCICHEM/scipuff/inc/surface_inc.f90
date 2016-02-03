!*******************************************************************************
!$RCSfile: surface_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module surface_inc
use param_inc
save

type  grid_str
  sequence
  integer     nunit       !Unit number
  integer     nvart       !Total number of variables
  integer     nx   !Primary grid
  integer     ny   !Primary grid
  integer     nvar !No. data values per variable
  integer     record      !Record for writing
  integer     ncells      !No. cells
  integer     ipgrd       !Pointer to grid data
  integer     ipdat       !Pointer to surface data
  integer     ipnam       !Offset to first name in list
  integer     maxlev      !Max refinement level
  real        xmin !grid origin
  real        ymin !grid origin
  real        dx   !grid size
  real        dy   !grid size
  real        delmin      !minimum grid size
end type  grid_str
end module surface_inc
