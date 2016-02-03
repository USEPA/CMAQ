!*******************************************************************************
!$RCSfile: common_srf.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module common_srf
use param_inc
use surface_inc
save

type ( grid_str ) srfdep,srfdos

integer   srfgrd(MAXSG,2)
real      srfdat(MAXSG,MAXSTYP)
character*4 srfnam(MAXSTYP)


end module common_srf
