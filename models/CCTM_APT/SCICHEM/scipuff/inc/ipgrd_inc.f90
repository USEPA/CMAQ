!*******************************************************************************
!$RCSfile: ipgrd_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module ipgrd_inc
save

integer   MAXG, MAXGZ

parameter (MAXG     =    4000)  !Maximum Number of Horiz. grids
parameter (MAXGZ    =      50)  !Maximum Number of Vertical grids

integer   ipgrd(MAXG,2,MAXGZ)
integer   npgrd(MAXGZ)
integer   mxlev_grd(MAXGZ)
integer   mxgrd

end module ipgrd_inc
