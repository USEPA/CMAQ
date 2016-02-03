!*******************************************************************************
!$RCSfile: get_met_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module get_met_inc
use metintrp_inc
save

!------ get_met include file

real hx, hy, hp, dp, zm, gx, gy, zsl, xml

logical lzinv, lbl_interp, lsl

type ( met1dh ) mx, mxu, my, myv
type ( meth   ) mxy
type ( metv   ) mz, mzw

end module get_met_inc
