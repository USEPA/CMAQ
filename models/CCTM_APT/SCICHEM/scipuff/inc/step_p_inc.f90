!*******************************************************************************
!$RCSfile: step_p_inc.f90,v $
!$Revision: 1.2 $
!$Date: 2007/01/05 22:33:33 $
!*******************************************************************************
module step_p_inc
save

! --- Local variables for STEP_P routines

real HSCALE, WMX, ZIFAC, SZFAC, ZIDEL
parameter (HSCALE  = 8000.    ) ! Atmospheric scale height
parameter (WMX     = 0.8      ) ! Well-mixed criterion sz/zinv
parameter (ZIFAC   = 1.1      ) ! Zi-split criterion (delta Zi)
parameter (SZFAC   = 3.0      ) ! Zi-split criterion (height)
parameter (ZIDEL   = 10.0     ) ! Zi-split criterion (height)

real xmap, ymap, hp, hx, hy, area_fac, sz, zbar
real csav, xsav, ysav, zsav, hsav, hxsav, hysav, zisav, ztop, zlim
real uubt, vvbt, uvbt, sly
real si, si2, sv, qosi_cc
real rhod, vfall, sigvd, vdry, vdtot, cmin2
real xuct, xvct, yvct, zwct, tauc, taur, fwash
real siq, svq, aqoszt, aqosyt, qs
real aqsosyt, aqbosyt, aqlosyt
real wpuff, vel2
real aspect, ztest
real dts, dtmc
real udyn, vdyn, wdyn, tdyn, bdyn, ddyn, fdyn
real tauw, difp, gt0, bv, gamma, ti
real tscale, qqfl, qqfb, qqfsh
real fsplit

integer imat, nsplit

logical lzinv, lsrf, ldos, ltot, ldense, lcap, lblcap
logical lscale, hazflag

end module step_p_inc
