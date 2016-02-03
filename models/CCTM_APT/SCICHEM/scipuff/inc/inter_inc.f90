!*******************************************************************************
!$RCSfile: inter_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module inter_inc
use param_inc
save

!------ include file for inter.f

real det, d33, x1, y1, z1, xr(3)
real dgrd, xp, yp, zp, h, hx, hy
real xx, yy, delx, dely, delz
real fac, facc
real hsmin, xmap, ymap, zp1, r_ipuf
real asig(7), bsig(7), hp, hxp, hyp
real betx, bety, betz, zp2, r_jpuf
real b33, s33, g_ipuf, g_jpuf, z_ipuf, z_jpuf, z_kpuf
real r_kpuf, facr, den
real facp, facn, facw, facwp, faccp, ctot, ctotp
real ptmp(MAXPUF)

integer iovlp, iprv, irfrst, irlast, nrlist
integer ip, ku(MAXPUF), kl(MAXPUF)

logical lprocess,lsame,lstatic,lrfl_ipuf,lrfl_jpuf
logical ltyp, lmat, ltot, ldyni, ldynj, lmc
logical lhazi, lhazj

end module inter_inc
