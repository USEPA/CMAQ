!*******************************************************************************
!$RCSfile: metintrp_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module metintrp_inc
save

!------ get_met interpolating structure

type  met1dh
  sequence

  integer i
  real dxr, rat, rm1

end type  met1dh

type  meth
  sequence

  integer ij, nxyi, nxi

  real dxr, dyr
  real ratx, rxm1
  real raty, rym1
  real cc1, cc2, cc3, cc4

end type  meth

type  metv
  sequence

  integer km, kp
  real    dzr, ratz, rzm1
  real    facp, facm

end type  metv
end module metintrp_inc
