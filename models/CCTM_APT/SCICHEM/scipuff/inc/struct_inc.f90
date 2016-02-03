!*******************************************************************************
!$RCSfile: struct_inc.f90,v $
!$Revision: 1.4 $
!$Date: 2007/02/06 23:33:01 $
!
! Updated Jan 2007 to change size of mc array to MAX_MC2
!*******************************************************************************
module struct_inc
use param_inc
save

type  puff_str
  sequence
real    xbar,ybar,zbar
real    sxx,sxy,sxz,syy,syz,szz
real    axx,axy,axz,ayy,ayz,azz,det
real    c,cc,xuc,xvc,yvc,yvsc,yvbc,zwc,wc,ccb
real    si,si2,sv
real    sr,cfo
real    zi,zc
real    uo,vo,wo
integer ityp,inxt,iprv,ipgd,idtl,idtn,iaux
end type  puff_str

!-------------------------------------------------------------------------------

type  puff_str_ri
  sequence
real    p_real(NP_REAL)
integer p_int(NP_INT)
end type  puff_str_ri

!-------------------------------------------------------------------------------

type  type_str
  sequence

integer imat    !Material Pointer
integer igrp    !SubGroup Pointer
integer npaux   !No.Puff Auxiliary data
integer nmc     !No.Puff Multi-Component
integer ipmc    !Multi-Component Offset
integer icls    !Puff Class

logical   ltot  !Total variance flag

end type  type_str

!-------------------------------------------------------------------------------

type  material_str
  sequence

integer   icls  !Class index
integer   iaux  !Matl Auxiliary pointer
integer   nmc   !Matl No. Multi-Comp
integer   ipmc  !Matl Multi-Comp pointer
integer   ioffp !Puff ityp Offset
integer   ioffs !Srf Deposition Offset
integer   ioffd !Srf Dose Offset
integer   ioffs_mc      !Multi-Comp Srf Deposition Offset
integer   ioffd_mc      !Multi-Comp Srf Dose Offset

logical   lsrfg !Srf Deposition by group
logical   lsrft !Srf Deposition total
logical   ldosg !Srf Dose by group
logical   ldost !Srf Dose total
logical   lsrf_mc       !Mutli-Comp Srf Deposition total
logical   ldos_mc       !Multi-Comp Srf Dose total

real   prop(10) !decay parameters

character*16 cmat       !Material Name
character*16 ccls       !Class Name
character*16 unit       !Mass units
character*16 file       !Material file name
character*64 path       !Material file path

end type  material_str

!-------------------------------------------------------------------------------

type  gas_material
  sequence

integer nsg  !Dummy integer
real    rho  !Density
real    vd   !Deposition velocity

real    param(MAXPMAUX)

end type  gas_material

!-------------------------------------------------------------------------------

type  part_material
  sequence

integer nsg     !No. of subgroups
real    rho     !Density
real    dmin    !Min diameter
real    dbar    !Mean diameter
real    vd      !Fall speed
real    sigvd   !Sigma speed
real    diff    !Brownian diffusion
real    dmax    !Max diameter

end type  part_material

!-------------------------------------------------------------------------------

type  puff_material
  sequence

integer nsg               !No. of subgroups
real    param(MAXPMAUX+2) !Particle since it is biggest

end type  puff_material

!-------------------------------------------------------------------------------

type  puff_dynamics
  sequence

real wcb!<wbar*cbar>
real ctb!<cbar*tbar>
real wcp!<wc>
real ctp!<ct>
real w  !<w>
real t  !<theta>
real bcb!<bbar*cbar> - buoyancy from gas density
real bcp!<bc>
real u  !Dense gas dynamic u-velocity
real v  !Dense gas dynamic v-velocity
real dudx       !Dense gas dynamic vel gradients
real dudy
real dvdx
real dvdy
real u0 !Dense gas velocity scale
real X  !Major axis
real Y  !Minor axis
real sn !Rotation matrix coefficients
real cs !Rotation matrix coefficients

end type  puff_dynamics

!-------------------------------------------------------------------------------

type  puff_totalcc
  sequence

real cctb
real cct

end type  puff_totalcc

!-------------------------------------------------------------------------------

type  puff_static
  sequence

real    sr
integer isnxt
integer isprv

end type  puff_static

!-------------------------------------------------------------------------------

type  puff_mc
  sequence

  real mc(MAX_MC2)

end type  puff_mc

end module struct_inc
