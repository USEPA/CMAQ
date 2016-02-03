!*******************************************************************************
!$RCSfile: srfparam_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module srfparam_inc
!----- Surface field pointers / parameters

! ---  Field variable pointers

integer  ISRF_C
parameter (ISRF_C = 1)  !Mean conc. pointer

integer  ISRF_CC
parameter (ISRF_CC = 2) !Group variance pointer

integer  ISRF_SL
parameter (ISRF_SL = 3) !Scale (*variance) pointer

integer  ISRF_CCT
parameter (ISRF_CCT = 4)!Total variance pointer

integer  ISRF_C0
parameter (ISRF_C0 = 5) !Max conc

! ---  Field counters

integer  NV_BASIC
parameter (NV_BASIC = 3)!Standard (Mean,var,scale)

end module srfparam_inc
