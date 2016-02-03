!*******************************************************************************
!$RCSfile: common_smp.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module common_smp
use param_inc
save

integer     MAXSMP
integer     MAXSMPV
parameter   (MAXSMP  = 10000)                !Max no. of samplers
parameter   (MAXSMPV = 4+MAX_MC*MAXSMP/2)

real xsmp(MAXSMP), ysmp(MAXSMP), zsmp(MAXSMP)  !Sampler locations
real dsmp(MAXSMPV)                             !Sampler values

logical      lmcsmp                     !Multi-component sampler output

integer      nsmp                       !No. of samplers
integer      nvarsmp                    !Total no.of output variables
integer      itys, itye                 !Range of puffs for sampler output


integer      isg                        !Subgroup for output
character*16 matname                    !Material for output

end module common_smp
