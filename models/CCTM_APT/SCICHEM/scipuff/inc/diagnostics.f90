!*******************************************************************************
!$RCSfile: diagnostics.f90,v $
!$Revision: 1.3 $
!$Date: 2010/08/24 19:09:56 $
!*******************************************************************************
module diagnostics

!
!Aug 2010: Change cdump to allocatable array -BC(Sage-Mgt)
!

use param_inc
use multcomp_mc_inc
save

integer ndump, ndump_old, ndump_current
double precision emiss(MAX_MC), emiss_old(MAX_MC), emiss_current(MAX_MC)
double precision statics(MAX_MC), statics_old(MAX_MC), statics_current(MAX_MC)
double precision bndry(MAX_MC), bndry_old(MAX_MC), bndry_current(MAX_MC)
double precision trans(MAX_MC), trans_old(MAX_MC), trans_current(MAX_MC)
double precision ddepos(MAX_MC), ddepos_old(MAX_MC), ddepos_current(MAX_MC)
double precision wdepos(MAX_MC), wdepos_old(MAX_MC), wdepos_current(MAX_MC)
double precision chem(MAX_MC), chem_old(MAX_MC), chem_current(MAX_MC)
double precision active(MAX_MC)
double precision remvd(MAX_MC)
double precision ppmfac(MAX_MC)

real, dimension(:,:,:),allocatable :: cdump !save tracer dumped to host grid

end module diagnostics
