!*******************************************************************************
!$RCSfile: cont_rel_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!
! Feb 2015, PKK, ENVIRON: Increase no of active releases
!*******************************************************************************
module cont_rel_inc

save

!------ include file for cont_rel.f

integer  MAX_ACTIVE_CREL
parameter (MAX_ACTIVE_CREL=5000)

logical lrupdate

real    c_start(MAX_ACTIVE_CREL)
real    c_end  (MAX_ACTIVE_CREL)
real    c_plen (MAX_ACTIVE_CREL)
real    c_dt   (MAX_ACTIVE_CREL)
real    c_dtr  (MAX_ACTIVE_CREL)
real    c_stat (MAX_ACTIVE_CREL)
integer c_lev  (MAX_ACTIVE_CREL)
integer c_opid (MAX_ACTIVE_CREL)
integer c_saux (MAX_ACTIVE_CREL)

end module cont_rel_inc
