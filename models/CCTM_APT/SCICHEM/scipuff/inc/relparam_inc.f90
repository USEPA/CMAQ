!*******************************************************************************
!$RCSfile: relparam_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module relparam_inc
      integer  REL_MMD_INDX
      parameter (REL_MMD_INDX = 1 )
      integer  REL_SIGMA_INDX
      parameter (REL_SIGMA_INDX = 2 )
      integer  REL_RAND_INDX
      parameter (REL_RAND_INDX = 3 )
      integer  REL_SPREAD_INDX
      parameter (REL_SPREAD_INDX = 4 )
      integer  REL_SEED_INDX
      parameter (REL_SEED_INDX = 5 )
      integer  REL_H_UNC_INDX
      parameter (REL_H_UNC_INDX = 6 )
      integer  REL_V_UNC_INDX
      parameter (REL_V_UNC_INDX = 7 )
end module relparam_inc
