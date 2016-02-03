!*******************************************************************************
!$RCSfile: class_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module class_inc
!------ Material class Names

  character*4 MAT_GAS,MAT_PRT

  parameter (MAT_GAS  ='GAS ') !Gas materials
  parameter (MAT_PRT  ='PART') !Particle materials

!------ Material class IDs and flags

  integer   MATID_GAS,MATID_PRT

  integer   MATID_HAZARD,MATID_MULTI

  parameter (MATID_GAS  = 0) !Gas materials
  parameter (MATID_PRT  = 1) !Particle materials
  parameter (MATID_HAZARD  = 7) !Met uncertainty / Hazard flag
  parameter (MATID_MULTI   = 8) !Multicomponent

end module class_inc
