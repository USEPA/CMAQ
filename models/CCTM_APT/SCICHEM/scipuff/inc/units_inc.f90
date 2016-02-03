!*******************************************************************************
!$RCSfile: units_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module units_inc
  integer   UNIT_PPM !PPM concentration units
  integer   UNIT_MOLECULE !molecule/cm3 concentration units
  integer   UNIT_G !g/m3 concentration units
  parameter  (UNIT_PPM      = 0)
  parameter  (UNIT_MOLECULE = 1)
  parameter  (UNIT_G        = 2)

end module units_inc
