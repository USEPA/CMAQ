!*******************************************************************************
!$RCSfile: stimet_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module stimet_inc
save

type  sTimeT
  sequence
          integer Year
          integer Month
          integer Day
          real  Hour
end type  sTimeT
end module stimet_inc
