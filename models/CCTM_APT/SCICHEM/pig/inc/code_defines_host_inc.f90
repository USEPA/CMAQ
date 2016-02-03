module code_defines_inc

  ! - parameters used to indicate that this is the host model version
  ! - i.e., CODEPARAM is equal to HOSTMODEL

  real, parameter :: CODEPARAM  = 1.0 !used in dump_pig to reset cgrid properly
  real, parameter :: CODEPARAM2 = 0.0 ! "
  real, parameter :: HOSTMODEL  = 1.0
  real, parameter :: STDAMODEL  = 0.0

end module code_defines_inc
