module common_mc_puf

  use param_inc
  use constants_fd
  use default_inc
  use struct_inc
  use error_inc

  save
 
  integer nspuf

  real    t          !Current time after start of run (secs)
  logical lwash      !Washout

end module common_mc_puf
