module params_pri
  save
  include 'params.pri'
end module
module ambient_pri
  use params_pri
  save
  include 'ambient.pri'
end module
module dfsn_pri
  use params_pri
  save
  include 'dfsn.pri'
end module
module numparm_pri
  use params_pri
  save
  include 'numparm.pri'
end module
module wakedat_pri
  use params_pri
  save
  include 'wakedat.pri'
end module
