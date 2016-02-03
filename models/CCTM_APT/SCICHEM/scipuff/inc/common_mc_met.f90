module common_mc_met

  save
  integer nxyb                  !Met grid dimensions

  real  cldall
  real  cldallt, cldallp

  real  fcc                     !Cloud cover
  real  fprcpc                  !fraction of convective precipitation

  real  cmasst          ! mass of cloud water in column containing puff
  real  cmassp          ! mass of precipitation water in column containing puff

  real  tb, pb, tab, hb         !Temp, etc at get_met point
  real  pratebl, prbl           !Precip

  real  zruf, h_cnp, ustdep     !Landuse params

  real  ddepspec, wdepspec
  real  us2, ws2                !u*-squared and w*-squared
  real  radyn                   !aerodynamic resistance (s/m)
  real  wcbar                   !average cloud lwc (g/m3)

end module common_mc_met
