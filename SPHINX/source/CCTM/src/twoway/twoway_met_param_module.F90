!===============================================================================
! Purpose:  Define general information in each of the buffer files
!
! Revised:  April 2007  Original version.  David Wong
!           July, 16 2013  David Wong -- corrected the unit for RA and RS
!                                     -- added DLUSE in GRIDCRO2D
!           Jan, 11 2016  David Wong -- added a new variable PV
!           Mar 04, 2019  Gilliam and Wong -- added new metcro2d variables 
!                         according to new PX implementation in WRFv4.1+
!           Aug 01, 2019  Wong -- added two new metdot3d variables, UWIND and
!                                 VWIND (wind component of the mass point)
!===============================================================================

  module twoway_met_param_module

    INTEGER, PARAMETER :: max_nvars       = 1000
    INTEGER, PARAMETER :: n_gridcro2d_var = 7
    INTEGER, PARAMETER :: n_griddot2d_var = 1
    INTEGER, PARAMETER :: n_metcro3d_var  = 17
    INTEGER, PARAMETER :: n_metdot3d_var  = 4
    INTEGER, PARAMETER :: n_metcro2d_var  = 38

    CHARACTER (LEN = 16), PARAMETER :: gridcro2d_vlist(n_gridcro2d_var) = &
      (/ 'LAT             ', 'LON             ',            &
         'MSFX2           ', 'HT              ',            &
         'LWMASK          ', 'PURB            ',            &
         'DLUSE           '                      /)

    CHARACTER (LEN = 16), PARAMETER :: gridcro2d_units(n_gridcro2d_var) = &
      (/ 'DEGREES         ', 'DEGREES         ',            &
         '(M/M)**2        ', 'M               ',            &
         '-               ', 'PERCENT         ',            &
         'CATEGORY        '                      /)

    CHARACTER (LEN = 16), PARAMETER :: griddot2d_vlist(n_griddot2d_var) = &
      (/ 'MSFD2           '                      /)

    CHARACTER (LEN = 16), PARAMETER :: griddot2d_units(n_griddot2d_var) = &
      (/ '(M/M)**2        '                      /)

    CHARACTER (LEN = 16), PARAMETER :: metcro3d_vlist(n_metcro3d_var) = &
      (/ 'JACOBF          ', 'JACOBM          ',            &
         'DENSA_J         ', 'TA              ',            &
         'QV              ', 'QC              ',            &
         'QR              ', 'QI              ',            &
         'QS              ', 'QG              ',            &
         'PRES            ', 'DENS            ',            &
         'ZH              ', 'ZF              ',            &
         'UWIND           ', 'VWIND           ',            &
         'PV              '                      /)

    CHARACTER (LEN = 16), PARAMETER :: metcro3d_units(n_metcro3d_var) = &
      (/ 'M               ', 'M               ',            &
         'KG/M**2         ', 'K               ',            &
         'KG/KG           ', 'KG/KG           ',            &
         'KG/KG           ', 'KG/KG           ',            &
         'KG/KG           ', 'KG/KG           ',            &
         'Pa              ', 'KG/M**3         ',            &
         'M               ', 'M               ',            &
         'M/S             ', 'M/S             ',            &
         'M^2*K/KG/S * E-6'                      /)

    CHARACTER (LEN = 16), PARAMETER :: metdot3d_vlist(n_metdot3d_var) =  &
      (/ 'UWINDC          ', 'VWINDC          ',            &
         'UHAT_JD         ', 'VHAT_JD         '  /)
  
    CHARACTER (LEN = 16), PARAMETER :: metdot3d_units(n_metdot3d_var) =  &
      (/ 'M/S             ', 'M/S             ',            &
         'KG/(M*S)        ', 'KG/(M*S)        '  /)

    CHARACTER (LEN = 16), PARAMETER :: metcro2d_vlist(n_metcro2d_var) = &
      (/ 'PRSFC           ', 'USTAR           ',            &
         'WSTAR           ', 'PBL             ',            &
         'ZRUF            ', 'MOLI            ',            &
         'HFX             ', 'RA              ',            &
         'RS              ', 'WSPD10          ',            &
         'GSW             ', 'RGRND           ',            &
         'RNA             ', 'RCA             ',            &
         'CFRAC           ', 'CLDT            ',            &
         'CLDB            ', 'WBAR            ',            &
         'SNOCOV          ', 'VEG             ',            &
         'TEMP2           ', 'WR              ',            &
         'TEMPG           ', 'LAI             ',            &
         'SLTYP           ', 'Q2              ',            &
         'SEAICE          ', 'SOIM1           ',            &
         'SOIM2           ', 'SOIT1           ',            &
         'SOIT2           ', 'LH              ',            &
         'WWLT_PX         ', 'WFC_PX          ',            &
         'WSAT_PX         ', 'CLAY_PX         ',            &
         'CSAND_PX        ', 'FMSAND_PX       ' /)

    CHARACTER (LEN = 16), PARAMETER :: metcro2d_units(n_metcro2d_var) = &
      (/ 'Pascal          ', 'M/S             ',            &
         'M/S             ', 'M               ',            &
         'M               ', '1/M             ',            &
         'WATTS/M**2      ', 'S/M             ',            &
         'S/M             ', 'M/S             ',            &
         'WATTS/M**2      ', 'WATTS/M**2      ',            &
         'CM              ', 'CM              ',            &
         'FRACTION        ', 'M               ',            &
         'M               ', 'G/M**3          ',            &
         'NODIM           ', 'NO UNIT         ',            &
         'K               ', 'M               ',            &
         'K               ', 'AREA/AREA       ',            &
         '-               ', 'KG/KG           ',            &
         'FRACTION        ', 'M**3/M**3       ',            &
         'M**3/M**3       ', 'K               ',            &
         'K               ', 'WATTS/M**2      ',            &
         'M**3/M**3       ', 'M**3/M**3       ',            &
         'M**3/M**3       ', 'FRACTION        ',            &
         'FRACTION        ', 'FRACTION        ' /)

end module twoway_met_param_module
