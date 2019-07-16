# Updates to the ICON/BCON Preprocessors

[Shawn Roselle](mailto:roselle.shawn@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Several bug fixes have been addressed in the ICON and BCON updates, including:

1. compiler info issues in scripts for ICON and BCON

2. ICON and BCON compilation problems in v5.3 resulting from changes in files symbolically linked 
to CCTM/src

An enhancement included in these updates allows for nesting from polar
stereographic projection grids.

The preprocessors have also been simplified to:

1. enable selection of IC/BC type (profile, regrid, tracer) as a run-time option instead of a compile-time option

2. remove the species mapping; users will need to use the *combine* utility for species mapping

3. remove the parallel processing code from ICON (feature no longer used/needed).

The updates also included new profile data from an annual average HCMAQ simulation
for a marine remote grid cell over the Pacific Ocean (37N, 157W)
from the OAQPS 2016 HCMAQ CMAQv5.2.1 *CB6R3_AE6NVPOA_AQ* simulation.



## Significance and Impact
ICON and BCON can now be used to generate ICs/BCs from HCMAQ simulations
using vertical and horizontal interpolation methods already available in 
ICON and BCON.

The profile data being released with CMAQv5.3 differs from previous 
model releases. The profile data included in this release is based on
a HCMAQ simulation that used chemical mechanism *CB6R3_AE6NVPOA_AQ*.
ICON and BCON will no longer map profile data species to different chemical 
mechanisms. Users will need to use the *combine* utility on ICON and BCON output
for species mapping to a different mechanism.


## Affected Files
#### Files modified:
PREP/bcon/scripts/bldit_bcon.csh  
PREP/bcon/scripts/run_bcon.csh  
PREP/bcon/src/common/VGRD_DEFN.F  
PREP/bcon/src/common/bcon.F  
PREP/bcon/src/common/lat_lon.F  
PREP/bcon/src/common/opn_bc_file.F  
PREP/bcon/src/m3conc/m3_bcout.F  
PREP/bcon/src/m3conc/m3_driver.F  
PREP/bcon/src/m3conc/m3_vinterp.F  
PREP/bcon/src/profile/prof_bcout.F  
PREP/bcon/src/profile/prof_driver.F  
PREP/bcon/src/profile/prof_vinterp.F  
PREP/bcon/src/tracer/trac_bc.F  
PREP/bcon/src/tracer/trac_driver.F  
PREP/icon/scripts/bldit_icon.csh  
PREP/icon/scripts/run_icon.csh  
PREP/icon/src/common/HGRD_DEFN.F  
PREP/icon/src/common/IC_PARMS.F  
PREP/icon/src/common/VGRD_DEFN.F  
PREP/icon/src/common/icon.F  
PREP/icon/src/common/lat_lon.F  
PREP/icon/src/common/lr_interp.F  
PREP/icon/src/common/opn_ic_file.F  
PREP/icon/src/m3conc/m3_ck_ctmmet.F  
PREP/icon/src/m3conc/m3_ck_ctms.F  
PREP/icon/src/m3conc/m3_ck_icmet.F  
PREP/icon/src/m3conc/m3_driver.F  
PREP/icon/src/m3conc/m3_icout.F  
PREP/icon/src/m3conc/m3_vinterp.F  
PREP/icon/src/profile/prof_driver.F  
PREP/icon/src/profile/prof_icout.F  
PREP/icon/src/profile/prof_vinterp.F  
PREP/icon/src/tracer/trac_driver.F  
PREP/icon/src/tracer/trac_ic.F  
#### Files added:
PREP/bcon/src/profile/avprofile_cb6r3aero6_hemi2016_col051_row068.csv  
PREP/bcon/src/profile/legacy_PM_toxics_profile.csv  
PREP/icon/src/profile/avprofile_cb6r3aero6_hemi2016_col051_row068.csv  
PREP/icon/src/profile/legacy_PM_toxics_profile.csv  
#### Files deleted:
PREP/bcon/src/common/CGRID_SPCS.F  
PREP/bcon/src/common/UTILIO_DEFN.F  
PREP/bcon/src/common/gc_spc_map.F  
PREP/bcon/src/common/lst_spc_map.F  
PREP/bcon/src/common/ngc_spc_map.F  
PREP/bcon/src/mech_conv/Common_Codes  
PREP/bcon/src/mech_conv/bc_profile_RADM2_AERO5.dat  
PREP/bcon/src/mech_conv/convdat.F  
PREP/bcon/src/mech_conv/radm2_to_cb05/Makefile  
PREP/bcon/src/mech_conv/radm2_to_cb05/cb05_table.f  
PREP/bcon/src/mech_conv/radm2_to_cb05/mech_table.ext  
PREP/bcon/src/mech_conv/radm2_to_cb05/out_fl_name.ext  
PREP/bcon/src/mech_conv/radm2_to_racm2/Makefile  
PREP/bcon/src/mech_conv/radm2_to_racm2/mech_table.ext  
PREP/bcon/src/mech_conv/radm2_to_racm2/out_fl_name.ext  
PREP/bcon/src/mech_conv/radm2_to_racm2/racm2_table.f  
PREP/bcon/src/mech_conv/radm2_to_saprc07t/Makefile  
PREP/bcon/src/mech_conv/radm2_to_saprc07t/mech_table.ext  
PREP/bcon/src/mech_conv/radm2_to_saprc07t/out_fl_name.ext  
PREP/bcon/src/mech_conv/radm2_to_saprc07t/saprc07t_table.f  
PREP/bcon/src/mech_conv/radm2_to_saprc99/Makefile  
PREP/bcon/src/mech_conv/radm2_to_saprc99/mech_table.ext  
PREP/bcon/src/mech_conv/radm2_to_saprc99/out_fl_name.ext  
PREP/bcon/src/mech_conv/radm2_to_saprc99/saprc99_table.f  
PREP/bcon/src/mech_conv/wrdate.f  
PREP/bcon/src/prof_data/cb05_ae6_aq/bc_profile_CB05.dat  
PREP/bcon/src/prof_data/racm2_ae6_aq/bc_profile_RACM2.dat  
PREP/bcon/src/prof_data/saprc07t_ae6_aq/bc_profile_SAPRC07T.dat  
PREP/bcon/src/prof_data/saprc99_ae6_aq/bc_profile_SAPRC99.dat  
PREP/icon/src/common/CGRID_SPCS.F  
PREP/icon/src/common/UTILIO_DEFN.F  
PREP/icon/src/common/gc_spc_map.F  
PREP/icon/src/common/lst_spc_map.F  
PREP/icon/src/common/ngc_spc_map.F  
PREP/icon/src/common/setup_logdev.F  
PREP/icon/src/common/subhdomain.F  
PREP/icon/src/m3conc/mapping_init.F  
PREP/icon/src/mech_conv/Common_Codes  
PREP/icon/src/mech_conv/convdat.F  
PREP/icon/src/mech_conv/ic_profile_RADM2_AERO5.dat  
PREP/icon/src/mech_conv/radm2_to_cb05/Makefile  
PREP/icon/src/mech_conv/radm2_to_cb05/cb05_table.f  
PREP/icon/src/mech_conv/radm2_to_cb05/mech_table.ext  
PREP/icon/src/mech_conv/radm2_to_cb05/out_fl_name.ext  
PREP/icon/src/mech_conv/radm2_to_racm2/Makefile  
PREP/icon/src/mech_conv/radm2_to_racm2/conv.log  
PREP/icon/src/mech_conv/radm2_to_racm2/convdat.F  
PREP/icon/src/mech_conv/radm2_to_racm2/mech_table.ext  
PREP/icon/src/mech_conv/radm2_to_racm2/out_fl_name.ext  
PREP/icon/src/mech_conv/radm2_to_racm2/racm2_table.f  
PREP/icon/src/mech_conv/radm2_to_racm2/wrdate.f  
PREP/icon/src/mech_conv/radm2_to_saprc07t/Makefile  
PREP/icon/src/mech_conv/radm2_to_saprc07t/mech_table.ext  
PREP/icon/src/mech_conv/radm2_to_saprc07t/out_fl_name.ext  
PREP/icon/src/mech_conv/radm2_to_saprc07t/saprc07t_table.f  
PREP/icon/src/mech_conv/radm2_to_saprc99/Makefile  
PREP/icon/src/mech_conv/radm2_to_saprc99/mech_table.ext  
PREP/icon/src/mech_conv/radm2_to_saprc99/out_fl_name.ext  
PREP/icon/src/mech_conv/radm2_to_saprc99/saprc99_table.f  
PREP/icon/src/mech_conv/wrdate.f  
PREP/icon/src/par/distr_env.c  
PREP/icon/src/par/mpcomm_init.F  
PREP/icon/src/par/par_term.F  
PREP/icon/src/par/reconfig_domain.F  
PREP/icon/src/par/shift_map.F  
PREP/icon/src/prof_data/cb05_ae6_aq/ic_profile_CB05.dat  
PREP/icon/src/prof_data/racm2_ae6_aq/ic_profile_RACM2.dat  
PREP/icon/src/prof_data/saprc07t_ae6_aq/ic_profile_SAPRC07T.dat  
PREP/icon/src/prof_data/saprc99_ae6_aq/ic_profile_SAPRC99.dat  


## References
NA           

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #423](https://github.com/USEPA/CMAQ_Dev/pull/423)  

#### Commit IDs:
[3a5cd7154d95afdfe1f484a2129ba2597b2e9d9c](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/3a5cd7154d95afdfe1f484a2129ba2597b2e9d9c)  
[ae51bdb1fe6bcfd96ecba54ac03828753223ca0a](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/ae51bdb1fe6bcfd96ecba54ac03828753223ca0a)  
[95e3fbabe3006860eae1991cd11a5d72e8dc0402](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/95e3fbabe3006860eae1991cd11a5d72e8dc0402)  
[211149949f12c4bcd17d90ed36eee2ce7ec565e9](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/211149949f12c4bcd17d90ed36eee2ce7ec565e9)  
[d8118ef125d922efef22ce38b4b16596050ebb0d](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/d8118ef125d922efef22ce38b4b16596050ebb0d)  
[f53e013357702b8a638b30fb356d4b01244e143c](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/f53e013357702b8a638b30fb356d4b01244e143c)  
[6a8f1468e56101e14c54b43299093b5aa31c8289](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/6a8f1468e56101e14c54b43299093b5aa31c8289)  
[e7d5dc0daaa25b58b0b85ab04130a645179411f3](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/e7d5dc0daaa25b58b0b85ab04130a645179411f3)  
[0a8eb0576c0c9d7a4850472c3306818fb20ada9d](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/0a8eb0576c0c9d7a4850472c3306818fb20ada9d)  
[a63e1e8a639dc8f30a2eda72e27bffaa01be11aa](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/a63e1e8a639dc8f30a2eda72e27bffaa01be11aa)  
[86f2aeedd9bf482e8ce40c1041be8bac230b7ab6](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/86f2aeedd9bf482e8ce40c1041be8bac230b7ab6)  
[f44518953e6a919841ff2d06b7974248b32ab4c9](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/f44518953e6a919841ff2d06b7974248b32ab4c9)  
[f4c66ade91efff224d6f1fbeccb4fd04b2936b23](https://github.com/USEPA/CMAQ_Dev/pull/423/commits/f4c66ade91efff224d6f1fbeccb4fd04b2936b23)  

-----
