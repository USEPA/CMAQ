# WRF-CMAQ Coupled Model

[David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency

## Brief Description

The new coupled WRF-CMAQ model which is based on WRF 4.3 and CMAQ 5.3.3, is an online meteorology-chemistry model that simulates the feedback between meteorology and chemistry. The feedback focuses on the interactions of estimated aerosol mass on incoming shortwave radiation. 

More information about the coupled model is provided in [Chapter 13 of the User's Guide](../Users_Guide/CMAQ_UG_ch13_WRF-CMAQ.md).


## Significance and Impact

The new release includes changes to the build process and runtime process of WRF-CMAQ, making it easier for users to obtain and run the latest WRF-CMAQ coupled model. 

## Affected Files

New Files:
* UTIL/wrfcmaq_twoway_coupler/Makefile
* UTIL/wrfcmaq_twoway_coupler/Registry/Registry.EM
* UTIL/wrfcmaq_twoway_coupler/Registry/registry.WRF-CMAQ-twoway
* UTIL/wrfcmaq_twoway_coupler/Registry/registry.em_shared_collection
* UTIL/wrfcmaq_twoway_coupler/arch/Config.pl
* UTIL/wrfcmaq_twoway_coupler/assemble
* UTIL/wrfcmaq_twoway_coupler/clean
* UTIL/wrfcmaq_twoway_coupler/configure
* UTIL/wrfcmaq_twoway_coupler/dyn_em/module_first_rk_step_part1.F
* UTIL/wrfcmaq_twoway_coupler/dyn_em/solve_em.F
* UTIL/wrfcmaq_twoway_coupler/dyn_nmm/module_PHYSICS_CALLS.F
* UTIL/wrfcmaq_twoway_coupler/external/makefile
* UTIL/wrfcmaq_twoway_coupler/main/Makefile
* UTIL/wrfcmaq_twoway_coupler/main/depend.common
* UTIL/wrfcmaq_twoway_coupler/phys/Makefile
* UTIL/wrfcmaq_twoway_coupler/phys/complex_number_module.F
* UTIL/wrfcmaq_twoway_coupler/phys/module_ra_rrtmg_sw.F
* UTIL/wrfcmaq_twoway_coupler/phys/module_radiation_driver.F
* UTIL/wrfcmaq_twoway_coupler/phys/module_sf_noahdrv.F
* UTIL/wrfcmaq_twoway_coupler/phys/module_twoway_ra_rrtmg_sw.F
* UTIL/wrfcmaq_twoway_coupler/phys/module_twoway_rrtmg_aero_optical_util.F
* CCTM/scripts/run_cctm_Bench_2016_12SE1.WRFCMAQ.csh
* UTIL/wrfcmaq_twoway_coupler/README.md

Files Modified: 
* CCTM/scripts/bldit_cctm.csh
* DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_WRF-CMAQ_build_gcc.md
* DOCS/Users_Guide/CMAQ_UG_ch13_WRF-CMAQ.md
* DOCS/Release_Notes/Coupled_WRF-CMAQ.md
* UTIL/README.md

# Internal Records
#### Relevant Pull Requests:


-----------------------
