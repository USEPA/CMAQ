# KZMIN setting update

[David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency

## Brief Description
As described in [Appendix A](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#science-options), the runtime variable 'KZMIN' may be set to Y/N to control the minimum eddy diffusivity in each grid cell. Depending on the option set (Y/N), CCTM may or may not require percent urban land-use fraction (PURB) data from the GRID_CRO_2D meteorology file to calculate the minimum eddy diffusivity in each grid cell. Recent updates broke this design, causing CCTM to always require PURB data from the GRID_CRO_2D meteorology file, regardless of KZMIN setting. Hence, even if KZMIN was appropriately set, the model would crash if PURB data was not available. 

Routine CIO within CCTM was updated to not read PURB data from the GRID_CRO_2D meteorology file if the correct KZMIN setting was set. If, the PURB data set is not available in the GRID_CRO_2D meteorology file, PURB is assumed to be 0.0 to allow other dependecies to be functional. 

## Significance and Impact  
Both minimum eddy diffusivity in each grid cell and production of heterogenous Nitrous Acid (HONO) from the interaction of NO2 on ground surfaces (controlled by runtime variable [CTM_SFC_HONO](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#6104-nitrous-acid-hono)) are dependent on PURB data. Due to the appropriate KZMIN setting, if PURB data is not read and assumed to be 0.0, users should expect lower predicated HONO as described by [Chapter 6](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#6104-nitrous-acid-hono).

## Affected Files
CCTM/scripts/run_cctm_2010_4CALIF1.csh

CCTM/scripts/run_cctm_2011_12US1.csh

CCTM/scripts/run_cctm_2014_12US1.csh

CCTM/scripts/run_cctm_2015_HEMI.csh

CCTM/scripts/run_cctm_2016_12US1.csh

CCTM/scripts/run_cctm_Bench_2011_12SE1.csh

CCTM/scripts/run_cctm_Bench_2016_12SE1.csh

CCTM/src/cio/centralized_io_module.F

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #707](https://github.com/USEPA/CMAQ_Dev/pull/707)

-----
