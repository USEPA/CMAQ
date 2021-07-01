# KZMIN setting update

[David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Bases on the setting of environment variale KZMIN to determine when to reading in
PURB (percentage of urban area) from a meteorological input file. Details can be
found in User Guide Ch6.

## Significance and Impact  
It impacts heterogeneous reaction in urban area. This also impacts HONO calculation
due to an interaction with other environment variable CTM_SFC_HONO (details can be
found in the User Guide Ch6.10.4.

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
