# OMI through 2019

[Barron Henderson](mailto:henderson.barron@epa.gov), U.S. Environmental Protection Agency

## Brief Description

This change replaces the OMI_1979_to_2017.dat with the OMI_1979_to_2019.dat file and updates cshell scripts that use it.

## Significance and Impact

It allows modeling covering year 2019. The file is consistent with the previous file except for the last 10 days in 
the old file, where there are less than 1% differences in a few cells.

## Affected Files

* CCTM/scripts/run_cctm_2010_4CALIF1.csh 
* CCTM/scripts/run_cctm_2011_12US1.csh 
* CCTM/scripts/run_cctm_2014_12US1.csh
* CCTM/scripts/run_cctm_2015_HEMI.csh
* CCTM/scripts/run_cctm_Bench_2011_12SE1.csh 
* CCTM/scripts/run_cctm_Bench_2016_12SE1.csh
* CCTM/src/phot/inline/OMI_1979_to_2017.dat modified and renamed CTM/src/phot/inline/OMI_1979_to_2019.dat 


# Internal Records
#### Relevant Pull Requests:
[PR # 626](https://github.com/usepa/cmaq_dev/pull/626)

#### Commit IDs:  
5f437c7894960523a140f0cfc76240f46c2405b3  


-----------------------
