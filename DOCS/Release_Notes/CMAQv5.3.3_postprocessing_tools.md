# Updates to Post-processing Utilities

[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency

## Brief Description
*hr2day* was updated with two bug fixes for the specific scenarios described on the [CMAQv5.3.3 bug fix page](./CMAQv5.3.3_bugfixes.md).

*sitecmp_dailyo3* was updated with one bug fix for the specific scenario described on the [CMAQv5.3.3 bug fix page](./CMAQv5.3.3_bugfixes.md).

*hr2day* code and documentation were updated to remove the legacy `HROFFSET` option.

*calc_tmetric* was updated to enable faster processing of 3D files.

*combine*, *hr2day*, *sitecmp*, *sitecmp_dailyo3*, and *writesite* were updated to add support for grids using an equatorial mercator projection.

*writesite* documentation was updated to elaborate on the expected format for SITE_FILE.

All POST tool run scripts were updated with minor cosmetic changes.                  

## Significance and Impact
Aside from the bug fixes for specific application scenarios of *hr2day* and *sitecmp_dailyo3* described on the [CMAQv5.3.3 bug fix page](./CMAQv5.3.3_bugfixes.md), the changes made to the POST tools do not affect any results. The updates to *calc_tmetric* allow for faster processing of 3D files, and the updates to *combine*, *hr2day*, *sitecmp*, *sitecmp_dailyo3*, and *writesite* allow the processing of grids using an equatorial mercator projection.
                       

## Affected Files
POST/appendwrf/scripts/run_appendwrf.csh
POST/bldoverlay/scripts/run_bldoverlay.csh
POST/block_extract/scripts/run_block_extract.csh
POST/calc_tmetric/scripts/run_calc_tmetric.csh
POST/calc_tmetric/src/calc_tmetric.F
POST/calc_tmetric/src/module_tstep.F
POST/combine/scripts/run_combine.csh
POST/combine/src/module_file.F
POST/hr2day/README.md
POST/hr2day/scripts/run_hr2day.csh
POST/hr2day/src/hr2day.F
POST/hr2day/src/module_envvar.F
POST/sitecmp/scripts/run_sitecmp_AQS_Daily.csh
POST/sitecmp/scripts/run_sitecmp_AQS_Hourly.csh
POST/sitecmp/scripts/run_sitecmp_CSN.csh
POST/sitecmp/scripts/run_sitecmp_IMPROVE.csh
POST/sitecmp/scripts/run_sitecmp_NADP.csh
POST/sitecmp/scripts/run_sitecmp_SEARCH_Hourly.csh
POST/sitecmp/src/module_sites.F
POST/sitecmp_dailyo3/scripts/run_sitecmp_dailyo3_AQS.csh
POST/sitecmp_dailyo3/scripts/run_sitecmp_dailyo3_CASTNET.csh
POST/sitecmp_dailyo3/src/module_sites.F
POST/sitecmp_dailyo3/src/utilities.F
POST/writesite/README.md
POST/writesite/scripts/run_writesite.csh
POST/writesite/src/writesite.F

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #732](https://github.com/USEPA/CMAQ_Dev/pull/732)  

#### Commit IDs:

      13a39462c557077a4edcebfb31f41cdadd325e1d  (https://github.com/USEPA/CMAQ_Dev/commit/13a39462c557077a4edcebfb31f41cdadd325e1d)
      43baf685552f86a893952721d2cee97ec58b37fb  (https://github.com/USEPA/CMAQ_Dev/commit/43baf685552f86a893952721d2cee97ec58b37fb)
      bdde4f9847748fae2a832a12a2eee19dc3f8a44a  (https://github.com/USEPA/CMAQ_Dev/commit/bdde4f9847748fae2a832a12a2eee19dc3f8a44a)
      a78c58559828caf14c3493557ad08ddba019b071  (https://github.com/USEPA/CMAQ_Dev/commit/a78c58559828caf14c3493557ad08ddba019b071)
      363075e88e81bc5f9418a6a47fed1dc96817bece  (https://github.com/USEPA/CMAQ_Dev/commit/363075e88e81bc5f9418a6a47fed1dc96817bece)
      6f1b837fc4588d3ce1edb8be7ba6003b11d25ac9  (https://github.com/USEPA/CMAQ_Dev/commit/6f1b837fc4588d3ce1edb8be7ba6003b11d25ac9)
      8cc8ded67371aac67768fc46b2e7197f0a64d7ac  (https://github.com/USEPA/CMAQ_Dev/commit/8cc8ded67371aac67768fc46b2e7197f0a64d7ac)
      909d33a07d2584ca67cb63b914b44f1325ebe742  (https://github.com/USEPA/CMAQ_Dev/commit/909d33a07d2584ca67cb63b914b44f1325ebe742)
      6a024524819c09e34d8374eed8d794fe03e20a72  (https://github.com/USEPA/CMAQ_Dev/commit/6a024524819c09e34d8374eed8d794fe03e20a72)
      873903589e20c99f6f8c7b565374d70f07b57a8c  (https://github.com/USEPA/CMAQ_Dev/commit/873903589e20c99f6f8c7b565374d70f07b57a8c)
      4473a09dc1781fbdf071456f6f562154425a762a  (https://github.com/USEPA/CMAQ_Dev/commit/4473a09dc1781fbdf071456f6f562154425a762a)
      41c23d8a4cbcf74f3e9c9f094a27e56c861c7e84  (https://github.com/USEPA/CMAQ_Dev/commit/41c23d8a4cbcf74f3e9c9f094a27e56c861c7e84)
      7d7e01e200aebf9284fff788bb52810b700c058d  (https://github.com/USEPA/CMAQ_Dev/commit/7d7e01e200aebf9284fff788bb52810b700c058d)
      05b18584e978575283b9661b7723e0f038af1b70  (https://github.com/USEPA/CMAQ_Dev/commit/05b18584e978575283b9661b7723e0f038af1b70)
      0c00a5e6f344f9d86c8e8597699ff3f6a33df1ac  (https://github.com/USEPA/CMAQ_Dev/commit/0c00a5e6f344f9d86c8e8597699ff3f6a33df1ac)
      cce858982bf729f410108950a69eeddedeba34a2  (https://github.com/USEPA/CMAQ_Dev/commit/cce858982bf729f410108950a69eeddedeba34a2)


-----

