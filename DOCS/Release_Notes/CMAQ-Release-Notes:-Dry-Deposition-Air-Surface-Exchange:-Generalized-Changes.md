## Activate Dry Deposition Diagnostic Output for PMOTHR
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.4  
**Description**:  
The diagnostic output for dry deposition of PMOTHR species was activated in the AE namelist by default.
 
**Significance and Impact**: None.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#808](https://github.com/USEPA/CMAQ/commit/20ffc395a7d2e6c4686621dc43ea6d65f00b3679) | [PR#808](https://github.com/USEPA/CMAQ_Dev/pull/808)  |

## Remove Remnants of Reading Dry Deposition Velocities from MCIP
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency  

**Type of Update**: Bug Fix
**Release Version/Date**: CMAQv5.4

**Description**: 
Dry deposition velocities used to be computed in MCIP, then read by CMAQ. The capability to compute deposition velocities online (or "inline") was added in CMAQv4.7 in December 2008. The computation of deposition velocities was later removed from MCIP. However, the CMAQ code still had `ILDEPV` as a commented-out flag in `DEPV_DEFN`, as well as the `RDDEPV` routine, which was never called. These remnants have all been removed. 

**Significance and Impact**:  Removes non-working option that had the potential for confusing users.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#679](https://github.com/USEPA/CMAQ/commit/e3021a6aadc355f3fad85d9bf325bbd6d871c2e8) | [PR#679](https://github.com/USEPA/CMAQ_Dev/pull/679)  |