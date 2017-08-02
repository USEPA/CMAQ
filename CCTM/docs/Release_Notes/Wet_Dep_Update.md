# WRF-CMAQ Accumulated Wet Deposition Update

**Author/P.O.C.:**, [Jesse Bash](mailto:bash.jesse@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

There was approximately a 30% systematic under estimation of wet deposition for all species when running the WRF-CMAQ coupled model and the output of wet deposition fields were hard coded for one hour. The wet deposition bug in the WRF-CMAQ coupled model was related to the writing of the output time step in cldproc_acm.F and setting the accumulated hour wet deposition for a grid cell to zero if the precipitation rate or cloud fraction fell below their respective threshold values in cldproc_acm.F. The logic governing the write time in cldproc_acm.F was revised by copying the logic in the aerosol and vdiff code for diagnostic outputs. The portion of the code in cldproc_acm.F where the accumulated wet deposition array was set to zero was removed because this array is re-initialized to zero at each time the output file is written. This was a redundant operation that was hardcoded for hourly meteorological inputs. opwdep.F was also modified to allow the WDEP2 diagnostic file to support a user defined output time step.

## Significance and Impact

WRF-CMAQ coupled wet deposition results running with a 5 minute communication between WRF and CMAQ were increased by approximately 30% for all species. This matches offline simulation much better with approximately a ±5% difference between online and offline deposition fields for the simulation day tested. All WRF-CMAQ simulations to date are impacted by this bug.

## Affected Files:
CCTM/src/cloud/acm_ae6/cldproc_acm.F
CCTM/src/cloud/acm_ae6/convcld_acm.F
CCTM/src/cloud/acm_ae6/opwdep.F
CCTM/src/cloud/acm_ae6_mp/cldproc_acm.F
CCTM/src/cloud/acm_ae6_mp/convcld_acm.F
-----
## Internal Records:

### Relevant Pull Requests:
  [PR #21](https://github.com/usepa/cmaq_dev/pull/21)

### Commit IDs:
992729db506091be3ce80f5086d909e0ea15ae9f  
3dc45f1e9b2e9b35454ad51eb218e420fc57b701  
62e4165b45ef933f29b34d061e0a545c8cb8632e  
60647d3b104b09e2e0afa47f53fd7bb5083aa82a    
