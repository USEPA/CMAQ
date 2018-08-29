# Two-way Coupled Meteorology Convective Scheme in WRF-CMAQ

**Author/P.O.C.:**, [David Wong](mailto:wong.david-c@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The new twoway_aqprep.F will take into account whether WRF portion of the coupled model is running with or without cumulus convective scheme. This reflects in the content of variable rainc. This code change does not affect CMAQ offline mode and only in WRF-CMAQ twoway coupled model and make it similar to offline run.

## Significance and Impact

There were minor effects on ozone in the coupled model run (based on Dr. Christian Hogrefe evaluation)

## Affected Files:

CCTM/src/twoway/twoway_aqprep.F90

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #275](https://github.com/USEPA/CMAQ_Dev/pull/275)  

### Commit IDs:

a85e17c7b8bd0a144a7919e8061f0a57a8cefec8

