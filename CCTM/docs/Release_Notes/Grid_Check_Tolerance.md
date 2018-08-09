# Improve calculation for checking agreement between CMAQ grid and meteorological inputs

**Author/P.O.C.:**, [Tanya Spero](mailto:spero.tanya@epa.gov), Systems Exposure Division, U.S. EPA  

## Brief Description

When using some grid files, the size of the advection spacing don''t exactly match. For example 1333.33333.... meters can be rounded many different ways. The code was modified to use a tolerance to test for consistency.

## Significance and Impact

CMAQ will now be more flexible in receiving grid spacings that are essentially equivalent but may have some small numerical precision differences.

## Affected Files:

CCTM/src/driver/yamo/advstep.F  

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #248](https://github.com/USEPA/CMAQ_Dev/pull/248)  

### Commit IDs:

fda4af970515f8302493b0ed606640261c20647d  

