# Diagnosing visibility with the recent IMPROVE equation

**Author/P.O.C.:**, [Peng Liu](mailto:liu.peng@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
CMAQ included the approach to estimating the light extinction of aerosols at 550nm based on the algorithm derived with IMPROVE (Interagency Monitoring of Protected Visual Environments) particle speciation data. The estimated aerosol extinction efficiency will be used to:  
1. Diagnose the AOD (to generate the diagnostic file, set CTM_AOD=Y in run script)  
2. Diagnose the visibility (output in CTM_VIS_1 file)

Subroutine get_extinc was modified to reflect the latest change in the IMPROVE based algorithm. In addition, the model's code was made more consistent when using the aerosol extinction efficiency to estimate the AOD and visibility, respectively.  


## Significance and Impact
NA


## Affected Files:
aero/AERO_DATA.F  
aero/AERO_DEFN.F   
aero/aero_subs.F   

## References:
Pitchford et al., 2007, Revised Algorithm for Estimating Light Extinction from IMPROVE Particle Speciation Data, Journal of the Air & Waste Management Association, 57, pp 1326-1336.   

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #92](https://github.com/usepa/cmaq_dev/pull/92)

### Commit IDs:
708a54d4cee235a3b8e03874cf8629403cf16f90   
62804bc1bebdd94d201f7371a98964f359152989   
