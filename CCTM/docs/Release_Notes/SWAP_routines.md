# Replace SWAP routine calls with SE_COMM Calls  

**Author/P.O.C.:**, [David Wong](mailto:wong.david@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description
To conform and preserve original SE library design, SWAP routines were directly replaced by original SE communication routines.


## Significance and Impact
No impact on numerical result.

## Affected Files:

CCTM/src/driver/yamo/driver.F  
CCTM/src/hadv/yamo/x_ppm.F  
CCTM/src/hadv/yamo/x_yamo.F  
CCTM/src/hadv/yamo/y_ppm.F  
CCTM/src/hadv/yamo/y_yamo.F  
CCTM/src/hdiff/multiscale/deform.F  
CCTM/src/hdiff/multiscale/hdiff.F  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #274](https://github.com/USEPA/CMAQ_Dev/pull/274)

### Commit IDs:

650ad613251011d904b257cd719c8c77200db58e  
