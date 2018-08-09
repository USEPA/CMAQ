# Update CCTM runscript to switch on BioSeason approach by default

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The default CCTM run script was pointing to the wrong Bioseason file. This has been corrected.

## Significance and Impact

A default run of CMAQ would not considered seasonality or summertime levels of emissions from biogenic sources. Now that seasonality has been restored.

## Affected Files:

CCTM/scripts/run_cctm.csh  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #247](https://github.com/USEPA/CMAQ_Dev/pull/247)  

### Commit IDs:

431157e2a54048054f133b3abccbdf13b345530f  
