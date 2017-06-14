# Update and Enhancement of PM Diagnostic Files

**Author/P.O.C.:**, [David Wong](mailto:wong.david@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

This update allows for an average value of various PM diagnostic variables that are defined in PMDIAG_DATA.F and four visibility diagnostic variables based on a time interval predefined by the user. It will follow the behaviour of ACONC file to output data to the top or bottom of the "hour".  

 Options:  
  -- user can define a subset of PM diagnostic variables  
  -- user can define a vertical layer range  

Name changes:

 * run script  
   -- AERODIAM to PMDIAG  
   -- CTM_AERDIAG to CTM_PMDIAG  
   -- AD1 to PMD1  
   -- AD1file to PMD1file  

 * aero_driver.F  
   -- CTM_AERDIAG to CTM_PMDIAG  
   -- AERDIAG to PMDIAG  

 * AEROSOL_CHEMISTRY.F  
   -- CTM_AERDIAG with CTM_PMDIAG  
   -- AERDIAG with PMDIAG  

New environmental variables:

 * run script  
   -- CTM_APMDIAG  
   -- APMDIAG_BLEV_ELEV  
   -- AVG_PMDIAG_SPCS  
   -- CTM_AVISDIAG  

Misc:

 * run script  
   -- added AAV1file  
   -- added APMD1file   

## Significance and Impact

This update ensures consistency among the major output files for variables related to aerosols and aerosol chemistry. It also enables the user to select instantaneous OR hourly averaged aerosol diagnostic species.

## Affected Files:  

run script   
FILES_CTM.EXT  
aero_driver.F  
opadiam.F  
opavis.F  
AEROSOL_CHEMISTRY.F  
PMDIAG_DATA.F  

## Environment Variable:

In order to output aerosol and/or visibility diagnotic file, you need to set envirnonment varialbes CTM_APMDIAG and CTM_AVISDIAG accordingly and both with default value N. Environment variable AVG_FILE_ENDTIME, which has default value N, is used to switch from top to bottom of hour. User can specify a particular layer range with environment variable APMDIAG_BLEV_ELEV. Here is a specific example:

  setenv AVG_FILE_ENDTIME     Y
  setenv CTM_APMDIAG          Y  
  setenv CTM_AVISDIAG         Y 
  setenv APMDIAG_BLEV_ELEV " 1 3"  


## References:

NA

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #82](https://github.com/usepa/cmaq_dev/pull/82)
  [PR #102](https://github.com/usepa/cmaq_dev/pull/102)
  [PR #104](https://github.com/usepa/cmaq_dev/pull/104)

### Commit IDs:
992729db506091be3ce80f5086d909e0ea15ae9f  
3dc45f1e9b2e9b35454ad51eb218e420fc57b701  
62e4165b45ef933f29b34d061e0a545c8cb8632e  
60647d3b104b09e2e0afa47f53fd7bb5083aa82a    
