# Update and Enhancement of PM Diagnostic Files 

**Author/P.O.C.:**, [David Wong](mailto:wong.david@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description 

To compute an average value of various PM diagnostic variables which are defined in PMDIAG_DATA.F and four visibility diagnostic variables bases on user predefined time interval. It will follow the behaviour of ACONC file to output data to the top or bottom of the "hour".  

 Options:  
  -- user can define a subset of PM diagnostic variables  
  -- user can define a vertical layer range  
  -- user can define a vertical layer range  

Name changes:

 * run script  
   -- AERODIAM to PMDIAG  
   -- CTM_AERDIAG to CTM_PMDIAG  
   -- AD1 to PMD1  
   -- AD1file to PMD1file  

 * outck_bidi.q  
   -- AD1file to PMD1file  

 * aero_driver.F  
   -- CTM_AERDIAG to CTM_PMDIAG  
   -- AERDIAG to PMDIAG  

 * AEROSOL_CHEMISTRY.F
   -- CTM_AERDIAG with CTM_PMDIAG  
   -- AERDIAG with PMDIAG  

New environmental variable:

 * run script  
   -- CTM_APMDIAG  
   -- APMDIAG_BLEV_ELEV  
   -- AVG_PMDIAG_SPCS  
   -- CTM_AVISDIAG  

Misc:

 * run script
   -- added AAV1file  
   -- added APMD1file  

 * outck_bidi.q  
   -- added CTM_AVIS_1  
   -- added CTM_ADIAM_1  


## Significance and Impact

This update ensures consistency among the major output files for aerosol and aerosol chemistry related variables. It also makes possible the user selection of instantaneous OR hourly averaged aerosol diagnostic species.

## Affected Files:  

run script  
outck_bidi.q  
FILES_CTM.EXT  
aero_driver.F  
opadiam.F  
opavis.F  
AEROSOL_CHEMISTRY.F  
PMDIAG_DATA.F  


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
