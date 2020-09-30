# Research Branch Bugfixes

 

## 1. Resolve omission of organic condensable vapors from aerosol process analysis
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue  
The flux of every condensable inorganic and organic vapor should be aggregated and saved in the processanalysis arrays for aerosol condensation (i.e. COND). This procedure was overlooked for the organic species, although it was implemented for the inorganics.
 
### Solution in CMAQv5.3.x
The code necessary for storing and propagating these rates has now been added. This is particularly important for future versions of ISAM which will use these net changes in aerosol processing to perform source apportionment.

### Files Affected 
aero/aero6/AERO_DATA.F
aero/aero6/PRECURSOR_DATA.F
aero/aero6/SOA_DEFN.F
src/procan/pa/pa_update.F


## 2. Informative title for bug fix
[Developer Name](mailto:Last.First@epa.gov), U.S. Environmental Protection Agency

 

### Description of model issue

 

### Solution in CMAQv5.3.x

 

### Files Affected 
