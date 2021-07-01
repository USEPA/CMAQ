# *handle_missing_omi_file*: Provide appropriate error message and abort if OMI photolysis file is missing

[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency

## Brief Description

The photolysis module reads a data file from NASA's Ozone Monitoring Instrument (OMI) describing total column ozone. 
The model attempted to read the file prior to the check whether the file had been successfully opened, leading to a crash.
The check has been moved prior to the first attempt to read the file, and the model aborts with an appropriate error if the OMI file is not found.

## Significance and Impact

No impact on model results in the normal case, where the OMI file is present. 

## Files Affected: 
CCTM/src/phot/inline/o3totcol.f 

-----
## Internal Records:
#### Relevant Pull Requests:
682

#### Commit 
IDs:
eedb5d6682d5d9a3a632fb24b598abf6614e2794
