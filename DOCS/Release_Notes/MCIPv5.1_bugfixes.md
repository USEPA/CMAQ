# MCIPv5.1 Bugfixes
[Tanya Spero](mailto:Spero.Tanya@epa.gov), U.S. Environmental Protection Agency

## 1. Corrected an error in the MCIP script that interfered with reading Geogrid files (introduced in MCIPv5.0) 
### Description of model issue:
Several users in the CMAS Forum reported an error reading the namelist for MCIPv5.0. 
This error was traced to an oversight in the modifications to MCIP to remove the processing of MM5 data in MCIPv5.0. 
In short, the namelist variable "file_ter" was changed to "file_geo", 
but it was not changed in the MCIP script in the location where the namelist is automatically generated. 
This issue did not come up in testing because the required data were already in the WRF output file, 
so the optional Geogrid file was not read for the test cases.

### Files Affected:
PREP/mcip/scripts/run_mcip.csh

## 2.	Corrected an error in filling the lateral boundaries of the WWIND and CFRAC3D fields when CFRAC3D is available (introduced in MCIPv5.0) 
### Description of model issue
One user reported this issue via email. This was a cut-and-paste failure in updates for MCIPv5.0. This issue is benign to EPA's internal runs of CMAQ because they do not use 3D cloud fraction from KF-rad, and WWIND (vertical velocities) are not used in CMAQ from MCIP. Without this change, one of the lateral boundaries for CFRAC3D is left undefined when 3D cloud fraction is available and used for CMAQ, and the lateral boundary information for WWIND is incorrect on that boundary.

### Files Affected:
PREP/mcip/src/ctmproc.f90

## 3.	Added support for processing surface FDDA fields generated with FASDAS in WRF 
### Description of model issue
One user reported this error via the CMAS Forum. The option to have surface FDDA from the FASDAS scheme has been available since WRFv3.8. This modification allows MCIP to properly process (and distinguish) setting the WRF variable "GRID_SFDDA" to either 1 or 2 (rather than just allowing it to be set to 1).  With this change, the surface FDDA is listed in the MCIP metadata as "STANDARD" (instead of "ON") with option 1, or "FASDAS" with option 2.

### Files Affected:
  PREP/mcip/src/blddesc.f90
  
  PREP/mcip/src/setup_wrfem.f90

## 4.	Updated release stamp
### Description of model issue:
The metadata is updated to reflect that output is now from MCIPv5.1.

### Files Affected:
PREP/mcip/src/mcipparm_mod.f90

## 5.	Updated documentation
### Description of model issue:
The change log and the release notes are updated to reflect the changes introduced for MCIPv5.1.

### Files Affected: 
PREP/mcip/docs/CHANGES

PREP/mcip/docs/ReleaseNotes

## A note about MCIP compatibility with CMAQ
If running MCIPv5.0+ you must use CMAQv5.3+ because of a change in where fractional land use is stored.  In MCIPv5.0+ the fractional land use is no longer in GRIDCRO2D.  It is now in a new file, LUFRAC_CRO and is stored as a 3D array rather than a suite of 2D arrays.
