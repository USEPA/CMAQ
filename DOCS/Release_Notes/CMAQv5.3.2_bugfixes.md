# CMAQv5.3.2 Bugfixes

## 1. Correct Emission Control File for cb6mp_ae6_aq mechanism
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

The cb6mp_ae6_aq mechanism's emissions control file did not include instructions for the hazardous air pollutants in the
NonReactive (NR) species namelist. It also gave incorrect mode for aerosol species, ADE_EC and ADE_NO3. 

### Solution in CMAQv5.3.2

Instructions were added to this control file for the above NR species. The mode was changed from COARSE to FINE for the ADE_EC and ADE_NO3 species.

### Files Affected 
CCTM/src/MECHS/cb6mp_ae6_aq/EmissCtrl_cb6mp_ae6_aq.nml

## 2. Replace data website in get TOMS data 
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

The cshell script that gets data to create the CMAQ OMI file uses a website that no
longer works.

### Solution in CMAQv5.3.2

The script was corrected with a working website.

### Files Affected 
PREP/create_omi/scripts/get_toms_data.q
