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

## 3. Centralized I/O (CIO) Bugfix for Representative Day 2-D & 3-D Surface Gridded Emissions Files
[David Wong](mailto:dwongepa@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX]((https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration). set to T, will prompt the centralized i/o module to store the start date of the file. However, this information was never passed on to the function used to extract the data from the netCDF file causing an error, as the time is not available on file. 

### Solution in CMAQv5.3.2

The information is now properly passed on to the variable required to extract the data from the netCDF file

### Files Affected 
CCTM/src/cio/centralized_io_module.F
