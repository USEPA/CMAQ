CMAQv5.3.1 Known Issues
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.3.1:


## *CMAQv5.3.1-i1:* 
Date: 12/19/2019
Contact: Sergey Napelenok (napelenok.sergey@epa.gov)

### Description  
CMAQ-ISAM overcontributes apportionment of secondary gaseous pollutants to the ICON and BCON tags.  As the result, contributions to other tracked sources is underestimated.  

### Scope and Impact
All secondary gas-phase species (O3, NOx, etc.) in CMAQ-ISAM v5.3 and v5.3.1.

### Solution
A modification to the ISAM chemistry algorithms is currenlty in testing.  It will be released in the near future. 


## *CMAQv5.3.1-i2:* 
Date: 2019-12-31
Contact: David Wong (dwongepa@epa.gov) 

### Description  
Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration). set to T, will prompt the centralized i/o module to store the start date of the file. However, this information was never passed on to the function used to extract the data from the netCDF file causing an error, as the time is not available on file. The information is now properly passed on to the variable required to extract the data from the netCDF file.

### Scope and Impact
The model will terminate execution with a time retrievel error. No model results will change.

### Solution
Replace CCTM/src/cio/centralized_io_module.F file in repository with the version located in the folder [DOCS/Known_Issues/CMAQv5.3.1-i2](CMAQv5.3.1-i2). This code fix will also be included with the next minor CMAQ release. 


## *CMAQv5.3.1-i3:* 
Date: 2020-01-23
Contact: Sergey Napelenok (napelenok.sergey@epa.gov)

### Description  
CMAQ version 5.3.1 release included an inadvertently modified version of EMIS_DEFN.F causing crashes during compiling. 

### Scope and Impact
The model did not compile with the -Disam option with errors pointing to EMIS_DEFN.F

### Solution
Redownload the model after 1/23/2020 or replace CCTM/src/emis/emis/EMIS_DEFN.F in repository with the version located in the folder [DOCS/Known_Issues/CMAQv5.3.1-i3](CMAQv5.3.1-i3).
