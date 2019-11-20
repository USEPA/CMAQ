CMAQv5.3 Known Issues
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.3:

## *CMAQv5.3-i1:* 
Date: 2019-09-05
Contact: Kristen Foley (foley.kristen@epa.gov) 

### Description  
Setting CTM_WVEL, a run time science option to write out the vertical velocity component to the concentration file, to N. The default setting, currently, is listed as Y in all runscripts within the repository. If the CTM_WVEL science option is set to N, the model immediately crashes. This is because the array that stores the vertical velocity component for writing to the concentration file is never allocated and is being used to calculate the average vertical velocity to be written to the average concentration file.

### Scope and Impact
The model will terminate execution with a segmentation fault.

### Solution
Users should leave CTM_WVEL set to Y in all runscripts. A code fix will be included with the next minor CMAQ release.

## *CMAQv5.3-i2:* 
Date: 2019-09-11
Contact: David Wong (wong.david-c@epa.gov) 

### Description  
The current implementation of the Centralized Input/Output Module only supports BNDY_CONC_1 files that have a time step of 1 hour. Previous versions of CMAQ were able to process boundary condition files with time steps greater than 1 hour. 

### Scope and Impact
If providing a boundary condition file with a time step greater than 1 hour, the model will terminate execution with an error message that the BNDY_CONC_1 cannot be read for the requested time step.

### Solution
Replace CCTM/src/cio/centralized_io_module.F file in repository with the version located in the folder DOCS/Known_Issues/CMAQv5.3-i2.
This code fix will also be included with the next minor CMAQ release.

