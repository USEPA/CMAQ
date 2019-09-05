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

