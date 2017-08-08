CMAQv5.2 Known Issues 
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.2

## [Example] Problematic Bug in Transport Module
*Issue ID: CMAQv5.2-i1*  
Date: 2017-8-8
Contact: David Wong

### Description  
An error was discovered in an advection routine, y_ppm, which leads to species not being transported correctly between neighboring decomposition domains. The most problematic situations arise when...

### Scope and Impact
This issue affects all species but only for high wind speeds when transport...


### Solution
In y_ppm.F, line XXX:
```
some code
```
should be chaged to:
```
different code
```

## [Example] Unstable Solver for Organic Aerosol Partitioing
*Issue ID: CMAQv5.2-i2*  
Date: 2017-8-8
Contact: David Wong

### Description  
The bisection solver that calculates the partitioning of organic aerosol relies on precise numerics when there is very little organic mass in the system. Thus, the precision of the input bounds to the algorithm becomes significant, and needs to be improved...

### Scope and Impact
This issue affects onle organic aerosol compounds at extremely low concetrations (below 1e-4 ug m-3).

### Solution
Because of extensive line changes we provide an update to SOA_DEFN.F in the folder named CMAQv5.2-i2
 

