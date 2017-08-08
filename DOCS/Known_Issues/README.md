CMAQv5.2 Known Issues 
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.2

## [Example] Problematic Bug in XXX Module
*Issue ID: CMAQv5.2-i1*  
Date: 2017-8-8
Contact: Developer A

### Description  
EXAMPLE: An error was discovered in an XXX routine, which leads... The most problematic situations arise when...

### Scope and Impact
This issue affects YYY species but only under these conditions....


### Solution
In XXX.F, line ZZZ:
```
some code
```
should be changed to:
```
different code
```

## [Example] Unstable Solver for Calculating AAA
*Issue ID: CMAQv5.2-i2*  
Date: 2017-8-8
Contact: Developer B

### Description  
The BBB solver that calculates AAA relies on precise numerics when there is very little mass in the system. Thus, the precision of the input bounds to the algorithm becomes significant, and needs to be improved...

### Scope and Impact
This issue affects only compounds at extremely low concentrations (below CCC ug m-3).

### Solution
Because of extensive line changes we provide an update to BBB.F in the folder named CMAQv5.2-i2
 
