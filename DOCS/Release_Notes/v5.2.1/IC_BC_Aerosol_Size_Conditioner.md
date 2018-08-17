# Check the aerosol size distirbution inputs and modify them if they are not well-behaved

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The aerosol size distirbutions coming from initial and boundary conditions can occasionally be inconsistent with the internal parameters exepcted by CMAQ. For example, profiles of the number, surface area, and chemically-resolved mass may be interpolated in the vertical in order to change the number of layers compared to a GEOS-Chem run that created the conditions upstream. When these modifications happen, they may result in size distributions with unrealistically large standard deviation or mean diameter that is not consistent with the mode that those particles will be simulated by.

This routine conserves the mass of all aerosol chemical components. It uses the number and surface area concentrations to calculate the standard deviation and diameter of each mode. If either of these quantities are inconsistent with the defaults in the model, then the default parameters are forced and the number and surface area are recalculated. A warning is printed if this action was taken so that the user is aware.

## Significance and Impact

Several spurious differences were noted for compounds like particulate nitrate and ammonium near the lateral and top boundaries of test cases in the Beta version of CMAQv5.2. These routines resolve a large part of that discrepancy and improve the consistency between model inputs and real atmospheric aerosol profiles.

## Affected Files:

aero/aero6/AERO_DATA.F  
hadv/yamo/rdbcon.F  
init/yamo/load_cgrid.F  

## References:

Not Applicable

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #189]  

### Commit IDs: 
d0af67d  
34b423f  
b50a7ab  
