# Brute force no chemistry capacity

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
These changes allow commenting out the call to CHEM in SCIPROC for purposes such as brute debugging or
process analysis when the aerosol diagnostic flag is true. Without them, the model crashes because it attempts to
use unallocated arrays. Members of the development and evaluation group have encountered this error and may want this
capacity within the model.

## Significance and Impact

A user can conduct brute force debugging or process analysis by modifying sciproc.F.    

## Affected Files:
aero/aero6/AEROSOL_CHEMISTRY.F  
aero/aero6/aero_driver.F  
aero/aero6i/AEROSOL_CHEMISTRY.F  
aero/aero6i/aero_driver.F  

## References:    

None

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #25](https://github.com/USEPA/CMAQ_Dev/pull/25)

### Commit IDs:

186074857e125cc0f51f3ae991b4fb58f38c3d5b  
1a245369e3318406602a3fa05441aedea2c03d03  
