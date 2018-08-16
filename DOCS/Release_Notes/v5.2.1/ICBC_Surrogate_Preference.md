# Modification in preference for surrogates listed for ICs and BCs

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

In order to ensure backwards compatibility of the semivolatile POA algorithm with the nonvolatile POA input datasets, the algorithm for mapping initial and boundary condition surrogates has changed. CMAQv5.2 now assumes that if a surrogate is identified in a correpsonding namelist and that same surrogate appears on the initial or boundary condition file, then it should be used. However if the surrogate does not exist on the input files, CMAQ will look for the internal name of the species and use that instead if it exists on the input file.

## Significance and Impact

This modification will allow users to avoid creating new initial and boundary condition files in order to run with semivolatile POA.

## Affected Files:

advbc_map.F  
rdbcon.F  
load_cgrid.F  


## References:

Not Applicable

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #123]  
  [PR #165]  
  [PR #218]  

### Commit IDs:  
2ed961c  
c5c71e6  
1cc1508  
e913870  
e7e4a90  
b6704d8  
d9bec29  
