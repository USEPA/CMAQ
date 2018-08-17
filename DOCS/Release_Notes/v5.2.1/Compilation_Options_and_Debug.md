# Add Debug flag to Makefiles and update compilation flags to be consistent with current compilers  

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description

Bldmake has been modified to allow the CMAQ Makefile to take "DEBUG=TRUE" as a command line option and then choose the debug flags if it is invoked. This will help, for instance, when users want to work debug-activated builds into part of a larger automated testing protocol.

This improvement also addresses the following issues the makefile:
* should indicate ioapi version explicitly, and replace module with explict Linux2_*.
* LINK_FLAGS should not have -openmp
* remove "MPICH = -L$(LIB)/mpi/lib -lcurl"
* remove MPICH in the "LIBRARY = " clause
* an explicit path should be included in this line "-DSUBST_MPI=mpif.h"

## Significance and Impact

No changes to results were observed with these model workflow and compilation changes.

## Affected Files:

config_cmaq.csh  
UTIL/bldmake/src/bldmake.f  
UTIL/bldmake/src/cfg_module.f  
UTIL/bldmake/src/utils.f  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #250](https://github.com/USEPA/CMAQ_Dev/pull/250)  
  [PR #266](https://github.com/USEPA/CMAQ_Dev/pull/266)  

### Commit IDs:

7d3f20d36eb166a7718e1f8872f477f454c58577  
d0c243a1acd0d04b4722eff5893f0c4c5a5be9da  
7f79f38070c8736595d7c0ca9e78397f602847a5  
