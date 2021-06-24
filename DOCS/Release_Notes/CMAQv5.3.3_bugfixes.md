# CMAQv5.3.3 Bugfixes

## 1. Bugfix, clean-up, and added option in bldscript for distr_env.c
[Fahim Sidi](mailto:sidi.fahim@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Bug reported by Steve Fine, EPA-OAR, when using AWS to run CMAQ across multiple adjioned instances. The issue is related to blank environmental variables causing a segmentation fault on AWS when invoking the directive -Dcluster in the CCTM Makefile. Additionally, it was found that there was no bldscript options to invoke C code distr_env, causing users to manually invoke this option via editing CCTM Makefile to include CPP flag -Dcluster. 

The update also enables users on different architectures and systems that do not appended C routine names with an underscore to compile the CCTM code.

### Solution in CMAQv5.3.2

Changed distr_env.c to only set environmental variables on other processors that are not blank, which resolved the segmentation fault. To fix manual addition of CPP Flag -Dcluster, a new bldscript option within CCTM that allows users to optionally invoke distr_env.c is added. This new option is called "DistrEnv", if set this option adds the CPP flag -Dcluster. It should be noted, that two conditions have to be met for the -Dcluster flag to be activiated:

(1) DistrEnv is set
(2) ParOpt is set (indicates this an MPI run)

Since DistrEnv is strictly an MPI option (containing MPI commands) it is only needed if ParOpt is invoked. It has no use when running CMAQ serially.

The second part of this update cleans-up the C code and adds C-Fortran Interoperability (Feldman Style Binding) that is consistent with the CPP flag provided in the Makefile (-DFLDMN) to compile this code with other architectures & compilers that don't append C code with underscore.  

### Files Affected 
CCTM/scripts/bldit_cctm.csh
CCTM/src/par/mpi/distr_env.c


## 2. Short description
[Firt Name Last Name](mailto:last.first@epa.gov), U.S. Environmental Protection Agency

### Description of model issue


### Solution in CMAQv5.3.3


### Files Affected 
