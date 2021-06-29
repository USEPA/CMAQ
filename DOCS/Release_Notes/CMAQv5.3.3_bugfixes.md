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
CCTM/scripts/bldit_cctm.csh<br>
CCTM/src/par/mpi/distr_env.c

## 2. POST tool bug fixes in hr2day and sitecmp_dailyo3
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

*hr2day*: When using the hr2day MAXDIF operation in conjunction with setting PARTIAL_DAY to F, the previous code returned missing values for all days.

*hr2day*: When run script variable START_DATE was set to a date later than the start date of M3_FILE_1, the previous code generated empty time steps for the time period between the start date of M3_FILE_1 and environment variable START_DATE. 

*sitecmp_dailyo3*: When the values in the in `OZONE_F` column of CASTNET `IN_TABLE` files were enclosed in quotes, the code did not remove those quotes, therefore  did not properly match it to any of the known QA codes for which values should be discarded, and consequently did not discard such flagged values before computing the daily metrics. In the CASTNET files distributed via CMAS, this only affected the 2005 observation file.

### Solution in CMAQv5.3.3

The *hr2day* code was updated to correct the behavior of the MAXDIF operation when PARTIAL_DAY is set to F. It also was updated so that OUTFILE only contains time steps between MAX(start of M3_FILE_1, START_DATE) and MIN(end of M3_FILE_n, END_DATE)
 
The *sitecmp_dailyo3* code was updated to remove any quotes from the `OZONE_F` column of CASTNET `IN_TABLE` files. 

### Files Affected 

POST/hr2day/src/hr2day.F
POST/sitecmp_dailyo3/src/utilities.F

## 3. Short description
[Firt Name Last Name](mailto:last.first@epa.gov), U.S. Environmental Protection Agency
