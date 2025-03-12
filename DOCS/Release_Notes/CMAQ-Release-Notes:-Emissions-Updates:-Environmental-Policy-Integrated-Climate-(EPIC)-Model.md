### Improved NaN traps for EPIC input files
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug fix  
**Release Version/Date**: CMAQv5.5   
**Description**: CCTM crashes when debug flags are turned on if NaNs are present in EPIC input files and the current NaN traps do no work when running CCTM with debug compilation flags. The FORTRAN 2003 IEEE arithmetic function ieee_is_nan was implemented to correctly trap NaN in these inputs when running in debug mode. It is unclear why EPIC output contains sporadic NaNs, these are not visible Verdi or R and may have to do with a periodic error in specifying BADVAL3.

**Significance and Impact**: Allows the user to run CCTM with bidirectional NH3 exchange with debug flags when EPIC data contains NaNs. Model runtime and results are unchanged. The FORTRAN 2003 IEEE arithmetic intrinsic functions are included in Intel 16+, PGI 16+ and GCC 5+ compiler versions.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1036](https://github.com/USEPA/CMAQ/commit/68377cdbc6fcd4d4e8d0cb94e448fcb60b048fd7) | [PR#1036](https://github.com/USEPA/CMAQ_Dev/pull/1036)  |