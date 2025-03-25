### BDSNP Dry Hours Calculation
[Jeff Willison](mailto:willison.jeff@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug fix  
**Release Version/Date**: CMAQv5.5  
**Description**: A bug in the pulsing subroutine of BDSNP was allowing for 24 additional time steps to be added to the dry hours total per day. On coarse grids with large time steps this bug was especially noticeable. For example, a 15 minute time step would would allow for accumulation of 30 dry hours per day.  

![Willison_MEGAN](https://github.com/user-attachments/assets/b35d125b-ce61-4ae4-be5a-fe5db2300724)


**Significance and Impact**: The effect of the bug fix on soil NO is small, with a resultant decrease of 0.3% in total 2018 soil NO emissions over the Northern Hemisphere at 108 km grid spacing.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1100](https://github.com/USEPA/CMAQ/commit/7c27b05203a677a078992d4cfc1d7f00e059eb68) | [PR#1100](https://github.com/USEPA/CMAQ_Dev/pull/1100)  | 

### Improved NaN traps for EPIC and MEGAN input files
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug fix  
**Release Version/Date**: CMAQv5.5   
**Description**: CCTM crashes when debug flags are turned on if NaNs are present in EPIC input files and the current NaN traps do no work when running CCTM with debug compilation flags. The FORTRAN 2003 IEEE arithmetic function ieee_is_nan was implemented to correctly trap NaN in these inputs when running in debug mode. It is unclear why EPIC output contains sporadic NaNs, these are not visible Verdi or R and may have to do with a periodic error in specifying BADVAL3.

**Significance and Impact**: Allows the user to run CCTM with bidirectional NH3 exchange with debug flags when EPIC data contains NaNs. Model runtime and results are unchanged. The FORTRAN 2003 IEEE arithmetic intrinsic functions are included in Intel 16+, PGI 16+ and GCC 5+ compiler versions.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1036](https://github.com/USEPA/CMAQ/commit/68377cdbc6fcd4d4e8d0cb94e448fcb60b048fd7) | [PR#1036](https://github.com/USEPA/CMAQ_Dev/pull/1036)  | 

### BDSNP nitrogen deposition reservoir bug fix
[Jeff Willison](mailto:willison.jeff@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug fix

**Release Version/Date**: CMAQv5.5  

**Description**: 
The soil nitrogen deposition reservoir (NDEPRES) was incorrectly calculated using online N deposition rates when the BDSNP soil NO option was enabled. This feature is not mature and contributes to unreasonably large NDEPRES values as the simulation progresses. This bug fix allows the MEGAN BDSNP module to use climatological N deposition rates from input files. The input files can be created using the MEGAN preprocessor along with the other necessary inputs for MEGAN and BDSNP.


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#956](https://github.com/USEPA/CMAQ/commit/3f6b535c6a3b5ce9c5a58c7b8889e1f970b8f059) | [PR#956](https://github.com/USEPA/CMAQ_Dev/pull/956)  | 

### New Biogenic Emissions Option, The Model of Emissions of Gases and Aerosols from Nature (MEGAN)
[Jeff Willison](mailto:willison.jeff@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  

**Release Version/Date**: CMAQv5.4  

**Description**: 
MEGAN 3.2 is available in CMAQv5.4 as an additional option for inline biogenic emissions. Input files will be available for several domains at release, but users can download the MEGAN preprocessor to create their own (https://bai.ess.uci.edu/megan/data-and-code). Soil NO can be calculated using the Berkeley-Dalhousie Soil NOx Parameterization (BDSNP) or an implementation of Yinger and Levy (YL95) when MEGAN is enabled. 

Users may enable MEGAN and BEIS emission streams simultaneously to select species from a given stream through the emission control file. This approach allows the use of BDSNP with BEIS, the use of BEIS-YL95 with MEGAN, etc. See Chapter 4 of the user guide for more information about required inputs, and Chapter 6 for more information about configuration options and recommendations. 

**References**: Guenther, A., Jiang, X., Shah, T., Huang, L., Kemball-Cook, S., and Yarwood, G., Model of Emissions of Gases and Aerosol from Nature  Version  3  (MEGAN3)  for  Estimating  Biogenic  Emissions:  Air  Pollution  Modeling  and  its  Application  XXVI, edited by Mensink, C., Gong, W., and Hakami, A., pp. 187–190, Springer International Publishing, Cham, 2020.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#652](https://github.com/USEPA/CMAQ/commit/0a82257b1f8a43b0315f7f44b7a9b5dd9d2a1801) | [PR#652](https://github.com/USEPA/CMAQ_Dev/pull/652)  | 
|[Merge for PR#701](https://github.com/USEPA/CMAQ/commit/49f5cf29d8991239e6fb0cda2702fdee65c7b981) | [PR#701](https://github.com/USEPA/CMAQ_Dev/pull/701)  | 
|[Merge for PR#889](https://github.com/USEPA/CMAQ/commit/1916ef06c4dc9ea5b7d67ece1d96654d07203cde) | [PR#889](https://github.com/USEPA/CMAQ_Dev/pull/889)  | 

