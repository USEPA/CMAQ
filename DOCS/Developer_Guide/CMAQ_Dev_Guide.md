**Developer Guide for the Community Multiscale Air Quality (CMAQ) Modeling System**
=
**Version 5.2 (2017 Release)**

Prepared in cooperation with the: 
Community Modeling and Analysis System  
Institute for the Environment  
University of North Carolina at Chapel Hill  
Chapel Hill, NC

Disclaimer
--
The information in this Developer Guide has been funded wholly or in part by the United States Environmental Protection Agency. The draft version of this document has not been subjected to the Agency’s peer and administrative review, nor has it been approved for publication as an EPA document. The draft document is currently being edited and reviewed by the Community Modeling and Analysis System Center. Mention of trade names or commercial products does not constitute endorsement or recommendation for use.

Introduction and Executive Summary
====================================

This document is intended to describe general development practices within the CMAQ Modeling Community. The information contained should be read prior to starting a project within the CMAQ framework. Instructions can be used by EPA developers, CMAS-Center developers, or external developers. Notes specific to external developers are made where relevant.

The public CMAQ release repository is located on GitHub (<https://github.com/USEPA/CMAQ>). 
Users should refer to this repository for bug fixes, issues, documentation and major releases for CMAQ. 
Developers intent on submitting code changes should contact the EPA CMAQ development team as soon as possible to discuss their motivation and plans for submitting the code change. 

First, the developer should 'fork' the release repository. 
Developers shall use standard git commands to create a feature branch off of the master branch, and to add and commit changes to that feature branch, not the master branch.

In order to have that feature merged into the CMAQ repository, the developer should follow the instructions on code requirements and repository layout as described in the CMAQ Operational Guidance Document, particularly [Chapter 11](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/CMAQ_OGD_ch11_code_management.md).
Once their feature branch meets requirements for code consistency, benchmark testing, model output evaluation, and documentation including release notes, they may push that branch back to their 'fork' of the CMAQ repository and submit a pull request, with thorough comments, to the CMAQ public repository.  

Submitted bug fixes and science features will undergo a code review within EPA before being merged into the release repository. 
Depending on the size, scope, and importance of the feature, the CMAQ development team may or may not agree to support the feature through future releases. 
Decisions regarding ongoing support will be made on a case-by-case basis.

The following sections outline this code development and review process for CMAQ in greater detail.  

Development Life-cycle
=================================

Prior to the public release of each major CMAQ version, the unofficial source code is released to the public as a development version that is not intended for regulatory or research application use. The purpose of releasing the development version to the public is to give community members:  
1. an opportunity to comment on the code improvements made in the new version.  
2. the ability to take advantage of improvements for preliminary studies of their own interest.
3. a role in helping to test, troubleshoot, and debug the unoffical code before the stable release.
4. a reasonable amount of time to ensure the new version is compatible with any features the member may have submitted in the past.  
5. a reasonable amount of time to complete any pending feature submissions they would like to submit for the stable release.

Version Numbering
----------------------
Public Release Versions
-----------------------
CMAQ will have a number versioning system for each release version branch, using major, minor, and sub-minor increments.
For example, 14.0.2  In this case, the first number (major version), second number (minor version), and third number (patch version) refers to a stable release version. 

The patch version (third number) of CMAQ increments with a new bug fix branch. These minor, interim versions never include feature additions to any of the science modules or shared framework.

The minor version (second number) of CMAQ increments when one or many new science developments have been adpoted. Although these changes may significantly affect model results, the model will still be generally compatible with inputs developed for versions of the same major number.
The major version (first number) of CMAQ increments when significant development changes to the code base have been adopted such that backward compatibility or comparability is no longer expected.

Modifications to the publically released version without incrementing are prohibited in order to ensure consistency among published literature referring to a particular model version. 

Development Versions
-----------------------
The unofficial (or beta) version of the code will first be vetted internally and then released generally 6 months in advance of the stable CMAQ release; this period is known as the "beta-phase". 
Version numbering for the beta series will append the letter 'b' and an incrementing number to the expected version number of the stable release. 
For example, before the hypothetical release of CMAQv14.3 the following series of version numbers would be expected:  
   v14.3.b0  (First tested internal EPA version)
   v14.3.b1  (Reslease to public after minor changes)
   v14.3.b2  (....incremental testing, ....)
   v14.3.b3  (....bug squashing, and  ...)
   v14.3.b4  (....documentation updates...)
   v14.3     (Stable Public Release)  
The number of beta versions is variable among releases. After public release, the instrumented versions of the code (e.g. DDM, ISAM, STM, etc) shall be expected within approximately 6 months to 1 year. 

Making Contributions
==================================

Get in touch
---------------------
Community members with an idea for a code contribution are encouraged to contact the EPA development team well before the beta-phase in order to plan appropriately for the testing and inclusion of the new feature. The team may be interested in knowing the following information:
-   What science module do you intend to develop?
-   What work do you intend to contribute to CMAQ?
-   Are you comfortable with the development strategy including code consistency, benchmarking, configuration testing, compiler testing, model output validation, documentation and merging?
-   Are you able to provide ongoing support and technical guidance for your proposed contribution?
 
Nuts and Bolts
---------------------
As described above, the CMAQ development process follows a "Forking Workflow." Atlassian has provided a helpful [explanation](https://www.atlassian.com/git/tutorials/comparing-workflows#forking-workflow).  

Developers should follow the guidance at [GitHub Help](https://help.github.com/) and [Atlassian](https://www.atlassian.com/git/tutorials/what-is-version-control) in order to:  
-   fork the CMAQ repo: https://help.github.com/articles/fork-a-repo/#platform-linux  
-   clone their newly-created fork: https://help.github.com/articles/cloning-a-repository/#platform-linux  
-   create a feature branch: https://www.atlassian.com/git/tutorials/using-branches  
-   add and commit changes to the new feature branch: https://www.atlassian.com/git/tutorials/saving-changes  
-   push the feature branch to the forked repo: https://help.github.com/articles/pushing-to-a-remote/  
-   submit a pull request to the public CMAQ repo: https://help.github.com/articles/creating-a-pull-request-from-a-fork/  

Code Review
------------------------------
CMAQ Developers at EPA will review all code submissions in order to ensure code stability and consistency, and prevent degradation of model performance. After review, the EPA team will either accept the submission, recommend specific improvements to the submission, or in some cases reject the submission. To avoid outright rejection, we urge developers to contact the EPA team early in the development process and maintain contact throughout to help ensure the submission is compatible with the CMAQ code base and is an attractive addition.  


Code Consistency
------------------------------
Please refer to the Operational Guidance Document, [Chapter 11](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/CMAQ_OGD_ch11_code_management.md), under the section titled "Guidelines for Developing New CMAQ Source Code".
A few minor, but important guidelines include:
- Eliminate global memory references (across modules). This implies no common blocks across modules, no hidden data paths, and no “back doors.”
- All subroutines should be named in a manner which prevents namespace conflicts.
- In general, variable names should be self-descriptive (e.g. NCELLS rather than N).
- Use the Fortran declaration IMPLICIT NONE to maintain some control on typographic errors and undefined variables. The use of IMPLICIT NONE forces the developer to declare all internal variables. This is standard in Fortran 90.
- In general, it is expected that MKS units are used for input and output variables, as these units have been standardized throughout the CMAQ system. 
If you use alternative units, please document this exhaustively.

Benchmark Testing
------------------------------
Dataset: The U.S. EPA Southeast US 12km domain July 1-14, 2011 testing dataset is provided with the CMAQv5.2 Release. This dataset is distributed for benchmarking and testing the model installation. 

Before making code changes, developers should test multiple compilers, multiple processor configurations, and single processor configuration runs for a single simulation day to verify results match the previous stable release, and/or that the answers are computationally and physically reasonable. 
After implementing their new feature, developers should repeat these tests and share the results as part of the pull request review.

Compiler tests use the default benchmark configuration with different compilers and MPI configurations. 
It is important for the user community that CMAQ always compile with Intel Fortran, Gnu Fortran and Portland Group Fortran compilers.
See appendix 1 for an example of a Compiler Test.  

Model Performance Testing
-------------------------------
Configuration tests use one compiler to test the impact of a model change on results.
See appendix 2 for an example of important information to collect when testing science options. 
The developer should consider submitting similar information with their pull request.

Several tools exist to document the effects of compiler choice and code change on model results. Examples include:
### m3diff
Quantify min, max, mean differences between two different model runs
### VERDI
Create absolute difference plots for multiple variables, timesteps, layers (see spatial differences)
### 1:1 Scatter Plots
Plot the differences between two model runs in a concise layout. 
Contact cmas@unc.edu for more information about 1:1 Scatter plot tools. 

Ongoing Feature Support
=============================

Depending on the size, scope, and importance of the feature, the CMAQ development team may or may not agree to support the feature through future releases. For example, bug fixes and minor, but helpful changes to the existing code will likely be incorporated into the general code base and supported. Large code additions, like a new process module or an instrumented version of CMAQ may require more effort to support than can be provided by resources of the EPA Office of Research and Development. However, if the feature is particularly of interest for the CMAQ user community, it may be supported. Decisions regarding ongoing support will be made on a case-by-case basis.

Documentation
==============================

Documentation for CMAQv5.2 is available at https://github.com/USEPA/CMAQ/tree/5.2/DOCS. Materials include:
-   an Operational Guidance Document which describes code structure and regular operation of the model.
-   Release Notes describing code improvements relevant for this model release. Developers should include a Release Note with their submission to ensure proper documentation of CMAQ.
-   several Tutorials that give specific instruction for common tasks like running CMAQ or adding chemical tracers.

Copyright Information
==================================

Contact EPA (CMAQ_Team@epa.gov) with questions and concerns.

CMAQ Developer Guide (c) 2017

Appendix
==================================

Appendix 1: Compiler Tests
--------------------------------
Compiler flags:

- PGI: -Mfixed -O3 -Mextend
- GCC: -ffixed-form -ffixed-line-length-132 -O3 -funroll-loops -finit-character=32
- Intel: -fixed -132 -O3 -override-limits -fno-alias -mp1 -fp-model precise -fp-model source -shared-intel -openmp

- In the Intel Basic Test: -fixed -132 -O3 -openmp
- In the NoOpt Tests: -O0 with extend source and fixed line length flags

Appendix 2: Compiler Tests
--------------------------------
<a id=Table5-1></a>
### Compilation Testing Manifest Table (Example)
|**Scenario**|**Compiler**|**netCDF**|**I/O API**|**MPI_YN(#P)**|**MPI**|**CMAQv5.1 Timing(HH:MM:SS)**|**CMAQ New Project Timing(HH:MM:SS)**| Notes|
|--------------|-----------|---------|---------|---------|-------------|----------------|-------------|------------|
|Gfortran Serial|Gfortran version 4.8.1| 4.3.3|3.1(11/15)|N|N/A|8:19:51|7:35:30|UNC module gcc/4.8.1|
|Gfortran MVAPICH2|Gfortran version 4.8.1|4.3.2|3.1(11/15) |Y (16)|mvapich2-1.7|0:45:55|0:42:40| |
|Intel Serial|Intel Fortran version 16.2.0 |4.3.2|3.1(11/15)|N |N/A |6:01:42|5:10:16|UNC module intel/16.2|
|Intel OpenMPI (EPA Config)|Intel Fortran v15.0.0|4.3.2|3.1(11/15)|Y (16)|openMPI-1.42|0:34:27|UNC module openmpi_intel/15.0|
|Intel OpenMPI|Intel Fortran v16.2.0|4.3.2|3.1(11/15)|Y (16)|openMPI-1.4.2|0:35:29|UNC module openmpi_intel/16.2| 
|Intel MVAPICH2|Intel Fortran v16.2.0|4.3.2|3.1(11/15)|Y (16)|mvapich2-1.7|0:36:34|UNC module mvapich2_intel/16.2| 
|Portland Serial|PG Fortran v16.1|4.3.2|3.1(11/15)|N|N/A|7:33:36|6:26:31|UNC module pgi/16.1|
|Portland OpenMPI|PGI Fortran v15.7|4.3.2|3.1(11/15)|Y (16)|openMPI-1.4.2|0:40:20|0:36:16|UNC module openmpi_pgi/15.7|

 
|**Scenario**|**Description**|**Mechanism**|**Notes**|**Timing(16PE)H:MM:SS**|
|----------------|-------------------|--------------------|--------------------|----------|
|Benchmark Case|Online emissions processing, inline photolysis, inline lightning from MCIP RC, no windblown dust, surface HONO, bidirectional NH3 and Hg, no potential vorticity scaling|cb05e51_ae6_aq |Done; LTNGNO InLine, LTNGPARM = N, LOG_START = 2.0|0:40:20|
|MOSAIC|Benchmark case with MOSAIC and additional stomatal flux files activated|cb05e51_ae6_aq |Done. set CTM_MOSAIC = Y; set CTM_FST = Y|0:44:02 |
|Dust|Benchmark case with dust, including new MODIS FP input|cb05e51_ae6_aq|Done. setenv CTM_WB_DUST Y; setenv CTM_ERODE_AGLAND Y; setenv CTM_WBDUST_BELD BELD3 |0:38:28|
|Hourly NLDN|Benchmark with lightning NOx calculated using hourly bNLDN strikes |cb05e51_ae6_aq |Done; LTNGNO InLine, LTNGPARM = Y, USE_NLDN Y|0:40:18 |
|POA Sensitivity|Benchmark with new POA mechanism |cb05e51_ae6nvPOA_aq|Done|0:34:42 |
 
