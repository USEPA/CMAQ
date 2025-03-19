# Utilities

## Update Chemistry Utilities for More Recent Versions of pgi Compilers

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix, New Feature  
**Release Version/Date**: CMAQ Version 5.5  

**Description**:  Two sets of changes were made to the chemistry utilities. One set of changes changes makefile pgi compiler and link flags for _chemmech_, _inline_phot_preproc_, and _create_ebi_ so building executables do not fail when using version of pgi greater than 17.4 (_nvhpc compilers_). For the second set of changes, the _chemmech_ utility will stop and writes error messages to the screen or re-directed file when the mechanism definitions file has duplicated reaction labels. To prevent the error for the cracmm-based mechanisms, their second occurrence of R132 has been changed to R133.  

The pull request also changes chemmech's build script so the makefile copied to the build directory uses the requested compiler by default. The change supports rebuilding chemmech with the requested compiler without re-using the build script.   

**Significance and Impact**:  Code change do not change model predictions but accomplish two goals. First, they allow building a mechanism (i.e., create its _RXNS\_DATA\_MODULE.F90_, _RXNS\_FUNC\_MODULE.F90_, _CSQY\_DATA_ and _ebi solver_ files) with nvfortran or recent version of pgf90 so a developer can perform the task using the two compilers. Second, chemmech give error messages to a developer when a mechanism definitions file uses reaction label more than one time. The problem impedes using IRR analysis because IRR control files reference reactions by their label and because a mechanism definition files can set reaction rate constants by referencing labels for other reactions.  
      
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1069](https://github.com/USEPA/CMAQ/commit/f27f30b92f85337e573d50f1516c24a5344fbc5a) | [PR#1069](https://github.com/USEPA/CMAQ_Dev/pull/1069)  | 


## Remove Typographic Errors in CHEMMECH Output Files

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Documentation  
**Release Version/Date**: CMAQ Version 5.5  
**Description**: Updates remove typographic errors in CHEMMECH output files documenting a chemical mechanism such as the markdown files found under the repository's _**CCTM/src/MECHS/mechanism_information**_ directory.  
**Significance and Impact**:  Updates have no effect on CCTM predictions but supporting documenting a photochemical mechanism's reactions.
   
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#964](https://github.com/USEPA/CMAQ/commit/1e8fa83fea7b6b4d0b476d755779e072c0ee6d5f) | [PR#964](https://github.com/USEPA/CMAQ_Dev/pull/964)  | 

## Remove Conflicting Filenames on Window and Macintosh Operating Systems

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQ Version 5.4  
**Description**: Both the create_ebi and inline_phot_prerproc have files in their source code directories that only differ in letter case in filenames such as _**UTIL/create_ebi/junit.F**_ and _**UTIL/create_ebi/junit.f**_. The property does not cause problems on Linux systems but can cause problems on Window and Macintosh systems. The problem is easily removed because only one file is needed or neither file is needed to build these chemistry utilities. The pull request removes the unneeded files causing the problems as well as removing two unneeded files under _**UTIL/inline_phot_preproc/src**_.   
**Significance and Impact**:  None, but allows to more efficiently use the CMAQ repository on Windows and Macintosh operating systems.  
     
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#899](https://github.com/USEPA/CMAQ/commit/fb1683a0497db9343fbc8158c5acdea888442124) | [PR#899](https://github.com/USEPA/CMAQ_Dev/pull/899)  | 

## Autochem
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: New Feature  
**Release Version/Date**: CMAQ Version 5.4  
**Description**:
Introduction of the Autochem utility script that processes new chemical mechanism files, including EBI files if necessary, for user-defined mechanisms when mech.def and chemical namelist files are provided. The script executes utilities that are already present in the CMAQ repo including chemmech and create_ebi. It also copies and transfers files among directories that are used for driving the utilities. 

The purpose of Autochem is to reduce the burden of introducing new chemical mechanisms to CMAQ. The script can be executed as part of the bldit_cctm.csh script. Just uncomment the line containing:  
set build_mech  
By default, the script will copy any new mechanism files produced back into the CMAQ repo so they can be used in the future. If there are existing mechanism files there that you would like to overwrite, uncomment  
set clobber_mech  

**Significance and Impact**:  None, but allows to more efficiently use the CMAQ repository.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#643](https://github.com/USEPA/CMAQ/commit/3e2a8fd5ede4751addcb638f3dabcbe564854ed5) | [PR#643](https://github.com/USEPA/CMAQ_Dev/pull/643)  | 
|[Merge for PR#770](https://github.com/USEPA/CMAQ/commit/2ab818e06e3d929d5f59b1d36112bd251aed932f) | [PR#770](https://github.com/USEPA/CMAQ_Dev/pull/770)  | 
  

## New Output Files and Runtime Options for CHEMMECH

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Code and Documentation Enhancements  
**Release Version/Date**: CMAQ Version 5.4      
**Description**:  The CHEMMECH utility was updated to accomplish the below items.

1. Add an option that revises the mechanism definition based on how reactions change the total number of elements between reactants and products. The elements consider is a subset of all chemical elements. If a reaction does not balance the initial and final number of elements, the revised file appends the reaction with the variables and coefficients measuring the unbalance. For each unbalance element, the added variable is called DELTA_element. DELTA_element's are not active chemistry species so CHEMMECH does not output information to solve how the DELTA_element's evolve over time. Both these output files are written to the output directory.    

2. Produce four matlab scripts to support the F0AM modeling that are produced by the chemmech utility. 

3. Revise the chemmech README page to include the new option as well the output files supporting F0AM box-model. A previous pull request added the F0AM outputs. After this numbered list, an relevant excerpt is given.

4. Fix a bug in chemmech that add stoichiometric coefficients for product when the products are atmospheric constants such as the reaction. The coefficient for M should not be added because M is not counted as product.

         <R2> O + O2 + M = O3 + M # 6.00E-34^-2.6;

5. Revise the chemmech source code to reduce the number of complex INTERFACE blocks. The change uses FORTRAN modules to accomplish the task. The module files combine several code files into a one file.

Consult UTIL/chemmech/README.md for more Items 1 and 2.

**Significance and Impact**:  (1) Allows users to run the F0AM box-model when creating a CMAQ mechanism via the chemmech utility. (2) Adds to chemmech a run-time option revising the input mechanism definitions file for how each reaction changes the balance of elements such as carbon, nitrogen, and sulfur.            

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#845](https://github.com/USEPA/CMAQ/commit/773eff8f8f6c432494b116648a4b95188b77db6a) | [PR#845](https://github.com/USEPA/CMAQ_Dev/pull/845)  | 
|[Merge for PR#913](https://github.com/USEPA/CMAQ/commit/f0673e722b5372ee38254f39130b5d59c50b14b8) | [PR#913](https://github.com/USEPA/CMAQ_Dev/pull/913)  | 
