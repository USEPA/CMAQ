CMAQv5.2 DDM-3D
========

Community Multiscale Air Quality Model US EPA CMAQ Website: (https://www.epa.gov/cmaq). 
CMAQv5.2 DDM3-D model description: (https://www.epa.gov/cmaq/cmaq-models-0#tab-4)

CMAQ is an active open-source development project of the U.S. EPA Computational Exposure Division
that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center: (http://www.cmascenter.org).

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

## Getting the CMAQ Repository for the first time
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository.
To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within
a working directory on your server:

```
git clone -b 5.2_DDM-3D https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

## Switching branches to 5.2_DDM-3D within an existing local CMAQ Repository
Check on the current status of your repository

```
git status
```
 
Result will be something like this:

``` 
On branch 5.2
Your branch is up-to-date with 'origin/5.2'.
 
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)
 
                modified:   CCTM/scripts/bldit_cctm.csh
                modified:   CCTM/scripts/run_cctm.csh
                modified:   config_cmaq.csh
``` 
 
If there are any files that have been changed locally, that you want to save, use the following command:

```
git stash
```
 
Use the following command to bring yourlocal repository up to date with the EPA github site:

```
git pull
```
 
Result: this will bring down the new branch

``` 
From https://github.com/USEPA/CMAQ
   409467d..68330b8  5.2        -> origin/5.2
   8633653..8fa4f10  5.0.2      -> origin/5.0.2
 * [new branch]      5.2_DDM-3D -> origin/5.2_DDM-3D
``` 
 
Then, to use the new DDM-3D code, switch branches using the command:

```
git checkout 5.2_DDM-3D
```
 
Then use the following command to restore the files that you had modified for your local machine.

```
git stash apply
```

## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ; also includes technical release notes for the release version of CMAQ.
* **DOCS:** CMAQ User's Guide, developers guidance, tutorials and known issues.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **TUTORIALS:** Short tutorials provide examples of different CMAQ features.
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation, IO-API, etc)

## Documentation
Release notes and Code documentation are included within this repository (they are version-controlled along with the code itself).  

[CMAQv5.2 Documentation](DOCS/README.md)   
[CMAQv5.2 Release Notes](CCTM/docs/Release_Notes/README.md)   
[CMAQv5.2 Known Issues](DOCS/Known_Issues/README.md)  
[CMAQv5.2 DDM-3D Documentation](DOCS/Instrumented_Docs/CMAQ_DDM.md)

## CMAQ Test Cases
Benchmark/tutorial input and output data for each CMAQ release version are available from the CMAS Center Software Clearinghouse. From http://www.cmascenter.org, select Download -> Software -> CMAQ and choose the version to get the tutorial data.  

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.    [<img src="https://licensebuttons.net/p/mark/1.0/88x31.png" width="50" height="15">](https://creativecommons.org/publicdomain/zero/1.0/)
