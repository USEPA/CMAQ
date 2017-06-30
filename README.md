CMAQv5.2 (Unofficial -- Final Release expected June 30, 2017)
========

**Intent of this CMAQ version:** This 5.2 branch of CMAQ includes the most up-to-date publically available code updates in preparation for the stable release of CMAQv5.2 on June 30, 2017. It is an update to the CMAQv5.2Gamma version with minor code additions and bug fixes that have been identified by EPA, the CMAS team, and the larger CMAQ community. Users wanting to test the CMAQv5.2 code before its release should utilize this 5.2 version. 

Do NOTE that this code is NOT OFFICIAL until the actual release date.

## CMAQ Overview
Community Multiscale Air Quality Model [US EPA Website](https://www.epa.gov/cmaq)

CMAQ is an active open-source development project of the U.S. EPA Computational Exposure Division
that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center (http://www.cmascenter.org).

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

## Getting the CMAQ Repository
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository.
To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within
a working directory on your server:

```
git clone -b 5.2 https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ; also includes technical release notes for the release version of CMAQ.
* **DOCS:** CMAQ User's Guide, developers guidance, and tutorials.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **TUTORIALS:** Short tutorials provide examples of different CMAQ features.
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation, IO-API, etc)

## Documentation
Release notes and Code documentation are included within this repository (they are version-controlled along with the code itself).  

[CMAQv5.2 Documentation](DOCS/README.md)   
[CMAQv5.2 Release Notes](CCTM/docs/Release_Notes/README.md)   

## CMAQ Test Cases
Benchmark/tutorial input and output data for each CMAQ release version are available from the CMAS Center Software Clearinghouse. From http://www.cmascenter.org, select Download -> Software -> CMAQ and choose the version to get the tutorial data.  

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.    [<img src="https://licensebuttons.net/p/mark/1.0/88x31.png" width="50" height="15">](https://creativecommons.org/publicdomain/zero/1.0/)
