CMAQv5.3.b1 (beta version)
==========

Community Multiscale Air Quality Model US EPA CMAQ Website: (https://www.epa.gov/cmaq).

CMAQ is an active open-source development project of the U.S. EPA Computational Exposure Division
that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center: (http://www.cmascenter.org).

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

## The purpose of CMAQv5.3 beta release

CMAQv5.3b1 is a development version that is not intended for regulatory or research application use. The purpose of releasing the development
version is to give community members:

* a reasonable amount of time to complete any pending feature submissions they would like to submit for the stable release.
* a role in helping to test, troubleshoot, and debug the unofficial code before the stable release.
* an opportunity to comment on the code improvements made in the new version.
* the ability to take advantage of improvements for preliminary studies of their own interest.
* a reasonable amount of time to ensure the new version is compatible with any features the member may have submitted in the past.


## Getting the CMAQ Repository
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository.
To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within
a working directory on your server:

```
git clone -b 5.3.b1 https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ.
* **DOCS:** Release notes for the release version of CMAQ, CMAQ User's Guide, developers guidance, short tutorials and known issues.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation, IO-API, etc)

## Documentation
Release notes and Code documentation are included within this repository (they are version-controlled along with the code itself).  
*Note: CMAQv5.3 updates to the Tutorials and User's Guide will be included in the final v5.3 release in Spring 2019.*

[CMAQv5.3 Documentation](DOCS/README.md)   
[CMAQv5.3 Release Notes](DOCS/Release_Notes/README.md)   
[CMAQv5.3 Known Issues](DOCS/Known_Issues/README.md)   
[CMAQv5.3 Tutorials](DOCS/Tutorials/README.md)   
[CMAQv5.3 Developers' Guide](DOCS/Developers_Guide/CMAQ_Dev_Guide.md)   

## CMAQ Test Cases
Benchmark/tutorial input and output data for each CMAQ release version are available from the CMAS Center Software Clearinghouse. From http://www.cmascenter.org, select Download -> Software -> CMAQ and choose the version to get the tutorial data.  

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.    [<img src="https://licensebuttons.net/p/mark/1.0/88x31.png" width="50" height="15">](https://creativecommons.org/publicdomain/zero/1.0/)
