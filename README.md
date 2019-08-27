CMAQv5.3
==========

Community Multiscale Air Quality Model US EPA CMAQ Website: (https://www.epa.gov/cmaq).

CMAQ is an active open-source development project of the U.S. EPA's Office of Research and Development that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center: (http://www.cmascenter.org).

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

## New features in CMAQ version 5.3 include:
* Simplified emissions scaling
* Improved representation of natural aerosols 
* Expanded capability for ecological applications 
* Stream-lined CMAQ-ISAM and CMAQ-STM
* Updates to pre-processors including ICON, BCON and MCIP
* Enhanced scientific complexity with faster run times
* Fully revised User's Guide and tutorials
* Additional updates are documented in the Release Notes

## Getting the CMAQ Repository
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository.
To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within
a working directory on your server:

```
git clone -b master https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ.
* **DOCS:** Release notes for the release version of CMAQ, CMAQ User's Guide, developers guidance, short tutorials and known issues.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, meteorology, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation, IO-API, etc)

## Documentation
Release Notes and code documentation are included within this repository (they are version-controlled along with the code itself).  

[CMAQv5.3 User's Guide](DOCS/Users_Guide/README.md)   
[CMAQv5.3 Release Notes](DOCS/Release_Notes/README.md)   
[CMAQv5.3 Known Issues](DOCS/Known_Issues/README.md)   
[CMAQv5.3 Tutorials](DOCS/Users_Guide/Tutorials/README.md)   
[CMAQv5.3 Developers' Guide](DOCS/Developers_Guide/CMAQ_Dev_Guide.md)   

## CMAQ Test Cases
Benchmark/tutorial data for each CMAQ release version are available from the CMAS Data Warehouse.  The input and output files are stored on Google Drive with metadata organized through Dataverse.  

|**Data Type**|**Domain**|**Simulation Dates**|**Dataverse DOI**| 
|:----:|:--------------:|:----:|:--------:|
|Input| Southeast US| July 1 - 14, 2016| https://doi.org/10.15139/S3/IQVABD |
|Output| Southeast US| July 1 - 14, 2016|https://doi.org/10.15139/S3/PDE4SS |
|Input| CONUS | Jan 1 - Dec 31, 2016 | https://doi.org/10.15139/S3/MHNUNE |

## Previous CMAQ Versions
The followoing release versions of CMAQ are currently available on GitHub.  DOI values from Zenodo can be used when referencing a specific version.
* [v5.2.1 (March 2018)](https://github.com/USEPA/CMAQ/tree/5.2.1) - [doi:10.5281/zenodo.1212601](https://zenodo.org/record/1212601) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.2.1/DOCS/User_Manual/README.md) | [Known Issues](https://github.com/USEPA/CMAQ/blob/5.2.1/DOCS/Known_Issues/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.2.1/CCTM/docs/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/tree/5.2.1/DOCS/Tutorials)  

* [v5.2 (June 2017)](https://github.com/USEPA/CMAQ/tree/5.2) - [doi:10.5281/zenodo.1167892](https://zenodo.org/record/1167892) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/README.md) | [Known Issues](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/Known_Issues/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/tree/5.2/DOCS/Tutorials)
* [v5.1   (December 2015)](https://github.com/USEPA/CMAQ/tree/5.1) - [doi:10.5281/zenodo.1079909](https://zenodo.org/record/1079909)
* [v5.0.2 (April 2014)](https://github.com/USEPA/CMAQ/tree/5.0.2) - [doi:10.5281/zenodo.1079898](https://zenodo.org/record/1079898)
* [v5.0.1 (July 2012)](https://github.com/USEPA/CMAQ/tree/5.0.1)
* [v5.0   (February 2012)](https://github.com/USEPA/CMAQ/tree/5.0) - [doi:10.5281/zenodo.1079888](https://zenodo.org/record/1079888)
* [v4.7.1 (June 2010)](https://github.com/USEPA/CMAQ/tree/4.7.1) - [doi:10.5281/zenodo.1079879](https://zenodo.org/record/1079879)
 

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.    [<img src="https://licensebuttons.net/p/mark/1.0/88x31.png" width="50" height="15">](https://creativecommons.org/publicdomain/zero/1.0/)
