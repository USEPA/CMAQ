CMAQv5.3.1
==========

Community Multiscale Air Quality Model US EPA CMAQ Website: (https://www.epa.gov/cmaq).

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3585898.svg)](https://doi.org/10.5281/zenodo.3585898)

CMAQ is an active open-source development project of the U.S. EPA's Office of Research and Development that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center: (http://www.cmascenter.org).

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.


## CMAQ version 5.3.1 Overview:
CMAQ model version 5.3.1 is a minor update to CMAQv5.3 that includes multiple bug fixes to both CMAQ and MCIP (released as MCIP version 5.1), as well as a feature addition to the Detailed Emission Scaling Isolation and Diagnostic (DESID) module to allow for the definition of chemical, region and stream families. 
* [CMAQv5.3.1 Release Notes](DOCS/Release_Notes/README.md)   
* [Frequently asked questions for upgrading to the latest CMAQ version](DOCS/Release_Notes/CMAQ_FAQ.md) **- NEW with v5.3.1 release!** 

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
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository. The most recently released version of the the model will always be on the branch called 'master'. To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within
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
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation)

## Documentation
Release Notes and code documentation are included within this repository (they are version-controlled along with the code itself).  

[CMAQv5.3.1 User's Guide](DOCS/Users_Guide/README.md)   
[CMAQv5.3.1 Release Notes](DOCS/Release_Notes/README.md)   
[CMAQv5.3.1 Known Issues](DOCS/Known_Issues/README.md)   
[CMAQv5.3.1 Tutorials](DOCS/Users_Guide/Tutorials/README.md)   
[CMAQv5.3.1 Developers' Guide](DOCS/Developers_Guide/CMAQ_Dev_Guide.md)   
[FAQ for upgrading to the latest CMAQ version](DOCS/Release_Notes/CMAQ_FAQ.md) 

## CMAQ Test Cases
Benchmark/tutorial data for each CMAQ release version are available from the CMAS Data Warehouse.  The input and output files are stored on Google Drive with metadata organized through Dataverse.  CMAQv5.3.1 comes with new input and output benchmark data for July 2016 over the Southeast US (links provided below).  The input datasets are identical to those released wtih v5.3 but additional files are now included in the .tar.gz files that will allow users to test the WRFv4.1.1-CMAQv5.3 coupled model on the Southeast benchmark domain. As a result, there is no need for users who have already downloaded the v5.3 Southeast benchmark input data to download the v5.3.1 files unless they are planning to run the coupled model.  The Southeast benchmark output data for v5.3.1 is slightly different from what was released with v5.3 as described in the [CMAQv5.3.1 Rlease Notes FAQ](DOCS/Release_Notes/CMAQ_FAQ.md).

|**CMAQ Version**|**Data Type**|**Domain**|**Simulation Dates**|**Dataverse DOI**| 
|:----:|:----:|:--------------:|:----:|:--------:|
|v5.3, v5.3.1|Input| Southeast US| July 1 - 14, 2016| https://doi.org/10.15139/S3/IQVABD |
|v5.3, v5.3.1|Output| Southeast US| July 1 - 14, 2016|https://doi.org/10.15139/S3/PDE4SS |
|v5.3|Input| CONUS | Jan 1 - Dec 31, 2016 | https://doi.org/10.15139/S3/MHNUNE |

## Previous CMAQ Versions
The followoing release versions of CMAQ are currently available on GitHub.  DOI values from Zenodo can be used when referencing a specific version.
* [v5.3 (August 2019)](https://github.com/USEPA/CMAQ/tree/5.3) - [doi:10.5281/zenodo.1212601](https://doi.org/10.5281/zenodo.3379043) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.3/DOCS/Users_Guide/README.md) | [Known Issues](https://github.com/USEPA/CMAQ/blob/5.3/DOCS/Known_Issues/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.3/DOCS/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/blob/5.3/DOCS/Users_Guide/Tutorials/README.md) 
* [v5.2.1 (March 2018)](https://github.com/USEPA/CMAQ/tree/5.2.1) - [doi:10.5281/zenodo.1212601](https://zenodo.org/record/1212601) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.2.1/DOCS/User_Manual/README.md) | [Known Issues](https://github.com/USEPA/CMAQ/blob/5.2.1/DOCS/Known_Issues/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.2.1/CCTM/docs/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/tree/5.2.1/DOCS/Tutorials)  

* [v5.2 (June 2017)](https://github.com/USEPA/CMAQ/tree/5.2) - [doi:10.5281/zenodo.1167892](https://zenodo.org/record/1167892) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/README.md) | [Known Issues](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/Known_Issues/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/Tutorials/README.md)
* [v5.1   (December 2015)](https://github.com/USEPA/CMAQ/tree/5.1) - [doi:10.5281/zenodo.1079909](https://zenodo.org/record/1079909)
* [v5.0.2 (April 2014)](https://github.com/USEPA/CMAQ/tree/5.0.2) - [doi:10.5281/zenodo.1079898](https://zenodo.org/record/1079898)
* [v5.0.1 (July 2012)](https://github.com/USEPA/CMAQ/tree/5.0.1)
* [v5.0   (February 2012)](https://github.com/USEPA/CMAQ/tree/5.0) - [doi:10.5281/zenodo.1079888](https://zenodo.org/record/1079888)
* [v4.7.1 (June 2010)](https://github.com/USEPA/CMAQ/tree/4.7.1) - [doi:10.5281/zenodo.1079879](https://zenodo.org/record/1079879)
 
## User Support
* [Frequent CMAQ Questions](https://www.epa.gov/cmaq/frequent-cmaq-questions) are available on our website. 
* [The CMAS User Forum](https://forum.cmascenter.org/) is available for users and developers to discuss issues related to using the CMAQ system.
 [**Please read and follow these steps**](https://forum.cmascenter.org/t/please-read-before-posting/1321) prior to submitting new questions to the User Forum.

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.    [<img src="https://licensebuttons.net/p/mark/1.0/88x31.png" width="50" height="15">](https://creativecommons.org/publicdomain/zero/1.0/)
