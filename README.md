CMAQv5.3.3+
===========

Community Multiscale Air Quality Model US EPA CMAQ Website: (https://www.epa.gov/cmaq).

The most recent publicly released CMAQ version may be cited using:
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5213949.svg)](https://doi.org/10.5281/zenodo.5213949)

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

## CMAQ version 5.3.3+ Overview:
CMAQv5.3.3+ is a continually evolving branch based on the most recent publicly released CMAQ version (5.3.3). It is intended to be used by researchers or other users who wish to adopt the most recent bugfixes or minor updates that improve model stability, efficiency, etc. CMAQv5.3.3+ has not been thoroughly evaluated or characterized so users must assume the risk of any unforeseen and undocumented impacts of code changes that have been incorporated since the most recent documented public release.

### Proposing updates to a continually evolving branch
Updates will be issued by the CMAQ development team or other users as Pull Requests to this branch. Please follow the default Pull Request template provided by the CMAQ team. Pull Requests will be reviewed by the CMAQ team and merged when and if they are deemed to contribute to the robustness of the model and improve predictions or performance.  

### Examples of updates to a continually evolving branch  
Potential updates include bugfixes that resolves runtime failures, segmentation faults, initialization issues, etc. Other updates could target algorithm inefficiencies that slow the model down. In these cases, it is expected that an improved algorithm would not change model results.  

### Record of changes to CMAQv5.3.3+
|      Tag        |   PR Number       |         PR Name          |   Merge Date     | Brief Description |
| -------------- | ------------------ | ------------------------ | ---------------- | -----------------------|
|[CMAQv5.3.3.1_11Feb2022](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.3.3.1_11Feb2022) | | | | |
| | [#163](https://github.com/USEPA/CMAQ/pull/163)     | Resolve Bug for reading Emission Stream Families        | 2022-02-09 | Bug fix to avoid errors when processing matches for emission stream families related to capitalization. |
|                | [#164](https://github.com/USEPA/CMAQ/pull/164)    | Resolve bug in Low-NOx formation of SOA  | 2022-02-09| A typo in the SOA_DEFN data table resulted in high NOx reactions contributing to low-NOx species, which are quite low in volatility. This bug fix has a considerable impact on SOA if using AE6. No impact for AE7.  |
|                | [#165](https://github.com/USEPA/CMAQ/pull/165)    | Bugfix Speed up emissions processing in PTMET subroutine        | 2022-02-09 | The PTMET subroutine is streamlined so that it is not run for every point source and every stream every time each stream is called. This update drastically reduces runtimes in areas where there happen to be lots of point sources on a particular processor. No impact on results. |



## CMAQ version 5.3.3 Overview:
CMAQv5.3.3 is a minor update to CMAQv5.3 that includes multiple bug fixes. These updates include restoration of windowing capability, allowing users to provide gridded input files which were larger in horizontal extent than the simulated domain. This release also includes a new version of the WRF-CMAQ coupled system with a streamlined build process. Users of WRF-CMAQ are strongly encouraged to try the latest version.  Additionally, MCIP users should note that starting with this release we are aligning the version numbering between CMAQ and MCIP, i.e., the MCIP updates included in this release are labeled MCIPv5.3.3. 
* [CMAQv5.3.3 Release Notes](DOCS/Release_Notes/README.md)   
* [Frequently asked questions for upgrading to the latest CMAQ version](DOCS/Release_Notes/CMAQ_FAQ.md) **- Updated for v5.3.3 release.** 
* [Tutorials for setting up and running CMAQ](DOCS/Users_Guide/Tutorials/README.md) **- Tutorials on running CMAQ, WRF-CMAQ, and CMAQ-ISAM test case.**

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
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository. The most recently released version of the the model will always be on the branch called 'main'. To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within
a working directory on your server:

```
git clone -b main https://github.com/USEPA/CMAQ.git CMAQ_REPO

```


## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ.
* **DOCS:** Release notes for the release version of CMAQ, CMAQ User's Guide, developers guidance, and short tutorials.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, meteorology, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation)

## Documentation
Release Notes and code documentation are included within this repository (they are version-controlled along with the code itself).  

* [CMAQ User's Guide](DOCS/Users_Guide/README.md)   
* [Release Notes](DOCS/Release_Notes/README.md)   
* [Tutorials](DOCS/Users_Guide/Tutorials/README.md)   
* [Developers' Guide](DOCS/Developers_Guide/CMAQ_Dev_Guide.md)   
* [FAQ for upgrading to the latest CMAQ version](DOCS/Release_Notes/CMAQ_FAQ.md) 

## CMAQ Test Cases
Benchmark/tutorial data for each CMAQ release version are available from the CMAS Data Warehouse.  The input and output files are stored on Google Drive with metadata organized through Dataverse.  CMAQv5.3.2 comes with new input and output benchmark data for July 1-2, 2016 over the Southeast US (links provided below). The input datasets for these two days are identical to those released with v5.3.1 except for the addition of a grid mask file for the United States: [GRIDMASK_STATES_12SE1.nc](https://drive.google.com/file/d/16JJ4d6ChBJsvMc_ErqwDBrFfGh2MnVYR/view?usp=sharing). As a result, there is no need for users who have already downloaded the v5.3.1 Southeast benchmark input data to download the v5.3.2 files unless they need the grid mask file for running the new [ISAM test case](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_ISAM.md) or to test out regional emissions scaling with [DESID](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md). The Southeast benchmark output data for v5.3.2 is slightly different from what was released with v5.3 as described in the [CMAQv5.3.2 Rlease Notes FAQ](DOCS/Release_Notes/CMAQ_FAQ.md).

|**CMAQ Version**|**Data Type**|**Domain**|**Simulation Dates**|**Dataverse DOI**| 
|:----:|:----:|:--------------:|:----:|:--------:|
|v5.3|Input| CONUS | Jan 1 - Dec 31, 2016 | https://doi.org/10.15139/S3/MHNUNE |
|v5.3, v5.3.1, v5.3.2, v5.3.3|Input| Southeast US| July 1 - 14, 2016| https://doi.org/10.15139/S3/IQVABD |
|v5.3, v5.3.1|Output| Southeast US| July 1 - 14, 2016|https://doi.org/10.15139/S3/PDE4SS |
|v5.3.2|Output| Southeast US| July 1 - 2, 2016|https://doi.org/10.15139/S3/PDE4SS |
|v5.3.3|Output| Southeast US| July 1 - 2, 2016|https://doi.org/10.15139/S3/PDE4SS |

## Previous CMAQ Versions
The following release versions of CMAQ are currently available on GitHub.  DOI values from Zenodo can be used when referencing a specific version.
* [v5.3.2 (October 2020)](https://github.com/USEPA/CMAQ/tree/5.3.2) - [doi:10.5281/zenodo.4081737](https://doi.org/10.5281/zenodo.4081737) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.3.2/DOCS/Users_Guide/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Users_Guide/Tutorials/README.md) 
* [v5.3.1 (December 2019)](https://github.com/USEPA/CMAQ/tree/5.3.1) - [doi:10.5281/zenodo.3585898](https://doi.org/10.5281/zenodo.3585898) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Users_Guide/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Users_Guide/Tutorials/README.md) 
* [v5.3 (August 2019)](https://github.com/USEPA/CMAQ/tree/5.3) - [doi:10.5281/zenodo.1212601](https://doi.org/10.5281/zenodo.3379043) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.3/DOCS/Users_Guide/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.3/DOCS/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/blob/5.3/DOCS/Users_Guide/Tutorials/README.md) 
* [v5.2.1 (March 2018)](https://github.com/USEPA/CMAQ/tree/5.2.1) - [doi:10.5281/zenodo.1212601](https://zenodo.org/record/1212601) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.2.1/DOCS/User_Manual/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.2.1/CCTM/docs/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/tree/5.2.1/DOCS/Tutorials)  

* [v5.2 (June 2017)](https://github.com/USEPA/CMAQ/tree/5.2) - [doi:10.5281/zenodo.1167892](https://zenodo.org/record/1167892) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/README.md)  | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/Tutorials/README.md)
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
This software/application was developed by the U.S. Environmental Protection Agency (USEPA). No warranty expressed or implied is made regarding the accuracy or utility of the system, nor shall the act of distribution constitute any such warranty. The USEPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the USEPA. The USEPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by the USEPA or the United States Government. 
[<img src="https://licensebuttons.net/p/mark/1.0/88x31.png" width="50" height="15">](https://creativecommons.org/publicdomain/zero/1.0/)
