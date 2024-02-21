CMAQv5.4
==========

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7218076.svg)](https://doi.org/10.5281/zenodo.7218076)

CMAQ is an active open-source development project of the U.S. EPA's Office of Research and Development that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center: http://www.cmascenter.org

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.


## CMAQ version 5.4 Overview:

* [Frequently asked questions for upgrading to the latest CMAQ version](https://github.com/USEPA/CMAQ/wiki/CMAQv5.4-Series-FAQ)) **- Updated for v5.4 release.** 
* [CMAQv5.4 Release Notes](https://github.com/USEPA/CMAQ/wiki/CMAQ-Release-Notes) - Release Notes are available on the CMAQ GitHub wiki.
* [Tutorials for setting up and running CMAQ](DOCS/Users_Guide/Tutorials/README.md) **- Tutorials on running CMAQ, CMAQ-ISAM, CMAQ-DDM, and WRF-CMAQ test cases.**

## New features in CMAQ version 5.4 include:

* Updated chemistry for ozone (O3) and particulate matter (PM) formation from global-to-local scales
* Introduction of the Community Regional Atmospheric Chemistry Multiphase Model (CRACMM)
* Biogenic emissions algorithm options have been expanded
* Revised algorithms for modeling the dry deposition of particles from the atmosphere (M3DRY and STAGE updates)
* Streamlined building of the coupled WRF-CMAQ system compatible with WRFv4.4+ 
* Improved efficiency, accuracy, and user experience for CMAQ instrumented model extensions CMAQ-DDM-3D and CMAQ-ISAM
* Expansion of emissions diagnostic output features
* Introduction of a domain-wide Budget Reporting tool 
* Online integration of common pollutant post-processing tasks (i.e. output total PM2.5 mass and more directly!)
* Community Contribution: Incorporation of the Two-Dimensional Volatility Bases Set [(2D-VBS)](https://github.com/USEPA/CMAQ/tree/2DVBS) chemical mechanism
* **See the full list of CMAQv5.4 updates on our new CMAQ Wiki page. [**CMAQv5.4 Updates**](https://github.com/USEPA/CMAQ/wiki/CMAQv5.4-Series-FAQ#do-i-need-to-update-from-v533-to-v54)**

## Important update for WRF-CMAQ users
A bug was identified within the CMAQ to WRF coupling routine (twoway_feedback.F90) where aerosol feedback information is transferred from CMAQ to WRF. In doing so, it was found that WRF was not receiving the correct aerosol feedback information due to a looping error relating to the number of layers set to 1 in some cases. The bug impacts the WRF-CMAQ coupled system in the CMAQv5.3 release series (v5.3, v5.3.1, v5.3.2, v5.3.3) when running with short wave radiative feedback. The bug was not present in prior WRF-CMAQ versions. The bugfix in CMAQv5.4 now correctly captures the variations in the aerosol optical properties and consequently the direct feedback effects through all layers.  **Users of WRF-CMAQ are strongly encouraged to update to CMAQv5.4.** 

## Getting the CMAQ Repository
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository. The most recently released version of the the model will always be on the branch called 'main'. To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within
a working directory on your server:

```
git clone -b main https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ.
* **DOCS:** CMAQ User's Guide, developers guidance, and short tutorials.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, meteorology, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **PYTOOLS:** Python pre- and postprocessing tools (currently this includes the DMSCHLO preprocessor)
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation)

## Documentation
Code documentation is included within this repository (they are version-controlled along with the code itself).  

* [FAQ for upgrading to the latest CMAQ version](https://github.com/USEPA/CMAQ/wiki/CMAQv5.4-Series-FAQ) 
* [CMAQv5.4 Release Notes](https://github.com/USEPA/CMAQ/wiki/CMAQ-Release-Notes)   
* [Tutorials](DOCS/Users_Guide/Tutorials/README.md)   
* [CMAQ User's Guide](DOCS/Users_Guide/README.md)   
* [Developers' Guide](DOCS/Developers_Guide/CMAQ_Dev_Guide.md)   


## CMAQ Test Cases   
Benchmark/tutorial data for the CMAQv5.4 release are available from the CMAS Data Warehouse.  The input and output files are stored on Google Drive and on Amazon Web Services (AWS) Open Data Registry.  CMAQv5.4 comes with new input and output benchmark data for July 1-2, 2018 over the Northeast US (links provided below). Tutorials are provided for using the benchmark data to test running of the base [CMAQ model](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md), [WRF-CMAQ](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_WRF-CMAQ_Benchmark.md), [CMAQ-ISAM](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_ISAM.md), and [CMAQ-DDM-3D](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_DDM.md).  The input datasets include a grid mask file for the United States (GRIDMASK_STATES_12SE1.nc). The grid mask file is used for running the new ISAM and DDM-3D test cases, or to test out regional emissions scaling with [DESID](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md).  The input datasets also include an ocean file with new variables needed to use the cb6r5_ae7 and cb6r5m_ae7 mechanisms.  See the [Ocean File tutorial](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_oceanfile.md) for more information on changes to the required ocean file input in v5.4.  

In addition, a full set of inputs for 2018 are provided for the 12US1 domain (299 column x  459 row x 35 layer, 12-km horizontal grid spacing) on AWS, including emissions compatible with both the CB6r5 and CRACMM chemical mechanisms.  Note that the 12US1 inputs are  netCDF-4/HDF5 compressed files to substantially reduce file sizes. Through testing at the EPA, we’ve noticed that certain domains encounter model crashes from reading in large amounts of compressed netCDF data.  A work around for those cases is uncompressing the data manually via [nccopy 1](https://www.unidata.ucar.edu/software/netcdf/workshops/2011/utilities/Nccopy.html) or [m3cple](https://www.cmascenter.org/ioapi/documentation/all_versions/html/M3CPLE.html) (compiled with HDF5) before running the CMAQ simulation.

|**CMAQ Version**|**Data Type (Size)**|**Domain**|**Simulation Dates**|**Data Access**| 
|:----:|:----:|:--------------:|:----:|:--------:|
|v5.4 CB6|Input (10.3 Gb)| Northeast US| July 1 - 2, 2018| [Metadata, DOI, and download instructions ](https://doi.org/10.15139/S3/BWMI8X) <br /> [Google Drive Link](https://drive.google.com/drive/folders/1AFUB-4kzIXXoZr4hOHNBqRvy9JQ9_MDp)  <br /> [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html)|
|v5.4 CB6|Output (13.9 Gb)| Northeast US| July 1 - 2, 2018|[Metadata, DOI, and download instructions ](https://doi.org/10.15139/S3/BWMI8X) <br />[Google Drive Link](https://drive.google.com/drive/folders/1AFUB-4kzIXXoZr4hOHNBqRvy9JQ9_MDp)  <br /> [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html) |
|v5.4 CB6 | Input | 12US1 | Jan 1 - Dec 31, 2018 | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/LDTWKH) |
|v5.4 CRACMM | Input | 12US1 | Jan 1 - Dec 31, 2018 | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/9AV907) |

## User Support
* [Frequent CMAQ Questions](https://www.epa.gov/cmaq/frequent-cmaq-questions) are available on our website. 
* [The CMAS User Forum](https://forum.cmascenter.org/) is available for users and developers to discuss issues related to using the CMAQ system.
 [**Please read and follow these steps**](https://forum.cmascenter.org/t/please-read-before-posting/1321) prior to submitting new questions to the User Forum.

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

* [Open source license](license.md)
