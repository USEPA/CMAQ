CMAQv5.5
==========

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13883210.svg)](https://doi.org/10.5281/zenodo.13883210)

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq

CMAQ is an active open-source development project of the U.S. EPA's Office of Research and Development that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center: http://www.cmascenter.org

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

Please see our 'How to Cite CMAQ' page if you are interested in referencing one of our released model versions, scientific algorithms, or model output in your own publication: https://www.epa.gov/cmaq/how-cite-cmaq

## CMAQ version 5.5 Overview:

* [Frequently asked questions for upgrading to the latest CMAQ version](https://github.com/USEPA/CMAQ/wiki/CMAQv5.5-Series-FAQ) **- Updated for v5.5 release.** 
* [CMAQv5.5 Release Notes](https://github.com/USEPA/CMAQ/wiki/CMAQ-Release-Notes) - Release Notes are available on the CMAQ GitHub wiki.
* [Tutorials for setting up and running CMAQ](DOCS/Users_Guide/Tutorials/README.md) **- Tutorials on running CMAQ, CMAQ-ISAM, CMAQ-DDM, and WRF-CMAQ test cases.**

## New features in CMAQ version 5.5 include:

* Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) version 2 including updated formaldehyde chemistry impacting ozone and secondary organic aerosol formation
* New support for running pre-configured global CMAQ simulations coupled with meteorology from the Model for Prediction Across Scales – Atmosphere (MPAS-A) 
* Expanded capabilities of the Integrated Source Apportionment Method (ISAM) to quantify source contributions to total secondary organic aerosol (SOA) and individual species
* Updates to ISAM source attribution estimates, mainly impacting coarse particles and secondary organic aerosols formed through cloud processes
* Updates to the Decoupled Direct Method (DDM) to improve second order ozone sensitivities  
* Updated chemistry to properly capture photolysis effects from sub-grid clouds
* Revised algorithms for modeling dry deposition (M3DRY and STAGE updates)
* Improved accuracy and error checking for BDSNP soil NO in the MEGAN biogenic emissions algorithm 
* MCIP (meteorology pre-preprocessor) updates to grid origin definition for fine scale Lambert Conformal Grids (i.e., < 4km )
* Updates to Sulfur Tracking Model (STM) to properly attribute sulfate from gas phase chemistry
* Updates to the Explicit and Lumped Model Output (ELMO) synthesizer to fix erroneous output for several PM aggregates including PMF_OC, PMF_NCOM, TNO3
* New shp2cmaq python tool to convert GIS shapefiles into gridded netCDF mask files that can be used for defining regions and region families with DESID and using geographic source regions when running CMAQ-ISAM
* Simplified workflows for easier CMAQ installation

* **See the full list of CMAQv5.5 updates on our new CMAQ Wiki page. [**CMAQv5.5 Updates**](https://github.com/USEPA/CMAQ/wiki/CMAQv5.5-Series-FAQ#do-i-need-to-update-from-v54-to-v55)**

## Important note for WRF-CMAQ users
Coupled WRF-CMAQv5.5 [(Wong et al., 2010)](https://doi.org/10.5194/gmd-5-299-2012) is compatible with WRF versions 4.4 to 4.5.1.  EPA's testing of WRF-CMAQ has included chemical mechanisms CB6r5 and CRACMMv1 with the M3DRY dry deposition scheme.  Other model options can be used with the WRF-CMAQ model but will have limited user support for issues that are encountered. See the [WRF-CMAQv5.5 Release Note](https://github.com/USEPA/CMAQ/wiki/CMAQ-Release-Notes:-WRF-CMAQ-Coupled-Model#compatibility-issues-with-wrf-versions-452-and-later) for more information.  

## Getting the CMAQ Repository
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository. The most recently released version of the the model will always be on the branch called 'main'. To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within a working directory on your server:

```
git clone -b main https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

or
```
git clone -b main git@github.com:USEPA/CMAQ.git CMAQ_REPO
```

### CMAQv5.5 Bug Fixes
Users who wish to adopt the most recent bugfixes or minor updates to v5.5 should review the tagged versions of v5.5+:  
**[Record of changes to CMAQv5.5](https://github.com/USEPA/CMAQ/wiki/CMAQ-Bugfix-Branch#record-of-changes-to-cmaqv55)**

To clone code with all of the CMAQv5.5 bug fixes issue the following command from within a working directory on your server:
```
 git clone -b 5.5+ https://github.com/USEPA/CMAQ.git CMAQ55plus_REPO
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

* [FAQ for upgrading to the latest CMAQ version](https://github.com/USEPA/CMAQ/wiki/CMAQv5.5-Series-FAQ) 
* [CMAQv5.5 Release Notes](https://github.com/USEPA/CMAQ/wiki/CMAQ-Release-Notes)
* [Tutorials](DOCS/Users_Guide/Tutorials/README.md)   
* [CMAQ User's Guide](DOCS/Users_Guide/README.md)   
* [Developers' Guide](DOCS/Developers_Guide/CMAQ_Dev_Guide.md)

## CMAQ Test Cases 
Test case input and output data for the CMAQv5.5 release are available from the CMAS Data Warehouse. Step-by-step benchmark tutorials using the test case data are provided in the GitHub repo. 
* [CMAQ Test Case Data](DOCS/Test_Case_Data.md)


## Other Online Resources 
* [Resources for Running CMAQ on Amazon Web Services](https://www.epa.gov/cmaq/cmaq-resourcesutilities-model-users#cmaq-on-the-cloud)
* [Software Programs for Preparing CMAQ Inputs](https://www.epa.gov/cmaq/cmaq-resourcesutilities-model-users#prepare_cmaq_inputs)
* [Software Programs for Evaluating and Visualizing CMAQ Outputs](https://www.epa.gov/cmaq/cmaq-resourcesutilities-model-users#evaluate_visualize_cmaq)
* [2000 - 2023 air quality observation data from the CMAS Center Data Warehouse](https://drive.google.com/drive/u/1/folders/1QUlUXnHXvXz9qwePi5APzzHkiH5GWACw) - These files are formatted to be compatible with the [Atmospheric Model Evaluation Tool](https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool).
  
## User Support
* [Frequent CMAQ Questions](https://www.epa.gov/cmaq/frequent-cmaq-questions) are available on our website.
* [Debugging tips](https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_debug.md) are included with the CMAQ tutorials. 
* [The CMAS User Forum](https://forum.cmascenter.org/) is available for users and developers to discuss issues related to using the CMAQ system.
 [**Please read and follow these steps**](https://forum.cmascenter.org/t/please-read-before-posting/1321) prior to submitting new questions to the User Forum.

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

* [Open source license](license.md)
