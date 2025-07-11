CMAQv5.5+
==========

The most recent publicly released CMAQ version may be cited using:
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13883210.svg)](https://doi.org/10.5281/zenodo.13883210)

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq

CMAQ is an active open-source development project of the U.S. EPA's Office of Research and Development that consists of a suite of programs for conducting air quality model simulations. CMAQ is supported by the CMAS Center: http://www.cmascenter.org

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

Please see our 'How to Cite CMAQ' page if you are interested in referencing one of our released model versions, scientific algorithms, or model output in your own publication: https://www.epa.gov/cmaq/how-cite-cmaq

## CMAQ version 5.5+ Overview:
CMAQv5.5+ is a continually evolving branch based on the most recent publicly released CMAQ version (5.5). It is intended to be used by researchers or other users who wish to adopt the most recent bugfixes or minor updates that improve model stability, efficiency, etc. CMAQv5.5+ has not been thoroughly evaluated or characterized so users must assume the risk of any unforeseen and undocumented impacts of code changes that have been incorporated since the most recent documented public release.

### Proposing updates to a continually evolving branch
Updates will be issued by the CMAQ development team or other users as Pull Requests to this branch. Please follow the default Pull Request template provided by the CMAQ team. Pull Requests will be reviewed by the CMAQ team and merged when and if they are deemed to contribute to the robustness of the model and improve predictions or performance.  

### Examples of updates to a continually evolving branch  
Potential updates include bugfixes that resolves runtime failures, segmentation faults, initialization issues, etc. Other updates could target algorithm inefficiencies that slow the model down. In these cases, it is expected that an improved algorithm would not change model results.  
 
### Record of changes to CMAQv5.5+

|      Tag        |   PR Number       |         PR Name          |   Merge Date     | Brief Description |
| -------------- | ------------------ | ------------------------ | ---------------- | -----------------------|
| [CMAQv5.5.0.3_11Jul2025](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.5.0.3_11Jul2025)|[#253](https://github.com/USEPA/CMAQ/pull/253)|Diagnostic NH3 emissions from Agriculture and Biogenic sources | 2025-07-11 | adds NH3 diagnostic emissions from agricultural and biogenic sources when using the STAGE deposition option and bidirectional NH3 exchange to support 2023 and later National Emissions Inventory development.|
| |[#252](https://github.com/USEPA/CMAQ/pull/252)|enable parallel I/O for ELMO, AELMO, and lightning diagnostic files | 2025-05-28 | Bugfix to avoid a model crash when using a parallel file system. This resolves an [issue](https://forum.cmascenter.org/t/unable-to-write-to-aelmo-even-though-new-aelmo-file-successfully-created/5762/20) raised on the CMAS User Forum.|
| |[#249](https://github.com/USEPA/CMAQ/pull/249)|Increase string size for DDM-3D control file | 2025-05-14 | Updates allows for longer strings of emissions stream names and region names for DDM-3D simulations.|
| [CMAQv5.5.0.2_13May2025](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.5.0.2_13May2025)  |[#246](https://github.com/USEPA/CMAQ/pull/246)|5.5 Tutorial Updates from CMAS  | 2025-04-29 | Improvements to the WRF-CMAQ Tutorial and supporting documentation for preparing your compute environment prior to running CMAQ.|
| |[#245](https://github.com/USEPA/CMAQ/pull/245)|Fix bugs in shp2cmaq | 2025-04-29 | Bugfix to shp2cmaq tool to avoid errors when CMAQ Emission Control reads the time information from the CMAQ-ready grid mask file.|
| |[#242](https://github.com/USEPA/CMAQ/pull/242)|Updated Post-Processing support files and code  | 2025-04-29 | Bug fixes and minor enhancements.|
| |[#241](https://github.com/USEPA/CMAQ/pull/241)|Adds pcVOC and NOy species to ISAM species lists  | 2025-04-29 | This change supports comprehensive accounting of ISAM source attribution of PM, ozone, and other pollutants.|
| |[#240](https://github.com/USEPA/CMAQ/pull/240)|Update conversion factor for area-based denominator  | 2025-04-29 | Bugfix will affect predictions for any species that includes emissions normalized to a specific area that is not in units of meters.|
| |[#239](https://github.com/USEPA/CMAQ/pull/239)| DDM3D Correction to ebi solvers | 2025-05-13 | DDM3D sensitivity calculations were updated for all ebi solvers to resolve some instabilities.|
| |[#238](https://github.com/USEPA/CMAQ/pull/238)|Corrections to ISAM aerosol and cloud processing  | 2025-04-29 | Resolve some spurious unreasonable results reported by CMAQ-ISAM users.|
|[CMAQv5.5.0.1_19Mar2025](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.5.0.1_19Mar2025) |[#231](https://github.com/USEPA/CMAQ/pull/231)|CRACMM2 fixes for ISAM and post processing  | 2025-03-19 | Small changes in post-processed concentrations due to missing species. Resolves issue [#221](https://github.com/USEPA/CMAQ/issues/221). |
| |[#226](https://github.com/USEPA/CMAQ/pull/226)|Fix bug preventing CMAQ from running using SAPRC mechanisms  | 2024-12-06 | The fix is to specify that CLNO2 does undergo dry deposition to avoid a model crash when using any of the SAPRC07 mechanisms.|
| |[#222](https://github.com/USEPA/CMAQ/pull/222)|Corrected NLCD to BELD3 mapping for shrub/scrub and dwarf scrub| 2024-12-06 | Corrects an error in NLCD40 land use mapping for inline windblown dust calculations. Addresses issues [#220](https://github.com/USEPA/CMAQ/issues/220).|

## Getting the CMAQ Repository
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository. The most recently released version of the the model will always be on the branch called 'main'. To clone code from the 5.5+ bugfix branch from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within a working directory on your server:

```
git clone -b 5.5+ https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

or 
```
git clone -b 5.5+ git@github.com:USEPA/CMAQ.git CMAQ_REPO
```

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
