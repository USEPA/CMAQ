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
Benchmark/tutorial data for the CMAQv5.5 release are available from the CMAS Data Warehouse.  The input and output files are stored on Amazon Web Services (AWS) Open Data Registry.  CMAQv5.5 benchmark input is the same as CMAQv5.4, provding a July 1-2, 2018 case over the Northeast US.  CMAQv5.5 comes with new output data for running several different model configurations (links below).  Tutorials are provided for using the benchmark data to test running of the base CMAQ model with either the CB6r5 or CRACMMv2 mechanisms, WRF-CMAQ, CMAQ-ISAM, and CMAQ-DDM. The input datasets include a grid mask file for the United States (GRIDMASK_STATES_12SE1.nc). The grid mask file is used for running the ISAM test case, or to test out regional emissions scaling with [DESID](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md).  The input datasets also include an ocean file with variables needed to use the cb6r5_ae7 and cb6r5m_ae7 mechanisms. See the [Ocean File tutorial](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_oceanfile.md) for more information on changes to the required ocean file input beginning in v5.4.  

In addition, a full set of inputs for 2018 are provided for the 12US1 domain (299 column x  459 row x 35 layer, 12-km horizontal grid spacing) on AWS, including emissions compatible with both the CB6r5 and CRACMMv1.0 chemical mechanisms.  Note that the 12US1 inputs are  netCDF-4/HDF5 compressed files to substantially reduce file sizes. Through testing at the EPA, we’ve noticed that certain domains encounter model crashes from reading in large amounts of compressed netCDF data.  A work around for those cases is uncompressing the data manually via [nccopy 1](https://www.unidata.ucar.edu/software/netcdf/workshops/2011/utilities/Nccopy.html) or [m3cple](https://www.cmascenter.org/ioapi/documentation/all_versions/html/M3CPLE.html) (compiled with HDF5) before running the CMAQ simulation.

|**CMAQ Version**|**Data Type (Size)**|**Domain**|**Simulation Dates**|**Data Access**|**Tutorial**| 
|:----:|:----:|:--------------:|:----:|:--------:|:----:|
|MPAS-CMAQ| Input (215 GB) | Global (uniform 120) | Jan 1, 2017|[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/PAHQFO)  <br /> [AWS Link](https://mpas-cmaq.s3.amazonaws.com/index.html) |[Tutorial](https://github.com/USEPA/CMAQ/blob/MPAS_CMAQ/DOCS/Users_Guide/PDF/MPAS_CMAQ_guide.pdf)|
|v5.4 CB6r5 | Input (6.1 TB) | 12US1 | Jan 1 - Dec 31, 2018 | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/LDTWKH)  <br /> [AWS Link](https://cmas-cmaq-modeling-platform-2018.s3.amazonaws.com/index.html) ||
|v5.4 CB6r5 | Input (10.3 GB)| Northeast US| July 1 - 2, 2018| [Metadata, DOI, and download instructions ](https://doi.org/10.15139/S3/BWMI8X) <br /> [Google Drive Link](https://drive.google.com/drive/folders/1AFUB-4kzIXXoZr4hOHNBqRvy9JQ9_MDp)  <br /> [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_4/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz)||
|v5.5 CRACMM2| Input (6 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS]( https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/CMAQv5.5_2018_12NE3_Benchmark_cracmm2_stage_2Day_Input.tar.gz) ||
|v5.5 CRACMM2| Output (19 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS]( https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cracmm2_stage.tar.gz)|[Tutorial](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark_cracmm2_stage.md)|
|v5.5 CB6r5 M3Dry | Output (15 GB) | 12NE3 | July 1 - 2, 2018 |  [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)|
|v5.5 CB6r5 STAGE | Output (16 GB) | 12NE3 | July 1 - 2, 2018 |  [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_stage.tar.gz) |[Modify the M3DRY Tutorial](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)|
|v5.5-ISAM CB6r5 M3Dry | Output (52 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_ISAM.md)|
|v5.5-DDM3D CB6r5 M3Dry | Output (16 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_DDM3D.md)|


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
