CMAQv6.0 
==========

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23267068.svg)](https://doi.org/10.5281/zenodo.23267068)

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq

CMAQ is an open-source development project of the U.S. EPA that consists of a suite of programs for conducting air quality model simulations. CMAQ combines emerging knowledge in atmospheric science and air quality modeling with advances in computational techniques in an open-source framework to deliver scientifically sound estimates of ozone, particulates and toxics in the air we breathe, as well as deposition of pollutants such as acids and nutrients to our land and water.

CMAQ user support is provided by the CMAS Center: http://www.cmascenter.org 

## CMAQ version 6.0 Overview:

The science updates and new features in this version (v6.0) are documented in the [CMAQv6.0 Release Notes](DOCS/Release_Notes/README.md) and summarized in the **[Release FAQ](DOCS/Release_FAQ/CMAQv6.0-FAQ.md)**.

## New features in CMAQ version 6.0 include:

* **Chemistry Updates:** 
  * Addition of particulate nitrate photolysis (pNO3; Sarwar et al. ([2024](https://doi.org/10.1016/j.scitotenv.2024.170406); [2025](https://doi.org/10.1016/j.scitotenv.2025.178968))) to both CB6R5 and CRACMM mechanisms. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Chemistry.md) for details.
  * Addition of heterogeneous sulfur chemistry ([Farrell et al. (2025)](https://doi.org/10.5194/acp-25-3287-2025)) to both CB6R5 and CRACMM mechanisms. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#heterogeneous-chemistry-of-sulfur-species) for details.
  * Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) version 3: new state-of-the-science chemical mechanisms. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md) for details. 
    * CRACMM3 adds chlorine chemistry, heterogeneous sulfur chemistry, and particle nitrate (pNO3) photolysis and improves conservation of carbon across reactions. In addition, it updates reactions for several systems including semivolatile organic compounds.
    * CRACMM3M includes all of CRACMM3 updates and offers additional detailed halogen chemistry to improve the representation of gas-phase and aerosol chemistry in marine environments.
    * CRACMM3HAPS includes all CRACMM3 updates and provides additional gas and particle Hazardous Air Pollutants 

* **Updates to Natural Emissions Estimates:**
  * Improvement of windblown dust emissions for NLCD40 land-use specification address high bias in dust estimates from earlier CMAQ versions. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Wind-Blown-Dust-Emissions.md#correction-for-nlcd40-land-use-mapping-in-windblown-dust-module) for details.
  * New satellite-based global vegetation dataset accounts for the effect of previously underestimated brown vegetation and further improves dust estimates for many regions in the US and Northern Hemisphere. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Wind-Blown-Dust-Emissions.md#brown-vegetation-added-to-windblown-dust-module) for details.
  * These improvements apply to both the U.S. and hemispheric scale simulations.
  * A new biogenic soil NO module is developed and released called Soil Emissions of Gases to the Atmosphere (SEGA) module. This module provides a simple, meteorological dependent method to estimate soil NO and HONO emissions for regional to global applications. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Soil-Emissions-of-Gases-to-the-Atmosphere-(SEGA).md) for details.

* **Improvements to source apportionment tools, CMAQ-ISAM and CMAQ-DDM3D:** 
  * Tagged source apportionment modeling via CMAQ-ISAM is now compatible with the most up-to-date chemistry CRACMM2, CRACMM3, CRACMM3M, and CRACMM3HAPS.
  * Sensitivity-based source apportionment via DDM3D is now more robust after instabilities from heterogeneous chemistry have been resolved. In addition, CMAQ-DDM3D now supports using the STAGE dry deposition module. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Instrumented-Models.md) for details.

* **New customization options and simplified user experience:**
  * The Explicit and Lumped air quality Model Output module (ELMO) version 2 offers:
    * expanded features for gas and deposition species. ELMOv1 focused on support for aerosol species.
    * full flexibility for defining aggregates (e.g., VOC, NOY, NOz, etc.) and assigning them to output files.
    * new chemical and meteorological diagnostic variables available for output. Tutorials are provided to support users in adding custom variables themselves.
    * automatic logging of the composition of aggregate output variables like PM2.5, fine-mode organic aerosol (PMF_OA), total Nitrogen deposition, etc.
    * new support for source apportionment tools, CMAQ-ISAM and CMAQ-DDM3D. For example, source-resolved PM2.5 and NOx may now be output directly.  Users no longer have to prescribe manually how to sum source-resolved species together.
    * See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md) for details.
  * Consolidated list of chemical mechanisms, highlighting the completion of CRACMM development milestones. See https://www.epa.gov/cmaq/cracmm for more details.
  * Two dry deposition modules, STAGE and M3DRY, are now both built in model executables and may be selected at run-time.

* **Updates to land-surface exchange and mixing modules:** 
  *	The resistance to dry deposition of volatile carbon-containing compounds has been increased consistent with their vapor-pressures. This increases VOC and CO concentrations across model applications. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange.md) for details.
  *	Minor updates and corrections to the Surface Tiled Aerosol and Gas Exchange deposition module. See [Release Note](DOCS/Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md) for details.
  * Boundary-layer mixing dynamics in stable conditions have been made consistent with upstream meteorological models.
    

 

## Getting the CMAQ Repository
This CMAQ Git archive is organized with each version stored as a branch on the main USEPA/CMAQ repository. The most recently released official version of the model will always be on the branch called 'main'. 
To clone code from the CMAQv6.0  version issue the following command from within a working directory on your server:

```
git clone https://github.com/USEPA/CMAQ.git CMAQ_REPO
```


## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ.
* **DOCS:** Release Notes, Release FAQ, Getting Started reference page, User's Guide, and short tutorials.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, meteorology, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **PYTOOLS:** Python pre- and postprocessing tools
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation)

## CMAQv6.0  Documentation
The User's Guide chapters, tutorials, and appendices have been updated for CMAQv6.0. Information on the updates in CMAQv6.0 is summarized in the **[Release FAQ](DOCS/Release_FAQ/CMAQv6.0-FAQ.md)**.


## CMAQ Test Case  
A full set of model-ready inputs for 2022 are provided for the 12US1 domain, including CRACMM emissions compatible with both the CRACMM2 and CRACMM3 chemical mechanisms. Input files can be used for running CMAQv5.5 (with CRACMM2) or CMAQv6.0  (with CRACMM2 or CRACMM3). 
* [CMAQ Data](DOCS/CMAQ_Data.md)


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

