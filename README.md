CMAQ-PFAS (Built upon CMAQv5.3.2)
=================================

This version of the Community Multiscale Air Quality (CMAQ) model supports emission, transport, gas/particle partitioning, and deposition of **Per- and Polyfluoroalkyl Substances (PFASs)**. The model has been applied and evaluated for one case study in Eastern North Carolina (D'Ambro et al., "Characterizing the Air Emissions, Transport, and Deposition of Per- and Polyfluoroalkyl Substances from a Fluoropolymer Manufacturing Facility", ES&T, 2021, https://doi.org/10.1021/acs.est.0c06580).

PFAS is of growing concern to EPA program offices and regions due to the ubiquitous exposures across the population, and the growing evidence for adverse effects in humans.   The potential for PFAS air emissions to transport and deposit on soils and waters exists but has not been sufficiently studied.  EPA has determined that mitigating PFAS exposures is an Agency priority.  Improved understanding of atmospheric fate and transport of PFAS is needed to identify potential sources of PFAS exposures and appropriate mitigation options.  This product aims to address gaps in knowledge about atmospheric fate and transport of PFAS by characterizing PFAS air emissions from important large point sources (e.g., Chemours Processing Facility outside Fayetteville, NC) and the exploration of the effects of likely atmospheric oxidation pathways on the form and abundance of atmospherically relevant PFAS compounds. 

The Community Multiscale Air Quality (CMAQ) model was used to simulate the transport and chemistry of PFAS compounds, including GenX-relevant species (hexafluoropropylene oxide dimer acid (HFPO-DA), hexafluoropropylene oxide dimer acid fluoride (HFPO-DAF)), and 47 other poly- and per-fluorinated compounds. The Carbon Bond mechanism (CB6r3_ae7) was chosen as the base mechanism to which PFAS species were added. High-profile compounds PFOA and PFOS are not thought to be constituents of modern air emissions from Chemours Fayetteville-Works, but will be included in future versions of CMAQ-PFAS. 

Updates to CMAQ physical and chemical processes included the following:
* Added PFAS gas and aerosol species to the GC and AE namelists in the CB6r3_ae7 mechanism. Aerosol species were also added to the master table in AERO_DATA.F
* PFAS-specific Henry's Law Coefficients were added to hlconst.F. The update to hlconst.F is relevant for dry deposition to wet surfaces as well as uptake of PFAS vapors into hydrometeors. For carboxylic acids, pH-dependence was also treated via specification of acid disssociation constants. An alternative version of hlconst.F (hlconst_carbacid.F) replaces the Henry's Law Coefficients of all acyl fluorides with those of carboxylic acids. 
* The gas-particle partitioning of PFAS was treated in aero_subs.F. Equilibrium partitioning was assumed and the fraction of the total PFAS in the particle organic phase (controlled by saturation concentration, C*) and the particle aqueous phase (controlled by pH-dependent Henry's Law) was solved simultaneously. The C* of each PFAS was added to the table in SOA_DEFN.F.
* SpecDef files for CB6r3_ae7 were updated to calculate aggregate PFAS output variables.

The model predicted concentration and deposition fields for PFAS compounds given estimated chemical properties (e.g., volatility, solubility, etc.) and incident meteorology within approximately 150 km of the facility at 1 km resolution. The deposition fields were compared to measured concentrations of PFAS compounds in total rainwater plus dry deposition samples to determine the accuracy of the model's prediction and the importance of uncertainties in emission, chemistry, and chemical property assumptions. A sensitivity simulation tested the feasibility of fast hydrolysis of acid fluoride compounds to form carboxylic acids, with resulting larger water solubility and higher deposition rates.

CMAQ-PFAS was able to reproduce the precipitation loadings of GenX-relevant species near the manufacturing facility sampled by the North Carolina Department of Environment Quality within a factor 2 (average modeled and measured annual deposition were 48.6 and 97.5 ng m-2 day-1). Only 5% or less of the GenX species and 2.5% of total PFAS emissions from the facility are deposited within the first 150 km from the plant. The rest of the mass is predicted to be transported out of the domain. Air concentrations of total GenX and total PFAS around the facility can reach 24.6 ng m-3 and 3.4 µg m-3 but decrease to ~0.1 ng m-3 and ~10 ng m-3 at 35 km downwind, respectively. For the sensitivity simulation, with immediate conversion of acid fluorides to carboxylic acids via hydrolysis, CMAQ-PFAS overpredicted the deposition for GenX-relevant species by a factor of 2-5 across each of the 5 sites. This indicates that the true conversion rate of these compounds in the atmosphere could be significant although not immediate.


## CMAQ System Overview
Community Multiscale Air Quality Model US EPA CMAQ Website: (https://www.epa.gov/cmaq).

CMAQ is an active open-source development project of the U.S. EPA's Office of Research and Development that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center: (http://www.cmascenter.org).

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.


## CMAQ version 5.3.2 Overview:
CMAQv5.3.2 includes significant updates to the CMAQ Integrated Source Apportionment Method (ISAM) and multiple minor fixes to address issues identified in CMAQv5.3.1. The new CMAQ-ISAM version includes substantial updates to the gas-phase chemistry apportionment algorithms that improves both physical and numerical aspects of the method. Users of ISAM are strongly encouraged to update to CMAQv5.3.2.
* [CMAQv5.3.2 Release Notes](DOCS/Release_Notes/README.md)   
* [Frequently asked questions for upgrading to the latest CMAQ version](DOCS/Release_Notes/CMAQ_FAQ.md) **- Updated for v5.3.2 release.** 
* [CMAQv5.3.2 Known Issues](DOCS/Known_Issues/README.md) - Updated after the v5.3.2 release to alert users of issues that will be addressed with the next release. 

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

* [CMAQ User's Guide](DOCS/Users_Guide/README.md)   
* [Release Notes](DOCS/Release_Notes/README.md)   
* [Known Issues](DOCS/Known_Issues/README.md)   
* [Tutorials](DOCS/Users_Guide/Tutorials/README.md)   
* [Developers' Guide](DOCS/Developers_Guide/CMAQ_Dev_Guide.md)   
* [FAQ for upgrading to the latest CMAQ version](DOCS/Release_Notes/CMAQ_FAQ.md) 

## CMAQ Test Cases
Benchmark/tutorial data for each CMAQ release version are available from the CMAS Data Warehouse.  The input and output files are stored on Google Drive with metadata organized through Dataverse.  CMAQv5.3.2 comes with new input and output benchmark data for July 1-2, 2016 over the Southeast US (links provided below). The input datasets for these two days are identical to those released with v5.3.1 except for the addition of a grid mask file for the United States: [GRIDMASK_STATES_12SE1.nc](https://drive.google.com/file/d/16JJ4d6ChBJsvMc_ErqwDBrFfGh2MnVYR/view?usp=sharing). As a result, there is no need for users who have already downloaded the v5.3.1 Southeast benchmark input data to download the v5.3.2 files unless they need the grid mask file for running the new [ISAM test case](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_ISAM.md) or to test out regional emissions scaling with [DESID](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md). The Southeast benchmark output data for v5.3.2 is slightly different from what was released with v5.3 as described in the [CMAQv5.3.2 Rlease Notes FAQ](DOCS/Release_Notes/CMAQ_FAQ.md).

|**CMAQ Version**|**Data Type**|**Domain**|**Simulation Dates**|**Dataverse DOI**| 
|:----:|:----:|:--------------:|:----:|:--------:|
|v5.3|Input| CONUS | Jan 1 - Dec 31, 2016 | https://doi.org/10.15139/S3/MHNUNE |
|v5.3, v5.3.1, v5.3.2|Input| Southeast US| July 1 - 14, 2016| https://doi.org/10.15139/S3/IQVABD |
|v5.3, v5.3.1|Output| Southeast US| July 1 - 14, 2016|https://doi.org/10.15139/S3/PDE4SS |
|v5.3.2|Output| Southeast US| July 1 - 2, 2016|https://doi.org/10.15139/S3/PDE4SS |

The benchmark data is also available form the US EPA annoymous ftp server: ftp://newftp.epa.gov/exposure/CMAQ/V5_3_2/Benchmark/WRFv4.1.1-CMAQv5.3.2/

## Previous CMAQ Versions
The followoing release versions of CMAQ are currently available on GitHub.  DOI values from Zenodo can be used when referencing a specific version.
* [v5.3.1 (December 2019)](https://github.com/USEPA/CMAQ/tree/5.3.1) - [doi:10.5281/zenodo.3585898](https://doi.org/10.5281/zenodo.3585898) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Users_Guide/README.md) | [Known Issues](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Known_Issues/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/blob/5.3.1/DOCS/Users_Guide/Tutorials/README.md) 
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
