### Updated mechanism CRACMM2
[Nash Skipper](mailto:skipper.nash@epa.gov) and [Havala Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.5   

<img src="https://github.com/user-attachments/assets/e0eeceb1-da06-4e94-a456-e83e50455b15" width="400">

**Description**: 
CRACMM2 includes several updates to CRACMM1. Many updates are intended to improve the representation of secondary formaldehyde (HCHO) in CRACMM. These include the incorporation of the AMORE v1.2 isoprene condensation into the primary CRACMM mechanism, updates to HCHO yields from monoterpenes, and the addition of styrene as a new explicit species. Some other opportunistic updates (mostly unrelated to formaldehyde) are changes to monoterpene nitrates that affect SOA formation and NOx recycling, the inclusion of emitted methane (ECH4), heterogeneous uptake of HO2 and NO3 radicals, and changes in how emissions of certain aromatic species are mapped to CRACMM species. CRACMM1 bug fixes noted below have also been incorporated into CRACMM2.  

**Significance and Impact**:  
Formaldehyde is a hazardous air pollutant (HAP) and is a major contributor of health risks from air toxics; however, it is biased low in CRACMM1 by about a factor of two. Formaldehyde performance should be improved to provide a more accurate estimate of risk from ambient exposure. Formaldehyde can also be sensed remotely from satellites and is often used (as a proxy for VOC abundance) along with satellite-based estimates of NO2 to diagnose ozone production regimes. Improvements to HCHO in CRACMM may allow for more meaningful comparisons between observed and modeled chemical regime. The updates in CRACMM2 also tend to increase ozone and decrease organic aerosol, particularly in the summer and particularly in the southeastern US.  

**References**:  
Skipper, T. N., D'Ambro, E. L., Wiser, F. C., McNeill, V. F., Schwantes, R. H., Henderson, B. H., Piletic, I. R., Baublitz, C. B., Bash, J. O., Whitehill, A. R., Valin, L. C., Mouat, A. P., Kaiser, J., Wolfe, G. M., St. Clair, J. M., Hanisco, T. F., Fried, A., Place, B. K., and Pye, H. O. T.: Role of chemical production and depositional losses on formaldehyde in the Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM), EGUsphere, 2024, 1-34, https://doi.org/10.5194/egusphere-2024-1680, 2024.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1095](https://github.com/USEPA/CMAQ/commit/5c9f5441b24efd94ad6be5ad939c5f6fc8af2980) | [PR#1095](https://github.com/USEPA/CMAQ_Dev/pull/1095)  |

### workaround for gcc incompatibility with CRACMM namelists
[Nash Skipper](mailto:skipper.nash@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Script update  
**Release Version/Date**:  CMAQv5.5  

**Description**:  Remove trailing comments in CRACMM species namelists when building CMAQ with gcc  

**Significance and Impact**: CRACMM uses trailing comments in the species namelists (e.g., `GC_cracmm2.nml`, `AE_cracmm2_nml`, and `NR_cracmm2.nml` files) for species metadata. If CMAQ is compiled with gcc, the model will crash at runtime because gcc does not allow trailing comments in namelist files. Previously if a user wanted to use CRACMM with gcc, they would have to either obtain versions of the namelist files without trailing comments from the CMAS Center or remove the trailing comments themselves. The removal of trailing comments in the namelist files has now been automated at build time if the `bldit_cctm.csh` script detects that a gcc compiler is used with a CRACMM mechansim.  

|Merge Commit | Internal record|
|:------:|:-------:|
| [Merge for PR#1154](https://github.com/USEPA/CMAQ/commit/c31983b72a3049d708138da3f57227875333eb39) | [PR#1154](https://github.com/USEPA/CMAQ_Dev/pull/1154) |  


### Bug fixes for CRACMM1 biogenic emissions using MEGAN
[Nash Skipper](mailto:skipper.nash@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**: 
The following fixes have also been implemented for CRACMM1 biogenic emissions using MEGAN:
* Fix emission mapping of CO when using MEGAN biogenic emissions. In CMAQv5.4 CO emissions were mapped to species SLOWROC for CRACMM1. Note: other CMAQ mechanisms were not affected by this CO mapping issue.
* Add mapping of semivolatile ROC species included in MEGAN biogenic emissions to CRACMM1 DESID file. In CMAQv5.4 emissions of these species would not be added because there was not an existing rule in the DESID file to map them to a model species.

**Significance and Impact**:  
These updates only impact applications that use the CRACMM1 or CRACMM1AMORE chemical mechanisms and the MEGAN biogenic emissions model. Applications using BEIS or using any other chemical mechanism are not impacted.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1095](https://github.com/USEPA/CMAQ/commit/5c9f5441b24efd94ad6be5ad939c5f6fc8af2980) | [PR#1095](https://github.com/USEPA/CMAQ_Dev/pull/1095)  | 

### Bug fixes for CRACMM1 IEPOX uptake rate
[Kathleen Fahey](mailto:fahey.kathleen@epa.gov) and [Nash Skipper](mailto:skipper.nash@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**: 
A typo in the rate of sulfate catalyzed IEPOX where the nucleophile was HSO4 has been corrected. The incorrect rate resulted in excessive uptake of IEPOX through this pathway.

**Significance and Impact**:  
These updates only impact applications that use the CRACMM1 or CRACMM1AMORE chemical mechanisms. The impact of this fix is to increase sulfate (as less sulfate is taken up as organosulfate) and to decrease organic aerosol.


**References**:  
Vannucci, P., K. Foley, B. Murphy, C. Hogrefe, R. Cohen and H. Pye: Temperature-dependent composition of summertime PM2.5 in observations and model predictions across the Eastern U.S., ACS Earth Space Chem. 2024, 8, 2, 381–392. [https://doi.org/10.1021/acsearthspacechem.3c00333](https://doi.org/10.1021/acsearthspacechem.3c00333)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#976](https://github.com/USEPA/CMAQ/commit/53d0884fc138ab2cb48cf733de961be448b4395d) | [PR#976](https://github.com/USEPA/CMAQ_Dev/pull/976)  |


### CRACMM DESID Input File Updates 
[Karl Seltzer](mailto:seltzer.karl@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Input File Update  
**Release Version/Date**: CMAQv5.5  
**Description**: Added ROC-ALK series of "Emission Surrogates" to ensure emission input files with and without the "ALK" identifier are processed/emitted.  
**Significance and Impact**: Impacts results (SOA and PM2.5 predictions) if emissions were prepared using ROC-ALK species names. This fix is needed to propagate ROC-ALK emissions to the proper model species. Users should check at least one processor log to ensure emissions were properly mapped. Depending on the emission preparation method, ROC-ALK may or may not be populated. Newer versions of emissions prepared with gspro files created by [S2S-Tool](https://github.com/USEPA/S2S-Tool) are more likely to use consistent names.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#970](https://github.com/USEPA/CMAQ/commit/17ce8e23b04ddec4fcf32dd20b55d5c8902d6c29) | [PR#970](https://github.com/USEPA/CMAQ_Dev/pull/970)  |

### The Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) Version 1.0
[Havala O. T. Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.4  
**Description**: The Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) builds on the history of the Regional Atmospheric Chemistry Mechanism, Version 2 (RACM2) and aims to couple gas- and particle-phase chemistry by treating the entire pool of atmospheric reactive organic carbon (ROC) relevant to present-day emissions. CRACMM species were developed to represent the total emissions of ROC, considering the OH reactivity, ability to form ozone and secondary organic aerosol (SOA), and other properties of individual emitted compounds. The chemistry of CRACMM, which includes autoxidation, multigenerational oxidation, and the treatment of semivolatile and intermediate volatility compounds, was built using a variety of sources including literature and other mechanisms (MCM, GECKO, and SAPRC18/mechgen). Compared to RACM2, the number of traditional volatile organic carbon species is reduced and the number of oxygenated and semivolatile to intermediate volatility precursors are increased in the mechanism. In addition, explicit hazardous air pollutants (toluene; 1,3-butadiene; and acrolein) are added to better characterize exposures relevant for human health. 

CRACMMv1 is available in two versions: base CRACMMv1 and CRACMMv1AMORE. The development of base CRACMMv1 is described by Pye et al. (2022) and the application of CRACMMv1 within CMAQ to the northeast U.S. in summer 2018 as well as comparison with other mechanisms is presented by Place et al. (in prep.). CRACMMv1AMORE replaces the base isoprene chemistry of CRACMMv1 (which was largely ported from RACM2) with a graph theory-based condensation of a detailed isoprene mechanism developed by Prof. Faye McNeill's team at Columbia University. The AMORE version is documented in work by Wiser et al. (2022).

One feature of CRACMM is the specification of representative structures for all species in the mechanism. Metadata, including a representative compound name, description of explicit vs lumped nature, a [SMILES string](https://en.wikipedia.org/wiki/Simplified_molecular-input_line-entry_system), and DTXSID identifier in the [EPA Chemicals Dashboard](https://comptox.epa.gov/dashboard/) (if available) are appended to the species namelists (GC, NR, and AE). This information is leveraged to determine conservation of mass across chemical reactions (see the CHEMMECH README in the UTIL directory), determination of species properties such as solubility, and to communicate how species are conceptualized. Representative compound information from the namelists are matched with species descriptions (a verbose string description in cracmm1_speciesdescription.csv) using python to provide markdown file descriptions of the mechanism species. See the CMAQ Users' Guide Chapter 6 for more information on CRACMM.

Supporting data for CRACMM, including information on how to map emissions to the mechanism, will be available in a [CRACMM github repository](https://github.com/USEPA/CRACMM). Information on getting started with CRACMM is available in [a tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_CRACMM.md).

**Significance and Impact**: CRACMM couples SOA formation with radical chemistry and updates the representation of a number of chemical systems. CRACMM is being released as a research mechanism so that it may undergo testing in various applications with the aim of making it the default chemistry option in the future. A fact sheet describing the CRACMM effort is available on [EPA's CMAQ website](https://www.epa.gov/cmaq/cmaq-fact-sheets).

**References**:  
1. Pye, H. O. T., Place, B. K., Murphy, B. N., Seltzer, K. M., D'Ambro, E. L., Allen, C., Piletic, I. R., Farrell, S., Schwantes, R. H., Coggon, M. M., Saunders, E., Xu, L., Sarwar, G., Hutzell, W. T., Foley, K. M., Pouliot, G., Bash, J., and Stockwell, W. R.: Linking gas, particulate, and toxic endpoints to air emissions in the Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) version 1.0, Atmos. Chem. Phys. Discuss. [preprint], https://doi.org/10.5194/acp-2022-695, in review, 2022.
2. Place, B. K., Hutzell, W. T., Appel, K. W., Farrell, S., Valin, L., Murphy, B. N., Seltzer, K. M., Sarwar, G., Allen, C., Piletic, I., D'Ambro, E., Saunders, E., Simon, H., Torres-Vasquez, A., Pleim, J., Schwantes, R., Coggon, M., Xu, L., Stockwell, W. R., and Pye, H. O. T.: Initial evaluation of the CRACMMv1.0 chemical mechanism: Surface ozone predictions across the Northeast US summer 2018 in CMAQ, in preparation for Atmospheric Chemistry and Physics.
3. Wiser, F., Place, B., Sen, S., Pye, H. O. T., Yang, B., Westervelt, D. M., Henze, D. K., Fiore, A. M., and McNeill, V. F.: AMORE-Isoprene v1.0: A new reduced mechanism for gas-phase isoprene oxidation, Geosci. Model Dev. Discuss. [preprint], https://doi.org/10.5194/gmd-2022-240, in review, 2022.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#908](https://github.com/USEPA/CMAQ/commit/7ea4c901f754376ccdb1ad8b0b82c4a5efd3a6ba) | [PR#908](https://github.com/USEPA/CMAQ_Dev/pull/908)  |

