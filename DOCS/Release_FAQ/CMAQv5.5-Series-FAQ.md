# Frequently Asked Questions for Upgrading to CMAQ Version 5.5

## Table of Contents:
* [Do I need to update from v5.4 to v5.5?](#why_update_v54_v55)
* [What do I need to do to update from v5.4 to v5.5?](#update_v54_v55)
  * [What differences should I expect in the required model input files?](#diff_v54_v55_input_files)
  * [What differences should I expect in my model output files?](#diff_v54_v55_ouput_files)
* [What differences should I expect in my model results with v5.5 compared to v5.4?](#diff_v54_v55_model_results)
* [Are there new benchmark data and documentation updates?](#data_and_docs)
* [Community contributions](#community_contributions)
* [How to cite CMAQ](#how_to_cite)
* [Additional FAQ](#additional_faq)
* [Technical support for CMAQ](#tech_support)

<a id=why_update_v54_v55></a>
## Do I need to update from v5.4 to v5.5?
CMAQv5.5 includes many scientific enhancements and new features that will benefit certain applications. See the v5.5 Release Notes for a description of each change.   

#### Instrumented Models
* Several updates were incorporated in CMAQ-ISAM: (1) includes the capability to quantify source contributions to total secondary organic aerosol (SOA) and individual species; (2) accounts for the loss of aerosol mass from gravitational settling was incorporated, so that the tagged and bulk concentrations matched more precisely; (3) incorporates a tag to attribute O3 to be of “stratospheric origin” based on the PV-scaling methodology; (4) addressed instabilities associated with mass distribution between tags immediately after cloud chemistry. See: [CMAQ-ISAM Release Notes](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-ISAM.md) for more information. 

* In CMAQ-DDM: (1) enabled estimation of sensitivity of O3 to PV-O3 specification; (2) corrected calculation of second-order sensitivity in chemistry routines; (3) corrected error that caused incorrect dry deposition sensitivity in output. See  [CMAQ-DDM Release Notes](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-DDM3D.md#cmaq-hddm-3d-second-order-sensitivity-fix) for more information. 

#### On-line coupling of CMAQ with meteorological models   
* CMAQv5.5 is the first public release that allows coupling of CMAQ with the Model for Prediction Across Scales (MPAS). Please see the [MPAS-CMAQ release note](../Release_Notes/CMAQ-Release-Notes:-MPAS-CMAQ-Coupled-Model.md) to learn more.

* A bug was identified within the CMAQ to WRF coupling routine in the CMAQv5.4 series; layer heights were incorrectly being assigned to wind speed in the plume rise calculations leading to erroneous allocation of point source emissions. Users of WRF-CMAQ are strongly encouraged to update to CMAQv5.5. See the [WRF-CMAQ Release Notes](../Release_Notes/CMAQ-Release-Notes:-WRF-CMAQ-Coupled-Model.md#new-wrf-cmaq-model-using-wrfv44-and-cmaqv54) for more information.


#### Chemistry
* In the Community Regional Atmospheric Chemistry Multiphase Mechanism version 1 (CRACMM1), the rate of sulfate catalyzed IEPOX was updated. The impact is to increase sulfate (as less sulfate is taken up as organosulfate) and to decrease organic aerosol. If you are interested in learning more, please see the [CRACMM1 release note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#bug-fixes-for-cracmm1-iepox-uptake-rate).

* CMAQv5.5 introduces CRACMM version 2. CRACMM2 includes several updates to CRACMM1. Many updates are intended to improve the representation of secondary formaldehyde (HCHO) in CRACMM. These include the incorporation of the AMORE v1.2 isoprene condensation into the primary CRACMM mechanism, updates to HCHO yields from monoterpenes, and the addition of styrene as a new explicit species.  Additional updates include changes to monoterpene nitrates that affect SOA formation and NOx recycling, the inclusion of emitted methane (ECH4), heterogeneous uptake of HO2 and NO3 radicals, and changes in how emissions of certain aromatic species are mapped to CRACMM species. If you are interested in learning more, please see the [CRACMM2 release note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updated-mechanism-cracmm2).

* The photolysis module was updated to correct an error in calculation of cloud optical depth for grid cells with sub-grid/convective clouds. The net impact is to increase the attenuation of photolysis frequencies under cloudy conditions. Please see the [photolysis release note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Photolysis.md) to learn more. 

#### Dry Deposition/Air Surface Exchange
* In STAGE, updates were made to land use mappings to an internal land use category via CMAQ_Control_STAGE.nml that is user customizable.  Updates were also made to rectify an underflow encountered in the STAGE Emerson in debug mode. See the [STAGE release notes](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md) to learn more. 

* In M3DRY, (1) changes to Ammonia (NH3) deposition in M3DRY for model configurations without NH3 BIDI, (2) changes to NH3 dry deposition flux and NH3 emissions flux calculations in M3DRY, upward and downwards fluxes changed, but net flux remains the same. See the [M3DRY release note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-M3DRY.md#revised-dry-dep-flux-for-nh3) to learn more. 

#### Stratosphere-Troposphere Exchange
* Removed dependency on sigma-P coordinate system in PV-O3 scaling. The update impacted O3 distributions between 300-100hPa and generally improved model estimates relative to observations at these levels. See the [Stratospheric-Troposphere exchange release note](../Release_Notes/CMAQ-Release-Notes:-Stratospheric‐Tropospheric-Exchange-(STE))for more information. 

#### Emissions
* Updates to the in-line version of MEGAN BDSNP, which corrects a bug in the pulsing subroutine of BDSNP (small impact on estimated soil NO). See the [MEGAN-BDSNP](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Model-of-Emissions-of-Gases-and-Aerosols-from-Nature-(MEGAN)-Biogenic-Emissions.md#bdsnp-dry-hours-calculation.md) to learn more. 

#### Process Analysis & Sulfur Tracking Model (STM) 
* Fixes to Sulfur Tracking Model (STM) correct errors in attribution of sulfate from gas phase chemistry, which inadvertently got introduced in the previous model version. See [CMAQ-STM Release Notes](../Release_Notes/CMAQ-Release-Notes:-Process-Analysis-&-Sulfur-Tracking-Model-(STM).md#fix-to-option-for-sulfur-tracking-model) for more information. 

#### Structural Improvements
* Updated model code and run-scripts so that PV-O3 scaling can be invoked now as a runtime option. See the [Stratospheric-Troposphere exchange release note](../Release_Notes/CMAQ-Release-Notes:-Stratospheric‐Tropospheric-Exchange-(STE)) for more information. 

#### Diagnostic Options
* Updates to ELMO to fix erroneous output for various PM aggregates (PMF_OC, PMF_NCOM, TNO3, etc.). See the [Diagnostic Options](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md) release notes to learn more. 

#### Pre-processors and Utilities
* MCIP: Updates were implemented to the definition of grid origin for fine-scale lambert conformal grids (i.e. < 4km resolution). See the [MCIP release note](../Release_Notes/CMAQ-Release-Notes:-Preprocessors.md#mcip) for additional details. 

#### Post-processors
* Corrected deposition SpecDef for NOy species. See [release note](../Release_Notes/CMAQ-Release-Notes:-Postprocessors.md#corrected-deposition-species-definition-specdef_dep-files-for-missing-nitrogen-species) for more information.  
* Improved checks on formulas used by Combine. See [combine release note](../Release_Notes/CMAQ-Release-Notes:-Postprocessors.md#improve-checks-on-formulas-used-by-combine) for more details.  
* Increase character limit for station ID in sitecmp_dailyO3 to accommodate emerging networks. See [Site Compare Daily O3 release note](../Release_Notes/CMAQ-Release-Notes:-Postprocessors.md#sitecmp_dailyo3) for more details.  
* Updated time zone csv file used to time shift CMAQ outputs in HR2DAY program to version 4.1.0 released by Natural Earth. Minor impacts on h2day calculations due to time zone updates. See [HR2DAY release note](../Release_Notes/CMAQ-Release-Notes:-Postprocessors.md#updating-tzcsv-to-natural-earth) for more details.

#### Python Tools
* New Python tool to create CMAQ-ready grid masks from a shape file for defining regions and region families with DESID and using geographic source regions when running CMAQ-ISAM. See [CMAQ-Python tools release note](../Release_Notes/CMAQ-Release-Notes:-PYTOOLS.md#shp2cmaq-create-cmaq-ready-file-from-shapefile). for more details. 

<a id=update_v54_v55></a>
## What do I need to do to update from v5.4 to v5.5?

<a id=diff_v54_v55_input_files></a>
### What differences should I expect in the required model input files?
* If you have already successfully migrated to v5.4, you will not need to any additional input to run with the analogous options in v5.5. However, if you trying to run with the newest released version of CRACMM in v5.5, users will have to generate or map existing emissions to CRACMMv2.0. For additional information on emissions for CRACMMv2.0 please see the [CRACMM GitHub Page](https://github.com/USEPA/CRACMM/tree/main/emissions).  

<a id=diff_v54_v55_ouput_files></a>
### What differences should I expect in my model output files?
* The photolysis diagnostic files were updated to include additional variables (e.g., absorption AOD) that may help in evaluation of optical properties against measurements.

<a id=diff_v54_v55_model_results></a>
## What differences should I expect in my model results with v5.5 compared to v5.4?

The following summary is based on our testing of CMAQv5.4 and CMAQv5.5 using annual 2018 simulations performed over both the northern hemisphere (108 km horizontal grid spacing, 44 vertical layers) and the CONUS (12 km horizontal grid resolution, 35 vertical layers). The 108 km simulations were used to generate lateral boundary conditions for the 12 km simulations, with science options (except chemical mechanisms & bidi-directional ammonia exchange) matching across both domains. 

All CMAQv5.5 108 km simulations used the cb6r5m_ae7_aq chemical mechanism that includes a detailed representation of halogen chemistry while the 12 km simulations used either the cb6r5_ae7_aq (simplified halogen chemistry) or cracmm2 chemical mechanisms. The CMAQv5.4 108 km simulations used the cb6r5m_ae7_aq chemical mechanism while the 12 km simulations used either the cb6r5_ae7_aq (simplified halogen chemistry), or cracmm1 chemical mechanisms.

The updates made to the representation of aerosol dry deposition in CMAQv5.4 differ markedly between the M3Dry and STAGE dry deposition schemes. CMAQv5.4 M3Dry updates are based on on [Pleim et al., 2022](https://doi.org/10.1029/2022MS003050) ([see M3DRY release notes](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-M3DRY.md#new-aerosol-deposition-model-aero_depv)) and STAGE updates in its default configuration are based on [Emerson et al., 2020](https://doi.org/10.1073/pnas.2014761117) ([see STAGE release notes](./CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md)). To investigate whether the impacts of switching from CMAQv5.4 to CMAQv5.5 differ depending on whether users select the M3Dry or STAGE dry deposition scheme, all simulations comparing v5.4 and v5.5 were performed for both M3Dry and STAGE and results for both schemes are summarized below

#### Natural Emissions
* Emissions from natural sources (e.g., lightning, wind-blown dust, biogenic emissions) remained the same in v5.5 when compared to v5.4.

### Ozone
* v5.5 CB6 - v5.4 CB6: ~0.2 ppbV increase in seasonal domain mean ozone mixing ratio from CMAQv5.5 to CMAQv5.4 with larger isolated differences in individual grid cells depending on sub-grid convective cloud activity. This difference is attributed to the updates in the photolysis module to properly capture impacts of sub-grid clouds. 

* v5.5 CRACMM2 - v5.4 CRACMM1: Increase of 1-2 ppbV seasonal domain mean surface ozone mixing ratio in warmer months, especially in eastern US. Decrease of 1 ppbV season domain mean surface ozone mixing ratio in the winter, particularly in the eastern US. These changes are mostly due to updates to Isoprene chemistry [Skipper et al., 2024, preprint](https://doi.org/10.5194/egusphere-2024-554).

* All of the ozone comparisons discussed above (CMAQv5.5 CB6 vs. CMAQv5.4 CB6 and CMAQv5.5 CRACMM2 vs. CMAQv5.4 CRACMM1) generally hold true for both the M3Dry and STAGE dry deposition schemes.

* **Summary**: Minor increase in seasonal mean ozone mixing ratio due to corrections in photolysis module for the CB6 system, with larger more transient impacts due to sub-grid convective activity. Updates to the CRACMM system in CMAQv5.5 increase ozone mixing ratio during warmer months, while decreasing ozone in the winter particularly in the eastern US. 


### PM2.5
* v5.5 CB6 - v5.4 CB6: In simulations without ammonia bi-directional exchange, there is less than 0.1 of a µg/m3 impact on seasonal surface mean PM fine concentrations, except for regions of extreme aerosol loading which has an increase in winter and slight decrease in summer. These changes are mostly due to updates to the uni-directional ammonia (NH3) deposition, where ammonia deposition is now scaled by leaf area index. For simulations with ammonia bi-directional exchange, there is little to no impact from incrementing from v5.4 to v5.5. 

* v5.5 CRACMM2 - v5.4 CRACMM1: Widespread 0.5 - 1 ug/m3 decrease in seasonal mean PM fine concentrations over most of the modeling domain. Reductions driven by treatment of monoterpene nitrates and highly oxygenated organic molecules (HOM) formation ([Skipper et al., 2024, preprint](https://doi.org/10.5194/egusphere-2024-554). Summer mean sulfate concentrations increased by 0.1 - 0.5 ug/m3 over the Southeastern U.S. due to the updates to the rate of sulfate catalyzed IEPOX which also contributed to the decrease in organic aerosols ([Vannucci et al, 2024](https://doi.org/10.1021/acsearthspacechem.3c00333)).

* All of the PM Fine comparisons discussed above (CMAQv5.5 CB6 vs. CMAQv5.4 CB6 and CMAQv5.5 CRACMM2 vs. CMAQv5.4 CRACMM1) generally hold true for both the M3Dry and STAGE dry deposition schemes.

* **Summary**: Very minor change in seasonal mean PM fine concentrations for simulations without bidi directional ammonia exchange. Updates to the CRACMM system caused a decrease in seasonal mean PM fine concentrations over most of the modeling domain. 

### Deposition
* v5.5 CRACMM2 - v5.4 CRACMM1: Changes in seasonal total sulfur (S) and nitrogen (N) Deposition for both M3DRY and STAGE were minor. The most substantial change was a 5-25% increase in S wet deposition in the summer over the Southeastern U.S., in agreement with the increase in mean sulfate concentrations in the same region. CRACMM2 updates led to substantial decreases (>50%) in organic nitrate (NTR) deposition in the Eastern U.S., mostly in regions with high biogenic emissions. This had little impact on Total N deposition since NTR makes up a small fraction of wet and dry N deposition.  
  
<a id=data_and_docs></a>
## Are there new benchmark data and documentation updates?
Yes! We have made updates throughout our [User's Guide](../Users_Guide/README.md), including Appendices and Tutorials, to improve clarity and describe new options and features. We recommend revisiting sections that relate to your work to see the latest guidance. 

We have also expanded our benchmark datasets and tutorials to include MPAS-CMAQ and CRACMM2.  The Table below summarizes the benchmark data available with v5.5 and the associated Tutorials for running a test case simulation for the specific model configuration.

|**CMAQ Version**|**Data Type (Size)**|**Domain**|**Simulation Dates**|**Data Access**|**Tutorial**| 
|:----:|:----:|:--------------:|:----:|:--------:|:----:|
|MPAS-CMAQ| Input (215 GB) | Global (uniform 120) | Jan 1, 2017|[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/PAHQFO)  <br /> [AWS Link](https://mpas-cmaq.s3.amazonaws.com/index.html) |[Tutorial](https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/PDF/CMAQ_UG_09_2019.pdf)|
|v5.4 CB6r5 | Input (6.1 TB) | 12US1 | Jan 1 - Dec 31, 2018 | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/LDTWKH)  <br /> [AWS Link](https://cmas-cmaq-modeling-platform-2018.s3.amazonaws.com/index.html) ||
|v5.4 CB6r5 | Input (10.3 GB)| Northeast US| July 1 - 2, 2018| [Metadata, DOI, and download instructions ](https://doi.org/10.15139/S3/BWMI8X) <br /> [Google Drive Link](https://drive.google.com/drive/folders/1AFUB-4kzIXXoZr4hOHNBqRvy9JQ9_MDp)  <br /> [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_4/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz)||
|v5.5 CRACMM2| Input (6 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS]( https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/CMAQv5.5_2018_12NE3_Benchmark_cracmm2_stage_2Day_Input.tar.gz) ||
|v5.5 CRACMM2| Output (19 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS]( https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cracmm2_stage.tar.gz)|[Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark_cracmm2_stage.md)|
|v5.5 CB6r5 M3Dry | Output (15 GB) | 12NE3 | July 1 - 2, 2018 |  [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)|
|v5.5 CB6r5 STAGE | Output (16 GB) | 12NE3 | July 1 - 2, 2018 |  [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_stage.tar.gz) |[Modify the M3DRY Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)|
|v5.5-ISAM CB6r5 M3Dry | Output (52 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_ISAM.md)|
|v5.5-DDM3D CB6r5 M3Dry | Output (16 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br>  [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_DDM3D.md)|


<a id=community_contributions></a>
## Community Contributions
The CMAQ team would like to thank our user community for contributing to model updates in CMAQv5.5 by identifying issues, performing tests, and/or proposing code changes. The following v5.5 updates include a community contribution*.

* Updates to unidirectional fluxes in M3DRY came out of fruitful conversations with researchers Brett Schichtel and Gustavo Cuchiara Copstein from Colorado State's Cooperative Institute for Research in the Atmosphere [(Release Note)](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-M3DRY.md#revised-dry-dep-flux-for-nh3).
* An issue identified by shatani on the [CMAS User Forum](https://forum.cmascenter.org/t/aerosol-bulk-name-as-emission-species-name-in-the-desid-chemical-mapping-control/3692/1) led to an updated to the DESID module [(Release Note)](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Detailed-Emissions-Scaling-Isolation-and-Diagnostics-Module-(DESID).md#chemical-family-support).  
* Bonyoung Koo identified an issue on the [CMAS User Forum ](https://forum.cmascenter.org/t/incorrect-pm25-na-calculation-in-the-elmo-module/4615) that led to an update to the ELMO module [(Release Note)](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#correct-calculation-of-pm1-pm25-and-pm25to10-as-well-as-speciated-na-k-ca-and-mg-in-elmo).  
* An issue identified by Abi on the [CMAS User Forum](https://forum.cmascenter.org/t/cmaqv5-2-ddm-not-able-to-output-more-than-one-sens/3669/1) led to an update to CMAQ-HDDM to allow for correct sub-setting of variables to the ASENS output file.   
* Sheng-Po Chen identified an error in the molecular weight of HGIIGAS in several species tables on the [CMAS User Forum](https://forum.cmascenter.org/t/error-of-the-molecular-weight-for-hgiigas/4673) which has now been fixed ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Carbon-Bond-6-Mechanism-(CB6).md#correction-to-molecular-weight-of-hgiigas-in-species-tables)).
* Will Hatheway caught a typo in the config_cmaq.csh file and proposed an update on the [CMAQ GitHub repository](https://github.com/USEPA/CMAQ/issues/199).
* Simon Rosanka, from the University of California, Irvine, proposed a bugfix to CCTM/src/gas/ros3/rbdriver.F on the [CMAQ GitHug repo](https://github.com/USEPA/CMAQ/pull/198) to avoid a model crash when using the conditional rbstats option.  
* Feng Liu identified an issue in CMAQ-HDDM on the [CMAS User Forum](https://forum.cmascenter.org/t/cmaqv5-4-hddm-gives-too-high-second-order-sensitivity/3722/1) and provided testing and calculations that led to an update to address unreasonably high 2nd order sensitivity coefficients.

\*We attempted to be comprehensive in this list but if we have missed a contribution from you or a colleague, please forgive our oversight and contact us at [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov) so that we can correct our mistake. Thank you for helping us improve the CMAQ modeling system!   

<a id=how_to_cite></a>
## How to Cite CMAQ
Please see our 'How to Cite CMAQ' page if you are interested in referencing one of our released model versions, scientific algorithms, or model output in your own publication: https://www.epa.gov/cmaq/how-cite-cmaq

<a id=additional_faq></a>
## Additional FAQ
A more general list of Frequent CMAQ Questions can be found on our website: https://www.epa.gov/cmaq/frequent-cmaq-questions

<a id=tech_support></a>
## Technical support for CMAQ
Technical support for CMAQ, including questions about model inputs, downloading, compiling, and running the model, 
and pre- and post-processing utilities, should be directed to the [CMAS Center User Forum](https://forum.cmascenter.org/). 
 [**Please read and follow these steps**](https://forum.cmascenter.org/t/please-read-before-posting/1321) prior to submitting new questions to the User Forum.
