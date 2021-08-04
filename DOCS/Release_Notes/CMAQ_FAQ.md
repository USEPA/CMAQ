# Frequently Asked Questions for Upgrading to the Latest CMAQ Version

## Table of Contents:
* [Why are there more frequent CMAQ releases?](#why_more_releases)
* [Do I need to update from v5.2.1 to v5.3?](#why_update_v521_v53)
* [What do I need to do to update from v5.2.1 to v5.3?](#update_v521_v53)
* [What differences should I expect in my model results with v5.3 compared to v5.2.1?](#diff_v521_v53)
* [Do I need to update from v5.3 to v5.3.1?](#why_update_v53_v531)
* [What do I need to do to update from v5.3 to v5.3.1?](#update_v53_v531)
* [What differences should I expect in my model results with v5.3.1 compared to v5.3?](#diff_v53_v531)
* [Do I need to update from v5.3.1 to v5.3.2?](#why_update_v531_v532)
* [What do I need to do to update from v5.3.1 to v5.3.2?](#update_v531_v532)
* [What differences should I expect in my model results with v5.3.2 compared to v5.3.1?](#diff_v531_v532)
* [Do I need to update from v5.3.2 to v5.3.3?](#why_update_v532_v533)
* [What do I need to do to update from v5.3.2 to v5.3.3?](#update_v532_v533)
* [What differences should I expect in my model results with v5.3.3 compared to v5.3.2?](#diff_v532_v533)
* [Additional FAQ](#additional_faq)
* [Technical support for CMAQ](#tech_support)

<a id=why_more_releases></a>
## Why are there more CMAQ releases?
CMAQ is being updated more frequently to provide bug fixes to the community in a more timely fashion. For these minor releases, users should carefully read the release notes associated with the new version and evaluate the impact of the updates for their application to inform their decision on whether the newer version is beneficial. 

<a id=why_update_v521_v53></a>
## Do I need to update from v5.2.1 to v5.3?
CMAQv5.3 is a major update from version 5.2.1 including many scientific enhancements and new features.
#### CMAQv5.3 System Updates
* Updated instrumented models: Previously, the Sulfur Tracking Model (STM) and the Integrated Source Apportionment Method (ISAM) were at least one version behind the base model. As of CMAQv5.3, these instrumented models are now compile-time options available with the current base version of the model. Additionally, improvements in computational efficiency of these instrumented techniques have led to substantially faster run times to support their practical applications.
* The new DESID emissions interface allows for substantial flexibility in the way emissions are mapped, scaled, and checked for quality and can greatly simplify the task of assessing air quality improvements resulting from emission changes.
* Incorporated updates (new data sources, updated vertical coordinate system) to CMAQ and the Meteorology-Chemistry Interface Processor (MCIPv5.0) to increase scientific consistency between the atmospheric dynamics and chemistry calculations.
#### New Features and Processes in v5.3
* Updated marine chemistry to represent impacts of (1) halogen chemistry on ozone depletion and sulfate formation and (2) dimethyl sulfide on aerosol sulfate.
* Expanded the representation of secondary pollutant formation in clouds (AQCHEM-KMT2).
* Updated aerosol module (AERO7) that explicitly tracks 84 particulate species.
* Updated pathways for secondary organic aerosol formation from biogenic VOCs.
* Harmonized treatment of water uptake to aerosol organic phase to improve representation of aerosol chemistry, mixing state, and optical properties.
* Improved the representation of bi-directional exchange of ammonia at the surface 
* Improved representation of O<sub>3</sub> dry deposition to snow.
* Incorporated a new deposition module – the Surface Tiled Aerosol and Gaseous Exchange (STAGE) model – to estimate land-use specific deposition.
#### Further information on v5.3
* [CMAQv5.3 Fact Sheet](https://www.epa.gov/sites/production/files/2018-10/documents/cmaq_factsheet_.pdf)
* February 27, 2019 webinar: Recent Enhancements to the CMAQ Modeling System - [Download Slides](https://www.epa.gov/research-states/recent-enhancements-cmaq-modeling-system-webinar-archive)
* October 8, 2019 webinar hosted by CMAS: Enhanced Capabilities to Analyze Emissions Scenarios in CMAQv5.3 - [Download slides](https://www.cmascenter.org/resources/pres_files/CMAQv5.3_CMAS_Webinar_Analyzing_Emissions_Scenarios_20191008.pptx) | [Download/Watch recording](https://www.cmascenter.org/resources/pres_files/GMT20191008-175951_CMAQv5-3--_1600x900.mp4)

<a id=update_v521_v53></a>
## What do I need to do to update from v5.2.1 to v5.3?
* A major change to CMAQ in version 5.3 is the incorporation of the new emissions module, DESID, and 
its control via a number of RunScript variables and the Emission Control File, which is explained in 
the [CMAQ User’s Guide section 6.9.3](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#6.9.3_Emission_Compatability). **Due to this change and other changes in environment variables, it is strongly recommended that you start with one of the run scripts released with the latest CMAQ version and adapt it for your application rather than working off of run scripts you may have developed for your application using earlier versions of CMAQ.**
* If you are upgrading to CMAQv5.3 but you are not changing mechanisms (i.e. you have been using cb6r3_ae6 and you 
will continue to do so), it is hoped that you will not need to do anything special to maintain default behavior. 
In other words, the default emission control files will handle simple assignment of CMAQ species to emissions 
surrogates (e.g. NO --> NO; ASO4 --> PSO4, etc.).  
* If you were using a chemical mechanism with nonvolatile POA (e.g. cb6r3_ae6nvpoa_aq; APOC and APNCOM were in the list of transported species), then you will need to make a modification to the emission control file to continue using this assumption. Beginning in CMAQv5.3, nonvolatile POA and semivolatile POA are available in all mechanisms. It is now up to the user to direct the primary organic emissions into either nonvolatile species or semivolatile species. Every chemical mechanism in CMAQv5.3 and beyond assumes that generic POA emissions will be treated as semivolatile. If you wish to treat all POA emissions as nonvolatile or only the POA emissions from certain streams (e.g. fugitive dust) as nonvolatile, then you can follow the instructions in the emission control file to implement this assumption. All emissions from POC and PNCOM that a user wants to treat as nonvolatile should be directed to APOC and APNCOM, respectively.
* It is good practice to review the new CMAQ logfiles labeled "CTM_LOG_[XXX]..." to confirm that your 
emissions are being read as you expect. In particular pay attention to the sections beginning:
  * "SCALING EMISSIONS CONSISTENT WITH EMISSIONS CONTROL FILE SUPPLIED BY USER"
  * "Checking for unused Emissions Surrogates"
  * "EMISSIONS SCALING DIAGNOSTIC"
  
	Note that the clarity of warnings and notices for potential emissions issues were improved in v5.3.1. 
* If you are upgrading to a new mechanism (i.e. cb6r3_ae6 --> cb6r3_ae7) then it is even more imperative 
that you review the log file, tutorials for how to control the emissions, and the Emissions Control File 
itself to make sure the correct assignments are being made. Alpha-pinene is of particular concern and you 
may read more about that in the [CMAQ User’s Guide section 6.9.3](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#6.9.3_Emission_Compatability).
* If you have custom-mapped any CMAQ species to an emission surrogate species, then DESID will help you 
perform this action in a more transparent way. Note that the previous method of mapping emissions (in CMAQv5.2 and before) 
via the chemical species namelists has been removed. Please see the User's Guide documentation (User’s Guide section 6.9.3) and 
the [DESID tutorial for the Emission Control File](../Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md) to learn how to implement your changes from now on. 
* If you are running CMAQ with the M3DRY option for dry deposition and ammonia bidirectional surface flux (bidi) you will need 
to prepare EPIC soil properties (L2C_SOIL) and EPIC crop types and fertilizer application 
(E2C_CHEM) input files using the latest version of FEST-C, version 1.4. Please see the User's Guide [Chapter 6](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#68-dry-depositionair-surface-exchange) for further information on M3DRY and [Chapter 4](CMAQ_UG_ch04_model_inputs.md#e2c_lu) to read more about the land surface input files required for running with ammonia bidi. 
* If you are running CMAQ with the STAGE option for dry deposition and ammonia bidirectional surface flux (bidi), you will need to use EPIC soil properties (L2C_SOIL) and EPIC crop types and fertilizer applications (E2C_CHEM) files from FEST-C. The STAGE option and ammonia bidirectional surface flux is compatible with the latest version and backward compatible with earlier versions of FEST-C.
Please see the User's Guide [Chapter 6](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#68-dry-depositionair-surface-exchange) for further information on STAGE and [Chapter 4](CMAQ_UG_ch04_model_inputs.md#e2c_lu) to read more about the land surface input files required for running with ammonia bidi. 

<a id=diff_v521_v53></a>
## What differences should I expect in my model results with v5.3 compared to v5.2.1?
The following summary is based on our incremental testing of the science updates in CMAQv5.3 using v5.3 and v5.2.1 annual 2016 simulations over the CONUS (12km horizontal grid resolution, 35 vertical layers, cb6r3_ae7_aq chemical mechanism).
### Ozone
* Large increase in wintertime O<sub>3</sub> due to reduced deposition to snow in M3Dry. As expected, primarily affects northern latitudes with snow cover. Increases in O<sub>3</sub> in winter in 2016 over the CONUS range from 1-2 ppbV as far south as VA, TN, MO, KS, and CO to 10+ ppbV over ND, SD, WI, MN and central Canada.
* Outside of winter, O<sub>3</sub> generally decreases across the CONUS primarily due to updates to the PX-LSM vegetation fraction and LAI, with significantly reduced vegetation fraction which in turn reduces O<sub>3</sub> dry deposition and increases O<sub>3</sub> ambient mixing ratios. Additional chemistry updates in the model had a small impact on O<sub>3</sub>. Decreases generally range between 1-6 ppbV, with the largest decrease in the western US, and similar magnitudes across spring, summer and fall.
* Science updates affecting O<sub>3</sub> had the overall effect of dramatically improving O<sub>3</sub> underestimation in the winter (particularly the northern latitudes), slightly increasing O<sub>3</sub> overestimation in the spring, reducing O<sub>3</sub> bias in the summer and fall in the eastern US, and increasing O<sub>3</sub> bias in the summer and fall in the western US, particularly California (except for along the coast where O<sub>3</sub> bias decreases).
* **Summary**: O<sub>3</sub> is higher (~2 ppbV on average across all sites) in the winter (December/January); and lower (~2 ppbV on average across all sites) in the spring, summer and fall.
### PM<sub>2.5</sub>
* Large increase in wintertime PM<sub>2.5</sub> in the eastern US due to increased oxidants in v5.3. This increase generally ranges between 1-4 µg m<sup>-3</sup>, with the largest increase in the upper Midwest. However, this large increase in wintertime PM<sub>2.5</sub> due to science updates in v5.3 is essentially offset here by a similar large decrease in PM<sub>2.5</sub> by removing the application of PCSOA to residential wood combustion (RWC). PCSOA was never intended to be applied to RWC, but prior to version 5.3 CMAQ was able to read only a single gridded emissions input file, so there was no way to enable PCSOA without also applying it to RWC. The new ability in CMAQv5.3 to read multiple gridded emission input files allows for separation of RWC, so that PCSOA can be implemented without being applied to RWC. This results in an overall decrease in total PM<sub>2.5</sub>, as PCSOA is intended to account for missing sources of aerosol. This decrease generally ranges between 1-4 µg m<sup>-3</sup>, with isolated areas with larger decreases.
* In addition to these changes in winter, there is a small increase in springtime, and a relatively large increase in summer and fall PM<sub>2.5</sub> in the Southeast US due to increased SOA from monoterpene oxidation. These increases are on the order of 1-2 µg m<sup>-3</sup> in spring, and generally 2-4 µg m<sup>-3</sup> in summer and fall.
* As the overall change in PM<sub>2.5</sub> in the winter was close to zero, overall model performance did not change much, with changes in mean bias between -2 to 2 µg m<sup>-3</sup>. Change in performance in the spring was also relatively small, although there was a slight overall increase in bias in the Southeast US, generally on the order of 2 µg m<sup>-3</sup> or less. Bias increased more significantly in the summer and fall in the Southeast US, with mean bias value increasing between 2-4 µg m<sup>-3</sup> (particularly in the summer). Elsewhere across the country, change in bias was generally small, with some notable isolated exceptions along the west coast.
* **Summary**: PM<sub>2.5</sub> is slightly lower (~0–0.5 µg m<sup>-3</sup> on average across all sites) in the late fall, winter and early spring; and higher (~0.5–1 µg m<sup>-3</sup> on average across all sites) in late spring, summer and early fall.

### Wet Deposition 
* Wet deposition SO<sub>4</sub> is higher in the winter and spring with CMAQv5.3 versus CMAQv5.2.1. Monthly accumulated SO<sub>4</sub> wet deposition averaged across the entire domain is underestimated throughout the year by between 10-40 kg/ha. Updates in CMAQv5.3 generally improve this underestimation by 2-10 kg/ha in the winter and spring. For the summer and fall, SO<sub>4</sub> wet deposition is very similar between CMAQv5.2.1 and CMAQv5.3. The largest improvement in performance in the winter and spring occurs in the eastern US (particularly the Northeast, Southeast and Great Lakes regions) where ambient SO<sub>4</sub> concentrations are generally the highest. Simulating with bi-directional NH<sub>3</sub> exchange (BiDi) turned on has little impact on wet deposition SO<sub>4</sub>.
* Wet deposition NO<sub>3</sub> is higher throughout the year with CMAQv5.3 versus CMAQv5.2.1. Monthly accumulated NO<sub>3</sub> wet deposition averaged across the entire domain is underestimated in the winter, spring and early summer with CMAQv5.2.1 by approximately 5-40 kg/ha. This underestimation improves with v5.3 by ~5-15 kg/ha, resulting in a smaller underestimation in the winter and spring to relatively unbiased in the early summer. In late summer and fall, wet deposition NO<sub>3</sub> with CMAQv5.2.1 is relatively unbiased, and this bias increases to approximately 4-8 kg/ha (average monthly accumulated) with CMAQv5.3. The spatial change in wet deposition NO<sub>3</sub> bias is much more heterogeneous than for SO<sub>4</sub> wet deposition, with some sites indicating a large improvement in bias, while other sites indicate an increase in bias. Overall, bias decreases at more sites than it increases in the winter and spring. The largest changes in bias again occur primarily in the eastern half of the US, with some larger values also showing up along the west coast of the US. In the summer and fall, more sites indicate an increase in wet deposition NO<sub>3</sub> bias with CMAQv5.3 than with CMAQv5.2.1. Simulating with bi-directional NH<sub>3</sub> exchange (BiDi) turned on has little impact on wet deposition NO<sub>3</sub>.
* Wet deposition NH<sub>4</sub> is slightly lower in the spring, summer and fall with CMAQv5.3 versus CMAQv5.2.1. Monthly accumulated NH<sub>4</sub> wet deposition averaged across the entire domain is underestimated throughout the year by approximately 20-50 kg/ha. This underestimation increases by approximately 2-10 kg/ha with CMAQv5.3, with the largest increase occuring during the summer. The largest increases in bias in the summer occur in the Midwest and Great Lakes regions. In the fall, the largest increases in bias occur in the upper Midwest, Great Lakes and Northeast regions. Simulating with bi-directional NH<sub>3</sub> exchange (BiDi) turned on greatly improves the wet deposition NH<sub>4</sub> underestimation, particularly in the late spring through early fall. Bias in the summer is reduced by 25-35 kg/ha, while in the spring and fall the bias is reduced by 5-15 kg/ha. This represents a significant reduction in bias and large improvement in model performance for wet deposition NH<sub>4</sub>.
* **Summary**: Wet deposition SO<sub>4</sub> is generally higher with CMAQv5.3 (versus CMAQv5.2.1), particularly in the winter and spring, resulting in an improvement in the underestimation of wet deposition SO<sub>4</sub>. Wet deposition NO<sub>3</sub> is also higher with CMAQv5.3, resulting in an improvement in bias in the winter, spring, and late summer and slight increase in bias in the late summer and fall. Wet deposition NH<sub>4</sub> is generally lower with CMAQv5.3, particularly in the late spring through early fall, which results in increased bias, particularly in the summer. Invoking bi-directional NH<sub>3</sub> exchange greatly improves the underestimation in wet deposition NH<sub>4</sub> in the late spring through early fall.

<a id=why_update_v53_v531></a>
## Do I need to update from v5.3 to v5.3.1?
CMAQv5.3.1 is a minor update to CMAQv5.3 that includes multiple bug fixes and a few feature additions. See the [v5.3.1 Release Notes](README.md#summary-of-cmaqv531-updates) for a list of bug fixes and new features. See below for a description of the bug fixes that have an impact on model output.

<a id=update_v53_v531></a>
## What do I need to do to update from v5.3 to v5.3.1?
If you have already successfully migrated to v5.3, you will not need to do anything special to maintain default behavior in v5.3.1.  CMAQv5.3.1 comes with new input and output benchmark data for the July 2016 test case over the Southeast US.  The input datasets are identical to those released wtih v5.3 but additional files are now included in the .tar.gz files that will allow users to test the WRFv4.1.1-CMAQv5.3 coupled model on the Southeast benchmark domain. As a result, there is no need for users who have already downloaded the v5.3 Southeast benchmark input data to download the v5.3.1 files unless they are planning to run the coupled model.  The Southeast benchmark output data for v5.3.1 is slightly different from what was released with v5.3 as described in the following FAQ.

<a id=diff_v53_v531></a>
## What differences should I expect in my model results with v5.3.1 compared to v5.3?
Two updates in v5.3.1 have the potential to change model output.  
1. If running CMAQ with the STAGE option for dry deposition, the [STAGE bugfix ](CMAQv5.3.1_bugfixes.md#5-stage) will lead to small changes in dry deposition fluxes and concentration values. Most of the differences follow a random spatial pattern with the exception of NH<sub>3</sub> dry deposition which shows the effect of including the previously omitted deposition to water in the updated code. This omission was only written to the output file and the impact on ambient concentrations are much smaller and only due to the simplication of the numerics in the STAGE module.   
2. In the v5.3 release there was a mismatch between the emission stream labels in the 2016 SE benchmark run script and the EmissCtrl file for cb6r3_ae7_aq. The result was that in the benchmark output files, PCSOA was not “switched off” for fire related emissions streams, including wildfires, agricultural burning, and residential wood combustion.  The emission stream labels have now been corrected.  This results in lower PM<sub>2.5</sub> for the 2016 SE benchmark case and updated benchmark output reference files have been posted on the [CMAS Data Warehouse Google Drive](https://drive.google.com/drive/folders/10wFNch1MkI49ZjD2XD6wK2xzDWOav2zY). The other run scripts in the repository had the correct emissions stream labels, and so are not changed in this release.  

<a id=why_update_v531_v532></a>
## Do I need to update from v5.3.1 to v5.3.2?
CMAQv5.3.2 is a minor update to CMAQv5.3.1 that includes multiple bug fixes and a few feature additions. See the [v5.3.2 Release Notes](README.md#summary-of-cmaqv532-updates) for a list of bug fixes and new features. See below for a description of the bug fixes that have an impact on model output.

CMAQv5.3.2 includes significant updates to the CMAQ Integrated Source Apportionment Method (ISAM). The new CMAQ-ISAM version includes substantial updates to the gas-phase chemistry apportionment algorithms that improves both physical and numerical aspects of the method. Users of ISAM are strongly encouraged to update to CMAQv5.3.2.

<a id=update_v531_v532></a>
## What do I need to do to update from v5.3.1 to v5.3.2?
If you have already successfully migrated to v5.3 or v5.3.1, you will not need to do anything special to maintain default behavior in v5.3.2. A new benchmark dataset has been released with version 5.3.2, including reference input and output to run a test case of CMAQ-ISAM.  

<a id=diff_v531_v532></a>
## What differences should I expect in my model results with v5.3.2 compared to v5.3.1?
Five updates in v5.3.2 have the potential to change model output.
1. If running CMAQ with the STAGE option for dry deposition, the [STAGE underflow bugfix](CMAQv5.3.2_bugfixes.md#10-correct-periodic-underflow-in-the-stage-deposition-option) will lead to small changes in the deposition of gaseous species. Most of the differences arise from water/coastal grid cells and can change the values of all species in the modeling domain by a few percent. 
2. In the v5.3 and subsequent v5.3.1 release, running with the CB6r3m mechanism (Dimethyl Sulfide (DMS) chemistry combined with CB6r3) resulted in a double counting of HO2 in one of the chemical reactions. This mainly resulted in an overprediction of ozone over large areas of seawater in the modeling domain. For more information please visit the [DMS Chemistry Update Release Note](CMAQv5.3.2_DMS_chemistry_update.md).
3. If running v5.3.2 with the Inline Lightning Option, users should expect to see changes of less than a ppb for species such as ozone. The vertical distribution of lightning NO has been modified to better represent what has been reported in literature. For more information please visit the [Lightning NO Vertical Profile Release Notes](CMAQv5.3.2_update_the_lightning_NO_vertical_profile.md). 
4. The v5.3.2 release also features an update to the Ozone Monitoring Instrument (OMI) Column Data through 2019. Most users should observe no changes, however, those working with modeling data in 2018 could see a difference between their v5.3.1 and v5.3.2 simulations caused by a 1% difference in a few grid cells in the new OMI data compared to the older OMI data for overlapping time periods in 2018. For more information please visit the [OMI data Release Notes](CMAQv5.3.2_OMI_through_2019.md).
5. The v5.3.2 release dramatically changes the chemistry algorithms governing the propogation of tagged concentrations through the various chemical mechanisms in CMAQ. This release addresses the previously identified issue with ISAM where the method was ersoneously overattributing tagged math into the "OTHER" tag at the expense of user-specified tags.  ISAM applications using this release should expect drastically different and much more realistic tagged predictions of secondary gaseous polluants such as ozone.  For additional information, please refer the the [ISAM release notes](CMAQv5.3.2_ISAM_gas_chemistry.md).


<a id=why_update_v532_v533></a>
## Do I need to update from v5.3.2 to v5.3.3?
CMAQv5.3.3 is a minor update to CMAQv5.3.1 that includes multiple bug fixes. See the [v5.3.3 Release Notes](README.md#summary-of-cmaqv533-updates) for a list of bug fixes and updates to pre- and post-processint utilities. See below for a description of the bug fixes that have an impact on model output. This release also includes a new version of the WRF-CMAQ coupled system with a streamlined build process. Users of WRF-CMAQ are strongly encouraged to try the latest version.

<a id=update_v532_v533></a>
## What do I need to do to update from v5.3.2 to v5.3.3?
If you have already successfully migrated to v5.3, v5.3.1, or v5.3.2 you will not need to do anything special to maintain default behavior in v5.3.3. A new benchmark output dataset has been released to run the test case for WRFv4.3-CMAQv5.3.3. 

<a id=diff_v532_v533></a>
## What differences should I expect in my model results with v5.3.3 compared to v5.3.2?
Six updates in v5.3.3 have the potential to change model output.
1. If running CMAQ with the STAGE option for dry deposition, the [STAGE O3 deposition to wet soil bugfix](CMAQv5.3.3_bugfixes.md#13-correct-o3-deposition-to-wet-soil-in-the-stage-deposition-option) will lead to slightly higher, typically less than 1ppb, ozone values primarily at night, early mornings and during precipitation events when both cuticular and soil surfaces are wet.
2. If running Process Analysis (PA), the [PA reintialization bugfix](CMAQv5.3.3_bugfixes.md#15-resolve-error-in-process-analysis-reintialization-after-aerosol-processing) resolves a mass closure issue for species that have non-negligible aerosol condensation.
3. If running CMAQ with chemical mechanism cb6r3m_ae7_kmtbr, the DMS chemistry bugfix leads to small (<= 1%) changes in DMS, SO2, and sulfate concentrations, as documented in the [DMS Release Note](CMAQv5.3.3_DMS_chemistry_bugfix.md).
4. If running CMAQ with chemical mechanism cb6mp_ae6_aq, the [corrected control file](CMAQv5.3.3_bugfixes.md#11-correct-chemistry-data-for-reactive-tracers) leads to significant changes in concentration for a subset of Hazardous Air Pollutants (HAPs). Based on US continental simulations for 2017, the most affected HAPs are ethyl benzene (NMB = 21.0%), hexane (NMB = 17.0%), acrylic acid (NMB = 17.8%), and chloroprene (NMB = 4.4%), where NMB is domain average normalized mean bias for July 2017.
5. The [bugfix to the inline photolyis module](CMAQv5.3.3_bugfixes.md#12-remove-differences-in-predictions-between-photdiag-true-and-false) can lead to small (<1% differences) in ozone and aerosol nitrate at select grid cell and timesteps.  This bugfix does not impact the model output of the 2016 Southeast benchmark dataset released with v5.3.2.
6. HONO dry deposition flux in CMAQv5.3 to v5.3.2 can be negative when surface heterogeneous production exceeds deposition.  Updates to [M3DRY](CMAQv5.3.3_bugfixes.md#4-hono-deposition-fix-for-the-m3dry-deposition-option) and [STAGE](CMAQv5.3.3_bugfixes.md#3-hono-deposition-fix-for-the-stage-deposition-option) deposition options in v5.3.3 ensure the dry deposition flux is always positive, representing downward dry deposition rather than total flux. 

Note that there is no new benchmark data for the base model or CMAQ-ISAM for v5.3.3.  Users can use the 2016 southeast benchmark case released with v5.3.2 for tesing their CMAQv5.3.3 and CMAQ-ISAM builds.  Model output for the southeast benchmark case for v5.3.3 is identical to the output from v5.3.2 with the exception of HONO dry deposition.  The HONO dry depostion update (item 6 above) leads to small changes (on the order of 1e-5) in HONO dry deposition in the dry deposition output files for the base model and ISAM benchmark (i.e., CCTM_DRYDEP_v532_ISAM_gcc_Bench_2016_12SE1_2016, CCTM_SA_DRYDEP_v532_ISAM_gcc_Bench_2016_12SE1_2016).  

New benchmark data has been released for the new WRFv4.3-CMAQv5.3.3 model.  See the [WRF-CMAQ tutorial for more information.](../Users_Guide/Tutorials/CMAQ_UG_tutorial_WRF-CMAQ_build_gcc.md)

<a id=additional_faq></a>
## Additional FAQ
A more general list of Frequent CMAQ Questions can be found on our website: https://www.epa.gov/cmaq/frequent-cmaq-questions

<a id=tech_support></a>
## Technical support for CMAQ
Technical support for CMAQ, including questions about model inputs, downloading, compiling, and running the model, 
and pre- and post-processing utilities, should be directed to the [CMAS Center User Forum](https://forum.cmascenter.org/). 
 [**Please read and follow these steps**](https://forum.cmascenter.org/t/please-read-before-posting/1321) prior to submitting new questions to the User Forum.

