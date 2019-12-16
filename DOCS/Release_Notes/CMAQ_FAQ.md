# Frequently Asked Questions for Upgrading to the Latest CMAQ Version

## Table of Contents:

* [What do I need to do to update from v5.2.1 to v5.3?](#update_v521_v53)
* [What differences should I expect in my model results with v5.3 compared to v5.2.1?](#diff_v521_v53)
* [What do I need to do to update from v5.3 to v5.3.1?](#update_v53_v531)
* [What differences should I expect in my model results with v5.3.1 compared to v5.3?](#diff_v53_v531)
* [Additional FAQ](#additional_faq)
* [Technical support for CMAQ](#tech_support)

<a id=update_v521_v53></a>
## What do I need to do to update from v5.2.1 to v5.3?
* A major change to CMAQ in version 5.3 is the incorporation of the new emissions module, DESID, and 
its control via a number of RunScript variables and the Emission Control File, which is explained in 
the [CMAQ User’s Guide section 6.9.3](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#6.9.3_Emission_Compatability). **Due to this change and other changes in environment variables, it is strongly recommended that you start with one of the run scripts released with the latest CMAQ version and adapt it for your application rather than working off of run scripts you may have developed for your application using earlier versions of CMAQ.**
* If you are upgrading to CMAQv5.3 but you are not changing mechanisms (i.e. you have been using cb6r3_ae6 and you 
will continue to do so), it is hoped that you will not need to do anything special to maintain default behavior. 
In other words, the default emission control files will handle simple assignment of CMAQ species to emissions 
surrogates (e.g. NO --> NO; ASO4 --> PSO4, etc.).  
* If you were using a chemical mechanism with nonvolatile POA (e.g. cb6r3_ae6nvpoa_aq; APOC and APNCOM were transported in the list of transported species), then you will need to make a modification to the emission control file to continue using this assumption. Beginning in CMAQv5.3, nonvolatile POA and semivolatile POA are available in all mechanisms. It is now up to the user to direct the primary organic emissions into either nonvolatile species or semivolatile species. Every chemical mechanism in CMAQv5.3 and beyond assumes that generic POA emissions will be treated as semivolatile. If you wish to treat all POA emissions as nonvolatile or only the POA emissions from certain streams (e.g. fugitive dust) as nonvolatile, then you can follow the instructions in the emission control file to implement this assumption. All emissions from POC and PNCOM that a user wants to treat as nonvolatile should be directed to APOC and APNCOM, respectively.
* It is good practice to review the new CMAQ logfiles labeled "CTM_LOG_[XXX]..." to confirm that your 
emissions are being read as you expect. In particular pay attention to the sections beginning:
  * "SCALING EMISSIONS CONSISTENT WITH EMISSIONS CONTROL FILE SUPPLIED BY USER"
  * "Checking for unused Emissions Surrogates"
  * "EMISSIONS SCALING DIAGNOSTIC"
  
	Note that the warnings and notices for potential emissions issues have improved in clarity in v5.3.1. 
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
(E2C_CHEM) input files using the latest version of FEST-C, version 1.4. See Chapter 6 for further information. Running CMAQ with the 
STAGE option and ammonia bidirectional surface flux is compatible with previous versions of FEST-C.  
Please see the User's Guide [Chapter 6](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#68-dry-depositionair-surface-exchange) for further information on 
M3DRY and STAGE and [Chapter 4](CMAQ_UG_ch04_model_inputs.md#e2c_lu) to read more about the land surface input files required for running with ammonia bidi. 

<a id=diff_v521_v53></a>
## What differences should I expect in my model results with v5.3 compared to v5.2.1?
The following summary is based off of our incremental testing of the science updates in CMAQv5.3 using v5.3 and v5.2.1 annual 2016 simulations over the CONUS (12km horizontal grid resolution, 35 vertical layers, cb6r3_ae7_aq chemical mechanism).
### Ozone
* Large increase in wintertime O<sub>3</sub> due to reduced deposition to snow in M3Dry. As expected, primarily affects northern latitudes with snow cover. Increases in O<sub>3</sub> in winter in 2016 over the CONUS range from 1-2 ppbV as far south as VA, TN, MO, KS, and CO to 10+ ppbV over ND, SD, WI, MN and central Canada.
* Outside of winter, O<sub>3</sub> generally decreases across the CONUS primarily due to updates to the PX-LSM vegetation fraction and LAI, with significantly reduces vegetation fraction which in turn reduces O<sub>3</sub> dry deposition and increases O<sub>3</sub> ambient mixing ratios. Additional chemistry updates in the model had a small impact on O<sub>3</sub>. Decreases generally range between 1-6 ppbV, with the largest decrease in the western US, and similar magnitudes across spring, summer and fall.
* Science updates affecting O<sub>3</sub> had the overall effect of dramatically improving O<sub>3</sub> underestimation in the winter (particularly the northern latitudes), slightly increasing O<sub>3</sub> overestimation in the spring, reducing O<sub>3</sub> bias in the summer and fall in the eastern US, and increasing O<sub>3</sub> bias in the summer and fall in the western US, particularly California (except for along the coast where O<sub>3</sub> bias decreases).
* **Summary**: O<sub>3</sub> is higher (~2 ppbV on average across all sites) in the winter (December/January); and lower (~2 ppbV on average across all sites) in the spring, summer and fall.
### PM<sub>2.5</sub>
* Large increase in wintertime PM<sub>2.5</sub> in the eastern US due to increased oxidants in v5.3. This increase generally ranges between 1-4 µgm<sup>-3</sup>, with the largest increase in the upper Mid-West. However, this large increase in wintertime PM<sub>2.5</sub> due to science updates in v5.3 is essentially offset here by a similar large decrease in PM<sub>2.5</sub> by removing the application of PCSOA to residential wood combustion (RWC). PCSOA was never intended to be applied to RWC, however a limitation in CMAQ versions prior to v5.3 to only allow a single gridded emissions input file to be read did not provide a mechanism for enabling PCSOA but removing its application to RWC. The new ability in CMAQv5.3 to read multiple gridded emission input files allows for the PCSOA to be implemented, but not on RWC. This results in an overall decrease in total PM<sub>2.5</sub>, as PCSOA is intended to account for missing sources of aerosol. This decrease generally ranges between 1-4 µgm<sup>-3</sup>, with isolated areas with larger decreases.
* In addition to these changes in winter, there is a small increase in springtime, and a relatively large increase in summer and fall PM<sub>2.5</sub> in the Southeast US due to increased SOA from monoterpene oxidation. These increases are on the order of 1-2 ugm<sup>-3</sup> in spring, and generally 2-4 µgm<sup>-3</sup> in summer and fall.
* As the overall change in PM<sub>2.5</sub> in the winter was close to zero, overall model performance did not change much, with changes in mean bias between -2 to 2 µgm<sup>-3</sup>. Change in performance in the spring was also relatively small, although there was a slight overall increase in bias in the Southeast US, generally on the order of 2 µgm<sup>-3</sup> or less. Bias increased more significantly in the summer and fall in the Southeast US, with mean bias value increasing between 2-4 µgm<sup>-3</sup> (particularly in the summer). Elsewhere across the county, change in bias was generally small, with some notable isolated exceptions along the west coast.
Bottom Line Takeaways:
* **Summary**: PM<sub>2.5</sub> is slightly lower (~0-0.5 µgm<sup>-3</sup> on average across all sites) in the late fall, winter and early spring; and higher (~0.5 – 1 µgm<sup>-3</sup> on average across all sites) in late spring, summer and early fall.

<a id=update_v53_v531></a>
## What do I need to do to update from v5.3 to v5.3.1?
CMAQv5.3.1 is a minor bugfix release.  If you have already successfully migrated to v5.3, you will not need to do anything special to maintain default behavior in v5.3.1.  CMAQv5.3.1 comes with new input and output benchmark data for the July 2016 test case over the Southeast US.  The input datasets are identical to those released wtih v5.3 but additional files are now included in the .tar.gz files that will allow users to test the WRFv4.1.1-CMAQv5.3 coupled model on the Southeast benchmark domain. As a result, there is no need for users who have already downloaded the v5.3 Southeast benchmark input data to download the v5.3.1 files unless they are planning to run the coupled model.  The Southeast benchmark output data for v5.3.1 is slightly different from what was released with v5.3 as described in the following FAQ.

<a id=diff_v53_v531></a>
## What differences should I expect in my model results with v5.3.1 compared to v5.3?
Two updates in v5.3.1 have the potential to change model output.  
1. If running CMAQ with the STAGE option for dry deposition, the [STAGE bugfix ](CMAQv5.3.1_bugfixes.md#5-stage) will lead to small changes in dry deposition fluxes and concentration values. Most of the difference follow a random spatial pattern with the exception of NH<sub>3</sub> dry deposition which shows the effect of including the deposition to water in the updated code.  The CMAQv5.3 benchmark output reference files were created using the M3DRY option for dry deposition and so are not impacted by this model update. 
2. In the v5.3 release there was a mismatch between the emission stream labels in the 2016 SE benchmark run script and the EmissCtrl file for cb6r3_ae7_aq. The result was that in the benchmark output files, PCSOA was not “switched off” for fire related emissions streams, including wildfires, agricultural burning, and residential wood combustion.  The emission stream labels have now been corrected.  This results in lower PM<sub>2.5</sub> for the 2016 SE benchmark case and updated benchmark output reference files have been posted on the [CMAS Data Warehouse Google Drive](https://drive.google.com/drive/folders/10wFNch1MkI49ZjD2XD6wK2xzDWOav2zY). The other run scripts in the repository had the correct emissions stream labels, and so are not changed in this release.  

<a id=additional_faq></a>
## Additional FAQ
A more general list of Frequent CMAQ Questions can be found on our website: https://www.epa.gov/cmaq/frequent-cmaq-questions

<a id=tech_support></a>
## Technical support for CMAQ
Technical support for CMAQ, including questions about model inputs, downloading, compiling, and running the model, 
and pre- and post-processing utilities, should be directed to the [CMAS Center User Forum](https://forum.cmascenter.org/). 
