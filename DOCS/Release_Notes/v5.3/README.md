CMAQv5.3 Release Notes 
=====================================

# Getting Started with CMAQ  
[Tutorial on installing and running CMAQ](../../../DOCS/Tutorials/CMAQ_GettingStarted.md)  
[Tutorial on running the CMAQ test case](../../../DOCS/Tutorials/CMAQ_Benchmark.md)  

# Summary of CMAQv5.3 Updates

The Community Multiscale Air Quality (CMAQ) Model version 5.3 is a major update to CMAQ that includes several changes to the science algorithms in the base model.  CMAQ v5.3 was developed by the U.S. EPA with contributions from other research partners. Summarized below are the main enhancements to the modeling system since the previous release, CMAQ v5.2.1.

<a id="chemistry"></a>
## Chemistry
### Photochemistry
There are 11 unique gas-phase chemical mechanisms in CMAQv5.3. These are all located in the MECHS/ folder and may be invoked when building the model and Makefile. Variations of Carbon Bond 6 (CB6), RACM2, and SAPRC07 are all available. Specific science updates include the following:  

  * [Halogen mediated first order ozone loss is revised for all mechanisms](simple_halogen_chemistry.md)
  * [Detailed halogen and DMS chemistry with CB6r3](detailed_halogen_and_DMS_chemistry.md)
  * EBI Solvers
    * [Setting Maximum Integration Time Step and Initial Changes for CMAS-ISAM](updates_to_create_ebi.md)
    * [Ability to conduct Integrated Reaction Rate Analysis](allow_ebi_to_do_IRR_analysis.md)
  
### Photolysis Rates
 * [In-line photolysis diagnostics and OMI files](inline_phot_diagnostic_and_OMI.md)
 * [Floating point crashes using the Portland Group Compiler](inline_phot_pgi_floating_point_crashes.md)
 * [inline_phot_preproc utility](updates_to_inline_phot_preproc.md)
 
### Aerosol Processes
CMAQ v5.3 introduces aero7 and aero7i. Aero6, available in previous versions of CMAQ, is still available. Aero 7/7i differs from aero6 in its treatment of organic aerosol.
#### AERO7/7i
  * [Overview of AERO7/7i](aero7_overview.md)  
  * [Monoterpene SOA](monoterpene_SOA.md)  
  * [Reorganization of anthropogenic SOA species](anthro_SOA.md)  
  * [Uptake of water onto hydrophilic organic aerosol](organic_water.md)  
  
### Aqueous and Heterogeneous Chemistry
 * [AQCHEM-KMT2: Extended inorganic and organic cloud chemistry using the Kinetic PreProcessor](aqchem-kmt2.md)
 
## Transport Processes
 * [Changed ACM cloud model to use Z-coordinates](Z-coords%20for%20ACMcloud.md)
 
## Air-Surface Exchange
 * [New NH3 bi-directional flux linked to daily EPIC input](M3dry-Bidi.md)

## Emission Updates
 * [Biogenic speciation update for aero7](biogenic_apinene.md)

## Process Analysis
 * [IRR analysis available in EBI solvers](allow_ebi_to_do_IRR_analysis.md)

## Structural Improvements
 * [Moved PHOT to Sciproc](move_phot_to_sciproc.md)

## Diagnostic
 * [Vertical Profile Extraction: extend CCTM to output vertical profiles at specified locations](vertical_extraction.md)

## Tools & Utilities
 * [SpecDef aerosol updates](specdef_aero.md)
 * [Updates to post-processing tools hr2day, sitecmp, and sitecmp_dailyo3; addition of new utility calc_tmetric.](postprocessing_tools.md)
 * [Updates to the create_ebi utility](updates_to_create_ebi.md)
 * [Updates to the inline_phot_preproc utility](updates_to_inline_phot_preproc.md)
 
## Instrumented Models
CMAQ-DDM and CMAQ-ISAM will be released with the final version of CMAQv5.3 in Spring 2019.


