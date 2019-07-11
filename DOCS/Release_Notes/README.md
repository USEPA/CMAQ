CMAQv5.3 Release Notes 
=====================================

[Tutorial on installing and running the CMAQ test case](../Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)  
[CMAQv5.3 User's Guide](../Users_Guide/README.md)

# Summary of CMAQv5.3 Updates

The Community Multiscale Air Quality (CMAQ) Modeling System, version 5.3, is a major update. CMAQv5.3 includes several changes to the science algorithms in the base model (CCTM), as well as updates to instrumented models, pre-processors, post-processors and utilities. CMAQv5.3 was primarily developed by the U.S. EPA, and it includes contributions from research partners.  The enhancements to the CMAQ modeling system since the previous release (CMAQv5.2.1) are summarized below.

<a id="instrumented_models"></a>
## Instrumented Models
 * [ISAM for CMAQv5.3](updates_to_CMAQ_ISAM.md)
 * [Sulfur Tracking Model for CMAQv5.3](updates_to_CMAQ_STM.md)
 * CMAQv5.3-DDM-3D will be released later in 2019. 
 * [Documentation for CMAQv5.2-DDM-3D](https://github.com/USEPA/CMAQ/blob/5.2_DDM-3D/DOCS/Instrumented_Docs/CMAQ_DDM.md)

 
<a id="chemistry"></a>
## Chemistry
### Photochemistry

  * [Removal of obsolete mechanisms in CMAQv5.3](obsolete_mechanisms.md)
  * [Revision of halogen-mediated first-order ozone loss (all mechanisms)](simple_halogen_chemistry.md)
  * [Adding detailed halogen and DMS chemistry to CB6r3](detailed_halogen_and_DMS_chemistry.md)
  * [Updates to the chlorine chemistry in CB6r3 in CMAQv5.3](chlorine_chemistry_CB6r3.md)
  * [Updates to the utilities that create an EBI solver](updates_to_create_ebi.md)
  * [Allow EBI solvers to conduct integrated reaction rate analysis](allow_ebi_to_do_IRR_analysis.md)
  
### Photolysis Rates
 * [Updates to inline photolysis diagnostics and OMI data files](inline_phot_diagnostic_and_OMI.md)
 * [Removing sporadic floating point crashes with the PGI Fortran compiler](inline_phot_pgi_floating_point_crashes.md)
 * [Updates to the _inline_phot_preproc_ utility](updates_to_inline_phot_preproc.md)
 
### New Aerosol Modules AERO7 and AERO7i
  * [Overview of *AERO7* and *AERO7i*](aero7_overview.md)  
  * [Monoterpene SOA](monoterpene_SOA.md)  
  * [Reorganization of anthropogenic SOA species](anthro_SOA.md)  
  * [Added uptake of water onto hydrophilic organic aerosol](organic_water.md)  
  * [Corrected the conversion of inorganic to organic sulfate](inorganicsulfate_iepox_fix.md)  

### Other Aerosol Processes
  * [Aerosol Dry Deposition Algorithm Updated](aerosol_dry_deposition.md)  
  * [Fix to Gravitational Settling Sub-Time-Step Calculation](gravitational_settling.md)  
  
### Aqueous and Heterogeneous Chemistry
 * [AQCHEM-KMT2: Extended inorganic and organic cloud chemistry using the Kinetic PreProcessor](aqchem-kmt2.md)
 
## Transport Processes
 * [Changed ACM cloud model to use Z-coordinates](Z-coords%20for%20ACMcloud.md)
 
## Air-Surface Exchange
 * [New NH<sub>3</sub> bi-directional flux in M3Dry linked to daily EPIC input](M3dry-Bidi.md)
 * [STAGE: Surface Tiled Aerosol and Gaseous Exchange dry deposition option](stage_overview.md)
 * [Minor updates to air-surface exchange options](asx_run_options.md)

## Emission Updates
 * [DESID: Detailed Emissions Scaling, Isolation and Diagnostic Module](emissions_redesign.md)
 * [BEIS Default Chemical Mapping](BEIS_mapping.md)
 * [Biogenic speciation update for aero7](biogenic_apinene.md)

## Process Analysis
 * [Extension to aerosol sub-processes](aerosol_process_analysis.md)
 * [IRR analysis available in EBI solvers](allow_ebi_to_do_IRR_analysis.md)

## Structural Improvements
 * [Standardized and Streamlined Logfiles](logfile.md)
 * [Moved PHOT to Sciproc](move_phot_to_sciproc.md)
 * [Standardized Units for Output Variables](output_units.md)

## Diagnostic Options
 * [Vertical Profile Extraction: extend CCTM to output vertical profiles at specified locations](vertical_extraction.md)

## Pre-processors, Post-processors, and Utilities
 * [Updates in MCIPv5.0](../../PREP/mcip/docs/ReleaseNotes)
 * [Updates to the ICON and BCON pre-processors.](updates_to_ICON_BCON.md)
 * [New create_omi pre-processor.](Add_create_omi_tool.md)
 * [Updates to the create_ebi utility](updates_to_create_ebi.md)
 * [Updates to the inline_phot_preproc utility](updates_to_inline_phot_preproc.md)
 * [Updates to species definition files (SpecDef*.txt) for aerosol modules](specdef_aero.md)
 * [Updates to post-processing tools: hr2day, sitecmp, and sitecmp_dailyo3; release of new post-processing tool: calc_tmetric.](postprocessing_tools.md)

