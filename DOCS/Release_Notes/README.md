CMAQv5.3.3 Release Notes
=====================================
[CMAQv5.3.3 User's Guide](../Users_Guide/README.md)   
[Tutorials on installing and running CMAQ](../Users_Guide/Tutorials/README.md) **- Tutorials on running CMAQ, WRF-CMAQ, and CMAQ-ISAM test case.**   
[Frequently asked questions for upgrading to the latest CMAQ version](CMAQ_FAQ.md)  **- Updated for v5.3.3 release.**  
[Frequently asked questions for upgrading to the latest MCIP version](../../PREP/mcip/docs/FAQ)  **- Updated for v5.3.3 release.**  
[CMAQv5.3.3 Known Issues](../Known_Issues/README.md) - Updated after the v5.3.3 release to alert users of issues that will be addressed with the next release. 

# Summary of CMAQv5.3.3 Updates 
The Community Multiscale Air Quality (CMAQ) Model version 5.3.3 is a minor update to CMAQv5.3 that includes multiple bug fixes. These updates include restoration of windowing capability, allowing users to provide gridded input files which were larger in horizontal extent than the simulated domain. This release also includes a new version of the WRF-CMAQ coupled system with a streamlined build process.  Users of WRF-CMAQ are strongly encouraged to try the latest version. Additionally, MCIP users should note that starting with this release we are aligning the version numbering between CMAQ and MCIP, i.e., the MCIP updates included in this release are labeled MCIPv5.3.3.  

* [CMAQv5.3.3 Bugfixes](CMAQv5.3.3_bugfixes.md)
* [New WRFv4.3-CMAQv5.3.3 Coupled Model](CMAQv5.3.3_Coupled_WRF-CMAQ.md)
* [Updates to MCIP meteorology pre-processor](../../PREP/mcip/docs/ReleaseNotes)
* [Updates to ICON and BCON pre-processing utilities](CMAQv5.3.3_ICON_BCON_updates.md)
* [Updates to the Detailed Emission Scaling Isolation and Diagnostics (DESID) Module](CMAQv5.3.3_DESID_updates.md)
* [Updates to post-processing utilities](CMAQv5.3.3_postprocessing_tools.md)

# Summary of CMAQv5.3.2 Updates 

CMAQv5.3.2 includes significant updates to the CMAQ Integrated Source Apportionment Method (ISAM) and multiple minor fixes to address issues identified in CMAQv5.3.1. The new CMAQ-ISAM version includes substantial updates to the gas-phase chemistry apportionment algorithms that improves both physical and numerical aspects of the method. Users of ISAM are strongly encouraged to update to CMAQv5.3.2.  

* [CMAQv5.3.2 Bugfixes](CMAQv5.3.2_bugfixes.md)
* [Added ISAM support for bidirectional NH3 flux](CMAQv5.3.2_ISAM_bidi_support.md)
* [Revised ISAM method on how gas chemistry affects apportionment](CMAQv5.3.2_ISAM_gas_chemistry.md)
* [Running with Temporally Finer Meteorology](CMAQv5.3.2_running_with_temporally_finer_MET.md)
* [Update to the DMS chemistry with CB6r3](CMAQv5.3.2_DMS_chemistry_update.md)
* [Update the Lightning NO Vertical Profile](CMAQv5.3.2_update_the_lightning_NO_vertical_profile.md)
* [Fine aerosol acidity output](CMAQv5.3.2_specdef_ae7_pH.md)
* [Add make options to bldmake configuration file](CMAQv5.3.2_add_make_options_to_the_cfg_file_for_bldmake.md)
* [Add 2019 data to CMAQ OMI input file](CMAQv5.3.2_OMI_through_2019.md)
* [Column Modeling](CMAQv5.3.2_enable_column_modeling.md)
* [Add ERF and SIGN functions to COMBINE](CMAQv5.3.2_add_ERF_and_SIGN_to_COMBINEs_grid_cell_functions.md)

# Summary of CMAQv5.3.1 Updates  

The Community Multiscale Air Quality (CMAQ) Model version 5.3.1 is a minor update to CMAQv5.3 that includes multiple bug fixes and a few  feature additions.

* [CMAQv5.3.1 Bugfixes](CMAQv5.3.1_bugfixes.md)
* [MCIPv5.1 Bugfixes](MCIPv5.1_bugfixes.md)
* [Add support for defining chemical, region and stream families in DESID](CMAQv5.3.1_DESID_families.md)
* [New  grid mask files to support regional analysis with DESID and ISAM](CMAQv5.3.1_regional_12US1_gridmask.md)

# Summary of CMAQv5.3 Updates

The Community Multiscale Air Quality (CMAQ) Modeling System, version 5.3, is a major update. CMAQv5.3 includes several changes to the science algorithms in the base model (CCTM), as well as updates to instrumented models, pre-processors, post-processors and utilities. CMAQv5.3 was primarily developed by the U.S. EPA, and it includes contributions from research partners.  The enhancements to the CMAQ modeling system since the previous release (CMAQv5.2.1) are summarized below.

<a id="instrumented_models"></a>
## Instrumented Models
 * [ISAM for CMAQv5.3](CMAQv5.3_updates_to_CMAQ_ISAM.md)
 * [Sulfur Tracking Model (STM) for CMAQv5.3](CMAQv5.3_sulfur_tracking.md)
 * [Documentation for CMAQv5.2-DDM-3D](https://github.com/USEPA/CMAQ/blob/5.2_DDM-3D/DOCS/Instrumented_Docs/CMAQ_DDM.md)

 
<a id="chemistry"></a>
## Chemistry
### Photochemistry

  * [Removal of obsolete mechanisms in CMAQv5.3](CMAQv5.3_obsolete_mechanisms.md)
  * [Revision of halogen-mediated first-order ozone loss (all mechanisms)](CMAQv5.3_simple_halogen_chemistry.md)
  * [Adding detailed halogen and DMS chemistry to CB6r3](CMAQv5.3_detailed_halogen_and_DMS_chemistry.md)
  * [Updates to the chlorine chemistry in CB6r3 in CMAQv5.3](CMAQv5.3_chlorine_chemistry_CB6r3.md)
  * [Updates to the utilities that create an EBI solver](CMAQv5.3_updates_to_create_ebi.md)
  * [Allow EBI solvers to conduct integrated reaction rate analysis](CMAQv5.3_allow_ebi_to_do_IRR_analysis.md)
  
### Photolysis Rates
 * [Updates to inline photolysis diagnostics and OMI data files](CMAQv5.3_inline_phot_diagnostic_and_OMI.md)
 * [Removing sporadic floating point crashes with the PGI Fortran compiler](CMAQv5.3_inline_phot_pgi_floating_point_crashes.md)
 * [Updates to the _inline_phot_preproc_ utility](CMAQv5.3_updates_to_inline_phot_preproc.md)
 
### New Aerosol Modules AERO7 and AERO7i
  * [Overview of *AERO7* and *AERO7i*](CMAQv5.3_aero7_overview.md)  
  * [Monoterpene SOA](CMAQv5.3_monoterpene_SOA.md)  
  * [Reorganization of anthropogenic SOA species](CMAQv5.3_anthro_SOA.md)  
  * [Added uptake of water onto hydrophilic organic aerosol](CMAQv5.3_organic_water.md)  
  * [Corrected the conversion of inorganic to organic sulfate](CMAQv5.3_inorganicsulfate_iepox_fix.md)  
  * [Nonvolatile POA Option in CMAQv5.3](CMAQv5.3_nonvolatile_POA.md)
  * [Streamlined SOA Module](CMAQv5.3_streamlined_SOA.md)

### Other Aerosol Processes
  * [Aerosol Dry Deposition Algorithm Updated](CMAQv5.3_aerosol_dry_deposition.md)  
  * [Fix to Gravitational Settling Sub-Time-Step Calculation](CMAQv5.3_gravitational_settling.md)  
  * [Simplify Propagation of Aerosol Surface Area to Heterogeneous Chemistry Algorithm](CMAQv5.3_HetChem_aerosol_param.md)
  
### Aqueous and Heterogeneous Chemistry
 * [AQCHEM-KMT2: Extended inorganic and organic cloud chemistry using the Kinetic PreProcessor](CMAQv5.3_aqchem-kmt2.md)
 
## Transport Processes
 * [Changed ACM2 PBL model _vdiff_ to use Z-coordinates](CMAQv5.3_VdiffZ.md)
 * [Changed ACM cloud model to use Z-coordinates](CMAQv5.3_Z-coords%20for%20ACMcloud.md)
 * [Changed hadv to ppm in both x and y](CMAQv5.3_PPM.md)
 
## Air-Surface Exchange
 * [New NH<sub>3</sub> bi-directional flux in M3Dry linked to daily EPIC input](CMAQv5.3_M3dry-Bidi.md)
 * [STAGE: *Surface Tiled Aerosol and Gaseous Exchange* dry deposition option](CMAQv5.3_stage_overview.md)
 * [Minor updates to air-surface exchange options](CMAQv5.3_asx_run_options.md)

## Emission Updates
 * [DESID: *Detailed Emissions Scaling, Isolation and Diagnostic* Module](CMAQv5.3_emissions_redesign.md)
 * [BEIS Default Chemical Mapping](CMAQv5.3_BEIS_mapping.md)
 * [Biogenic speciation update for *AERO7*](CMAQv5.3_biogenic_apinene.md)

## Process Analysis
 * [Process analysis for aerosol sub-processes](CMAQv5.3_aerosol_process_analysis.md)
 * [IRR analysis available in EBI solvers](CMAQv5.3_allow_ebi_to_do_IRR_analysis.md)

## Structural Improvements
 * [Log File Output: Streamlining and Centralization](CMAQv5.3_logfile.md)
 * [Move Photolysis Rate Calculation to SCIPROC](CMAQv5.3_move_phot_to_sciproc.md)
 * [Standardized Units for Output Variables](CMAQv5.3_output_units.md)
 * [New Centralized Input/Output Module (CIO)](CMAQv5.3_centralized_io.md)
 * [Implementation of New Chemical Namelist Format](CMAQv5.3_chemical_namelists.md)

## Diagnostic Options
 * [Vertical Profile Extraction: extend CCTM to output vertical profiles at specified locations](CMAQv5.3_vertical_extraction.md)

## Pre-processors, Post-processors, and Utilities
 * [Updates in MCIPv5.0](../../PREP/mcip/docs/ReleaseNotes)
 * [Updates to the ICON and BCON pre-processors.](CMAQv5.3_updates_to_ICON_BCON.md)
 * [*create_omi*: Creating the OMI data file for inline photolysis](CMAQv5.3_add_create_omi_tool.md)
 * [Updates to the *create_ebi* utility](CMAQv5.3_updates_to_create_ebi.md)
 * [Updates to the *inline_phot_preproc* utility](CMAQv5.3_updates_to_inline_phot_preproc.md)
 * [Updates to species definition files for organic aerosols](CMAQv5.3_specdef_aero.md)
 * [Updates to post-processing utilities](CMAQv5.3_postprocessing_tools.md)

