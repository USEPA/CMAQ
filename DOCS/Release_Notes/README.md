CMAQv5.3.2 Release Notes
=====================================
[CMAQv5.3.2 User's Guide](../Users_Guide/README.md)  
[Tutorials on installing and running CMAQ](DOCS/Users_Guide/Tutorials/README.md) **- Tutorial on running CMAQ test case plus new tutorials on WRF-CMAQ, ISAM, and modifying a chemcial mechanism.**
[Frequently asked questions for upgrading to the latest CMAQ version](CMAQ_FAQ.md)  **- Updated for v5.3.2 release.** 
[CMAQv5.3.2 Known Issues](../Known_Issues/README.md) - Updated after the v5.3.2 release to alert users of issues that will be addressed with the next release. 

# Summary of CMAQv5.3.2 Updates 

CMAQv5.3.2 includes significant updates to the CMAQ Integrated Source Apportionment Method (ISAM) and multiple minor fixes to address issues identified in CMAQv5.3.1. The new CMAQ-ISAM version includes substantial updates to the gas-phase chemistry apportionment algorithms that improves both physical and numerical aspects of the method. Users of ISAM are strongly encouraged to update to CMAQv5.3.2.

* [CMAQv5.3.2 Bugfixes](CMAQv5.3.2_bugfixes.md)
* [Added ISAM support for bidirectional NH3 flux](ISAM_bidi_support.md)
* [Revised ISAM method on how gas chemistry affects apportionment](ISAM_gas_chemistry_v532.md)
* [Running with Temporally Finer Meteorology](running_with_temporally_finer_MET.md)
* [Update to the DMS chemistry with CB6r3](DMS_chemistry_update.md)
* [Update the Lightning NO Vertical Profile](Update_the_lightning_NO_vertical_profile.md)
* [Fine aerosol acidity output](specdef_ae7_pH.md)
* [Add make options to bldmake configuration file](Add_make_options_to_the_cfg_file_for_bldmake.md)
* [Add 2019 data to CMAQ OMI input file](OMI_through_2019.md)
* [Column Modeling](Enable_Column_Modeling.md)
* [Add ERF and SIGN functions to COMBINE](Add_ERF_and_SIGN_to_COMBINEs_grid_cell_functions.md)

# Summary of CMAQv5.3.1 Updates  

The Community Multiscale Air Quality (CMAQ) Model version 5.3.1 is a minor update to CMAQv5.3 that includes multiple bug fixes and a few  feature additions.

* [CMAQv5.3.1 Bugfixes](CMAQv5.3.1_bugfixes.md)
* [MCIPv5.1 Bugfixes](MCIPv5.1_bugfixes.md)
* [Add support for defining chemical, region and stream families in DESID](DESID_families.md)
* [New  grid mask files to support regional analysis with DESID and ISAM](regional_12US1_gridmask.md)

# Summary of CMAQv5.3 Updates

The Community Multiscale Air Quality (CMAQ) Modeling System, version 5.3, is a major update. CMAQv5.3 includes several changes to the science algorithms in the base model (CCTM), as well as updates to instrumented models, pre-processors, post-processors and utilities. CMAQv5.3 was primarily developed by the U.S. EPA, and it includes contributions from research partners.  The enhancements to the CMAQ modeling system since the previous release (CMAQv5.2.1) are summarized below.

<a id="instrumented_models"></a>
## Instrumented Models
 * [ISAM for CMAQv5.3](updates_to_CMAQ_ISAM.md)
 * [Sulfur Tracking Model (STM) for CMAQv5.3](sulfur_tracking.md)
 * CMAQv5.3-DDM-3D will be released later in 2020. 
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
  * [Nonvolatile POA Option in CMAQv5.3](nonvolatile_POA.md)
  * [Streamlined SOA Module](streamlined_SOA.md)

### Other Aerosol Processes
  * [Aerosol Dry Deposition Algorithm Updated](aerosol_dry_deposition.md)  
  * [Fix to Gravitational Settling Sub-Time-Step Calculation](gravitational_settling.md)  
  * [Simplify Propagation of Aerosol Surface Area to Heterogeneous Chemistry Algorithm](HetChem_aerosol_param.md)
  
### Aqueous and Heterogeneous Chemistry
 * [AQCHEM-KMT2: Extended inorganic and organic cloud chemistry using the Kinetic PreProcessor](aqchem-kmt2.md)
 
## Transport Processes
 * [Changed ACM2 PBL model _vdiff_ to use Z-coordinates](VdiffZ.md)
 * [Changed ACM cloud model to use Z-coordinates](Z-coords%20for%20ACMcloud.md)
 
## Air-Surface Exchange
 * [New NH<sub>3</sub> bi-directional flux in M3Dry linked to daily EPIC input](M3dry-Bidi.md)
 * [STAGE: *Surface Tiled Aerosol and Gaseous Exchange* dry deposition option](stage_overview.md)
 * [Minor updates to air-surface exchange options](asx_run_options.md)

## Emission Updates
 * [DESID: *Detailed Emissions Scaling, Isolation and Diagnostic* Module](emissions_redesign.md)
 * [BEIS Default Chemical Mapping](BEIS_mapping.md)
 * [Biogenic speciation update for *AERO7*](biogenic_apinene.md)

## Process Analysis
 * [Process analysis for aerosol sub-processes](aerosol_process_analysis.md)
 * [IRR analysis available in EBI solvers](allow_ebi_to_do_IRR_analysis.md)

## Structural Improvements
 * [Log File Output: Streamlining and Centralization](logfile.md)
 * [Move Photolysis Rate Calculation to SCIPROC](move_phot_to_sciproc.md)
 * [Standardized Units for Output Variables](output_units.md)
 * [New Centralized Input/Output Module (CIO)](centralized_io.md)
 * [Implementation of New Chemical Namelist Format](chemical_namelists.md)

## Diagnostic Options
 * [Vertical Profile Extraction: extend CCTM to output vertical profiles at specified locations](vertical_extraction.md)

## Pre-processors, Post-processors, and Utilities
 * [Updates in MCIPv5.0](../../PREP/mcip/docs/ReleaseNotes)
 * [Updates to the ICON and BCON pre-processors.](updates_to_ICON_BCON.md)
 * [*create_omi*: Creating the OMI data file for inline photolysis](Add_create_omi_tool.md)
 * [Updates to the *create_ebi* utility](updates_to_create_ebi.md)
 * [Updates to the *inline_phot_preproc* utility](updates_to_inline_phot_preproc.md)
 * [Updates to species definition files for organic aerosols](specdef_aero.md)
 * [Updates to post-processing utilities](postprocessing_tools.md)

