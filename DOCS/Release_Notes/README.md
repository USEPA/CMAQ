CMAQv5.3 Release Notes 
=====================================

# Getting Started with CMAQ 
**>>COMMENT<<**  Is this still consistent with the documentation we are providing?  Should we put the full users guide link here, too?  TLS 25 Jun 2019

[Tutorial on installing and running CMAQ](../Tutorials/CMAQ_GettingStarted.md)  
[Tutorial on running the CMAQ test case](../Tutorials/CMAQ_Benchmark.md)  

# Summary of CMAQv5.3 Updates

The Community Multiscale Air Quality (CMAQ) Modeling System, version 5.3, is a major update. CMAQv5.3 includes several changes to the science algorithms in the base model (CCTM), as well as updates to instrumented models, pre-processors, post-processors, utilities, and tools. CMAQv5.3 was primarily developed by the U.S. EPA, and it includes contributions from research partners.  The enhancements to the CMAQ modeling system since the previous release (CMAQv5.2.1) are summarized below.

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
 * [Floating point crashes using the Portland Group Compiler](inline_phot_pgi_floating_point_crashes.md)
 * [inline_phot_preproc utility](updates_to_inline_phot_preproc.md)
 
### New Aerosol Modules AERO7 and AERO7i
  * [Overview of AERO7/7i](aero7_overview.md)  
  * [Monoterpene SOA](monoterpene_SOA.md)  
  * [Reorganization of anthropogenic SOA species](anthro_SOA.md)  
  * [Uptake of water onto hydrophilic organic aerosol](organic_water.md)  
  * [Conversion of inorganic to organic sulfate bug fix](inorganicsulfate_iepox_fix.md)  

### Other Aerosol Processes
  * [Dry Deposition Algorithm](aerosol_dry_deposition.md)  
  * [Gravitational Settling](gravitational_settling.md)  
  
### Aqueous and Heterogeneous Chemistry
 * [AQCHEM-KMT2: Extended inorganic and organic cloud chemistry using the Kinetic PreProcessor](aqchem-kmt2.md)
 
## Transport Processes
 * [Changed ACM cloud model to use Z-coordinates](Z-coords%20for%20ACMcloud.md)
 
## Air-Surface Exchange
 * [New NH<sub>3</sub> bi-directional flux linked to daily EPIC input](M3dry-Bidi.md)
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

## Pre-processors, Post-processors, Tools, and Utilities
**>>COMMENT<<** Sugggest broadening the title; I don't see ICON and BCON as "tools", per se.  Also suggest reordering to the order they might be used in the workflow.  Suggest adding a line and hyperlink for MCIPv5.0 here; that link does not work yet.  TLS 25 Jun 2019
 * [Updates in MCIPv5.0](updates_to_MCIPv5_0.md)
 * [Updates to the ICON and BCON pre-processors.](updates_to_ICON_BCON.md)
 * [Updates to the create_ebi utility](updates_to_create_ebi.md)
 * [Updates to the inline_phot_preproc utility](updates_to_inline_phot_preproc.md)
 * [The create_omi tool.](Add_create_omi_tool.md)
 * [SpecDef aerosol updates](specdef_aero.md)
 * [Updates to post-processing tools: hr2day, sitecmp, and sitecmp_dailyo3; release of new post-processing tool: calc_tmetric.](postprocessing_tools.md)

 
## Instrumented Models
**>>COMMENT<<** The CMAQ-DDM and CMAQ-ISAM need README files.  Will sulfur tracking be released, too?  TLS 25 Jun 2019
 * [Updates in CMAQ-DDM](updates_to_CMAQ_DDM.md)
 * [Updates in CMAQ-ISAM](updates_to_CMAQ_ISAM.md)


