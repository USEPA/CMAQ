CMAQv5.3 Release Notes 
=====================================

# Getting Started with CMAQ  
[Building and running CMAQ](../../../DOCS/User_Manual/CMAQ_OGD_ch05_sys_req.md)  
[Building and running WRF-CMAQ Two Way Model](Two_Way_Coupled_WRF-CMAQ.md)

# Summary of CMAQv5.3 Updates

The Community Multiscale Air Quality (CMAQ) Model version 5.3 is a major update to CMAQ that includes several changes to the science algorithms in the base model.  CMAQ v5.3 was developed by the U.S. EPA with contributions from other research partners. Summarized below are the main enhancements to the modeling system since the previous release, CMAQ v5.2.1.

<a id="chemistry"></a>
## Chemistry
### Photochemistry
There are 11 unique gas-phase chemical mechanisms in CMAQv5.3. These are all located in the MECHS/ folder and may be invoked when building the model and Makefile. Variations of Carbon Bond 6 (CB6), RACM2, and SAPRC07 are all available. Specific science updates include the following:  

  * Removal of obsolete mechanisms (deborahluecken)
  * [Halogen mediated first order ozone loss is revised for all mechanisms](simple_halogen_chemistry.md)
  * [Detailed halogen and DMS chemistry with CB6r3](detailed_halogen_and_DMS_chemistry.md)
  * Updates to chlorine chemistry in CB6 and CB6r3 (PR 359, 342; deborahluecken)
  * EBI integration (PR 377, 364; bhutzell)
  
### Photolysis Rates
 * Updates to in-line photolysis (PR 334, 306, 276; bhutzell)

### Aerosol Processes
CMAQ v5.3 introduces aero7 and aero7i. Aero6, available in previous versions of CMAQ, is still available. Aero 7/7i differs from aero6 in its treatment of organic aerosol.
#### AERO7/7i
  * [Overview of AERO7/7i](aero7_overview.md)  
  * [Monoterpene SOA](monoterpene_SOA.md)  
  * [Reorganization of anthropogenic SOA species](anthro_SOA.md)  
  * [Uptake of water onto hydrophilic organic aerosol](organic_water.md)  
  
#### Other aerosol updates
  * pcSOA flag
  * getpar
  * dry deposition

### Aqueous and Heterogeneous Chemistry
 * AQCHEM-KMT2 cloud chemistry updates (PR363; kfahey92)

 
## Transport Processes
 * ACM2 updates related to z-coord (PR 354; jpleim)
 * Settling (PR 381, 378; bnmurphy)
 
## Air-Surface Exchange
 * Centralized EPIC input (PR 315; jpleim)
 * STAGE (PR 385, 375, 370, 368, 361, 345, 340; jessebash)
 * M3dry, Namelist, MEDIACONC file corrections (PR 380, 348, 379; bnmurphy)


## Emission Updates
 * [Biogenic speciation update for aero7](biogenic_apinene.md)
 * DESID (PR 383, 376, 371, 356, 355, 305; bnmurphy, cgnolte)
 * Lightning NOx (PR 324; dkang2)
 * BEIS mapping lookup, bugs in BEIS (PR 318, 309, 308, 307; bnmurphy, jessebash)

## Process Analysis
 * EBI and IRR (PR 326; bhutzell)
 * Aerosols (PR 311; bnmurphy)

## Structural Improvements
 * LOGFILE (PR 384, 382, 277; bnmurphy)
 * Moved PHOT to Sciproc (PR 352; bhutzell)
 * Output units (PR 323; bnmurphy)
 * CZANGLE centralization (PR 320; dschwede)
 * Remove SWAP and SNL Routines (PR 274, 272; dwongepa)

## Diagnostic
 * Improved vertical extraction diagnostics (PR 319; barronh)

## Tools & Utilities
 * [SpecDef aerosol updates](specdef_aero.md)
 * Updates to POST (PR 343; chogrefe)

## Instrumented Models
CMAQ-DDM and CMAQ-ISAM will be released with the final version of CMAQv5.3 in Spring 2019.


