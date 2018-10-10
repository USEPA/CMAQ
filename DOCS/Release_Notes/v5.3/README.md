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

### Photolysis Rates

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

### Lightning Interactions
 
## Transport Processes

## Air-Surface Exchange
### Windblown Dust Emissions

### Deposition

## Emission Updates
 * [Biogenic speciation update for aero7](biogenic_apinene.md)

## Process Analysis

## Tools & Utilities
 * [SpecDef aerosol updates](specdef_aero.md)

## Instrumented Models

## Community Contributions

-----
# Release Testing

