CMAQv5.2 Release Notes - October 2016
=====================================

Refer to the CMAQv5.0 [Technical Documentation](http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQ_version_5.0_(February_2012_release)_Technical_Documentation) for information on the technical features of this version.  

The Community Multiscale Air Quality (CMAQ) Model version 5.2 is major update to CMAQ that includes several changes to the science algorithms in the base model.  CMAQ v5.2 was developed by the U.S. EPA with contributions from other research partners. Summarized below are the main enhancements to the modeling system since the previous release, CMAQ v5.2. 

# Base Documentation  
[Building and running CMAQv5.1](http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.1_Readme_file)  
[Building and running WRF-CMAQ Two Way Model](http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.1_Two-way_model_release_notes)

# Patches 
###    Patch 1 

-----
# Release Notes  

## Chemistry 
### Photochemistry
  * CB6r3 details
  * [Brute force no chemistry capacity](Brute_force_no_chemistry_capacity.md)
  * [RXNS_DATA_MODULEs comply to FORTRAN 132 column limit](MECHS_RXNS_DATA_MODULEs_comply_to_FORTRAN_132_column_limit.md)
  * [Chemistry EBI solvers corrected in error messages](GAS_EBI_solvers_corrected_in_error_messages.md)
### Photolysis Rates
  * [Updates to the OMI data file](In-Line_Photolysis_Updates_to_the_OMI_data_file.md)
  * [In-line Photolysis CLOUD_OPTICS.F fixed for possible floating point error](In-line_Photolysis_CLOUD_OPTICS.F_fixed_for_possible_floating_point_error.md)

### Aerosol Processes
  * Semivolatile POA and pcSOA implementation
  * New Organic Properties
  * [Transmission Factors for PM<sub>1</sub> and AMS size-dependent collection have been added and are output in the diagnostic routine](Aerosol_Transmission_Factors.md)
  * New flexibility in Aero_Data table
  * Consistent treatment of M2wet
  * SOA Bisection: numerical considerations for choice of upper bound
  * [Reduce underflow errors in aerosol physics and chemistry](Reduce_underflow_errors_in_aerosol_physics_and_chemistry.md)

### Aqueous and Heterogeneous Chemistry
#### Aqueous aerosol chemistry
  * Adoption of tracer species into acm_cld standard code

## Transport Processes 
  * Monin-Obukhov fix

## Air-Surface Exchange 
### Bi-Directional Exchange Updates
  

### Windblown Dust Emissions
  * New Dust Module

### Dry Deposition
  * [The deposition parameters of H2O2, HACET, and Organic Nitrates have been updated.](Gas-Phase_Dep_H2O2_HACET_OrgNtr_s07tic_Species.md)
  * [The cuticular resistance resistance parameter of Ozone has been updated](O3_Cuticular_Resistance.md)

## VOC Emission Updates
  * Multiple emissions files for fire sources capability

## Structural Updates

## Tools & Utilities
  * SpecDef files and SpecDef_Dep files

## Two-way Coupled WRF-CMAQ

## Instrumented Models
The instrumented versions of CMAQv5.2 (e.g. CMAQ-DDM) will be release at a later date.

## Community Contributions
None

-----
# Release Testing

The CMAQv5.2 release package was tested with the Portland Group 15.7, Intel 16.0, and GNU Fortran 4.8.1 compilers.  In addition to different serial and parallel configurations, the release was tested under different science configurations. See the details of the [http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.1_Readme_file#Testing_Procedures CMAQv5.1 Release Test Results], including run times for different compiler configurations.
