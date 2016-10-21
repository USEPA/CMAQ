CMAQv5.2 Release Notes - October 2016
=====================================

Refer to the CMAQv5.0 [Technical Documentation](http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQ_version_5.0_(February_2012_release)_Technical_Documentation) for information on the technical features of this version.  

The Community Multiscale Air Quality (CMAQ) Model version 5.2 is a major update to CMAQ that includes several changes to the science algorithms in the base model.  CMAQ v5.2 was developed by the U.S. EPA with contributions from other research partners. Summarized below are the main enhancements to the modeling system since the previous release, CMAQ v5.1. 

# Base Documentation  
[Building and running CMAQv5.2](http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.1_Readme_file)  
[Building and running WRF-CMAQ Two Way Model](http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.1_Two-way_model_release_notes)

-----
# Summary of CMAQv5.2 Updates

## Chemistry 
### Photochemistry
There are 11 unique gas-phase chemical mechanisms in CMAQv5.2. These are all located in the MECHS/ folder and may be invoked when building the model and Makefile. Variations of Carbon Bond 5 (CB05), Carbon Bond 6 (CB6), RACM2, and SAPRC07 are all available. Specific science updates include the following:  
  * [Implementation of CB6r3](CB6_release_notes.md)
  * [Implementation of CB05eh51: Bromine and Iodine Chemistry](Halogen_Chemistry.md)  

Structural updates to the chemistry module in general include:  
  * [Brute force no chemistry capacity](Brute_force_no_chemistry_capacity.md)
  * [RXNS_DATA modules now comply to FORTRAN 132 column limit](MECHS_RXNS_DATA_MODULEs_comply_to_FORTRAN_132_column_limit.md)
  * [Chemistry EBI solvers corrected in error messages](GAS_EBI_solvers_corrected_in_error_messages.md)

### Photolysis Rates
Updates to the treatment of photolysis rates online include:
  * [Updates to the OMI data file](In-Line_Photolysis_Updates_to_the_OMI_data_file.md)
  * [In-line Photolysis CLOUD_OPTICS.F fixed for possible floating point error](In-line_Photolysis_CLOUD_OPTICS.F_fixed_for_possible_floating_point_error.md)

### Aerosol Processes
The aerosol module has undergone significant changes that affect both its structure and the science it represents. Major science updates include the following:
  * [Semivolatile POA and pcSOA implementation](SemiVolPOA_pcSOA.md)
  * [New Organic Compound Properties](SOA_properties.md)
  * [Glyoxal and methylglyoxal uptake onto aqueous particles](gly_mgly_soa_update.md)
  * [IEPOX organosulfate formation rate constant update](iepoxos_rateconstant.md)
  * [Update to Speciation of coarse-mode aerosol](Coarse_Aerosol_Speciation.md)
  
The following structural updates ensure consistent treatment of aerosols in CMAQv5.2 and flexibility as the code is developed in the future:
  * [New flexibility in Aero_Data table](aero6_6i_6mp_consolidation.md)
  * [Consistent treatment of aerosol water contribution to second and third aerosol moments](Aerosol_Moment_Consistency.md)
  * [Consolidation of aero module](aero6_6i_6mp_consolidation.md)
  
The following updates affect application of CMAQ aerosol components to observation data:
  * [PM Diagnsotic files have been enhanced to provide more detailed and robust information about CMAQ particulate properties](PM_Diagnostic_Files.md)
  * [Transmission Factors for PM<sub>1</sub> and AMS size-dependent collection have been added and are output in the diagnostic routine](Aerosol_Transmission_Factors.md)
  * [The estimation of AOD and visibility have been updated with the recent version of the IMPROVE equations](AOD_Visibility.md)
  
These minor updates/bug fixes were necessary:
  * [Reduce underflow errors in aerosol physics and chemistry](Reduce_underflow_errors_in_aerosol_physics_and_chemistry.md)
  * [OA Bisection upper bound adjustment](OA_bisection_update.md)
  * [Acidity bug fix and IEPOX SOA update](AH3OPJ_IEPOX_update.md)
  
### Aqueous and Heterogeneous Chemistry
  * [The subgrid non-precipitating shallow convective cloud treatment was updated to have scale-independent critical RH values consistent with the WRF meteorological model](Subgrid_Shallow_Conv_Cloud.md)
  * [AQCHEM-KMT(I) has been updated to be consistent with updates to the aerosol module](AQCHEM-KMT.md)
  * [Tracer species options have been implemented into the standard acm_cld module so that the user can choose to invoke them without negotiating which acm_cld module to use](aero6_6i_6mp_consolidation.md) 
  
### Lightning Interactions
  * [Lightning NO<sub>x</sub> generation has been updated to leverage existing lightning fields for hindsight cases and an improved parameterization for future cases](Lightning_NOx.md)
  * [Fix hard-coded time-step for lightning NO<sub>x</sub> generation](Wet_Dep_Update.md)

## Transport Processes
  * [The scaling of ozone in the upper troposphere has been corrected using an approahc relying on potential voriticity](Potential_Vorticity_Scaling.md)

## Air-Surface Exchange 
### Windblown Dust Emissions
  * [A major improvement was made to the windblown dust generation parameterization](Windblown_Dust_Emis.md)

### Deposition
Important parameters for soluble gases have been updated to better represent resistances to dry deposition:
  * [Update to the deposition parameters of H2O2, HACET, and Organic Nitrates.](Gas-Phase_Dep_H2O2_HACET_OrgNtr_s07tic_Species.md)
  * [Update to the cuticular resistance resistance parameter of Ozone](O3_Cuticular_Resistance.md)
 
The following minor update repairs an error in the wet deposition calculation:
  * [Correct an intent declaration that resulted in errors when th emodel was not run at one hour intervals](Wet_Dep_Update.md)
  
## Emission Updates
CMAQ can now read multiple files for fire point sources into the model and apply them to the bulk species emissions rates.

## Process Analysis
CCTM can now do process analysis with Integrated Process Rates (IPR) and/or Integrated Reactions Rates (IRR) as a run-time option.
  * [Optional inline IPR and IRR process analysis](inline_procan.md)

## Tools & Utilities
  * [Distribution of SpecDef files and SpecDef_Dep files into mechanism sub-modules](Misc_Aerosol_Operation_Updates.md)

## Two-way Coupled WRF-CMAQ

## Instrumented Models
The instrumented versions of CMAQv5.2 (e.g. CMAQ-DDM) will be release at a later date.

## Community Contributions
None

-----
# Release Testing
[we need to get testing going]

The CMAQv5.2 release package was tested with the Portland Group 15.7, Intel 16.0, and GNU Fortran 4.8.1 compilers.  In addition to different serial and parallel configurations, the release was tested under different science configurations. See the details of the [http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.1_Readme_file#Testing_Procedures CMAQv5.1 Release Test Results], including run times for different compiler configurations.
