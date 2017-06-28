CMAQv5.2 Release Notes 
=====================================

The Community Multiscale Air Quality (CMAQ) Model version 5.2 is a major update to CMAQ that includes several changes to the science algorithms in the base model.  CMAQ v5.2 was developed by the U.S. EPA with contributions from other research partners. Summarized below are the main enhancements to the modeling system since the previous release, CMAQ v5.1.

# Getting Started with CMAQ  
[Building and running CMAQv5.2](../User_Manual/CMAQ_OGD_ch05_sys_req.md)  
[Building and running WRF-CMAQ Two Way Model](Two_Way_Coupled_WRF-CMAQ.md)

-----
# Summary of CMAQv5.2 Updates

<a id="chemistry"></a>
## Chemistry
### Photochemistry
There are 10 unique gas-phase chemical mechanisms in CMAQv5.2. These are all located in the MECHS/ folder and may be invoked when building the model and Makefile. Variations of Carbon Bond 5 (CB05), Carbon Bond 6 (CB6), RACM2, and SAPRC07 are all available. Specific science updates include the following:  
  * [CMAQv5.2 Photochemical Mechanisms](CMAQv5.2_Mechanisms.md)
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
  * [Consolidation of aero module and increased flexibility in AERO_DATA table](aero6_6i_6mp_consolidation.md)
  * [Consistent treatment of aerosol water contribution to second and third aerosol moments](Aerosol_Moment_Consistency.md)
  * [Check for feasible aerosol size distirbution properties from initial and boundary conditions](IC_BC_Aerosol_Size_Conditioner.md)

The following updates affect how aerosol concentrations from CMAQ output may be interpreted when evaluated against observation data:
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
  * [The previously separate aerosol modules (aero6, aero6i and aero6i_mp) have been consolidated so that only one module is needed](aero6_6i_6mp_consolidation.md)

### Lightning Interactions
  * [Lightning NO<sub>x</sub> generation has been updated to leverage existing lightning fields for hindsight cases and an improved parameterization for future cases](Lightning_NOx.md)
 
<a id="transport"></a>
## Transport Processes
  * [The scaling of ozone in the upper troposphere has been corrected using an approach relying on potential vorticity](Potential_Vorticity_Scaling.md)

<a id="exchange"></a>
## Air-Surface Exchange
### Windblown Dust Emissions
  * [A major improvement was made to the windblown dust generation parameterization](Windblown_Dust_Emis.md)

### Deposition
Important parameters for soluble gases have been updated to better represent resistances to dry deposition:
  * [Update to the deposition parameters of H2O2, HACET, and Organic Nitrates.](Gas-Phase_Dep_H2O2_HACET_OrgNtr_s07tic_Species.md)
  * [Update to the cuticular resistance resistance parameter of Ozone](O3_Cuticular_Resistance.md)

The following minor update repairs an error in the wet deposition calculation:
  * [Correct an intent declaration that resulted in errors when th emodel was not run at one hour intervals](Wet_Dep_Update.md)

<a id="emissions"></a>
## Emission Updates
CMAQ can now read multiple files for fire point sources into the model and apply them to the bulk species emissions rates:
  * [Multiple fire inputs capability](Multiple_Fire_Inputs.md)

Improvements to speciation and error checking:  
  * [Online speciation of biogenic VOCs from BEIS is now available and customizable](GSPRO.md)
  * [Mapping of emissions input species names to internal CMAQ surrogates is checked and will produce warnings if inconsistent](Emissions_Check.md)



<a id="procan"></a>
## Process Analysis
CCTM can now do process analysis with Integrated Process Rates (IPR) and/or Integrated Reactions Rates (IRR) as a run-time option.
  * [Optional inline IPR and IRR process analysis](inline_procan.md)

## Tools & Utilities
  * [Update to run and build scripts for the CCTM and all pre- and post-processing tools](runscripts.md)
  * [Distribution of updated SpecDef files and SpecDef_Dep files in mechanism sub-modules](Misc_Aerosol_Operation_Updates.md)
  * [Updates to post-processing tools bldoverlay, combine, hr2day, sitecmp, sitecmp_dailyo3, writesite](Update_POST.md)
  * [Update to logic for mapping initial and boundary condition surrogates to internal CMAQ species](ICBC_Surrogate_Preference.md)

## Instrumented Models
The instrumented versions of CMAQv5.2 (e.g. CMAQ-DDM) will be release at a later date.

## Community Contributions
New code to allow for multiple fire emission input files was contributed by Dr. Yongtao Hu from Georgia Tech University.

-----
# Release Testing

The CMAQv5.2 release package was tested with the Portland Group 15.7, Intel 15.0, and GNU Fortran 4.8.1 compilers.  In addition to different serial and parallel configurations, the release was tested under different science configurations. See the details of the [CMAQv5.2Beta Test Results (Feb 2017)](https://www.airqualitymodeling.org/index.php/CMAQ_version_5.2beta_(February_2017_release)_Technical_Documentation) and the final [CMAQv5.2 Release Results (June 2017)](https://www.airqualitymodeling.org/index.php/CMAQ_version_5.2_(June_2017_release)_Technical_Documentation#Release_Testing).
