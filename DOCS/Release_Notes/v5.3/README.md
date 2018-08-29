CMAQv5.3 Release Notes 
=====================================

# Getting Started with CMAQ  
[Building and running CMAQv5.2.1](../../../DOCS/User_Manual/CMAQ_OGD_ch05_sys_req.md)  
[Building and running WRF-CMAQ Two Way Model](Two_Way_Coupled_WRF-CMAQ.md)

# Summary of CMAQv5.3 Updates

The Community Multiscale Air Quality (CMAQ) Model version 5.3 is a major update to CMAQ that includes several changes to the science algorithms in the base model.  CMAQ v5.3 was developed by the U.S. EPA with contributions from other research partners. Summarized below are the main enhancements to the modeling system since the previous release, CMAQ v5.2.1.

<a id="chemistry"></a>
## Chemistry
### Photochemistry
There are 10 unique gas-phase chemical mechanisms in CMAQv5.2. These are all located in the MECHS/ folder and may be invoked when building the model and Makefile. Variations of Carbon Bond 5 (CB05), Carbon Bond 6 (CB6), RACM2, and SAPRC07 are all available. Specific science updates include the following:  
  * [CMAQv5.2 Photochemical Mechanisms](CMAQv5.2_Mechanisms.md)

Structural updates to the chemistry module in general include:  
  * [Brute force no chemistry capacity](Brute_force_no_chemistry_capacity.md)

### Photolysis Rates
Updates to the treatment of photolysis rates online include:
  * [Updates to the OMI data file](In-Line_Photolysis_Updates_to_the_OMI_data_file.md)

### Aerosol Processes
The aerosol module has undergone significant changes that affect both its structure and the science it represents. Major science updates include the following:
  * [Semivolatile POA and pcSOA implementation](SemiVolPOA_pcSOA.md)

The following structural updates ensure consistent treatment of aerosols in CMAQv5.2 and flexibility as the code is developed in the future:
  * [Consolidation of aero module and increased flexibility in AERO_DATA table](aero6_6i_6mp_consolidation.md)

The following updates affect how aerosol concentrations from CMAQ output may be interpreted when evaluated against observation data:
  * [PM Diagnsotic files have been enhanced to provide more detailed and robust information about CMAQ particulate properties](PM_Diagnostic_Files.md)

These minor updates/bug fixes were necessary:
  * [Reduce underflow errors in aerosol physics and chemistry](Reduce_underflow_errors_in_aerosol_physics_and_chemistry.md)

### Aqueous and Heterogeneous Chemistry
  * [The subgrid non-precipitating shallow convective cloud treatment was updated to have scale-independent critical RH values consistent with the WRF meteorological model](Subgrid_Shallow_Conv_Cloud.md)

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

The following minor update repairs an error in the wet deposition calculation:
  * [Correct an intent declaration that resulted in errors when th emodel was not run at one hour intervals](Wet_Dep_Update.md)

<a id="emissions"></a>
## Emission Updates
CMAQ can now read multiple files for fire point sources into the model and apply them to the bulk species emissions rates:
  * [Multiple fire inputs capability](Multiple_Fire_Inputs.md)

Improvements to speciation and error checking:  
  * [Online speciation of biogenic VOCs from BEIS is now available and customizable](GSPRO.md)



<a id="procan"></a>
## Process Analysis
CCTM can now do process analysis with Integrated Process Rates (IPR) and/or Integrated Reactions Rates (IRR) as a run-time option.
  * [Optional inline IPR and IRR process analysis](inline_procan.md)

## Tools & Utilities
  * [Update to run and build scripts for the CCTM and all pre- and post-processing tools](runscripts.md)

## Instrumented Models
The instrumented versions of CMAQv5.2 (e.g. CMAQ-DDM) will be release at a later date.

## Community Contributions
New code to allow for multiple fire emission input files was contributed by Dr. Yongtao Hu from Georgia Tech University.

-----
# Release Testing

