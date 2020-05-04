CMAQv5.3 Known Issues
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.3:

## *CMAQv5.3-i1:* 
Date: 2019-09-05
Contact: Kristen Foley (foley.kristen@epa.gov) 

### Description  
Setting CTM_WVEL, a run time science option to write out the vertical velocity component to the concentration file, to N. The default setting, currently, is listed as Y in all runscripts within the repository. If the CTM_WVEL science option is set to N, the model immediately crashes. This is because the array that stores the vertical velocity component for writing to the concentration file is never allocated and is being used to calculate the average vertical velocity to be written to the average concentration file.

### Scope and Impact
The model will terminate execution with a segmentation fault.

### Solution
Users should leave CTM_WVEL set to Y in all runscripts. A code fix will be included with the next minor CMAQ release.

## *CMAQv5.3-i2:* 
Date: 2019-09-11
Contact: David Wong (wong.david-c@epa.gov) 

### Description  
The current implementation of the Centralized Input/Output Module only supports BNDY_CONC_1 files that have a time step of 1 hour. Previous versions of CMAQ were able to process boundary condition files with time steps greater than 1 hour. 

### Scope and Impact
If providing a boundary condition file with a time step greater than 1 hour, the model will terminate execution with an error message that the BNDY_CONC_1 cannot be read for the requested time step.

### Solution
Replace CCTM/src/cio/centralized_io_module.F file in repository with the version located in the folder DOCS/Known_Issues/CMAQv5.3-i2.
This code fix will also be included with the next minor CMAQ release.

## *CMAQv5.3-i3:* 
Date: 2020-05-04  
Contact: Christian Hogrefe (hogrefe.christian@epa.gov)

### Description  
In CMAQv5.3, there is an inconsistency in the naming convention of the bi-directional NH3 flux components contained in the CCTM_DRYDEP file between the STAGE and M3DRY dry deposition options. When running CMAQ with the NH3 bi-directional flux option enabled (CTM_ABFLUX set to Y in the run script), the model calculates and stores three NH3 flux components in the CCTM_DRYDEP file: the downward deposition flux, the upward emissions flux, and the net flux representing the difference between the deposition and emissions fluxes. The table below shows how these flux components are named in the CCTM_DRYDEP file depending on whether the STAGE or M3DRY dry deposition option is used. 
||**STAGE**|**M3DRY**|
|:--------------:|:--------:|:--------:|
|Downward Deposition Flux (always positive)  |	NH3_Dep     | NH3_DDEP|
|Upward Emissions Flux (always positive)	   |  NH3_Emis	| NH3_EMIS|
|Net Flux (positive if downward and negative if upward)  | NH3 |	NH3|

When using the STAGE dry deposition option with bi-directional NH3 flux enabled, the CCTM_DRYDEP file contains additional diagnostic deposition values (NH3_Stom, NH3_Cut, NH3_Soil, NH3_Ag, NH3_Nat, and NH3_Wat).

Note that when the model is run without the bi-directional NH3 flux option enabled (CTM_ABFLUX set to N in the run script), the variable NH3 in the CCTM_DRYDEP file represents the unidirectional NH3 dry deposition flux in both STAGE and M3DRY.

### Scope and Impact
These differences in naming conventions need to be considered when performing post-processing of CCTM_DRYDEP outputs. For example, the [“combine”](../../POST/combine/README.md) example species definition file for deposition fields utilizes the CCTM_DRYDEP variable “NH3” when defining variables “DDEP_NH3” and “DDEP_NHX” (see for example see [the CB6r3_AE7 species definition file under CCTM/src/MECHS/cb6r3_ae7_aq](../../CCTM/src/MECHS/cb6r3_ae7_aq/SpecDef_Dep_cb6r3_ae7_aq.txt)). 

### Solution
If the intent of these derived fields is to represent downward deposition fluxes, no changes are needed if CMAQ was run without the bi-directional NH3 flux option with either STAGE or M3DRY. However, if CMAQ was run with the bi-directional NH3 flux option and the M3DRY dry deposition option, these definitions in the species definition file should be changed from “NH3” to “NH3_DDEP”. If CMAQ was run with the bi-directional NH3 flux option and the STAGE dry deposition option, these definitions in the species definition file should be changed from “NH3” to “NH3_Dep”.

