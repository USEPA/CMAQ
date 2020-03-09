CMAQv5.3.1 Known Issues
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.3.1:


## *CMAQv5.3.1-i1:* 
Date: 12/19/2019  
Contact: Sergey Napelenok (napelenok.sergey@epa.gov)

### Description  
CMAQ-ISAM overcontributes apportionment of secondary gaseous pollutants to the ICON and BCON tags.  As the result, contributions to other tracked sources is underestimated.  

### Scope and Impact
All secondary gas-phase species (O3, NOx, etc.) in CMAQ-ISAM v5.3 and v5.3.1.

### Solution
A modification to the ISAM chemistry algorithms is currenlty in testing.  It will be released in the near future. 


## *CMAQv5.3.1-i2:* 
Date: 2019-12-31  
Contact: David Wong (Wong.David-C@epa.gov) 

### Description  
Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration) set to T, will prompt the centralized i/o module to store the start date of the file. However, this information was never passed on to the function used to extract the data from the netCDF file causing an error, as the time is not available on file. The information is now properly passed on to the variable required to extract the data from the netCDF file.

### Scope and Impact
The model will terminate execution with a time retrieval error. No model results will change.

### Solution
Replace CCTM/src/cio/centralized_io_module.F file in repository with the version located in the folder [DOCS/Known_Issues/CMAQv5.3.1-i2](CMAQv5.3.1-i2). Directions on how to replace this file in your repository can be found in [Appendix F of the CMAQ User's Guide](../Users_Guide/Appendix/CMAQ_UG_appendixF_importing_bugfixes.md). This code fix will also be included with the next minor CMAQ release. 


## *CMAQv5.3.1-i3:* 
Date: 2020-01-23  
Contact: Sergey Napelenok (napelenok.sergey@epa.gov)

### Description  
CMAQ version 5.3.1 release included an inadvertently modified version of EMIS_DEFN.F causing crashes during compiling. 

### Scope and Impact
The model did not compile with the -Disam option with errors pointing to EMIS_DEFN.F

### Solution
1. Option 1: Replace CCTM/src/emis/emis/EMIS_DEFN.F in repository with the version located in the folder [DOCS/Known_Issues/CMAQv5.3.1-i3](CMAQv5.3.1-i3). Directions on how to replace this file in your repository can be found in [Appendix F of the CMAQ User's Guide](../Users_Guide/Appendix/CMAQ_UG_appendixF_importing_bugfixes.md). 
2. Option 2: The code fix to EMIS_DEFN.F was merged into the master branch on 1/23/2020. You can download the updated v5.3.1 source code from this link: https://github.com/USEPA/CMAQ/archive/master.zip, or use git commands to update your current repository. 


## *CMAQv5.3.1-i4:* 
Date: 2020-03-09  
Contact: David Wong (Wong.David-C@epa.gov) 

### Description  
Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration) set to T, will prompt the centralized i/o module to store the start date of the file. However a memory issue related to the implementation of 2-D or 3-D Emission file is a representative day type will occur. This issue stems from the choice to psuedo-interpolate the ICs instead of extracting them directly, which is problematic if an emissions file is a 2-D or 3-D Emissions file of representative type. This is because to linearly interpolate temporally, two points are required stored as the head and tail. The head was stored correctly, but the tail was not being stored correctly and was picking up from whatever was in memory last, which usually is the 2-D/3-D Gridded Emissions file. This resulted in an error when trying to extract the data at the second point as the IC file only has data at the head.

If no emissions are present, it would pick the point from whatever file was read last whether that be a MET file, bioseason file, lightning file or IC file.

It should be noted, this issue would have not been seen if the IC file were time independent as IOAPI ignores the time input when trying to extract data from time independent files.

### Scope and Impact
The model will terminate execution with a time retrievel error for the initial conditions. No model results will change.

### Solution
Replace CCTM/src/cio/centralized_io_module.F file in repository with the version located in the folder [DOCS/Known_Issues/CMAQv5.3.1-i2](CMAQv5.3.1-i2). Directions on how to replace this file in your repository can be found in [Appendix F of the CMAQ User's Guide](../Users_Guide/Appendix/CMAQ_UG_appendixF_importing_bugfixes.md). This code fix will also be included with the next minor CMAQ release.

## *CMAQv5.3.1-i5:* 
Date: 2020-03-09  
Contact: David Wong (Wong.David-C@epa.gov) 

### Description  
The inability to run with the EMIS_SYM_DATE flag due inconsistent implementation between cio and the emissions modules.

### Scope and Impact
The model will terminate execution with a time retrievel error for the emission stream.

### Solution
Do not use the environmental variable EMIS_SYM_DATE and a code fix will also be included with the next minor CMAQ release.

## *CMAQv5.3.1-i6:* 
Date: 2020-03-09  
Contact: Christian Hogrefe (hogrefe.christian@epa.gov)

### Description  
In CMAQv5.3.1, there is an inconsistency in the naming convention of the bi-directional NH3 flux components contained in the CCTM_DRYDEP file between the STAGE and M3DRY dry deposition options. When running CMAQ with the NH3 bi-directional flux option enabled (CTM_ABFLUX set to Y in the run script), the model calculates and stores three NH3 flux components in the CCTM_DRYDEP file: the downward deposition flux, the upward emissions flux, and the net flux representing the difference between the deposition and emissions fluxes. The table below shows how these flux components are named in the CCTM_DRYDEP file depending on whether the STAGE or M3DRY dry deposition option is used. 
||**STAGE**|**M3DRY**|
|:--------------:|:--------:|:--------:|
|Downward Deposition Flux (always positive)  |	NH3	      | NH3_DDEP|
|Upward Emissions Flux (always positive)	   |  NH3_Emis	| NH3_EMIS|
|Net Flux (positive if downward and negative if upward)  | NH3_Flux |	NH3|

When using the STAGE dry deposition option with bi-directional NH3 flux enabled, the CCTM_DRYDEP file contains additional diagnostic deposition values (NH3_Stom, NH3_Cut, NH3_Soil, NH3_Ag, NH3_Nat, and NH3_Wat) that are described in the [CMAQv5.3.1 Release Notes]( ../Release_Notes/CMAQv5.3.1_bugfixes.md#5-stage).

Note that when the model is run without the bi-directional NH3 flux option enabled (CTM_ABFLUX set to N in the run script), the variable NH3 in the CCTM_DRYDEP file represents the unidirectional NH3 dry deposition flux in both STAGE and M3DRY.

### Scope and Impact
These differences in naming conventions need to be considered when performing post-processing of CCTM_DRYDEP outputs. For example, the [“combine”](../../POST/combine/README.md) example species definition file for deposition fields utilizes the CCTM_DRYDEP variable “NH3” when defining variables “DDEP_NH3” and “DDEP_NHX” (see for example see [the CB6r3_AE7 species definition file under CCTM/src/MECHS/cb6r3_ae7_aq](../../CCTM/src/MECHS/cb6r3_ae7_aq/SpecDef_Dep_cb6r3_ae7_aq.txt)). 

### Solution
If the intent of these derived fields is to represent downward deposition fluxes, no changes are needed if CMAQ was run without the bi-directional NH3 flux option with either STAGE or M3DRY, or if CMAQ was run with the bi-directional NH3 flux option and the STAGE dry deposition option. However, if CMAQ was run with the bi-directional NH3 flux option and the M3DRY dry deposition option, these definitions in the species definition file should be changed from “NH3” to “NH3_DDEP”.
