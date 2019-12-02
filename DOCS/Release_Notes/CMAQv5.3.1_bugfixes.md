# CMAQv5.3.1 Bugfixes

## 1. *CTM_WVEL* run script option 
[Ben Murphy](mailto:Murphy.Ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Setting CTM_WVEL, a run time science option to write out the vertical velocity component to the concentration file, to N. 
The default setting, currently, is listed as Y in all runscripts within the repository. If the CTM_WVEL science option is set to N, 
the model immediately crashes. This is because the array that stores the vertical velocity component for writing to the concentration 
file is never allocated and is being used to calculate the average vertical velocity to be written to the average concentration file.

### Solution in CMAQv5.3.1
The array that stores the vertical velocity component for writing is now properly allocated. The model will no longer terminate execution with a segmentation fault. The updates will also allow users to flexibly toggle CTM_WVEL on/off independently of the CONC_SPCS_LIST. **Note: If the user decides to write this diagnostic variable out, the variable will be reported to both the conc and aconc species.**

### Files Affected 
CCTM/src/driver/AVG_CONC.F

CCTM/src/driver/STD_CONC.F

CCTM/src/driver/WVEL_DEFN.F

CCTM/src/driver/driver.F

CCTM/src/driver/wr_aconc.F

CCTM/src/driver/wr_conc.F

CCTM/src/driver/wr_init.F

CCTM/src/init/opaconc.F

CCTM/src/init/opconc.F 

CCTM/src/vadv/local_cons/zadvyppm.F

CCTM/src/vadv/wrf_cons/zadvppmwrf.F

CCTM/scripts/run_cctm_2010_4CALIF1.csh (Moved CTM_WVEL to diagnostic outputs) 

CCTM/scripts/run_cctm_2011_12US1.csh (Moved CTM_WVEL to diagnostic outputs)
 
CCTM/scripts/run_cctm_2014_12US1.csh (Moved CTM_WVEL to diagnostic outputs) 

CCTM/scripts/run_cctm_2015_HEMI.csh (Moved CTM_WVEL to diagnostic outputs) 

CCTM/scripts/run_cctm_2016_12US1.csh (Moved CTM_WVEL to diagnostic outputs)

CCTM/scripts/run_cctm_Bench_2011_12SE1.csh (Moved CTM_WVEL to diagnostic outputs)

CCTM/scripts/run_cctm_Bench_2016_12SE1.csh (Moved CTM_WVEL to diagnostic outputs)

## 2. Error reading multiple Region Files for use in DESID 
[Ben Murphy](mailto:Murphy.Ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
When multiple region files were read in, the model crashed with segmentaion fault.

### Solution in CMAQv5.3.1 
In the initialization routine for region masks, the population of the region masks should to occur outside of the file loop rather than inside. Essentially, the arrays storing the region values were being incremented incorrectly and beyond the appropriate length set by the allocation commands. 

### Files Affected 
centralized_io_module.F

## 3. Diagnostic File for Lightning NOx
[Daiwen Kang](mailto:Kang.Daiwen@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The 3D diagnostic files are meant to provide the lightning NO emissions at each vertical grid cell. The current implementation has mistakenly accumulated the emissions from lower layers, i.e, it is correct for the lowest layer (Layer 1), but the values at Layer 2 is the sum of Layer 1 and Layer 2, and the values at Layer 3, is the sum of Layer 1 through Layer 3, and so on, so forth. 
### Solution in CMAQv5.3.1
We have now revoved the accumulation loop and output emissions at each layer correctly.
### Files Affected 
CCTM/emis/emis/LTNG_DEFN.F

## 4. Updates to CCTM Centralized I/O (CIO) module 
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The current implementation of the Centralized Input/Output Module was encoded based on three assumptions: 1. non-metrology input data was expected to be at the same frequency as the output [tstep](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#timestep-configuration) (a runscript environment variable the user set). 2. all gridded emissions have the same number of layers. 3. the CCTM run starts at the zeroth hour.

### Solution in CMAQv5.3.1
Issue #1: A new algorithm was developed to keep track of the time step from each input file and to allow the model to write data out at the pre-defined frequency in the run script. The algorithm was also implemented to store the start date and start time of each file, incase the user had emissions input data that used representative days. A new environmental variable was also re-introduced to keep track of which emissions files were representative days and which are not. **Note: this algorithm only allows a maximum of 500 files to be opened.**

Issue #2: A new array was introduced to store the number of layers in each emisison file. Using this new information, the buffer array storing the emissions data being read in was re-allocated to be the no greater than the size of the initial condition file, but no smaller than the size of the largest emissions file. In addition, each gridded emission file was extracted using the newly introduce array that stored the number of layers in each emission file. **Note: this algorithm does not limit the extraction of data greater than the model top (i.e. files that have nlays greater than the model top). However doing so will cause a segmentation fault with memory issues as what is allocated will not match what is being extracted.**

Issue #3: The date advancement is now properly updated, i.e. performs the local time update only when the model date is updated. An exit call is also implemented to stop the model when an improper interpolation takes place. During the exit call, the following information: the interpolation date, time and bounds, will be sent to the [processor log files](../Users_Guide/CMAQ_UG_ch05_running_a_simulation.md#571-cctm-logfiles) for further debugging. 

### Files Affected 
CCTM/src/cio/centralized_io_module.F

CCTM/src/cio/centralized_io_util_module.F 

CCTM/src/emis/emis/EMIS_DEFN.F 

CCTM/src/emis/emis/PT3D_DEFN.F

CCTM/src/driver/advstep.F

CCTM/src/phot/inline/concld_prop_acm.F

CCTM/src/cloud/acm_ae7_kmt2/rescld.F

CCTM/src/cloud/acm_ae7_kmt2/convcld_acm.F

CCTM/src/cloud/acm_ae6_mp/rescld.F

CCTM/src/cloud/acm_ae6_mp/convcld_acm.F

CCTM/src/cloud/acm_ae6/rescld.F

CCTM/src/cloud/acm_ae6/convcld_acm.F

CCTM/src/emis/emis/EMIS_VARS.F

CCTM/src/emis/emis/STK_EMIS.F

CCTM/src/emis/emis/opemis.F

CCTM/src/util/util/RUNTIME_VARS.F

CCTM/scripts/run_cctm_2010_4CALIF1.csh

CCTM/scripts/run_cctm_2011_12US1.csh

CCTM/scripts/run_cctm_2014_12US1.csh

CCTM/scripts/run_cctm_2015_HEMI.csh

CCTM/scripts/run_cctm_2016_12US1.csh

CCTM/scripts/run_cctm_Bench_2011_12SE1.csh

CCTM/scripts/run_cctm_Bench_2016_12SE1.csh

## 5. STAGE
[Jesse Bash](mailto:Bash.Jesse@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Two issues were identified in CMAQv5.3 NH3 output when running with bidirectional exchange. 
1) The deposition to surface waters was omitted from the diagnostic NH3_Dep output. 
2) Modifications were needed to the deposition species definition file for post processing to accurate capture nitrogen deposition.

### Solution in CMAQv5.3.1
The aggregation of fluxes from land use types was simplified and a model conditional statement was removed to correct the omission of NH3 deposition to surface waters in the diagnostic deposition output. Note, this bug did not impact the diagnostic land use specific deposition totals output when *setenv CTM_MOSAIC = T*. The diagnostic deposition output was remapped to allow users to use the standard deposition species definition file (*SpecDef_Dep*) distributed with the model. The new mapped diagnostic species in the DRYDEP file are:

NH3 – NH<sub>3</sub> dry deposition (positive values are deposition)

NH3_Flux – NH<sub>3</sub> surface flux (positive values are deposition and negative values are emission)

NH3_Wat - NH<sub>3</sub> flux over water bodies (positive values are emissions and negative values are deposition)

NH3_Ag - NH<sub>3</sub> flux over agriculture land use (positive values are emissions and negative values are deposition)

NH3_Nat - NH<sub>3</sub> flux over non-agriculture land use (positive values are emissions and negative values are deposition)

NH3_Emis – Diagnostic grid cell NH<sub>3</sub> emissions from fertilizers and biogenic sources (positive values are emissions)

NH3_Soil - NH<sub>3</sub> flux from soil pathways (positive values are emissions and negative values are deposition)

NH3_Stom - NH<sub>3</sub> flux from leaf stomatal pathways (positive values are emissions and negative values are deposition)

NH3_Cut - NH<sub>3</sub> flux from leaf cuticular pathways (positive values are emissions and negative values are deposition)

### Files Affected 
STAGE_MOD.F

Vdiffproc.F

opddep.F


## 6. ISAM
[Sergey Napelenok](mailto:Napelenok.Sergey@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Specifying “PM25_IONS” as a TAG CLASS resulted in unstable attribution output for ACLI and ACLJ concentration and deposition.

### Solution in CMAQv5.3.1
ACLI and ACLJ were removed from the “PM25_IONS” TAG CLASS and a new class was added called “CHLORINE.”  This TAG CLASS also includes HCL gas in addition to ACLI and ACLJ, and the algorithms now include partitioning calculations.

### Files Affected 
CCTM/scripts/isam_control.txt

CCTM/src/isam/SA_DEFN.F

CCTM/src/isam/SA_WRAP_AE.F


## 7. Coupled WRF-CMAQ Model
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
1. Setting [CTM_BIOGEMIS](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#science-options) to Y in the WRF-CMAQ model did not correctly produce the SOILOUT file after a simulation period was completed. This led to a crash when restarting the model the next day with inilization from the previous days run. This issue was traced back to the inline biogenics algorithm which only writes the SOILOUT file if the model has reached its run length, a runscript environmental variable (CTM_RUNLEN). However, in the WRF-CMAQ Model this runscript environmental variable was not being read in so the default value of 48 hours defined in RUNTIME_VARS.F was used. Hence SOILOUT will only be produced at the 48th hour.

2. Setting [CTM_WBDUST](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#science-options) to Y in the WRF-CMAQ model and setting [CTM_WBDUST](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#science-options) to "unknown" results in a crash. This crash is a result of the bounds of extraction being incorrect. 

### Solution in CMAQv5.3.1
Issue #1: The WRF-CMAQ model was updated to properly read the environmental variable CTM_RUNLEN in RUNTIME_VARS.F.

Issue #2: Adding variables to store the calculation of the bounds for the land-use database from the appropriate file whether it be from aqprep (mcip counterpart) or from BELD data.

### Files Affected 
CCTM/src/cio/centralized_io_module.F, CCTM/src/util/util/RUNTIME_VARS.F

