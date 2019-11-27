# CMAQv5.3.1 and MCIPv5.1 Bugfixes

## 1. *CTM_WVEL* run script option 
[Ben Murphy](mailto:Murphy.Ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Setting CTM_WVEL, a run time science option to write out the vertical velocity component to the concentration file, to N. 
The default setting, currently, is listed as Y in all runscripts within the repository. If the CTM_WVEL science option is set to N, 
the model immediately crashes. This is because the array that stores the vertical velocity component for writing to the concentration 
file is never allocated and is being used to calculate the average vertical velocity to be written to the average concentration file.

### Solution in CMAQv5.3.1
The array that stores the vertical velocity component for writing is now properly allocated. The model will no longer terminate execution with a segmentation fault. The updates will also allow users to flexibly toggle CTM_WVEL on/off independently of the CONC_SPCS_LIST. Note: If the user decides to write this diagnostic variable out, the variable will be reported to both the conc and aconc species.

### Files Affected 
CCTM/src/driver/AVG_CONC.F, CCTM/src/driver/STD_CONC.F, CCTM/src/driver/WVEL_DEFN.F, CCTM/src/driver/driver.F, CCTM/src/driver/wr_aconc.F, CCTM/src/driver/wr_conc.F, CCTM/src/driver/wr_init.F, CCTM/src/init/opaconc.F, CCTM/src/init/opconc.F, CCTM/src/vadv/local_cons/zadvyppm.F, CCTM/src/vadv/wrf_cons/zadvppmwrf.F, CCTM/scripts/run_cctm_2010_4CALIF1.csh (Moved CTM_WVEL to diagnostic outputs), CCTM/scripts/run_cctm_2011_12US1.csh (Moved CTM_WVEL to diagnostic outputs), CCTM/scripts/run_cctm_2014_12US1.csh (Moved CTM_WVEL to diagnostic outputs), CCTM/scripts/run_cctm_2015_HEMI.csh (Moved CTM_WVEL to diagnostic outputs), CCTM/scripts/run_cctm_2016_12US1.csh (Moved CTM_WVEL to diagnostic outputs), CCTM/scripts/run_cctm_Bench_2011_12SE1.csh (Moved CTM_WVEL to diagnostic outputs), CCTM/scripts/run_cctm_Bench_2016_12SE1.csh (Moved CTM_WVEL to diagnostic outputs)

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

### Solution in CMAQv5.3.1

### Files Affected 

## 4. Updates to CCTM Centralized I/O (CIO) module 
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The current implementation of the Centralized Input/Output Module was encoded based on three assumptions: 1. non-metrology input data was expected to be at the same frequency as the output [tstep] (../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#timestep-configuration) (a runscript environment variable the user set). 2. all gridded emissions have the same number of layers. 3. model starts at 0 hour.

### Solution in CMAQv5.3.1
For issue #1: a new algorithm was developed to keep track of time step from each input file and allow the model to write data out at the pre-defined frequency in the run script. The algorithm was also implemented to store the start date and start time of each file, incase the user had emissions input data that used representative days. A new environmental variable was also re-introduced to keep track of which emissions files were representative days and which are not. **Note: this algorithm only allows a maximum of 500 files to be opened.**

for issue #2: a new array was introduced to store the number of layers in each emisison file. Using this new information, the buffer array storing the emissions data being read in was re-allocated to be the no greater than the size of the initial condition file, but no smaller than the size of the largest emissions file. In addition, each gridded emission file was extracted using the newly introduce array that stored the number of layers in each emission file. **Note: this algorithm does not limit the extraction of data greater than the model top (i.e. files that have nlays greater than the model top). However doing so will cause a segmentation fault with memory issues as what is allocated will not match what is being extracted.**

for issue #3: the date advancement is now properly updated, i.e. performs the model date updates only once. An exit call is also implemented to stop the model when an improper interpolation takes place. During the exit call, the following information: the interpolation date, time and bounds, will be sent to the [processor log files](../Users_Guide/CMAQ_UG_ch05_running_a_simulation.md#571-cctm-logfiles) for further debugging. 

### Files Affected 
CCTM/src/cio/centralized_io_module.F, CCTM/src/cio/centralized_io_util_module.F, CCTM/src/emis/emis/EMIS_DEFN.F, CCTM/src/emis/emis/PT3D_DEFN.F, CCTM/src/driver/advstep.F, CCTM/src/phot/inline/concld_prop_acm.F, CCTM/src/cloud/acm_ae7_kmt2/rescld.F,
CCTM/src/cloud/acm_ae7_kmt2/convcld_acm.F, CCTM/src/cloud/acm_ae6_mp/rescld.F, CCTM/src/cloud/acm_ae6_mp/convcld_acm.F, CCTM/src/cloud/acm_ae6/rescld.F, CCTM/src/cloud/acm_ae6/convcld_acm.F, CCTM/src/emis/emis/EMIS_VARS.F, CCTM/src/emis/emis/STK_EMIS.F, CCTM/src/emis/emis/opemis.F, CCTM/src/util/util/RUNTIME_VARS.F, CCTM/scripts/run_cctm_2010_4CALIF1.csh, CCTM/scripts/run_cctm_2011_12US1.csh, CCTM/scripts/run_cctm_2014_12US1.csh, CCTM/scripts/run_cctm_2015_HEMI.csh, CCTM/scripts/run_cctm_2016_12US1.csh, CCTM/scripts/run_cctm_Bench_2011_12SE1.csh, CCTM/scripts/run_cctm_Bench_2016_12SE1.csh


## 8. STAGE
[Jesse Bash](mailto:Bash.Jesse@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 9. ISAM
[Sergey Napelenok](mailto:Napelenok.Sergey@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 10. Coupled WRF-CMAQ Model
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Setting [CTM_BIOGEMIS](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#science-options) to Y in the WRF-CMAQ model did not correctly produce the SOILOUT file after a simulation period was completed. This led to a crash when restarting the model the next day with inilization from the previous days run. This issue was traced back to the inline biogenics algorithm which only writes the SOILOUT file if the model has reached its run length, a runscript environmental variable (CTM_RUNLEN). However, in the WRF-CMAQ Model this runscript environmental variable was not being read in picking up the default value of 48 hours defined in RUNTIME_VARS.F. Thus, if no value is read in and a simulation period other than 48 hours was used the SOILOUT file would not be produced.

Setting [CTM_WBDUST](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#science-options) to Y in the WRF-CMAQ model when running this option with the land-use database being what is provided from WRF results in a crash. This crash is a result of the bounds of extraction being incorrect. The bounds of extraction are calculated once for all standard files (i.e. files with the same spatial information as the initial condition file). This is not an issue in the offline model, as WRF outputs are processed through MCIP and physical files are created to be read in, where the file size is the same as the IC file. However, in the WRF-CMAQ model WRF outputs are processed *by each processor* through aq_prep and are written to in-memory buffers for each processor. Therefore, each processor stores a piece of the entire domain and the same bounds for a standard physical file cannot be used to extract this data. 

### Solution in CMAQv5.3.1
To solve the SOILOUT issue, the WRF-CMAQ model was updated to properly read the environmental variable CTM_RUNLEN in RUNTIME_VARS.F resolving this issue.

The second issue was resolved by adding variables to store the calculation of the bounds for the land-use database from the appropriate file whether it be from WRF or from BELD data. The dust setup will no longer use the standard bounds to extract the data.

### Files Affected 
CCTM/src/cio/centralized_io_module.F, CCTM/src/util/util/RUNTIME_VARS.F

## 11. Bugfixes for MCIP
[Tanya Spero](mailto:Spero.Tanya@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in MCIPv5.1

### Files Affected

