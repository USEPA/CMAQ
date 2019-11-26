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
The current implementation of the Centralized Input/Output Module was encoded to only support input files with several implicit characteristics. The first being, non-metrology input data was expected to be at the same frequency as the output [tstep](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#timestep-configuration) (a runscript environment variable the user set). Often times, this variable is set to be 1-hr steps which in many cases matches the input data. However, if the variable were to be changed or if your input files contained data reported at various frequencies a crash would often times follow. The second characterisic being, that all gridded emissions must have the same number of layers. This often times led to a crash because of incorrect memory allocation or incorrect reading of files skewing results greatly. The thrid characteristic being, that an interpolation issue would cause a model crash if conducting a continuous run through midnight, since the dates were not being properly updated in the emissions processing routines.

### Solution in CMAQv5.3.1
To combat some of the issues encountered above, a new algorithm was developed to allow input files to have data at different time frequencies and to allow the model to write data out at the frequency desired (i.e. 1-hr steps, 5 min steps, etc...) by storing each individual files tstep information when opened. The algorithm was also implemented to store the start date and start time of each file, incase the user had emissions input data that used representative days. A new environmental variable was also re-introduced to keep track of which emissions files were representative days and which are not. **Note: this algorithm only allows a maximum of 500 files to be opened, to limit the amount of memory that needs to be allocated.**

To allow individual 3-D gridded emissions files the flexibility to have a different number of layers, a new array was introduced to store the number of layers in each emisison file. Using this new information, the buffer array storing the emissions data being read in was re-allocated to be the no greater than the size of the initial condition file, but no smaller than the size of the largest emissions file. In addition, each gridded emission file was extracted using the newly introduce array that stored the number of layers in each emission file. **Note: this algorithm does not limit the extraction of data greater than the model top (i.e. files that have nlays greater than the model top). However doing so will cause a segmentation fault with memory issues as what is allocated will not match what is being extracted.**

Lastly, to address the issue of the improper interpolation in the emissions processing routines, the date advancement is now properly updated to only update once the model date updates resolving the interpolation issues. To stop the model if improper interpolation takes place, an exit call is now implemented to stop the model with a general program related signal while printing the interpolation date, time and bounds requested to the [processor log files](../Users_Guide/CMAQ_UG_ch05_running_a_simulation.md#571-cctm-logfiles) for further debugging. 

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

### Solution in CMAQv5.3.1

### Files Affected 


## 11. Bugfixes for MCIP
[Tanya Spero](mailto:Spero.Tanya@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in MCIPv5.1

### Files Affected

