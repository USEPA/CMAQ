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

### Solution in CMAQv5.3.1

### Files Affected 

## 4. Time step limiation for boundary condition input file
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The current implementation of the Centralized Input/Output Module only supports BNDY_CONC_1 files that have a time step of 1 hour. 
Previous versions of CMAQ were able to process boundary condition files with time steps greater than 1 hour. If providing a 
boundary condition file with a time step greater than 1 hour, the model will terminate execution with an error message 
that the BNDY_CONC_1 cannot be read for the requested time step.

### Solution in CMAQv5.3.1

### Files Affected 
CCTM/src/cio/centralized_io_module.F


## 5. Start Time at 8:00UTC
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 6. Emission Layers
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 7. Variable Output Timestep
[Bill Hutzell](mailto:Hutzell.Bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


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

