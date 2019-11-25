# CMAQv5.3.1 Bug fixex

## 1. *CTM_WVEL* run script option 
[Ben Murphy](mailto:Murphy.Ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Setting CTM_WVEL, a run time science option to write out the vertical velocity component to the concentration file, to N. 
The default setting, currently, is listed as Y in all runscripts within the repository. If the CTM_WVEL science option is set to N, 
the model immediately crashes. This is because the array that stores the vertical velocity component for writing to the concentration 
file is never allocated and is being used to calculate the average vertical velocity to be written to the average concentration file.

### Solution in CMAQv5.3.1

### Files Affected 


## 2. Diagnostic File for Lightning NOx
[Daiwen Kang](mailto:Kang.Daiwen@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 

## 3. Time step limiation for boundary condition input file
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The current implementation of the Centralized Input/Output Module only supports BNDY_CONC_1 files that have a time step of 1 hour. 
Previous versions of CMAQ were able to process boundary condition files with time steps greater than 1 hour. If providing a 
boundary condition file with a time step greater than 1 hour, the model will terminate execution with an error message 
that the BNDY_CONC_1 cannot be read for the requested time step.

### Solution in CMAQv5.3.1

### Files Affected 
CCTM/src/cio/centralized_io_module.F


## 4. Start Time at 8:00UTC
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 5. Emission Layers
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 6. Variable Output Timestep
[Bill Hutzell](mailto:Hutzell.Bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 7. STAGE
[Jesse Bash](mailto:Bash.Jesse@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 8. ISAM Chlorine
[Sergey Napelenok](mailto:Napelenok.Sergey@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


## 9. Coupled WRF-CMAQ Model issue with RUNLEN environment variable
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 

## 10. Bugfix for Reading Multiple Regions Files
[Ben Murphy](mailto:Murphy.Ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 
