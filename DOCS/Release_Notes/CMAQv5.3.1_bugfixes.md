# CMAQv5.3.1 Bugfixes

## 1. *CTM_WVEL* run script option 
[Ben Murphy](mailto:Murphy.Ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Setting CTM_WVEL, a run time science option to write out the vertical velocity component to the concentration file, to N. 
The default setting, currently, is listed as Y in all runscripts within the repository. If the CTM_WVEL science option is set to N, 
the model immediately crashes. This is because the array that stores the vertical velocity component for writing to the concentration 
file is never allocated and is being used to calculate the average vertical velocity to be written to the average concentration file.

### Solution in CMAQv5.3.1

### Files Affected 

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
Specifying “PM25_IONS” as a TAG CLASS resulted in unstable attribution output for ACLI and ACLJ concentration and deposition.

### Solution in CMAQv5.3.1
ACLI and ACLJ were removed from the “PM25_IONS” TAG CLASS and a new class was added called “CHLORINE.”  This TAG CLASS also includes HCL gas in addition to ACLI and ACLJ, and the algorithms now include partitioning calculations.

### Files Affected 
CCTM/scripts/isam_control.txt

CCTM/src/isam/SA_DEFN.F

CCTM/src/isam/SA_WRAP_AE.F


## 10. Coupled WRF-CMAQ Model
[David Wong](mailto:Wong.David-C@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

### Solution in CMAQv5.3.1

### Files Affected 


