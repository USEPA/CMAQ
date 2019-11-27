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

The model stops when the the output timestep, TSTEP in the cctm run-script, is not an integer multiple of the input files' timestep. The example error messge is below.

     *** ERROR ABORT in subroutine retrieve_time_de on PE 002
     Could not extract GR_EMIS_001      file

The problem occurs because the centralized data module uses TSTEP to set the timestep of the input data. 

### Solution in CMAQv5.3.1

The solution changes the data module to use the timestep of the meteorological input files instead of TSTEP. The fix assumes that the timesteps across input files are the same. Later code changes may remove the constraint.

### Files Affected 

CCTM/src/cio/centralized_io_module.F 

## 8. STAGE
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


