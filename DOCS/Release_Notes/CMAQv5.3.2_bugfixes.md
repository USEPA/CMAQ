# CMAQv5.3.2 Bugfixes

## 1. Correct Emission Control File for cb6mp_ae6_aq mechanism
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

The cb6mp_ae6_aq mechanism's emissions control file did not include instructions for the hazardous air pollutants in the
NonReactive (NR) species namelist. It also gave incorrect mode for aerosol species, ADE_EC and ADE_NO3. 

### Solution in CMAQv5.3.2

Instructions were added to this control file for the above NR species. The mode was changed from COARSE to FINE for the ADE_EC and ADE_NO3 species.

### Files Affected 
CCTM/src/MECHS/cb6mp_ae6_aq/EmissCtrl_cb6mp_ae6_aq.nml

## 2. Replace data website in get TOMS data 
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

The cshell script that gets data to create the CMAQ OMI file uses a website that no
longer works.

### Solution in CMAQv5.3.2

The script was corrected with a working website.

### Files Affected 
PREP/create_omi/scripts/get_toms_data.q

## 3. Centralized I/O (CIO) Bugfix for Representative Day 2-D & 3-D Surface Gridded Emissions Files
[David Wong](mailto:dwongepa@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration). set to T, will prompt the centralized i/o module to store the start date of the file. However, this information was never passed on to the function used to extract the data from the netCDF file causing an error, as the time is not available on file. 

### Solution in CMAQv5.3.2

The information is now properly passed on to the variable required to extract the data from the netCDF file.

### Files Affected 
CCTM/src/cio/centralized_io_module.F

## 4. Centralized I/O (CIO) Bugfix for Initial Conditions Caused by Representative Day 2-D & 3-D Surface Gridded Emissions Files
[David Wong](mailto:dwongepa@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration) set to T, will prompt the centralized i/o module to store the start date of the file.

### Solution in CMAQv5.3.2

The memory issue relating to the tail of the circular buffer is now correctly implemented. The head and tail of this file is now automatically set to the start date and time of the simulation for all species.

### Files Affected 
CCTM/src/cio/centralized_io_module.F

## 5. Bugfix for Runscript Variable EMIS_SYM_DATE
[David Wong](mailto:dwongepa@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

An inconsistency was found in the use of the EMIS_SYM_DATE flag between cio and the emissions modules. The EMIS_SYM_DATE flag is no longer a default runscript environmental variable and has subsequently been removed from the runscripts to avoid confusion as the behavior of this flag is unconventional and cannot be explained in a concise manner. To learn more about this flag please visit [Appendix A](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md).

### Solution in CMAQv5.3.2

EMIS_SYM_DATE is now consistent beween both DESID and CIO. For users who wish to use the EMIS_SYM_DATE environmental variable in their runscripts can visit [Appendix A](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md) for a complete description of the variable.

### Files Affected 
CCTM/src/cio/centralized_io_module.F


## 6. Bugfix and code cleanup for lightning NOx production
[Daiwen Kang](mailto:kang.daiwen@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The CMAQ model could not be run with lightning NO emissions if the model domain used a non-lambert projection (such as hemispheric CMAQ applications which use a polar sterographic projection).

### Solution in CMAQv5.3.2
The LTNG_DEFN.F file was updated to (1) remove redundant calculations related to mapping lat/lon locations to model grid cells - these are no longer needed, (2) make the code better conform with the centralized I/O (CIO) implementation in v5.3 and on-wards, and (3) remove redundant calculations. These changes do not impact model calculations. 
### Files Affected 
CCTM/src/emis/emis/LTNG_DEFN.F

## 7. Update Warnings for "Missing Variables" to Errors
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
When variables are missing from input files but requested by the model, CMAQ produces a warning and then exits with a 'stop' command, providing no error message to indicate the problem. 'Stop' commands are generally to be avoided.  

### Solution in CMAQv5.3.2
Revised 'stop' commands to call m3exit instead with an appropriate error message instructing the user of the nature of the error.

### Files Affected 
CCTM/src/cio/centralized_io_module.F  
CCTM/src/aero/aero6/aero_subs.F90  
CCTM/src/aero/aero6/SOA_DEFN.F90  
CCTM/src/driver/driver.F  
CCTM/src/emis/emis/EMIS_DEFN.F  
CCTM/src/util/util/setup_logdev.F  

## 8. Resolve errors in unmatched emission aerosol modes and diagnostic output
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

### Description of model issue and solution in CMAQv5.3.2
This bugfix restores the emissions diagnostic output procedure to full functionality, regardless of time step. Previously, the algorithm would fail if the time step was an odd number of seconds.

There was also a missing error-check in the case that a user references an aerosol mode in their Emission Control file that does not exist in the AERO_DATA reference table. This error was previously caught if the user were adding a mode in addition to fine and coarse modes, but not if they were reassigning the properties of the fine and coarse aerosol modes.

There are also important corrections to the diagnostic calculation and units of aerosol number, mass and surface area for the emission diagnostic files. Everything is consistent on NetCDF files now. 

### Files Affected 
CCTM/src/aero/aero6/AERO_EMIS.F  
CCTM/src/emis/emis/EMIS_DEFN.F  

## 9. Correct Typos in Sample Process Analysis Control File
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The sample Process Analysis control file for the cb6r3_ae7_aq mechanism and documented in Chapter 9 of the User's Guide had typos in the definitions of two of the IRR output variables (aVOCwithO3 and aVOCwithNO3). These have been corrected so that the definitions match the variable names.

### Solution in CMAQv5.3.2
The typos in the control files and documentation have been corrected so that the definitions match the variable names.  These changes have no effect on results for the base model. The process analysis control file that is provided is merely an illustrative example that is intended to be modified by the user for a specific application.

### Files Affected 
CCTM/src/MECHS/cb6r3_ae7_aq/pa_cb6r3_ae7_aq.ctl  
CCTM/src/MECHS/cb6r3_ae6_aq/pa_cb6r3_ae6_aq.ctl  
CCTM/src/MECHS/cb6r3_ae7_aqkmt2/pa_cb6r3_ae7_aq.ctl  
DOCS/Users_Guide/CMAQ_UG_ch09_process_analysis.md  

## 10. Correct Periodic Underflow in the STAGE deposition option
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
An underflow error was found in the STAGE resulting in a deposition velocity of 0 under highly stable conditions over water/coastal grid cells. This results in a NaN in vdiffacmx.F that is then reset to the model floor value in clouds. This underflow is encountered a handful of times for a handful of grid cells in the span of an annual simulation. In all cases and several years of simulations the NaN has been reset to the model minimum once encountered in clouds.

### Solution in CMAQv5.3.2
A lower bound of the machine specific single precision value, TINY(1.0),  was set on the deposition velocity in STAGE_MOD.F.  Additionally, cksummer.F has been modified to exit when a NaN or infinity has been encountered.

### Files Affected 
CCTM/src/depv/stage/STAGE_MOD.F  
CCTM/src/util/util/cksummer.F  

## 11. M3DRY/STAGE unified naming conventions for deposition
[Jon Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The variable names in the DRYDEP output file for ammonia when the M3dry bidirectional NH3 model is used were inconsistent with past versions of CMAQ and with the current version of STAGE.  This created some difficulties with postprocessors.
   
### Solution in CMAQv5.3.2
The variable names in the DRYDEP output file for ammonia when the M3dry bidirectional NH3 model is used were changed to be consistent with past versions of CMAQ and with current version of STAGE. There are 3 variables related to NH3: the dry deposition flux is in the variable named "NH3", the emission flux is in the variable "NH3_Emis", and the net flux is in the variable "NH3_Flux"

### Files Affected
CCTM/src/vdiff/acm2_m3dry/VDIFF_MAP.F        
CCTM/src/vdiff/acm2_m3dry/opddep.F  
CCTM/src/vdiff/acm2_m3dry/vdiffproc.F   




