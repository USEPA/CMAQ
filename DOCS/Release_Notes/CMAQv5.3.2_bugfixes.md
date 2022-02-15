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

Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration) set to T, will prompt the centralized i/o module to store the start date of the file. However, this information was never passed on to the function used to extract the data from the netCDF file causing an error, as the time is not available on file. 

Note: This issue was documented in v5.3.1 as **Known Issue CMAQv5.3.1-i2**.

### Solution in CMAQv5.3.2

The information is now properly passed on to the variable required to extract the data from the netCDF file.

### Files Affected 
CCTM/src/cio/centralized_io_module.F

## 4. Centralized I/O (CIO) Bugfix for Initial Conditions Caused by Representative Day 2-D & 3-D Surface Gridded Emissions Files
[David Wong](mailto:dwongepa@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration) set to T, will prompt the centralized i/o module to store the start date of the file. This, however, resulted in a memory issue in the subsequent pseudo-interpolation of the initial conditions (IC). Ideally, ICs should be extracted at the specified date and time. To linearly interpolate in time, data at the start and end of a time-step are stored in the “head and “tail”, respectively. The head was stored correctly, but the tail was not being stored correctly and was being picked up from whatever was in memory last. This resulted in an error when trying to extract the data at the second point as the IC file only has data at the head. If no emissions are present, it would pick the point from whatever file was read last whether that be a MET file, bioseason file, lightning file or IC file. 

Note: This issue was documented in v5.3.1 as **Known Issue CMAQv5.3.1-i4**.

### Solution in CMAQv5.3.2

The memory issue relating to the tail of the circular buffer is now correctly implemented. The head and tail of this file is now automatically set to the start date and time of the simulation for all species.

### Files Affected 
CCTM/src/cio/centralized_io_module.F

## 5. Bugfix for Runscript Variable EMIS_SYM_DATE
[David Wong](mailto:dwongepa@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

An inconsistency was found in the use of the EMIS_SYM_DATE flag between cio and the emissions modules. The EMIS_SYM_DATE flag is no longer a default runscript environmental variable and has subsequently been removed from the runscripts to avoid confusion as the behavior of this flag is unconventional and cannot be explained in a concise manner. To learn more about this flag please visit [Appendix A](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md).

Note: This issue was documented in v5.3.1 as **Known Issue CMAQv5.3.1-i5**.

### Solution in CMAQv5.3.2

EMIS_SYM_DATE is now consistent beween both DESID and CIO. For users who wish to use the EMIS_SYM_DATE environmental variable in their runscripts can visit [Appendix A](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md) for a complete description of the variable.

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
The STAGE deposition code is sensitive to numerical underflows because it uses the ambient concentration of pollutants to estimate concentration gradients between the atmosphere, soil and vegetation. This is done to unify surface exchange processes that can represent deposition and emission processes simultaneously. This can result in numerical underflows when ambient concentrations are low and/or there is little turbulent mixing, e.g. very stable conditions. These numerical underflows have little impact on ambient concentrations because deposition losses under these conditions are negligible, but it can lead to numerical instabilities.  

### Solution in CMAQv5.3.2
This bug fix corrects a potential underflow error in the calculation of dry deposition velocities due to dry deposition factors from the species name lists. Additionally, vdiffacmx.F was modified to estimate the production over loss term only if there was production estimated from bidirectional exchange or heterogenous chemistry eliminating the potential for most divide by zero errors. In most cases, the production and loss terms are estimated using the same modeled diffusive properties and the estimate of production over loss is likely to result in a real number. If there is a production term and the deposition velocity is numerically zero, the deposition velocity is reset to the smallest single precision number available and a warning message is printed in the log file. This warning may be triggered by heterogenous HONO production at the surface in CMAQ v5.3.2 as the processes in this parameterization are not diffusively limited. However, this is not likely to lead to model issues as the large production over loss term is negated by the exponential term when estimating the updates to the ambient concentration in vdiffacmx.F. The dry deposition factor for ECH4 was changed to 1 as the deposition velocity for this species is now explicitly calculated. Additionally, cksummer.F was also modified to stop on NaN and inf as these errors can propagate though the model simulation resulting in erroneous results.

### Files Affected
CCTM/src/MECHS/cb6r3_ae6_aq/GC_cb6r3_ae6_aq.nml 
CCTM/src/MECHS/cb6r3_ae7_aq/GC_cb6r3_ae7_aq.nml
CCTM/src/MECHS/cb6r3_ae7_aqkmt2/GC_cb6r3_ae7_aq.nml 
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/GC_cb6r3m_ae7_kmtbr.nml 
CCTM/src/depv/stage/STAGE_MOD.F
CCTM/src/vdiff/acm2_stage/vdiffacmx.F
CCTM/src/util/util/cksummer.F

## 11. M3DRY/STAGE unified naming conventions for deposition
[Jon Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The variable names in the CCTM_DRYDEP output file for ammonia when the M3dry bidirectional NH3 model is used were inconsistent with past versions of CMAQ and with the current version of STAGE.  These differences in naming conventions could cause issues when performing post-processing of CCTM_DRYDEP outputs.
   
Note: This issue was documented in v5.3.1 as **Known Issue CMAQv5.3.1-i6**.
   
### Solution in CMAQv5.3.2
The variable names in the DRYDEP output file for ammonia when the M3dry bidirectional NH3 model is used were changed to be consistent with the current version of STAGE. There are 3 variables related to NH3: the dry deposition flux is in the variable named "NH3", the emission flux is in the variable "NH3_Emis", and the net flux is in the variable "NH3_Flux"

See the [User's Guide Chapter 7](../Users_Guide/CMAQ_UG_ch07_model_outputs.md#nh3-flux-components-in-cctm_drydep) for additional information on the the NH3 flux components available in the CCTM_DRYDEP output file. 

### Files Affected
CCTM/src/vdiff/acm2_m3dry/VDIFF_MAP.F        
CCTM/src/vdiff/acm2_m3dry/opddep.F  
CCTM/src/vdiff/acm2_m3dry/vdiffproc.F   

## 12. Removes unnecessary request for NLDN file when it is not needed
[Daiwen Kang](mailto:kang.daiwen@epa.gov), U.S. Environmental Protection Agency

### Description of the model issue
When inline lightning NO production is turned on, there are two options: 
Option 1 uses hourly lightning flash data which are provided by an external input file through the enviromental variable NLDN_STRIKES;
Option 2 uses parameters provided in the LTNGPARMS_FILE file. However, the current code requests the NLDN_STRIKES file even when Option 2 is selected. Without the file being set, the model crashes.

### Solution in CMAQv5.3.2
In the centralized_io_module.F file, the vairable NLDNSTRIKE (logical variable controls Options 1 and 2 described above) from the RUNTIME_VARS module and added into the control statement to remove the request of the lightning flash data file when Option 2 is selected.

In addtion, the variale LTNG_FNAME is converted to all uppercase by calling UPCASE before it is used. The current used the word "InLine" to dertermine if the inline lightning NO is produced. Some users have complained that in some documentation, it says "Inline" not "InLine". With this change, it can be either uppercase or lowercase or the mixed of both when define the environmental variable LTNGNO in runscript.
### File Afftected

CCTM/src/cio/centralized_io_module.F 
CCTM/src/emis/emis/LTNG_DEFN.F 

## 13. fixed excessive reading of time independent boundary file data Bug CIO
[David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Inside each interpolation routine, the following logic guides whether current time step is within the circular buffer or not.

             if ((cio_bndy_data_tstamp(1, loc_tail, var_loc) .lt. date) .or.
     &           ((cio_bndy_data_tstamp(2, loc_tail, var_loc) .lt. time) .and.
     &            (cio_bndy_data_tstamp(1, loc_tail, var_loc) .eq. date))) then

In the time independent boundary file case, both time stamps, cio_bndy_data_tstamp(2, loc_head, :) and cio_bndy_data_tstamp(2, loc_tail, :), are the same. Hence any future time step is always falls outside the circular buffer and the block of code in this if block will be executed. The loc_tstep is 0 so reading the data, which has been read in, again. The overall execution time increases.

### Solution in CMAQv5.3.2
At the initial reading circular buffer  phase, for time independent boundary file data, data will be read in once and stored in the head of the circular buffer, and the tail time stamp is set to 250000 which means any future will fall in the circular buffer and the body of the above if block will never be executed.

In addition, the interpolation ratio checking is put in a ifdef block to eliminate non-essential check.

### Files Affected
CCTM/src/cio/centralized_io_module.F   

## 14. STAGE NH3 bidi bugfix
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

This bugfix corrects four issues in CMAQ using the STAGE deposition option. 

1) EPIC model data processed by FEST-C does not always correctly write bad value tags for missing data. In the STAGE deposition option, there is code to determine when a grid cell has valid EPIC data. 

2) These routines failed when the bad value tag was not correctly identified and there were errors in identifying valid data when reading the STAGE bidi restart file, INIT_MEDC_1, that resulted in errors in estimating soil pH. 

3) There as a unit conversion error in the evasive loss of NH3 on the overall soil NH4 budget.

4) The dry deposition and deposition velocity output files global attributes were changed to tag the deposition option used.

### Solution in CMAQv5.3.2

The logic identifying bad values in EPIC files in NH3_BIDI_MOD.F was revised to omit data tagged with bad values and values that are out of the expected range of the variables, e.g. negative concentrations. The unit conversion error was corrected in NH3_BIDI_MOD.F. Overall, this resulted in up to 10% reduction in modeled NH3 concentrations on the CONUS domain after several months of simulation but can have larger local impacts. This updated has reduced spuriously large emissions in areas where evasion is a sizable part of the soil NH4 budget, typically isolated grid cells in Western US with high soil pH in the CONUS domain.  

### Files Affected

CCTM/src/depv/stage/NH3_BIDI_MOD.F
CCTM/src/vdiff/acm2_m3dry/opddep.F
CCTM/src/vdiff/acm2_stage/opddep.F
CCTM/src/vdiff/acm2_stage/opddep_fst.F
CCTM/src/vdiff/acm2_stage/opddep_mos.F

## 15. Time step fix in the m3dry dry deposition code
[Jon Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
There was an error in the timestep in the subroutine m3dry.F. Previously, the second element of the array TSTEP was passed to m3dry.F where it was used assuming it was in seconds. But, TSTEP is in HHMMSS, so a 300s timestep was interpreted as 500 which created a 40% error in the NH3_Emis and NH3 dry deposition output in the DRYDEP file. The error only affected these two outputs and not the model itself and only when running NH3 bidi. This also affects HGBIDI and ISAM results for NH3.
The timestep in m3dry was used for accumulating NH3_Emis and also was passed to subroutines ATX and ASWX which are used when HGBIDI are turned on.

### Solution in CMAQv5.3.2

The fix involves replacing the TSTEP(2) in the call to m3dry.F from DEPV_DEFN.F with real dtsec which is loaded with the new statement:
DTSEC = FLOAT( TIME2SEC( TSTEP( 2 ) ) )

### Files Affected 
CCTM/src/depv/m3dry/DEPV_DEFN.F
CCTM/src/depv/m3dry/m3dry.F

