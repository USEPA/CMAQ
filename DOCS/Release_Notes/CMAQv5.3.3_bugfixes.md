# CMAQv5.3.3 Bugfixes
## 1. Domain windowing option restored
[David Wong](wong.david-c@epa.gov), U.S. Environmental Protection Agency

## Description of model issue
Previous versions of CCTM (before CMAQv5.3) allowed users to provide gridded input files which were larger in horizontal extent than the simulated domain. The model would then check if the simulated domain input was contained in the larger domain input (checking for matching grid resolution and projection parameters). If so, the model would then extract data corresponding to the simulated domain from the larger horizontal domain input by identifying the corresponding subrectangle of the horizontal grid, online. This feature called "Windowing", is more often done offline, using I/O API tools such as [m3wndw](https://www.cmascenter.org/ioapi/documentation/all_versions/html/M3WNDW.html) and [bcwndw](https://www.cmascenter.org/ioapi/documentation/all_versions/html/BCWNDW.html) before running CMAQ.

Recent versions of the model, CMAQv5.3+, broke this functionality, allowing users to only window gridded inputs offline using I/O API.

### Solution in CMAQv5.3.3
CMAQv5.3.3 reintroduces this functionality, allowing users to window gridded inputs online. Users must note, the windowing functionality does not apply to Chemical Boundary files. Users are responsible for generating their own domain specific Chemical boundary files!

More details about this new feature can be found in the [Users Guide](../Users_Guide/CMAQ_UG_ch04_model_inputs.md#431-windowing-capability). 

## Files Affected
CCTM/src/cio/centralized_io_module.F        
CCTM/src/util/util/subhfile.F    

## 2. DMS chemistry bug fix
[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Dimethyl sulfide (DMS) emissions and chemistry are included in chemical mechanism cb6r3m_ae7_kmtbr as part of CMAQv5.3. In v5.3 DMS emissions are calculated using water-side DMS gas-transfer velocity (KW) following Liss and Merlivat (1986). However, the parameterization for KW at low wind speed (≤ 3.6 m/s) contained an error. 

### Solution in CMAQv5.3.3
CMAQv5.3-v5.3.2 uses the following parameterization:      
 KW = 0.17 × WSPD10 / SQRT(SCN / 600.0)    
Where, KW = water-side DMS gas-transfer velocity, WSPD10 = wind speed, SCN = Schmidt number of DMS.

CMAQv5.3.3 is corrected and uses the following parameterization:    
 KW = 0.17 × WSPD10 / (SCN / 600.0)<sup>0.667</sup>

Additional information on the impact on model concentrations is documented in the [DMS Release Note](CMAQv5.3.3_DMS_chemistry_bugfix.md).

### Files Affected 
CCTM/src/emis/emis/MGEMIS.F

## 3. HONO Deposition Fix for the STAGE Deposition Option
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The HONO dry deposition flux in CMAQ v5.3 to v5.3.2 can be negative when surface heterogeneous production exceeds deposition. Similar negative values are found in both M3DRY and STAGE. 

### Solution in CMAQv5.3.3
This issue is fixed in CMAQv5.3.3.  Testing was done for the 2016 benchmark and CONUS domains with the STAGE option for all combinations of NH3 and Hg bidirectional exchange and heterogeneous HONO production options and with ISAM simulations. Predicted HONO dry deposition with CMAQv5.3.2 can be negative while HONO dry deposition with v5.3.3 is positive for all dry deposition, grid cell average and MOSAIC and ISAM outputs. Negative Hg dry deposition fluxes were removed from dry deposition, MOSAIC and ISAM outputs and negative NH3 dry deposition fluxes were removed from the MOSAIC deposition option. Two new variables were added to capture Hg emissions or heterogeneous HONO production, HG_Emis and HONO_Het respectively, if those options are selected in the run script. The diagnostic NH3 dry deposition flux in CMAQ v5.3 - v5.3.2 was replaced with the actual dry deposition flux resulting in up to a 2% change, more typically less than 1% for the CONUS domain, in the modeled deposition. Dry depositions of other chemical species and model concentrations are unaffected by the changes. 

Additionally, several large redundant 4D arrays in MOSAIC were removed and some variables in vdiff/acm2_stage were renamed to more accurately indicate the data that they contain. 
### Files Affected 
CCTM/src/depv/stage/DEPV_DEFN.F    
CCTM/src/depv/stage/HGSIM.F    
CCTM/src/depv/stage/MOSAIC_MOD.F    
CCTM/src/depv/stage/NH3_BIDI_MOD.F    
CCTM/src/depv/stage/STAGE_DATA.F    
CCTM/src/depv/stage/STAGE_MOD.F    
CCTM/src/vdiff/acm2_stage/VDIFF_MAP.F    
CCTM/src/vdiff/acm2_stage/opddep.F    
CCTM/src/vdiff/acm2_stage/vdiffacmx.F    
CCTM/src/vdiff/acm2_stage/vdiffproc.F    

## 4. HONO Deposition Fix for the M3DRY Deposition Option
[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The HONO dry deposition flux in CMAQ v5.3 to v5.3.2 can be negative when surface heterogeneous production exceeds deposition. Similar negative values are found in both M3DRY and STAGE. 

### Solution in CMAQv5.3.3
This issue is fixed in CMAQv5.3.3.  Testing was done for the 2011 benchmark domain with the M3DRY option. Predicted HONO dry deposition with CMAQv5.3.2 can be negative while HONO dry deposition with the v5.3.3 is positive. Dry depositions of other chemical species and model concentrations are unaffected by the changes. It also removes the negative HONO deposition in the ISAM output.

### Files Affected 
CMAQ/CCTM/CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F


## 5. Correction to BEIS in-line emissions for RACM2 
[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Xiaoyang Chen at Northeastern University identified that the inline biogenics module in CMAQ (BEIS) is not generating any monoterpene emissions when inline option is enabled with RACM2. Emission profile 'B10RD' is currently used for RACM2; however, it does not contain correct mapping which results in no monoterpene emissions. 

### Solution in CMAQv5.3.3
An emission profile 'B3V10' was generated when RACM2 was initially implemented in CMAQ which contains correct mapping of model species. The model is revised to remove emission profile 'B10RD' and add the emission profile 'B3V10'. Biogenic emissions calculation using MEGAN does not use this profile and works properly.

### Files Affected 
CCTM/CCTM/src/emis/emis/BIOG_EMIS.F    
CMAQ/CCTM/CCTM/src/biog/beis3/gspro_biogenics.txt    

## 6. Bugfix, clean-up, and added option in bldscript for distr_env.c
[Fahim Sidi](mailto:sidi.fahim@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Bug reported by Steve Fine, EPA-OAR, when using Amazon Web Services (AWS) to run CMAQ across multiple adjoined instances. The issue is related to blank environmental variables causing a segmentation fault on AWS when invoking the directive -Dcluster in the CCTM Makefile. Additionally, it was found that there was no build script option to invoke C code distr_env, causing users to manually invoke this option via editing the CCTM Makefile to include the CPP flag -Dcluster. 


### Solution in CMAQv5.3.3

Changed distr_env.c to only set environmental variables on other processors that are not blank, which resolved the segmentation fault. To fix manual addition of the CPP Flag -Dcluster, a new bldscript option within CCTM that allows users to optionally invoke distr_env.c is added. This new option is called "DistrEnv", if set this option adds the CPP flag -Dcluster. It should be noted, that two conditions have to be met for the -Dcluster flag to be activiated:

(1) DistrEnv is set
(2) ParOpt is set (indicates this an MPI run)

Since DistrEnv is strictly an MPI option (containing MPI commands) it is only needed if ParOpt is invoked. It has no use when running CMAQ serially.

The second part of this update cleans up the C code and adds C-Fortran Interoperability (Feldman Style Binding) that is consistent with the CPP flag provided in the Makefile (-DFLDMN) to compile under other architectures and with compilers that do not append underscores to C function names.

### Files Affected 
CCTM/scripts/bldit_cctm.csh    
CCTM/src/par/mpi/distr_env.c    

## 7. POST tool bug fixes in hr2day and sitecmp_dailyo3
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

*hr2day*: When using the hr2day MAXDIF operation in conjunction with setting PARTIAL_DAY to F, the previous code returned missing values for all days.

*hr2day*: When run script variable START_DATE was set to a date later than the start date of M3_FILE_1, the previous code generated empty time steps for the time period between the start date of M3_FILE_1 and environment variable START_DATE. 

*sitecmp_dailyo3*: When the values in the in `OZONE_F` column of CASTNET `IN_TABLE` files were enclosed in quotes, the code did not remove those quotes, therefore  did not properly match it to any of the known QA codes for which values should be discarded, and consequently did not discard such flagged values before computing the daily metrics. In the CASTNET files distributed via CMAS, this only affected the 2005 observation file.

### Solution in CMAQv5.3.3

The *hr2day* code was updated to correct the behavior of the MAXDIF operation when PARTIAL_DAY is set to F. It also was updated so that OUTFILE only contains time steps between MAX(start of M3_FILE_1, START_DATE) and MIN(end of M3_FILE_n, END_DATE)
 
The *sitecmp_dailyo3* code was updated to remove any quotes from the `OZONE_F` column of CASTNET `IN_TABLE` files. 

### Files Affected 

POST/hr2day/src/hr2day.F    
POST/sitecmp_dailyo3/src/utilities.F    

## 8. Updated bldmake & config_cmaq.csh to add mpi library in CCTM Makefile
[Fahim Sidi](mailto:sidi.fahim@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Discrepancy reported by Liz Adams and Christos Efstathiou, CMAS, that CMAQ (namely CCTM only) didn’t have the capability to specify different paths to the mpi include files and mpi library directory in the config_cmaq.csh, both needed to compile CCTM. Instead to do this, you had to manually edit the Makefiles and recompile the model.

The update enables users to specify, explicitly, paths to the MPI Library and include directories. 

### Solution in CMAQv5.3.3

Changed config_cmaq.csh to include new variable MPI_INCL_DIR, consistent with treatment of other external libraries used in CMAQ (I/O API & netCDF). A change is also made in bldmake to reflect this updated variable. 

### Files Affected 
UTIL/bldmake/src/bldmake.f    
config_cmaq.csh    

## 9. Provide appropriate error message and abort if OMI photolysis file is missing
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The photolysis module reads a data file from NASA's Ozone Monitoring Instrument (OMI) describing total column ozone. The model attempted to read the file prior to the check whether the file had been successfully opened, leading to a crash. 

### Solution in CMAQv5.3.3
The check has been moved prior to the first attempt to read the file, and the model aborts with an appropriate error if the OMI file is not found.  There is no impact on model results in the normal case, where the OMI file is present.

### Files Affected 
CCTM/src/phot/inline/o3totcol.f    


## 10. Revise how the photolysis module checks write time for diagnostics. 
[Bill Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
Both options of the photolysis module used a 'mod' function to determine whether 
to write diagnostic outputs. The method can cause errors with
higher resolution domains such as 1X1 km<sup>2</sup> and and 4X4 km<sup>2</sup>.

### Solution in CMAQv5.3.3
A more robust method was taken from CCTM/src/emis/emis/EMIS_DEFN.F. The change has no
impact on model predictions. 

### Files Affected 
CCTM/src/phot/inline/phot.F    
CCTM/src/phot/table/phot.F     

## 11. Correct chemistry data for Reactive Tracers
[Bill Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
For the cbr3_ae6_aq and cb6r3m_ae6_kmtbr mechanisms, the available gas chemistry solvers had errors 
in the loss reactions for several reactive tracers representing Hazardous Air Pollutants.

1.   Destruction by OH was double counted for acetonitrile, acrylic acid, carbon sulfide,
ethyl benzene, hexane, methyl chloride, chloroprene and stryene.
2.   Also for styrene, the OH reaction had an activation energy with the wrong sign based on Cho, J.; Roueintan, M.; Li, Z. J., Kinetic and Dynamic
Investigations of OH Reaction with Styrene, J. Phys. Chem. A, 2014, vol 118,
9460 - 9470.
See the NIST webpage: https://kinetics.nist.gov/kinetics/Detail?id=2014CHO/ROU9460-9470:1.

On a minor note, the cb6mp_e6_aq's emissions control file used incorrect emission surrogates
or omitted them.

### Solution in CMAQv5.3.3
Corrections removed these errors in files containing the gas chemistry data for reactive tracers.

Changes corrected the cb6mp_e6_aq's emissions control file.
 
### Files Affected 
CCTM/src/gas/smvgear/degrade_data.F    
CCTM/src/gas/ros3/degrade_data.F    
CCTM/src/gas/ebi_cb6r3_ae6_aq/degrade_data.F    
CCTM/src/gas/ebi_cb6mp_ae6_aq/degrade_data.F     
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/degrade_data.F    
CCTM/src/gas/smvgear/degrade_data.F    
CCTM/src/MECHS/cb6mp_ae6_aq/EmissCtrl_cb6mp_ae6_aq.nml    


## 12. Remove Differences in Predictions between PHOTDIAG True and False. 
[Bill Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
For the inline option of the photolysis module, CCTM predictions differed depending on whether PHOTDIAG was TRUE or FALSE. For example, small differences in ozone occurred (less than 0.00005 ppmV) in simulations over the 
hemispheric domain. Occurances appeared dependent on the meteorological input files and minimum synchronization
time-step. They occured because the O3TOTCOL routine saved the observed global total ozone columns with their date 
and times from the last call. The saved information determined how to update data used to interpolate the observations 
to the domain's vertical columns.

### Solution in CMAQv5.3.3
The solution added calls to the O3TOTCOL routine when all columns are DARK and PHOTDIAG is FALSE. 
When PHOTDIAG equals FALSE, model predictions now match predictions when PHOTDIAG is TRUE.

### Files Affected 
CCTM/src/phot/inline/phot.F     

## 13. Correct O3 deposition to wet soil in the STAGE deposition option
[Jesse O. Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency

### Description of the model issue
The wet cuticular resistance would overwrite the wet soil resistance due to using the same variable names, rwet, for ozone. This resulted in sslightly faster than intended deposition of ozone to wet soil surfaces when the leaf cuticles were also wet. 

### Solution in CMAQ v5.3.3
A new variable was created for wet cuticular surfaces, rcwet, in STAGE_MOD.F. This results in slightly higher, typically less than 1ppb, ozone values primarily at night, early mornings and during precipitation events when both cuticular and soil surfaces are wet. 

### Files Affected
CCTM/src/depv/stage/STAGE_MOD.F    

## 14. Correct Mosaic Land Use Specific Dry Deposition Velocity Diagnostic Output

[Jesse O. Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency
### Description of the model issue
An indexing error prevented the output arrays of some land use specific species deposition velocities in the Mosaic output arrays to be populated and resulted in 0 values being written to the output file. This bug did not impact modeled dry deposition or ambient concentrations. 

### Solution in CMAQ v5.3.3
The indexing error was corrected, and land use specific deposition velocities are now correctly populated and written to the Mosaic deposition velocity file when CTM_MOSAIC is set to Y. 

### Files Affected
CCTM/src/depv/stage/MOSAIC_MOD.F    
CCTM/src/depv/stage/STAGE_MOD.F    

## 15. Resolve error in Process Analysis reintialization after aerosol processing
[Ben Murphy](mailto:murphy.benjamin@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
This bugfix resolves a problem demonstrated by a user where mass is not conserved in process analysis rates for species that have non-negligible aerosol condensation. The difference between the change in instantaneous concentrations and the sum of the process rates looked very much like (but not exactly equivalent to) the contribution from condensation. It turns out that the state of the concentration field was not saved after the aerosol process analysis subroutine (pa_update_aero) and so when the change due to vdiff in the next iteration of sciproc was calculated, it artificially included the impact of condensation/evaporation as well. This means that cond/evap were double-counted for every iteration of sciproc except for the first one of an output time step, meaning that the residual disappeared if the output time step were sufficiently small to ensure only one pass through sciproc (e.g. 5 minutes).

### Solution in CMAQv5.3.3
The main fix is in pa_update_aero and involves the calculation of csav. Other fixes that were applied including real 8 conversion in aero_subs, and the subs_data_copy in pa_update_aero did not have a detectable impact on process analysis results but are good coding practice and consistent with existing approaches for other processes, respectively. The assignment of PA_EM_CONV was determined to lead to a segmentation fault when run with gcc in debug mode so this was revised as well.

This update does not affect CMAQ predictions. It resolves a mass closure issue and ensures consistency in output.

### Files Affected
CCTM/src/MECHS/cb6r3_ae7_aq/pa_cb6r3_ae7_aq.ctl    
CCTM/src/aero/aero6/aero_subs.F    
CCTM/src/driver/sciproc.F    
CCTM/src/procan/pa/pa_update.F    

## 16. KZMIN setting update
[David Wong](wong.david-c@epa.gov), U.S. Environmental Protection Agency

## Description of model issue
As described in [Appendix A](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#science-options), the runtime variable 'KZMIN' may be set to Y/N to control the minimum eddy diffusivity in each grid cell. Depending on the option set, CCTM does or does not require percent urban land-use fraction (PURB) data from the GRID_CRO_2D meteorology file to calculate the minimum eddy diffusivity in each grid cell. Recent updates broke this design, causing CCTM to always require PURB data from the GRID_CRO_2D meteorology file, regardless of KZMIN setting. Hence, even if KZMIN was appropriately set, the model would crash if PURB data was not available. 

### Solution in CMAQv5.3.3
Routine CIO within CCTM was updated to not read PURB data from the GRID_CRO_2D meteorology file but instead assume PURB = 0.0 everywhere, if the correct KZMIN setting is set. 

Heterogeneous production of nitric acid (HONO) from the interaction of NO2 on ground surfaces (controlled by runtime variable [CTM_SFC_HONO](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#6104-nitrous-acid-hono)) is dependent on PURB data (even if KZMIN is set appropriately). If PURB data is not read in and assumed to be 0.0 (controlled by appropriate KZMIN setting), users should expect lower predicted HONO as described in [Chapter 6](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md#6104-nitrous-acid-hono).

## Files Affected
CCTM/scripts/run_cctm_2010_4CALIF1.csh    
CCTM/scripts/run_cctm_2011_12US1.csh    
CCTM/scripts/run_cctm_2014_12US1.csh    
CCTM/scripts/run_cctm_2015_HEMI.csh    
CCTM/scripts/run_cctm_2016_12US1.csh    
CCTM/scripts/run_cctm_Bench_2011_12SE1.csh    
CCTM/scripts/run_cctm_Bench_2016_12SE1.csh    
CCTM/src/cio/centralized_io_module.F    


