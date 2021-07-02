# CMAQv5.3.3 Bugfixes

## 1. Bugfix, clean-up, and added option in bldscript for distr_env.c
[Fahim Sidi](mailto:sidi.fahim@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Bug reported by Steve Fine, EPA-OAR, when using AWS to run CMAQ across multiple adjioned instances. The issue is related to blank environmental variables causing a segmentation fault on AWS when invoking the directive -Dcluster in the CCTM Makefile. Additionally, it was found that there was no bldscript options to invoke C code distr_env, causing users to manually invoke this option via editing CCTM Makefile to include CPP flag -Dcluster. 

The update also enables users on different architectures and systems that do not appended C routine names with an underscore to compile the CCTM code.

### Solution in CMAQv5.3.3

Changed distr_env.c to only set environmental variables on other processors that are not blank, which resolved the segmentation fault. To fix manual addition of CPP Flag -Dcluster, a new bldscript option within CCTM that allows users to optionally invoke distr_env.c is added. This new option is called "DistrEnv", if set this option adds the CPP flag -Dcluster. It should be noted, that two conditions have to be met for the -Dcluster flag to be activiated:

(1) DistrEnv is set
(2) ParOpt is set (indicates this an MPI run)

Since DistrEnv is strictly an MPI option (containing MPI commands) it is only needed if ParOpt is invoked. It has no use when running CMAQ serially.

The second part of this update cleans-up the C code and adds C-Fortran Interoperability (Feldman Style Binding) that is consistent with the CPP flag provided in the Makefile (-DFLDMN) to compile this code with other architectures & compilers that don't append C code with underscore.  

### Files Affected 
CCTM/scripts/bldit_cctm.csh<br>
CCTM/src/par/mpi/distr_env.c

## 2. POST tool bug fixes in hr2day and sitecmp_dailyo3
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

## 3. Updated bldmake & config_cmaq.csh to add mpi library in CCTM Makefile
[Fahim Sidi](mailto:sidi.fahim@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

Discrepancy reported by Liz Adams and Christos Efstathiou, CMAS, that CMAQ (namely CCTM only) didn’t have the capability to specify different paths to the mpi include files and mpi library directory in the config_cmaq.csh, both needed to compile CCTM. Instead to do this, you had to manually edit the Makefiles and recompile the model.

The update enables users to specify, explicitly, paths to the MPI Library and include directories. 

### Solution in CMAQv5.3.3

Changed config_cmaq.csh to include new variable MPI_INCL_DIR, consistent with treatment of other external libraries used in CMAQ (I/O API & netCDF). A change is also made in bldmake to reflect this updated variable. 

### Files Affected 
UTIL/bldmake/src/bldmake.f<br>
config_cmaq.csh 

## 4. Provide appropriate error message and abort if OMI photolysis file is missing
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The photolysis module reads a data file from NASA's Ozone Monitoring Instrument (OMI) describing total column ozone. The model attempted to read the file prior to the check whether the file had been successfully opened, leading to a crash. 

### Solution in CMAQv5.3.3
The check has been moved prior to the first attempt to read the file, and the model aborts with an appropriate error if the OMI file is not found.  There is no impact on model results in the normal case, where the OMI file is present.

### Files Affected 
CCTM/src/phot/inline/o3totcol.f


## 5. Revise how the photolysis module checks write time for diagnostics. 
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

## 6. Correct chemistry data for Reactive Tracers
[Bill Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
For the cbr3_ae6_aq and cb6r3m_ae6_kmtbr mechanisms, the available gas chemistry solvers contain errors 
in the loss reactions for several reactive tracers representing Hazardous Air Pollutants.

1.   Destruction by OH was double counted for acetonitrile, acrylic acid, carbon sulfide
ethyl benzene, hexane, methyl chloride, chloroprene and stryene.
2.   Also for styrene, the OH reaction had an activation energy of OH reactions
with the wrong sign based on Cho, J.; Roueintan, M.; Li, Z. J., Kinetic and Dynamic
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
CCTM/src/gas/ros3/degrade_data.F (_symbolic link to above file_)
CCTM/src/gas/ebi_cb6r3_ae6_aq/degrade_data.F
CCTM/src/gas/ebi_cb6mp_ae6_aq/degrade_data.F (_symbolic link to above file_)   
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/degrade_data.F
CCTM/src/gas/smvgear/degrade_data.F
CCTM/src/MECHS/cb6mp_ae6_aq/EmissCtrl_cb6mp_ae6_aq.nml


## 7. Remove Differences in Predictions between PHOTDIAG True and False. 
[Bill Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
For the inline option of the photolysis module, CCTM predictions can differ between when PHOTDIAG option equals True
and False. For example, small ozone differences exist (less than 0.00005 ppmV) in simulations over the 
hemispheric domain. Occurances appear dependent on the meteorological input files and minimum synchronization
time-step. They occur because the O3TOTCOL routine saves the observed global total ozone columns with their date 
and times from the last call. The saved information determines how to update data used to interpolate the observations 
to the domain's vertical columns.

### Solution in CMAQv5.3.3
The solution added calls to the O3TOTCOL routine when all columns are DARK and PHOTDIAG is false. 
When PHOTDIAG equals false, model predictions now match predictions when PHOTDIAG True.

### Files Affected 
CCTM/src/phot/inline/phot.F 

## 8. Correct O3 deposition to wet soil in the STAGE deposition option
[Jesse O. Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency

### Description of the model issue
The wet cuticular resistance would overwrite the wet soil resistance due to using the same variable names, rwet, for ozone. This resulted in slightly faster deposition of ozone to wet soil surfaces when the leaf cuticles were also wet than intended. 

### Solution in CMAQ v5.3.3
A new variable was created for wet cuticular surfaces, rcwet, in STAGE_MOD.F. This results in slightly higher, typically less than 1ppb, ozone values primarily at night, early mornings and during precipitation events when both cuticular and soil surfaces are wet. 

### Files Affected
CCTM/src/depv/stage/STAGE_MOD.F

## 9. Correct Mosaic Land Use Specific Dry Deposition Velocity Diagnostic Output

[Jesse O. Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency
### Description of the model issue
An indexing error prevented the output arrays of some land use specific species deposition velocities in the Mosaic output arrays to be populated and resulted in values 0 being written to file. This bug did not impact modeled dry deposition or ambient concentrations. 

### Solution in CMAQ v5.3.3
The indexing error was corrected, and land use specific deposition velocities are now correctly populated and written to the Mosaic deposition velocity file when CTM_MOSAIC is set to Y. 

### Files Affected
CCTM/src/depv/stage/MOSAIC_MOD.F
CCTM/src/depv/stage/STAGE_MOD.F

## 10. Short description
[Firt Name Last Name](mailto:last.first@epa.gov), U.S. Environmental Protection Agency
