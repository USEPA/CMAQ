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

Using a 2-D and/or 3-D Gridded Emission File with representative day format specified via runscript with the environmental variable [GR_EM_SYM_DATE_XXX](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#offline-emissions-configuration). set to T, will prompt the centralized i/o module to store the start date of the file. However a memory issue related to the implementation of 2-D or 3-D Emission file is a representative day type will occur. This issue stems from the choice to psuedo-interpolate the initial conditions (ICs) instead of extracting them directly, which is problematic if an emissions file is a 2-D or 3-D Emissions file of representative type. This is because to linearly interpolate temporally, two points are required stored as the head and tail. The head was stored correctly, but the tail was not being stored correctly and was picking up from whatever was in memory last, which usually is the 2-D/3-D Gridded Emissions file. This resulted in an error when trying to extract the data at the second point as the IC file only has data at the head.

If no emissions are present, it would pick the point from whatever file was read last whether that be a MET file, bioseason file,  lightning file or IC file. 

It should be noted, this issue would have not been seen if the IC file were time independent as IOAPI ignores the time input when trying to extract data from time independent files.

### Solution in CMAQv5.3.2

The memory issue relating to the tail of the circular buffer is now correctly implemented. The head and tail of this file is now automatically set to the start date and time of the simulation for all species.

### Files Affected 
CCTM/src/cio/centralized_io_module.F

## 5. Bugfix for Runscript Variable EMIS_SYM_DATE
[David Wong](mailto:dwongepa@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

The second issue is the inability to run with the EMIS_SYM_DATE flag due inconsistent implementation between cio and the emissions modules. The EMIS_SYM_DATE flag has subsequently been removed from the runscripts to avoid confusion as the behavior of this flag is unconventional and cannot be explained in a concise manner. To learn more about this flag please visit [Appendix A](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md).

### Solution in CMAQv5.3.2

EMIS_SYM_DATE is now consistent beween both DESID and CIO. The implementation flows the following logic: Using EMIS_SYM_DATE you can change the default behavior of a stream that you have. E.g. If you set EMIS_SYM_DATE to TRUE, the default for every stream will be TRUE (meaning it will expect each stream to be of representative day type) – you can still override this behavior for specific streams via the individual streams GR_EM_SYM_DATE variable. For further information you can look here

### Files Affected 
CCTM/src/cio/centralized_io_module.F


## 6. Bug fix and code cleanup to lightning NOx production
[Daiwen Kang](mailto:kang.daiwen@epa.gov), U.S. Environmental Protection Agency

### Description of model issue
The changes (1) remove redundant calculations related to mapping lat/lon locations to model grid cells - these are no longer needed, (2) make the code better conform with the CIO implementation in v5.3 and on-wards, and (3) remove redundant calculations. These changes do not impact model calculations.

### Solution in CMAQv5.3.2
The code changes fixed a bug that previously would prevent the CMAQ model from running with lightning NOx using non-lambert projection grid (such as HCMAQ with polar map projection). These changes don’t impact model results.

### Files Affected 
CCTM/src/emis/emis/LTNG_DEFN.F

