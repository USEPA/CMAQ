# Updates to post-processing utilities
    
**Author/P.O.C.:**, [Kristen M. Foley](mailto:foley.kristen@epa.gov), Computational Exposure Division, U.S. EPA    
    
## Brief Description

* **bldoverlay** - Updated the time zone calculation for bldoverlay to fix a bug in the previous version and to add the option to use 17 instead of 24 8hr ozone values to compute the daily maximum 8hr ozone concentration. When running the updated program, a new environment variable "TZFILE" has to be defined. The default value for TZFILE is tz.csv and should refer to the same file used by hr2day.  When invoking the new option to use 17 instead of 24 8hr ozone values to compute the daily maximum 8hr ozone concentration, only 8hr averages with starting hours between 7 and 23 local standard time are considered for determining the daily maximum 8hr ozone concentration. If fewer than 13 non-missing 8hr values are present in this time window, a missing value is returned.The new option was implemented for the 8hrmax, @8hrmaxO3, and hr@8hrmax operators supported by hr2day. The option is controlled by the new environment variable HOURS_8HRMAX. Valid values are 24 (default, old option) and 17. If the environment variable is set to other values, the program will exit with an error message.

 * **combine** - 
  1. Bugfix to correctly implement optional start/end settings from spec_def. The optional start/end date/time settings from the spec_def species definition file were ignored (more precisely, the start date and time as well as the end time were ignored, the end date was processed correctly). As a result, combine produced outputs for the entire time period common to all input files even if a later start date was specified in the spec_def file.
  2. Padded WRF file required dimension names. The previous code did not add trailing spaces in the statement defining the strings for the required WRF dimensions ('west_east', 'south_north', 'bottom_top', and 'Time' for 4D variables, 'west_east', 'south_north',
 and 'Time' for 3D variables). This did not comply with F90 standard and led to an error message when compiling the code with GNU fortran

* **hr2day** - 
 1. Added new option for 8hr daily max calculation. The new option only uses 17 instead of 24 8hr ozone values to compute the daily maximum 8hr ozone concentration. When invoking the new option, only 8hr averages with starting hours between 7 and 23 local standard time are considered for determining the daily maximum 8hr ozone concentration. If fewer than 13 non-missing 8hr values are present in this time window, a missing value is returned. The changes were implemented for the 8hrmax, @8hrmaxO3, and hr@8hrmax operators supported by hr2day Which option is used is controlled by the new environment variable HOURS_8HRMAX. Valid values are 24 (default, old option) and 17. If the environment variable is set to other values,the program will exit with an error message.
 2. As flagged in issue#61 (#61), the previous dimensioning of the hrValues and tvalues arrays (0:42,NCOLS,NROWS) was not appropriate for handling locations in the Eastern hemisphere where tzoffset becomes negative. This bug was confirmed by compiling with bounds checking and running a test for a hemispheric application. That test led to a crash with the first index of hrValues exceeding the speficied dimensions. The code was modified to use indexes -36:66, reflecting a) time zone offsets between -12 and 12 hours, b) additional optional HROFFSET constants between -24 and 24, c) 24 values for each day, and c) seven additional values at the end to compute the forward-looking 8-hr average window. Implementing this change and compiling with bounds checking led to a successful completion of the hemispheric test case. When comparing the output to the output from the previous version of the code compiled without bounds checking, differences appear in the Eastern Hemisphere while no differences were noted in the Western Hemisphere. Comparing outputs for the new code for a CONUS application against outputs from the old code compiled without bounds checking for the same CONUS applications shows no differences, confirming that the bug only affected locations in the Eastern hemisphere, unless non-zero HROFFSET values were used.
 3. Corrected the start date and start time specification for the reading of O3 values to be used in the maxO3Values subroutine that is invoked when the user selects the operation "@8HRMAXO3" for a given variable, i.e. when the user wants to calculate the variable value averaged over the time period when the daily maximum 8-hr ozone concentration was simulated. The previous code had a mismatch of 36 hours between the time specified in the "READ3" command and the hour index used in the O3values array storing the retrieved hourly ozone values.

* **sitecmp** - 
 1. Increased maximum number of species from 50 to 300. Changed a loop counter in module_spec.F to look for up to 300 species rather than up to 50 species as previously coded. The spec_var type was already dimensioned to hold up to 300 values but for each subtype of species names (GAS, AERO, WETDEP, WETCON, PREC, CHAR) only up to 50 values (e.g. AERO_1, ... AERO_50, ...GAS_1, ..GAS_50, ...PREC_1, ..) were expected. With the change, all 300 species can be GAS_N or AERO_N or PREC_N or a combination thereof as long as the total number of species is below 300.
 2. Changed error message for missing IN_TABLE file. The previous code printed "Cannot open site file" when IN_TABLE was not available. Changed this error message to "Cannot open IN_TABLE" to be more explicit.
 
* **sitecmp_dailyo3** - Added new option for 8hr daily max calculation. The new option only uses 17 instead of 24 8hr ozone values to compute the daily maximum 8hr ozone concentration. When invoking the new option, only 8hr averages with starting hours between 7 and 23 local standard time are considered for determining the daily maximum 8hr ozone concentration. If fewer than 13 non-missing 8hr values are present in this time window, a missing value is returned. Which option is used is controlled by the new environment variable HOURS_8HRMAX. Valid values are 24 (default, old option) and 17. If the environment variable is set to other values, the program will exit with an error message.

* **writesite** -  Added support for polar stereographic map projections. Increased the maximum number of species from 120 to 2048.
 
## Significance and Impact
    

    
## Affected Files:
    


## References:    

None

-----
## Internal Records:
    
### Relevant Pull Requests:


### Commit IDs:
    

    
