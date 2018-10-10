# Updates to post-processing utilities
    
**Author/P.O.C.:**, [Kristen M. Foley](mailto:foley.kristen@epa.gov), Computational Exposure Division, U.S. EPA    
    
## Brief Description

* **bldoverlay** - Updated the time zone calculation for bldoverlay to fix a bug in the previous version and to add the option of using 17 instead of 24 8hr ozone values to compute the daily maximum 8hr ozone concentration. When running the updated program, a new environment variable "TZFILE" has to be defined. The default value for TZFILE is tz.csv and should refer to the same file used by hr2day.  When invoking the new option to use 17 instead of 24 8hr ozone values to compute the daily maximum 8hr ozone concentration, only 8hr averages with starting hours between 7 and 23 local standard time are considered for determining the daily maximum 8hr ozone concentration. If fewer than 13 non-missing 8hr values are present in this time window, a missing value is returned. The option is controlled by the new environment variable HOURS_8HRMAX. Valid values are 24 (default, old option) and 17. If the environment variable is set to other values, the program will exit with an error message.

* **block_extract** - Added a new utility block_extract that allows user to extract specified variables from a specified range of cells in an IOAPI file and write as text file.

 * **combine** - 
  Implemented two minor bug fixes. The previous version of the code did not correctly implement the optional start/end settings from the spec_def file. As a result, combine produced outputs for the entire time period common to all input files even if a later start date was specified in the spec_def file. This has been addressed in the updated code. Furthermore, the code was updated to pad the character strings defining the required WRF dimension names. The previous version of the code did not add trailing spaces in the statement defining the strings for the required WRF dimensions ('west_east', 'south_north', 'bottom_top', and 'Time' for 4D variables, 'west_east', 'south_north', and 'Time' for 3D variables). This did not comply with F90 standard and led to an error message when compiling the code with some older versions of the GNU fortran compiler. This issue was first noted in a post on m3list and the solution implemented here was provided in a follow-up post by Dr. Carlie J. Coats, Jr.  

* **hr2day** - 
 Implemented two bug fixes and added the option of using 17 instead of 24 8hr ozone values to compute the daily maximum 8hr ozone concentration. When invoking the new option, only 8hr averages with starting hours between 7 and 23 local standard time are considered for determining the daily maximum 8hr ozone concentration. If fewer than 13 non-missing 8hr values are present in this time window, a missing value is returned. The changes were implemented for the 8hrmax, @8hrmaxO3, and hr@8hrmax operators supported by hr2day. The option is controlled by the new environment variable HOURS_8HRMAX. Valid values are 24 (default, old option) and 17. If the environment variable is set to other values, the program will exit with an error message. The bug fixes corrected problems in the previous version of the code when computing values in the eastern hemisphere as well as problems when using the operation "@8HRMAXO3" for a given variable, i.e. when the user wants to calculate the variable value averaged over the time period when the daily maximum 8-hr ozone concentration was simulated.

* **sitecmp** - 
 Updated the code to increase the maximum number of species for each subtype of species names (GAS, AERO, WETDEP, WETCON, PREC, CHAR) from 50 to 300. The total number of species remains 300. With the change, all 300 species can be GAS_N or AERO_N or PREC_N or a combination thereof as long as the total number of species does not exceed 300.
 
* **sitecmp_dailyo3** - Added the option of using 17 instead of 24 8hr ozone values to compute the daily maximum 8hr ozone concentration. When invoking the new option, only 8hr averages with starting hours between 7 and 23 local standard time are considered for determining the daily maximum 8hr ozone concentration. If fewer than 13 non-missing 8hr values are present in this time window, a missing value is returned. The option is controlled by the new environment variable HOURS_8HRMAX. Valid values are 24 (default, old option) and 17. If the environment variable is set to other values, the program will exit with an error message. 

* **writesite** -  Updated the code to add support for polar stereographic map projections and to increase the maximum allowed number of species from 120 to 2048.
 
## Significance and Impact
    
Changes were made to post-processing utilities and have no impact on CCTM run time or output.
    
## Affected Files:
    
* POST/bldoverlay/src/bldoverlay.F
* POST/bldoverlay/src/getTZ.F
* POST/bldoverlay/src/parser.F
* POST/combine/src/module_file.F
* POST/combine/src/module_specdef.F
* POST/hr2day/src/hr2day.F
* POST/sitecmp/src/module_spec.F
* POST/sitecmp_dailyo3/module_envvar.F
* POST/sitecmp_dailyo3/process.F
* POST/writesite/src/module_spec.F

## References:    

None

-----
## Internal Records:
    
### Relevant Pull Requests:


### Commit IDs:
    

    
