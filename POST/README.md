Post-processing Tools
========

##Overview
The following utility programs are provided to process and prepare data for model evaluation.  Documentation for each utility is provided in the README files within each subdirectory.  Note that in previous CMAQ releases these utlities have been located under "models/TOOLS".
Additional updates to these utilities are scheduled to conicide with the release of CMAQv5.2 in June of 2017.

##Utility Programs
* **appendwrf**:  user can concatenate variables from multiple WRF input or output files into a single file along the time (unlimited) dimension.
* **bldoveraly**:  user can create an observation overlay file that can be imported into either PAVE or VERDI.
* **block_extract**: user can extract a time series of 1 or more variables from 1 or more (up to 99) IOAPI files for a specified range of cells.
* **combine**: user can combine species from raw CMAQ output files or wrfout input files into a new IOAPI output file.  Species can  be aggregated or transformed into variables of interest (i.e. to match observed quantities from a specific monitoring network).
* **hr2day**: user can create gridded IOAPI files with daily values (e.g. daily sum, daily max 8hr average, etc.) from gridded IOAPI files containing hourly values.  Daily values can be computed using GMT or LST.
* **merge_aqs_species**: user can create a merged AQS data file from pre-generated files posted on the EPA's AQS website in a format that is compatible with the sitecmp and sitecmp_dailyo3 programs.
* **sitecmp**: user can generate a csv (comma separated values) file that compares CMAQ generated concentrations with an observed dataset.
* **sitecmp_dailyo3**: user can generate a csv (comma separated values) file that compares various daily ozone metrics computed from hourly CMAQ generated and observed ozone concentrations.
* **writesite**: user can generate a csv file from an IOAPI data file for a set of species at defined site locations.
 
