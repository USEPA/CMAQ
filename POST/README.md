Post-processing Tools
========

## Overview
The following utility programs are provided to process and prepare data for model evaluation.  Documentation for each utility is provided in the README files within each subdirectory.  Note that in previous CMAQ releases these utlities have been located under "models/TOOLS".

## Release Notes for version 5.2
The following link summarizes the main updates to these utility programs since the previous release (CMAQ v5.1).
* [Updates to post-processing tools in CMAQv5.2](../CCTM/docs/Release_Notes/Update_POST.md)

## Utility Programs
* **[appendwrf](appendwrf/README.md)**:  user can concatenate variables from multiple WRF input or output files into a single file along the time (unlimited) dimension.
* **[bldoverlay](bldoverlay/README.md)**:  user can create an observation overlay file that can be imported into either PAVE or VERDI.
* **[block_extract](block_extract/README.md)**: user can extract a time series of 1 or more variables from 1 or more (up to 99) IOAPI files for a specified range of cells.
* **[combine](combine/README.md)**: user can combine species from raw CMAQ output files or wrfout input files into a new IOAPI output file.  Species can  be aggregated or transformed into variables of interest (i.e. to match observed quantities from a specific monitoring network).
* **[hr2day](hr2day/README.md)**: user can create gridded IOAPI files with daily values (e.g. daily sum, daily max 8hr average, etc.) from gridded IOAPI files containing hourly values.  Daily values can be computed using GMT or LST.
* **[sitecmp](sitecmp/README.md)**: user can generate a csv (comma separated values) file that compares CMAQ generated concentrations with an observed dataset.
* **[sitecmp_dailyo3](sitecmp_dailyo3/README.md)**: user can generate a csv (comma separated values) file that compares various daily ozone metrics computed from hourly CMAQ generated and observed ozone concentrations.
* **[writesite](writesite/README.md)**: user can generate a csv file from an IOAPI data file for a set of species at defined site locations.

## A note on model-observation pairing for model evaluation
The task of matching model simulations to observations is performed by the sitecmp and sitecmp_dailyo3 utility programs.

### Spatial matching
* In sitecmp, model values are extracted for the grid cell containing the monitor location. In sitecmp_dailyo3 the model value of the grid cell containing the observation is provided, as well as the maximum model value of the 9 grid cells centered on the monitor location. These variables in the output file contain the character string "9cell" in the variable name.

### Temporal matching
* **AQS_HOURLY, CASTNET_HOURLY, SEARCH_HOURLY, AERONET**: Air quality observations are assumed to be hourly averages time stamped at the beginning of the hour with local standard time (LST). The sitecmp utility will use the time stamp from the observations to determine the matching model time step, accounting for the time zone of the monitor. Therefore, best practice would be for the model time step to also represent hourly average time stamped at the beginning of the hour. This can be accomplished by running the combine utility on the CMAQ "ACONC" output files which follow this convention. These networks also include meteorological measurements. Since meteorological observations are near instantaneous measurements (e.g. 1 or 5 minute averages), using meteorological fields from MCIP or wrfout in combine results in the correct matching since these fields are also instantaneous. One exception is the calculation of modeled relative humidity (RH). This variable is not available from MCIP or wrfout files but is stored in the CMAQ "APMDIAG" output file which represents hourly average values. This creates a slight inconsistency between observed and modeled values for this variable in the sitecmp output files. Note that modeled and observed precipitation for a given hour represents the hourly total rather than the hourly average. 
* **AQS_DAILY_O3, CASTNET_DAILY_O3**: sitecmp_dailyo3 computes various daily metrics from observed and modeled hourly ozone values. The temporal matching of the hourly observed and modeled values used in these computations follows the same approach described above for AQS_HOURLY. Therefore it is best practice to use output from CMAQ "ACONC" files for modeled ozone predictions. Details on the computation of the various daily metrics is provided in the sitecmp_dailyo3 documentation.
* **AQS_DAILY, CSN, IMPROVE, SEARCH_DAILY**: Air quality observations are daily averages time stamped with the date in local standard time. The sitecmp utility will use the date from the observations to compute daily averages using 24 hourly modeled values, accounting for the time zone of the monitor. Therefore it is best practice to use output from CMAQ "ACONC" files for modeled air quality predictions which represent hourly average concentrations.
* **CASTNET**: Air quality observations are weekly averages time stamped with beginning and end date and time of the weekly interval in local standard time. The sitecmp utility will use the start and end date and time from the observations to compute weekly averages using hourly modeled values, accounting for the time zone of the monitor. Therefore it is best practice to use output from CMAQ "ACONC" files for modeled air quality predictions which represent hourly average concentrations.
* **NADP**: Air quality observations are weekly sums time stamped with beginning and end date of the weekly interval in local standard time. The sitecmp utility will use the start and end date from the observations to compute weekly sums using hourly modeled values, accounting for the time zone of the monitor. Observations are matched to output from CMAQ "DEP" files which represent hourly totals.
