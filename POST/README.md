Post-processing Tools
========

## Overview
The following utility programs are provided to process and prepare data for model evaluation.  Documentation for each utility is provided in the README files within each subdirectory.  Note that in previous CMAQ releases these utilities have been located under "models/TOOLS".

## Release Notes for version 5.3.2
The following link summarizes an update to the combine utility since the previous release (CMAQ v5.3.1).
* [New functions available in combine utility](../DOCS/Release_Notes/CMAQv5.3.2_add_ERF_and_SIGN_to_COMBINEs_grid_cell_functions.md)

## Utility Programs
* **[appendwrf](appendwrf/README.md)**:  user can concatenate variables from multiple WRF input or output files into a single file along the time (unlimited) dimension.
* **[bldoverlay](bldoverlay/README.md)**:  user can create an observation overlay file that can be imported into either PAVE or VERDI.
* **[block_extract](block_extract/README.md)**: user can extract a time series of 1 or more variables from 1 or more (up to 99) IOAPI files for a specified range of cells.
* **[calc_tmetric](calc_tmetric/README.md)**: user can create gridded IOAPI files with temporally averaged or summed values that were calculated from one or more gridded time-dependent IOAPI files.
* **[combine](combine/README.md)**: user can combine species from raw CMAQ output files or wrfout input files into a new IOAPI output file.  Species can  be aggregated or transformed into variables of interest (i.e. to match observed quantities from a specific monitoring network).
* **[hr2day](hr2day/README.md)**: user can create gridded IOAPI files with daily values (e.g. daily sum, daily max 8hr average, etc.) from gridded IOAPI files containing hourly values.  Daily values can be computed using GMT or LST.
* **[sitecmp](sitecmp/README.md)**: user can generate a csv (comma separated values) file that compares CMAQ generated concentrations with an observed dataset.
* **[sitecmp_dailyo3](sitecmp_dailyo3/README.md)**: user can generate a csv (comma separated values) file that compares various daily ozone metrics computed from hourly CMAQ generated and observed ozone concentrations.
* **[writesite](writesite/README.md)**: user can generate a csv file from an IOAPI data file for a set of species at defined site locations.

## Observed data for model evaluation
The formatted observation data files needed for running the sitecmp and sitecmp_dailyo3 utilities are available for 2000 through 2017 from the CMAS Center Data Clearinghouse under the heading "2000-2017 North American Air Quality Observation Data": https://www.cmascenter.org/download/data.cfm.

## A note on model-observation pairing for model evaluation
The task of matching model simulations to observations is performed by the sitecmp and sitecmp_dailyo3 utility programs. Documentation on how these programs handle matching model and observed species in space and time is provided in [Chapter 8 of the CMAQ User's Guide](../DOCS/Users_Guide/CMAQ_UG_ch08_analysis_tools.md).

