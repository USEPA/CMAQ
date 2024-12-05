Pre-processing Tools
========

## Overview
The following programs are provided to process and prepare input data for the CMAQ Chemistry Transport Model.  Documentation for these programs is provided in README files within each foler.  

## Pre-processing Programs
* **[bcon](bcon/README.md)**: Prepares lateral chemical boundary conditions (BCs) for CCTM from either ASCII vertical profiles or from an existing CCTM output concentration (CONC) file
* **[icon](icon/README.md)**: Prepares chemical initial conditions (ICs) for CCTM from either ASCII vertical profiles or from an existing CCTM output concentration (CONC) file
data
* **[mcip](mcip/README.md)**: Processes meteorological model output from either MM5 or WRF-ARW model into I/O API-formatted files that are compatible with CMAQ and SMOKE
* **[create_omi](create_omi/README.md)**: Produces an OMI input file that supports CMAQ CCTM's in-line calculation of photolysis rates.
* **[dmschlo](../PYTOOLS/dmschlo/README.md)**: Prepares OCEAN file with DMS and CHLO variables required by DMS and halogen chemistry.
* **[shp2cmaq](../PYTOOLS/shp2cmaq/README.md)**: Creates a CMAQ-ready grid mask from a shape file that can be used for defining regions and region families with DESID and using geographic source regions when running CMAQ-ISAM. 
