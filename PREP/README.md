Pre-processing Tools
========

## Overview
The following programs are provided to process and prepare input data for the CMAQ Chemistry Transport Model.  Documentation for these programs is provided in README files within each foler.  

## Pre-processing Programs
* **[Calmap (agdust)](agdust/README.md)**:  produces gridded planting start dates, planting end dates, and harvesting end dates for different crop types for estimating the impacts of agricultural activities on windblown dust emissions
* **[bcon](bcon/README.md)**: prepares lateral chemical boundary conditions (BCs) for CCTM from either ASCII vertical profiles or from an existing CCTM output concentration (CONC) file
* **[icon](icon/README.md)**: prepares chemical initial conditions (ICs) for CCTM from either ASCII vertical profiles or from an existing CCTM output concentration (CONC) file
data
* **[mcip](mcip/README.md)**: processes meteorological model output from either MM5 or WRF-ARW model into I/O API-formatted files that are compatible with CMAQ and SMOKE
* **[wbdust](wbdust/README.md)**: produces gridded MODIS FPAR data required by windblown dust emissions module.   
* **[create_omi](create_omi/README.md)**: produces an OMI input file that supports CMAQ CCTM's in-line calculation of photolysis rates.
 
