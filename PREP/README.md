Pre-processing Tools
========

## Overview
The following programs are provided to process and prepare input data for the CMAQ Chemistry Transport Model.  Documentation for these programs is provided in the [CMAQ Operational Guidance](../CCTM/docs/User_Manual).  

## Pre-processing Programs
* **Calmap (agdust)**:  produces gridded planting start dates, planting end dates, and harvesting end dates for different crop types for estimating the impacts of agricultural activities on windblown dust emissions
* **bcon**: prepares lateral chemical boundary conditions (BCs) for CCTM from either ASCII vertical profiles or from an existing CCTM output concentration (CONC) file
* **icon**: prepares chemical initial conditions (ICs) for CCTM from either ASCII vertical profiles or from an existing CCTM output concentration (CONC) file
* **jproc**: calculates daily clear-sky photolysis rates from look-up tables of molecular absorption cross-section and quantum yield (CSQY) data, and climatologically derived ozone-column and optical depth data
* **mcip**: processes meteorological model output from either MM5 or WRF-ARW model into I/O API-formatted files that are compatible with CMAQ and SMOKE
* **wbdust**: 
 
