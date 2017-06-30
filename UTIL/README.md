CMAQ Utilities 
========

## Overview
With the exception of Bldmake, the following utility programs prepare chemical mechanism source code for the CMAQ programs. Bldmake is the general model builder that is used to generate Makefile and compile CMAQ source code.  The other programs generate chemical mechanism and chemistry solver source code based on a new or modified mechanism definition file. Details about how to add new or modified chemical mechanisms to CMAQ are provided in [Chapter 9 of the CMAQ Operational Guidance](../CCTM/docs/User_Manual/CMAQ_OGD_ch09_grid_defn.mod).

## Utility Programs

* **bldmake**: CMAQ Makefile generator and model buildewr
* **chemmech**: generates chemical mechanism input files for the CMAQ programs from a mechanism definition file
* **create_ebi**: creates mechanisms-dependent EBI chemistry solver source code 
* **inline_phot_preproc**: creates photolysis reaction parameter tables for the CCTM inline photlysis module
* **nml**: converts chemical mechanism csv output files from chemmech to the namelist files required by the CMAQ programs
 
