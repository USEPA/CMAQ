CMAQv5.2.1 Known Issues
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.2.1

## *CMAQv5.2.1-i1:* Error in SpecDef files for the saprc07tb_ae6_aq, saprc07tc_ae6_aq, and saprc07tc_ae6nvPOA_aq mechanisms causes error when running the combine utility 
Date: 2018-12-18
Contact: Christian Hogrefe (hogrefe.christian@epa.gov) 

### Description  
BENZENE is defined twice in the species definition files for the saprc07tb_ae6_aq, saprc07tc_ae6_aq, and saprc07tc_ae6nvPOA_aq mechanisms.  These SpecDef files are located under CCTM/src/MECHS/ and are used for the SPECIES_DEF environment variable in the combine run script. The duplicated BENZENE definition causes the combine utility to fail with error:

    Variable name VNAME3D( 40 ) = “BENZENE” duplicates VNAME3D( 10 ) = “BENZENE” in file “OUTFILE”
    (some more variables set)
    Error creating netCDF variable BENZENE

### Scope and Impact
This will only impact users who are trying to use the combine utility on CMAQv5.2.1 output from a simulation that used the saprc07tb_ae6_aq, saprc07tc_ae6_aq, or saprc07tc_ae6nvPOA_aq chemical mechanism.

### Solution
Commenting out or deleting the second definition for BENZENE in the species definition files for saprc07tb_ae6_aq, saprc07tc_ae6_aq, and saprc07tc_ae6nvPOA_aq will resolve this issue, allowing the combine utility to run successfully.


## *CMAQv5.2.1-i2:* ICON and BCON fail to compile in v5.2.1
Date: 2018-12-21
Contact: Shawn Roselle (roselle.shawn@epa.gov) 

### Description  
In the CMAQv5.2.1 release, a change in "config_cmaq.csh" for the environment variable that specifies the FORTRAN compiler version was not propagated into the build and run scripts for ICON and BCON.  

### Scope and Impact
ICON and BCON did not compile with the build scripts released with v5.2.1.  Updated build and run scripts are now provided that will allow these utilities to be used. 

### Solution
Replace build and run scripts in PREP/icon/scripts/ and PREP/bcon/scripts/ in repository with the updated versions located in the folder CMAQv5.2.1-i2
