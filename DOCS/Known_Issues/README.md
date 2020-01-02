CMAQv5.3.1 Known Issues
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.3.1:


## *CMAQv5.3.1-i1:* 
Date: 12/19/2019
Contact: Sergey Napelenok (napelenok.sergey@epa.gov)

### Description  
CMAQ-ISAM overcontributes apportionment of secondary gaseous pollutants to the ICON and BCON tags.  As the result, contributions to other tracked sources is underestimated.  

### Scope and Impact
All secondary gas-phase species (O3, NOx, etc.) in CMAQ-ISAM v5.3 and v5.3.1.

### Solution
A modification to the ISAM chemistry algorithms is currenlty in testing.  It will be released in the near future. 
