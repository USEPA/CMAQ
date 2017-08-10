CMAQv5.2 Known Issues 
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.2

## *CMAQv5.2-i1:* Allow longer strings for CTM_LOGs
Date: 2017-8-10
Contact: Ben Murphy (murphy.benjamin@epa.gov)

### Description  
The CCTM outputs log files for each processor. The names of these files are concatenated with the following format: CTM_LOG_[Processor #]_[User String] where the "User String" is defined by the variable CTM_APPL in the CCTM runscript. Currently, the CCTM source code truncates CTM_APPL at 48 characters, limiting the usefulness of this name for long strings. Follow the steps below to allow for longer names.

### Scope and Impact
This issue only effects the names of Log files, not results or runtimes for CMAQ.

### Solution
In CCTM/src/util/util/setup_logdev.F, edit line 53:
```
CHARACTER( 48 ) :: EQNAME
```
Increase the length of EQNAME to any desired number (e.g. 200). Note that the legnth of IOLOGEQ on line 56 should be at least 11 characters greater than the length of EQNAME. Don't exceed the Linux maximum of 255 total characters for IOLOGEQ.

