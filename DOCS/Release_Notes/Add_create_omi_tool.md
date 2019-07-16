# *create_omi*: Creating the OMI Data File for Inline Photolysis

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

## Brief Description

A *create_omi* tool has been added to the PREP subdirectory to create the OMI input file that describes total ozone column density.
The file supports the inline calculation of photolysis rates. The tool also creates I/O API files for visualizing observations
and the OMI file's data. Please check the [README](../../../PREP/create_omi/README.md) file for *create_omi* for more information.

## Significance and Impact

The tool allows users to create an OMI data file for times beyond the period covered by the data file available in
the CMAQ repository, **CCTM/src/phot/inline/OMI_1979_to_2017.dat**. It also allow users to create a file with a different
spatial resolution than this file.

## Files Affected: 
PREP/create_omi/README.md and supporting image files  
PREP/create_omi/src/\*   
PREP/create_omi/scripts/\*   
