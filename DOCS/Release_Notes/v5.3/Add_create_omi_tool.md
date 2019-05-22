# create_omi: a tool to create the OMI data file for CMAQ in-line photolysis

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

A tool has been added to the PREP subdirectory that creates the OMI input file describing total ozone column density.
The file supports the CMAQ model's in-line calculation of photolysis rates. The tool also creates IOAPI files for visualizing observations
and the OMI file's data.

## Significance and Impact

The tool allows users to create an OMI data file for times beyond the period covered by the data file available in
the CMAQ repository, **CCTM/src/phot/inline/OMI_1979_to_2017.dat**. It also allow users to create a file with a different
spatial resolution than this file.

## Files Affected: 
PREP/create_omi/README.md and supporting image files  
PREP/create_omi/src/\*   
PREP/create_omi/scripts/\*   
