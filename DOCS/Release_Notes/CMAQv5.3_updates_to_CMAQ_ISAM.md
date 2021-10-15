# Updates to CMAQ-ISAM
 
[Sergey L. Napelenok](mailto:napelenok.sergey@epa.gov), U.S. Environmental Protection Agency
 
## Brief Description
The Integrated Source Apportionment Method (ISAM) calculates source attribution information for user-specified ozone and particulate matter precursors within the CMAQ model. CMAQ-ISAM has been substantially updated for CMAQv5.3.

The updates include new algorithms to calculate source tracking through chemistry, cloud, and aerosol modules.  Various code improvements have also contributed to substantially faster run times for CMAQ-ISAM.
 
Furthermore, ISAM has been fully integrated into the base CMAQ model and can be utilized through a C preprocessor flag during compilation of the base code (-Disam). 

## Affected Files
#### Files modified:
Chemistry Module Files

Cloud Module Files

Aerosol Module Files

Emissions Module Files

Transport Module Files

ISAM Module Files

Run and Build Scripts

 
## References
Kwok, R.H.F, Napelenok, S.L., & Baker, K.R. (2013). Implementation and evaluation of PM2.5 source contribution analysis in a photochemical model. _Atmospheric Environment_, __80__, 398–407 https://doi.org/10.1016/j.atmosenv.2013.08.017.

Kwok, R.H.F, Baker, K.R., Napelenok, S.L., & Tonnesen, G.S. (2015). Photochemical grid model implementation of VOC, NOx, and O3 source apportionment. _Geoscientific Model Development_, __8__, 99-114. https://doi.org/10.5194/gmd-8-99-2015.

## Internal Records:
#### Relevant Pull Requests:

[PR #473](https://github.com/USEPA/CMAQ_Dev/pull/473)  
[PR #506](https://github.com/USEPA/CMAQ_Dev/pull/506)  
 
#### Commit IDs:
