# Changed ACM Cloud Model to Use Z-Coordinates

[Jonathan Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency

## Brief Description

ACM cloud model in CMAQv5.3 uses Z-coordinates rather than sigma-coordinates as in previous versions of CMAQ.  This change makes the ACM cloud model compatible with the new hybrid vertical coordinate that is the default in WRF starting with version 4.0.  This change allows ACM cloud to be used with either the sigma vertical coordinates or the hybrid vertical coordinates. 

## Significance and Impact
Without this change the ACM cloud model would be in error when using hybrid vertical coordinates.  The revised code conserves mass and results differ very slightly from the previous version using terrain-following (sigma) vertical coordinates (see Figure 1).  The small difference is due to a revised calculation of air density that includes the effect of humidity. While the New code gives almost the same results as the Base code for terrain-following coordinates, larger differences are expected for hybrid coordinates. 

![aso4j](vertical_profile_aso4j.PNG)

Figure 1: Vertical profiles of ASO4j on June 6, 2011 at 13Z.  "Base" is CMAQv5.2.1, and "New" is CMAQv5.3.    

## Affected Files
CCTM/src/cloud/acm_ae6/acmcld.f

CCTM/src/cloud/acm_ae6/convcld_acm.F

## References
## Internal Records:
#### Relevant Pull Requests:
[PR #354](https://github.com/USEPA/CMAQ_Dev/pull/354)
