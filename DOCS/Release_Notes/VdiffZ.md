# Changed ACM2 PBL Model _vdiff_ to Use Z-Coordinates
 
[Jonathan Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency
 
## Brief Description
Changed the ACM2 PBL model _vdiff_ to use Z-coordinates rather than sigma-coordinates as in the previous model. This makes the ACM2 compatible with the new hybrid vertical coordinates that are the default in WRF version 4.0 and beyond.  This also makes the ACM2 version in CMAQ consistent with the ACM2 versions in the latest versions of WRF and MPAS.
 
## Significance and Impact

The ACM2 is a one-dimensional PBL model that was originally derived using Cartesian (Z) coordinates. When it was first applied to MM5 and WRF, it was coded in sigma coordinates. In CMAQ, it was coded in generalized vertical coordinates which were essentially sigma when run with MM5 or WRF.  Recently, with the new hybrid vertical coordinate in WRF, CMAQ automatically conforms to this coordinate system through the generalized coordinates.  However, the calculation of the convective mixing rates (Equation 9 in Pleim, 2007) is slightly different in sigma versus Z coordinates.  Starting with WRFv4.0, the ACM2 has been changed to Z-coordinates, which makes the ACM2 calculations consistent between WRF, MPAS, and with the original derivation in Pleim (2007).  The CMAQ version is now also in Z-coordinates.
 
## Affected Files
#### Files modified:
 CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F 
 
 CCTM/src/vdiff/acm2_m3dry/vdiffproc.F 
 
 CCTM/src/vdiff/acm2_stage/vdiffacmx.F
 
 CCTM/src/vdiff/acm2_stage/vdiffproc.F
 
 
## References

Pleim, J. E. (2007). A combined local and nonlocal closure model for the atmospheric boundary layer. Part I: Model description and testing. _Journal of Applied Meteorology and Climatology_, **46**, 1383-1395.       

## Internal Records:
#### Relevant Pull Requests:
[PR #504](https://github.com/USEPA/CMAQ_Dev/pull/504)  


