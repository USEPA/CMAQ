# Changed Vdiff-ACM2 to use Z-coordinates
 
**Author/P.O.C.:** [Jon Pleim](mailto:pleim.jon@epa.gov), Computational Exposure Division, U.S. EPA
 
## Brief Description
Changed ACM2 PBL model (vdiff) to use Z-coordinates rather than sigma-coordinates as in the preveous model. This makes the ACM2 compatable with the new hybrid vertical coordinate that is the default in WRF starting with version 4.0.  This also makes the ACM2 version in CMAQ consistent with the ACM2 versions in the latest versions of WRF and MPAS.
 
## Significance and Impact

The ACM2 is a one-dimensional PBL model which was originally derived using Cartesian (Z) coordinates.  When it was first applied to MM5 and WRF, it was coded in sigma coordinates.    In CMAQ, it was coded in generalized vertical coordinates which were essentially sigma when run with MM5 or WRF.  Recently, with the new hybrid vertical coordinate in WRF, CMAQ automatically conforms to this coordinate system via the generalized coordinates.  However, the calculation of the convective mixing rates (Equation 9 in Pleim 2007) is slightly different in sigma versus Z coordinates.  In recent versions of WRF the ACM2 has been changed to Z-coordinates.  Therefore, to make the ACM2 calculations consistent with WRF and MPAS and with the original derivation in Pleim (2007).  The CMAQ version is now also in Z-coordinates.
 
## Affected Files
#### Files modified:
 CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F 
 CCTM/src/vdiff/acm2_m3dry/vdiffproc.F 
 CCTM/src/vdiff/acm2_stage/vdiffacmx.F
 CCTM/src/vdiff/acm2_stage/vdiffproc.F
 
 
## References

Pleim, J. E. (2007). A combined local and nonlocal closure model for the atmospheric boundary layer. Part I: Model description and testing. Journal of Applied Meteorology and Climatology, 46(9), 1383-1395.       

## Internal Records:
#### Relevant Pull Requests:
[PR #504](https://github.com/USEPA/CMAQ_Dev/pull/504)  


