# Changed hadv to ppm in both x and y
 
**Author/P.O.C.:** [Pleim](mailto:Pleim.jon@epa.gov), Computational Exposure Division, U.S. EPA
 
## Brief Description
 This change simplifies horizontal advection so that the ppm advection scheme (Colella and Woodward, 1984) is used sequentially for the X and Y directions in alternating order without the densiity weighting adjustment that was formerly used in the so-called "yamo" scheme.
 
## Significance and Impact
Removed the adjustment to horizontal velocities for the second component of horizontal advection by the ratio of initial air density / x or y component advected density. This was intended to partially correct for cross term errors when using alternating 1-D advection.
Now, rather than x-ppm followed by y_yamo and y_ppm followed by x_yamo, we have x-ppm then y ppm and y_ppm then x_ppm

Advantage is that it is now positive definite (no more floor files) and it is robust for changes in domain decomposition.
Disadvantage is that there may be greater cross-term errors that could induce small lumpiness.
 
## Affected Files
#### Files modified:
 
#### Files added:
 CCTM/src/hadv/ppm/hadvppm.F
 CCTM/src/hadv/ppm/advbc_map.F
 CCTM/src/hadv/ppm/hcontvel.F
 CCTM/src/hadv/ppm/hppm.F
 CCTM/src/hadv/ppm/rdbcon.F
 CCTM/src/hadv/ppm/x_ppm.F
 CCTM/src/hadv/ppm/y_ppm.F
 CCTM/src/hadv/ppm/zfdbc.f
#### Files deleted:
 
 
## References
Colella, P., & Woodward, P. R. (1984). The piecewise parabolic method (PPM) for gas-dynamical simulations. Journal of computational physics, 54(1), 174-201.
## Internal Records:
#### Relevant Pull Requests:
[PR #491](https://github.com/USEPA/CMAQ_Dev/pull/491)  
 
