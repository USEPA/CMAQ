# Fix to Gravitational Settling Sub-Time-Step Calculation

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
The gravitational settling algorithm in the vertical dispersion module allows
large particles (coarse mode) to transport to lower grid cells with the force of gravity. It had 
been noted in CMAQv5.2.1 and earlier that the algorithm did not conserve mass for certain 
aerosol size distribution parameter combinations. These conditions were observed to occur in the
highest-altitude model layers, but could propagate to other parts of the domain if they were large
enough.

Two updates are introduced in CMAQv5.3 to resolve these problems. First, the time step difference calculation has been 
corrected to give the remainder of the current sub-time-step and the master time step, which was
the intended result when the algorithm was introduced. Second, a ceiling on the number of iterations
used to calculate settling has been introduced, and is equal to 10 iterations. This way, if the 
algorithm becomes unstable, it will minimally disrupt the concentration field.

## Significance and Impact
Resolves infrequent mass accumulation errors on the order of 10-50% of the total coarse mode particle 
mass. Particularly important for hemispheric CMAQ simulations where errors can accumulate over time.

## Affected Files
CCTM/src/vdiff/acm2_m3dry/SEDIMENTATION.F

## References
NA           

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #378](https://github.com/USEPA/CMAQ_Dev/pull/378) 
[PR #381](https://github.com/USEPA/CMAQ_Dev/pull/381) 

#### Commit 
IDs:                        
5d4c5bd02777749801c37042aa056a76035a03f6  
1ad9196f341ad12e1c8ae8ad8a7e09e906f735eb  
-----

