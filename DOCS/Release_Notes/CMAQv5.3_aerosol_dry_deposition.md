# Aerosol Dry Deposition Algorithm Updated

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
In CMAQv5.2.1, dry deposition velocities--particularly for coarse-mode 
particles--were too high by a factor of 10-100 in some cases. Detailed testing revealed that the
algorithm in CMAQv5.2.1 was not suitable for coarse-mode particles, especially when the mode width (sigma) 
of the coarse mode approached 2.5, the upper bound. 

A revised algorithm is introduced in CMAQv5.3 to reduce the strong dependence on sigma and introduce a dependence on Leaf Area 
Index. This revision intends to capture the increased deposition hypothesized over forest canopies, etc., compared to 
bluff-body surfaces.

## Significance and Impact
The deposition of coarse-mode particles is strongly reduced in areas of the domain with high sigma. 
Deposition for the coarse mode is increased in areas with significant vegetation, like the southeast 
US. The deposition of Aitken-mode particles is increased substantially in highly vegetated areas.

## Affected Files
CCTM/src/vdiff/acm2_m3dry/aero_depv.F

## References
NA           

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #374](https://github.com/USEPA/CMAQ_Dev/pull/374)  

#### Commit 
IDs:                        
b40a9a76ed4afbfba5465a997f88925fe72514a6  
0c8bb116a5f42671b8c024070648951f911a5787  
d62aae1c56a777bee8c638f38d129f36071e9682  
1d1cfd67c016f4f45bbf96f391e76218ab5e3ceb  
e617b9fe7fe891ea6d3d775d55fb6d5d98aa5501  
8f7acbd800f20790aa012a330514f74fd1602a4f  

-----

