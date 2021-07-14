
# Operational Branch HONO Deposition Fix for M3DRY

[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency

## Brief Description
HONO dry deposition flux in the EQUATES runs is negative. Similar negative values are found in both m3dry and STAGE results with CMAQv53 and may affect the results of other CMAQ runs. This pull request fix HONO dry deposition with the M3DRY deposition option. Jesse has submitted pull requests (740 and 741) to address the STAGE deposition option. 

Note: This pull request address issue #724 (HONO dry deposition flux for the M3DRY deposition option in the operational branch). 
 
## Significance and Impact

Testing was done for the 2011 benchmark domain with the M3DRY option. Predicted HONO dry deposition with existing model can be negative while HONO dry deposition with the revised model is positive. Dry depositions of other chemical species and model concentrations are unaffected by the changes. It also removes the negative HONO deposition in the ISAM output.

## Affected Files

CMAQ/CCTM/CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F

## References:
None

-----
## Internal Records
#### Relevant Pull Requests:
[PR #745](https://github.com/usepa/cmaq_dev/pull/745)
#### Commit IDs:
3c00f7a3036730161a81ab9f10c0406bffdffef7
35ad093fe6af741034775bc8312b4eb990f61637

-----
