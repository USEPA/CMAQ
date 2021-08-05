
# Research Branch HONO Deposition Fix for M3DRY

[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency

## Brief Description
HONO dry deposition flux in the EQUATES runs is negative. Similar negative values are found in both m3dry and STAGE results with CMAQv53 and may affect the results of other CMAQ runs. This pull request fix HONO dry deposition with the M3DRY deposition option. Jesse has submitted pull requests (740 and 741) to address the STAGE deposition option. 

Note: This pull request address issue #724 (HONO dry deposition flux for the M3DRY deposition option in the research branch). 
 
## Significance and Impact

Testing was done for the 2011 benchmark domain with the M3DRY option. Predicted HONO dry deposition with existing model can be negative while HONO dry deposition with the revised model is positive. Dry depositions of other chemical species and model concentrations are unaffected by the changes. It also removes the negative HONO deposition in the ISAM output.

## Affected Files

CMAQ/CCTM/CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F

## References:
None

-----
## Internal Records
#### Relevant Pull Requests:
[PR #744](https://github.com/usepa/cmaq_dev/pull/744)
#### Commit IDs:
385d16ca738fec5196bf718b8417c1c75253aed2
ae081918a4d729917cae37fa062018beed617f39

-----
