# ISAM bidi support
[Sergey Napelenok](mailto:napelenok.sergey@epa.gov) and [Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Support for bi-directional NH3 flux tagging with ISAM is now enabled. When ABFLUX=TRUE in the CMAQ runscript, ISAM generates a new tag "-BID" where it stores the contribution from the bidi emissions. This tag is written out in the ISAM output file with the other user-defined and automatically generated tags.

With this update, the STAGE deposition option now estimates the deposition velocity directly instead of diagnosed from the modeled flux and the estimated emission variable is now used directly in vdiffacmx.F rather than being a diagnostic output. Numerically, this is identical to CMAQ v5.3.1 but results in code that is simpler, less prone to numerical underflow errors, and results in better agreement between the sum of the ISAM sector sensitivities and bulk NH3 deposition.  

Some changes to centralized IO were also rolled into this update to enable additionally required IO. 

## Significance and Impact
This update allows for more strict mass balance accounting for reduced nitrogen species in ISAM when the ABLFUX option is selected. 


## Affected Files
CCTM/src/cio/centralized_io_module.F  
CCTM/src/emis/emis/EMIS_DEFN.F  
CCTM/src/isam/SA_DEFN.F  
CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F  
CCTM/src/depv/stage/STAGE_MOD.F
CCTM/src/vdiff/acm2_stage/vdiffacmx.F  

## Relevant Pull Requests:
[PR #620](https://github.com/USEPA/CMAQ_Dev/pull/620)
[PR #655](https://github.com/USEPA/CMAQ_Dev/pull/655)

## Commit IDs:


