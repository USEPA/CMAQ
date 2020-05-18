#ISAM bidi support
Sergey Napelenok and Jesse Bash, U.S. Environmental Protection Agency

##Brief Description
Support for bi-directional NH3 flux tagging with ISAM is not enabled. When ABFLUX=TRUE, in the CMAQ runscript, the ISAM code generates a new tag "-BID" where it stores the contribution from the bidi emissions.

This update also requires a change to the io module in order to run the split emissions for the 2016 NADP case.

The following script was used for the testing simulations:
/work/MOD3EVAL/nsu/isam_v531_bidi/CCTM/scripts/run_cctm_2016_12US1_NADP.csh

Some changes to centralized IO were also rolled into this update to enable additionally required IO. 

##Significance and Impact
This update allows for more strict mass balance accounting for reduced nitrogen species in ISAM when the ABLFUX option is selected. 


##Affected Files
CCTM/src/cio/centralized_io_module.F
CCTM/src/emis/emis/EMIS_DEFN.F
CCTM/src/isam/SA_DEFN.F
CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F
CCTM/src/vdiff/acm2_stage/vdiffacmx.F

##Relevant Pull Requests:
620

##Commit IDs:


