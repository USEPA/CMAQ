# Allow Emissions Check to Accomodate Runs with No Point Source Files  

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description

The EMIS_SPC_CHECK subroutine does not set NPTSPC, the number emission species from point sources, if a simulation does not use point source emissions as in ongoing hemispheric simulations. The uninitialized variable causes CCTM to stall for some versions of the intel compiler used.

This pull request removes the problem by adding a line to initially set NPTSPC to zero.

## Significance and Impact

CMAQ will now run even if no point emission files are used as input.

## Affected Files:

CCTM/src/emis/emis/EMIS_DEFN.F  
UTIL/create_ebi/template_RXNSU_OPT/hrdriver.F  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #237](https://github.com/USEPA/CMAQ_Dev/pull/237)  

### Commit IDs:

d156b2afdc151bfb59155c87c6a6aef362194f3d  
314a516a637b77fdd9006af2706d49cfc3867072  
ebb83c4a38f53af6a8b2f2b952c669d7109830a2  
31daed89d228a4d4671d93b6a25eb58e9ebaec50  
e06bcec45b332a316bbdf6d057b49938aa7df7a9  
cc60b0ccf617256d5d71adbc9d040f653fefa510  
113cbe4ed6dd51a7a70272b2e76bc8d4692da9e5  
955743ac748d926604aed6d81afc26e0c380653b  
