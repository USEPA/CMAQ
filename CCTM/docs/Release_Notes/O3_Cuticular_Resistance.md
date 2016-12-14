# Updated Scaling of Ozone Cuticular Resistance

**Author/P.O.C.:**, [Jesse Bash](mailto:bash.jesse@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

This update implements the scaling of the O3 cuticular resistance between dry and wet values to account for physisorbed H2O on the cuticular surfaces (Altimir et al., 2006). This was not correctly implemented in CMAQv5.1 and the wet cuticular value was used only for vegetation wet from dew or precipitation.

## Significance and Impact

This update will result in lower O3 concentrations, particularly in the morning, by approximately 5%.

## Affected Files:  
cloud/acm_ae6/cldproc_acm.F  
cloud/acm_ae6/convcld_acm.F  
cloud/acm_ae6/rescld.F  
cloud/acm_ae6_mp/cldproc_acm.F  
cloud/acm_ae6_mp/convcld_acm.F  
cloud/acm_ae6_mp/rescld.F  
depv/m3dry/m3dry.F  
emis/emis/LTNG_DEFN.F  

## References:

Altimir et al., 2006

-----
## Internal Records:

### Relevant Pull Requests:
[PR #12](https:github.com/usepa/cmaq/pulls/12)  

### Commit IDs:
1c3acae905a03f8ef4db56ac8e4a7ce127010429  
d293062342b788ee14ccb147a772f47e68e8e134  
