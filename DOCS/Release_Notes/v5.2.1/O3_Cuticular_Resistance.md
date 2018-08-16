# Updated Scaling of Ozone Cuticular Resistance

**Author/P.O.C.:**, [Jesse Bash](mailto:bash.jesse@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

This update implements the scaling of the O3 cuticular resistance between dry and wet values to account for physisorbed H2O on the cuticular surfaces (Altimir et al., 2006). In earlier version of CMAQ the wet cuticular value was used only for vegetation wet from dew or precipitation. This update has a relative humidity dependence where it is assumed that moisture will phsisorb to the cuticle at relative humidities greater than 70%. The resistance is linearly interpolated betwen dry and wet cuticle values between 70% and 100% RH. 

## Significance and Impact

This update will result in lower O3 concentrations, particularly in the morning, by approximately 5%.

## Affected Files:  
depv/m3dry/m3dry.F  

## References:

Altimir, N., Kolari, P., Tuovinen, J.P., Vesala, T., Bäck, J., Suni, T., Kulmala, M., Hari, P., 2006.  Foliage surface ozone  deposition:  a role for surface  moisture?  Biogeosciences 3, 1 e 20.: [link](http://www.biogeosciences.net/3/209/2006/bg-3-209-2006.pdf)

-----
## Internal Records:

### Relevant Pull Requests:
[PR #12](https:github.com/usepa/cmaq/pulls/12)  

### Commit IDs:
1c3acae905a03f8ef4db56ac8e4a7ce127010429  
d293062342b788ee14ccb147a772f47e68e8e134  
