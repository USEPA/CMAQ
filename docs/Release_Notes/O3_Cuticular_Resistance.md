# Updated Scaling of Ozone Cuticular Resistance

'''Author/P.O.C.:''', Jesse Bash, bash.jesse@epa.gov, Computational Exposure Division, U.S. EPA

## Brief Description 

This update implements the scaling of the O3 cuticular resistance between dry and wet values to account for physisorbed H2O on the cuticular surfaces (Altimir et al., 2006). This was not correctly implemented in v5.1 and the wet cuticular value was only used for vegetation wet from dew or precipitation. This update will result in lower O3 concentrations, particularly in the morning, by approximately 5%.

## Significance and Impact

(significance and impact on modeled results and runtime)

## Relevant Pull Requests: 
  [PR #12](/usepa/cmaq/pull/12)

## References: 

Altimir et al., 2006
