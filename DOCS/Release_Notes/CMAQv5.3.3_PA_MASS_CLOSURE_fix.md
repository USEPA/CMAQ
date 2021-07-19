# Resolve error in Process Analysis reintialization after aerosol processing

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description

This bugfix resolves a problem demonstrated by a user where mass is not conserved in process analysis rates for species that have non-negligible aerosol condensation. The difference between the change in instantaneous concentrations and the sum of the process rates looked very much like (but not exactly equivalent to) the contribution from condensation. It turns out that the state of the concentration field was not saved after the aerosol process analysis subroutine (pa_update_aero) and so when the change due to vdiff in the next iteration of sciproc was calculated, it artificially included the impact of condensation/evaporation as well. This means that cond/evap were double-counted for every iteration of sciproc except for the first one of an output time step, meaning that the residual disappeared if the output time step were sufficiently small to ensure only one pass through sciproc (e.g. 5 minutes).

The main fix is in pa_update_aero and involves the calculation of csav. Other fixes that were applied including real 8 conversion in aero_subs, and the subs_data_copy in pa_update_aero did not have a detectable impact on process analysis results but are godd coding practice and consistent with existing approaches for other processes, respectively. The assignment of PA_EM_CONV was determined to lead to a seg fault when run with gcc in debug mode so this was revised as well.

## Significance and Impact

This update does not affect CMAQ predictions. It resolves a mass closure issues and ensures consistency in output.

## Affected Files

CCTM/src/MECHS/cb6r3_ae7_aq/pa_cb6r3_ae7_aq.ctl  
CCTM/src/aero/aero6/aero_subs.F   
CCTM/src/driver/sciproc.F   
CCTM/src/procan/pa/pa_update.F


## References

N/A

-----
## Internal Records

#### Relevant Pull Requests:

PR683

#### Commit IDs:

c0fb68150223b598123996f247c5e1d5c9bf7b58   
7cf1b3f627f27e94d832084a7daab7d766c89bf8   

-----
