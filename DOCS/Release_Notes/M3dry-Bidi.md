# New NH<sub>3</sub> Bi-directional Flux in M3Dry Linked to Daily EPIC Input 

[Jonathan Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency

## Brief Description

This update to M3Dry for bi-directional surface flux of NH<sub>3</sub> is closely linked to EPIC input. M3Dry in CMAQv5.3 uses daily input from EPIC of total NH<sub>4</sub> in 1-cm and 5-cm soil layers, soil pH, soil moisture content, soil cation exchange capacity (CEC), and soil texture parameters for each of 42 crop types. These data are read into CMAQ and used to compute the amount of NH<sub>4</sub> in the soil available for volatilization and the soil NH<sub>3</sub> compensation concentration. Bi-directional NH<sub>3</sub> flux is computed from the soil compensation concentration (the maximum of the 1-cm or 5-cm layer) according to the resistance model described in Pleim et al. (2013).  

Hourly net bi-directional NH<sub>3</sub> flux is output in the DRYDEP file (NH3), where negatives values indicate net upward flux.  Two output fields have been added to the DRYDEP file for NH<sub>3</sub> dry depositon (NH3_DDEP) and NH<sub>3</sub> emissions (NH3_EMIS).

Note that mosaic dry deposition output capability has been removed.  Sub-grid dry deposition fluxes by land use type could be calculated by a postprocessor.

## Significance and Impact
This new NH<sub>3</sub> bi-directional flux model results in substantial changes in NH<sub>3</sub> concentrations and surface flux which depend on EPIC input data. The model is identical to the box model developed and evaluated compared to field flux measurements (Pleim et al., 2013). Unlike previous versions, the new model relies on EPIC, which includes comprehensive C-N-P cycles and agricultural management, to model the soil biogeochemistry related to the soil ammonium (NH<sub>4</sub>) and its availability to volatilize on a daily basis.    

## Affected Files
CCTM/src/depv/m3dry/ABFLUX_MOD.F

CCTM/src/depv/m3dry/DEPVVARS.F

CCTM/src/depv/m3dry/DEPV_DEFN.F

CCTM/src/depv/m3dry/LSM_MOD.F

CCTM/src/depv/m3dry/m3dry.F

CCTM/src/vdiff/acm2/ASX_DATA_MOD.F

CCTM/src/vdiff/acm2/opddep.F

CCTM/src/vdiff/acm2/vdiffacmx.F

CCTM/src/vdiff/acm2/vdiffproc.F

*Removed files:*

CCTM/src/depv/m3dry/MOSAIC_MOD.F

CCTM/src/depv/m3dry/opdepv_fst.F

CCTM/src/depv/m3dry/opdepv_mos.F

CCTM/src/vdiff/acm2/opddep_fst.F

CCTM/src/vdiff/acm2/opddep_mos.F

## References
Pleim, J.E., L. Ran, W. Appel, M. W. Shephard, and K. Cady-Pereira. (2019) New bidirectional ammonia flux model in an air quality model coupled with an agricultural model. Accepted at Jounal of Advances in Modeling Earth Systems.

Pleim, J.E., Bash, J.O., Walker, J.T., Cooter, E.J. Development and testing of an ammonia bi-directional flux model for air-quality models, *Journal of Geophysical Research-Atmospheres*, **118**, https://doi.org/10.1002/jgrd.50262, 2013.

Williams J. R., 1995. The EPIC model, In: Singh, V.P. (ed.), *Computer models in watershed hydrology*, Water Resources Publications, Highlands Ranch, 909–1000.

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #315](https://github.com/USEPA/CMAQ_Dev/pull/315)
