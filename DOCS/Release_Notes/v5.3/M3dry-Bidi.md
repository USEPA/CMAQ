# New NH3 bi-directional flux linked to daily EPIC input 

**Author/P.O.C.:**, [Jonathan Pleim](mailto:pleim.jon@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

This new model for bi-directional surface flux of NH3 is closely linked to EPIC input. Daily input from EPIC of total NH4 in 1cm and 5cm soil layers, soil pH, soil moisture content, soil cation exchange capacity (CEC), and soil texture parameters for each of 42 crop types are read into CMAQ and used to compute the amount of NH4 in the soil available for volatilization and the soil NH3 compensation concentration. Bi-directional NH3 flux is computed from the soil compensation concentration (the maximum of the 1cm or 5cm layer) according to the resistance model described in Pleim et al (2013).  

Hourly net bi-directional NH3 flux is output in the DRYDEP file (NH3) where negatives values indicate net upward flux.  Two output fields have been added to the DRYDEP file for NH3 dry depositon (NH3_DDEP) and NH3 emissions (NH3_EMIS).

Note that mosaic dry deposition output capability has been removed.  Subgrid dry deposition fluxes by land use type could be calculated by a postprocessor

## Significance and Impact
This new NH3 bi-directional flux model results in substantial changes in NH3 concentrations and surface flux which depend on EPIC input data. The model is identical to the box model developed and evaluated compared to field flux measurements (Pleim et al 2013). Unlike previous versions, the new model relies on EPIC, which includes comprehensive C-N-P cycles and agricultural management, to model the soil biogeochemistry related to the soil ammonium (NH4) and its availability to volatilize on a daily basis.    

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

Removed files:

CCTM/src/depv/m3dry/MOSAIC_MOD.F

CCTM/src/depv/m3dry/opdepv_fst.F

CCTM/src/depv/m3dry/opdepv_mos.F

CCTM/src/vdiff/acm2/opddep_fst.F

CCTM/src/vdiff/acm2/opddep_mos.F

## References
Pleim, J.E., Bash, J.O., Walker, J.T., Cooter, E.J.; Development and testing of an ammonia bi-directional flux model for air-quality models, J. Geophys. Res.  118, doi:10.1002/jgrd.50262, 2013.

Williams J. R., 1995, The EPIC model, In: Singh VP (ed) Computer models in watershed hydrology, Water Resources Publications, Highlands Ranch: 909–1000.

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #315](https://github.com/USEPA/CMAQ_Dev/pull/315)
