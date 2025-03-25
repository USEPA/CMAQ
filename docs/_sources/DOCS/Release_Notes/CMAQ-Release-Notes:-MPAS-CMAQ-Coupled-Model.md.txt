# MPAS-CMAQ Coupled Model

## New MPAS-CMAQ model
[Jeff Willison](mailto:willison.jeff@epa.gov), U.S. Environmental Protection Agency  
**Type of update:** New Feature  
**Release Version/Date:** CMAQv5.5  

**Description:** CMAQ simulations often rely on community-developed meteorological models developed and maintained by the National Center for Atmospheric Research (NCAR). NCAR has created the Model for Prediction Across Scales (MPAS) as the successor to the decades-old Weather Research and Forecasting model (WRF). NCAR has signaled that WRF development will wind down over the next decade. In response, EPA is preparing for a gradual transition of the CMAQ ecosystem from WRF to MPAS. Unlike WRF, MPAS supports global domains with seamless regional resolution refinement over areas of interest. CMAQ version 5.5 will be the first publicly available version of CMAQ’s chemical transport model that supports coupling with MPAS. 

MPAS-CMAQ code is available in [a branch of the CMAQ repository](https://github.com/USEPA/CMAQ/tree/MPAS_CMAQ) and includes new directories mio and mpas_cmaq in CCTM/src. The mio directory includes a prototype of an I/O system that removes dependence on I/O API. The mpas_cmaq directory includes a coupling interface for use with MPAS. For the initial release a configuration has been designed for use with the CRACMM2 mechanism on a 120 km uniform global mesh. Not all CMAQ features are supported at this time. See the [user guide](https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/PDF/CMAQ_UG_09_2019.pdf) for more information about downloading the required [MPAS code](https://github.com/USEPA/MPAS), [CMAQ code](https://github.com/USEPA/CMAQ/tree/MPAS_CMAQ), and [available inputs](https://mpas-cmaq.s3.amazonaws.com/index.html). The user guide also includes a description of available features and instructions for building and running the MPAS-CMAQ model.

     
![image](https://github.com/user-attachments/assets/fedc3e86-7af2-4759-9bdd-d687de9e1d5f)
  

**References**  
Wong, D., Willison, J., Pleim, J.E., Sarwar, G., Beidler, J., Bullock, O. R., Herwehe, J.A., Gilliam, R., Kang, D., Hogrefe, C., Pouliot, G., and Foroutan, H. (2024). Development of the MPAS-CMAQ Coupled System (V1.0) for Multiscale Global Air Quality Modeling. Geosci. Model Dev. Discussions (preprint), [https://doi.org/10.5194/gmd-2024-52](https://doi.org/10.5194/gmd-2024-52).

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1188](https://github.com/USEPA/CMAQ/commit/bdd6aeb895a42a2615f9d752ececb5bd96b1d303) | [PR#1188](https://github.com/USEPA/CMAQ_Dev/pull/1188)  |
