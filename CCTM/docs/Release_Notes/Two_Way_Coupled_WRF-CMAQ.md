# Two-way Coupled Meteorology Chemistry WRF-CMAQ    

**Author/P.O.C.:**, [David Wong](mailto:wong.david-c@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The CMAQv5.2 Two-Way Model is an online meteorology-chemistry model that simulates the two-way feedback between meteorology and chemistry in a single simulation. Coupled with the Weather Research Forecast Model version 3.8 (WRFv3.8), the CMAQv5.2 Two-Way Model, or WRF-CMAQ, simulates the interactions of estimated aerosol mass on incoming shortwave radiation.

The current release of the WRF-CMAQ model supports only the RRTMG radiation scheme for short wave aerosol direct effect. It does not simulate the effects of aerosols on long wave radiation and cannot be used with the CAM radiation scheme. This release also uses a core-shell model to perform the aerosol optics calculation rather than the volume mixing technique used in the previous version of WRF-CMAQ. This version of the model also only supports the CB05 chemical mechanism and AE6 aerosol mechanism (cb05tucl_ae6_aq).

## Build Instructions

A twoway model tarball, twoway.tar.gz, is required to construct the twoway. This file can be download at the ftp site: ftp://newftp.epa.gov/exposure/CMAQ/V5_2/WRF-CMAQ_Coupled_Model/. A build instruction is included.


## Run Instructions
A sample run script, run.wrf38_cctm52, and a test dataset can be found in the same ftp site. 

## WRF-CMAQ Input/Output Data

The WRF-CMAQ benchmark data provide examples of the files needed to run the model. The general list of inputs required for WRF-CMAQ include,

* REAL outputs
required: wrfbdy, wrfinput, wrflowinp
optional: wrffdda, wrfsfdda, wrfrstrt
* CMAQ inputs
required: emissions (CB05ae6 speciation), IC, BC, OMI, ocean file
optional: lightning NOx, gridded landuse for inline biogenics and windblown dust

WRF-CMAQ outputs standard WRF (wrfout) and CMAQ output files.

## References:

Wong, D. C., Pleim, J., Mathur, R., Binkowski, F., Otte, T., Gilliam, R., Pouliot, G., Xiu, A., Young, J. O., and Kang, D.: WRF-CMAQ two-way coupled system with aerosol feedback: software development and preliminary results, Geosci. Model Dev., 5, 299-312, doi:10.5194/gmd-5-299-2012 , 2012.

For an overview of the 2-way Coupled WRF-CMAQ see: http://www.cmascenter.org/conference/2011/slides/mathur_overview_two-way_2011.pptx

and for more details on the 2-way Coupled WRF-CMAQ system see: http://www.cmascenter.org/conference/2011/slides/wong_wrf-cmaq_two-way_2011.pptx

-----
