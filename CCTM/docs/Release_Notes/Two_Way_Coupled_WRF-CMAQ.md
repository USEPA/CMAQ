# Two-way Coupled Meteorology Chemistry WRF-CMAQ    

**Author/P.O.C.:**, [David Wong](mailto:wong.david-c@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The new two-way coupled WRF-CMAQ model which is based on WRF 3.8 and CMAQ 5.2, is an online meteorology-chemistry model that simulates the two-way feedback between meteorology and chemistry. The feeback focuses on the interactions of estimated aerosol mass on incoming shortwave radiation.


## Build Instructions

Directions and code for constructing the WRF-CMAQ coupled model is available from the CMAS Center Software Clearinghouse. From http://www.cmascenter.org, select Download -> Software -> CMAQ and choose version 5.2 to download the coupled model tarball, twoway.tar.gz.

## Run Instructions
A test dataset is also available from the CMAS Center Software Clearinghouse (wrf_38_cmaq_52_input.tar.gz, wrf_38_cmaq_52_output.tar.gz). A sample run script, twoway_model_run_script, is in the twoway.tar.gz under script subdirectory.

## WRF-CMAQ Input/Output Data

The WRF-CMAQ benchmark data provide examples of the files needed to run the model. The general list of inputs required for WRF-CMAQ include,

* REAL outputs :: wrfbdy, wrflowinp, wrffdda, wrfsfdda, wrfrstrt
* CMAQ inputs  :: emissions, IC, BC, OMI, ocean file

WRF-CMAQ outputs standard WRF (wrfout) and CMAQ output files.

## References:

Wong, D. C., Pleim, J., Mathur, R., Binkowski, F., Otte, T., Gilliam, R., Pouliot, G., Xiu, A., Young, J. O., and Kang, D.: WRF-CMAQ two-way coupled system with aerosol feedback: software development and preliminary results, Geosci. Model Dev., 5, 299-312, doi:10.5194/gmd-5-299-2012 , 2012.

For an overview of the 2-way Coupled WRF-CMAQ see: http://www.cmascenter.org/conference/2011/slides/mathur_overview_two-way_2011.pptx

and for more details on the 2-way Coupled WRF-CMAQ system see: http://www.cmascenter.org/conference/2011/slides/wong_wrf-cmaq_two-way_2011.pptx

-----
