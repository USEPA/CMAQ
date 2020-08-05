<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixD_parallel_implementation.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixF_importing_bugfixes.md)

<!-- END COMMENT -->

# Appendix E: Configuring the Weather Research and Forecasting Model (WRF) for Use with Air Quality Models 

## E.1 WRF version 4+

* WRF4.0 has updates to the ACM2 PBL model to account for the new default hybrid coordinate system. Our internal model runs suggest that the hybrid option (hybrid_opt =2) improves the model in areas where topographical variations are more extreme like the Rocky Mountains. As such, it is suggested, but not a requirement, to use this option in WRF that became the default in WRF4.0.

* Several important updates were made to the Pleim-Xiu LSM in WRF4.1. Soil hydraulics are calculated using analytical equations (Noilhan and Mahfouf, 1996) rather than lookup tables. And, users can opt to use time-varying vegetation fraction from a wrflowinp input file in WRF that can be based off MODIS satellite data instead of the old weighting method that is based on lookup tables and landuse fraction. This satellite data option is activated using the physics namelist option "pxlsm_modis_veg = 1".


## E.2 WRF version 3.7 
* **[Section from WRFv3.7 Technical Documentation related to air quality modeling](http://www2.mmm.ucar.edu/wrf/users/docs/PX-ACM.pdf):** This 8 page pdf provides description and procedures for using the Pleim-Xiu LSM, ACM2 PBL and Pleim Surface Layer Scheme in WRF including best practices and namelist options.

## E.3 WRF with lightning assimilation 
* **[WRF with Lightning Assimilation User's Guide](https://wcms.epa.gov/sites/production/files/2017-02/documents/wrf_with_ltga_userguide.pdf):** This 3 page pdf describes how to run WRF with the lightning assimilation technique described in Heath et al. (2016). 
The assimilation method uses gridded lightning data to trigger and suppress sub-grid deep convection in Kain-Fritsch. 
The gridded lightning data (variable name is ‘LNT’) is read in through auxinput8. The lightning data is grouped into 
30-min intervals and treated as simple zeros (no lightning) or ones (lightning) for the assimilation method. 
All of the necessary code modifications and data are described in the document.

* **[WRF with Lightning Assimilation Code](https://wcms.epa.gov/sites/production/files/2017-02/ltgda_wrf_16feb2017.zip):** This .zip file (ltgda_wrf_16feb2017.zip; 220K) contains the registry and FORTRAN files with the updates needed to run WRF with lightning assimilation, as well as a generic Python script to grid lightning data to your WRF domain.

## E.4 Reference:
Heath, N. K., J. E. Pleim, R. C. Gilliam, & D. Kang (2016). A simple lightning assimilation technique for improving retrospective WRF simulations, J. Adv. Model. Earth Syst., 8, 1806 – 1824, http://dx.doi.org/10.1002/2016MS000735.

Noilhan, J., & Mahfouf, J. F. (1996). The ISBA land surface parameterization scheme. Global and planetary Change, 13(1-4), 145-159.





<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixD_parallel_implementation.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixF_importing_bugfixes.md) <br>
CMAQ User's Guide (c) 2020<br>
<!-- END COMMENT -->
