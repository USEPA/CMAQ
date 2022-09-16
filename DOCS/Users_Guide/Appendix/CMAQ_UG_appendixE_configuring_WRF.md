<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixD_parallel_implementation.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixF_elmo_output.md)

<!-- END COMMENT -->

# Appendix E: Configuring the Weather Research and Forecasting Model (WRF) for Use with Air Quality Models 

## E.1 WRF version 4.3+

* **[WRF configuration guide for CMAQ applications](http://www2.mmm.ucar.edu/wrf/users/docs/PX-ACM.pdf)**
* UPDATE: Modified the ACM2 PBL height algorithm for stable conditions so that the Richardson number is computed using windspeed
in layer k rather than wind speed difference between layer k and ksrc.
* UPDATE: Added new pathway for evaporation from the ground in the vegetated fraction of the grid cell in PX LSM module.
* UPDATE: Consolidated WRF PX LSM code with MPAS versions. The PX LSM code in WRFv4.3 is the exact same code as that for MPASv7.2+


## E.2 WRF version 4.0

* WRF4.0 has updates to the ACM2 PBL model to account for the new default hybrid coordinate system. Our internal model runs suggest that the hybrid option (hybrid_opt =2) improves the model in areas where topographical variations are more extreme like the Rocky Mountains. As such, it is suggested, but not a requirement, to use this option in WRF that became the default in WRF4.0.

* UPDATE: Added vegetation and leaf-area index option for Pleim-Xiu land-surface runs. Until this version, the PX LSM uses VEGFRA and LAI computed from the module_sf_pxlsm_data.F PX data table. This uses fractional landuse and these lookup values to compute the LAI and VEGFRA for each grid cell. The new option (pxlsm_modis_veg = 1) is activated using this option in the physics section of the namelist.input file. It uses the time-varying VEGFRA and LAI from the wrflowinp_d01 file instead of the look-up values in the PX data table. This allows use of more accurate high resolution MODIS that is now available in WPS in WRFv4+. Alternatively, users can process their own MODIS data for specific years and put in this same input file.
* UPDATE: Also, the soil calculation in the PX LSM were modified to use analytical functions from Noilhan and Mahfouf (1996) for field capacity, saturation and wilting point based on fractional soil data. Also, variables for fractional clay, fine and coarse sand were added in PX for output to the CMAQ air quality model. This is an important update because these data are used for dust emissions in the air quality model along with the new soil properties (wilting, saturation and field capacity). SOILTYP was also updated in PX LSM so soil classes are consistent with the standard 16 soil types in the WRF system. Prior, PX only had 12 classes and classes 4-12 were not the same as those classes used by other LSMs.


## E.3 WRF version 3.7 
* **[Section from WRFv3.7 Technical Documentation related to air quality modeling](http://www2.mmm.ucar.edu/wrf/users/docs/PX-ACM.pdf):** This 8 page pdf provides description and procedures for using the Pleim-Xiu LSM, ACM2 PBL and Pleim Surface Layer Scheme in WRF including best practices and namelist options.

## E.4 WRF with lightning assimilation 
* **[WRF with Lightning Assimilation User's Guide](https://wcms.epa.gov/sites/production/files/2017-02/documents/wrf_with_ltga_userguide.pdf):** This 3 page pdf describes how to run WRF with the lightning assimilation technique described in Heath et al. (2016). 
The assimilation method uses gridded lightning data to trigger and suppress sub-grid deep convection in Kain-Fritsch. 
The gridded lightning data (variable name is ‘LNT’) is read in through auxinput8. The lightning data is grouped into 
30-min intervals and treated as simple zeros (no lightning) or ones (lightning) for the assimilation method. 
All of the necessary code modifications and data are described in the document.

* **[WRF with Lightning Assimilation Code](https://wcms.epa.gov/sites/production/files/2017-02/ltgda_wrf_16feb2017.zip):** This .zip file (ltgda_wrf_16feb2017.zip; 220K) contains the registry and FORTRAN files with the updates needed to run WRF with lightning assimilation, as well as a generic Python script to grid lightning data to your WRF domain.

## E.5 Reference:

Noilhan, J., & Mahfouf, J. F. (1996). The ISBA land surface parameterization scheme. Global and planetary Change, 13(1-4), 145-159.

Noilan, J., and S. Planton, 1989: A simple parameterization of land surface processes for meteorological models. Mon. Wea. Rev., 117, 536-549.

Pleim, J. E., and A. Xiu, 1995: Development and testing of a surface flux and planetary boundary layer model for application in mesoscale models. J. Appl. Meteor., 34, 16-32.

Xiu, Aijun, and J. E. Pleim, 2001: Development of a Land Surface Model. Part I: Application in a Mesoscale Meteorological Model. J. Appl. Meteor., 40, 192–209. 

Pleim, J. E., and A. Xiu, 2003: Development of a land surface model. Part II: Data assimilation. J. Appl. Meteor., 42, 1811-1822.

Pleim, J. E., 2006: A simple, efficient solution of flux-profile relationships in the atmospheric surface layer, J. Appl. Meteor. and Clim., 45, 341–347.

Pleim, Jonathan E., 2007: A Combined Local and Nonlocal Closure Model for the Atmospheric Boundary Layer. Part I: Model Description and Testing. J. Appl. Meteor. Climatol.,46, 1383–1395.

Pleim, J. E., and R. Gilliam, 2009: An indirect data assimilation scheme for deep soil temperature in the Pleim-Xiu land surface model. J. Appl. Meteor. Climatol., 48, 1362-1376.

Gilliam, R. C., and J. E. Pleim, 2010: Performance assessment of new land-surface and planetary boundary layer physics in the WRF-ARW. J. App. Meteor. Climatol., 49(4), 760-774.

Heath, N. K., J. E. Pleim, R. C. Gilliam, & D. Kang (2016). A simple lightning assimilation technique for improving retrospective WRF simulations, J. Adv. Model. Earth Syst., 8, 1806 – 1824, http://dx.doi.org/10.1002/2016MS000735.

Gilliam, R. C., Herwehe, J. A., Bullock, Jr, O. R., Pleim, J. E., Ran, L., Campbell, P. C., & Foroutan, H. (2021). Establishing the suitability of the model for prediction across scales for global retrospective air quality modeling. Journal of Geophysical Research: Atmospheres, 126, e2020JD033588. https://doi.org/10.1029/2020JD033588







<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixD_parallel_implementation.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixF_elmo_output.md) <br>
CMAQ User's Guide (c) 2020<br>
<!-- END COMMENT -->
