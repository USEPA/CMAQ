<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixD_parallel_implementation.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixF_elmo_output.md)

<!-- END COMMENT -->

# Appendix E: Configuring the Weather Research and Forecasting Model (WRF) for Use with Air Quality Models 

## E.1 WRF for CMAQ & Output

* **[WRF configuration guide for CMAQ applications][link_E_pdf]**
* CMAQ is best connected to WRF that uses the P-X LSM with key variables in the output (see WRF configuration guide above).
* Variable names: RS, RA, ZNT_PX, VEGF_PX, LAI_PX, LANDUSEF, WFC_PX, WSAT_PX, WWLT_PX, CSAND_PX, FMSAND_PX, CLAY_PX
* Method 1: Compiled into executable with directives in the Registry file.
* Method 2: At run-time using a simple text file and namelist option.


## E.2 WRF version 4.6

* UPDATE: P-X LSM compatible with 61 class MODIS Local Climate Zone (LCZ) landuse option
* UPDATE: Latent heat effect on ground temperature from the vegetated fraction of the grid cell and from wet canopy was added to P-X LSM.
* UPDATE: NaN fix from a divide by zero because of a zero-value soil parameter when a water cell turns to sea ice.

## E.3 WRF version 4.3+

* UPDATE: Modified the ACM2 PBL height algorithm for stable conditions so that the Richardson number is computed using windspeed
in layer k rather than wind speed difference between layer k and ksrc.
* UPDATE: Added new pathway for evaporation from the ground in the vegetated fraction of the grid cell in P-X LSM module.
* UPDATE: Consolidated WRF P-X LSM code with MPAS versions. The P-X LSM code in WRFv4.3 is the exact same code as that for MPASv7.2+


## E.4 WRF version 4.0

* WRF4.0 has updates to the ACM2 PBL model to account for the new default hybrid coordinate system. Our internal model runs suggest that the hybrid option (hybrid_opt =2) improves the model in areas where topographical variations are more extreme like the Rocky Mountains. As such, it is suggested, but not a requirement, to use this option in WRF that became the default in WRF4.0.

* UPDATE: Added vegetation and leaf-area index option for Pleim-Xiu land-surface runs. Until this version, the P-X LSM uses VEGFRA and LAI computed from the module_sf_pxlsm_data.F PX data table. This uses fractional landuse and these lookup values to compute the LAI and VEGFRA for each grid cell. The new option (pxlsm_modis_veg = 1) is activated using this option in the physics section of the namelist.input file. It uses the time-varying VEGFRA and LAI from the wrflowinp_d01 file instead of the look-up values in the P-X data table. This allows use of more accurate high resolution MODIS that is now available in WPS in WRFv4+. Alternatively, users can process their own MODIS data for specific years and put in this same input file.
* UPDATE: Also, the soil calculation in the P-X LSM were modified to use analytical functions from Noilhan and Mahfouf (1996) for field capacity, saturation and wilting point based on fractional soil data. Also, variables for fractional clay, fine and coarse sand were added in P-X for output to the CMAQ air quality model. This is an important update because these data are used for dust emissions in the air quality model along with the new soil properties (wilting, saturation and field capacity). SOILTYP was also updated in P-X LSM so soil classes are consistent with the standard 16 soil types in the WRF system. Prior, P-X only had 12 classes and classes 4-12 were not the same as those classes used by other LSMs.


## E.5 WRF version 3.7 
* **[Section from WRFv3.7 Technical Documentation related to air quality modeling](http://www2.mmm.ucar.edu/wrf/users/docs/PX-ACM.pdf):** This 8 page pdf provides description and procedures for using the Pleim-Xiu LSM, ACM2 PBL and Pleim Surface Layer Scheme in WRF including best practices and namelist options.


## E.6 Reference:

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

Kang, D., H. K. Heath, R. C. Gilliam, T. L. Spero, and J. E. Pleim (2022), Lightning assimilation in the WRF model (Version 4.1. 1): technique updates and assessment of the applications from regional to hemispheric scales, Geosci. Model Dev., 15, 8561–8579, https://doi.org/10.5194/gmd-15-8561-2022, 2022.



<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixD_parallel_implementation.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixF_elmo_output.md) <br>
CMAQv5.5 User's Guide<br>

<!-- END COMMENT -->

<!-- START_OF_COMMENT -->

[link_E_pdf]: ../PDF/PX-ACM-WRFV4.6-MPAS.pdf

<!-- END_OF_COMMENT --> 

[link_E_pdf]: https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/PDF/PX-ACM-WRFV4.6-MPAS.pdf  
