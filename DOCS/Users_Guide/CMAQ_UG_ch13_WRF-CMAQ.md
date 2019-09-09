<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch12_sulfur_tracking.md) - [Home](README.md) - [Tables and Figures >>](CMAQ_UG_tables_figures.md)

<!-- END COMMENT -->

# 13. WRF-CMAQ Coupled Model

## 13.1 Introduction
Air quality models are typically run in two different ways:
* Standalone – Archived output from a meteorological model is used to drive the air quality model.
* Coupled – The air quality and meteorological models are run simultaneously and the chemistry can impact the weather.

The latter “coupled” method is beneficial for studying important interactions between present of aerosol and weather. For example, aerosols can affect the amount of sunlight that reaches the surface, thus impacting temperature (aerosol direct effect). Aerosols also have important impacts on cloud formation (aerosol indirect effect) and cloud albedo. CMAQ has been coupled to the Weather Research and Forecasting (WRF) model for this purpose with the emphasis on aerosol direct effect. In addition, meteorological information is passing to CMAQ in a high frequency than standalone mode. 

In the WRF-CMAQ coupled model (Wong et al., 2012), WRF and CMAQ are simultaneously integrated and information from CMAQ, like aerosol concentration, is passed into WRF so that the present of aerosol can impact the weather. Specifically, the CMAQv5.3 coupled model gives users the options to pass aerosol optical properties to the radiation modules in WRF (aerosol direct radiative effects).  The ability to pass aerosol information into the cloud microphysics routines (aerosol indirect effects; Yu et al, 2014) is currently under development and will be available in a future release.   

## 13.2 Aerosol Direct Radiative Feedback Effects
Aerosol information from CMAQ is transferred to the meteorological model, WRF.  Wavelength dependent aerosol optical properties (extinction, single scattering albedo, and asymmetry factor) are estimated using aerosol composition and size distribution information simulated by CMAQ in conjunction with an algorithm based on Mie theory.  Black carbon is treated by the core-shell approach developed by Frank Binkowski based on Bohren and Huffman (1983). This has been implemented in the shortwave Rapid Radiative Transfer Model for General Circulation Models (RRTMG) radiation scheme in WRF, where aerosol optical properties are calculated for 14 wavelength bands (Clough et al. 2005). The aerosol optics calculations in the WRF-CMAQ model were assessed through comparison to measured optical properties of ambient aerosols made during the Carbonaceous Aerosol and Radiation Effects Study (CARES) as detailed by Gan et al. (2015a).

## 13.3 Application and Evaluation 
The ability of the coupled WRF-CMAQ system to reproduce historical trends in the tropospheric aerosol burden, aerosol optical depths, and clear-sky short wave radiation across the northern hemisphere and the U.S., has recently been assessed through extensive comparisons of long-term simulations of these quantities with observation-derived records from 1990 to 2010 (Xing et al. 2015a,b; Gan et al., 2015b). The model captured declining Aerosol Optical Depth (AOD) trends along with the corresponding decreased top-of-atmosphere (TOA) short-wave radiation (SWR), or  upwellingHelpupwellingThe upward movement of an air mass in the atmosphere., and increased surface SWR, or downwellingHelpdownwellingThe downwelling movement of an air mass in the atmosphere., in the eastern US, Europe and the northern Atlantic for the period of 2000–2010. Estimates of the aerosol direct radiative effects (ADE) at TOA were comparable with those derived from measurements and, compared to general circulation models, the model exhibited better estimates of surface-aerosol direct radiative efficiency (Eτ) (Xing et al., 2015b).

Additionally, top-of-atmosphere clear-sky shortwave radiation during 2000-2010, inferred from the NASA Cloud and Earth’s Radiant Energy System (CERES) satellite retrievals show decreasing trends in the eastern U.S. and increasing trends in eastern China. The inclusion of ADE in WRF-CMAQ yielded better agreement with these contrasting trends suggesting that the trends in clear-sky radiation are influenced by trends in the tropospheric aerosol burden.

Impacts of aerosol cooling are not limited to changes in surface temperature, since variation in atmospheric dynamics caused by the increased stability can worsen local air quality and impact human health.

Hemispheric WRF-CMAQ model simulation over two decades (1990−2010) shows enhanced surface PM2.5 concentrations in the most polluted regions of the world due to the aerosol direct effect.


## 13.4 Latest WRF-CMAQ Release

The new coupled WRF-CMAQ model is based on WRFv4.1.1 and CMAQv5.3. It supports only RRTMG radiation scheme for short wave aerosol direct effect. It uses core-shell model to perform aerosol optics calculation rather than volume mixing technique as in the previous version of the coupled model. 

The WRF-CMAQ coupled model is released as a tarball (WRF411_CMAQ5.3_Coupled_Model_20190821.tar.gz) from the CMAS Center Data Warehouse Google Drive and from the US EPA annoymous ftp server.
- [Link to WRFv4.1.1-CMAQv5.3 Coupled_Model on Google Drive](https://drive.google.com/open?id=1ru64xxZeRcE5buQ9fygV9jUHAQ9aDnL7)
- WRFv4.1.1-CMAQv5.3 Couple Model on ftp:  ftp://newftp.epa.gov/exposure/CMAQ/V5_3/WRF-CMAQ_Coupled_Model/

Build and run instructions are provided in the toplevel *readme* file in the tarball.

Benchmark input and output datasets are also available from the CMAS Center Data Warehouse Google Drive and the EPA ftp server.
- [Link to WRF-CMAQ Benchmark input and output datasets on Google Drive](https://drive.google.com/drive/folders/1apg7_LXU4Kpzx5EZgdc07Q3UTTpL-jah)
- WRF-CMAQ Benchmark input and output datasets on ftp: ftp://newftp.epa.gov/exposure/CMAQ/V5_3/WRF-CMAQ_Coupled_Model/

If you have any question, please contact David Wong at wong.david-c@epa.gov


## 13.5 References

Clough, S.A., Shephard, M. W., Mlawer, E. J., Delamere, J. S., Iacono, M. J., Cady-Pereira, K., Boukabara, S., & Brown, P. D. (2005). Atmospheric radiative transfer modeling: a summary of the AER codes. J. Quant. Spectrosc. Ra., 91, 233–244.

Gan, C., Binkowski, F., Pleim, J., Xing, J., Wong, D-C., Mathur, R., & Gilliam, R. (2015a). Assessment of the Aerosol Optics Component of the Coupled WRF-CMAQ Model using CARES Field Campaign data and a Single Column Model. Atmospheric Environment, 115, 670-682. doi: 10.1016/j.atmosenv.2014.11.028 

Gan, C., Pleim, J., Mathur, R., Hogrefe, C., Long, C., Xing, J., Wong, D-C., Gilliam, R., & Wei, C. (2015b). Assessment of long-term WRF–CMAQ simulations for understanding direct aerosol effects on radiation "brightening" in the United States. Atmospheric Chemistry and Physics, 15, 12193-12209. doi: 10.5194/acp-15-12193-2015 EXIT

Mathur, R., Pleim, J., Wong, D., Otte, T., Gilliam, R., Roselle, S., Young, J. (2011). Overview of the Two-way Coupled WRF-CMAQ Modeling System. 2011 CMAS Conference, Chapel Hill, NC. Presentation available from the CMAS conference website. 

Wong, D.C., Pleim, J., Mathur, R., Binkowski, F., Otte, T., Gilliam, R., Pouliot, G., Xiu, A., and Kang, D. (2012). WRF-CMAQ two-way coupled system with aerosol feedback: software development and preliminary results. Geosci. Model Dev., 5, 299-312. doi: 10.5194/gmd-5-299-2012

Yu, S., Mathur, R., Pleim, J., Wong, D., Gilliam, R., Alapaty, K., Zhao, C., Liu, X. (2014). Aerosol indirect effect on the grid-scale clouds in the two-way coupled WRF-CMAQ: model description, development, evaluation and regional analysis.  Atmos. Chem. Phys., 14, 11247–11285. doi: 10.5194/acp-14-11247-2014

For an overview of the 2-way Coupled WRF-CMAQ see: http://www.cmascenter.org/conference/2011/slides/mathur_overview_two-way_2011.pptx

and for more details on the 2-way Coupled WRF-CMAQ system see: http://www.cmascenter.org/conference/2011/slides/wong_wrf-cmaq_two-way_2011.pptx

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch12_sulfur_tracking.md) - [Home](README.md) - [Tables and Figures >>](CMAQ_UG_tables_figures.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
