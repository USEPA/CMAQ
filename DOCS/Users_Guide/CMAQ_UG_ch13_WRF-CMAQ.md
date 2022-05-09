<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch12_sulfur_tracking.md) - [Home](README.md) - [Tables and Figures >>](CMAQ_UG_tables_figures.md)

<!-- END COMMENT -->

# 13. WRF-CMAQ Model

## 13.1 Introduction
Air Quality Models (AQMs) such as CMAQ are typically run in retrospective mode using archived meteorological data to drive the chemistry-transport calculations. Here the model producing the meteorological data is run first producing inputs that are synthesized into AQM model ready inputs via pre-processors, such as MCIP. 
 
But this one-way coupling process, coarsely interpolates simulated dynamics and does not take into consideration important chemical effects on the energy budget of the atmosphere, both of which can drastically change the driving meteorology. For example, aerosols can affect the amount of sunlight that reaches the surface, thus impacting temperature (aerosol direct effect). 

To address these short-comings, a coupled WRF-CMAQ model was developed (Wong et al., 2012). A single source code principle was used to construct this two-way coupling system so that CMAQ can be consistently executed either as a stand-alone model or part of the coupled system without any code changes, by treating CMAQ as a subroutine in the WRF structure; this approach eliminates maintenance of separate code versions for the coupled and uncoupled systems. 
 
The design also provides the ﬂexibility to permit users: (1) to adjust the call frequency of WRF and CMAQ to balance the accuracy of the simulation versus computational intensity of the system, and (2) to execute the two-way coupling system with feedbacks to study the eﬀect of gases and aerosols on short wave radiation and subsequent simulated dynamics.  

## 13.2 Aerosol Direct Radiative Feedback Effects
The presence of aerosols impacts the amount of radiation reaching the surface, in turn altering the energy budget of the atmosphere (manifesting itself as changes to temperature and PBL). This is called the short-wave aerosol direct radiative effect. This has been implemented in the shortwave Rapid Radiative Transfer Model for General Circulation Models (RRTMG) radiation scheme in WRF, where aerosol optical properties such as extinction, single scattering albedo, and asymmetry factor, are calculated for 14 wavelength bands (Clough et al. 2005) using aerosol composiiton and size information simulated by CMAQ. Specifically, the aerosol information from CMAQ is transferred in form of seven variables representing: water soluble mass (mass_ws), water insoluble mass (mass_wi), elemental carbon (mass_ec), sea salt (mass_ss), water (mass_h2o), mode-mean diameters and standard deviations, for all three modes (aitken, accumulation, and coarse) to WRF. The implementation utilizes the core-shell model where black carbon is treated as the center and the other substances comprise the shell. The aerosol optics calculations are based on Bohren and Huffman (1983).

## 13.3 Application and Evaluation 
The aerosol optics calculations in the WRF-CMAQ model were assessed through comparison to measured optical properties of ambient aerosols made during the Carbonaceous Aerosol and Radiation Effects Study (CARES) as detailed by Gan et al. (2015a).

The ability of the WRF-CMAQ system to reproduce historical trends in the tropospheric aerosol burden, aerosol optical depths, and clear-sky short wave radiation across the northern hemisphere and the U.S., has recently been assessed through extensive comparisons of long-term simulations of these quantities with observation-derived records from 1990 to 2010 (Xing et al. 2015a,b; Gan et al., 2015b). The model captured declining Aerosol Optical Depth (AOD) trends along with the corresponding decreased top-of-atmosphere (TOA) short-wave radiation (SWR), or upwelling, and increased surface SWR, or downwelling, in the eastern US, Europe and the northern Atlantic for the period of 2000–2010. Estimates of the aerosol direct radiative effects (ADE) at TOA were comparable with those derived from measurements and, compared to general circulation models, the model exhibited better estimates of surface-aerosol direct radiative efficiency (Eτ) (Xing et al., 2015b).

Additionally, top-of-atmosphere clear-sky shortwave radiation during 2000-2010, inferred from the NASA Cloud and Earth’s Radiant Energy System (CERES) satellite retrievals show decreasing trends in the eastern U.S. and increasing trends in eastern China. The inclusion of ADE in WRF-CMAQ yielded better agreement with these contrasting trends suggesting that the trends in clear-sky radiation are influenced by trends in the tropospheric aerosol burden (Xing et al., 2015; Mathur et al., 2017).
 
Impacts of aerosol cooling are not limited to changes in surface temperature, since variation in atmospheric dynamics caused by the increased stability can worsen local air quality and impact human health (Xing et al., 2016).
 
Hemispheric WRF-CMAQ model simulation over two decades (1990−2010) shows enhanced surface PM2.5 concentrations in the most polluted regions of the world due to the aerosol direct effect (Xing et al., 2016).


## 13.4 Latest WRF-CMAQ Release

The new WRF-CMAQ model is based on WRFv4.4 and CMAQv5.3.3 and later. It supports only RRTMG radiation scheme for short wave aerosol direct effect. It uses core-shell model to perform aerosol optics calculations rather than volume mixing technique as in the previous version of the WRF-CMAQ model. 

The code used to couple the WRFv4.4 and CMAQv5.3.3+ models is now released as part of the [CMAQ Github Repository](../../../UTIL/wrfcmaq_twoway_coupler).

Build and run instructions are provided in the [WRF-CMAQ Tutorial](Tutorials/CMAQ_UG_tutorial_WRF-CMAQ_Benchmark.md).
                   
If you have any questions, please contact David Wong at wong.david-c@epa.gov


## 13.7 References

Bohren, C. F. and Huffman, D. R. (1983). Absorption and Scattering of Light by Small Particles, Wiley-Interscience, New York, USA, 530 pp.

Clough, S.A., Shephard, M. W., Mlawer, E. J., Delamere, J. S., Iacono, M. J., Cady-Pereira, K., Boukabara, S., & Brown, P. D. (2005). Atmospheric radiative transfer modeling: a summary of the AER codes. J. Quant. Spectrosc. Ra., 91, 233–244.

Gan, C., Binkowski, F., Pleim, J., Xing, J., Wong, D-C., Mathur, R., Gilliam, R. (2015a). Assessment of the Aerosol Optics Component of the Coupled WRF-CMAQ Model using CARES Field Campaign data and a Single Column Model. Atmospheric Environment, 115, 670-682. https://doi.org/10.1016/j.atmosenv.2014.11.028 

Gan, C., Pleim, J., Mathur, R., Hogrefe, C., Long, C., Xing, J., Wong, D-C., Gilliam, R., Wei, C. (2015b). Assessment of long-term WRF–CMAQ simulations for understanding direct aerosol effects on radiation "brightening" in the United States. Atmospheric Chemistry and Physics, 15, 12193-12209. https://doi.org/10.5194/acp-15-12193-2015 EXIT

Mathur, R., Pleim, J., Wong, D., Otte, T., Gilliam, R., Roselle, S., Young, J. (2011). Overview of the Two-way Coupled WRF-CMAQ Modeling System. 2011 CMAS Conference, Chapel Hill, NC. Presentation available from the CMAS conference website. 

Mathur, R., Xing, J., Gilliam, R., Sarwar, G., Hogrefe, C., Pleim, J., Pouliot, G., Roselle, S., Spero, T. L., Wong, D. C., Young, J. (2017). Extending the Community Multiscale Air Quality (CMAQ) modeling system to hemispheric scales: overview of process considerations and initial applications, Atmos. Chem. Phys., 17, 12449–12474, https://doi.org/10.5194/acp-17-12449-2017

Wong, D.C., Pleim, J., Mathur, R., Binkowski, F., Otte, T., Gilliam, R., Pouliot, G., Xiu, A., Kang, D. (2012). WRF-CMAQ two-way coupled system with aerosol feedback: software development and preliminary results. Geosci. Model Dev., 5, 299-312. https://doi.org/10.5194/gmd-5-299-2012

Xing, J., Mathur, R., Pleim, J., Hogrefe, C., Gan, C.-M., Wong, D. C., Wei, C. (2015a). Can a coupled meteorology–chemistry model reproduce the historical trend in aerosol direct radiative effects over the Northern Hemisphere?, Atmos. Chem. Phys., 15, 9997–10018, https://doi.org/10.5194/acp-15-9997-2015

Xing, J., Mathur, R., Pleim, J., Hogrefe, C., Gan, C.-M., Wong, D., Wei, C., Wang, J. (2015b). Air pollution and climate response to aerosol direct radiative effects: a modeling study of decadal rends across the Northern Hemisphere, J. Geophys. Res.-Atmos., 120, 12221–12236, https://doi.org/10.1002/2015JD023933

Xing, J., Wang, J., Mathur, R., Pleim, J., Wang, S., Hogrefe, C., Gan, C.-M., Wong, D., Hao, J. (2016). Unexpected benefits of reducing aerosol cooling effects, Environ. Sci. Technol., 50, 7527– 7534, https://doi.org/10.1021/acs.est.6b00767

For an overview of the 2-way Coupled WRF-CMAQ see: http://www.cmascenter.org/conference/2011/slides/mathur_overview_two-way_2011.pptx

and for more details on the 2-way Coupled WRF-CMAQ system see: http://www.cmascenter.org/conference/2011/slides/wong_wrf-cmaq_two-way_2011.pptx

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch12_sulfur_tracking.md) - [Home](README.md) - [Tables and Figures >>](CMAQ_UG_tables_figures.md)<br>
CMAQ User's Guide (c) 2021<br>

<!-- END COMMENT -->
