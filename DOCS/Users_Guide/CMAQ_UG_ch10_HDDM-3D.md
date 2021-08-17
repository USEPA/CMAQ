
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch09_process_analysis.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch11_ISAM.md)

<!-- END COMMENT -->

# 10. Decoupled Direct Method in Three Dimensions (CMAQ-DDM-3D)

## 10.1 Introduction

The Decoupled Direct Method in Three Dimensions (DDM-3D) provides CMAQ concentration and deposition sensitivity information for user specified model parameters.

In air quality modeling, sensitivities measure the response of a model output to a change in one or several predefined model parameters. In policy applications, the parameters of interest are usually emissions and the output of interest is pollutant concentrations. We may be interested in emissions from a particular geographical region, like an urban area, a group of states, or a country, and/or emissions from a particular source, such as wildfires, electricity generating units (EGUs), or light duty diesel trucks.

Emissions sensitivities can be calculated by simply running the air quality model twice – once with standard emissions inputs, and once with the emissions of interest adjusted in some way. The difference in outputs between the two runs in relation to the size of the adjustment then becomes the model sensitivity. While this process is fairly easy to implement and interpret, it quickly becomes computationally complex as the number of desired sensitivities increases. For example, calculating sensitivity to EGU emissions from 10 southeastern states in the U.S. would require 11 separate air quality model simulations.

Alternatively, model sensitivities can be calculated with CMAQ-DDM-3D. This is done by altering the existing model algorithms to allow for sensitivity propagation through every science module in CMAQ. While this process does require more computational resources than standard CMAQ, it scales favorably with the number of desired parameters.

Besides emissions, sensitivities to other model parameters can also be calculated. Currently, CMAQ-DDM-3D can be used for sensitivity to emission rates, boundary conditions, initial conditions, reaction rates, potential vorticity, or any combination of these parameters. Second order sensitivity calculations, or sensitivity of sensitivity, sometimes known as higher-order DDM-3D (HDDM-3D) are also available.  Note: second order sensitivity outputs for particulate matter species are still in development and should be considered as a research option. 

## 10.2 CMAQ-DDM-3D Releases

Current CMAQ-DDM-3D implementation is available for version 5.2 of the Community Multiscale Air Quality (CMAQ) model.  

* [Link to CMAQv5.2 DDM-3D source code and scripts](https://github.com/USEPA/CMAQ/tree/5.2_DDM-3D)
* [Direct download to CMAQv5.2 DDM-3D source code and scripts](https://github.com/USEPA/CMAQ/archive/5.2_DDM-3D.zip)
* [Documentation for CMAQv5.2 DDM-3D](https://github.com/USEPA/CMAQ/blob/5.2_DDM-3D/DOCS/Instrumented_Docs/CMAQ_DDM.md)

The migration of DDM-3D to the more recent CMAQ release is currently in development and will be included sometime after the base model release.  This documentation will be updated at that time.  

**A note about I/O API installation for DDM applications**

I/O APIv3.2  supports up to MXFILE3=64 open files, each with up to MXVARS3=2048. DDM applications configured to calculate sensitivity to a large number of parameters may exceed this upper limit of model variables, leading to a model crash. To avoid this issue, users may use I/O API version 3.2 "large" that increases MXFILE3 to 512 and MXVARS3 to 16384. Instructions to build this version are found in [Chapter 3](https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/CMAQ_UG_ch03_preparing_compute_environment.md#333-io-api-library).

## 10.3 References

Cohan, D.S., & Napelenok, S.L. (2011). Air Quality Response Modeling for Decision Support. Atmosphere, 2(3), 407-425. [doi: 10.3390/atmos2030407](https://www.mdpi.com/2073-4433/2/3/407)

Napelenok, S.L., Cohan, D.S., Odman, M.T., & Tonse, S. (2008). Extension and evaluation of sensitivity analysis capabilities in a photochemical model. Environmental Modelling & Software, 23(8), 994-999. [doi: 10.1016/j.envsoft.2007.11.004](https://www.sciencedirect.com/science/article/pii/S1364815207002186)

Napelenok, S.L., Cohan, D.S., Hu, Y.T., & Russell, A.G. (2006). Decoupled direct 3D sensitivity analysis for particulate matter (DDM-3D/PM). Atmospheric Environment, 40(32), 6112-6121. [doi: 10.1016/j.atmosenv.2006.05.039](https://www.sciencedirect.com/science/article/pii/S1352231006005012)

**Contact**

[Sergey L. Napelenok](mailto:napelenok.sergey@epa.gov), Computational Exposure Division, U.S. EPA

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch09_process_analysis.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch11_ISAM.md)<br>
CMAQ User's Guide (c) 2021<br>

<!-- END COMMENT -->
