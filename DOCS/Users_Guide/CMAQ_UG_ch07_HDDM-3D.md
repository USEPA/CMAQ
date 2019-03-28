
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch08_ISAM.md)

<!-- END COMMENT -->

# HDDM-3D

## Overview

The Decoupled Direct Method in Three Dimensions (DDM-3D) provides CMAQ concentration and deposition sensitivity information for user specified model parameters.

In air quality modeling, sensitivities measure the response of a model output to a change in one or several predefined model parameters. In policy applications, the parameters of interest are usually emissions and the output of interest is pollutant concentrations. We may be interested in emissions from a particular geographical region, like an urban area, a group of states, or a country, and/or emissions from a particular source, such as wildfires, electricity generating units (EGUs), or light duty diesel trucks.

![Figure7.1: A hypothetical relationship between emissions of SO2 and sulfate concentrations.  The green tangent line illustrates the sensitivity of sulfate concentration to emissions of SO2.](ddm.png)
This plot shows how the direct decoupled method works. Sensitivities are vital to Air Quality model analysis.
A hypothetical relationship between emissions of SO2 and sulfate concentrations.  The green tangent line illustrates the sensitivity of sulfate concentration to emissions of SO2.
Emissions sensitivities can be calculated by simply running the air quality model twice – once with standard emissions inputs, and once with the emissions of interest adjusted in some way. The difference in outputs between the two runs in relation to the size of the adjustment then becomes the model sensitivity. While this process is fairly easy to implement and interpret, it quickly becomes computationally complex as the number of desired sensitivities increases. For example, calculating sensitivity to EGU emissions from 10 southeastern states in the U.S. would require 11 separate air quality model simulations. 

An alternative approach to calculate sensitivities is available with the CMAQ model – CMAQ-DDM-3D. CMAQ-DDM-3D is a separately downloadable version of the CMAQ model that allows for sensitivity calculation simultaneously with the standard concentrations and deposition fields. This is done by altering the existing model algorithms to allow for sensitivity propagation through every science module in CMAQ. While CMAQ-DDM-3D does require more computational resources than standard CMAQ, it scales much more favorably with the number of desired calculations.

Besides emissions, sensitivities to other model parameters can also be calculated. Currently, CMAQ-DDM-3D can be used for sensitivity to emission rates, boundary conditions, initial conditions, reaction rates, potential vorticity, or any combination of these parameters. Second order sensitivity calculations, or sensitivity of sensitivity, are also available.

## CMAQ-DDM-3D Releases
**CMAQv5.2 DDM-3D**

Current DDM-3D implementation is available for version 5.2 of the Community Multiscale Air Quality (CMAQ) model.  

* [Link to CMAQv5.2 DDM-3D source code and scripts](https://github.com/USEPA/CMAQ/tree/5.2_DDM-3D)
* [Direct download to CMAQv5.2 DDM-3D source code and scripts](https://github.com/USEPA/CMAQ/archive/5.2_DDM-3D.zip)
* [Documentation for CMAQv5.2 DDM-3D](https://github.com/USEPA/CMAQ/blob/5.2_DDM-3D/DOCS/Instrumented_Docs/CMAQ_DDM.md)

**CMAQv5.3.1 DDM-3D**

The migration of DDM-3D to the most recent CMAQ release will be included in CMAQv5.3.1 which is scheduled to be released in fall 2019.  This documentation will be updated at that time.  

## References

Cohan, D.S., & Napelenok, S.L. (2011). Air Quality Response Modeling for Decision Support. Atmosphere, 2(3), 407-425. doi: 10.3390/atmos2030407EXIT

Napelenok, S.L., Cohan, D.S., Odman, M.T., & Tonse, S. (2008). Extension and evaluation of sensitivity analysis capabilities in a photochemical model. Environmental Modelling & Software, 23(8), 994-999. doi: 10.1016/j.envsoft.2007.11.004EXIT

Napelenok, S.L., Cohan, D.S., Hu, Y.T., & Russell, A.G. (2006). Decoupled direct 3D sensitivity analysis for particulate matter (DDM-3D/PM). Atmospheric Environment, 40(32), 6112-6121. doi: 10.1016/j.atmosenv.2006.05.039EXIT

## Contact

[Sergey L. Napelenok](mailto:napelenok.sergey@epa.gov), Atmospheric Modeling and Analysis Division, U.S. EPA

<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch08_ISAM.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
