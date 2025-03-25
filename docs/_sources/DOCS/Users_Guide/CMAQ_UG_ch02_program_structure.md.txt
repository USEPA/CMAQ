
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch01_overview.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch03_preparing_compute_environment.md)

<!-- END COMMENT -->

# 2. Program Structure
## 2.1 Introduction
The CMAQ system is a suite of software programs that work in concert to estimate ozone, particulate matter, toxic compounds, and acid deposition in addition to other atmospheric pollutants of interest.  As a framework for simulating the interactions of multiple complex atmospheric processes, CMAQ requires many types of inputs including meteorological information, primary pollutant emission rates, chemical properties and reactions, and land properties that are influential for exchange of pollutants with the atmosphere.  

## 2.2 CMAQ Core Programs
Weather conditions such as the changes in temperature, winds, cloud formation, and precipitation rates are the primary physical driving forces for transport in the atmosphere.  These conditions are represented in CMAQ using output from the [Weather Research and Forecasting (WRF) Model](https://www.mmm.ucar.edu/weather-research-and-forecasting-model) for regional- and hemispheric-scale simulations and from the [Model for Prediction Across Scales (MPAS)](https://www.mmm.ucar.edu/models/mpas) for global simulations.  To obtain inputs on emissions, CMAQ relies on the open-source Sparse Matrix Operator Kernel Emissions [(SMOKE)](http://www.smoke-model.org) model to estimate the magnitude and location of pollution sources. Another open-source system, the 
[Fertilizer Emission Scenario Tool for CMAQ (FEST-C)](https://www.cmascenter.org/fest-c/) is used to run the Environmental Policy Integrated Climate (EPIC) model to generate agricultural land nitrogen and soil information needed for CMAQ bi-directional NH<sub>3</sub> modeling.

The structure of the CMAQ system is illustrated in Fig. 2-1. The main CMAQ program, the CMAQ Chemistry Transport Model (CCTM), which is often referred to simply as CMAQ, contains the principal equations used for predicting pollutant concentrations given the inputs discussed above. These partial differential equations are designed for mass conservation and consider a myriad of important processes such as emissions, chemical reaction, uptake to clouds and precipitation, and dry deposition.  An overview of the science configuration options in CMAQ can be found in [Chapter 6](CMAQ_UG_ch06_model_configuration_options.md).  Instructions for compiling and running the CCTM are covered in Chapters 3 ([Preparing Compute Environment](CMAQ_UG_ch03_preparing_compute_environment.md)), 4 ([Model Inputs](CMAQ_UG_ch04_model_inputs.md)) and 5 ([Running a CMAQ Simulation](CMAQ_UG_ch05_running_a_simulation.md)). 

<a id=Figure2-1></a> ![Figure 2-1](./images/Figure2-1.jpg)

**Figure 2‑1. Overview of the CMAQ System**

Several important tools are provided with the CMAQ system to handle the preparation of important input data. The meteorology data provided by the upstream meteorological model (e.g., WRF) is prepared for input to the CCTM by the Meteorology-Chemistry Interface Processor (MCIP). The CCTM also requires inputs for specifying the initial and boundary conditions of each chemical species treated by the model. These data may be processed and prepared for use by the ICON and BCON tools, respectively.  Documentation on compiling and running MCIP, ICON, and BCON is available under the [PREP][link_2.2] folder. 

## 2.3 Online Emissions Options
CMAQ includes several "online" emissions options to support coupling between meteorology and chemistry processes, and to facilitate operational air quality forecast modeling. The emissions streams available for running online in CMAQ are: biogenics, wind-blown dust, sea spray, marine gas emissions, and nitrogen oxides from lightning. One important advantage of incorporating these processes directly in a CCTM simulation is that emissions are meteorologically modulated at the synchronization (chemistry) time step rather than being linearly time-interpolated within each simulation hour.  In addition, disk space may be saved, because a 3‑D emissions file is no longer needed for elevated point sources.

## 2.4 Post-processing Tools
The CMAQ release includes a set of Fortran programs for manipulating CMAQ input and output files including matching model output to observed air quality data.  Information on these post-processing tools is available under the [POST][link_2.4] folder.  There are many additional resources available for visualizing and evaluating CMAQ input and output which are described in [Chapter 8](CMAQ_UG_ch08_analysis_tools.md).  

## 2.5 Utilities for Developers
The CMAQ release includes several optional utilities for model developers. These tools may be useful for advanced users who wish to use other chemical mechanisms and/or a different set of photolysis reaction input data. Chemical reaction data is processed by the Chemical Mechanism Compiler (*chemmech*) for all chemical reaction solver approaches. This tool needs chemical namelists (e.g. GC_NAMELIST, AE_NAMELIST, etc) in order to run, and these namelists can be modified directly with a text editor or converted to CSV with the namelist converter *nml*. After running chemmech, to then generate files specifically for the Euler Backward Iterative (EBI) solver approach, the *create_ebi* is provided. Finally the Photolysis Preprocessor (*inline_phot_preproc*) provides support for generating photolysis rate input to customize chemical mechanisms.  In addition, the CMAQ repository includes software for generating Makefiles necessary for compiling the CCTM and other components. This *bldmake* utility is designed to account for user options, diagnose dependencies in source code and produce a Makefile ready to build executable files.  Documentation for each utilitiy program is provided under the [UTIL][link_2.5] folder.  

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch01_overview.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch03_preparing_compute_environment.md)
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->


<!-- START_OF_COMMENT -->

[link_2.2]: ../../PREP/
[link_2.4]: ../../POST/
[link_2.5]: ../../UTIL/

<!-- END_OF_COMMENT -->

[link_2.2]: https://github.com/USEPA/CMAQ/blob/main/PREP/
[link_2.4]: https://github.com/USEPA/CMAQ/blob/main/POST/ 
[link_2.5]: https://github.com/USEPA/CMAQ/blob/main/UTIL/