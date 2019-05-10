
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch01_overview.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch03_preparing_to_run.md)

<!-- END COMMENT -->

# 2. Program Structure
## 2.1 CMAQ Core Programs
The CMAQ system is a suite of software programs that work in concert to estimate ozone, particulate matter, toxic compounds, and acid deposition in addition to other atmospheric pollutants of interest.  As a framework for simulating the interactions of multiple complex atmospheric processes, CMAQ requires many types of inputs including meteorological information, primary pollutant emission rates, chemical properties and reactions, and land properties that are influential for exchange of pollutants with the atmosphere.  

Weather conditions such as the changes in temperature, winds, cloud formation, and precipitation rates are the primary physical driving forces for transport in the atmosphere.  These conditions are represented in air quality model simulations using output from regional-scale numerical meteorology models, such as WRF.  To obtain inputs on emissions, CMAQ relies on the open-source Sparse Matrix Operator Kernel Emissions [(SMOKE)](http://www.smoke-model.org) model to estimate the magnitude and location of pollution sources.

The structure of the CMAQ system is illustrated in Fig. 2-1. The main CMAQ program, the CMAQ Chemistry Transport Model (CCTM), which is often referred to simply as CMAQ, contains the principal equations used for predicting pollutant concentrations given the inputs discussed above. These partial differential equations are designed for mass conservation and take into account a myriad of important processes like emissions, chemical reaction, uptake to clouds and rain, and dry deposition.  An overview of the science processes in CMAQ can be found in [Chapter 8](CMAQ_UG_ch08_model_formulation.md).  Instructions for compiling and running the CCTM are covered in Chapters 3 ([Preparing to Run](CMAQ_UG_ch03_preparing_to_run.md)), 4 ([Model Inputs](CMAQ_UG_ch04_model_inputs.md)) and 5 ([Compile and Run CMAQ](CMAQ_UG_ch05_compile_and_run.md)). 

![Figure 2-1](./images/Figure2-1.jpg)

**Figure 2‑1. Overview of the CMAQ System**

Several important tools are provided with the CMAQ system to handle the preparation of important input data. The meteorology data provided by the upstream meteorological model (e.g. [Weather Research and Forecasting (WRF) Model](https://www.mmm.ucar.edu/weather-research-and-forecasting-model)) is prepared for input to the CCTM by the Meteorology-Chemistry Interface Processor (MCIP). The CCTM also requires inputs for specifying the initial and boundary conditions of each chemical species treated by the model. These data may be processed and prepared for use by the ICON and BCON tools, respectively.  Documentation on compiling and running MCIP, ICON and BCON is available under the [PREP](../../PREP/README.md) folder. 

## 2.2 In-line options
CMAQ includes several "in-line" options to support coupling between meteorology and chemistry processes, and to facilitate operational air quality forecast modeling. The user can incorporate photolysis rate calculations and emissions processing during a CCTM simulation. There are several advantages of incorporating these processes directly in a CCTM simulation:

1. Photolysis rate calculations use the aerosol concentrations and meteorology from the CCTM simulation, simulating the feedbacks of the input emissions and resulting air quality on photochemistry
2. Emissions are meteorologically modulated at the synchronization (chemistry) time step rather than being linearly time-interpolated within each simulation hour
3. Disk space may be saved, because a 3‑D emissions file is no longer needed for elevated point sources
4. CMAQ can more easily be coupled with a meteorological model, enabling direct emissions modulation by the underlying, freshly computed meteorological variables

## 2.3 Post-processing Tools
The CMAQ release includes a set of Fortran programs for manipulating CMAQ input and output files including matching model output to observed air quality data.  Information on these post-processing tools is available under the [POST](../../POST/README.md) folder.  There are many additional resources available for visualizing and evaluating CMAQ input and output which are described in [Chapter 7](CMAQ_UG_ch07_analysis_tools.md).  

## 2.4 Utilities for Developers
The CMAQ release includes several optional utilities for model developers. These tools may be useful for advanced users who wish to use other chemical mechanisms and/or a different set of photolysis reaction input data. Chemical reaction data is processed by the Chemical Mechanism Compiler (chemmech) for all chemical reaction solver approaches. This tool needs chemical namelists (e.g. GC_NAMELIST, AE_NAMELIST, etc) in order to run, and these namelists can be modified directly with a text editor or converted to CSV with the namelist converter nml. After running chemmech, to then generate files specifically for the Euler Backward Iterative (EBI) solver approach, the create_ebi is provided. Finally the Inline Photolysis Preprocessor (inline_phot_preproc) provides support for generating photylisis rate input to custom chemical mechanisms.  In addition, the CMAQ repository includes software for generating Makefiles necessary for compiling the CCTM and other components. This bldmake utility is designed to account for user options, diagnose dependencies in source code and produce a Makefile ready to build executable files.  Documentaiton for each utilitiy programs is provided under the [UTIL](../../UTIL/README.md) folder.  


<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch01_overview.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch03_preparing_to_run.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
