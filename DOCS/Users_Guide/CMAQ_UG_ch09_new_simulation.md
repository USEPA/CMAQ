
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch06_process_analysis.md)

<!-- END COMMENT -->

# Setting Up a New Simulation

## Overview
Before using CMAQ for operational modeling in a new computing environment, it is recommended that the model be benchmarked using the test dataset that is distributed with the model. Installation and benchmarking CMAQ on a Linux system is described elsewhere. After benchmarking CMAQ, it can be configured for other simulations. The same steps that are required to build the model for the test case apply to building it for new simulations. However, not all the steps need to be repeated for a new model configuration unless new code becomes available or bug fixes are identified. It is not necessary to rebuild any of the libraries that CMAQ uses once working versions are built on a user’s system. A single installation of the libraries (netCDF, I/O API, MPI) can be linked to for multiple configurations and applications of the model. Likewise, the CMAQ model builder, Bldmake, only needs to be compiled once and used for all applications of the model.

Except for MCIP, all of the CMAQ programs need to be recompiled when the chemistry mechanism or science configuration of the model change. If the science configuration does not change between applications, the CMAQ executables can be reused for different applications on the same Linux system. MCIP needs to be compiled only once on a user’s Linux system and the executables may be reused for all applications of the model, unless new source code, including libraries, become available.

The reason for modeling a particular time period or area evolves from a research question, such as determining why a severe air pollution episode happened or studying the dominant sources of visibility degradation in a specific geographic region. When deciding upon a modeling period, it is necessary to have a “spin-up” interval prior to the beginning of the initial time of interest. The spin-up period is a sequence of days at the beginning of an air quality simulation that are not used in the analysis of the modeling results. These days are simulated to minimize the impacts of the initial conditions on the CCTM modeling results. Spin-up periods vary in length depending on the size of the modeling domain, the magnitude of the emissions sources within the modeling domain, and the pollutants being studied.

Once a modeling episode and area of interest are selected, several steps must be taken before CMAQ can be run. Configuring CMAQ for new simulations involves defining the model grid, vertical layers, time periods, initial and boundary conditions, input/output file locations, and science options of the model. To configure a new CMAQ simulation, the following steps must be taken to set up the new scenario-specific modeling system:

* Run and evaluate the WRF meteorology model for the area and episode of interest
* Convert the WRF data for input to SMOKE and CMAQ using MCIP
* Create a GRIDDESC file that defines the horizontal model grid
* Run and evaluate SMOKE to process emissions input data for CMAQ
* Prepare biogenic emissions input data, either for inline or offline processing
* Prepare initial and boundary conditions for CMAQ
* Configure the CCTM script to use the new input data
* Run and evaluate the CCTM

The following sections cover these topics in the context of setting up a new CMAQ simulation.

## Defining a new horizontal grid
The grid-dependent CMAQ programs (CCTM, ICON, BCON) use a GRIDDESC grid description file to define the map projection and horizontal grid for a simulation. The GRIDDESC file is either output by MCIP or it can be created manually using a text editor. The CMAQ run scripts for the grid-dependent CMAQ programs must refer to a GRIDDESC file that contains the definition of the model grid to be simulated. The name of the grid for the current application must be specified in the CMAQ run script because a single GRIDDESC file can contain multiple grid definitions. For CCTM, which uses both a GRIDDESC file and gridded input data (emissions, meteorology, ICs/BCs), the grid defined in the GRIDDESC file must be consistent across all of the gridded input files, or the simulation will fail.

## Photolysis related inputs
There are multiple input files related to photolysis for CMAQ. One is static and can be used for any CMAQ simulation: PHOT_OPTICS.dat. The CSQY_DATA_{mechanism} input file is specific to the chemical mechanism selected for the model simulation. Both are distributed with the CMAQ code. Ozone column data is also needed for CMAQ and distributed with CMAQ: OMI_1979_to_2017.dat. However, if the new simulation is outside the period of the provided file then the user will need to either provide OMI data in the appropriate format for the needed time period or let the model default to using the last record of OMI data provided on the file distributed with the model. 

## Meteorological inputs
A prognostic meteorological model such as WRF must be applied to generate meteorological inputs to CMAQ when users need to simulate a new area (domain) or time period. It is important to quality assure and evaluate the WRF output against ambient measurements to ensure realistic inputs are being provided to CMAQ. MCIP translates output files from WRF meteorological models to create grid-specific input meteorology data that are used by SMOKE (the emissions processor that computes emissions inputs to CMAQ) and by CMAQ. MCIP prepares and diagnoses all meteorological fields that are required for SMOKE and CCTM. Since domains used for meteorological models are typically larger than photochemical models, MCIP can can be used to uniformly trim cells off the lateral boundary of the domain defined by the meteorological model, or to window in on a subset of that domain. Configuration options for MCIP include the time periods over which to extract data from the meteorological model output files, horizontal and vertical grid definitions.

## Initial and boundary conditions
The ICON processor provides initial chemical fields of individual species concentrations for a specific modeling domain. BCON provides concentrations of individual chemical species for the grid cells surrounding the modeling domain. ICON and BCON both require multiple inputs: concentration values for the chemical species needed in the simulation, vertical and horizontal grid specifications, and a predefined chemical mechanism.

There are two types of input concentrations for ICON and BCON: either (1) tabulated tropospheric vertical profiles or (2) three-dimensional fields from a previous CMAQ or larger-scale CTM simulation, such as GEOS-Chem. The horizontal and vertical grid specifications are input to these processors from the GRIDDESC and MCIP files developed for the particular scenario. 

The same chemical mechanism must be selected for ICON, BCON, and CCTM. Both ICON and BCON assume that the input species concentrations are for the selected mechanism.

## Emissions inputs
Emissions model such as SMOKE (https://www.cmascenter.org/smoke/) are critically important to translate emission inventory data to fit the spatial and temporal scales needed for a photochemical model. This includes temporal allocation of annual emission totals, spatial allocation of emissions, and provide expanded speciation for broad pollutant categories such as VOC, NOX, and primary PM2.5. 

The CMAQ model accepts as input gridded emissions and emissions with known locations provided with specific coordinates. Usually the point source inventory emissions are provided to CMAQ in these “inline” point source format files and grouped by sector. Gridded emissions are typically 2-dimensional (surface layer only) and represent emissions that were spatially allocated using spatial surrogates. 

Biogenic emissions can be input to CMAQ either merged with anthropogenic emissions in the gridded 2D emission input file or estimating “online” by the CMAQ model itself. If the “online” option is selected, it is critical that biogenic emissions are not included in the merged 2D emission input file. The CCTM run script has options that must be selected to use the “online” approach and specific files including a gridded map of the timing of biogenic emissions (BIOSEASON file), gridded vegetation data (B3GRD), and chemical speciation must be provided. 

## CMAQ application
Configure the CCTM run script to loop through the days to be modeled, using the correct input files for each model day and writing output files with the desired number of time steps. The model output time step and run length of each CMAQ simulation are flexible and can be configured in the run script for the applicable CMAQ program. For CCTM, it is possible to have output files with a different number of time steps than the corresponding meteorology or emission input data. For example, it is common to have input meteorology files in 4- or 5-day intervals, input emission files in 1- or 2-day intervals, and CCTM output files in 1-day intervals. The CMAQ scripts allow the user to configure the model to output data with any number of time steps. The number of CCTM output time steps does not need to correspond with the input meteorology and emissions output time steps. To keep CMAQ output file sizes manageable, CMAQ output is typically stored in 1-day (24 hour) blocks.


<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch06_process_analysis.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
