
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch07_model_outputs.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch09_process_analysis.md)

<!-- END COMMENT -->

# 8. Analysis Tools for CMAQ output

## 8.1 Introduction
Many software programs are freely available for pre- and post-processing, evaluating and visualizing CMAQ outputs. Examples of such freeware are provided in [Table 8-1](#Analysis_Software_Table). Several other commercial packages, including MATLAB and IDL, also support the analysis and visualization of CMAQ inputs and outputs. Most visualization and analysis software that support netCDF file formats, such as R and Python, will work with CMAQ outputs. 

<a id=Analysis_Software_Table></a>
<a id=Table8-1></a>

**Table 8-1. Software Programs for Evaluating and Visualizing CMAQ Data**

|**Software**|**Description**|     **Source**    |
|------------|-------------------------------|---------------------------------------------|
|***Post-processing***|||
|CMAQ POST Tools|Programs released with CMAQ source code to prepare output data for model evaluation|[https://github.com/USEPA/CMAQ/blob/main/POST/README.md](https://github.com/USEPA/CMAQ/blob/main/POST/README.md)|
|I/O API Tools|Postprocessing tools for manipulating data in the I/O API/netCDF format|[https://www.cmascenter.org/ioapi](https://www.cmascenter.org/ioapi)|
|NCO|netCDF Operators: Postprocessing tools for manipulating data in the netCDF format|[http://nco.sourceforge.net](http://nco.sourceforge.net)
|***Evaluation/Visualization***| | |
|AMET|Atmospheric Model Evaluation Tool for analysis and evaluation of meteorological and air quality models|[https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool](https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool)|
|VERDI|Visualization Environment for Rich Data Interpretation for graphical analysis of netCDF gridded data|[https://www.cmascenter.org/verdi](https://www.cmascenter.org/verdi)|
|PseudoNetCDF|Reading, plotting, and sometimes writing capabilities for atmospheric science data formats including CMAQ files|[https://github.com/barronh/pseudonetcdf/wiki](https://github.com/barronh/pseudonetcdf/wiki)|
|RSIG|2D and 3D visualization of satellite and modeled data|[https://www.epa.gov/hesc/remote-sensing-information-gateway](https://www.epa.gov/hesc/remote-sensing-information-gateway)|
|NCL|NCAR Command Language for scientific data processing and visualization|[http://www.ncl.ucar.edu](http://www.ncl.ucar.edu)|
|IDV|Integrated Data Viewer for 3-D graphical analysis of netCDF gridded data|[http://www.unidata.ucar.edu/software/idv/](http://www.unidata.ucar.edu/software/idv/)|

This chapter briefly describes how to use some of the software tools supported by the EPA and CMAS to aggregate CMAQ output, pair aggregated CMAQ output in space and time to air quality observations, create various evaluation plots, and visualize model fields.

## 8.2 Aggregating and Transforming Model Species Concentrations

The *combine* Fortran program, released as part of the CMAQ POST tools, can combine variables from CMAQ output, emissions, or meteorology files into a new I/O API file. Model species can be aggregated or transformed into variables of interest, e.g., to change units from ppmV to ppbV or match observed quantities from a specific monitoring network. Model output files can be concatenated to create files for longer time periods, e.g., files with hourly data for individual days can be combined into a single file for an entire month.  More information on the *combine* utility and its use can be found in this [README file.][Link_8.2]

The *combine* utility relies on a chemical mechanism-specific "Species Definition" files that prescribe how CMAQ output variables should be mapped and/or combined to become comparable to different measured gas, particle, and deposition species. When you download the CMAQ code, the Species Definition files corresponding to default model output variables are automatically included under the subdirectory "CCTM/src/MECHS".  Within each of the listed mechanism folders, you will find files "SpecDef_MECH_NAME.txt" (for gas and aerosol species) and "SpecDef_dep_MECH_NAME.txt" (for deposition species) that contain a long list of species definitions and corresponding documentation. For example, the calculation of NO<sub>X</sub> in ppbV is given as
```
NOX             ,ppbV      ,1000.0*(NO[1] + NO2[1])
```
where NO and NO2 are pulled from an hourly instantaneous [CCTM_CONC](CMAQ_UG_ch07_model_outputs.md#conc) or hourly average [CCTM_ACONC](CMAQ_UG_ch07_model_outputs.md#aconc) model output file in units of ppmV. 

Note that some species aggregation is already happening within the CMAQ model. The CMAQ aerosol module explicitly represents a number of individual aerosol species that need to be combined for comparisons to measured total PM<sub>2.5</sub> mass.  The Explicit and Lumped Model Output (ELMO) capability introduced in CMAQv5.4 prescribes the calculation of aggregated aerosol species (e.g., PM<sub>1</sub>, PM<sub>2.5</sub>, and PM<sub>10</sub>),  as well as diagnostic values (e.g., aerosol surface area and number) and CMAQ calculates them online.  The species are then written to the [CCTM_ELMO](CMAQ_UG_ch07_model_outputs.md#ELMO) or [CCTM_AELMO](CMAQ_UG_ch07_model_outputs.md#AELMO) output files. 

The "SpecDef_MECH_NAME.txt" file relies on output from the CCTM_CONC (or CCTM_ACONC) and the CCTM_ELMO (or CCTM_AELMO) files in addition to meteorological variables for completing all of its calculations. Generally, gas-phase species coming from the direct CCTM_CONC output and aerosol species from CCTM_ELMO. For reference and transparency, each mechanism folder also includes a file named "SpecDef_Conc_MECH_NAME.txt", which processes both gas- and aerosol species from the CCTM_CONC file (CCTM_ELMO variables is still needed for applying hard bounds on particle size, like 2.5 um). These "SpecDef_Conc_MECH_NAME.txt" files are provided for all Carbon Bond 6-based mechanisms and all CRACMM-based mechanisms. Either of these two approaches should yield highly similar or exactly equal results.  

Finally, the SpecDef_Dep_MECH_NAME.txt files are provided to processes deposition variable output from CMAQ.  

See [Appendix F](Appendix/CMAQ_UG_appendixF_elmo_output.md) for more information about the calculation of ELMO output variables and how this relates to use of the *combine* tool.

## 8.3 Model-Observation Pairing for Model Evaluation 
 Once model output has been processed using *combine*, the *sitecmp* and *sitecmp_dailyo3* utilities can be used to match air pollutant measurements with the appropriate model predicted variables.  This pairing of model and observed variables is specified in the run scripts for *sitecmp* and *sitecmp_dailyo3*.  In *sitecmp_dailyo3* this step is controlled by the definition of environment variables OBS_SPECIES and OZONE.  See the [README.md][link_8.3] and the sample run script in the [*sitecmp_dailyo3* scripts][link_8.3_II] folder for more information on setting these environment variables.  The run script for the *sitecmp* utility can be customized for many different types of chemical and meteorological quantities as described in the [README.md][link_8.3_III] for sitecmp.  Sample run scripts for the AQS, CSN, IMPROVE, NADP and SEARCH networks based on the 2016 CMAQ test case are provided in the [*sitecmp* scripts][link_8.3_IV] folder.  In addition, the [README.md][link_8.3_IV] file within the *sitecmp* scripts folder provides the configuration options for monitoring networks.  Note that there are multiple formats for CSN and SEARCH observed data files depending on the year.  The README.txt file is broken into different sections to reflect the change in species names in the observation files for these two networks.  (For example, elemental carbon measurements from the CSN network are labeled as “ec_niosh” in 2009 and earlier, “ec_tor” in 2010, and “88380_val” starting in 2011.)

### 8.3.1 Spatial matching in sitecmp and sitecmp_dailyo3
In *sitecmp*, model values are extracted for the grid cell containing the monitor location. In *sitecmp_dailyo3* the model value of the grid cell containing the observation is provided, as well as the maximum model value of the 9 grid cells centered on the monitor location. These variables in the output file contain the character string "9cell" in the variable name.

### 8.3.2 Temporal matching in sitecmp and sitecmp_dailyo3
* **AQS_HOURLY, CASTNET_HOURLY, SEARCH_HOURLY, NAPS_HOURLY, AERONET**: Air quality observations are assumed to be hourly averages time stamped at the beginning of the hour with local standard time (LST). The *sitecmp* utility will use the time stamp from the observations to determine the matching model time step, accounting for the time zone of the monitor. Therefore, best practice would be for the model time step to also represent hourly average time stamped at the beginning of the hour. This can be accomplished by running the *combine* utility on the CMAQ [CCTM_ACONC](CMAQ_UG_ch07_model_outputs.md#ACONC) or [CCTM_AELMO](CMAQ_UG_ch07_model_outputs.md#AELMO) output files which follow this convention (rather than the instantaneous model ouput files [CCTM_CONC](CMAQ_UG_ch07_model_outputs.md#CONC) and [CCTM_ELMO](CMAQ_UG_ch07_model_outputs.md#AELMO)). These networks also include meteorological measurements. Since meteorological observations are near instantaneous measurements (e.g. 1- or 5-minute averages), using meteorological fields from MCIP or wrfout in *combine* results in the correct matching since these fields are also instantaneous. One exception is the calculation of modeled relative humidity (RH). This variable is not available from MCIP or wrfout files but is stored in the CMAQ CCTM_AELMO output file which represents hourly average values. This creates a slight inconsistency between observed and modeled values for this variable in the sitecmp output files. Note that modeled and observed precipitation for a given hour represents the hourly total rather than the hourly average. 
* **AQS_DAILY_O3, CASTNET_DAILY_O3, NAPS_DAILY_O3**: *sitecmp_dailyo3* computes various daily metrics from observed and modeled hourly ozone values. The temporal matching of the hourly observed and modeled values used in these computations follows the same approach described above for AQS_HOURLY. Therefore, it is best practice to use output from CMAQ CCTM_ACONC files for modeled ozone predictions (rather than CCTM_CONC). Details on the computation of the various daily metrics is provided in the *sitecmp_dailyo3* documentation.
* **AQS_DAILY, CSN, IMPROVE, SEARCH_DAILY**: Air quality observations are daily averages time stamped with the date in local standard time. The *sitecmp* utility will use the date from the observations to compute daily averages using 24 hourly modeled values, accounting for the time zone of the monitor. Therefore, it is best practice to use output from CMAQ CCTM_ACONC and CCTM_AELMO files for modeled air quality predictions which represent hourly average concentrations (rather than CCTM_CONC and CCTM_ELMO).
* **CASTNET**: Air quality observations are weekly averages time stamped with beginning and end date and time of the weekly interval in local standard time. The *sitecmp* utility will use the start and end date and time from the observations to compute weekly averages using hourly modeled values, accounting for the time zone of the monitor. Therefore, it is best practice to use output from CMAQ CMAQ CCTM_ACONC and CCTM_AELMO files for modeled air quality predictions which represent hourly average concentrations (rather than CCTM_CONC and CCTM_ELMO).
* **NADP**: Air quality observations are weekly sums time stamped with beginning and end date of the weekly interval in local standard time. The *sitecmp* utility will use the start and end date from the observations to compute weekly sums using hourly modeled values, accounting for the time zone of the monitor. Observations are matched to output from CMAQ [CCTM_WETDEP1](CMAQ_UG_ch07_model_outputs.md#wetdep) files which represent hourly totals.
* **TOAR**: Air quality observations are daily average values of O3, MDA8 O3, O3 daytime average and O3 nighttime average. The *sitecmp* utility must be given daily average values computed from hourly values using the *hr2day* utility.

## 8.4 The Atmospheric Model Evaluation Tool (AMET)

The Atmospheric Model Evaluation Tool (AMET) was developed to aid in the evaluation of the meteorological and air quality models within the CMAQ modeling system (i.e. WRF, MPAS, CMAQ-CTM). AMET organizes, provides consistency and speeds-up the evaluation process for operational meteorological and air quality model simulations. The AMET software is written primarily in R, with support from several fortran programs and cshell scripts. The tool also requires the presence of a MySQL database for analysis of meteorological data and full functional analysis of air quality (CMAQ) data (analysis of CMAQ output can be done without a database present). Although it was developed specifically to aid in the evaluation of the CMAQ modeling system, the AMET software can be adapted to work with other modeling systems. 

There are separate modules in AMET for evaluating meteorological and air quality model output. This separation is necessary because both the observed and predicted meteorological and air quality data are quite different, utilizing different file formats for both the observed and model data. In addition, the observed meteorological and air quality data are often obtained from networks that use different sampling protocols, which can make pairing meteorological and air quality data together difficult. One advantage of separate meteorological and air quality modules in AMET is that the modules can be installed individually, allowing a user to reduce installation time and complexity if only meteorological or air quality analysis is required.

A more detailed description of AMET can be found at https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool, including a flow diagram of the AMET system and example output plots from the tool. The AMET github repository resides at https://github.com/USEPA/AMET. The repository includes the latest version of AMET, along with a complete description of the tool, a User's Guide, an Installation Guide, and a Quick Start Guide. 

#### Observation data for model evaluation
AMET requires observation data to be in a specific format.  AMET-ready observation data files going back to 2000 are available on the CMAS Data Warehouse Google Drive:    
[North America Air Quaility Observation Files](https://drive.google.com/drive/folders/1QUlUXnHXvXz9qwePi5APzzHkiH5GWACw?usp=drive_link)

The network data available include: AERONET, AMON, AQS, CASTNET, CSN, FLUXNET, IMPROVE, NADP, NAPS, NOAA ESRL, SEARCH, and TOAR.

## 8.5 Visualization Environment for Rich Data Interpretation (VERDI)

The Visualization Environment for Rich Data Interpretation (VERDI) is a visual analysis tool for evaluating and plotting multivariate gridded results from meteorological and air quality models.  VERDI is written in Java, so it can be run on a variety of computer operating systems; VERDI packages are currently released for Linux, Windows, and Mac.  In addition to supporting the CMAQ modeling system, VERDI also currently supports analysis and visualization of model results from the regional [Weather Research and Forecasting (WRF) model](https://ncar.ucar.edu/what-we-offer/models/weather-research-and-forecasting-model-wrf), the global [Model for Prediction Across Scales (MPAS)](https://ncar.ucar.edu/what-we-offer/models/model-prediction-across-scales-mpas), the [Meteorology-Chemistry Interface Processor (MCIP)][link_8.5], and the [Comprehensive Air Quality Model with Extensions (CAMx)](http://www.camx.com).  In addition, VERDI can read and overlay observational data at monitoring site locations to visually compare model results to observations, both spatially and temporally.

VERDI’s interactive graphical user interface (GUI) allows for quick examination of model results, while the command line scripting capability in VERDI can be used for more routine analysis and plot production.  Supported input data formats include I/O API, netCDF (both WRF-style and MPAS-style), and UAM-IV from models and ASCII text, and netCDF for observational data sets.  Supported map projections include Lambert conformal conic, Mercator, Universal Transverse Mercator, and polar stereographic.  

Once data are loaded into VERDI, individual selected variables can be plotted or utilized as inputs to mathematical formulas which can then be plotted.  Available plot types include spatial tile, areal interpolation based on shapefiles, vertical cross section, time series, time series bar, scatter, and 3-D contour plots.  Plots can then be enhanced with overlays of observations from monitoring sites, wind vectors, grid lines/cell boundaries, and additional GIS layers, such as boundaries for states, counties, HUCs (hydrologic unit codes), rivers, roads, and user-defined shapefiles.  Plotting of variables can be limited to specified spatial and/or temporal ranges, with minimum/maximum values for the variable for the displayed area and time automatically shown at the bottom of each plot frame.  

Plots can be saved as raster images (BMP, JPEG, PNG, TIFF) of a chosen pixel size, vector images (EPS), or animated GIF “movies.”  Areal ESRI-compatible shapefiles and ASCII text or comma-separated-values can also be exported.  Interactive analysis is aided with the ability to quickly zoom into areas of interest and to probe data values within a grid cell.  To facilitate plot reproducibility, VERDI can save the session as a project file and the customization of each plot (e.g., data range, color palette, font characteristics, titles, and labels) as a plot configuration file.  Plus, quick statistical analysis of the displayed data is easily accomplished by using VERDI’s built-in algorithms for minimum/maximum, mean, geometric mean, median, first and third quartiles, variance, standard deviation, coefficient of variance, range, interquartile range, sum, timesteps of minimum and maximum, hours of non-compliance, maximum 8-h mean, count, fourth max, and custom percentiles.

The CMAS Center currently hosts VERDI at https://www.cmascenter.org/verdi, providing a brief description with links to download VERDI and its documentation.  The main code repository for VERDI resides at https://github.com/CEMPD/VERDI where users can download the latest release, peruse the documentation, and note the latest known issues and bugs.


<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch07_model_outputs.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch09_process_analysis.md)<br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->

<!-- START_OF_COMMENT --> 

[link_8.2]: ../../POST/combine/
[link_8.3]: ../../POST/sitecmp_dailyo3/
[link_8.3_II]: ../../POST/sitecmp_dailyo3/scripts/
[link_8.3_III]: ../../POST/sitecmp/
[link_8.3_IV]: ../../POST/sitecmp/scripts/
[link_8.5]: ../../PREP/mcip/

<!-- END_OF_COMMENT -->

[link_8.2]: https://github.com/USEPA/CMAQ/blob/main/POST/combine/
[link_8.3]: https://github.com/USEPA/CMAQ/blob/main/POST/sitecmp_dailyo3/
[link_8.3_II]: https://github.com/USEPA/CMAQ/blob/main/POST/sitecmp_dailyo3/scripts/
[link_8.3_III]: https://github.com/USEPA/CMAQ/blob/main/POST/sitecmp/
[link_8.3_IV]: https://github.com/USEPA/CMAQ/blob/main/POST/sitecmp/scripts/
[link_8.5]: https://github.com/USEPA/CMAQ/blob/main/PREP/mcip/
