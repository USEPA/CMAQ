
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch07_model_outputs.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch09_new_simulation.md)

<!-- END COMMENT -->

# 8. Analysis Tools for CMAQ output

## 8.1 Overview
Many software programs are freely available for pre- and post-processing, evaluating and visualizing CMAQ output. Examples of such freeware are provided in Table [8-1](#Analysis_Software_Table). Several other commercial packages, including MATLAB and IDL, also support the analysis and visualization of CMAQ inputs and outputs. Most visualization and analysis software that supports netCDF file formats will work with CMAQ output data. 

<a id=Analysis_Software_Table></a>
**Table 8-1. Software Programs for Evaluating and Visualizing CMAQ Data**

|**Software**|**Description**|     **Source**    |
|------------|-------------------------------|---------------------------------------------|
|***Post-processing***|||
|CMAQ POST Tools|Programs released with CMAQ source code to prepare output data for model evaluation|[https://github.com/USEPA/CMAQ](../../POST)|
|I/O API Tools|Postprocessing tools for manipulating data in the I/O API/netCDF format|[https://www.cmascenter.org/ioapi](https://www.cmascenter.org/ioapi)|
|NCO|netCDF Operators: Postprocessing tools for manipulating data in the netCDF format|[http://nco.sourceforge.net](http://nco.sourceforge.net)
|***Evaluation/Visualization***| | |
|AMET|Atmospheric Model Evaluation Tool for analysis and evaluation of meteorlogical and air quality models|[https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool](https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool)|
|VERDI|Visualization Environment for Rich Data Interpretation for graphical analysis of netCDF gridded data|[http://www.verdi-tool.org](http://www.verdi-tool.org/)|
|PseudoNetCDF|Reading, plotting, and sometimes writing capabilities for atmospheric science data formats including CMAQ files|[https://github.com/barronh/pseudonetcdf/wiki](https://github.com/barronh/pseudonetcdf/wiki)|
|RSIG|2D and 3D visualization of satellite and modeled data|[https://www.epa.gov/hesc/remote-sensing-information-gateway](https://www.epa.gov/hesc/remote-sensing-information-gateway)|
|NCL|NCAR Command Language for scientific data processing and visualization|[http://www.ncl.ucar.edu](http://www.ncl.ucar.edu)|
|IDV|Integrated Data Viewer for 3-D graphical analysis of netCDF gridded data|[http://www.unidata.ucar.edu/software/idv/](http://www.unidata.ucar.edu/software/idv/)|

This chapter briefly describes how to use some of the software tools supported by the EPA and CMAS to aggregate CMAQ output, pair aggregated CMAQ output in space and time to air quality observations, create various evaluation plots, and visualize model fields.

## 8.2 Aggregating and Transforming Model Species into Variables of Interest Using CMAQ POST Tools
Many CMAQ output species need to be postprocessed to allow comparisons to measurements. For example, the CMAQ aerosol module explicitly represents a number of invididual PM2.5 species that need to be combined for comparisons to measured total PM2.5 mass. The `combine` utility released as part of the CMAQ POST tools can be used to accomplish this task. Since the number and definition of model output species differs between chemical mechanisms, the `combine` utility relies on a mechanism-specific "Species Definition" file that prescribes how model output variables should be combined to become comparable to different measured gas, particle and deposition species.  When you download the CMAQ code for version 5.2 or later, these definition files are automatically included under the subdirectory "CCTM/src/MECHS". Within each of the listed mechanism folders, you will find  files "SpecDef_MECH_NAME.txt" and "SpecDef_dep_MECH_NAME.txt" that contain a long list of species definitions and corresponding documentation.  For example, to find how to calculate PM2.5 using the cb6r3_ae7_aq mechanism, open the file "SpecDef_cb6r3_ae7_aq.txt" and read the documentation on PM2.5 calculations.  The species definition file will indicate which species should be included in PM2.5 (for example: sulfate, ammonium, and organic carbon) as well as factors to obtain the fraction of each CMAQ size distribution mode that corresponds to 2.5 micron (diameter) and smaller particles.  Similar information is available for calculating PM10 and PM1.  These species definition files are designed to be used with the `combine` utility to extract model variables that can be matched to observed quantities from specific monitoring networks. More information on the `combine` utility and its use can be found in this [README file.]( ../../POST/combine/README.md)

## 8.3 Model-Observation Pairing for Model Evaluation Using CMAS POST Tools
 Once model output has been processed using `combine`, the `sitecmp` and `sitecmp_dailyo3` utilities can be used used to match air pollutant measurements with the appropriate model predicted output.  This pairing of model and observed variables is specified in the run scripts for `sitecmp` and `sitecmp_dailyo3`.  In `sitecmp_dailyo3` this step is controlled by the definition of environment variables OBS_SPECIES and OZONE.  See the [README.md](../../POST/sitecmp_dailyo3/README.md) and the sample run script in the [sitecmp_dailyo3 scripts](../../POST/sitecmp_dailyo3/scripts) folder for more information on setting these environment variables.  The run script for the `sitecmp` utility can be customized for many different types of chemical and meteorological quantities as described in the [README.md](../../POST/sitecmp/README.md) for `sitecmp`.  A sample run script for the AQS network is provided in the [sitecmp scripts](../../POST/sitecmp/scripts) folder.  In addition, the [README.txt](../../POST/sitecmp/scripts/README.txt) file within the sitecmp scripts folder provides the configuration options for other monitoring networks.  Note that there are multiple formats for CSN and SEARCH observed data files depending on the year.  The README.txt file is broken into different sections to reflect the change in species names in the observation files for these two networks.  (For example, elemental carbon measurements from the CSN network are labeled as “ec_niosh” in 2009 and earlier, “ec_tor” in 2010, and “88380_val” starting in 2011.)

### 8.3.1 Spatial matching in sitecmp and sitecmp_dailyo3
* In sitecmp, model values are extracted for the grid cell containing the monitor location. In sitecmp_dailyo3 the model value of the grid cell containing the observation is provided, as well as the maximum model value of the 9 grid cells centered on the monitor location. These variables in the output file contain the character string "9cell" in the variable name.

### 8.3.2 Temporal matching in sitecmp and sitecmp_dailyo3
* **AQS_HOURLY, CASTNET_HOURLY, SEARCH_HOURLY, AERONET**: Air quality observations are assumed to be hourly averages time stamped at the beginning of the hour with local standard time (LST). The sitecmp utility will use the time stamp from the observations to determine the matching model time step, accounting for the time zone of the monitor. Therefore, best practice would be for the model time step to also represent hourly average time stamped at the beginning of the hour. This can be accomplished by running the combine utility on the CMAQ "ACONC" output files which follow this convention. These networks also include meteorological measurements. Since meteorological observations are near instantaneous measurements (e.g. 1 or 5 minute averages), using meteorological fields from MCIP or wrfout in combine results in the correct matching since these fields are also instantaneous. One exception is the calculation of modeled relative humidity (RH). This variable is not available from MCIP or wrfout files but is stored in the CMAQ "APMDIAG" output file which represents hourly average values. This creates a slight inconsistency between observed and modeled values for this variable in the sitecmp output files. Note that modeled and observed precipitation for a given hour represents the hourly total rather than the hourly average. 
* **AQS_DAILY_O3, CASTNET_DAILY_O3**: sitecmp_dailyo3 computes various daily metrics from observed and modeled hourly ozone values. The temporal matching of the hourly observed and modeled values used in these computations follows the same approach described above for AQS_HOURLY. Therefore it is best practice to use output from CMAQ "ACONC" files for modeled ozone predictions. Details on the computation of the various daily metrics is provided in the sitecmp_dailyo3 documentation.
* **AQS_DAILY, CSN, IMPROVE, SEARCH_DAILY**: Air quality observations are daily averages time stamped with the date in local standard time. The sitecmp utility will use the date from the observations to compute daily averages using 24 hourly modeled values, accounting for the time zone of the monitor. Therefore it is best practice to use output from CMAQ "ACONC" files for modeled air quality predictions which represent hourly average concentrations.
* **CASTNET**: Air quality observations are weekly averages time stamped with beginning and end date and time of the weekly interval in local standard time. The sitecmp utility will use the start and end date and time from the observations to compute weekly averages using hourly modeled values, accounting for the time zone of the monitor. Therefore it is best practice to use output from CMAQ "ACONC" files for modeled air quality predictions which represent hourly average concentrations.
* **NADP**: Air quality observations are weekly sums time stamped with beginning and end date of the weekly interval in local standard time. The sitecmp utility will use the start and end date from the observations to compute weekly sums using hourly modeled values, accounting for the time zone of the monitor. Observations are matched to output from CMAQ "DEP" files which represent hourly totals.

## 8.4 Evaluation of model output with the Atmospheric Model Evaluation Tool (AMET)

The Atmospheric Model Evaluation Tool (AMET) was developed to aid in its evaluation of the meteorological and air quality models within the CMAQ modeling system. AMET organizes, provides consistency and speeds-up the evaluation process for operational meteorological and air quality model simulations. Although it was developed specifically to aid in the evaluation of the CMAQ modeling system, the AMET software can be adapted to work with other modeling systems. 

AMET is used to pair observations and gridded model output in space and time and to evaluate the model performance for a set of predicted or diagnosed atmospheric fields. There are separate modules in AMET for evaluating meteorological and air quality model output. This separation is necessary because both the observed and predicted meteorological and air quality data are quite different, utilizing different file formats for both the observed and model data. In addition, the observed meteorological and air quality data are often obtained from networks that use different sampling protocols, which can make pairing meteorological and air quality data together difficult. One advantage of separate meteorological and air quality modules in AMET is that the modules can be installed individually, allowing a user to reduce installation time and complexity if only meteorological or air quality analysis is required.

A more detailed descripition of AMET can be found here https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool, including a flow diagram of the AMET system and example output plots from the tool. The AMET github repository can be found here https://github.com/USEPA/AMET. The repository includes the latest version of AMET, along with a complete description of the tool, a comprehensive User's Guide, an Installation Guide, and a Quick Start Guide. Finally, additional information regarding AMET (including how to download AMET-ready observation data files) can be found on the CMAS Center website here https://www.cmascenter.org/amet/.

## 8.5 Probing Model Output with Visualization Environment for Rich Data Interpretation (VERDI)


<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch07_model_outputs.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch09_new_simulation.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
