
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch06_model_configuration_options.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch08_analysis_tools.md)

<!-- END COMMENT -->

# 7. Model Output Files

## 7.1 Introduction
In this section, details on the routine CCTM output files are provided. All CMAQ programs produce model output files that adhere to the netCDF format.  In addition to model data output, CMAQ can optionally produce ASCII log files that contain intermediate model execution information from the various CMAQ processes and captured with respect to processor number. If the log file option is not selected by the user and the simulation is run interactively, CMAQ will write all of the log information to the screen along with the standard error, which can be captured to a text file using basic UNIX syntax. Additional output files are created when using the Process Analysis (PA), Integrated Source Apportionment Method (ISAM) and Detailed Emissions Scaling, Isolation and Diagnostics Module (DESID) options.  The files associated with these options are discussed in [Chapter 9](CMAQ_UG_ch09_process_analysis.md), [Chapter 11](CMAQ_UG_ch11_ISAM.md), and [Appendix B](Appendix/CMAQ_UG_appendixB_emissions_control.md), respectively.

<a id=Output_Table></a>
<a id=Table7-1></a>

**Table 7-1. CMAQ Output files**

|**File Name<sup>1</sup>**|**File Type**|**Time-Dependence<sup>2</sup>**|**Spatial Dimensions<sup>3</sup>** |
|----------------------------|------|----|-----------------------------------|
|**Standard**| | | |
|[Output Log](#cmaq_output_log) <a id=cmaq_output_log_t></a>|ASCII|n/a|n/a
|[CCTM_CONC](#conc)<a id=conc_t></a>|GRDDED3|Hourly|XYZ'
|[CCTM_ACONC](#aconc) <a id=aconc_t></a>|GRDDED3|Hourly|XYZ'
|[CCTM_DRYDEP](#drydep) <a id=drydep_t></a>|GRDDED3|Hourly|XY
|[CCTM_WETDEP1](#wetdep) <a id=wetdep_t></a>|GRDDED3|Hourly|XY
|**Restart**| | | |
|[CCTM_CGRID](#cgrid) <a id=cgrid_t></a>|GRDDED3|1-hour|XYZ
|[CCTM_MEDIA](#media)<a id=media_t></a>|GRDDED3|Hourly|XY
|[CCTM_SOILOUT](#soilout) <a id=soilout_t></a>|GRDDED3|n/a (see detailed file description below)|XY
|**Diagnostic and Advanced**| | | |
|[CCTM_PMDIAG](#pmdiag) <a id=pmdiag_t></a>|GRDDED3|Hourly|XYZ'
|[CCTM_APMDIAG](#apmdiag) <a id=apmdiag_t></a>|GRDDED3|Hourly|XYZ'
|[CCTM_B3GTS_S](#b3gts) <a id=b3gts_t></a>|GRDDED3|Hourly| XY
|[CCTM_DEPV](#depv) <a id=depv_t></a>|GRDDED3|Hourly|XY
|[CCTM_PT3D](#pt3d) <a id=pt3d_t></a>|GRDDED3|Hourly|XYZ
|[CCTM_DUSTEMIS](#dust) <a id=dust_t></a>|GRDDED3|Hourly|XY
|[CCTM_DEPVMOS](#depv_mos) <a id=depv_mos_t></a>|GRDDED3|Hourly|XYW
|[CCTM_DEPVFST](#depv_fst) <a id=depv_fst_t></a>|GRDDED3|Hourly|XYW
|[CCTM_DDEP_MOS](#dry_dep_mos) <a id=dry_dep_mos_t></a>|GRDDED3|Hourly|XYW
|[CCTM_DDEP_FST](#dry_dep_fst) <a id=dry_dep_fst_t></a>|GRDDED3|Hourly|XYW
|[CCTM_LTNGHRLY](#ltngdiag1) <a id=ltngdiag1_t></a>|GRDDED3|Hourly|XYZ
|[CCTM_LTNGCOL](#ltngdiag2) <a id=ltngdiag2_t></a>|GRDDED3|Hourly|XY
|[CCTM_PHOTDIAG1](#ctm_rj_1) <a id=ctm_rj1_t></a>|GRDDED3|Hourly|XY
|[CCTM_PHOTDIAG2](#ctm_rj_2) <a id=ctm_rj2_t></a>|GRDDED3|Hourly|XYZ'
|[CCTM_PHOTDIAG3](#ctm_rj_3) <a id=ctm_rj3_t></a>|GRDDED3|Hourly|XYZ'
|[CCTM_SSEMIS](#ssemis) <a id=ssemis_t></a>|GRDDED3|Hourly|XY
|[CCTM_WETDEP2](#wetdep2) <a id=wetdep2_t></a>|GRDDED3|Hourly|XY
|[CCTM_VEXT](#vext) <a id=vext_t></a>|GRDDED3|Hourly|WZ

<sup>1</sup>By default, output files are named CCTM_XXX_${CTM_APPL}.nc where XXX is the file identifier and ${CTM_APPL} is a user defined string that identifies the model run.   
<sup>2</sup>While "Hourly" is indicated, users may define a different time step (e.g. 30 minutes) for model output by changing the TSTEP variable in the runscript. From here onward, the term "Hourly" will be used for description purposes.    
<sup>3</sup>X is the dimension along the x-axis, Y is the dimension along the y-axis, Z is the vertical dimension, Z' is the user pre-defined size of the vertical dimension controlled by the environment variables CONC_BLEV_ELEV, ACONC_BLEV_ELEV, APMDIAG_BLEV_ELEV, and NLAYS_PHOTDIAG (range from 1 to all layers) and W is a non-layer dimension, e.g. number of LU fractions, number of sites for vertical extraction.    
<sup>4</sup>A special ASCII output file, FLOOR_xxx with xxx being the processor number, contains information when a simulation results in negative concentrations. 

## 7.2 CCTM Output Files

Some output files created by the CCTM are considered standard output as these contain hourly concentration and deposition values and information to document the run. Options for these files are controlled by their corresponding environment variable in the CCTM RunScript (e.g. run_cctm.csh).
<a id=cmaq_output_log></a>

**CMAQ output log**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#cmaq_output_log_t)
<!-- END COMMENT -->
All of the CMAQ processors generate standard output and standard error during execution. When you run the CMAQ executable interactively, diagnostic output information can be captured to a log file using a UNIX redirect command:

```
run.cctm >& tee cctm.log
```


The LOGFILE environment variable allows users to specify the name of a log file for capturing the standard output from the program. If this variable is not set, the standard output is written to the terminal and can be captured using the UNIX redirect command (“>”), as shown in the example above.

<a id=conc></a>

**CCTM_CONC: CCTM hourly instantaneous concentration file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#conc_t)
<!-- END COMMENT -->

The 2-D or 3-D CCTM hourly concentration file (CONC) contains instantaneous gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>) at the end of each model output time step. The number and type of species contained in the CONC files depends on the chemical mechanism and aerosol model configurations that are selected when the CCTM is compiled. The [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) within the mechanism directories list the modeled species, and contain a column that specifies which species are written to the CONC files (e.g. [AE_cb6r3_ae7_aq.nml](../../CCTM/src/MECHS/cb6r3_ae7_aq/AE_cb6r3_ae7_aq.nml)). The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the CONC file by editing the CONC column in the NameList file(s) to reduce the number of species that are written to, and thus the size of the CONC file. Users can also specify the output species list (including temperature, pressure & relative humidity) by modifying the environment variable CONC_SPCS in the RunScript which overrides the setting of the CONC column in the NameList file(s). By default, concentrations for all model layers are output to the CONC file.  Users may specify the layers to output using the CONC_BLEV_ELEV environment variable in the RunScript where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number.


<a id=aconc></a>

**CCTM_ACONC: hourly average concentration file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#aconc_t)
<!-- END COMMENT -->

The 2-D or 3-D CCTM integral average concentration file contains average model species concentrations for each model hour, as opposed to instantaneous concentrations at the end of each output time step. The species written to the ACONC file are set by the user in the CCTM RunScript using the environment variable AVG_CONC_SPCS. The model layers for which hourly average concentrations are calculated are also set in the CCTM RunScript using the environment variable ACONC_BLEV_ELEV, where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number. An example setting for the ACONC_BLEV_ELEV variable is “1 6”, which defines layers 1 through 6 as the vertical extent for which hourly average concentrations are calculated and written to the ACONC file.

<a id=drydep></a>

**CCTM_DRYDEP: hourly cumulative dry deposition file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#drydep_t)
<!-- END COMMENT -->

The 2-D CCTM dry deposition file contains cumulative hourly dry deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates dry deposition for all of the species listed in the dry deposition column of the [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the dry deposition file by editing the DDEP column in the NameList file(s).

##### NH<sub>3</sub> flux components in CCTM_DRYDEP
CMAQ v5.3 and later contains two build-time options for calculating dry deposition/surface exchange: M3DRY and STAGE. (See [Section 6.8 ](CMAQ_UG_ch06_model_configuration_options.md#68-dry-depositionair-surface-exchange) for further information).  Both M3DRY and STAGE support modeling ammonia bidirectional surface flux.  The definition of the NH3 flux components in the CCTM_DRYDEP file will depend on whether or not bidirectional NH<sub>3</sub> flux option has been enabled (a run-time option controlled by setting CTM_ABFLUX to Y or N).  When the model is run without the bidirectional NH<sub>3</sub> flux option enabled (CTM_ABFLUX set to N), the variable NH3 in the CCTM_DRYDEP file represents the unidirectional ammonia dry deposition flux in both STAGE and M3DRY.

When the model is run with CTM_ABFLUX set to Y, the CCTM_DRYDEP file will contain additional NH3 flux components.  The variable names and definitions are defined in Table 7-2.  Note that these variables definitions may not agree with the definitions used in CMAQ versions prior to version 5.3.2.    

<a id=Table7-2></a>
**Table 7-2. NH3 Flux components in CCTM_DRYDEP Output files when ammonia bidirectional surface flus is enabled**

|**Variable Name**|**Variable Description**|
|:----:|:----------------------------:|
|NH3|Downward Deposition Flux (always positive)  |	
|NH3_Emis|Upward Emissions Flux (always positive)	   |  
|NH3_Flux|Net Flux (positive if downward and negative if upward)  | 
|NH3_Stom*|NH3 flux from leaf stomatal pathways (positive values are emissions and negative values are deposition) |
|NH3_Cut*|NH3 flux from leaf cuticular pathways (positive values are emissions and negative values are deposition) |
|NH3_Soil*|NH3 flux from soil pathways (positive values are emissions and negative values are deposition) |
|NH3_Ag*|NH3 flux over agriculture land use (positive values are emissions and negative values are deposition)|
|NH3_Nat*|NH3 flux over non-agriculture land use (positive values are emissions and negative values are deposition) |
|NH3_Wat*|NH3 flux over water bodies (positive values are emissions and negative values are deposition) |

\*Additional diagnostic deposition values available when using the STAGE dry deposition option with bi-directional ammonia flux enabled.

<a id=wetdep></a>

**CCTM_WETDEP1: hourly cumulative wet deposition file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#wetdep_t)
<!-- END COMMENT -->

The 2-D CCTM wet deposition file contains cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s).

## 7.3 Restart Files

There are several files created by the CCTM that are used to enable a restart of the run for any specific day.  The files contain values for parameters at the end of the day which are used to initialize the values for the start of calculations for the next day.

<a id=cgrid></a>

**CCTM_CGRID: gridded concentration restart file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#cgrid_t)
<!-- END COMMENT -->

The 3-D CCTM ending concentration file contains gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>) at the end of each simulation period. The number and types of species contained in the output CGRID files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. This file can be used to initialize CCTM from a simulation period that the model completed. For example, if the CCTM is configuring to produce daily output files, a CGRID file will be written out at the end of each simulation day. These concentrations then become the initial conditions for the next simulation period.

<a id=media></a>

**CCTM_MEDIA: Bidirectional modeling media concentration file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#media_t)
<!-- END COMMENT -->

This 2-D CCTM file contains the soil NH<sub>4</sub><sup>+</sup> and pH concentrations and/or the soil, vegetation and water Hg concentrations. This file is only created when the CTM_ABFLUX environment variable or the CTM_HGBIDI variable in the RunScript is set to Y (Default is N) and is used to initialize the next day of the model simulation.

<a id=soilout></a>

**CCTM_SOILOUT**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#soilout_t)
<!-- END COMMENT -->

This optional 2-D CCTM file contains hourly total rainfall information for subsequent use by the CCTM in-line biogenics module. It is written out at the end of each simulation day and is only created if the CTM_BIOGEMIS environment variable in the RunScript is set to Y (Default is N). With the exception of the first day of the simulation when the environment variable NEW_START is set to TRUE, the previous day's rainfall information contained in the file is used in the calculation of soil NO emissions by the CCTM in-line biogenics module. This is accomplished by setting the SOILINP environment variable in the RunScript for a given day to the CCTM_SOILOUT file created at the end of the previous day's simulation. Note that even though this file contains 24 hourly gridded rainfall fields, it has a time-independent file structure and stores these 24 values as 24 separate time-independent variables (RAINFALL01, ... RAINFALL24). However, while the structure of the file is time-independent, each day's CCTM_SOILOUT file is unique due to the daily variations in meteorology. Therefore, care must be taken to ensure that the SOILINP file specified for a given day is indeed the CCTM_SOILOUT file for the previous day rather than that for a different day.  


## 7.4 Diagnostic and Advanced CMAQ Output Files

Along with the standard output files detailed in the previous section, CCTM can be configured to output several auxiliary files for diagnostic model purposes. Each option is controlled by its corresponding environment variable in the CCTM RunScript (e.g. run_cctm.csh). For logical values, TRUE/T is equivalent to Y and FALSE/F is equivalent to N.

Note that I/O APIv3.2 supports up to MXFILE3=64 open files, each with up to MXVARS3=2048.  Turning on all of the diagnostic and advanced CMAQ output files can exceed this upper limit of open files, leading to a model crash. To avoid this issue, users may use I/O API version 3.2 "large" that increases MXFILE3 to 512 and MXVARS3 to 16384. This version is available as a zip file from the following address:

https://www.cmascenter.org/ioapi/download/ioapi-3.2-large.tar.gz

Installation instructions for I/O API v5.3-large are provided in README.txt in the .tar.gz file. 

<a id=floor></a>

**FLOOR: concentration-reset diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#floor_t)
<!-- END COMMENT -->

This optional ASCII file contains specific gridcells/timesteps in which species with negative concentrations are reset to zero. The location and name of the file is set by the FLOOR_FILE environment variable.

<a id=pmdiag></a>

**CCTM_PMDIAG: instantaneous hourly aerosol diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#pmdiag_t)
<!-- END COMMENT -->

This optional 2-D or 3-D CCTM diagnostic file contains instantaneous information at the end of the hour for each model hour on the geometric mean diameters, geometric standard deviations, bulk densities, 2nd moments and 3rd moments for the lognormal modes. 
It also includes the fraction of each mode that contributes to PM1, PM2.5, and PM10 and the AMS transmission factor for each mode. Many diagnostics relating to heterogenous chemistry are provided including the N<sub>2</sub>O<sub>5</sub> reaction probability, 
the ClNO<sub>2</sub> reaction yield, and the IEPOX uptake coefficient. Units for all variables are specified in the output file. The number of layers in this output file is determined by the setting of the CONC_BLEV_ELEV environment variable in the RunScript. This file is only created if the CTM_APMDIAG environment variable in the RunScript is set to Y (Default is N).

<a id=apmdiag></a>

**CCTM_APMDIAG: average hourly aerosol diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#apmdiag_t)
<!-- END COMMENT -->

This optional 2-D or 3-D CCTM diagnostic file contains integral average information for each model hour on the geometric mean diameters, geometric standard deviations, bulk densities, 2nd moments and 3rd moments for the lognormal modes. 
It also includes the fraction of each mode that contributes to PM1, PM2.5, and PM10 and the AMS transmission factor for each mode. Many diagnostics relating to heterogenous chemistry are provided including the N<sub>2</sub>O<sub>5</sub> reaction probability, 
the ClNO<sub>2</sub> reaction yield, and the IEPOX uptake coefficient. Units for all variables are specified in the output file. The number of layers in this output file is determined by the setting of the APMDIAG_BLEV_ELEV environment variable in the RunScript. This file is only created if the CTM_APMDIAG environment variable in the RunScript is set to Y (Default is N).

<a id=b3gts></a>

**CCTM_B3GTS_S: biogenic emissions diagnostic file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#b3gts_t)
<!-- END COMMENT -->

This optional 2-D CCTM hourly output file contains total hourly biogenic emissions in mass units calculated in-line by the CCTM when the CTM_BIOGEMIS environment variable is set to Y. This file is only created if the B3GTS_DIAG environment variable in the RunScript is set to Y (Default is Y).

<a id=depv></a>

**CCTM_DEPV: inline deposition diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#depv_t)
<!-- END COMMENT -->

This optional 2-D CCTM file contains the deposition velocity (m/s) for each chemical species calculated for the final time step for the hour. CCTM calculates the deposition velocity for all of the species listed in the deposition velocity column of the [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the deposition velocity file by editing the DDEP column in the NameList file(s). This file is only created if the CTM_DEPV_FILE environment variable in the RunScript is set to Y (Default is N). 

<a id=pt3d></a>

**CCTM_PT3D: point source emissions diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#pt3d_t)
<!-- END COMMENT -->

This optional 3-D CCTM file records the 3-D point source emissions for each layer as a linear average over the output timestep. This file is only created if the PT3DDIAG environment variable in the RunScript is set to Y (Default is N).

<a id=dust></a>

**CCTM_DUSTEMIS: dust emissions diagnostic file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#dust_t)
<!-- END COMMENT -->

This optional 2-D CCTM hourly output file contains dust emissions in mass units calculated in-line by the CCTM when the CTM_WB_DUST environment variable is set to Y. This file is only created if the CTM_DUSTEM_DIAG environment variable in the RunScript is set to Y (Default is N).

<a id=depv_mos></a>

**CCTM_DEPVMOS: land use specific deposition velocity file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#depv_mos_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) for the final time step of the hour for each land use type within a grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers".  This file is only created if the DepMod environment variable in the BuildScript is set to stage (rather than m3dry) and if the CTM_MOSAIC environment variable in the RunScript is set to Y (Default is N).


<a id=dry_dep_mos></a>

**CCTM_DDMOS: land use specific deposition flux file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#dry_dep_mos_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) for the hour for each land use type within each grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers". This file is only created if the ModDepv environment variable in the BuildScript is set to stage (rather than m3dry) and if the CTM_MOSAIC environment variable in the RunScript is set to Y (Default is N).


<a id=depv_fst></a>

**CCTM_DEPVFST: stomatal deposition velocity file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#depv_fst_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) through the stomatal pathway for the final time step of the hour for each land use type within a grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers". This file is only created if the ModDepv environment variable in the BuildScript is set to stage (rather than m3dry) and if the CTM_FST environment variable in the RunScript is set to Y (Default is N).

<a id=dry_dep_fst></a>

**CCTM_DDFST: stomatal flux file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#dry_dep_fst_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) through the stomatal pathway for the hour for each land use type within each grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers". This file is only created if the ModDepv environment variable in the BuildScript is set to stage (rather than m3dry) and if the CTM_FST environment variable in the RunScript is set to Y (Default is N).

<a id=ltngdiag1></a>

**CCTM_LTNGHRLY: hourly lightning emissions file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ltngdiag1_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains hourly lightning NO emissions (mol/s) calculated in-line by the CCTM when setting the CTM_LTNG_NO environment variable to Y. This file is only created if the CTM_LTNGDIAG_1 environment variable in the RunScript is set to Y (Default is N).

<a id=ltngdiag2></a>

**CCTM_LTNGCOL: hourly column total lightning emissions**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ltngdiag2_t)
<!-- END COMMENT -->

This optional 2-D CCTM file contains hourly column-total lightning NO emissions (mol/s) calculated in-line by the CCTM when setting the CTM_LTNG_NO environment variable to Y. This file is only created if the CTM_LTNGDIAG_2 environment variable in the RunScript is set to Y (Default is N).

<a id=ctm_rj1></a>

**CCTM_PHOTDIAG1: In-line photolysis inputs and outputs - summary file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ctm_rj1_t)
<!-- END COMMENT -->

This optional 2-D CCTM file contains general summary information for the photolysis calculation including the surface albedo, 
select photolysis rates and flux values.  This file is only created if the CTM_PHOTDIAG environment variable in the RunScript is set to Y (Default is N).

<a id=ctm_rj2></a>

**CCTM_PHOTDIAG2_2: In-line photolysis output – gridded photolysis rates**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ctm_rj2_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains the photolysis rates calculated in-line by the CCTM.  The number of layers is set by the  NLAYS_PHOTDIAG environment variable (Default is all layers). This file is only created if the CTM_PHOTDIAG environment variable in the RunScript is set to T (Default is N).

<a id=ctm_rj3></a>

**CCTM_PHOTDIAG3: In-line photolysis inputs and outputs – detailed**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ctm_rj3_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains detailed inputs and results from the photolysis rate calculation done in-line by the CCTM. The number of layers is set by the  NLAYS_PHOTDIAG environment variable (Default is all layers). The number of wavelengths included in the file is set by the NWAVE_PHOTDIAG environment variable (Default is all wavelengths). This file is only created if the CTM_PHOTDIAG environment variable in the RunScript is set to T (Default is N).

<a id=ssemis></a>

**CCTM_SSEMIS: Sea salt emissions diagnostic file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ssemis_t)
<!-- END COMMENT -->

This optional 2-D CCTM hourly output file contains calculated sea salt emissions (g/s). This file is only created if the CTM_SSEMDIAG environment variable in the RunScript is set to Y (Default is N).

<a id=wetdep2></a>

**CCTM_WETDEP2: CCTM cloud diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#wetdep2_t)
<!-- END COMMENT -->

In CMAQ, wet deposition is calculated separately for resolved (grid-scale) clouds and for convective (subgrid) clouds. The WETDEP1 file contains the total wet deposition, i.e., the sum of both resolved-scale and subgrid-scale deposition. The WETDEP2 file contains only subgrid-scale deposition, plus some cloud diagnostic variables. The 2-D CCTM wet deposition file (WETDEP2) includes cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s). This file is only created if the CLD_DIAG environment variable in the RunScript is set to Y (Default is N).

<a id=vext></a>

**CCTM_VEXT: file of vertical profiles of concentration at selected sites**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#vext_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains vertical profiles of the concentration of multiple chemical species for latitude / longitude coordinates specified in the VERTEXT_COORD_PATH file. The species written to this output file are identical to those written to the 3D CONC file which in turn are controlled either by the setting of CONC_SPCS in the RunScript or the last column in the GC, AE, NR, and TR namelist files. There is one row for each location specified. The coordinates for each location are echoed in the file metadata in the "history" field. This file is only created if the VERTEXT environment variable in the RunScript is set to Y (Default is N).

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch06_model_configuration_options.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch08_analysis_tools.md)<br>
CMAQ User's Guide (c) 2021<br>

<!-- END COMMENT -->
