
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch06_model_configuration_options.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch08_analysis_tools.md)

<!-- END COMMENT -->

# 7. Model Output Files

In this section, details on the CCTM output files are provided. All CMAQ programs produce model output files that adhere to the netCDF format.  In addition to model data output, CMAQ can optionally produce ASCII log files that contain intermediate model execution information from the various CMAQ processes and captured with respect to processor number. If the log file option is not selected by the user and the simulation is run interactively, CMAQ will write all of the log information to the screen along with the standard error, which can be captured to a text file using basic UNIX syntax.

<a id=Output_Table></a>

**Table 7-1. CMAQ Output files**

|**File Name**|**File Type**|**Time-Dependence***|**Spatial Dimensions** ** |
|----------------------------|------|----|-----------------------------------|
|**Standard**| | | |
|[Output Log](#cmaq_output_log) <a id=cmaq_output_log_t></a>|ASCII|n/a|n/a
|[CTM_CONC_1](#conc)<a id=conc_t></a>|GRDDED3|Hourly|X * Y * Z
|[A_CONC_1](#aconc) <a id=aconc_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_DRY_DEP_1](#drydep) <a id=drydep_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_WETDEP_1](#wetdep) <a id=wetdep_t></a>|GRDDED3|Hourly|X * Y * 1
|**Restart**| | | |
|[S_CGRID](#cgrid) <a id=cgrid_t></a>|GRDDED3|1-hour|X * Y * Z
|[MEDIA_CONC](#media)<a id=media_t></a>|GRDDED3|Hourly|X * Y * 1
|[SOILOUT](#soilout) <a id=soilout_t></a>|GRDDED3|Hourly|X * Y * 1
|**Diagnostic and Advanced**| | | |
|[CTM_PMDIAG_1](#pmdiag) <a id=pmdiag_t></a>|GRDDED3|Hourly|X * Y * Z
|[CTM_APMDIAG_1](#apmdiag) <a id=apmdiag_t></a>|GRDDED3|Hourly|X * Y * Z'
|[B3GTS_S](#b3gts) <a id=b3gts_t></a>|GRDDED3|Hourly| X * Y * 1]
|[CTM_DEPV_DIAG](#depv) <a id=depv_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_PT3D_DIAG](#pt3d) <a id=pt3d_t></a>|GRDDED3|Hourly|X * Y * Z
|[CTM_DUST_EMIS_1](#dust) <a id=dust_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_DEPV_MOS](#depv_mos) <a id=depv_mos_t></a>|GRDDED3|Hourly|X * Y * W
|[CTM_DEPV_FST](#depv_fst) <a id=depv_fst_t></a>|GRDDED3|Hourly|X * Y * W
|[CTM_DRY_DEP_MOS](#dry_dep_mos) <a id=dry_dep_mos_t></a>|GRDDED3|Hourly|X * Y * W
|[CTM_DRY_DEP_FST](#dry_dep_fst) <a id=dry_dep_fst_t></a>|GRDDED3|Hourly|X * Y * W
|[CTM_VDIFF_DIAG](#vdiff_diag) <a id=vdiff_diag_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_VSED_DIAG](#vsed_diag)<a id=vsed_diag_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_LTNGDIAG_1](#ltngdiag1) <a id=ltngdiag1_t></a>|GRDDED3|Hourly|X * Y * Z
|[CTM_LTNGDIAG_2](#ltngdiag2) <a id=ltngdiag2_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_RJ_1](#ctm_rj_1) <a id=ctm_rj1_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_RJ_2](#ctm_rj_2) <a id=ctm_rj2_t></a>|GRDDED3|Hourly|X * Y * Z'
|[CTM_RJ_3](#ctm_rj_3) <a id=ctm_rj3_t></a>|GRDDED3|Hourly|X * Y * Z'
|[CTM_SSEMIS_1](#ssemis) <a id=ssemis_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_WETDEP_2](#wetdep2) <a id=wetdep2_t></a>|GRDDED3|Hourly|X * Y * 1
|[CTM_VEXT_1](#vext) <a id=vext_t></a>|GRDDED3|Hourly|X * Y * Z

 *1. While "Hourly" is indicated, users may define a different time step (e.g. 30 minutes) for model output by changing the TSTEP variable in the runscript. From here onward, the term "Hourly" will be used for description purposes.    
 ** 2. X is the dimension along the x-axis, Y is the dimension along the y-axis, Z is the vertical dimension, Z' is the user pre-defined size of the vertical dimension (ragne from 1 to all layers) and W is a non layer dimension, e.g. number of LU fractions.    
 *** 3. A special ASCII output file, FLOOR_xxx with xxx being the processor number, contains information when a simulation results in negative concentrations. 

## 7.1 CCTM Output Files

Some output files created by the CCTM are considered standard output as these contain hourly concentration and deposition values and information to document the run.
<a id=cmaq_output_log></a>
### CMAQ output log
[Return to Table 7-1](#cmaq_output_log_t)

All of the CMAQ processors generate standard output and standard error during execution. When you run the CMAQ executable interactively, diagnostic output information can be captured to a log file using a UNIX redirect command:

```
run.cctm >& tee cctm.log
```


The LOGFILE environment variable allows users to specify the name of a log file for capturing the standard output from the program. If this variable is not set, the standard output is written to the terminal and can be captured using the UNIX redirect command (“>”), as shown in the example above.

<a id=conc></a>
### CTM_CONC_1: CCTM hourly instantaneous concentration file
[Return to Table 7-1](#conc_t)

The 3-D CCTM hourly concentration file (CONC) contains instantaneous gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>) at the end of each model output time step. The number and types of species contained in the CONC files depend on the chemical mechanism and aerosol model configurations that are selected when the CCTM is compiled. The [FORTRAN NameLists](#matrix.nml) within the mechanism directories list the modeled species, and contain a column that specifies which species are written to the CONC files. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the CONC file by editing the CONC column in the NameList file(s) to reduce the number of species that are written to, and thus the size of the CONC file. User can also speify the output species list by modifying the environment variable CONC_SPCS.


<a id=aconc></a>
### A_CONC_1: CCTM hourly average concentration file
[Return to Table 7-1](#aconc_t)

The 3-D CCTM integral average concentration file (ACONC) contains average model species concentrations for each model hour, as opposed to instantaneous concentrations at the end of each output time step. The species written to the ACONC file are set by the user in the CCTM run script using the environment variable AVG_CONC_SPCS. The model layers that are used to calculate the integral average concentration are also set in the CCTM run script using the environment variable ACONC_BLEV_ELEV, where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number. An example setting for the ACONC_BLEV_ELEV variable is “1 6”, which defines layers 1 through 6 as the vertical extent over which to calculate hourly average concentrations.

<a id=drydep></a>
### CTM_DRY_DEP_1: CCTM hourly cumulative dry deposition file
[Return to Table 7-1](#drydep_t)

The 2-D CCTM dry deposition file (DRYDEP) contains cumulative hourly dry deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates dry deposition for all of the species listed in the dry deposition column of the [FORTRAN NameLists](#matrix_nml) within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the dry deposition file by editing the DDEP column in the NameList file(s).

<a id=wetdep></a>
### CTM_WETDEP_1: CCTM hourly cumulative wet deposition file
[Return to Table 7-1](#wetdep_t)

The 2-D CCTM wet deposition file (WETDEP1) contains cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the [FORTRAN NameLists](#matrix_nml) within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s).

## 7.2 Restart Files

There are several files created by the CCTM that are used to enable a restart of the run for any specific day.  The files contain values for parameters at the end of the day which are used to initialize the values for the start of calculations for the next day.

<a id=cgrid></a>
### S_CGRID: CCTM restart file
[Return to Table 7-1](#cgrid_t)

The 3-D CCTM ending concentration file (CGRID) contains gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>) at the end of each simulation period. The number and types of species contained in the output CGRID files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. This file can be used to initialize CCTM from a simulation period that the model completed. For example, if the CCTM is configure to produce daily output files, a CGRID file will be written out at the end of each simulation day. These concentrations then become the initial conditions for the next simulation period.

<a id=media></a>
### MEDIA_CONC: Bidirectional modeling media concentration file
[Return to Table 7-1](#media_t)

This 2-D CCTM file contains the soil NH<sub>4</sub><sup>+</sup> and pH concentrations and/or the soil, vegetation and water Hg concentrations. This file is only created when the CTM_ABFLUX environment variable or the CTM_HGBIDI variable is set to Y (Default is N) and is used to initialize the next day of the model simulation.

<a id=soilout></a>
### SOILOUT
[Return to Table 7-1](#soilout_t)

This optional 2-D CCTM file contains name and location of hourly soil NO emissions calculated in-line by the CCTM. This file is only created if the CTM_BIOGEMIS environment variable is set to Y (Default is N). 


## 7.3 Diagnostic and Advanced CMAQ Output Files

Along with the standard output files detailed in the previous section, CCTM can be configured to output several auxiliary files for diagnostic model purposes. Each option is controlled by its corresponding environment variable. For logical values, TRUE/T is equivalent to Y and FALSE/F is equivalent to N.

<a id=floor></a>
### FLOOR: concentration-reset diagnostics file
[Return to Table 7-1](#floor_t)

This optional ASCII file contains specific gridcells/timesteps in which species with negative concentrations are reset to zero. The location and name of the file is set by the FLOOR_FILE environment variable.

<a id=pmdiag></a>
### CTM_PMDIAG_1: CCTM instantaneous hourly aerosol diagnostics file
[Return to Table 7-1](#pmdiag_t)

This optional 2-D CCTM diagnostic file contains instantaneous information at the end of the hour for each model hour on the geometric mean diameters, geometric standard deviations, bulk densities, 2nd moments and 3rd moments for the lognormal modes. 
It also inlcudes the fraction of each mode that contributes to PM1, PM2.5, and PM10 and the AMS transmissio factor for each mode. Many diagnostics relating to hetereogenous chemistry are provided inlcuding the N<sub>2</sub>O<sub>5</sub> reaction probability, 
the ClNO<sub>2</sub> reaction yield, and the IEPOX updake coefficinet. Units for all variables are specified in the output file. This file is only created if the CTM_APMDIAG environment variable is set to Y (Default is N).

<a id=apmdiag></a>
### CTM_APMDIAG_1: CCTM average hourly aerosol diagnostics file
[Return to Table 7-1](#apmdiag_t)

This optional 2-D CCTM diagnostic file contains integral average information for each model hour on the geometric mean diameters, geometric standard deviations, bulk densities, 2nd moments and 3rd moments for the lognormal modes. 
It also inlcudes the fraction of each mode that contributes to PM1, PM2.5, and PM10 and the AMS transmissio factor for each mode. Many diagnostics relating to hetereogenous chemistry are provided inlcuding the N<sub>2</sub>O<sub>5</sub> reaction probability, 
the ClNO<sub>2</sub> reaction yield, and the IEPOX updake coefficinet. Units for all variables are specified in the output file. This file is only created if the CTM_APMDIAG environment variable is set to Y (Default is N).

<a id=b3gts></a>
### B3GTS_S: CCTM biogenic emissions diagnostic file
[Return to Table 7-1](#b3gts_t)

This optional 2-D CCTM hourly output file contains total hourly biogenic emissions in mass units calculated in-line by the CCTM. This file is only created if the B3GTS_DIAG environment variable is set to Y (Default is Y).

<a id=depv></a>
### CTM_DEPV_DIAG: CCTM inline deposition diagnostics file
[Return to Table 7-1](#depv_t)

This optional 2-D CCTM file contains the deposition velocity (m/s) for each chemical species calculated for the final time step for the hour. CCTM calculates the deposition velocity for all of the species listed in the deposition velocity column of the [FORTRAN Namelist](#matrix_nml) files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the deposition velocity file by editing the DDEP column in the NameList file(s). This file is only created if the CTM_DEPV_FILE environment variable is set to Y (Default is N). 

<a id=pt3d></a>
### CTM_PT3D_DIAG: CCTM PT3D diagnostics file
[Return to Table 7-1](#pt3d_t)

This optional 3-D CCTM file records the 3-D point source emissions for each layer as a linear average over the output timestep. This file is only created if the PT3DDIAG environment variable is set to Y (Default is N).

<a id=dust></a>
### CTM_DUST_EMIS_1
[Return to Table 7-1](#dust_t)

This optional 2-D CCTM hourly output file contains dust emissions in mass units calculated in-line by the CCTM. This file is only created if the CTM_DUSTEM_DIAG environment variable is set to Y (Default is N).

<a id=depv_mos></a>
### CTM_DEPV_MOS
[Return to Table 7-1](#depv_mos_t)

This optional 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) for the final time step of the hour for each land use type within a grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers".  This file is only created if the CTM_MOSAIC environment variable is set to Y (Default is N).

<a id=dry_dep_mos></a>
### CTM_DRY_DEP_MOS
[Return to Table 7-1](#dry_dep_mos_t)

This optional 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) for the hour for each land use type within each grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers". This file is only created if the CTM_MOSAIC environment variable is set to Y (Default is N).

<a id=depv_fst></a>
### CTM_DEPV_FST
[Return to Table 7-1](#depv_fst_t)

This optional 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) through the stomatal pathway for the final time step of the hour for each land use type within a grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers". This file is only created if the CTM_FST environment variable is set to Y (Default is N).

<a id=dry_dep_fst></a>
### CTM_DRY_DEP_FST
[Return to Table 7-1](#dry_dep_fst_t)

This optional 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) through the stomatal pathway for the hour for each land use type within each grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers". This file is only created if the CTM_FST environment variable is set to Y (Default is N).

<a id=vdiff_diag></a>
### CTM_VDIFF_DIAG
[Return to Table 7-1](#vdiff_diag_t)

This optional 3-D CCTM file contains diagnostic output of the vertical dispersion parameters. This file is only created if the VDIFF_DIAG_FILE environment variable is set to Y (Default is N).

<a id=vsed_diag></a>
### CTM_VSED_DIAG
[Return to Table 7-1](#vsed_diag_t)

This optional 3-D CCTM file contains diagnostic output of particle gravitational settling velocities (m s<sup>-1</sup>). This file is created in addition to the vertical dispersion paramet er file if both the VDIFF_DIAG_FILE environment variable and the gravitational sedimentation environment variable (CTM_GRAV_SETL) are set to Y (Default is N).

<a id=ltngdiag1></a>
### LTNG_DIAG1
[Return to Table 7-1](#ltngdiag1_t)

This optional 3-D CCTM file contains hourly lightning NO emissions (mol/s) calculated in-line by the CCTM. This file is only created if the CTM_LTNGDIAG_1 environment variable is set to Y (Dfault is N).

<a id=ltngdiag2></a>
### LTNG_DIAG2
[Return to Table 7-1](#ltngdiag2_t)

This optional 2-D CCTM file contains hourly column-total lightning NO emissions (mol/s) calculated in-line by the CCTM. This file is only created if the CTM_LTNGDIAG_2 environment variable is set to Y (Default is N).

<a id=ctm_rj1></a>
### CTM_RJ_1: In-line photolysis inputs and outputs - summary file
[Return to Table 7-1](#ctm_rj1_t)

This optional 2-D CCTM file contains general summary information for the photolysis calculation including the surface albedo, 
select photolysis rates and flux values.  This file is only created if the CTM_PHOTDIAG environment variable is set to Y (Default is N).

<a id=ctm_rj2></a>
### CTM_RJ_2: In-line photolysis output – gridded photolysis rates
[Return to Table 7-1](#ctm_rj2_t)

This optional 3-D CCTM file contains the photolysis rates calculated in-line by the CCTM.  The number of layers is set by the  NLAYS_PHOTDIAG environment variable (Default is all layers). This file is only created if the CTM_PHOTDIAG environment variable is set to T (Default is N).

<a id=ctm_rj3></a>
### CTM_RJ_3: In-line photolysis inputs and outputs – detailed
[Return to Table 7-1](#ctm_rj3_t)

This optional 3-D CCTM file contains detailed inputs and results from the photolysis rate calculation done in-line by the CCTM. The number of layers is set by the  NLAYS_PHOTDIAG environment variable (Default is all layers). The number of wavelengths included in the file is set by the NWAVE_PHOTDIAG environment variable (Default is all wavelengths). This file is only created if the CTM_PHOTDIAG environment variable is set to T (Default is N).

<a id=ssemis></a>
### CTM_SSEMIS_1: Sea salt emissions diagnostic file
[Return to Table 7-1](#ssemis_t)

This optional 2-D CCTM hourly output file contains calculated sea salt emissions. This file is only created if the CTM_SSEMDIAG environment variable is set to Y (Default is N).

<a id=wetdep2></a>
### CTM_WETDEP_2: CCTM cloud diagnostics file
[Return to Table 7-1](#wetdep2_t)

In CMAQ, wet deposition is calculated separately for resolved (grid-scale) clouds and for convective (subgrid) clouds. The WETDEP1 file contains the total wet deposition, i.e., the sum of both resolved-scale and subgrid-scale deposition. The WETDEP2 file contains only subgrid-scale deposition, plus some cloud diagnostic variables. The 2-D CCTM wet deposition file (WETDEP2) includes cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the [FORTRAN Namelist](#matrix_nml) files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s). Generation of this file is controlled by the environment variable, CLD_DIAG. (Default is N)

<a id=vext></a>
### CTM_VEXT_1
[Return to Table 7-1](#vext_t)

This optional 3-D CCTM file contains vertical profiles of the concentration of multiple chemical species for latitude / longitude coordinates specified in the VERTEXT_COORD_PATH file. The species written to this output file are identical to those written to the 3D CONC file which in turn are controlled either by the setting of CONC_SPCS in the run script or the last column in the GC, AE, NR, and TR namelist files. This file is only created if the VERTEXT environment variable is set to Y (Default is N).

[<< Previous Chapter](CMAQ_UG_ch06_model_configuration_options.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch08_analysis_tools.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
