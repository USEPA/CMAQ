
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch04_model_formulation.md)

<!-- END COMMENT -->

# Input and Output Files

This chapter provides basic information on the format and content of CMAQ files.  A list of CMAQ input files can be found in [Table 3-3](#Input_Table) and a list of output files is given in [Table 3-8](#Output_Table). Some CMAQ files are in ASCII format while the majority of them are binary files and are in the [Network Common Data Form (netCDF)](http://www.unidata.ucar.edu/software/netcdf). The netCDF is a set of software libraries and machine-independent data formats that support the creation, access, and sharing of array-oriented scientific data (Unidata, 2009). The netCDF library provides an implementation of the netCDF interface for several different programming languages. CMAQ input and output files are self-describing netCDF-format files in which the file headers have all the dimensioning and descriptive information needed to define the resident data. Users should download the latest code for the NetCDF from the [NetCDF website](http://www.unidata.ucar.edu/software/netcdf). Compilation and configuration information for the NetCDF is available through the Unidata website.

**>>Comment<<** Throughout… need to de-emphasize I/O API language and dependencies. I think we can reference Carlie's page rather than trying to repeat it all here. I/O API may not be required and may become optional VERY soon.

Input/Output Applications Programming Interface (I/O API)
---------------------------------------------------------
**>>COMMENT<<** only a subset of this information needs to be kept; much of the subsection material can be found online and needs to be de-emphasized here.Consider removing the table describing Time Step Structures and Data Type Structures altogether.  This information can be briefly described in text to describe the GRIDDED3 and BNDARY3 files.

The Models-3 Input/Output Applications Programming Interface (I/O API) is an environmental software development library that provides an input/output framework for the CMAQ programs, inlcuding a set of commonly used subroutines for passing information between source code modules and for reading and writing data files (Coats, 2005). An additional benefit of the I/O API is that an expansive set of data manipulation utilities and statistical analysis programs is available to evaluate and postprocess the binary CMAQ input/output data files. Users should download the latest code for the I/O API from the [website](https://www.cmascenter.org/ioapi). In order to work with I/O API buffered file format, which is used in the WRF-CMAQ two-way coupled model, and/or to perform parallel I/O operations in CMAQ 5.0.1 or later version, users should use IOAPI 3.2. For CMAQ users using preconfigured applications of the model, the I/O API system can be essentially transparent. For users who plan to modify the code or implement updated modules for research purposes, a few key elements of the I/O API should be understood, and they are discussed below. For more detailed information about developing new modules for CMAQ using the I/O API code libraries, please refer to the [I/O API User's Manual](https://www.cmascenter.org/ioapi/documentation/all_versions/html).

** >> Comment <<** In the paragraph below, the “CMAQ_OGD_Chapter6_review_DW.docx” marked-up file had an edit that described a “Red Hat 51 Linux Workstation.” However, the number 51 did not appear in the original Atom file, nor did “51” appear to be an addition within the marked-up file. So I am just making sure "51" actually belongs in the document and isn’t an unintentional remnant of something else

### I/O API Data Structure and Data File Types
Each CMAQ data file has internal file descriptions that contain the file type, the file start date and time, the file time step, the grid and coordinate descriptions, and a set of descriptions for the set of variables contained within the file (i.e., names, units specifications, and text descriptions). Some of the elements in a file description, such as the dates and times for file creation and update and the name of the program that created the file, are maintained automatically by the I/O API. The remainder of the descriptive information must be provided at the time of file creation.

**>>COMMENT<<** DS: We should probable talk about dot and cross points below

While the CMAQ input and output files use a Network Common Data Form (netCDF) file format, the files contain specific spatial information that is expected by the I/O API. The I/O API convention of defining horizontal grids is to use a combination of the map projection and an offset from the projection center to the southwest corner of the modeling domain. After defining the southwest corner of the domain, or the “offset” from the projection center, the I/O API grid definition specifies the size of the horizontal grid cells and the number of cells in the X and Y directions. According to the I/O API format, files and variables are referred to by names, layers are referred to by numbers (from 1 to the greatest number of layers in the file), and dates and times are stored as integers, using the coding formats *YYYYDDD* (commonly called “JDATE”) and *HHMMSS* (commonly called “JTIME”), where

YYYYDAY = (1000 * Year) + Julian Day
HHMMSS = (10000 * Hour) + (100 * Minute) + Seconds.

All files manipulated by the I/O API may have multiple variables and multiple layers. Each file also has a time-step structure that is shared by all of its variables. There are three kinds of time-step structures supported ([Table 3‑1](#Time_Steps)). The data type structures that are supported are listed in [Table 3-2](#Data_Structure). Within a file, all the variables are data arrays with the same dimensions, number of layers, and data structure type, although possibly different basic types (e.g., gridded and boundary variables cannot be mixed within the same file, but real and integer variables can). 

<a id=Time_Steps></a>
**Table 3‑1. Possible Time Step Structures in I/O API Files**

|**File Type**|**Description**|
|-------------------|-----------------------------------------------------------------------|
|Time-independent|The file’s time-step attribute is set to zero. Routines that use time-independent files ignore the date and time arguments.|
|Time-stepped|The file has a starting date, a starting time, and a positive time step. Read and write requests must be for some positive integer multiple of the time step from the starting date and time.|
|Circular-buffer|This type of file keeps only two “records”, the “even” part and the “odd” part (useful, for example, for “restart” files where only the last data written in the file are used). The file’s description has a starting date, a starting time, and a negative time step (set to the negative of the actual time step). Read and write requests must be for some positive integer multiple of the time step from the starting date and time, and they must reflect a specific time step that is in the file.|

<a id=Data_Structure></a>
**Table 3‑2. Example Data Type Structures in I/O API Files**

|**File Type**|**Data Type**|**Description**|
|-------------|----------------|---------------------------|
|GRDDED3|Gridded|Dimension as REAL4 ARRAY (NCOLS, NROWS, NLAYS, NVARS)|
|GRDDED3|Gridded|Dimension as REAL4 ARRAY (NCOLS, NROWS, NVARS)|
|BNDARY3|Boundary|Dimension as REAL4 ARRAY (SIZE, NLAYS, NVARS)|



# CMAQ Input and Output Files #

[Jump to Input Files](#inputs)<br>
[Jump to CCTM Output Files](#outputs)

**>>COMMENT<<** Update or remove reference to chapters from old User's document

CMAQ requires a basic set of input files: initial condition file, which is created by ICON process or previous day output; boundary condition file, which is created by BCON process; emission files; and meteorological data created by MCIP using WRF and terrain data. There are a few other input files are required bases on user chosen run time option. CMAQ output files include a basic set of files with aerosol and gas-phase species concentrations, wet and dry deposition estimates, and visibility metrics, and an auxiliary set of output files for diagnosing model performance and in-line-calculated emissions.

Rather than forcing the programmer and program-user to deal with hard-coded file names or hard-coded unit numbers, the I/O API utilizes the concept of logical file names. The modelers can define the logical names as properties of a program, and then at run-time the logical names can be linked to the actual file name using environment variables. For programming purposes, the only limitations are that file names cannot contain blank spaces and must be at most 16 characters long. When a modeler runs a program that uses the I/O API, environment variables must be used to set the values for the program’s logical file names. A complete list of CMAQ input and output files by logical name is provided in Tables 3-3 and 3-8.

<a id=inputs></a>
## CMAQ Input Files

This section describes each of the input files required by the various CMAQ programs. The section begins with a description of the grid definition file, GRIDDESC, which is used by several CMAQ programs, and then goes through a program-by-program listing of the CMAQ input file requirements. [Table 3-3](#$Input_Table) lists the source, file type, and temporal and spatial dimensions of each CMAQ input file. Sample disk space requirements for a desired input data set can easily be calculated from the information in [Table 3-3](#Input_Table); each data record is four bytes. The I/O API file sizes can be calculated using the number of variables in a CMAQ file and the spatial and temporal coverage of the data. The user should consult the CMAQ release notes for additional file information. The programs used to generate the files ("Source") are described in Section 2.

<!-- BEGIN COMMENT -->
**>> Comment <<** In Table 8.1, some of the potential sources for the files have not been explained elsewhere (e.g., CSV2NML, Spatial Allocator, Cropcal).

**>> Comment <<** In Table 8.1, those Hourly denotation, we should put an asterisk to indicate that it can be user defined, e.g. 30 minutes rather 1 hour and in the same table and table 8-13, there are places with +1 and I think we can make it more generic as +NTHIK (your call).

**>> Comment <<** DW: Also I think we should remove everything related to/associated with JPROC and JTABLE since CMAQ does not use it anymore and they are highlighted in the PDF file.

**>> Comment <<** DS: Should the column header by "Environment Variable Namefor File" or "Logical File Name"?

<a id=Input_Table></a>
**Table 3-3. CMAQ input files**

|**Environment Variable Name for File**|**File Type**|**Time-Dependence**|**Spatial Dimensions**|**Source**|**Required**|
|-------------------------|----------------|----------------|----------------|-----------------------------------|---------|
|**General**| | | | ||
|[GRIDDESC](#griddesc) <a id=griddesc></a>|ASCII|n/a|n/a|MCIP|required|
|[gc_matrix_nml](#matrix_nml) <a id=matrix_nml_t></a>|ASCII|n/a|n/a|CMAQ repo|required|
|[ae_matrix_nml](#matrix_nml) <a id=matrix_nml_t></a>|ASCII|n/a|n/a|CMAQ repo|required|
|[nr_matrix_nml](#matrix_nml) <a id=matrix_nml_t></a>|ASCII|n/a|n/a|CMAQ repo|required|
|[tr_matrix_nml](#matrix_nml) <a id=matrix_nml_t></a>|ASCII|n/a|n/a|CMAQ repo|required|
|**Initial Condition Inputs**| | | | ||
|[INIT_CONC_1](#init_conc_1) <a id=init_conc_1_t></a> | GRDDED3 | Time-invariant | XYZ | ICON or CCTM|required|
|[INIT_GASC_1](#init_conc_1) <a id=init_conc_1_t></a>|GRDDED3|Time-invariant | XYZ | ICON or CCTM |required|
|[INIT_AERO_1](#init_conc_1) <a id=init_conc_1_t></a>|GRDDED3|Time-invariant | XYZ | ICON or CCTM|required|
|[INIT_NONR_1](#init_conc_1) <a id=init_conc_1_t></a>|GRDDED3|Time-invariant | XYZ | ICON or CCTM|required|
|[INIT_TRAC_1](#init_conc_1) <a id=init_conc_1_t></a>|GRDDED3|Time-invariant | XYZ | ICON or CCTM|required|
|**Boundary Condition Inputs**| | | | ||
|[BNDY_CONC_1](#bndy_conc_1) <a id=bndy_conc_1_t></a> | BNDARY3 | Hourly |[2(X+1)+2(Y+1)]\*Z | BCON|required|
|[BNDY_GASC_1](#bndy_conc_1) <a id=bndy_conc_1_t></a> |BNDARY3| Hourly |[2(X+1)+2(Y+1)]\*Z|BCON|required|
|[BNDY_AERO_1](#bndy_conc_1) <a id=bndy_conc_1_t></a> |BNDARY3| Hourly |[2(X+1)+2(Y+1)]\*Z|BCON|required|
|[BNDY_NONR_1](#bndy_conc_1) <a id=bndy_conc_1_t></a> |BNDARY3| Hourly |[2(X+1)+2(Y+1)]\*Z|BCON|required|
|[BNDY_TRAC_1](#bndy_conc_1) <a id=bndy_conc_1_t></a> |BNDARY3| Hourly |[2(X+1)+2(Y+1)]\*Z|BCON|required|
|**MCIP**| | | | ||
|[GRID_CRO_2D](#grid_cro_2d) <a id=grid_cro_2d_t></a>| GRDDED3 | Time-invariant | XY | MCIP|required|
|[GRID_CRO_3D](#grid_cro_3d) <a id=grid_cro_3d_t></a>**>>missing text** | GRDDED3 | Time-invariant | XYZ | MCIP|required|
|[GRID_BDY_2D](#grid_bdy_2D) <a id=grid_bdy_2D_t></a> **>>missing text** | GRDDED3 | Time-invariant | PERIM\*Z | MCIP|required|
|[GRID_DOT_2D](#grid_dot_2d) <a id=grid_dot_2d_t></a>| GRDDED3 | Time-invariant | (X+1)\*(Y+1) | MCIP|required|
|[MET_BDY_3D](#met_bdy_3d) <a id=met_bdy_3d_t></a>| BNDARY3 | Hourly | PERIM\*Z | MCIP|required|
|[MET_CRO_2D](#met_cro_2d) <a id=met_cro_2d_t></a>| GRDDED3 | Hourly | XY | MCIP|required|
|[MET_CRO_3D](#met_cro_3d) <a id=met_cro_3d_t></a>| GRDDED3 | Hourly | XYZ | MCIP|required|
|[MET_DOT_3D](#met_dot_3d) <a id=met_dot_3d_t></a>| GRDDED3 | Hourly | (X+1)\*(Y+1)Z | MCIP|required|
|**Emissions Inputs**||||||
|[EMIS_1](#emis_1) <a id=emis_1_t></a> | GRDDED3 | Hourly | XYZ | SMOKE|required|
|[STK_GRPS_nn](#stk_grps) <a id=stk_grps_t></a> | GRDDED3 |Time-invariant|XY | SMOKE|required|
|[STK_EMIS_nn](#stk_emis) <a id=stk_emis_t></a> | GRDDED3 | Hourly | XY | SMOKE|required|
|[NLDN_STRIKES](#nldn_strikes) <a id=nldn_strikes_t></a>| GRDDED3 | Hourly | XY |Must purchase data|optional for including NO from lightning|
|[LTNGPARMS_FILE](#ltngparm_file) <a id=ltngparm_file_t></a>| GRDDED3 | Time-invariant | XY |CMAS|required for including NO from lightning|
|**Biogenic and Land Surface Inputs**||||||
|[OCEAN_1](#ocean_1) <a id=ocean_1_t></a>| GRDDED3 | Time-invariant | XY |Spatial Allocator|required|
|[GSPRO](#gspro) <a id=gspro_t></a>| ASCII | Time-invariant | N/a | CMAQ repo|required|
|[B3GRD](#b3grd) <a id=b3grd_t></a>| GRDDED3 | Time-invariant | XY | SMOKE|required for running CMAQ with inline biogenics|
|[BIOSEASON](#bioseason) <a id=bioseason_t></a>|GRDDED3 |Time-invariant | XY | Metscan|optional (run-time option)|
|[BELD4_LU](#beld4_lu) <a id=beld4_lu_t></a>| GRDDED3 | Time-invariant |XY||required for running CMAQ with bidirectional NH3|
|[E2C_SOIL](#e2c_soil) <a id=e2c_soil_t></a>| GRDDED3 | Time-invariant | XY||required for running CMAQ with bidirectional NH3|
|[E2C_FERT](#e2c_fert) <a id=e2c_fert_t></a>| GRDDED3 | Daily |XY||optional|
|**Photolysis** | | | | ||
|[JTABLE](#jtable) <a id=jtable_t></a>| ASCII | Daily | n/a | JPROC|optional|
|[OMI](#omi) <a id=omi_t></a>| ASCII | daily | n/a |||optional|


## General
<a id=griddesc></a>
### GRIDDESC: Horizontal domain definition
[Return to Table 3-3](##griddesc_t)

Used by: ICON, BCON, CCTM

The CMAQ grid description file (**GRIDDESC**) is an ASCII file that contains two sections: a horizontal coordinate section, and grid description section.  The GRIDDESC file is generated automatically with MCIP; alternatively, GRIDDESC can be created using a text editor.

The horizontal coordinate section consists of text records that provide the coordinate-system name, the map projection, and descriptive parameters that are relevant to the projection type (e.g. longitude for coordinate system center)

The grid description section consists of text records that indicate the grid name, related coordinate-system name (i.e., which GRIDDESC horizontal coordinate name that is defined in the previous section that is applied to this grid), and descriptive parameters for teh coordinates of the lower-left corner of the grid, grid cell size, number of coluns, and rows. For a typical CMAQ application, both "dot-point" and "cross-point" grids are defined in the GRIDDESC file; these grids are topological duals in the sense that the vertices (corners) of one correspond to the cell-centers of the other. There are at most 32 coordinate systems and 256 grids listed in one of these files. These files are small enough to be archived easily with a study, and have a sufficiently simple format that new ones can easily be constructed "by hand."

An example of a GRIDDESC file is shown below:

` ' '`
` 'LAM_40N100W'`
` 2 30.0 60.0 -100.0 -100.0 40.0`
` ' '`
` 'M_32_99TUT02'`
` 'LAM_40N100W' 544000.0 -992000.0 32000.0 32000.0 38 38 1`
` ' '`

The horizontal coordinate section (first section) in this example GRIDDESC file defines a horizontal coordinate named “LAM_40N100W”. The coordinate definition is for a Lambert conformal grid, keyed by the first column of the coordinate description line, which corresponds to the numeric code for the various I/O API-supported grid types (2 = Lambert). The next three parameters (P_ALP, P_BET, and P_GAM) have different definitions for different map projections. For Lambert conformal, P_ALP and P_BET are the true latitudes of the projection cone (30°N and 60°N in the example), and P_GAM (100°W in the example) is the central meridian of the projection. The last two parameters, XCENT and YCENT, are the latitude-longitude coordinates of the grid center of the Cartesian coordinate system, which are 100°W and 40°N in the example. If using WRF-ARW as the meteorological model, the user should be aware of differences from this method.

The example grid definition section above describes a grid named “M_32_99TUT02”. The definition of the grid begins with a reference to a coordinate name from the coordinate definition section of the file; in this example, the coordinate named “LAM_40N100W” is referenced in the grid definition. The next two parameters in the grid definition (XORIG and YORIG) are the east-west and north-south offsets from XCENT and YCENT in meters (WRF-ARW usages may differ). The next two parameters (XCELL and YCELL) are the horizontal grid spacing in meters for the X and Y directions (i.e., delta-x and delta-y). The next two parameters (NCOLS and NROWS) are the numbers of grid cells in the X and Y directions. The grid definition concludes with the number of boundary cells, NTHIK, which is typically set to 1.

<a id=matrix_nml></a>

### {gc|ae|nr|tr}_matrix.nml: Species namelist files
[Return to Table 3-3](#matrix_nml_t)

Used by: BCON, CCTM, ICON, CHEMMECH

Namelist look-up tables for different classes of simulated pollutants are used to define the parameters of different model species during the execution of the CMAQ programs. Gas-phase (gc), aerosol (ae), non-reactive (nr), and tracer (tr) species namelist files contain parameters for the model species that are included in these different classifications. The species namelist files are used to control how the different CMAQ programs and processes handle the model species. The namelist files define the following processes for each model species:


-   Deposition velocity – which (if any) deposition velocity is the deposition velocity for the pollutant mapped to; allowed velocities are specified within the model source code.
-   Deposition velocity factor – if the pollutant is mapped to a deposition velocity, uniformly apply a scaling factor to this velocity.
-   Initial/boundary conditions – which initial and boundary condition species is the pollutant mapped to; if not specified, this will default to the species name.
-   IC/BC Factor – if the pollutant is mapped to an initial/boundary condition species, uniformly apply a scaling factor to the concentrations.
-   Scavenging - which (if any) species is the pollutant mapped to; Allowed scavenging surrogates are specified within the model source code.
-   Scavenging factor - if the pollutant is mapped to an species for scavenging, uniformly apply a scaling factor to the scavenging rate.
-   Gas-to-aerosol conversion – which (if any) aerosol chemistry species does the gas phase pollutant concentration go into for transformation from the gas-phase to the aerosol-phase.
-   Gas-to-aqueous Surrogate – which (if any) cloud chemistry species does the gas pollutant concentration go into for simulating chemistry within cloud water.
-   Aerosol-to-aqueous Surrogate – which (if any) cloud chemistry species does the aerosol pollutant concentration go into for simulating chemistry within cloud water.
-   Transport – is the pollutant transported by advection and diffusion in the model?
-   Dry deposition – Write the pollutant to the dry deposition output file?
-   Wet deposition – Write the pollutant to the wet deposition output file?
-   Concentration – Write the pollutant to the instantaneous concentration output file?

The namelist files contain header information that describe which class of species are contained in the file, the number of parameters contained in the file, headers describing the parameter fields, and then a series of rows with configuration parameters for every model species. [Table 3-4](#Table8-4) contains the namelist file format for the gas-phase (GC) species namelist file. The namelist files for the other species classifications (AE, NR, TR) are similar to the format shown in [Table 3-4](#Table8-4).

<a id=Table8-4></a>

 **Table 3-4. GC species namelist file format**

| **Line**| **Column** |**Name** | **Type**| **Description** |**Options for Syntax**:|
|-----|-----|----------------------|----------|--------------------------------------------|----------------------------|
| 1 || File Type |String|String to delineate Gas Phase (GC), Aerosol (AE), Non-reactive (NR) and Tracer (TR) species namelist|{&GC_nml, &AE_nml, &NR_nml, &TR_nml}|
| 3 || Header ID | String |String to define data structure relating to namelist|{GC_SPECIES_DATA=, AE_SPECIES DATA= , NR_SPECIES_DATA= ,TR_SPECIES_DATA = }|
| 5 |1| SPECIES | String |CMAQ Species name, i.e. NO, HNO3, PAR; dependent on chemical mechanism|-|
||2| MOLWT| Integer |Species Molecular Weight|-|
|  |3| ICBC | String |IC/BC surrogate species name for the CMAQ Species|{'Species name', ' '}|
|  |4| FAC | Integer |Scaling factor for the IC/BC concentration|-|
| |5| DRYDEP SURR | String |Deposition velocity variable name for the CMAQ Species|-|
| |6| FAC | Integer |Scaling factor for the deposition velocity|-|
| |7| WET-SCAV SURR | String |Wet Deposition Scavenging surrogate species|-|
| | 8 | FAC | Integer |Scaling factor for Scavenging|-|
|| 9 | GC2AE SURR | String |Gas-to-aerosol transformation species|-|
|| 10 | GC2AQ SURR | String |Gas-to-aqueous transformation species|-|
|| 11 | TRNS | String |Transport Switch. *NOTE: Instead of using one column labeled "TRNS" to turn/off both advection and diffusion for a pollutant, two separate columns labeled "ADV" and "DIFF" can be used to switch on/off advection and diffusion separately.|{YES/NO}|
|| 13 | DDEP | String |Dry deposition output file switch|{YES/NO}|
|| 14 | WDEP | Real |Wet deposition output file switch|{YES/NO}|
|| 15 | CONC | String |Concentration output file switch|{YES/NO}|


The namelist files for the other pollutant classes have similar configurations as the gas-phase species configuration shown in [Table 3-4](#Table8-4). For an example see this [link](../../CCTM/src/MECHS/cb06r3_ae7_aq/GC_cb6r3_ae7_aq.nml) to the GC namelist species file for the cb06r3_ae7_aq mechanism.

## Initial Condition Inputs
<a id=init_conc_1></a>

### INIT_CONC_1: Initial conditions
[Return to Table 3-3](#init_conc_1_t)

Used by: CCTM

The initial concentrations of each species being modeled must be input to CMAQ. The initial conditions input file type is GRDDED3 and does not vary with time. The file data are looped in this manner: by column, by row, by layer, by variable. Initial conditions files have the same structure as concentration files, so the predicted concentrations from the last hour of day 1 can be used to initialize the following day’s simulation. This gives CMAQ users the flexibility to segment simulations in any way they choose.

## Boundary Condition Inputs
<a id=bndy_conc_1></a>

### BNDY_CONC_1: Boundary conditions
[Return to Table 3-3](#bndy_conc_1_t)

Used by: CCTM

CMAQ boundary condition data are of the BNDARY3 file type. Produced by the boundary condition processor, BCON, CCTM reads these data and correlates them with the interior data by the use of a pointer system. This pointer system designates the beginning location of the data in memory that start a new side of the domain (i.e., south, east, north, or west).

Each species being modeled should be in the BNDY_CONC_1 file. If some modeled species are not contained in this file, the boundary condition for these species will default to the value 1 × 10e<sup>-30</sup>. The perimeter of the CMAQ domain is NTHIK cell wide (typically NTHIK = 1), where the number of boundary cells = (2*NROW*NTHIK)+(2*NCOL*NTHIK)+(4*NTHIK*NTHIK).

## MCIP
<a id=grid_cro_2d></a>

### GRID_CRO_2D: Two-dimensional grid cross-point fields
[Return to Table 3-3](#grid_cro_2d_t)

Used by: CCTM

The GRID_CRO_2D time-independent file contains surface fields at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM. The following variables are in this file:

** >> Comment <<** [Comment covers the next three pages in the document, pp 150-153 in the full OGD]: The MCIP variable lists are VERY outdated.

-   LAT: latitude (degrees, where Northern Hemisphere is positive)
-   LON: longitude (degrees, where Western Hemisphere is negative)
-   MSFX2: squared map-scale factor (m<sup>2</sup> m<sup>-2</sup>)
-   HT: terrain elevation (m)
-   DLUSE: dominant land use (category)
-   LWMASK: land-water mask (1=land, 0=water)
-   PURB: urban percentage if cell is based on land (percent)
-   LUFRAC_01: land use fraction of NLCD40: Evergreen Needleleaf Forest
-   LUFRAC_XX: <repeated for 40 land use fractions>

<a id=grid_cro_3d></a>

### GRID_CRO_3D
[Return to Table 3-3](#grid_cro_3d_t)

<a id=grid_bdy_2d></a>

### GRID_BDY_2D
[Return to Table 3-3](#grid_bdy_2d_t)

<a id=grid_dot_2d></a>

### GRID_DOT_2D: Two-dimensional grid dot-point fields
[Return to Table 3-3](#grid_dot_2d_t)

Used by: CCTM

The GRID_DOT_2D time-independent file contains surface fields at dot points (i.e., at cell corners). It is created by MCIP and used by CCTM. The following variables are in the GRID_DOT_2D file:

-   LAT: latitude (degrees, where Northern Hemisphere is positive)
-   LON: longitude (degrees, where Western Hemisphere is negative)
-   MSFD2: squared map scale factor (m<sup>2</sup> m<sup>-2</sup>)

<a id=met_bdy_3d></a>

### MET_BDY_3D: Three-dimensional meteorological boundary input
[Return to Table 3-3](#met_bdy_3d_t)

Used by: CCTM

The MET_BDY_3D time-dependent file contains 3-D meteorological descriptions at the lateral boundaries (on cross points). It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET_BDY_3D file:

-   JACOBF: total Jacobian at layer face (m)
-   JACOBM: total Jacobian at layer middle (m)
-   DENSA_J: Jacobian-weighted total air density [? J m<sup>-2</sup>] (kg m<sup>-2</sup>)
-   WHAT_JD: Jacobian- and density-weighted vertical contravariant velocity (kg m<sup>-1</sup> s<sup>-1</sup>)
-   TA: air temperature (K)
-   QV: water vapor mixing ratio (kg kg<sup>-1</sup>)
-   PRES: air pressure (Pa)
-   DENS: air density (kg m<sup>-3</sup>)
-   WWIND: vertical velocity (m s<sup>-1</sup>)
-   ZH: midlayer height above ground (m)
-   ZF: full layer height above ground (m)
-   QC: cloud water mixing ratio (kg kg<sup>-1</sup>)
-   QR: rain water mixing ratio (kg kg<sup>-1</sup>)
-   QI: ice mixing ratio (kg kg<sup>-1</sup>)
-   QS: snow mixing ratio (kg kg<sup>-1</sup>)
-   QG: graupel mixing ratio (kg kg<sup>-1</sup>)

<a id=met_cro_2d></a>

### MET_CRO_2D: Two-dimensional meteorological cross-point fields
[Return to Table 3-3](#met_cro_2d_t)

Used by: CCTM

The MET_CRO_2D time-dependent file contains surface and other 2-D meteorological fields at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET_CRO_2D file:

-   PRSFC: surface pressure (Pa)
-   JACOBS: total Jacobian at surface (m)
-   USTAR: cell-averaged horizontal friction velocity (m s<sup>-1</sup>)
-   WSTAR: convective velocity scale (m s<sup>-1</sup>)
-   PBL: planetary boundary layer height (m)
-   ZRUF: surface roughness length (m)
-   MOLI: inverse Monin-Obukhov length (m<sup>-1</sup>)
-   QFX: latent heat flux (W m<sup>-2</sup>)
-   HFX: sensible heat flux (W m<sup>-2</sup>)
-   RADYNI: inverse aerodynamic resistance (m s<sup>-1</sup>)
-   RBNDYI: inverse laminar boundary layer resistance (m s<sup>-1</sup>)
-   RSTOMI: inverse bulk stomatal resistance (m s<sup>-1</sup>)
-   TEMPG: skin temperature at ground (K)
-   TEMP10: 10-m temperature (K)
-   TEMP1P5: 1.5-m temperature (K)
-   WSPD10: 10-m wind speed (m s<sup>-1</sup>)
-   WDIR10: 10-m wind direction (m s<sup>-1</sup>)
-   GLW: longwave radiation at ground (W m<sup>-2</sup>)
-   GSW: solar radiation absorbed at ground (W m<sup>-2</sup>)
-   RGRND: solar radiation reaching the surface (W m<sup>-2</sup>)
-   RN: incremental (per output time step) nonconvective precipitation (cm)
-   RC: incremental (per output time step) convective precipitation (cm)
-   CFRAC: total cloud fraction (fraction)
-   WBAR: average liquid water content of clouds (g m<sup>-3</sup>)
-   CLDT: cloud-top layer height (m)
-   CLDB: cloud-bottom layer height (m)
-   SNOCOV: snow cover (1 = yes, 0 = no)
-   TEMP2: 2-m temperature (K)
-   SOIM1: volumetric soil moisture in top cm (m<sup>3</sup> m<sup>-3</sup>)
-   SOIM2: volumetric soil moisture in top m (m<sup>3</sup> m<sup>-3</sup>)
-   SOIT1: soil temperature in top cm (K)
-   SOIT2: soil temperature in top m (K)
-   SLTYP: soil texture type (category)
-   LAI: leaf-area index (area area<sup>-1</sup>)

<a id=met_cro_3d></a>

### MET_CRO_3D: Three-dimensional meteorological cross-point fields
[Return to Table 3-3](#met_cro_3d_t)

Used by: CCTM, ICON, BCON

The MET_CRO_3D time-dependent file contains 3-D meteorological descriptions at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM, ICON, BCON, and PDM. The variables that may exist in MET_CRO_3D are the same as those that may be in MET_BDY_3D.

<a id=met_dot_3d></a>

### MET_DOT_3D: Three-dimensional meteorological dot-point fields
[Return to Table 3-1](#met_dot_3d_t)

Used by: CCTM

The MET_DOT_3D time-dependent file contains 3-D meteorological descriptions at dot points (i.e., at cell corners) and at cell faces. It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET_DOT_3D file:

-   UWIND: u-component of horizontal wind (m s<sup>-1</sup>) [dot points; Arakawa-B grid]]
-   VWIND: v-component of horizontal wind (m s<sup>-1</sup>) [dot points; Arakawa-B grid]
-   UHAT_JD: contravariant-U*Jacobian*density (kg m<sup>-1</sup> s<sup>-1</sup>) [cell faces; Arakawa-C grid]
-   VHAT_JD: contravariant-V*Jacobian*density (kg m<sup>-1</sup> s<sup>-1</sup>) [cell faces; Arakawa-C grid]

## Emissions Inputs
<a id=emis_1></a>

### EMIS_1: Emissions
[Return to Table 3-1](#emis_1_t)

**>> Comment <<** DW: Since Ben has new emission system in place in CMAQ 53, he should modify the emission portion accordingly.

Used by: CCTM

CMAQ can accept emissions inputs from a variety of emissions models and preprocessors. The most commonly used option is the Sparse Matrix Operator Kernel Emissions (SMOKE) modeling system, which is a collection of programs that separately process and merge emissions data for each emissions sector for input to air quality models.

The emissions file sorts the emitted gas-phase and aerosol species by grid cell and time. The file type is GRDDED3, and the units are in moles per second (moles s<sup>-1</sup>) for gas-phase species and grams per second (g s<sup>-1</sup>) for aerosol species. The file data are looped as follows: by column, by row, by layer, by variable, and by input time step. CMAQ does not artificially distinguish between surface and elevated emissions sources; elevated sources are provided to CMAQ as vertically resolved emissions. For CCTM configurations that do not use in-line emissions calculations, all emissions estimates are contained within a single input emission file for each day. In v4.7, CMAQ now has the capability to process point-source, sea salt, and biogenic emissions in-line. The supplemental input files to use for calculating the in-line emissions are described in the CMAQv4.7 release notes.

<a id=stk_grps></a>

### STK_GRPS_nn: Stack groups
[Return to Table 3-1](#stk_grps_t)

Used by: CCTM – in-line emissions version only

The ## mark is unique and represents the sector identification.

The stack groups file is an I/O API netCDF file containing stack parameters for elevated sources. This file can be created using the SMOKE program ELEVPOINT. For additional information about creating the stack groups file, see the [ELEVPOINT documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch06s03.html) in the SMOKE user’s manual

<a id=stk_emis></a>

### STK_EMIS_nn: Point source emissions
[Return to Table 3-1](#stk_emis_t)

Used by: CCTM – inline emissions version only

The ## mark is unique and represents the sector identification.

The elevated-point-source emissions file is an I/O API GRDDED3 file with emissions for point sources to be treated as elevated sources by CCTM. The emissions in this file are distributed through the vertical model layers using a plume-rise algorithm contained in CCTM. The elevated-point-source emissions file can be creating using SMOKE. For additional information about preparing point-source emissions for using the CMAQ in-line plume rise calculation, see the [ELEVPOINT documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch06s03.html) in the SMOKE user’s manual.

<a id=nldn_strikes></a>

### NLDN_STRIKES: Hourly observed lightning strikes
[Return to Table 3-1](#nldn_strikes_t)

Used by: CCTM – lightning NO<sub>x</sub> version only

The NLDN lightning strikes file is used for calculating in-line NO emissions from hourly observed strike counts. This file contains the following variables interpolated to the modeling grid:

-   NLDNstrk (km-2): hourly flash counts per sq. km.

<a id=ltngparm_file></a>

### LTNGPARMS_FILE: Lightning parameters file
[Return to Table 3-1](#ltngparm_file_t)

Used by: CCTM – lightning NO<sub>x</sub> version only

The lightning parameters file is used for calculating in-line NO emissions from hourly observed strike counts. This file contains the following variables interpolated to the modeling grid:

-   SLOPE (unitless): linear equation parameter for estimating NO emissions from hourly flash counts
-   INTERCEPT: linear equation parameter for estimating NO emissions from hourly flash counts
-   SLOPE_lg: logarithmic equation parameter for estimating NO emissions from hourly flash counts
-   INTERCEPT_lg: logarithmic equation parameter for estimating NO emissions from hourly flash counts
-   ICCG_SUM (unitless): Ratio of intercloud to cloud-to-ground flashes during the summer season
-   ICCG_WIN (unitless): Ratio of intercloud to cloud-to-ground flashes during the winter season
-   OCNMASK (unitless): Land/water mask to remove spurious flashes over the ocean

## Biogenic and Land Surface Inputs
<a id=ocean_1></a>
### OCEAN_1: Sea salt mask
[Return to Table 3-1](#ocean_1_t)

Used by: CCTM

The CMAQ aerosol models AERO5 and AERO6 can compute sea salt emissions from both open ocean grid cells and surf zone grid cells. The addition of the surf zone option simulates the elevated emissions rates of sea salt in coastal regions where wave action occurs. The OCEAN_1 file contains data on the fraction of each grid cell that is either open ocean (OPEN) or in the surf zone (SURF). When CCTM is compiled with AERO5 or AERO6, it will expect the OCEAN_1 file as input.

<a id=gspro></a>
### GSPRO: Speciation profiles
[Return to Table 3-1](#gspro_t)

Used by: CCTM – inline emissions version only

The speciation profile file, GSPRO, contains the factors that are used to separate aggregated inventory pollutant emissions totals into emissions of model species in the form required by CMAQ. If only biogenic emissions are being calculated in-line in CMAQ, the GSPRO file used by CCTM needs to contain split factors only for the biogenic VOC emissions that are input in the B3GRD file. If other emissions sources are being calculated by CCTM, VOC split factors for these other sources must be included in the GSPRO file. The GSPRO file format is listed in the SMOKE user’s manual, see: [GSPRO documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch08s05s02.html).

<a id=b3grd></a>
### B3GRD: Gridded, normalized biogenic emissions
[Return to Table 3-1](#b3grd_t)

Used by: CCTM – inline-emissions version only

An I/O API GRDDED3 file of gridded, normalized biogenic emissions (in grams of carbon or nitrogen per hour, depending on the species) and leaf area index. The B3GRD file contains normalized emissions calculated with both summer and winter emissions factors. The B3GRD file is generated with the SMOKE program NORMBEIS3 using gridded land use data. For additional information about creating the B3GRD file, see the [NORMBEIS3 documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch06s12.html) in the SMOKE users’ manual.

<a id=bioseason></a>
### BIOSEASON: Freeze dates
[Return to Table 3-1](#bioseason_t)

Used by: CCTM – inline-emissions version only

The BIOSEASON switch file is an I/O API GRDDED3 file used to indicate which biogenic emissions factor to use on each day in a given year for every grid cell in the modeling domain. This file can be created using the Metscan utility program that is distributed with SMOKE. The BIOSEASON file is time-dependent and usually contains data for an entire year (365 or 366 days). It uses one variable, SEASON, which is either 0 (grid cell should use winter factors for current day) or 1 (grid cell should use summer factors for current day). For additional information about creating the BIOSEASON file, see the [Metscan documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch05s03s10.html) in the SMOKE user’s manual.

<a id=beld4_lu></a>
### BELD4_LU – Fractional crop distributions
[Return to Table 3-1](#beld4_lu_t)

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

BELD4 land use file with fractional crop distributions gridded to the modeling domain.


<a id="e2c_soil"></a>
### E2C_SOIL – EPIC soil properties
[Return to Table 3-1](#e2c_soil_t)

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

This 3-D file is created by the EPIC model via the FEST-C interface and contains soil properties for Layer 1 (0 to 1 cm depth) and Layer 2 (1 cm to 100 cm depth) for each crop and soil combination in each grid cell. Additional information on the EPIC model and the FEST-C interface are available at https://www.cmascenter.org/fest-c/. The following variables are in this file:

-   L1_SoilNum: Soil Number (none)
-   L1_Bulk_D: Layer1 Bulk Density (t/m**3)
-   L1_Wilt_P: Layer1 Wilting Point(m/m)
-   L1_Field_C: Layer1 Field Capacity (m/m)
-   L1_Porosity: Layer1 Porosity (%)
-   L1_PH: Layer1 PH (none)
-   L1_Cation: Layer1 Cation Ex (cmol/kg )
-   L2_Bulk_D: Layer2 Bulk Density (t/m**3)
-   L2_Wilt_P: Layer2 Wilting Point (m/m)
-   L2_Field_C: Layer2 Field Capacity (m/m)
-   L2_Porosity: Layer2 Porosity (%)
-   L2_PH: Layer2 PH (none)
-   L2_Cation: Layer2 Cation Ex (cmol/kg)

<a id="e2c_fert"></a>
### E2C_FERT – EPIC crop types and fertilizer application
[Return to Table 3-1](#e2c_fert_t)

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

This is a 3-D daily file created by the EPIC model via the FEST-C interface and contains information on fertilizer application rate and depth for each crop and soil combination in each grid cell. Additional information on the EPIC model and the FEST-C interface are available at https://www.cmascenter.org/fest-c/. The file contains many more variables than are used by CMAQ. The following variables are in this file:

-   QNO3: N Loss in Surface Runoff (kg/ha)
-   SSFN: N in Subsurface Flow (kg/ha)
-   PRKN: N Loss in Percolate (kg/ha)
-   DN: N-NO3 Denitrification (kg/ha)
-   DN2: N-N2O from NO3 Denitrification (kg/ha)
-   AVOL: N-NH3 Emission (kg/ha)
-   HMN: OC Change by Soil Respiration (kg/ha)
-   NFIX: N Fixation (kg/ha)
-   YP: P Loss with Sediment (kg/ha)
-   QAP: Labile P Loss in Runoff (kg/ha)
-   YON: N Loss with Sediment (kg/ha)
-   YW: Wind Erosion (ton/ha)
-   Q: Runoff (mm)
-   HUSC: Heat Unit Schedule (none)
-   HU_BASE0: Base Heat Unit (none)
-   HU_FRAC: Heat Unit fraction (none)
-   L1_DEP: Layer1 Depth (m)
-   L1_BD: Layer1 Bulk Density (t/m**3)
-   L1_NO3: Layer1 N - Nitrate (kg/ha)
-   L1_NH3: Layer1 N - Ammonia (kg/ha)
-   L1_ON: Layer1 Organic N (kg/ha)
-   L1_P: Layer1 Mineral P (kg/ha)
-   L1_OP: Layer1 Organic P (kg/ha)
-   L1_C: Layer1 Carbon (kg/ha)
-   L1_NITR: Layer1 N - Nitrified NH3 (kg/ha)
-   L2_DEP: Layer2 Depth (m)
-   L2_BD: Layer2 Bulk Density (t/m**3)
-   L2_NO3: Layer2 N - Nitrate (kg/ha)
-   L2_NH3: Layer2 N - Ammonia (kg/ha)
-   L2_ON: Layer2 Organic N (kg/ha)
-   L2_P: Layer2 Mineral P (kg/ha)
-   L2_OP: Layer2 Organic P (kg/ha)
-   L2_C: Layer2 Carbon (kg/ha)
-   L2_NITR: Layer2 N - Nitrified NH3 (kg/ha)
-   T1_DEP: Layert (Total Soil Profile) Depth (m)
-   T1_BD: Layert Bulk Density (t/m**3)
-   T1_NO3: Layert N - Nitrate (kg/ha)
-   T1_NH3: Layert N - Ammonia (kg/ha)
-   T1_ON: Layert Organic N (kg/ha)
-   T1_P: Layert Mineral P (kg/ha)
-   T1_OP: Layert Organic P (kg/ha)
-   T1_C: Layert Carbon (kg/ha)
-   T1_NITR: Layert N - Nitrified NH3 (kg/ha)
-   L1_ANO3: Layer1 N-NO3 AppRate (kg/ha)
-   L1_ANH3: Layer1 N-NH3 AppRate (kg/ha
-   L1_AON: Layer1 ON AppRate (kg/ha)
-   L1_AMP: Layer1 MP AppRate (kg/ha)
-   L1_AOP: Layer1 OP AppRate (kg/ha)
-   L2_ANO3: Layer2 N-NO3 AppRate (kg/ha)
-   L2_ANH3: Layer2 N-NH3 AppRate (kg/ha)
-   L2_AON: Layer2 ON AppRate (kg/ha)
-   L2_AMP: Layer2 MP AppRate (kg/ha)
-   L2_AOP: Layer2 OP AppRate (kg/ha)
-   UN1: N Uptake by Crop (kg/ha)
-   HUI: Heat Unit Index (none)
-   LAI: Leaf Area Index (none)
-   CPHT: Crop Height (m)

## Photolysis
<a id=jtable></a>
### JTABLE: Photolysis rates look-up table
[Return to Table 3-1](#jtable_t)

Used by: CCTM

Each of the gas-phase mechanisms in CMAQ contains photolysis reactions that require clear-sky reaction rates precomputed from kinetics data at various altitudes and latitude bands. The CMAQ program used to base on JPROC to generate photolysis rate look up table but correct CMAQ is based on inline photolysis calculation.

<a id=omi></a>
### OMI: Ozone Monitoring Instrument Column Data
[Return to Table 3-1](#omi_t)

Used by: CCTM

OMI ozone column data by latitude and longitude for use in the inline photolysis calculations. CMAQ is distributed with ozone columns from 1978 to 2015. The data are 22.5°x10° gridded ozone columns in Dobson units. [Table 3-5](#Table8-12) lists the format of the OMI data file.

<a id=Table8-12></a>

**>> Comment <<** DW: For the OMI part, please ask Bill to double check. He might have update with respect to data year as well as data resolution.

**Table 3-5. OMI data format**

| **Line** | **Column** | **Name** | **Type** | **Description**|
|-----|-----|------|-----|-----------------------------------|
| 1 || Header ||Header with names for each column|
| 2 | A | Yeardate | Real |YYYY.??? or YYYY.???? formatted date field.|
|| B | Latitude 1 | Int |80 North latitude|
|| C | Longitude 1 | Int |180.0Z longitude ozone column (DU)|
|| D | Longitude 2 | Int |157.5W longitude ozone column (DU)|
|| E | Longitude 3 | Int |135.0W longitude ozone column (DU)|
|| F | Longitude 4 | Int |112.5W longitude ozone column (DU)|
|| G | Longitude 5 | Int |090.0W longitude ozone column (DU)|
|| H | Longitude 6 | Int |067.5W longitude ozone column (DU)|
|| I | Longitude 7 | Int |045.0W longitude ozone column (DU)|
|| J | Longitude 8 | Int |022.5W longitude ozone column (DU)|
|| K | Longitude 9 | Int |000.0Z longitude ozone column (DU)|
|| L | Longitude 10 | Int |022.5E longitude ozone column (DU)|
|| M | Longitude 11 | Int |045.0E longitude ozone column (DU)|
|| N | Longitude 12 | Int |067.5E longitude ozone column (DU)|
|| O | Longitude 13 | Int |090.0E longitude ozone column (DU)|
|| P | Longitude 14 | Int |112.5E longitude ozone column (DU)|
|| Q | Longitude 15 | Int |135.0E longitude ozone column (DU)|
|| R | Longitude 16 | Int |157.5E longitude ozone column (DU)|
|| S | Longitude 17 | Int |180.0Z longitude ozone column (DU)|
| 3 | A | Yeardate | Real |YYYY.??? or YYYY.???? formatted date field.|
|| B | Latitude 2 | Int |70 North latitude|
|| C | Longitude 1 | Int |180.0Z longitude ozone column (DU)|
| … | … | … | … | … |
|| S | Longitude 17 | Int |180.0Z longitude ozone column (DU)|
| 4 | A | Yeardate | Real |YYYY.??? or YYYY.???? formatted date field.|
|| B | Latitude 3 | Int |60 North latitude|
|| C | Longitude 1 | Int |180.0Z longitude ozone column (DU)|
| … | … | … | … | … |
|| S | Longitude 17 | Int |180.0Z longitude ozone column (DU)|
| 5 | A | Yeardate | Real |YYYY.??? or YYYY.???? formatted date field.|
|| B | Latitude 4 | Int |50 North latitude|
|| C | Longitude 1 | Int |180.0Z longitude ozone column (DU)|
| … | … | … | … | … |
|| S | Longitude 17 | Int |180.0Z longitude ozone column (DU)|
|…|…|…|…| Repeat for total of 17 latitudes of data
|…|…|…|…| Repeat for (1978-2008) there are ~48 days (4 days per month) of data
|…|…|…|…| Repeat for (2009-2015) there are 365 days of data

## CCTM Output Files

** >> Comment <<** P153 (for example):  Should remove M3 I/O API "file type" from these tables.  Use more general descriptions.

In this section, details on the CCTM output files are provided. All CMAQ programs produce output files that adhere to the netCDF format.  In addition to model data output, CMAQ can optionally produce log files that contain the standard output from the various CMAQ processors. If the log file option is not selected by the user, CMAQ will write all of the log information to the screen along with the standard error, which can be captured to a text file using basic UNIX syntax.

<a id=Output_Table></a>

** >> Comment <<** In Table 8.1, those Hourly denotation, we should put an asterisk to indicate that it can be user defined, e.g. 30 minutes rather 1 hour and in the same table and table 8-13, there are places with +1 and I think we can make it more generic as +NTHIK (your call).


**Table 3-6. CMAQ Output files**

|**File Name**|**File Type**|**Time-Dependence**|**Spatial Dimensions**|
|----------------------------|------|----|-----------------------------------|
|**Standard**| | | |
|[Output Log](#cmaq_output_log) <a id=cmaq_output_log_t></a>|ASCII|n/a|n/a
|[CTM_CONC_1](#conc)<a id=conc_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_CGRID_1](#cgrid) <a id=cgrid_t></a>|GRDDED3|1-hour|[2(X+1)+2(Y+1)]*Z
|[CTM_ACONC_1](#aconc) <a id=aconc_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_DRY_DEP_1](#drydep) <a id=drydep_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_WETDEP_1](#wetdep) <a id=wetdep_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_VIS_1](#vis) <a id=vis_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_AVIS_1](#avis) <a id=avis_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|**Diagnostic and Advanced**| | | |
|[CTM_PMDIAG_1](#pmdiag) <a id=pmdiag_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_APMDIAG_1](#apmdiag) <a id=apmdiag_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[B3GTS_S](#b3gts) <a id=b3gts_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DEPV_DIAG](#depv) <a id=depv_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_PT3D_DIAG](#pt3d) <a id=pt3d_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_DUST_EMIS_1](#dust) <a id=dust_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_AOD_1](#aod) <a id=aod_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_IPR_1-3](#ipr) <a id=ipr_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_IRR_1-3](#irr) <a id=irr_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[FLOOR](#floor) <a id=floor_t></a>|ASCII|Hourly|n/a
|[MEDIA_CONC](#media)<a id=media_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DEPV_MOS](#depv_mos) <a id=depv_mos_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DRY_DEPV_MOS](#dry_depv_mos) <a id=dry_depv_mos_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DEPV_FST](#depv_fst) <a id=depv_fst_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DRY_DEPV_FST](#dry_depv_fst) <a id=dry_depv_fst_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_VDIFF_DIAG](#vdiff_diag) <a id=vdiff_diag_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_VSED_DIAG](#vsed_diag)<a id=vsed_diag_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[LTNG_DIAG1](#ltngdiag1) <a id=ltngdiag1_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[LTNG_DIAG2](#ltngdiag2) <a id=ltngdiag2_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[PLAY_SRCID](#play_srcid) <a id=play_srcid_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_RJ_1-2](#ctm_rj) <a id=ctm_rj_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[SOILOUT](#soilout) <a id=soilout_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_SSEMIS_1](#ssemis) <a id=ssemis_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_WETDEP_2](#wetdep2) <a id=wetdep2_t></a>|GRDDED3|Hourly|[2(X+1)+2(Y+1)]


<a id=cmaq_output_log></a>
### CMAQ output log
[Return to Table 3-6](#cmaq_output_log_t)

All of the CMAQ processors generate standard output and standard error during execution. For all of the processors other than CCTM, this diagnostic output information can be captured to a log file at execution using a UNIX redirect command. For example, to capture the standard output and error of a CCTM simulation, use the following command:

```
run.cctm >& tee cctm.log
```

For the CCTM, the LOGFILE environment variable allows users to specify the name of a log file for capturing the standard output from the program. If this variable is not set, the standard output is written to the terminal and can be captured using the UNIX redirect command (“>”), as shown in the example above.

<a id=conc></a>
### CTM_CONC_1: CCTM hourly instantaneous concentration file
[Return to Table 3-6](#conc_t)

The 3-D CCTM hourly concentration file (CONC) contains gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>). CONC files include instantaneous model species concentrations at the end of each model hour. The number and types of species contained in the CONC files depend on the chemical mechanism and aerosol model configurations that are selected when the CCTM is compiled. The [FORTRAN NameLists](#matrix_nml) within the mechanism directories list the modeled species, and contain a column that specifies which species are written to the CONC files. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the CONC file by editing the CONC column in the NameList file(s) to reduce the number of species that are written to, and thus the size of the CONC file.

<a id=cgrid></a>
### CTM_CGRID_1: CCTM restart file
[Return to Table 3-13](#cgrid_t)

The 3-D CCTM ending concentration file (CGRID) is the CCTM restart file. Containing gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>), the CGRID file includes model species concentrations at the end of each simulation period. The number and types of species contained in the output CGRID files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. This file can be used to initialize CCTM from the simulation period that the model completed. For example, if the CCTM is configure to produce daily output files, a CGRID file will be written out at the end of each simulation day.

<a id=aconc></a>
### CTM_ACONC_1: CCTM hourly average concentration file
[Return to Table 3-13](#aconc_t)

The 3-D CCTM integral average concentration file (ACONC) contains average model species concentrations for each model hour, as opposed to instantaneous concentrations at the end of each output time step. The species written to the ACONC file are set by the user in the CCTM run script using the variable AVG_CONC_SPCS. The model layers that are used to calculate the integral average concentration are also set in the CCTM run script using the variable ACONC_BLEV_ELEV, where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number. An example setting for the ACONC_BLEV_ELEV variable is “1 6”, which defines layers 1 through 6 as the vertical extent over which to calculate hourly average concentrations.

<a id=drydep></a>
### CTM_DRY_DEP_1: CCTM hourly cumulative dry deposition file
[Return to Table 3-13](#drydep_t)

The 2-D CCTM dry deposition file (DRYDEP) includes cumulative hourly dry deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates dry deposition for all of the species listed in the dry deposition column of the [FORTRAN NameLists](#matrix_nml) within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the dry deposition file by editing the DDEP column in the NameList file(s).

<a id=wetdep></a>
### CTM_WETDEP_1: CCTM hourly cumulative wet deposition file
[Return to Table 3-13](#wetdep_t)

The 2-D CCTM wet deposition file (WETDEP) includes cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the [FORTRAN NameLists](#matrix_nml) within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s).

<a id=vis></a>
### CTM_VIS_1
[Return to Table 3-13](#vis_t)

<a id=avis></a>
### CTM_AVIS_1
[Return to Table 3-13](#avis_t)

## Diagnostic and Advanced CMAQ Output Files

Along with the basic outputs detailed in the previous section, CMAQ can be configured to output several auxiliary files for diagnosing model performance.

<a id=pmdiag></a>
### CTM_PMDIAG_1: Instantaneous hourly aerosol diagnostics file
[Return to Table 3-13](#pmdiag_t)

This diagnostic file contains information on the geometric mean diameters and geometric standard deviations for the lognormal modes.

<a id=apmdiag></a>
### CTM_APMDIAG_1: Average hourly aerosol diagnostics file
[Return to Table 3-13](#apmdiag_t)


This diagnostic file contains information on the geometric mean diameters and geometric standard deviations for the lognormal modes.

<a id=b3gts></a>
### B3GTS_S: Biogenic emissions diagnostic file
[Return to Table 3-13](#b3gts_t)

This optional 2-D CCTM hourly output file contains calculated biogenic emissions in mass units. The B3GTS_S file will be produced only if in-line biogenic emissions are being calculated by CCTM and if the B3GTS_DIAG variable is turned on.

<a id=depv></a>
### CTM_DEPV_DIAG: CCTM inline deposition diagnostics file
[Return to Table 3-13](#depv_t)

This 2-D CCTM file contains the deposition velocity (m/s) for each chemical species calculated for the final time step for the hour.

<a id=pt3d></a>
### CTM_PT3D_DIAG: CCTM PT3D diagnostics file
[Return to Table 3-13](#pt3d_t)

The PT3D diagnostics file records the 3-D point source emissions as a linear average over the output timestep.

<a id=dust></a>
### CTM_DUST_EMIS_1
[Return to Table 3-13](#dust_t)

This optional 2-D CCTM hourly output file contains calculated dust emissions in mass units. The DUST_EMIS_1 file will be produced only if in-line windblown dust emissions are being calculated by CCTM and if the CTM_DUSTEM_DIAG variable is turned on.

<a id=aod></a>
### CTM_AOD_1
[Return to Table 3-13](#aod_t)

<a id=ipr></a>
### CTM_IPR_[1-3]
[Return to Table 3-13](#ipr_t)

The 3-D CCTM integrated process rate files (IPR) contains hourly concentrations of selected model output species in terms of the model process that contributed to the predicted concentration at each hour. For each grid cell in the process analysis domain (which is most likely a subset of the full modeling domain), the IPR file shows the hourly change in species concentration that is due to particular source/sink processes in the model. The input file procan.inp is used to set the model species for which to capture process analysis information, and the processes to track during the process analysis.

<a id=irr></a>
### CTM\_IRR_[1-3] Process analysis output – integrated reaction rates
[Return to Table 3-13](#irr_t)

The 3-D CCTM integrated reaction rate file (IRR) contains hourly concentrations of selected model output species in terms of the gas-phase chemistry pathways that contributed to the predicted concentration at each hour. For each grid cell in the process analysis domain (which is most likely a subset of the full modeling domain), the IRR file shows the hourly change in species concentration that is due to particular gas-phase chemistry reactions or reaction groups. The input file procan.inp is used to select the process analysis domain, the model species for which to capture process analysis information, and the chemistry reactions or groups of reactions to track during the process analysis.

<a id=floor></a>
### FLOOR: concentration-reset diagnostics file
[Return to Table 3-13](#floor_t)

FLOOR files are optional output diagnostic files which list specific gridboxes/timesteps in which species with negative concentrations are reset to zero.

<a id=media></a>
### MEDIA_CONC: Bidirectional soil NH4+ restart file
[Return to Table 3-13](#media_t)

This 2-D CCTM file contains the soil NH<sub>4</sub> and pH concentrations if using the bidirectional NH<sub>3</sub> option and/or the soil, vegetation and water Hg concentrations. This file is used to initialize the next day of the model simulation.

<a id=depv_mos></a>
### CTM_DEPV_MOS
[Return to Table 3-13](#depv_mos_t)

This 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) for the final time step of the hour for each land use type within a grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers".  

<a id=ctm_dry_depv_mos></a>
### CTM_DRY_DEPV_MOS
[Return to Table 3-13](#ctm_dry_depv_mos_t)

This 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) for the hour for each land use type within each grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers".  

<a id=depv_fst></a>
### CTM_DEPV_FST
[Return to Table 3-13](#depv_fst_t)

This 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) through the stomatal pathway for the final time step of the hour for each land use type within a grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers".  


<a id=ctm_dry_depv_fst></a>
### CTM_DRY_DEPV_FST
[Return to Table 3-13](#ctm_dry_depv_fst_t)

This 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) through the stomatal pathway for the hour for each land use type within each grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers".  

<a id=ctm_vdiff_diag></a>
### CTM_VDIFF_DIAG
[Return to Table 3-13](#ctm_vdiff_diag_t)

The VDIFF_DIAG file provides diagnostic output of vertical dispersion parameters.  It is controlled by the VDIFF_DIAG_FILE environment variable.


<a id=ctm_vsed_diag></a>
### CTM_VSED_DIAG
[Return to Table 3-13](#ctm_vsed_diag_t)

The VSED_DIAG file provides diagnostic output of particle gravitational settling velocities. It is controlled by the VDIFF_DIAG_FILE environment variable and activated when gravitational sedimentation is turned on.

<a id=ltnghourly></a>
### LTNG_DIAG1
[Return to Table 3-13](#ltnghourly_t)

Hourly 3-D lightning NO emissions calculated in-line by the CCTM.

<a id=ltngcol></a>
### LTNG_DIAG2
[Return to Table 3-13](#ltngcol_t)

Hourly column-total lightning NO emissions calculated in-line by the CCTM.

<a id=play_srcid></a>
### PLAY_SRCID
[Return to Table 3-13](#play_srcid_t)

<a id=ctm_rj></a>
### CTM\_RJ_[1-2]: In-line photolysis output – gridded photolysis rates
[Return to Table 3-13](#ctm_rj_t)

The photolysis diagnostic output files (RJ) contain the photolysis rates calculated by CCTM when the in-line photolysis option is used.

<a id=soilout></a>
### SOILOUT
[Return to Table 3-13](#soilout_t)

Name and location of hourly soil NO emissions file; output when in-line biogenic emissions processing is activated by setting CTM_BIOGEMIS to “T” or “Y”.

<a id=ssemis></a>
### CTM_SSEMIS_1: Sea salt emissions diagnostic file
[Return to Table 3-13](#ssemis_t)

This optional 2-D CCTM hourly output file contains calculated sea salt emissions. The SSEMIS file will be produced by CCTM only if the CTM_SSEMDIAG variables are turned on.

<a id=wetdep2></a>
### CTM_WETDEP_2: CCTM cloud diagnostics file
[Return to Table 3-13](#wetdep2_t)

In CMAQ, wet deposition is calculated separately for resolved (grid-scale) clouds and for convective (subgrid) clouds. The WETDEP1 file contains the total wet deposition, i.e., the sum of both resolved-scale and subgrid-scale deposition. The WETDEP2 file contains only subgrid-scale deposition, plus some cloud diagnostic variables. The 2-D CCTM wet deposition file (WETDEP2) includes cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the FORTRAN Namelist files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s).


[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch04_model_formulation.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
