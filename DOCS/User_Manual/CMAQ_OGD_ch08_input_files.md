<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch07_programs_libraries.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch09_grid_defn.md)

<!-- END COMMENT -->

# CMAQ Input and Output Files #

[Jump to Input Files](#inputs)<br>
[Jump to CCTM Output Files](#outputs)

The input files for CMAQ consist of a domain definition file for all programs; two sets of file options for both ICON and BCON; two types of input files (WRF/MM5 and terrain) for MCIP; five mandatory and one optional input file for JPROC; and for CCTM, emissions, initial conditions, and boundary conditions files, six files that define the meteorological conditions to be simulated, and a photolysis rates file. For most CCTM input, a separate data set is required for each horizontal domain that is modeled. When CMAQ is configured for in-line emissions and deposition, there are additional emissions input files that are required.

CMAQ output files include a basic set of files with aerosol and gas-phase species concentrations, wet and dry deposition estimates, and visibility metrics, and an auxiliary set of output files for diagnosing model performance and in-line-calculated emissions.

<a id=inputs></a>
## CMAQ Input Files

This section describes each of the input files required by the various CMAQ programs. The section begins with a description of the grid definition file, which is used by several CMAQ programs, and then goes through a program-by-program listing of the CMAQ input file requirements. [Table 8-1](#Table8-1) lists the source, file type, and temporal and spatial dimensions of each CMAQ input file. Sample disk space requirements for a desired input data set can easily be calculated from the information in [Table 8-1](#Table8-1); each data record is four bytes. The I/O API file sizes can be calculated using the number of variables in a CMAQ file and the spatial and temporal coverage of the data. The user should consult the CMAQ release notes for additional file information.

<a id=Table8-1></a>

**Table 8-1. CMAQ input files**

|**File Name**|**File Type**|**Time-Dependence**|**Spatial Dimensions**|**Source**|
|-------------------------|----------------|----------------|----------------|-----------------------------------|
|**General**| | | | |
|[GRIDDESC](#griddesc)|ASCII|n/a|n/a|user/MCIP
|[gc_matrix.nml](#matrix_nml)|ASCII|n/a|n/a|CSV2NML
|[ae_matrix.nml](#matrix_nml)|ASCII|n/a|n/a|CSV2NML
|[nr_matrix.nml](#matrix_nml)|ASCII|n/a|n/a|CSV2NML
|[tr_matrix.nml](#matrix_nml)|ASCII|n/a|n/a|CSV2NML
|**ICON**| | | | |
|[IC_PROFILE](#ic_profile)|ASCII|Annual|n/a|user|
|[CTM_CONC_1](#ctm_conc_1)|GRDDED3|Hourly|X*Y*Z|CCTM|
|[MET_CRO_3D](#met_cro_3d) |GRDDED3|Hourly|X*Y*Z|MCIP
|**BCON**| | | | |
|[BC_PROFILE](#bc_profile) |ASCII|Annual|n/a|user
|[CTM_CONC_1](#ctm_conc_1) |GRDDED3|Hourly|X*Y*Z|CCTM
|[MET_CRO_3D](#met_cro_3d) |GRDDED3|Hourly|X*Y*Z|MCIP|
|**JPROC**| | | | |
|[ET](#et) |ASCII|Annual|n/a|user
|[PROFILES](#profiles) |ASCII|Annual|n/a|user
|[O2ABS](#o2abs) |ASCII|Annual|n/a|user
|[O3ABS](#o3abs) |ASCII|Annual|n/a|user
|[TOMS](#toms) |ASCII|varies|n/a|user
|[CSQY](#csqy) |ASCII| Annual|n/a| User
|**MCIP**| | | | |
|[InMetFiles](#inmetfiles) |Binary or netCDF| typically hourly, but sometimes sub-hourly| X*Y*Z| MM5 or WRF-ARW
|[InTerFile](#interfile) |Binary| n/a| X*Y | MM5 or WRF
|[InSatFiles](#insatfiles)||||Spatial Allocator|
|**CCTM** | | | | |
|[INIT_CONC_1](#init_conc_1) | GRDDED3 | Time-invariant | X*Y*Z | ICON/CCTM
|[BNDY_CONC_1](#bndy_conc_1) | BNDARY3 | Hourly |[2(X+1)+2(Y+1)]*Z | BCON
|[JTABLE](#jtable) | ASCII | Daily | n/a | JPROC
|[OMI](#omi) | ASCII | daily | n/a ||
|[EMIS_1](#emis_1) | GRDDED3 | Hourly | X\*Y\*Z | SMOKE
|[OCEAN_1](#ocean_1) | GRDDED3 | Time-invariant | X\*Y |Spatial Allocator
|[GSPRO](#gspro) | ASCII | Time-invariant | N/a | User
|[B3GRD](#b3grd) | GRDDED3 | Time-invariant | X\*Y | SMOKE
|[BIOSEASON](#bioseason) |GRDDED3 |Time-invariant | X\*Y | Metscan
|[STK_GRPS_nn](#stk_grps) | GRDDED3 |Time-invariant|X\*Y | SMOKE
|[STK_EMIS_nn](#stk_emis) | GRDDED3 | Hourly | X\*Y | SMOKE
|[DUST_LU_1](#dust_lu_1)|GRDDED3 | Time-invariant | X\*Y | Spatial Allocator
|[DUST_LU_2](#dust_lu_1)|GRDDED3 | Time-invariant | X\*Y | Spatial Allocator
|[MODIS_FPAR](#modis_fpar)|GRDDED3 | Daily | X\*Y | Spatial Allocator
|[CROPMAP01](#cropmap01)| GRDDED3 | Time-invariant | X\*Y | Cropcal
|[CROPMAP04](#cropmap04)| GRDDED3 | Time-invariant | X\*Y | Cropcal
|[CROPMAP08](#cropmap08)|GRDDED3 | Time-invariant | X\*Y | Cropcal
|[LTNGNO](#ltngno)| GRDDED3 | Hourly | X\*Y\*Z | User|
|[NLDN_STRIKES](#nldn_strikes)| GRDDED3 | Hourly | X\*Y ||
|[LTNGPARMS_FILE](#ltngparm_file)| GRDDED3 | Time-invariant | X\*Y ||
|[BELD4_LU](beld4_lu)| GRDDED3 | Time-invariant |X\*Y||
|[E2C_SOIL](#e2c_soil)| GRDDED3 | Time-invariant | X\*Y||
|[E2C_FERT](#e2c_fert)| GRDDED3 | Daily |X\*Y||
|[MEDIA_CONC](#init_medc_1)| GRDDED3 |Hourly| X\*Y ||
|[INIT_GASC_1](#init_gasc_1)| GRDDED3 || X\*Y ||
|[INIT_AERO_1](#init_aero_1)| GRDDED3 || X\*Y ||
|[INIT_NONR_1](#init_nonr_1)| GRDDED3 || X\*Y ||
|[INIT_TRAC_1](#init_trac_1)| GRDDED3 || X\*Y ||
|[GRID_CRO_2D](#grid_cro_2d) | GRDDED3 | Time-invariant | X\*Y | MCIP
|[GRID_CRO_3D](#grid_cro_3d) | GRDDED3 | Time-invariant | X\*Y\*Z | MCIP
|[GRID_BDY_2D](#grid_bdy_2D) | GRDDED3 | Time-invariant | PERIM\*Z | MCIP
|[GRID_DOT_2D](#grid_dot_2d) | GRDDED3 | Time-invariant | (X+1)\*(Y+1) | MCIP
|[MET_BDY_3D](#met_bdy_3d) | BNDARY3 | Hourly | PERIM\*Z | MCIP
|[MET_CRO_2D](#met_cro_2d) | GRDDED3 | Hourly | X\*Y | MCIP
|[MET_CRO_3D](#met_cro_3d) | GRDDED3 | Hourly | X\*Y\*Z | MCIP
|[MET_DOT_3D](#met_dot_3d) | GRDDED3 | Hourly | (X+1)\*(Y+1)\*Z | MCIP


<a id=griddesc></a>
### GRIDDESC: Horizontal domain definition

Used by: ICON, BCON, CCTM

The CMAQ grid description file (**GRIDDESC**) is used by all programs except JPROC and MCIP to define the horizontal spatial grid of the modeling domain. GRIDDESC implements [I/O API](CMAQ_OGD_ch06_req_lib.md#IOAPI) grid conventions: for more details see the section on [Grids and coordinate systems](CMAQ_OGD_ch09_grid_defn.md#grids-and-coordinate-systems).

A GRIDDESC is an ASCII file that contains two sections: a horizontal coordinate section, and grid description section. GRIDDESC is the logical name for text files that store horizontal coordinate and grid descriptions, and that are read by the DSCGRID() and DSCOORD() utility routines. Each segment has a one-line header (which by convention provides titles for the columns in the data records), a sequence of data records, and a terminal record with name field blank (i.e., ' '). The GRIDDESC file is generated automatically with MCIP; alternatively, GRIDDESC can be created using a text editor.

The horizontal coordinate section ([Table 8-2](#Table8-2)) consists of text records that provide coordinate-system name, the map projection, and descriptive parameters P_ALP, P_BET, P_GAM, XCENT, and YCENT.

The grid description section ([Table 8-3](#Table8-3)) consists of text records that indicate the grid name, related coordinate-system name (i.e., which GRIDDESC horizontal coordinate name that is defined in the previous section that is applied to this grid), and descriptive parameters XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, and NTHIK. For a typical CMAQ application, both "dot-point" and "cross-point" grids are defined in the GRIDDESC file; these grids are topological duals in the sense that the vertices (corners) of one correspond to the cell-centers of the other.

<a id=Table8-2></a>

 **Table 8-2. Coordinate sytem description segment of GRIDDESC**

| **Line**| **Column**| **Name** | **Type** | **Description**|
|-------|----------|----------------|----------|--------------------------------------------------------|
|1|A| Header | String |Single-quote-delimited header describing section contents; may be blank, i.e., ' '|
|2|A| COORD-NAME | String |Name of the coordinate description (required); single quote delimited|
|3|A| COORDTYPE | Int |I/O API index defining the map projection type (required)| |
|3|B| P_ALP | Double |First map projection descriptive parameter (dependent on projection type)| |
|3|C| P_BET | Double |Second map projection descriptive parameter (dependent on projection type)| |  
|3|D| P_GAM | Double |Third map projection descriptive parameter (dependent on projection type)| |  
|3|E| XCENT | Double |Longitude for coordinate system center| | 
|3|F| YCENT | Double |Latitude for coordinate system center|

<a id=Table8-3></a>

 **Table 8-3. Grid definition segment of GRIDDESC**

|**Line** | **Column** | **Name** | **Type** | **Description**|
|-----------|----------|----------|----------|-----------------------------------------------------|
| 1 | A | Header | String|Single-quote-delimited header describing section contents; may be blank, i.e., ' '|
| 2 | A | GRID-NAME | String |Name of the horizontal grid (required); single quote delimited|
| 3 | A | COORD-NAME| String |Name of the coordinate description in the previous section (required); single quote delimited|
| 3 | B | XORIG | Double |X-coordinate for lower-left (southwest) corner of the grid with respect to (XCENT,YCENT) (dependent on projection type)|
|3 | C | YORIG | Double |Y-coordinate for lower-left (southwest) corner of the grid with respect to (XCENT,YCENT) (dependent on projection type)|
|3 | D | XCELL | Double|X-coordinate grid cell size (dependent on projection type)|
|3 | E | YCELL | Double|Y-coordinate grid cell size (dependent on projection type)|
|3 | F | NCOLS | Int |Number of horizontal grid columns (dependent on projection type)|
|3 | G | NROWS | Int |Number of horizontal grid rows (dependent on projection type)|
|3 | H | NTHIK | Int |Boundary perimeter thickness (number of cells) (optional)|

Each data record in these files consists of two or three list-formatted lines (i.e., items are separated by either blanks or commas). Name fields are quoted strings, and appear on the first of these lines. Numeric fields are given in double precision, and occur on either the second line or the second and third lines (this allows you to organize the text so that it is easily viewed in a text editor without running off-screen). The records have the following organization, depending upon whether they are in the first or second segment of GRIDDESC:

` COORD-NAME`
` COORDTYPE, P_ALP, P_BET, P_GAM`
` XCENT, YCENT`

or

` COORD-NAME`
` COORDTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT`

and

` GRID-NAME`
` COORD-NAME, XORIG, YORIG, XCELL, YCELL`
` NCOLS, NROWS, NTHIK`

or

` GRID-NAME`
` COORD-NAME, XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK`

There are at most 32 coordinate systems and 256 grids listed in one of these files. These files are small enough to be archived easily with a study, and have a sufficiently simple format that new ones can easily be constructed "by hand."

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

Used by: BCON, CCTM, ICON, CHEMMECH

Namelist look-up tables for different classes of simulated pollutants are used to define the parameters of different model species during the execution of the CMAQ programs. Gas-phase (gc), aerosol (ae), non-reactive (nr), and tracer (tr) species namelist files contain parameters for the model species that are included in these different classifications. The species namelist files are used to control how the different CMAQ programs and processes handle the model species. The namelist files define the following processes for each model species:

-   Emissions surrogate – which (if any) emitted species is the pollutant mapped to; For GC, NR, and TR species, the variable names in the emission files determine the allowed surrogate values; For AE species, the model source code determines the allowed values.
-   Emissions factor – if the pollutant is mapped to an emitted species, uniformly apply a scaling factor to the emissions
-   Deposition velocity – which (if any) deposition velocity is the deposition velocity for the pollutant mapped to; Allowed velocities are specified within the model source code.
-   Deposition velocity factor – if the pollutant is mapped to a deposition velocity, uniformly apply a scaling factor to this velocity
-   Initial/boundary conditions – which initial and boundary condition species is the pollutant mapped to; if not specified, this will default to the species name
-   IC/BC Factor – if the pollutant is mapped to an initial/boundary condition species, uniformly apply a scaling factor to the concentrations
-   Scavenging - which (if any) species is the pollutant mapped to; Allowed scavenging surrogates are specified within the model source code.
-   Scavenging factor - if the pollutant is mapped to an species for scavenging, uniformly apply a scaling factor to the scavenging rate
-   Gas-to-aerosol conversion – which (if any) aerosol chemistry species does the gas phase pollutant concentration go into for transformation from the gas-phase to the aerosol-phase
-   Gas-to-aqueous Surrogate – which (if any) cloud chemistry species does the gas pollutant concentration go into for simulating chemistry within cloud water
-   Aerosol-to-aqueous Surrogate – which (if any) cloud chemistry species does the aerosol pollutant concentration go into for simulating chemistry within cloud water
-   Transport – is the pollutant transported by advection and diffusion in the model?
-   Dry deposition – Write the pollutant to the dry deposition output file?
-   Wet deposition – Write the pollutant to the wet deposition output file?
-   Concentration – Write the pollutant to the instantaneous concentration output file?

The namelist files contain header information that describe which class of species are contained in the file, the number of parameters contained in the file, headers describing the parameter fields, and then a series of rows with configuration parameters for every model species. [Table 8-4](#Table8-4) contains the namelist file format for the gas-phase (GC) species namelist file. The namelist files for the other species classifications (AE, NR, TR) are similar to the format shown in [Table 8-4](#Table8-4).

<a id=Table8-4></a>

 **Table 8-4. GC species namelist file format**

| **Line**| **Column** |**Name** | **Type**| **Description** |
|-----|-----|----------------------|----------|--------------------------------------------|
| 1 || !Revision Control System(RCS file) |
| 2 || Header: filename, version, date/time, author |
| 4 || File type | String |&GC_nml|
| 6 || Number of group 1 surrogate params | String |n_surr1 = x, where x is the number of surrogates that are specified in pairs of surrogate species and surrogate factor, i.e. emissions, deposition, initial/boundary conditions, and scavenging|
| 7 || Number of group2 surrogate params | String |n_surr2 = x, where x is the number of surrogates that are specified only as a surrogate species, i.e. gas-to-aerosol conversion, gas-to-aqueous conversion, and aerosol-to aqueous conversion|
| 8 || Number of control params | String |n_ctrl = x, where x is the number of Y/N parameters controlling whether this pollutant is transported and whether it is written to the dry deposition, wet deposition, and/or concentration output files|
| 10 || Header ID | String |TYPE_HEADER =|
| 11 || HEADER | String |Abbreviated names of file columns, enclosed by single quotes|
|  12 || Matrix ID | String |TYPE_MATRIX =|
| 13 | 1 | SPC | String |CMAQ pollutant name, i.e. NO, HNO3, PAR; dependent on chemical mechanism|
|| 2 | MOLWT | Integer |Pollutant molecular weight|
|| 3 | EMIS_SUR | String |Emissions species name for the CMAQ pollutant|
|| 4 | EMIS_FAC | Real |Scaling factor for input emissions|
|| 5 | DEPV_SUR | String |Deposition velocity variable name for the CMAQ pollutant|
|| 6 | DEPV_FAC | Real |Scaling factor for the deposition velocity|
|| 7 | ICBC_SUR | String |IC/BC species name for the CMAQ pollutant|
|| 8 | ICBC_FAC | Real |Scaling factor for the IC/BC concentration|
|| 9 | SCAV_SUR | String |Wet Deposition Scavenging Coefficient|
|| 10 | SCAV_FAC | Real |Scaling factor for Scavenging |
|| 11 | G2AE_SUR | String |Gas-to-aerosol transformation species|
|| 12 | G2AQ_SUR | String |Gas-to-aqueous transformation species|
|| 13 | TRNS | Yes/No |Transport switch. Note: Instead of using one column labeled "TRNS" to turn on/off both advection and diffusion for a pollutant, two separate columns labeled "ADV" and "DIFF" can be used to switch on/off advection and diffusion separately|
|| 14 | DDEP | Yes/No |Dry deposition output file switch|
|| 15 | WDEP | Yes/No |Wet deposition output file switch|
|| 16 | CONC | Yes/No |Concentration output file switch|
| … | ...| ...|... | Repeat for the number of gas-phase pollutants in the mechanism being modeling|

The namelist files for the other pollutant classes have similar configurations as the gas-phase species configuration shown in [Table 8-4](#Table8-4). For an example see this [link](../../src/MECHS/cb05e51_ae6_aq/GC_cb05e51_ae6_aq.nml) to the GC namelist species file for the cb05e51_ae6_aq mechanism.

<a id=ic_profile></a>
### IC_PROFILE: Initial conditions vertical profiles

Used by: ICON

ICON can generate initial conditions from two different input file types. The first file type is an ASCII vertical profile file that lists species concentrations at various model layers that are fixed in space and time. To configure ICON to generate initial conditions from ASCII vertical profiles, the “prof” input module is chosen when compiling the program (see [Chapter 8](CMAQ_OGD_ch07_programs_libraries.md#ICON)). These ASCII-formatted vertical profile files are IC_PROFILE files, and are described in this section. IC_PROFILE files must be developed by the user and can be generated from climatologically averaged observational data or as an a priori estimate from previous modeling studies of the region being modeled. The second file type that ICON can use to generate initial conditions is a concentration file from a previous CMAQ run. These are CTM_CONC_1 files, and are described later in the [CTM_CONC_1 section](#ctm_conc_1).

IC_PROFILE begins with a header that contains a comment section that describes the data, and a file description section that defines the number of vertical levels in the file, the number of pollutants in the file, and the distribution of the vertical levels. The next entries in IC_PROFILE are the Julian start date and the start time of the data; they are not used by ICON.

Each line in IC_PROFILE corresponds to a different pollutant and begins with the name of the pollutant. The subsequent columns on each line list the chemical concentration at each layer contained in the file. Gas-phase species are in ppmV, and aerosol species are in µg m<sup>-3</sup>. The layer structure of the IC_PROFILE vertical profiles does not need to correspond exactly to the layer structure that will be modeled; the ICON program will interpolate the data to the correct vertical format for the simulation.

Initial conditions are provided for only the first hour of a model simulation. The initial conditions that are based on an ASCII vertical profile include a gridded file for input to CCTM that has horizontally uniform species concentrations at each model layer. For spatially resolved initial conditions in the horizontal direction, it is necessary to use the other input file type to ICON, an existing CCTM concentration file (CTM_CONC_1).

A detailed description of the vertical profile file format for initial conditions is provided in [Table 8-5](#Table8-5). The header of the profiles file is list-formatted, while the data section of the file is fixed format.

<a id=Table8-5></a>

 **Table 8-5. IC_PROFILE format description**

|**Line**|**Column**|**Name**|**Type**|**Description**|
|---------|-------------|--------------------------|---------|------------------------------------------------------|
| 1-3 || Text Header | String |Text description of the contents and source of the initial conditions file (optional)|
| 4 | A | NUM_SIGMA_LVL | Int |Number of sigma levels contained in the file (required)|
|| B | NUM_POLL | Int |Number of pollutants contained in the file (required)|
|| C | SIGMA_LVL | Real |Vertical coordinate values of sigma-p levels; number of values (n+1) is one more than the NUM_SIGMA_LVL (n) (required)|
| 4 | n | ...  | ...  |...|
| 5 | A | STDATE | String |Julian start date of the file, YYYYDDD (optional)|
|| B | STTIME | String |Start time of the file, HH (optional)|
| 6 | 1-10 | SPECIES1 | String |Pollutant name, enclosed in double quotes (required)|
|| 12-20 | LAYER1_IC | Exp |IC concentration for species 1 in lowest sigma layer (required)|
|| 23-31 | LAYER2_IC | Exp |IC concentration for species 1 in 2nd sigma layer (required)|
|| 34-42 | LAYER3_IC | Exp |IC concentration for species 1 in 3rd sigma layer (required)|
|| 45-53 | LAYER4_IC | Exp |IC concentration for species 1 in 4th sigma layer (required)|
|| ...  | LAYERX_IC | Exp |IC concentration for species 1 in Xth sigma layer (required)|
 | 7 | 1-10 | SPECIES2 | String |Pollutant name, enclosed in double quotes (required)|
|| 12-20 | LAYER1_IC | Exp |IC concentration for species 2 in lowest sigma layer (required)|
|| 23-31 | LAYER2_IC | Exp |IC concentration for species 2 in 2nd sigma layer (required)|
|| 34-42 | LAYER3_IC | Exp |IC concentration for species 2 in 3rd sigma layer (required)|
|| 45-53 | LAYER4_IC | Exp |IC concentration for species 2 in 4th sigma layer (required)|
|| ...  | LAYERX_IC | Exp |IC concentration for species 2 in Xth sigma layer (required)|
| ...  | ...  | ...  | ...  |...|
| Z | 1-10 | SPECIESZ | String |Pollutant name, enclosed in double quotes (required)|
| ...  | 12-20 | LAYER1_IC | Exp |IC concentration for species Z in lowest sigma layer (required)|
| ...  | 23-31 | LAYER2_IC | Exp |IC concentration for species Z in 2nd sigma layer (required)|
| ...  | 34-42 | LAYER3_IC | Exp |IC concentration for species Z in 3rd sigma layer (required)|
| ...  | 45-53 | LAYER4_IC | Exp |IC concentration for species Z in 4th sigma layer (required)|
| ...  | ...  | LAYERX_IC | Exp |IC concentration for species Z in Xth sigma layer (required)|

A sample of the four sections of an IC_PROFILE file is shown below.

```
Example initial condition: The vertical coordinate of the model to generate
these i.c. is the terrain-following sigma coordinate. 
The number of sigma layers and defined sigma levels are listed below. 
 6 55 1.00 0.98 0.93 0.84 0.60 0.30 0.00
1988180 00
"SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04
```

<a id=bc_profile></a>
### BC_PROFILE: Boundary conditions vertical profiles

Used by: BCON

As with the ICON program, BCON can generate boundary conditions from two different input file types. The first file type is an ASCII vertical profile file that list species concentrations at various model layers that are fixed in space in time. To configure BCON to generate boundary conditions from ASCII vertical profiles, the “prof” input module is chosen when compiling the program (see Section 5.2 on BCON). These ASCII-formatted vertical profile files are BC_PROFILE files, and are described in this section. BC_PROFILE files must be developed by the user and can be generated from climatologically averaged observational data or as an a priori estimate from previous modeling studies of the region being modeled. The second file type that BCON can use to generate initial conditions is a concentration file from a previous CMAQ run. These are CTM_CONC_1 files, and are described later in the [CTM_CONC_1 section](#ctm_conc_1).

BC_PROFILE begins with a header that contains a comment section that describes the data, and a file description section that defines the number of vertical levels in the file, the number of pollutants in the file, and the distribution of the vertical levels. The next entries in BC_PROFILE are the Julian start date and the start time of the data; they are not used by BCON. The BCON input consists of four data sections that correspond to the lateral boundaries (i.e., north, south, east, and west) of the model grid. The BCON input profiles contain a field that precedes each data section to indicate which horizontal boundary the data section describes.

The format of the data sections in BC_PROFILE files is the same as in IC_PROFILE files. Each line corresponds to a different pollutant and begins with the name of the pollutant. The subsequent columns on each line list the chemical concentration at each layer contained in the file. Gas-phase species are in ppmV, and aerosol species are in ?g m<sup>-3</sup>. The layer structure of the BC_PROFILE vertical profiles does not need to correspond exactly to the layer structure that will be modeled; the BCON program will interpolate the data to the correct vertical format for the simulation.

Boundary conditions can either be time-independent (static) or time-dependent (dynamic). Boundary conditions generated with BC_PROFILE’s ASCII vertical profiles are both static and spatially uniform along each of the four horizontal boundaries at each model layer. For spatially resolved (in the horizontal direction) and temporally resolved boundary conditions, it is necessary to use the other input file type to BCON, an existing CCTM concentration file (CTM_CONC_1).

A detailed description of the vertical profile file format for boundary conditions is provided in [Table 8-6](#Table8-6). The header of the profiles file is list-formatted, while the data section of the file is fixed format.

<a id=Table8-6></a>

**Table 8-6. BC_PROFILE format description**

|**Line**|**Column**|**Name**|**Type**|**Description**|
|--------|------------|--------------------|----------|---------------------------------------------|
| 1-3 || Text Header | String |Text description of the contents and source of the initial conditions file (optional) |
| 4 | A | NUM_SIGMA_LVL | Int |Number of sigma levels contained in the file (required)|
|| B | NUM_POLL | Int |Number of pollutants contained in the file (required)|
|| C | SIGMA_LVL | Real |Vertical coordinate values of sigma-p levels; number of values (n+1) is one more than the NUM_SIGMA_LVL (n) (required)|
| 4 | n | ...  | ...  |...|
| 5 | A | STDATE | String |Julian start date of the file, YYYYDDD (optional)|
|| B | STTIME | String |Start time of the file, HH (optional)|
| 6 | A | Direction | String |North, South, East, West; indicates the boundary described by the subsequent data section (required)|
| 7 | 1-10 | SPECIES1 | String |Pollutant name, enclosed in double quotes (required)|
|| 12-20 | LAYER1_BC | Exp |BC concentration for species 1 in lowest sigma layer (required)|
|| 23-31 | LAYER2_BC | Exp |BC concentration for species 1 in 2nd sigma layer (required)|
|| 34-42 | LAYER3_BC | Exp |BC concentration for species 1 in 3rd sigma layer (required)|
|| 45-53 | LAYER4_BC | Exp |BC concentration for species 1 in 4th sigma layer (required)|
|| ...  | LAYERX_BC | Exp |BC concentration for species 1 in Xth sigma layer (required)|
| 8 | 1-10 | SPECIES2 | String |Pollutant name, enclosed in double quotes (required)|
|| 12-20 | LAYER1_BC | Exp |BC concentration for species 2 in lowest sigma layer (required)|
|| 23-31 | LAYER2_BC | Exp |BC concentration for species 2 in 2nd sigma layer (required)|
|| 34-42 | LAYER3_BC | Exp |BC concentration for species 2 in 3rd sigma layer (required)|
|| 45-53 | LAYER4_BC | Exp |BC concentration for species 2 in 4th sigma layer (required)|
|| ...  | LAYERX_BC | Exp |BC concentration for species 2 in Xth sigma layer (required)|
| ...  | ...  | ...  | ...  |...|
| Y | A | Direction | String |North, South, East, West; indicates the horizontal boundary described by the subsequent data section (required)|
| Z+1 | 1-10 | SPECIESZ | String |Pollutant name, enclosed in double quotes (required)|
| ...  | 12-20 | LAYER1_BC | Exp |BC concentration for species Z in lowest sigma layer (required)|
| ...  | 23-31 | LAYER2_BC | Exp |BC concentration for species Z in 2nd sigma layer (required)|
| ...  | 34-42 | LAYER3_BC | Exp |BC concentration for species Z in 3rd sigma layer (required)|
| ...  | 45-53 | LAYER4_BC | Exp |BC concentration for species Z in 4th sigma layer (required)|
| ...  | ...  | LAYERX_BC | Exp |BC concentration for species Z in Xth sigma layer (required)|

A sample of the important sections of a BC_PROFILE file is shown below.

``` 
6 55 1.00 0.98 0.93 0.84 0.60 0.30 0.00
1988180 00
 North
 "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04
 West
 "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04
```

<a name="ctm_conc_1"></a>
### CTM_CONC_1: CCTM concentration files

Used by: ICON, BCON

An I/O API GRDDED3-formatted CCTM output concentration file, CTM_CONC_1, can be used to create spatially and temporally varying initial and boundary conditions. To configure ICON and BCON to generate initial and boundary conditions from a CCTM concentration file, the “m3conc” input module is chosen when compiling the programs (see Section 5.5 on ICON and Section 5.2 on BCON). The input concentration file must cover a temporal and spatial extent that is consistent with the time period and domain that are being modeled, respectively. Both ICON and BCON require a Julian start date to be specified at execution that identifies the first time step to extract from the input concentration file; BCON also requires a run-length specification to indicate the number of time steps of boundary conditions to extract from the input file. For nested runs, the nested domain for which initial and boundary conditions are being extracted must be on the same projection and fall within the domain contained in the input concentration file.

<a name="CSQY"></a>
### CSQY: Absorption cross section and quantum yields

Used by: JPROC

CSQY is the logical name for the ASCII data file containing absorption cross section and quantum yield data for unique photolysis reactions. The data in these files are listed as a function of wavelength and correspond to the standard photolysis reactions in each of the chemical mechanisms implemented in CMAQ. A flexible format allows users to deviate from these standards and test new data with minimal effort.

The ASCII-formatted CSQY files begin with the number and a list of the applicable photolysis reactions. The data section of the CSQY file includes (1) the reaction name/ID; (2) The temperature bands (3) the number of wavelength bins (4) the start, effect and end of the wavelength bin and the photon flux for each inline band. The absorption cross section averaged over UCI Solar Flux(cm), and the quantum yield averaged over UCI Solar Flux as columns, for each wavelength bin, for each temperature, and for each reaction. The CSQY file uses a space-delimited, free-form format for the data section of the file. A detailed description of the CSQY file format is provided in [Table 8-7](#Table8-7).

<a id=Table8-7></a>

**Table 8-7. CSQY format description**

|**Line** | **Column** | **Name** | **Type** | **Description** |
|--------------------------|----|--------------------------|---------|-----------------------------------------|
| 1 | A | CSQY Mechanism Table Name | String |Text name indicating this is the CSQY for the Mechanism Specified.  This name is cross-referenced in the chemical mechanism description files (required)|
| 1 | A | NPHOTAB | String |Number of Photolysis Reactions|
| 2 | A | Comments | String |Preceded by "!", Individual Reaction Rates Listed below|
| 3 | A  | Name | String| Reaction Name  |
| repeat for all (NPHOTAB) reaction names|
| 3+NPHOTAB | A | Name | String |Reaction Name|
| 3+NPHOTAB+1 | A | NTEMP| Int or Real |Number of Temperature Bands|
| 3+NPHOTAB+2 | A | Number| Integer | Index of temperature |
|| B | TEMP | Real | Temperature in Kelvin|
| repeat for all NTEMP values|
| 3+NPHOTAB+NTEMP | A | Comments | String |Preceded by "!", column header: I,      START_WL_BIN(nm),    EFFECT_WL_BIN_(nm),       END_WL_BIN_(nm), photon_flux(cm-2\*s-1), |
| 3+NPHOTAB+NTEMP+1 | A | Number of Inline Photolysis Bands| Integer | |
|| B | Start Wavelength | Real |Start Wavelength for Bin |
|| C | Effect Wavelength | Real |Effect Wavelength for Bin |
|| D | End Wavelength | Real |End Wavelength for Bin |
|| E | Photon Flux| Real |Photon Flux |
| 3+NPHOTAB+NTEMP+2 | A | Comments | String |Preceded by "!", CS  = absorption cross sections averaged over UCI Solar Flux|
| 3+NPHOTAB+NTEMP+3 | A | Comments | String |Preceded by "!", QY  = quantum yields averaged over UCI Solar Flux|
| 3+NPHOTAB+NTEMP+4 | A | Comments | String |Preceded by "!", EQY = eCS\*eQY/CS averaged over Solar Flux and 77 bins in UCI Model|
| 3+NPHOTAB+NTEMP+5 | A | Comments | String |Preceded by "!", header for reactions, CS or EQY, and Wavelength Bins|
| 3+NPHOTAB+NTEMP+6| A |Reaction Name | String |Reaction Name|
|| B | Quantity | String |CS |
|| C | TEMP| Real | Temperature|
|| D | WBIN (1) | Real or EXP | Absorption Cross Section (CS)  Value for BIN 1|
|| E | WBIN (2)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 2|
|| F | WBIN (3)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 3|
|| G | WBIN (4)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 4|
|| H | WBIN (5)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 5|
|| I | WBIN (6)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 6|
|| J | WBIN (7)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 7|
| 3+NPHOTAB+NTEMP+7 | A |Reaction Name | String |Reaction Name|
|| B | Quantity | String |EQY |
|| C | TEMP| Real | Temperature|
|| D | WBIN (1) | Real or EXP | Absorption Cross Section (EQY)  Value for BIN 1|
|| E | WBIN (2)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 2|
|| F | WBIN (3)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 3|
|| G | WBIN (4)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 4|
|| H | WBIN (5)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 5|
|| I | WBIN (6)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 6|
|| J | WBIN (7)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 7|
|<repeat for 6 temperature bands and for each of the 26 reactions)|
| n-11 | A | Comments | String |Preceded by "!", Ozone Cross-Section for Stratospheric Opacity based on Table 4-5 in|
| n-10 | A | Comments | String |Chemical Kinetics and Photochemical Data for Use in Atmospheric|
| n-9 | A | Comments | String |Studies Evaluation Number 15. Sander et. al: NASA-JPL 2006|
| n-8 | A |NTEMP_STRAT | Int |Number of Temperature Bins in Stratosphere|
| n-7 | A | Comments | String |Preceded by "!", Header|
| n-6 | A |Reaction Name | String |Stratospheric Reaction Name|
|| B | Quantity | String |CS |
|| C | TEMP| Real | Temperature|
|| D | WBIN (1) | Real or EXP | Absorption Cross Section (CS)  Value for BIN 1|
|| E | WBIN (2)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 2|
|| F | WBIN (3)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 3|
|| G | WBIN (4)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 4|
|| H | WBIN (5)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 5|
|| I | WBIN (6)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 6|
|| J | WBIN (7)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 7|

An example of the CSQY table is shown below.

```Tcsh
Table for Mechanism = CB05E51_AE6_AQ
NPHOTAB =   26
!Individual rates listed below:
NO2_IUPAC10
O3_O3P_IUPAC10
repeat for each photolysis reaction
...
...
NTEMP =    6
! I   TEMP( I ) K
  1  210.00
  2  230.00
  3  250.00
  4  270.00
  5  290.00
  6  310.00
! Wavelength and Photolysis Rate Parameters for CMAQ In-Line Photolysis
! calculation . The values are determined by averaging technique over the below
! number of wavelength bins. The technique employs an approach used by the FASTJX
! versions 6 and 7.
!References:
! 1) Bian, H. S. and Prather, M. J.: Fast-J2: accurate simulation of stratospheric
!     photolysis in global chemical models, J. Atmos. Chem., 41, 281-296
     doi:10.1023/A:1014980619462, 2002.
! 2) Hsu, J. and Prather, M. J.: Stratospheric variability and tropospheric ozone,
!    J. Geophys. Res., 114, D06102, doi:10.1029/2008JD010942, 2009.
! 3) Prather, M. J.: Fast-JX versions and utilities, available at:
!    http://www.ess.uci.edu/researchgrp/prather/scholar_software/fast-jx
!    (last accessed: 24 January 2014), 2014.
! Wave bands and Solar Flux
!
N_INLINE_BAND =    7
! I,      START_WL_BIN(nm),    EFFECT_WL_BIN_(nm),       END_WL_BIN_(nm), photon_flux(cm-2*s-1),
  1,               291.000,               294.590,               298.300,            5.8828E+14,
  2,               298.300,               303.151,               307.500,            7.6354E+14,
  3,               307.500,               310.007,               312.500,            5.0412E+14,
  4,               312.500,               316.434,               320.300,            8.9628E+14,
  5,               320.300,               333.076,               345.000,            3.8537E+15,
  6,               345.000,               381.997,               412.500,            1.5457E+16,
  7,               412.500,               607.723,               850.000,            2.1307E+17,
!...  CS  = absorption cross sections averaged over UCI Solar Flux
!...  QY  = quantum yields averaged over UCI Solar Flux
!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model
!PHOTAB       QUANTITY   TEMP   WBIN(  1)     WBIN(  2)     WBIN(  3)     WBIN(  4)     WBIN(  5)     WBIN(  6)     WBIN(  7)
NO2_IUPAC10         CS  210.000 9.420958E-20  1.354622E-19  1.774391E-19  2.174503E-19  3.547179E-19  5.767915E-19  3.786790E-21
NO2_IUPAC10        EQY  210.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  7.963594E-01  4.393946E-02
NO2_IUPAC10         CS  230.000 9.434513E-20  1.353973E-19  1.770686E-19  2.162921E-19  3.512878E-19  5.752929E-19  3.777024E-21
NO2_IUPAC10        EQY  230.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  7.963594E-01  4.393946E-02
NO2_IUPAC10         CS  250.000 9.461621E-20  1.352675E-19  1.763276E-19  2.139758E-19  3.444275E-19  5.722957E-19  3.757493E-21
NO2_IUPAC10        EQY  250.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  7.971660E-01  4.545939E-02
NO2_IUPAC10         CS  270.000 9.488729E-20  1.351377E-19  1.755867E-19  2.116595E-19  3.375671E-19  5.692984E-19  3.737961E-21
NO2_IUPAC10        EQY  270.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  8.052323E-01  6.065865E-02
NO2_IUPAC10         CS  290.000 9.515838E-20  1.350079E-19  1.748457E-19  2.093432E-19  3.307068E-19  5.663012E-19  3.718430E-21
NO2_IUPAC10        EQY  290.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  8.132986E-01  7.585791E-02
NO2_IUPAC10         CS  310.000 9.521259E-20  1.349819E-19  1.746975E-19  2.088799E-19  3.293347E-19  5.657017E-19  3.714523E-21
NO2_IUPAC10        EQY  310.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  8.165251E-01  8.193760E-02
repeat (6 temperature bands  x 2 (CS or EQY) lines for each of the 26 reactions:
...
...
after last reaction:
! Ozone Cross-Section for Stratospheric Opacity based on Table 4-5 in
! Chemical Kinetics and Photochemical Data for Use in Atmospheric
! Studies Evaluation Number 15. Sander et. al: NASA-JPL 2006
NTEMP_STRAT =    6
!             QUANTITY   TEMP   WBIN(  1)     WBIN(  2)     WBIN(  3)     WBIN(  4)     WBIN(  5)     WBIN(  6)     WBIN(  7)
O3_STRAT            CS  180.000 6.113743E-19  2.263004E-19  8.354117E-20  3.417408E-20  3.688351E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  260.000 6.434133E-19  2.434104E-19  9.254548E-20  3.928807E-20  4.583227E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  300.000 6.685868E-19  2.568540E-19  9.962031E-20  4.330621E-20  5.286344E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  340.000 6.685868E-19  2.568540E-19  9.962031E-20  4.330621E-20  5.286344E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  380.000 6.685868E-19  2.568540E-19  9.962031E-20  4.330621E-20  5.286344E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  420.000 6.685868E-19  2.568540E-19  9.962031E-20  4.330621E-20  5.286344E-21  4.086261E-23  1.663175E-21
```

See this [link](../../src/MECHS/cb05e51_ae6_aq/CSQY_DATA_cb05e51_ae6_aq) for an complete CSQY file for the cb05e51_ae6_aq mechanism.

<a id=et></a>
### ET: Extraterrestrial irradiance

Used by: JPROC

ET is the logical name for the ASCII data file containing extraterrestrial radiation as a function of wavelength. The extraterrestrial irradiance file has a format similar to that of the CSQY file ([CSQY Section](#csqy)). The file begins with a header section; comment lines are preceded with a “!”. Like the CSQY file, the header contains a field describing the location on the wavelength interval that the data represent, and a multiplier. The data section uses a space-delimited, free-form format and lists the wavelength of the incoming solar radiation (nm) and the irradiance (photons cm<sup>-2</sup> s<sup>-1</sup>) at each wavelength, with each row corresponding to a specific wavelength interval. A detailed description of the file format is provided in [Table 8-8](Table8-8).

<a id=Table8-8></a>

**Table 8-8 ET file format description**

| **Line** | **Column** | **Name** | **Type** | **Description**|
|-----|-----|-------------|------------|-----------------------------------------------------|
| 1 | A | Comments | String |Preceded by "!", comment lines describe the file contents and document the source of the data (optional)|
| n | ...  | ...  | ...  | ...  |
| n + 1 | A | Data Location | String |Field indicating the location of the data as measured across the wavelength band; possible answers: beginning, ending, centered, point (required)|
| n + 2 | A | Multiplier | String |Multiplication factor to apply to photolysis rate equation; line begins with FAC=; factor listed in real or exponential format (required)|
| n+3 | A | Wavelength | Int or Real |Wavelength corresponding to ET data; units = nm (required)|
|| B | Extra–terrestrial Irradiance | Real or Exp |Estimation of the photon flux reaching the exterior of the earth’s atmosphere; units = photons cm<sup>-2</sup> second<sup>-1</sup> (required)|
| n+4 | A | Wavelength | Int |Wavelength corresponding to ET data; units = nm (required)|
|| B | Extra–terrestrial Irradiance | Real or Exp |Estimation of the photon flux reaching the exterior of the earth’s atmosphere; units = photons cm<sup>-2</sup> second<sup>-1</sup> (required)|
| n+X | ...  | ...  | ...  | ...  |

See this [link](../../../UTIL/inline_phot_preproc/photolysis_CSQY_data/ETirradiance.dat) for an example ET file.
A sample of the important sections of an ET file is shown below.

```
 ! Extraterrestrial Irradiance
 ! Taken from the RADM data---derived from the WMO 1985 report Table 7-4
 ! Format: wl, et_irradBeginning
 ! With FAC, units are (photons cm-2 s-1)
 FAC=1.0
 185.185 3.620E+11
 186.916 4.730E+11
```

<a id=profiles></a>
### PROFILES: Atmospheric vertical profiles

Used by: JPROC

PROFILES is the logical name for the ASCII data file containing seasonal and vertical profiles for ozone, aerosol attenuation, temperature, air pressure, and Dobson values. The ASCII-formatted data provided in the PROFILES file are at 19 latitudes (90°N to 90°S) and 51 altitudes (0 to 50 km) in three distinct data sections. The first data section contains seasonal, latitude-dependent vertical profiles of O<sub>3</sub> concentrations (molecules cm<sup>-3</sup>), air temperature (K), and air density (molecules cm<sup>-3</sup>). The second data section contains monthly Dobson values at the 19 latitude bands. The last data section contains vertical profiles from the 1976 U.S. Standard Atmosphere of air temperature (K), air density (molecules cm<sup>-3</sup>), ozone concentrations (molecules cm<sup>-3</sup>), and aerosol attenuation coefficients (km<sup>-1</sup>).

The first data section of the PROFILES file is divided into 228 (19x3x4) data blocks, with each block representing one of the three variables (O<sub>3</sub>, air temperature, and air density) at one of the 19 latitude bands, for each of the 4 seasons of the year. The individual data blocks contain 51 values per variable, representing standard conditions at altitudes ranging from 0 to 50 km. The data are ordered, from general to specific, by season (spring, summer, autumn, winter), variable (O<sub>3</sub>, air temperature, air density), latitude, and altitude. For example, the first block in the PROFILES file contains spring O<sub>3</sub> concentration profiles at the latitude band ranging from 90°N to 80°N from 0 to 50 km above sea level; the first value in the block is at 0 km and the last value is at 50 km. The next data block is again spring O<sub>3</sub> concentration profiles but at the latitude band ranging from 80°N to 70°N. The next 17 data blocks complete the spring O<sub>3</sub> concentration profiles by continuing through the rest of the 19 latitude bands, with the last block representing the 80°S to 90°S latitude band. The 20<sup>th</sup> data block begins the spring air temperature profiles at the latitude band ranging from 90°N to 80°N and is followed by 18 more data blocks of spring air temperature profiles. The following 19 data blocks follow an identical format for air density and round out the spring profiles. The 19x3 data blocks are then repeated for summer profiles, autumn profiles, and finally winter profiles.

The second data section in the PROFILES file contains monthly average Dobson values. The data are organized in 12 rows (January through December) and 19 columns (90°N to 80°N through 80°S to 90°S).

The last data section in the PROFILES file contains vertical profiles from the 1976 U.S. Standard Atmosphere of temperature, air density, ozone concentrations, and aerosol attenuation coefficients. The data are organized in 51 rows, corresponding to altitude (0 to 50 km), with four columns per row for each of the four variables. See this [link](../../../UTIL/inline_phot_preproc/photolysis_CSQY_data/PROFILES.dat) for an example PROFILES file. A detailed description of the file format is provided in [Table 8-9](#Table8-9).

<a id=Table8-9></a>

 **Table 8-9. PROFILES file format description.**

| **Line** | **Column**| **Name**|**Type**|**Description**|
|---------|-------|-------------------------------------|----------|----------------------------------------|
| 1 | A | Ozone concentration at Season 1, Latitude 1, Level 1 | Exp (E10.3) |Ozone measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|| B | Ozone concentration at Season 1, Latitude 1, Level 2 | Exp (E10.3) |Ozone measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
| ...  | ...  | ...  | ...  |...|
| 127 | A | Ozone concentration at Season 1, Latitude 19, Level 1 | Exp (E10.3) |Ozone measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|| B | Ozone concentration at Season 1, Latitude 19, Level 2 | Exp (E10.3) |Ozone measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
| ...  | ...  | ...  | ...  |...|
| 134 | A | Temperature profiles at Season 1, Latitude 1, Level 1 | Exp (E10.3) |Temperature measurements as a function of season, latitude, and vertical level; units = K (required)|
|| B | Temperature profiles at Season 1, Latitude 1, Level 2 | Exp (E10.3) |Temperature measurements as a function of season, latitude, and vertical level; units = K (required)|
| ...  | ...  | ...  | ...  |...|
| 260 | A | Temperature profiles at Season 1, Latitude 19, Level 1 | Exp (E10.3) |Temperature measurements as a function of season, latitude, and vertical level; units = K (required)|
|| B | Temperature profiles at Season 1, Latitude 19, Level 2 | Exp (E10.3) |Temperature measurements as a function of season, latitude, and vertical level; units = K (required)|
| ...  | ...  | ...  | ...  |...|
| 267 | A | Air density profiles at Season 1, Latitude 1, Level 1 | Exp (E10.3) |Air density measurements as a function of month, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|| B | Air density profiles at Season 1, Latitude 1, Level 2 | Exp (E10.3) |Air density measurements as a function of month, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
| ...  | ...  | ...  | ...  |...|
| 393 | A | Air density profiles at Season 1, Latitude 19, Level 1 | Exp (E10.3) |Air density measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|| B | Air density profiles at Season 1, Latitude 19, Level 2 | Exp (E10.3) |Air density measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
| ...  | ...  | ...  | ...  |...|
| 1596 | A | Air density profiles at Season 4, Latitude 19, Level 51 | Exp (E10.3) |Air density measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
| 1597 | A | Average Dobson Values at Latitude 1, Month 1 | Real |Average Dobson value as a function of latitude and month (required)|
| 1597 | B | Average Dobson Values at Latitude 2, Month 1 | Real |Average Dobson value as a function of latitude and month (required)|
| ...  | ...  | ...  | ...  |...|
| 1608 | A | Average Dobson Values at Latitude 19, Month 12 | Real |Average Dobson value as a function of latitude and month (required)|
| 1609 | A | Air Temperature at Level 1 | Real |Air temperature for a standard atmospheric profile; units = K (required)|
|| B | Air Density at Level 1 | Real or Exp |Air Density for a standard atmospheric profile; units = molecules cm<sup>-3</sup>(required)|
|| C | Ozone Concentration at Level 1 | Real or Exp |Ozone concentration for a standard atmospheric profile; units = molecules cm<sup>-3</sup>(required)|
|| D | Aerosol Attenuation at Level 1 | Real or Exp |Aerosol attenuation coefficient for a standard atmospheric profile; units = km<sup>-1</sup>(required)|
| 1659 | A | Air Temperature at Level 51 | Real |Air temperature for a standard atmospheric profile, units = K (required)|
|| B | Air Pressure at Level 51 | Real or Exp |Air pressure for a standard atmospheric profile, units = molecules cm<sup>-3</sup>(required)|
|| C | Ozone Concentration at Level 51 | Real or Exp |Ozone concentration for a standard atmospheric profile, units = molecules cm<sup>-3</sup>(required)|
|| D | Aerosol Attenuation at Level 51 | Real or Exp |Aerosol attenuation coefficient for a standard atmospheric profile; units = km<sup>-1</sup>(required)|

<a id=toms></a>
### TOMS: Total ozone mapping spectrometer data

Used by: JPROC

TOMS is the logical name for the ASCII data file containing total ozone column measurements in Dobson units taken by the Total Ozone Mapping Spectrometer instrument aboard the sun-synchronous polar orbiting Nimbus satellite. The data are presented for specific Julian dates as a function of location on the earth (latitude and longitude).

A detailed description of the file format is provided in [Table 8-10](#Table8-10). The files are fixed format.

<a id=Table8-10></a>

 ** Table 8-10 TOMS Data Profile **

|**Line** | **Column**| **Name** | **Type**| **Description** |
|-----|--------|-------|-------|--------------------------------------------------------------------|
| 1 | A | Julian Day | Int |Julian start day of the file, DDD; preceded by 6 blank spaces (required)|
|| B | Year | Int |Start year of the file, YYYY; preceded by 9 blank spaces (required)|
| 2 || Header | String |80-character line describing the contents of the file (if omitted, needs line placeholder)|
| 3 || Header | String |80-character line describing the contents of the file (if omitted, needs line placeholder)|
| 4 | A | TOMS Data | Int |TOMS ozone measurements as a function of longitude and latitude, ###; line starts with a space, then space-delimited 25 values per line (required)|
| ...  | ...  | ...  | ...  | ...  |

<a id=o2abs></a>
### O2ABS: Molecular oxygen absorption cross-section data

Used by: JPROC

O2ABS is the logical name for the ASCII data file containing absorption cross section and quantum yield data for O<sub>2</sub> photolysis. The data in this file are listed as a function of wavelength. This file follows the same format as the CSQY files described in this [section](#csqy).

<a id=o3abs></a>
### O3ABS: Ozone absorption cross-section data

Used by: JPROC

O3ABS is the logical name for the ASCII data file containing absorption cross section and quantum yield data for O<sub>3</sub> photolysis. The data in this file are listed as a function of wavelength. For an example see this [link](../../../UTIL/inline_phot_preproc/photolysis_CSQY_data/O3O1D_JPL06-2) This file follows the same format as the CSQY files described in this [section](#csqy).

<a id=inmetfiles></a>
### InMetFiles: List of MM5 or WRF-ARW output files

Used by: MCIP

MCIP can read MM5 (version 3) binary output files (MMOUT) and WRF-ARW netCDF-based files to generate I/O API-formatted netCDF files for input to CMAQ and emissions modeling. For details about the format of the MMOUT files, visit the [MM5 homepage](http://www.mmm.ucar.edu/mm5). For information about the format of the WRF output files, visit the [WRF-ARW homepage](http://www.mmm.ucar.edu/wrf/users).

<a id=interfile></a>
### InTerFile: Terrain file

Used by: MCIP

MM5 or WRF output file  containing fractional land use data. This file is generated by the MM5 program TERRAIN and the WRF program GEOGRID.

<a id=insatfiles></a>
### InSatFiles: GOES cloud data file

Used by: MCIP

[EPA: need a description of this file]

<a id=bndy_conc_1></a>
### BNDY_CONC_1: Boundary conditions

Used by: CCTM

CMAQ boundary condition data are of the BNDARY3 file type. Produced by the boundary condition processor, BCON, CCTM reads these data and correlates them with the interior data by the use of a pointer system. This pointer system designates the beginning location of the data in memory that start a new side of the domain (i.e., south, east, north, or west). [Figure 8-1](#Figure8-1) illustrates this input data structure and the relationship of the boundary data to the interior (“main grid”) data within CMAQ modules.

Each species being modeled should be in the BNDY_CONC_1 file. If some modeled species are not contained in this file, the boundary condition for these species will default to the value 1 × 10<sup>-30</sup>. The perimeter of the CMAQ domain is 1 cell wide, where the number of boundary cells = (2*NROW)+(2*NCOL)+4. [Figure 8-2](#Figure8-2) is a graphical example of the CMAQ boundary conditions file; the west and north boundaries have ozone values of 0.035 ppmV, while the east and south boundaries have values of 0.030 ppmV.

``![](./images/Figure8-1.png "Figure8-1.png")``

**Figure 8-1. Illustration of CMAQ boundary condition file**

``![](./images/Figure8-2.png "Figure8-2.png")``

**Figure 8-2. Graphical example of a CMAQ gridded boundary conditions file**

<a id=init_conc_1></a>
### INIT_CONC_1: Initial conditions

Used by: CCTM

The initial concentrations of each species being modeled must be input to CMAQ. The initial conditions input file type is GRDDED3 and does not vary with time. The file data are looped in this manner: by column, by row, by layer, by variable. Initial conditions files have the same structure as concentration files, so the predicted concentrations from the last hour of day 1 can be used to initialize the following day’s simulation. This gives CMAQ users the flexibility to segment simulations in any way they choose. [Figure 8-3](#Figure8-3) is a graphical example of the CMAQ initial conditions file. The file shows spatially varying data that can be used to initialize a following run beginning at the time shown (i.e., June 25, 1996 0:00:00).

<Image:>

**Figure 8-3. Graphical example of a CMAQ gridded initial conditions file**


<a id=jtable></a>
### JTABLE: Photolysis rates look-up table

Used by: CCTM

Each of the gas-phase mechanisms in CMAQ contains photolysis reactions that require clear-sky reaction rates precomputed from kinetics data at various altitudes and latitude bands. The CMAQ program JPROC (Section 2.2.3) generates photolysis rate look-up tables for input to CMAQ. The photolysis files, called JTABLE, contain a header section that describes the contents of the file, and a data section with clear-sky photolysis rates at different times of the day.

The first line of the header contains the Julian date of the data in the file. This date corresponds to the start date for the simulation that uses the JTABLE data. This is followed by four pairs of data that describe the altitude (m), latitude (degrees), hour angle (from noon), and photolytic reaction name for the data contained in the file. Each data pair begins with a line describing the number of values for each variable and the variable name, followed by the values for each variable. The altitude (variable = LEVELS), latitude (variable = LATITUDES), and hour angle (variable = HOUR ANGLES) variables have fixed values that are hard-coded in the program JPROC (routine jproc.F). The reaction names (variable = PHOTOLYTIC REACTIONS) are mechanism-dependent and vary with the choice of gas-phase mechanism.

The data section of the file contains data blocks that are mapped to the header using a three-digit code. Each block corresponds to an altitude, latitude, and photolysis reaction and contains nine values of clear-sky photolysis rates for the nine hour angles listed in the header. The three-digit code maps the altitude/latitude/reaction number to the data block. For example, the data block that uses the code “3 1 2” corresponds to altitude 3, latitude 1, and reaction 2 as listed in the file header. A detailed description of the JTABLE file format is provided in [Table 8-11](Table8-11). The files are list-formatted.

<a id=Table8-11></a>

**Table 8-11. JTABLE file format description**

| **Line** | **Column** |**Name** | **Type** | **Description** |
|----|--------|-------|--------|------------------------------------------------|
|1|A|JVDATE|String|Julian date of the file; YYYYDDD (required)|
||B|Comment|String|Description of the Julian date field (optional)|
|2|A|JVHT|Int|Number of vertical levels covered by the data (required)|
||B|Level Field Indicator|String|The word “LEVELS” (required)|
| |C|Comment|String|Description of the level field; usually “(m)”, for meters (optional)|
|3|A|XZJV_1|Real|Height of level 1; units in m (required)|
||B|XZJV_2|Real|Height of level 2; units in m (required)|
|...|...|...|...|...|
|...|x|XZJV_x|Real|Height of level x; units in m (required)|
|4|A|JVLAT|Int|Number of latitudes covered by the data (required)|
||B|Latitude Field Indicator|String|The word “LATITUDES” (required)|
| |C|Comment|String|Description of the latitudes field; usually “(deg)”, for degrees (optional)|
|5|A|XLATJV_1|Real|Latitude 1; units in degrees (required)|
||B|XLATJV_2|Real|Latitude 2; units in degrees (required)|
|...|...|...|...|...|
|...|x|XLATJV_x|Real|Latitude x; units in degrees (required)|
|6|A|JVTMAX|Int|Number of hour angles covered by the data (required)|
||B|Hour Angle Field Indicator|String|The words “HOUR ANGLES” (required)|
| |C|Comment|String|Description of the hour angles field; usually “(from noon)”, for hours from noon (optional)|
|7|A|XHAJV_1|Real|Hour angle 1; units in hours from noon (required)|
||B|XHAJV_2|Real|Hour angle 2; units in hours from noon (required)|
|...|...|...|...|...|
|...|x|XHAJV_x|Real|Hour angle x; units in hours from noon (required)|
|8|A|JPHOT|Int|Number of photolytic reactions covered by the data (required)|
||B|Reaction Field Indicator|String|The words “PHOTOLYTIC REACTIONS” (required)|
| |C|Comment|String|Description of the reactions field (optional)|
|9|A|PHOT_NM_1|String|Single quote-delimited name of photolysis reaction 1 (required)|
||B|Delimiter|Char|Comma delimiter separating reaction name from multiplier (required)|
||C|ACLD_1|Real|Multiplier for reaction 1 (required)|
|10|A|PHOT_NM_2|String|Single quote-delimited name of photolysis reaction 2 (required)|
||B|Delimiter|Char|Comma delimiter separating reaction name from multiplier (required)|
||C|ACLD_2|Real|Multiplier for reaction 2 (required)|
|...|...|...|...|...|
|x|A|PHOT_NM_x|String|Single quote-delimited name of photolysis reaction x (required)|
||B|Delimiter|Char|Comma delimiter separating reaction name from multiplier (required)|
||C|ACLD_x|Real|Multiplier for reaction x (required)|
|x + 1|A|NHTO|Int|Vertical level cross-reference to header data (required)|
| |B|NLATO|Int|Latitude cross-reference to header data (required)|
| |C|NPHOTO|Int|Photolysis reaction cross-reference to header data (required)|
|x+2|A|XJVAL_1|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 1 (required)|
| |B|XJVAL_2|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 2 (required)|
| |C|XJVAL_3|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 3 (required)|
|...|...|...|...|...|
||JVTMAX|XJVAL_(JVTMAX)|Real or Exp|Clear sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle corresponding to JVTMAX (required)|
|x + 3|A|NHTO|Int|Vertical level cross-reference to header data (required)|
| |B|NLATO|Int|Latitude cross-reference to header data (required)|
| |C|NPHOTO|Int|Photolysis reaction cross-reference to header data (required)|
|x+4|A|XJVAL_1|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 1 (required)|
| |B|XJVAL_2|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 2 (required)|
| |C|XJVAL_3|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 3 (required)|
|...|...|...|...|...|
||JVTMAX|XJVAL_(JVTMAX)|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle corresponding to JVTMAX (required)|
|...|...|...|...|...|

A sample of the important sections of a JTABLE file is shown below.

` 1999181 (yyyyddd) Julian Date for the file`
` 7 LEVELS (m)`
` 0.0 1000.0 2000.0 3000.0 4000.0 5000.0 10000.0`
` 6 LATITUDES (deg)`
` 10.0 20.0 30.0 40.0 50.0 60.0`
` 9 HOUR ANGLES (from noon)`
` 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0`
` 6 PHOTOLYTIC REACTIONS`
` 'NO2_CBIV88 ', 1.0`
` 'O3O1D_CBIV88 ', 1.0`
` 'HCHOmol_CBIV88 ', 1.0`
` 'HCHOrad_CBIV88 ', 1.0`
` 'ALD_CBIV88 ', 1.0`
` 'ACROLEIN ', 1.0`
` 1 1 1`
` 5.0964761E-01 4.9923715E-01 4.6422747E-01 4.0129572E-01 3.0394882E-01`
` 1.6590215E-01 3.2829735E-02 0.0000000E+00 0.0000000E+00`

<a id=omi></a>
### OMI: Ozone Monitoring Instrument Column Data

Used by: CCTM

OMI ozone column data by latitude and longitude for use in the inline photolysis calculations. CMAQ is distributed with ozone columns from 1978 to 2015. The data are 22.5°x10° gridded ozone columns in Dobson units. [Table 8-12](#Table8-12) lists the format of the OMI data file.

<a id=Table8-12></a>

**Table 8-12. OMI data format**

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

<a id=emis_1></a>
### EMIS_1: Emissions

Used by: CCTM

CMAQ can accept emissions inputs from a variety of emissions models and preprocessors. The most commonly used option is the Sparse Matrix Operator Kernel Emissions (SMOKE) modeling system, which is a collection of programs that separately process and merge emissions data for each emissions sector for input to air quality models.

The emissions file sorts the emitted gas-phase and aerosol species by grid cell and time. The file type is GRDDED3, and the units are in moles per second (moles s<sup>-1</sup>) for gas-phase species and grams per second (g s<sup>-1</sup>) for aerosol species. The file data are looped as follows: by column, by row, by layer, by variable, and by input time step. CMAQ does not artificially distinguish between surface and elevated emissions sources; elevated sources are provided to CMAQ as vertically resolved emissions. For CCTM configurations that do not use in-line emissions calculations, all emissions estimates are contained within a single input emission file for each day. In v4.7, CMAQ now has the capability to process point-source, sea salt, and biogenic emissions in-line. The supplemental input files to use for calculating the in-line emissions are described in the CMAQv4.7 release notes.

<a id=ocean_1></a>
### OCEAN_1: Sea salt mask

Used by: CCTM

The CMAQ aerosol models AERO5 and AERO6 can compute sea salt emissions from both open ocean grid cells and surf zone grid cells. The addition of the surf zone option simulates the elevated emissions rates of sea salt in coastal regions where wave action occurs. The OCEAN_1 file contains data on the fraction of each grid cell that is either open ocean (OPEN) or in the surf zone (SURF). When CCTM is compiled with AERO5 or AERO6, it will expect the OCEAN_1 file as input.

<a id=gspro></a>
### GSPRO: Speciation profiles

Used by: CCTM – inline emissions version only

The speciation profile file, GSPRO, contains the factors that are used to separate aggregated inventory pollutant emissions totals into emissions of model species in the form required by CMAQ. If only biogenic emissions are being calculated in-line in CMAQ, the GSPRO file used by CCTM needs to contain split factors only for the biogenic VOC emissions that are input in the B3GRD file. If other emissions sources are being calculated by CCTM, VOC split factors for these other sources must be included in the GSPRO file. The GSPRO file format is listed in the SMOKE user’s manual, see: [GSPRO documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch08s05s02.html).

<a id=b3grd></a>
### B3GRD: Gridded, normalized biogenic emissions

Used by: CCTM – inline-emissions version only

An I/O API GRDDED3 file of gridded, normalized biogenic emissions (in grams of carbon or nitrogen per hour, depending on the species) and leaf area index. The B3GRD file contains normalized emissions calculated with both summer and winter emissions factors. The B3GRD file is generated with the SMOKE program NORMBEIS3 using gridded land use data. For additional information about creating the B3GRD file, see the [NORMBEIS3 documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch06s12.html) in the SMOKE users’ manual.

<a id=bioseason></a>
### BIOSEASON: Freeze dates

Used by: CCTM – inline-emissions version only

The BIOSEASON switch file is an I/O API GRDDED3 file used to indicate which biogenic emissions factor to use on each day in a given year for every grid cell in the modeling domain. This file can be created using the Metscan utility program that is distributed with SMOKE. The BIOSEASON file is time-dependent and usually contains data for an entire year (365 or 366 days). It uses one variable, SEASON, which is either 0 (grid cell should use winter factors for current day) or 1 (grid cell should use summer factors for current day). For additional information about creating the BIOSEASON file, see the [Metscan documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch05s03s10.html) in the SMOKE user’s manual.

<a id=stk_grps></a>
### STK_GRPS_nn: Stack groups

Used by: CCTM – in-line emissions version only

The ## mark is unique and represents the sector identification.

The stack groups file is an I/O API netCDF file containing stack parameters for elevated sources. This file can be created using the SMOKE program ELEVPOINT. For additional information about creating the stack groups file, see the [ELEVPOINT documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch06s03.html) in the SMOKE user’s manual

<a id=stk_emis></a>
### STK_EMIS_nn: Point source emissions

Used by: CCTM – inline emissions version only

The ## mark is unique and represents the sector identification.

The elevated-point-source emissions file is an I/O API GRDDED3 file with emissions for point sources to be treated as elevated sources by CCTM. The emissions in this file are distributed through the vertical model layers using a plume-rise algorithm contained in CCTM. The elevated-point-source emissions file can be creating using SMOKE. For additional information about preparing point-source emissions for using the CMAQ in-line plume rise calculation, see the [ELEVPOINT documentation](https://www.cmascenter.org/smoke/documentation/4.0/html/ch06s03.html) in the SMOKE user’s manual.

<a id=dust_lu_1></a>
### DUST_LU_1: Gridded land cover/land use

Used by: CCTM – in-line dust emission version only

The gridded land cover/land use (LCLU) file is an I/O API GRDDED3 file of BELD3 data projected to the modeling domain. This file must contain the following LCLU variables to be compatible with the CMAQ dust module:

-   USGS_urban
-   USGS_drycrop
-   USGS_irrcrop
-   USGS_cropgrass
-   USGS_cropwdlnd
-   USGS_grassland
-   USGS_shrubland
-   USGS_shrubgrass
-   USGS_savanna
-   USGS_decidforest
-   USGS_evbrdleaf
-   USGS_coniferfor
-   USGS_mxforest
-   USGS_water
-   USGS_wetwoods
-   USGS_sprsbarren
-   USGS_woodtundr
-   USGS_mxtundra
-   USGS_snowice

These categories are used to determine dust source locations and canopy scavenging factors for estimating dust emission in the model. This file can be created for North America using the Spatial Allocator and BELD4 tiles. The DUST_LU_1 file corresponds to the “a” output file from the Spatial Allocator. See the chapter on [creating biogenic inputs to SMOKE](https://www.cmascenter.org/sa-tools/documentation/4.2/html/raster/Raster_Users_Guide_4_2.htm#_Toc389118706) of the Spatial Allocator User’s Guide for details.

<a id=dust_lu_2></a>
### DUST_LU_2: Gridded land cover/land use

Used by: CCTM – in-line dust emission version only

The gridded land cover/land use (LCLU) file is an I/O API GRDDED3 file of BELD3 data projected to the modeling domain. This file must contain the following variables to be compatible with the CMAQ dust module:

-   FOREST

This variable is used in combination with the variables in the DUST_LU_1 file to determine canopy scavenging factors for estimating dust emission in the model. This file can be created for North America using the Spatial Allocator and BELD3 tiles. The DUST_LU_2 file corresponds to the “tot” output file from the Spatial Allocator. See the chapter on [creating biogenic inputs to SMOKE](https://www.cmascenter.org/sa-tools/documentation/4.2/html/raster/Raster_Users_Guide_4_2.htm#_Toc389118706) of the Spatial Allocator User’s Guide for details

<a id=modis_fpar></a>
### MODIS_FPAR: MODIS vegetation coverage

Used by: CCTM – in-line dust emission version only

MODIS_FPAR file is an I/O API file with 2-d (row x col) daily values of Fraction of Photosynthetically Active Radiation (FPAR) from MODIS instrument interpolated to the modeling domain. It is required to obtain a dynamic vegetation fraction used by the in-line windblown dust emission module.

<a id=cropmap01></a>
### CROPMAP01: Gridded planting start dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29) – in-line dust emission version with crops only

The gridded planting start dates file is an I/O API GRDDED3 file of planting start dates for various crops interpolated to the modeling domain. The variables in this file are planting start dates for different crop types, where each variable is an integer representing the number of days after January 1 that planting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor) reads a crop activity calendar and a [GRID_CRO_2D](#grid_cro_2d) file to generate the CROPMAP08 file.

<a id=cropmap04></a>
### CROPMAP04: Gridded planting end dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29) – in-line dust emission version with crops only

The gridded planting end dates file is an I/O API GRDDED3 file of planting end dates for various crops interpolated to the modeling domain. The variables in this file are planting end dates for different crop types, where each variable is an integer representing the number of days after January 1 that planting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor) reads a crop activity calendar and a [GRID_CRO_2D](#grid_cro_2d) file to generate the CROPMAP08 file.

<a id=cropmap08></a>
### CROPMAP08: Gridded harvesting end dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29) – in-line dust emission version with crops only

The gridded harvesting end dates file is an I/O API GRDDED3 file of harvesting end dates for various crops interpolated to the modeling domain. The variables in this file are harvesting end dates for different crop types, where each variable is an integer representing the number of days after January 1 that harvesting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor) reads a crop activity calendar and a [GRID_CRO_2D](#grid_cro_2d) file to generate the CROPMAP08 file.

<a id=ltngno></a>
### LTNGNO: Lightning NOx emissions

Used by: CCTM – lightning NO<sub>x</sub> version only

The lightning NO<sub>x</sub> emissions file is an I/O API GRDDED3 file with 3-d (row x col x layer) hourly NO emissions (moles/s) interpolated to the modeling domain. This is a lightning NO emissions file calculated off-line for input to CMAQ.

<a id=ltngparm_file></a>
### LTNGPARMS_FILE: Lightning parameters file

Used by: CCTM – lightning NO<sub>x</sub> version only

The lightning parameters file is used for calculating in-line NO emissions from hourly observed strike counts. This file contains the following variables interpolated to the modeling grid:

-   SLOPE (unitless): linear equation parameter for estimating NO emissions from hourly flash counts
-   INTERCEPT: linear equation parameter for estimating NO emissions from hourly flash counts
-   SLOPE_lg: logarithmic equation parameter for estimating NO emissions from hourly flash counts
-   INTERCEPT_lg: logarithmic equation parameter for estimating NO emissions from hourly flash counts
-   ICCG_SUM (unitless): Ratio of intercloud to cloud-to-ground flashes during the summer season
-   ICCG_WIN (unitless): Ratio of intercloud to cloud-to-ground flashes during the winter season
-   OCNMASK (unitless): Land/water mask to remove spurious flashes over the ocean.

<a id=nldn_strikes></a>
### NLDN_STRIKES: Hourly observed lightning strikes

Used by: CCTM – lightning NO<sub>x</sub> version only

The NLDN lightning strikes file is used for calculating in-line NO emissions from hourly observed strike counts. This file contains the following variables interpolated to the modeling grid:

-   NLDNstrk (km-2): hourly flash counts per sq. km.

<a id=beld4_lu></a>
### BELD4_LU – Fractional crop distributions

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

Add content

<a id="e2c_soil"></a>
### E2C_SOIL – EPIC soil properties

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

This 3-D  file is created by the EPIC model via the FEST-C interface and contains soil properties for Layer 1 (0 to 1 cm depth) and Layer 2 (1 cm to 100 cm depth) for each crop and soil combination in each grid cell.  Additional information on the EPIC model and the FEST-C interface are available at https://www.cmascenter.org/fest-c/. The following variables are in this file:

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

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

This is a 3-D daily file created by the EPIC model via the FEST-C interface and contains information on fertilizer application rate and depth for each crop and soil combination in each grid cell.   Additional information on the EPIC model and the FEST-C interface are available at https://www.cmascenter.org/fest-c/. The file contains many more variables than are used by CMAQ. The following variables are in this file:

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


<a id="init_medc_1"></a>
### INIT_MEDC_1 – Soil initial conditions file

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

The ASXfile output for the previous day from the bidirectional NH<sub>3</sub> and/or Hg model is used to initialize the soil conditions for each simulation day. This file contains soil NH4 concentrations, soil pH, and Soil, vegetation and water Hg.

<a id="grid_cro_2d"></a>
### GRID_CRO_2D: Two-dimensional grid cross-point fields

Used by: CCTM

The GRID_CRO_2D time-independent file contains surface fields at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM. The following variables are in this file:

-   LAT: latitude (degrees, where Northern Hemisphere is positive)
-   LON: longitude (degrees, where Western Hemisphere is negative)
-   MSFX2: squared map-scale factor (m<sup>2</sup> m<sup>-2</sup>)
-   HT: terrain elevation (m)
-   DLUSE: dominant land use (category)
-   LWMASK: land-water mask (1=land, 0=water)
-   PURB: urban percentage if cell is based on land (percent)
-   LUFRAC_01: land use fraction of NLCD40: Evergreen Needleleaf Forest
-   LUFRAC_XX: <repeated for 40 land use fractions>

<a id=grid_dot_2d></a>
### GRID_DOT_2D: Two-dimensional grid dot-point fields

Used by: CCTM

The GRID_DOT_2D time-independent file contains surface fields at dot points (i.e., at cell corners). It is created by MCIP and used by CCTM. The following variables are in the GRID_DOT_2D file:

-   LAT: latitude (degrees, where Northern Hemisphere is positive)
-   LON: longitude (degrees, where Western Hemisphere is negative)
-   MSFD2: squared map scale factor (m<sup>2</sup> m<sup>-2</sup>)

<a id=grid_cro_3d></a>
### GRID_CRO_3D: Three-dimensional grid cross-point fields
Used by: CCTM

The GRID_CRO_3D time-independent file contains surface fields at cross points (i.e., at cell centers) that vary by height. It is created by MCIP and used by CCTM for the PT3D. The following variables are in this file:
add content

<a id=met_bdy_3d></a>
### MET_BDY_3D: Three-dimensional meteorological boundary input

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

The following deposition velocities are calculated by MCIP3 by default and written to the MET_CRO_2D file:

-   VD_SO2: deposition velocities for SO<sub>2</sub> (m s<sup>-1</sup>)
-   VD_SULF: deposition velocities for SO<sub>4</sub> (m s<sup>-1</sup>)
-   VD_NO2: deposition velocities for NO<sub>2</sub> (m s<sup>-1</sup>)
-   VD_NO: deposition velocities for NO (m s<sup>-1</sup>)
-   VD_O3: deposition velocities for O<sub>3</sub> (m s<sup>-1</sup>)
-   VD_HNO3: deposition velocities for HNO<sub>3</sub> (m s<sup>-1</sup>)
-   VD_H2O2: deposition velocities for H<sub>2</sub>O<sub>2</sub> (m s<sup>-1</sup>)
-   VD_ALD: deposition velocities for ALD (m s<sup>-1</sup>)
-   VD_HCHO: deposition velocities for HCHO (m s<sup>-1</sup>)
-   VD_OP: deposition velocities for OP (m s<sup>-1</sup>)
-   VD_PAA: deposition velocities for PAA (m s<sup>-1</sup>)
-   VD_ORA: deposition velocities for ORA (m s<sup>-1</sup>)
-   VD_NH3: deposition velocities for NH<sub>3</sub> (m s<sup>-1</sup>)
-   VD_PAN: deposition velocities for PAN (m s<sup>-1</sup>)
-   VD_HONO: deposition velocities for HONO (m s<sup>-1</sup>)
-   VD_CO: deposition velocities for CO (m s<sup>-1</sup>)
-   VD_METHANOL: deposition velocities for methanol (m s<sup>-1</sup>)
-   VD_N2O5: deposition velocities for N<sub>2</sub>O<sub>5</sub> (m s<sup>-1</sup>)
-   VD_NO3: deposition velocities for NO<sub>3</sub> (m s<sup>-1</sup>)
-   VD_GEN_ALD: deposition velocities for generic aldehyde (m s<sup>-1</sup>)
-   VD_CL2: deposition velocities for CL2 (m s<sup>-1</sup>)
-   VD_HOCL: deposition velocities for HOCL (m s<sup>-1</sup>)
-   VD_HCL: deposition velocities for HCL (m s<sup>-1</sup>)
-   VD_FMCL: deposition velocities for FMCL (m s<sup>-1</sup>)
-   VD_ICL1: deposition velocities for ICL1 (m s<sup>-1</sup>)
-   VD_ICL2: deposition velocities for ICL2 (m s<sup>-1</sup>)
-   VD_HG: deposition velocities for HG (m s<sup>-1</sup>)
-   VD_HGIIGAS: deposition velocities for HGIIGAS (m s<sup>-1</sup>)


<a id=met_cro_3d></a>
### MET_CRO_3D: Three-dimensional meteorological cross-point fields

Used by: CCTM, ICON, BCON

The MET_CRO_3D time-dependent file contains 3-D meteorological descriptions at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM, ICON, BCON, and PDM. The variables that may exist in MET_CRO_3D are the same as those that may be in MET_BDY_3D.
<a id=met_dot_3d></a>

### MET_DOT_3D: Three-dimensional meteorological dot-point fields

Used by: CCTM

The MET_DOT_3D time-dependent file contains 3-D meteorological descriptions at dot points (i.e., at cell corners) and at cell faces. It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET_DOT_3D file:

-   UWIND: u-component of horizontal wind (m s<sup>-1</sup>) [dot points; Arakawa-B grid]]
-   VWIND: v-component of horizontal wind (m s<sup>-1</sup>) [dot points; Arakawa-B grid]
-   UHAT_JD: contravariant-U*Jacobian*density (kg m<sup>-1</sup> s<sup>-1</sup>) [cell faces; Arakawa-C grid]
-   VHAT_JD: contravariant-V*Jacobian*density (kg m<sup>-1</sup> s<sup>-1</sup>) [cell faces; Arakawa-C grid]

<a id=outputs></a>
## CCTM Output Files

The previous section described the output files from JPROC, ICON, BCON, and MCIP that are input to CCTM. In this section, details on the CCTM output files are provided. Except for JPROC (which creates ASCII files), all CMAQ programs produce output files that adhere to the I/O API netCDF format (Chapter 4). The I/O API-formatted CMAQ output files are three-dimensional, gridded, time-stepped binary files that contain headers with metadata describing the file contents. These machine-independent and network transparent binary files are transferable between different computer architectures. In addition to model data output, CMAQ can optionally produce log files that contain the standard output from the various CMAQ processors. If the log file option is not selected by the user, CMAQ will write all of the log information to the screen along with the standard error, which can be captured to a text file using basic UNIX syntax.

<a id=Table8-13></a>
**Table 8-13. CMAQ Output files**

|**File Name**|**File Type**|**Time-Dependence**|**Spatial Dimensions**|
|----------------------------|------|----|-----------------------------------|
|**General**| | | |
|[Output Log](#cmaq_output_log)|ASCII|n/a|n/a
|[CTM_CONC_1](#conc)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_CGRID_1](#cgrid)|GRDDED3|1-hour|[2(X+1)+2(Y+1)]*Z
|[CTM_ACONC_1](#aconc)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_DRY_DEP_1](#drydep)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_WETDEP_1](#wetdep)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_VIS_1](#vis)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_AVIS_1](#avis)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|**Diagnostic and Advanced**| | | |
|[CTM_PMDIAG_1](#pmdiag)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_APMDIAG_1](#apmdiag)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[B3GTS_S](#b3gts)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DEPV_DIAG](#depv)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_PT3D_DIAG](#pt3d)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_DUST_EMIS_1](#dust)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_AOD_1](#aod)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_IPR_1-3](#ipr)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[CTM_IRR_1-3](#irr)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[FLOOR](#floor)|ASCII|Hourly|n/a
|[MEDIA_CONC](#media)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DEPV_MOS](#depv_mos)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DRY_DEPV_MOS](#dry_depv_mos)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DEPV_FST](#depv_fst)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_DRY_DEPV_FST](#dry_depv_fst)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_VDIFF_DIAG](#vdiff_diag)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_VSED_DIAG](#vsed_diag)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[LTNG_HOURLY](#ltnghourly)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]*Z
|[LTNG_COL](#ltngcol)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[PLAY_SRCID](#play_srcid)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_RJ_1-2](#ctm_rj)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[SOILOUT](#soilout)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_SSEMIS_1](#ssemis)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]
|[CTM_WETDEP_2](#wetdep2)|GRDDED3|Hourly|[2(X+1)+2(Y+1)]

The previous section described the output files from JPROC, ICON, BCON, and MCIP that are input to CCTM. In this section, details on the CCTM output files are provided. Except for JPROC (which creates ASCII files), all CMAQ programs produce output files that adhere to the I/O API netCDF format (Chapter 4). The I/O API-formatted CMAQ output files are three-dimensional, gridded, time-stepped binary files that contain headers with metadata describing the file contents. These machine-independent and network transparent binary files are transferable between different computer architectures. In addition to model data output, CMAQ can optionally produce log files that contain the standard output from the various CMAQ processors. If the log file option is not selected by the user, CMAQ will write all of the log information to the screen along with the standard error, which can be captured to a text file using basic UNIX syntax.

<a id=cmaq_output_log></a>
### CMAQ output log

All of the CMAQ processors generate standard output and standard error during execution. For all of the processors other than CCTM, this diagnostic output information can be captured to a log file at execution using a UNIX redirect command. For example, to capture the standard output and error of a CCTM simulation, use the following command:

```
run.cctm >& tee cctm.log
```

For CCTM, the LOGFILE environment variable allows users to specify the name of a log file for capturing the standard output from the program. If this variable is not set, the standard output is written to the terminal and can be captured using the UNIX redirect command (“>”), as shown in the example above.

<a id=conc></a>
### CTM_CONC_1: CCTM hourly instantaneous concentration file

The 3-D CCTM hourly concentration file (CONC) is the most commonly referenced CCTM output file. Containing gas-phase species mixing ratios (ppmV) and aerosol species concentra­tions (µg m<sup>-3</sup>), CONC files include instantaneous model species concentrations at the end of each model hour. The number and types of species contained in the CONC files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. The FORTRAN NameLists within the mechanism directories list the modeled species, and contain a column that specifies which species are written to the CONC files. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the CONC file by editing the CONC column in the NameList file(s) to reduce the number of species that are written to, and thus the size of the CONC file.

<a id=cgrid></a>
### CTM_CGRID_1: CCTM restart file

The 3-D CCTM ending concentration file (CGRID) is the CCTM restart file. Containing gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>), the CGRID file includes model species concentrations at the end of each simulation period. The number and types of species contained in the output CGRID files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. This file can be used to initialize CCTM from the simulation period that the model completed. For example, if the CCTM is configure to produce daily output files, a CGRID file will be written out at the end of each simulation day.

<a id=aconc></a>
### CTM_ACONC_1: CCTM hourly average concentration file

The 3-D CCTM integral average concentration file (ACONC) contains average model species concentrations for each model hour, as opposed to instantaneous concentrations at the end of each output time step. The species written to the ACONC file are set by the user in the CCTM run script using the variable AVG_CONC_SPCS. The model layers that are used to calculate the integral average concentration are also set in the CCTM run script using the variable ACONC_BLEV_ELEV, where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number. An example setting for the ACONC_BLEV_ELEV variable is “1 6”, which defines layers 1 through 6 as the vertical extent over which to calculate hourly average concentrations.

<a id=drydep></a>
### CTM_DRY_DEP_1: CCTM hourly cumulative dry deposition file

The 2-D CCTM dry deposition file (DRYDEP) includes cumulative hourly dry deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates dry deposition for all of the species listed in the dry deposition column of the FORTRAN Namelist files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the dry deposition file by editing the DDEP column in the NameList file(s).

<a id=wetdep></a>
### CTM_WETDEP_1: CCTM hourly cumulative wet deposition file

The 2-D CCTM wet deposition file (WETDEP) includes cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the FORTRAN Namelist files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s).

<a id=vis></a>
### CTM_VIS_1: CCTM hourly instantaneous visibility metrics

The 2-D CCTM visibility file contains hourly Mie and reconstructed visual range coefficients (km<sup>-1</sup>) and normalized extinction coefficients (deciviews).

<a id=avis></a>
### CTM_AVIS_1: CCTM hourly average visibility metrics

The 2-D CCTM visibility file contains hourly Mie and reconstructed visual range coefficients (km<sup>-1</sup>) and normalized extinction coefficients (deciviews).


## Diagnostic and Advanced CMAQ Output Files

Along with the basic outputs detailed in the previous section, CMAQ can be configured to output several auxiliary files for diagnosing model performance.

<a id=pmdiag></a>
### CTM_PMDIAG_1: Instantaneous hourly aerosol diagnostics file

This diagnostic file contains information on the geometric mean diameters and geometric standard deviations for the lognormal modes.

<a id=apmdiag></a>
### CTM_APMDIAG_1: Average hourly aerosol diagnostics file

This diagnostic file contains information on the geometric mean diameters and geometric standard deviations for the lognormal modes.

<a id=b3gts></a>
### B3GTS_S: Biogenic emissions diagnostic file

This optional 2-D CCTM hourly output file contains calculated biogenic emissions in mass units. The B3GTS_S file will be produced only if in-line biogenic emissions are being calculated by CCTM and if the B3GTS_DIAG variable is turned on.

<a id=depv></a>
### CTM_DEPV_DIAG: CCTM inline deposition diagnostics file

This 2-D CCTM file contains the deposition velocity (m/s) for each chemical species calculated for the final time step for the hour.

<a id=pt3d></a>
### CTM_PT3D_DIAG: CCTM PT3D diagnostics file
Add content

<a id=dust></a>
### CTM_DUST_EMIS_1
This optional 2-D CCTM hourly output file contains calculated dust emissions in mass units. The DUST_EMIS_1 file will be produced only if in-line windblown dust emissions are being calculated by CCTM and if the CTM_DUSTEM_DIAG variable is turned on.

<a id=aod></a>
### CTM_AOD_1
Aerosol optical depths calculated by the CCTM. This file will only be produced if CTM_AOD=Y in the CCTM run script.

<a id=ipr></a>
### CTM_IPR_[1-3]
The 3-D CCTM integrated process rate files (IPR) contains hourly concentrations of selected model output species in terms of the model process that contributed to the predicted concentration at each hour. For each grid cell in the process analysis domain (which is most likely a subset of the full modeling domain), the IPR file shows the hourly change in species concentration that is due to particular source/sink processes in the model. The input file procan.inp is used to set the model species for which to capture process analysis information, and the processes to track during the process analysis.

<a id=irr></a>
### CTM\_IRR_[1-3] Process analysis output – integrated reaction rates
The 3-D CCTM integrated reaction rate file (IRR) contains hourly concentrations of selected model output species in terms of the gas-phase chemistry pathways that contributed to the predicted concentration at each hour. For each grid cell in the process analysis domain (which is most likely a subset of the full modeling domain), the IRR file shows the hourly change in species concentration that is due to particular gas-phase chemistry reactions or reaction groups. The input file procan.inp is used to select the process analysis domain, the model species for which to capture process analysis information, and the chemistry reactions or groups of reactions to track during the process analysis.

<a id=floor></a>
### FLOOR: concentration-reset diagnostics file

FLOOR files are optional output diagnostic files which list specific gridboxes/timesteps in which species with negative concentrations are reset to zero.

<a id=media></a>
### MEDIA_CONC: Bidirectional soil NH4+ restart file

This 2-D CCTM file contains the the soil NH<sub>4</sub> and pH concentrations if using the bidirectional NH<sub>3</sub> option and/or the soil, vegetation and water Hg concentrations. This file is used to initialize the next day of the model simulation.

<a id=depv_mos></a>
### CTM_DEPV_MOS

This 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) for the final time step of the hour for each land use type within a grid cell.

<a id=ctm_dry_dep_mos></a>
### CTM_DRY_DEP_MOS

This 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) for the hour for each land use type within each grid cell.

<a id=ctm_dry_depv_fst></a>
### CTM_DRY_DEP_FST

This 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) through the stomatal pathway for the hour for each land use type within each grid cell.

<a id=depv_fst></a>
### CTM_DEPV_FST

This 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) through the stomatal pathway for the final time step of the hour for each land use type within a grid cell.

<a id=ctm_vdiff_diag></a>
### CTM_VDIFF_DIAG
Add content

<a id=vsed_diag></a>
### CTM_VSED_DIAG
Add content

<a id=ltnghourly></a>
### LTNG_HOURLY
Hourly 3-D lightning NO emissions calculated in-line by the CCTM.

<a id=ltngcol></a>
### LTNG_COL
Hourly column-total lightning NO emissions calculated in-line by the CCTM.

<a id=play_srcid></a>
### PLAY_SRCID
Add content

<a id=ctm_rj></a>
### CTM\_RJ_[1-2]: In-line photolysis output – gridded photolysis rates

The photolysis diagnostic output files (RJ) contain the photolysis rates calculated by CCTM when the in-line photolysis option is used.
<a id=soilout></a>
### SOILOUT

Name and location of hourly soil NO emissions file; output when in-line biogenic emissions processing is activated by setting CTM_BIOGEMIS to “T” or “Y”.

<a id=ssemis></a>
### CTM_SSEMIS_1: Sea salt emissions diagnostic file

This optional 2-D CCTM hourly output file contains calculated sea salt emissions. The SSEMIS file will be produced by CCTM only if the AERO5 aerosol mechanism is being used and if the CTM_SSEMDIAG variable is turned on.

<a id=wetdep2></a>
### CTM_WET_DEP_2: CCTM cloud diagnostics file

In CMAQ, wet deposition is calculated separately for resolved (grid-scale) clouds and for convective (subgrid) clouds. The WETDEP1 file contains the total wet deposition, i.e., the sum of both resolved-scale and subgrid-scale deposition. The WETDEP2 file contains only subgrid-scale deposition, plus some cloud diagnostic variables.  The 2-D CCTM wet deposition file (WETDEP2) includes cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the FORTRAN Namelist files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s).

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch07_programs_libraries.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch09_grid_defn.md)  
CMAQ Operational Guidance Document (c) 2016  

<!-- END COMMENT -->
