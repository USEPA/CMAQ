
[<< Previous Chapter](CMAQ_OGD_ch07_programs_libraries.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch09_grid_defn.md)
***
8. CMAQ Input and Output Files
==========

The input files for CMAQv5 consist of a domain definition file for all programs; two sets of file options for both ICON and BCON; two types of input files (WRF/MM5 and terrain) for MCIP; five mandatory and one optional input file for JPROC; and for CCTM, emissions, initial conditions, and boundary conditions files, six files that define the meteorological conditions to be simulated, and a photolysis rates file. For most CCTM input, a separate data set is required for each horizontal domain that is modeled. When CMAQv5 is configured for in-line emissions and deposition, there are additional emissions input files that are required. CMAQ output files include a basic set of files with aerosol and gas-phase species concentrations, wet and dry deposition estimates, and visibility metrics, and an auxiliary set of output files for diagnosing model performance and in-line-calculated emissions.

CMAQ Input Files
----------------

This section describes each of the input files required by the various CMAQ programs. The section begins with a description of the grid definition file, which is used by several CMAQ programs, and then goes through a program-by-program listing of the CMAQ input file requirements. [Table 8‑1](#Table8-1) lists the source, file type, and temporal and spatial dimensions of each CMAQ input file. Sample disk space requirements for a desired input data set can easily be calculated from the information in [Table 8‑1](#Table8-1); each data record is four bytes. The I/O API file sizes can be calculated using the number of variables in a CMAQ file and the spatial and temporal coverage of the data. The user should consult the CMAQv5 release notes for additional file information.

<span id=Table8-1></span>

<center>**Table 8‑1. CMAQ input files**</center>

|**File Name**|**File Type**|**Time-Dependence**|**Spatial Dimensions**|**Source**|
|---|---|---|---|---|
|**General**| | | | |
|GRIDDESC (horizontal domain definition)|ASCII|n/a|n/a|user/MCIP
|gc_matrix.nml|ASCII|n/a|n/a|user/CSV2NML
|ae_matrix.nml|ASCII|n/a|n/a|user/CSV2NML
|nr_matrix.nml|ASCII|n/a|n/a|user/CSV2NML
|tr_matrix.nml|ASCII|n/a|n/a|user/CSV2NML
|**ICON**| | | | |
|IC_PROFILE (initial conditions vertical profiles)|ASCII|Annual|n/a|user|
|CTM_CONC_1 (CCTM concentration files)|GRDDED3|Hourly|X*Y*Z|CCTM|
|MET_CRO_3D (3‑D meteorological cross-point fields)|GRDDED3|Hourly|X*Y*Z|MCIP
|**BCON**| | | | |
|BC_PROFILE (boundary conditions vertical profiles)|ASCII|Annual|n/a|user
|CTM_CONC_1 (CCTM concentration files)|GRDDED3|Hourly|X*Y*Z|CCTM
|MET_CRO_3D (3‑D meteorological cross-point fields)|GRDDED3|Hourly|X*Y*Z|MCIP|
|**JPROC**| | | | |
|ET (extraterrestrial irradiance)|ASCII|Annual|n/a|user
|PROFILES (default atmospheric profiles)|ASCII|Annual|n/a|user
|O2ABS (O2 absorption)|ASCII|Annual|n/a|user
|O3ABS (O3 absorption)|ASCII|Annual|n/a|user
|TOMS (total ozone mapping spectrometer data)|ASCII|varies|n/a|user
|CSQY (absorption cross section and quantum yields)|ASCII| Annual|n/a| User
|**MCIP**| | | | |
|InMetFiles (list of MM5 or WRF‑ARW output files)|Binary or netCDF| typically hourly, but sometimes sub-hourly| X*Y*Z| MM5 or WRF‑ARW
|InTerFile (MM5 terrain file)|Binary| n/a| X*Y | MM5
|InSatFiles||||| |
|**CCTM** | | | | |
|INIT_CONC_1 (initial conditions)| GRDDED3 | Time-invariant | X*Y*Z | ICON/CCTM
|BNDY_CONC_1 (boundary conditions)| BNDARY3 | Hourly |[2(X+1)+2(Y+1)]*Z | BCON
|JTABLE (photolysis rates look-up table)| ASCII | Daily | n/a | JPROC
|OMI| ASCII | Annual | n/a ||
|EMIS_1 (Emissions)| GRDDED3 | Hourly | X*Y*Z | SMOKE
|OCEAN_1 (sea salt mask)| GRDDED3 | Time-invariant | X*Y | |Spatial Allocator
|GSPRO (speciation profiles)| ASCII | Time-invariant | N/a | User
|B3GRD (grid-normalized biogenic emissions)| GRDDED3 | Time-invariant | X*Y | SMOKE
|BIOSEASON (freeze dates)|GRDDED3 |Time-invariant | X*Y | Metscan
|STK_GRPS_## (stack groups)| GRDDED3 |Time-invariant|X*Y | SMOKE
|STK_EMIS_## (point-source emissions)| GRDDED3 | Hourly | X*Y | SMOKE
|DUST_LU_1|GRDDED3 | Time-invariant | X*Y | Spatial Allocator
|DUST_LU_2|GRDDED3 | Time-invariant | X*Y | Spatial Allocator
|CROPMAP01| GRDDED3 | Time-invariant | X*Y | Cropcal
|CROPMAP04| GRDDED3 | Time-invariant | X*Y | Cropcal
|CROPMAP08|GRDDED3 | Time-invariant | X*Y | Cropcal
|LTNGNO|<center> GRDDED3 </center>|<center> Hourly </center>|<center> X*Y*Z </center>|<center> User
|LTNGPARM_FILE| GRDDED3 | Monthly | X*Y |LTNG_2D_DATA
|B4LU_file|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> X*Y
|E2C_Soilfile|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> X*Y
|E2C_Fertfile|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> X*Y
|INIT_MEDC_1|<center> GRDDED3 </center>||<center> X*Y </center>||
|GRID_CRO_2D (2‑D grid cross-point fields)|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> X*Y </center>|<center> MCIP
|GRID_DOT_2D (2‑D grid dot-point fields)|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> (X+1)*(Y+1) </center>|<center> MCIP
|MET_BDY_3D (3‑D meteorological boundary input)|<center> BNDARY3 </center>|<center> Hourly </center>|<center> PERIM*Z </center>|<center> MCIP
|MET_CRO_2D (2‑D meteorological cross-point fields)|<center> GRDDED3 </center>|<center> Hourly </center>|<center> X*Y </center>|<center> MCIP
|MET_CRO_3D (3‑D meteorological cross-point fields)|<center> GRDDED3 </center>|<center> Hourly </center>|<center> X*Y*Z </center>|<center> MCIP
|MET_DOT_3D (3‑D meteorological dot-point fields)|<center> GRDDED3 </center>|<center> Hourly </center>|<center> (X+1)*(Y+1)*Z </center>|<center> MCIP
</center>|

### GRIDDESC: Horizontal domain definition

Used by: ICON, BCON, CCTM

The CMAQ grid description file (**GRIDDESC**) is used by all programs except JPROC and MCIP to define the horizontal spatial grid of the modeling domain. GRIDDESC implements [I/O API](CMAQ_OGD_ch06_req_lib.md#IOAPI) grid conventions: for more details see the section on [Grids and coordinate systems](CMAQ_OGD_ch09_grid_defn.md#grids-and-coordinate-systems).

A GRIDDESC is an ASCII file that contains two sections: a horizontal coordinate section, and grid description section. GRIDDESC is the logical name for text files that store horizontal coordinate and grid descriptions, and that are read by the DSCGRID() and DSCOORD() utility routines. Each segment has a one-line header (which by convention provides titles for the columns in the data records), a sequence of data records, and a terminal record with name field blank (i.e., ' '). The GRIDDESC file is generated automatically with MCIP; alternatively, GRIDDESC can be created using a text editor.

The horizontal coordinate section ([Table 8-2](#Table8-2)) consists of text records that provide coordinate-system name, the map projection, and descriptive parameters P_ALP, P_BET, P_GAM, XCENT, and YCENT.

The grid description section ([Table 8-3](#Table8-3)) consists of text records that indicate the grid name, related coordinate-system name (i.e., which GRIDDESC horizontal coordinate name that is defined in the previous section that is applied to this grid), and descriptive parameters XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, and NTHIK. For a typical CMAQ application, both "dot-point" and "cross-point" grids are defined in the GRIDDESC file; these grids are topological duals in the sense that the vertices (corners) of one correspond to the cell-centers of the other.

<span id=Table8-2></span>

<center> **Table 8‑2. Coordinate sytem description segment of GRIDDESC**</center>

| **Line**| **Column**| **Name** | **Type** | **Description**|
|---|---|---|---|---|
|1|A|<center> Header </center>|<center> String </center>|Single-quote-delimited header describing section contents; may be blank, i.e., ' '|
|2|A|<center> COORD-NAME </center>|<center> String </center>|Name of the coordinate description (required); single quote delimited|
|3|A|<center> COORDTYPE </center>|<center> Int </center>|I/O API index defining the map projection type (required)| |<center>
|3|B| P_ALP </center>|<center> Double </center>|First map projection descriptive parameter (dependent on projection type)| |<center>
|3|C| P_BET </center>|<center> Double </center>|Second map projection descriptive parameter (dependent on projection type)| |<center>  
|3|D| P_GAM </center>|<center> Double </center>|Third map projection descriptive parameter (dependent on projection type)| |<center>  
|3|E|<center> XCENT </center>|<center> Double </center>|Longitude for coordinate system center| | 
|3|F|<center> YCENT </center>|<center> Double </center>|Latitude for coordinate system center|

<a id=Table8-3></a>

<center> **Table 8‑3. Grid definition segment of GRIDDESC**</center>

|**Line** | **Column** | **Name** | **Type** | **Description**|
|---|---|---|---|---|
| 1 | A | Header </center>|<center> String|Single-quote-delimited header describing section contents; may be blank, i.e., ' '|
| 2 | A | GRID-NAME | String |Name of the horizontal grid (required); single quote delimited|
| 3 | A | COORD-NAME| String |Name of the coordinate description in the previous section (required); single quote delimited|
| 3 | B | XORIG | Double |X-coordinate for lower-left (southwest) corner of the grid with respect to (XCENT,YCENT) (dependent on projection type)|
|3 | C | YORIG | Double |Y-coordinate for lower-left (southwest) corner of the grid with respect to (XCENT,YCENT) (dependent on projection type)|
|3 | D | XCELL | Double|X-coordinate grid cell size (dependent on projection type)|
|3 | E | YCELL | Double|Y-coordinate grid cell size (dependent on projection type)|
|3 | F | NCOLS | Int |Number of horizontal grid columns (dependent on projection type)|
|3 | G | NROWS | Int |Number of horizontal grid rows (dependent on projection type)|
|3 | H </center>|<center> NTHIK </center>|<center> Int </center>|Boundary perimeter thickness (number of cells) (optional)|

Each data record in these files consists of two or three list-formatted lines (i.e., items are separated by either blanks or commas). Name fields are quoted strings, and appear on the first of these lines. Numeric fields are given in double precision, and occur on either the second line or the second and third lines (this allows you to organize the text so that it is easily viewed in a text editor without running off-screen). The records have the following organization, depending upon whether they are in the first or second segment of GRIDDESC:

` COORD-NAME`
` COORDTYPE, P_ALP, P_BET, P_GAM`
` XCENT, YCENT`

or

` COORD-NAME`
` COORDTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT`

and

` GRID-NAME`
` COORD-NAME, XORIG, YORIG, XCELL, YCELL`
` NCOLS, NROWS, NTHIK`

or

` GRID-NAME`
` COORD-NAME, XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK`

There are at most 32 coordinate systems and 256 grids listed in one of these files. These files are small enough to be archived easily with a study, and have a sufficiently simple format that new ones can easily be constructed "by hand."

An example of a GRIDDESC file is shown below:

` ' '`
` 'LAM_40N100W'`
` 2 30.0 60.0 -100.0 -100.0 40.0`
` ' '`
` 'M_32_99TUT02'`
` 'LAM_40N100W' 544000.0 -992000.0 32000.0 32000.0 38 38 1`
` ' '`

The horizontal coordinate section (first section) in this example GRIDDESC file defines a horizontal coordinate named “LAM_40N100W”. The coordinate definition is for a Lambert conformal grid, keyed by the first column of the coordinate description line, which corresponds to the numeric code for the various I/O API-supported grid types (2 = Lambert). The next three parameters (P_ALP, P_BET, and P_GAM) have different definitions for different map projections. For Lambert conformal, P_ALP and P_BET are the true latitudes of the projection cone (30°N and 60°N in the example), and P_GAM (100°W in the example) is the central meridian of the projection. The last two parameters, XCENT and YCENT, are the latitude-longitude coordinates of the grid center of the Cartesian coordinate system, which are 100°W and 40°N in the example. If using WRF-ARW as the meteorological model, the user should be aware of differences from this method.

The example grid definition section above describes a grid named “M_32_99TUT02”. The definition of the grid begins with a reference to a coordinate name from the coordinate definition section of the file; in this example, the coordinate named “LAM_40N100W” is referenced in the grid definition. The next two parameters in the grid definition (XORIG and YORIG) are the east-west and north-south offsets from XCENT and YCENT in meters (WRF-ARW usages may differ). The next two parameters (XCELL and YCELL) are the horizontal grid spacing in meters for the X and Y directions (i.e., delta‑x and delta‑y). The next two parameters (NCOLS and NROWS) are the numbers of grid cells in the X and Y directions. The grid definition concludes with the number of boundary cells, NTHIK, which is typically set to 1.

### [gc|ae|nr|tr]_matrix.nml: Species namelist files

Used by: BCON, CCTM, ICON, JPROC, PROCAN

Namelist look-up tables for different classes of simulated pollutants are used to define the parameters of different model species during the execution of the CMAQ programs. Gas-phase (gc), aerosol (ae), non-reactive (nr), and tracer (tr) species namelist files contain parameters for the model species that are included in these different classifications. The species namelist files are used to control how the different CMAQ programs and processes handle the model species. The namelist files define the following processes for each model species:

-   Emissions – is the pollutant an emitted species
-   Emissions factor – if the pollutant is an emitted species, uniformly apply a scaling factor to the emissions
-   Deposition velocity – does the pollutant have a deposition velocity
-   Deposition velocity factor – if the pollutant does have a deposition velocity, uniformly apply a scaling factor to this velocity
-   Initial/boundary conditions – is the pollutant in the initial and boundary conditions
-   IC/BC Factor – if the pollutant is in the initial/boundary conditions, uniformly apply a scaling factor to the concentrations
-   Scavenging
-   Scavenging factor
-   Gas-to-aerosol conversion – does the pollutant undergo heterogeneous transformation from the gas-phase to the aerosol-phase
-   Gas-to-aqueous conversion – does the pollutant undergo heterogeneous transformation from the gas-phase to the liquid phase
-   Aerosol-to-aqueous conversion – does the pollutant undergo heterogeneous transformation from the aerosol-phase to the liquid phase
-   Transport – is the pollutant transported by advection and diffustion in the model
-   Dry deposition – Write the pollutant to the dry deposition output file
-   Wet deposition – Write the pollutant to the wet deposition output file
-   Concentration – Write the pollutant to the instantaneous concentration output file

The namelist files contain header information that describe which class of species are contained in the file, the number of parameters contained in the file, headers describing the parameter fields, and then a series of rows with configuration parameters for every model species. Table 8-4 contains the namelist file format for the gas-phase (GC) species namelist file. The namelist files for the other species classifications (AE, NR, TR) are similar to the format shown in Table 8-4.

<a id=Table8-4></a>

<center> **Table 8‑4. GC species namelist file format** </center>

| **Line**| **Column** |**Name** | **Type**| **Description** |
|---|---|---|---|---|
|<center> 1 </center>||<center> File type </center>|<center> String </center>|&GC_nml|
|<center> 2 </center>||<center> Number of surrogate params </center>|<center> String </center>|n_surr1 = x, where x is the number of ????|
|<center> 3 </center>||<center> Number of ???? params </center>|<center> String </center>|n_surr2 = x, where x is the number of ????|
|<center> 4 </center>||<center> Number of control params </center>|<center> String </center>|n_ctrl = x, where x is the number of ????|
|<center> 5 </center>||<center> Header ID </center>|<center> String </center>|TYPE_HEADER =|
|<center> 6 </center>||<center> HEADER </center>|<center> String </center>|Abbreviated names of file columns, enclosed by single quotes|
|<center>  7 </center>||<center> Matrix ID </center>|<center> String </center>|TYPE_MATRIX =|
|<center> 8 </center>|<center> 1 </center>|<center> SPC </center>|<center> String </center>|CMAQ pollutant name, i.e. NO, HNO3, PAR; dependent on chemical mechanism|
||<center> 2 </center>|<center> MOLWT </center>|<center> Integer </center>|Pollutant molecular weight|
||<center> 3 </center>|<center> EMIS_SUR </center>|<center> String </center>|Emissions species name for the CMAQ pollutant|
||<center> 4 </center>|<center> EMIS_FAC </center>|<center> Real </center>|Scaling factor for input emissions|
||<center> 5 </center>|<center> DEPV_SUR </center>|<center> String </center>|Deposition velocity variable name for the CMAQ pollutant|
||<center> 6 </center>|<center> DEPV_FAC </center>|<center> Real </center>|Scaling factor for the deposition velocity|
||<center> 7 </center>|<center> ICBC_SUR </center>|<center> String </center>|IC/BC species name for the CMAQ pollutant|
||<center> 8 </center>|<center> ICBC_FAC </center>|<center> Real </center>|Scaling factor for the IC/BC concentration|
||<center> 9 </center>|<center> SCAV_SUR </center>|<center> String </center>||
||<center> 10 </center>|<center> SCAV_FAC </center>|<center> Real </center>||
||<center> 11 </center>|<center> G2AE_SUR </center>|<center> String </center>|Gas-to-aerosol transformation species|
||<center> 12 </center>|<center> G2AQ_SUR </center>|<center> String </center>|Gas-to-aqueous transformation species|
||<center> 13 </center>|<center> TRNS </center>|<center> Yes/No </center>|Transport switch|
||<center> 14 </center>|<center> DDEP </center>|<center> Yes/No </center>|Dry deposition output file switch|
||<center> 15 </center>|<center> WDEP </center>|<center> Yes/No </center>|Wet deposition output file switch|
||<center> 16 </center>|<center> CONC </center>|<center> Yes/No </center>|Concentration output file switch|
| … | ...| ...|... | Repeat for the number of gas-phase pollutants in the mechanism being modeling|

The namelist files for the other pollutant classes have similar configurations as the gas-phase species configuration shown in [Table 8-4](#Table8-4). See existing namelist files in the CMAQv5 distribution for examples.

### IC_PROFILE: Initial conditions vertical profiles

Used by: ICON

ICON can generate initial conditions from two different input file types. The first file type is an ASCII vertical profile file that lists species concentrations at various model layers that are fixed in space and time. To configure ICON to generate initial conditions from ASCII vertical profiles, the “prof” input module is chosen when compiling the program (see Section 5.5 on ICON). These ASCII-formatted vertical profile files are IC_PROFILE files, and are described in this section. IC_PROFILE files must be developed by the user and can be generated from climatologically averaged observational data or as an a priori estimate from previous modeling studies of the region being modeled. The second file type that ICON can use to generate initial conditions is a concentration file from a previous CMAQ run. These are CTM_CONC_1 files, and are described later in Section 6.1.5.

IC_PROFILE begins with a header that contains a comment section that describes the data, and a file description section that defines the number of vertical levels in the file, the number of pollutants in the file, and the distribution of the vertical levels. The next entries in IC_PROFILE are the Julian start date and the start time of the data; they are not used by ICON.

Each line in IC_PROFILE corresponds to a different pollutant and begins with the name of the pollutant. The subsequent columns on each line list the chemical concentration at each layer contained in the file. Gas-phase species are in ppmV, and aerosol species are in μg m<sup>‑3</sup>. The layer structure of the IC_PROFILE vertical profiles does not need to correspond exactly to the layer structure that will be modeled; the ICON program will interpolate the data to the correct vertical format for the simulation.

Initial conditions are provided for only the first hour of a model simulation. The initial conditions that are based on an ASCII vertical profile include a gridded file for input to CCTM that has horizontally uniform species concentrations at each model layer. For spatially resolved initial conditions in the horizontal direction, it is necessary to use the other input file type to ICON, an existing CCTM concentration file (CTM_CONC_1).

A detailed description of the vertical profile file format for initial conditions is provided in [Table 8-5](#Table8-5). The header of the profiles file is list-formatted, while the data section of the file is fixed format.

<a id=Table8-5></a>

<center> **Table 8-5. IC_PROFILE format description** </center>

|**Line**|**Column**|**Name**|**Type**|**Description**|
|---|---|---|---|---|
|<center> 1-3 </center>||<center> Text Header </center>|<center> String </center>|Text description of the contents and source of the initial conditions file (optional)|
|<center> 4 </center>|<center> A </center>|<center> NUM_SIGMA_LVL </center>|<center> Int </center>|Number of sigma levels contained in the file (required)|
||<center> B </center>|<center> NUM_POLL </center>|<center> Int </center>|Number of pollutants contained in the file (required)|
||<center> C </center>|<center> SIGMA_LVL </center>|<center> Real </center>|Vertical coordinate values of sigma-p levels; number of values (n+1) is one more than the NUM_SIGMA_LVL (n) (required)|
|<center> 4 </center>|<center> n </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 5 </center>|<center> A </center>|<center> STDATE </center>|<center> String </center>|Julian start date of the file, YYYYDDD (optional)|
||<center> B </center>|<center> STTIME </center>|<center> String </center>|Start time of the file, HH (optional)|
|<center> 6 </center>|<center> 1-10 </center>|<center> SPECIES1 </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
||<center> 12-20 </center>|<center> LAYER1_IC </center>|<center> Exp </center>|IC concentration for species 1 in lowest sigma layer (required)|
||<center> 23-31 </center>|<center> LAYER2_IC </center>|<center> Exp </center>|IC concentration for species 1 in 2nd sigma layer (required)|
||<center> 34-42 </center>|<center> LAYER3_IC </center>|<center> Exp </center>|IC concentration for species 1 in 3rd sigma layer (required)|
||<center> 45-53 </center>|<center> LAYER4_IC </center>|<center> Exp </center>|IC concentration for species 1 in 4th sigma layer (required)|
||<center> ...  </center>|<center> LAYERX_IC </center>|<center> Exp </center>|IC concentration for species 1 in Xth sigma layer (required)|
 |<center> 7 </center>|<center> 1-10 </center>|<center> SPECIES2 </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
||<center> 12-20 </center>|<center> LAYER1_IC </center>|<center> Exp </center>|IC concentration for species 2 in lowest sigma layer (required)|
||<center> 23-31 </center>|<center> LAYER2_IC </center>|<center> Exp </center>|IC concentration for species 2 in 2nd sigma layer (required)|
||<center> 34-42 </center>|<center> LAYER3_IC </center>|<center> Exp </center>|IC concentration for species 2 in 3rd sigma layer (required)|
||<center> 45-53 </center>|<center> LAYER4_IC </center>|<center> Exp </center>|IC concentration for species 2 in 4th sigma layer (required)|
||<center> ...  </center>|<center> LAYERX_IC </center>|<center> Exp </center>|IC concentration for species 2 in Xth sigma layer (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> Z </center>|<center> 1-10 </center>|<center> SPECIESZ </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
|<center> ...  </center>|<center> 12-20 </center>|<center> LAYER1_IC </center>|<center> Exp </center>|IC concentration for species Z in lowest sigma layer (required)|
|<center> ...  </center>|<center> 23-31 </center>|<center> LAYER2_IC </center>|<center> Exp </center>|IC concentration for species Z in 2nd sigma layer (required)|
|<center> ...  </center>|<center> 34-42 </center>|<center> LAYER3_IC </center>|<center> Exp </center>|IC concentration for species Z in 3rd sigma layer (required)|
|<center> ...  </center>|<center> 45-53 </center>|<center> LAYER4_IC </center>|<center> Exp </center>|IC concentration for species Z in 4th sigma layer (required)|
|<center> ...  </center>|<center> ...  </center>|<center> LAYERX_IC </center>|<center> Exp </center>|IC concentration for species Z in Xth sigma layer (required)|

A sample of the four sections of an IC_PROFILE file is shown below.

` Example initial condition: The vertical coordinate of the model to generate`
` these i.c. is the terrain-following sigma coordinate. `
` The number of sigma layers and defined sigma levels are listed below. `
` 6 55 1.00 0.98 0.93 0.84 0.60 0.30 0.00`
` 1988180 00`
` "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04`

### BC_PROFILE: Boundary conditions vertical profiles

Used by: BCON

As with the ICON program, BCON can generate boundary conditions from two different input file types. The first file type is an ASCII vertical profile file that list species concentrations at various model layers that are fixed in space in time. To configure BCON to generate boundary conditions from ASCII vertical profiles, the “prof” input module is chosen when compiling the program (see Section 5.2 on BCON). These ASCII-formatted vertical profile files are BC_PROFILE files, and are described in this section. BC_PROFILE files must be developed by the user and can be generated from climatologically averaged observational data or as an a priori estimate from previous modeling studies of the region being modeled. The second file type that BCON can use to generate initial conditions is a concentration file from a previous CMAQ run. These are CTM_CONC_1 files, and are described later in Section 6.1.5.

BC_PROFILE begins with a header that contains a comment section that describes the data, and a file description section that defines the number of vertical levels in the file, the number of pollutants in the file, and the distribution of the vertical levels. The next entries in BC_PROFILE are the Julian start date and the start time of the data; they are not used by BCON. The BCON input consists of four data sections that correspond to the lateral boundaries (i.e., north, south, east, and west) of the model grid. The BCON input profiles contain a field that precedes each data section to indicate which horizontal boundary the data section describes.

The format of the data sections in BC_PROFILE files is the same as in IC_PROFILE files. Each line corresponds to a different pollutant and begins with the name of the pollutant. The subsequent columns on each line list the chemical concentration at each layer contained in the file. Gas-phase species are in ppmV, and aerosol species are in g m<sup>‑3</sup>. The layer structure of the BC_PROFILE vertical profiles does not need to correspond exactly to the layer structure that will be modeled; the BCON program will interpolate the data to the correct vertical format for the simulation.

Boundary conditions can either be time-independent (static) or time-dependent (dynamic). Boundary conditions generated with BC_PROFILE’s ASCII vertical profiles are both static and spatially uniform along each of the four horizontal boundaries at each model layer. For spatially resolved (in the horizontal direction) and temporally resolved boundary conditions, it is necessary to use the other input file type to BCON, an existing CCTM concentration file (CTM_CONC_1).

A detailed description of the vertical profile file format for boundary conditions is provided in Table 8-6. The header of the profiles file is list-formatted, while the data section of the file is fixed format.

<a id=Table8-6></a>

<center> **Table 8-6. BC_PROFILE format description** </center>

|**Line**|**Column**|**Name**|**Type**|**Description**|
|---|---|---|---|---|
| 1-3 || Text Header | String |Text description of the contents and source of the initial conditions file (optional) |
|<center> 4 </center>|<center> A </center>|<center> NUM_SIGMA_LVL </center>|<center> Int </center>|Number of sigma levels contained in the file (required)|
||<center> B </center>|<center> NUM_POLL </center>|<center> Int </center>|Number of pollutants contained in the file (required)|
||<center> C </center>|<center> SIGMA_LVL </center>|<center> Real </center>|Vertical coordinate values of sigma-p levels; number of values (n+1) is one more than the NUM_SIGMA_LVL (n) (required)|
|<center> 4 </center>|<center> n </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 5 </center>|<center> A </center>|<center> STDATE </center>|<center> String </center>|Julian start date of the file, YYYYDDD (optional)|
||<center> B </center>|<center> STTIME </center>|<center> String </center>|Start time of the file, HH (optional)|
|<center> 6 </center>|<center> A </center>|<center> Direction </center>|<center> String </center>|North, South, East, West; indicates the boundary described by the subsequent data section (required)|
|<center> 7 </center>|<center> 1-10 </center>|<center> SPECIES1 </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
||<center> 12-20 </center>|<center> LAYER1_BC </center>|<center> Exp </center>|BC concentration for species 1 in lowest sigma layer (required)|
||<center> 23-31 </center>|<center> LAYER2_BC </center>|<center> Exp </center>|BC concentration for species 1 in 2nd sigma layer (required)|
||<center> 34-42 </center>|<center> LAYER3_BC </center>|<center> Exp </center>|BC concentration for species 1 in 3rd sigma layer (required)|
||<center> 45-53 </center>|<center> LAYER4_BC </center>|<center> Exp </center>|BC concentration for species 1 in 4th sigma layer (required)|
||<center> ...  </center>|<center> LAYERX_BC </center>|<center> Exp </center>|BC concentration for species 1 in Xth sigma layer (required)|
|<center> 8 </center>|<center> 1-10 </center>|<center> SPECIES2 </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
||<center> 12-20 </center>|<center> LAYER1_BC </center>|<center> Exp </center>|BC concentration for species 2 in lowest sigma layer (required)|
||<center> 23-31 </center>|<center> LAYER2_BC </center>|<center> Exp </center>|BC concentration for species 2 in 2nd sigma layer (required)|
||<center> 34-42 </center>|<center> LAYER3_BC </center>|<center> Exp </center>|BC concentration for species 2 in 3rd sigma layer (required)|
||<center> 45-53 </center>|<center> LAYER4_BC </center>|<center> Exp </center>|BC concentration for species 2 in 4th sigma layer (required)|
||<center> ...  </center>|<center> LAYERX_BC </center>|<center> Exp </center>|BC concentration for species 2 in Xth sigma layer (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> Y </center>|<center> A </center>|<center> Direction </center>|<center> String </center>|North, South, East, West; indicates the horizontal boundary described by the subsequent data section (required)|
|<center> Z+1 </center>|<center> 1-10 </center>|<center> SPECIESZ </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
|<center> ...  </center>|<center> 12-20 </center>|<center> LAYER1_BC </center>|<center> Exp </center>|BC concentration for species Z in lowest sigma layer (required)|
|<center> ...  </center>|<center> 23-31 </center>|<center> LAYER2_BC </center>|<center> Exp </center>|BC concentration for species Z in 2nd sigma layer (required)|
|<center> ...  </center>|<center> 34-42 </center>|<center> LAYER3_BC </center>|<center> Exp </center>|BC concentration for species Z in 3rd sigma layer (required)|
|<center> ...  </center>|<center> 45-53 </center>|<center> LAYER4_BC </center>|<center> Exp </center>|BC concentration for species Z in 4th sigma layer (required)|
|<center> ...  </center>|<center> ...  </center>|<center> LAYERX_BC </center>|<center> Exp </center>|BC concentration for species Z in Xth sigma layer (required)|

A sample of the important sections of a BC_PROFILE file is shown below.  ` 6 55 1.00 0.98 0.93 0.84 0.60 0.30 0.00`
` 1988180 00`
` North`
` "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04`
` West`
` "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04`

### CTM_CONC_1: CCTM concentration files

Used by: ICON, BCON

An I/O API GRDDED3-formatted CCTM output concentration file, CTM_CONC_1, can be used to create spatially and temporally varying initial and boundary conditions. To configure ICON and BCON to generate initial and boundary conditions from a CCTM concentration file, the “m3conc” input module is chosen when compiling the programs (see Section 5.5 on ICON and Section 5.2 on BCON). The input concentration file must cover a temporal and spatial extent that is consistent with the time period and domain that are being modeled, respectively. Both ICON and BCON require a Julian start date to be specified at execution that identifies the first time step to extract from the input concentration file; BCON also requires a run-length specification to indicate the number of time steps of boundary conditions to extract from the input file. For nested runs, the nested domain for which initial and boundary conditions are being extracted must be on the same projection and fall within the domain contained in the input concentration file.

### CSQY: Absorption cross section and quantum yields

Used by: JPROC

CSQY is the logical name for the ASCII data file containing absorption cross section and quantum yield data for unique photolysis reactions. The data in these files are listed as a function of wavelength and correspond to the standard photolysis reactions in each of the chemical mechanisms implemented in CMAQ. A flexible format allows users to deviate from these standards and test new data with minimal effort.

The ASCII-formatted CSQY files begin with a header that describes the applicable photolysis reaction. This header includes (1) the reaction name/ID; (2) comment lines describing the reaction, the stoichiometry, and the data source (note that comment lines are preceded with a “!”); (3) the location on the wavelength interval that the data represent (beginning, centered, ending, or point); and (4) a multiplier (FAC) that is applied to the photolysis rate calculation. The data section of the CSQY file lists the wavelength of the incoming solar radiation (nm), the absorption cross section (cm), and the quantum yield as columns, with each row corresponding to a specific wavelength interval. The CSQY file uses a space-delimited, free-form format for the data section of the file. A detailed description of the CSQY file format is provided in Table 8-7.

<a id=Table8-7></a>

<center> **Table 8-7. CSQY format description** </center>

|**Line** | **Column** | **Name** | **Type** | **Description** |
|---|---|---|---|---|
|<center> 1 </center>|<center> A </center>|<center> Reaction ID </center>|<center> String </center>|Text name identifying the CSQY data this name is cross-referenced in the chemical mechanism description and INCLUDE files (required)|
|<center> 2 </center>|<center> A </center>|<center> Comments </center>|<center> String </center>|Preceded by "!", comment lines describe the reaction, list the stoichiometry, and document the source of the data (optional)|
|<center> n </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|
|<center> n+1 </center>|<center> A </center>|<center> Data Location </center>|<center> String </center>|Field indicating the location of the data as measured across the wavelength band; possible answers: beginning, ending, centered, point (required)|
|<center> n+2 </center>|<center> A </center>|<center> Multiplier </center>|<center> String </center>|Multiplication factor to apply to photolysis rate equation; line begins with FAC=; factor is listed in real or exponential format (required)|
|<center> n+3 </center>|<center> A </center>|<center> Wavelength </center>|<center> Int or Real </center>|Wavelength corresponding to CSQY data; units = nm (required)|
||<center> B </center>|<center> Absorption Cross-Section </center>|<center> Real or Exp </center>|Measurement of the cross-section of a molecule’s spherical receiving surface for actinic flux; units = cm<sup>2</sup> molecule<sup>-1</sup> (required)|
||<center> C </center>|<center> Quantum Yield </center>|<center> Real </center>|Ratio of the number of molecules reacting via a specific pathway to the number of molecules absorbing photons in that wavelength interval; units = molecules photon<sup>-1</sup> (required)|
|<center> n+4 </center>|<center> A </center>|<center> Wavelength </center>|<center> Int </center>|Wavelength corresponding to CSQY data; units = nm (required)|
||<center> B </center>|<center> Absorption Cross-Section </center>|<center> Real or Exp </center>|Measurement of the cross-section of a molecule’s spherical receiving surface for actinic flux; units = cm<sup>2</sup> molecule<sup>-1</sup> (required)|
||<center> C </center>|<center> Quantum Yield </center>|<center> Real </center>|Ratio of the number of molecules reacting via a specific pathway to the number of molecules absorbing photons in that wavelength interval; units = molecules photon<sup>-1</sup> (required)|
|<center> n+X </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|

A sample of the important sections of a CSQY file is shown below.

` ALD_CBIV88`
` ! Acetaldehyde Photolysis (ALD)`
` ! CH3CHO + hv (+2O2)-> CH3OO + HO2 + CO`
` ! Taken from Gery et al. (1988); CSQY from Baulch et al. 5 (1984).`
` ! format: wl, abs_cs, qy`
` Centered`
` ! With FAC, units are (cm^2/molecule)`
` FAC=1.0E-20`
` 280 4.50 0.580`
` 281 4.54 0.575`
` 282 4.58 0.570`

### ET: Extraterrestrial irradiance

Used by: JPROC

ET is the logical name for the ASCII data file containing extraterrestrial radiation as a function of wavelength. The extraterrestrial irradiance file has a format similar to that of the CSQY file (Section 6.1.6). The file begins with a header section; comment lines are preceded with a “!”. Like the CSQY file, the header contains a field describing the location on the wavelength interval that the data represent, and a multiplier. The data section uses a space-delimited, free-form format and lists the wavelength of the incoming solar radiation (nm) and the irradiance (photons cm<sup>‑2</sup> s<sup>‑1</sup>) at each wavelength, with each row corresponding to a specific wavelength interval. A detailed description of the file format is provided in [Table 8-8](Table8-8).

<span id=Table8-8></span>

<center> **Table 8-8 ET file format description** </center>

| **Line** | **Column** | **Name** | **Type** | **Description**|
|---|---|---|---|---|
|<center> 1 </center>|<center> A </center>|<center> Comments </center>|<center> String </center>|Preceded by "!", comment lines describe the file contents and document the source of the data (optional)|
|<center> n </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|
|<center> n + 1 </center>|<center> A </center>|<center> Data Location </center>|<center> String </center>|Field indicating the location of the data as measured across the wavelength band; possible answers: beginning, ending, centered, point (required)|
|<center> n + 2 </center>|<center> A </center>|<center> Multiplier </center>|<center> String </center>|Multiplication factor to apply to photolysis rate equation; line begins with FAC=; factor listed in real or exponential format (required)|
|<center> n+3 </center>|<center> A </center>|<center> Wavelength </center>|<center> Int or Real </center>|Wavelength corresponding to ET data; units = nm (required)|
||<center> B </center>|<center> Extra–terrestrial Irradiance </center>|<center> Real or Exp </center>|Estimation of the photon flux reaching the exterior of the earth’s atmosphere; units = photons cm<sup>-2</sup> second<sup>-1</sup> (required)|
|<center> n+4 </center>|<center> A </center>|<center> Wavelength </center>|<center> Int </center>|Wavelength corresponding to ET data; units = nm (required)|
||<center> B </center>|<center> Extra–terrestrial Irradiance </center>|<center> Real or Exp </center>|Estimation of the photon flux reaching the exterior of the earth’s atmosphere; units = photons cm<sup>-2</sup> second<sup>-1</sup> (required)|
|<center> n+X </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|

A sample of the important sections of an ET file is shown below.

` ! Extraterrestrial Irradiance`
` ! Taken from the RADM data---derived from the WMO 1985 report Table 7-4`
` ! Format: wl, et_irradBeginning`
` ! With FAC, units are (photons cm-2 s-1)`
` FAC=1.0`
` 185.185 3.620E+11`
` 186.916 4.730E+11`

### PROFILES: Atmospheric vertical profiles

Used by: JPROC

PROFILES is the logical name for the ASCII data file containing seasonal and vertical profiles for ozone, aerosol attenuation, temperature, air pressure, and Dobson values. The ASCII-formatted data provided in the PROFILES file are at 19 latitudes (90°N to 90°S) and 51 altitudes (0 to 50 km) in three distinct data sections. The first data section contains seasonal, latitude-dependent vertical profiles of O<sub>3</sub> concentrations (molecules cm<sup>‑3</sup>), air temperature (K), and air density (molecules cm<sup>‑3</sup>). The second data section contains monthly Dobson values at the 19 latitude bands. The last data section contains vertical profiles from the 1976 U.S. Standard Atmosphere of air temperature (K), air density (molecules cm<sup>‑3</sup>), ozone concentrations (molecules cm<sup>‑3</sup>), and aerosol attenuation coefficients (km<sup>‑1</sup>).

The first data section of the PROFILES file is divided into 228 (1934) data blocks, with each block representing one of the three variables (O<sub>3</sub>, air temperature, and air density) at one of the 19 latitude bands, for each of the 4 seasons of the year. The individual data blocks contain 51 values per variable, representing standard conditions at altitudes ranging from 0 to 50 km. The data are ordered, from general to specific, by season (spring, summer, autumn, winter), variable (O<sub>3</sub>, air temperature, air density), latitude, and altitude. For example, the first block in the PROFILES file contains spring O<sub>3</sub> concentration profiles at the latitude band ranging from 90°N to 80°N from 0 to 50 km above sea level; the first value in the block is at 0 km and the last value is at 50 km. The next data block is again spring O<sub>3</sub> concentration profiles but at the latitude band ranging from 80°N to 70°N. The next 17 data blocks complete the spring O<sub>3</sub> concentration profiles by continuing through the rest of the 19 latitude bands, with the last block representing the 80°S to 90°S latitude band. The 20<sup>th</sup> data block begins the spring air temperature profiles at the latitude band ranging from 90°N to 80°N and is followed by 18 more data blocks of spring air temperature profiles. The following 19 data blocks follow an identical format for air density and round out the spring profiles. The 193 data blocks are then repeated for summer profiles, autumn profiles, and finally winter profiles.

The second data section in the PROFILES file contains monthly average Dobson values. The data are organized in 12 rows (January through December) and 19 columns (90°N to 80°N through 80°S to 90°S).

The last data section in the PROFILES file contains vertical profiles from the 1976 U.S. Standard Atmosphere of temperature, air density, ozone concentrations, and aerosol attenuation coefficients. The data are organized in 51 rows, corresponding to altitude (0 to 50 km), with four columns per row for each of the four variables. A detailed description of the file format is provided in Table 8-9.

<span id=Table8-9></span>

<center> **Table 8-9. PROFILES file format description.** </center>

| **Line** | **Column**| **Name**|**Type**|**Description**|
|---|---|---|---|---|
|<center> 1 </center>|<center> A </center>|<center> Ozone concentration at Season 1, Latitude 1, Level 1 </center>|<center> Exp (E10.3) </center>|Ozone measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
||<center> B </center>|<center> Ozone concentration at Season 1, Latitude 1, Level 2 </center>|<center> Exp (E10.3) </center>|Ozone measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 127 </center>|<center> A </center>|<center> Ozone concentration at Season 1, Latitude 19, Level 1 </center>|<center> Exp (E10.3) </center>|Ozone measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
||<center> B </center>|<center> Ozone concentration at Season 1, Latitude 19, Level 2 </center>|<center> Exp (E10.3) </center>|Ozone measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 134 </center>|<center> A </center>|<center> Temperature profiles at Season 1, Latitude 1, Level 1 </center>|<center> Exp (E10.3) </center>|Temperature measurements as a function of season, latitude, and vertical level; units = K (required)|
||<center> B </center>|<center> Temperature profiles at Season 1, Latitude 1, Level 2 </center>|<center> Exp (E10.3) </center>|Temperature measurements as a function of season, latitude, and vertical level; units = K (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 260 </center>|<center> A </center>|<center> Temperature profiles at Season 1, Latitude 19, Level 1 </center>|<center> Exp (E10.3) </center>|Temperature measurements as a function of season, latitude, and vertical level; units = K (required)|
||<center> B </center>|<center> Temperature profiles at Season 1, Latitude 19, Level 2 </center>|<center> Exp (E10.3) </center>|Temperature measurements as a function of season, latitude, and vertical level; units = K (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 267 </center>|<center> A </center>|<center> Air density profiles at Season 1, Latitude 1, Level 1 </center>|<center> Exp (E10.3) </center>|Air density measurements as a function of month, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
||<center> B </center>|<center> Air density profiles at Season 1, Latitude 1, Level 2 </center>|<center> Exp (E10.3) </center>|Air density measurements as a function of month, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 393 </center>|<center> A </center>|<center> Air density profiles at Season 1, Latitude 19, Level 1 </center>|<center> Exp (E10.3) </center>|Air density measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
||<center> B </center>|<center> Air density profiles at Season 1, Latitude 19, Level 2 </center>|<center> Exp (E10.3) </center>|Air density measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 1596 </center>|<center> A </center>|<center> Air density profiles at Season 4, Latitude 19, Level 51 </center>|<center> Exp (E10.3) </center>|Air density measurements as a function of season, latitude, and vertical level; units = molecules cm<sup>-3</sup> (required)|
|<center> 1597 </center>|<center> A </center>|<center> Average Dobson Values at Latitude 1, Month 1 </center>|<center> Real </center>|Average Dobson value as a function of latitude and month (required)|
|<center> 1597 </center>|<center> B </center>|<center> Average Dobson Values at Latitude 2, Month 1 </center>|<center> Real </center>|Average Dobson value as a function of latitude and month (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 1608 </center>|<center> A </center>|<center> Average Dobson Values at Latitude 19, Month 12 </center>|<center> Real </center>|Average Dobson value as a function of latitude and month (required)|
|<center> 1609 </center>|<center> A </center>|<center> Air Temperature at Level 1 </center>|<center> Real </center>|Air temperature for a standard atmospheric profile; units = K (required)|
||<center> B </center>|<center> Air Density at Level 1 </center>|<center> Real or Exp </center>|Air Density for a standard atmospheric profile; units = molecules cm<sup>-3</sup>(required)|
||<center> C </center>|<center> Ozone Concentration at Level 1 </center>|<center> Real or Exp </center>|Ozone concentration for a standard atmospheric profile; units = molecules cm<sup>-3</sup>(required)|
||<center> D </center>|<center> Aerosol Attenuation at Level 1 </center>|<center> Real or Exp </center>|Aerosol attenuation coefficient for a standard atmospheric profile; units = km<sup>-1</sup>(required)|
|<center> 1659 </center>|<center> A </center>|<center> Air Temperature at Level 51 </center>|<center> Real </center>|Air temperature for a standard atmospheric profile, units = K (required)|
||<center> B </center>|<center> Air Pressure at Level 51 </center>|<center> Real or Exp </center>|Air pressure for a standard atmospheric profile, units = molecules cm<sup>-3</sup>(required)|
||<center> C </center>|<center> Ozone Concentration at Level 51 </center>|<center> Real or Exp </center>|Ozone concentration for a standard atmospheric profile, units = molecules cm<sup>-3</sup>(required)|
||<center> D </center>|<center> Aerosol Attenuation at Level 51 </center>|<center> Real or Exp </center>|Aerosol attenuation coefficient for a standard atmospheric profile; units = km<sup>-1</sup>(required)|

### TOMS: Total ozone mapping spectrometer data

Used by: JPROC

TOMS is the logical name for the ASCII data file containing total ozone column measurements in Dobson units taken by the Total Ozone Mapping Spectrometer instrument aboard the sun-synchronous polar orbiting Nimbus satellite. The data are presented for specific Julian dates as a function of location on the earth (latitude and longitude).

A detailed description of the file format is provided in Table 8-10. The files are fixed format.

<span id=Table8-10></span>

<center> '''Table 8-10 TOMS Data Profile ''' </center>

|**Line** | **Column**| **Name** | **Type**| **Description** |
|---|---|---|---|---|
|<center> 1 </center>|<center> A </center>|<center> Julian Day </center>|<center> Int </center>|Julian start day of the file, DDD; preceded by 6 blank spaces (required)|
||<center> B </center>|<center> Year </center>|<center> Int </center>|Start year of the file, YYYY; preceded by 9 blank spaces (required)|
|<center> 2 </center>||<center> Header </center>|<center> String </center>|80-character line describing the contents of the file (if omitted, needs line placeholder)|
|<center> 3 </center>||<center> Header </center>|<center> String </center>|80-character line describing the contents of the file (if omitted, needs line placeholder)|
|<center> 4 </center>|<center> A </center>|<center> TOMS Data </center>|<center> Int </center>|TOMS ozone measurements as a function of longitude and latitude, ###; line starts with a space, then space-delimited 25 values per line (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|

### O2ABS: Molecular oxygen absorption cross-section data

Used by: JPROC

O2ABS is the logical name for the ASCII data file containing absorption cross section and quantum yield data for O<sub>2</sub> photolysis. The data in this file are listed as a function of wavelength. This file follows the same format as the CSQY files described in Section 6.1.6.

### O3ABS: Ozone absorption cross-section data

Used by: JPROC

O3ABS is the logical name for the ASCII data file containing absorption cross section and quantum yield data for O<sub>3</sub> photolysis. The data in this file are listed as a function of wavelength. This file follows the same format as the CSQY files described in Section 6.1.6.

### InMetFiles: List of MM5 or WRF‑ARW output files

Used by: MCIP

MCIP can read MM5 (version 3) binary output files (MMOUT) and WRF‑ARW netCDF-based files to generate I/O API-formatted netCDF files for input to CMAQ and emissions modeling. For details about the format of the MMOUT files, visit the MM5 homepage at [<http://www.mmm.ucar.edu/mm5>](http://www.mmm.ucar.edu/mm5). For information about the format of the WRF output files, visit the WRF‑ARW homepage at [<http://www.mmm.ucar.edu/wrf/users>](http://www.mmm.ucar.edu/wrf/users).

### InTerFile: MM5 terrain file

Used by: MCIP

MM5 output file containing fractional land use data. This file is generated by the MM5 program TERRAIN.

### InSatFiles: GOES cloud data file

Used by: MCIP

[EPA: need a description of this file]

### BNDY_CONC_1: Boundary conditions

Used by: CCTM

CMAQ boundary condition data are of the BNDARY3 file type. Produced by the boundary condition processor, BCON, CCTM reads these data and correlates them with the interior data by the use of a pointer system. This pointer system designates the beginning location of the data in memory that start a new side of the domain (i.e., south, east, north, or west). [Figure 6‑1](#Figure6-1) illustrates this input data structure and the relationship of the boundary data to the interior (“main grid”) data within CMAQ modules.

Each species being modeled should be in the BNDY_CONC_1 file. If some modeled species are not contained in this file, the boundary condition for these species will default to the value 1 × 10<sup>‑30</sup>. The perimeter of the CMAQ domain is 1 cell wide, where the number of boundary cells = (2*NROW)+(2*NCOL)+4. [Figure 6-2](#Figure6-2) is a graphical example of the CMAQ boundary conditions file; the west and north boundaries have ozone values of 0.035 ppmV, while the east and south boundaries have values of 0.030 ppmV.

<center>
<Image:>

</center>
<center>
**Figure 6-1. Illustration of CMAQ boundary condition file**

</center>
<center>
<Image:>

</center>
<center>
**Figure 6-2. Graphical example of a CMAQ gridded boundary conditions file**

</center>
### INIT_CONC_1: Initial conditions

Used by: CCTM

The initial concentrations of each species being modeled must be input to CMAQ. The initial conditions input file type is GRDDED3 and does not vary with time. The file data are looped in this manner: by column, by row, by layer, by variable. Initial conditions files have the same structure as concentration files, so the predicted concentrations from the last hour of day 1 can be used to initialize the following day’s simulation. This gives CMAQ users the flexibility to segment simulations in any way they choose. [Figure 6-3](#Figure6-3) is a graphical example of the CMAQ initial conditions file. The file shows spatially varying data that can be used to initialize a following run beginning at the time shown (i.e., June 25, 1996 0:00:00).

<center>
<Image:>

</center>
<center>
**Figure 6-3. Graphical example of a CMAQ gridded initial conditions file**

</center>
### JTABLE: Photolysis rates look-up table

Used by: CCTM

Each of the gas-phase mechanisms in CMAQ contains photolysis reactions that require clear-sky reaction rates precomputed from kinetics data at various altitudes and latitude bands. The CMAQ program JPROC (Section 2.2.3) generates photolysis rate look-up tables for input to CMAQ. The photolysis files, called JTABLE, contain a header section that describes the contents of the file, and a data section with clear-sky photolysis rates at different times of the day.

The first line of the header contains the Julian date of the data in the file. This date corresponds to the start date for the simulation that uses the JTABLE data. This is followed by four pairs of data that describe the altitude (m), latitude (degrees), hour angle (from noon), and photolytic reaction name for the data contained in the file. Each data pair begins with a line describing the number of values for each variable and the variable name, followed by the values for each variable. The altitude (variable = LEVELS), latitude (variable = LATITUDES), and hour angle (variable = HOUR ANGLES) variables have fixed values that are hard-coded in the program JPROC (routine jproc.F). The reaction names (variable = PHOTOLYTIC REACTIONS) are mechanism-dependent and vary with the choice of gas-phase mechanism.

The data section of the file contains data blocks that are mapped to the header using a three-digit code. Each block corresponds to an altitude, latitude, and photolysis reaction and contains nine values of clear-sky photolysis rates for the nine hour angles listed in the header. The three-digit code maps the altitude/latitude/reaction number to the data block. For example, the data block that uses the code “3 1 2” corresponds to altitude 3, latitude 1, and reaction 2 as listed in the file header. A detailed description of the JTABLE file format is provided in [Table 8-11](Table8-11). The files are list-formatted.

<span id=Table8-11></span>

<center> **Table 8-11. JTABLE file format description** </center>

| **Line** | **Column** |**Name** | **Type** | **Description** |
|---|---|---|---|---|
|1|A|JVDATE|String|Julian date of the file; YYYYDDD (required)|
||B|Comment|String|Description of the Julian date field (optional)|
|2|A|JVHT|Int|Number of vertical levels covered by the data (required)|
||B|Level Field Indicator|String|The word “LEVELS” (required)|
| |C|Comment|String|Description of the level field; usually “(m)”, for meters (optional)|
|3|A|XZJV_1|Real|Height of level 1; units in m (required)|
||B|XZJV_2|Real|Height of level 2; units in m (required)|
|...|...|...|...|...|
|...|x|XZJV_x|Real|Height of level x; units in m (required)|
|4|A|JVLAT|Int|Number of latitudes covered by the data (required)|
||B|Latitude Field Indicator|String|The word “LATITUDES” (required)|
| |C|Comment|String|Description of the latitudes field; usually “(deg)”, for degrees (optional)|
|5|A|XLATJV_1|Real|Latitude 1; units in degrees (required)|
||B|XLATJV_2|Real|Latitude 2; units in degrees (required)|
|...|...|...|...|...|
|...|x|XLATJV_x|Real|Latitude x; units in degrees (required)|
|6|A|JVTMAX|Int|Number of hour angles covered by the data (required)|
||B|Hour Angle Field Indicator|String|The words “HOUR ANGLES” (required)|
| |C|Comment|String|Description of the hour angles field; usually “(from noon)”, for hours from noon (optional)|
|7|A|XHAJV_1|Real|Hour angle 1; units in hours from noon (required)|
||B|XHAJV_2|Real|Hour angle 2; units in hours from noon (required)|
|...|...|...|...|...|
|...|x|XHAJV_x|Real|Hour angle x; units in hours from noon (required)|
|8|A|JPHOT|Int|Number of photolytic reactions covered by the data (required)|
||B|Reaction Field Indicator|String|The words “PHOTOLYTIC REACTIONS” (required)|
| |C|Comment|String|Description of the reactions field (optional)|
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
| |B|NLATO|Int|Latitude cross-reference to header data (required)|
| |C|NPHOTO|Int|Photolysis reaction cross-reference to header data (required)|
|x+2|A|XJVAL_1|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 1 (required)|
| |B|XJVAL_2|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 2 (required)|
| |C|XJVAL_3|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 3 (required)|
|...|...|...|...|...|
||JVTMAX|XJVAL_(JVTMAX)|Real or Exp|Clear sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle corresponding to JVTMAX (required)|
|x + 3|A|NHTO|Int|Vertical level cross-reference to header data (required)|
| |B|NLATO|Int|Latitude cross-reference to header data (required)|
| |C|NPHOTO|Int|Photolysis reaction cross-reference to header data (required)|
|x+4|A|XJVAL_1|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 1 (required)|
| |B|XJVAL_2|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 2 (required)|
| |C|XJVAL_3|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 3 (required)|
|...|...|...|...|...|
||JVTMAX|XJVAL_(JVTMAX)|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle corresponding to JVTMAX (required)|
|...|...|...|...|...|

A sample of the important sections of a JTABLE file is shown below.

` 1999181 (yyyyddd) Julian Date for the file`
` 7 LEVELS (m)`
` 0.0 1000.0 2000.0 3000.0 4000.0 5000.0 10000.0`
` 6 LATITUDES (deg)`
` 10.0 20.0 30.0 40.0 50.0 60.0`
` 9 HOUR ANGLES (from noon)`
` 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0`
` 6 PHOTOLYTIC REACTIONS`
` 'NO2_CBIV88 ', 1.0`
` 'O3O1D_CBIV88 ', 1.0`
` 'HCHOmol_CBIV88 ', 1.0`
` 'HCHOrad_CBIV88 ', 1.0`
` 'ALD_CBIV88 ', 1.0`
` 'ACROLEIN ', 1.0`
` 1 1 1`
` 5.0964761E-01 4.9923715E-01 4.6422747E-01 4.0129572E-01 3.0394882E-01`
` 1.6590215E-01 3.2829735E-02 0.0000000E+00 0.0000000E+00`

### OMI: Ozone Monitoring Instrument Column Data

Used by: CCTM

OMI ozone column data by latitude and longitude for use in the inline photolysis calculations. CMAQv5 is distributed with ozone columns from 1978 to 2011. The data are 22.5°x10° gridded ozone columns in Dobson units. Table 8-12 lists the format of the OMI data file.

<span id=Table8-12></span>

<center> **Table 8‑12. OMI data format** </center>

| **Line** | **Column** | **Name** | **Type** | **Description**|
|---|---|---|---|---|
|<center> 1 </center>||<center> Header </center>||Header with names for each column|
|<center> 2 </center>|<center> A </center>|<center> Yeardate 1 </center>|<center> Real </center>|YYYY.YYY formatted date field.|
||<center> B </center>|<center> Latitude 1 </center>|<center> Int </center>|80 North latitude|
||<center> C </center>|<center> Longitude 1 </center>|<center> Int </center>|180.0Z longitude ozone column (DU)|
||<center> D </center>|<center> Longitude 2 </center>|<center> Int </center>|157.5W longitude ozone column (DU)|
 ||<center> E </center>|<center> Longitude 3 </center>|<center> Int </center>|135.0W longitude ozone column (DU)|
|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|
|<center> 2 </center>|<center> R </center>|<center> Longitude 16 </center>||<center> 157.5E longitude ozone column (DU) </center>|
|<center> 3 </center>|<center> A </center>|<center> Yeardate 1 </center>|<center> Real </center>|<center> YYYY.YYY formatted date field.</center>|
||<center> B </center>|<center> Latitude 2 </center>|<center> Int </center>|<center> 70 North latitude </center>|
||<center> C </center>|<center> Longitude 1 </center>|<center> Int </center>|<center> 180.0Z longitude ozone column (DU) </center>|
||<center> D </center>|<center> Longitude 2 </center>|<center> Int </center>|<center> 157.5W longitude ozone column (DU) </center>|
||<center> E </center>|<center> Longitude 3 </center>|<center> Int </center>|<center> 135.0W longitude ozone column (DU) </center>|
|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|
|<center> 3 </center>|<center> R </center>|<center> Longitude 16 </center>||<center> 157.5E longitude ozone column (DU) </center>|
|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|
|<center> 18 </center>|<center> A </center>|<center> Yeardate 1 </center>|<center> Real </center>|<center> YYYY.YYY formatted date field.  </center>|
||<center> B </center>|<center> Latitude 17 </center>|<center> Int </center>|<center> 80 South latitude </center>|
||<center> C </center>|<center> Longitude 1 </center>|<center> Int </center>|<center> 180.0Z longitude ozone column (DU) </center>|
||<center> D </center>|<center> Longitude 2 </center>|<center> Int </center>|<center> 157.5W longitude ozone column (DU) </center>|
||<center> E </center>|<center> Longitude 3 </center>|<center> Int </center>|<center> 135.0W longitude ozone column (DU) </center>|
|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|
|<center> 18 </center>|<center> R </center>|<center> Longitude 16 </center>||<center> 157.5E longitude ozone column (DU) </center>|
|<center> 19 </center>|<center> A </center>|<center> Yeardate 2 </center>|<center> Real </center>|<center> YYYY.YYY formatted date field </center>|
||<center> B </center>|<center> Latitude 1 </center>|<center> Int </center>|<center> 80 North latitude </center>|
||<center> C </center>|<center> Longitude 1 </center>|<center> Int </center>|<center> 180.0Z longitude ozone column (DU) </center>|
||<center> D </center>|<center> Longitude 2 </center>|<center> Int </center>|<center> 157.5W longitude ozone column (DU) </center>|
||<center> E </center>|<center> Longitude 3 </center>|<center> Int </center>|<center> 135.0W longitude ozone column (DU) </center>|
|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|<center> … </center>|
|<center> 19 </center>|<center> R </center>|<center> Longitude 16 </center>||<center> 157.5E longitude ozone column (DU) </center>|
| … |…|…|…| <center> Repeat for N years of data </center>|

### EMIS_1: Emissions

Used by: CCTM

CMAQ can accept emissions inputs from a variety of emissions models and preprocessors. The most commonly used option is the Sparse Matrix Operator Kernel Emissions (SMOKE) modeling system, which is a collection of programs that separately process and merge emissions data for each emissions sector for input to air quality models.

The emissions file sorts the emitted gas-phase and aerosol species by grid cell and time. The file type is GRDDED3, and the units are in moles per second (moles s<sup>‑1</sup>) for gas-phase species and grams per second (g s<sup>‑1</sup>) for aerosol species. The file data are looped as follows: by column, by row, by layer, by variable, and by input time step. CMAQ does not artificially distinguish between surface and elevated emissions sources; elevated sources are provided to CMAQ as vertically resolved emissions. For CCTM configurations that do not use in-line emissions calculations, all emissions estimates are contained within a single input emission file for each day. In v4.7, CMAQ now has the capability to process point-source, sea salt, and biogenic emissions in-line. The supplemental input files to use for calculating the in-line emissions are described i in the CMAQv4.7 release notes.

### OCEAN_1: Sea salt mask

Used by: CCTM

The CMAQ aerosol model AERO5 can compute sea salt emissions from both open ocean grid cells and surf zone grid cells. The addition of the surf zone option simulates the elevated emissions rates of sea salt in coastal regions where wave action occurs. The OCEAN_1 file contains data on the fraction of each grid cell that is either open ocean (OPEN) or in the surf zone (SURF). When CCTM is compiled with AERO5, it will expect the OCEAN_1 file as input.

### GSPRO: Speciation profiles

Used by: CCTM – inline emissions version only

The speciation profile file, GSPRO, contains the factors that are used to separate aggregated inventory pollutant emissions totals into emissions of model species in the form required by CMAQ. If only biogenic emissions are being calculated in-line in CMAQ, the GSPRO file used by CCTM needs to contain split factors only for the biogenic VOC emissions that are input in the B3GRD file. If other emissions sources are being calculated by CCTM, VOC split factors for these other sources must be included in the GSPRO file. The GSPRO file format is listed in the SMOKE user’s manual ([<http://www.smoke-model.org/version2.5/html/ch08s05s02.html>](http://www.smoke-model.org/version2.5/html/ch08s05s02.html)).

### B3GRD: Gridded, normalized biogenic emissions

Used by: CCTM – inline-emissions version only

An I/O API GRDDED3 file of gridded, normalized biogenic emissions (in grams of carbon or nitrogen per hour, depending on the species) and leaf area index. The B3GRD file contains normalized emissions calculated with both summer and winter emissions factors. The B3GRD file is generated with the SMOKE program NORMBEIS3 using gridded land use data. For additional information about creating the B3GRD file, see the NORMBEIS3 documentation in the SMOKE users’ manual ([<http://www.smoke-model.org/version2.5/html/ch06s11.html>](http://www.smoke-model.org/version2.5/html/ch06s11.html)).

### BIOSEASON: Freeze dates

Used by: CCTM – inline-emissions version only

The BIOSEASON switch file is an I/O API GRDDED3 file used to indicate which biogenic emissions factor to use on each day in a given year for every grid cell in the modeling domain. This file can be created using the Metscan utility program that is distributed with SMOKE. The BIOSEASON file is time-dependent and usually contains data for an entire year (365 or 366 days). It uses one variable, SEASON, which is either 0 (grid cell should use winter factors for current day) or 1 (grid cell should use summer factors for current day). For additional information about creating the BIOSEASON file, see the Metscan documentation in the SMOKE user’s manual ([<http://www.smoke-model.org/version2.5/html/ch05s11.html>](http://www.smoke-model.org/version2.5/html/ch05s11.html)).

### STK_GRPS_##: Stack groups

Used by: CCTM – in-line emissions version only

The ## mark is unique and represents the sector identification.

The stack groups file is an I/O API netCDF file containing stack parameters for elevated sources. This file can be created using the SMOKE program ELEVPOINT. For additional information about creating the stack groups file, see the ELEVPOINT documentation in the SMOKE user’s manual ([<http://www.smoke-model.org/version2.5/html/ch06s03.html>](http://www.smoke-model.org/version2.5/html/ch06s03.html)).

### STK_EMIS_##: Point source emissions

Used by: CCTM – inline emissions version only

The ## mark is unique and represents the sector identification.

The elevated-point-source emissions file is an I/O API GRDDED3 file with emissions for point sources to be treated as elevated sources by CCTM. The emissions in this file are distributed through the vertical model layers using a plume-rise algorithm contained in CCTM. The elevated-point-source emissions file can be creating using SMOKE. For additional information about preparing point-source emissions for using the CMAQ in-line plume rise calculation, see the ELEVPOINT documentation in the SMOKE user’s manual ([<http://www.smoke-model.org/version2.5/html/ch06s03.html>](http://www.smoke-model.org/version2.5/html/ch06s03.html)).

### DUST_LU_1: Gridded land cover/land use

Used by: CCTM – in-line dust emission version only

The gridded land cover/land use (LCLU) file is an I/O API GRDDED3 file of BELD3 data projected to the modeling domain. This file must contain the following LCLU variables to be compatible with the CMAQv5 dust module:

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

These categories are used to determine dust source locations and canopy scavenging factors for estimating dust emission in the model. This file can be created for North America using the Spatial Allocator and BELD3 tiles. The DUST_LU_1 file corresponds to the “a” output file from the Spatial Allocator. See the chapter on [creating biogenic inputs to SMOKE](http://www.ie.unc.edu/cempd/projects/mims/spatial/smoke_bio_inputs.html) of the Spatial Allocator User’s Guide for details.

### DUST_LU_2: Gridded land cover/land use

Used by: CCTM – in-line dust emission version only

The gridded land cover/land use (LCLU) file is an I/O API GRDDED3 file of BELD3 data projected to the modeling domain. This file must contain the following variables to be compatible with the CMAQv5 dust module:

-   FOREST

This variable is used in combination with the variables in the DUST_LU_1 file to determine canopy scavenging factors for estimating dust emission in the model. This file can be created for North America using the Spatial Allocator and BELD3 tiles. The DUST_LU_2 file corresponds to the “tot” output file from the Spatial Allocator. See the chapter on [creating biogenic inputs to SMOKE](http://www.ie.unc.edu/cempd/projects/mims/spatial/smoke_bio_inputs.html) of the Spatial Allocator User’s Guide for details

### CROPMAP01: Gridded planting start dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29) – in-line dust emission version with crops only

The gridded planting start dates file is an I/O API GRDDED3 file of planting start dates for various crops interpolated to the modeling domain. The variables in this file are planting start dates for different crop types, where each variable is an integer representing the number of days after January 1 that planting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor) reads a crop activity calendar and a [GRID_CRO_2D](#GRID_CRO_2D:_Two-dimensional_grid_cross-point_fields) file to generate the CROPMAP08 file.

### CROPMAP04: Gridded planting end dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29) – in-line dust emission version with crops only

The gridded planting end dates file is an I/O API GRDDED3 file of planting end dates for various crops interpolated to the modeling domain. The variables in this file are planting end dates for different crop types, where each variable is an integer representing the number of days after January 1 that planting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor) reads a crop activity calendar and a [GRID_CRO_2D](#GRID_CRO_2D:_Two-dimensional_grid_cross-point_fields) file to generate the CROPMAP08 file.

### CROPMAP08: Gridded harvesting end dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29) – in-line dust emission version with crops only

The gridded harvesting end dates file is an I/O API GRDDED3 file of harvesting end dates for various crops interpolated to the modeling domain. The variables in this file are harvesting end dates for different crop types, where each variable is an integer representing the number of days after January 1 that harvesting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor) reads a crop activity calendar and a [GRID_CRO_2D](#GRID_CRO_2D:_Two-dimensional_grid_cross-point_fields) file to generate the CROPMAP08 file.

### LTNGNO: Lightning NOx emissions

Used by: CCTM – lightning NO<sub>x</sub> version only

The lightning NO<sub>x</sub> emissions file is an I/O API GRDDED3 file with 3-d (row x col x layer) hourly NO emissions (moles/s) interpolated to the modeling domain. This is a lightning NO emissions file calculated off-line for input to CMAQ.

### LTNGPARM_FILE: Lightning parameters file

Used by: CCTM – lightning NO<sub>x</sub> version only

The lightning parameters file is used for calculating in-line NO emissions from lightning using convective precipitation rates. The LTNG_2D_DATA processor that is part of the CMAQv5 distribution package produces the I/O API GRDDED3 lightning parameters file. This file contains the following variables interpolated to the modeling grid:

-   STRKCNT (unitless): Lightning flash count
-   NLDNstrk (km<sup>2</sup>/day): Monthly flash totals (normalized to day<sup>-1</sup>) from NLDN input data
-   LTstrk (km<sup>2</sup>/day): Monthly flash totals (normalized to day<sup>-1</sup>) in each CMAQ grid cell calculated as the convective precipitation rate (RC) x STRKCNT
-   NLDNstrk (km<sup>2</sup>/day): Monthly flash totals from the input NLDN data
-   LTratio (unitless): Grid-cell scaling factors for calculating flashes using the convective precipitation rate. These scaling factors ensure that the calculated flash count matches the monthly total.
-   ICCG (unitless): Ratio of intercloud to cloud-to-ground flashes
-   OCNMASK (unitless): Land/water mask to remove spurious flashes over the ocean.
-   MOLSN (molN): moles of N per cloud-to-ground flash
-   MOLSNIC (molN): moles of N per intercloud flash

The lightning parameter file can be created from lightning network flash detections, MCIP output data and intercloud to cloud-to-ground ratios. The University of Maryland prepared National Lightning Detection Network (NLDN) flash frequencies for North American for the years 2002-2010. These data are available from the [CMAS Center Data Clearinghouse](ftp://ftp.unc.edu/pub/cmas/DATA/NLDN_CMAQ/).

### B4LU_file – Fractional crop distributions

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

Add content

### E2C_Soilfile – EPIC soil properties

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

Add content

### E2C_Fertfile – EPIC crop types and fertilizer application

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

Add content

### INIT_MEDC_1 – Soil initial conditions file

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

The ASXfile output for the previous day from the bidirectional NH<sub>3</sub> model is used to initialize the soil conditions for each simulation day.

### GRID_CRO_2D: Two-dimensional grid cross-point fields

Used by: CCTM

The GRID_CRO_2D time-independent file contains surface fields at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM. The following variables are in this file:

LAT:latitude (degrees, where Northern Hemisphere is positive)

LON:longitude (degrees, where Western Hemisphere is negative)

MSFX2:squared map-scale factor (m<sup>2</sup> m<sup>‑2</sup>)

HT:terrain elevation (m)

DLUSE:dominant land use (category)

LWMASKland-water mask (1=land, 0=water)

PURB:urban percentage if cell is based on land (percent)

### GRID_DOT_2D: Two-dimensional grid dot-point fields

Used by: CCTM

The GRID_DOT_2D time-independent file contains surface fields at dot points (i.e., at cell corners). It is created by MCIP and used by CCTM. The following variables are in the GRID_DOT_2D file:

LAT:latitude (degrees, where Northern Hemisphere is positive)

LON:longitude (degrees, where Western Hemisphere is negative)

MSFD2:squared map scale factor (m<sup>2</sup> m<sup>‑2</sup>)

### MET_BDY_3D: Three-dimensional meteorological boundary input

Used by: CCTM

The MET_BDY_3D time-dependent file contains 3-D meteorological descriptions at the lateral boundaries (on cross points). It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET_BDY_3D file:

JACOBF:total Jacobian at layer face (m)

JACOBM:total Jacobian at layer middle (m)

DENSA_J:Jacobian-weighted total air density [ J m<sup>‑2</sup>] (kg m<sup>‑2</sup>)

WHAT_JD:Jacobian- and density-weighted vertical contravariant velocity (kg m<sup>‑1</sup> s<sup>‑1</sup>)

TA:air temperature (K)

QV:water vapor mixing ratio (kg kg<sup>‑1</sup>)

<PRES:air> pressure (Pa)

DENS:air density (kg m<sup>‑3</sup>)

WWIND:vertical velocity (m s<sup>‑1</sup>)

ZH:midlayer height above ground (m)

ZF:full layer height above ground (m)

QC:cloud water mixing ratio (kg kg<sup>‑1</sup>)

QR:rain water mixing ratio (kg kg<sup>‑1</sup>)

QI:ice mixing ratio (kg kg<sup>‑1</sup>)

QS:snow mixing ratio (kg kg<sup>‑1</sup>)

QG:graupel mixing ratio (kg kg<sup>‑1</sup>)

### MET_CRO_2D: Two-dimensional meteorological cross-point fields

Used by: CCTM

The MET_CRO_2D time-dependent file contains surface and other 2-D meteorological fields at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET_CRO_2D file:

PRSFC:surface pressure (Pa)

JACOBS:total Jacobian at surface (m)

USTAR:cell-averaged horizontal friction velocity (m s<sup>‑1</sup>)

WSTAR:convective velocity scale (m s<sup>‑1</sup>)

PBL:planetary boundary layer height (m)

ZRUF:surface roughness length (m)

MOLI:inverse Monin-Obukhov length (m<sup>‑1</sup>)

QFX:latent heat flux (W m<sup>‑2</sup>)

HFX:sensible heat flux (W m<sup>‑2</sup>)

RADYNI:inverse aerodynamic resistance (m s<sup>‑1</sup>)

RBNDYI:inverse laminar boundary layer resistance (m s<sup>‑1</sup>)

RSTOMI:inverse bulk stomatal resistance (m s<sup>‑1</sup>)

TEMPG:skin temperature at ground (K)

TEMP10:10‑m temperature (K)

TEMP1P5:1.5‑m temperature (K)

WSPD10:10‑m wind speed (m s<sup>‑1</sup>)

WDIR10:10‑m wind direction (m s<sup>‑1</sup>)

GLW:longwave radiation at ground (W m<sup>‑2</sup>)

GSW:solar radiation absorbed at ground (W m<sup>‑2</sup>)

RGRND:solar radiation reaching the surface (W m<sup>‑2</sup>)

RN:incremental (per output time step) nonconvective precipitation (cm)

RC:incremental (per output time step) convective precipitation (cm)

CFRAC:total cloud fraction (fraction)

WBAR:average liquid water content of clouds (g m<sup>‑3</sup>)

CLDT:cloud-top layer height (m)

CLDB:cloud-bottom layer height (m)

SNOCOV:snow cover (1 = yes, 0 = no)

TEMP2:2‑m temperature (K)

SOIM1:volumetric soil moisture in top cm (m<sup>3</sup> m<sup>‑3</sup>)

SOIM2:volumetric soil moisture in top m (m<sup>3</sup> m<sup>‑3</sup>)

SOIT1:soil temperature in top cm (K)

SOIT2:soil temperature in top m (K)

SLTYP:soil texture type (category)

LAI:leaf-area index (area area<sup>‑1</sup>)

The following deposition velocities are calculated by MCIP3 by default and written to the MET_CRO_2D file:

VD_SO2:deposition velocities for SO<sub>2</sub> (m s<sup>‑1</sup>)

VD_SULF:deposition velocities for SO<sub>4</sub> (m s<sup>‑1</sup>)

VD_NO2:deposition velocities for NO<sub>2</sub> (m s<sup>‑1</sup>)

VD_NO: deposition velocities for NO (m s<sup>‑1</sup>)

VD_O3:deposition velocities for O<sub>3</sub> (m s<sup>‑1</sup>)

VD_HNO3:deposition velocities for HNO<sub>3</sub> (m s<sup>‑1</sup>)

VD_H2O2:deposition velocities for H<sub>2</sub>O<sub>2</sub> (m s<sup>‑1</sup>)

VD_ALD:deposition velocities for ALD (m s<sup>‑1</sup>)

VD_HCHO:deposition velocities for HCHO (m s<sup>‑1</sup>)

VD_OP:deposition velocities for OP (m s<sup>‑1</sup>)

VD_PAA:deposition velocities for PAA (m s<sup>‑1</sup>)

VD_ORA:deposition velocities for ORA (m s<sup>‑1</sup>)

VD_NH3:deposition velocities for NH<sub>3</sub> (m s<sup>‑1</sup>)

VD_PAN:deposition velocities for PAN (m s<sup>‑1</sup>)

VD_HONO:deposition velocities for HONO (m s<sup>‑1</sup>)

VD_CO:deposition velocities for CO (m s<sup>‑1</sup>)

VD_METHANOL:deposition velocities for methanol (m s<sup>‑1</sup>)

VD_N2O5:deposition velocities for N<sub>2</sub>O<sub>5</sub> (m s<sup>‑1</sup>)

VD_NO3:deposition velocities for NO<sub>3</sub> (m s<sup>‑1</sup>)

VD_GEN_ALD:deposition velocities for generic aldehyde (m s<sup>‑1</sup>)

VD_CL2:deposition velocities for CL2 (m s<sup>‑1</sup>)

VD_HOCL:deposition velocities for HOCL (m s<sup>‑1</sup>)

VD_HCL:deposition velocities for HCL (m s<sup>‑1</sup>)

VD_FMCL:deposition velocities for FMCL (m s<sup>‑1</sup>)

VD_ICL1:deposition velocities for ICL1 (m s<sup>‑1</sup>)

VD_ICL2:deposition velocities for ICL2 (m s<sup>‑1</sup>)

VD_HG:deposition velocities for HG (m s<sup>‑1</sup>)

VD_HGIIGAS:deposition velocities for HGIIGAS (m s<sup>‑1</sup>)

### MET_CRO_3D: Three-dimensional meteorological cross-point fields

Used by: CCTM, ICON, BCON

The MET_CRO_3D time-dependent file contains 3-D meteorological descriptions at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM, ICON, BCON, and PDM. The variables that may exist in MET_CRO_3D are the same as those that may be in MET_BDY_3D.

### MET_DOT_3D: Three-dimensional meteorological dot-point fields

Used by: CCTM

The MET_DOT_3D time-dependent file contains 3-D meteorological descriptions at dot points (i.e., at cell corners) and at cell faces. It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET_DOT_3D file:

UWIND:u-component of horizontal wind (m s<sup>‑1</sup>) [dot points; Arakawa-B grid]]

VWIND:v-component of horizontal wind (m s<sup>‑1</sup>) [dot points; Arakawa-B grid]

UHAT_JD:contravariant-U*Jacobian*density (kg m<sup>‑1</sup> s<sup>‑1</sup>) [cell faces; Arakawa-C grid]

VHAT_JD:contravariant-V*Jacobian*density (kg m<sup>‑1</sup> s<sup>‑1</sup>) [cell faces; Arakawa-C grid]

Basic CCTM Output Files
-----------------------

The previous section described the output files from JPROC, ICON, BCON, and MCIP that are input to CCTM. In this section, details on the CCTM output files are provided. Except for JPROC (which creates ASCII files), all CMAQ programs produce output files that adhere to the I/O API netCDF format (Chapter 4). The I/O API-formatted CMAQ output files are three-dimensional, gridded, time-stepped binary files that contain headers with metadata describing the file contents. These machine-independent and network transparent binary files are transferable between different computer architectures. In addition to model data output, CMAQ can optionally produce log files that contain the standard output from the various CMAQ processors. If the log file option is not selected by the user, CMAQ will write all of the log information to the screen along with the standard error, which can be captured to a text file using basic UNIXsyntax.

### CMAQ output log

All of the CMAQ processors generate standard output and standard error during execution. For all of the processors other than CCTM, this diagnostic output information can be captured to a log file at execution using a UNIX redirect command. For example, to capture the standard output and error of a BCON simulation, use the following command:

run.bcon >& bcon_e1a.log

For CCTM, the LOGFILE environment variable allows users to specify the name of a log file for capturing the standard output from the program. If this variable is not set, the standard output is written to the terminal and can be captured using the UNIXredirect command (“>”), as shown in the example above.

### CONC: CCTM hourly instantaneous concentration file

The 3-D CCTM hourly concentration file (CONC) is the most commonly referenced CCTM output file. Containing gas-phase species mixing ratios (ppmV) and aerosol species concentra­tions (µg m<sup>‑3</sup>), CONC files include instantaneous model species concentrations at the end of each model hour. The number and types of species contained in the CONC files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. The species concentration INCLUDE files (CONC.EXT) within the mechanism INCLUDE directories list the species that are written to the CONC files for each mechanism configuration. The GC_CONC.EXT file lists the gas-phase species, the AE_CONC.EXT file lists the aerosol species, and the NR_CONC lists the nonreactive (inert) species written to the CONC file. Species can be removed from the CONC.EXT files to reduce the number of species that are written to, and thus the size of, the CONC file.

### CGRID: CCTM restart file

The 3-D CCTM ending concentration file (CGRID) is the CCTM restart file. Containing gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>‑3</sup>), the CGRID file includes model species concentrations at the end of each simulation period. The number and types of species contained in the output CGRID files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. This file can be used to initialize CCTM from the simulation period that the model completed. For example, if the CCTM is configure to produce daily output files, a CGRID file will be written out at the end of each simulation day.

### ACONC: CCTM hourly average concentration file

The 3-D CCTM integral average concentration file (ACONC) contains average model species concentrations for each model hour, as opposed to instantaneous concentrations at the end of each output time step. The species written to the ACONC file are set by the user in the CCTM run script using the variable AVG_CONC_SPCS. The model layers that are used to calculate the integral average concentration are also set in the CCTM run script using the variable ACONC_BLEV_ELEV, where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number. An example setting for the ACONC_BLEV_ELEV variable is “1 6”, which defines layers 1 through 6 as the vertical extent over which to calculate hourly average concentrations.

### DRYDEP: CCTM hourly cumulative dry deposition file

The 2-D CCTM dry deposition file (DRYDEP) includes cumulative hourly dry deposition fluxes (kg hectare<sup>‑1</sup>) for selected model species. CCTM calculates dry deposition for all of the species listed in the dry deposition INCLUDE'' *files within the mechanism INCLUDE directories. Dry deposition INCLUDE* ''files exist for gas-phase species (GC_DDEP.EXT), aerosol species (AE_DDEP.EXT), and inert model species (NR_DDEP.EXT). Species can be removed from the DDEP.EXT files to adjust the number of species that undergo the dry deposition process and are written to the DRYDEP output file.

### WETDEP: CCTM hourly cumulative wet deposition file

The 2-D CCTM wet deposition file (WETDEP) includes cumulative hourly wet deposition fluxes (kg hectare<sup>‑1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition INCLUDE files within the mechanism INCLUDE directories. Wet deposition INCLUDE files exist for gas-phase species (GC_WDEP.EXT), aerosol species (AE_WDEP.EXT), and inert model species (NR_WDEP.EXT). Species can be removed from the WDEP.EXT files to adjust the number of species that undergo the wet deposition process and are written to the WETDEP output file.

### AEROVIS: CCTM hourly instantaneous visibility metrics

The 2-D CCTM visibility file (AEROVIS) contains hourly Mie and reconstructed visual range coefficients (km<sup>‑1</sup>) and normalized extinction coefficients (deciviews).

Diagnostic and Advanced CMAQ Output Files
-----------------------------------------

Along with the basic outputs detailed in the previous section, CMAQ can be configured to output several auxiliary files for diagnosing model performance.

### AERODIAM: Instantaneous hourly aerosol diameter file

This diagnostic file contains information on the geometric mean diameters and geometric standard deviations for the lognormal modes.

### B3GTS_S: Biogenic emissions diagnostic file

This optional 2-D CCTM hourly output file contains calculated biogenic emissions in mass units. The B3GTS_S file will be produced only if in-line biogenic emissions are being calculated by CCTM and if the B3GTS_DIAG variable is turned on.

### DEPV_DIAG: CCTM inline deposition diagnostics file

Add content

### DUST_EMIS

Add content

### FLOOR: concentration-reset diagnostics file

FLOOR files are optional output diagnostic files which list specific gridboxes/timesteps in which species with `-ve` concentrations are reset to zero.

### INIT_MEDC_1

Add content

### IRR: Process analysis output – integrated reaction rates

The 3-D CCTM integrated reaction rate file (IRR) contains hourly concentrations of selected model output species in terms of the gas-phase chemistry pathways that contributed to the predicted concentration at each hour. For each grid cell in the process analysis domain (which is most likely a subset of the full modeling domain), the IRR file shows the hourly change in species concentration that is due to particular gas-phase chemistry reactions or reaction groups. The process analysis preprocessor, PROCAN (Section 2.2.6), is used to select the process analysis domain, the model species for which to capture process analysis information, and the chemistry reactions or groups of reactions to track during the process analysis.

### LTNGOUT

Add content

### PA: Process analysis output – integrated process rate file

The 3-D CCTM integrated process rate file (PA) contains hourly concentrations of selected model output species in terms of the model process that contributed to the concentration in each grid cell at each hour. For each grid cell in the process analysis domain (which is most likely a subset of the full modeling domain), the PA file shows the hourly change in species concentration that is due to the major model processes, such as horizontal and vertical advection, chemistry, and wet deposition. The process analysis preprocessor, PROCAN (Section 2.2.6), is used to select the process analysis domain, the model species for which to capture process analysis information, and the model processes to track during the process analysis.

### PLAY_SRCID

Add content

### PT3D_DIAG

Add content

### RJ: In-line photolysis output – gridded photolysis rates

The photolysis diagnostic output files (RJ) contain the photolysis rates calculated by CCTM when the in-line photolysis option is used.

### SOILOUT

Name and location of hourly soil NO emissions file; output when in-line biogenic emissions processing is activated by setting CTM_BIOGEMIS to “T” or “Y”.

### SSEMIS: Sea salt emissions diagnostic file

This optional 2-D CCTM hourly output file contains calculated sea salt emissions. The SSEMIS file will be produced by CCTM only if the AERO5 aerosol mechanism is being used and if the CTM_SSEMDIAG variable is turned on.

### WETDEP2: CCTM cloud diagnostics file

The 2-D CCTM wet deposition file (WETDEP2) includes cumulative hourly wet deposition fluxes (kg hectare<sup>‑1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition INCLUDE files within the mechanism INCLUDE directories. Wet deposition INCLUDE files exist for gas-phase species (GC_WDEP.EXT), aerosol species (AE_WDEP.EXT), and inert model species (NR_WDEP.EXT). Species can be removed from the WDEP.EXT files to adjust the number of species that undergo the wet deposition process. These extra species are written to the WETDEP2 output file.

In CMAQ, wet deposition is calculated separately for resolved (grid-scale) clouds and for convective (subgrid) clouds. The WETDEP1 files contain the total wet deposition, i.e., the sum of both resolved-scale and subgrid-scale deposition. The WETDEP2 file contains only subgrid-scale deposition, plus some cloud diagnostic variables.


[<< Previous Chapter](CMAQ_OGD_ch07_programs_libraries.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch09_grid_defn.md)<br>
CMAQ Operational Guidance Document (c) 2016<br>
