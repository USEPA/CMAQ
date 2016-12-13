[Previous: Libraries](CMAQ_OGD_ch07_programs_libraries.md)
<span id=Section8.0></span> <span id=Section8></span>
CMAQ FILES
==========

The input files for CMAQv5 consist of a domain definition file for all programs; two sets of file options for both ICON and BCON; two types of input files (WRF/MM5 and terrain) for MCIP; five mandatory and one optional input file for JPROC; and for CCTM, emissions, initial conditions, and boundary conditions files, six files that define the meteorological conditions to be simulated, and a photolysis rates file. For most CCTM input, a separate data set is required for each horizontal domain that is modeled. When CMAQv5 is configured for in-line emissions and deposition, there are additional emissions input files that are required. CMAQ output files include a basic set of files with aerosol and gas-phase species concentrations, wet and dry deposition estimates, and visibility metrics, and an auxiliary set of output files for diagnosing model performance and in-line-calculated emissions.

CMAQ Input Files
----------------

This section describes each of the input files required by the various CMAQ programs. The section begins with a description of the grid definition file, which is used by several CMAQ programs, and then goes through a program-by-program listing of the CMAQ input file requirements. [Table 8‑1](#Table8-1 "wikilink") lists the source, file type, and temporal and spatial dimensions of each CMAQ input file. Sample disk space requirements for a desired input data set can easily be calculated from the information in [Table 8‑1](#Table8-1 "wikilink"); each data record is four bytes. The I/O API file sizes can be calculated using the number of variables in a CMAQ file and the spatial and temporal coverage of the data. The user should consult the CMAQv5 release notes for additional file information.

<span id=Table8-1></span>

<center>**Table 8‑1. CMAQ input files**</center>

|**File Name**|**File Type**|**Time-Dependence**|**Spatial Dimensions**|**Source*|
|---|---|---|---|---|
|**General**| | | | |
|GRIDDESC (horizontal domain definition)|ASCII|n/a|n/a|user/MCIP
|gc\_matrix.nml|ASCII|n/a|n/a|user/CSV2NML
|ae\_matrix.nml|ASCII|n/a|n/a|user/CSV2NML
|nr\_matrix.nml|ASCII|n/a|n/a|user/CSV2NML
|tr\_matrix.nml|ASCII|n/a|n/a|user/CSV2NML
|**ICON**| | | | |
|IC\_PROFILE (initial conditions vertical profiles)|ASCII|Annual|n/a|user|
|CTM\_CONC\_1 (CCTM concentration files)|GRDDED3|Hourly|X\*Y\*Z|CCTM|
|MET\_CRO\_3D (3‑D meteorological cross-point fields)|GRDDED3|Hourly|X\*Y\*Z|MCIP
|**BCON**| | | | |
|BC\_PROFILE (boundary conditions vertical profiles)|ASCII|Annual|n/a|user
|CTM\_CONC\_1 (CCTM concentration files)|GRDDED3|Hourly|X\*Y\*Z|CCTM
|MET\_CRO\_3D (3‑D meteorological cross-point fields)|GRDDED3|Hourly|X\*Y\*Z|MCIP|
|**JPROC**| | | | |
|ET (extraterrestrial irradiance)|ASCII|Annual|n/a|user
|PROFILES (default atmospheric profiles)|ASCII|Annual|n/a|user
|O2ABS (O2 absorption)|ASCII|Annual|n/a|user
|O3ABS (O3 absorption)|ASCII|Annual|n/a|user
|TOMS (total ozone mapping spectrometer data)|ASCII|varies|n/a|user
|CSQY (absorption cross section and quantum yields)|ASCII| Annual|n/a| User
|**MCIP**| | | | |
|InMetFiles (list of MM5 or WRF‑ARW output files)|Binary or netCDF| typically hourly, but sometimes sub-hourly| X\*Y\*Z| MM5 or WRF‑ARW
|InTerFile (MM5 terrain file)|Binary| n/a| X\*Y | MM5
|InSatFiles||||| |
|**CCTM** | | | | |
|INIT\_CONC\_1 (initial conditions)| GRDDED3 | Time-invariant | X\*Y\*Z | ICON/CCTM
|BNDY\_CONC\_1 (boundary conditions)| BNDARY3 | Hourly |[2(X+1)+2(Y+1)]\*Z | BCON
|JTABLE (photolysis rates look-up table)| ASCII | Daily | n/a | JPROC
|OMI| ASCII | Annual | n/a ||
|EMIS\_1 (Emissions)| GRDDED3 | Hourly | X\*Y\*Z | SMOKE
|OCEAN\_1 (sea salt mask)| GRDDED3 | Time-invariant | X\*Y | |Spatial Allocator
|GSPRO (speciation profiles)| ASCII | Time-invariant | N/a | User
|B3GRD (grid-normalized biogenic emissions)| GRDDED3 | Time-invariant | X\*Y | SMOKE
|BIOSEASON (freeze dates)|GRDDED3 |Time-invariant | X\*Y | Metscan
|STK\_GRPS\_\#\# (stack groups)| GRDDED3 |Time-invariant|X\*Y | SMOKE
|STK\_EMIS\_\#\# (point-source emissions)| GRDDED3 | Hourly | X\*Y | SMOKE
|DUST\_LU\_1|GRDDED3 | Time-invariant | X\*Y | Spatial Allocator
|DUST\_LU\_2|GRDDED3 | Time-invariant | X\*Y | Spatial Allocator
|CROPMAP01| GRDDED3 | Time-invariant | X\*Y | Cropcal
|CROPMAP04| GRDDED3 | Time-invariant | X\*Y | Cropcal
|CROPMAP08|GRDDED3 | Time-invariant | X\*Y | Cropcal
|LTNGNO|<center> GRDDED3 </center>|<center> Hourly </center>|<center> X\*Y\*Z </center>|<center> User
|LTNGPARM\_FILE| GRDDED3 | Monthly | X\*Y |LTNG\_2D\_DATA
|B4LU\_file|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> X\*Y
|E2C\_Soilfile|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> X\*Y
|E2C\_Fertfile|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> X\*Y
|INIT\_MEDC\_1|<center> GRDDED3 </center>||<center> X\*Y </center>||
|GRID\_CRO\_2D (2‑D grid cross-point fields)|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> X\*Y </center>|<center> MCIP
|GRID\_DOT\_2D (2‑D grid dot-point fields)|<center> GRDDED3 </center>|<center> Time-invariant </center>|<center> (X+1)\*(Y+1) </center>|<center> MCIP
|MET\_BDY\_3D (3‑D meteorological boundary input)|<center> BNDARY3 </center>|<center> Hourly </center>|<center> PERIM\*Z </center>|<center> MCIP
|MET\_CRO\_2D (2‑D meteorological cross-point fields)|<center> GRDDED3 </center>|<center> Hourly </center>|<center> X\*Y </center>|<center> MCIP
|MET\_CRO\_3D (3‑D meteorological cross-point fields)|<center> GRDDED3 </center>|<center> Hourly </center>|<center> X\*Y\*Z </center>|<center> MCIP
|MET\_DOT\_3D (3‑D meteorological dot-point fields)|<center> GRDDED3 </center>|<center> Hourly </center>|<center> (X+1)\*(Y+1)\*Z </center>|<center> MCIP
</center>|

### GRIDDESC: Horizontal domain definition

Used by: ICON, BCON, CCTM

The CMAQ grid description file (**GRIDDESC**) is used by all programs except JPROC and MCIP to define the horizontal spatial grid of the modeling domain. GRIDDESC implements [I/O API](#Input.2FOutput_Applications_Programming_Interface_.28I.2FO_API.29 "wikilink") grid conventions: for more details see the section on [Grids and coordinate systems](#Grids_and_coordinate_systems "wikilink").

A GRIDDESC is an ASCII file that contains two sections: a horizontal coordinate section, and grid description section. GRIDDESC is the logical name for text files that store horizontal coordinate and grid descriptions, and that are read by the DSCGRID() and DSCOORD() utility routines. Each segment has a one-line header (which by convention provides titles for the columns in the data records), a sequence of data records, and a terminal record with name field blank (i.e., ' '). The GRIDDESC file is generated automatically with MCIP; alternatively, GRIDDESC can be created using a text editor.

The horizontal coordinate section ([Table 8-2](#Table8-2 "wikilink")) consists of text records that provide coordinate-system name, the map projection, and descriptive parameters P\_ALP, P\_BET, P\_GAM, XCENT, and YCENT.

The grid description section ([Table 8-3](#Table8-3 "wikilink")) consists of text records that indicate the grid name, related coordinate-system name (i.e., which GRIDDESC horizontal coordinate name that is defined in the previous section that is applied to this grid), and descriptive parameters XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, and NTHIK. For a typical CMAQ application, both "dot-point" and "cross-point" grids are defined in the GRIDDESC file; these grids are topological duals in the sense that the vertices (corners) of one correspond to the cell-centers of the other.

<span id=Table8-2></span>

<center> **Table 8‑2. Coordinate sytem description segment of GRIDDESC**</center>

| **Line**| **Column**| **Name** | **Type** | **Description**|
|---|---|---|---|---|
|1|A|<center> Header </center>|<center> String </center>|Single-quote-delimited header describing section contents; may be blank, i.e., ' '|
|2|A|<center> COORD-NAME </center>|<center> String </center>|Name of the coordinate description (required); single quote delimited|
|3|A|<center> COORDTYPE </center>|<center> Int </center>|I/O API index defining the map projection type (required)| |<center>
|3|B| P\_ALP </center>|<center> Double </center>|First map projection descriptive parameter (dependent on projection type)| |<center>
|3|C| P\_BET </center>|<center> Double </center>|Second map projection descriptive parameter (dependent on projection type)| |<center>  
|3|D| P\_GAM </center>|<center> Double </center>|Third map projection descriptive parameter (dependent on projection type)| |<center>  
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

The horizontal coordinate section (first section) in this example GRIDDESC file defines a horizontal coordinate named “LAM\_40N100W”. The coordinate definition is for a Lambert conformal grid, keyed by the first column of the coordinate description line, which corresponds to the numeric code for the various I/O API-supported grid types (2 = Lambert). The next three parameters (P\_ALP, P\_BET, and P\_GAM) have different definitions for different map projections. For Lambert conformal, P\_ALP and P\_BET are the true latitudes of the projection cone (30°N and 60°N in the example), and P\_GAM (100°W in the example) is the central meridian of the projection. The last two parameters, XCENT and YCENT, are the latitude-longitude coordinates of the grid center of the Cartesian coordinate system, which are 100°W and 40°N in the example. If using WRF-ARW as the meteorological model, the user should be aware of differences from this method.

The example grid definition section above describes a grid named “M\_32\_99TUT02”. The definition of the grid begins with a reference to a coordinate name from the coordinate definition section of the file; in this example, the coordinate named “LAM\_40N100W” is referenced in the grid definition. The next two parameters in the grid definition (XORIG and YORIG) are the east-west and north-south offsets from XCENT and YCENT in meters (WRF-ARW usages may differ). The next two parameters (XCELL and YCELL) are the horizontal grid spacing in meters for the X and Y directions (i.e., delta‑x and delta‑y). The next two parameters (NCOLS and NROWS) are the numbers of grid cells in the X and Y directions. The grid definition concludes with the number of boundary cells, NTHIK, which is typically set to 1.

### [gc|ae|nr|tr]\_matrix.nml: Species namelist files

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
|<center> 1 </center>||<center> File type </center>|<center> String </center>|&GC\_nml|
|<center> 2 </center>||<center> Number of surrogate params </center>|<center> String </center>|n\_surr1 = x, where x is the number of ????|
|<center> 3 </center>||<center> Number of ???? params </center>|<center> String </center>|n\_surr2 = x, where x is the number of ????|
|<center> 4 </center>||<center> Number of control params </center>|<center> String </center>|n\_ctrl = x, where x is the number of ????|
|<center> 5 </center>||<center> Header ID </center>|<center> String </center>|TYPE\_HEADER =|
|<center> 6 </center>||<center> HEADER </center>|<center> String </center>|Abbreviated names of file columns, enclosed by single quotes|
|<center>  7 </center>||<center> Matrix ID </center>|<center> String </center>|TYPE\_MATRIX =|
|<center> 8 </center>|<center> 1 </center>|<center> SPC </center>|<center> String </center>|CMAQ pollutant name, i.e. NO, HNO3, PAR; dependent on chemical mechanism|
||<center> 2 </center>|<center> MOLWT </center>|<center> Integer </center>|Pollutant molecular weight|
||<center> 3 </center>|<center> EMIS\_SUR </center>|<center> String </center>|Emissions species name for the CMAQ pollutant|
||<center> 4 </center>|<center> EMIS\_FAC </center>|<center> Real </center>|Scaling factor for input emissions|
||<center> 5 </center>|<center> DEPV\_SUR </center>|<center> String </center>|Deposition velocity variable name for the CMAQ pollutant|
||<center> 6 </center>|<center> DEPV\_FAC </center>|<center> Real </center>|Scaling factor for the deposition velocity|
||<center> 7 </center>|<center> ICBC\_SUR </center>|<center> String </center>|IC/BC species name for the CMAQ pollutant|
||<center> 8 </center>|<center> ICBC\_FAC </center>|<center> Real </center>|Scaling factor for the IC/BC concentration|
||<center> 9 </center>|<center> SCAV\_SUR </center>|<center> String </center>||
||<center> 10 </center>|<center> SCAV\_FAC </center>|<center> Real </center>||
||<center> 11 </center>|<center> G2AE\_SUR </center>|<center> String </center>|Gas-to-aerosol transformation species|
||<center> 12 </center>|<center> G2AQ\_SUR </center>|<center> String </center>|Gas-to-aqueous transformation species|
||<center> 13 </center>|<center> TRNS </center>|<center> Yes/No </center>|Transport switch|
||<center> 14 </center>|<center> DDEP </center>|<center> Yes/No </center>|Dry deposition output file switch|
||<center> 15 </center>|<center> WDEP </center>|<center> Yes/No </center>|Wet deposition output file switch|
||<center> 16 </center>|<center> CONC </center>|<center> Yes/No </center>|Concentration output file switch|
| … | ...| ...|... | Repeat for the number of gas-phase pollutants in the mechanism being modeling|

The namelist files for the other pollutant classes have similar configurations as the gas-phase species configuration shown in [Table 8-4](#Table8-4 "wikilink"). See existing namelist files in the CMAQv5 distribution for examples.

### IC\_PROFILE: Initial conditions vertical profiles

Used by: ICON

ICON can generate initial conditions from two different input file types. The first file type is an ASCII vertical profile file that lists species concentrations at various model layers that are fixed in space and time. To configure ICON to generate initial conditions from ASCII vertical profiles, the “prof” input module is chosen when compiling the program (see Section 5.5 on ICON). These ASCII-formatted vertical profile files are IC\_PROFILE files, and are described in this section. IC\_PROFILE files must be developed by the user and can be generated from climatologically averaged observational data or as an a priori estimate from previous modeling studies of the region being modeled. The second file type that ICON can use to generate initial conditions is a concentration file from a previous CMAQ run. These are CTM\_CONC\_1 files, and are described later in Section 6.1.5.

IC\_PROFILE begins with a header that contains a comment section that describes the data, and a file description section that defines the number of vertical levels in the file, the number of pollutants in the file, and the distribution of the vertical levels. The next entries in IC\_PROFILE are the Julian start date and the start time of the data; they are not used by ICON.

Each line in IC\_PROFILE corresponds to a different pollutant and begins with the name of the pollutant. The subsequent columns on each line list the chemical concentration at each layer contained in the file. Gas-phase species are in ppmV, and aerosol species are in μg m<sup>‑3</sup>. The layer structure of the IC\_PROFILE vertical profiles does not need to correspond exactly to the layer structure that will be modeled; the ICON program will interpolate the data to the correct vertical format for the simulation.

Initial conditions are provided for only the first hour of a model simulation. The initial conditions that are based on an ASCII vertical profile include a gridded file for input to CCTM that has horizontally uniform species concentrations at each model layer. For spatially resolved initial conditions in the horizontal direction, it is necessary to use the other input file type to ICON, an existing CCTM concentration file (CTM\_CONC\_1).

A detailed description of the vertical profile file format for initial conditions is provided in [Table 8-5](#Table8-5 "wikilink"). The header of the profiles file is list-formatted, while the data section of the file is fixed format.

<a id=Table8-5></a>

<center> **Table 8-5. IC\_PROFILE format description** </center>

|**Line**|**Column**|**Name**|**Type**|**Description**|
|---|---|---|---|---|
|<center> 1-3 </center>||<center> Text Header </center>|<center> String </center>|Text description of the contents and source of the initial conditions file (optional)|
|<center> 4 </center>|<center> A </center>|<center> NUM\_SIGMA\_LVL </center>|<center> Int </center>|Number of sigma levels contained in the file (required)|
||<center> B </center>|<center> NUM\_POLL </center>|<center> Int </center>|Number of pollutants contained in the file (required)|
||<center> C </center>|<center> SIGMA\_LVL </center>|<center> Real </center>|Vertical coordinate values of sigma-p levels; number of values (n+1) is one more than the NUM\_SIGMA\_LVL (n) (required)|
|<center> 4 </center>|<center> n </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 5 </center>|<center> A </center>|<center> STDATE </center>|<center> String </center>|Julian start date of the file, YYYYDDD (optional)|
||<center> B </center>|<center> STTIME </center>|<center> String </center>|Start time of the file, HH (optional)|
|<center> 6 </center>|<center> 1-10 </center>|<center> SPECIES1 </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
||<center> 12-20 </center>|<center> LAYER1\_IC </center>|<center> Exp </center>|IC concentration for species 1 in lowest sigma layer (required)|
||<center> 23-31 </center>|<center> LAYER2\_IC </center>|<center> Exp </center>|IC concentration for species 1 in 2nd sigma layer (required)|
||<center> 34-42 </center>|<center> LAYER3\_IC </center>|<center> Exp </center>|IC concentration for species 1 in 3rd sigma layer (required)|
||<center> 45-53 </center>|<center> LAYER4\_IC </center>|<center> Exp </center>|IC concentration for species 1 in 4th sigma layer (required)|
||<center> ...  </center>|<center> LAYERX\_IC </center>|<center> Exp </center>|IC concentration for species 1 in Xth sigma layer (required)|
 |<center> 7 </center>|<center> 1-10 </center>|<center> SPECIES2 </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
||<center> 12-20 </center>|<center> LAYER1\_IC </center>|<center> Exp </center>|IC concentration for species 2 in lowest sigma layer (required)|
||<center> 23-31 </center>|<center> LAYER2\_IC </center>|<center> Exp </center>|IC concentration for species 2 in 2nd sigma layer (required)|
||<center> 34-42 </center>|<center> LAYER3\_IC </center>|<center> Exp </center>|IC concentration for species 2 in 3rd sigma layer (required)|
||<center> 45-53 </center>|<center> LAYER4\_IC </center>|<center> Exp </center>|IC concentration for species 2 in 4th sigma layer (required)|
||<center> ...  </center>|<center> LAYERX\_IC </center>|<center> Exp </center>|IC concentration for species 2 in Xth sigma layer (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> Z </center>|<center> 1-10 </center>|<center> SPECIESZ </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
|<center> ...  </center>|<center> 12-20 </center>|<center> LAYER1\_IC </center>|<center> Exp </center>|IC concentration for species Z in lowest sigma layer (required)|
|<center> ...  </center>|<center> 23-31 </center>|<center> LAYER2\_IC </center>|<center> Exp </center>|IC concentration for species Z in 2nd sigma layer (required)|
|<center> ...  </center>|<center> 34-42 </center>|<center> LAYER3\_IC </center>|<center> Exp </center>|IC concentration for species Z in 3rd sigma layer (required)|
|<center> ...  </center>|<center> 45-53 </center>|<center> LAYER4\_IC </center>|<center> Exp </center>|IC concentration for species Z in 4th sigma layer (required)|
|<center> ...  </center>|<center> ...  </center>|<center> LAYERX\_IC </center>|<center> Exp </center>|IC concentration for species Z in Xth sigma layer (required)|

A sample of the four sections of an IC\_PROFILE file is shown below.

` Example initial condition: The vertical coordinate of the model to generate`
` these i.c. is the terrain-following sigma coordinate. `
` The number of sigma layers and defined sigma levels are listed below. `
` 6 55 1.00 0.98 0.93 0.84 0.60 0.30 0.00`
` 1988180 00`
` "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04`

### BC\_PROFILE: Boundary conditions vertical profiles

Used by: BCON

As with the ICON program, BCON can generate boundary conditions from two different input file types. The first file type is an ASCII vertical profile file that list species concentrations at various model layers that are fixed in space in time. To configure BCON to generate boundary conditions from ASCII vertical profiles, the “prof” input module is chosen when compiling the program (see Section 5.2 on BCON). These ASCII-formatted vertical profile files are BC\_PROFILE files, and are described in this section. BC\_PROFILE files must be developed by the user and can be generated from climatologically averaged observational data or as an a priori estimate from previous modeling studies of the region being modeled. The second file type that BCON can use to generate initial conditions is a concentration file from a previous CMAQ run. These are CTM\_CONC\_1 files, and are described later in Section 6.1.5.

BC\_PROFILE begins with a header that contains a comment section that describes the data, and a file description section that defines the number of vertical levels in the file, the number of pollutants in the file, and the distribution of the vertical levels. The next entries in BC\_PROFILE are the Julian start date and the start time of the data; they are not used by BCON. The BCON input consists of four data sections that correspond to the lateral boundaries (i.e., north, south, east, and west) of the model grid. The BCON input profiles contain a field that precedes each data section to indicate which horizontal boundary the data section describes.

The format of the data sections in BC\_PROFILE files is the same as in IC\_PROFILE files. Each line corresponds to a different pollutant and begins with the name of the pollutant. The subsequent columns on each line list the chemical concentration at each layer contained in the file. Gas-phase species are in ppmV, and aerosol species are in g m<sup>‑3</sup>. The layer structure of the BC\_PROFILE vertical profiles does not need to correspond exactly to the layer structure that will be modeled; the BCON program will interpolate the data to the correct vertical format for the simulation.

Boundary conditions can either be time-independent (static) or time-dependent (dynamic). Boundary conditions generated with BC\_PROFILE’s ASCII vertical profiles are both static and spatially uniform along each of the four horizontal boundaries at each model layer. For spatially resolved (in the horizontal direction) and temporally resolved boundary conditions, it is necessary to use the other input file type to BCON, an existing CCTM concentration file (CTM\_CONC\_1).

A detailed description of the vertical profile file format for boundary conditions is provided in Table 8-6. The header of the profiles file is list-formatted, while the data section of the file is fixed format.

<a id=Table8-6></a>

<center> **Table 8-6. BC\_PROFILE format description** </center>

|**Line**|**Column**|**Name**|**Type**|**Description**|
|---|---|---|---|---|
| 1-3 || Text Header | String |Text description of the contents and source of the initial conditions file (optional) |
|<center> 4 </center>|<center> A </center>|<center> NUM\_SIGMA\_LVL </center>|<center> Int </center>|Number of sigma levels contained in the file (required)|
||<center> B </center>|<center> NUM\_POLL </center>|<center> Int </center>|Number of pollutants contained in the file (required)|
||<center> C </center>|<center> SIGMA\_LVL </center>|<center> Real </center>|Vertical coordinate values of sigma-p levels; number of values (n+1) is one more than the NUM\_SIGMA\_LVL (n) (required)|
|<center> 4 </center>|<center> n </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> 5 </center>|<center> A </center>|<center> STDATE </center>|<center> String </center>|Julian start date of the file, YYYYDDD (optional)|
||<center> B </center>|<center> STTIME </center>|<center> String </center>|Start time of the file, HH (optional)|
|<center> 6 </center>|<center> A </center>|<center> Direction </center>|<center> String </center>|North, South, East, West; indicates the boundary described by the subsequent data section (required)|
|<center> 7 </center>|<center> 1-10 </center>|<center> SPECIES1 </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
||<center> 12-20 </center>|<center> LAYER1\_BC </center>|<center> Exp </center>|BC concentration for species 1 in lowest sigma layer (required)|
||<center> 23-31 </center>|<center> LAYER2\_BC </center>|<center> Exp </center>|BC concentration for species 1 in 2nd sigma layer (required)|
||<center> 34-42 </center>|<center> LAYER3\_BC </center>|<center> Exp </center>|BC concentration for species 1 in 3rd sigma layer (required)|
||<center> 45-53 </center>|<center> LAYER4\_BC </center>|<center> Exp </center>|BC concentration for species 1 in 4th sigma layer (required)|
||<center> ...  </center>|<center> LAYERX\_BC </center>|<center> Exp </center>|BC concentration for species 1 in Xth sigma layer (required)|
|<center> 8 </center>|<center> 1-10 </center>|<center> SPECIES2 </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
||<center> 12-20 </center>|<center> LAYER1\_BC </center>|<center> Exp </center>|BC concentration for species 2 in lowest sigma layer (required)|
||<center> 23-31 </center>|<center> LAYER2\_BC </center>|<center> Exp </center>|BC concentration for species 2 in 2nd sigma layer (required)|
||<center> 34-42 </center>|<center> LAYER3\_BC </center>|<center> Exp </center>|BC concentration for species 2 in 3rd sigma layer (required)|
||<center> 45-53 </center>|<center> LAYER4\_BC </center>|<center> Exp </center>|BC concentration for species 2 in 4th sigma layer (required)|
||<center> ...  </center>|<center> LAYERX\_BC </center>|<center> Exp </center>|BC concentration for species 2 in Xth sigma layer (required)|
|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|<center> ...  </center>|...|
|<center> Y </center>|<center> A </center>|<center> Direction </center>|<center> String </center>|North, South, East, West; indicates the horizontal boundary described by the subsequent data section (required)|
|<center> Z+1 </center>|<center> 1-10 </center>|<center> SPECIESZ </center>|<center> String </center>|Pollutant name, enclosed in double quotes (required)|
|<center> ...  </center>|<center> 12-20 </center>|<center> LAYER1\_BC </center>|<center> Exp </center>|BC concentration for species Z in lowest sigma layer (required)|
|<center> ...  </center>|<center> 23-31 </center>|<center> LAYER2\_BC </center>|<center> Exp </center>|BC concentration for species Z in 2nd sigma layer (required)|
|<center> ...  </center>|<center> 34-42 </center>|<center> LAYER3\_BC </center>|<center> Exp </center>|BC concentration for species Z in 3rd sigma layer (required)|
|<center> ...  </center>|<center> 45-53 </center>|<center> LAYER4\_BC </center>|<center> Exp </center>|BC concentration for species Z in 4th sigma layer (required)|
|<center> ...  </center>|<center> ...  </center>|<center> LAYERX\_BC </center>|<center> Exp </center>|BC concentration for species Z in Xth sigma layer (required)|

A sample of the important sections of a BC\_PROFILE file is shown below.  ` 6 55 1.00 0.98 0.93 0.84 0.60 0.30 0.00`
` 1988180 00`
` North`
` "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04`
` West`
` "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04`

### CTM\_CONC\_1: CCTM concentration files

Used by: ICON, BCON

An I/O API GRDDED3-formatted CCTM output concentration file, CTM\_CONC\_1, can be used to create spatially and temporally varying initial and boundary conditions. To configure ICON and BCON to generate initial and boundary conditions from a CCTM concentration file, the “m3conc” input module is chosen when compiling the programs (see Section 5.5 on ICON and Section 5.2 on BCON). The input concentration file must cover a temporal and spatial extent that is consistent with the time period and domain that are being modeled, respectively. Both ICON and BCON require a Julian start date to be specified at execution that identifies the first time step to extract from the input concentration file; BCON also requires a run-length specification to indicate the number of time steps of boundary conditions to extract from the input file. For nested runs, the nested domain for which initial and boundary conditions are being extracted must be on the same projection and fall within the domain contained in the input concentration file.

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

ET is the logical name for the ASCII data file containing extraterrestrial radiation as a function of wavelength. The extraterrestrial irradiance file has a format similar to that of the CSQY file (Section 6.1.6). The file begins with a header section; comment lines are preceded with a “!”. Like the CSQY file, the header contains a field describing the location on the wavelength interval that the data represent, and a multiplier. The data section uses a space-delimited, free-form format and lists the wavelength of the incoming solar radiation (nm) and the irradiance (photons cm<sup>‑2</sup> s<sup>‑1</sup>) at each wavelength, with each row corresponding to a specific wavelength interval. A detailed description of the file format is provided in [Table 8-8](Table8-8 "wikilink").

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
|<center> 4 </center>|<center> A </center>|<center> TOMS Data </center>|<center> Int </center>|TOMS ozone measurements as a function of longitude and latitude, \#\#\#; line starts with a space, then space-delimited 25 values per line (required)|
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

### BNDY\_CONC\_1: Boundary conditions

Used by: CCTM

CMAQ boundary condition data are of the BNDARY3 file type. Produced by the boundary condition processor, BCON, CCTM reads these data and correlates them with the interior data by the use of a pointer system. This pointer system designates the beginning location of the data in memory that start a new side of the domain (i.e., south, east, north, or west). [Figure 6‑1](#Figure6-1) illustrates this input data structure and the relationship of the boundary data to the interior (“main grid”) data within CMAQ modules.

Each species being modeled should be in the BNDY\_CONC\_1 file. If some modeled species are not contained in this file, the boundary condition for these species will default to the value 1 × 10<sup>‑30</sup>. The perimeter of the CMAQ domain is 1 cell wide, where the number of boundary cells = (2\*NROW)+(2\*NCOL)+4. [Figure 6-2](#Figure6-2 "wikilink") is a graphical example of the CMAQ boundary conditions file; the west and north boundaries have ozone values of 0.035 ppmV, while the east and south boundaries have values of 0.030 ppmV.

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
### INIT\_CONC\_1: Initial conditions

Used by: CCTM

The initial concentrations of each species being modeled must be input to CMAQ. The initial conditions input file type is GRDDED3 and does not vary with time. The file data are looped in this manner: by column, by row, by layer, by variable. Initial conditions files have the same structure as concentration files, so the predicted concentrations from the last hour of day 1 can be used to initialize the following day’s simulation. This gives CMAQ users the flexibility to segment simulations in any way they choose. [Figure 6-3](#Figure6-3 "wikilink") is a graphical example of the CMAQ initial conditions file. The file shows spatially varying data that can be used to initialize a following run beginning at the time shown (i.e., June 25, 1996 0:00:00).

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

The data section of the file contains data blocks that are mapped to the header using a three-digit code. Each block corresponds to an altitude, latitude, and photolysis reaction and contains nine values of clear-sky photolysis rates for the nine hour angles listed in the header. The three-digit code maps the altitude/latitude/reaction number to the data block. For example, the data block that uses the code “3 1 2” corresponds to altitude 3, latitude 1, and reaction 2 as listed in the file header. A detailed description of the JTABLE file format is provided in [Table 8-11](Table8-11 "wikilink"). The files are list-formatted.

<span id=Table8-11></span>

<center> **Table 8-11. JTABLE file format description** </center>

| **Line** | **Column** |**Name** | **Type** | **Description** |
|---|---|---|---|---|
|1|A|JVDATE|String|Julian date of the file; YYYYDDD (required)|
||B|Comment|String|Description of the Julian date field (optional)|
|2|A|JVHT|Int|Number of vertical levels covered by the data (required)|
||B|Level Field Indicator|String|The word “LEVELS” (required)|
| |C|Comment|String|Description of the level field; usually “(m)”, for meters (optional)|
|3|A|XZJV\_1|Real|Height of level 1; units in m (required)|
||B|XZJV\_2|Real|Height of level 2; units in m (required)|
|...|...|...|...|...|
|...|x|XZJV\_x|Real|Height of level x; units in m (required)|
|4|A|JVLAT|Int|Number of latitudes covered by the data (required)|
||B|Latitude Field Indicator|String|The word “LATITUDES” (required)|
| |C|Comment|String|Description of the latitudes field; usually “(deg)”, for degrees (optional)|
|5|A|XLATJV\_1|Real|Latitude 1; units in degrees (required)|
||B|XLATJV\_2|Real|Latitude 2; units in degrees (required)|
|...|...|...|...|...|
|...|x|XLATJV\_x|Real|Latitude x; units in degrees (required)|
|6|A|JVTMAX|Int|Number of hour angles covered by the data (required)|
||B|Hour Angle Field Indicator|String|The words “HOUR ANGLES” (required)|
| |C|Comment|String|Description of the hour angles field; usually “(from noon)”, for hours from noon (optional)|
|7|A|XHAJV\_1|Real|Hour angle 1; units in hours from noon (required)|
||B|XHAJV\_2|Real|Hour angle 2; units in hours from noon (required)|
|...|...|...|...|...|
|...|x|XHAJV\_x|Real|Hour angle x; units in hours from noon (required)|
|8|A|JPHOT|Int|Number of photolytic reactions covered by the data (required)|
||B|Reaction Field Indicator|String|The words “PHOTOLYTIC REACTIONS” (required)|
| |C|Comment|String|Description of the reactions field (optional)|
|9|A|PHOT\_NM\_1|String|Single quote-delimited name of photolysis reaction 1 (required)|
||B|Delimiter|Char|Comma delimiter separating reaction name from multiplier (required)|
||C|ACLD\_1|Real|Multiplier for reaction 1 (required)|
|10|A|PHOT\_NM\_2|String|Single quote-delimited name of photolysis reaction 2 (required)|
||B|Delimiter|Char|Comma delimiter separating reaction name from multiplier (required)|
||C|ACLD\_2|Real|Multiplier for reaction 2 (required)|
|...|...|...|...|...|
|x|A|PHOT\_NM\_x|String|Single quote-delimited name of photolysis reaction x (required)|
||B|Delimiter|Char|Comma delimiter separating reaction name from multiplier (required)|
||C|ACLD\_x|Real|Multiplier for reaction x (required)|
|x + 1|A|NHTO|Int|Vertical level cross-reference to header data (required)|
| |B|NLATO|Int|Latitude cross-reference to header data (required)|
| |C|NPHOTO|Int|Photolysis reaction cross-reference to header data (required)|
|x+2|A|XJVAL\_1|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 1 (required)|
| |B|XJVAL\_2|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 2 (required)|
| |C|XJVAL\_3|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 3 (required)|
|...|...|...|...|...|
||JVTMAX|XJVAL\_(JVTMAX)|Real or Exp|Clear sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle corresponding to JVTMAX (required)|
|x + 3|A|NHTO|Int|Vertical level cross-reference to header data (required)|
| |B|NLATO|Int|Latitude cross-reference to header data (required)|
| |C|NPHOTO|Int|Photolysis reaction cross-reference to header data (required)|
|x+4|A|XJVAL\_1|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 1 (required)|
| |B|XJVAL\_2|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 2 (required)|
| |C|XJVAL\_3|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle 3 (required)|
|...|...|...|...|...|
||JVTMAX|XJVAL\_(JVTMAX)|Real or Exp|Clear-sky photolysis rate at NHTO, NLATO, NPHOTO, and hour angle corresponding to JVTMAX (required)|
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

### EMIS\_1: Emissions

Used by: CCTM

CMAQ can accept emissions inputs from a variety of emissions models and preprocessors. The most commonly used option is the Sparse Matrix Operator Kernel Emissions (SMOKE) modeling system, which is a collection of programs that separately process and merge emissions data for each emissions sector for input to air quality models.

The emissions file sorts the emitted gas-phase and aerosol species by grid cell and time. The file type is GRDDED3, and the units are in moles per second (moles s<sup>‑1</sup>) for gas-phase species and grams per second (g s<sup>‑1</sup>) for aerosol species. The file data are looped as follows: by column, by row, by layer, by variable, and by input time step. CMAQ does not artificially distinguish between surface and elevated emissions sources; elevated sources are provided to CMAQ as vertically resolved emissions. For CCTM configurations that do not use in-line emissions calculations, all emissions estimates are contained within a single input emission file for each day. In v4.7, CMAQ now has the capability to process point-source, sea salt, and biogenic emissions in-line. The supplemental input files to use for calculating the in-line emissions are described i in the CMAQv4.7 release notes.

### OCEAN\_1: Sea salt mask

Used by: CCTM

The CMAQ aerosol model AERO5 can compute sea salt emissions from both open ocean grid cells and surf zone grid cells. The addition of the surf zone option simulates the elevated emissions rates of sea salt in coastal regions where wave action occurs. The OCEAN\_1 file contains data on the fraction of each grid cell that is either open ocean (OPEN) or in the surf zone (SURF). When CCTM is compiled with AERO5, it will expect the OCEAN\_1 file as input.

### GSPRO: Speciation profiles

Used by: CCTM – inline emissions version only

The speciation profile file, GSPRO, contains the factors that are used to separate aggregated inventory pollutant emissions totals into emissions of model species in the form required by CMAQ. If only biogenic emissions are being calculated in-line in CMAQ, the GSPRO file used by CCTM needs to contain split factors only for the biogenic VOC emissions that are input in the B3GRD file. If other emissions sources are being calculated by CCTM, VOC split factors for these other sources must be included in the GSPRO file. The GSPRO file format is listed in the SMOKE user’s manual ([<http://www.smoke-model.org/version2.5/html/ch08s05s02.html>](http://www.smoke-model.org/version2.5/html/ch08s05s02.html)).

### B3GRD: Gridded, normalized biogenic emissions

Used by: CCTM – inline-emissions version only

An I/O API GRDDED3 file of gridded, normalized biogenic emissions (in grams of carbon or nitrogen per hour, depending on the species) and leaf area index. The B3GRD file contains normalized emissions calculated with both summer and winter emissions factors. The B3GRD file is generated with the SMOKE program NORMBEIS3 using gridded land use data. For additional information about creating the B3GRD file, see the NORMBEIS3 documentation in the SMOKE users’ manual ([<http://www.smoke-model.org/version2.5/html/ch06s11.html>](http://www.smoke-model.org/version2.5/html/ch06s11.html)).

### BIOSEASON: Freeze dates

Used by: CCTM – inline-emissions version only

The BIOSEASON switch file is an I/O API GRDDED3 file used to indicate which biogenic emissions factor to use on each day in a given year for every grid cell in the modeling domain. This file can be created using the Metscan utility program that is distributed with SMOKE. The BIOSEASON file is time-dependent and usually contains data for an entire year (365 or 366 days). It uses one variable, SEASON, which is either 0 (grid cell should use winter factors for current day) or 1 (grid cell should use summer factors for current day). For additional information about creating the BIOSEASON file, see the Metscan documentation in the SMOKE user’s manual ([<http://www.smoke-model.org/version2.5/html/ch05s11.html>](http://www.smoke-model.org/version2.5/html/ch05s11.html)).

### STK\_GRPS\_\#\#: Stack groups

Used by: CCTM – in-line emissions version only

The \#\# mark is unique and represents the sector identification.

The stack groups file is an I/O API netCDF file containing stack parameters for elevated sources. This file can be created using the SMOKE program ELEVPOINT. For additional information about creating the stack groups file, see the ELEVPOINT documentation in the SMOKE user’s manual ([<http://www.smoke-model.org/version2.5/html/ch06s03.html>](http://www.smoke-model.org/version2.5/html/ch06s03.html)).

### STK\_EMIS\_\#\#: Point source emissions

Used by: CCTM – inline emissions version only

The \#\# mark is unique and represents the sector identification.

The elevated-point-source emissions file is an I/O API GRDDED3 file with emissions for point sources to be treated as elevated sources by CCTM. The emissions in this file are distributed through the vertical model layers using a plume-rise algorithm contained in CCTM. The elevated-point-source emissions file can be creating using SMOKE. For additional information about preparing point-source emissions for using the CMAQ in-line plume rise calculation, see the ELEVPOINT documentation in the SMOKE user’s manual ([<http://www.smoke-model.org/version2.5/html/ch06s03.html>](http://www.smoke-model.org/version2.5/html/ch06s03.html)).

### DUST\_LU\_1: Gridded land cover/land use

Used by: CCTM – in-line dust emission version only

The gridded land cover/land use (LCLU) file is an I/O API GRDDED3 file of BELD3 data projected to the modeling domain. This file must contain the following LCLU variables to be compatible with the CMAQv5 dust module:

-   USGS\_urban
-   USGS\_drycrop
-   USGS\_irrcrop
-   USGS\_cropgrass
-   USGS\_cropwdlnd
-   USGS\_grassland
-   USGS\_shrubland
-   USGS\_shrubgrass
-   USGS\_savanna
-   USGS\_decidforest
-   USGS\_evbrdleaf
-   USGS\_coniferfor
-   USGS\_mxforest
-   USGS\_water
-   USGS\_wetwoods
-   USGS\_sprsbarren
-   USGS\_woodtundr
-   USGS\_mxtundra
-   USGS\_snowice

These categories are used to determine dust source locations and canopy scavenging factors for estimating dust emission in the model. This file can be created for North America using the Spatial Allocator and BELD3 tiles. The DUST\_LU\_1 file corresponds to the “a” output file from the Spatial Allocator. See the chapter on [creating biogenic inputs to SMOKE](http://www.ie.unc.edu/cempd/projects/mims/spatial/smoke_bio_inputs.html) of the Spatial Allocator User’s Guide for details.

### DUST\_LU\_2: Gridded land cover/land use

Used by: CCTM – in-line dust emission version only

The gridded land cover/land use (LCLU) file is an I/O API GRDDED3 file of BELD3 data projected to the modeling domain. This file must contain the following variables to be compatible with the CMAQv5 dust module:

-   FOREST

This variable is used in combination with the variables in the DUST\_LU\_1 file to determine canopy scavenging factors for estimating dust emission in the model. This file can be created for North America using the Spatial Allocator and BELD3 tiles. The DUST\_LU\_2 file corresponds to the “tot” output file from the Spatial Allocator. See the chapter on [creating biogenic inputs to SMOKE](http://www.ie.unc.edu/cempd/projects/mims/spatial/smoke_bio_inputs.html) of the Spatial Allocator User’s Guide for details

### CROPMAP01: Gridded planting start dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29 "wikilink") – in-line dust emission version with crops only

The gridded planting start dates file is an I/O API GRDDED3 file of planting start dates for various crops interpolated to the modeling domain. The variables in this file are planting start dates for different crop types, where each variable is an integer representing the number of days after January 1 that planting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor "wikilink") reads a crop activity calendar and a [GRID\_CRO\_2D](#GRID_CRO_2D:_Two-dimensional_grid_cross-point_fields "wikilink") file to generate the CROPMAP08 file.

### CROPMAP04: Gridded planting end dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29 "wikilink") – in-line dust emission version with crops only

The gridded planting end dates file is an I/O API GRDDED3 file of planting end dates for various crops interpolated to the modeling domain. The variables in this file are planting end dates for different crop types, where each variable is an integer representing the number of days after January 1 that planting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor "wikilink") reads a crop activity calendar and a [GRID\_CRO\_2D](#GRID_CRO_2D:_Two-dimensional_grid_cross-point_fields "wikilink") file to generate the CROPMAP08 file.

### CROPMAP08: Gridded harvesting end dates

Used by: [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29 "wikilink") – in-line dust emission version with crops only

The gridded harvesting end dates file is an I/O API GRDDED3 file of harvesting end dates for various crops interpolated to the modeling domain. The variables in this file are harvesting end dates for different crop types, where each variable is an integer representing the number of days after January 1 that harvesting stops for each crop. The CMAQ preprocessing program [CALMAP](#CALMAP:_Crop_calendar_map_preprocessor "wikilink") reads a crop activity calendar and a [GRID\_CRO\_2D](#GRID_CRO_2D:_Two-dimensional_grid_cross-point_fields "wikilink") file to generate the CROPMAP08 file.

### LTNGNO: Lightning NOx emissions

Used by: CCTM – lightning NO<sub>x</sub> version only

The lightning NO<sub>x</sub> emissions file is an I/O API GRDDED3 file with 3-d (row x col x layer) hourly NO emissions (moles/s) interpolated to the modeling domain. This is a lightning NO emissions file calculated off-line for input to CMAQ.

### LTNGPARM\_FILE: Lightning parameters file

Used by: CCTM – lightning NO<sub>x</sub> version only

The lightning parameters file is used for calculating in-line NO emissions from lightning using convective precipitation rates. The LTNG\_2D\_DATA processor that is part of the CMAQv5 distribution package produces the I/O API GRDDED3 lightning parameters file. This file contains the following variables interpolated to the modeling grid:

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

### B4LU\_file – Fractional crop distributions

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

Add content

### E2C\_Soilfile – EPIC soil properties

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

Add content

### E2C\_Fertfile – EPIC crop types and fertilizer application

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

Add content

### INIT\_MEDC\_1 – Soil initial conditions file

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

The ASXfile output for the previous day from the bidirectional NH<sub>3</sub> model is used to initialize the soil conditions for each simulation day.

### GRID\_CRO\_2D: Two-dimensional grid cross-point fields

Used by: CCTM

The GRID\_CRO\_2D time-independent file contains surface fields at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM. The following variables are in this file:

LAT:latitude (degrees, where Northern Hemisphere is positive)

LON:longitude (degrees, where Western Hemisphere is negative)

MSFX2:squared map-scale factor (m<sup>2</sup> m<sup>‑2</sup>)

HT:terrain elevation (m)

DLUSE:dominant land use (category)

LWMASKland-water mask (1=land, 0=water)

PURB:urban percentage if cell is based on land (percent)

### GRID\_DOT\_2D: Two-dimensional grid dot-point fields

Used by: CCTM

The GRID\_DOT\_2D time-independent file contains surface fields at dot points (i.e., at cell corners). It is created by MCIP and used by CCTM. The following variables are in the GRID\_DOT\_2D file:

LAT:latitude (degrees, where Northern Hemisphere is positive)

LON:longitude (degrees, where Western Hemisphere is negative)

MSFD2:squared map scale factor (m<sup>2</sup> m<sup>‑2</sup>)

### MET\_BDY\_3D: Three-dimensional meteorological boundary input

Used by: CCTM

The MET\_BDY\_3D time-dependent file contains 3-D meteorological descriptions at the lateral boundaries (on cross points). It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET\_BDY\_3D file:

JACOBF:total Jacobian at layer face (m)

JACOBM:total Jacobian at layer middle (m)

DENSA\_J:Jacobian-weighted total air density [ J m<sup>‑2</sup>] (kg m<sup>‑2</sup>)

WHAT\_JD:Jacobian- and density-weighted vertical contravariant velocity (kg m<sup>‑1</sup> s<sup>‑1</sup>)

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

### MET\_CRO\_2D: Two-dimensional meteorological cross-point fields

Used by: CCTM

The MET\_CRO\_2D time-dependent file contains surface and other 2-D meteorological fields at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET\_CRO\_2D file:

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

The following deposition velocities are calculated by MCIP3 by default and written to the MET\_CRO\_2D file:

VD\_SO2:deposition velocities for SO<sub>2</sub> (m s<sup>‑1</sup>)

VD\_SULF:deposition velocities for SO<sub>4</sub> (m s<sup>‑1</sup>)

VD\_NO2:deposition velocities for NO<sub>2</sub> (m s<sup>‑1</sup>)

VD\_NO: deposition velocities for NO (m s<sup>‑1</sup>)

VD\_O3:deposition velocities for O<sub>3</sub> (m s<sup>‑1</sup>)

VD\_HNO3:deposition velocities for HNO<sub>3</sub> (m s<sup>‑1</sup>)

VD\_H2O2:deposition velocities for H<sub>2</sub>O<sub>2</sub> (m s<sup>‑1</sup>)

VD\_ALD:deposition velocities for ALD (m s<sup>‑1</sup>)

VD\_HCHO:deposition velocities for HCHO (m s<sup>‑1</sup>)

VD\_OP:deposition velocities for OP (m s<sup>‑1</sup>)

VD\_PAA:deposition velocities for PAA (m s<sup>‑1</sup>)

VD\_ORA:deposition velocities for ORA (m s<sup>‑1</sup>)

VD\_NH3:deposition velocities for NH<sub>3</sub> (m s<sup>‑1</sup>)

VD\_PAN:deposition velocities for PAN (m s<sup>‑1</sup>)

VD\_HONO:deposition velocities for HONO (m s<sup>‑1</sup>)

VD\_CO:deposition velocities for CO (m s<sup>‑1</sup>)

VD\_METHANOL:deposition velocities for methanol (m s<sup>‑1</sup>)

VD\_N2O5:deposition velocities for N<sub>2</sub>O<sub>5</sub> (m s<sup>‑1</sup>)

VD\_NO3:deposition velocities for NO<sub>3</sub> (m s<sup>‑1</sup>)

VD\_GEN\_ALD:deposition velocities for generic aldehyde (m s<sup>‑1</sup>)

VD\_CL2:deposition velocities for CL2 (m s<sup>‑1</sup>)

VD\_HOCL:deposition velocities for HOCL (m s<sup>‑1</sup>)

VD\_HCL:deposition velocities for HCL (m s<sup>‑1</sup>)

VD\_FMCL:deposition velocities for FMCL (m s<sup>‑1</sup>)

VD\_ICL1:deposition velocities for ICL1 (m s<sup>‑1</sup>)

VD\_ICL2:deposition velocities for ICL2 (m s<sup>‑1</sup>)

VD\_HG:deposition velocities for HG (m s<sup>‑1</sup>)

VD\_HGIIGAS:deposition velocities for HGIIGAS (m s<sup>‑1</sup>)

### MET\_CRO\_3D: Three-dimensional meteorological cross-point fields

Used by: CCTM, ICON, BCON

The MET\_CRO\_3D time-dependent file contains 3-D meteorological descriptions at cross points (i.e., at cell centers). It is created by MCIP and used by CCTM, ICON, BCON, and PDM. The variables that may exist in MET\_CRO\_3D are the same as those that may be in MET\_BDY\_3D.

### MET\_DOT\_3D: Three-dimensional meteorological dot-point fields

Used by: CCTM

The MET\_DOT\_3D time-dependent file contains 3-D meteorological descriptions at dot points (i.e., at cell corners) and at cell faces. It is created by MCIP and used by CCTM and PDM. The following variables may be in the MET\_DOT\_3D file:

UWIND:u-component of horizontal wind (m s<sup>‑1</sup>) [dot points; Arakawa-B grid]]

VWIND:v-component of horizontal wind (m s<sup>‑1</sup>) [dot points; Arakawa-B grid]

UHAT\_JD:contravariant-U\*Jacobian\*density (kg m<sup>‑1</sup> s<sup>‑1</sup>) [cell faces; Arakawa-C grid]

VHAT\_JD:contravariant-V\*Jacobian\*density (kg m<sup>‑1</sup> s<sup>‑1</sup>) [cell faces; Arakawa-C grid]

Basic CCTM Output Files
-----------------------

The previous section described the output files from JPROC, ICON, BCON, and MCIP that are input to CCTM. In this section, details on the CCTM output files are provided. Except for JPROC (which creates ASCII files), all CMAQ programs produce output files that adhere to the I/O API netCDF format (Chapter 4). The I/O API-formatted CMAQ output files are three-dimensional, gridded, time-stepped binary files that contain headers with metadata describing the file contents. These machine-independent and network transparent binary files are transferable between different computer architectures. In addition to model data output, CMAQ can optionally produce log files that contain the standard output from the various CMAQ processors. If the log file option is not selected by the user, CMAQ will write all of the log information to the screen along with the standard error, which can be captured to a text file using basic UNIXsyntax.

### CMAQ output log

All of the CMAQ processors generate standard output and standard error during execution. For all of the processors other than CCTM, this diagnostic output information can be captured to a log file at execution using a UNIX redirect command. For example, to capture the standard output and error of a BCON simulation, use the following command:

run.bcon \>& bcon\_e1a.log

For CCTM, the LOGFILE environment variable allows users to specify the name of a log file for capturing the standard output from the program. If this variable is not set, the standard output is written to the terminal and can be captured using the UNIXredirect command (“\>”), as shown in the example above.

### CONC: CCTM hourly instantaneous concentration file

The 3-D CCTM hourly concentration file (CONC) is the most commonly referenced CCTM output file. Containing gas-phase species mixing ratios (ppmV) and aerosol species concentra­tions (µg m<sup>‑3</sup>), CONC files include instantaneous model species concentrations at the end of each model hour. The number and types of species contained in the CONC files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. The species concentration INCLUDE files (CONC.EXT) within the mechanism INCLUDE directories list the species that are written to the CONC files for each mechanism configuration. The GC\_CONC.EXT file lists the gas-phase species, the AE\_CONC.EXT file lists the aerosol species, and the NR\_CONC lists the nonreactive (inert) species written to the CONC file. Species can be removed from the CONC.EXT files to reduce the number of species that are written to, and thus the size of, the CONC file.

### CGRID: CCTM restart file

The 3-D CCTM ending concentration file (CGRID) is the CCTM restart file. Containing gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>‑3</sup>), the CGRID file includes model species concentrations at the end of each simulation period. The number and types of species contained in the output CGRID files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. This file can be used to initialize CCTM from the simulation period that the model completed. For example, if the CCTM is configure to produce daily output files, a CGRID file will be written out at the end of each simulation day.

### ACONC: CCTM hourly average concentration file

The 3-D CCTM integral average concentration file (ACONC) contains average model species concentrations for each model hour, as opposed to instantaneous concentrations at the end of each output time step. The species written to the ACONC file are set by the user in the CCTM run script using the variable AVG\_CONC\_SPCS. The model layers that are used to calculate the integral average concentration are also set in the CCTM run script using the variable ACONC\_BLEV\_ELEV, where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number. An example setting for the ACONC\_BLEV\_ELEV variable is “1 6”, which defines layers 1 through 6 as the vertical extent over which to calculate hourly average concentrations.

### DRYDEP: CCTM hourly cumulative dry deposition file

The 2-D CCTM dry deposition file (DRYDEP) includes cumulative hourly dry deposition fluxes (kg hectare<sup>‑1</sup>) for selected model species. CCTM calculates dry deposition for all of the species listed in the dry deposition INCLUDE'' *files within the mechanism INCLUDE directories. Dry deposition INCLUDE* ''files exist for gas-phase species (GC\_DDEP.EXT), aerosol species (AE\_DDEP.EXT), and inert model species (NR\_DDEP.EXT). Species can be removed from the DDEP.EXT files to adjust the number of species that undergo the dry deposition process and are written to the DRYDEP output file.

### WETDEP: CCTM hourly cumulative wet deposition file

The 2-D CCTM wet deposition file (WETDEP) includes cumulative hourly wet deposition fluxes (kg hectare<sup>‑1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition INCLUDE files within the mechanism INCLUDE directories. Wet deposition INCLUDE files exist for gas-phase species (GC\_WDEP.EXT), aerosol species (AE\_WDEP.EXT), and inert model species (NR\_WDEP.EXT). Species can be removed from the WDEP.EXT files to adjust the number of species that undergo the wet deposition process and are written to the WETDEP output file.

### AEROVIS: CCTM hourly instantaneous visibility metrics

The 2-D CCTM visibility file (AEROVIS) contains hourly Mie and reconstructed visual range coefficients (km<sup>‑1</sup>) and normalized extinction coefficients (deciviews).

Diagnostic and Advanced CMAQ Output Files
-----------------------------------------

Along with the basic outputs detailed in the previous section, CMAQ can be configured to output several auxiliary files for diagnosing model performance.

### AERODIAM: Instantaneous hourly aerosol diameter file

This diagnostic file contains information on the geometric mean diameters and geometric standard deviations for the lognormal modes.

### B3GTS\_S: Biogenic emissions diagnostic file

This optional 2-D CCTM hourly output file contains calculated biogenic emissions in mass units. The B3GTS\_S file will be produced only if in-line biogenic emissions are being calculated by CCTM and if the B3GTS\_DIAG variable is turned on.

### DEPV\_DIAG: CCTM inline deposition diagnostics file

Add content

### DUST\_EMIS

Add content

### FLOOR: concentration-reset diagnostics file

FLOOR files are optional output diagnostic files which list specific gridboxes/timesteps in which species with `-ve` concentrations are reset to zero.

### INIT\_MEDC\_1

Add content

### IRR: Process analysis output – integrated reaction rates

The 3-D CCTM integrated reaction rate file (IRR) contains hourly concentrations of selected model output species in terms of the gas-phase chemistry pathways that contributed to the predicted concentration at each hour. For each grid cell in the process analysis domain (which is most likely a subset of the full modeling domain), the IRR file shows the hourly change in species concentration that is due to particular gas-phase chemistry reactions or reaction groups. The process analysis preprocessor, PROCAN (Section 2.2.6), is used to select the process analysis domain, the model species for which to capture process analysis information, and the chemistry reactions or groups of reactions to track during the process analysis.

### LTNGOUT

Add content

### PA: Process analysis output – integrated process rate file

The 3-D CCTM integrated process rate file (PA) contains hourly concentrations of selected model output species in terms of the model process that contributed to the concentration in each grid cell at each hour. For each grid cell in the process analysis domain (which is most likely a subset of the full modeling domain), the PA file shows the hourly change in species concentration that is due to the major model processes, such as horizontal and vertical advection, chemistry, and wet deposition. The process analysis preprocessor, PROCAN (Section 2.2.6), is used to select the process analysis domain, the model species for which to capture process analysis information, and the model processes to track during the process analysis.

### PLAY\_SRCID

Add content

### PT3D\_DIAG

Add content

### RJ: In-line photolysis output – gridded photolysis rates

The photolysis diagnostic output files (RJ) contain the photolysis rates calculated by CCTM when the in-line photolysis option is used.

### SOILOUT

Name and location of hourly soil NO emissions file; output when in-line biogenic emissions processing is activated by setting CTM\_BIOGEMIS to “T” or “Y”.

### SSEMIS: Sea salt emissions diagnostic file

This optional 2-D CCTM hourly output file contains calculated sea salt emissions. The SSEMIS file will be produced by CCTM only if the AERO5 aerosol mechanism is being used and if the CTM\_SSEMDIAG variable is turned on.

### WETDEP2: CCTM cloud diagnostics file

The 2-D CCTM wet deposition file (WETDEP2) includes cumulative hourly wet deposition fluxes (kg hectare<sup>‑1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition INCLUDE files within the mechanism INCLUDE directories. Wet deposition INCLUDE files exist for gas-phase species (GC\_WDEP.EXT), aerosol species (AE\_WDEP.EXT), and inert model species (NR\_WDEP.EXT). Species can be removed from the WDEP.EXT files to adjust the number of species that undergo the wet deposition process. These extra species are written to the WETDEP2 output file.

In CMAQ, wet deposition is calculated separately for resolved (grid-scale) clouds and for convective (subgrid) clouds. The WETDEP1 files contain the total wet deposition, i.e., the sum of both resolved-scale and subgrid-scale deposition. The WETDEP2 file contains only subgrid-scale deposition, plus some cloud diagnostic variables.

<span id=Section9></span>

DEFINING GRIDS, LAYERS, DOMAINS, AND CHEMISTRY
==============================================

This chapter describes how to define new horizontal grids, vertical layers, and chemical mechanisms in CMAQ. These specifications apply to multiple programs in the CMAQ modeling system, including ICON, BCON, JPROC, and CCTM. When configuring new simulations, users must define the location, extent, and structure of the horizontal and vertical grids, and the chemical mechanism for representing pollutant chemical transformations. CMAQ contains several default options for these parameters that can be used as templates for setting up new configurations. Before deciding to create definitions for new grids and mechanisms, check to see whether the existing options are sufficient for your model simulation. If a predefined choice is not appro­priate, then follow the steps described in this section to create a new definition.

Once you have configured a simulation that is suitable for your purposes in terms of the horizontal grid, vertical layers, and chemical mechanism, proceed to Chapter 8 to learn how to develop new model executables for running a CMAQ simulation.

Grids and coordinate systems
----------------------------

CMAQ is a three-dimensional Eulerian air quality model. The *domain* of a model run (the extent of its area of interest) is divided into three-dimensional cells (or [*voxels*](http://en.wikipedia.org/wiki/Voxel)), the boundaries of which (the *grid* of the domain) must be rigorously and consistently defined for all functional components of the model (e.g., chemistry, emissions, meteorology). Mathematical algorithms describing atmospheric transport and air/surface exchange govern the flow of material into and out of each grid cell. Mathematical algorithms describing chemical reactions and aerosol dynamics govern the production and loss of material contained in each grid cell.

Horizontal (or *2D*) and vertical components of a model run's grid are treated differently. The horizontal grid specification (setting the *x* and *y* dimensions) must be *regular*: the horizontal projection of each grid cell (sometimes referred to as a *pixel*) has the same resolution, and the boundaries of each pixel are time-invariant. By contrast, the vertical grid specification (setting the *z* dimension) need not be regular; it can vary in space and time.

After determining the horizontal and vertical extent of the domain of interest, a meteorological model must be run for a horizontal domain slightly larger than the CMAQ domain. A larger meteorology domain is necessary for distinguishing the meteorological boundary conditions from the CMAQ boundary conditions.

### Supported CMAQ Coordinate Systems

Specifications for CMAQ and [MCIP](#Meteorology-Chemistry_Interface_Processor_.28MCIP.29 "wikilink") grids are governed by [I/O API](#Input.2FOutput_Applications_Programming_Interface_.28I.2FO_API.29 "wikilink") [grid conventions](https://www.cmascenter.org/ioapi/documentation/3.1/html/GRIDS.html). The choice of horizontal coordinate system, or map projection, for CMAQ is governed by the input emissions inventories and meteorological model fields, which must agree. [MM5](http://en.wikipedia.org/wiki/MM5_%28weather_model%29) and [WRF/ARW](https://en.wikipedia.org/wiki/Weather_Research_and_Forecasting_model) support the [Lambert conformal](https://en.wikipedia.org/wiki/Lambert_conformal_conic_projection), [polar stereographic](https://en.wikipedia.org/wiki/Universal_polar_stereographic_coordinate_system), and [Mercator projection](https://en.wikipedia.org/wiki/Mercator_projection)s, which can be directly passed to CMAQ.

### Horizontal Grids

Available horizontal grids for a given CMAQ run are defined at runtime by setting the GRIDDESC and GRID\_NAME environment variables to point to an existing grid definition file and to one of the grids defined in the file, respectively. Horizontal grids are defined by the [grid definition file](#GRIDDESC:_Horizontal_domain_definition "wikilink"), which can be edited by the user (more below).

The extent of the horizontal grid used in CMAQ is limited by the size of the domain of the input meteorology. [MCIP](#Meteorology-Chemistry_Interface_Processor_.28MCIP.29 "wikilink") and the [I/O API](#Input.2FOutput_Applications_Programming_Interface_.28I.2FO_API.29 "wikilink") utilities can be used to *window* subsets of meteorology data. Choosing the appropriate horizontal grid scale and extent for a [CCTM](#CMAQ_Chemistry-Transport_Model_.28CCTM.29 "wikilink") run is largely dependent on the issues to be addressed by the modeling. However, practical consideration should also be paid to the relationship between grid size, output file size, and execution times.

#### CMAQ horizontal grid conventions

Grid conventions are specified (at length) by [I/O API](#Input.2FOutput_Applications_Programming_Interface_.28I.2FO_API.29 "wikilink") [here](https://www.cmascenter.org/ioapi/documentation/3.1/html/GRIDS.html). In summary, users should be aware that CMAQ uses both "cross-point" and "dot-point" grids.

<span id=Figure9-1></span>

<center>
![Figure 9-1. relating cross and dot grids](CMAQ_IOAPI_dot_and_point_grids.jpg "fig:Figure 9-1. relating cross and dot grids") **Figure 9-1. relating cross and dot grids**

</center>
Hence, a user interested in a particular grid should be aware of its type. "Cross-point" is often abbreviated *CRO*, as in [`GRID_CRO_2D`](#GRID_CRO_2D:_Two-dimensional_grid_cross-point_fields "wikilink"). "Dot-point" is often abbreviated *DOT*, as in [`MET_DOT_3D`](MET_DOT_3D:_Three-dimensional_meteorological_dot-point_fields "wikilink").

Similarly, the user should be aware of the grid's

-   [projection](https://en.wikipedia.org/wiki/Map_projection)
-   units. Usually meters, except when using [lat-lon coordinate systems](https://en.wikipedia.org/wiki/Geographic_coordinates#Geographic_latitude_and_longitude).

Regarding both grid types, the terms with which most users must be concerned are

origin  
lower left corner of the cell at column=row=1

`X_ORIG`  
X coordinate of the grid origin (in projection units)

`Y_ORIG`  
Y coordinate of the grid origin (in projection units)

`X_CELL`  
horizontal resolution parallel to the X coordinate axis (in projection units)

`Y_CELL`  
horizontal resolution parallel to the Y coordinate axis (in projection units)

`NCOLS`  
number of grid columns

dimensionality in the X direction

`NROWS`  
number of grid rows

dimensionality in the Y direction

#### Using predefined horizontal grids

CMAQv5 is distributed with a GRIDDESC file that contains a definition for a grid covering the southeastern U.S. that uses a Lambert Conformal Conic coordinate. A picture of the grid and the grid definition, in the GRIDDESC format, is shown in [Figure 9-2](#Figure9-1 "wikilink").

|---|---|
|<span id=Figure9-2></span>![](Figure9-1.png "fig:Figure9-1.png")|Coordinate: Lambert Conformal

Latitude 0: 40.0

Longitude 0: -97.0

Standard Parallel 1: 33.0

Standard Parallel 2: 45.0

X origin = 504,000

Y origin = -1,488,000

Rows: 122

Columns: 127

dX = 12,000

dY = 12,000

Layers = 35|

<center>
**Figure 9-2. CMAQ benchmark grid**

</center>
#### Creating or modifying horizontal grids

Creating a grid in CMAQ involves simply adding a few lines of text to the GRIDDESC file. Using a combination of the file format documentation in Chapter 6 and existing grid definitions as examples, new grids can be defined for CMAQ by adding a coordinate and grid description to the GRIDDESC file. Set the GRID\_NAME environment variable in the CMAQ run scripts to point to the name of the new grid. The most common situation for creating a new CMAQ grid definition is encountered when using meteorology and/or emissions data that have not yet been modeled with CMAQ. MM5 or WRF‑ARW outputs can be run through MCIP to generate a GRIDDESC file that can be input directly to both CMAQ and SMOKE. If the meteorology data have already been processed by MCIP and the GRIDDESC file is missing, the grid definition of the input meteorology (and emissions) can be determined by using the netCDF utility *ncdump* to view the header of one of the I/O API files and then use that information to manually create a GRIDDESC file.

#### Further information on horizontal grids

-   Horizontal grid dimensions should be no smaller than 30 rows and 30 columns.
-   External boundary thickness should be set to “1”.
-   A CMAQ grid should be smaller than its parent meteorology grid by at least four grid cells on a side, and preferably by six.
-   Horizontal grid spacing for the parent meteorology grid often has a 3:1 ratio, although other ratios have been employed.

### Vertical Layers

The vertical structure of CMAQ is inherited from the model used to prepare the meteorological information. Both MM5 and WRF-ARW use a sigma coordinate that is based upon surface pressure, not sea level pressure, and a pressure at the top boundary (e.g., 100 hecto-Pascals). The sigma coordinate is terrain following. Because MM5 and WRF-ARW are nonhydrostatic models, the vertical coordinate is time varying.

#### Vertical layer resolution

Resolving the surface boundary layer requires high resolution near the surface for meteorological simulations. To determine mass exchange between the boundary layer and free troposphere, good resolution near the boundary layer top is preferable. In addition, different cloud parameter­izations may perform differently depending on the layering structure. Layer definitions should be appropriate for the topographic features of the simulation domain. Aerodynamic resistance, which influences dry deposition velocities, is a function of layer thickness and the boundary layer stability. For emissions processing, the layer thickness affects the plume rise from major stacks. The vertical extent of the surface-based emission effects is determined by the thickness of the lowest model layer for CCTM. For consistency, CCTM should use the same vertical resolution as the meteorological model used to prepare the input data.

#### Further information on vertical layers

-   CMAQ redefines the vertical coordinates to monotonically increase with height, a capability necessary to handle a generalized coordinate system.
-   Although MCIP may be used to reduce the number of vertical layers by collapsing layers, this is ***not recommended,*** as dynamical inconsistencies can develop and lead to misleading results. This is particularly true when cloud processes are important.
-   Increasing the number of vertical layers increases the CPU time and the computational complexity.
-   Computational limits arise from the Courant number limitation of vertical advection and diffusion processes. When using K-theory, a very shallow layer definition increases CPU time tremendously under the convective conditions.

### References for grid and coordinate system topics

The definitive technical reference for CMAQ grid conventions is [*On The Definition of Horizontal and Vertical Grids and Coordinates for Models-3*](https://www.cmascenter.org/ioapi/documentation/3.1/html/GRIDS.html). Other useful works include

-   [chapter 12 (MCIP) of the 1999 Models-3/CMAQ Science document](http://www.cmascenter.org/cmaq/science_documentation/pdf/ch12.pdf)
-   [Otte and Pleim 2009 (in GMD) on MCIP](http://www.geosci-model-dev.net/3/243/2010/gmd-3-243-2010.html)

Chemical Mechanism
------------------

The CMAQ modeling system accounts for chemistry in three phases: a gas phase, aerosols (solid or liquid), and an aqueous phase. The CMAQ modeling system’s existing modules for gas‑phase chemistry are the 2005 update to the Carbon Bond mechanism (CB05), and the Statewide Air Pollution Research Center-2007 (SAPRC-07) gas-phase mechanism. Several variations of the base gas-phase mechanisms, with and without chlorine, mercury, and toxic species chemistry, are distributed with CMAQ. The modularity of CMAQ makes it possible to create or modify the gas-phase chemical mechanism. This procedure is described in Sections 5.4 and 7.4.2.

Gas-phase chemical mechanisms are defined in CMAQ as a series of Fortran INCLUDE files. Located in subdirectories of the \$M3MODEL/mechs/release directory (each correspond­ing to a mechanism name), these INCLUDE files define the source, reaction parameters, and atmospheric processes (e.g., diffusion, deposition, advection) of the various mechanism species. The species definitions are contained in namelist files that are read in during execution of the CMAQ programs. The CMAQ mechanism configuration is more similar to the science module configuration than to the horizontal grid or vertical layer configuration in that the mechanism is defined at compilation, resulting in executables that are hard-wired to a specific gas-phase mechanism. To change chemical mechanisms between simulations, a new executable that includes the desired mechanism configuration must be compiled.

### Using predefined chemical mechanisms

To select a predefined mechanism configuration in CMAQ, set the *Mechanism* variable in the build scripts to the name of one of the mechanism directories located under \$M3MODEL/mechs/release. Table 7‑1 lists the available chemical mechanisms in CMAQv4.7 and what is included with each mechanism. Set the *Mechanism* variable in the CMAQ build script to the Mechanism ID in Table 7‑1 to select a particular mechanism. Detailed descriptions of some of these mechanisms can be found in Byun and Ching (1999).

<span id=Table7-1></span>

<center>**Table 7-1. CMAQ chemical mechanisms**</center>

|**MechanismID**|**cb05**|**saprc07**|**saprc99**|**CMAQ Aerosols**|**Aqueous Chemistry**|**Additional species**|
|---|---|---|---|---|---|---|
|<center>**5<sup>rd</sup> gen.**</center>
|<center>**6<sup>th</sup> gen.*</center>|
|cb05cl\_ae5\_aq|<center>x</center>|||<center>x</center>||<center>x</center>|<center>Cl</center>|
|cb05tucl\_ae5\_aq|<center></center>|||<center>x</center>||<center>x</center>|<center>Cl, updated toluene</center>|
|cb05tucl\_ae6\_aq|<center>x</center>||||<centerx</center>|<center>x</center>|<center>Cl, updated toluene</center>|
|cb05tump\_ae6\_aq|<center>x</center>||||<center>x</center>|<center>x</center>|<center>Cl, updated toluene, air toxics, Hg</center>|
|saprc07tb\_ae6\_aq||<center>x</center>|||<center>x</center>|<center>x</center>|<center>Updated toluene</center>|
|saprc07tc\_ae6\_aq||<center>x</center>|||<center>x</center>|<center>x</center>|<center>Updated toluene</center>|
|saprc99\_ae5\_aq|||<center>x</center>|<center>x</center>||<center>x</center>||
|saprc99\_ae6\_aq|||<center>x</center>||<center>x</center>|<center>x</center>||

### Creating or modifying chemical mechanisms

Creating or modifying mechanisms in CMAQ requires the use of the CMAQ chemical mecha­nism compiler, CHEMMECH, to produce the required Fortran INCLUDE files. CHEMMECH translates an ASCII mechanism listing to the INCLUDE files required by CMAQ. Like all of the CMAQ preprocessors, CHEMMECH is a Fortran program that must be compiled by the user prior to use. Distributed with a Makefile for compilation and run scripts for execution, CHEMMECH reads a mechanism definition (mech.def) file and outputs the mechanism INCLUDE files. See Section 5.4 for a description of CHEMMECH.

To modify an existing mechanism, copy the mech.def file that is contained in one of the existing mechanism INCLUDE file directories to a new directory and modify the mechanism accordingly. Provide this modified mechanism definition file to CHEMMECH as input to produce the mechanism INCLUDE files needed to compile CMAQ. Put these mechanism INCLUDE files in a new directory under the \$M3MODEL/mechs/release directory. To invoke this new mechanism, set the *Mechanism* variable in the CMAQ build scripts to the name of the new mechanism directory and compile new executables.

To create a new mechanism for CMAQ, follow a procedure similar to the above for modifying mechanisms. Use an existing mech.def file as a template to format the new mechanism for inclusion in CMAQ. After formatting the mechanism in the form of the mech.def file, provide this file as an input to CHEMMECH to create the required INCLUDE files for CMAQ. Move the resulting INCLUDE files to a new directory under \$M3MODEL/mechs/release. To invoke this new mechanism, set the *Mechanism* variable in the CMAQ build scripts to the name of the new mechanism directory and compile new executables. See Section 5.4 for additional details about the CHEMMECH program.

### Using species namelist files

New to CMAQv5 are species namelist files that define the parameters of the gas, aerosol, non-reactive, and tracer species simulated by the model. The CMAQ programs read the namelist files during execution to define the sources and processes that impact the simulated concentrations of each of the model output species. The namelist files can be used to apply uniform scaling factors by model species for major model processes. For example, emissions of NO can be reduced by 50% across the board by applying a factor of 0.5 to the emissions scalar column of the gas-phase species namelist file. Similarly, the boundary conditions of O<sub>3</sub> can be increased by 50% by applying a factor of 1.5 to the boundary conditions scalar column of the gas-phase species namelist file.

When mechanisms are modified or created in CMAQ, new namelist files must be created that include the new species in the mechanism. Existing namelist files can be used as templates to guide the creation of the new files.

### Further information on chemical mechanisms

-   The versions of each chemical mechanism that include both aerosols and aqueous chemistry represent the most comprehensive modeling approach.
-   The same chemical mechanism must be used for CCTM and all of the mechanism-dependent input processors that are part of the CMAQ system.
-   The Euler Backward Iterative (EBI) chemistry solver is mechanism-dependent. If a chemical mechanism is modified, then the default EBI solver cannot be used for the new mechanism. The Rosenbrock and SMVGEAR solvers are the only mechanism-independent choices of chemistry solvers with CCTM.
-   When adding new species to CMAQ, it is important to check that the sources of these new species into the modeling domain are accounted for correctly in the mechanism INCLUDE files. If species are added to the domain through the emissions files, the namelist files that define the mechanism species must contain these new species.

<span id=Section10></span>

DEVELOPING NEW CMAQ SIMULATIONS
===============================

Second-generation air quality modeling systems, such as the Regional Oxidant Model, were composed of a single air quality model and the associated input data processors. Changes in the extent of the modeling domain, its horizontal resolution, the chemistry mechanism, etc., required extensive coding changes in the air quality model and several associated input processors. Each executable program was then compiled and tested to ensure that errors were not introduced during the code modifications. Users had no control over the extent of the model domain and no flexibility to use an alternate chemistry mechanism from an existing model.

The design of the CMAQ modeling system overcomes the inflexibility of second-generation systems through the Bldmake model builder program, which builds models and processors according to user specifications. For application users of CMAQ, Bldmake is used only at the beginning of a simulation to compile executables for a specific science configuration. Since the horizontal grid and vertical layer structure are defined dynamically at execution of the model, there is no need to recompile the programs when changing these parameters. Compilation is required only when either developing CMAQ simulations for the first time or when the chemistry/science configuration within the model is changed. A model developer would use Bldmake to check out working versions of the CMAQ source code and create a Makefile to facilitate the interchange of science components within a model, to modify reaction details within an existing chemistry mechanism, or to experiment with source code modifications (see Chapter 10).

The purpose of this chapter is to demonstrate how to build the executables for the CMAQ programs beyond running the benchmark case. Before proceeding with this chapter, review Chapter 3 for an overview of the system requirements for CMAQ. In general, there are three major steps in compiling CMAQ executables:

1.  Install the CMAQ libraries (netCDF and I/O API)
2.  Build Bldmake
3.  Configure and build the executables for the various CMAQ programs.

All compilations of libraries and CMAQ must be done with the *same compilers and settings*. The details of these three major steps with respect to creating new CMAQ simulations are covered in this chapter.

General Introduction to Model Building
--------------------------------------

Before using CMAQ for operational modeling in a new computing environment, it is recommended that the model be exercised using the benchmark dataset that is distributed with the model. Chapter 3 describes how to configure a minimal hardware system for CMAQ and how to install CMAQ to run the benchmark simulation. After benchmarking CMAQ, it can be configured for other simulations. The same steps that are required to build the model for the benchmark case apply to building it for new simulations. However, not all of the steps need to be repeated for a new model configuration unless new code becomes available or bug fixes are identified. In particular, it is not necessary to rebuild any of the libraries that CMAQ uses once working versions are built on a user’s system. A single installation of the CMAQ libraries (STENEX and PARIO) and the non-CMAQ libraries (netCDF, I/O API, MPICH) can be linked to for multiple configurations and applications of the model. Likewise, the CMAQ model builder, Bldmake, can be compiled once and used for all applications of the model.

Except for MCIP, all of the CMAQ programs need to be recompiled when the chemistry mechanism or science configuration of the model change. If the science configuration does not change between applications, the CMAQ programs can be reused. MCIP needs to be compiled only once on a user’s system and then reused for all applications of the model, unless new source code, including libraries, becomes available.

Configuring New Simulations
---------------------------

The reason for modeling a particular time period evolves from a research question, such as determining why a severe air pollution episode happened, or studying the dominant sources of visibility degradation in a specific geographic region. The selection of episodes in CMAQ, however, is completely determined by the availability of input meteorology and emissions data. The horizontal model grid, vertical layer structure, and model time periods must be consistent across the meteorology data, emissions data, and CMAQ.

Configuring CMAQ for new simulations involves defining the model grid, vertical layers, time periods, initial and boundary conditions, input/output file locations, and science options of the model. The following sections cover these topics in the context of setting up a new CMAQ simulation.

### Defining a new horizontal grid

The grid-dependent CMAQ programs (CCTM, ICON, BCON) use the GRIDDESC grid descrip­tion file (Section 6.1.1) to define the map projection and horizontal grid for a simulation. The GRIDDESC file is either output by MCIP or it can be created manually using a text editor. The CMAQ run scripts for the grid-dependent CMAQ programs must refer to a GRIDDESC file that contains the definition of the model grid to be simulated. The name of the grid for the current application must be specified in the CMAQ run script because a single GRIDDESC file can contain multiple grid definitions. Additional information about setting up horizontal grids in CMAQ is contained in Chapter 7.

The following error from any of the CMAQ programs can occur if the name and/or location of the GRIDDESC file and/or the name of the horizontal grid are incorrect in the run script:

\*\*\* Failure defining horizontal domain

For CCTM, which uses both a GRIDDESC file and gridded input data (emissions, meteorology, ICs/BCs), the grid defined in the GRIDDESC file must be consistent across all of the gridded input files, or the simulation will fail.

To configure a new CMAQ simulation, the following steps must be taken to set up the horizontal model grid:

-   Produce emissions and meteorology data on a consistent horizontal model grid to be modeled with CCTM
-   Create a GRIDDESC file that defines the horizontal model grid
-   Generate ICs and BCs on the horizontal model grid; for nested simulations, generate BCs from a parent grid
-   Configure the CCTM script to use the GRIDDESC file and input data on the desired horizontal model grid

### Defining a new vertical layer structure

The CMAQ programs that produce 3-D output (CCTM, ICON, BCON) use the MET\_CRO\_3D file (Section 6.1.21) to define a vertical layer structure. The MET\_CRO\_3D file is output from MCIP and takes the vertical layer structure from the input meteorology. The vertical layer structure must be consistent across all of the CMAQ programs used to complete a CCTM simulation. New vertical layer structures for CMAQ simulations are configured with MCIP when processing raw meteorological model data. See Section 5.7 for details on configuring MCIP.

### Setting a new episode time period

The temporal extent of a CMAQ simulation is limited by the availability of input meteorology and emission data. Similar to the horizontal grid and vertical layer structures, the time period to model with CMAQ must be considered when designing the meteorological modeling simulation used to produce CMAQ input data.

The model output time step and run length of each CMAQ simulation are flexible and can be configured in the run script for the applicable CMAQ program. For CCTM, it is possible to have output files with a different number of time steps than the corresponding meteorology or emission input data. For example, it is common to have input meteorology files in 4- or 5-day intervals, input emission files in 1- or 2-day intervals, and CCTM output files in 1-day intervals. The CMAQ scripts allow the user to configure the model to output data with any number of time steps. The number of CCTM output time steps does not need to correspond with the input meteorology and emissions output time steps. To keep CMAQ output file sizes manageable, CMAQ output is generally stored in 1‑day (24‑hour) blocks.

To configure a new CMAQ simulation, the follow steps must be taken to set up the modeling time period:

-   Produce emissions and meteorology data for the time period to be modeled with CMAQ. When deciding upon a modeling period, it is necessary to have a “spin-up” interval prior to the beginning of the initial time of interest. The spin-up period is a sequence of days at the beginning of an air quality simulation that are not used in the analysis of the modeling results. These days are simulated to minimize the impacts of the initial conditions on the CCTM modeling results. Spin-up periods vary in length depending on the size of the modeling domain, the magnitude of the emissions sources within the modeling domain, and the pollutants being studied. As a general rule of thumb, a period of at least 10 days should be considered for a spin-up period.
-   Generate ICs for the first time step of the CCTM simulation; if running multiple days in sequence, configure the CCTM run script to use the ICs from ICON for the first model day and to initialize the subsequent days with the previous day’s CCTM output
-   Generate either time-independent BCs for the grid to be modeled, or for a nested simulation generate temporally resolved BCs for the model time period from CCTM outputs for the parent grid
-   Create photolysis look-up tables (JTABLE) with JPROC for each day to be modeled
-   Configure the CCTM run script to loop through the days to be modeled, using the correct input files for each model day and writing output files with the desired number of time steps

### Initial and boundary conditions

After preparing the meteorology and emissions input data files and determining the model grid and model time periods, the next step for setting up a new CMAQ simulation is creating initial and boundary conditions (ICs and BCs) for CCTM. The ICON processor provides initial chemical fields of individual species concentrations for a specific modeling domain. BCON provides concentrations of individual chemical species for the grid cells surrounding the modeling domain. ICON and BCON both require two inputs: concentration values for the chemical species needed in the simulation, and a predefined chemical mechanism.

As described in Chapter 5, there are two types of input concentrations for ICON and BCON: either (1) tabulated tropospheric vertical profiles or (2) three-dimensional fields from a previous CMAQ or larger-scale CTM simulation, such as GEOS-CHEM (2009) or MOZART (2009). The input file type to use for ICON and BCON depends on whether the simulation is a restart of a previous run and/or whether the simulation is a nest of a larger parent grid. The same chemical mechanism must be selected for ICON, BCON, and CCTM. Both ICON and BCON assume that the input species concentrations are for the selected mechanism; however, a module can be included in each processor to convert from the RADM2 to the CB05 or SAPRC‑99 mechanisms. This option would be necessary, for example, when the CB05 mechanism is to be used in the model but the tabulated tropospheric vertical profiles that are used as inputs to ICON and BCON were constructed using RADM2 mechanism species. Refer to Chapter 5 for information on how to configure ICON and BCON for the two input file types.

Standard operation of CMAQ uses boundary conditions that remain time independent (i.e., the same boundary conditions are used for each day of a multiday case) for the outermost, coarse-grid modeling domain. Nested grids use temporally resolved boundary conditions extracted from the results of a parent-grid CCTM simulation. The initial conditions produced by ICON are time independent for the first spin-up day. If the initial conditions were generated from vertical profile data then they will also be spatially uniform, meaning that for a given layer they will contain the same concentration in every grid cell. The remaining days of a simulation should use spatially heterogeneous output from the previous day’s CCTM simulation to initialize each day. See [Figure 8‑1](#Figure8-1 "wikilink") for an example of the initial and boundary conditions used for a CCTM simulation with a two‑day spin-up followed by a two‑day period of interest. In this example, each of the days was run separately.

<span id=Figure10-1></span>

<center>
![](Figure10-1.png "Figure10-1.png")

</center>
<center>
**Figure 10-1. 36-km four-day modeling period IC/BC schematic**

</center>
When using a nested-grid configuration, the fine-grid-resolution grids can use time-varying boundary conditions generated by BCON with input from time-varying output concentrations from the coarse-grid CCTM simulation. The initial conditions for start-up of the fine grid are also generated using concentrations from the coarse grid. Subsequent runs in the period of interest use the last hour of the concentration file generated from the previous day’s run. See Figure 8‑2 for an example of a one-way, nested simulation. This example uses initial and boundary conditions from the coarser 36‑km grid simulation to perform the nested, finer-grid simulation for the same two‑day period of interest.

<span id=Figure10-2></span>

<center>
![](Figure10-2.png "Figure10-2.png")

</center>
<center>
**Figure 10-2. 12-km nested two-day modeling period IC/BC schematic**

</center>
### Input/output file names and locations

Configuring the CMAQ run scripts for a new simulation involves creating an application identifier to use in the name of the CMAQ outputs and to specify the correct input and output file names and locations on the system where CMAQ will be run. Application identifiers are selected to uniquely identify the output files with respect to the simulation. For example if you are running a base simulation for the year 2007, a simulation identifier, or APPL setting in CMAQ terms, could be Base07. For time-dependent output files, a date identifier is commonly added to the file name to identify the time period covered by the file. Following the previous example, if you configured a simulation to run on January 5, 2007 (Julian date 2007005), an obvious file name for a CMAQ version 4.7 CCTM concentration file would be CCTMv47.CONC.Base07.2007005.ncf. Additional identifying information, such as the chemistry mechanism name, versioning identifiers for the input meteorology and emissions, and the operating system version (i.e. Linux\_x86\_64) are also commonly added to CMAQ output file names.

Before starting a new CMAQ simulation, the user needs to confirm the existence of the input files for all of the CMAQ programs, and to check the names of the output files and directory locations. A common error that occurs with all of the CMAQ programs is an execution failure caused by output files that already exist. If a CMAQ program does not run successfully for this reason, the incomplete or erroneous output files must be manually deleted, or else the *DISP* variable should be invoked in the CMAQ run scripts (Chapter 5) to dispose of the output files before attempting to rerun the program. Alternatively, if the names of the output files are not correctly configured, an error will occur immediately when CMAQ is run. Check the last few lines of the standard error from the CMAQ program to determine whether the problem was due to incorrect specifications of input/output files.

### Science option configuration

The CMAQ scripts as they are distributed use a default science option configuration. While this default configuration is acceptable for all applications of the model, it is not the only configura­tion that can be used with CMAQ. The ICON and BCON science options depend on the nature of the input data, the chemical mechanism chosen, and whether one-way nests are being used. JPROC science options are limited by the choice of chemical mechanism. MCIP output is affected by the source of input meteorological model output (i.e., WRF‑ARW or MM5), the option to recalculate some of the meteorology variables, and the method used to compute dry deposition velocities for various species. CCTM science configuration is the most complex of all the CMAQ programs because it contains a number of combinations of different transport algorithms, chemistry options, and special features, such as process analysis. To see a choice of all the different options available for configuring CCTM, refer to Section 5.3.2.2.

Not all of the combinations of the various CCTM configuration options have been tested. It is possible that some combinations of different science modules will not build a working executable. Generally, here are few rules to follow when configuring CCTM:

-   For configurations that use the Yamartino model driver, the Yamartino options must be used for the initialization module, the advection routines, and the concentration adjustment scheme.
-   For configurations that do not use the Yamartino model driver, the Yamartino options cannot be used for the initialization module and the advection routines, and *the density-based concentration adjustment scheme must be used*.
-   Ensure that there is consistency in the selection of the chemistry mechanism and the aerosol module; for example, the *aero4*aerosol module cannot be used with a chemistry mechanism that is tagged “ae5.
-   Ensure that there is consistency in the selection of the chemistry mechanism with the cloud module; a chemistry mechanism that contains linkages to aqueous chemistry cannot be used when the *noop* option for the cloud module is selected.
-   The EBI chemistry solver is mechanism-dependent and must be consistent with the chemistry mechanism; for example, the EBI solver for CB05 cannot be used with a SAPRC‑99-based chemistry mechanism.

The availability of different science options in CMAQ creates the flexibility to select a configuration that optimizes the model performance for different applications. Through testing the various science configurations with respect to model accuracy and speed, the CMAQ user can find the combination of science options that gives the best performance for a particular modeling study.

References
----------

GEOS-CHEM (2009) <http://wiki.seas.harvard.edu/geos-chem/index.php/Main_Page>

MOZART (2009), <http://www.mpimet.mpg.de/en/wissenschaft/modelle/mozart.html>

<span id=Section11></span>

CODE MANAGEMENT AND DEVELOPMENT
===============================

As a public domain model, CMAQ is the product of contributions from many developers, whose numbers are only expected to increase with the number of users worldwide. Some degree of standardization is necessary for management and archiving of these development versions, as well as to compile and execute the code once it is ready for use, and to submit it to the CMAS Center for archiving and benchmark testing. This chapter provides guidance on source code manage­ment, coding guidelines for new code development, the compilation of new source code using the build scripts, and guidelines for writing shell scripts usable by CMAQ. Much of this informa­tion is derived from Chapter 18 (Young, 1999) in Byun and Ching (1999), with updates where appropriate, particularly for new versions of the model code and for the Fortran 90 standard. The chapter also includes the procedure that is in place for distributing code versions other than the operational CMAQ that are submitted to the development code archives.

Source Code Management
----------------------

### The need for a configuration-management tool

Faced with a large and growing community that uses and develops a wide variety of programs, modules, and codes, it is imperative to systematically manage the cross-community access to this software. Typically, successful management of software involves the following:

-   A repository – a place where all of the public code resides.
-   The concept of archived code – codes that have been deposited into the repository in such a manner that anyone can extract the exact code at a later time. This involves some kind of transformation program to maintain master copies of the codes with embedded change tables.
-   The concept of revision control – archiving codes results in modifying the tags or unique revision identifiers in the change tables in the master copies in order to recover the exact code at a later date.
-   The concept of released code – codes that have reached some state of maturity and have been designated with some kind of “released” status. They can be used with reasonable expectation of reliability. The paradigm used employs the following scenario:
    1.  A user modifies or develops code. The code may be one subroutine or many, possibly constituting whole science modules. The code may originate from “scratch,” or be extracted from the repository and modified.
    2.  After testing or reaching a point of being satisfied with his/her results, he/she decides to save it in the repository so that others can have access to it.
    3.  Some archived codes may still be in an experimental, or development, state, while others may be reasonably stable and more completely tested. The latter may be designated as “released.” There is no enforceable means to control access based on an experimental or released state. The community will have, and should have, access indiscriminately, well aware that using development-state code is risky.
    4.  As the user continues to work with the codes, he/she may make enhancements or discover and fix errors. The upgrades are then installed in the repository, which automatically assigns unique revision identifiers.
    5.  The repository is located where it is conveniently accessible to all users, and is maintained by an administrator who sets and enforces general access rules.

### Choice of a configuration-management tool

Prior to CMAQ version=5.0.2, CMAQ developers used [CVS](https://en.wikipedia.org/wiki/Concurrent_Versions_System) for versioning, and distributed tarballs included CVS artifacts (e.g., files with names ending with ',v'). Starting with version=5.0.2, CMAQ developers switched to [git](https://en.wikipedia.org/wiki/Git_%28software%29).

#### CVS explained

There are many configuration management tools, both free and commercially available. We chose The Concurrent Versions System (CVS) mainly because of its versatility. CVS controls the concurrent editing of sources by several users working on releases built from a hierarchical set of directories. CVS uses the Revision Control System (RCS) as the base system. Other reasons that CVS was an attractive choice include the following:

-   It works on virtually all UNIX and Linux platforms and on many PCs.
-   It is publicly available and free.

The CVS wiki states that “CVS uses a client-server architecture: a server stores the current version(s) of a project and its history, and clients connect to the server in order to ‘check out’ a complete copy of the project, work on this copy and then later ‘check in’ their changes. Typically, the client and server connect over a LAN or over the Internet, but client and server may both run on the same machine if CVS has the task of keeping track of the version history of a project with only local developers. The server software normally runs on UNIX and Linux.

“Several developers may work on the same project concurrently, each one editing files within their own ‘working copy’ of the project, and sending (or checking in) their modifications to the server. To avoid the possibility of people stepping on each other's toes, the server will only accept changes made to the most recent version of a file. Developers are therefore expected to keep their working copy up-to-date by incorporating other people’s changes on a regular basis. This task is mostly handled automatically by the CVS client, requiring manual intervention only when a conflict arises between a checked-in modification and the yet-unchecked local version of a file.” Thus, CVS adds power and features that are attractive for the CMAQ system.

#### The CVS repository

The CVS repository structure, i.e., the UNIX directory hierarchy, follows the class/module organ­ization discussed in Young (1999). The repository is actually divided into many reposi­tories, one for each generic model. This division makes it easier to maintain the class/module organization that is important for the model-building operation described in Chapter 8. CVS allows for the use of a “modules” file,[2] which enables a user to easily check out or extract a complete CMAQ module. For example, a user might check out a module to make code modifications. Complete modules are checked out during the CMAQ model building operation. The following shows a small portion of a symbolic CVS UNIX directory tree that represents the current structure for CCTM:

+-\> CCTM

+-\> CVSROOT *(CVS administrative files)*

+-\> src

+-\> adjcon

| +-\> adjcon\_noop --\> RCS files

| +-\> denrate --\> RCS files

+-\> aero

| +-\> aero\_noop --\> RCS files

| +-\> aero4 --\> RCS files

| +-\> aero5 --\> RCS files

| +-\> aero5\_txhg --\> RCS files

+-\> aero\_depv

| +-\> aero\_depv\_noop --\> RCS files

| +-\> aero\_depv2 --\> RCS files

+-\> chem

| +-\> chem\_noop --\> RCS files

| +-\> smvgear --\> RCS files

| +-\> ros3 --\> RCS files

| +-\> ebi\_cb05cltx\_ae5 --\> RCS files

| +-\> ebi\_cb05cltxhg\_ae5 --\> RCS files

| +-\> ebi\_saprc99 --\> RCS files

| +-\> ebi\_saprc99 --\> RCS files

The symbolic tree is shown relative to the subdirectory in the repository named for the CCTM model. Similar trees exist for each of the generic models. The RCS files are the revision control history files that contain the change tables to reconstruct the actual source code according to a specific revision identifier. The tree closely follows the organization of classes and modules for CCTM and contains alternate modules within the classes. In particular, most classes contain a “no-operation” (noop) module that allows a user to essentially turn off that particular science process modeling. This is useful, for example, in debugging, where rapid turnaround is important, and a computationally demanding module that is not needed can be bypassed.

Guidelines for Developing New CMAQ Source Code
----------------------------------------------

### Object-oriented concepts

To make the CMAQ system robust and flexible, object-oriented concepts were incorporated into the design of the system. The incorporation of these ideas helps developers avoid introducing errors when code modifications are needed. Additionally, the system can easily and efficiently be modified, allowing the user to quickly create models for different applications. The implemen­tation language for CMAQ is Fortran 90, which imposes limits on how far one can go in terms of object-oriented design. In particular, because Fortran is a static language, objects cannot be instantiated dynamically; they must be declared explicitly in the source code to be created at compile time. However, to encourage a user community that will be contributing code for future enhancements, every attempt has been made to adhere to the Fortran 90 standard.

### Global name data table

To implement modularity and data independence, we have employed design ideas that draw heavily from the object-oriented concept of ''inheritance ''and code re-use. The data structures in the codes that deal with the chemical mechanism, I/O API, logical file names, general constants, and pointers are determined by Fortran declarations in data and parameter statements in the CMAQ system. These data structures pertain to a particular application and are meant to apply globally—not just to one particular CCTM through all its subroutines, but also to all the models that supply data to CCTM for that application. These data structures are contained in Fortran INCLUDE files, which are essentially header files, included in the declaration sections near the top of the Fortran code source files. The inclusion of these source files is made automatic by using a generic string that represents the INCLUDE file and that is parsed and expanded to the actual INCLUDE file during a preprocessing stage in the compilation. The Fortran global INCLUDE files contain name tables that define:

1.  The chemical mechanism;
2.  The I/O API interface, including logical file names;
3.  The global modeling constants; and
4.  Other constants or parameters that apply across the model.

To effect the implementation of the INCLUDE files into the code, a special compiling system, Bldmake, was developed (Fine et al., 1998), which reads a configuration file that, based on the application, completely determines the model executable to be built. The ASCII configuration file can be generated either by the CMAQ system or by the users following a few, simple syntactical rules. In addition to the global INCLUDE files, the configuration file contains module commands that tell Bldmake to extract the codes for that module from the model code repository for compilation.

### Thin Interface

As mentioned in Section 9.2.2, CMAQ is designed to be robust and flexible with respect to the interchange of modules and the elimination of cross-module data dependencies. Consequently, the concept of a “thin interface” has been employed in the design, which applies principally to the class-drivers (i.e. the top level call to a science module). At a minimum, the thin interface implementation implies the following requirements:

-   Eliminate global memory references (across modules). This implies no common blocks across modules, no hidden data paths, and no “back doors.”
-   Each module reads and interpolates its required data independently. The I/O API helps to ensure this kind of data independence.
-   Standardized argument list (CGRID, Date, Time, TimeStep) for calling the class-driver. See the example in Section 9.2.6. These requirements attempt to incorporate the object-oriented idea of encapsulation in the CMAQ design. Rumbaugh et al. (1991) suggest that “Encapsulation (also information hiding) consists of separating the external aspects of an object, which are accessible to other objects, from the internal implementation details of the object, which are hidden from other objects. Encapsulation prevents a program from becoming so interdependent that a small change has massive ripple effects. The implementation'' ''of an object can be changed without affecting the applications that use it.”

The encapsulation design makes the CMAQ system safer and enables the transaction processing, plug-and-play capability. This design also makes it easier for a user to trace data and usage within a module, particularly at the class-driver level.

### Coding guidelines

To maintain the object-oriented concepts implemented in the CMAQ system design, we have established a small set of coding guidelines that apply to those who develop CMAQ science modules and affect the low-level design of the models. We have developed standards to control data depen­dencies at the class-driver level, but we have not propagated these coding standards to the submodule level.

1.  The models are generally coded in Fortran (both Fortran 90 and Fortran 77 conventions are used by various developers). It is possible to link in subroutines written in the C language, although this has not been done within the current CMAQ implementation. While the Fortran 90 compiler will compile Fortran 77 code, the reverse is not true. Thus the Makefiles are set up to invoke the Fortran 90 compiler.
2.  To enable code compatibility between the Fortran 77 compiler and Fortran 90 code, the following guidance is provided: Line length beyond 72 characters is permissible in Fortran 90 (with line continuation indicated by an ending ‘&’), but not in Fortran 77; therefore, insertion of the ‘&’ in column 73 of the first line and in column 6 of the next line of the Fortran 90 code will ensure compatibility with both compilers (the ‘&’ at the beginning of a line is “in principle” ignored by the Fortran 90 compiler, but interpreted as a continuation character by the Fortran 77 compiler if it appears in column 6).
3.  The modules must be controlled by a top-level class-driver routine, whose calling arguments must be the computational concentration grid array (CGRID), the current scenario date (Date), scenario time (Time), and the controlling time step vector (TimeStep). (See Section 9.2.3 above.)
4.  The class-driver is also responsible for any temporal integration required within the module. (The time steps for process integration at the module level are usually shorter than those of the CCTM synchronization time step.)
5.  Any reads and writes for the module should be done at the level of the class-driver routine. Although not absolutely necessary, this is strongly suggested because it is usually much easier to control the timing of the data accesses at the highest level of the module where the current scenario date and time are known.
6.  Use the Fortran declaration IMPLICIT NONE to maintain some control on typographic errors and undefined variables. The use of IMPLICIT NONE forces the developer to declare all internal variables. This is standard in Fortran 90.
7.  Use the global INCLUDE files for chemical mechanism data, and other data where available.
8.  Use the I/O API for external data references where appropriate. For an illustration of these rules, see the code template provided in Section 9.2.6.

At the submodule level, there are no strict I/O or coding standards. Here it is envisioned that individual researchers/programmers use their own coding styles for their algorithms. However, the following suggestions are offered to facilitate the potential incorporation of a module into the CMAQ system:

-   In general, it is expected that MKS units are used for input and output variables, as these units have been standardized throughout the CMAQ system. Within a submodule subroutine, whatever units are most convenient can be used. However, the developer must be responsible for any unit conversions to MKS for input and output, and thus avoid potential errors.
-   For efficiency and performance considerations, operations may need to be done on groups of grid cells (a block of cells) at a time. If there are ''N ''cells in the block and the entire domain contains ''M ''cells, then the entire domain can be decomposed into ''M/N *blocks. The default value of N is set to 500. For operations in the horizontal (*x,y''), the cell constraint becomes X×Y≤N, where ''X *= number of cells in the*x''-direction, and *Y*= number of cells in the y-direction. For operations in both the horizontal and vertical, the constraint becomes X×Y×Z≤N, where ''Z *= number of cells in the*z''-direction. There may be some operations, such as for some horizontal advection schemes, where this decomposition into blocks becomes more difficult or impossible.

### Documentation guidelines

Appropriate documentation is critical to the ease of use and maintainability of code developed for CMAQ. The official released version of CMAQ contains extensive in-line documentation and references to pertinent technical information whenever possible. Given the increasing number of new developers and code modules, the following guidelines are provided for new code developed for CMAQ:

-   The code revision history should be initiated or updated as appropriate for new and modified code, indicating the author, date, and nature of the revision. The revision history appears at the top of the subroutine.
-   Complete references to the pertinent technical documents should be provided whenever possible, and listed in comment lines immediately following the revision history notes. They should be cited in comments preceding, or embedded in-line with, the relevant code segments.
-   In-line documentation of the variable definitions indicating units is highly recommended in both subroutines and INCLUDE files, to facilitate the correct implementation of any code modifications in the future. This information is generally included in comments embedded in-line with the declaration of each variable.

### Science process code template

The following example from CMAQ v4.7 illustrates a science process class-driver Fortran 90 subroutine. Code developers should follow this template, where appropriate, to maximize the benefit from the design concepts implemented in CMAQ. This template is generic and demonstrates many of the available features. Some class drivers and most other subprograms within a module may not have, nor require, most or any of these features. (The numbers at the left-hand margin refer to footnotes and are not part of the code, and the text within “\< \>” indicates code removed from the example for brevity in this section)

<center>
**Example of Science Process Class-Driver**

</center>
SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )

( 1)C-----------------------------------------------------------------------

( 1)C Function:

( 1)C Preconditions:

( 1)C Subroutines and Functions Called:

( 1)C Revision History:

( 1)C References:

C-----------------------------------------------------------------------

( 2) USE AERO\_EMIS ! inherits GRID\_CONF

( 2) USE SUBST\_MODULES ! stenex

! USE SUBST\_GLOBAL\_SUM\_MODULE ! stenex

( 3) IMPLICIT NONE

! INCLUDE SUBST\_HGRD\_ID ! horizontal dimensioning parameters

! INCLUDE SUBST\_VGRD\_ID ! vertical dimensioning parameters

( 4) INCLUDE SUBST\_RXCMMN ! model mechanism name

( 4) INCLUDE SUBST\_GC\_SPC ! gas chemistry species table

( 4) INCLUDE SUBST\_GC\_EMIS ! gas chem emis surrogate names and map table

( 4) INCLUDE SUBST\_GC\_DEPV ! gas chem dep vel surrogate names and map table

( 4) INCLUDE SUBST\_GC\_DDEP ! gas chem dry dep species and map table

INCLUDE SUBST\_GC\_DIFF ! gas chem diffusion species and map table

( 4) INCLUDE SUBST\_AE\_SPC ! aerosol species table''' '''

! INCLUDE SUBST\_AE\_EMIS ! aerosol emis surrogate names and map table

( 4) INCLUDE SUBST\_AE\_DEPV ! aerosol dep vel surrogate names and map table

( 4) INCLUDE SUBST\_AE\_DDEP ! aerosol dry dep species and map table

( 4) INCLUDE SUBST\_AE\_DIFF ! aerosol diffusion species and map table

( 4) INCLUDE SUBST\_NR\_SPC ! non-reactive species table

( 4) INCLUDE SUBST\_NR\_EMIS ! non-react emis surrogate names and map table

( 4) INCLUDE SUBST\_NR\_DEPV ! non-react dep vel surrogate names and map table

( 4) INCLUDE SUBST\_NR\_DDEP ! non-react dry dep species and map table

( 4) INCLUDE SUBST\_NR\_DIFF ! non-react diffusion species and map table

( 4) INCLUDE SUBST\_TR\_SPC ! tracer species table

( 4) INCLUDE SUBST\_TR\_EMIS ! tracer emis surrogate names and map table

( 4) INCLUDE SUBST\_TR\_DEPV ! tracer dep vel surrogate names and map table

( 4) INCLUDE SUBST\_TR\_DDEP ! tracer dry dep species and map table

( 4) INCLUDE SUBST\_TR\_DIFF ! tracer diffusion species and map table

! INCLUDE SUBST\_EMLYRS\_ID ! emissions layers parameter

( 5)\#ifdef emis\_chem

( 5) INCLUDE SUBST\_EMPR\_CH ! emissions processing in chem

( 5)\#else

( 5) INCLUDE SUBST\_EMPR\_VD ! emissions processing in vdif

( 5)\#endif

( 6) INCLUDE SUBST\_PACTL\_ID ! PA control parameters

( 6) INCLUDE SUBST\_CONST ! constants

( 6) INCLUDE SUBST\_FILES\_ID ! file name parameters

( 6) INCLUDE SUBST\_IOPARMS ! I/O parameters definitions

\#include SUBST\_IODECL \# I/O definitions and declarations

! INCLUDE SUBST\_COORD\_ID ! coordinate and domain definitions (req IOPARMS)

( 7) CHARACTER( 120 ) :: XMSG = ' '

( 8)C Arguments:

! REAL CGRID( NCOLS,NROWS,NLAYS,\* ) ! concentrations

! REAL :: CGRID( :,:,:,: ) ! concentrations

( 8) REAL, POINTER :: CGRID( :,:,:,: ) ! concentrations

( 8) INTEGER JDATE ! current model date, coded YYYYDDD

( 8) INTEGER JTIME ! current model time, coded HHMMSS

( 8) INTEGER TSTEP( 2 ) ! time step vector (HHMMSS)

`! TSTEP(1) = local output step`

`! TSTEP(2) = sciproc sync. step (chem)`

( 9)C Parameters:

( 9)C explicit, THETA = 0, implicit, THETA = 1

( 9) REAL, PARAMETER :: THETA = 0.5, ! Semi-implicit (Crank-Nicolson)

( 9) & THBAR = 1.0 - THETA

( 9) REAL THRAT ! THBAR/THETA

( 9) INTEGER, PARAMETER :: N\_SPC\_DDEP = N\_GC\_DDEP

( 9) & + N\_AE\_DDEP

( 9) & + N\_NR\_DDEP

( 9) & + N\_TR\_DDEP

( 9)\< \>

( 9)C number of species on the PM emissions input file. Set in OPEMIS

( 9)C the value changes with the type of emissions file.

( 9) INTEGER, SAVE :: NAESPCEMIS

( 9) REAL, PARAMETER :: M2PHA = 1.0E+04 ! 1 hectare = 1.0e4 m\*\*2

( 9) REAL, PARAMETER :: CMLMR = 1.0E+06 ! ppmV/Molar Mixing Ratio

( 9) REAL, PARAMETER :: CNVTD = M2PHA / CMLMR / MWAIR ! combined ddep

! conversion factor

( 9) REAL, PARAMETER :: GPKG = 1.0E+03 ! g/Kg

( 9) REAL, PARAMETER :: MGPG = 1.0E+06 ! micro-g/g

(10)C External Functions not previously declared in IODECL3.EXT:

(10) INTEGER, EXTERNAL :: SECSDIFF, SEC2TIME, TIME2SEC

(10) LOGICAL, EXTERNAL :: ENVYN

(11)C File variables:

(11)\< \>

(12)C Local Variables:

(12) CHARACTER( 16 ), SAVE :: PNAME = 'VDIFFIM'

(12)\< \>

(12) REAL, ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: ) ! total emissions array

(12)\< \>

(13) INTERFACE

(13) SUBROUTINE RDMET( MDATE, MTIME, RDEPVHT, RJACM, RVJACMF, RRHOJ, DENS1 )

(13) IMPLICIT NONE

(13) INTEGER, INTENT( IN ) :: MDATE, MTIME

(13) REAL, INTENT( OUT ) :: RDEPVHT( :,: )

(13) REAL, INTENT( OUT ) :: RJACM ( :,:,: )

(13) REAL, INTENT( OUT ) :: RVJACMF( :,:,: )

(13) REAL, INTENT( OUT ) :: RRHOJ ( :,:,: )

(13) REAL, INTENT( OUT ) :: DENS1 ( :,: )

(13) END SUBROUTINE RDMET

(13) SUBROUTINE RDDEPV ( MDATE, MTIME, MSTEP, CGRID, DEPV )

(13) IMPLICIT NONE

(13) INTEGER, INTENT( IN ) :: MDATE, MTIME, MSTEP

(13) REAL, POINTER :: CGRID( :,:,:,: )

(13) REAL, INTENT( OUT ) :: DEPV( :,:,: )

(13) END SUBROUTINE RDDEPV

(13)\< \>

END INTERFACE

C-----------------------------------------------------------------------

(14) IF ( FIRSTIME ) THEN

(14) FIRSTIME = .FALSE.

(14) LOGDEV = INIT3()

(14) C for emissions (from COORD.EXT) .......................................

(14) IF ( GDTYP\_GD .EQ. LATGRD3 ) THEN

(14) DX1 = DG2M \* XCELL\_GD ! in m.

(14) DX2 = DG2M \* YCELL\_GD

(14) & \* COS( PI180\*( YORIG\_GD + YCELL\_GD \* FLOAT( GL\_NROWS/2 ))) ! in m.

(14) ELSE

(14) DX1 = XCELL\_GD ! in m.

(14) DX2 = YCELL\_GD ! in m.

(14) END IF

(14) C create global maps

(14) CALL VDIFF\_MAP ( DF2EM, DF2DV, DD2DV, DEPV\_MAP, DIFF\_MAP, DDEP\_SPC,

(14) & DV2DF )

(14) C set vertical layer definitions from COORD.EXT

(15) ALLOCATE ( RDX3F( NLAYS ), STAT = ALLOCSTAT )

(15) ALLOCATE ( RDX3M( NLAYS ), STAT = ALLOCSTAT )

(15) IF ( ALLOCSTAT .NE. 0 ) THEN

(15) XMSG = 'Failure allocating RDX3F or RDX3M'

(15) CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

(15) END IF

(14) \< other calculations that need to be performed only the first time \>

`END IF ! if Firstime`

(16) MDATE = JDATE

(16) MTIME = JTIME

(16) MSTEP = TIME2SEC( TSTEP( 2 ) )

(16) DTSEC = FLOAT( MSTEP )

(16) CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP / 2 ) )

C read & interpolate met data

(17) CALL RDMET ( MDATE, MTIME, RDEPVHT, RJACM, RVJACMF, RRHOJ, DENS1 )

C read & interpolate deposition velocities

`< perform other operations > `

(18) IF ( LIPR ) THEN

(18) DO S = 1, N\_SPC\_EMIS+1

(18) DO L = 1, ELAYS

(18) DO R = 1, MY\_NROWS

(18) DO C = 1, MY\_NCOLS

(18) EMIS\_PA( C,R,L,S ) = VDEMIS( S,L,C,R )

(18) END DO

(18) END DO

(18) END DO

(18) END DO

(18) CALL PA\_UPDATE\_EMIS ( 'VDIF', EMIS\_PA, JDATE, JTIME, TSTEP )

(18) END IF

(19) CALL EDYINTB ( EDDYV, DT, JDATE, JTIME, TSTEP( 2 ) )

\< Perform other operations to set up for tridiagonal solver \>

(20) DO 345 R = 1, MY\_NROWS

(20) DO 344 C = 1, MY\_NCOLS

\< Perform operations \>

(21) DO 301 N = 1, NSTEPS( C,R )

\< Perform operations \>

(21) 301 CONTINUE ! end time steps loop

\< Update concentration and deposition arrays \>

(20) 344 CONTINUE ! end loop on col C

(20) 345 CONTINUE ! end loop on row R

\< Perform other operations \>

C If last call this hour: write accumulated depositions:

(22) WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )

(22) IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN

(22) MDATE = JDATE

(22) MTIME = JTIME

(22) CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) )

(22) WSTEP = 0

(22) DO V = 1, N\_SPC\_DDEP

(22) S = DD2DV( V )

(22) DO R = 1, MY\_NROWS

(22) DO C = 1, MY\_NCOLS

(22) WRDD( C,R ) = DDEP( S,C,R )

(22) END DO

(22) END DO

(22) IF ( .NOT. WRITE3( CTM\_DRY\_DEP\_1, DDEP\_SPC( V ),

(22) & MDATE, MTIME, WRDD ) ) THEN

(22) XMSG = 'Could not write ' // CTM\_DRY\_DEP\_1 // ' file'

(22) CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

(22) END IF

(22) END DO

(18) EMIS\_PA( C,R,L,S ) = VDEMIS( S,L,C,R )

(18) END DO

(18) END DO

(18) END DO

(18) END DO

(18) CALL PA\_UPDATE\_EMIS ( 'VDIF', EMIS\_PA, JDATE, JTIME, TSTEP )

(18) END IF

(19) CALL EDYINTB ( EDDYV, DT, JDATE, JTIME, TSTEP( 2 ) )

\< Perform other operations to set up for tridiagonal solver \>

(20) DO 345 R = 1, MY\_NROWS

(20) DO 344 C = 1, MY\_NCOLS

\< Perform operations \>

(21) DO 301 N = 1, NSTEPS( C,R )

\< Perform operations \>

(21) 301 CONTINUE ! end time steps loop

\< Update concentration and deposition arrays \>

(20) 344 CONTINUE ! end loop on col C

(20) 345 CONTINUE ! end loop on row R

\< Perform other operations \>

C If last call this hour: write accumulated depositions:

(22) WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )

(22) IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN

(22) MDATE = JDATE

(22) MTIME = JTIME

(22) CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) )

(22) WSTEP = 0

(22) DO V = 1, N\_SPC\_DDEP

(22) S = DD2DV( V )

(22) DO R = 1, MY\_NROWS

(22) DO C = 1, MY\_NCOLS

(22) WRDD( C,R ) = DDEP( S,C,R )

(22) END DO

(22) END DO

(22) IF ( .NOT. WRITE3( CTM\_DRY\_DEP\_1, DDEP\_SPC( V ),

(22) & MDATE, MTIME, WRDD ) ) THEN

(22) XMSG = 'Could not write ' // CTM\_DRY\_DEP\_1 // ' file'

(22) CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

(22) END IF

(22) END DO

(22) WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )

(22) & 'Timestep written to', CTM\_DRY\_DEP\_1,

(22) & 'for date and time', MDATE, MTIME

(18) IF ( LIPR ) THEN

! DO V = 1, N\_SPC\_DDEP

(18) DO V = 1, N\_SPC\_DEPV

(18) DO R = 1, MY\_NROWS

(18) DO C = 1, MY\_NCOLS

(18) DDEP\_PA( C,R,V ) = DDEP( V,C,R )

(18) END DO

(18) END DO

(18) END DO

(18) CALL PA\_UPDATE\_DDEP ( 'VDIF', DDEP\_PA, JDATE, JTIME, TSTEP )

(18) END IF

C re-set dry deposition array to zero

DDEP = 0.0

END IF

(23) RETURN

(23) END

**Footnotes:**

*( 1)Header comments - Highly recommended for internal documentation.*

*( 2)USE \<module name\> includes the Fortran source file specified.*

*( 3)IMPLICIT NONE must be used in Fortran 90, i.e., implicit declarations are not supported. This dramatically reduces errors due to typos and undefined variables.*

*( 4)Chemical mechanism array dimensioning and looping global variables.*

*( 5)C preprocessor flags that determine which emissions control dimensioning and looping variables are compiled.*

*( 6)Other global array dimensioning and looping global variables, including those for the I/O API. The logical variable LIPR is defined in the SUBST\_PACTL\_ID INCLUDE file for use at lines labeled (18).*

*( 7)Local variable declaration. Note syntax differences from Fortran-77.*

*( 8)Declarations for the argument list (standardized).*

*( 9)Declarations and PARAMETER statements for local Fortran parameters, illustrating in-line documentation of variables and units. Note syntax differences from Fortran-77.*

*(10)Declarations for external functions not previously declared.*

*(11)Declarations for arrays to hold external file data.*

*(12)Declarations and definitions for local and saved variables, and dynamic memory allocations.*

*(13)Interface is a convenient way to declare calling arguments to a subroutine as input, output, or both in the calling program through the INTENT variable specification (as IN, OUT, or IN OUT). No other declaration of the calling arguments is necessary in the calling program. If IN only, the values of arguments can be passed explicitly in the subroutine call. If OUT, the argument must be passed as a variable.*

*(14)Code section for subroutine initialization and for any local data that need not be set at every entry into the subroutine. Such data would require a SAVE statement in the declarations. For example, FIRSTIME is initialized to .TRUE. in the local variables section.*

*(15)Illustration of memory allocation for a variable declared as allocatable. In this example, NLAYS is accessed from the COORD.EXT file.*

*(16)Illustrates using an I/O API function to set file interpolation time.*

*(17)Meteorological and other data are read and interpolated through a series of subroutine calls. These subroutines in turn use I/O API utilities to perform the time interpolation of the desired met variables, deposited and emitted species.*

*(18)Call to process analysis routine to obtain data for the optional integrated process rates function.*

*(19)Illustrates call to another science process within the module.*

*(20)Main computational loop over the horizontal grid.*

*(21)Time-step loop over subsynchronization time step intervals.*

*(22)Illustrates writing to an I/O API file within a module.*

*(23)Subroutine end*

Compiling CMAQ with New Source Code
-----------------------------------

The following steps are recommended for compiling CMAQ when a new module has been developed. The procedure creates a Makefile, which can then be modified to add the new module in the appropriate class, but the same steps can be used to obtain a configuration file that can be similarly modified to add the new module.

-   On the computational platform of choice, create a working directory for the model download.
-   Download the appropriate tar file CMAQv5.tar.gz from the CMAS web site ([www.cmascenter.org](http://www.cmascenter.org/)) for the chosen platform. Users must register before proceeding with the download steps.
-   Untar the file using the command:

\> tar xvfz CMAQv5.tar.gz

This will expand a directory labeled *scripts* that contains all the scripts necessary to compile and run CMAQ.

-   Either install the CMAQ source code and libraries (Chapter 3) or create links to the CMAQ models and libraries as follows:

\> ln –s \<models directory\> models

\> ln –s \<lib directory\> lib

-   In the scripts/cctm subdirectory, modify a file called bldit.cctm as follows:

uncomment the line “set MakeOpt” by removing the leading ‘\#’ character.

-   Execute the bldit.cctm script. This creates a Makefile as well as a configuration file in the subdirectory scripts/cctm/BLD\_V5f, where the model code has been copied.
-   The Makefile can be modified to compile and link the new module by specifying \<full path name\>.o for the object file that needs to be linked in. It is essential that a source file with the corresponding name (with extension “.F”) reside in the same directory as the specified path name for the object file.
-   Issue the “make” command to compile the source code into an executable.

\> make –f Makefile

Guidelines to Writing Shell Scripts for CMAQ
--------------------------------------------

To run a model executable, various UNIX environment variables must be set in the shell that invokes the execute command. Generally, these variables involve the modeling scenario start date and time, the run duration, the output time step interval, various internal code flags that differ among the models, and all the input and output logical (symbolic) file names. There are various ways that external file names can be referenced in the source code, and UNIX platforms can link them by using environment variables. There are I/O API utility functions that allow users to easily access these variables in the code in a generic and portable manner. An additional feature that is provided through the I/O API is the ability to declare a file “volatile” by appending a -v flag in the shell’s declaration for the environment variable. By doing this, the I/O API will cause the netCDF file to update (sync) its disk copy after every write and thereby update the netCDF header. Otherwise, netCDF (I/O API) file headers are not updated until the files are closed. This feature is useful, for example, for allowing a user to analyze an open netCDF file using visualization tools while the model is executing. It is also useful in case of a system crash. A CCTM model can be restarted at the scenario time step after the last successful write using the aborted output file as the input initial data.

The following is a sample run script that can be downloaded from the CMAS web site. The build and run scripts are part of the downloaded tar file from this site.

\#! /bin/csh –f

\# ======================== CCTMv4.7 Run Script ====================== \#

\# Usage: run.cctm \>&! cctm\_e3a.log & \#

\# The following environment variables must be set for this script to \#

\# execute properly: \#

\# setenv M3DATA = input/output data directory \#

\# To report problems or request help with this script/program: \#

\# <http://www.cmascenter.org/html/help.html> \#

\# =================================================================== \#

\#\> Check that M3DATA is set:

if ( ! -e \$M3DATA ) then

echo " \$M3DATA path does not exist"

exit 1

endif

echo " "; echo " Input data path, M3DATA set to \$M3DATA"; echo " "

set APPL = e3a

set CFG = e3a

\#set CFG = \$APPL

set EXEC = CCTM\_\$CFG \# ctm version

\#\> horizontal domain decomposition

\#setenv NPCOL\_NPROW "1 1"; set NPROCS = 1 \# single processor setting

setenv NPCOL\_NPROW "4 2"; set NPROCS = 8

\#\> for Scyld Beowulf ...

\#setenv NP \$NPROCS

\#setenv BEOWULF\_JOB\_MAP -1:-1:0:0:1:1:2:2:3:3:4:4

\#echo " task-processor map \`beomap\`"

\#\> Set the working directory:

set BASE = \$cwd

cd \$BASE; date; cat \$BASE/cfg.\$CFG; echo " "; set echo

\#\> timestep run parameters

set STDATE = 2001203 \# beginning date

set STTIME = 000000 \# beginning GMT time (HHMMSS)

set NSTEPS = 240000 \# time duration (HHMMSS) for this run

set TSTEP = 010000 \# output time step interval (HHMMSS)

\#\> set log file [ default = unit 6 ]; uncomment to write standard output to a log

\#setenv LOGFILE \$BASE/\$APPL.log

\#\> turn off excess WRITE3 logging

setenv IOAPI\_LOG\_WRITE F

\#\> max sync time step (sec) (default is 720)

\#setenv CTM\_MAXSYNC 300

\#\> aerosol diagnostic file [ T | Y | F | N ] (default is F|N)

\#setenv CTM\_AERDIAG Y

\#\> sea-salt emissions diagnostic file [ T | Y | F | N ] (default is F|N)

\#setenv CTM\_SSEMDIAG Y

\#\> stop on inconsistent input file [ T | Y | F | N ]

setenv FL\_ERR\_STOP F

\#\> remove existing output files?

\#set DISP = delete

\#set DISP = update

set DISP = keep

\#\> output files and directories

set OUTDIR = \$M3DATA/cctm

if ( ! -d "\$OUTDIR" ) mkdir -p \$OUTDIR

set CONCfile = \$EXEC"CONC".\$APPL \# CTM\_CONC\_1

set ACONCfile = \$EXEC"ACONC".\${APPL} \# CTM\_ACONC\_1

set CGRIDfile = \$EXEC"CGRID".\${APPL} \# CTM\_CGRID\_1

set DD1file = \$EXEC"DRYDEP".\$APPL \# CTM\_DRY\_DEP\_1

set WD1file = \$EXEC"WETDEP1".\$APPL \# CTM\_WET\_DEP\_1

set WD2file = \$EXEC"WETDEP2".\$APPL \# CTM\_WET\_DEP\_2

set SS1file = \$EXEC"SSEMIS1".\$APPL \# CTM\_SSEMIS\_1

set AV1file = \$EXEC"AEROVIS".\$APPL \# CTM\_VIS\_1

set AD1file = \$EXEC"AERODIAM".\$APPL \# CTM\_DIAM\_1

set PA1file = \$EXEC"PA\_1".\$APPL \# CTM\_IPR\_1

set PA2file = \$EXEC"PA\_2".\$APPL \# CTM\_IPR\_2

set PA3file = \$EXEC"PA\_3".\$APPL \# CTM\_IPR\_3

set IRR1file = \$EXEC"IRR\_1".\$APPL \# CTM\_IRR\_1

set IRR2file = \$EXEC"IRR\_2".\$APPL \# CTM\_IRR\_2

set IRR3file = \$EXEC"IRR\_3".\$APPL \# CTM\_IRR\_3

set RJ1file = \$EXEC"RJ\_1".\$APPL \# CTM\_RJ\_1

set RJ2file = \$EXEC"RJ\_2".\$APPL \# CTM\_RJ\_2

\#\> set ancillary log file name extensions

setenv CTM\_APPL \$APPL

\#\> set floor file (neg concs)

setenv FLOOR\_FILE \$BASE/FLOOR\_\${APPL}

\#\> horizontal grid defn; check GRIDDESC file for GRID\_NAME options

setenv GRIDDESC ../GRIDDESC1

setenv GRID\_NAME M\_36\_2001

\#\> species for standard conc

\#setenv CONC\_SPCS "O3 NO ANO3I ANO3J NO2 FORM ISOP ANH4J ASO4I ASO4J"

\#\> layer range for standard conc

\#setenv CONC\_BLEV\_ELEV " 1 4"

\#\> species for integral average conc

setenv AVG\_CONC\_SPCS "O3 NO CO NO2 ASO4I ASO4J NH3"

\#setenv AVG\_CONC\_SPCS "ALL"

\#\> layer range for integral average conc

setenv ACONC\_BLEV\_ELEV " 1 1"

\#\> input files and directories

set OCEANpath = \$M3DATA/emis/2001

set OCEANfile = us36\_surf.40x44.ncf

set EMISpath = \$M3DATA/emis/2001

set EMISfile = emis3d.20010722.US36\_40X44.ncf

\#set TR\_EMpath =

\#set TR\_EMfile =

\#set GC\_ICpath = \$OUTDIR

\#set GC\_ICfile = CCTM\_e3aCGRID.d1b

set GC\_ICpath = \$M3DATA/icon

set GC\_ICfile = ICON\_cb05cl\_M\_36\_2001\_profile

set GC\_BCpath = \$M3DATA/bcon

set GC\_BCfile = BCON\_cb05cl\_M\_36\_2001\_profile

set METpath = \$M3DATA/mcip3/M\_36\_2001

set extn = 010722

set GC2file = GRIDCRO2D\_\${extn}

set GD2file = GRIDDOT2D\_\${extn}

set MC2file = METCRO2D\_\${extn}

set MD3file = METDOT3D\_\${extn}

set MC3file = METCRO3D\_\${extn}

set MB3file = METBDY3D\_\${extn}

set TR\_DVpath = \$METpath

set TR\_DVfile = \$MC2file

\#\> 7-level photolysis data w/ file header

set JVALpath = \$M3DATA/jproc

set JVALfile = JTABLE\_\${STDATE}

set AE\_ICpath = \$GC\_ICpath

set NR\_ICpath = \$GC\_ICpath

set TR\_ICpath = \$GC\_ICpath

set AE\_ICfile = \$GC\_ICfile

set NR\_ICfile = \$GC\_ICfile

set TR\_ICfile = \$GC\_ICfile

set AE\_BCpath = \$GC\_BCpath

set NR\_BCpath = \$GC\_BCpath

set TR\_BCpath = \$GC\_BCpath

set AE\_BCfile = \$GC\_BCfile

set NR\_BCfile = \$GC\_BCfile

set TR\_BCfile = \$GC\_BCfile

\#\> input and output files and directories (boilerplate)

source in\_out.q

if ( \$status ) exit 1

\#\> for the run control ...

setenv CTM\_STDATE \$STDATE

setenv CTM\_STTIME \$STTIME

setenv CTM\_RUNLEN \$NSTEPS

setenv CTM\_TSTEP \$TSTEP

setenv CTM\_PROGNAME \$EXEC

\#\> look for existing log files

set test = \`ls CTM\_LOG\_???.\${APPL}\`

if ( "\$test" != "" ) then

if ( \$DISP == 'delete' ) then

echo " ancillary log files being deleted"

foreach file ( \$test )

echo " deleting \$file"

rm \$file

end

else

echo "\*\*\* Logs exist - run ABORTED \*\*\*"

exit 1

endif

endif

\#\> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

env

ls -l \$BASE/\$EXEC; size \$BASE/\$EXEC

\#\> Executable call for single PE, uncomment to invoke

\# time \$BASE/\$EXEC

\#\> Executable call for multiple PE, set location of MPIRUN script

set MPIRUN = /share/linux/bin/mpich-ch\_p4/bin/mpirun

set TASKMAP = \$BASE/machines8

cat \$TASKMAP

time \$MPIRUN -v -machinefile \$TASKMAP -np \$NPROCS \$BASE/\$EXEC

date

exit

Testing and Distribution of Development Source Code
---------------------------------------------------

The CMAS Center collects, tests, and distributes various operational and development versions of CMAQ through the web site [<http://www.cmaq-model.org>](http://www.cmaq-model.org/). An archive of official releases (both current and past) and development versions of CMAQ is available to the user community. The CMAQ-MADRID and CMAQ-AMSTERDAM developed by AER, Inc. under funding from the Electric Power Research Institute can be downloaded from this archive. As a benefit to the CMAQ community, CMAS periodically updates its documentation on testing such development code versions to include additional feedback as it becomes available, based on users’ experiences with these versions. Questions or comments about development versions of CMAQ such as CMAQ-MADRID should be directed to the developers at AER. Questions or comments about downloading the source code and associated documentation, and on the software development guidelines, may be directed to [<http://www.cmascenter.org>](http://www.cmascenter.org/).

Based on the insights gained from the testing and archiving of a development version of the model such as CMAQ-MADRID, CMAS recom­mends the following steps as the minimum level of coding and testing practices to be adopted by developers wishing to contribute code to the public CMAQ archive:

1.  To make the best use of the CMAQ features in developing new code, the developer should review the coding conventions that are provided in the previous sections of this chapter. [Also see [<http://www.epa.gov/asmdnerl/CMAQ/CMAQscienceDoc.html>](http://www.epa.gov/asmdnerl/CMAQ/CMAQscienceDoc.html)].
2.  New code should be built using the current operational CMAQ version as a template whenever possible. This will facilitate consistency in coding practices, including naming conventions, in-line documentation, and the specification of compile time versus run-time parameters.
3.  Before submitting source code to the CMAS Center, the developer should verify that the code is consistent with the operational CMAQ version from which it was built, especially in the use of common INCLUDE files (such as horizontal and vertical grid definition files) and run-time parameter settings. Mixing code from different operational versions of the CMAQ model within the same development code version can lead to problems in using the generalized CMAQ scripts.
4.  Comprehensive documentation or other references to peer-reviewed literature should be provided for any new science algorithms include in the source code (see Section 9.2.5).
5.  The developer must document the computational platform used for the testing, including type and speed of the processor(s), the compiler version used, and CPU usage. It is recommended that developers use any combination of the above for testing code intended for release through the CMAS Center, to facilitate benchmarking and portability testing by CMAS staff. Any documentation on potential differences in model outputs between different computing platforms would be useful for end-users who may not be able to duplicate the platform on which the model was initially developed and tested. To this end, code testing and documentation of test results by developers, using more than one platform if available, are highly desirable.
6.  The developer should provide all input data for the test case so that interested users may attempt to run the code and reproduce the results on their own platforms.
7.  It is recommended that benchmark results from the testing be provided for at least one 5‑day simulation. Shorter simulations do not provide adequate results from which to discern model trends beyond the spin-up period.
8.  When making incremental changes to model science, the developer should provide documentation of the results, including (a) the results for all variables that show a deviation of greater than 1.0e10<sup>‑6</sup> ppm for the gas-phase species or 1.0e10<sup>‑4</sup> µg m<sup>‑3</sup> for the particulate species from the base model results for the same case, (b) an analysis of what was done to understand these differences, and (c) conclusions of the analysis.
9.  Note that more than one simulation may be necessary to adequately demonstrate seasonal or regional biases, if any, in the results. It is also understood that with models still under development, the analysis may not resolve all differences from the operational model results. It is recommended that these unresolved issues also be documented.

Model developers are also recommended to check the CMAS website to see if there are any additional guidelines that have been recommended since the first set listed above.

References
----------

Fine, S. S., W. T. Smith, D. Hwang, T. L. Turner, 1998: Improving model development with configuration management, IEEE Computational Science and Engineering, 5(1, Ja-Mr), 56-65.

J. Rumbaugh, M. Blaha, W. Premerlani, F. Eddy, and W. Lorensen, 1991: Object-Oriented Modeling and Design, Prentice Hall

Young, J. O.,'' ''Integration of Science Code into Models-3, 1999. In *Science Algorithms of the EPA Models-3 Community Multiscale Air Quality (CMAQ) Modeling System*, D. W. Byun and J. K. S. Ching (ed.), EPA/600/R-99/030, U. S. EPA, Research Triangle Park, NC.

<span id=Section12></span>

ANALYSES TOOLS FOR CMAQ
=======================

Several tools are freely available for visualizing, analyzing, and evaluating CMAQ inputs and outputs. The list includes CMAQ utility tools, m3tools, PAVE, VERDI, the Atmospheric Model Evaluation Tool (AMET), netCDF Operators (NCO), Python-based ioapiTools, UNIDATA Integrated Data Viewer (IDV), and the NCAR Command-line Language (NCL). Several other commercial packages, including MATLAB and IDL, also support the analysis and visualization of CMAQ inputs and outputs.

Almost all of the CMAQ input and output files use the I/O API file format, which is a modified version of the netCDF format. If the user has already built the netCDF library ([<http://www.unidata.ucar.edu/software/netcdf/index.html>](http://www.unidata.ucar.edu/software/netcdf/index.html) ) for compiling CMAQ, the ncdump utility should also be available on the user’s machine. This utility generates an ASCII representation of the netCDF file using the CDF notation developed by NCAR.

The UNIX syntax for invoking ncdump is the following:

ncdump [-h] [-c] [-n name] [inputfile]

where:

-hproduces only the "header" information in the output file; i.e., the declarations of dimensions, variables, and attribute, but no data values for the variables.

-cproduces the "header" information in the output file and the data values for coordinate variables (variables that are also dimensions).

-nname is used to specify a different name for the network Common data form Description Language (CDL) description than the default.

This chapter presents a brief overview of each set of tools mentioned above. At the beginning of each subsection below is a table listing helpful information for users who wish to use the tools.

CMAQ Utility Tools
------------------

|Latest Version|Version 4.7, released on 12/09/2008|
|---|---|
|Main website|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|Support|[<http://bugz.unc.edu>](http://bugz.unc.edu/)|

Several utility tools (Fortran-based) are provided along with the CMAQ code/scripts distribution. These are included in the MODELS.tar.gz file, and located in the \$M3MODEL/TOOLS source code directory. These tools work directly with the CMAQ outputs and help in processing, formatting, and preparing datasets from various ambient monitoring networks for subsequent evaluation. These networks include the EPA Air Quality System (AQS)AIRS-AQS, Interagency Monitoring of Protected Visual Environments (IMPROVE), Clean Air Status Trends Network (CASTNET), Speciated Trends Network (STN), National Atmospheric Deposition Program (NADP), Mercury Deposition Network (MDN) and the Southeast Aerosol Research and Characterization Study (SEARCH). The various CMAQ utility tools are described below.

1.  **combine**

This utility combines fields from a set of I/O API input files to create an output file. The file assigned to the environment variable SPECIES\_DEF defines the new species variables and how they are constructed.

1.  **sitecmp**

This utility, “site compare,” generates a CSV (comma-separated values) file that compares CMAQ-generated concentrations with an observed dataset. The various environment variables required by sitecmp are:

1.  -   TABLE\_TYPE: dataset type (AQS, IMPROVE, CASTNET, STN, NADP, MDN, SEARCH)
    -   M3\_FILE\_n: I/O API input files that contain modeled species data (maximum of 12 files)
    -   SITE\_FILE: site file containing site ID, longitude, latitude (tab-delimited)
    -   IN\_TABLE: observed data (comma-delimited with header)
    -   OUT\_TABLE: output data table with columns of observed and modeled values

2.  **rd\_airs**

This utility reads the raw AQS data and writes hourly values in one-day-per-record format. The various environment variables required by rd\_airs are:

1.  -   INFILE: AIRS data file downloaded from web site <http://www.epa.gov/ttn/airs/airsaqs/detaildata/downloadaqsdata.htm>
    -   SITEFILE: list of AIRS site codes with latitude and longitude values
    -   OUTFILE: output data file (hourly values in 24/record format)
    -   STATES: list of states to process (default is all states)
    -   YEARS: list of years to process (default is all years)
    -   PARAMETER: code of species to process (default is 44201 OZONE)
    -   CHECKUNITS: switch to check for valid units

2.  **airs2ext**

This utility reads the output from the rd\_airs program and writes hourly or daily values in the sitecmp CASTNET input format (CSV). The various environment variables required by airs2ext are:

1.  -   INFILE: data file generated with the rd\_airs program
    -   OUTFILE: output data file used with sitecmp (hourly or daily values)
    -   SPECIES: name of species for header line
    -   STEP: time step of output (DAY or HOUR)
    -   START\_DATE: starting date to window data (YYYYDDD format)
    -   END\_DATE: end date of window data (YYYYDDD format)

2.  **cast2ext**

This utility reads the CASTNET hourly values downloaded from [www.epa.gov/castnet](http://www.epa.gov/castnet) and generates an input file for the sitecmp program. The various environment variables required by cast2ext are:

1.  -   INFILE: hourly CASTNET data file
    -   OUTFILE: output file in format to use with sitecmp

M3tools
-------
|Latest Version | |
|---|---|
|Mainwebsite|[<https://www.cmascenter.org/ioapi/>](https://www.cmascenter.org/ioapi/)|
|Download|[<https://www.cmascenter.org/download/forms/step_2.cfm?prod=5>](https://www.cmascenter.org/download/forms/step_2.cfm?prod=5)|
|Answers to FAQ|[<https://www.cmascenter.org/ioapi/documentation/3.1/html/ERRORS.html>](https://www.cmascenter.org/ioapi/documentation/3.1/html/ERRORS.html)|
|Latest User’s Manual|[<https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools>](https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools)|

An extensive set of utility programs called *m3tools* that use the I/O API library have been developed and made available for the modeling community. These utility routines assist in manipulating dates and times, performing coordinate conversions, storing and recalling grid definitions, sparse matrix arithmetic, etc., as well as in data manipulation and statistical analyses. All *m3tools* can be run at the command line, and the various options can be provided interactively, or all of them can be stored in a file and executed as scripts.

A list of these utility programs and brief descriptions is provided below.

-   **airs2m3**:''' '''Imports air quality monitor data from an AIRS AMP350-format ASCII file and puts them into an I/O API "observational data" file
-   **bcwndw**: Extracts data from a gridded file to the boundary of a subgrid window (see **m3wndw** later in this list for extracting to the window itself)
-   **datshift**: Takes calendar date (form YYYYMMDD) and a number of days D, and reports the date D days later.
-   **gregdate**: Computes calendar-style date "Month DD, YYYY", day-of-week (Sunday, Monday, ..., Saturday), and whether or not Daylight Saving Time is in effect from Julian date YYYYDDD, or from "yesterday", "today", or "tomorrow"
-   **juldate**: Computes Julian date YYYYDDD, day-of-week (Sunday, Monday, ..., Saturday), and whether or not Daylight Saving Time is in effect from calendar-style date "Month DD, YYYY", or from "yesterday", "today", or "tomorrow".
-   **m3combo**: Computes linear combinations of sets of variables from an I/O API input file, and writes the resulting variables to an I/O API output file
-   **m3cple**: Copies to the same grid, or interpolates to another grid, a time sequence of all variables from a source file to a target file, under the optional control of an I/O API coupling-mode "synch file"
-   **m3diff**: Computes statistics for pairs of variables and for the results of applying various comparison ("differencing") operations to those variables in a pair of files.
-   **m3edhdr**: Edits header attributes/file descriptive parameters
-   **m3fake**: Builds a file according to user specifications, filled either with dummy data or with data read in from a set of user-supplied files
-   **m3merge**: Merges selected variables from a set of input files for a specified time period, and writes them to a single output file, with optional variable renaming in the process
-   **m3pair**: Builds an ASCII file of paired values for two variables from two files, within a user-selected window into the grid, according to user specifications
-   **m3stat**: Computes statistics for variables in a file
-   **m3tproc**: Computes time period aggregates (e.g., 08:00-16:00 gridded daily maxima) and writes them to an output file. Can be used to create running averages, (e.g., 8-h O<sub>3</sub> data from 1-h O<sub>3</sub>), daily averages, daily maxima, etc.
-   **m3tshift**: Copies/time-shifts data from a file
-   **m3wndw**: Windows data from a gridded file to a subgrid (see **bcwndw** earlier in this list for extracting to the boundary of the subgrid window)
-   **m3xtract**: Extracts a subset of variables from a file for *\<time interval\>* .Can also be used to concatenate data from two or more files with different time periods into one file
-   **m4filter**: Converts first-edition Models-3 files to current version
-   **mtxblend**: Uses a sparse-matrix file to interpolate/transform data from an input file to the grid of a "base" file and to merge it with data from the "base" file
-   **mtxbuild**: Builds a sparse-matrix transform file from user-supplied ASCII coefficient inputs
-   **mtxcalc**: Builds a grid-to-grid sparse-matrix transform file using a subsampling algorithm
-   **mtxcple**: Uses a sparse-matrix file to interpolate a time sequence of all variables from a source file to a target file, under the optional control of an I/O API coupling-mode "synch file"
-   **presterp**: Interpolates from a 3-D sigma-coordinate file to a new 3-D pressure-coordinate file, using coefficients from PRES\_CRO\_3D
-   **selmrg2d**: Selects multiple 2-D layer/variable combinations from multiple gridded input files, and writes result to merged 2-D gridded output file
-   **utmtool**: Performs coordinate conversions and grid-related computations for lat-lon, Lambert, and UTM coordinate systems.
-   **vertot**: Computes vertical-column totals of variables in a file

Package for Analyses and Visualization of Environmental Data (PAVE)
-------------------------------------------------------------------

|Latest Version|Version 2.3 released on October 18, 2004|
|---|---|
|Main website|[<http://paved.sourceforge.net>](http://paved.sourceforge.net/)|
|Download|[<http://paved.sourceforge.net/#Downloads>](http://paved.sourceforge.net/#Downloads)|
|Latest User’s Manual|[<http://paved.sourceforge.net/pave_doc/Pave.html>](http://paved.sourceforge.net/pave_doc/Pave.html)|
|Answers to FAQ|[<http://paved.sourceforge.net/pave_doc/Pave.FAQ.html>](http://paved.sourceforge.net/pave_doc/Pave.FAQ.html)|
|Support website|[<http://bugz.unc.edu>](http://bugz.unc.edu/)|

PAVE is a flexible and distributed application to visualize multivariate gridded environmental datasets. Features include

-   baseline graphics with the option to export data to high-end commercial packages
-   the ability to access and manipulate datasets located on remote machines
-   support for multiple simultaneous visualizations
-   an architecture that allows PAVE to be controlled by external processes
-   low computational overhead
-   no software distribution cost

PAVE is very widely used by the air quality modeling community, and it can produce various types of plots, including scatter plots, time-series plots, 2-D tile plots, 3-D surface plots, bar plots, wind-vector plots, etc. The source code for PAVE is also distributed under the terms of the GNU General Public License Version 2. PAVE can be run at the Linux command prompt, and the various commands/options can be invoked using the graphical user interface (GUI), or all of them can be stored in a script file and executed by running the script. However, note that PAVE is not being updated any more, and CMAS has ceased support for PAVE, and encourages the user community to move towards VERDI (discussed next).

Visualization Environment for Rich Data Interpretation (VERDI)
--------------------------------------------------------------

|Latest Version|Version 1.1 released on May 22, 2009|
|---|---|
|Main website|[<http://www.verdi-tool.org>](http://www.verdi-tool.org/)|
|Download|[<http://www.verdi-tool.org>](http://www.verdi-tool.org/)|
|Latest User’s Manual|[<http://www.verdi-tool.org>](http://www.verdi-tool.org/)|
|Answers to FAQ|[<http://www.verdi-tool.org>](http://www.verdi-tool.org/)|
|Support website|[<http://bugz.unc.edu>](http://bugz.unc.edu/)|

The Visualization Environment for Rich Data Interpretation (VERDI) is a flexible and modular Java-based visualization software tool that allows users to visualize multivariate gridded environmental datasets created by environmental modeling systems such as SMOKE, CMAQ and WRF, namely gridded concentration and deposition fields that users need to visualize and compare with observational data both spatially and temporally. VERDI has been designed keeping most of the functionality of PAVE in mind, and hence can help users analyze and visualize model outputs in a very similar vein, using both command-line driven scripts as well as using a Graphical User Interface (GUI). Further, VERDI is under active development to enhance its features beyond PAVE.

Atmospheric Model Evaluation Tool (AMET)
----------------------------------------

|Latest Version|Version 1.1 released on May 31, 2008|
|---|---|
|Mainwebsite|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|Download|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|Latest User’s Manual|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|Training/Workshop|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|Answers to FAQ|N/A|
|Support website|[<http://bugz.unc.edu>](http://bugz.unc.edu/)|

The Atmospheric Model Evaluation Tool (AMET) is a suite of software designed to facilitate the analysis and evaluation of meteorological and air quality models. AMET matches the model output for particular locations to the corresponding observed values from one or more networks of monitors. These pairings of values (model and observation) are then used to statistically and graphically analyze the model’s performance. More specifically, AMET is currently designed to analyze outputs from MM5, WRF, and CMAQ, as well as MCIP-postprocessed meteorological data (surface only). The basic structure of AMET consists of two ''fields ''and two *processes*. The two fields (scientific topics) are MET and AQ, corresponding to meteorology and air quality data. The two processes (actions) are database population and analysis. Database population refers to the underlying structure of AMET; after the observations and model data are paired in space and time, the pairs are inserted into a MySQL database. Analysis refers to the statistical evaluation of these pairings and their subsequent plotting. Practically, a user may be interested in using only one of the fields (either MET or AQ), or may be interested in using both fields. That decision is based on the scope of the study. The three main software components of AMET are MySQL (an open-source database software system), R (a free software environment for statistical computing and graphics), and perl (an open-source, cross-platform programming language).

netCDF Operators (NCO)
----------------------


|Latest version|Version 3.9.9 released on July 24, 2009|
|---|---|
|Main website|[<http://nco.sourceforge.net/>](http://nco.sourceforge.net/)|
|Download|[<http://nco.sourceforge.net/#Download>](http://nco.sourceforge.net/#Download)|
|Latest User’s Manual|[<http://nco.sourceforge.net/#RTFM>](http://nco.sourceforge.net/#RTFM)|
|Answers to FAQ|[<http://nco.sourceforge.net/#FAQ>](http://nco.sourceforge.net/#FAQ)|
|Support website|[<http://nco.sourceforge.net/nco.html#help>](http://nco.sourceforge.net/nco.html#help)|

The netCDF Operators (NCO) are a suite of programs known as operators. Each operator is a stand-alone, command-line program that is executed at the UNIX shell level, similar to the commands ls or mkdir. The operators take netCDF files as input, then perform a set of operations (e.g., deriving new data, averaging, hyperslabbing, or metadata manipulation) and produce a netCDF file as output. The operators are primarily designed to aid manipulation and analysis of gridded scientific data. The single command style of NCO allows users to manipulate and analyze files interactively and with simple scripts, avoiding the overhead (and some of the power) of a high-level programming environment.

NCO achieves flexibility by using *command-line options*. These options are implemented in all traditional UNIX commands as single-letter *switches*, e.g., \`ls -l'. NCO supports both short-format (single letter) and long-format (multiletter) options.

An overview of the various netCDF operators is given below.

-   **ncap (netCDF Arithmetic Processor)**: ncap and ncap2 arithmetically process netCDF files. The processing instructions are contained either in the NCO script file fl.nco or in a sequence of command-line arguments.
-   **ncatted (netCDF Attribute Editor)**: ncatted edits attributes in a netCDF file. ncatted can *append*, *create*, *delete*, *modify*, and *overwrite* attributes (all explained below). Furthermore, ncatted allows each editing operation to be applied to every variable in a file. This saves time when changing attribute conventions throughout a file.
-   **ncbo (netCDF Binary Operator)**: ncbo performs binary operations on variables in *file\_1* and the corresponding variables (those with the same name) in *file\_2* and stores the results in *file\_3*. The binary operation operates on the entire files.
-   **ncea (netCDF Ensemble Averager)**: ncea performs grid-point averages of variables across an arbitrary number (an *ensemble*) of *input-files*, with each file receiving an equal weight in the average. Each variable in the *output-file* will be the same size as the same variable in any one of the *input-files*, and all *input-files* must be the same size. ncea averages entire files, and weights each file evenly. This is distinct from ncra (discussed later in this list), which averages only over the record dimension (e.g., time), and weights each record in the record dimension evenly; ncea *always averages* coordinate variables, regardless of the arithmetic operation type performed on the noncoordinate variables. All dimensions, including the record dimension, are treated identically and preserved in the *output-file*.
-   **ncecat (netCDF Ensemble Concatenator**): ncecat concatenates an arbitrary number of input files into a single output file. A new record dimension acts as the glue to bind the input files data together. Each variable in each input file becomes one record in the same variable in the output file. All *input-files* must contain all extracted variables (or else there would be "gaps" in the output file). Each extracted variable must be constant in size and rank across all *input-files*. The *input-files* are stored consecutively as a single record in *output-file*. Thus, the *output-file* size is the sum of the sizes of the extracted variable in the input files.
-   **ncflint (netCDF File Interpolator)**: ncflint creates an output file that is a linear combi­nation of the input files. This linear combination is a weighted average, a normalized weighted average, or an interpolation of the input files. Coordinate variables are not acted upon in any case; they are simply copied from *file\_1*.
-   **ncks (netCDF Kitchen Sink)**: ncks combines selected features of ncdump, ncextr, and the nccut and ncpaste specifications into one versatile utility. ncks extracts a subset of the data from *input-file* and prints it as ASCII text to stdout, writes it in flat binary format to binary-file, and writes (or pastes) it in netCDF format to *output-file*.
-   **ncpdq (netCDF Permute Dimensions Quickly)**: ncpdq performs one of two distinct functions—packing or dimension permutation—but not both. ncpdq is optimized to perform these actions in a parallel fashion with a minimum of time and memory.
-   **ncra (netCDF Record Averager)**: ncra averages record variables across an arbitrary number of *input-files*. The record dimension is, by default, retained as a degenerate (size 1) dimension in the output variables.
-   **ncrcat (netCDF Record Concatenator)**: ncrcat concatenates record variables across an arbitrary number of *input-files*. The final record dimension is by default the sum of the lengths of the record dimensions in the input files.
-   **ncrename (netCDF Renamer)**: ncrename renames dimensions, variables, and attributes in a netCDF file. Each object that has a name in the list of old names is renamed using the corresponding name in the list of new names. All the new names must be unique.
-   **ncwa (netCDF Weighted Averager)**: ncwa averages variables in a single file over arbitrary dimensions, with options to specify weights, masks, and normalization.

Python ioapiTools
-----------------

|Latest version| |
|---|---|
|Main website|[<http://www-pcmdi.llnl.gov/software-portal/Members/azubrow/ioapiTools/index_html>](http://www-pcmdi.llnl.gov/software-portal/Members/azubrow/ioapiTools/index_html)|
|Download|[<http://www-pcmdi.llnl.gov/softwareportal/Members/azubrow/ioapiTools/download-source-file>](http://www-pcmdi.llnl.gov/softwareportal/Members/azubrow/ioapiTools/download-source-file)|
|Latest User’s Manual|[<http://www-pcmdi.llnl.gov/software-portal/Members/azubrow/ioapiTools/index_html>](http://www-pcmdi.llnl.gov/software-portal/Members/azubrow/ioapiTools/index_html)|
|Support info|[<http://www-pcmdi.llnl.gov/software-portal/Members/azubrow/ioapiTools/problems-file>](http://www-pcmdi.llnl.gov/software-portal/Members/azubrow/ioapiTools/problems-file)|

The *ioapiTools* package was developed to include I/O API data within the Climate Data Analyses Tools (CDAT) framework. The *ioapiTools* module provides functions for extracting and manipulating data (individual variables) and writing to either an I/O API file or a climate and forecast compliant (CF) netCDF file with I/O API metadata.

The *ioapiTools* module is a contributed package to CDAT and is built on top of the Climate Data Management System (cdms) module. The key object in *ioapiTools*, *iovar,* is a daughter of the *cdms* transient variable. In other words, an *iovar* object has all the capabilities of a *cdms* variable plus some extra methods and attributes. So, one can use the methods and attributes that the user may already be familiar with from *cdms* variables, as well as the new methods and attributes. The user needs to install *pyIoapi*, a low-level python interface to the I/O API library, as well as *ioapiTools*, the high-level python module that integrates I/O API data into *cdms*.

After installing CDAT and the ioapiTools, the user should use a python interpreter (*python*, *idle*, *ipython*, etc.) when using the various packages.

Integrated Data Viewer (IDV)
----------------------------

|Latest version||
|---|---|
|Main website|[<http://www.unidata.ucar.edu/software/idv/>](http://www.unidata.ucar.edu/software/idv/)|
|Download|[<http://www.unidata.ucar.edu/software/idv/docs/userguide/Starting.html>](http://www.unidata.ucar.edu/software/idv/docs/userguide/Starting.html)|
|Latest User’s Manual|[<http://www.unidata.ucar.edu/software/idv/docs/userguide/>](http://www.unidata.ucar.edu/software/idv/docs/userguide/)|
|Training/Workshop|[<http://www.unidata.ucar.edu/software/idv/docs/workshop/>](http://www.unidata.ucar.edu/software/idv/docs/workshop/)|
|Answers to FAQ|[<http://www.unidata.ucar.edu/software/idv/docs/userguide/Faq.html>](http://www.unidata.ucar.edu/software/idv/docs/userguide/Faq.html)|
|Support info|[<http://www.unidata.ucar.edu/software/idv/docs/userguide/Support.html>](http://www.unidata.ucar.edu/software/idv/docs/userguide/Support.html)|

The Integrated Data Viewer (IDV) from Unidata is a Java™-based software framework for analyzing and visualizing geoscience data. The IDV release includes a software library and a reference application made from that software. It uses the VisAD library ([<http://www.ssec.wisc.edu/~billh/visad.html>](http://www.ssec.wisc.edu/~billh/visad.html)) and other Java-based utility packages.

The IDV is developed at the Unidata Program Center (UPC), part of the University Corporation for Atmospheric Research in Boulder, CO, which is funded by the National Science Foundation. The software is freely available under the terms of the GNU Lesser General Public License.

The IDV "reference application" is a geoscience display and analysis software system with many of the standard data displays that other Unidata software (e.g., GEMPAK and McIDAS) provides. It brings together the ability to display and work with satellite imagery, gridded data (for example, numerical weather prediction model output), surface observations, balloon soundings, NWS WSR-88D Level II and Level III RADAR data, and NOAA National Profiler Network data, all within a unified interface. It also provides 3-D views of the earth system and allows users to interactively slice, dice, and probe the data, creating cross-sections, profiles, animations and value read-outs of multidimensional data sets. The IDV can display any Earth-located data if they are provided in a supported format.

More recently, two features have been added to IDV that make it attractive for use by the air quality modeling community: (1) the capability to read I/O API netCDF formatted files, and (2) a scripting interface to create and manipulate images and movies. The scripting is accomplished through an XML file: *IDV Scripting Language* (ISL). The ISL file can be opened from a running IDV, or one can be passed to the IDV as a command-line argument:

runIDV capture.isl

NCAR Command Language (NCL)
---------------------------

|Latest Version|Version 5.1.1 released on June 16, 2009|
|---|---|
|Mainwebsite|[<http://www.ncl.ucar.edu>](http://www.ncl.ucar.edu/)|
|Download|[<http://www.ncl.ucar.edu/Download/index.shtml>](http://www.ncl.ucar.edu/Download/index.shtml)|
|Latest User’s Manual|[<http://www.ncl.ucar.edu/Document/index.shtml>](http://www.ncl.ucar.edu/Document/index.shtml)|
|Training/Workshop|[<http://www.ncl.ucar.edu/Training/index.shtml>](http://www.ncl.ucar.edu/Training/index.shtml)|
|Support info|[<http://www.ncl.ucar.edu/Support/ncl_talk.shtml>](http://www.ncl.ucar.edu/Support/ncl_talk.shtml)|

The NCAR Command Language (NCL) is a free, interpreted language designed specifically for scientific data processing and visualization. NCL has robust file input and output. It can read in netCDF, HDF4, HDF4-EOS, GRIB, binary, and ASCII data. The output graphics from NCL are of high quality, and highly customizable.

It runs on many different operating systems, including Solaris, AIX, IRIX, Linux, MacOSX, Dec Alpha, and Cygwin/X running on Windows. It is available for free in binary format.

NCL can be run in interactive mode, where each line is interpreted as it is entered at the user’s workstation, or it can be run in batch mode as an interpreter of complete scripts. The user can also use command-line options to set options or variables on the NCL command line.

The power and utility of the language are evident in three areas:

-   file input and output
-   data analysis
-   visualization

NCL has many features common to modern programming languages, including types, variables, operators, expressions, conditional statements, loops, and functions and procedures. The various NCL commands can be executed at the NCL command prompt, or all the commands can be stored in a file and invoked as follows:

ncl commands.ncl

NCL comes with numerous predefined functions and resources that the user can easily invoke. These include functions for data processing, visualization, and various mathematical and statistical analyses, such as empirical orthogonal functions (EOFs) and singular value decomposition (SVD). As an example, contributed.ncl is a library of user-contributed functions within NCL. This library is distributed with NCL, and loading the script at the beginning of the user’s NCL script therein can access the functions.

load "\$NCARG\_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

NCL also has a capability to call external Fortran or C routines. This is enabled through an NCL wrapper script called WRAPIT. The wrapper file is a C program that describes all of the arguments and passes them back and forth between the function/procedure the user wants to call, and the NCL script that is calling it. Thus, when the user invokes WRAPIT on the Fortran code that needs to be called from NCL, it creates a special C wrapper file, compiles it and the Fortran file, and generates a \*.so file that the user can then load into NCL using the "external" statement.

<span id=Section13></span>

CMAQ SUPPORT
============

Technical and operational user support for CMAQ are available free of charge from the Community Modeling and Analysis System Center ([<http://www.cmascenter.org>](http://www.cmascenter.org/)). The CMAS Center offers an e-mail help desk, and community listservs for posting questions about CMAQ. In addition to these community-based resources, the CMAS Center offers fee-based trainings, and provides a documentation library for CMAQ that includes operational and technical guidance manuals as well as references to primary literature involving CMAQ. The CMAS Center does not offer telephone or on-site support.

Additional CMAQ support can be obtained for a fee through support contracts with modeling contractors experienced in applying and developing CMAQ source code. Contact information for these contractors is available through the links page on the CMAS Center website.

The CMAS Center
---------------

Under contract to EPA, the Center for Environmental Modeling for Policy Development ([CEMPD](http://cf.unc.edu/cep/empd/)) at the University of North Carolina at Chapel Hill ([UNC](http://www.unc.edu/)) Institute for the Environment maintains the CMAS Center for supporting community-based air quality modeling. CMAS is an approach to the development, application, and analysis of environmental models that leverages the complementary talents and resources of the modeling community in order to set new standards for quality in science and in the reliability of the application of the technology.

From research to application to outreach, the CMAS Center advances the community modeling paradigm through the establishment of a centralized resource to serve the members of the national and international environmental modeling community.

### CMAS functions

Currently, the following activities are available through the CMAS Center:

-   [On-line help desk](http://www.cmascenter.org/help_desk.cfm?temp_id=99999) - Get help with the supported CMAS products
-   [Model clearinghouse](http://www.cmascenter.org/download/models.cfm?temp_id=99999) - Download the supported CMAS products
-   [Training courses](http://www.cmascenter.org/training.cfm?temp_id=99999) - Attend a training course on emissions modeling, air quality modeling, or other related topics
-   [Conferences](http://www.cmascenter.org/conference.cfm?temp_id=99999) - Attend the annual CMAS conference to interact with the community
-   [Development assistance](http://www.cmascenter.org/rnd.cfm?temp_id=99999) - Add new science to the supported CMAS products
-   [Model documentation](http://www.cmascenter.org/help/documentation.cfm?temp_id=99999) - Access on-line documentation for the CMAS products
-   [Model-related research](http://www.cmascenter.org/rnd.cfm?temp_id=99999) - Learn about the latest developments in modeling research
-   [Data clearinghouse](http://www.cmascenter.org/download/data.cfm?temp_id=99999) - Access air quality modeling data from around the community

### CMAS community

A primary focus of CMAS is to instill a sense of community among the users of environmental models. From the individual to the organizational level, the beneficiaries of CMAS include (but are not limited to):

-   Government
-   Industry
-   Academia
-   Research
-   Consultants
-   Stakeholder groups

By promoting cooperation within and between the various groups in the environmental modeling community, CMAS forms the foundation to enable the community to participate in the examina­tion of issues and the subsequent development of strategies that meet societal challenges of environmental protection.

### Why is CMAS needed?

Historically, advancements in air quality model technology could not easily be shared among modelers because of technical incompatibilities. By standardizing with open-source, advanced modeling systems, CMAS enables collaborative development and linking of models for meteorology, emissions, air quality, and environmental and health effects. CMAS builds on the pioneering work by EPA and other research organizations by developing advanced tools to assist users in building models, developing datasets, analyzing results, and understanding model behavior. Because the CMAS-supported models use a “modular” approach with well-defined communications between modules, developers can upgrade existing processes or add new ones, thus ensuring the rapid evolution of the technology to meet the changing needs of the environmental modeling community.

CMAQ was EPA’s first tangible product to evolve out of the community modeling paradigm. With a framework for a community model in CMAQ, there is a need for centralized coordination of development and application efforts for mutual benefit to scientists, model developers, practitioners of modeling, and regulatory users of modeling results. CMAS and its accompany­ing center at the CEMPD facilitate the effort to draw the interest of the modeling community toward advancement through cooperation.

Getting Help with CMAQ
----------------------

The CMAS Center website ([<http://www.cmascenter.org>](http://www.cmascenter.org/)) includes a help desk with resources that are available to assist with CMAQ-related issues. The CMAS help desk services are free to the community. Many of the services in the help desk benefit from increased usage, such as the listserv discussion groups. E‑mail-based CMAQ technical consultation is available to registered CMAS participants only.

The following resources are available through the CMAS Center to address CMAQ-related questions. Community members should use these resources in the order that follows. There is currently a large, searchable database of resolved CMAQ support tickets; before submitting a new ticket to the help desk, be sure to search the database for keywords to see if the issue has been addressed previously. Section 11.3 provides a list of the web pages referenced in Section 11.2.

### Documentation

The first place to look for an answer to CMAQ-related questions is the on-line documentation for the software. The CMAS documentation page contains links to available documentation for current and previous releases of the various kinds of software that CMAS supports. Peruse these on-line manuals, as many of them contain FAQs and discussion specific to the various programs.

### Interactive resources

Search the CMAS FAQs and listservs for information about the question that you have. These services are organized by topic to facilitate searching. Look under the CMAS Model Clearinghouse area to find out about new releases, and read through the release notes of past releases for detailed information about the features of the models.

### Tutorials/training

General questions regarding model installation or application may be addressed in the online tutorials for the CMAS-supported software. More-specific tutorials will be added over time; users can suggest tutorial topics by contacting the CMAS Center.

The CMAS Center offers quarterly trainings on CMAQ at the Institute for the Environment offices in Chapel Hill, North Carolina, USA. CMAS training staff are also available to travel for on-site training anywhere in the world. The currently available training is an introductory course to CMAQ that covers configuration, compilation, and basic operation of the model. Visit the CMAS training web page to see an agenda, fees, and the schedules for upcoming training courses

### E-mail support

E-mail support is available to CMAS users who have a support account, and provides case-specific support for all CMAS-supported software, which includes CMAQ. CMAS e-mail support provides direct access to expert CMAQ users for questions about installation or operational issues. E-mail support also provides direct access to the CMAQ developers for technical questions about model formulation, model science, and code integration. Visit the e-mail support page for an explanation of how to use the system and to register.

Contacting CMAS
---------------

The CMAS Center is available on the web at [<http://www.cmascenter.org>](http://www.cmascenter.org/). Table 11-1 lists important contacts for the CMAS Center.

<span id=Table11-1></span>

<center>**Table 11‑1. CMAS contact information and important links**</center>

|<center>**Resource**</center>|<center>**Link**</center>|
|---|---|
|Main website|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|General Questions|<cmas@unc.edu>|
|Help Desk|[<http://www.cmascenter.org/help_desk.cfm>](http://www.cmascenter.org/help_desk.cfm)|
|Training Information|[<http://www.cmascenter.org/training.cfm>](http://www.cmascenter.org/training.cfm)|
|Conferences and Workshops|[<http://www.cmascenter.org/conference.cfm>](http://www.cmascenter.org/conference.cfm)|
|Downloads|[<http://www.cmascenter.org/download.cfm>](http://www.cmascenter.org/download.cfm)|
|Release Calendar|[<http://www.cmascenter.org/release_calendar.cfm>](http://www.cmascenter.org/release_calendar.cfm)|
|FAQs|[<http://www.cmascenter.org/help/faq.cfm>](http://www.cmascenter.org/help/faq.cfm)|
|CMAQ Home Page|[<http://www.cmaq-model.org>](http://www.cmaq-model.org/)|

<span id=AppendixA></span>

Appendix A – CMAQ Chemical Mechanisms and Species
=================================================

<span id=TableA-2></span>

<center>**Table A‑2. CB05tucl species**</center>

|Species|Description|MW|
|---|---|---|
|AACD|Acetic and higher acids|<div align="right">60</div>|
|ALD2|Acetaldehyde|<div align="right">44</div>|
|ALDX|Propionaldehyde and higher aldehydes|<div align="right">44</div>|
|BENZENE|Benzene|<div align="right">78</div>|
|BENZRO2|First generation SOA intermediate from benzene oxidation|<div align="right">127</div>|
|BNZHRXN|Counter species for computing SOA from BENZENE under low NOx conditions|<div align="right">127</div>|
|BNZNRXN|Counter species for computing SOA from BENZENE under high NOx conditions|<div align="right">127</div>|
|C2O3|Acetylperoxy radical|<div align="right">75</div>|
|CAO2||<div align="right">133</div>|
|CAT1||<div align="right">124</div>|
|CL||<div align="right">35.5</div>|
|CL2||<div align="right">71</div>|
|CLO||<div align="right">51.5</div>|
|CO|Carbon monoxide|<div align="right">28</div>|
|CRES|Cresol and higher molecular weight phenols|<div align="right">108</div>|
|CRN2||<div align="right">168</div>|
|CRNO|Alkoxy radical from oxidation of CRON|<div align="right">152</div>|
|CRO|Methylphenoxy radical|<div align="right">107</div>|
|CRON|Nitro cresol|<div align="right">153</div>|
|CRPX||<div align="right">169</div>|
|CXO3|C3 and higher acylperoxy radicals|<div align="right">75</div>|
|ETH|Ethene|<div align="right">28</div>|
|ETHA|Ethane|<div align="right">30</div>|
|ETOH|Ethanol|<div align="right">46</div>|
|FACD|Formic acid|<div align="right">46</div>|
|FMCL||<div align="right">64.5</div>|
|FORM|Formaldehyde|<div align="right">30</div>|
|H2O2|Hydrogen peroxide|<div align="right">34</div>|
|HCL||<div align="right">36.5</div>|
|HCO3||<div align="right">63</div>|
|HNO3|Nitric acid|<div align="right">63</div>|
|HO2|Hydroperoxy radical|<div align="right">33</div>|
|HOCL||<div align="right">52.5</div>|
|HONO|Nitrous acid|<div align="right">47</div>|
|IOLE|Internal olefin carbon bond (R-C=C-R)|<div align="right">48</div>|
|ISOP|Isoprene|<div align="right">68</div>|
|ISOPRXN|Counter species for computing SOA from ISOP|<div align="right">68</div>|
|ISPD|Isoprene oxidation product|<div align="right">70</div>|
|MEO2|Methylperoxy radical|<div align="right">47</div>|
|MEOH|Methanol|<div align="right">32</div>|
|MEPX|Methylhydroperoxide|<div align="right">48</div>|
|MGLY|Methylglyoxal and other aromatic products|<div align="right">72</div>|
|N2O5|Nitrogen pentoxide|<div align="right">108</div>|
|NO|Nitric oxide|<div align="right">30</div>|
|NO2|Nitrogen dioxide|<div align="right">46 </div>|
|NO3|Nitrogen trioxide|<div align="right"> 62 </div>|
|NTR|Organic nitrate|<div align="right"> 130 </div>|
|O|Oxygen atom (triplet)|<div align="right"> 16 </div>|
|O1D|Oxygen atom (singlet)|<div align="right"> 16 </div>|
|O3|Ozone|<div align="right"> 48 </div>|
|OH|Hydroxyl radical|<div align="right"> 17 </div>|
|OLE|Terminal olefin carbon bond (R-C=C)|<div align="right"> 27 </div>|
|OPAN||<div align="right"> 161 </div>| 
|OPEN|Aromatic ring open product|<div align="right"> 84 </div>|
|OPO3|Peroxyacyl radical from oxidation of OPEN|<div align="right"> 115 </div>|
|PACD|Peroxy acetic acid|<div align="right"> 76 </div>|
|PAN|Peroxyacyl nitrate|<div align="right"> 121 </div>|
|PANX|C3 and higher peroxyacyl nitrates|<div align="right"> 121 </div>|
|PAR|Paraffin carbon bond (C-C)|<div align="right"> 14 </div>|
|PNA|Peroxynitric acid|<div align="right"> 79 </div>|
|ROOH|Higher organic peroxide|<div align="right"> 62 </div>|
|ROR|Secondary alkoxy radical|<div align="right"> 31 </div>|
|SESQ|Sesquiterpene|<div align="right"> 204 </div>|
|SESQRXN|Counter species for computing SOA from SESQ|<div align="right"> 204 </div>|
|SO2|Sulfur dioxide|<div align="right"> 64 </div>|
|SULF|Sulfuric acid gas|<div align="right"> 98 </div>|
|SULRXN|Counter species for computing aerosols from SULF|<div align="right"> 98 </div>|
|TERP|Terpene|<div align="right"> 136 </div>|
|TO2|Tolene-hydroxyl radical adduct|<div align="right"> 173 </div>|
|TOL|Toluene and other monoalkyl aromatics|<div align="right"> 92 </div>|
|TOLHRXN|Counter species for computing SOA from TOL under low NOx conditions|<div align="right"> 141 </div>|
|TOLNRXN|Counter species for computing SOA from TOL under high NOx conditions|<div align="right"> 141 </div>|
|TOLRO2|First generation SOA intermediate from TOL oxidation|<div align="right"> 141 </div>|
|TRPRXN|Counter species for computing SOA from TERP|<div align="right"> 136 </div>|
|XO2|NO-to-NO2 conversion from alkylperoxy radical|<div align="right"> 1 </div>|
|XO2N|NO-to-nitrate conversion from alkylperoxy radical|<div align="right"> 1 </div>|
|XYL|Xylene and other polyalkyl aromatics|<div align="right"> 106 </div>|
|XYLHRXN|Counter species for computing SOA from XYL under low NOx conditions|<div align="right"> 155 </div>|
|XYLNRXN|Counter species for computing SOA from XYL under high NOx conditions|<div align="right"> 155 </div>|
|XYLRO2|First generation SOA intermediate from XYL oxidation|<div align="right"> 155 </div>|

<a id=Glossary></a>

GLOSSARY
========

**Accumulation mode.** Aerosol particles with diameters nominally between 0.1 and 1.0 micrometers. The term is based upon the concept that particles in this size range accumulate in the atmosphere from the growth of much smaller particles. (See also “Aitken mode.”) The growth may be from new mass condensing on the smaller particles, or from the coagulation of the smaller particles.

**Adaptive grid.** A grid structure that varies during model execution according to the value(s) of some model parameter(s). For example, in a photochemistry model, grid resolution may auto­matically increase in areas of high spatial gradients of NO<sub>x</sub>. This allows more accurate determ­ination of plume-to-background concentration ratios, which greatly influence photochemical ozone production. In a meteorological model, an adaptive grid may automatically increase the grid resolution in an area of modeling where there is a large atmospheric pressure change across a grid cell.

**Air quality modeling system.** A computational system environment that combines a set of physical and chemical models that describe atmospheric processes, which are important to trace-gas and particulate matter distributions in the atmosphere. These systems typically include meteorological models, emissions models, chemistry-transport models, and the analysis and visualization tools necessary for supporting decisions related to air quality.

**Aitken mode.** Aerosol particles with diameters nominally between 0.01 and 0.1 micrometers (µm). Such particles are formed in the atmosphere by nucleation of gas‑phase precursors, or by direct emissions from sources. The most common source is combustion (e.g., diesel soot).

**Arakawa-B.** Horizontal grid staggering system described by Arakawa and Lamb (1977) and used by MM5. Mass variables and momentum variables are defined on separate horizontal grids of spacing equal to Delta-x. The two grids are offset by 0.5  Delta-x in the north-south and east-west directions.

**Arakawa-C.** Horizontal grid staggering system described by Arakawa and Lamb (1977) and used by WRF‑ARW and CCTM. Mass variables and each horizontal component of the momentum are defined using separate horizontal grids of spacing equal to Delta-x.

**ArcInfo.** A high-end geographical information system (GIS) with capabilities for the automation, modification, management, analysis, and display of geographical information.

**Automatic quality control (QC).** QC correction that is accomplished automatically without user intervention.

**Bookmark.** In on-line help, a bookmark marks an entry of a help document so it can be quickly accessed later. A list of bookmarks appears in the Bookmarks Menu on the help browser window and also in the bookmark’s window. Each item on the list is a hypertext link to a marked entry.

**Chemistry-transport model (CTM).** A model that simulates various physical and chemical processes that are important for understanding atmospheric trace-gas and particles distributions. The processes include atmospheric transport; vertical mixing; atmospheric chemistry in the gas phase, in the aqueous phase, and in aerosols; cloud mixing and precipitation scavenging; and surface removal processes. Generally, a chemistry-transport model relies on a meteorological model for the description of atmospheric states and motions, and depends on an emissions model for the description of anthropogenic and biogenic emissions that are injected into the atmosphere.

**Class.** A collection of software modules associated with a science process.

**Conforming datasets.** Conforming datasets are in I/O API format. Most programs (models, visualization and analysis routines) in the CMAQ system are able to read and write conforming datasets. This capability eliminates the need for data conversion, even within a distributed computing environment.

**Conforming programs.** Conforming programs generally use the I/O API library routines for reading and writing data. Key data structures are defined by globally shared information. You define this critical data structure information once, and it is automatically made available for use in conforming code throughout the system. This globally shared information is permanently stored as objects in an object data base. In Models-3, these objects are a stored collection of related information, such as grid dimensions and resolution, coordinate system definitions, chemical mechanism species, and reactions.

'''Cross point. '''Grid point where all scalars are defined.

**Daemon.** A process that runs in the background independently and performs a function. An example is a printer daemon that controls the job queue for the printer.

**Decision support system.** An automated system that provides information for decision making.

**Domain.** The domain for a CMAQ calculation is the horizontal geographic area (e.g., the eastern United States) that is of interest. CMAQ calculations are performed for physical and chemical processes occurring in the atmosphere above this horizontal geographic area.

'''Dot point. '''Grid point where wind components are defined

**Emission model.** This type of model calculates the emission rates of trace gas and particulate matter into the atmosphere under ambient meteorological conditions and socioeconomic activities. Emissions from both natural and manmade sources are calculated. One example of an emissions model is SMOKE.

**Environmental modeling system.** A set of computational models that mathematically represents a simplified version of real-world phenomena, along with the necessary mechanisms for managing the processing, the data produced by it, and the analysis and visualization tools necessary for making related decisions. For example, this could be the creation and behavior of environmental pollutants as a function of weather and chemical interactions. Researchers use these “scaled-down” versions of the world to perform experiments that would be too dangerous, too costly, or simply impossible in the real world.

**Eulerian.** Fluid motion specification where one concentrates on what happens at a spatial point, ''x, ''so that independent variables are taken as a function of time at that point.

**Evasion.** Loss of material from the land surface (e.g. the upward flux of mercury from the land surface).

**Exploratory models.** Test-bed models for developing scientific algorithms that are not thoroughly reviewed and evaluated. (See also “operational models” and “screening models.”)

**Fortran.** Formula translator (computer programming language).

**Framework.** A system of mechanisms to manage the scheduling and execution of computational models, the data produced by them, the analysis and visualization tools necessary for understanding their results for decision making, and the interfaces to all these capabilities.

**Generalized coordinate system.** A scheme for constructing coordinate systems that allows for choices of horizontal map projection type (e.g., latitude/longitude, Lambert, Mercator, polar stereographic, Universal Transverse Mercator, or matrix) and projection parameters, as well as for choices of various vertical coordinate types (e.g., pressure coordinate, height above ground, height above sea level, sigma-p hydrostatic, sigma-p nonhydrostatic, and sigma-z). The advantage of a generalized coordinate system is that a CMAQ model can adapt to a variety of different possibilities in horizontal and vertical parameters.

**Generic grid system.** A scheme for constructing grid systems that allows for choices of origin, orientation, and spatial resolution. It allows a model to be expressed in a grid system that is optimal for representing the governing equations. For a regular, rectangular grid system, mapping gridded data to the earth’s surface can be achieved by defining the number of rows and columns, cell size, and location and extent. For an irregular grid system, grid intersections (nodes) are described by coordinates from a reference position.

**Geocoded.** An entity with an identifier associated with geographic boundaries (e.g., state or county code).

**Geographic information system (GIS).** A computer-based system for managing, analyzing, manipulating, and displaying geographic information in both numeric and graphical ways.

**Geospatial.** Refers to the spatial extent of a geographic boundary.

**Grid cell.** The smallest subdivision of a grid.

**Grid size.** Length of shortest side of a rectangular grid cell.

**Grid.** A network of conceptual locations at which numerical calculations are performed. This network extends horizontally to cover the domain of interest, and vertically through the troposphere.

**Growth factor**. An adjustment factor used to estimate the growth in a source’s activity level between the inventory base year and a projected year. Valid values are 0.00 ‑ 99.99.

**Heterogeneous, distributed computing environment.** A heterogeneous computing environment consists of multiple machines of different types (e.g., supercomputers, graphics workstations, and workstation file servers). A distributed computing environment permits the execution of a large computational task to be shared across multiple machines linked together by a network. Thus, a heterogeneous, distributed computing environment consists of many different kinds of machines networked together.

**Hydrostatic.** Used to indicate to a model that it is to assume that the atmosphere is in hydrostatic equilibrium (i.e., surfaces of constant pressure and constant mass coincide and are horizontal throughout). Complete balance exists between the force of gravity and the pressure force.

**Hypertext link.** A hypertext link is a specially designated word or phrase displayed in text that has been saved in Hypertext Markup Language (HTML) format. A hypertext link provides nonsequential access to other entries in a document set. In Models-3, the Help facility is done using hypertext. Hypertext linking is done to all help entries.

**Input/Output Applications Programming Interface (I/O API).** A software library that reads and writes files. It uses an internal data format that is machine independent and that conforms to the widely used University Corporation for Atmospheric Research Network Common Data Format (netCDF). The I/O API files contain self-describing headers with complete information that is necessary to use and interpret the data contained in the file. The Models-3 I/O API format is slightly more restrictive than the standard netCDF format regarding how the header infor­ma­tion must be written. The I/O API library provides a variety of data structure types and a set of reusable access routines that offer selective direct access to the data in terms that are meaningful to the environmental modeler. Supported data types include gridded, boundary, vertical profile, grid nest, time series, and event-driven. For additional information on the I/O API, see Chapter 4.

'''Internal. '''With respect to Models-3 data, internal means that the data are available within the software; the user does not have to provide them. Examples incude look-up tables, ranges, and lists of state/county names.

**Inventory Data Analyzer (IDA).** Program used for input and quality control checks of emission inventories.

**Inventory**. With respect to an emission processing system, inventory refers to a file or a database containing emission data for a specific set of pollutants for a specific time period (typically for the entirety of a specific year) for an area (country, states, counties).

**Irix.** Operating system for SGI computers.

**Keyword.** A keyword is a word or phrase, up to 40 characters long, that can be used to locate an entity in help text or a Models-3 object (e.g., dataset, program, study, or model) using a Find screen.

**Layer collapsing.** A procedure in which the layer structure prepared by a meteorological model is modified by reducing the number of layers. '**'This is not recommended.** ''

**Linux.** An open-source, UNIX-like operating system. It is available and redistributable under the terms of the GNU Public License.

**Makefile**. A Makefile is a list of UNIX commands that perform activities such as mounting files and compiling source code.

**Massively parallel processing.** Computer processing employing multiple CPUs to execute multiple computational tasks simultaneously. Massively parallel systems employ a large number of CPUs simultaneously on the same task. In contrast, conventional computer design uses a single CPU to perform computational tasks in a strictly linear, sequential order.

**Mesoscale.** Medium scale. In meteorology, mesoscale denotes medium horizontal and vertical spatial scale. The horizontal scale extends to several hundred kilometers. The vertical scale extends from tens of meters to the depth of the troposphere.

**Metadata.** Information about data and about the processes that produced them. In particular, information about the data’s structure, internal characteristics and history, and location within a data storage environment, as well as information derived from the data.

**Meteorological model.** This type of model provides descriptions of atmospheric motions, momentum, moisture, heat fluxes, turbulence characteristics, clouds and precipitation, and atmospheric radiative characteristics. Most meteorological models currently in use for air quality modeling were originally developed for the prediction of weather. CMAQ models require information from a meteorological model that is designed to address specific issues relevant to air quality modeling, such as planetary boundary layer information, cloud distribution and mixing characteristics, precipitation, and surface fluxes.

'''Mie scattering. '''A generalized particulate light-scattering mechanism that follows from the laws of electromagnetism applied to particulate matter.

**Mixed-media**. Simultaneously involving more than one environmental pollutant medium, such as air and water.

**Model developer.** Those scientists and software developers who construct and study theories to explain physical and chemical processes, and use computer models to study their theories.

**Model users.** Research, production, and quality assurance personnel (i.e., applied scientists and engineers) who generate valid input for the modeling systems, run the modeling systems, maintain audit trails, analyze input and output for correctness, and produce analyses of the modeling results.

**Model.** A representation of a planned or existing object or condition.

**Modeling structure.** A design specification that provides the paradigm of operation and the interface specifications for the modules used to construct a particular family of models. In a CMAQ model, for example, the paradigm is that modules act as operators upon a shared concentration field, and four types of interfaces (call interfaces, INCLUDE-file interfaces, I/O interfaces, and UNIX-environment interfaces) must be specified.

**Modeling system.** A set of computational models and associated data processors that together provide for the simulation of processes of interest.

**Models-3 components.** The various subsystems within the Models-3 framework. Each component is represented by its own icon. The available components are Dataset Manager, Model Builder, Program Manager, Science Manager, Strategy Manager, Study Planner, and Tools Manager.

**Models-3.** The third-generation air quality modeling system. It is a flexible system that addresses multiple air quality issues, such as regional- and urban-scale oxidant and acid deposition.

**Module.** A subset that is part of a larger program (such as a meteorological model, an emissions model, or CMAQ). In a modular program, all modules of a particular type (e.g., those that compute dry deposition) are interchangeable, allowing you to replace one module with another to determine, for example, how each module affects modeling results. Examples of modules include science modules and analysis and visualization modules.

**Monotonic.** A quality of strictly increasing or decreasing within some interval.

**Multilevel nesting.** Multilevel nesting refers to employing nested grids within nested grids, possibly several levels deep.

**National Emissions Inventory.** A database at EPA containing the information about sources that emit criteria air pollutants and their precursors, and hazardous air pollutants.

**Nested grids.** Nesting refers to fitting a finer-resolution grid over part of a coarser-resolution grid. The finer-resolution grid receives information (such as boundary conditions) from the coarser-grid simulation.

**Nonconforming datasets.** Nonconforming datasets are ones that are not in I/O API format. They can be used in the Models-3 framework by programs that are specifically designed to read those datasets. When nonconforming datasets and programs are used, however, you must know how to match programs and datasets, and which data formats and programs are transportable to different machine architectures. Those considerations are automatically managed by the Models‑3 framework for those who use conforming datasets and conforming programs.

**Nonhydrostatic.** Used to indicate that the model does not assume that the atmosphere is in hydrostatic equilibrium. Air is not assumed to have only horizontal motion relative to the earth.

**Open-source software.** Open-source software began as a marketing campaign for free software. OSS can be defined as computer software for which the human-readable source code is made available under a copyright license (or arrangement such as the public domain) that meets the “open source” definition. This permits users to use, change, and improve the software, and to redistribute it in modified or unmodified form. It is very often developed in a public, collabor­ative manner. Open-source software is the most prominent example of open-source development and often compared to user-generated content.

**Operational models.** These models offer fully functional modeling of relevant science processes, such as atmospheric, emissions, and chemical transport processes. They represent the current state-of-the-art that has undergone significant quality assurance review, peer review, and evaluation. The turnaround time for these models is generally much longer than for screening models but short enough to allow episodic studies.

**Parameterize.** To create an algorithm that describes the average large-scale behavior of a physical phenomenon, rather than describing the subgrid-scale behavior in terms of the underlying physics and chemistry. For example, a parameterized cloud algorithm might describe average cloud behavior over 80-km-square cells, although the individual clouds are much smaller that 80 km across.

**Planetary boundary layer.** The portion of the atmosphere adjacent to the earth’s surface. This layer generally ranges from 100 m to 2 km in height, and is significantly influenced by surface effects (e.g., heating, friction). These effects can cause the layer to be well-mixed, which affects the distribution of pollutants in the air. (See also “troposphere.”)

**Popup window.** A popup window is a special window for displaying an on-line help entry. The window opens when you select a specially designated hypertext link. Pop-up windows are useful for quickly displaying short, concise units of information. You cannot jump or step to other entries from a pop-up window.

**Prepare.** Read and process a file or a set of data.

**Process analysis.** Traces the source(s) of a chemical species within a simulation. One example of process analysis is determining whether a species concentration originated within the cell in which it is found or instead within some other cell(s). Another example is determining what chemical constituents were involved in creating a species produced by reaction (rather than transported from somewhere else).

**Process.** Read a file or a set of data, perform the desired functionality (quality control, reformat­ting, algebraic operations, etc.) and submit the processed data to the next set of actions.

**Quality control (QC).** The act of reading data (inventories, files) and checking for correctness, completeness, and consistency. QC may involve automatic correction, substitution, or the filling of missing data. All QC processes are followed by QC reports.

**Register data.** When you register data, you are making something that already exists (e.g., a file) known to the system.

**Rule effectiveness percent.** An adjustment to projected estimated emissions data to account for emissions underestimates due to compliance failures and the inability of most inventory techniques to include these failures in an emission estimate. The adjustment accounts for known underestimates due to noncompliance with existing rules, control equipment downtime, or operational problems and process upsets. Valid values: 0 to 100.

**Rule penetration percent.** An adjustment to projected estimated emissions data to account for emissions underestimates due to failure to implement rules throughout the area of intent. Valid values: 0 to 100.

**Scalable.** In the context of parallel computer architectures and algorithms, a parallel architecture or algorithm is termed scalable if its performance behaves linearly in terms of the number of processors employed. For example, doubling the number of processors does not cause new communications bottlenecks to appear but doubles the performance achieved.

**Scale flexibility.** The modeling system’s ability to accurately simulate physical and chemical processes that describe atmospheric pollutants at different spatial and temporal scales. A modeling system with scalable algorithms for physical and chemical processes and with a generic grid system has this quality.

**Science module.** A component that is part of a modeling program (such as a meteorological model, an emissions model, or CMAQ) and that computes data for a discrete category of environmental phenomena (e.g., dry deposition, photochemistry, vertical advection).

**Screening models.** These models have simplified science processes designed for quick assess­ment studies. These models are employed when users are willing to sacrifice some accuracy for faster turnaround time. A typical screening study involves making a large number of runs in order to identify episodes or situations that warrant more detailed modeling.

**Source Classification Code (SCC).** The SCC system is used for classifying emissions sources at the level of individual processes (e.g., automobiles, boilers, solvent sources) within an emissions data inventory.

**Source.** With respect to air pollution, a point, area, or mobile source that produces and/or emits air pollutants.

**Speciation.** In Models-3, speciation refers to using one of the chemical mechanisms available with Models-3 to disaggregate a chemical substance (pollutant) into simpler compounds.

**Species.** Typically, a chemical substance or group of related substances whose behavior is modeled during environmental simulation modeling.

**Sub-grid-scale process.** Physical process that occurs on a scale smaller than the grid resolution of the modeling system, such as point-source plumes and convective clouds. Since the scale is smaller than the grid resolution, these processes must be estimated or parameterized.

**Summary report.** Generally refers to an automatic, short report generated after the execution of a process.

**Surface fluxes.** The exchange of material, energy, or momentum between the surface of the earth and the atmosphere.

**Time step.** A time step is a fixed unit of time. A model may have one or more internal time steps for various processors. In the Models-3 framework, a time step is used to indicate the length of time between output of variables from the model or a process within the model. Another term might be “output time interval.”

**Troposphere.** The troposphere is the lowest portion of Earth's atmosphere. It contains approximately 75% of the atmosphere's mass and almost all of its water vapor and aerosols. The average depth of the troposphere is about 11 km (7 miles) in the middle latitudes. It is deeper in the tropical regions (up to 20 km [12 miles]) and shallower near the poles (about 7 km [4 miles] in summer, indistinct in winter). Within the troposphere, temperature decreases with altitude. The lowest part of the troposphere, where friction with the Earth's surface influences air flow, is the planetary boundary layer (PBL). This layer is typically a few hundred meters to 2 km (1.2 miles) deep, depending on the landform and time of day. The border between the troposphere and stratosphere is called the tropopause. Above this layer is a temperature inversion—that is, in the stratosphere temperature increases with altitude.

**Visualization.** An important aspect of scientific computing that provides a method for presenting easily understandable data quickly and compactly in the form of charts and graphs.

* * * * *

<references/>

[1] <sup>Future\\ efforts\\ toward\\ fourth-generation\\ systems\\ will\\ extend\\ linkages\\ and\\ process\\ feedback\\ to\\ include\\ air,\\ water,\\ land,\\ and\\ biota\\ to\\ provide\\ a\\ more\\ holistic\\ approach\\ to\\ simulating\\ transport\\ and\\ fate\\ of\\ chemicals\\ and\\ nutrients\\ throughout\\ an\\ ecosystem</sup>

[2] <sup>The\\ CVS\\ ''modules\\ ''file\\ has\\ no\\ intrinsic\\ relationship\\ with\\ the\\ CMAQ\\ classes/module\\ design\\ implementation.</sup>

[Next: Grid Definition](CMAQ_OGD_ch09_grid_defn.md)
