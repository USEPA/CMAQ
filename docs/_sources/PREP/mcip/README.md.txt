# Meteorology-Chemistry Interface Processor (MCIP)

The Meteorology-Chemistry Interface Processor (MCIP) ingests output from the [Weather Research and Forecasting (WRF) Model](http://www.wrf-model.org) to prepare the meteorology files that are used within the CMAQ Modeling System. Where possible, MCIP uses data directly from the meteorological model to maximize consistency with the CMAQ Modeling System. When specific atmospheric fields are not explicitly output by WRF, MCIP uses scientific algorithms to create those fields for CMAQ.  MCIP output is used by the emissions model (for example, to provide time-varying temperatures for mobile emissions) and by the CCTM to define the atmospheric conditions. A scientific overview of MCIP is in [Otte and Pleim (2010)](https://www.geosci-model-dev.net/3/243/2010/).

MCIP performs the following functions using the output (history) file from WRF:

-   Defines the computational domain for the CCTM. The CCTM typically uses a smaller computational domain than the meteorological model, and the lateral boundary cells from the meteorological model generally are not used by CCTM.

-   Extracts meteorological model output on the computational domain that is prescribed for the CCTM.

-   Processes all required meteorological fields for the emissions model and the CCTM. Meteorological fields such as atmospheric temperature, pressure, humidity, and winds are acquired directly from the meteorological model (i.e., "passed through").

-   Uses the available meteorological fields to compute additional fields that are required by the CCTM but are not part of the meteorological model's output stream, such as the Jacobian which is used for coordinate transformations.

-   Outputs files that contain meteorological and geospatial information used by the emissions model and the CCTM.  The output can be either in I/O API or netCDF.

MCIP is written in FORTRAN, and it runs on a single processor in a Unix/Linux environment. MCIP is driven by a C-shell script with several run-time options that are defined through a FORTRAN namelist. It is typical to use MCIP to process hourly output fields from the meteorological model for each one-day period.

MCIP is often updated concurrently with the CCTM.  The changes to MCIP are documented with each update to the software, and a "Frequently Asked Questions" (FAQ) file exists that is specific to MCIP.

As of MCIPv5.0, WRF is the only meteorological model that can be processed with MCIP, but MCIP could be expanded to process data from other meteorological models.

MCIP can be used to determine the spatial region that is processed by CMAQ. MCIP can process the full meteorological modeling domain, uniformly trim cells from that domain, or "window" a rectilinear subset of that domain. Configuration options for MCIP include the time periods over which to extract data from the meteorological model output files, horizontal and vertical grid definitions, and selections for integrating satellite cloud observations into MCIP output.

## Files, configuration, and environment variables

All MCIP configurations are established at run-time (rather than at compile time) via Fortran namelist variables rather than environment variables, which is a distinction from the rest of the CMAQ programs. The user does not need to directly edit the MCIP namelist file. All configuration settings are contained in the MCIP run script (run_mcip.csh), which automatically creates a new namelist file each time the script is executed.  The MCIP input files are listed in Table 1, and the MCIP output files are listed in Table 2.


## Compilation Configuration

All model configuration options for MCIP are set during execution. System compiler options must be set in the provided Linux Makefile to build the program for different operating system/compiler combinations. Example compiler paths, flags, and library locations are provided in the default Makefile.


## Execution Configuration Variables

The variables listed here are set by the user in the MCIP script (run_mcip.csh), and they are used during execution of the program.

-   `APPL [default: None]`  
    Application name; scenario ID for file naming
-   `CoordName [default: None]`  
    Coordinate system name of the MCIP output grid that is written to the GRIDDESC file. Additional information about the parameters in the GRIDDESC file can be found in the [I/O API Documentation](https://www.cmascenter.org/ioapi/documentation/all_versions/html/GRIDDESC.html).
-   `GridName [default: None]`  
    Model grid name of the MCIP output grid that is written to the GRIDDESC file. 
-   `DataPath [default: $CMAQ_DATA]`  
    Input/output data directory path
-   `InMetDir [default: None]`  
    Path of the input data directory containing the WRF‑ARW output data files
-   `InGeoDir [default: None]`  
    Path of the input data directory containing the WRF Geogrid file
-   `OutDir [default: $CMAQ_HOME/data/mcip]`  
    Path of the MCIP output data directory
-   `ProgDir [default: $CMAQ_HOME/PREP/mcip/src]`  
    Working directory containing the MCIP executable
-   `WorkDir [default: $OutDir]`  
    Temporary working directory for Fortran links and the namelist file
-   `InMetFiles [default: None]`  
    List of input meteorology files, including the directory path for each file; without modifying MCIP, up to 300 meteorological model output files are allowed as input to a single MCIP execution
-   `IfGeo [default: F]`  
    Binary flag indicating the availability of an input WRF Geogrid file; options include T (true) or F (false)
-   `InGeoFile [default: None]`  
    Name and location of input WRF Geogrid file
-   `LPV: [default: 0]`  
    Compute and output potential vorticity. This must be activated to support the [CCTM O3 potential vorticity scaling](../../CCTM/docs/ReleaseNotes/Potential_Vorticity_Scaling.md).
    -   `0`: Do not compute and output potential vorticity
    -   `1`: Compute and output potential vorticity
-   `LWOUT [default: 0]`  
    Output vertical velocities.
    -   `0`: Do not output vertical velocity
    -   `1`: Output vertical velocity
-   `LUVBOUT [default: 0]`  
    Output u- and v-component winds on B staggered grid.
    -   `0`: Do not output u- and v-component winds on B-grid
    -   `1`: Output u- and v-component winds on B-grid (in addition to the C-grid)
-   `MCIP_START [format: YYYY-MM-DD-HH:MM:SS.SSSS]`  
    Beginning date and time (UTC) of data to output from MCIP. The start date and time must be contained within the input data from WRF.
-   `MCIP_END [format: YYYY-MM-DD-HH:MM:SS.SSSS]`  
    End date and time (UTC) of data to output from MCIP. The end date and time must be contained within the input data from WRF.
-   `INTVL [default: 60]`  
    Output interval in minutes. This setting determines the amount of model time contained in each output time step. The output interval for MCIP can be less frequent than the incoming meteorological model output (e.g., process 30-minute data for CCTM from 15-minute WRF output).
-   `MKGRID [default: T]`  
    Determines whether to output static (GRID) meteorology files
-   `IOFORM [default: 1]`  
    Choose output format.
    -   `1`: Models-3 I/O API
    -   `2`: netCDF
-   `BTRIM [default: 5]`  
    The number of boundary points to remove on each of the four horizontal sides of the meteorology output to define the MCIP output domain. Setting BTRIM = 0 will specify the maximum extent of the input meteorology domain. To remove the WRF lateral boundaries, set BTRIM = 5 (recommended).
    This setting affects the output MCIP horizontal domain by reducing the input meteorology domain by 2*BTRIM + 2*NTHIK + 1, where NTHIK is the lateral boundary thickness (from the BDY files). The extra point reflects the conversion from the grid points (dot points) to grid cells (cross points).
    To crop a subset of the input meteorology ("window"), set BTRIM = -1; this setting causes BTRIM to be replaced by the information provided by X0, Y0, NCOLS, and NROWS (see below).
-   `X0 [used only if BTRIM = -1]`  
    The *x*-coordinate of the lower-left corner of the full MCIP cross-point domain (including the MCIP lateral boundary) based on the input WRF‑ARW domain. X0 refers to the east-west direction.
-   `Y0 [used only if BTRIM = -1]`  
    The *y*-coordinate of the lower-left corner of the full MCIP cross-point domain (including the MCIP lateral boundary) based on the input WRF‑ARW domain. Y0 refers to the north-south direction.
-   `NCOLS [used only if BTRIM = -1]`  
    Number of columns in the output MCIP domain (excluding MCIP lateral boundaries)
-   `NROWS [used only if BTRIM = -1]`  
    Number of rows in the output MCIP domain (excluding MCIP lateral boundaries)
-   `LPRT_COL [default: 0]`  
    Column cell coordinate for diagnostic outputs on the MCIP modeling domain
-   `LPRT_ROW [default: 0]`  
    Row cell coordinate for diagnostic outputs on the MCIP modeling domain
-   `WRF_LC_REF_LAT [optional; used only for Lambert conformal projections; default: -999.0]`  
    WRF Lambert Conformal reference latitude. Use this setting to force the reference latitude in the output MCIP data. If not set, MCIP will use the average of the two true latitudes.


## Compiling and Running

**Compile MCIP**

MCIP is compiled with a Makefile. The configuration options in the Makefile include the compiler and compiler flags to use for building the executable. Note that MCIP is not a parallelized code, so parallel versions of netCDF and I/O API are not required.  The Makefile is located in the directory with the MCIP source code (`$CMAQ_HOME/PREP/mcip/src`). To compile MCIP, source the config_cmaq.csh file and invoke the Makefile at the command line:

```
cd $CMAQ_HOME/PREP/mcip/src/
source $CMAQ_HOME/config_cmaq.csh
./make |& tee make.mcip.log
```

To port MCIP to different compilers, change the compiler names, locations, and flags in the config_cmaq.csh script.

**Run MCIP**

Set the run script settings according to the execution configuration variables described above. Run MCIP to produce meteorology input data for the CCTM:

```
cd $CMAQ_HOME/PREP/mcip/scripts
./run_mcip.csh |& tee run_mcip.log
```


**Table 1. MCIP input files**

|**File Name**|**Format**|**Description**|**Required**|
|------------|------------------------------|-----------------------------------------------------|---------------------|
|InMetFiles|netCDF (WRF)|List of WRF output files for input to MCIP|required|
|InGeoFile|netCDF (WRF)|Output from WRF Geogrid processor|optional; only required if fractional land use are not part of the WRF output|


**Table 2. MCIP output files**

|**File Name**|**Format**|**Description**|**Required**|
|--------------------|-----------------|------------------------------------------------------------------|---------------------------|
|GRIDDESC|ASCII|Grid description file with coordinate and grid definition information|required|
|GRID_BDY_2D|I/O API|Time-independent 2-D boundary meteorology file|required|
|GRID_CRO_2D|I/O API|Time-independent 2-D cross-point meteorology file|required|
|GRID_CRO_3D|I/O API|Time-independent 3-D cross-point meteorology file|required|
|GRID_DOT_2D|I/O API|Time-independent 2-D dot-point meteorology file|required|
|LUFRAC_CRO|I/O API|Time-independent fractional land use by category|created if fractional land use was provided in WRF's output or in Geogrid output|
|MET_BDY_3D|I/O API|Time-varying 3-D boundary meteorology file|required|
|MET_CRO_2D|I/O API|Time-varying 2-D cross-point meteorology file|required|
|MET_CRO_3D|I/O API|Time-varying 3-D cross-point meteorology file|required|
|MET_DOT_3D|I/O API|Time-varying 3-D dot-point meteorology file|required|
|MOSAIC_CRO|I/O API|Time-varying 3-D output from mosaic land use|created if the Noah Mosaic land-surface model was run in WRF|
|SOI_CRO|I/O API|Time-varying soil properties in each soil layer|created if a land-surface model was run in WRF|
|mcip.nc|netCDF|contains both time-independent and time-varying output variables that contain 2-D layers (either only in 2-D or in 3-D, where the third dimension could be atmospheric layers, soil layers, land use categories, mosaic categories, etc.)|required, if IOFORM=2|
|mcip_bdy.nc|netCDF|contains time-independent and time-varying output along the domain perimeter|required, if IOFORM=2| 

The default location of the MCIP output files is the `$CMAQ_HOME/data/mcip/$GridName` directory, but it can be changed in the MCIP script using the `$OutDir` variable. The names of the MCIP output files are generic and do not have any information about the model grid that they are simulating or the time period that is covered. These attributes can be controlled by the MCIP script. For example, the name of the grid can be used in the output directory path. In addition, the default naming convention for all MCIP output files appends the `APPL` environment variable to the file name to identify files by the time period that is represented by the file. All of the file naming variables for the MCIP outputs are set in the run script, and they can be easily tailored to fit each user's application or style.

**Previous Versions of MCIP**

MCIPv4.3 and MCIPv4.2 were released via the CMASCenter MCIP GitHub repository.  Beginning with version 4.5, MCIP is released as part of the CMAQ repository. The MCIP version and CMAQ version are aligned beginning with version 5.3.3. 
* [MCIPv5.4 (October 2022)](https://github.com/USEPA/CMAQ/blob/5.4/PREP/mcip/docs/ReleaseNotes) - bundled under CMAQv5.4
* [MCIPv5.3.3 (August 2021)](https://github.com/USEPA/CMAQ/blob/5.3.3/PREP/mcip/docs/ReleaseNotes) - bundled under CMAQv5.3.3
* [MCIPv5.1 (December 2019)](https://github.com/USEPA/CMAQ/blob/5.3.1/PREP/mcip/docs/ReleaseNotes) - bundled under CMAQv5.3.1
* [MCIP v5.0 (August 2019)](https://github.com/USEPA/CMAQ/blob/5.3/PREP/mcip/docs/ReleaseNotes) - bundled under CMAQv5.3 
* [MCIP v4.5 (October 2018)](https://github.com/USEPA/CMAQ/blob/CMAQv5.3.b2_19Oct2018/PREP/mcip/docs/ReleaseNotes) - bundled under CMAQv5.3beta2 
* [MCIP v4.3 (November 2015)](https://github.com/CMASCenter/MCIP/tree/4.3) - available on CMASCenter GitHub repository
* [MCIP v4.2 (December 2013)](https://github.com/CMASCenter/MCIP/tree/4.2) - available on CMASCenter GitHub repository
