ICON
========

The program ICON prepares chemical initial conditions (ICs) for CCTM from either ASCII vertical profiles or from an existing CCTM output concentration (CONC) file. ICON creates an output file with a single time step that represents the chemical conditions in each grid cell at the beginning of a CCTM simulation. The ICs can be either spatially uniform or variable across the model grid, depending on the source of the initial chemical concentration data. If deriving ICs from the ASCII vertical profiles, ICON creates spatially uniform ICs. From CONC files, ICON can extract spatially varying ICs, either on the same grid cell resolution, as a windowed modeling domain, or for a finer-resolution model grid (as for a nested simulation).

There are three distinct modes of operation for ICON.  When running ICON, the user must specify whether to generate ICs: (1) based on ASCII vertical profiles (*profile*); (2) regridded from an existing CONC file (*regrid*); or (3) tracers for used in transport algorithm testing (*tracer*).

CMAQ can also be initialized using downscaled from global chemistry models (GCMs), such as GEOS-Chem and MOZART. ICON does not support the processing of data from GCMs. ICs derived from GCMs must be calculated with custom codes or scripts that are not available in the CMAQ distribution package. The CAMx developers (Ramboll Environ) have codes available for extracting regional model ICs from both GEOS-Chem and MOZART. Visit the [Support Software section of www.CAMx.com](http://www.camx.com/download/support-software.aspx) to download these utilities.

## Environment variables used:

```

 VRSN   [default: v53]
    Configuration identifier for the ICON simulation. Must match CFG Variable setting in the ICON build script.
 APPL   [default: SE52BENCH]
    ICON executable identifier. Must match APPL Variable setting in the ICON build script.
 ICTYPE: [default: regrid]
    Sets the IC type you want to generate.
    profile   (generate spatially homogeneous ICs from the background profile data)
    regrid    (generate ICs nested (or windowed) from a CMAQ CONC file)
    tracer    (generate ICs of various spatial patterns for testing transport algorithms)
 EXEC    [default: ICON_${VRSN}.exe]
    Executable to use for the simulation. The variable CFG is set in the ICON run script. The variable EXECID is set in the config_cmaq.csh configuration file.
 GRIDDESC   [default: $CMAQ_HOME/scripts/GRIDDESC1]
    Grid description file for setting the horizontal grid definition.
 GRID_NAME    [default:SE52BENCH]
    Name of the grid definition contained in the GRIDDESC file that specifies the horizontal grid for the current application of the model.
 OUTDIR   [default: $CMAQ_HOME/data/bcon]
    Output data directory.
 DATE      
    Sets the Julian date to use in naming the ICON output file for nested runs.
 SDATE   [default: ${DATE}]
    Julian start date for extracting boundary conditions from a CCTM CONC file for a nested simulation. If SDATE is not set, it will be set automatically from the CTM_CONC_1 file.
 STIME   [default: 000000 ]
    Start time for extracting boundary conditions from a CCTM CONC file for a nested simulation. If STIME is not set, it will be set automatically from the CTM_CONC_1 file.
```

## Environment Variables (not required):
```
 IOAPI_ISPH    [default: 20]
    I/O API setting for spheroid type. See I/O API documentation for [setsphere](https://www.cmascenter.org/ioapi/documentation/3.1/html/SETSPHERE.html) for more information.
 IOAPI_OFFSET_64   [default: NO]
    I/O API setting for large time-step records. If your output time step is going to produce data that are >2GB per time step, then this needs to be set to YES.
```

## ICON input files

<a id=Table1></a>

**Table 1. ICON input files**

|**File Name**|**Format**|**Description**|
|---------------------|-------------|-----------------------------------------------------------------------|
|IC_PROFILE|ASCII|Vertical chemical profiles from which to derive initial conditions; this file is created by the user; used only when the IC environment variable is set to “profile”|
|CTM_CONC_1|GRDDED3|Name and location of the CMAQ concentration file from which to derive initial conditions; this file is output from CCTM; used only when the BC environment variable is set to “m3conc”|
|MET_CRO_3D_CRS|GRDDED3|Name and location of the coarse-grid MET_CRO_3D file that is required for creating the vertical grid structure if this structure changes between nested simulations; this file is output by MCIP|
|MET_CRO_3D_FIN|GRDDED3|Name and location of the fine grid MET_CRO_3D file that is required if the vertical grid structure changes between nested simulations; this file is output by MCIP|
|GRIDDESC|ASCII|Horizontal grid description file for defining the model grid; this file is output by MCIP or can be created by the user|

## ICON output files

<a id=Table2></a>

**Table 2. ICON output files**

|**File Name**|**Format**|**Description**|
|------------|-----------|---------------------------------------------------------------|
|INIT_CONC_1|`GRDDED3`|Name and location of the gridded initial conditions data output on the model grid defined by `GRID_NAME`|

The default location of the ICON output files is the `$CMAQ_DATA/icon` directory, controlled by the `OUTDIR` variable in the run script. The default naming convention for all ICON output files uses the `APPL` and `GRID_NAME` environment variables in the file name. For initial conditions created from existing `CCTM CONC` files, the Julian date is also used in the file name through the `DATE` environment variable. All of the file-naming variables for `ICON` outputs are set in the run script.

## Compile ICON source code

Execute the build script to compile ICON:

```
cd $CMAQ_HOME/PREP/icon/scripts
./bldit_icon.csh [compiler] [version] |& tee build_icon.log
```

## Run ICON

Set the run script settings according to the execution configuration variables described above. Run ICON to produce initial conditions for the CCTM:

```
cd $CMAQ_HOME/PREP/icon/scripts
./run_icon.csh |& tee run_icon.log
```
Check the log file to ensure complete and correct execution without errors.

