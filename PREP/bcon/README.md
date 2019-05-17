BCON
========

The program BCON prepares chemical boundary conditions (BCs) for the CMAQ
Chemistry Transport Model (CCTM). BCON will generate an output file with
chemical concentrations for all grid cells along the modeling domain's
horizontal boundaries.  The BCs can be time varing, time independent, and either
spatially uniform or variable across the model boundaries, depending on user
specified options and/or input datasets.  If deriving BCs from the ASCII
vertical profiles, BCON creates spatially uniform, time independent BCs. From
concentration (CONC) files, BCON extracts spatially varying BCs, either on the
same grid cell resolution (windowed modeling domain), or for a finer grid-
resolution modeling domain (nested modeling domain).

There are three distinct modes of operation for BCON.  When running BCON, the
user must specify whether to generate BCs: (1) regridded from an existing CONC
file (*regrid*); (2) based on ASCII vertical profiles (*profile*); or (3)
representing patterns for use in transport algorithm testing (*patterns*).

CMAQ can also use boundary conditions derived from global chemistry models
(GCMs). While BCON does not directly support processing of datasets from GCMs
(in their native formats), users could develop their own custom codes to
transform their GCM datasets into I/O API format, which would then allow these
datasets to be input into BCON to generate BCs for the CCTM. In addition, other
tools do exist in the community to generate BCs from GCMs. For example, the CAMx
developers (Ramboll Environ) have codes available for extracting regional model
BCs from both GEOS- Chem and MOZART. Visit the [Support Software section of
www.CAMx.com](http://www.camx.com/download/support- software.aspx) to download
these utilities.

## Environment Variables:

**Table 1. Environment Variables**

|**Variable Name**|**required**|**Default Value**|**Description**|
|---------------------|-------|-------------|-----------------------------------------------------------------------|
|VRSN||v53|Configuration identifier for the BCON simulation. Must match CFG Variable setting in the BCON build script.|
|APPL||SE52BENCH|BCON executable identifier. Must match APPL Variable setting in the BCON build script.|
|BCTYPE|Yes||Sets the BC type you want to generate.||
|||profile|generate spatially homogeneous BCs from the background profile data|
|||regrid|generate BCs nested (or windowed) from a CMAQ CONC file|
|||patterns|generate BCs of various spatial patterns for testing transport algorithms|
|EXEC||BCON_${VRSN}.exe|Executable to use for the simulation. The variable CFG is set in the BCON run script. The variable EXECID is set in the config_cmaq.csh configuration file.|
|GRIDDESC||$CMAQ_HOME/scripts/GRIDDESC1|Grid description file for setting the horizontal grid definition.|
|GRID_NAME||SE53BENCH|Name of the grid definition contained in the GRIDDESC file that specifies the horizontal grid for the current application of the model.|
|OUTDIR||$CMAQ_HOME/data/bcon|Output data directory|
|DATE|No||Sets the Julian date to use in naming the BCON output file for nested runs.|
|SDATE|No|0|Julian start date for extracting boundary conditions from a CCTM CONC file for a nested simulation. If SDATE is not set, it will be set automatically from the MET_BDY_3D_FIN file.|
|STIME|No|0|Start time for extracting boundary conditions from a CCTM CONC file for a nested simulation. If STIME is not set, it will be set automatically from the MET_BDY_3D_FIN file.|
|RUNLEN|No|0|Run length for extracting boundary conditions from a CCTM CONC file for a ested simulation. If RUNLEN is not set, it will be set automatically from the MET_BDY_3D_FIN file.
|IOAPI_ISPH||20|I/O API setting for spheroid type. See I/O API documentation for [setsphere](https://www.cmascenter.org/ioapi/documentation/3.1/html/SETSPHERE.html) for more information.|
|IOAPI_OFFSET_64||YES|I/O API setting for large time-step records. If your output time step is going to produce data that are >2GB per time step, then this needs to be set to YES.|

## BCON input files

**Table 2. BCON input files**

|**File Name**|**Format**|**Description**|
|---------------------|-------------|-----------------------------------------------------------------------|
|BC_PROFILE|`ASCII`|Vertical chemical profiles from which to derive boundary conditions; this file is created by the user; used only when the BC environment variable is set to “profile”|
|CTM_CONC_1|`GRDDED3`|Name and location of the CMAQ concentration file from which to derive boundary conditions; this file is output from CCTM; used only when the BC environment variable is set to “m3conc”|
|MET_CRO_3D_CRS|`GRDDED3`|Name and location of the coarse-grid MET_CRO_3D file that is required for creating the vertical grid structure if this structure changes between nested simulations; this file is output by MCIP|
|MET_BDY_3D_FIN|`BNDARY3`|Name and location of the fine-grid MET_CRO_3D file that is required if the vertical grid structure changes between nested simulations; this file is output by MCIP|
|GRIDDESC|`ASCII`|Horizontal grid description file for defining the model grid; this file is output by MCIP or can be created by the user|

## BCON output files

**Table 3. BCON output files**

|**File Name**|**Format**|**Description**|
|------------|-----------|---------------------------------------------------------------|
|BNDY_CONC_1|`BNDARY3`|Name and location of the gridded boundary conditions data output on the model grid defined by `GRID_NAME`|

The default location of the BCON output files is the `$CMAQ_DATA/bcon`
directory, controlled by the `OUTDIR` variable in the run script. The default
naming convention for all BCON output files uses the `APPL` and `GRID_NAME`
environment variables in the file name. For boundary conditions created from
existing `CCTM CONC` files, the Julian date is also used in the file name
through the `DATE` environment variable. All of the file-naming variables for
`BCON` outputs are set in the run script.

## Compile BCON source code

Execute the build script to compile BCON:

```
cd $CMAQ_HOME/PREP/bcon/scripts
./bldit_bcon.csh [compiler] [version] |& tee build_bcon.log
```

## Run BCON

Set the run script settings according to the execution configuration variables
described above. Run BCON to produce boundary conditions for the CCTM:

```
cd $CMAQ_HOME/PREP/bcon/scripts
./run_bcon.csh |& tee run_bcon.log
```

Check the log file to ensure complete and correct execution without errors.

