# JPROC

### Description

JPROC calculates clear-sky photolysis rates used by a photochemical mechanism based on their molecular absorption cross-sections and quantum yields (CSQY) as well as climatological data. Output files are ASCII look-up tables containing for the computed rates versus altitude, latitude, and hour angle from noon. The photochemical mechanism selected is determined by the build script for JPROC. Altitudes (meters), latitudes (degrees), and hour angles are hardwired in JPROC's source code.

The recommended configuration of CMAQ's CCTM does not use JPROC output but the look-up tables are required if CCTM is compiled with *ModPhot* set to phot/table.

### Files, configuration, and environment variables

Tables 1 and 2 show the input and output files for JPROC. Some options are invoked at compilation, while others are invoked with execution of the program. When compiling JPROC, the user specifies a chemical mechanism to indicate the gas-phase chemistry for which to calculate photolysis rates. Setting the *Mechanism* variable in the JPROC compile script configures the program to use a specific set RXNS_DATA_MODULE.F90 file to build an executable so the executable is hard-wired to a specific mechanism configuration.

Several required and optional input files are used the JPROC. For the selected photochemical mechanism, the user must provide a set of data file containing molecular absorption and yield data files for photolysis reactions used by the photochemical mechanism. CMAQ is distributed with a full set of CSQY files for the Carbon Bond, SAPRC, and RACM photochemical mechanism versions supported by the model. If a user develops new mechanism with new photolysis rates, they must produce the appropriate CSQY data files for each rate. The user also has the option of using the default atmospheric profiles contained in the PROFILES input file or using Total Ozone Mapping Spectrometer (TOMS) data to replace the climatologically derived ozone column data in the PROFILES file.

**Table 1. JPROC input files**

|**File Name**|**Format**|**Description**|
|---------|--------|----------------------------------------------------------------------|
|ET|ASCII|Extraterrestrial radiation as a function of wavelength|
|PROFILES|ASCII|Seasonal vertical profiles of ozone concentrations, aerosol attenuation, temperature, air density and Dobson values|
|TOMS|ASCII|Total ozone column measurements from satellite (optional input file(s))|
|O2ABS|ASCII|Absorption CSQY data for molecular oxygen as a function of wavelength|
|O3ABS|ASCII|Absorption CSQY data for ozone as a function of wavelength|
|CSQY|ASCII (directory path)|Directory path containing absorption CSQY data for gas-phase photolysis reactions as a function of wavelength|

**Table 2. JPROC output files**

|**File Name**|**Format**|**Description**|
|---------------|--------|----------------------------------------------------------------|
|`JTABLE_$Date`|`ASCII`|Daily clear-sky photolysis rates file|

The location of the JPROC output files is controlled by the `OUTDIR` variable in the run script. The default name for output files uses the Date environment variable in the file name aliased to the `STDATE` environment variable in the run script.

#### Key Build Script Variables

The configuration options are listed below. The build script set their values for compiling a JPROC executable that is fixed to the specified configuration. To change these options it is necessary to re-run the build script for a new executable.

-   `CopySrc`  
    Uncomment to copy the source code into a working build (BLD) directory. If commented, only the compiled object and executable files will be placed in the BLD directory.
-   `MakefileOnly`
    Uncomment to build a Makefile to compile the executable. Comment out to create a Makefile and compile.
-  `Mechanism: [default: None]`  
    Determines the path to FORTRAN data module for the photochemistry mechanism based on its full name. The possible choices are subdirectories under the `$CMAQ_HOME/CCTM/src/MECHS` directory. If the application is using a new mechanism, the user defines the Mechanism and its location. 
-   `Tracer [default trac0] `  
      Specifies tracer species. Invoking inert tracer species in CMAQ requires defining the tracers using namelist files and compiling the CMAQ programs with these files. The setting for this module corresponds to the directory name in the `$CMAQ_HOME/CCTM/src/MECHS` directory that contains the namelist files for the tracer configuration. The default setting does not use any tracers.

#### Key Run Script Variables

The environment variables listed here are invoked during execution of the program and are set in the JPROC run script.

-   `APPL [default: None]`  
    JPROC executable identifier. Must match APPL Variable setting in the JRPOC build script.
-   `CFG [default: None]`  
    Configuration identifier for the JPROC simulation.
-   `MECH [default: None]`  
    CMAQ chemical mechanism. Must match Mechanism variable setting in the JPROC build script.
-   `EXEC: [default: JPROC_${APPL}_${EXEC_ID}]`  
    Executable to use for the simulation. The variable CFG is set in the JPROC run script. The variable EXEC_ID is set in the config_cmaq.csh configuration file.
-   `STDATE`  
    Start Julian date (YYYYDDD) for computing clear sky photolysis rates.
-   `ENDATE`  
    End Julian date (YYYYDDD) for computing clear sky photolysis rates.

### Compiling and Running

#### Building JPROC 

Follow the below steps for compiling JPROC. The process requires the bldmake utility. Check the bldmake README on compiling it if an executable does not exist.

1.    Configure the JPROC build script to use the config_cmaq.csh script, which points to the available I/O API and netCDF libraries.
2.    Configure the JPROC build script for your application by setting the compilation configuration variables described above.
3.   Invoke the build script to create an executable:

```
cd $CMAQ_HOME/UTIL/jproc/scripts
./bldit_jproc.csh [compiler] [version] |& tee build_jproc.log
```

#### Running JPROC ####

Set the run script settings according to the execution configuration variables described above. Run JPROC to produce offline clear-sky photolysis rates for the CCTM:

```
cd $CMAQ_HOME/UTIL/jproc/scripts
./run_jproc.csh |& tee run_jproc.log
```

