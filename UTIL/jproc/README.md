# JPROC

### Description

The program JPROC calculates daily clear-sky photolysis rates from look-up tables of molecular absorption cross-section and quantum yield (CSQY) data, and climatologically derived ozone-column and optical depth data. The outputs from JPROC are ASCII look-up tables of daily clear-sky photolysis rates for photochemical reactions in a selected gas-phase photochemical mechanism at different altitudes, latitudes, and hours from noon. The photochemical mechanism from which these rates are derived is selected during compilation of JPROC. The altitudes (meters), latitudes (degrees), and hour angles (from noon) for which the rates are derived are hardwired in the JPROC source code.

CCTM currently uses an in-line photolysis option that calculates photolysis rates using predicted ozone and aerosols. JPROC is notused for the default configuration of ModPhot set to  phot/inline). JPROC is required to produce daily photolysis rate look-up tables if CCTM is compiled with *ModPhot* set to phot/table.

### Files, configuration, and environment variables

[Figure 7‑7](#Figure7-7) shows the input and output files for JPROC. Some options are invoked at compilation, while others are invoked with execution of the program. When compiling JPROC, the user specifies a chemical mechanism to indicate the gas-phase chemistry for which to calculate photolysis rates. Setting the *Mechanism* variable in the JPROC compile script configures the program to use a specific set of mechanism INCLUDE files to build an executable. JPROC executables are hard-wired to a specific mechanism configuration.

<a id=Figure7-7></a>

![](./images/Figure7-7.png "Figure7-7.png")

**Figure 7‑7. JPROC input and output files**

While JPROC does not require any technical configuration at execution, such as domain specifications, there are several required and optional input files that the user must provide to the program. For the selected photochemical mechanism, the user must provide a set of molecular absorption CSQY data files that are consistent with the photolysis reactions in the mechanism. CMAQ is distributed with a full set of CSQY files for the Carbon Bond, SAPRC, and RACM photochemical mechanism versions supported by the model. If new mechanisms are added to CMAQ, the user must produce the appropriate CSQY data files for the added mechanism. The user also has the option of using the default atmospheric profiles contained in the PROFILES input file or using Total Ozone Mapping Spectrometer (TOMS) data to replace the climatologically derived ozone column data in the PROFILES file.

#### JPROC input files

<a id=Table7-19></a>

**Table 7‑19. JPROC input files**

|**File Name**|**Format**|**Description**|
|---------|--------|----------------------------------------------------------------------|
|ET|ASCII|Extraterrestrial radiation as a function of wavelength|
|PROFILES|ASCII|Seasonal vertical profiles of ozone concentrations, aerosol attenuation, temperature, air density and Dobson values|
|TOMS|ASCII|Total ozone column measurements from the Total Ozone Mapping Spectrometer instrument aboard the sun-synchronous polar orbiting Nimbus satellite|
|O2ABS|ASCII|Absorption CSQY data for molecular oxygen as a function of wavelength|
|O3ABS|ASCII|Absorption CSQY data for ozone as a function of wavelength|
|CSQY|ASCII (directory path)|Directory path containing absorption CSQY data for gas-phase photolysis reactions as a function of wavelength|

#### JPROC output files

<a id=Table7-20></a>

**Table 7‑20. JPROC output files**

|**File Name**|**Format**|**Description**|
|---------------|--------|----------------------------------------------------------------|
|`JTABLE_$Date`|`ASCII`|Daily clear-sky photolysis rates file|

The default location of the JPROC output files is the `$CMAQ_HOME/data/jproc` directory, controlled by the `OUTDIR` variable in the run script. The default naming convention for all JPROC output files uses the Date environment variable in the file name, which is aliased to the `STDATE` environment variable in the run script.

#### Compilation Configuration Variables

The configuration options listed here are set during compilation of the JPROC executable. When these options are invoked they create a binary executable that is fixed to the specified configuration. To change these options it is necessary to recompile JPROC and create a new executable.

-   `CopySrc`  
    Uncomment to copy the source code into a working build (BLD) directory. If commented, only the compiled object and executable files will be placed in the BLD directory.
-   `MakefileOnly`
    Uncomment to build a Makefile to compile the executable. Comment out to create a Makefile and compile.
-  `Mechanism: [default: cb6r3_ae6_aq]`  
    Specifies the gas-phase, aerosol, and aqueous-phase chemical mechanisms for which to create initial conditions. The choices for the *Mechanism* variable are the mechanism directory names under the `$CMAQ_HOME/CCTM/src/MECHS` directory. 
-   `Tracer [default trac0] `  
      Specifies tracer species. Invoking inert tracer species in CMAQ requires defining the tracers using namelist files and compiling the CMAQ programs with these files. The setting for this module corresponds to the directory name in the `$CMAQ_HOME/CCTM/src/MECHS` directory that contains the namelist files for the tracer configuration. The default setting is to not use any tracers.
      - `trac[n]`

#### Execution Configuration variables

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

#### JPROC compilation

[Chapter 5](CMAQ_OGD_ch05_sys_req.md) provides an overview of how to install and compile the CMAQ programs for the tutorial simulation. Follow the steps outlined in Chapter 5 (summarized below) to compile new versions of JPROC:

1.   Compile Bldmake, the CMAQ source code and compilation management program. This needs to be done only once—the first time CMAQ is installed.
-   Cnfigure the JPROC build script to use the config_cmaq.csh script, which points to the available I/O API and netCDF libraries.
-   Configure the JPROC build script for your application by setting the compilation configuration variables described above.
-   Invoke the build script to create an executable:

```
cd $CMAQ_HOME/UTIL/jproc/scripts
./bldit_jproc.csh [compiler] [version] |& tee build_jproc.log
```

#### Run JPROC ####

Set the run script settings according to the execution configuration variables described above. Run JPROC to produce offline clear-sky photolysis rates for the CCTM:

```
cd $CMAQ_HOME/UTIL/jproc/scripts
./run_jproc.csh |& tee run_jproc.log
```

