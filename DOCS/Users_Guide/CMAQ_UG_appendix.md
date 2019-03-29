<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch13_support.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_glossary.md)

<!-- END COMMENT -->

* * *

# Appendix A: Model options

## A.1 config_cmaq.csh
Consistency of configuration variables is critical for building CMAQ itself, not just its libraries. Accordingly CMAQ includes the configuration script config_cmaq.csh to help enforce consistent environment settings for CMAQ and its associated libraries. The following lists the config_cmaq.csh variables defined for the build process and suggests values to which to set those variables.

Note that for multiprocessor applications it is recommended that the Fortran MPI wrapper script mpif90 be specified for the Fortran compiler (myFC). Using this script, instead of a direct call to the Fortran compiler, will ensure that the full suite of MPI components (libraries and include files) for the compiler are included in the parallel build.
  
-   `CMAQ_HOME` <a id=CMAQ_HOME></a>
    The central CMAQ installation directory. For example, if you installed the CMAQ source code in the directory `/home/user/CMAQ` set CMAQ_HOME with `export CMAQ_HOME=/home/user/CMAQ` for bash or `setenv CMAQ_HOME /home/user/CMAQ` for csh; note that this variable is M3HOME prior to CMAQv5.2

-   `CMAQ_DATA`<a id=CMAQ_DATA></a>
    Automatically set by config_cmaq.csh; note that this variable is M3DATA prior to CMAQv5.2

-   `CMAQ_LIB`<a id=CMAQ_LIB></a>
    Automatically set by config_cmaq.csh; note that this variable is M3LIB prior to CMAQv5.2

-   `M3MODEL`
    Automatically set by config_cmaq.csh; deprecated in CMAQv5.2

-   `compiler` 
    Set the Fortran compiler type that you will use to compile CMAQ; choices are intel, pgi, or gcc

-   `compilerVrsn`
    (Optional) Set the Fortran compiler version number that you will use to compile CMAQ; if you employ this variable, it will be appended to the compiler type when naming build directories and executables

-   `IOAPI_MOD_DIR`
    Location of the I/O API modules installation on your Linux system

-   `IOAPI_INCL_DIR`
    Location of the I/O API include file installation on your Linux system

-   `IOAPI_LIB_DIR`
    Location of the I/O API library installation on your Linux system

-   `NETCDF_LIB_DIR`
    Location of the netCDF installation on your Linux system

-   `MPI_LIB_DIR`
    Location of the Message Passing Interface installation on your Linux system

-   `netcdf_lib`
    Name of the netCDF library on your system;  set to "-lnetcdf" for versions < 4.2.0, "-lnetcdff -lnetcdf" for version 4.2.0 and later

-   `ioapi_lib`
    Name of the I/O API libraryar on your system; set to "-lioapi"

-   `pnetcdf_lib`
    Name of the parallel netCDF library on your system; set to "-lpnetcdf"

-   `mpi_lib`
    Name of the MPI library on your system; set to "-lmpich" for MVAPICH, "-lmpi" for OpenMPI

-   `myFC`
    Set to match the `FC` (Fortran compiler) you used to compile netCDF

-   `myCC`
    Set to match the `CC` (C compiler) you used to compile netCDF

-   `myFFLAGS`
    Fixed-format Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `myCFLAGS`
    C compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `myFRFLAGS`
    Free form-format Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `MPI_INC`
    Set to the path to your MPI library INCLUDE files, e.g. `$M3LIB/mpich/include`

-   `extra_lib`
    Set to other libraries required for compiling on your Linux system; users will likely need to change this setting in the distributed script for portability to their system.

-   `EXEC_ID`
    build tag, should be automatically set by config_cmaq.csh

## A.2 Compilation Configuration Variables

The configuration options listed here are set during compilation of the CCTM executable. When these options are invoked they create a binary executable that is fixed to the specified configuration. To change these options you must recompile CCTM and create a new executable.

Several of the CCTM science modules have more than one option.  Brief descriptions of these options are provided here.  For details on the science of the different options refer to the [CMAQ Release Notes](../../CCTM/docs/Release_Notes/README.md).

The following five options are invoked by uncommenting the line in the CCTM build script.  Comment the line in the script using a "#" to turn the option off.

-   `CopySrc`  
    Uncomment to copy the source code into a working build (BLD) directory. If commented, only the compiled object and executable files will be placed in the BLD directory.

-   `set ParOpt`  
    Build an executable for running on multiple processors. Invoking this command requires the availability of the MPI library/INCLUDE files.

-   `MakeFileOnly`  
    Uncomment to build a Makefile to compile the executable. Comment out to both create a Makefile and compile.

-   `set build_parallel_io`  
     Uncomment to build CMAQ with true parallel I/O feature (requires ioapi3.2 and pnetcdf)

-   `set build_twoway`  
    Uncomment to build WRF-CMAQ twoway - this cannot be set for stand-alone CMAQ

-   `set potvortO3`  
    Uncomment to build CMAQ with potential vorticity free-troposphere O3 scaling

The following configuration settings may have multiple options. Select one option in the CCTM build script.

-   `ModDriver: [default: driver/wrf]`  
    The CCTM generalized -coordinate driver module.
    - `driver/wrf`  
    use WRF-based scheme for mass-conserving advection; select this option when using WRF meteorology
    - `driver/yamo`  
    use Yamartino scheme for mass-conserving advection

-   `ModGrid: [default: Cartesian]`  
    The CCTM model grid configuration module. Currently only Cartesian coordinates are supported by CMAQ. Do not change this module setting.
    -   `grid/cartesian`

-   `ModInit: [default: init/yamo]`  
    The CCTM time-step initialization module that uses a Yamartino scheme for mass-conserving advection. Do not change this module setting.
    -   `init/yamo`

-   `ModCpl: [default: couple/gencoor_wrf]`  
    Mass coupling concentration converstion module options. Unit conversion and concentration coupling module.
    -   `couple/gencoor_wrf`  
    Coupling scheme compatible with the WRF-based advection scheme; select this option when `ModDriver` is set to `driver/wrf`
    -  `couple/gencoor`  
    Coupling scheme compatible with the Yamartino advection scheme; select this option when `ModDriver` is set to `driver/yamo`.  

-    `ModHadv: [default: hadv/yamo]`  
      Horizontal advection module.  Currently only the Yamartino global mass-conserving hoizontal advection scheme is supported.
     -   `hadv/yamo`

-   `ModVadv: [default: vadv/wrf]`  
    Vertical advection module.
    -   `vadv/wrf`  
    use the WRF omega calculation with the Piecewise Parabolic Method (PPM) to calculate vertical advection; this module should be used only with WRF meteorology
    -   `vadv/yamo`  
    use the global mass-conserving scheme to calculate vertical advection
-   `ModHdiff: [default: hdiff/multiscale]`  
    The only option in CMAQv5 for the horizontal diffusion module is `hdiff/multiscale`, which uses a diffusion coefficient based on local wind deformation. Do not change this module setting.
    -   `hdiff/multiscale`  
-   `ModVdiff: [default: vdiff/acm2]`  
    Vertical diffusion and surface exchange module. Do not change this module setting.
    -   `vdiff/acm2`  
    calculate vertical diffusion using the Asymmetric Convective Model version 2 (ACM2)
-   `ModDepv: [default: depv/m3dry]`  
    Deposition velocity calculation module. Do not change this module setting.
    -   `depv/m3dry`  
    CMAQ dry deposition velocity routine
-   `ModEmis: [default: emis/emis]`  
    CMAQ in-line anthropogenic and natural emissions module. In line emissions are activated in the CCTM run script. Do not change this module setting.
    -   `emis/emis`
-   `ModBiog: [default: biog/beis3]`  
Calculate biogenic emissions in-line with the BEIS3 model. Inline biogenic emissions are activated in the CCTM run script. Do not change this module setting.
    - `biog/beis3`
-   `ModPlmrs: [default: plrise/smoke]`  
Calculate in-line plume rise for large point sources using the Briggs algorithm as it is implemented in SMOKE. Inline emissions plume rise in controlled in the CCTM run script. Do not change this module setting.
    - `plrise/smoke`  
-   `ModCgrds: [default: spcs/cgrid_spcs_nml]`  
    CMAQ model species configuration module.
    -   `spcs/cgrid_spcs_nml`  
    namelist files used to configure CMAQ model species
    -   `spcs/cgrid_specs_icl`  
    use Fortran INCLUDE files used to configure CMAQ model species
-   `ModPhot: [default: phot/inline]`  
    Photolysis calculation module.
    -   `phot/inline`  
    calculate photolysis rates in-line using simulated aerosols and ozone concentrations
    -   `phot/table`  
    calculate clear-sky photolysis rates off-line using the CMAQ program JPROC; provide daily photolysis rate look-up tables to CCTM
-   `Mechanism: [default: cb05e51_ae6_aq`]  
    Chemistry mechanism for gas, aerosol, and aqueous chemistry. See the [CMAQ Mechanism Definitions Table](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/CMAQ_OGD_appendix_A.md) for a listing of the mechanism choices that are available in CMAQv5.2.
-   `Tracer [default trac0] `  
    Specifies tracer species. Invoking inert tracer species in CMAQ requires defining the tracers using namelist files and compiling the CMAQ programs with these files. The setting for this module corresponds to the directory name in the ``$CMAQ_HOME/CCTM/src/MECHS`` directory that contains the namelist files for the tracer configuration. The default setting is to not use any tracers.
    - `trac[n]`
-   `ModGas: [default: gas/ebi_${Mechanism}]`  
     Gas-phase chemistry solver module.
     -  `smvgear`  
     use the SMVGEAR chemistry solver
     -  `ros3`  
     use gas/the Rosenbrock chemistry solver
     -  `ebi`  
     use the Euler Backward Iterative solver
-   `ModAero: [default: aero6]`  
    CMAQ aero/aerosol module.
    -   `aero6`  
    sixth-generation modal CMAQ aerosol model with extensions for sea salt emissions and thermodynamics; includes a new formulation for secondary organic aerosol yields
-   `ModCloud: [default: cloud/acm_ae6]`  
    CMAQ cloud module for modeling the impacts of clouds on deposition, mixing, photolysis, and aqueous chemistry.
    -   `cloud/acm_ae6`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with heterogeneous chemistry for AERO6
    -   `cloud/acm_ae6_mp`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with heterogeneous chemistry for AERO6 and air toxics; this is the multipollutant mechanism in CMAQv5
    -   `cloud/acm_ae6_kmt`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with heterogeneous chemistry for AERO6 and aqueous chemistry with kinetic mass transfer and Rosenbrock solver
    -   `cloud/acm_ae6i_kmti`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with heterogeneous chemistry for AERO6 and aqueous chemistry with kinetic mass transfer and Rosenbrock solver with an extension to simulate the aqueous phase formation of SOA in cloud droplets, see: [CMAQv5.1 Aqueous Chemistry](https://www.airqualitymodeling.org/index.php/CMAQv5.1_Aqueous_Chemistry)
-   `ModUtil: [default: util]`  
    CMAQ utility modules. Do not change this module setting.
    -  `util/util`
-   `Tracer: [default: trac0]`
     Add chemically inert tracers to the CCTM, default no tracer species.
-   `ModPa: [default: procan/pa]`
    Process analsis is controlled in the CCTM run script. Do not change this module setting.
     - `procan/pa`
-   `ModPvO3: [default: pv_o3]`
    Potential vorticity parameterization for free-troposphere exhange of ozone. This option is configured using the potvorO3 variable in the CCTM build script. Do not change this module setting.
    - `pv_o3`

## A3. Execution Configuration Variables

The environment variables listed below are invoked during execution of the CCTM and are set in the CCTM run script.

-   `compiler [default: intel]`
-   `compilerVrsn [default: 13.1]`
-   `VRSN [default: v52]`
-   `PROC [default: mpi]`   
Sets if the CCTM will run in multi-processor or serial mode.
    - `mpi`  
    Use MPI multi-processor configuration. Additional configuration settings are required when selecting `mpi`. The CCTM must have been built to support MPI. The run script requires settings for the number of processors and other MPI configuration variables required by the Linux system.
    - `serial`  
    Run the CCTM in serial, single-processor mode.
-   `MECH [default: None]` 
    CMAQ chemical mechanism. Must match Mechanism variable setting in the CCTM build script.
-   `EMIS [default: 2013ef]`
-   `APPL [default: SE52BENCH]`  
    CCTM executable identifier. Must match APPL Variable setting in the CCTM build script.
-   `RUNID [default: $VRSN_compiler_APPL]` 
    Run ID used to track version number, compiler, and application case name.
-   `EXEC [default: CCTM_$APPL_$EXECID]`  
    The name of the CCTM executable.

### MPI Configuration
-   `NPCOL_NPROW [default: 1 1]`  
    The numbers of columns and rows for decomposing the modeling domain in an MPI configuration. The product of this pair of numbers must equal the total number of processors allocated to the CCTM simulation. For serial or single-processor MPI runs set to `1 1`. For multi-processor simulations, the number of columns (i.e, the first number in the pair) should be greater than or equal to the number of rows.  For example, for an 8 processor MPI simulation, set to `4 2`
-   `NPROCS [default: 1]`  
    Number of processors to allocate for the CCTM simulation; equal to the product of NPCOL x NPROW. For serial or single-processor MPI runs set to `1`, otherwise set to the product of the two numbers used in NPCOL_NPROW.

### Vertical extent
-    `NZ [default: 35]` 
      Set the number of vertical layers. 

### Timestep Configuration

-   `NEW_START_TRUE [default: TRUE]` 
     For a model restart set to FALSE
-   `START_DATE`  
    Simulation start date in Gregorian format (YYYY-MM-DD)
-   `END_DATE`
    Simulation end date in Gregorian format (YYYY-MM-DD)
-   `STTIME`  
    Simulation start time (HHMMSS)
-   `NSTEPS [default: 240000]`  
    Number of simulation time steps (HHMMSS)
-   `TSTEP [default: 010000]`  
    Simulation output time step interval (HHMMSS)

### CCTM Configuration Options

-   `LOGFILE [default: $BASE/$APPL.log]`  
    Uncomment to capture CCTM standard output to a log file; the LOGFILE variable sets the name and location of the log.
-   `GRID_NAME [default: CMAQ-BENCHMARK]`  
    Name of the grid definition contained in the GRIDDESC file that specifies the horizontal grid for the current application of the model.
-   `GRIDDESC [default: $CMAQ_HOME/scripts/GRIDDESC1]`  
    Grid description file for setting the horizontal grid definition.
-   `CTM_APPL [default: ${RUNID}_${YYYYMMDD}]`  
    CCTM log file naming extension.
-   `CONC_SPCS [if commented out, all species]`  
    Model species to be written to the CCTM CONC file.
-   `CONC_BLEV_ELEV [if commented out, all layers]`  
    Vertical model layer range for the CONC-file concentrations; this variable sets the lower and upper layers over which to output the CONC file.
-   `AVG_CONC_SPCS [if commented out, output all species]`  
    Model species for calculating integral average concentrations for each output time step. Options can be any of the standard output species that are written to the CCTM CONC file. The species in this list will be written to the ACONC output file.
-   `ACONC_BLEV_ELEV [default: if commented out, all layers]`  
    Vertical model layer range for integral average concentrations; this variable sets the lower and upper layers over which to calculate integral average concentrations. For example, setting this variable to “1 5” will produce integral average concentrations for model layers 1 through 5.
-   `ACONC_END_TIME [default: N]`  
    Change the time stamp of the ACONC file output time step from the default of the beginning of the hour to the end of the hour.
    - `Y`: Set the time stamp to the end of each hour.
    - `N`: Set the time stamp to the beginning of the hour.
-   `EXECUTION_ID`  
    The name of the CCTM executable; automatically set by the script.

### Synchronization Time Step and Tolerance Options

-   `CTM_MAXSYNC [default: 300]`  
    Maximum synchronization time step in seconds
-   `CTM_MINSYNC [default: 60]`  
    Minimum synchronization time step in seconds
-   `SIGMA_SYNC_TOP [default: .70]`  
    Top sigma level thru which sync step determined
-   `ADV_HDIV_LIM [default: .95]` 
     Maximum horizontal division limit for advection time step adjustment
-   `CTM_ADV_CFL [default: .95]`  
    Maximum Courant–Friedrichs–Lewy (cfl) condition
-   `RB_ATOL [default: 1.0E-09]`  
    Global Rosenbrock (ROS3) chemistry solver absolute tolerance

### Science Options

-   `CTM_WB_DUST [default: Y]`  
    Setting to calculate in-line windblown dust emissions in CCTM. Setting this variable to Y requires the availability of gridded land use input files that include the following BELD USGS land use classifications: shrubland, shrubgrass, and sprsbarren. See [Chapter 8](CMAQ_OGD_ch08_input_files.md#Table8-1) for a description of the DUST_LU_1 and DUST_LU_2 input files. Comment out variable or set to Y to turn on; set to N to turn off.
-   `CTM_ERODE_AGLAND [default: Y]`  
    Setting to use optional erodible agricultural land classifications for computing windblown dust emissions from agricultural land. Setting this variable to Y requires the availability of gridded crop timing data that describe planting start dates, planting end dates, and harvesting end dates for 18 crop types. See [Chapter 8](CMAQ_OGD_ch08_input_files.md#Table8-1) for a description of the CROPMAP01, CROPMAP04, and CROPMAP08 input files. If CTM_WB_DUST is set to N, this setting will be ignored. Set to Y to turn on; comment out variable or set to N to turn off.
-   `CTM_WBDUST_BELD [default: BELD3]` 
    Landuse database for identifying dust source regions;  ignore if `CTM_WB_DUST = N`
    - `BELD3`  
    Use BELD3 landuse data
    - `BELD4`
    Use BELD4 landuse data
-   `CTM_LTNG_NO [default: Y]`  
    Setting to activate lightning NO emissions. Setting this variable to Y requires additional variables to define the configuration of the lightning NO emissions calculation. See the settings for `LTNGNO`, `LTNGPARAMS`, `NLDN_STRIKES`, and `LTNGDIAG` below. Set to Y to turn on; comment out variable or set to N to turn off.
-   `CTM_WVEL [default: Y]`  
    Setting to output the CCTM-calculated vertical velocities to the CONC file. Set to Y to turn on; comment out variable or set to N to turn off.
-   `KZMIN [default: Y]`  
    If KZMIN is set to Y, CCTM will read the urban land use fraction variable (PURB) from the GRID_CRO_2D meteorology file and use this information to determine the minimum eddy diffusivity in each grid cell. In CMAQv5, grid cells that are predominantly urban use a KZMIN value of 1.0 m<sup>2</sup>/s and non-urban cells use a value of 0.01 m<sup>2</sup>/s. If this variable is set to N, the PURB variable will not be used and a uniform KZMIN value of 1.0 m<sup>2</sup>/s will be used throughout the modeling domain.
-   `CTM_ILDEPV [default: Y]`  
    Calculate in-line deposition velocities. Comment out variable or set to Y to turn on; set to N to turn off.
-   `CTM_MOSAIC [default N]`  
    Calculate land use specific deposition velocities and fluxes.
-   `CTM_FST [default: N]`  
    Use MOSAIC method to get land-use specific stomatal flux.
-   `CTM_ABFLUX [default: Y]`  
    Activate fertilizer ammonia bidirectional flux for in-line emissions and deposition velocities. If CTM_ILDEPV is set to N this variable is ignored. Setting this variable to Y requires four additional input files that include gridded fractional crop distributions (B4LU_file), soil properties (E2C_Soilfile), fertilizer conditions (E2C_Fertfile), and an agricultural soil initial conditions file (INIT_MEDC_1). Activation of this setting will produce additional variables in the output dry deposition file. See [Chapter 8](CMAQ_OGD_ch08_input_files.md#Table8-1) for a description of the required input files. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_HGBIDI [default: N]`  
    Activate mercury bidirectional flux for in-line emissions and deposition velocities. If CTM_ILDEPV is set to N this variable is ignored. Activation of this setting will produce additional variables in the output dry deposition file. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_SFC_HONO [default: Y]`  
    Calculate surface HONO interactions. If CTM_ILDEPV is set to N this variable is ignored. Comment out or set to Y to turn on; set to N to turn off.
-   `CTM_GRAV_SETL [default Y]`  
    Activate gravitational sedimentation for aerosols. Comment out or set to Y to turn on; set to N to turn off.
-   `CTM_BIOGEMIS [default: Y]`  
    Calculate biogenic emissions. Comment out or set to Y to turn on; set to N to turn off.  If this option is activated, several additional variables must be set (see the In-line biogenic emissions configuration settings)     
-   `CTM_PT3DEMIS [default: Y]`  
    Calculate plume rise for elevated point sources. Set to Y to turn on; comment out or set N to turn off. If this option is activated several additional variables must be set (see the Inline emissions configuration settings) following variables must be set.
-   `CTM_ZERO_PCSOA [default: N]`
     Turn off the emissions of the VOC precursor to pcSOA. The CMAQ dev team recommends leaving pcSOA mass in the model for production runs.

### Process analysis options

-   `CTM_PROCAN [default: N]`  
    Activate process analysis in the CCTM. Set this to Y and use $CMAQ_DATA/pacp/pacp.inp to configure the integrated process rate and integrated reaction rate settings for the CCTM.  Additional process analysis output files will be created when this setting is activated.
-   `PA_BCOL_ECOL [default: None]`  
    Modeling grid domain column range for the process analysis calculations. Set to the two digits representing the beginning and ending column number bounding the process analysis domain.
-   `PA_BROW_EROW [default: None]`  
    Modeling grid domain row range for the process analysis calculations. Set to the two digits representing the beginning and ending row number bounding the process analysis domain.
-   `PA_BLEV_ELEV [default: None]`  
    Modeling grid domain layer range for the process analysis calculations. Set to the two digits representing the bottom and top layer numbers bounding the process analysis domain.

### I/O Controls

-   `IOAPI_LOG_WRITE [default: Y]`  
    Set to T to turn on excess WRITE3 logging by the I/O API.
-   `FL_ERR_STOP [default: N]`  
    Set to T to configure the program to exit if inconsistent headers are found in the input files.
-   `PROMPTFLAG [default: N]`  
    Turn on I/O-API PROMPTFILE interactive mode. Set to T to require interactive prompts for different I/O API operations.  
-   `IOAPI_OFFSET_64 [default: N]`  
    I/O API setting for large time step records. If your output time step is going to produce data that are >2GB per time step, then this needs to be set to YES.

### Aerosol Diagnostics Controls

-   `CTM_AVISDIAG [default: N]`  
    Output visibility diagnostics file. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_PMDIAG [default: N]`  
    Output aerosol diagnostics and properties file. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_APMDIAG [default: N]`  
    Output hourly average aerosol diagnostics and properties file. Set to Y to turn on; comment out or set to N to turn off.
-   `APMDIAG_BLEV_ELEV [default: None]`  
    Modeling grid domain layer range for the hourly average aerosol diagnostics and properties file. Set to the two digits representing the bottom and top layer numbers to bound the output domain.

### Diagnostic Output Flags

-   `CTM_CKSUM [default: Y]`  
    Write science processes summaries to the standard output. Impacts run speed and log file output size. Comment out or set to Y to turn on; set to N to turn off.
-   `CLD_DIAG [default: N]`  
    Output an hourly wet deposition diagnostic file (CTM_WET_DEP_2) that includes convective wet deposition estimates. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_PHOTDIAG [default: N]`  
    Output in-line photolysis rates and associated data to diagnostic netCDF output files. The file CTM_RJ_1 contains gridded photolysis rates for O3 (JO3O1D) and NO2 (JNO2) that include both clear-sky and cloud effects, total downward irradiance at the surface (ETOT_SFC_W), aerosol optical depth (TAU_AERO_W), total optical depth (TAU_TOT_W), optical depth of ozone above the model domain (TAUO3_TOP_W), Rayleigh optical depth above the model domain (TAU_RAY_W), and surface albedo (ALBEDO_W). The file CTM_RJ_2 contains gridded photolysis rates for all other photolysis reactions in the selected chemical mechanism. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_SSEMDIAG [default: N]`  
    Output the calculated sea salt emissions to a diagnostic netCDF output file (CTM_SSEMIS_1). Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_DUSTEM_DIAG [default: N]`  
    Output the in-line dust emissions to a diagnostic netCDF output file (CTM_DUST_EMIS_1). The diagnostic file includes not only the total dust emissions, but also dust emissions by land use category and dust model parameters, such as gridded erodible land use fractions. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_DEPV_FILE [default: N]`  
    Output an hourly diagnostic file (CTM_DEPV_DIAG) for the in-line deposition velocity calculations. If CTM_ILDEPV is set to N this variable is ignored. Set to Y to turn on; comment out or set to N to turn off.
-   `VDIFF_DIAG_FILE [default: N]`  
    Output a diffusion and aero gravitational sedimentation diagnostic file. Set to Y to turn on; comment out or set to N to turn off.
-    `CTM_AOD [default N]`  
    Output an aerosol optical depth (AOD) calculation diagnostics file. Set to Y to turn on; comment out or set to N to turn off.
-   `DISP [default: keep]`  
    Controls the maintenance of existing log files.
    -   `delete` delete output log if it already exists
    -   `keep` abort simulation if output log exists

### Inline emissions configuration

-   `NPTGRPS [default: 1]`  
    The number of input point-source elevated emission sector file groups. A maximum of 9 sectors is allowed.

-   `STK_GRPS_## `  
    Directory path and file name of the stack groups file for sector ##, where ## = 01, 02,…,NPTGRPS. Each ## refers to one of the plume rise point-source sectors.

-   `STK_EMIS_##`  
    Directory path and file name of the point emissions file for sector ##, where ## = 01, 02,…,NPTGRPS. Each ## refers to the one of the plume rise point-source sectors.

-   `LAYP_STDATE [HHMMSS]`  
    Start date for calculating elevated-point-source emissions.

-   `LAYP_STTIME [HHMMSS]`   
    Start time for calculating elevated-point-source emissions.

-   `LAYP_NSTEPS [HHHHHH]`  
    Number of time steps for calculating elevated-point-source emissions.

-   `CTM_EMLAYS [default: max no of model layers]`  
    Number of emissions layers for calculating elevated-point-source emissions. If not set (commented out), the maximum number of model layers will be used.

-   `PT3DDIAG [default: N]`  
    Output the in-line 3-D point-source emissions to a diagnostic netCDF output file (CTM_PT3D_DIAG). Set to Y to turn on; comment out or set to N to turn off.

-   `PT3DFRAC [default: N]`  
    Output the in-line 3-D point-source layer fractions to a diagnostic netCDF output file (PLAY_SRCID_NAME). Set to Y to turn on; comment out or set to N to turn off.

-   `REP_LAYER_MIN [default: -1]`  
    Minimum layer number for reporting plume rise values to the plume rise diagnostics file. Set to `-1` or comment out to report all layers.

### Lightning NOx configuration

[CMAQ Lightning NOx Module Documentation](../../CCTM/docs/Release_Notes/Lightning_NOx.md)

-   `LTNGNO [default:InLine]`  
    Setting to define whether the lightning emissions calculation will be in-line or off-line. This variable can be set to a gridded netCDF file of lightning NO emissions to use emissions calculated with a preprocessor outside of CCTM. Setting this variable to “inline” activates the in-line emissions calculation in CCTM and requires the LTNGPARMS variable (see below) to define the configuration of the in-line emissions.

-   `USE_NLDN [default: Y]`  
    Use hourly NLDN strikes file to compute inline lighing NO emissions. Activating this setting requires the NLDN_STRIKES input file.  Comment out or set to Y to turn on; set to N to turn off.

-   `LTNGPARAMS [default: Y]`  
    Use the lightning parameters configuration file to compute inline lightning NO emissions. When the variable `LTNGNO` is set to `inline`, this setting is used to define how the in-line emissions will be calculated. Commenting out this variable or setting it to Y will compute lightning NO from input hourly flash count observations. Setting this variable to N will compute lightning NO strictly from convective precipitation rates in the input meteorology data. When this variable is set to Y, an additional input lightning parameter file (LTNGPARMS_FILE) will need to be available that includes intercloud to cloud-to-ground flash ratios, scaling factors for calculating flashes using the convective precipitation rate, and the moles of NO per flash.

-  `NLDN_STRIKES [default: None]`  
    Hourly NLDN lightning strike netCDF FILE. Required when `LTNGNO` is set to `Inline` and `USE_NLDN` is set to `Y`; otherwise ignore this setting.

-  `LOG_START [default: 0.9]`  
    Convective precipitation (RC) value to transition the lightning NO emissions calculation from linear to log linear.

-   `LTNGDIAG [default: N]`  
    Output a lightning NO emissions diagnostics file. Set to `Y` to turn on; comment out or set to `N` to turn off.

-  `LTNGPARMS_FILE [default: None]`  
    Lightning parameters output netCDF file; ignore if `LTNGPARAMS = N`

-  `LTNGOUT [default: None]`  
    Lightning diagnostics output netCDF file; ignore if `LTNGDIAG = N`

### In-line biogenic emissions configuration

-   `GSPRO [default: None]`  
    Directory path and file name for input ASCII speciation profiles.

-   `B3GRD [default: None]`  
    Grid-normalized biogenic emissions input netCDF file.

-   `BIOG_SPRO [default: None]`  
    Profile ID for speciating biogenic VOCs. This profile ID must be present in the GSPRO file.

-   `BIOSW_YN [default: Y]`  
    Use the frost dates switch file to determine whether to use winter or summer biogenic emissions. Comment out or set to Y to turn on; set to N to turn off.

-   `BIOSEASON [default: None]`  
    File name for the frost dates switch input netCDF file.

-   `SUMMER_YN [default: Y]`  
    Toggle for summer season normalized biogenic emissions. This variable is ignored if BIOSW_YN is set to Y. Comment out or set to Y to select summer season biogenic emissions factors; set to N to turn off.

-   `PX_VERSION [default: Y]`  
    Setting to indicate whether the Pleim-Xiu land-surface model was used for the input meteorology. If this setting is set to Y the input meteorology data must include soil moisture (SOILM), soil temperature (SOILT), and soil type (ISLTYP) variables for use in the calculation of soil NO emissions.

-   `INITIAL_RUN [default: N]`  
    Set to Y if this is the first time that biogenic NO soil emissions will be calculated. If there is a previously created file, set to N.

-   `SOILINP [default: None]`  
    Directory path and file name of biogenic NO soil emissions file. If INITIAL_RUN is set to N or F, the soil NO emissions file from the previous day’s simulation will be a required input file.

-   `B3GTS_DIAG [default: N]`  
    Write the inline biogenic emissions (mass units) to a diagnostic netCDF output file (B3GTS_S). Set to Y to turn on; comment out or set to N to turn off.

-   `B3GTS_S`  
    Diagnostic output netCDF file of biogenic emissions. This variable is ignored if B3GTS_DIAG is set to N.


### Windblown dust emissions configuration

[CMAQ Windblown Dust Module Documentation](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/Windblown_Dust_Emis.md)
-   `DUST_LU_1`  
    Input BELD "A" landuse netCDF file gridded to the modeling domain. Used if `CTM_WBDUST_BELD` is set to BELD3.

-   `DUST_LU_2`  
    Input BELD "TOT" landuse netCDF file gridded to the modeling domain. Used if `CTM_WBDUST_BELD` is set to BELD3.

-   `MODIS_FPAR`  
    Input MODIS FPAR time-varying vegetation netCDF file gridded to the modeling domain.

-   `BELD4_LU`  
    Input BELD4 landuse netCDF file gridded to the modeling domain. Used if `CTM_WBDUST_BELD` is set to BELD4.

-   `CROPMAP01`  
    Input beginning planting dates netCDF file gridded to the modeling domain.

-   `CROPMAP04`  
    Input end planting dates netCDF file gridded to the modeling domain.

-   `CROPMAP08`  
    Input end harvesting dates netCDF file gridded to the modeling domain.
    
    

# Appendix B: Emissions Input and Control
[Jump to DESID Tutorial](../Tutorials/CMAQ_Emissions.md) for step by step instructions on performing some basic manipulation of emission streams.
[Jump to DESID overview](CMAQ_UG_ch04_model_formulation.md) in Chapter 4 of this User's Guide. 

## B.1 Emissions Control with the Detailed Emissions Scaling, Isolation and Diagnostics Module (DESID)

In addition to the options available in the RunScript, CMAQ now reads a dedicated namelist in order to apply comprehensive rules for reading and scaling emissions. The namelist, called the **Emission Control Namelist** is named "EmissCtrl.nml" by default and a separate version exists for every mechanism because these namelists are preloaded with likely rules linking emissions of important CMAQ primary species to their typical surrogate names as output by SMOKE. By default, this namelist is stored in each chemical mechanism folder (e.g. MECHS/cb6r3_ae7_aq), and is copied into the user's build directory when bldit_cctm.csh is executed. If the user modifies the name or location of this namelist, then the following command in the RunScript should be updated as well:
```
setenv EMISSCTRL_NML ${BLD}/EmissCtrl.nml
```

The Detailed Emissions Speciation, Isolation and Diagnostics (DESID) module included with CMAQv5.3 provides comprehensive customization and transparency of emissions manipulation to the user. The customization of emissions is accomplished via the Emission Control Namelist, which contains four sections of variables that modify the behavior of the emissions module. These include ***General Specs***, ***Emission Scaling Rules***, ***Size Distributions***, and ***Regions Registry***

## B.2 ***General Specs***
These variables modify or constrain the effects of other sections of the namelist. The "Guard_XXX" options allow the user to protect specific streams from being modified by scaling rules (explained in section 4.3.1.2) with the "ALL" keyword in the stream field. For example, the "Guard_BiogenicVOC" option instructs the model not to scale biogenic VOC emissions from the online BEIS module, even if a rule indicates that "ALL" streams are to be scaled. The other "Guard_XXX" options achieve the same effect for other online emissions sources like wind-blown dust, sea spray, marine gas, and lightning NO.

## B.3 ***Emission Scaling Rules***
With the rules present in this section, the user is able to exert sophisticated, precise control over the scaling applied to emissions from specific streams, in specific geographic areas, and/or for specific compounds. The set of rules used by CMAQ to interpret emissions shall be provided in one array called EM_NML. It is necessary that every field (i.e. column) be populated for every rule. The fields are given and defined here and in the comment section of the Emission Control Namelist:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
```
- 'Region Label' - Apply scaling for specific regions of the domain. Set this field to "EVERYWHERE" to apply the rule to the entire domain.
- 'Stream Label' - Short Name from Run Script (ie. the value of GR_EMIS_01_LAB or STK_EMIS_01_LAB). There are a few reserved names that apply to inline emissions streams. These are:
  - BIOG - Biogenic VOC emissions
  - MGEM - Marine Gas Emissions
  - LTNG - Lightning NO Emissions
  - WBDUST - Wind-Blown Dust Emissions
  - SeaSpray - Sea Spray Aerosol Emissions  

  Set this field to 'ALL' to apply the rule to all emission streams.  
- 'Emission Surrogate' - The character string identifying the surrogate on the emission file or in the inline calculation that the CMAQ species should be mapped to. Usually this name is the same as the CMAQ species for convenience. For aerosols, it's usually slightly different (e.g ANO3 vs. PNO3). Set this field to 'ALL' to apply the rule to all emission surogates.  
- 'CMAQ-Species' - Internal Species Name. Set this field to 'ALL' to apply the rule to all CMAQ internal species.
- 'Phase/Mode' - If the CMAQ-Species is a Gas, this field should equal 'Gas'. If the CMAQ-Species is an aerosol, this field should indicate one of the possible emission aerosol modes. Every stream by default is given a 'COARSE' and 'FINE' mode. The user may refer to these, or define others above and refer to them as well. This level of specificity is needed so that aerosol number and surface area are calculated correctly, and so that any unit conversions between gases and aerosols can be handled correctly.  
- 'Scale Factor' - Adjustment factor to be applied to the mapping
- 'Basis' - Specifies whether the scaling option should directly apply, or if the operation should conserve moles or mass when performing scaling operations. CMAQ has a lookup table of molecular weights for known emission surrogate species and can use these to translate molar and mass emission rates from the input file to the CMAQ species. CMAQ determines the units of the emission surrogate species by reading the file header (i.e. it is important the units are accurate. Options for input are:
  - 'MASS' - Conserve Mass. For example, if emissions of an aerosol are to be scaled to emissions of a gas surrogate, it is common to want to conserve mass.
  - 'MOLE' - Conserve Moles. For example, if emissions of a gas-phase species are to be scaled to another gas, it is sometimes desired to conserve moles since gas emissions are provided on a mole basis.
  - 'UNIT' - Ignore molecular weight conversions and apply emission rate directly regardless of units.
- 'Operation' - Specifies the kind of rule to be carried out. Options are:
  - 'a' - add the rule to existing instructions. This operation should be used for new entries, too.
  - 'm' - find existing scaling instructions matching this rule's features (ie. species, streams, etc) and multiply them by the factor in this particular rule.
  - 'o' - find existing scaling instructions matching this rule and overwrite them.  

### B.3.1 Default Rules
The Emission Control Namelists provided with the CMAQ repo have default rules included that correspond to each chemical mechanism. Here is an example default rule that links NO in CMAQ to NO from every emission stream in every model grid cell with a scale factor of 1.0.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'NO'     ,'NO'          ,'GAS' ,1.0  ,'UNIT','a',
```
Many rules are needed here in order to properly link every emitted pollutant to a CMAQ species. Rules are needed for gas- and aerosol-phase species. Additional rules also exist for online aerosol modules like wind-blown dust and sea spray because the names of aerosol surrogates from these modules are different than those typically used for SMOKE output. For example, fine-mode aerosol sulfate is commonly called PSO4 in SMOKE, but is PMFINE_SO4 from dust and sea spray.

### B.3.2 Modifying Default rules
The user can modify any default rule in order to change the scale factor applied. Alternatively, the user can add new rules after the default rules in order to customize the emissions. Typical modifications may include multiplying the emissions of a particular species from a particular stream by a factor of 2, zeroing out emissions of all species from a particular stream, etc. Please see the tutorial on [Prescribing Emissions with DESID](../Tutorials/CMAQ_Emissions.md) for specific examples of modifications and the syntax used to invoke them.

#### B.3.2.1 Supporting the Volatility Basis Set
The *Volatility Basis Set* for treating the semivolatile partitioning of primary organic emissions is an example of a model feature that is well-supported by DESID. The approach involves distributing the emissions of total primary organic aerosol (carbon and noncarbon mass, or POC and PNCOM) among a series of aerosol and gas species of varying volatility.

If the user would like to invoke the nonvolatile partitioning assumption, it can be accomplished by directing all POC and PNCOM emissions to the POC and PNCOM species in CMAQ.
```
  ! --> Nonvolatile POA
  'EVERYWHERE', 'ALL'         ,'POC'    ,'APOC'        ,'FINE',1.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'APNCOM'      ,'FINE',1.   ,'MASS','a',
```
If the user would like to apply the default volatility distribution to the POA emissions, it can be accomplished with the following default rules.
```
  ! --> Semivolatile POA
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VLVPO1'      ,'GAS' ,0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VLVPO1'      ,'GAS' ,0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VSVPO1'      ,'GAS' ,0.045,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VSVPO1'      ,'GAS' ,0.045,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VSVPO2'      ,'GAS' ,0.14 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VSVPO2'      ,'GAS' ,0.14 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VSVPO3'      ,'GAS' ,0.18 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VSVPO3'      ,'GAS' ,0.18 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VIVPO1'      ,'GAS' ,0.50 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VIVPO1'      ,'GAS' ,0.50 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'ALVPO1'      ,'FINE',0.09 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'ALVPO1'      ,'FINE',0.09 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'ASVPO1'      ,'FINE',0.045,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'ASVPO1'      ,'FINE',0.045,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'ASVPO2'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'ASVPO2'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'ASVPO3'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'ASVPO3'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'AIVPO1'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'AIVPO1'      ,'FINE',0.   ,'MASS','a',
```
Notice that for each species (e.g. ALVPO1) a rule is needed to link the species to the emissions of POC and another rule is needed to add PNCOM. This is because both carbon and noncarbon mass are part of the emissions of every semivolatile species. To change the volatility distribution for all streams, the user may modify the scaling factors in the default rules above. To introduce specialized volatility distributions for specific stream (e.g. residential wood burning, forest fires, diesel vehicles, etc), rules may be added whiech explicitly identify a strem in the "Stream Label" field.

## B.4 Applying Masks for Spatial Dependence
Gridded masks are used to apply rules to specific areas of the domain. For example, the following rule:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'KENTUCKY'    , 'All'         ,'All'    ,'All'         ,'All' ,1.50 ,'UNIT','m',
```
will scale emissions of all species from all streams by +50% but only in grid cells in the state of Kentucky. A gridded field of real numbers from 0.0 to 1.0 is needed to accomplish this task, with 0.0 outside of the region of interest and 1.0 completely inside the region. Grid cells on the border of a region would then have some fraction of their emissions, corresponding to the real decimal number in the gridded mask, modified according to the scaling rule.

These masks are provided to CMAQ by at least one offline gridded file, which are identified in the RunScript using environment variables (more later). The *RegionsRegistry* section of the Emission Control Namelist maps each "Region Label" to specific variables on specific files. Here is the *RegionsRegistry* section in the default namelist:
```
&RegionsRegistry
 RGN_NML  =   
 !          | Region Label   | File_Label  | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'        ,'N/A',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
/
```
As indicated, the Region Label "EVERYWHERE" is active by default and returns a mask that operates uniformly across the entire domain. The "File_Label" field identifies the environment variable in the RunScript that stores the location and name of the file containing the mask. The user may modify this to any name they wish as long as it is consistent with the variable name on the RunScript. The "Variable on File" field identifies the variable on the input file that stores gridded field to be used for this region. Examples are provided for two cases.

In the first case, a region with label "WATER" is defined and referenced to the variable "OPEN", which is short for *open water*. Using this "WATER" region will apply a scaling rule only for open water grid cells and fractionally along coastlines. The second example provides a shortcut for files with many variables that are all desired (e.g. states of the Unites States). Rather than listing out all variables on the file and explicitly linking them to "Region Labels", the user can invoke the "All" keyword and all variables will be read and stored with "Region Labels" that equal the names of the variables on the file.

## B.5 Aerosol Size Distributions
The treatment of aerosol size distributions in CMAQv5.3 has been updated to be more consistent with the way particle sizes and modes are treated by the National Emission Inventory and in emissions processing tools like SMOKE, MOVES, SPECIATE, and Speciation Tool. Specifically, in these tools, aerosol emissions are typically parameterized into two main modes, Fine and Coarse. Although the size distribution parameters (i.e. total number, diameter, standard deviation, etc.) for these modes will vary among emission sources, previous versions of CMAQ assumed that all primary fine particles had the same size distribution upon emission. Coarse-mode particles were assumed to exhibit a larger diameter but were also uniform across all sources (excluding wind-blown dust and sea spray).

In CMAQv5.3, users link particle emission surrogates to CMAQ particle species via the Emission Control Namelist. Examples of default mapping rules can be found in any of the Emission Control Namelists in the CMAQ repository. The three lines below assign emissions for all streams for particulate-phase sulfate, ammoium, and nitrate.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'ALL'         ,'PSO4'   ,'ASO4'        ,'FINE',1.0   ,'UNIT','a',
'EVERYWHERE'  , 'ALL'         ,'PNH4'   ,'ANH4'        ,'FINE',1.0   ,'UNIT','a',
'EVERYWHERE'  , 'ALL'         ,'PNO3'   ,'ANO3'        ,'FINE',1.0   ,'UNIT','a',
```
The CMAQ-Species field should be popoluated with bulk chemical names (e.g. ASO4, AEC, AK, ACA, etc). In other words, the 'i','j', or 'k' which usually designates the mode of the aerosol species name should be omitted. A list of the valid aerosol bulknames exists in the source file "AERO_DATA.F" in the array named "aerolist". The user should also identify the aerosol mode to be populated using the "Phase/Mode" field. In the example above, all of the rules identify the "FINE" mode as the desitination mode. CMAQ uses this value to look up the size distribution parameters (diameter and standard deviation) to apply for this particular emission.

Aerosol mode keywords are linked to reference mode labels in the SizeDistributions section of the Emission Control Namelist. These assignments can be made for all streams at once, as demonstrated by the first two default entries initializing the 'FINE' and 'COARSE' modes, or they can be made on a stream-by-stream basis as shown below for Wind-Blown Dust and Sea Spray aerosol.
```
&SizeDistributions
 SD_NML    =
 !         | Stream Label   | Mode Keyword | Ref. Mode
 !<Default>  'ALL'          ,'FINE'        ,'FINE_REF',
 !<Default>  'ALL'          ,'COARSE'      ,'COARSE_REF',
             'WBDUST'       ,'FINE'        ,'FINE_WBDUST',
             'WBDUST'       ,'COARSE'      ,'COARSE_WBDUST',
             'SEASPRAY'     ,'FINE'        ,'FINE_SEASPRAY',
             'SEASPRAY'     ,'COARSE'      ,'COARSE_SEASPRAY',
 !<Example>  'AIRCRAFT'     ,'FINE'        ,'AIR_FINE',   !To use these examples, you
 !<Example>  'AIRCRAFT'     ,'COARSE'      ,'AIR_COARSE', ! must add entries for AIR_FINE
                                                          ! and AIR_COARSE to the data structure
                                                          ! em_aero_ref in AERO_DATA.
```
The 'Ref. Mode Labels' are used to lookup size distrubution parameters in AERO_DATA.F. The following reference modes are defined in the public repo:
```
TYPE em_aero
    Character( 20 ) :: name
    Real            :: split( n_mode )  ! dimensionless
    Real            :: dgvem( n_mode )  ! meters
    Real            :: sgem ( n_mode )  ! dimensionless
END TYPE em_aero
INTEGER, PARAMETER  :: n_em_aero_ref = 9

TYPE( em_aero ), Parameter :: em_aero_ref( n_em_aero_ref ) = (/

!              ----Name----     -----Split-----    ---Geo. Mean Diameter---   ---Stnd Dev.---
& em_aero('FINE_REF       ',(/0.1,0.9,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Default Accum and Aitken Mode
& em_aero('ACC_REF        ',(/0.0,1.0,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Just Accumulation Mode
& em_aero('COARSE_REF     ',(/0.0,0.0,1.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Just Coarse Mode
& em_aero('UNITY_REF      ',(/1.0,1.0,1.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Used for online sectors (e.g. SeaSpray)
& em_aero('ZERO_REF       ',(/0.0,0.0,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Zero out the emissions
& em_aero('FINE_WBDUST    ',(/0.0,1.0,0.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Default Fine Wind-Blown Dust Parameterization
& em_aero('COARSE_WBDUST  ',(/0.0,0.0,1.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Default Coarse Wind-Blown Dust Param.
& em_aero('FINE_SEASPRAY  ',(/0.0,1.0,0.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Fine Sea Spray Parameterization is Dynamic.
& em_aero('COARSE_SEASPRAY',(/0.0,0.0,1.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/))  ! Coarse Sea Spray Parameterization is Dynamic.
                                                                                           !  The values here are not actually used but
                                                                                           !  are replaced in SSEMIS when FACNUM and FACSRF
                                                                                           !  are calculated online.
& /)
````
Users can add as many new size distributions as they want, as long as they increment the variable n_em_aero_ref to always equal the number of size distirbutions in the lookup array (em_aero_ref).

CMAQ will use the size distribution reference value linked to each rule via the phase/mode keyword to calculate the fraction of each aerosol primary emission that should go into the 'i', 'j', and 'k' modes in the internal aerosol module. At first, it may seem that the lining step between phase/mode keywords and reference mode labels is unnecesary, but it serves an important function. As stated earlier, it is common that modes of similar size from a variety of sources will be referred to by common names like 'FINE' and 'COARSE', even though the size distribution parameters may differ considerably. With the linking step provided in the SizeDistributions section, parameters for several streams can be specified individually, but all be labeled 'FINE' and applied with one rule in the EmissionsScalingRules section.

In the example above, fine mode Wind-Blown Dust are linked to 'FINE_WBDUST', sea spray aerosols are linked to 'FINE_SEASPRAY' and all other sources are linked to 'FINE_REF'. Thus different size distributions will be calculated for each of these streams. However, if the user wants to scale the mass of all fine mode aerosol by a factor of 2, the following emission rule is valid:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'ALL'         ,'ALL'    ,'ALL'         ,'FINE',1.0   ,'UNIT','m',
```

## B.6 Additional DESID Features
### B.6.1 Diagnostic Log Output
Diagnostic output is an important feature of the new emissions module, DESID. Because the impact of emissions is so critical for CMAQ predictions and because the features available for scaling emissions are now quite complex, a comprehensive text-based output has been added to the CMAQ logfiles to enhance transparency.

The logfiles now provide several lists of information to help protect users from mistakes like inconsistent naming between emissions and CMAQ speciation. First, CMAQ reports for each stream the number and names of all of the surrogate species that were not used. Second, prints the names of surrogates that the user told it to look for but that it could not find on any of the emission streams. If the environment variable:
```
setenv CTM_EMISCHK Y         #> Abort CMAQ if missing surrogates from emissions Input files
```
is set to 'Y' or 'True', then the model will abort if it cannot find any individual surrogate. If the variable is set to 'N' or 'False' then CMAQ will print a warning and proceed.

Finally, CMAQ loops through stream and outputs the size distribution modes available for each stream and the full list of every emission instructions applied to each stream. These are ordered by CMAQ species (with 'i', 'j', and 'k' modes listed separately) and surrogate species name so that a full understanding of the scaling rules applied to each CMAQ species' emissions can be grasped quickly. Columns are printed for the applicable region of the grid, the phase/mode applied, the input scale factor, the scaling basis, the operation, and the final scale factor applied taking into account any molecular weight conversions, if needed, and size distribution fractions.

### B.6.2 Diagnostic Gridded Output
Many complex scaling procedures are now possible with DESID. Users are advised to confirm that the emissions are scaled the way they have intended. One tool to help this step is the Gridded Diagnostic Output. This is enabled on a stream-by-stream basis in the CMAQ RunScript with the following options:
```
# Gridded Emissions Diagnostic files
  setenv GR_EMIS_DIAG_001 TRUE
  setenv GR_EMIS_DIAG_002 2D

# Stack emissions diagnostic files
  setenv STK_EMIS_DIAG_001 2DSUM
  setenv STK_EMIS_DIAG_002 2DSUM
  setenv STK_EMIS_DIAG_003 FALSE
  setenv STK_EMIS_DIAG_004 2DSUM
  setenv STK_EMIS_DIAG_005 2DSUM
```
The lines above set the behavior of the gridded diagnostic output for gridded and inline emission streams. The values available for each stream are 'TRUE', 'FALSE', '2D', '2DSUM', and '3D'. The '2D' option prints just the surface layer of emissions for a particular stream. The '3D' option prints all layers populated by that stream. The '2DSUM' option prints one 2D field, but it equals the column of sum of emissions throughout the gridded model domain. The 'TRUE' option equates to '2D'. The user can also set the diagnostic behavior of online streams using the following variables:
```
setenv BIOG_EMIS_DIAG TRUE
setenv MG_EMIS_DIAG TRUE
setenv LTNG_EMIS_DIAG TRUE
setenv DUST_EMIS_DIAG TRUE
setenv SEASPRAY_EMIS_DIAG TRUE
```
In order to change the default value of all emission streams modify the "EMIS_DIAG" variable:
```
setenv EMIS_DIAG TRUE
```
The emission rates printed to the diagnostic files reflect all of the scaling rules applied and are written just before the emissions are added to the CMAQ transport module. Because the model interpolates in time, it is very likley that the rates written to the diagnostic file will not correspond in time to the rates from the input files. In most cases, the rates will be one-half time step before the top of the hour, the time point of the emission inputs. For this reason, it is not entirely helpful for users to compare the scaled emissions directly to the rates on the input files. However, comparing them qualitatively can be helpful.

### B.6.3 Date Override
For offline emissions, CMAQ tries to check for errors in misapplying dates by comparing the date of the offline emisisons file to the mode simulation date. However, there are many instances where using a repeating emission day for multiple simulation days can be helpful, and save disk space. Users can direct CMAQ to skip the date error check using the following environment variable in the CMAQ RunScript:
```
# Gridded Emission Date Override
setenv GR_EM_DTOVRD_001 F

# Allow CMAQ to Use Point Source files with dates that do not
 # match the internal model date
 setenv STK_EM_DTOVRD_001 T
 setenv STK_EM_DTOVRD_002 T
 setenv STK_EM_DTOVRD_003 T
 setenv STK_EM_DTOVRD_004 T
 setenv STK_EM_DTOVRD_005 T
```
To set the value of these variables globally, the user may set master switch for date overrides:
```
setenv EMIS_DATE_OVRD N      #> Master switch for allowing CMAQ to use the date from each Emission file
                             #>   rather than checking the emissions date against the internal model date.
                             #>   [options: T | F or Y | N]. If false (F/N), then the date from CMAQ's internal
                             #>   time will be used and an error check will be performed (recommended). Users
                             #>   may switch the behavior for individual emission files below using the variables:
                             #>       GR_EM_DTOVRD_## | STK_EM_DTOVRD_##
```


