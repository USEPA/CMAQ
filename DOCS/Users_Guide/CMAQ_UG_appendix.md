<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch13_support.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_glossary.md)

<!-- END COMMENT -->

* * *

## Appendix A: Model options
Below is text from the OGD.  Desired format for new User's Guide is:

Alphabetical list of all options (column 1: variable name, column 2: what type of option it is, e.g. run time, column 3: what it does, column 4: what section describes it in the main text)


#### Compilation Configuration Variables

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
    Chemistry mechanism for gas, aerosol, and aqueous chemistry. See the [CMAQ Mechanism Definitions Table(https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/CMAQ_OGD_appendix_A.md) for a listing of the mechanism choices that are available in CMAQv5.2.
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

#### Execution Configuration Variables

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

##### MPI Configuration
-   `NPCOL_NPROW [default: 1 1]`  
    The numbers of columns and rows for decomposing the modeling domain in an MPI configuration. The product of this pair of numbers must equal the total number of processors allocated to the CCTM simulation. For serial or single-processor MPI runs set to `1 1`. For multi-processor simulations, the number of columns (i.e, the first number in the pair) should be greater than or equal to the number of rows.  For example, for an 8 processor MPI simulation, set to `4 2`
-   `NPROCS [default: 1]`  
    Number of processors to allocate for the CCTM simulation; equal to the product of NPCOL x NPROW. For serial or single-processor MPI runs set to `1`, otherwise set to the product of the two numbers used in NPCOL_NPROW.

##### Vertical extent
-    `NZ [default: 35]` 
      Set the number of vertical layers. 

##### Timestep Configuration

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

##### CCTM Configuration Options

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

##### Synchronization Time Step and Tolerance Options

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

##### Science Options

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

##### Process analysis options

-   `CTM_PROCAN [default: N]`  
    Activate process analysis in the CCTM. Set this to Y and use $CMAQ_DATA/pacp/pacp.inp to configure the integrated process rate and integrated reaction rate settings for the CCTM.  Additional process analysis output files will be created when this setting is activated.
-   `PA_BCOL_ECOL [default: None]`  
    Modeling grid domain column range for the process analysis calculations. Set to the two digits representing the beginning and ending column number bounding the process analysis domain.
-   `PA_BROW_EROW [default: None]`  
    Modeling grid domain row range for the process analysis calculations. Set to the two digits representing the beginning and ending row number bounding the process analysis domain.
-   `PA_BLEV_ELEV [default: None]`  
    Modeling grid domain layer range for the process analysis calculations. Set to the two digits representing the bottom and top layer numbers bounding the process analysis domain.

##### I/O Controls

-   `IOAPI_LOG_WRITE [default: Y]`  
    Set to T to turn on excess WRITE3 logging by the I/O API.
-   `FL_ERR_STOP [default: N]`  
    Set to T to configure the program to exit if inconsistent headers are found in the input files.
-   `PROMPTFLAG [default: N]`  
    Turn on I/O-API PROMPTFILE interactive mode. Set to T to require interactive prompts for different I/O API operations.  
-   `IOAPI_OFFSET_64 [default: N]`  
    I/O API setting for large time step records. If your output time step is going to produce data that are >2GB per time step, then this needs to be set to YES.

##### Aerosol Diagnostics Controls

-   `CTM_AVISDIAG [default: N]`  
    Output visibility diagnostics file. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_PMDIAG [default: N]`  
    Output aerosol diagnostics and properties file. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_APMDIAG [default: N]`  
    Output hourly average aerosol diagnostics and properties file. Set to Y to turn on; comment out or set to N to turn off.
-   `APMDIAG_BLEV_ELEV [default: None]`  
    Modeling grid domain layer range for the hourly average aerosol diagnostics and properties file. Set to the two digits representing the bottom and top layer numbers to bound the output domain.

##### Diagnostic Output Flags

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

##### Inline emissions configuration

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

##### Lightning NOx configuration

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

##### In-line biogenic emissions configuration

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


##### Windblown dust emissions configuration


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

