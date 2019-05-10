<!-- BEGIN COMMENT -->

 [Home](README.md) - [Next Appendix >>](CMAQ_UG_appendixB_emissions_control.md)

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

-   `M3MODEL`<a id=M3MODEL></a>
    Automatically set by config_cmaq.csh; deprecated in CMAQv5.2

-   `compiler`<a id=compiler_config></a>
    Set the Fortran compiler type that you will use to compile CMAQ; choices are intel, pgi, or gcc

-   `compilerVrsn`<a id=compilerVrsn></a>
    (Optional) Set the Fortran compiler version number that you will use to compile CMAQ; if you employ this variable, it will be appended to the compiler type when naming build directories and executables

-   `IOAPI_MOD_DIR`<a id=IOAPI_MOD_DIR></a>
    Location of the I/O API modules installation on your Linux system

-   `IOAPI_INCL_DIR`<a id=IOAPI_INCL_DIR></a>
    Location of the I/O API include file installation on your Linux system

-   `IOAPI_LIB_DIR`<a id=IOAPI_LIB_DIR></a>
    Location of the I/O API library installation on your Linux system

-   `NETCDF_LIB_DIR`<a id=NETCDF_LIB_DIR></a>
    Location of the netCDF installation on your Linux system

-   `MPI_LIB_DIR`<a id=MPI_LIB_DIR></a>
    Location of the Message Passing Interface installation on your Linux system

-   `netcdf_lib`<a id=netcdf_lib></a>
    Name of the netCDF library on your system;  set to "-lnetcdf" for versions < 4.2.0, "-lnetcdff -lnetcdf" for version 4.2.0 and later

-   `ioapi_lib`<a id=ioapi_lib></a>
    Name of the I/O API libraryar on your system; set to "-lioapi"

-   `pnetcdf_lib`<a id=pnetcdf_lib></a>
    Name of the parallel netCDF library on your system; set to "-lpnetcdf"

-   `mpi_lib`<a id=mpi_lib></a>
    Name of the MPI library on your system; set to "-lmpich" for MVAPICH, "-lmpi" for OpenMPI

-   `myFC`<a id=myFC></a>
    Set to match the `FC` (Fortran compiler) you used to compile netCDF

-   `myCC`<a id=myCC></a>
    Set to match the `CC` (C compiler) you used to compile netCDF

-   `myFFLAGS`<a id=myFFLAGS></a>
    Fixed-format Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `myCFLAGS`<a id=myCFLAGS></a>
    C compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `myFRFLAGS`<a id=myFRFLAGS></a>
    Free form-format Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `MPI_INC`<a id=MPI_INC></a>
    Set to the path to your MPI library INCLUDE files, e.g. `$M3LIB/mpich/include`

-   `extra_lib`<a id=extra_lib></a>
    Set to other libraries required for compiling on your Linux system; users will likely need to change this setting in the distributed script for portability to their system.

-   `EXEC_ID`<a id=EXEC_ID></a>
    build tag, should be automatically set by config_cmaq.csh

## A.2 Compilation Configuration Variables

The configuration options listed here are set during compilation of the CCTM executable. When these options are invoked they create a binary executable that is fixed to the specified configuration. To change these options you must recompile CCTM and create a new executable.

Several of the CCTM science modules have more than one option.  Brief descriptions of these options are provided here.  For details on the science of the different options refer to the [CMAQ Release Notes](../../CCTM/docs/Release_Notes/README.md).

The following five options are invoked by uncommenting the line in the CCTM build script.  Comment the line in the script using a "#" to turn the option off.

-   `CopySrc`<a id=CopySrc></a>  
    Uncomment to copy the source code into a working build (BLD) directory. If commented, only the compiled object and executable files will be placed in the BLD directory.

-   `set ParOpt`<a id=set ParOpt></a>  
    Build an executable for running on multiple processors. Invoking this command requires the availability of the MPI library/INCLUDE files.

-   `MakeFileOnly`<a id=MakeFileOnly></a>  
    Uncomment to build a Makefile to compile the executable. Comment out to both create a Makefile and compile.

-   `build_parallel_io`<a id=set build_parallel_io></a>  
     Uncomment to build CMAQ with true parallel I/O feature (requires ioapi3.2 and pnetcdf)

-   `build_twoway`<a id=set build_twoway></a>  
    Uncomment to build WRF-CMAQ twoway - this cannot be set for stand-alone CMAQ

-   `potvortO3`<a id=set potvort03></a>   
    Uncomment to build CMAQ with potential vorticity free-troposphere O3 scaling

The following configuration settings may have multiple options. Select one option in the CCTM build script.

-   `ModDriver: [default: driver/wrf]`<a id=ModDriver></a>
    The CCTM generalized -coordinate driver module.
    - `driver/wrf`  
    use WRF-based scheme for mass-conserving advection; select this option when using WRF meteorology
    - `driver/yamo`  
    use Yamartino scheme for mass-conserving advection

-   `ModGrid: [default: Cartesian]`<a id=ModGrid></a>  
    The CCTM model grid configuration module. Currently only Cartesian coordinates are supported by CMAQ. Do not change this module setting.
    -   `grid/cartesian`

-   `ModInit: [default: init/yamo]`<a id=ModInit></a>  
    The CCTM time-step initialization module that uses a Yamartino scheme for mass-conserving advection. Do not change this module setting.
    -   `init/yamo`

-   `ModCpl: [default: couple/gencoor_wrf]`<a id=ModCpl></a>  
    Mass coupling concentration converstion module options. Unit conversion and concentration coupling module.
    -   `couple/gencoor_wrf`  
    Coupling scheme compatible with the WRF-based advection scheme; select this option when `ModDriver` is set to `driver/wrf`
    -  `couple/gencoor`  
    Coupling scheme compatible with the Yamartino advection scheme; select this option when `ModDriver` is set to `driver/yamo`.  

-    `ModHadv: [default: hadv/yamo]`<a id=ModHadv></a>  
      Horizontal advection module.  Currently only the Yamartino global mass-conserving hoizontal advection scheme is supported.
     -   `hadv/yamo`

-   `ModVadv: [default: vadv/wrf]`<a id=ModVadv></a>  
    Vertical advection module.
    -   `vadv/wrf`  
    use the WRF omega calculation with the Piecewise Parabolic Method (PPM) to calculate vertical advection; this module should be used only with WRF meteorology
    -   `vadv/yamo`  
    use the global mass-conserving scheme to calculate vertical advection
-   `ModHdiff: [default: hdiff/multiscale]`<a id=ModHdiff></a>  
    The only option in CMAQv5 for the horizontal diffusion module is `hdiff/multiscale`, which uses a diffusion coefficient based on local wind deformation. Do not change this module setting.
    -   `hdiff/multiscale`  
-   `ModVdiff: [default: vdiff/acm2]`<a id=ModVdiff></a>  
    Vertical diffusion and surface exchange module. Do not change this module setting.
    -   `vdiff/acm2`  
    calculate vertical diffusion using the Asymmetric Convective Model version 2 (ACM2)
-   `ModDepv: [default: depv/m3dry]`<a id=ModDepv></a>  
    Deposition velocity calculation module. Do not change this module setting.
    -   `depv/m3dry`  
    CMAQ dry deposition velocity routine
-   `ModEmis: [default: emis/emis]`<a id=ModEmis></a>  
    CMAQ in-line anthropogenic and natural emissions module. In line emissions are activated in the CCTM run script. Do not change this module setting.
    -   `emis/emis`
-   `ModBiog: [default: biog/beis3]`<a id=ModBiog></a>  
Calculate biogenic emissions in-line with the BEIS3 model. Inline biogenic emissions are activated in the CCTM run script. Do not change this module setting.
    - `biog/beis3`
-   `ModPlmrs: [default: plrise/smoke]`<a id=ModPlmrs></a>  
Calculate in-line plume rise for large point sources using the Briggs algorithm as it is implemented in SMOKE. Inline emissions plume rise in controlled in the CCTM run script. Do not change this module setting.
    - `plrise/smoke`  
-   `ModCgrds: [default: spcs/cgrid_spcs_nml]`<a id=ModCgrds></a>  
    CMAQ model species configuration module.
    -   `spcs/cgrid_spcs_nml`  
    namelist files used to configure CMAQ model species
    -   `spcs/cgrid_specs_icl`  
    use Fortran INCLUDE files used to configure CMAQ model species
-   `ModPhot: [default: phot/inline]`<a id=ModPhot></a>  
    Photolysis calculation module.
    -   `phot/inline`  
    calculate photolysis rates in-line using simulated aerosols and ozone concentrations
    -   `phot/table`  
    calculate clear-sky photolysis rates off-line using the CMAQ program JPROC; provide daily photolysis rate look-up tables to CCTM
-   `Mechanism: [default: cb05e51_ae6_aq`]<a id=Mechanism></a>  
    Chemistry mechanism for gas, aerosol, and aqueous chemistry. See the [CMAQ Mechanism Definitions Table](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/CMAQ_OGD_appendix_A.md) for a listing of the mechanism choices that are available in CMAQv5.2.
-   `Tracer [default trac0] `<a id=Tracer></a>  
    Specifies tracer species. Invoking inert tracer species in CMAQ requires defining the tracers using namelist files and compiling the CMAQ programs with these files. The setting for this module corresponds to the directory name in the ``$CMAQ_HOME/CCTM/src/MECHS`` directory that contains the namelist files for the tracer configuration. The default setting is to not use any tracers.
    - `trac[n]`
-   `ModGas: [default: gas/ebi_${Mechanism}]`<a id=ModGas></a>  
     Gas-phase chemistry solver module.
     -  `smvgear`  
     use the SMVGEAR chemistry solver
     -  `ros3`  
     use gas/the Rosenbrock chemistry solver
     -  `ebi`  
     use the Euler Backward Iterative solver
-   `ModAero: [default: aero6]`<a id=ModAero></a>  
    CMAQ aero/aerosol module.
    -   `aero6`  
    sixth-generation modal CMAQ aerosol model with extensions for sea salt emissions and thermodynamics; includes a new formulation for secondary organic aerosol yields
-   `ModCloud: [default: cloud/acm_ae6]`<a id=ModCloud></a>  
    CMAQ cloud module for modeling the impacts of clouds on deposition, mixing, photolysis, and aqueous chemistry.
    -   `cloud/acm_ae6`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with heterogeneous chemistry for AERO6
    -   `cloud/acm_ae6_mp`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with heterogeneous chemistry for AERO6 and air toxics; this is the multipollutant mechanism in CMAQv5
    -   `cloud/acm_ae6_kmt`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with heterogeneous chemistry for AERO6 and aqueous chemistry with kinetic mass transfer and Rosenbrock solver
    -   `cloud/acm_ae6i_kmti`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with heterogeneous chemistry for AERO6 and aqueous chemistry with kinetic mass transfer and Rosenbrock solver with an extension to simulate the aqueous phase formation of SOA in cloud droplets, see: [CMAQv5.1 Aqueous Chemistry](https://www.airqualitymodeling.org/index.php/CMAQv5.1_Aqueous_Chemistry)
-   `ModUtil: [default: util]`<a id=ModUtil></a>  
    CMAQ utility modules. Do not change this module setting.
    -  `util/util`
-   `Tracer: [default: trac0]`<a id=Tracer_2></a>
     Add chemically inert tracers to the CCTM, default no tracer species.
-   `ModPa: [default: procan/pa]`<a id=ModPa></a>
    Process analsis is controlled in the CCTM run script. Do not change this module setting.
     - `procan/pa`
-   `ModPvO3: [default: pv_o3]`<a id=ModPvO3></a>
    Potential vorticity parameterization for free-troposphere exhange of ozone. This option is configured using the potvorO3 variable in the CCTM build script. Do not change this module setting.
    - `pv_o3`

## A3. Execution Configuration Variables

The environment variables listed below are invoked during execution of the CCTM and are set in the CCTM run script.

-   `compiler [default: intel]`<a id=compiler></a>
-   `compilerVrsn [default: 13.1]`<a id=compilerVrsn></a>
-   `VRSN [default: v52]`<a id=VRSN></a>
-   `PROC [default: mpi]`<a id=PROC></a>   
Sets if the CCTM will run in multi-processor or serial mode.
    - `mpi`  
    Use MPI multi-processor configuration. Additional configuration settings are required when selecting `mpi`. The CCTM must have been built to support MPI. The run script requires settings for the number of processors and other MPI configuration variables required by the Linux system.
    - `serial`  
    Run the CCTM in serial, single-processor mode.
-   `MECH [default: None]`<a id=MECH></a>
    CMAQ chemical mechanism. Must match Mechanism variable setting in the CCTM build script.
-   `EMIS [default: 2013ef]`<a id=EMIS></a>
-   `APPL [default: SE52BENCH]`<a id=APPL></a>  
    CCTM executable identifier. Must match APPL Variable setting in the CCTM build script.
-   `RUNID [default: $VRSN_compiler_APPL]`<a id=RUNID></a>
    Run ID used to track version number, compiler, and application case name.
-   `EXEC [default: CCTM_$APPL_$EXECID]`<a id=EXEC></a>  
    The name of the CCTM executable.

### MPI Configuration
-   `NPCOL_NPROW [default: 1 1]`<a id=NPCOL_NPROW></a>  
    The numbers of columns and rows for decomposing the modeling domain in an MPI configuration. The product of this pair of numbers must equal the total number of processors allocated to the CCTM simulation. For serial or single-processor MPI runs set to `1 1`. For multi-processor simulations, the number of columns (i.e, the first number in the pair) should be greater than or equal to the number of rows.  For example, for an 8 processor MPI simulation, set to `4 2`
-   `NPROCS [default: 1]`<a id=NPROCS></a>  
    Number of processors to allocate for the CCTM simulation; equal to the product of NPCOL x NPROW. For serial or single-processor MPI runs set to `1`, otherwise set to the product of the two numbers used in NPCOL_NPROW.

### Vertical extent
-    `NZ [default: 35]`<a id=NZ></a>
      Set the number of vertical layers.

### Timestep Configuration

-   `NEW_START_TRUE [default: TRUE]`<a id=NEW_START_TRUE></a>
     For a model restart set to FALSE
-   `START_DATE`<a id=START_DATE></a>  
    Simulation start date in Gregorian format (YYYY-MM-DD)
-   `END_DATE`<a id=END_DATE></a>
    Simulation end date in Gregorian format (YYYY-MM-DD)
-   `STTIME`<a id=STTIME></a>  
    Simulation start time (HHMMSS)
-   `NSTEPS [default: 240000]`<a id=NSTEPS></a>  
    Number of simulation time steps (HHMMSS)
-   `TSTEP [default: 010000]`<a id=TSTEP></a>  
    Simulation output time step interval (HHMMSS)

### CCTM Configuration Options

-   `LOGFILE [default: $BASE/$APPL.log]`<a id=LOGFILE></a>  
    Uncomment to capture CCTM standard output to a log file; the LOGFILE variable sets the name and location of the log.
-   `GRID_NAME [default: CMAQ-BENCHMARK]`<a id=GRID_NAME></a>  
    Name of the grid definition contained in the GRIDDESC file that specifies the horizontal grid for the current application of the model.
-   `GRIDDESC [default: $CMAQ_HOME/scripts/GRIDDESC1]`<a id=GRIDDESC></a>  
    Grid description file for setting the horizontal grid definition.
-   `CTM_APPL [default: ${RUNID}_${YYYYMMDD}]`<a id=CTM_APPL></a>  
    CCTM log file naming extension.
-   `CONC_SPCS [if commented out, all species]`<a id=CONC_SPCS></a>  
    Model species to be written to the CCTM CONC file.
-   `CONC_BLEV_ELEV [if commented out, all layers]`<a id=CONC_BLEV_ELEV></a>  
    Vertical model layer range for the CONC-file concentrations; this variable sets the lower and upper layers over which to output the CONC file.
-   `AVG_CONC_SPCS [if commented out, output all species]`<a id=AVG_CONC_SPCS></a>  
    Model species for calculating integral average concentrations for each output time step. Options can be any of the standard output species that are written to the CCTM CONC file. The species in this list will be written to the ACONC output file.
-   `ACONC_BLEV_ELEV [default: if commented out, all layers]`<a id=ACONC_BLEV_ELEV></a>  
    Vertical model layer range for integral average concentrations; this variable sets the lower and upper layers over which to calculate integral average concentrations. For example, setting this variable to “1 5” will produce integral average concentrations for model layers 1 through 5.
-   `ACONC_END_TIME [default: N]`<a id=ACONC_END_TIME></a>  
    Change the time stamp of the ACONC file output time step from the default of the beginning of the hour to the end of the hour.
    - `Y`: Set the time stamp to the end of each hour.
    - `N`: Set the time stamp to the beginning of the hour.
-   `EXECUTION_ID`<a id=EXECUTION_ID></a>  
    The name of the CCTM executable; automatically set by the script.

### Synchronization Time Step and Tolerance Options

-   `CTM_MAXSYNC [default: 300]`<a id=CTM_MAXSYNC></a>  
    Maximum synchronization time step in seconds
-   `CTM_MINSYNC [default: 60]`<a id=CTM_MINSYNC></a>  
    Minimum synchronization time step in seconds
-   `SIGMA_SYNC_TOP [default: .70]`<a id=SIGMA_SYNC_TOP></a>  
    Top sigma level thru which sync step determined
-   `ADV_HDIV_LIM [default: .95]`<a id=ADV_HDIV_LIM></a>
     Maximum horizontal division limit for advection time step adjustment
-   `CTM_ADV_CFL [default: .95]`<a id=CTM_ADV_CFL></a>  
    Maximum Courant–Friedrichs–Lewy (cfl) condition
-   `RB_ATOL [default: 1.0E-09]`<a id=RB_ATOL></a>  
    Global Rosenbrock (ROS3) chemistry solver absolute tolerance

### Science Options

-   `CTM_WB_DUST [default: Y]`<a id=CTM_WB_DUST></a>  
    Setting to calculate in-line windblown dust emissions in CCTM. Setting this variable to Y requires the availability of gridded land use input files that include the following BELD USGS land use classifications: shrubland, shrubgrass, and sprsbarren. See [Chapter 8](CMAQ_OGD_ch08_input_files.md#Table8-1) for a description of the DUST_LU_1 and DUST_LU_2 input files. Comment out variable or set to Y to turn on; set to N to turn off.
-   `CTM_ERODE_AGLAND [default: Y]`<a id=CTM_ERODE_AGLAND></a>  
    Setting to use optional erodible agricultural land classifications for computing windblown dust emissions from agricultural land. Setting this variable to Y requires the availability of gridded crop timing data that describe planting start dates, planting end dates, and harvesting end dates for 18 crop types. See [Chapter 8](CMAQ_OGD_ch08_input_files.md#Table8-1) for a description of the CROPMAP01, CROPMAP04, and CROPMAP08 input files. If CTM_WB_DUST is set to N, this setting will be ignored. Set to Y to turn on; comment out variable or set to N to turn off.
-   `CTM_WBDUST_BELD [default: BELD3]`<a id=CTM_WBDUST_BELD></a>
    Landuse database for identifying dust source regions;  ignore if `CTM_WB_DUST = N`
    - `BELD3`  
    Use BELD3 landuse data
    - `BELD4`
    Use BELD4 landuse data
-   `CTM_LTNG_NO [default: Y]`<a id=CTM_LING_NO></a>  
    Setting to activate lightning NO emissions. Setting this variable to Y requires additional variables to define the configuration of the lightning NO emissions calculation. See the settings for `LTNGNO`, `LTNGPARAMS`, `NLDN_STRIKES`, and `LTNGDIAG` below. Set to Y to turn on; comment out variable or set to N to turn off.
-   `CTM_WVEL [default: Y]`<a id=CTM_WVEL></a>  
    Setting to output the CCTM-calculated vertical velocities to the CONC file. Set to Y to turn on; comment out variable or set to N to turn off.
-   `KZMIN [default: Y]`<a id=KZMIN></a>  
    If KZMIN is set to Y, CCTM will read the urban land use fraction variable (PURB) from the GRID_CRO_2D meteorology file and use this information to determine the minimum eddy diffusivity in each grid cell. In CMAQv5, grid cells that are predominantly urban use a KZMIN value of 1.0 m<sup>2</sup>/s and non-urban cells use a value of 0.01 m<sup>2</sup>/s. If this variable is set to N, the PURB variable will not be used and a uniform KZMIN value of 1.0 m<sup>2</sup>/s will be used throughout the modeling domain.
-   `CTM_ILDEPV [default: Y]`<a id=CTM_ILDEPV></a>  
    Calculate in-line deposition velocities. Comment out variable or set to Y to turn on; set to N to turn off.
-   `CTM_MOSAIC [default N]`<a id=CTM_MOSAIC></a>  
    Calculate land use specific deposition velocities and fluxes.
-   `CTM_FST [default: N]`<a id=CTM_FST></a>  
    Use MOSAIC method to get land-use specific stomatal flux.
-   `CTM_ABFLUX [default: Y]`<a id=CTM_ABFLUX></a>  
    Activate fertilizer ammonia bidirectional flux for in-line emissions and deposition velocities. If CTM_ILDEPV is set to N this variable is ignored. Setting this variable to Y requires four additional input files that include gridded fractional crop distributions (B4LU_file), soil properties (E2C_Soilfile), fertilizer conditions (E2C_Fertfile), and an agricultural soil initial conditions file (INIT_MEDC_1). Activation of this setting will produce additional variables in the output dry deposition file. See [Chapter 8](CMAQ_OGD_ch08_input_files.md#Table8-1) for a description of the required input files. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_HGBIDI [default: N]`<a id=CTM_HGBIDI></a>  
    Activate mercury bidirectional flux for in-line emissions and deposition velocities. If CTM_ILDEPV is set to N this variable is ignored. Activation of this setting will produce additional variables in the output dry deposition file. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_SFC_HONO [default: Y]`<a id=CTM_SFC_HONO></a>  
    Calculate surface HONO interactions. If CTM_ILDEPV is set to N this variable is ignored. Comment out or set to Y to turn on; set to N to turn off.
-   `CTM_GRAV_SETL [default Y]`<a id=CTM_GRAV_SETL></a>  
    Activate gravitational sedimentation for aerosols. Comment out or set to Y to turn on; set to N to turn off.
-   `CTM_BIOGEMIS [default: Y]`<a id=CTM_BIOGEMIS></a>  
    Calculate biogenic emissions. Comment out or set to Y to turn on; set to N to turn off.  If this option is activated, several additional variables must be set (see the In-line biogenic emissions configuration settings)     
-   `CTM_PT3DEMIS [default: Y]`<a id=CTM_PT3DEMIS></a>  
    Calculate plume rise for elevated point sources. Set to Y to turn on; comment out or set N to turn off. If this option is activated several additional variables must be set (see the Inline emissions configuration settings) following variables must be set.
-   `CTM_ZERO_PCSOA [default: N]`<a id=CTM_ZERO_PCSOA></a>
     Turn off the emissions of the VOC precursor to pcSOA. The CMAQ dev team recommends leaving pcSOA mass in the model for production runs.

### Process analysis options

-   `CTM_PROCAN [default: N]`<a id=CTM_PROCAN></a>  
    Activate process analysis in the CCTM. Set this to Y and use $CMAQ_DATA/pacp/pacp.inp to configure the integrated process rate and integrated reaction rate settings for the CCTM.  Additional process analysis output files will be created when this setting is activated.
-   `PA_BCOL_ECOL [default: None]`<a id=PA_BCOL_ECOL></a>  
    Modeling grid domain column range for the process analysis calculations. Set to the two digits representing the beginning and ending column number bounding the process analysis domain.
-   `PA_BROW_EROW [default: None]`<a id=PA_BROW_EROW></a>  
    Modeling grid domain row range for the process analysis calculations. Set to the two digits representing the beginning and ending row number bounding the process analysis domain.
-   `PA_BLEV_ELEV [default: None]`<a id=PA_BLEV_ELEV></a>  
    Modeling grid domain layer range for the process analysis calculations. Set to the two digits representing the bottom and top layer numbers bounding the process analysis domain.

### I/O Controls

-   `IOAPI_LOG_WRITE [default: Y]`<a id=IOAPI_LOG_WRITE></a>  
    Set to T to turn on excess WRITE3 logging by the I/O API.
-   `FL_ERR_STOP [default: N]`<a id=FL_ERR_STOP></a>  
    Set to T to configure the program to exit if inconsistent headers are found in the input files.
-   `PROMPTFLAG [default: N]`<a id=PROMPTFLAG></a>  
    Turn on I/O-API PROMPTFILE interactive mode. Set to T to require interactive prompts for different I/O API operations.  
-   `IOAPI_OFFSET_64 [default: N]`<a id=IOAPI_OFFSET_64></a>  
    I/O API setting for large time step records. If your output time step is going to produce data that are >2GB per time step, then this needs to be set to YES.

### Aerosol Diagnostics Controls

-   `CTM_AVISDIAG [default: N]`<a id=CTM_AVISDIAG></a>  
    Output visibility diagnostics file. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_PMDIAG [default: N]`<a id=CTM_PMDIAG></a>  
    Output aerosol diagnostics and properties file. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_APMDIAG [default: N]`<a id=CTM_APMDIAG></a>  
    Output hourly average aerosol diagnostics and properties file. Set to Y to turn on; comment out or set to N to turn off.
-   `APMDIAG_BLEV_ELEV [default: None]`<a id=APMDIAG_BLEV_ELEV></a>  
    Modeling grid domain layer range for the hourly average aerosol diagnostics and properties file. Set to the two digits representing the bottom and top layer numbers to bound the output domain.

### Diagnostic Output Flags

-   `CTM_CKSUM [default: Y]`<a id=CTM_CKSUM></a>  
    Write science processes summaries to the standard output. Impacts run speed and log file output size. Comment out or set to Y to turn on; set to N to turn off.
-   `CLD_DIAG [default: N]`<a id=CLD_DIAG></a>  
    Output an hourly wet deposition diagnostic file (CTM_WET_DEP_2) that includes convective wet deposition estimates. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_PHOTDIAG [default: N]`<a id=CTM_PHOTDIAG></a>  
    Output in-line photolysis rates and associated data to diagnostic netCDF output files. The file CTM_RJ_1 contains gridded photolysis rates for O3 (JO3O1D) and NO2 (JNO2) that include both clear-sky and cloud effects, total downward irradiance at the surface (ETOT_SFC_W), aerosol optical depth (TAU_AERO_W), total optical depth (TAU_TOT_W), optical depth of ozone above the model domain (TAUO3_TOP_W), Rayleigh optical depth above the model domain (TAU_RAY_W), and surface albedo (ALBEDO_W). The file CTM_RJ_2 contains gridded photolysis rates for all other photolysis reactions in the selected chemical mechanism. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_SSEMDIAG [default: N]`<a id=CTM_SSEMDIAG></a>  
    Output the calculated sea salt emissions to a diagnostic netCDF output file (CTM_SSEMIS_1). Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_DUSTEM_DIAG [default: N]`<a id=CTM_DUSTEM_DIAG></a>  
    Output the in-line dust emissions to a diagnostic netCDF output file (CTM_DUST_EMIS_1). The diagnostic file includes not only the total dust emissions, but also dust emissions by land use category and dust model parameters, such as gridded erodible land use fractions. Set to Y to turn on; comment out or set to N to turn off.
-   `CTM_DEPV_FILE [default: N]`<a id=CTM_DEPV_FILE></a>  
    Output an hourly diagnostic file (CTM_DEPV_DIAG) for the in-line deposition velocity calculations. If CTM_ILDEPV is set to N this variable is ignored. Set to Y to turn on; comment out or set to N to turn off.
-   `VDIFF_DIAG_FILE [default: N]`<a id=VDIFF_DIAG_FILE></a>  
    Output a diffusion and aero gravitational sedimentation diagnostic file. Set to Y to turn on; comment out or set to N to turn off.
-    `CTM_AOD [default N]`<a id=CTM_AOD></a>  
    Output an aerosol optical depth (AOD) calculation diagnostics file. Set to Y to turn on; comment out or set to N to turn off.
-   `LTNGDIAG [default: N]`<a id=LTNGDIAG></a>  
    Output a lightning NO emissions diagnostics file. Set to `Y` to turn on; comment out or set to `N` to turn off.
-   `DISP [default: keep]`<a id=DISP></a>  
    Controls the maintenance of existing log files.
    -   `delete` delete output log if it already exists
    -   `keep` abort simulation if output log exists

### Inline emissions configuration

-   `NPTGRPS [default: 1]`<a id=NPTGRPS></a>  
    The number of input point-source elevated emission sector file groups. A maximum of 9 sectors is allowed.

-   `STK_GRPS_## `<a id=STK_GRPS_##></a>  
    Directory path and file name of the stack groups file for sector ##, where ## = 01, 02,…,NPTGRPS. Each ## refers to one of the plume rise point-source sectors.

-   `STK_EMIS_##`<a id=STK_EMIS_##></a>  
    Directory path and file name of the point emissions file for sector ##, where ## = 01, 02,…,NPTGRPS. Each ## refers to the one of the plume rise point-source sectors.

-   `LAYP_STDATE [HHMMSS]`<a id=LAYP_STDATE></a>  
    Start date for calculating elevated-point-source emissions.

-   `LAYP_STTIME [HHMMSS]`<a id=LAYP_STTIME></a>   
    Start time for calculating elevated-point-source emissions.

-   `LAYP_NSTEPS [HHHHHH]`<a id=LAYP_NSTEPS></a>  
    Number of time steps for calculating elevated-point-source emissions.

-   `CTM_EMLAYS [default: max no of model layers]`<a id=CTM_EMLAYS></a>  
    Number of emissions layers for calculating elevated-point-source emissions. If not set (commented out), the maximum number of model layers will be used.

-   `PT3DDIAG [default: N]`<a id=PT3DDIAG></a>  
    Output the in-line 3-D point-source emissions to a diagnostic netCDF output file (CTM_PT3D_DIAG). Set to Y to turn on; comment out or set to N to turn off.

-   `PT3DFRAC [default: N]`<a id=PT3DFRAC></a>  
    Output the in-line 3-D point-source layer fractions to a diagnostic netCDF output file (PLAY_SRCID_NAME). Set to Y to turn on; comment out or set to N to turn off.

-   `REP_LAYER_MIN [default: -1]`<a id=REP_LAYER_MIN></a>  
    Minimum layer number for reporting plume rise values to the plume rise diagnostics file. Set to `-1` or comment out to report all layers.

### Lightning NOx configuration

-   `LTNGNO [default:InLine]`<a id=LTNGNO></a>  
    Setting to define whether the lightning emissions calculation will be in-line or off-line. This variable can be set to a gridded netCDF file of lightning NO emissions to use emissions calculated with a preprocessor outside of CCTM. Setting this variable to “inline” activates the in-line emissions calculation in CCTM and requires the LTNGPARMS_FILE variable (see below) to provide parameters for generating in-line lightning NO emissions.

-   `USE_NLDN [default: Y]`<a id=USE_NLDN></a>  
    Use hourly NLDN strikes file to compute inline lighning NO emissions. Activating this setting requires the NLDN_STRIKES input file.  Comment out or set to Y to turn on; set to N to turn off. If USE_NLDN is set to N and LTNGNO set to "InLine", lightning NO emissions will be generated using parameters provided in the LTNGPARMS_FILE.

-  `NLDN_STRIKES [default: None]`<a id=NLDN_STRIKES></a>  
    Hourly NLDN lightning strike netCDF FILE. Required when `LTNGNO` is set to `Inline` and `USE_NLDN` is set to `Y`; otherwise ignore this setting.
    
-  `LTNGPARMS_FILE [default: None]`<a id=LTNGPARMS_FILE></a>  
    Lightning parameters netCDF file, which contains the linear regression parameters for generating lightning NO using the parameterization scheme when LTNGNO set to "InLine" and USE_NLDN set to N. In addition, it also contains the intercloud to cloud-to-ground flash ratios, scaling factors for calculating flashes using the convective precipitation rate, land-ocean masks, and the moles of NO per flash (cloud-to-ground and intercloud) which are used by both lightning production schemes (NLDN and parameterization). Ingore if LTINGNO set to an external input file. 

-  `CTM_LTNGDIAG_1`<a id=LTNGOUT></a>  
    Lightning diagnostics output 3D netCDF file; ignore if `LTNGDIAG = N`

-  `CTM_LTNGDIAG_2`<a id=LTNGOUT></a>  
    Lightning diagnostics output 2D netCDF file (column total lightning NO emissions); ignore if `LTNGDIAG = N`
    
### In-line biogenic emissions configuration

-   `GSPRO [default: None]`<a id=GSPRO></a>  
    Directory path and file name for input ASCII speciation profiles.

-   `B3GRD [default: None]`<a id=B3GRD></a>  
    Grid-normalized biogenic emissions input netCDF file.

-   `BIOG_SPRO [default: None]`<a id=BIOG_SPRO></a>  
    Profile ID for speciating biogenic VOCs. This profile ID must be present in the GSPRO file.

-   `BIOSW_YN [default: Y]`<a id=BIOSW_YN></a>  
    Use the frost dates switch file to determine whether to use winter or summer biogenic emissions. Comment out or set to Y to turn on; set to N to turn off.

-   `BIOSEASON [default: None]`<a id=BIOSEASON></a>  
    File name for the frost dates switch input netCDF file.

-   `SUMMER_YN [default: Y]`<a id=SUMMER_YN></a>  
    Toggle for summer season normalized biogenic emissions. This variable is ignored if BIOSW_YN is set to Y. Comment out or set to Y to select summer season biogenic emissions factors; set to N to turn off.

-   `PX_VERSION [default: Y]`<a id=PX_VERSION></a>  
    Setting to indicate whether the Pleim-Xiu land-surface model was used for the input meteorology. If this setting is set to Y the input meteorology data must include soil moisture (SOILM), soil temperature (SOILT), and soil type (ISLTYP) variables for use in the calculation of soil NO emissions.

-   `INITIAL_RUN [default: N]`<a id=INITIAL_RUN></a>  
    Set to Y if this is the first time that biogenic NO soil emissions will be calculated. If there is a previously created file, set to N.

-   `SOILINP [default: None]`<a id=SOILINP></a>  
    Directory path and file name of biogenic NO soil emissions file. If INITIAL_RUN is set to N or F, the soil NO emissions file from the previous day’s simulation will be a required input file.

-   `B3GTS_DIAG [default: N]`<a id=B3GTS_DIAG></a>  
    Write the inline biogenic emissions (mass units) to a diagnostic netCDF output file (B3GTS_S). Set to Y to turn on; comment out or set to N to turn off.

-   `B3GTS_S`<a id=B3GTS_S></a>  
    Diagnostic output netCDF file of biogenic emissions. This variable is ignored if B3GTS_DIAG is set to N.


### Windblown dust emissions configuration

-   `DUST_LU_1`<a id=DUST_LU_1></a>  
    Input BELD "A" landuse netCDF file gridded to the modeling domain. Used if `CTM_WBDUST_BELD` is set to BELD3.

-   `DUST_LU_2`<a id=DUST_LU_2></a>  
    Input BELD "TOT" landuse netCDF file gridded to the modeling domain. Used if `CTM_WBDUST_BELD` is set to BELD3.

-   `MODIS_FPAR`<a id=MODIS_FPAR></a>  
    Input MODIS FPAR time-varying vegetation netCDF file gridded to the modeling domain.

-   `BELD4_LU`<a id=BELD4_LU></a>  
    Input BELD4 landuse netCDF file gridded to the modeling domain. Used if `CTM_WBDUST_BELD` is set to BELD4.

-   `CROPMAP01`<a id=CROPMAP01></a>  
    Input beginning planting dates netCDF file gridded to the modeling domain.

-   `CROPMAP04`<a id=CROPMAP04></a>  
    Input end planting dates netCDF file gridded to the modeling domain.

-   `CROPMAP08`<a id=CROPMAP08></a>  
    Input end harvesting dates netCDF file gridded to the modeling domain.

 [Home](README.md) - [Next Appendix >>](CMAQ_UG_appendixB_emissions_control.md)<br>
 CMAQ User's Guide (c) 2019<br>
