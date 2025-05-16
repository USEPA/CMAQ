
<!-- BEGIN COMMENT -->

[<< Tables and Figures](../CMAQ_UG_tables_figures.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixB_emissions_control.md)

<!-- END COMMENT -->

* * *

# Appendix A: Model options

<!-- BEGIN COMMENT -->

<a id=TOC_A></a>
## Table of Contents:

* [A.1 Configuration Options (config_cmaq.csh)](#config_cmaq.csh)
* [A.2 Compilation Options (bldit_cctm.csh)](#bldit_cctm.csh)
* [A.3 Execution Options (run_cctm.csh)](#run_cctm.csh)
	* [MPI Configuration](#MPI_Config)
	* [Vertical Extent](#Vertical_Ext)
	* [Timestep Configuration](#Timestep_Config)
	* [CCTM Configuration Options](#CCTM_Config_Options)
	* [Synchronization Time Step and Tolerance Options](#Syn_time_Option)
	* [Science Options](#Science_Options)
	* [Process Analysis Options](#Process_Analysis_Options)
	* [I/O Controls](#I/O_Controls)
	* [Aerosol Diagnostics Controls](#Aersol_Diagnostics_Controls)
	* [Diagnostic Output Flags](#Diagnostic_Output_Flags)
	* [Offline Emissions Configuration](#Offline_Emissions_Config)
	* [Lightning NOx Configuration](#Lightning_NOx_Config)
	* [Online Biogenic Emissions Configuration](#Online_Bio_Config)
	* [Windblown Dust Emissions Configuration](#windblown_dust_config)

<!-- END COMMENT -->

<a id=config_cmaq.csh></a>

## A.1 Configuration Options (config_cmaq.csh)

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

Consistency of configuration variables is critical for building CMAQ itself, not just its libraries. Accordingly, CMAQ includes the configuration script config_cmaq.csh to help enforce consistent environment settings for CMAQ and its associated libraries. The following lists the config_cmaq.csh variables defined for the build process and suggests values to which to set those variables.

Note that for multiprocessor applications it is recommended that the Fortran MPI wrapper script mpif90 be specified for the Fortran compiler (myFC). Using this script, instead of a direct call to the Fortran compiler, will ensure that the full suite of MPI components (libraries and include files) for the compiler are included in the parallel build.

-   `CMAQ_HOME` <a id=CMAQ_HOME></a>
    The central CMAQ installation directory. For example, if you installed the CMAQ source code in the directory `/home/user/CMAQ` set CMAQ_HOME with `export CMAQ_HOME=/home/user/CMAQ` for bash or `setenv CMAQ_HOME /home/user/CMAQ` for csh; note that this variable is M3HOME prior to CMAQv5.2

-   `CMAQ_DATA`<a id=CMAQ_DATA></a>
    Automatically set by config_cmaq.csh; note that this variable is M3DATA prior to CMAQv5.2

-   `OUTDIR`<a id=OUTDIR></a>
    Only necessary if the Budget Tool is activated. If this variable is unspecified, CMAQ will try to output the Budget Tool file to the root directory. This will be updated in future CMAQ versions to be consistent with other output files.  

-   `CMAQ_LIB`<a id=CMAQ_LIB></a>
    Automatically set by config_cmaq.csh; note that this variable is M3LIB prior to CMAQv5.2

-   `compiler`<a id=compiler_config></a>
    Set the Fortran compiler type that you will use to compile CMAQ; choices are intel, pgi, or gcc

-   `compilerVrsn`<a id=compilerVrsn></a>
    (Optional) Set the Fortran compiler version number that you will use to compile CMAQ; if you employ this variable, it will be appended to the compiler type when naming build directories and executables

-   `IOAPI_INCL_DIR`<a id=IOAPI_INCL_DIR></a>
    Location of the I/O API include files installed on your Linux system

-   `IOAPI_LIB_DIR`<a id=IOAPI_LIB_DIR></a>
    Location of the I/O API library on your Linux system

-   `NETCDF_LIB_DIR`<a id=NETCDF_LIB_DIR></a>
    Location of the netCDF C Library on your Linux system
    
-   `NETCDF_INCL_DIR`<a id=NETCDF_LIB_DIR></a>
    Location of the netCDF C include files on your Linux system

-   `NETCDFF_LIB_DIR`<a id=NETCDF_LIB_DIR></a>
    Location of the netCDF Fortran Library on your Linux system
    
-   `NETCDFF_INCL_DIR`<a id=NETCDF_LIB_DIR></a>
    Location of the netCDF Fortran include files on your Linux system

-   `MPI_LIB_DIR`<a id=MPI_LIB_DIR></a>
    Location of the Message Passing Interface Library on your Linux system

-   `ioapi_lib`<a id=ioapi_lib></a>
    Name of the I/O API library on your system; set to "-lioapi"

-   `netcdf_lib`<a id=netcdf_lib></a>
    Name of the netCDF library C on your system;  set to "-lnetcdf" for versions < 4.2.0, "-lnetcdf" for version 4.2.0 and later
    
-   `netcdff_lib`<a id=netcdf_lib></a>
    Name of the netCDF Fortran library on your system;  set to "-lnetcdff" for versions 4.2.0 and later, for version before 4.2.0 this    library is bundled with the C library.

-   `pnetcdf_lib`<a id=pnetcdf_lib></a>
    Name of the parallel netCDF library on your system; set to "-lpnetcdf"

-   `mpi_lib`<a id=mpi_lib></a>
    Name of the MPI library on your system; set to "-lmpich" for MVAPICH, "-lmpi" for OpenMPI

-   `myFC`<a id=myFC></a>
    Set to match the `FC` (Fortran compiler) you use to compile netCDF

-   `myCC`<a id=myCC></a>
    Set to match the `CC` (C compiler) you use to compile netCDF

-   `myFSTD` <a id=myFSTD></a>
    Standard Mode Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script  

-   `myDBG` <a id=myDBG></a>
    Debug Mode Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `myLINK_FLAGS` <a id=myLINK_FLAGS></a>
    Fortran compile linker flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `myFFLAGS`<a id=myFFLAGS></a>
    Fixed-format Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `myCFLAGS`<a id=myCFLAGS></a>
    C compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `myFRFLAGS`<a id=myFRFLAGS></a>
    Free form-format Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script

-   `extra_lib`<a id=extra_lib></a>
    Set to other libraries required for compiling on your Linux system; users will likely need to change this setting in the distributed script for portability to their system.

-   `EXEC_ID`<a id=EXEC_ID></a>
    Build tag, should be automatically set by config_cmaq.csh. Users who build the CMAQ model from a git repository will see the SHA ID, corresponding to the state of the repository code used to build their model, inserted into this EXEC_ID. This will support the model build and resulting output being uniquely associated with the model source code version.

-   `CMAQ_REPO` <a id=CMAQ_REPO></a> This is always the location of the CMAQ repository that the user will pull from to create exectuables. If the user is building CMAQ inside the repository then it will be equal to CMAQ_HOME. If not, the user must supply an alternative folder location.

<a id=bldit_cctm.csh></a>

## A.2 Compilation Options (bldit_cctm.csh)

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

The configuration options listed here are set during compilation of the CCTM executable through the build script, bldit_cctm.csh, located under the CCTM/scripts folder. When these options are invoked they create a binary executable that is fixed to the specified configuration. To change these options, you must recompile CCTM and create a new executable.

Several of the CCTM science modules have more than one option.  Brief descriptions of these options are provided here.

The following options are invoked by uncommenting the line in the CCTM build script.  Comment the line in the script using a "#" to turn the option off.

-   `CompileBLDMAKE`<a id=CopySrc></a>  
    Uncomment to use an existing BLDMAKE executable to build CCTM executable. If commented out, recompile BLDMAKE utility from the source.

-   `CopySrc`<a id=CopySrc></a>  
    Uncomment to copy the source code into a working build (BLD) directory. Currently, this option cannot be commented out to successfully compile the model. 

-   `MakeFileOnly`<a id=MakeFileOnly></a>  
    Uncomment to build a Makefile but to not compile the executable. The Makefile will be located in the BLD directory and can subsequently be used to manually compile the executable by typing 'make' in the BLD direcotry. Comment out to both create a Makefile and compile the executable when invoking the bldit_cctm.csh script.
    
-   `ParOpt`<a id=ParOpt></a>  
    Build an executable for running on multiple processors. Invoking this command requires the availability of the MPI library/INCLUDE files.
    
-   `build_parallel_io`<a id=build_parallel_io></a>  
     Uncomment to build CMAQ with true parallel I/O feature (requires mpi version of ioapi 3.2 and pnetcdf, refer to [Appendix D](./CMAQ_UG_appendixD_parallel_implementation.md).)

-   `Debug_CCTM`<a id=CopySrc></a>  
    Uncomment to compile the CCTM executable in Debug Mode.
    
-   `ISAM_CCTM`<a id=CopySrc></a>  
    Uncomment to compile the CCTM executable with Integrated Source Apportionment Method (ISAM). See [Chapter 11](../CMAQ_UG_ch11_ISAM.md) for futher information before invoking this option. 
    
-   `build_twoway`<a id=build_twoway></a>  
    Uncomment to build WRF-CMAQ two way model with explicit meteorological-chemical feedbacks - to build a stand-alone CMAQ, comment this option out.  During run time, if you encounter any problem, please contact David Wong (wong.david@epa.gov) for specific instructions for building WRF-CMAQ.


The following configuration settings may have multiple options. Select one option in the CCTM build script.

-   `ModGrid: [default: Cartesian]`<a id=ModGrid></a>  
    The CCTM model grid configuration module. Currently only Cartesian coordinates are supported by CMAQ. Do not change this module setting.
    -   `grid/cartesian`

-   `ModAdv: [default: wrf_cons]`<a id=ModHadv></a>  
    3-D Horizontal module. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#65-advection) for further information.
    -   `wrf_cons`  
    use the WRF vertically integrated column mass to calculate vertical advection
    -   `local_cons`  
    use the layer-by-layer integrated mass-conserving scheme to calculate vertical advection
    
-   `ModHdiff: [default: hdiff/multiscale]`<a id=ModHdiff></a>  
    The only option in CMAQv5 for the horizontal diffusion module is `hdiff/multiscale`, which uses a diffusion coefficient based on local wind deformation. Do not change this module setting. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#66-horizontal-diffusion) for further information.
    -   `hdiff/multiscale`
    
-   `ModVdiff: [default: vdiff/acm2]`<a id=ModVdiff></a>  
    Vertical diffusion and surface exchange module. Do not change this module setting. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#67-vertical-diffusion) for further information.
    -   `vdiff/acm2`  
    calculate vertical diffusion using the Asymmetric Convective Model version 2 (ACM2)
    
-   `ModDepv: [default: depv/m3dry]`<a id=ModDepv></a>  
    Deposition calculation module. Users may choose between the msdry and stage options.  If CMAQ output of land use specific deposition or stomatal flux is desired, then the stage option must be selected. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#68-dry-depositionair-surface-exchange) for further information. 
    -   `depv/m3dry`   
    CMAQ m3dry dry deposition routine.  This is an updated version of the routine that has always been in CMAQ. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#681-dry-deposition---m3dry) for further information.
    -   `depv/stage`
    CMAQ stage dry deposition routine.  This option is new in version 5.3. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#682-dry-depostion---stage) for further information. 

-   `ModEmis: [default: emis/emis]`<a id=ModEmis></a>  
    CMAQ inline anthropogenic and natural emissions module. Inline emissions are activated by the user via the CCTM run script. Do not change this module setting. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.
    -   `emis/emis`


-   `ModBiog: [default: biog/beis4]`<a id=ModBiog></a>  
Calculate biogenic emissions online with the BEIS4 model. Online biogenic emissions are activated in the CCTM run script. Do not change this module setting. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.
    - `biog/beis4`
-   `ModMegBiog: [default: biog/megan3]`<a id=ModMegBiog></a>  
Calculate biogenic emissions online with the MEGAN3.1 model. Online biogenic emissions are activated in the CCTM run script. Do not change this module setting. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.
    - `biog/megan3`
-   `ModPlmrs: [default: plrise/smoke]`<a id=ModPlmrs></a>  
Calculate inline plume rise for large point sources using the Briggs algorithm as it is implemented in SMOKE. Inline emissions plume rise is controlled in the CCTM run script. Do not change this module setting. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.
    - `plrise/smoke`  

-   `ModCgrds: [default: spcs/cgrid_spcs_nml]`<a id=ModCgrds></a>  
    CMAQ model species configuration module.
    -   `spcs/cgrid_spcs_nml`  
    namelist files used to configure CMAQ model species
    -   `spcs/cgrid_specs_icl`  
    use Fortran INCLUDE files to configure CMAQ model species

-   `ModPhot: [default: phot/inline]`<a id=ModPhot></a>  
    Photolysis calculation module. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#6103-photolysis) for further information.
    -   `phot/inline`  
    calculate photolysis rates inline using simulated aerosols and ozone concentrations
    -   `phot/table`  
    calculate clear-sky photolysis rates off-line using the CMAQ program JPROC; provide daily photolysis rate look-up tables to CCTM

-   `Mechanism: [default: cb6r5_ae7_aq`]<a id=Mechanism></a>  
    Chemistry mechanism for gas, aerosol, and aqueous chemistry. See the [CMAQv5.3 Chemical Mechanisms Table](../../../CCTM/src/MECHS/README.md) for a listing of the mechanism choices that are available in CMAQv5.3. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#610-gas-phase-chemistry) for further information.
-   `Tracer [default trac0] `<a id=Tracer></a>  
    Specifies tracer species. Invoking inert tracer species in CMAQ requires defining the tracers using namelist files and compiling the CMAQ programs with these files. The setting for this module corresponds to the directory name in the ``$CMAQ_HOME/CCTM/src/MECHS`` directory that contains the namelist files for the tracer configuration. The default setting does not use any tracers.
    - `trac[n]`

-   `ModGas: [default: gas/ebi_${Mechanism}]`<a id=ModGas></a>  
     Gas-phase chemistry solver module. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#6102-solvers) for further information.
     -  `smvgear`  
     use the SMVGEAR chemistry solver
     -  `ros3`  
     use gas/the Rosenbrock chemistry solver
     -  `ebi`  
     use the Euler Backward Iterative solver

-    `ModDiag` <a id=ModDiag></a>
     use various diagnostic routines. Currently only the vertical extraction tool is implemented here.

-   `ModAero: [default: aero7]`<a id=ModAero></a>  
    CMAQ aero/aerosol module. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#611-aerosol-dynamics-and-chemistry) for further information.
    -   `aero7`  
    seventh-generation modal CMAQ aerosol model with extensions for sea salt emissions and thermodynamics; includes a new formulation for secondary organic aerosol yields

-   `ModCloud: [default: cloud/acm_ae6]`<a id=ModCloud></a>  
    CMAQ cloud module for modeling the impacts of clouds on deposition, mixing, photolysis, and aqueous chemistry. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#612-aqueous-chemistry-scavenging-and-wet-deposition) for further information.
    -   `cloud/acm_ae6`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with aqueous chemistry for AERO6
    -   `cloud/acm_ae6_mp`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with aqueous chemistry for AERO6 and air toxics; this is the multipollutant mechanism in CMAQv5
    -   `cloud/acm_ae7`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with aqueous chemistry for AERO7
    -   `cloud/acm_ae7_kmt2`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with aqueous chemistry for AERO7. This cloud mechanism considers kinetic mass transfer and uses a Rosenbrock solver to simulate extended aqueous chemistry in cloud droplets
    -   `cloud/acm_cracmm`  
    ACM cloud processor that uses the ACM methodology to compute convective mixing with aqueous chemistry for CRACMM

-   `ModUtil: [default: util]`<a id=ModUtil></a>  
    CMAQ utility modules. Do not change this module setting.
    -  `util/util`

-  `ModPa: [default: procan/pa]`<a id=ModPa></a>
    Process analysis is controlled in the CCTM run script. Do not change this module setting.
     - `procan/pa`

-   `ModPvO3: [default: pv_o3]`<a id=ModPvO3></a>
    Potential vorticity parameterization for free-troposphere exchange of ozone. Do not change this module setting. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#613-potential-vorticity-scaling) for further information.
    - `pv_o3`
    
<a id=run_cctm.csh></a>

## A.3 Execution Options (run_cctm.csh)

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

The environment variables listed below are invoked during execution of the CCTM and are set in the CCTM run script, run_cctm.csh located under the CCTM/scripts folder.

-   `compiler [default: intel]`<a id=compiler></a>
-   `compilerVrsn [default: Empty]`<a id=compilerVrsn></a>
-   `VRSN [default: v55]`<a id=VRSN></a>
-   `PROC [default: mpi]`<a id=PROC></a>   
Sets if the CCTM will run in multi-processor or serial mode.
    - `mpi`  
    Use MPI multi-processor configuration. The CCTM executable must have been built to support MPI, see bldit_cctm.csh compilation options above. The run script requires settings for the number of processors and other MPI configuration variables required by the Linux system.  
    - `serial`  
    Run the CCTM in serial, single-processor mode.  
-   `MECH [default: None]`<a id=MECH></a>  
    CMAQ chemical mechanism. Must match `Mechanism` variable setting in the CCTM build script. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#using-predefined-chemical-mechanisms) for further information.  
-   `APPL [default: none]`<a id=APPL></a>  
    Application name used to label output binaries and log files.  
-   `RUNID [default: $VRSN_compiler_APPL]`<a id=RUNID></a>  
    Run ID used to track version number, compiler, and application case name.  
-   `BLD` <a id=BLD></a>  
    Directory path of the built CCTM executable  
-   `EXEC [default: CCTM_$VRSN]`<a id=EXEC></a>  
    The name of the CCTM executable.  

<a id=MPI_Config></a>

### MPI Configuration

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `NPCOL_NPROW [default: 1 1]`<a id=NPCOL_NPROW></a>  
    The numbers of columns and rows for decomposing the modeling domain in an MPI configuration. The product of this pair of numbers must equal the total number of processors allocated to the CCTM simulation. For serial or single-processor MPI runs set to `1 1`. For example, for an 8 processor MPI simulation, set to `4 2`.  
-   `NPROCS [default: 1]`<a id=NPROCS></a>  
    Number of processors to allocate for the CCTM simulation; equal to the product of NPCOL x NPROW. For serial or single-processor MPI runs set to `1`, otherwise set to the product of the two numbers used in NPCOL_NPROW.  

<a id=Vertical_Ext></a>

### Vertical extent

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-    `NZ [default: 35]`<a id=NZ></a>  
      Set the number of vertical layers. Script variable only, variable not used by CCTM model. Vertical extent inherited from MCIP model inputs. 

<a id=Timestep_Config></a>

### Timestep Configuration

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `NEW_START [default: TRUE]`<a id=NEW_START></a>  
    Value should be true for new simulations starting from an initial condition file. To restart from a previous days simulation output, set to FALSE. For all standard runscripts, this variable is automatically set to FALSE after looping to the second day of the simulation.  
-   `START_DATE`<a id=START_DATE></a>  
    Simulation start date in Gregorian format (YYYY-MM-DD)  
-   `END_DATE`<a id=END_DATE></a>  
    Simulation end date in Gregorian format (YYYY-MM-DD)  
-   `STTIME`<a id=STTIME></a>  
    Simulation start time (HHMMSS)  
-   `NSTEPS [default: 240000]`<a id=NSTEPS></a>  
    Number of simulation time steps (HHMMSS)  
-   `TSTEP [default: 010000]`<a id=TSTEP></a>   
    Simulation output time step interval (HHMMSS). Must be a mutiple of the run length. 
-   `MET_TSTEP [default: time step of METCRO3D file]`<a id=MET_TSTEP></a>   
    Meteorology input time step interval (HHMMSS). Users who wish to specify temporally coarser meteorology then their input meteorology may do so using this environment variable; this environmental variable is not included in our default runscripts. The default value of MET_TSTEP is the time-step of the METCRO3D file (input meteorology data step). Users may however specify MET_TSTEP to be multiples of the input meterology time-step as long as they add up to the output time step (define as environmental variable TSTEP). Ex. If the meteorology files have data available at 10 minute intervals and a desired 1-hour output frequency, valid MET_STEPS are {10,20,30,30,60...} minutes. 

<a id=CCTM_Config_Options></a>

### CCTM Configuration Options

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `GRID_NAME [default: Blank]`<a id=GRID_NAME></a>  
    Name of the grid definition contained in the GRIDDESC file that specifies the horizontal grid for the current application of the model.  
-   `GRIDDESC [default: Path to GRIDDESC file]`<a id=GRIDDESC></a>  
    Grid description file for setting the horizontal grid definition.  
-   `CTM_APPL [default: ${RUNID}_${YYYYMMDD}]`<a id=CTM_APPL></a>  
    CCTM log and output file naming extension.      
-   `CONC_SPCS [if commented out, all species]`<a id=CONC_SPCS></a>  
    Model species to be written to the CCTM_CONC file, including temperature, relative humidity and pressure. See [Chapter 7](../CMAQ_UG_ch07_model_outputs.md#72-cctm-output-files) for further information.
-   `CONC_BLEV_ELEV [if commented out, all layers]`<a id=CONC_BLEV_ELEV></a>  
    Vertical model layer range for the CCTM_CONC file concentrations; this variable sets the lower and upper layers over which to output the CCTM_CONC file. In the example script, BLEV and ELEV are both set to 1, so concentrations will only be written for the first layer.
-   `AVG_CONC_SPCS [if commented out, output all species]`<a id=AVG_CONC_SPCS></a>  
    Model species for calculating integral average concentrations for each output time step. Options can be any of the standard output species that are written to the CCTM_CONC file, including temperature, relative humidity and pressure. The species in this list will be written to the CCTM_ACONC output file. See [Chapter 7](../CMAQ_UG_ch07_model_outputs.md#72-cctm-output-files) for further information.
-   `ACONC_BLEV_ELEV [default: if commented out, all layers]`<a id=ACONC_BLEV_ELEV></a>  
    Vertical model layer range for integral average concentrations; this variable sets the lower and upper layers over which to calculate integral average concentrations. For example, setting this variable to “1 5” will produce integral average concentrations for model layers 1 through 5.
-   `AVG_FILE_END_TIME [default: N]`<a id=AVG_FILE_END_TIME></a>  
    Change the time stamp of the ACONC file output time step from the default of the beginning of the hour to the end of the hour.
    - `Y`: Set the time stamp to the end of each hour.  
    - `N`: Set the time stamp to the beginning of the hour.  
-   `EXECUTION_ID [default: Blank]`<a id=EXECUTION_ID></a>  
    The name of the CCTM executable; automatically set by the script.  

<a id=Syn_time_Option></a>

### Synchronization Time Step and Tolerance Options

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `CTM_MAXSYNC [default: 720]`<a id=CTM_MAXSYNC></a>  
    Maximum synchronization time step in seconds  
-   `CTM_MINSYNC [default: 60]`<a id=CTM_MINSYNC></a>  
    Minimum synchronization time step in seconds  
-   `SIGMA_SYNC_TOP [default: .70]`<a id=SIGMA_SYNC_TOP></a>  
    Top sigma level thru. which sync step determined  
-   `ADV_HDIV_LIM [default: .9]`<a id=ADV_HDIV_LIM></a>  
     Maximum horizontal divergence limit for advection time step adjustment  
-   `CTM_ADV_CFL [default: .75]`<a id=CTM_ADV_CFL></a>  
    Maximum Courant–Friedrichs–Lewy (CFL) condition  
-   `RB_ATOL [default: 1.0E-07]`<a id=RB_ATOL></a>  
    If using Rosenbrock (ros3) photochemistry solver, the absolute tolerance for
    converging to solution
-   `RB_RTOL [default: 1.0E-03]`<a id=RB_RTOL></a>  
    If using Rosenbrock (ros3) photochemistry solver, relative tolerance for converging 
    to solution
-   `GEAR_ATOL [default: 1.0E-09]`<a id=RB_ATOL></a>  
    If using Gear (smvgear) photochemistry solver, the absolute tolerance for
    converging to solution
-   `GEAR_RTOL [default: 1.0E-03]`<a id=RB_RTOL></a>  
    If using Gear (smvgear) photochemistry solver, relative tolerance for converging 
    to solution

<a id=Science_Options></a>

### Science Options

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `CTM_OCEAN_CHEM [default: True]`<a id=CTM_SS_AERO></a>   
    Use online sea spray aerosol emissions, halogen ozone chemistry, and enhanced ozone deposition over ocean waters. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#sea-spray) for further information.  
-   `CTM_LTNG_NO [default: Y]`<a id=CTM_LING_NO></a>  
    Y/N setting to activate lightning NO emissions. Setting this variable to Y requires additional variables to define the configuration of the lightning NO emissions calculation. See the settings for `LTNGNO`, `LTNGPARAMS`, `NLDN_STRIKES`, and `LTNGDIAG` below. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#lightning-no) for further information.
-   `KZMIN [default: Y]`<a id=KZMIN></a>  
    If KZMIN is set to Y, CCTM will read the urban land use fraction variable (PURB) from the GRID_CRO_2D meteorology file and use this information to determine the minimum eddy diffusivity in each grid cell. In CMAQv5, grid cells that are predominantly urban use a KZMIN value of 1.0 m<sup>2</sup>/s and non-urban cells use a value of 0.01 m<sup>2</sup>/s. If this variable is set to N, the PURB variable will not be used and a uniform KZMIN value of 1.0 m<sup>2</sup>/s will be used throughout the modeling domain.
-   `CTM_MOSAIC [default N]`<a id=CTM_MOSAIC></a>  
    Y/N setting to ouput land use specific deposition velocities and fluxes. This option is only available when using the STAGE deposition module. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#682-dry-depostion---stage) for further information.
-   `CTM_STAGE_P22 [default: N]`<a id=CTM_FST></a>  
   Y/N setting to select the land use specific implementation of the Pleim et al. 2022 aerosol deposition parameterization in the STAGE deposition option. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#682-dry-depostion---stage) for further information.
-   `CTM_STAGE_E20 [default: Y]`<a id=CTM_FST></a>  
   Y/N setting to select the land use specific and modal implementation of the Emerson et al. 2020 aerosol deposition parameterization in the STAGE deposition option. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#682-dry-depostion---stage) for further information.
-   `CTM_STAGE_S22 [default: N]`<a id=CTM_FST></a>  
   Y/N setting to select the land use specific implementation of the Shu et al. 2022 and v5.3 STAGE aerosol deposition parameterization in the STAGE deposition option. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#682-dry-depostion---stage) for further information.   
-   `PX_VERSION` <a id=PX_VERSION></a>
    Y/N setting to indicate whether the Pleim-Xiu land-surface model was used for the input meteorology. If this setting is set to Y the input meteorology data must include soil moisture (SOILM), soil temperature (SOILT), and soil type (ISLTYP) variables for use in the calculation of soil NO emissions. Additionally, the soil properties from PX will be used in the dust model and in the STAGE deposition module for calculating the soil compensation point for ammonia bidirectional exchange. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#682-dry-depostion---stage) for further information.
-   `CLM_VERSION` <a id=CLM_VERSION></a>
   Y/N setting to indicate whether the Community Land Model (CLM) land-surface model was used in generating the input meteorology. If this setting is set to Y the input meteorology data must include soil moisture (SOILM), soil temperature (SOILT), and soil type (ISLTYP) variables for use in the calculation of soil NO emissions.  Additionally, the soil properties from CLM will be used in the dust model and in the STAGE deposition module for calculating the soil compensation point for ammonia bidirectional exchange. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#682-dry-depostion---stage) for further information.
-    `NOAH_VERSION` <a id=NOAH_VERSION></a>
   Y/N setting to indicate whether the Noah land-surface model was used in generating the input meteorology. If this setting is set to Y the input meteorology data must include soil moisture (SOILM), soil temperature (SOILT), and soil type (ISLTYP) variables for use in the calculation of soil NO emissions.  Additionally, the soil properties from Noah will be used in the dust model and in the STAGE deposition module for calculating the soil compensation point for ammonia bidirectional exchange.  See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#682-dry-depostion---stage) for further information.
-   `CTM_ABFLUX [default: Y]`<a id=CTM_ABFLUX></a>  
    Y/N setting to activate fertilizer ammonia bidirectional flux for in-line emissions and deposition velocities. Setting this variable to Y requires four additional input files that include gridded fractional crop distributions (E2C_LU), soil properties (E2C_SOIL), fertilizer conditions (E2C_CHEM), and an agricultural soil initial conditions file (INIT_MEDC_1). Activation of this setting will produce additional variables in the output dry deposition file. 
-   `CTM_BIDI_FERT_NH3` <a id=CTM_BIDI_FERT_NH3></a>
    Y/N setting to indicate whether fertilizer NH3 should be subtracted from the emissions and handled instead by the NH3 bidirectional flux model.  Note that the bidirectional flux model must also be invoked by setting CTM_ABFLUX to Y.    
- `CTM_HGBIDI [default: N]`
     Y/N setting to activate mercury bidirectional flux for in-line emissions and deposition velocities. Activation of this setting will produce additional variables in the output dry deposition file. 
- `CTM_SFC_HONO [default: Y]`
     Y/N setting to include  surface HONO interactions.  See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#6.10.4_HONO) for further information. 
-   `CTM_GRAV_SETL [default Y]`<a id=CTM_GRAV_SETL></a>  
    Y/N setting to activate gravitational sedimentation for aerosols. 
-   `CTM_BIOGEMIS_BEIS [default: Y]`<a id=CTM_BIOGEMIS_BEIS></a>  
    Y/N setting to calculate biogenic emissions using BEIS. If this option is activated, several additional variables must be set (see the online biogenic emissions configuration settings). See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.
-   `CTM_BIOGEMIS_MEGAN [default: N]`<a id=CTM_BIOGEMIS_MEGAN></a>  
    Y/N setting to calculate biogenic emissions using MEGAN. If this option is activated, several additional variables must be set (see the online biogenic emissions configuration settings). See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.
-   `AEROSOL_OPTICS  [default: 3]`<a id=AEROSOL_OPTICS></a>  Determines how aerosol optical properies are calculated for the Inline Calculation of Photolysis Frequencies.      
    -- VALUES 1 thru 3 determined by using Uniformly Volume Mixed sphere for each aerosol mode   
    ---  1-Inline Tabular Method based on Mie Calculations over range of aerosol properties    
    ---  2-Solves Mie Theory using aerosol size distribution and mean refractive indices    
    ---  3-Approximations to Mie Theory based on Mie Parameters and mean refractive indices     
    --  VALUES 4 thru 6 attempts to use core-shell mixing model when the aerosol mode has signficant black carbon core otherwise uses Volume Mixed model where optics determined as    
    ---  4-Inline Tabular Method based on Mie Calculations     
    ---  5-Solves Mie Theory    
    ---  6-Approximations to Mie Theory    
-   `CTM_PVO3 [default: N]`<a id=CTM_PVO3></a>    
     Y/N determines whether to scale ozone in free-troposphere to potential vorticity. Option requires that METCRO3D file has PV, potential vorticity. See [User Guide 6.13](../CMAQ_UG_ch06_model_configuration_options.md#613-potential-vorticity-scaling) for more information.
-   `IC_AERO_M2USE [default: T]`<a id=IC_AERO_M2USE></a> Instructs CMAQ whether or not to use aerosol surface area from the Initial Condition file. If this option is set to false, then uniform diameter and standard deviation will be applied to each aerosol mode. If a particular simulation is a restart from a simulation preceeding in time (i.e. if this is any day after the first simulation day), then IC_AERO_M2USE is automatically set to True inside CMAQ.
-   `IC_AERO_M2WET [default: F=dry]`<a id=IC_AERO_M2WET></a> Instructs CMAQ whether or not to assume the initial condition surface area is consistent with dry or wet diameter. Note that most air quality models assume mode parameters are dry, and then will calculate wet diameter when needed (e.g. for deposition).
-   `BC_AERO_M2USE [default: T]`<a id=BC_AERO_M2USE></a> Instructs CMAQ whether or not to use aerosol surface area from the Boundary Condition file. If this option is set to false, then uniform diameter and standard deviation will be applied to each aerosol mode from the boundaries. 
-   `BC_AERO_M2WET [default: F=dry]`<a id=BC_AERO_M2WET></a> Instructs CMAQ whether or not to assume the boundary condition surface area is consistent with dry or wet diameter. Note that most air quality models assume mode parameters are dry, and then will calculate wet diameter when needed (e.g. for deposition). For more information about the IC_AERO and BC_AERO options, please see [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#6.11.1_Aero_BC)

<a id=Process_Analysis_Options></a>

### Process analysis options

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `CTM_PROCAN [default: N]`<a id=CTM_PROCAN></a>  
    Activate process analysis in the CCTM. Set the environment variable, PACM_INFILE (Read below for more information), that defines the integrated process rate and integrated reaction rate outputs from CCTM. Additional process analysis output files will be created when this setting is activated.  
-   `PA_BCOL_ECOL [default: 0]`<a id=PA_BCOL_ECOL></a>  
    Modeling grid domain column range for the process analysis calculations. Set to the two digits representing the beginning and ending column number bounding the process analysis domain.  
-   `PA_BROW_EROW [default: 0]`<a id=PA_BROW_EROW></a>  
    Modeling grid domain row range for the process analysis calculations. Set to the two digits representing the beginning and ending row number bounding the process analysis domain.  
-   `PA_BLEV_ELEV [default: 0]`<a id=PA_BLEV_ELEV></a>  
    Modeling grid domain layer range for the process analysis calculations. Set to the two digits representing the bottom and top layer numbers bounding the process analysis domain.  
-   `PACM_INFILE` <a id=PACM_INFILE></a>  
     Input file that specifies the desired output information (read by pa_read.F). See Table 1 in [Chapter 9](../CMAQ_UG_ch09_process_analysis.md) for details on the types of equations and operators that can be used in this file. A sample file is included in each of the mechanism folders under the CCTM/src/MECHS directory. For example, the file pa_cb6r3_ae7_aq.ctl in CCTM/src/MECHS/cb6r3_ae7_aq provides a template of IRR and IPR commands.
-   `PACM_REPORT` <a id=PACM_REPORT></a>  
     The output file that displays how CMAQ translates the variables listed in `PACM_INFILE`, and lists the reactions (including reactants, products and yields) that will be used in calculating the IPR and IRR values.  

<a id=I/O_Controls></a>

### I/O Controls

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `IOAPI_LOG_WRITE [default:False]`<a id=IOAPI_LOG_WRITE></a>  
    Set to T to turn on excess WRITE3 logging by the I/O API.  
-   `FL_ERR_STOP [default: False]`<a id=FL_ERR_STOP></a>  
    Set to T to configure the program to exit if inconsistent headers are found in the input files.  
-   `PROMPTFLAG [default: False]`<a id=PROMPTFLAG></a>  
    Turn on I/O-API PROMPTFILE interactive mode. Set to T to require interactive prompts for different I/O API operations.  
-   `IOAPI_OFFSET_64 [default: True]`<a id=IOAPI_OFFSET_64></a>  
    I/O API setting for large time step records. If your output time step is going to produce data that are >2GB per time step, then this needs to be set to YES.  

<a id=Aersol_Diagnostics_Controls></a>

### Aerosol Diagnostics Controls

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

Aerosol Diagnostics are now handled by the Explicit and Lumped Model Output module (ELMO), which is directed by the Emission Control Interface (ECI). See [Appendix B.6: ELMO](CMAQ_UG_appendixB_emissions_control.md) 

<a id=Diagnostic_Output_Flags></a>

### Diagnostic Output Flags

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `CTM_CKSUM [default: True]`<a id=CTM_CKSUM></a>  
    Write science processes summaries to the standard output. Impacts run speed and log file output size. 
-   `CLD_DIAG [default: False]`<a id=CLD_DIAG></a>  
    Output an hourly wet deposition diagnostic file (CTM_WET_DEP_2) that includes convective wet deposition estimates. 
-   `CTM_PHOTDIAG [default: False]`<a id=CTM_PHOTDIAG></a>  
    Output files for viewing the photolysis rates used in the model simulation and what meterological and other factors determined the rates. The inline and table options produce three files (`CTM_RJ_1`, `CTM_RJ_2` and `CTM_RJ_3`) and one file (`CTM_RJ_2`), respectively. `CTM_RJ_1` is a two dimensional file that contains key photolysis rates and radiative parameters. `CTM_RJ_2` contains the photolysis rates used over the model domain. `CTM_RJ_3` contains data used to calculate the photolysis rates.   
--   `NLAYS_PHOTDIAG [default: 1]` <a id=NLAYS_PHOTDIAG></a>: Number of layers in `CTM_RJ_2` and
     `CTM_RJ_3` files. Permitted values equal 1 to number of layers in model domain. Only the inline option uses this runtime option.  
--   `NWAVE_PHOTDIAG [default:294 303 310 316 333 381 607]` <a id= NWAVE_PHOTDIAG></a>: In 
     `CTM_RJ_3`, the wavelengths of diagnostic data written. The user can use or subset the default 
     values.  
-   `CTM_SSEMDIAG [default: False]`<a id=CTM_SSEMDIAG></a>  
    Output the calculated sea salt emissions to a diagnostic netCDF output file (CTM_SSEMIS_1). 
-   `CTM_DUSTEM_DIAG [default: False]`<a id=CTM_DUSTEM_DIAG></a>  
    Output the online dust emissions to a diagnostic netCDF output file (CTM_DUST_EMIS_1). The diagnostic file includes not only the total dust emissions, but also dust emissions by land use category and dust model parameters, such as gridded erodible land use fractions. 
-   `CTM_DEPV_FILE [default: False]`<a id=CTM_DEPV_FILE></a>  
    Output an hourly diagnostic file (CTM_DEPV_DIAG) for the inline deposition velocity calculations. 
-   `LTNGDIAG [default: False]`<a id=LTNGDIAG></a>  
    Output a lightning NO emissions diagnostics file. 
-   `CTM_WVEL [default: Y]`<a id=CTM_WVEL></a>  
    Y/N setting to output the CCTM-calculated vertical velocities to the CONC and ACONC file. 
    
<a id=Offline_Emissions_Config></a>

### Offline emissions configuration

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `N_EMIS_GR `<a id=N_EMIS_GR></a>  
    The number of offline gridded streams to be used by the model. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.
    
-   `GR_EMIS_### `<a id=GR_EMIS_###></a>  
    Directory path and file name of the gridded file for stream number ###, where ### = 001, 002,…,N_EMIS_GR. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.
    
-   `GR_EMIS_LAB_### `<a id=GR_EMIS_LAB_###></a>  
    Short label of the gridded file for stream ###, where ### = 001, 002,…,N_EMIS_GR. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information. 

-   `GR_EM_SYM_DATE_### [default: False]`<a id=GR_EM_SYM_DATE_###></a>  
    Switch to indicate whether gridded emission is of representative day type for stream ###, where ### = 01, 02,…,N_EMIS_GR. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.
    
-   `N_EMIS_PT `<a id=N_EMIS_PT></a>
    The number of offline Point emission streams to be used by the model. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.
  
-   `STK_GRPS_### `<a id=STK_GRPS_###></a>  
    Directory path and file name of the stack groups file for sector ###, where ### = 001, 002,…,N_EMIS_PT. Each ### refers to one of the inline plume rise point-source sectors. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information. 

-   `STK_EMIS_###`<a id=STK_EMIS_###></a>  
    Directory path and file name of the point emissions file for sector ###, where ### = 01, 02,…,N_EMIS_PT. Each ### refers to the one of the plume rise point-source sectors. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.  

-   `STK_EMIS_DIAG_###`<a id=STK_EMIS_DIAG_###></a>  
    Logical for turning on/off diagnostic output for point emissions file for sector ###, where ### = 01, 02,…,N_EMIS_PT. Each ### refers to the one of the plume rise point-source sectors. These data reflect the emission rates after scaling rules have been applied by DESID, the emissions control interface. Values for STK_EMIS_DIAG_### include FALSE, TRUE, 2D, 2DSUM, and 3D. The TRUE and 2D options are synonymous and will output just the surface layer of emissions. The 2DSUM option outputs a 2D file with values calculated from summing the entire column of emissions in each horizontal grid cell. The 3D option outputs a full 3D file. All options provide output across all output time steps during the simulation day. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.  
    
 -   `STK_EMIS_LAB_### `<a id=STK_EMIS_LAB_###></a>  
    Short label of the point emissions file for sector ###, where ### = 001, 002,…,N_EMIS_PT. Each ### refers to the one of the plume rise point-source sectors. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.
    
 -   `STK_EM_SYM_DATE_### [default: False]`<a id=STK_EM_SYM_DATE_###></a>  
    Switch to indicate whether point emission file is of representative day type for sector ###, where ### = 01, 02,…,N_EMIS_PT. Each ### refers to the one of the plume rise point-source sectors. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information. 
    
-   `EMIS_SYM_DATE [default: False]`<a id=EMIS_SYM_DATE></a>  
    The default for GR_EM_SYM_DATE_### and STK_EM_SYM_DATE_### if not set explicitly is false, however users have the option to set this default by setting this environment variable. Users should note, that if this variable is set and GR_EM_SYM_DATE_### or STK_EM_SYM_DATE_### is set, the individual stream switch takes precedent over this variable. This switch maybe useful if all offline emissions are of representative day type. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#inline-stream-offline) for further information.

<a id=Lightning_NOx_Config></a>

### Lightning NOx configuration

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `LTNGNO [default: "InLine"]`<a id=LTNGNO></a>  
    Setting to define whether the lightning emissions calculation will be inline or off-line. This variable can be set to a gridded netCDF file of lightning NO emissions to use emissions calculated with a preprocessor outside of CCTM. Setting this variable to “inline” activates the inline emissions calculation in CCTM and requires the LTNGPARMS_FILE variable (see below) to provide parameters for generating inline lightning NO emissions. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#lightning-no) for further information.  

-   `USE_NLDN [default: False]`<a id=USE_NLDN></a>  
    Use hourly NLDN or WWLLNs strikes data to compute inline lightning NO emissions. Activating this setting requires the NLDN_STRIKES input files (the files can be either NLDN hourly data or WWLLNs hourly data).  If USE_NLDN is set to N and LTNGNO set to "InLine", lightning NO emissions will be generated using parameters provided in the LTNGPARMS_FILE.  
    Lightning parameters netCDF file, which contains the linear regression parameters for generating lightning NO using the parameterization scheme when LTNGNO set to "InLine" and USE_NLDN set to N. In addition, it also contains the intercloud to cloud-to-ground flash ratios, scaling factors for calculating flashes using the convective precipitation rate, land-ocean masks, and the moles of NO per flash (cloud-to-ground and intercloud) which are used by both lightning production schemes (NLDN and parameterization). Ingore if LTINGNO set to an external input file. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#lightning-no) for further information.   

-  `CTM_LTNGDIAG_1`<a id=LTNGOUT></a>  
    Lightning diagnostics output 3D netCDF file; ignore if `LTNGDIAG = N`  

-  `CTM_LTNGDIAG_2`<a id=LTNGOUT></a>  
    Lightning diagnostics output 2D netCDF file (column total lightning NO emissions); ignore if `LTNGDIAG = N`  

<a id=Online_Bio_Config></a>
### Online biogenic emissions configuration

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `BEIS_SOILOUT [default: [Out Directory/CCTM_BSOILOUT_$RUNID_$TODAY]`<a id=SOILOUT></a>  
    Directory path and file name of biogenic NO soil emissions output file. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.   

-   `BEIS_SOILINP [default: [Out Directory/CCTM_BSOILOUT_$RUNID_$YESTERDAY]`<a id=SOILINP></a>  
    Directory path and file name of biogenic NO soil emissions input file. If NEW_START is set to N or F, the soil NO emissions file from the previous day's simulation will be a required input file. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.   

Options for use with BEIS:

-   `GSPRO [default: Build Directory]`<a id=GSPRO></a>  
    Directory path and file name for input ASCII speciation profiles. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information. 

-   `BEIS_NORM_EMIS [default: None]`<a id=BEIS_NORM_EMIS></a>  
    Grid-normalized biogenic emissions input netCDF file. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.  
    
-   `PX_VERSION [default: True]`<a id=PX_VERSION></a>  
    Setting to indicate whether the Pleim-Xiu land-surface model was used for the input meteorology. If this setting is set to Y the input meteorology data must include soil moisture (SOILM), soil temperature (SOILT), and soil type (ISLTYP) variables for use in the calculation of soil NO emissions.  

-   `B3GTS_DIAG [default: False]`<a id=B3GTS_DIAG></a>  
    Write the online biogenic emissions (mass units) to a diagnostic netCDF output file (B3GTS_S). 

-   `B3GTS_S [default: [Output Directory]/CCTM_B3GTS_$CTM_APPL.nc`<a id=B3GTS_S></a>  
    Diagnostic output netCDF file of biogenic emissions. This variable is ignored if B3GTS_DIAG is set to N.  

Options for use with MEGAN:

-   `USE_MEGAN_LAI [default: N]`<a id=USE_MEGAN_LAI></a>
    By default MEGAN will use the same leaf area index information as the rest of CMAQ. Toggle if a separate LAI dataset is desired. When this option is enabled the user must also set the environment variable MEGAN_LAI. 
    
-   `MEGAN_SOILOUT [default: [Out Directory/CCTM_MSOILOUT_$RUNID_$TODAY]`<a id=SOILOUT></a>  
    Directory path and file name of MEGAN's biogenic NO soil emissions output file. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.   

-   `MEGAN_SOILINP [default: [Out Directory/CCTM_MSOILOUT_$RUNID_$YESTERDAY]`<a id=SOILINP></a>  
    Directory path and file name of MEGAN's biogenic NO soil emissions input file. If NEW_START is set to N or F, or if IGNORE_SOILINP is set to F (Default), the soil NO emissions file from the previous day's simulation will be a required input file. See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.   
    
-   `IGNORE_SOILINP [default: N]`<a id=IGNORE_SOILINP></a>
    Similar to the obsolete INITIAL_RUN option for BEIS, this option allows a user to perform a CMAQ restart without needing an MEGAN_SOILINP file for the previous day. Instanteous values of shortwave radiation and surface temperature will be used instead of the previous daily average, and soil NO variables are set to their initialization values as for a new run. 

-   `USE_MEGAN_BDSNP [default: N ]`<a id=USE_MEGAN_BDSNP></a>
    Toggle to use the Berkeley-Dalhousie Soil NOx Parameterization (BDSNP) instead of the default option based on Yinger and Levy (1995). If the BDSNP option is activated, several additional variables must be set (see below). See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#biogenics) for further information.

-   `MEGAN_CTS`<a id=MEGAN_CTS></a>
    Points to canopy file that was created using the MEGAN preprocessor.

-   `MEGAN_EFS`<a id=MEGAN_EFS></a>
    Points to emission factor file that was created using the MEGAN preprocessor.

-   `MEGAN_LDF`<a id=MEGAN_LDF></a>
    Points to light dependent fraction file that was created using the MEGAN preprocessor.

-   `MEGAN_LAI`<a id=MEGAN_CTS></a>
    Optional. Points to leaf area index file that was created using the MEGAN preprocessor.

-   `BDSNP_AFILE`<a id=MEGAN_ARID></a>
    For BDSNP. Points to the ARID file that was created using the MEGAN preprocessor.

-   `BDSNP_NAFILE`<a id=MEGAN_NONARID></a>
    For BDSNP. Points to the NONARID file that was created using the MEGAN preprocessor.

-   `BDSNP_FFILE`<a id=MEGAN_FERT></a>
    For BDSNP. Points to the FERT file that was created using the MEGAN preprocessor.

-   `BDSNP_LFILE`<a id=MEGAN_FERT></a>
    For BDSNP. Points to the LANDTYPE file that was created using the MEGAN preprocessor.

-   `BDSNP_NFILE`<a id=MEGAN_NDF></a>
    For BDSNP. Points to the nitrogen deposition file that was created using the MEGAN preprocessor.


<a id=windblown_dust_config></a>

### Windblown dust emissions configuration

<!-- BEGIN COMMENT -->

[Return to Top](#TOC_A)

<!-- END COMMENT -->

-   `CTM_WB_DUST [default: False]`<a id=CTM_WB_DUST></a>  
    Setting to calculate online windblown dust emissions in CCTM. Requires additional setting of environmental variable `PX_VERSION` to "Y". See [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#wind-blown-dust) for further information.
    


<!-- BEGIN COMMENT -->

[<< Tables and Figures](../CMAQ_UG_tables_figures.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixB_emissions_control.md)<br>
CMAQv5.5 User's Guide <br>

 <!-- END COMMENT -->
