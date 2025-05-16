# CMAQ Installation & Benchmarking Tutorial for CB6R5 

Purpose: This guide describes how to run the CMAQ test case for the CB6R5 mechanism with the M3DRY dry deposition scheme, which serves two different purposes. The first being to familiarize the user with the CMAQ suite of programs and how they work together, and secondly to verify the installation of the software on your system via benchmarking. 

Benchmarking refers to a simulation that is used to verify that the software is installed correctly.  Benchmarking CMAQ is recommended in the following circumstances:
- Installation by a new user
- Installation on a new server     
- Following kernel upgrades
- Following compiler or system library updates
  
## System Checks 

CMAQ requires a specific hardware and software configuration. To learn about these requirements, please refer to the tutorial on [preparing your compute environment for CMAQ simulations](CMAQ_UG_tutorial_configure_linux_environment.md).

## Install CMAQ 

In the directory where you would like to install CMAQ, create the directory issue the following command to clone the EPA GitHub repository for CMAQv5.5:

```
git clone -b main https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

For instructions on installing CMAQ from Zip files, see [Chapter 5](../CMAQ_UG_ch05_running_a_simulation.md).

## Check Out a new Branch in the CMAQ Repository 

Checking out a new branch is a good idea even if you are not doing code development, per se. It is likely that you will want to retrieve new updates in the future, and an easy way to do this is through the main branch in the git repo. Thus, it is beneficial to leave it unperturbed if possible.
```
cd CMAQ_REPO
git checkout -b my_branch
```

## Configure the CMAQ build environment

The user has two options for building an environment. She or he may build and run CMAQ components directly in the repository structure (object files and executables will be ignored with .gitignore), or they may extract the build and run scripts out of the repository and work in a separate location. If you would like to build directly in the repository, skip to "Link the CMAQ Libraries" below.

### Build and run in a user-specified directory outside of the repository
In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

In bldit_project.csh, modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:
```
set CMAQ_HOME = [your_install_path]/CMAQ_v5.5
```

Now execute the script.
```
./bldit_project.csh
```

## Link the CMAQ Libraries
The CMAQ build scripts require the following libraries and INCLUDE files to be available in the CMAQ_LIB directory (Note: the CMAQ_LIB gets set automatically by the config_cmaq.csh script, where `CMAQ_LIB = $CMAQ_HOME/lib`): 

- netCDF C library files are located in the `$CMAQ_LIB/netcdf/lib` directory
- netCDF Fortran library files are located in the `$CMAQ_LIB/netcdff/lib` directory
- I/O API library, include files and module files are located in the `$CMAQ_LIB/ioapi` directory
- MPI library and INCLUDE files are located in the `$CMAQ_LIB/mpi` directory

The config_cmaq.csh script will automatically link the required libraries into the CMAQ_LIB directory. Set the locations of the netCDF, I/O API, and MPI installations on your Linux system with the following config_cmaq.csh environment variables:

- `setenv IOAPI_INCL_DIR`: the location of the I/O API include header files on your system.
- `setenv IOAPI_LIB_DIR`: the location of compiled I/O API libraries on your system.
- `setenv NETCDF_LIB_DIR`: the location of the netCDF C library installation on your system.
- `setenv NETCDF_INCL_DIR`: the location of the netCDF C include files on your system.
- `setenv NETCDFF_LIB_DIR`: the location of the netCDF Fortran library installation on your system.
- `setenv NETCDFF_INCL_DIR`: the location of the netCDF Fortran include files on your system.
- `setenv MPI_LIB_DIR`: the location of the MPI (OpenMPI or MVAPICH) on your system.

For example, if your netCDF C libraries are installed in /usr/local/netcdf/lib, set `NETCDF_LIB_DIR` to /usr/local/netcdf/lib. Similarly, if your I/O API library is installed in /home/cmaq/ioapi/Linux2_x86_64gfort, set `IOAPI_LIB_DIR` to /home/cmaq/ioapi/Linux2_x86_64gfort. 

*1.* Check the names of the I/O API and netCDF libraries using the `ioapi_lib` and `netcdf_lib` script variables.

*2.* Check the name of the MPI library using the `mpi_lib` script variable. For MVAPICH use `-lmpich`; for openMPI use `-lmpi`.

Links to these libraries will automatically be created when you run any of the build or run scripts. To manually create these libraries (this is optional), execute the config_cmaq.csh script, identifying the compiler in the command line [intel | gcc | pgi]:
```
source config_cmaq.csh [compiler] 
```
You may also identify the version of the compiler if you wish it to be identified in build directory and executable names. This is optional. For example:
```
source config_cmaq.csh gcc 9.5
```


## Install the CMAQ reference input and output benchmark data

Download the CMAQ two day reference input and output data for the cb6r5_ae7 mechanism (using inputs from CMAQv5.4 Benchmark release) from the [CMAS Center Data Warehouse Amazon Web Services S3 Bucket](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html#v5_5/): CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz	and output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz. The CMAQ benchmark test case is a two day simulation for July 1-2 2018 on a 100 column x 105 row x 35 layer 12-km resolution domain over the northeast U.S.  

Download and copy the data to `$CMAQ_DATA`. Navigate to the `$CMAQ_DATA` directory, unzip and untar the two day benchmark input and output files:

```
cd $CMAQ_DATA
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz
tar -xzvf CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz
mkdir ref_output
cd ref_output
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz
tar -xzvf output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz
```

*Note that there is also benchmark output data for CMAQv5.5 with CB6r5 and the STAGE dry deposition module. Look for output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_stage.tar.gz in the AWS link above.* 

## Compiling CMAQ 

*Before proceeding, it should be noted that building the ICON and BCON executables are optional steps when working specifically with the benchmark data. This is because the initial condition and boundary condition files have been provided for you within the benchmark data set. For further information on these preprocessors please reference [Chapter 4](../CMAQ_UG_ch04_model_inputs.md).*   

Create the model executables for CCTM using the steps shown below. 

Create a bldit_cctm script for this benchmark and verify or modify the settings listed below.

```
cp bldit_cctm.csh bldit_cctm_cb6r5_m3dry.csh
vi bldit_cctm_cb6r5_m3dry.csh
```

##### Configuration for multi-processor runs (default):

```
cd $CMAQ_HOME/CCTM/scripts
cp bldit_cctm.csh bldit_cctm_cb6r5_ae7_aq_m3dry.csh
```

Edit the bldit_cctm_cb6r5_ae7_aq_m3dry.csh script to use the following settings:


```
set ParOpt #>  Option for MPI Runs
````

##### Configuration for single-processor runs (optional):

For single-processor computing, edit the CCTM build script (bldit_cctm.csh) to indicate a single-processor run by commenting out set ParOpt as shown below. 

```
#set ParOpt #> Option for Single Processor Runs
````

#### Configure CMAQ benchmark Science Modules:

The build directory parameters for the benchmark test case include the following:

-   Multiprocessor simulation 
-   3-D Advection Scheme: wrf_cons
-   Horizontal diffusion: Multiscale
-   Vertical diffusion: ACM2_M3Dry
-   Deposition: M3Dry
-   Chemistry solver: EBI
-   Aerosol module: AERO7 
-   Cloud module: ACM_AE7
-   Mechanism: cb6r5_ae7_aq
-   Inline biogenic emissions
-   Inline plume rise

To configure these parameters, the CCTM Science Modules within the bldit_cctm.csh need to be set. The comments within the script itself should help guide the user on the options for each variable and how to set them. Further information on variable names can be found in 
[Appendix A](../Appendix/CMAQ_UG_appendixA_model_options.md).

Verify that the dry deposition scheme to use M3DRY
```
#> Set Dry Deposition Scheme to m3dry 

 set DepMod    = m3dry
```

*Note that there is reference benchmark output for both the M3DRY and STAGE dry deposition schemes.  To try a simulation using STAGE simply change this model setting to set DepMod  =stage.*

Following the requisite changes to the CCTM build script, use the following command to create the CCTM executable: 

```
cd $CMAQ_HOME/CCTM/scripts
./bldit_cctm_cb6r5_ae7_aq_m3dry [compiler] [version] |& tee bldit_cctm_cb6r5_ae7_aq_m3dry.log
```

Verify that the BLD directory contains a namelist called

```
cd BLD_CCTM_v55_gcc_cb6r5_ae7_aq_m3dry
ls CMAQ_Control_DESID_cb6r5_ae7_aq.nml
```


## Configure the CCTM script 

For an MPI configuration with 32 processors,

```
cd $CMAQ_HOME/CCTM/scripts
```

Edit the CCTM run script (run_cctm_Bench_2018_12NE3_CB6R5.csh) for the MPI configuration and compiler that you will use:

```
setenv compiler gcc
setenv compilerVrsn 9.5
setenv INPDIR  ${CMAQ_DATA}/2018_12NE3
@ NPCOL 8 ; @ NPROW = 4
```

Most clustered multiprocessor systems require a command to start the MPI run-time environment. The default CCTM run script uses the *mpirun* command. Consult your system administrator to find out how to invoke MPI when running multiprocessor applications.

For single-processor computing, set PROC to serial:

```
set PROC     = serial
```

CCTM Science Configuration Options set to **Y** in the RunScript for the benchmark case include the following: 

-  ```CTM_OCEAN_CHEM``` - use ocean halgoen chemistry and sea spray aerosol emissions
-  ```KZMIN``` - minimum eddy diffusivity in each grid cell determined by land use fraction
-  ```PX_VERSION``` - WRF PX land surface model 
-  ```CTM_ABFLUX``` - bidirectional ammonia flux for online deposition velocities
-  ```CTM_BIDI_FERT_NH3``` - subtract fertilizer NH3 from emissions because it will be handled by the BiDi calculation
-  ```CTM_SFC_HONO``` - surface HONO interaction
-  ```CTM_GRAV_SETL``` - vdiff aerosol gravitational sedmentation
-  ```CTM_BIOGEMIS``` - online biogenic emissions

To configure these parameters, the Science Options within the $CMAQ_HOME/CCTM/scripts/run_cctm_Bench_2018_12NE3_CB6R5.csh need to be set. The comments within the script itself should help guide the user on the options for each variable and how to set them. Further information on variable names can be found in 
[Appendix A](../Appendix/CMAQ_UG_appendixA_model_options.md).

After configuring the MPI settings for your Linux system, check the rest of the script to ensure the correct path, date and names are used for the input data files. Per the note above, different Linux systems have different requirements for submitting MPI jobs.  The command below is an example of how to submit the CCTM run script and may differ depending on the MPI requirements of your Linux system. 

```
./run_cctm_Bench_2018_12NE3_CB6R5.csh |& tee cctm.log
```

## Confirm that the Benchmark Simulation Completed

To confirm that the benchmark case ran to completion view the run.benchmark.log file. For MPI runs, check each of the CTM_LOG_[ProcessorID]*.log files. A successful run will contain the following line at the bottom of the log(s):

``>>---->  Program completed successfully  <----<<``

Note: If you are running on multiple processors the log file for each processor is also moved from the $CMAQ_HOME/CCTM/scripts directory to the benchmark output directory: 

```
$CMAQ_DATA/output_CCTM_v55_[compiler]_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry
```
and these log files have the name convention: 

```
CTM_LOG_[ProcessorID].v55_[compiler]_[APPL]_[YYYYMMDD]
CTM_LOG_[ProcessorID].v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180702
```

The benchmark output results will have been placed in the directory: 

```
$CMAQ_DATA/output_CCTM_v55_[compiler]_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry
```

and can include upto 23 netCDF-type files: ACONC, AELMO, B3GTS_S, BSOILOUT, BUDGET, CGRID, CONC, DEPV, DRYDEP, DUSTEMIS, LTNGCOL, LTNGHRLY, MEDIA_CONC, PHOTDIAG1, PHOTDIAG2, ELMO, SOILOUT, SSEMIS, VDIFF, VSED, WETDEP1, and WETDEP2.


Common errors in a CCTM simulation include the following:
- Incorrect paths to input files. Look in the CCTM screen output (capture in your log file) for an Error message about an input file not being found.  
- Incorrect MPI implementation. A series of MPI errors at the end of the log file often indicate that the MPI job was not submitted correctly.   

Check the last few lines of the CCTM output log for messages to help diagnose why the simulation did not complete.

## Check the CMAQ Benchmark Results

To determine if CMAQ is correctly installed on your Linux system compare the results from your benchmark simulation to the reference output data downloaded from the CMAS Center. This data was generated on a Linux system with the following specifications:
- Linux Kernel 3.10.0-514.el7.x86_64
- Red Hat Enterprise Linux Server 7.3 (Maipo) (use command: cat /etc/os-release)
- GNU GCC compiler version 9.1.0, 16 processors with OpenMPIv4.0.1 and I/O APIv3.2 tagged version 20200828
- Debug mode turned off (```set Debug_CCTM``` commented out in $CMAQ_HOME/CCTM/scripts/bldit_cctm.csh)
- CMAQv5.5

The CMAQv5.5 reference output data includes a set of CCTM_ACONC_\*.nc files with layer 1 average model species concentrations for each model hour for 226 variables and a set of CCTM_WETDEP1_\*.nc files with cumulative hourly wet deposition fluxes for an additional 136 variables. 

Use your netCDF evaluation tool of choice to evaluate your benchmark results. For example, [VERDI](https://www.verdi-tool.org/) is a visualization tool to view CCTM results as tile plots. Statistical comparison of the results can be made with the I/O API Tools or R. 

Note, even with a successful installation and run of the benchmark case, some differences between your simulation and the reference data can occur due to differences in domain decomposition for multi-processor simulations as well as differences in compiler.  These differences tend to manifest in upper layers of the model and are mostly found in predicting aerosol water (AH2O) and aerosol acidity (AH3OP), while differences are smaller for other key species like ASO4, ANO3, ACL, ALOO1, etc. These species have short atmospheric lifetimes with large changes in time and space derivatives or have model physics sensitive to small changes in concentration. Predicting these species is more sensitive to small changes in machine precision and accuracy.

