## CMAQ Tutorial ##
### Running the CMAQ Test Case (Benchmarking) ###
Purpose: This tutorial describes how to run the CMAQ test case and use the result to verify the installation of the software.

CMAQv5.2 is distributed with a complete set of input files required for running the CCTM. These data provide useful examples of the input files needed to run the CCTM. They can also be used to `benchmark` new installations of the software.

Benchmarking refers to a simulation that is used to verify that the software is installed correctly.  Benchmarking CMAQ is recommended in the following circumstances:
- Installation on a new server     
- Following kernel upgrades
- Following Fortran compiler upgrades
- Following netCDF or I/O API library upgrades

This tutorial assumes you have already downloaded, installed, and compiled the CMAQ model on your server. For further instructions on those processes, see [Chapter 5 of the CMAQ Operational Guidance Document (OGD)](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/User_Manual/CMAQ_OGD_ch05_sys_req.md).

### Download CMAQ Test Data

- Download CMAQ test input data by navigating to https://www.cmascenter.org/ and logging into the site using the “Log In” shortcut on the top horizontal menu.
- Click the “Software” pulldown menu on the horizontal menu bar and choose “CMAQ”.
- Click “DOWNLOAD” on the right-hand side of the page and choose CMAQv5.2, platform, and compiler for your machine and click submit.
- Choose the Base Model release package and click submit. This page will display links for the source code, benchmark data and utilities.
- Click "Download" for the CMAQ benchmark input data and CMAQ benchmark output data.


### Install CMAQ Test Data

Put the CMAQ test data into the CMAQ_HOME directory on your Linux system. From the CMAQv5.2 installation directory, use the following commands to install the data:

```
source config.cmaq
cd $CMAQ_HOME
mv /path of downloaded data/DATA.CMAQv5.2.tar.gz .
mv /path of downloaded data/DATA_REF.CMAQv5.2.tar.gz .
tar xvzf DATA.CMAQv5.2.tar.gz
tar xvzf DATA_REF.CMAQv5.2.tar.gz
```

### Install CMAQ and Required Libraries

In the directory where you would like to install CMAQ, create the directory issue the following command to clone the EPA GitHub repository for CMAQv5.2:

```
git clone -b 5.2 https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

For instructions on installing CMAQ from tarballs, see [Chapter 5](CMAQ_OGD_ch05_sys_req.md).

#### Configure the CMAQ build environment

*1.* By default, this script will copy configuration, build and run scripts from the repo. An arbitrary folder can be selected by the user by modifying the `CMAQ_WORK` variable in extract_scripts.csh.

*2.* Install the CMAQ libraries and specify their location in the config_cmaq.csh script.

The CMAQ build scripts require the following libraries and INCLUDE files to be available in the CMAQ_LIB directory (Note the CMAQ_LIB gets set automatically by the config_cmaq.csh script, where `CMAQ_LIB = $CMAQ_HOME/lib`):

- netCDF library and INCLUDE files are located in the `$CMAQ_LIB/netcdf` directory
- I/O API library and module files are located in the `$CMAQ_LIB/ioapi` directory
- MPI library and INCLUDE files are located in the `$CMAQ_LIB/mpi` directory

The config_cmaq.csh script will automatically link the required libraries into the CMAQ_LIB directory. Set the locations of the netCDF, I/O API, and MPI installations on your Linux system with the following config_cmaq.csh environment variables:

- `setenv IOAPI_MOD`: the location of the I/O API module files on your system.
- `setenv IOAPI_INCL`: the location of the I/O API include files on your system.
[1] 14370                                                                              1,1           Top
- `setenv IOAPI_LIB`: the location of compiled I/O API libraries on your system.       
- `setenv NETCDF`: the location of the netCDF installation on your system.
- `setenv MPI`: the location of the MPI (OpenMPI or MVAPICH) on your system.

For example, if your netCDF libraries and includes files are installed in /usr/local/netcdf, set `NETCDF` to /usr/local/netcdf. Similarly, if your I/O API library is installed in /home/cmaq/ioapi/Linux2_x86_64ifort, set `IOAPI_LIB` to /home/cmaq/ioapi/Linux2_x86_64ifort.

*3.* Check the names of the I/O API and netCDF libraries using the `ioapi_lib` and `netcdf_lib` script variables.

*4.* Check the name of the MPI library using the `mpi` script variable. For MVAPICH use `-lmpich`; for openMPI use `-lmpi`.

Links to these libraries will automatically be created when you run any of the build or run scripts. To manually (this is optional) create these libraries, execute the config_cmaq.csh script, identifying the compiler in the command line [intel | gcc | pgi]:

```
./config_cmaq.csh [compiler]
```


### Build the preprocessor executables

```
cd $CMAQ_WORK/PREP/bcon/scripts
./bldit.icon [compiler]
```

```
cd $CMAQ_WORK
./bldit.bcon [compiler]
```

### Run the preprocessor executables

To run the test simulation for the various CMAQ preprocessor programs, change directories to the location of each program and execute the run script.

Run ICON to produce initial conditions:

```
cd $CMAQ_WORK/PREP/icon/scripts
./run.icon |& tee icon.log
```

Run BCON to produce boundary conditions:

```
cd $CMAQ_WORK/PREP/bcon/scripts
./run.bcon |& tee bcon.log
```

Check the ICON and BCON log file to ensure that the programs completed successfully. Note that CMAQ test simulation "doesn't" actually require that ICON and BCON be run; the test input data include CCTM-ready initial and boundary conditions files.

### Build the CMAQ executable

```
cd $CMAQ_WORK
./bldit_cctm.csh [compiler]
```

### Run the CCTM Benchmark Script

The default CCTM script in the CMAQ installation is configured to run the benchmark case. You will need to have compiled the CMAQ model builder (Bldmake) and installed the I/O API, netCDF, and MPI libraries before preceding with this step (See [CMAQ OGD Chapter 5](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/User_Manual/CMAQ_OGD_ch05_sys_req.md)).  Use the following commands to run the CCTM benchmark script:

```
cd $M3HOME/scripts/cctm
run.cctm |& tee run.benchmark.log
```

### Confirm that the Benchmark Simulation Completed

To confirm that the benchmark case ran to completion view the run.benchmark.log file. A successful run will contain the following line at the bottom of the log:

``>>---->  Program completed successfully  <----<<``

The benchmark output results will have been placed in $CMAQ_DATA/cctm/ and should include fifteen netCDF-type files: ACONC, AERODIAM, AEROVIS, B3GTS_S, CGRID, CONC, DEPV, DRYDEP, MEDIA_CONC, PHOTDIAG1, PHOTDIAG2, SOILOUT, SSEMIS, WETDEP1, and WETDEP2.

Common errors in a CCTM simulation include the following:
- Incorrect paths to input files. Look in the CCTM screen output (capture in your log file) for an Error message about an input file not being found.  
- Incorrect MPI implementation. A series of MPI errors at the end of the log file often indicate that the MPI job was not submitted correctly.   

Check the last few lines of the CCTM output log for messages to help diagnose why the simulation did not complete.

### Check the CMAQ Benchmark Results

To determine if CMAQ is correctly installed on your Linux system compare the results from your benchmark simulation to the reference data downloaded from the CMAS Center. These data were generated on a Linux system with the following specifications:
- Red Hat Enterprise Linux Server release 5.11 (Tikanga)
- Linux Kernel 2.6.18-238.12.1.el5 x86_64
- Intel v15.0 compiler, 8 processors with OpenMPI

The CMAQv5.2 reference data include output from BCON, ICON, JPROC, and the CCTM. You will only need to compare the results for the CCTM to evaluate the benchmark results.

Use your netCDF evaluation tool of choice to evaluate your benchmark results. For example, [VERDI](https://www.verdi-tool.org/) is a visualization tool to view CCTM results as tile plots. Statistical comparison of the results can be made with the I/O API Tools or R. [Chapter 12 of the CMAQ OGD](https://github.com/USEPA/ECMAQ/blob/5.2/CCTM/docs/User_Manual/CMAQ_OGD_ch12_analysis_tools.md) lists descriptions of various analysis software options for viewing CMAQ output.

In general, if the largest domain-wide and simulation period differences between your simulation and the reference data are <1%, the model is considered to be successfully benchmarked (i.e., the installation is verified).
