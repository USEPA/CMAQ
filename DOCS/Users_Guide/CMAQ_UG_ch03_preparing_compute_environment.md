
<!-- BEGIN COMMENT -->

 [<< Previous Chapter](CMAQ_UG_ch02_program_structure.md)- [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch04_model_inputs.md)

<!-- END COMMENT -->

# 3. Preparing Compute Environment for CMAQ Simulations

In this chapter the user will learn basic hardware and software requirements to run CMAQ. In addition, if the user does not have the required software, this chapter provides links to download the required software. 

## 3.1 Hardware Requirements

The suggested hardware requirements for running the CMAQ Southeast Benchmark case on a Linux workstation are:

-   8 processors
-   16 GB RAM
-   400 GB hard drive storage

However, to use CMAQ in a production environment where multiple iterations of the model will be executed for different spatial domains and/or emissions control strategies, either a cluster of multiprocessor PCs on a high-end network or an expandable rack-mounted Linux server is recommended.

For example, the CMAQ team at the EPA uses a Dell cluster. The cluster consists 128 nodes and each node contains two Intel Xeon E5-2697A v4 16-core processors (with a total of 4096 processors), 256 GB memory (8 GB/core), EDR infiniband interconnect and runs on Red Hat Enterprise Linux 7 operating system.

Table 3-1 provides a general snapshot of three different CMAQ setups for a day of simulation conducted at the EPA. The output only included: the concentration file (CONC), the average concentration file (ACONC), 3-D average concetration file (CGRID), hourly dry deposition file (DRYDEP), and wet deposition from the clouds file (WETDEP1). The run time and domain size are dictated by the system hardware. Furthermore, the run time may vary due to compiler choice and system load.

**Table 3‑1. Example of job scenarios at EPA**

|**Domain**|**Domain size**|**Species Tracked**| **Total output files size** | **Run time (# cores)**  | 
|------------|--------------------------|---|-------|--|
| Conus | 459 X 299 X 35 | 219 | 107GB | 2963.8s (128) |
| South-East | 100 X 80 X 35| 218 | 6.3GB | 493.9s (32) |
| Hemispheric | 187 X 187 X 44 | 255 | 40GB | 1518.1s (128) |

## 3.2 Software Requirements

In order to build the CMAQ program suite, user must first install these three libraries, MPI, netCDF and IOAPI. As always, we recommend using the latest release available at the time of your CMAQ installation.


## 3.2.1 Message Passing Interface (MPI) library

CMAQ is a MPI based program that runs on parallel programming platforms. Various flavour of MPI libraries are available for user to choose. CMAQ has been tested with the [OpenMPI](https://www.open-mpi.org), [MPICH](https://www.mpich.org/downloads), [MVAPICH2](http://mvapich.cse.ohio-state.edu), and the [Intel MPI](https://software.intel.com/en-us/intel-mpi-library) libraries. The choice of MPI affects model run time. For example, if you have Intel compiler suite available on your system, chooses Intel MPI or if your system is using InfiniBand (IB) interconnect, chooses MVAPICH2 which is tailored for IB.

User can download the MPI library source code from one of these sites and follow provided procedure for proper installation.

## 3.2.2 netCDF library

Most of the CMAQ input files (the rest is in ASCII format) and all output files are in netCDF format. Hence netCDF library is an essential component of the CMAQ model. Source code of the library is avaialbe at http://www.unidata.ucar.edu/software/netcdf/ and user should follow the instructions for proper installation. We recommend to install netCDF libray without **netCDF4** and **HDF5** support. In order to do so, user should provide appropriate flags, such as --disable-netcdf-4, at the configure stage.

## 3.2.3 IOAPI library

The IOAPI library provides interface to handle input and output (I/O) functions throughout the CMAQ code. We recommend to download IOAPI 3.2 library which is used in stand alone CMAQ, WRF-CMAQ two-way coupled model, and CMAQ code with parallel I/O feature (a slightly different library is used, see explanation below). Download the latest version of IOAPI 3.2 source code from the CMAS Center.

**COMMENT we have test the latest IOAPI 3.2 and it failed with parallel I/O function turns on in CMAQ

Procedure to install "basic" IOAPI library (this is based on ifort compiler, for other compiler, look for corresponding Linux2_x86_64*):

step 1: mkdir ioapi_3.2
step 2: untar the downloaded source code inside ioapi_3.2 directory, tar xfz io*gz
step 3: setenv BIN Linux2_x86_64gfort
step 4: comment out the line with BASEDIR and make sure nocpl option is used
step 5: setenv BASEDIR $PWD
step 6: comment out all openMP options and remove -Bstatic flag in ioapi/Makeinclude.Linux2_x86_64gfort
step 7: make configure
step 8: make

Procedure to install "mpi" IOAPI library which is for CMAQ model with parallel I/O feature (this is a continuation from the above steps):

step 9: setenv BIN Linux2_x86_64gfortmpi
step 10: comment out all openMP options and remove -Bstatic flag in ioapi/Makeinclude.Linux2_x86_64gfortmpi
step 11: make configure
step 12: make

## 3.3 Optional Software

**Table 3‑2. Optional support software for CMAQ**

|**Software**|**Description**|     **Source**    |
|------------|-------------------------------|---------------------------------------------|
|***Evaluation and visualization tools***| | |
|VERDI|Visualization Environment for Rich Data Interpretation for graphical analysis of netCDF gridded data|[<http://www.verdi-tool.org>](http://www.verdi-tool.org/)|
|PAVE|Package for Analysis and Visualization of Environmental data for graphical analysis of netCDF gridded data|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|IDV|Integrated Data Viewer for 3-D graphical analysis of netCDF gridded data|[<http://www.unidata.ucar.edu/software/idv/>](http://www.unidata.ucar.edu/software/idv/)|
|I/O API Tools|Postprocessing tools for manipulating data in the I/O API/netCDF format|[<https://www.cmascenter.org/ioapi/>](https://www.cmascenter.org/ioapi/)|
|netCDF Tools|Postprocessing tools for manipulating data in the netCDF format|[<http://my.unidata.ucar.edu/content/software/netcdf/index.html>](http://my.unidata.ucar.edu/content/software/netcdf/index.html)|
| ***Source code diagnostics*** |
|GDB|Gnu Fortran debugger|[<https://www.sourceware.org/gdb/>](https://www.sourceware.org/gdb/)|
|PGDBG|Portland Group Fortran debugger|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|PGPROF|Portland Group Fortran code profiler|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|IDB|Intel Fortran debugger|[<https://software.intel.com/en-us/articles/idb-linux>](https://software.intel.com/en-us/articles/idb-linux)|



<!-- BEGIN COMMENT -->

 [<< Previous Chapter](CMAQ_UG_ch02_program_structure.md)- [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch04_model_inputs.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
