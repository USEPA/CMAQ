
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
|:------------:|:-----:|:---:|:-------:|:--:|
| Conus | 459 X 299 X 35 | 219 | 107GB | 2963.8s (128) |
| South-East | 100 X 80 X 35| 218 | 6.3GB | 493.9s (32) |
| Hemispheric | 187 X 187 X 44 | 255 | 40GB | 1518.1s (128) |

## 3.2 Software Requirements

In order to build the CMAQ program suite, users must install these libraries: MPI, netCDF and IOAPI. As always, we recommend using the latest release available at the time of your CMAQ installation.

## 3.2.1 Message Passing Interface (MPI) library

CMAQ is a MPI based program that runs on parallel programming platforms. Various flavour of MPI libraries are available for users to choose. CMAQ has been tested with the [OpenMPI](https://www.open-mpi.org), [MPICH](https://www.mpich.org/downloads), [MVAPICH2](http://mvapich.cse.ohio-state.edu), and the [Intel MPI](https://software.intel.com/en-us/intel-mpi-library) libraries. The choice of MPI library may affect model run time. For example, if you have Intel compiler suite available on your system, chooses Intel MPI or if your system is using InfiniBand (IB) interconnect, chooses MVAPICH2 which is tailored for IB.

Users can download the MPI library source code from one of these sites and follow provided procedures for proper installation.

## 3.2.2 netCDF library

Most of the CMAQ input files (the rest are in ASCII format) and all output files are in netCDF format. Hence netCDF library is an essential component of the CMAQ model. The netCDF library is avaialbe for download at http://www.unidata.ucar.edu/software/netcdf/ and users should follow the instructions for proper installation. Users should install classic netCDF C and Fortran libraries without **netCDF4** and **HDF5** support. In order to do so, users should provide the appropriate flags, such as --disable-netcdf-4 and --disable-dap, at the configure stage.

## 3.2.3 IOAPI library

The IOAPI library provides an interface between the netCDF libraries and CMAQ to handle input and output (I/O) calls throughout the CMAQ code. The lastest version of the IOAPI library (version 3.2) is available for download at https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html#v32. Users should note that the IOAPI library requires netCDF files to be adhere to a strict formatting guideline 

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

The I/O API library is used for the input/output data in CMAQ.

Download the I/O API source code from the CMAS Center and put the gzipped tarball on your Linux system. The general steps for installation on a Linux system (with C-shell, GCC and GFortran) are below. These instructions are an example and we recommend using the latest release available at the time of your CMAQ installation.

```
### Set up your Linux system environment
setenv BIN Linux2_x86_64gfort

## Install I/O API
tar xvzf ioapi-3.2.tar.gz
cp Makefile.template Makefile
```
Edit the Makefile with the following steps:

1. comment out the line with BIN ="
2. add explicit netCDF libray path in front of -lnetcdf -lnetcdff, the following is an example.

```
NCFLIBS = -L/usr/local/apps/netcdf-4.4.1/intel-17.0/lib -lnetcdf -lnetcdff
```

Also you need to edit ioapi/Makeinclude.${BIN} to comment out openmp option. Here is an example with Linux2_x86_64gfort:

```
OMPFLAGS = # -fopenmp OMPLIBS = # -fopenmp
```

If you are using ifort compiler, you need to remove -Bstatic in ioapi/Makeinclude.Linux2_x86_64ifort as well.

```
make configure
make
```
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
