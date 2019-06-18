
<!-- BEGIN COMMENT -->

 [<< Previous Chapter](CMAQ_UG_ch02_program_structure.md)- [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch04_model_inputs.md)

<!-- END COMMENT -->

# 3. Preparing Compute Environment for CMAQ Simulations

In this chapter the user will learn basic hardware and software requirements to run CMAQ. In addition, if the user does not have the required software, this chapter provides links to download the required software. 

## 3.1 Hardware Requirements

The suggested hardware requirements for running the CMAQ Southeast Benchmark case on a Linux workstation are:

-   8 processors
-   4 GB RAM
-   400 GB hard drive storage

However, to use CMAQ in a production environment where multiple iterations of the model will be executed for different spatial domains and/or emissions control strategies, either a cluster of multiprocessor PCs on a high-end network or an expandable rack-mounted Linux server is recommended.

For example, the CMAQ team at the EPA uses a Dell cluster. The cluster consists 128 nodes and each node contains two Intel Xeon E5-2697A v4 16-core processors (with a total of 4096 processors), 256 GB memory (8 GB/core), EDR infiniband interconnect and runs on Red Hat Enterprise Linux 7 operating system.

Table 3-1 provides a general snapshot of three different CMAQ setups for a day of simulation conducted at the EPA. The output only included: the concentration file (CONC), the average concentration file (ACONC), 3-D average concetration file (CGRID), hourly dry deposition file (DRYDEP), and wet deposition from the clouds file (WETDEP1). The run time and domain size are dictated by the system hardware. Furthermore, the run time may vary due to compiler choice and system load.

**Table 3‑1. Example of job scenarios at EPA for a single day simulation**

|**Domain**|**Domain size**|**Species Tracked**|**Input files size**|**Output files size**| **Run time (# cores)**  | 
|:--------------:|:----:|:-:|:-:|:-:|:--------:|
| Southeast US | 100 X 80 X 35| 218 |6.7GB |6.3GB |8 min/day (32); 47 min/day (4) |
| CONUS | 459 X 299 X 35 | 219 |18GB| 107GB | 50 min/day (128); 90 min/day (32) |
| N. Hemisphere | 187 X 187 X 44 | 255 |15GB| 40GB | 25 min/day (128) |


## 3.2 Software Requirements

In order to build the CMAQ program suite, users must install these libraries: MPI, netCDF and IOAPI. As always, we recommend using the latest release available at the time of your CMAQ installation.

## 3.2.1 Message Passing Interface (MPI) library

CMAQ is a MPI based program that runs on parallel programming platforms. Various flavour of MPI libraries are available for users to choose. CMAQ has been tested with the [OpenMPI](https://www.open-mpi.org), [MPICH](https://www.mpich.org/downloads), [MVAPICH2](http://mvapich.cse.ohio-state.edu), and the [Intel MPI](https://software.intel.com/en-us/intel-mpi-library) libraries. The choice of MPI library may affect model run time. For example, if you have Intel compiler suite available on your system, chooses Intel MPI or if your system is using InfiniBand (IB) interconnect, chooses MVAPICH2 which is tailored for IB.

Users can download the MPI library source code from one of these sites and follow provided procedures for proper installation.

## 3.2.2 netCDF library

Most of the CMAQ input files (the rest are in ASCII format) and all output files are in netCDF format. Hence netCDF library is an essential component of the CMAQ model. The netCDF library is avaialbe for download at http://www.unidata.ucar.edu/software/netcdf/ and users should follow the instructions for proper installation. Users should install **classic shared netCDF C and Fortran libraries only without netCDF4, HDF5, HDF4, DAP client, and PnetCDF, or zlib support.** In order to do so, users should provide the appropriate flags to build and install minimal netCDF-3 with no DAP client support, such as --disable-netcdf-4 and --disable-dap, at the configure stage for netCDF C .

## 3.2.3 IOAPI library

The IOAPI library provides an interface between the netCDF libraries and CMAQ to handle input and output (I/O) calls throughout the CMAQ code. The lastest version of the IOAPI library (version 3.2) is available for download at https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html#v32. Users should note that the IOAPI library requires netCDF files to be adhere to a strict formatting guidelines that can be found in the IOAPI documentation. For simplicity, files following the IOAPI-netCDF formatting guidelines will be called "IOAPI FILES" from now on.

**>>COMMENT<<**  Testing the latest IOAPI 3.2 library from CMAS is incompatible with the parallel I/O turned on. 

The general steps for installation of IOAPI libraries on a Linux system (with C-shell and GNU compilers) are below. These instructions are an example and we recommend using the latest release available at the time of your CMAQ installation.

The following is a procedure to install "basic" IOAPI libraries (this is based on gfortran compiler, for other compilers, look for corresponding Linux2_x86_64*):

```
mkdir ioapi_3.2
cd IOAPI_3.2

## Download IOAPI Libraries and untar downloaded source code in this directory
tar xvzf ioapi-3.2.tar.gz 

### Set up your Linux system environment
setenv BIN Linux2_x86_64gfort
```

Edit the top level Makefile with the following steps:

1. comment out the line with BIN ="
2. Add explicit netCDF C and Fortran libray paths in front of -lnetcdf -lnetcdff, respectively , the following is an example:

```
NCFLIBS = -L/usr/local/apps/netcdf-c-4.7.0/gcc-9.1.0/lib -lnetcdf -L/usr/local/apps/netcdf-fortran-4.4.5/gcc-9.1.0/lib -lnetcdff
```

Edit the file in the ioapi folder called Makeinclude.Linux2_x86_64gfort to comment out all openMP options as CMAQ does not support openMP. Note: If users are using the ifort compiler you also need to remove -Bstatic flag within the ioapi/Makeinclude.Linux2_x86_64ifort file as well.

```
OMPFLAGS = # -fopenmp 
OMPLIBS = # -fopenmp
```

In the top level IOAPI_3.2 directory run: 

```
make configure
make
```

Other IOAPI library configuration options are available and users can see a list of these options within the IOAPI documentation. For example, IOAPI can be configured in a manner that allows the CMAQ model to be run with the parallel I/O (PIO) feature turned on called the "mpi" IOAPI libraries. More information about how to enable PIO within CMAQ can be found in [Appendix D](Appendix/CMAQ_UG_appendixD_parallel_implementation.md). The procedure to install "mpi" IOAPI libraries is shown below (this is a continuation from the above steps): 

```
setenv BIN Linux2_x86_64gfortmpi
```

Edit the file in the ioapi folder called Makeinclude.Linux2_x86_64gfortmpi to comment out all openMP options as CMAQ does not support openMP. Note: If users are using the ifort compiler you also need to remove -Bstatic flag within the ioapi/Makeinclude.Linux2_x86_64ifortmpi file as well.

```
OMPFLAGS = # -fopenmp 
OMPLIBS = # -fopenmp
```

In the top level IOAPI_3.2 directory run: 
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
