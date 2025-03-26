
<!-- BEGIN COMMENT -->

 [<< Previous Chapter](CMAQ_UG_ch02_program_structure.md)- [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch04_model_inputs.md)

<!-- END COMMENT -->

# 3. Preparing Compute Environment for CMAQ Simulations

## 3.1 Introduction

In this chapter the user will learn basic hardware and software requirements to run CMAQ. In addition, if the user does not have the required software, this chapter provides links to download the required software. 

## 3.2 Hardware Requirements

The suggested hardware requirements for running the CMAQ Northeast Benchmark case on a Linux workstation are:

-   8 processors
-   4 GB RAM
-   400 GB hard drive storage

However, to use CMAQ in a production environment where multiple iterations of the model will be executed for different spatial domains and/or emissions control strategies, either a cluster of multiprocessor PCs on a high-end network or an expandable rack-mounted Linux server is recommended.

For example, the CMAQ team at the EPA uses a Dell cluster. The cluster consists 128 nodes and each node contain two Intel Xeon E5-2697A v4 16-core processors (with a total of 4096 processors), 256 GB memory (8 GB/core), EDR InfiniBand interconnect and runs on Red Hat Enterprise Linux 8 operating system.

Table 3-1 provides a general snapshot of three different CMAQ setups for a day of simulation conducted at the EPA. The output only included: the concentration file (CONC), the average concentration file (ACONC), 3-D average concentration file (CGRID), hourly dry deposition file (DRYDEP), and wet deposition from the clouds file (WETDEP1). The run time and domain size are dictated by the system hardware. Furthermore, the run time may vary due to compiler choice and system load.

<a id=Table3-1></a>

**Table 3‑1. Example of job scenarios at EPA for a single day simulation**

|**Domain**|**Domain size**|**Species Tracked**|**Input files size**|**Output files size**| **Run time (# cores)**  | 
|:--------------:|:----:|:-:|:-:|:-:|:--------:|
| 2016 Southeast US | 100 X 80 X 35| 218 |6.7GB |6.3GB |8 min/day (32); 47 min/day (4) |
| 2016 CONUS | 459 X 299 X 35 | 219 |18GB| 107GB | 50 min/day (128); 90 min/day (32) |
| 2016 N. Hemisphere | 187 X 187 X 44 | 255 |15GB| 40GB | 25 min/day (128) |
| 2018 North East US | 100 X 105 X 35 | 225 |26GB| 2GB | 15 min/day (32) |


## 3.3 Software Requirements

To build the CMAQ program suite, users must install these libraries in the order listed: MPI, netCDF and IOAPI. As always, we recommend using the latest release available at the time of your CMAQ installation. A table of the software versions CMAQ has been developed and tested with is shown below: 

<a id=Table3-2></a>

**Table 3‑2. CMAQ Development and Testing software versions**

|**Software**|**Versions**|
|:--------------:|:----:|
| Intel Compiler | 18.0, 21.4  | 
| GNU Compiler | 6.1.0, 9.1, 12.2 | 
| PGI Compiler | 17.4, 21.9, 22.11 |

**NOTE: The CMAQ team recommends using a single compiler suite when building these libraries. Mixing compiler suites when building these libraries can cause unexpected behavior (e.g., mixing intel 18.0 to build netCDF C libraries and gcc 6.1.0 to build netCDF fortran libraries may lead to compile time errors).** 

### 3.3.1 Message Passing Interface (MPI) library

CMAQ is primarily a MPI based programming system that runs on parallel programming platforms. Many programs within the CMAQ system require a flavor of MPI installed on your machine. CMAQ has been tested with the [OpenMPI](https://www.open-mpi.org), [MPICH](https://www.mpich.org/downloads), [MVAPICH2](http://mvapich.cse.ohio-state.edu), and the [Intel MPI](https://software.intel.com/en-us/intel-mpi-library) libraries. The choice of MPI library may affect model run time. For example, if you have the Intel compiler suite available on your system, you may want to choose Intel MPI or if your system is using InfiniBand (IB) interconnects, choose MVAPICH2 which is tailored for IB.

Users can download the MPI library source code from one of these sites and follow provided procedures for proper installation. **Versions Tested: IntelMPI 2017.0, 21.4 | MPICH 3.3.1 | MVAPICH2 2.3.1 | OpenMPI 2.1.0, 4.1.4**

### 3.3.2 netCDF library

Most of the CMAQ input files and all output files are in netCDF format (the rest are in ASCII format). Hence the netCDF library is an essential component of the CMAQ model. The netCDF library is available for download at http://www.unidata.ucar.edu/software/netcdf/; users should follow the instructions for proper installation of **both** netCDF-C and netCDF-Fortran libraries. After successful installation, check the environment PATH & LD_LIBRARY_PATH to ensure that the paths have been updated to include the path of the netCDF C and Fortran libraries and bin. Note you may have to set these paths manually if not set, and these paths must be loaded every time you start a new shell. For additional installation resources plesae see the [CMAQ Tutorial Page](./Tutorials/README.md). **Versions Tested: NetCDF-C 4.2, 4.8.1 | NetCDF-Fortran 4.4.2, 4.5.3**

### 3.3.3 I/O API library

The I/O API library provides an interface between the netCDF libraries and CMAQ, as well as WRF-CMAQ, to handle input and output (I/O) calls throughout the CMAQ code. The version of the I/O API library supported with CMAQv5.3.2+ (version 3.2 tagged 20200828) is available for download at https://github.com/cjcoats/ioapi-3.2/releases/tag/20200828. Users should note that the I/O API library requires netCDF files to be adhere to a strict formatting guidelines that can be found in the I/O API documentation. For simplicity, files following the IOAPI-netCDF formatting guidelines will be called "IOAPI FILES" from now on. **Versions Tested: IOAPI 3.2 tagged 20200828**

The general steps for installation of I/O API libraries on a Linux system (with C-shell and GNU compilers) are below. These instructions are an example. 

The following is a procedure to install "basic" I/O API libraries (this is based on gfortran compiler, for other compilers, look for corresponding Linux2_x86_64*):

```
mkdir ioapi_3.2
cd ioapi_3.2

## Download IOAPI Libraries and untar downloaded source code in this directory
wget http://github.com/cjcoats/ioapi-3.2/archive/20200828.tar.gz
tar -xzvf 20200828.tar.gz
cd ioapi-3.2-20200828

### Set up your Linux system environment
setenv BIN Linux2_x86_64gfort
setenv BASEDIR $cwd
setenv CPLMODE nocpl
cp Makefile.template Makefile
touch ioapi/Makefile
touch m3tools/Makefile
```

Edit the top level Makefile with the following steps:

1. comment out the line with NCFLIBS=
2. Add explicit netCDF C and Fortran library paths in front of -lnetcdf -lnetcdff, respectively, the following is an example:

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

Other I/O API library configuration options are available, and users can see a list of these options within the I/O API documentation. For example, I/O API can be configured in a manner that allows the CMAQ model to be run with the parallel I/O (PIO) feature turned on called the "mpi" I/O API libraries (Wong et al. 2015). More information about how to enable PIO within CMAQ can be found in [Appendix D.3](Appendix/CMAQ_UG_appendixD_parallel_implementation.md#d3-parallel-io). 

There is also an I/O API version 3.2 "large" that is designed for applications with a large number of model output files (e.g. utilizing all of CMAQv5.3+ optional diagnostic output files) and/or a large number of model variables (e.g. CMAQ-HDDM or CMAQ-ISAM applications). I/O API v3.2-large increases the MXFILE3 variable from 256 to 512 and increases the MXVARS3 variable from 2048 to 16384, both found in PARAMS3.EXT, as noted in the [I/O API documentation](https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html#build). Users can [build](https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html#build) a copy of the large version by using: 

```
cp -r ioapi-3.2-20200828 ioapi-3.2-20200828_large
cd ioapi-3.2-20200828_large/ioapi/fixed_src
cp ../PARMS3-LARGE.EXT ./PARMS3.EXT
mv ../PARMS3-LARGE.EXT ../PARMS3.EXT
```

This version is also available as a zip file from the following address:

https://www.cmascenter.org/ioapi/download/ioapi-3.2-large-20200828.tar.gz

Installation instructions for I/O API v3.2-large are provided in README.txt in the .tar.gz file. 

**Note: Users using the I/O API v3.2-large will require additional computional resources during compile and runtime to account for the increase in memory footprint. Additionally, users may encounter upward and backwards compatability issues using different versions of I/O API, if the files are produced with this version of I/O API.**


## 3.4 Optional Software

<a id=Table3-3></a>

**Table 3‑3. Optional support software for CMAQ**

|**Software**|**Description**|     **Source**    |
|------------|-------------------------------|---------------------------------------------|
|***Evaluation and visualization tools***| | |
|VERDI|Visualization Environment for Rich Data Interpretation for graphical analysis of netCDF gridded data|[<https://www.cmascenter.org/verdi/>](https://www.cmascenter.org/verdi/)|
|PAVE|Package for Analysis and Visualization of Environmental data for graphical analysis of netCDF gridded data|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|IDV|Integrated Data Viewer for 3-D graphical analysis of netCDF gridded data|[<http://www.unidata.ucar.edu/software/idv/>](http://www.unidata.ucar.edu/software/idv/)|
|I/O API Tools|Postprocessing tools for manipulating data in the I/O API/netCDF format|[<https://www.cmascenter.org/ioapi/>](https://www.cmascenter.org/ioapi/)|
|netCDF Tools|Postprocessing tools for manipulating data in the netCDF format|[<http://my.unidata.ucar.edu/content/software/netcdf/index.html>](http://my.unidata.ucar.edu/content/software/netcdf/index.html)|
| ***Source code diagnostics*** |
|GDB|Gnu Fortran debugger|[<https://www.sourceware.org/gdb/>](https://www.sourceware.org/gdb/)|
|PGDBG|Portland Group Fortran debugger|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|PGPROF|Portland Group Fortran code profiler|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|IDB|Intel Fortran debugger|[<https://software.intel.com/en-us/articles/idb-linux>](https://software.intel.com/en-us/articles/idb-linux)|

## 3.5 References:

Wong, D. C., Yang, C. E., Fu, J. S., Wong, K., and Gao, Y., “An approach to enhance pnetCDF performance in environmental modeling applications”, Geosci. Model Dev., 8, 1033-1046, 2015.

<!-- BEGIN COMMENT -->

 [<< Previous Chapter](CMAQ_UG_ch02_program_structure.md)- [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch04_model_inputs.md)<br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->
