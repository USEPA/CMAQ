
<!-- BEGIN COMMENT -->

 [<< Previous Chapter](CMAQ_UG_ch02_program_structure.md)- [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch04_model_inputs.md)

<!-- END COMMENT -->

# 3. Preparing to Run CMAQ

In this chapter the user will learn basic hardware and software requirements to run CMAQ. In addition, if the user does not have the required software, this chapter provides detailed examples on how the user could obtain this software in a linux environment.

## 3.1 Hardware Requirements

The minimum hardware requirements for running the CMAQ Program Suite on a PC are:

-   A single processor
-   1 GB RAM
-   100 GB hard drive storage

However, to use CMAQ in a production environment where multiple iterations of the model will be executed for different spatial domains and/or emissions control strategies, either a cluster of multiprocessor PCs on a high-end network or an expandable rack-mounted Linux server is recommended.

For example, the CMAQ team at the EPA uses a Dell cluster. The cluster consists 128 nodes and each node contains two Intel Xeon E5-2697A v4 16-core processors (with a total of 4096 processors), 256 GB memory (8 GB/core), EDR infiniband interconnect and runs on Red Hat Enterprise Linux 7 operating system

## 3.2 Software Requirements
**>>Comment<<** Do we want to also recommend a compiler (intel) for speed?

Before using the CMAQ program suite, the user must first download and configure their "computing" environment. A requirement for CMAQ is access to a linux environment, which can be done through a terminal emulator. Examples of terminal emulators are shown below:

-   **MAC OS X:** iTerm2
-   **Windows:** PuTTY, MobaXterm
-   **Linux:** Terminal, XTerm

When using the CMAQ program suite a common theme that will be seen is the use of shell scripts. In short, a shell script is basically a recipe of commands that can be run to save time when performing a task. In the case of CMAQ, it is used to set variables, science options and run other programs.

At this point faimiliarity with linux command line prompts as well as basic shell scripting is expected.

Several of these steps are facilitated by having either root or sudo privileges on your system.  If you do not have these privileges, work with the system administrator to complete these steps.

## 3.3 Install Fortran and C compilers
-----
There are several options for Fortran and C compilers. The [Fortran wiki](http://fortranwiki.org/fortran/show/Compilers) includes a full list of free and commercial Fortran compilers.

Most Linux systems include the Gnu gfortran compiler by default. Other options that have been tested with CMAQ include the **Intel and Portland Group** compilers.

The following instructions are for Gnu compilers.  For other Fortran compilers, follow the instructions from the vendor.

Check for the Gnu gfortran compiler:

```
gfortran -v
```

Install on Debian/Ubuntu Linux:

```
sudo apt-get update
sudo apt-get install build-essential
sudo apt-get install gfortran
```

Install on Red Hat/CentOS Linux:

```
sudo yum update
sudo yum install gcc-gfortran
```

[Makefiles](https://www.gnu.org/software/make/manual/make.pdf) are similar to shell scripts in the sense that they describe the relationship between Fortran/C files and provides a recipe on how to link and compile them to build an executable. This is convenient because if one file changes in the program suite, the Makefile will recompile and link all the necessary programs by running the commands below.

Common shell commands to invoke the Makefile include:

``
make
``

This shell command invokes the Makefile and follows the recipe on how to compile and link the program files listed to update the CMAQ executable.

``
make clean
``

This shell command cleans the directory (i.e. removes any links between files and any intermediary files).

## 3.4 Install Git
-----
This step is optional. User can skip this step if the user perfers to download CMAQ tarball, work from there and does not care about version control.

Git is a source code control and management software.  It is used in the CMAQ installation to download stable versions of the source code. Additional resources can be found at https://git-scm.com/book/en/v2.

Install on Debian/Ubuntu Linux:

```
$ sudo apt-get install git
```

Install on Red Hat/CentOS Linux:

```
$ sudo yum install git
```

## 3.5 Download the source and install the netCDF library
-----
The netCDF library is used to control the data structures of the CMAQ input/output files.

Download the latest netCDF release from http://www.unidata.ucar.edu/software/netcdf/ and follow the instructions to install on your system.  

**Compile netCDF without netCDF4 and HDF5 support**

The general steps for installation on a Linux system (with C-shell, GCC and GFortran) are below.  These instructions are an example and we recommend using the latest release available at the time of your CMAQ installation.

```
### Set up your Linux system environment
setenv CC gcc
setenv CPPFLAGS  '-DNDEBUG –DgFortran'
setenv CFLAGS  -O
setenv FC  gfortran
setenv FFLAGS  '-O –w'
setenv CXX  g++

## Install netCDF-C
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4.4.1.1.tar.gz
tar xvzf netcdf-4.4.1.1.tar.gz
cd netcdf-4.4.1.1
./configure --disable-netcdf-4 --disable-dap
make check install
cd ../

## Install netCDF-Fortran
setenv NCDIR {location of netCDF C installation}
setenv CPPFLAGS "$CPPFLAGS -I${NCDIR}/include LDFLAGS=-L${NCDIR}/lib"
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-4.4.4.tar.gz
tar xvzf netcdf-4.4.4.tar.gz
cd netcdf-4.4.4
./configure
make check
make install
```
Additional instruction and options for compiling netCDF are on the [Unidata C netCDF installation](http://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html) and [Fortran netCDF installation](http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html) pages.

## 3.6 Download the source and install the I/O API library
-----
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
   
   NCFLIBS    = -L/usr/local/apps/netcdf-4.4.1/intel-17.0/lib -lnetcdf -lnetcdff
   
Also you need to edit ioapi/Makeinclude.${BIN} to comment out openmp option. Here is an example with Linux2_x86_64gfort:

OMPFLAGS  = # -fopenmp
OMPLIBS   = # -fopenmp

```
make configure
make

```

## 3.7 Install a Message Passing Interface (MPI) library
-----
The MPI library is used to create parallel (multiple processor) applications of CMAQ.  CMAQ has been tested with the [OpenMPI](https://www.open-mpi.org), [MVAPICH2](http://mvapich.cse.ohio-state.edu), and the [Intel MPI](https://software.intel.com/en-us/intel-mpi-library) libraries. This example shows how to install OpenMPI on a Linux system with C-shell, GCC and Gfortran. These instructions are an example and we recommend using the latest release available at the time of your CMAQ installation.

Install on Debian/Ubuntu Linux:

```
sudo apt-get install openmpi openmpi-devel
```

Install on Red Hat/CentOS Linux:

```
sudo yum install openmpi openmpi-devel
```

## 3.8 Optional Software

**Table 3‑3. Optional support software for CMAQ**

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
