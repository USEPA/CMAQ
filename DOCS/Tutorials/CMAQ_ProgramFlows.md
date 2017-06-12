## CMAQ Tutorial ##
### Getting Started ###
Purpose: This tutorial describes the steps required to prepare your Linux system for installing and running CMAQ.

----

Several steps are required to prepare your Linux system for compiling and running CMAQ. Complete all of these steps before installing CMAQ.

Several of these steps are facilitated by having either root or sudo privileges on your system.  If you do not have these privileges, work with the system administrator to complete these steps.

1. Install Fortran and C compilers
-----
There are several options for Fortran and C compilers. The [Fortran wiki](http://fortranwiki.org/fortran/show/Compilers) includes a full list of free and commercial Fortran compilers.

Most Linux systems include the Gnu gfortran compiler by default. Other options that have been tested with CMAQ include the **Intel, Portland Group, and Absoft** compilers.

The following instructions are for Gnu compilers.  For other Fortran compilers, follow the instructions from the vendor.

Check for the Gnu gfortran compiler:

```
$ gfortran -v
```

Install on Debian/Ubuntu Linux:

```
$ sudo apt-get update
$ sudo apt-get install build-essential
$ sudo apt-get install gfortran
```

Install on Red Hat/CentOS Linux:

```
$ sudo yum update
$ sudo yum install gcc-gfortran
```

2. Install Git
-----
Git is a source code control and management software.  It is used in the CMAQ installation to download stable versions of the source code.

Install on Debian/Ubuntu Linux:

```
$ sudo apt-get install git
```

Install on Red Hat/CentOS Linux:

```
$ sudo yum install git
```

3. Download the source and install the netCDF library
-----
The netCDF library is used to control the data structures of the CMAQ input/output files.

Download the latest netCDF release from http://www.unidata.ucar.edu/software/netcdf/ and follow the instructions to install on your system.  

** Compile netCDF without netCDF4 and HDF5 support. **

The general steps for installation on a Linux system with, C-shell, GCC and Gfortran are below.  These instructions are an example and we recommend using the latest release available at the time of your CMAQ installation.

```
### Set up your Linux system environment
$ setenv CC = gcc
$ setenv CPPFLAGS = -DNDEBUG –DgFortran
$ setenv CFLAGS = -O
$ setenv FC = gfortran
$ setenv FFLAGS = -O –w
$ setenv CXX = g++

## Install netCDF-C
$ wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4.4.1.1.tar.gz
$ tar xvzf netcdf-4.4.1.1.tar.gz
$ cd netcdf-4.4.1.1
$ ./configure --disable-netcdf-4 --disable-dap
$ make check install
$ cd ../

## Install netCDF-Fortran
$ setenv NCDIR {location of netCDF C installation}
$ setenv CPPFLAGS "$CPPFLAGS -I${NCDIR}/include LDFLAGS=-L${NCDIR}/lib"
$ wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-4.4.4.tar.gz
$ tar xvzf netcdf-4.4.4.tar.gz
$ cd netcdf-4.4.4
$ ./configure
$ make check
$ make install
```
Additional instruction and options for compiling netCDF are on the [Unidata C netCDF installation](http://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html) and [Fortran netCDF installation](http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html) pages.

4. Download the source and install the I/O API library
-----
The I/O API library is used for the input/output data flows in CMAQ.

Download the I/O API source code from the CMAS Center and put the gzipped tarball on your Linux system. The general steps for installation on a Linux system with, C-shell, GCC and Gfortran are below. These instructions are an example and we recommend using the latest release available at the time of your CMAQ installation.

```
### Set up your Linux system environment
$ setenv BIN Linux2_x86_64gfort

## Install I/O API
$ tar xvzf ioapi-3.1.tar.gz
$ cd ioapi
$ cp Makefile.nocpl Makefile
```

Edit the Makefile and change the variable BASEDIR to point to the I/O API installation directory on your Linux system. After saving the file run the command make to build the library:

```
make
```

5. Install a Message Passing Interface (MPI) library
-----
The MPI library is used to create parallel (multiple processor) applications of CMAQ.  CMAQ has been tested with the [OpenMPI](https://www.open-mpi.org), [MVAPICH2](http://mvapich.cse.ohio-state.edu), and the [Intel MPI](https://software.intel.com/en-us/intel-mpi-library) libraries. This example shows how to install OpenMPI on a Linux system with, C-shell, GCC and Gfortran are below. These instructions are an example and we recommend using the latest release available at the time of your CMAQ installation.

nstall on Debian/Ubuntu Linux:

```
$ sudo apt-get install openmpi openmpi-devel
```

Install on Red Hat/CentOS Linux:

```
$ sudo yum install openmpi openmpi-devel
```
