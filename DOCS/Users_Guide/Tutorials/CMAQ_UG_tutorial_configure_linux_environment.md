# Configuring your compute environment for CMAQ simulations

CMAQ is a comprehensive air quality model that requires a multiprocessor computing system and a number of software packages to produce sound estimates of ozone, particulate matter and a number of species. If you are interested in using CMAQ please make sure your computing system meets the following hardware and software requirements. 

## Hardware Requirements

### Operating System

The CMAQ system will require that you have access to a Unix-based or Unix-like computer system. Common distrbutions include Liunx (e.g., Red-Hat, Ubuntu, CentOs, Debian, Fedora, etc) and BSD variations (MacOS, FreeBSD, OpenBSD, etc). 

### Memory Requirements 

The memory requirements needed to perform a CMAQ simulation will vary with what you are trying to simulate. Benchmark simulations require 16 GB RAM along with 200 GB of disk space, where as typical simulations may require several hundred GB of RAM along with multiple TB of disk space, depending on domain and output specification. 

### Compatibility with Cloud Service Providers

Depending on the ability to access resources with considerable computing power, users should consider running CMAQ on a cloud service provider (CSP). There are several advantages of migrating a users workflow to CSPs, a few include access to on-demand HPC computing resources and availability of canned versions of CMAQ on the cloud with all necessary software pre-built. 

More information on how to use CMAQ on the cloud can be found on the [CMAQ on AWS webpage](https://pcluster-cmaq.readthedocs.io/en/latest/).

## Software Requirements

The following support software are required for compiling and running CMAQ.

1. Fortran and C compilers, e.g., [Intel](https://software.intel.com/en-us/fortran-compilers), [Portland Group](http://www.pgroup.com), [Gnu](https://gcc.gnu.org/wiki/GFortran)
2. [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
2. Message Passing Interface (MPI), e.g., [OpenMPI](https://www.open-mpi.org) or [MVAPICH2](http://www.mcs.anl.gov/research/projects/mpich2).
3. Latest release of [netCDF-C](https://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html) and [netCDF-Fortran](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html).
   - Building netCDF4 libraries that disable compression with GNU compiler ([Tutorial](CMAQ_UG_tutorial_build_library_gcc.md) | [Script](./scripts/gcc_11.4_install_netcdf_classic.csh))
   - Building netCDF4 libraries that disable compression with INTEL compiler ([Tutorial](CMAQ_UG_tutorial_build_library_intel.md))
   - Building netCDF4 libraries that enable compression with GNU compiler ([Tutorial](CMAQ_UG_tutorial_build_library_gcc_support_nc4.md) | [Script](./scripts/cmaq_libraries/gcc_11.4_install_netcdf_for_nc4_compression.csh))
   - Building netCDF4 libraries that enable compression with INTEL compiler ([Tutorial](CMAQ_UG_tutorial_build_library_intel_support_nc4.md) | [Script](./scripts/intel_18.2_install_netcdf_for_nc4_compression.csh))
5. [I/O API](https://github.com/cjcoats/ioapi-3.2)
   - Building I/O API libraries ([Intel Compiler Script](./scripts/intel_18.2_install_netcdf_for_nc4_compression.csh) | [GNU Compiler Script](./scripts/cmaq_libraries/gcc_11.4_install_ioapi_for_nc4_compression.csh))








