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
3. Message Passing Interface (MPI), e.g., [OpenMPI](https://www.open-mpi.org) or [MVAPICH2](http://www.mcs.anl.gov/research/projects/mpich2).
4. Latest release of [netCDF-C](https://docs.unidata.ucar.edu/nug/current/getting_and_building_netcdf.html).
5. Latest release of [netCDF-Fortran](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html). 
6. [I/O API version 3.2 **tagged 20200828**](https://github.com/cjcoats/ioapi-3.2/releases/tag/20200828).

## Tutorials and scripts for building netCDF and I/O API libraries for CMAQ

The following set of tutorials provide step by step instructions on how to install netCDF and I/O API software using various compilers. We also provide shell scripts that include commands to install the libraires from source. The scripts will require edits to specify where the libraries should be installed. We recommend looking at the tutorials before using the shell scripts.

We recommend using the intel compiler to build netCDF with compression (bullet 1 or 2). If this is not possible on your system, we provide other options (bullets 3 - 5). 

  - Building netCDF4 and I/O API libraries that enable compression with Intel - 20.2 compiler ([Tutorial](CMAQ_UG_tutorial_build_library_intel_support_nc4.md) | [netCDF Script](./scripts/cmaq_libraries/intel_20.2_install_netcdf_for_nc4_compression.csh) | [I/O API Script](./scripts/cmaq_libraries/intel_20.2_install_ioapi_for_nc4_compression.csh))
  
  - Building netCDF4 and I/O API libraries that enable compression with Intel - 2024 compiler ([Tutorial](CMAQ_UG_tutorial_build_library_intel_support_nc4.md) | [netCDF Script](./scripts/cmaq_libraries/intel_2024_install_netcdf_for_nc4_compression.csh) | [I/O API Script](./scripts/cmaq_libraries/intel_2024_install_ioapi_for_nc4_compression.csh))

  - Building netCDF4 and I/O API libraries that enable compression with GNU compiler ([Tutorial](CMAQ_UG_tutorial_build_library_gcc_support_nc4.md) | [netCDF Script](./scripts/cmaq_libraries/gcc_11.4_install_netcdf_for_nc4_compression.csh) | [I/O API Script](./scripts/cmaq_libraries/gcc_11.4_install_ioapi_for_nc4_compression.csh))
  
  - Building netCDF and I/O API libraries that disable nc4 compression with Intel - 18.2 compiler ([Tutorial](CMAQ_UG_tutorial_build_library_intel.md) | [netCDF Script](./scripts/cmaq_libraries/intel_18.2_install_netcdf_classic.csh) | [I/O API Script](./scripts/cmaq_libraries/intel_18.2_install_ioapi_classic.csh))
  
  - Building netCDF and I/O API libraries that disable nc4 compression with GNU compiler ([Tutorial](CMAQ_UG_tutorial_build_library_gcc.md) | [netCDF Script](./scripts/cmaq_libraries/gcc_11.4_install_netcdf_classic.csh) | [I/O API Script](./scripts/cmaq_libraries/gcc_11.4_install_ioapi_classic.csh))                    


## Use custom environment modules or LMOD to manage the different library versions.<br>

Adding the libraries locations to the LD_LIBRARY_PATH, PATH, MANPATH and other environment variables can be managed using custom modules.
<br>
[Custom Modules using environment modules](https://researchcomputing.princeton.edu/support/knowledge-base/custom-modules)
<br>
[Custom Modules using Lmod](https://lmod.readthedocs.io/en/latest/)
