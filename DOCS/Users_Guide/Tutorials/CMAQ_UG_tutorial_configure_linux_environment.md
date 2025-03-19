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
6. [I/O API](https://github.com/cjcoats/ioapi-3.2).

## Tutorials and scripts for building libraries and CMAQ


   - [Tutorial](CMAQ_UG_tutorial_build_library_gcc.md) building netCDF and I/O API libraries that disable compression with GNU compiler 

     | gcc 11.4           |   script  | 
     |  ----              |  ------     |
     | netCDF    | [gcc_11.4_install_netcdf_classic.csh](./scripts/cmaq_libraries/gcc_11.4_install_netcdf_classic.csh) |
     | I/O API   | [gcc_11.4_install_ioapi_classic.csh](./scripts/cmaq_libraries/gcc_11.4_install_ioapi_classic.csh) | 
     | CMAQv5.5  | [gcc_11.4_install_cmaq55_cb6r5_m3dry_classic.csh](./scripts/cmaq_libraries/gcc_11.4_install_cmaq55_cb6r5_m3dry_classic.csh) |

   - [Tutorial](CMAQ_UG_tutorial_build_library_intel.md) building netCDF and I/O API libraries that disable compression with INTEL compiler 

     | intel 18.2   |    script |
     |  ----              |  ------     |
     | netCDF | [intel_18.2_install_netcdf_classic.csh](./scripts/cmaq_libraries/intel_18.2_install_netcdf_classic.csh) |
     | I/O API | [intel_18.2_install_ioapi_classic.csh](./scripts/cmaq_libraries/intel_18.2_install_ioapi_classic.csh) |                         
     | CMAQv5.5 | [intel_18.2_install_cmaq55_cb6r5_m3dry_classic.csh](./scripts/cmaq_libraries/intel_18.2_install_cmaq55_cb6r5_m3dry_classic.csh) |

   - [Tutorial](CMAQ_UG_tutorial_build_library_gcc_support_nc4.md) Building netCDF4 and I/O API libraries that enable compression with GNU compiler 

     | intel 18.2   |    script |
     |  ----              |  ------     |
     | netCDF | [gcc_11.4_install_netcdf_for_nc4_compression.csh](./scripts/cmaq_libraries/gcc_11.4_install_netcdf_for_nc4_compression.csh) |
     | I/O API | [gcc_11.4_install_ioapi_for_nc4_compression.csh](./scripts/cmaq_libraries/gcc_11.4_install_ioapi_for_nc4_compression.csh) |                          
     | CMAQv5.5 | [gcc_11.4_install_cmaq55_cb6r5_m3dry_classic.csh](./scripts/cmaq_libraries/gcc_11.4_install_cmaq55_cb6r5_m3dry_classic.csh) |

   - [Tutorial](CMAQ_UG_tutorial_build_library_intel_support_nc4.md) building netCDF4 libraries that enable compression with INTEL compiler 

     | intel 18.2   |    script |
     |  ----              |  ------     |
     | netCDF | [intel_18.2_install_netcdf_for_nc4_compression.csh](./scripts/cmaq_libraries/intel_18.2_install_netcdf_for_nc4_compression.csh) |
     | I/O API | [intel_18.2_install_ioapi_for_nc4_compression.csh](./scripts/cmaq_libraries/intel_18.2_install_ioapi_for_nc4_compression.csh) |                          
     | CMAQv5.5 |[intel_18.2_install_cmaq55_cb6r5_m3dry_for_nc4_compression.csh](./scripts/cmaq_libraries/intel_18.2_install_cmaq55_cb6r5_m3dry_for_nc4_compression.csh) |


     | intel 2024   |    script |
     |  ----              |  ------     |
     | netCDF | [intel_2024_install_netcdf_for_nc4_compression.csh](./scripts/cmaq_libraries/intel_2024_install_netcdf_for_nc4_compression.csh) |
     | I/O API | [intel_2024_install_ioapi_for_nc4_compression.csh](./scripts/cmaq_libraries/intel_2024_install_ioapi_for_nc4_compression.csh) |
     | CMAQv5.5 | [intel_2024_install_cmaq55_cb6r5_m3dry_for_nc4_compression.csh](./scripts/cmaq_libraries/intel_2024_install_cmaq55_cb6r5_m3dry_for_nc4_compression.csh) |

note, please review the tutorials prior to attempting to use the install scripts, as the CMAQ install script need to be edited for your local path.<br>
Use environment modules to manage the different library versions.<br>




