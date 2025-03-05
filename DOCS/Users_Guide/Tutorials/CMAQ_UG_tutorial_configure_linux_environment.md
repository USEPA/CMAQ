# Building CMAQ and the required libraries (netCDF and I/O API)


There are two ways to build netCDF libraries, and it is critical to use a consistent set of builds.  

### Building libraries to support netCDF classic 

netCDF classic is based on the classic data model and defines a storage format which is custom to netCDF. 

### Building libraries to support netCDF-4 Format

netCDF4 is based on the enhanced data model and uses HDF5 as its backend storage format


### Are your input files netcdf4 compressed (*.nc4) or classic netCDF (*.nc)?

The EQUATES data that is provided uses netCDF4 compressed (*.nc4) input data

Determine the format of an input file with an *.nc extension

``` 
 ncdump -k file_name.nc
```

Output for file created with the netCDF3 libraries:

`classic`

Output that is identical to classic netcdf:

`64-bit offset`

Determine the format of an input file with an *.nc4 extension

```
 ncdump -k file_name.nc4
```

Output for files created with the netCDF4 libraries:

`netCDF-4 classic model`

### Build Method

First build netCDF, second build I/O API using your netCDF build, and then build CMAQ using your libraries from netCDF and I/O API.

In addition to two different methods of building libraries, the libraries can be built with different versions of compilers.

It is recommended to use the module environment to avoid mixing and matching other library build configuations.

Instructions will be provided for both Intel and GNU compilers.

### Three install scripts are provided to build CMAQ and the underlying libraries:

1. netCDF-C and netCDF-Fortran install script
2. I/O API install script
3. CMAQ install script for cb6r5 mechanism and m3dry dry deposition scheme


Instructions for editing and running these install scripts are available in the following tutorials

- **Directions on how to prepare your Linux system for installing CMAQ** (These instructions use a subset of the install scripts available below)

For netCDF classic
  - [Tutorial for building CMAQ with GNU using libraries that disable netCDF4 compression](CMAQ_UG_tutorial_build_library_gcc.md)
  - [Tutorial for building CMAQ with Intel using libraries that disable netCDF4 compression](CMAQ_UG_tutorial_build_library_intel.md)

for netcdf4
  - [Tutorial for building CMAQ with GNU using libraries that support compressed netCDF4](CMAQ_UG_tutorial_build_library_gcc_support_nc4.md)
  - [Tutorial for building CMAQ for Intel using libraries that support compressed netCDF4](CMAQ_UG_tutorial_build_library_intel_support_nc4.md)

Note, CMAQ built using libraries for compressed netCDF4 can read and use the netCDF classic input files.

After successfull completion of this tutorial, the user is now ready to proceed to the CMAQ Installation & Benchmarking Tutorial

If you have a different compiler than what was used in the tutorials above, then you can use the scripts below.
Note, you will need to edit the scripts to replace hard coded paths with your local file system path as described in the tutorials above.

### Install scripts for classic NetCDF

Using the GNU 11.4 compiler

  - [Script to install netCDF libraries to support netCDF classic](./scripts/gcc_11.4_install_netcdf_classic.csh)
  - [Script to install I/O API library to support netCDF classic](./scripts/gcc_11.4_install_ioapi_classic.csh)
  - [Script to install CMAQv55 for CB6r5 and M3DRY using netCDF classic](./scripts/gcc_11.4_install_cmaq55_cb6r5_m3dry_classic.csh)

### Install scripts for netCDF4 

Using the GNU 11.4 compiler (gcc/gfortran)

  - [Script to install netCDF libraries to support netCDF4 compression](./scripts/cmaq_libraries/gcc_11.4_install_netcdf_for_nc4_compression.csh)
  - [Script to install I/O API library to support netCDF4 compression](./scripts/cmaq_libraries/gcc_11.4_install_ioapi_for_nc4_compression.csh)
  - [Script to install CMAQv55 for CB6r5 and M3DRY](./scripts/cmaq_libraries/gcc_11.4_install_cmaq55_cb6r5_m3dry_for_nc4_compression.csh)

Using the Intel 18.2 compiler (ifort/icc)

  - [Script to install netCDF libraries to support netCDF4 compression](./scripts/intel_18.2_install_netcdf_for_nc4_compression.csh)
  - [Script to install I/O API library to support netCDF4 compression](./scripts/intel_18.2_install_ioapi_for_nc4_compression.csh)
  - [Script to install CMAQv55 for CB6r5 and M3DRY](./scripts/intel_18.2_install_cmaq55_cb6r5_m3dry_for_nc4_compression.csh)

Using the intel 2024 compiler (ifx/icx)

  - [Script to install netCDF libraries to support netCDF4 compression](./scripts/intel_2024_install_netcdf_for_nc4_compression.csh)
  - [Script to install I/O API library to support netCDF4 compression](./scripts/intel_2024_install_ioapi_for_nc4_compression.csh)
  - [Script to install CMAQv55 for CB6r5 and M3DRY](./scripts/intel_2024_install_cmaq55_cb6r5_m3dry_for_nc4_compression.csh)

