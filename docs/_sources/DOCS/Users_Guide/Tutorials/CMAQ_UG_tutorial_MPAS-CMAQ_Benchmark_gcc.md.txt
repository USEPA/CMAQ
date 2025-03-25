# MPAS-CMAQ Benchmarking Tutorial # 

**Purpose**: This guide describes how to install and run the MPAS-CMAQ test case, which serves two different purposes. The first being to familiarize the user with the MPAS-CMAQ suite of programs and how they work together, and secondly to verify the installation of the software on your system via benchmarking. 

Users are highly encouraged to work through the [MPAS-CMAQ User Guide][link_MPAS_PDF] and the [MPAS-CMAQ Chapter within the CMAQ User Guide](../CMAQ_UG_ch14_MPAS-CMAQ.md) to familiarize themselves with the individuals program components.

The following support software are required for compiling and running MPAS-CMAQ.

1. Fortran and C compilers, e.g., [Intel](https://software.intel.com/en-us/fortran-compilers), [Portland Group](http://www.pgroup.com), [Gnu](https://gcc.gnu.org/wiki/GFortran)
2. [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
3. Message Passing Interface (MPI), e.g., [OpenMPI](https://www.open-mpi.org) or [MVAPICH2](http://www.mcs.anl.gov/research/projects/mpich2).
4. Zlib, Curl, HDF5, 3. Parallel-netCDF, netCDF (C library), netCDF (Fortran interface library), sources for these libraries are obtained from http://www2.mmm.ucar.edu/people/duda/files/mpas/sources/ 
5. Information about latest release of [netCDF-C](https://docs.unidata.ucar.edu/netcdf-c/current/) and [netCDF-Fortran](https://docs.unidata.ucar.edu/netcdf-fortran/current/)
6. Parallel I/O Library (PIO) https://github.com/NCAR/ParallelIO
7. [C-Shell](https://github.com/tcsh-org/tcsh) 
8. Environment Modules https://modules.sourceforge.net/

**Note: To install the above libraries, please edit and use the following install scripts, depending on what compiler type and version you have on your system.** 

Library install scripts are available for the following compiler versions, but this tutorial uses the compiler version gcc-11.4.1:

```
gcc-9.1
gcc-11.4.1
ifort-18.0.2
ifort-2024
```

Using environment modules, it is possible to use multiple compilers and compiler versions to compile the libraries and then MPAS-CMAQ.

The suggested hardware requirements for running the CMAQ-MPAS 120km Uniform Grid Benchmark case on a Linux workstation are:


1. Linux environment with a 32 processors
2. 16 GB RAM
3. 400 GB hard drive storage


## Install the MPAS-CMAQ I/O Libraries using the GCC 11.4.1 compiler

Load the environment modules for gcc and openmpi

```
module load openmpi_5.0.5/gcc_11.4.1
```

**Create directory for libraries**

```
#change directories to the location where you would like to install MPAS-CMAQ
mkdir -p $cwd/MPAS-CMAQ/build/LIBRARIES_gcc11.4.1
cd $cwd/MPAS-CMAQ/build/LIBRARIES_gcc11.4.1
```

**Obtain and run libraries install script**

```
wget https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Tutorials/scripts/mpas-cmaq_libraries/iolib_installation_gcc11.4.1.sh
# change permission of the script so that it can be executed
chmod 755 iolib_installation_gcc11.4.1.sh
# run the library installation script
./iolib_installation_gcc11.4.1.sh |& tee ./iolib_installation_gcc11.4.1.log
```

**Review the log file to verify that the libraries were installed successfully**

```
grep -i error iolib_installation_gcc11.4.1.log
```

**Verify the libraries are installed**

```
ls lib/*
lib/libcurl.a   lib/libcurl.so.4      lib/libhdf5_hl.a   lib/libhdf5.settings  lib/libnetcdff.la       lib/libpioc.a        lib/libpnetcdf.a   lib/libpnetcdf.so.6
lib/libcurl.la  lib/libcurl.so.4.8.0  lib/libhdf5_hl.la  lib/libnetcdf.a       lib/libnetcdf.la        lib/libpiof.a        lib/libpnetcdf.la  lib/libpnetcdf.so.6.0.0
lib/libcurl.so  lib/libhdf5.a         lib/libhdf5.la     lib/libnetcdff.a      lib/libnetcdf.settings  lib/libpio.settings  lib/libpnetcdf.so  lib/libz.a

```

## Configure Private Environment Modules

```
mkdir -p $cwd/Modules/modulefiles/mpas-cmaq-iolib
```

Edit the module name file to specify the PATH and LD_LIBARY_PATH.

Next, create the module file and save it to the mpas-cmaq-iolib directory

Example:
```
vi  gcc-11.4.1
# add the following contents to the gcc-11.4.1 file, and edit the basedir to specify your local directory path
#%Module

proc ModulesHelp { } {
   puts stderr "This module adds mpas-cmaq-iolib/gcc-11.4.1 to your path"
}

module-whatis "This module adds mpas-cmaq-iolib/gcc-11.4.1 to your path\n"

set basedir "/work/users/l/i/lizadams/MPAS-CMAQ/build/LIBRARIES_gcc11.4.1/"
prepend-path PATH "${basedir}/bin"
prepend-path LD_LIBRARY_PATH "${basedir}/lib"
module load openmpi_5.0.5/gcc_11.4.1
```


Add the following command to your .cshrc, then logout and log back in.

```
module use --append /work/users/l/i/lizadams/MPAS-CMAQ/build/Modules/modulefiles
```

**Load Private Environment Modules**

```
## check that your private module can be found
module list
## load the private module
module load mpas-cmaq-iolib/gcc-11.4.1
## verify the module was loaded
module list
```

## Build MPAS-CMAQ

These instructions follow the [MPAS-CMAQ User Manual](https://github.com/USEPA/CMAQ/blob/MPAS-CMAQ/DOCS/Users_Guide/PDF/MPAS-CMAQ.pdf)

### Clone the MPAS-CMAQ Branch

```
mkdir MPAS-CMAQ
cd MPAS-CMAQ
git clone -b MPAS_CMAQ https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

**Building and running in a user-specified directory outside of the repository**

In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

In bldit_project.csh, modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:

```
set CMAQ_HOME = [your_install_path]/MPAS-CMAQ/CMAQ_5.5
```
Now execute the script.
```
./bldit_project.csh
```


### Configure the MPAS-CMAQ Build Environment

```
cd [your_install_path]/MPAS-CMAQ/CMAQ_5.5/CCTM/scripts
# note that the following environment variable has been uncommented in the bldit_cctm.csh: set build_mpas_cmaq
./bldit_cctm.csh gcc |& tee ./bldit_cctm.log
```

**Review Makefile created for MPAS-CMAQ**

Note that the config_cmaq.csh compiler settings are not utilized in the Makefile that is created by running the bldit_cctm.csh script.

```
cd BLD_CCTM_v55_gcc_cracmm2_m3dry
vi Makefile.mpas_cmaq
```

Successful execution of this script creates a BLD* directory with CMAQ code and a Makefile. The CMAQ code is now ready to be compiled as part of the MPAS model.

If this step fails, please reach post on the [CMAS Forum](https://forum.cmascenter.org/c/wrf-cmaq).

**Change directories to the top level MPAS-CMAQ directory**

```
cd /your-path/MPAS-CMAQ
```

### Download MPAS

```
git clone -b MPAS7.0 git@github.com:USEPA/MPAS.git MPAS
cd MPAS
```

**Copy the CMAQ build directory from the previous section to the MPAS src/core atmosphere directory and re-name it "cmaq."**

```
cp -rp ../CMAQ_5.5/CCTM/scripts/BLD_CCTM_v55_gcc_cracmm2_m3dry/ ./src/core_atmosphere/cmaq
```

### Compile MPAS-CMAQ

```
# return to the top level MPAS directory
cd ../MPAS
# compile MPAS with CMAQ code
make gfortran CORE=atmosphere USE_PIO2=true
```

### Examine the error message

```
************ ERROR ************
Failed to compile a PIO test program
Please ensure the PIO environment variable is set to the PIO installation directory
************ ERROR ************
make[1]: *** [Makefile:747: pio_test] Error 1
make[1]: Leaving directory '/work/users/l/i/lizadams/MPAS-CMAQ/MPAS'
make: *** [Makefile:214: gfortran] Error 2
```

**Recommend commending out -lpio from the Makefile at line 462 and setting the PIO environment variable**


```
vi Makfile
# comment out -lpio, as we only have -lpiof and -lpioc
#       LIBS += -lpio
```

Set the following environment variable to allow the pio libraries to be found 

```
 setenv PIO /work/users/l/i/lizadams/MPAS-CMAQ/build/LIBRARIES_gcc11.4.1/
```


**Recompile MPAS**

```
make gfortran CORE=atmosphere USE_PIO2=true
```

**Examine the log for additional errors**


```
*** No compatible version of WRF physics tables found; attempting to download compatible tables ***
/usr/bin/git
*** Trying git to obtain WRF physics tables ***
Cloning into 'MPAS-Data'...
fatal: unable to connect to github.com:
```

Need to use ssh instead of https, follow these instructions to obtain the required files:

```
cd MPAS/src/core_atmosphere/physics/physics_wrf/files
git clone --branch v7.0 -n --depth=1 --filter=tree:0 ssh://github.com/MPAS-Dev/MPAS-Data
cd MPAS-Data
git sparse-checkout set --no-cone /atmosphere/physics_wrf/files
git checkout
mv atmosphere/physics_wrf/files/* ../
```

Now the files should be available and the using VERSION number 7.0, so recompile MPAS-CMAQ

```
make gfortran CORE=atmosphere USE_PIO2=true
```

## next error


gfortran: error: unrecognized command-line option ‘-traceback’


```
#Edit the Makefile in the physics directory
vi /work/users/l/i/lizadams/MPAS-CMAQ/MPAS/src/core_atmosphere/physics/Makefile
#replace -traceback with -fbacktrace
```

**Recompile**

```
make gfortran CORE=atmosphere USE_PIO2=true
```


**Another error**


```
Error: BOZ literal constant at (1) is neither a data-stmt-constant nor an actual argument to INT, REAL, DBLE, or CMPLX intrinsic function [see ‘-fno-allow-invalid-boz’]
```

Edit the Makefile to add the option -fallow-invalid-boz
```
vi MPAS/Makefile
#add the following option
-fallow-invalid-boz
"FFLAGS_OPT = -O3 -m64 -ffree-line-length-none -fconvert=big-endian -ffree-form -fallow-invalid-boz -fallow-argument-mismatch"
```

**Another error**

```
module_sf_pxlsm.F:430:34:

  430 |       READ(ANAL_INTERVALC(1:2),'(i)') HH
      |                                  1
Error: Nonnegative width required in format string at (1)
module_sf_pxlsm.F:431:34:

  431 |       READ(ANAL_INTERVALC(4:5),'(i)') MM
      |                                  1
Error: Nonnegative width required in format string at (1)
module_sf_pxlsm.F:432:34:

  432 |       READ(ANAL_INTERVALC(7:8),'(i)') SS
      |                                  1
Error: Nonnegative width required in format string at (1)
```


**change the format i to i2:**

```

      ! New MPAS Code. Convert MPAS Char string of soilndg interval to
      READ(ANAL_INTERVALC(1:2),'(i2)') HH
      READ(ANAL_INTERVALC(4:5),'(i2)') MM
      READ(ANAL_INTERVALC(7:8),'(i2)') SS
      ANAL_INTERVAL = (HH * 3600) + (MM * 60) + (SS)
```

see https://stackoverflow.com/questions/11037954/error-nonnegative-width-required-in-format-string-at-1

**Error**

```
mio_gather_data_mod.F90:1100:34:

 1080 |                    call mpi_recv (who, 1, mpi_integer, mpi_any_source,    &
      |                                  2
......
 1100 |                    call mpi_recv (recv_buf, recv_size, mpi_int, who,      &
      |                                  1
Error: Rank mismatch between actual argument at (1) and actual argument at (2) (scalar and rank-1)
```


```
# add the following option to the makefile
-fallow-argument-mismatch
cd ./src/core_atmosphere/cmaq/
vi Makefile

 FSTD = -O3 -funroll-loops -finit-character=32 -Wtabs -Wsurprising -ftree-vectorize -ftree-loop-if-convert -finline-limit=512 -fallow-argument-mismatch
 DBG  = -Wall -O0 -g -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace -fallow-argument-mismatch
```

**Error**

```
mpif90 -O3 -m64 -o atmosphere_model driver/*.o -L. -ldycore -lops -lframework -L/work/users/l/i/lizadams/MPAS-CMAQ/build/LIBRARIES_gcc11.4.1//lib -lpiof -lpioc -I./external/esmf_time_f90 -L./external/esmf_time_f90 -lesmf_time
/usr/bin/ld: ./libdycore.a(mio_fcreate_mod.o): in function `__mio_fcreate_module_MOD_mio_fcreate':
mio_fcreate_mod.F90:(.text+0x240d): undefined reference to `__netcdf_MOD_nf90_create'
/usr/bin/ld: mio_fcreate_mod.F90:(.text+0x2549): undefined reference to `__netcdf_MOD_nf90_strerror'
```

Add the missing libraries to the end of the link statement:

```
cd MPAS/src
vi Makefile
add the following libraries to the end of the compile command (need to figure out where to add these options to the Makefile)
-lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -ll:libz.a -lpnetcdf -lnetcdf
```


### Verify that you have created the executable

```
ls -rlt atmosphere_model
-rwxrwxr-x 1 lizadams rc_cep-emc_psx 157259272 Jan 30 18:37 atmosphere_model
```


### Install the MPAS-CMAQ input data

Use the AWS command line to download data from the following s3 bucket:
<a href="https://mpas-cmaq.s3.amazonaws.com/index.html">MPAS-CMAQ S3 Bucket</a>


```
cd /your-path/MPAS-CMAQ
# note remove the --dryrun command from the following line after you have tested the following script
aws s3 --no-sign-request cp --recursive --region=us-east-1 --dryrun s3://mpas-cmaq/120_uniform ./120_uniform
# This will obtain both the mpas_inputs and cmas_inputs folders

```

The input files for the MPAS-CMAQ benchmark case are provided in the 120_uniform directory . Output MPAS-CMAQ files associated with the sample run script for the coupled MPAS-CMAQ model in this release package are also available.

### Link input files

It may be helpful aggregate all input  les into a common directory by linking to their full paths.  mkdir links

```
cd links
ln -s [full/path/to/mpas_inputs/*] .
ln -s [full/path/to/cmaq_inputs/emissions/one_day/*] .
ln -s [full/path/to//cmaq_inputs/other/*] .
```

### Run MPAS-CMAQ

One sample run scripts is provided (MPAS/run/run.csh)

Edit the slurm optins for your machine


Edit the run script

```
    cd MPAS/run/
    vi run.csh
```

Change end date to run for two days


```
 set START_DATE = "2017-01-01"     #> beginning date (Jan 1, 2017)
 set END_DATE   = "2017-01-02"     #> ending date    (Jan 2, 2017)
```

Modify the following section to specify your local paths:

```
    set input_path        = /your-path/MPAS-CMAQ/MPAS/links
    set MPAS_path_output    = /your-path/MPAS-CMAQ/MPAS/output_retest
```

Set the number of processors to run MPAS-CMAQ

```
    setenv NPROCS 24
```

Load the environment modules

```
    module load openmpi_5.0.5/gcc_11.4.1  mpas-cmaq-iolib/gcc-11.4.1
```

Submit the job to the slurm queue

```
  sbatch run.csh
```

### Verify that the run was successful
   - look for the output directory

   ```
   cd ../output
   ```
   If the run was successful you will see the following output, with the log files saved in the 20170101 directory

   ```
-rw-rw-r-- 1 lizadams rc_cep-emc_psx        2917 Nov 20 04:33 namelist.atmosphere
-rw-rw-r-- 1 lizadams rc_cep-emc_psx    87014088 Nov 20 05:00 diag.2017-01-01.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 56977988792 Nov 20 05:07 history.2017-01-01.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx   554322016 Nov 20 05:07 CCTM_EMIS_DIAG_20170101.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx   527939604 Nov 20 05:07 CCTM_OUT20170101.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx    40311788 Nov 20 05:07 CCTM_SOILOUT_20170101.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx    31299892 Nov 20 05:07 CCTM_BDSNPOUT_20170101.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx   199418764 Nov 20 05:08 CCTM_BDSNPDIAG_20170101.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx  6613956148 Nov 20 05:09 restart.2017-01-02_00.00.00.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx  2529702504 Nov 20 05:09 history.2017-01-02.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx    29011864 Nov 20 05:09 diag.2017-01-02.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx          21 Nov 20 05:09 restart_timestamp
-rw-rw-r-- 1 lizadams rc_cep-emc_psx        2385 Nov 20 05:09 CCTM_BUDGET_APPL.txt
drwxrwxr-x 2 lizadams rc_cep-emc_psx        8393 Nov 20 05:09 20170101

   ```

```
cd MPAS-CMAQ/MPAS/output_288_36x8/20170101
tail -n 5 CTM_LOG_000

     After NEXTIME: returned JDATE, JTIME 2017001 235230
            Master Time Step
            Processing completed...       3.8486 seconds

```

```
tail -n 8 log.atmosphere.0000.out

 -----------------------------------------
 Total log messages printed:
    Output messages =                57411
    Warning messages =                 197
    Error messages =                     0
    Critical error messages =            0
 -----------------------------------------
 Logging complete.  Closing file at 2024/11/20 05:09:20
```


### Compare the output 

Download data from s3 bucket to and compare to your results.

    ```
     cd $CMAQ_DATA
     wget  https://mpas-cmaq.s3.amazonaws.com/output/MPAS-CMAQ.120km_uniform_grid_gcc_output_288_36x8.tar.gz
     tar xvzf  MPAS-CMAQ.120km_uniform_grid_gcc_output_288_36x8.tar.gz
     ```


<!-- START_OF_COMMENT -->

[link_MPAS_PDF]: https://github.com/USEPA/CMAQ/blob/MPAS_CMAQ/DOCS/Users_Guide/PDF/MPAS_CMAQ_guide.pdf

<!-- END_OF_COMMENT -->


[link_MPAS_PDF]: https://github.com/USEPA/CMAQ/blob/MPAS_CMAQ/DOCS/Users_Guide/PDF/MPAS_CMAQ_guide.pdf
