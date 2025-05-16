## Follow these instructions to build the netCDF libraries for compressed netCDF-4, I/O API, and CMAQv5.5 

This tutorial is based on these instructions: [Installing NetCDF](https://www.unidata.ucar.edu/software/netcdf/documentation/NUG/getting_and_building_netcdf.html)

* netCDF requires the HDF5, zlib, and curl libraries. 
* This Tutorial is for the intel 2024.2.1 compiler.   

## netCDF requires the HDF5, zlib, and curl libraries, these instructions use HDF5 1.14.3, zlib 1.3, and curl 8.11.1. 

This Tutorial uses library install scripts that were created for the intel 2024.2.1 compiler that uses ifx instead of ifort, there are also scripts for gcc 11.2. These install scripts assume that you have environment modules available on your system, and that you can use a module load command to load the compiler and openmpi version. Different scripts are provided, as different options are required, such as for gcc 10 and above, requires using the  -fallow-argument-mismatch argument <br>

The libraries can be installed using install scripts that are provided.

Install the netCDF libraries and their prerequisites for the compiler version that is available on your machine.


### Create install directory

```
mkdir -p $cwd/CMAQv5.5/build
```

### Download the install scripts for the intel 2024 compiler.

```
cd $cwd/CMAQv5.5/build
wget https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Tutorials/scripts/cmaq_libraries/intel_2024_install_netcdf_for_nc4_compression.csh
wget https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Tutorials/scripts/cmaq_libraries/intel_2024_install_ioapi_for_nc4_compression.csh
```


Load the modules for your compiler then run the library install script for the netcdf libraries.

```
module load intel/2024.2.1  
```

### Run script to install the netcdf libraries:

```
./intel_2024_install_netcdf_for_nc4_compression.csh
```

### Run script to install the I/O API Library

```
./intel_2024_install_ioapi_for_nc4_compression.csh
```

Note, if you obtain the following error:

```
git clone https://github.com/cjcoats/ioapi-3.2
Cloning into 'ioapi-3.2'...
fatal: unable to access 'https://github.com/cjcoats/ioapi-3.2/': Protocol "https" not supported
```

Then you will unload any modules using module purge. Then the git clone command should work.<br>

```
module purge
cd LIBRARIES_intel
git clone https://github.com/cjcoats/ioapi-3.2
cd ..
```

After the git clone has been successful then reload the modules

```
module load intel/2024.2.1 
```

Then re-run the install script above.

```
./intel_2024_install_ioapi_for_nc4_compression.csh
```

If this is successful, you will see a stream of log messages including the m3tools program wrfwndw being compiled.

Output
```
cd /proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/ioapi-3.2/Linux2_x86_64ifx; ifx -auto -warn notruncated_source -Bstatic -static-intel -I/proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/ioapi-3.2/ioapi -I/proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/ioapi-3.2/Linux2_x86_64ifx -DIOAPI_NCF4=1 -DAUTO_ARRAYS=1 -DF90=1 -DFLDMN=1 -DFSTR_L=int -DIOAPI_NO_STDOUT=1 -DAVOID_FLUSH=1 -DBIT32=1 -O3 -unroll -stack-temps -safe-cray-ptr -convert big_endian -assume byterecl  -traceback                                           -DIOAPI_NCF4=1 -DAUTO_ARRAYS=1 -DF90=1 -DFLDMN=1 -DFSTR_L=int -DIOAPI_NO_STDOUT=1 -DAVOID_FLUSH=1 -DBIT32=1 -c /proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/ioapi-3.2/m3tools/wrfwndw.f90
cd /proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/ioapi-3.2/Linux2_x86_64ifx; ifx -auto -warn notruncated_source -Bstatic -static-intel  wrfwndw.o -L/proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/ioapi-3.2/Linux2_x86_64ifx -lioapi -L/proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/lib -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lm -lcurl -lz -lsz -ldl -lm  -lnetcdf -qopenmp -shared-intel   -o wrfwndw
ifx: command line warning #10121: overriding '-static-intel' with '-shared-intel'

```


### Create a custom module
Add the netCDF and I/O API modules following these instructions: [Custom Modules](https://researchcomputing.princeton.edu/support/knowledge-base/custom-modules)

Example module files are available here:

[Example Modules](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html#v5_5/scripts/Modules/)
You would need to create the same directory structure, and edit the basedir in each module file to use your local directory. 

Create a directory with the directory structure Modules/modulefiles/ [module name]

### Create a module for ioapi-3.2

```
mkdir -p $cwd/Modules/modulefiles/ioapi-3.2
```

Edit the module name file to specify the PATH and LD_LIBARY_PATH.

Next, create the module file named intel-2024 and save it to the ioapi-3.2 directory 

Example:

```
cat intel-2024
```
output:
```
#%Module
  
proc ModulesHelp { } {
   puts stderr "This module adds ioapi-3.2/intel-2024 to your path"
}

module-whatis "This module adds ioapi-3.2/intel-2024 to your path\n"

set basedir "/proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/ioapi-3.2"
prepend-path PATH "${basedir}/Linux2_x86_64ifx"
prepend-path LD_LIBRARY_PATH "${basedir}/ioapi/fixed_src"
```

### Create a module for netcdf

```
mkdir -p $cwd/Modules/modulefiles/netcdf-4.5.3-for_nc4 
```

Next, create the module file and save it to the netcdf-4.5.3-for_nc4 directory

```
cat  ifort-2024.2.1
```
output:
```
#%Module
proc ModulesHelp { } {
   puts stderr "This module adds netcdf-4.5.3-for_nc4/ifort-2024.2.1 to your path"
}

module-whatis "This module adds netcdf-4.5.3-for_nc4/ifort-2024.2.1 to your path\n"

set basedir "/proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel/"
prepend-path PATH "${basedir}/bin"
prepend-path LD_LIBRARY_PATH "${basedir}/lib"
module load intel/2024.2.1

```


### Add module to .cshrc
Now that the module files have been created, add the following line to your .cshrc

```
module use --append /proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build/Modules/modulefiles
```

### Use module avail to see custom modules, and then load them

```
module avail
module load netcdf-4.5.3-for_nc4/ifort-2024.2.1 ioapi-3.2/intel-2024
```

Now you should see 3 modules loaded.

```
module list
```

Output:
```
Currently Loaded Modules:
  1) intel/2024.2.1   2) netcdf-4.5.3-for_nc4/ifort-2024.2.1   3) ioapi-3.2/intel-2024

```

### To build and run for the CRACMM2 mechanism and stage dry deposition scheme see the following tutorial:

[CMAQ Installation Tutorial for CRACMM2](CMAQ_UG_tutorial_benchmark_cracmm2_stage.md)

### To build and run for the CB6r5 mechanism and m3dry deposition scheme see the following tutorial:

[CMAQ Installation Tutorial for CRACMM2](CMAQ_UG_tutorial_benchmark.md)

### To build and run WRF-CMAQ see the following tutorial

[WRF-CMAQ Installation Tutorial](CMAQ_UG_tutorial_WRF-CMAQ_Benchmark.md)



### Note - for review only.
If you have successfully installed the netCDF, I/O API libraries and CMAQ, then <b>YOU CAN STOP HERE</b>, otherwise, you can go over the steps manually to see where an error has occurred in the above install scripts.


The following instructions go over the steps that were performed in the scripts above:


###
###  unset environment variables that would conflict with this installation
###

```
   unsetenv LDFLAGS
   unsetenv CPPFLAGS
```

1. If your compute server uses modules use the following command to see what packages are available

```
module avail
```
2. Load module environment for a compiler (Intel|GCC|PGI) and mpi package corresponding to that compiler (e.g. openmpi).
Note, that mpirun is available with the intel 2024 compiler, so there isn't another module that needs to be loaded.

```
module load intel/2024.2.1 
```


## Install zlib

```
setenv INSTDIR $cwd/LIBRARIES_intel
cd $INSTDIR
```

```
wget  https://github.com/madler/zlib/releases/download/v1.3/zlib-1.3.tar.gz
```

```
 tar -xzvf zlib-1.3.tar.gz
```

```
cd zlib-1.3
```

```
./configure  --prefix=${INSTDIR}
make -j 4
make install |& tee make.install.log
```


## Install curl (check first to determine if it is already installed)

1. check to see if which is installed using

```
curl --version
```


###IF it is not installed use the following steps to install it.

```
cd ${INSTDIR}
wget https://github.com/curl/curl/releases/download/curl-8_11_0/curl-8.11.0.tar.gz
```
```
tar -xzvf curl-8.11.0.tar.gz
 cd curl-8.11.0
 ./configure --prefix=${INSTDIR} --without-ssl --without-libpsl
 make |& tee make.curl.log
 make install |& tee make.install.curl.log
```

## Install HDF5

```
   cd ${INSTDIR}
   wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.5/src/hdf5-1.10.5.tar.gz
   tar xvf hdf5-1.10.5.tar.gz
   rm -f hdf5-1.10.5.tar.gz
   cd hdf5-1.10.5
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv CPPFLAGS "-I${INSTDIR}/include"
   setenv CFLAGS "-O3"
   setenv FFLAGS "-O3"
   setenv CXXFLAGS "-O3"
   setenv FCFLAGS "-O3"
   ./configure --prefix=${INSTDIR} --enable-fortran --enable-cxx --with-zlib=${INSTDIR}/include,${INSTDIR}/lib -enable-shared --enable-hl
   make -j 4 |& tee make.intel.log
#  make check > make.intel.check
   make install |& tee make.intel.log
```

## Install netCDF-C


1. cd ${INSTDIR}

2. Download netCDF-C from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-c-4.8.1.tar.gz
```

3. Untar the netCDF-C tar.gz file

```
tar -xzvf netcdf-c-4.8.1.tar.gz
```

4. Change directories into the extracted directory
```
cd netcdf-c-4.8.1
```

5. Review the installation instructions for netcdf-c-4.8.1 for building netCDF to support nc4 compression 

```
more INSTALL.md
```


6. Run the configure --help command to see what settings can be used for the build.
```
./configure --help
```

7. Set the Compiler environment variables

Make sure these compilers can be found.
```
which icx
which ifx
which icpx
```

If they are found, proceed to set the environment variables.
The paths will be dependent on your compute environment
If they are not found, reload your module (see above), or ask your system administrator for the paths to a compiler

```
setenv FC ifx
setenv CC icx
setenv CXX icpx
```

8. Specify the CPPFLAGS and LDFLAGS to tell netCDF where to obtain the underlying libraries, without this, netCDF may be built with a different version of the underlying libraries, leading to an error when using netCDF. 

```
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv CPPFLAGS "-I${INSTDIR}/include"
```

8. Run the configure command

```
./configure --with-pic --enable-netcdf-4 --enable-shared --prefix=${INSTALL_DIR}
```

9. Check that the configure command worked correctly, then run the install command

```
make |& tee  make.intel.log
make install
```

10. Verify that the following message is obtained

```
| Congratulations! You have successfully installed netCDF!    |
```

## Install netCDF-Fortran

1. Change directories

```
cd $INSTDIR
```


2. Download netCDF-Fortran from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-4.5.3.tar.gz 
```

3. Untar the tar.gz file

```
tar -xzvf netcdf-fortran-4.5.3.tar.gz
```

4. Change directories to netcdf-fortran-4.5.3

```
cd netcdf-fortran-4.5.3
```

5. Review the installation document http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html


6. Set the CC environment variable to use the ifx and icx (intel 2024) compilers

```
which ifx
which icx
which icpx

setenv FC ifx
setenv CC icx
setenv CXX icpx
```

7. Set your LD_LIBRARY_PATH to include the netcdf-C library path for netCDF build

```
   setenv LIBS "-L${INSTDIR}/lib -lnetcdf -lhdf5_hl -lhdf5 libhdf5_fortran libhdf5_fortran_hl -lm -ldl -lz -lcurl "
   setenv NCDIR ${INSTDIR}
   setenv CPPFLAGS "-I${INSTDIR}/include"
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv LD_LIBRARY_PATH ${INSTDIR}/lib:${LD_LIBRARY_PATH}
```

8. Check your LD_LIBRARY_PATH

```
echo $LD_LIBRARY_PATH
```

9. Run the configure command

```
./configure --with-pic --enable-shared --prefix=${INSTDIR}
```

10. Run the make check command

```
make check
```

Output if successful:

```
Testsuite summary for netCDF-Fortran 4.4.5
==========================================
# TOTAL: 6
# PASS:  6
```

11. Run the make install command

```
make install
```

Output successful if you see Libraries have been installed in the install directory

```
ls ${INSTALL_DIR}/lib
```

If you ever happen to want to link against installed libraries
in a given directory, LIBDIR, you must either use libtool, and
specify the full pathname of the library, or use the '-LLIBDIR'
flag during linking and do at least one of the following:
   - add LIBDIR to the 'LD_LIBRARY_PATH' environment variable
     during execution
   - add LIBDIR to the 'LD_RUN_PATH' environment variable
     during linking
   - use the '-Wl,-rpath -Wl,LIBDIR' linker flag
   - have your system administrator add LIBDIR to '/etc/ld.so.conf'


12. set your LD_LIBRARY_PATH to include the netcdf-Fortran library path for netCDF build

```
setenv NFDIR ${INSTALL_DIR}
setenv LD_LIBRARY_PATH ${NFDIR}/lib:${LD_LIBRARY_PATH}
```
(may need to add the NCDIR and NFDIR to .cshrc)

## Install I/O API
Note
The complete I/O API installation guide can be found at either of the following:

https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html

or

https://cjcoats.github.io/ioapi/AVAIL.html

1. Change directories to one level up from your current location

```
cd ../
```

2. Download I/O API

```
git clone https://github.com/cjcoats/ioapi-3.2
```

3. change directories to the ioapi-3.2 directory

```
cd ioapi-3.2
```

4. Change branches to 20200828 for a tagged stable version

```
git checkout -b 20200828
```

5. Change directories to the ioapi directory

```
cd ioapi
```

6. copy the Makefile.nocpl file to create a Makefile

```
cp Makefile.nocpl Makefile
```

7. Set the BIN environment variable 

```
setenv BIN Linux2_x86_64ifort
```

8. Edit the Makeinclude.Linux2_x86_64ifort to comment out OMPFLAG and OMPLIBS 
settings.  This will remove the need to link the shared memory OPENMP libraries when compiling CMAQ and WRF-CMAQ.

```
OMPFLAGS  = #-fopenmp
OMPLIBS   = # -fopenmp
```

10. Create a BIN directory where the library and m3tools executables will be installed

```
mkdir ../$BIN
```

11. Set the HOME environment variable to be your LIBRARY install directory and run the make command to compile and link the ioapi library

```
cd ioapi
make 'HOME=[your_install_path]/LIBRARIES' |& tee make.log
```

12. Change directories to the $BIN dir and verify that both the libioapi.a library was successfully built

```
cd ../$BIN
ls -lrt libioapi.a
```

13. If you need to do a make clean, to rebuild the I/O API Library, specify the HOME directory at the command line as follows

```
cd ../ioapi
make 'HOME=[your_install_path]/LIBRARIES' clean 
```

14. Change directories to the m3tools directory

```
cd ../m3tools
```

15. Copy the Makefile.nocpl to create a Makefile

```
cp Makefile.nocpl Makefile
```

16. Edit line 65 of the Makefile to use the NCDIR and NFDIR environment variables that you have set in the above steps to locate the netcdf C and netcdf Fortran libraries

```
 LIBS = -L${OBJDIR} -lioapi -L${NFDIR}/lib -lnetcdff -L${NCDIR}/lib -lnetcdf $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)
 ```

17. Run make to compile the m3tools

```
make |& tee make.log
```

18. Check to see that the m3tools have been installed successfully

```
cd ../$BIN
ls -rlt m3xtract
```

20. Use test script to verify that the executables are working

```
make test
```

## Install CMAQv55

1. Download the CMAQv55 code using the following

```
git clone -b 55 https://github.com/USEPA/CMAQ/cmaq.git CMAQ_REPO
```

2. Build and run in a user-specified directory outside of the repository
In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

In bldit_project.csh, modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:

set CMAQ_HOME = [your_work_location]/CMAQv55
Now execute the script.

```
./bldit_project.csh
```


3. Edit the config_cmaq.csh to specify the netCDF C, netCDF Fortran, and I/O API Library locations

```
cd [your_work_location]/CMAQv55
vi config_cmaq.csh
```

Edit the case intel section 
note, the paths need to be edited to match the location for your installation

```
#>  Intel fortran compiler......................................................
    case intel:
       setenv BUILD /proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/LIBRARIES_intel
       setenv MPI /nas/sycamore/apps/intel/2024.2.1/intel/oneapi/mpi/latest


        #> I/O API, netCDF Library Locations -- used in WRF-CMAQ
        setenv NETCDF netcdf_root_intel # Note please combine netCDF-C & Fortran Libraries 
        setenv IOAPI  ioapi_root_intel
        setenv WRF_ARCH # [1-75]  

        #> I/O API, netCDF, and MPI Library Locations -- used in CMAQ
        setenv IOAPI_INCL_DIR   $BUILD/ioapi-3.2/ioapi/fixed_src             #> I/O API include header files
        setenv IOAPI_LIB_DIR    $BUILD/ioapi-3.2/Linux2_x86_64ifx             #> I/O API libraries
        setenv NETCDF_LIB_DIR   $BUILD/lib            #> netCDF C directory path
        setenv NETCDF_INCL_DIR  $BUILD/include            #> netCDF C directory path
        setenv NETCDFF_LIB_DIR  $BUILD/lib           #> netCDF Fortran directory path
        setenv NETCDFF_INCL_DIR $BUILD/include           #> netCDF Fortran directory path
        setenv MPI_INCL_DIR     $MPI/include              #> MPI Include directory path
        setenv MPI_LIB_DIR      $MPI/lib               #> MPI Lib directory path

        #> Compiler Aliases and Flags
        #> set the compiler flag -qopt-report=5 to get a model optimization report in the build directory with the optrpt extension
        setenv myFC mpiifx
        setenv myCC icx
        #setenv myFSTD "-O3 -fno-alias -mp1 -fp-model source -ftz -simd -align all -xHost -vec-guard-write -unroll-aggressive"
        setenv myFSTD "-O3 -fno-strict-aliasing"
        setenv myDBG  "-O0 -g -check bounds -check uninit -fpe0 -fno-alias -ftrapuv -traceback"
        setenv myLINK_FLAG #"-qopenmp-simd" openMP not supported w/ CMAQ
        setenv myFFLAGS "-fixed -132"
        setenv myFRFLAGS "-free"
        setenv myCFLAGS "-O2"
        setenv extra_lib "-lhdf5_hl -lhdf5 -lm -lzip -lcurl -lz -lsz -ldl -lm -fpp -auto -qopenmp"

        breaksw
```

4. Source the config_cmaq.csh to create the lib directory

```
source config_cmaq.csh
```

5. Copy the buildit script

```
cp bldit_cctm.csh bldit_cctmv55_cb6r5_m3dry/
```

6. Build CMAQv55 to support the cb6r5 and m3dry dry deposition option 

```
./bldit_cctmv55_cb6r5_m3dry.csh intel | & tee ./bldit_cctmv55_cb6r5_m3dry.log
```

7. Build the POST processing routines

```
cd POST/combine/
./bldit_combine.csh intel |& tee ./bldit_combine.intel.log
```

```
cd POST/calc_tmetric/scripts
./bldit_calc_tmetric.csh intel |& tee ./bldit_calc_tmetric.intel.log
```

```
cd POST/hr2day/scripts
./bldit_hr2day.csh intel |& tee ./bldit_hr2day.intel.log
```

```
cd POST/bldoverlay/scripts
./bldit_bldoverlay.csh intel |& tee ./bldit_bldoverlay.intel.log
```

## Modify Benchmark Post-processing Scripts for your installation

1. The POST/combine directory is available under the CMAQv55 directory. 
These scripts will need to be edited  

```
run_combine.csh
```

2. Edit the scripts under to specify the APPL for this benchmark

```
 set APPL      = Bench_2018_12NE3        #> Application Name (e.g. Gridname)

```

3. Edit the start and end date.

```
 set START_DATE = "2016-07-01"     #> beginning date (July 1, 2016)
 set END_DATE   = "2016-07-14"     #> ending date    (July 14, 2016)
```

After successfull completion of this tutorial, the user is now ready to proceed to the [CMAQ Installation & Benchmarking Tutorial](./CMAQ_UG_tutorial_benchmark.md).
