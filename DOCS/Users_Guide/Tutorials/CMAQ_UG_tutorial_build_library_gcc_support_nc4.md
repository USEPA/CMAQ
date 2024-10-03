##  Building with netCDF-4 and the Remote Data Client

This tutorial is based on these instructions: https://www.unidata.ucar.edu/software/netcdf/documentation/NUG/getting_and_building_netcdf.html

* netCDF requires the HDF5, zlib, and curl libraries. 
* This Tutorial is for the gcc 9.1.0 compiler.   
* For gcc 10 and above, use the  -fallow-argument-mismatch argument  
  * As of Aug. 28, 2020, there are now new *BIN=Linux\*gfort10\** types and corresponding *Makeinclude.Linux\*gfort10\** that incorporate this flag for the I/O API and M3Tools.
  * Source: I/O API documentation, https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html


## Versions required are at least HDF5 1.8.9, zlib 1.2.5, and curl 7.18.0 or later.

set install directory

```
set INSTALL_DIR = $cwd/CMAQv5.5/LIBRARIES 
mkdir -p $cwd/CMAQv5.5/LIBRARIES
```

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

```
module load gcc9.1.0
module load openmpi_4.0.1/gcc_9.1.0
```


## Install zlib

```
 cd $INSTALL_DIR
```

```
wget  https://sourceforge.net/projects/libpng/files/zlib/1.2.11/zlib-1.2.11.tar.gz
```

```
 tar -xzvf zlib-1.2.11.tar.gz
```

```
cd zlib-1.2.11
```

```
./configure ; make test
```

```
mkdir ${INSTALL_DIR}/zlib-1.2.11_gcc9.1.0
```

```
make install prefix=${INSTALL_DIR}/zlib-1.2.11_gcc9.1.0
```

## Install curl (check first to determine if it is already installed)

1. check to see if which is installed using

```
curl --version
```


###IF it is not installed use the following steps to install it.

```
cd ${INSTALL_DIR}
wget https://curl.se/download/curl-7.79.0.tar.gz
```
```
tar -xzvf curl-7.79.0.tar.gz
```

```
./configure --without-ssl --prefix=${INSTALL_DIR}/curl-7.79.0-gcc9.1.0
```

```
make
make install
```

## Install HDF5

```
cd $INSTALL_DIR
```

```
wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.7/src/hdf5-1.10.7.tar.gz
tar -xzvf hdf5-1.10.7.tar.gz
```
```
mkdir ${INSTALL_DIR}/hdf5-1.10-7-gcc9.1.0
```

```
cd hdf5-1.10.7
```

Set flags

```
   setenv CFLAGS "-O3"
   setenv FFLAGS "-O3"
   setenv CXXFLAGS "-O3"
   setenv FCFLAGS "-O3"
```

Configure (note, you may need to add a path if you installed the curl library using --with-curl=${INSTALL_DIR}/curl-7.79.0-gcc9.1.0)


```
./configure --prefix=${INSTALL_DIR} --with-zlib=${INSTALL_DIR}/zlib-1.2.11_gcc9.1.0/include,${INSTALL_DIR}/zlib-1.2.11_gcc9.1.0/lib --enable-hl
```

Build

```
make
make check   # run test suite
make install # install
make check-install # verify installation
```

## Install netCDF-C


1. cd ${INSTALL_DIR}

2. Download netCDF-C from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-c-4.8.0.tar.gz
```

3. Untar the netCDF-C tar.gz file

```
tar -xzvf netcdf-c-4.8.0.tar.gz
```

4. Change directories into the extracted directory
```
cd netcdf-c-4.8.0
```

5. Review the installation instructions for netcdf-c-4.8.0 for building netCDF to support nc4 compression 

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
which gfortran
which gcc
which g++
```

If they are found, proceed to set the environment variables.
The paths will be dependent on your compute environment
If they are not found, reload your module (see above), or ask your system administrator for the paths to a compiler

```
setenv FC gfortran
setenv CC gcc
setenv CXX g++
```

8. Run the configure command

```
./configure --with-pic --enable-netcdf-4 --enable-shared --prefix=${INSTALL_DIR}
```

9. Check that the configure command worked correctly, then run the install command

```
make |& tee  make.gcc9.log
make install
```

10. Verify that the following message is obtained

```
| Congratulations! You have successfully installed netCDF!    |
```

## Install netCDF-Fortran

1. Change directories

```
cd $INSTALL_DIR
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


6. Set the CC environment variable to use the gcc and gfortran compilers

```
which gfortran
which gcc
which g++

setenv FC gfortran
setenv CC gcc
setenv CXX g++
```

7. Set your LD_LIBRARY_PATH to include the netcdf-C library path for netCDF build

```
setenv NCDIR ${INSTALL_DIR}
setenv LIBS "-lnetcdf"
setenv CPPFLAGS -I${NCDIR}/include
setenv LDFLAGS -L${NCDIR}/lib

setenv LD_LIBRARY_PATH ${NCDIR}/lib:${LD_LIBRARY_PATH}
```

8. Check your LD_LIBRARY_PATH

```
echo $LD_LIBRARY_PATH
```

9. Set the install directory for netCDF fortran

```
setenv NFDIR  ${INSTALL_DIR} 
setenv CPPFLAGS -I${NCDIR}/include
setenv LDFLAGS -L${NCDIR}/lib
```

10. Run the configure command

```
./configure --with-pic --enable-shared --prefix=${NFDIR}
```

11. Run the make check command

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

12. Run the make install command

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


14. set your LD_LIBRARY_PATH to include the netcdf-Fortran library path for netCDF build

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

7. Set the BIN environment variable to include the module that will be used to compile CMAQ
This will help future users identify what compiler version is compatible with this library.

```
setenv BIN Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
```

8. Copy an existing Makeinclude file to have this BIN name at the end

```
cp Makeinclude.Linux2_x86_64gfort Makeinclude.Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
```

9. Edit the Makeinclude.Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0 to comment out OMPFLAG and OMPLIBS 
settings.  This will remove the need to link the shared memory OPENMP libraries when compiling CMAQ and WRF-CMAQ.

```
#OMPFLAGS  = -fopenmp
#OMPLIBS   =  -fopenmp
```

10. Create a BIN directory where the library and m3tools executables will be installed

```
mkdir ../$BIN
```

11. Link the BIN directory to a the gfort BIN directory - this step is needed for WRF-CMAQ.

```
cd ../
ln -s Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0 Linux2_x86_64gfort
```

12. Set the HOME environment variable to be your LIBRARY install directory and run the make command to compile and link the ioapi library

```
cd ioapi
make 'HOME=[your_install_path]/LIBRARIES' |& tee make.log
```

13. Change directories to the $BIN dir and verify that both the libioapi.a library was successfully built

```
cd ../$BIN
ls -lrt libioapi.a
```

14. If you need to do a make clean, to rebuild the I/O API Library, specify the HOME directory at the command line as follows

```
cd ../ioapi
make 'HOME=[your_install_path]/LIBRARIES' clean 
```

15. Change directories to the m3tools directory

```
cd ../m3tools
```

16. Copy the Makefile.nocpl to create a Makefile

```
cp Makefile.nocpl Makefile
```

17. Edit line 65 of the Makefile to use the NCDIR and NFDIR environment variables that you have set in the above steps to locate the netcdf C and netcdf Fortran libraries

```
 LIBS = -L${OBJDIR} -lioapi -L${NFDIR}/lib -lnetcdff -L${NCDIR}/lib -lnetcdf $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)
 ```

18. Run make to compile the m3tools

```
make |& tee make.log
```

19. Check to see that the m3tools have been installed successfully

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
git clone -b 55 https://github.com/lizadams/cmaq.git CMAQ_REPO
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

Edit the case gcc section 
note, the paths need to be edited to match the location for your installation

```
#>  gfortran compiler............................................................
    case gcc:

        #> I/O API and netCDF for WRF-CMAQ 
        setenv NCDIR /your_local_path/LIBRARIES/                  # C netCDF install path
        setenv NFDIR /your_local_path/LIBRARIES/           # Fortran netCDF install path for CMAQ
        setenv NETCDF /your_local_path/LIBRARIES/          # Note only for  WRF-CMAQ as it requires combining the netcdf C and netcdf F into a single directory. CMAQ users - don't change this setting
        setenv IOAPI  /your_local_path/LIBRARIES/ioapi-3.2/   # I/O API 
        setenv WRF_ARCH 34                              # [1-75] Optional, ONLY for WRF-CMAQ  

        #> I/O API, netCDF, and MPI library locations
        setenv IOAPI_INCL_DIR   ${IOAPI}/ioapi/fixed_src    #> I/O API include header files
        setenv IOAPI_LIB_DIR    ${IOAPI}/Linux2_x86_64gfort    #> I/O API libraries
        if ( $NETCDF == "netcdf_combined_directory_path" ) then
            setenv NETCDF_LIB_DIR   ${NCDIR}/lib                       #> netCDF C directory path
            setenv NETCDF_INCL_DIR  ${NCDIR}/include                   #> netCDF C directory path
            setenv NETCDFF_LIB_DIR  ${NFDIR}/lib                       #> netCDF Fortran directory path
            setenv NETCDFF_INCL_DIR ${NFDIR}/include                   #> netCDF Fortran directory path
        endif

        setenv MPI_INCL_DIR      /nas/longleaf/apps-dogwood/mpi/gcc_9.1.0/openmpi_4.0.1/include #> MPI Include directory path
        setenv MPI_LIB_DIR      /nas/longleaf/apps-dogwood/mpi/gcc_9.1.0/openmpi_4.0.1/lib               #> MPI Lib directory path
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
./bldit_cctmv55_cb6r5_m3dry.csh gcc | & tee ./bldit_cctmv55_cb6r5_m3dry.log
```

7. Build the POST processing routines

```
cd POST/combine/
./bldit_combine.csh gcc |& tee ./bldit_combine.gcc.log
```

```
cd POST/calc_tmetric/scripts
./bldit_calc_tmetric.csh gcc |& tee ./bldit_calc_tmetric.gcc.log
```

```
cd POST/hr2day/scripts
./bldit_hr2day.csh gcc |& tee ./bldit_hr2day.gcc.log
```

```
cd POST/bldoverlay/scripts
./bldit_bldoverlay.csh gcc |& tee ./bldit_bldoverlay.gcc.log
```

8.  Edit the SBATCH section of the Benchmark run script to use the SLURM resources on your machine 
and modify the CMAQ_HOME directory to specify your local path

```
vi run_cctm_Bench_2018_12NE3.csh
```



## Download Benchmark Input Data


## Update EQUATES Benchmark Run Script to specify path to downloaded input data

1. Specify the 2018_12US1 as the input data directory in the CMAQv55 CCTM run script

```
vi run_cctm_Bench_2018_12NE3.csh  
```

verify the location of the 12NE3 benchmark input files

```
 setenv INPDIR  ${CMAQ_DATA}/2018_12NE3
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


## Run the combine run script

```
sbatch run_combine.csh
```
