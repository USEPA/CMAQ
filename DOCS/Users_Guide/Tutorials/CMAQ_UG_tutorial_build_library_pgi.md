## Load PGI compiler

```
module load pgi
```

1. Check version number

```
pgf90 --version
```


## Install openmpi 
tips on how to do this using pgi compiler found here: https://www.pgroup.com/userforum/viewtopic.php?t=5413
I also had to download and install the latest UCX library following these instructions:
https://github.com/open-mpi/ompi/issues/4441
https://github.com/openucx/ucx/wiki/OpenMPI-and-OpenSHMEM-installation-with-UCX

1. Download openmp from the following website https://www.open-mpi.org/software/ompi/v4.0/

```
wget https://download.open-mpi.org/release/open-mpi/v4.0/openmpi-4.0.2.tar.gz
```
2. Read INSTALL file and follow instructions to create a build directory and run configure 

3. Run configure command

```
#./configure --with-ucx=no --prefix=/proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_pgi/pgi_19.7_openmpi_4.0.2/LIB/openmpi-4.0.2/build 
# ./configure CC=pgcc CXX=pgc++ F77=pgf77 FC=pgf90 CFLAGS='-fPIC -m64 ' CXXFLAGS='-fPIC -m64 ' FCFLAGS='-fPIC -m64 ' --prefix="/proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_pgi/pgi_19.7_openmpi_4.0.2/LIB/openmpi-4.0.2/build"
./configure CC=pgcc CXX=pgc++ F77=pgf77 FC=pgf90 CFLAGS='-fPIC -m64' CXXFLAGS='-fPIC -m64' FCFLAGS='-fPIC -m64' --enable-mpi-fortran --enable-mpi-cxx --with-ucx=/proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_pgi/pgi_19.7_openmpi_4.0.2/LIB/ucx/install --enable-mca-no-build=btl-uct --prefix=/proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_pgi/pgi_19.7_openmpi_4.0.2/LIB/openmpi-4.0.2/build

```

4. Run make command

```
make |& tee make.log
```

5.  Run check command

```
make check |& tee make.check.log
```

6. Run install command

```
make install |& tee make.install.log
```

7. Note the following instructions in the output from the make all install command

Libraries have been installed in:
   /proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_pgi/pgi_19.7_openmpi_4.0.2/LIB/openmpi-4.0.2/build/lib

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

See any operating system documentation about shared libraries for
more information, such as the ld(1) and ld.so(8) manual pages.


## Install netCDF-C

1. Download netCDF-C from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-c-4.7.0.tar.gz
```

2. Untar the file 

```
tar -xzvf netcdf-c-4.7.0.tar.gz
```

3. Change directories into the package
```
cd netcdf-c-4.7.0
```

3. Verify that no modules are currently loaded using module commands. 

```
module list
```

4. See what modules are available on your compute server use the command: 

```
module avail
```

5. Load module environment for the compiler (PGI) 

```
module load pgi 
```

6. Review the installation instructions for netcdf-c-4.7.0 for building Classic netCDF

```
more INSTALL.md
```

7. Create a target installation directory that includes the loaded module environment name. 

```
mkdir $cwd/netcdf-c-4.7.0-pgi_19.7
```

8. Run the configure --help command to see what settings can be used for the build.
```
./configure --help
```

9. Set the Compiler environment variables

First find the path to the CC compiler on your system using the which command
```
which pgicc
```
Next, replace the following path in the setenv command below to use the path to your CC compiler

```
setenv CC pgicc
```

Find the path to the Fortran compiler on your ssystem using the which command
```
which pgf90
```
Next, replace the following path in the setenv command below to use the path to the Fortran compiler on your system
```
setenv FC pgf90
```

Find the path to the CXX compiler on your system using the which command
```
which pgc++
```
Next, replace the following path in the setenv command below to use the path to the CXX compiler on your system:
```
setenv CXX pgc++
```

10. Run the configure command

```
./configure --prefix=$cwd/netcdf-c-4.7.0-pgi_19.7 --disable-netcdf-4 --disable-dap
./configure CC=pgcc CXX=pgc++ F77=pgf77 FC=pgf90  --prefix=$cwd/netcdf-c-4.7.0-pgi_19.7 --disable-netcdf-4 --disable-dap
```

11. Check that the configure command worked correctly

```
make check install |& tee make.install.log.txt
```

12. Verify that the following message is obtained at the end of your make.install.log.txt file

```
| Congratulations! You have successfully installed netCDF!    |
```

## Install netCDF-Fortran

1. Download netCDF-Fortran from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-4.4.5.tar.gz 
```

2. Untar the tar.gz file

```
tar -xzvf netcdf-fortran-4.4.5.tar.gz
```

3. Change directories to netcdf-fortran-4.4.5

```
cd netcdf-fortran-4.4.5
```

4. Make an install directory that matches the name of your loaded module environment

```
mkdir $cwd/netcdf-fortran-4.4.5-pgi_19.7
```

5. Review the installation document http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html

6. Set the environment variable NCDIR

```
setenv NCDIR $cwd/netcdf-c-4.7.0/netcdf-c-4.7.0-pgi_19.7
```

7. Set the CC environment variable to use the pgi compilers


8. Set your LD_LIBRARY_PATH to include the netcdf-C library path for netCDF build

```
setenv NCDIR /proj/ie/proj/staff/lizadams/netcdf-c-4.7.0/openmpi_4.0.1_gcc_9.1.0
setenv LD_LIBRARY_PATH ${NCDIR}/lib:${LD_LIBRARY_PATH}
```

9. Check your LD_LIBRARY_PATH

```
echo $LD_LIBRARY_PATH
```

10. Set the install directory for netCDF fortran

```
setenv NFDIR /proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_pgi/pgi_19.7_openmpi_4.0.2/LIB/netcdf-fortran-4.4.5/netcdf-fortran-4.4.5-pgi_19.7 
setenv CPPFLAGS -I${NCDIR}/include
setenv LDFLAGS -L${NCDIR}/lib
```

11. check your LD_LIBRARY_PATH environment variable

```
echo $LD_LIBRARY_PATH
```

12. Run the configure command

```
./configure CC=pgcc CXX=pgc++ F77=pgf90 FC=pgf90 --prefix=${NFDIR}
```

13. Run the make check command

```
make check |& tee make.check.log.txt
```

Output if successful:

```
Testsuite summary for netCDF-Fortran 4.4.5
==========================================
# TOTAL: 6
# PASS:  6
```

14. Run the make install command

```
make install |& tee ./make.install.log.txt
```

Output successful if you see:

```
Libraries have been installed in:
   
/proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_pgi/pgi_19.7_openmpi_4.0.2/LIB/netcdf-fortran-4.4.5/netcdf-fortran-4.4.5-pgi_19.7/lib

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
```

15. set your LD_LIBRARY_PATH to include the netcdf-Fortran library path for netCDF build

```
setenv NFDIR /proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_pgi/pgi_19.7_openmpi_4.0.2/LIB/netcdf-fortran-4.4.5/netcdf-fortran-4.4.5-pgi_19.7
setenv LD_LIBRARY_PATH ${NFDIR}/lib:${LD_LIBRARY_PATH}
```
(may need to add the NCDIR and NFDIR to .cshrc)

## Install I/O API

Note The complete I/O API installation guide can be found at either of the following:

https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html

or

https://cjcoats.github.io/ioapi/AVAIL.html

1. Download I/O API

```
git clone https://github.com/cjcoats/ioapi-3.2
```

2. Change the BIN setting on line 133 of the Makefile to include the loaded module name

```
BIN        = Linux2_x86_64_openmpi_4.0.2_pgi_19.7
```

3. Change the NCFLIBS setting on line 141 of the Makefile to be

```
NCFLIBS    = -lnetcdff -lnetcdf
```

4. Copy an existing Makeinclude file to have this BIN name at the end

```
cd ioapi
cp Makeinclude.Linux2_x86_64ifort Makeinclude.Linux2_x86_64ifort_openmpi_3.1.4_intel18.2
```

5. Edit the Makeinclude file, lines 27 and 28 to use -qopenmp instead of -openmp

```
OMPFLAGS  = -qopenmp
OMPLIBS   = -qopenmp
```

6. Set the environment variable BIN

```
setenv BIN Linux2_x86_64ifort_openmpi_3.1.4_intel18.2
```

7. Create a BIN directory under the ioapi-3.2 directory

```
cd ..
mkdir $BIN
```

8. Link the netcdf-C and netcdf-Fortran library in the $BIN directory

```
cd $BIN
ln -s /home/netcdf-c-4.7.0-intel18.2/libnetcdff.a
ln -s /home/netcdf-fortran-4.4.5-intel18.2/libnetcdf.a
```

9. Run the make command to compile and link the ioapi library

```
make all |& tee make.log
```

10. Change directories to the $BIN dir and verify that both the libioapi.a and the m3tools were successfully built

```
cd $BIN
ls -lrt libioapi.a
ls -rlt m3xtract
```

11. After successfull completion of this tutorial, the user is now ready to proceed to the [CMAQ Installation & Benchmarking Tutorial](./CMAQ_UG_tutorial_benchmark.md). 

