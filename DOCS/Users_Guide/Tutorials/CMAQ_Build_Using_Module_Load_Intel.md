## Install netCDF-C

1. Download netCDF-C from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-c-4.7.0.tar.gz
```

2. Untar the file 

```
tar -tzvf netcdf-c-4.7.0.tar.gz
```

3. Verify that no modules are currently loaded using module commands. 

```
module list
```

4. See what modules are available on your compute server use the command: 

```
module avail
```

5. Load module environment for a compiler (Intel|GCC|PGI) and mpi package corresponding to that compiler (e.g. openmpi).

```
module load intel18.2
module load openmpi_3.1.4/intel_18.2
```

6. Review the installation instructions for netcdf-c-4.7.0 for building Classic netCDF

```
more INSTALL.md
```

7. Create a target installation directory that includes the loaded module environment name. 

```
mkdir /home/netcdf-c-4.7.0-intel18.2
```

8. Run the configure --help command to see what settings can be used for the build.
```
./configure --help
```

9. Set the Compiler environment variables

```
which ifort
which icc
wihch icpc

setenv CC /urs/local/apps/intel/18.2/bin/icc
setenv FC /urs/local/apps/intel/18.2/bin/ifort
setenv CXX /urs/local/apps/intel/18.2/bin/icpc
```

10. Run the configure command

```
./configure --prefix=/home/netcdf-c-4.7.0-intel18.2 --disable-netcdf-4 --disable-dap
```

11. Check that the configure command worked correctly

```
make check install
```

12. Verify that the following message is obtained

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
mkdir /home/netcdf-fortran-4.4.5-intel18.2
```

5. Review the installation document http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html

6. Set the environment variable NCDIR

```
setenv NCDIR /home/netcdf-c-4.7.0-intel18.2
```

7. Set the CC environment variable to use the intel compilers

```
which ifort
which icc
wihch icpc

setenv CC /urs/local/apps/intel/18.2/bin/icc
setenv FC /urs/local/apps/intel/18.2/bin/ifort
setenv CXX /urs/local/apps/intel/18.2/bin/icpc
```

8. Set your LD_LIBRARY_PATH to include the netcdf-C library path for netCDF build

```
setenv NCDIR /home/netcdf-c-4.7.0-intel18.2
setenv LD_LIBRARY_PATH ${NCDIR}/lib:${LD_LIBRARY_PATH}
```

9. Check your LD_LIBRARY_PATH

```
echo $LD_LIBRARY_PATH
```

10. Set the install directory for netCDF fortran

```
setenv NFDIR /home/netcdf-fortran-4.4.5-intel18.2
setenv CPPFLAGS -I${NCDIR}/include
setenv LDFLAGS -L${NCDIR}/lib
```

11. check your LD_LIBRARY_PATH environment variable

```
echo $LD_LIBRARY_PATH
```

12. Run the configure command

```
./configure --prefix=${NFDIR}
```

13. Run the make check command

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

14. Run the make install command

```
make install
```

Output successful if you see:

```
Libraries have been installed in:
   
   /home/netcdf-fortran-4.4.5-intel18.2

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
setenv NFDIR /home/netcdf-fortran-4.4.5-intel18.2
setenv LD_LIBRARY_PATH ${NFDIR}/lib:${LD_LIBRARY_PATH}
```
(may need to add the NCDIR and NFDIR to .cshrc)

## Install I/O API

1. Download I/O API

```
git clone https://github.com/cjcoats/ioapi-3.2
```

2. Change the BIN setting in the Makefile to include the loaded module name

```
BIN        = Linux2_x86_64ifort_openmpi_4.0.1_intel18.2
```

3. Copy an existing Makeinclude file to have this BIN name at the end

```
cd ioapi
cp Makeinclude.Linux2_x86_64ifort Makeinclude.Linux2_x86_64ifort_openmpi_4.0.1_intel18.2
```

4. Create a BIN directory

```
mkdir $BIN
```

5. Link the netcdf-C and netcdf-Fortran library in the $BIN directory

```
cd $BIN
ln -s /home/netcdf-c-4.7.0-intel18.2/libnetcdff.a
ln -s /home/netcdf-fortran-4.4.5-intel18.2/libnetcdf.a
```

6. Run the make command to compile and link the ioapi library

```
make |& tee make.log
```

7. change directories to the $BIN dir and verify that both the libioapi.a and the m3tools were successfully built

```
cd $BIN
ls -lrt libioapi.a
ls -rlt m3xtract
```

8. After successfull completion of this tutorial, the user is now ready to proceed to the [CMAQ Installation & Benchmarking Tutorial](./CMAQ_UG_tutorial_benchmark.md). 

