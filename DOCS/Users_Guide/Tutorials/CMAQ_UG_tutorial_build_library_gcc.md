## Install netCDF-C

1. Download netCDF-C from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-c-4.7.0.tar.gz
```

2. Untar

```
tar -xzvf netcdf-c-4.7.0.tar.gz
```

3. Verify that no modules are currently loaded

```
module list
```

4. See what modules are available on your compute server

```
module avail
```

5. Load module environment for a compiler (Intel|GCC|PGI) and mpi package corresponding to that compiler (e.g. openmpi).

```
module load gcc9.1.0
module load openmpi_4.0.1/gcc_9.1.0
```

6. Review the installation instructions for netcdf-c-4.7.0 for building Classic netCDF

```
more INSTALL.md
```

7. Create a target installation directory that includes the loaded module environment name

```
mkdir netcdf-c-4.7.0-gcc9.1.0
```

8 Change directories to where the files were extracted 
```
cd netcdf-c-4.7.0
```

9. Run the configure --help command to see what settings can be used for the build.
```
./configure --help
```

10. Set the Compiler environment variables

Make sure these compilers can be found.
```
which gfortran
which gcc
which g++
```
If they are found, proceed to set the environment variables.
The paths will be dependent on your compute environment
If they are not found, reload your module (see above)
```

setenv FC gfortran
setenv CC gcc
setenv CXX g++
```

10. Run the configure command

```
./configure --prefix=$cwd/../netcdf-c-4.7.0-gcc9.1.0 --disable-netcdf-4 --disable-dap
```

11. Check that the configure command worked correctly

```
make check install
```

12. Verify that the following message is obtained

```
| Congratulations! You have successfully installed netCDF!    |
```

13. Change directories to one level up from your current directory
```
cd ..
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
mkdir ../netcdf-fortran-4.4.5-gcc9.1.0
```

5. Review the installation document http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html

6. Set the environment variable NCDIR

```
setenv NCDIR $cwd/../netcdf-c-4.7.0-gcc9.1.0
```

7. Set the CC environment variable to use the gcc and gfortran compilers

```
which gfortran
which gcc
which g++

setenv FC gfortran
setenv CC gcc
setenv CXX g++
```

8. Set your LD_LIBRARY_PATH to include the netcdf-C library path for netCDF build

```
setenv NCDIR $cwd/../netcdf-c-4.7.0-gcc9.1.0
setenv LD_LIBRARY_PATH ${NCDIR}/lib:${LD_LIBRARY_PATH}
```

9. Check your LD_LIBRARY_PATH

```
echo $LD_LIBRARY_PATH
```

10. Set the install directory for netCDF fortran

```
setenv NFDIR $cwd/../netcdf-fortran-4.4.5-gcc9.1.0

setenv CPPFLAGS -I${NCDIR}/include
setenv LDFLAGS -L${NCDIR}/lib
```

11. Check your LD_LIBRARY_PATH environment variable

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
   
   $cwd/../netcdf-fortran-4.4.5-gcc9.1.0

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
setenv NFDIR $cwd/../netcdf-fortran-4.4.5-gcc9.1.0
setenv LD_LIBRARY_PATH ${NFDIR}/lib:${LD_LIBRARY_PATH}
```
(may need to add the NCDIR and NFDIR to .cshrc)

## Install I/O API
Note
The complete I/O API installation guide can be found at either of the following:

https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html

or

https://cjcoats.github.io/ioapi/AVAIL.html

1. Download I/O API

```
git clone https://github.com/cjcoats/ioapi-3.2
```

2. Change the BIN setting in the Makefile to include the loaded module name

```
BIN        = Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
```

3. Copy an existing Makeinclude file to have this BIN name at the end

```
cd ioapi
cp Makeinclude.Linux2_x86_64gfort Makeinclude.Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
```

4. Create a BIN directory

```
mkdir $BIN
```

5. Link the netcdf-C and netcdf-Fortran library in the $BIN directory

```
cd $BIN
ln -s /home/netcdf-c-gcc9.1.0/lib/libnetcdff.a
ln -s /home/netcdf-fortran-4.4.5-gcc9.1.0/lib/libnetcdf.a
```

6. Run the make command to compile and link the ioapi library

```
make |& tee make.log
```

7. Change directories to the $BIN dir and verify that both the libioapi.a and the m3tools were successfully built

```
cd $BIN
ls -lrt libioapi.a
ls -rlt m3xtract
```

8. After successfull completion of this tutorial, the user is now ready to proceed to the [CMAQ Installation & Benchmarking Tutorial](./CMAQ_UG_tutorial_benchmark.md)

