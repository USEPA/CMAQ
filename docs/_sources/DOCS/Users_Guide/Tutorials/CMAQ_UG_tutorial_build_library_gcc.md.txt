## Install netCDF-C

### This tutorial assumes that you are using the C-shell, (csh or tcsh), GCC version 9.1.0, and OpenMPI 4.0.1

1. To enter the csh shell you can type the following at the command line:

```
csh
```

2. To verify what shell you are in

```
echo $SHELL
```

3. If your compute server uses modules use the following command to see what packages are available

```
module avail
```

4. Load module environment for a compiler (Intel|GCC|PGI) and mpi package corresponding to that compiler (e.g. openmpi).

```
module load gcc9.1.0
module load openmpi_4.0.1/gcc_9.1.0
```

5. Create a LIBRARY directory where you would like to install the libraries required for CMAQ

```
/[your_install_path]/LIBRARIES

```

6. Change directories to the new LIBRARIES Directory

```
cd /[your_install_path]/LIBRARIES
```

7. Download netCDF-C from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.8.1.tar.gz
```

8. Untar the netCDF-C tar.gz file

```
tar -xzvf v4.8.1.tar.gz
```

9. Change directories into the extracted directory
```
cd netcdf-c-4.8.1
```

10. Review the installation instructions for netcdf-c-4.7.0 for building netCDF

```
more INSTALL.md
```

11. Create a target installation directory that includes the loaded module environment name

```
mkdir ../netcdf
```


12. Run the configure --help command to see what settings can be used for the build.
```
./configure --help
```

13. Set the Compiler environment variables

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

14. Run the configure command

```
./configure --prefix=$cwd/../netcdf --disable-dap
```
Building netCDF without the compression capabilities of netCDF4 can be done using the command

```
./configure --prefix=$cwd/../netcdf --disable-netcdf-4 --disable-dap
```

This simpler installation can work for some applications, but the CMAQ ecosystem increasingly includes netCDF4 compression. For example, the cracmm1_aq 2018 benchmark and the MEGAN 3.2 preprocessor both require netCDF4. The error "Attempt to use feature that was not turned on when netCDF was built" suggests that your workflow requires netCDF4. If you need to build libraries that support netCDF4 please see tutorials for building netCDF4 in [Configuring your Environment](CMAQ_UG_tutorial_configure_linux_environment.md) 

15. Check that the configure command worked correctly, then run the install command

```
make check install
```

16. Verify that the following message is obtained

```
| Congratulations! You have successfully installed netCDF!    |
```

17. Change directories to one level up from your current directory
```
cd ..
```

## Install netCDF-Fortran

1. Download netCDF-Fortran from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.5.4.tar.gz
```

2. Untar the tar.gz file

```
tar -xzvf v4.5.4.tar.gz
```

3. Change directories to netcdf-fortran-4.5.4

```
cd netcdf-fortran-4.5.4
```

4. Review the installation document http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html


5. Set the CC environment variable to use the gcc and gfortran compilers

```
which gfortran
which gcc
which g++

setenv FC gfortran
setenv CC gcc
setenv CXX g++
```

6. Set your LD_LIBRARY_PATH to include the netcdf-C library path for netCDF build

```
setenv NCDIR $cwd/../netcdf
setenv LD_LIBRARY_PATH ${NCDIR}/lib:${LD_LIBRARY_PATH}
```

7. Check your LD_LIBRARY_PATH

```
echo $LD_LIBRARY_PATH
```

8. Set the install directory for netCDF fortran (note it will be the same location as the install directory for netCDF C libraries)

```
setenv NFDIR $cwd/../netcdf

setenv CPPFLAGS -I${NCDIR}/include
setenv LDFLAGS -L${NCDIR}/lib
setenv LIBS "-lnetcdf"
```

9. Run the configure command

```
./configure --disable-shared --prefix=${NFDIR}
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

Note, this often fails, even if the library is ok.

11. Run the make install command

```
make install
```

Output successful if you see Libraries have been installed in the install directory

```
ls $cwd/../netcdf
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
setenv NFDIR $cwd/../netcdf
setenv LD_LIBRARY_PATH ${NFDIR}/lib:${LD_LIBRARY_PATH}
```

13. Update the library bin directory path and LD_LIBRARY_PATH to use  $NCDIR and $NFDIR to in your .cshrc.

Verify the paths for $NCDIR and $NFDIR

```
echo $NCDIR
echo $NFDIR
```

14. Edit the .cshrc file in your home directory to add the paths to the libraries.
Note, in this case we installed both NetCDF C and NetCDF Fortran into the same location.

vi ~/.cshrc

```
# start .cshrc
set $NCDIR /your_path/netcdf
set $NFDIR /your_path/netcdf
set path = ($path $NCDIR\bin $NFDIR\bin )
setenv LD_LIBRARY_PATH ${NCDIR}/lib:${NFDIR}/lib:${LD_LIBRARY_PATH}
```

15. Source your updated .cshrc file by restarting the c-shell

```
csh
```


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

2. Change branches to 20200828 for a tagged stable version

```
git checkout -b 20200828
```

3. Change directories to the ioapi directory

```
cd ioapi
```

4. copy the Makefile.nocpl file to create a Makefile

```
cp Makefile.nocpl Makefile
```


5. Set the BIN environment variable to specify the compiler version used.
This will help future users identify what compiler version is compatible with this library.

```
setenv BIN Linux2_x86_64gfort_gcc_9.1.0
```

6. Copy an existing Makeinclude file to have this BIN name at the end

```
cp Makeinclude.Linux2_x86_64gfort Makeinclude.Linux2_x86_64gfort_gcc_9.1.0

```

7. Edit the Makeinclude.Linux2_x86_64gfort_gcc_9.1.0 to comment out OMPFLAG and OMPLIBS 
settings.  This will remove the need to link the shared memory OPENMP libraries when compiling CMAQ and WRF-CMAQ.

```
OMPFLAGS  = # -fopenmp
OMPLIBS   = # -fopenmp
```

8. Create a BIN directory where the library and m3tools executables will be installed

```
mkdir ../$BIN
```

9. Link the BIN directory to a the gfort BIN directory - this step is needed for WRF-CMAQ.

```
cd ../
ln -s Linux2_x86_64gfort_gcc_9.1.0 Linux2_x86_64gfort
```

10. Set the HOME environment variable to be your LIBRARY install directory and run the make command to compile and link the ioapi library

```
cd ioapi
make 'HOME=[your_install_path]/LIBRARIES' |& tee make.log
```

11. Change directories to the $BIN dir and verify that both the libioapi.a library was successfully built

```
cd ../$BIN
ls -lrt libioapi.a
```

12. If you need to do a make clean, to rebuild the I/O API Library, specify the HOME directory at the command line as follows

```
cd ../ioapi
make 'HOME=[your_install_path]/LIBRARIES' clean 
```

12. Change directories to the m3tools directory
```
cd ../m3tools
```

13. Copy the Makefile.nocpl to create a Makefile
```
cp Makefile.nocpl Makefile
```

14. Edit line 65 of the Makefile to use the NCDIR and NFDIR environment variables that you have set in the above steps to locate the netcdf C and netcdf Fortran libraries

```
 LIBS = -L${OBJDIR} -lioapi -L${NFDIR}/lib -lnetcdff -L${NCDIR}/lib -lnetcdf $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)
 ```

15. Run make to compile the m3tools
```
make 'HOME=[your_install_path]/LIBRARIES' |& tee make.log
```

16. Check to see that the m3tools have been installed successfully
```
cd ../$BIN
ls -rlt m3xtract
```

## Configure the CMAQ build environment

The user has two options for building an environment. She or he may build and run CMAQ components directly in the repository structure (object files and executables will be ignored with .gitignore), or they may extract the build and run scripts out of the repository and work in a separate location. If you would like to build directly in the repository, skip to "Link the CMAQ Libraries" below.

### Build and run in a user-specified directory outside of the repository
In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

In bldit_project.csh, modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:
```
set CMAQ_HOME = [your_install_path]/CMAQ_v5.5
```

Now execute the script.
```
./bldit_project.csh
```

## Link the CMAQ Libraries
The CMAQ build scripts require the following libraries and INCLUDE files to be available in the CMAQ_LIB directory (Note: the CMAQ_LIB gets set automatically by the config_cmaq.csh script, where `CMAQ_LIB = $CMAQ_HOME/lib`):

- netCDF C library files are located in the `$CMAQ_LIB/netcdf/lib` directory
- netCDF Fortran library files are located in the `$CMAQ_LIB/netcdff/lib` directory
- I/O API library, include files and module files are located in the `$CMAQ_LIB/ioapi` directory
- MPI library and INCLUDE files are located in the `$CMAQ_LIB/mpi` directory

The config_cmaq.csh script will automatically link the required libraries into the CMAQ_LIB directory. Set the locations of the netCDF, I/O API, and MPI installations on your Linux system with the following config_cmaq.csh environment variables:

- `setenv IOAPI_INCL_DIR`: the location of the I/O API include header files on your system.
- `setenv IOAPI_LIB_DIR`: the location of compiled I/O API libraries on your system.
- `setenv NETCDF_LIB_DIR`: the location of the netCDF C library installation on your system.
- `setenv NETCDF_INCL_DIR`: the location of the netCDF C include files on your system.
- `setenv NETCDFF_LIB_DIR`: the location of the netCDF Fortran library installation on your system.
- `setenv NETCDFF_INCL_DIR`: the location of the netCDF Fortran include files on your system.
- `setenv MPI_LIB_DIR`: the location of the MPI (OpenMPI or MVAPICH) on your system.

For example, if your netCDF C libraries are installed in /usr/local/netcdf/lib, set `NETCDF_LIB_DIR` to /usr/local/netcdf/lib. Similarly, if your I/O API library is installed in /home/cmaq/ioapi/Linux2_x86_64gfort, set `IOAPI_LIB_DIR` to /home/cmaq/ioapi/Linux2_x86_64gfort.

*1.* Check the names of the I/O API and netCDF libraries using the `ioapi_lib` and `netcdf_lib` script variables.

*2.* Check the name of the MPI library using the `mpi_lib` script variable. For MVAPICH use `-lmpich`; for openMPI use `-lmpi`.

Links to these libraries will automatically be created when you run any of the build or run scripts. To manually create these libraries (this is optional), execute the config_cmaq.csh script, identifying the compiler in the command line [intel | gcc | pgi]:
```
source config_cmaq.csh [compiler]
```
You may also identify the version of the compiler if you wish it to be identified in build directory and executable names. This is optional. For example:
```
source config_cmaq.csh gcc 9.5
```

17. After successfull completion of this tutorial, the user is now ready to proceed to the [CMAQ Installation & Benchmarking Tutorial](./CMAQ_UG_tutorial_benchmark.md)


