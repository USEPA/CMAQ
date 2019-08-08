## Install netCDF-C

1. Download netCDF-C from the following website https://www.unidata.ucar.edu/downloads/netcdf/index.jsp

```
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-c-4.7.0.tar.gz
```

2. Untar

```
tar -tzvf netcdf-c-4.7.0.tar.gz
```

3. Verify that no modules are currently loaded

```
module list
```

4. See what modules are available on your compute server

```
module avail
```

5. Load module environment for the compiler and mpi package

```
module load openmpi_4.0.1/gcc_9.1.0
```

6. Review the installation instructions for netcdf-c-4.7.0 for building Classic netCDF

```
more INSTALL.md
```

7. Create a target installation directory that includes the loaded module environment name

8. Run the configure --help command to see what settings can be used for the build.
```
./configure --help
```

9. Run the configure command

```
./configure --prefix=/proj/ie/proj/staff/lizadams/netcdf-c-4.7.0/openmpi_4.0.1_gcc_9.1.0 --disable-netcdf-4 --disable-dap
```

10. Check that the configure command worked correctly

```
make check install
```

11. Verify that the following message is obtained

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
mkdir openmpi_4.0.1_gcc_9.1.0
```

5. Review the installation document http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html

6. Set the environment variable NCDIR

```
setenv NCDIR /proj/ie/proj/staff/lizadams/netcdf-c-4.7.0/openmpi_4.0.1_gcc_9.1.0
```

7. set the CC environment variable to use the gcc and gfortran compilers

```
which gcc
which gfortran

setenv CC /nas/longleaf/apps/gcc/9.1.0/bin/gcc
setenv FC /nas/longleaf/apps/gcc/9.1.0/bin/gfortran
```

8. set your LD_LIBRARY_PATH to include the netcdf-C library path for netCDF build

```
setenv NCDIR /proj/ie/proj/staff/lizadams/netcdf-c-4.7.0/openmpi_4.0.1_gcc_9.1.0
setenv LD_LIBRARY_PATH ${NCDIR}/lib:${LD_LIBRARY_PATH}
```

9. check your LD_LIBRARY_PATH

```
echo $LD_LIBRARY_PATH
```

10. set the install directory for netCDF fortran

```
setenv NFDIR /proj/ie/proj/staff/lizadams/netcdf-fortran-4.4.5/openmpi_4.0.1_gcc_9.1.0

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
   /proj/ie/proj/staff/lizadams/netcdf-fortran-4.4.5/openmpi_4.0.1_gcc_9.1.0/lib

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
setenv NFDIR /proj/ie/proj/staff/lizadams/netcdf-fortran-4.4.5/openmpi_4.0.1_gcc_9.1.0
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
BIN        = Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
```

3. copy an existing Makeinclude file to have this BIN name at the end

```
cd ioapi
cp Makeinclude.Linux2_x86_64gfort Makeinclude.Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
```

4. create a BIN directory

```
mkdir $BIN
```

5. link the netcdf-C and netcdf-Fortran library in the $BIN directory

```
cd $BIN
ln -s /proj/ie/proj/staff/lizadams/netcdf-fortran-4.4.5/openmpi_4.0.1_gcc_9.1.0/lib/libnetcdff.a
ln -s /proj/ie/proj/staff/lizadams/netcdf-c-4.7.0/openmpi_4.0.1_gcc_9.1.0/lib/libnetcdf.a
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

## Build CMAQ

1. download the CMAQ code

```
git clone -b v53_20190613 https://github.com/kmfoley/CMAQ.git
```

2. edit the bldit_project.csh script to include the name of your loaded module

```
set CMAQ_HOME = /proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_UNC5/openmpi_4.0.1_gcc_9.1.0
```

3. run the bldit_project.csh script to create your working directory

```
./bldit_project.csh
```

4. change directories to your working directory

```
cd /proj/ie/proj/CMAS/CMAQ/CMAQv5.3_branch_UNC5/openmpi_4.0.1_gcc_9.1.0
```

5. edit the config_cmaq.csh to set the netcdf-C, netcdf-Fortran and ioapi library and include locations

go to the case gcc section

        #> I/O API, netCDF, and MPI library locations
        setenv IOAPI_MOD_DIR   ioapi_mod_gcc  #> I/O API precompiled modules
        setenv IOAPI_INCL_DIR  iopai_inc_gcc  #> I/O API include header files
        setenv IOAPI_LIB_DIR   ioapi_lib_gcc  #> I/O API libraries
        setenv NETCDF_LIB_DIR  netcdf_lib_gcc #> netCDF directory path
        setenv NETCDF_INCL_DIR netcdf_inc_gcc #> netCDF directory path
        setenv MPI_LIB_DIR     mpi_lib_gcc    #> MPI directory path
        
change this to

```
 #> I/O API, netCDF, and MPI library locations
        setenv IOAPI_MOD_DIR   /proj/ie/proj/staff/lizadams/ioapi-3.2/Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0   #> I/O API precompiled modules
        setenv IOAPI_INCL_DIR  /proj/ie/proj/staff/lizadams/ioapi-3.2/ioapi/fixed_src   #> I/O API include header files
        setenv IOAPI_LIB_DIR   /proj/ie/proj/staff/lizadams/ioapi-3.2/Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0  #> I/O API libraries
        setenv NETCDF_LIB_DIR  /proj/ie/proj/staff/lizadams/netcdf-c-4.7.0/openmpi_4.0.1_gcc_9.1.0/lib  #netCDF library directory path
        setenv NETCDF_INCL_DIR /proj/ie/proj/staff/lizadams/netcdf-c-4.7.0/openmpi_4.0.1_gcc_9.1.0/include #> netCDF include directory path
        setenv MPI_LIB_DIR     /nas/longleaf/apps-dogwood/mpi/gcc_9.1.0/openmpi_4.0.1   #> MPI directory path
```
        
  To find the MPI_LIB_DIR verify that you have the module loaded
  then use the command
  ```
  which mpirun
  ```
  
  This points to
  /nas/longleaf/apps-dogwood/mpi/gcc_9.1.0/openmpi_4.0.1/bin/mpirun
  
  The include directory is 
  /nas/longleaf/apps-dogwood/mpi/gcc_9.1.0/openmpi_4.0.1/include
  
  edit the mpi_lib environment variable to use -lmpi
  setenv mpi_lib "-lmpi" 
  
  The following section on the config_cmaq.csh 
   if ( ! -e $NETCDF_DIR/lib/libnetcdf.a ) then
    echo "ERROR: $NETCDF_DIR/lib/libnetcdf.a does not exist in your CMAQ_LIB directory!!! Check your installation before proceeding with CMAQ build."
    exit
 endif

should be augmented to add a check for the libnetcdff.a 

   if ( ! -e $NETCDF_DIR/lib/libnetcdff.a ) then
    echo "ERROR: $NETCDF_DIR/lib/libnetcdff.a does not exist in your CMAQ_LIB directory!!! Check your installation before proceeding with CMAQ build."
    exit
 endif

        
## Building CMAQ

1. change to the CCTM/scripts directory
```
cd $CMAQ_HOME/CCTM/scripts
```
2. ./bldit_cctm.csh gcc |& tee ./bldit_cctm.gcc.log


currently getting undefined references, as the config_cmaq.csh doesn't include instructions on how to set up the netcdf-fortran libraries and include files.

