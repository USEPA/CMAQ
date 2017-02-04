[Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch01_intro.md)
***
CMAQ Installation Quick Start Guide
=========================================

### System Checks ###

The following support software are required for compiling and running CMAQ.  

1. Fortran and C compilers, e.g., [Intel](https://software.intel.com/en-us/fortran-compilers), [Portland Group](http://www.pgroup.com), [Gnu](https://gcc.gnu.org/wiki/GFortran)
2. [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
3. [I/O API](http://www.cmascenter.org/ioapi)
4. [netCDF](http://www.unidata.ucar.edu/software/netcdf/)
5. Message Passing Interface (MPI), e.g., [OpenMPI](https://www.open-mpi.org) or [MVAPICH2](http://www.mcs.anl.gov/research/projects/mpich2/).

### Install CMAQ and Required Libraries ###

In the directory where you would like to install CMAQ, issue the following command to clone the CMAS CENTER GitHub repository for CMAQv5.2 Beta:

`git clone -b 5.2Beta https://github.com/CMASCenter/EPA-CMAQ.git`

For instructions on installing CMAQ from tarballs, see [Chapter 5](CMAQ_OGD_ch05_sys_req.md).

#### Configure the CMAQ build environment

Edit the file `CMAQv5.2/config.cmaq` to configure the CMAQ installation for the local computing architecture and compilers.

1. Set the script variable `CMAQ_HOME` to point the installation directory location of CMAQ on your Linux system.
2. Set the script variable `compiler` for one of the supported compilers.
3. Check the names of the I/O API and netCDF libraries using the `ioapi_lib` and `netcdf_lib` script variables.
4. Check the name of the MPI library using the `mpi` script variable. For MVAPICH use `-lmpich`; for openMPI use `-lmpi`.
5. Invoke the settings in the configuration script: `source $CMAQ_HOME/config.cmaq`

#### Install the CMAQ libraries

You must install or link the netCDF, I/O API, and MPICH libraries into the CMAQ_LIB directory. The CMAQ compilation scripts assume that the netCDF library and INCLUDE files reside in the `$CMAQ_LIB/netCDF` directory. For example, if your netCDF libraries and includes files are installed in /usr/local/netcdf, link find the lib for your specified compiler and link it into the CMAQ library directory:

  1. use the echo command to determine what compiler you have chosen
   (in the following example the intel compiler was set in the config.cmaq file)
`echo $CMAQ_LIB`
  2. The output should contain your local path with the compiler listed at the end:  `EPA-CMAQ/lib/x86_64/intel`
  3. If you are using a computer that uses modules to load software, then use the module command to confirm that the compiler that is loaded is the same that was used to build the netcdf and ioapi library using:
 * `module list`: for this example the openmpi_pgi/15.7 module was used 
 * `module avail` - will show you the modules are available on your machine to be loaded 
 * `module load openmpi_pgi/15.7` loads the openmpi_pgi/15.7 module
  3. Change Directories 
`cd $CMAQ_LIB`<br>
  4. Make a directory called netcdf
`mkdir netcdf`
  5. Link to the lib and include directories that are installed on your machine for the compiler that you specified.
`ln –s /usr/local/netcdf-4.1.1/Linux86-64-pgf90-gcc_157/lib lib`
`ln -s /usr/local/netcdf-4.1.1/Linux86-64-pgf90-gcc_157/include include`

The CMAQ compilation scripts assume that the I/O API library resides in the `$CMAQ_LIB/ioapi` directory. For example, if you are using an x86_64 architecture and the Portland Group Fortran compiler and your I/O API installation is located in /usr/lib/ioapi_31:

`mkdir $CMAQ_LIB/ioapi`<br>
`cd $CMAQ_LIB/ioapi`<br>
`ln –s  /usr/lib/ioapi_31/20151106/Linux2_x86pg_pgcc_nomp15.7 lib`<br>
`ln –s /usr/lib/ioapi_31/20151106/Linux2_x86pg_pgcc_nomp15.7 include`<br>
`ln –s /usr/lib/ioapi_31/20151106/ioapi/fixed_src src`

The CMAQ compilation scripts assume that an MPI library and INCLUDE files reside in the `$CMAQ_LIB/mpi` directory. For example, if you are using OpenMPI located in /usr/mpi/pgi/openmpi:

`cd $CMAQ_LIB`<br>
` ln –s /usr/mpi/pgi/openmpi mpi`

### Compiling CMAQ ###

Compile the model builder, bldmake:

`cd $CMAQ_HOME/UTIL/bldmake/src`<br>
`make`

Create the model executables for ICON, BCON, and CCTM:

`cd $CMAQ_HOME/PREP/icon/scripts`<br>
`./bldit.icon |& tee bldit.icon.log`

If you get the following error
```
Makefile generated
mpif90 -c -Mfixed -O3 -Mextend -I /nas02/home/l/i/lizadams/CMAQ_TESTING/EPA-CMAQ/lib/x86_64/pgi/ioapi/lib -I /nas02/home/l/i/lizadams/CMAQ_TESTING/EPA-CMAQ/lib/x86_64/pgi/ioapi/src   UTILIO_DEFN.F
make: mpif90: Command not found
make: *** [UTILIO_DEFN.o] Error 127
**ERROR** while running make command
```
Then you are using a compiler that is different than the compiler that was used to build the ioapi library.
Re-check that you are using the correct compiler and compiler version and retry the bldit.icon command.

`cd $CMAQ_HOME/PREP/bcon/scripts`<br>
`./bldit.bcon`

`cd $CMAQ_HOME/CCTM/scripts`<br>
`./bldit.cctm`

### Install the CMAQ input reference/benchmark data

Dwnloaded the CMAQ reference data from the [CMAS Center Software Clearinghouse](https://www.cmascenter.org/download/software.cfm) and copy to `$CMAQ_HOME`. Navigate to the `$CMAQ_HOME` directory, unzip and untar the `CMAQv5.2.DATA.tar.gz` file:

`cd $CMAQ_HOME`<br>
`tar xvzf CMAQv5.2.DATA.tar.gz`

### Running the CMAQ Installation Test Simulation

To run the test simulation for the various CMAQ programs, change directories to the location of each program and execute the run script.

Run ICON to produce initial conditions:

`cd $CMAQ_HOME/PREP/icon/scripts`<br>
`./run.icon >&! icon.log &`

Run BCON to produce boundary conditions:

`cd $CMAQ_HOME/PREP/bcon/scripts`<br>
`./run.bcon >&! bcon.log &`

Check the ICON and BCON log file to ensure that the programs completed successfully.

#### Configure the CCTM script for MPI

For an MPI configuration with 6 processors,

`cd $CMAQ_HOME/CCTM/scripts`<br>

Edit the CCTM run script (run.cctm) for the MPI configuration that you will use:

`setenv NPROCS 6`<br>
`setenv NPCOL_NPROW “3 2”`

Most clustered multiprocessor systems require a command to start the MPI run-time environment. The default CCTM run script uses the *mpirun* command. Consult your system administrator to find out how to invoke MPI when running multiprocessor applications. F

For single-processor computing, set NPROCS to 1 and NPCOL_NPROW to “1 1"

For single-processor computing,

`setenv NPROCS 1`<br>
`setenv NPCOL_NPROW to “1 1"`

After configuring the MPI settings for your Linux system, run the CCTM:

`./run.cctm >&! cctm.log &`

***

[Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch01_intro.md)<br>
CMAQ Operational Guidance Document (c) 2016<br>
