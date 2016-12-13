[<< Previous Chapter](CMAQ_OGD_ch04_science) - [Home](CMAQ_OGD_index) - [Next Chapter >>](CMAQ_OGD_ch06_req_lib)

CMAQ System Requirements and Installation
=========================================

This section provides recommended hardware configurations and software requirements for installing and running CMAQ. The hardware configurations in particular are subject to change with each new release of CMAQ and with the development of new computing technologies. The installation instructions in this section guide the user through obtaining the CMAQ source code and installing it on your system. Brief instructions for running the CMAQ benchmark case and benchmarking the model are also addressed. Here, the term “benchmarking” refers to the process of verifying that a model has installed correctly on a new computer. CMAQ is distributed with a reference dataset that can be used to benchmark the CMAQ installation; in the distribution, output data from CMAQ are bundled with the input data (including emissions and meteorology) that can be used to reproduce the reference results.

After benchmarking has been successfully completed, the CMAQ system can be configured for other simulations. The same steps that are required to build the model for the benchmark case apply to building it for new simulations. Configuring CMAQ for new applications is covered in Section 9.

System Recommendations
----------------------

All of the CMAQ programs are written in Fortran and are optimized for use on computers running a version of the Linux operating system (OS). Most personal computers (PCs) running a Linux OS are sufficiently powerful to handle basic CMAQ applications. However, to use CMAQ in a production environment where multiple iterations of the model will be executed for different spatial domains and/or emissions control strategies, either a cluster of multiprocessor PCs on a high-end network or an expandable rack-mounted Linux server is recommended. In light of the dynamic nature of the computer hardware world, the specifications listed in this section are current recommendations, not requirements. While there are minimum levels of processing power and disk capacity needed for running CMAQ, there is no single system on which CMAQ is optimized to run. The flexibility of the modeling system enables users to optimize CMAQ for most current hardware configurations.

CMAQ is distributed and supported for executing on Linux operating systems with the Intel Fortran, Portland Group Fortran (PGF), or Gnu Fortran compilers. CMAQ can be ported to most computers running Linux. Documented configurations include the SGI Altix, Red Hat Enterprise, Fedora, Ubuntu, Mandrake, MacOSX, and Suse operating systems. In addition to the Intel and PGF compilers, CMAQ has been built with Sun and Absoft compilers. Information about these ports and up-to-date hardware recommendations are available through the CMAS Center web site ([<http://www>. cmascenter.org](http://www.cmascenter.org/)). The hardware recommendations provided below are the minimum level required to run CMAQ. The software recommendations may be considered as “requirements,” as all of the necessary source code libraries and utilities needed for running CMAQ are listed.

### Hardware

The minimum hardware requirements for running the CMAQ benchmark case are:

-   PC with a single 1.0 GHz processor with a Linux operating system OS
-   1 GB RAM
-   10 GB hard drive storage (Note: the benchmark simulation requires 3 GB of free storage capacity).

Below are two examples of optimal hardware configurations for running CMAQ on multiple processors in a production environment:

*Optimal CMAQ Hardware Solution \#1*

-   4 dual-CPU 2.8 GHz Xeon IBM BladeCenter rack-mounted nodes with Red Hat Enterprise Linux OS
-   2 GB RAM per node
-   Gigabit Ethernet network
-   1.5 TB hard drive storage
-   Super DLT 110 GB tapes for system backups
-   Uninterruptible power supply (UPS)

*Optimal CMAQ Hardware Solution \#2*

-   8 dual-CPU 2.5 GHz AMD Athlon MP 2000+ PCs with Ubuntu Linux O/S
-   2.0 GB RAM per PC
-   Gigabit Ethernet network
-   80 GB system storage
-   10 TB IDE-SCSI RAID 5 array
-   UPS

### Software

CMAQ requires all of the programs listed in [Table 5‑1](#Table5-1 "wikilink"); this list includes the programs distributed with CMAQ. CMAQv5 requires I/O API version 3.1. It will not compile with earlier versions of the I/O API library. [Table 5‑2](#Table5-2 "wikilink") lists additional utility software that is not required for running CMAQ, but is useful for model diagnostics and evaluation. Note that MPICH needs to be installed by the system administrator because this program is specific to the computing platform.

<span id=Table5-1></span>

<center>
**Table 5‑1. Software required for running CMAQ**

</center>
|---|---|---|
|**Software**|**Description**|**Source**|
|<center>
***CMAQ Programs***

</center>|
|Bldmake|Models-3 program builder for source code management and code compilation|<center>
Contained in the standard CMAQ distribution available at [<http://www.cmascenter.org>](http://www.cmascenter.org/)

</center>
<center>
Release notes and documentation available at [<http://www.cmaq-model.org>](http://www.cmaq-model.org/)

</center>|
|JPROC|Photolysis rate preprocessor|
|ICON|Initial conditions preprocessor|
|BCON|Boundary conditions preprocessor|
|MCIP|Meteorology-Chemistry Interface Processor|
|CCTM|CMAQ Chemistry-Transport Model|
|CHEMMECH|Chemical mechanism compiler for modifying or adding reactions to the CMAQ chemistry|
|LTNG\_2D\_DATA|Lightning flash count preprocessor|
|PROCAN|Process analysis preprocessor for setting up CMAQ to generate integrated reaction rates or integrated process rates|
|<center>
***Compilers***

</center>|
|IFORT|Intel Fortran 90 compiler|[<http://www.intel.com>](http://www.intel.com/)|
|PGF90|Portland Group Fortran 90 compiler|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|GFORT|Gnu Fortran compiler|[<http://gcc.gnu.org/fortran/>](http://gcc.gnu.org/fortran/)|
|GCC|Gnu C compiler|[<http://gcc.gnu.org/>](http://gcc.gnu.org/)|
|<center>
***Code libraries***

</center>|
|STENEX|CMAQ stencil exchange library for parallel job management|<center>
Contained in the standard CMAQ distribution available at [<http://www.cmascenter.org>](http://www.cmascenter.org/)

</center>|
|PARIO|CMAQ parallel input/output management library|
|MPICH|Library for the message passing interface; used for multiprocessor CMAQ simulations|[<http://www.mcs.anl.gov/research/projects/mpich2/>](http://www.mcs.anl.gov/research/projects/mpich2/)|
|netCDF|Network Common Data Form library for controlling CMAQ file formats|[<http://my.unidata.ucar.edu/content/software/netcdf/index.html>](http://my.unidata.ucar.edu/content/software/netcdf/index.html)|
|I/O API|Input/Output Application Programming Interface for controlling internal and external communications|[<https://www.cmascenter.org/ioapi/>](https://www.cmascenter.org/ioapi/)|
|LAPACK|Linear algebra packages for use with the bidirectional mercury module|[<http://www.netlib.org/lapack/>](http://www.netlib.org/lapack/)|
|BLAS|Basic Linear Algebra Subprograms|[<http://netlib.org/blas/>](http://netlib.org/blas/)|
|***for versions prior to CMAQ-5.0.2***|
|CVS|Concurrent Versions System for managing the distributed archive of the CMAQ source code|[<http://ximbiot.com/cvs/cvshome/>](http://ximbiot.com/cvs/cvshome/) or your host's package management system|

<span id=Table5-2></span>

<center>
**Table 5‑2. Optional support software for CMAQ**

</center>
|---|---|---|
|**Software**|**Description**|**Source**|
|<center>
***Evaluation and visualization tools***

</center>|
|VERDI|Visualization Environment for Rich Data Interpretation for graphical analysis of netCDF gridded data|[<http://www.verdi-tool.org>](http://www.verdi-tool.org/)|
|PAVE|Package for Analysis and Visualization of Environmental data for graphical analysis of netCDF gridded data|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|IDV|Integrated Data Viewer for 3-D graphical analysis of netCDF gridded data|[<http://www.unidata.ucar.edu/software/idv/>](http://www.unidata.ucar.edu/software/idv/)|
|I/O API Tools|Postprocessing tools for manipulating data in the I/O API/netCDF format|[<https://www.cmascenter.org/ioapi/>](https://www.cmascenter.org/ioapi/)|
|netCDF Tools|Postprocessing tools for manipulating data in the netCDF format|[<http://my.unidata.ucar.edu/content/software/netcdf/index.html>](http://my.unidata.ucar.edu/content/software/netcdf/index.html)|
|<center>
***Source code diagnostics***

</center>|
|PGDBG|Portland Group Fortran 90 debugger|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|PGPROF|Portland Group Fortran 90 code profiler|[<http://www.pgroup.com/>](http://www.pgroup.com/)|

Installing and Compiling CMAQ Source Code
-----------------------------------------

After installing the [I/O API](#Input.2FOutput_Applications_Programming_Interface_.28I.2FO_API.29 "wikilink") and [netCDF](#Network_Common_Data_Form_.28netCDF.29 "wikilink") libraries, Fortran and C compilers, and CVS if required for your CMAQ version, users should then download the CMAQ source code, scripts, and benchmark data files from the CMAS Center web site ([<http://www.cmascenter.org>](http://www.cmascenter.org/)). After registering to download CMAQ on the CMAS Center Software Clearinghouse, users are redirected to a page that contains links to download Linux tar files of the CMAQ code, scripts, and benchmark data along with various documents describing the installation and execution processes.

### Distribution contents

The following files and archives compose the CMAQ distribution:

-   `CMAQv5.tar.gz` – gzipped tar file (\~7.2 MB) containing model, tools, and libraries source code
-   `CMAQv5.twoway.tar.gz` - gzipped tar file containing code for coupling WRF3.3 and CMAQ5.0
-   `DATA.CMAQv5.tar.gz` – gzipped tar file (\~52 MB) containing the required datasets necessary to run the benchmark case.
-   `SCRIPTS.CMAQv5.tar.gz` – gzipped tar file (\~16 KB) containing C-shell scripts to build and execute the CMAQ models
-   `DATA_REF.CMAQv5.tar.gz` – gzipped tar file (\~217 MB) containing reference data to compare with datasets produced by the tutorial on a Linux workstation

The CMAQ installation scripts are configured for a Linux system with either the Portland Group, Intel, or Gnu Fortran compiler.

### Notes on the CMAQ directory structure

The CMAQ installation includes a dataset for benchmarking the modeling system. Unpacking the various tar files of the distribution in the `M3HOME` directory installs the CMAQ source code, scripts, and benchmark data files in a directory structure recognized by the default run and build scripts. The `M3HOME` directory is the base location of the CMAQ installation for a specific application. Under `M3HOME`, the `scripts` directory contains the build and run scripts, the `models` directory contains the model source code (in a CVS archive, if CMAQ version \<= 5.0.1), the `data` directory contains the input and output data for the model, and the `lib` directory contains the compiled binary library files required to build the CMAQ executables. The CMAQ scripts use the following environment variables to alias the locations of these directories:

`M3LIB   = $M3HOME/lib`
`M3DATA  = $M3HOME/data`
`M3MODEL = $M3HOME/models `

The CMAQ scripts require users to select only the location of the `M3HOME` directory; the other CMAQ directories are relative to `M3HOME`. While this directory structure is convenient for the benchmark case and most CMAQ applications, other configurations are possible. Detailed instructions for installing and compiling CMAQ are contained in the next section.

### Configuring your system for compiling CMAQ

Compiler flag consistency between the Fortran and C compilers used to build netCDF and I/O API is critical for building library files compatible with CMAQ. [Table 5-3](#Table5-3 "wikilink") lists the suggested compilation options for building netCDF and I/O API libraries that are compatible with CMAQ. Refer to the documentation for these libraries for additional information on installation and compiling.

<span id=Table5-3></span> **Table 5‑3. NetCDF and I/O API compilation options for CMAQ**

|---|---|---|---|
|<center>
**Library Type**

</center>|<center>
**Intel Fortran**

</center>|<center>
**PGI Fortran**

</center>|<center>
**Gnu Fortran**|
|netCDF|CC = icc

CPPFLAGS = \`-DNDEBUG

–DpgiFortran\`

CFLAGS = \`-g –O\`

FC = ifort

F77 = ifort

FFLAGS = \`-g –O2 –mp –recursive\`

CXX = icpc|CC = gcc

CPPFLAGS = \`-DNDEBUG

–DpgiFortran\`

CFLAGS = -O

FC = pgf90

FFLAGS = \`-O –w\`

CXX = g++|CC = gcc

CPPFLAGS = \`-DNDEBUG

–DgFortran\`

CFLAGS = -O

FC = gfortran

FFLAGS = \`-O –w\`

CXX = g++|
|I/O API 32-bit|BIN = Linux2\_x86ifort|BIN = Linux2\_x86pg\_pgcc\_nomp|N/A|
|I/O API 64-bit|BIN = Linux2\_x86\_64ifort|BIN = Linux2\_x86\_64pg\_pgcc\_nomp|BIN = Linux2\_x86\_64gfort|

</center>
#### config.cmaq

Consistency of configuration variables is critical for building CMAQ itself, not just its libraries. Accordingly CMAQ now distributes with `CMAQv5.0/scripts/config.cmaq` to help prevent multiple, inconsistent settings. [Table 5-4](#Table5-4 "wikilink") lists variables defined for the build process and suggests values to which to set those variables.

<span id=Table5-4></span> **Table 5‑4. config.cmaq configuration variables**

|---|---|
|<center>
**Variable Name**

</center>|<center>
**Suggested Value**

</center>|
|`M3HOME`|the directory to which you are installing CMAQ. For example, if you installed the CMAQ source code in the directory

`/home/user/CMAQv5.0`

set the M3HOME environment variable with (e.g.)

`export M3HOME='/home/user/CMAQv5.0' # bash`

or

`setenv M3HOME /home/user/CMAQv5.0   # csh`|
|`M3DATA`|should be automatically set by config.cmaq|
|`M3LIB`|should be automatically set by config.cmaq|
|`M3MODEL`|should be automatically set by config.cmaq|
|`myFC`|should match the `FC` (Fortran compiler) you used to compile netCDF|
|`myCC`|should match the `CC` (C compiler) you used to compile netCDF|
|`myFFLAGS`|should match the `FFLAGS` (Fortran flags) you used to compile netCDF|
|`myCFLAGS`|should match the `CFLAGS` (C flags) you used to compile netCDF|
|`myFRFLAGS`||
|`MPI_INC`|should give the path to your MPI library includes, e.g. `$M3LIB/mpich/include`|
|`mpi`|should denote your MPI library, e.g. `-lmpich`|
|`extra_lib`|other libraries to be included in compilation, e.g. `-lmpiif`|
|`EXEC_ID`|build tag, should be automatically set by config.cmaq|

### Installing CMAQ on your system

To install CMAQ (with examples using a C-shell environment, a Red Hat Linux system, and the Portland Group Fortran compiler):

-   In the directory where you would like to install CMAQ, unzip and untar the model distribution file:

`tar xvzf CMAQv5.0.tar.gz`

This will produce the following subdirectories:

`CMAQv5.0/lib`
`CMAQv5.0/models`
`CMAQv5.0/scripts`

-   Edit the file `CMAQv5.0/scripts/config.cmaq` to set the environment variable `M3HOME` as discussed in [Table 5-4](#Table5-4 "wikilink").

-   Edit the file `CMAQv5.0/scripts/config.cmaq` to configure the CMAQ installation for the local computing architecture and compilers. Under the “\#architecture & compiler specific settings” section of the script there are three example compiler configurations: (1) Intel Fortran, (2) Portland Group Fortran, and (3) Gnu Fortran. Configure this section of the script for your system using the comment character (\#) to deactivate the settings that will not be used; similarly, uncomment (and, if necessary, edit) the settings that will be used to compile CMAQ. Save and exit from the config.cmaq file and use the source command to invoke the settings in the file:

`source CMAQv5.0/scripts/config.cmaq`

-   Navigate to the `$M3HOME` directory, unzip and untar the `CMAQv5.0.DATA.tar.gz` file:

`cd $M3HOME`
`tar xvzf CMAQv5.0.DATA.tar.gz`

This will produce the following subdirectories:

`CMAQv5.0/data/`
`bcon/`
`cctm/`
`crop/`
`dust/`
`emis/`
`icon/`
`jproc/`
`lightning/`
`mcip/`
`ocean/`
`procan/`
`raw/`
`phot/`
`lnox/`

-   Create the `M3LIB` directory under the `$M3HOME` directory:

`cd $M3HOME`
`mkdir -p $M3LIB`

The library files that you install in the library directory will depend on the system and compiler that you specified in the config.cmaq script. For example, if you are running on an x86\_64 architecture with the Portland Group Fortran compiler, the config.cmaq script will set `M3LIB` to `$M3HOME/lib/x86_64/pgf`.

-   Install or link the netCDF, I/O API, and MPICH libraries in the M3LIB directory. The CMAQ compilation scripts assume that the netCDF library and INCLUDE files reside in the `$M3LIB/netCDF` directory. If netCDF is installed elsewhere on your system, create a symbolic link in \$M3LIB/netcdf:

`cd $M3LIB`
`ln –s /lib/netcdf netcdf`

A listing of the netCDF directory under `M3LIB` should show the three subdirectories of the native netCDF installation:

`bin/ `
`include/ `
`lib/`

The CMAQ compilation scripts assume that the I/O API library resides in the `$M3LIB/ioapi_3.1` directory. If I/O API is installed elsewhere on your system, create a symbolic link in `$M3LIB/ioapi_3.1`. For example, if you are using an x86\_64 architecture and the Portland Group Fortran compiler:

`mkdir $M3LIB/ioapi_3.1`
`cd $M3LIB/ioapi_3.1`
`ln –s /lib/ioapi_31/Linux2_x86pg_pgcc_nomp Linux2_x86_64pgf`

A list of the alternative I/O API library directory names expected by the CMAQ compile and run scripts for 32-bit and 64-bit operating systems using the Intel, Portland Group, and Gnu compilers include:

-   Portland Group 32-bit Linux: Linux2\_x86pg
-   Portland Group 64-bit Linux: Linux2\_x86\_64pg
-   Intel 32-bit Linux: Linux2\_x86ifort
-   Intel 64-bit Linux: Linux2\_x86\_64ifort
-   Gnu Fortran 32-bit Linux: Linux2\_x86gfort
-   Gnu Fortran 64-bit Linux: Linux2\_x86\_64gfort

Other operating systems and compiler combinations are possible but may require manual configuration of the CMAQ scripts to point to the correct directory paths for the netCDF and I/O API libraries.

The CMAQ compilation scripts assume that the MPICH library and INCLUDE files reside in the `$M3LIB/MPICH` directory. Create a symbolic link to the MPICH installation on your system:

`cd $M3LIB`
` ln –s /lib/mpich mpich`

After unpacking all of the CMAQ tar files under the `$M3HOME` directory and installing the I/O API, netCDF, and MPICH libraries in the `$M3LIB` directory, the CMAQ executables can then be compiled.

### CVS

If you are building CMAQ-5.0.1 or older, check to make sure CVS is available on your system by typing the following command:

`which cvs`

If the message “cvs: Command not found.” is returned, you must install CVS on your system before you can continue with the CMAQ installation.

Benchmarking
------------

Benchmarking is the process of confirming that the model source code compiles and executes correctly on a new computer system. CMAQ should be benchmarked on a computing system before the model is used for actual applications on that system. The purpose of benchmarking is to ensure that there are no inconsistencies introduced into the model solution from local system parameters, such as compilers, processors, or operating systems. While differences are expected in the CMAQ results produced by different operating systems, hardware, and compilers, these differences should be small and within the numerical error of the model. Input and output reference data are packaged with CMAQv5 to use for benchmarking the model on new systems. After running the test case packaged with the source code, compare the results against the reference data provided in the CMAQv5 distribution.

### CMAQ benchmark parameters

The CMAQv5 benchmark test case is a single day simulation for August 1, 2006 on a 127 column x 122 row x 35 layer 12-km resolution domain over the Southeast U.S. The CCTM configuration parameters for the benchmark test case include the following:

-   Multiprocessor simulation
-   Horizontal advection: Yamo
-   Vertical advection: WRF
-   Horizontal diffusion: Multiscale
-   Vertical diffusion: ACM2
-   Deposition: M3Dry
-   Chemistry solver: EBI
-   Aerosol module: AERO6
-   Cloud module: ACM
-   Mechanism: cb05tucl\_ae6\_aq
-   In-line windblown dust calculation with agricultural activity
-   Lightning NOx with emissions calculated off-line
-   Dynamic vertical diffusivity
-   In-line deposition velocities
-   Surface HONO interaction
-   In-line biogenic emissions
-   In-line plume rise

The system configuration parameters used to generate the benchmark reference data include the following:

-   Hardware: Dell C6100 server, 2.93 GHz Intel processor, 12M L3 cache (Model X5670), 48 GM memory
-   Operating System: RHEL 5.6
-   Compiler: Intel 11.1
-   MPI: MVAPICH 2.1.7
-   12 processors

### Compiling CMAQ for the Benchmark Test Case Simulation

For all CMAQ programs (other than MCIP), the program Bldmake is used to compile the source code into executables. The first step in the compilation of CMAQ is to compile Bldmake. Then compile the program libraries STENEX and PARIO before moving on to compiling the rest of the CMAQ programs. *For all of the CMAQ programs and libraries, the directory paths for the Fortran and C compilers in the build scripts have to be changed to reflect the correct locations on your system*.

A new feature in the CMAQv5 compilation system is the centralization of the compiler configuration into the config.cmaq script. None of the CMAQ build scripts contain compiler settings. Instead, the build scripts for each program reference the config.cmaq script using the Linux command “source”.

All of the CMAQ programs other than CCTM are run in single-processor mode. CCTM may be run either in single-processor (serial) mode or in parallel on multiple processors. Program-specific compilation instructions are provided below. These compilation instructions are for building executables for simulating the benchmark data sets distributed with CMAQ. Additional information about the configuration options for the various CMAQ programs is provided in [Section 4](#Overview_of_the_Science_in_the_CMAQ_Modeling_System "wikilink") and [Section 7](#CMAQ_Programs_and_Libraries "wikilink").

**`Note` `about` `compiling` `with` `netCDF4`**
`The Fortran and C components of the netCDF library are separated in netCDF version 4.1 `
`and greater. When linking the CMAQ programs with netCDF4.1+ it's necessary to include `
`both the Fortran and C netCDF libraries, in a specified order, in the CMAQ build scripts.`
`When using netCDF4 replace all instances of '-lnetcdf' in the CMAQ build scripts with`
`'-lnetcdff -lnetcdf'.`

-   Use the following commands to compile Bldmake:

`cd $M3HOME/scripts/build`
`bldit.bldmake`

-   Next create the stencil exchange (STENEX) libraries for serial and parallel processing. Verify that the MPI INC variable in the bldit.se file is pointing to the correct directory path for the MPICH INCLUDE files on your system. Use the following commands to compile the STENEX libraries:

`cd $M3HOME/scripts/stenex`
`bldit.se_noop`
`bldit.se`

-   For parallel CCTM operation, create the parallel input/output (PARIO) library:

`cd $M3HOME/scripts/pario`
`bldit.pario`

Now create the model executables for JPROC, ICON, BCON, MCIP, and CCTM.

-   For the benchmark case, confirm that JPROC is configured to produce photolysis rates for the mechanism labeled “cb05tucl\_ae6\_aq”. Use the following command to compile JPROC:

`cd $M3HOME/scripts/jproc`
`bldit.jproc`

Note that because the in-line photolysis option is used for the benchmark simulation, JPROC output data are not actually needed to benchmark the model.

-   ICON and BCON can be configured for different chemical mechanisms and for different kinds of input data. The configuration options for ICON and BCON are discussed in detail in Sections 4 and 8. Use the following commands to compile ICON and BCON:

`cd $M3HOME/scripts/icon`
`bldit.icon`

`cd $M3HOME/scripts/bcon`
`bldit.bcon`

-   MCIP is compiled using a Fortran Makefile instead of Bldmake. To create the MCIP executable, set the compiler, compiler flags, and netCDF and I/O API library paths in the Makefile distributed with MCIP.

Once you have configured the MCIP Makefile for your system, use the following commands to compile MCIP:

`cd $M3HOME/scripts/mcip4/src`
`make`

Note that the CMAQ benchmark input data include MCIP output data, so MCIP does not actually need to be run to benchmark CMAQ.

-   CCTM has multiple configuration options that can be changed to optimize model performance for different applications. In addition to selecting the chemical mechanism to model gas-phase chemistry, the user can also select from several different science modules. The science configuration options for CCTM are discussed in detail in Sections 4 and 8. The CCTM build script is configured by default to create a multiprocessor executable for the benchmark simulation. For multiprocessor applications, CMAQ uses the MPICH message passing interface (MPI) to manage communication between processors in a clustered multiprocessor computing environment. Before compiling CCTM for parallel execution, you must specify the location of the MPICH directory on your system in the CCTM build script. For single-processor (serial) systems, configure the CCTM build script to create a single-processor executable by commenting out the line that activates the variable “ParOpt” of the CCTM build script. Use the following commands to compile CCTM:

`cd $M3HOME/scripts/cctm`
`bldit.cctm`

Although not used for the benchmark simulation, the PROCAN processor can also be compiled using Bldmake. The chemical mechanism compiler, CHEMMECH, is also not needed for the benchmark simulations but it can be compiled using a Makefile.

### Running the CMAQ Benchmark Simulation

After successfully compiling the various CMAQ programs, use the distributed run scripts to generate the CCTM input files and then to run CCTM for the CMAQ benchmark case. CCTM must be run last in the simulation sequence; MCIP must be run first. Note however that CMAQ-ready meteorology data are distributed with the CMAQv5 benchmark case, which means that MCIP does not actually need to be run to benchmark the model. With the exception of MCIP, there are no dependencies among the other CMAQ programs, so they can be run in any order to create input data for CCTM.

To run the benchmark simulation for the various CMAQ programs, change directories to the location of each program and execute the run script. For example, to run ICON to produce initial conditions:

`cd $M3HOME/scripts/icon`
`./run.icon >&! icon.log &`

Check the ICON log file to ensure that the program completed successfully. Follow the same procedures for BCON and JPROC.

CCTM is configured by default to run in multiprocessor mode. Running in multiprocessor mode requires building a CCTM executable that includes routines from the MPICH library and setting the number of processors to allocate to the simulation and the location of the MPI initialization command (mpirun) on your system. Configuring the CCTM run script for parallel processing requires selecting the number of processors to use for the simulation by setting the NPROCS environment variable, and choosing the domain decomposition configuration by setting the variable NPCOL\_NPROW. The number of processors must be equal to the product of the two values selected for NPCOL\_NPROW. For example, if you have a system with six processors available to run CMAQ, set NPROCS to 6 and NPCOL\_NPROW equal to “3 2”. For 6 processor computing,

`setenv NPROCS 6`
`setenv NPCOL_NPROW “3 2”`

Most clustered multiprocessor systems require a command to start the MPI run-time environment. The CCTM run script is configured by default to use the *mpirun* command. Consult your system administrator to find out how to invoke MPI when running multiprocessor applications. For single-processor computing, set NPROCS to 1 and NPCOL\_NPROW to “1 1"

For single-processor computing,

`setenv NPROCS 1`
`setenv NPCOL_NPROW to “1 1"`

### Benchmarking CMAQ

After completing the CMAQ benchmark case, the CCTM output files can be compared with the reference datasets provided in the CMAQ distribution. (As of CMAQ-5.0.1, the reference [tarball](http://en.wikipedia.org/wiki/Tar_%28computing%29) for "one-way" CMAQ is named DATA\_REF.tar.gz, and the reference tarball for two-way coupled WRF-CMAQ is named DATA\_REF.WRF3.3-CMAQv5.tar.gz.) A plot of percent difference between the output and the benchmark data is the easiest method.

If the benchmark case is run on a system similar to that used to create the reference data (as of CMAQ-5.0.1, [RHEL](http://en.wikipedia.org/wiki/Red_Hat_Enterprise_Linux) with the [Gnu Fortran](https://en.wikipedia.org/wiki/Gfortran) compiler), the results should differ by no more than 0.5% for every model species. Changing the optimization of the compiler or compiling on other operating systems with different compilers can lead to larger differences between the benchmark results and the reference datasets. The CCTM benchmark targets for compilers other than those listed in [Table 5.1](#Table5-1 "wikilink") are differences less than 1% for every model species. Differences greater than this require a review of the installation. Remember it is necessary that the same compilers be used for all programs.

[<< Previous Chapter](CMAQ_OGD_ch04_science) - [Home](CMAQ_OGD_index) - [Next Chapter >>](CMAQ_OGD_ch06_req_lib)
