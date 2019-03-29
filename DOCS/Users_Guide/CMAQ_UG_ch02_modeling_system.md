
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch03_input_and_output.md)

<!-- END COMMENT -->

# 2. Modeling System

## 2.1 Program Structure

**>>COMMENT<<** Are these really the four "main CMAQ programs"?

**>>COMMENT<<** Here (and elsewhere), need to be consistent in what "CMAQ" means and "CCTM" means.  They seem to be synonymous here.  Elsewhere CCTM is the model, while CMAQ is the entire system. 

**>>COMMENT<<**  Ensure the list of utility programs is complete (EPIC?) and includes post-proc (e.g., AMET, VERDI).

**>>COMMENT<<** a system graphic would be helpful here

**>>COMMENT<<**  Sections 2.1 and 2.2  don't belong under the heading of "overview of components"

CMAQ is a suite of Fortran90 programs that work in concert to estimate tropospheric distributions and deposition of ozone, PM, toxic compound and acidic substances. The four main CMAQ programs are:

-   The initial conditions processor [ICON](#icon)
-   The boundary conditions processor [BCON](#bcon)
-   The Meteorology-Chemistry Interface Processor [MCIP](#mcip)
-   The CMAQ Chemistry-Transport Model [CCTM](#cctm)

Utility programs distributed with CMAQ include:

-   The code builder/manager [Bldmake](#bldmake)
-   The chemical mechanism compiler [chemmech](#chemmech)
-   EBI chemistry solver builder [create_ebi](#create_ebi)
-   The inline photolysis preprocessor [inline_phot_preproc](#inline_phot_preproc)
-   The namelist converter [nml](#nml)

The following sections describe the CMAQ system concept, followed by [details of the programs listed above](#summary-descriptions-of-the-major-cmaq-programs).

**>> Comment <<** Section 7 doesn’t really mention a couple of the input files: how to make the OCEAN file? Mention the GRIDDESC file and options to where to get this info (like MCIP)

The core CMAQ programs that are needed to perform a basic air quality model simulation are [MCIP](#mcip), [ICON](#icon), [BCON](#bcon), and [CCTM](#cctm). The relationships among these programs are depicted within the green box in [Figure 2-1 CMAQ Core Programs](#Figure7-1). The blue boxes represent programs that are not part of the CMAQ distribution package but supply data necessary for an air quality simulation (emissions and meteorology data). The yellow boxes represent the standard CMAQ preprocessors: MCIP, ICON, and BCON. The red box represents the CMAQ chemistry-transport model (CCTM), the Eulerian air quality modeling component of CMAQ. Data flows between the CMAQ programs are represented in by arrows. The red arrows illustrate the flow of data from the CMAQ preprocessors and the emissions model to CCTM. The green arrows show the data feedbacks from CCTM to create initial and boundary conditions for nested simulations. The black arrow illustrates the connection between the meteorological model and MCIP. Finally, the blue arrow shows that the output from MCIP can be used to drive an emissions model.

A meteorological model, such as [WRF‑ARW](http://www.wrf-model.org), generates gridded meteorology for input to both CMAQ and the emissions model. These data are processed for input to CMAQ using MCIP.

An emissions model converts emissions inventories to gridded, hourly emissions formatted for CMAQ. The [SMOKE](http://www.smoke-model.org) emissions model is currently available for preparing emissions data for CMAQ.

CMAQ includes several "in-line" options to support coupling between meteorology and chemistry processes, and to facilitate operational air quality forecast modeling (see [Chapter 4](CMAQ_UG_ch04_model_formulation.md)). The user can incorporate photolysis rate calculations and emissions processing during a CCTM simulation. There are several advantages of incorporating these processes directly in a CCTM simulation:

1. Photolysis rate calculations use the aerosol concentrations and meteorology from the CCTM simulation, simulating the feedbacks of the input emissions and resulting air quality on photochemistry
2. Emissions are meteorologically modulated at the synchronization (chemistry) time step rather than being linearly time-interpolated within each simulation hour
3. Disk space may be saved, because a 3‑D emissions file is no longer needed for elevated point sources
4. CMAQ can more easily be coupled with a meteorological model, enabling direct emissions modulation by the underlying, freshly computed meteorological variables

**>> Comment <<** Fig. 2-1 seems unnecessary.  A better version of this figure should be developed (where optional codes are shown) and it should appear earlier in the document.  The quality of this figure also needs to be improved.



![Figure 2-1](./images/Figure7-1.png)

**Figure 2‑1.CMAQ core programs**

**>> Comment <<** Not sure that MCIP is the "first program…that a user should run".  There may be emissions-related programs that could/should be run earlier.

[MCIP](#mcip) is the first program in the CMAQ distribution package that a user should run when setting up a new simulation. MCIP is used to preprocess the data from a meteorological model for CMAQ and SMOKE.

[ICON](#icon) creates a binary netCDF initial conditions file for input to CCTM. Users have the option to create initial conditions data either from a text file of vertical concentration profiles or from an existing CCTM output file. ICON outputs initial conditions data that are configured for a specific modeling grid and chemical parameterization.

[BCON](#bcon) creates a binary netCDF lateral boundary conditions file for input to CCTM. Users have the option to create boundary conditions from either a text file of vertical concentration profiles or from an existing CCTM or larger-scale (e.g., global-scale) output file. BCON outputs boundary conditions data that are configured for a specific modeling grid and chemical parameterization. If derived from an existing CCTM or larger-scale output file, BCON produces dynamic boundary conditions that vary in time and space. When derived from vertical concentration profiles, BCON produces static boundary conditions for input to CCTM.

<!---[JPROC](#jproc) converts physical information about photoreactive molecules into clear-sky photolysis rate look-up tables for input to CCTM. The in-line photolysis approach allows photolysis rates to be adjusted by simulated gas and aerosol concentrations rather than by climatological values in the off-line approach. JPROC is not required when the in-line photolysis approach is selected.
--->
[CCTM](#cctm) is run last in the sequence of programs. All of the other CMAQ programs, and the emissions and meteorological models, are used to prepare the inputs to CCTM. By using data that are synchronized for a particular modeling time period, model grid, vertical layer configuration, and chemical parameterization, CCTM can produce estimates of pollutant concentrations, wet and dry deposition rates, and visibility metrics.

In addition to the core programs shown in [Figure 2‑1 CMAQ Core Programs](#Figure7-1), the CMAQ distribution package also includes utilities `($CMAQ_HOME/UTIL)` and post-processors `($CMAQ_HOME/POST)` for utilizing additional technical and diagnostic features in CMAQ. The CMAQ utility programs [CHEMMECH](#chemmech) [CSV2NML](#chemmech), [CREATE_EBI](#create_ebi), and [INLINE_PHOT_PREPROC](#inline_phot_preproc), and [JPROC](#jproc) are used to modify or prepare new chemical mechanisms for CMAQ. The CMAQ preprocessor [CALMAP](#calmap) creates maps of the crop calendar for use in estimating windblown dust emissions. The CMAQ post-processors are described in the [CMAQv5.2.1 documentation](https://github.com/USEPA/CMAQ/blob/5.2.1/POST/README.md) and are used to prepare CMAQ output data for analysis.

**>>Comment<<** Update or remove reference to chapter 6 from old User's document

This chapter provides detailed descriptions of the CMAQ programs and utilities. Information about the third-party libraries used by CMAQ—such as I/O API, netCDF, and MPI are available in [Chapter 6](CMAQ_OGD_ch06_req_lib.md). When viewing the tables that list each program’s input and output files, recall that the various I/O API file formats shown are also described in [Chapter 6](CMAQ_OGD_ch06_req_lib.md).

[Add graphic from CMAQ website?; Maybe graphic 4-6]

## 2.2 Compiling CMAQ

**>>COMMENT<<** All of the GitHub stuff seems really detailed and weedy right now.  I don't even know what CMAQ is (at this point in the document), but I have to learn Git to get it?  I think I've given up, if I'm a new user. Do we really need the history???

Prior to CMAQ version 5.0.2, CMAQ developers used [CVS](https://en.wikipedia.org/wiki/Concurrent_Versions_System) for source code management, and distributed [tarballs](https://en.wikipedia.org/wiki/Tar_%28computing%29#Format_details) (except for MCIP) were CVS archives. Starting with version 5.0.2, CMAQ developers switched to [git](https://en.wikipedia.org/wiki/Git_%28software%29). All versions of CMAQ from 4.7.1 to the present are available for download from the [U.S. EPA GitHub repository](https://github.com/USEPA/CMAQ).

CMAQ source codes are used to build "executables": binary files that consist of instructions that have been translated from their original [source code](http://www.linfo.org/sourcecode.html) (e.g., Fortran) into [machine code](http://www.linfo.org/machine_code.html) (also called machine language or object code) so that they are ready to be run (executed). Executable files are created through the use of a specialized program called a [compiler](http://www.linfo.org/compiler.html).

**>>COMMENT<<**  "These are particularly useful…"  What are "these"?

Bldmake provides an interface to CMAQ source code repository, and to the Fortran 90 compiler for building binary executables. Because Bldmake is required to create all of the CMAQ executables except MCIP (which has its own Makefile procedure), it is the first program that needs to be compiled after installing the CMAQ source code on your system. In addition to creating executables, it also provides the option to generate a Linux Makefile. These are particularly useful for porting the CMAQ code to new operating systems, testing new code in a development environment, or trouble-shooting problems with CMAQ compilation or execution.

When cloning the repository or unpacking the tar file of the CMAQ distribution, the top-level directory is recognized by the default build and run scripts as `CMAQ_HOME` (formerly M3HOME prior to CMAQv5.2). This directory is an arbitrary base location of the CMAQ installation on your Linux system for a specific application. It's up to the user to decide where to install CMAQ. If the user will build and run CMAQ within the repository folder structure, then `CMAQ_HOME` does not need to be set explicitly. If, on the other hand, the user wishes to extract the build and run scripts and compile the model outside of the repository, then `CMAQ_HOME` will need to be specified in `bldit_project.csh`. Executing `bldit_project.csh` will automatically perform this extraction and create a CMAQ folder structure under the location now specified by `CMAQ_HOME`.

Under `CMAQ_HOME`, the `data` directory serves as a container for the input and output data for the model, and the `lib` directory contains links to the compiled binary library files required to build the CMAQ executables. The CMAQ scripts use the following environment variables to alias the locations of these directories:

`CMAQ_LIB   = $CMAQ_HOME/lib` (M3LIB before CMAQv5.2)<br>
`CMAQ_DATA  = $CMAQ_HOME/data` (M3DATA before CMAQv5.2)

The CMAQ scripts require users to select only the location of the `CMAQ_HOME` directory; the other CMAQ directories are referenced relative to `CMAQ_HOME`. While this directory structure is convenient for the benchmark case and most CMAQ applications, other configurations are possible. Detailed instructions for installing and compiling CMAQ are contained in the next section.

Compiler flag consistency between the Fortran and C compilers used to build netCDF and I/O API is critical for building library files compatible with CMAQ. [Table 2-1](#Table5-3) lists the suggested compilation options for building netCDF and I/O API libraries that are compatible with CMAQ. Refer to the documentation for these libraries for additional information on installation and compiling.


**>>COMMENT<<** 	11. Get David to review Table 2-1 on NetCDF and I/O API compilation options for CMAQ

<a id=Table5-3></a>

**Table 2-1. NetCDF and I/O API compilation options for CMAQ**

|**Library Type**|**Intel Fortran**|**PGI Fortran**|**Gnu Fortran**|
|-------------|-------------------------|-----------------------|---------------------------|
|netCDF|`CC = icc`<br> `CPPFLAGS = -DNDEBUG –DpgiFortran` <br>`CFLAGS = -g –O` <br>`FC = ifort` <br>`F77 = ifort`<br>`FFLAGS = –O2 –mp –recursive`<br> `CXX = icpc`|`CC = gcc`<br> `CPPFLAGS = -DNDEBUG –DpgiFortran`<br>`CFLAGS = -O`<br>`FC = pgf90`<br>`FFLAGS = -O –w` <br>`CXX = g++`|`CC = gcc`<br> `CPPFLAGS = -DNDEBUG –DgFortran` <br>`CFLAGS = -O`<br>`FC = gfortran`<br>`FFLAGS = -O –w`<br>`CXX = g++`|
|I/O API 32-bit|BIN = Linux2_x86ifort|BIN = Linux2_x86pg_pgcc_nomp|N/A|
|I/O API 64-bit|BIN = Linux2_x86_64ifort|BIN = Linux2_x86_64pg_pgcc_nomp|BIN = Linux2_x86_64gfort|

### 2.2.1 config_cmaq.csh

Consistency of configuration variables is critical for building CMAQ itself, not just its libraries. Accordingly CMAQ includes the configuration script `config_cmaq.csh` to help enforce consistent environment settings for CMAQ and its associated libraries [Appendix A](CMAQ_UG_appendix.md) lists the `config_cmaq.csh` variables defined for the build process and suggests values to which to set those variables.

Note that for multiprocessor applications it is recommended that the Fortran MPI wrapper script `mpif90` be specified for the Fortran compiler (myFC). Using this script, instead of a direct call to the Fortran compiler, will ensure that the full suite of MPI components (libraries and include files) for the compiler are included in the parallel build.

Use the following steps to install CMAQ (with examples using a C-shell environment, a Red Hat Linux system, and the Portland Group Fortran compiler) on a Linux system. CMAQ is not distributed with scripts for installing on Windows or MacOSX.

### 2.2.2 Obtain CMAQ source codes 

CMAQ source code can be installed either using git or from tarballs downloaded from the git repository hosted by GitHub. Both options are described here.

### 2.2.3 Git Installation 

In the directory where you would like to install CMAQ, issue the following command to clone the official EPA GitHub repository for CMAQv5.2.1:

`git clone -b 5.2.1 https://github.com/USEPA/CMAQ CMAQ_REPO`

### 2.2.4 Zip file Installation 

Zip files of the CMAQ source code are available from the public GitHub repository. Click the button "Clone or download" from https://github.com/USEPA/CMAQ/tree/5.2.1 and select "Download ZIP" to download a Zip file of the CMAQv5.2.1 repository. Alternatively, you may download the Zip file from the [EPA CMAQ website](https://www.epa.gov/cmaq/access-cmaq-source-code).

Reference input/output data for testing the installation of the software are available from the CMAS Center; *data are not available through GitHub*. You must register/login to access the source codes and data from the CMAS Center.

In the directory where you would like to install CMAQ, unzip the model distribution file:

`unzip CMAQ-5.2.1.zip`

The following directories will be created:

```
CMAQ-5.2.1/CCTM
CMAQ-5.2.1/PREP
CMAQ-5.2.1/POST
CMAQ-5.2.1/UTIL
CMAQ-5.2.1/DOCS
```

The Git and Zip file installation options will produce slightly different subdirectories on your Linux system. The base installation directory using the git clone command will be `CMAQ_REPO`; the directory from the Zip file will be `CMAQ-5.2.1`. The subsequent instructions in this guide will be based on the git clone installation. For Zip file installations, replace CMAQ_REPO with CMAQ-v5.2.1 in the instructions that follow. The differences in the directory names highlights the difference in functionality between the two options. Cloning the repository gives the user access to the full repository and its history, while downloading the Zip file will only give access to version 5.2.1.

Using the git clone option, CMAQ will install into the following directories:

```
CMAQ_REPO/CCTM
CMAQ_REPO/PREP
CMAQ_REPO/POST
CMAQ_REPO/UTIL
CMAQ_REPO/DOCS
```
### 2.2.5 Build and run in a user-specified directory outside of the repository
In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

In bldit_project.csh, modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:
```
set CMAQ_HOME = /home/username/CMAQ_v5.2.1
```
Now execute the script.
```
./bldit_project.csh
```
### 2.2.6 Install the CMAQ Libraries
The CMAQ build scripts require the following libraries and INCLUDE files to be available in the CMAQ_LIB directory (Note the CMAQ_LIB gets set automatically by the config_cmaq.csh script, where `CMAQ_LIB = $CMAQ_HOME/lib`):

- netCDF library files are located in the `$CMAQ_LIB/netcdf/lib` directory
- I/O API library, module and INCLUDE files are located in the `$CMAQ_LIB/ioapi` directory
- MPI library and INCLUDE files are located in the `$CMAQ_LIB/mpi` directory

The config_cmaq.csh script will automatically link the required libraries into the CMAQ_LIB directory. Set the locations of the netCDF, I/O API, and MPI installations on your Linux system with the following config_cmaq.csh environment variables:

- `setenv IOAPI_MOD_DIR`: the location of the precompiled I/O API module files on your system.
- `setenv IOAPI_INCL_DIR`: the location of the I/O API include header files on your system.
- `setenv IOAPI_LIB_DIR`: the location of the I/O API libraries on your system.
- `setenv NETCDF_LIB_DIR`: the location of the netCDF libraries on your system.
- `setenv NETCDF_INCL_DIR`: the location of the netCDF INCLUDE files on your system.
- `setenv MPI_LIB_DIR`: the location of the MPI (OpenMPI or MVAPICH) on your system.

For example, if your netCDF libraries are installed in /usr/local/netcdf/lib, set `NETCDF_LIB_DIR` to /usr/local/netcdf/lib. Similarly, if your I/O API library is installed in /home/cmaq/ioapi/Linux2_x86_64ifort, set `IOAPI_LIB` to /home/cmaq/ioapi/Linux2_x86_64ifort.

*1.* Check the names of the I/O API and netCDF libraries using the `ioapi_lib` and `netcdf_lib` script variables.

*2.* Check the name of the MPI library using the `mpi` script variable. For MVAPICH use `-lmpich`; for openMPI use `-lmpi`.

Links to these libraries will automatically be created when you run any of the build or run scripts. To manually create these libraries (this is optional), execute the config_cmaq.csh script, identifying the compiler in the command line [intel | gcc | pgi]:
```
source config_cmaq.csh [compiler]
```
You may also identify the version of the compiler if you wish it to be identified in build directory and executable names. This is optional. For example:
```
source config_cmaq.csh intel 17.0
```
When you source the config_cmaq.csh script the CMAQ directories, including all required libraries will be installed in the CMAQ working directory.  If you encounter errors about libraries not being found, check the settings of the config_cmaq.csh script variables IOAPI, NETCDF, or MPI to ensure that they are correctly point to the locations of these libraries on your Linux system.

### 2.2.7 Install the CMAQ input reference/benchmark data 

Download the CMAQ single day reference data from the [CMAS Center Software Clearinghouse](https://www.cmascenter.org/download/software.cfm) and copy to `$CMAQ_DATA`. Navigate to the `$CMAQ_DATA` directory, unzip and untar the single day benchmark input and output files:

```
cd $CMAQ_DATA
tar xvzf CMAQv5.2.1_Benchmark_SingleDay_Input.tar.gz
tar xvzf CMAQv5.2.1_Benchmark_SingleDay_Output.tar.gz
```
This will produce the following subdirectories:

```
  CMAQ_REPO/data/SE52BENCH/single_day/cctm_input
  emis/
  icbc/
  land/
  lightning/
  met/
```

### 2.2.8 Compiling CMAQ 

For all CMAQ programs (other than MCIP), the program Bldmake is used to compile the source code into executables. The first step in the compilation of CMAQ is to compile Bldmake. Bldmake will then be used to compile the CMAQ programs.  *Note that the compiler paths and flags are all set in the config_cmaq.csh script and then passed along to the build scripts*. None of the CMAQ build scripts contain compiler settings. Instead, the build scripts for each program reference the config_cmaq.csh script using the Linux command “source”. See [Table 2-2](#Table5-4) for a description of the compilation flags in the config_cmaq.csh script.

Note that every build and run script will source the config_cmaq.csh configuration script in order to ensure consistency in computing environment conditions. When executing each script, you will need to specify the compiler and (if desired) the compiler version as documented above for the config_cmaq.csh execution.

**>>COMMENT<<** Update or remove reference to chapters 4 and 7 from old User's documents

All of the CMAQ programs other than CCTM are run in single-processor mode. CCTM may be run either in single-processor (serial) mode or in parallel on multiple processors. Program-specific compilation instructions are provided below. These compilation instructions are for building executables for simulating the test data sets distributed with CMAQ. Additional information about the configuration options for the various CMAQ programs is provided in [Chapter 4](CMAQ_OGD_ch04_science.md) and [Chapter 7](CMAQ_OGD_ch07_programs_libraries.md).

#### 2.2.8.1 Compile Bldmake 

In CMAQv5.2.1, bldmake is compiled automatically the first time you run any of the CMAQ build scripts. If you prefer to run the build as a separate step, the bldmake build script is in $CMAQ_REPO/UTIL/bldmake/scripts.

#### 2.2.8.2 Compile the CMAQ programs 

Create the model executables for ICON, BCON, MCIP, and CCTM.

ICON and BCON can be configured for different kinds of input data. The configuration options for ICON and BCON are described in the [README.md file in the PREP/icon folder](../../PREP/icon/README.md) and the [README.md file in the PREP/bcon folder](../../PREP/bcon/README.md).  

Use the following commands to compile ICON and BCON:

```
cd $CMAQ_HOME/PREP/icon
source bldit_icon.csh [compiler] [version] |& tee build_icon.log

cd $CMAQ_HOME/PREP/bcon
source bldit_bcon.csh  [compiler] [version] |& tee build_bcon.log

where,

[compiler] can be either intel, pgi, or gcc
[version] is the version number of the compiler, e.g. 4.8.1
```

Like the program Bldmake, MCIP is compiled using a Fortran Makefile.

Use the following commands to compile MCIP:

```
cd $CMAQ_HOME/PREP/mcip/src
source ../../../config_cmaq.csh
make |& tee make.mcip.log
```

**>>COMMENT>>** Update or remove reference to chapter 7 from old User's document

The CCTM has multiple configuration options that can be changed to optimize model performance for different applications. In addition to selecting the chemical mechanism to model gas-phase chemistry, the user can also select from several different science modules. The science configuration options for CCTM are discussed in [Chapter 5](CMAQ_UG_ch05_new_simulation.md). The distribution CCTM build script is configured to create a multiprocessor executable for the installation test simulation. For multiprocessor applications, CMAQ uses the message passing interface (MPI) to manage communication between processors in a clustered multiprocessor computing environment. The location of the MPI include and library files on your Linux system are specified in the config_cmaq.csh script.

For single-processor (serial) systems, configure the CCTM build script to create a single-processor executable by commenting out the line that activates the variable “ParOpt” of the CCTM build script. Use the following commands to compile CCTM:

```
cd $CMAQ_HOME/CCTM/scripts
source bldit_cctm.csh [compiler] [version] |& tee build_cctm.log
```

**>>COMMENT<<**  Should we say that we recommend Intel for speed?

All of the CMAQ programs are written in Fortran and are optimized for use on computers running a version of the Linux operating system (OS). However, to use CMAQ in a production environment where multiple iterations of the model will be executed for different spatial domains and/or emissions control strategies, either a cluster of multiprocessor PCs on a high-end network or an expandable rack-mounted Linux server is recommended.

CMAQ is distributed and supported for executing on Linux operating systems with the Intel Fortran, Portland Group Fortran (PGF), or Gnu Fortran compilers. CMAQ can be ported to most computers running Linux. Documented configurations include the SGI Altix, Red Hat Enterprise, Fedora, Ubuntu, Mandrake, MacOSX, and Suse operating systems.

The [CMAS Release Testing Page](https://www.airqualitymodeling.org/index.php/CMAQ) provides technical and operational details on current and prior CMAQ releases.

**>>COMMENT<<** After this secion on Hardware add a section with examples of what is actually used, e.g. at EPA compiler X is used on a ___ type of linux cluster….  In an academic setting, this system is used, etc.

The minimum hardware requirements for running the CMAQ benchmark case are:

-   Linux PC with a single processor
-   1 GB RAM
-   100 GB hard drive storage


CMAQ requires all of the programs listed in [Table 2-3](#Table5-1). This list includes the programs distributed with CMAQ. Note that CMAQv5.0 and greater requires I/O API version 3.1. Newer version of CMAQ will not compile with earlier versions of the I/O API library. [Table 2-4](#Table5-2) lists additional utility software that is not required for running CMAQ, but is useful for model diagnostics and evaluation.

**>>COMMENT<<** 	9. Table 2-3. Some of these are not required for all runs: CHEMMECH, CREATE_EBI, LAPACK (maybe not needed at all? Ask Jesse), BLAS (ask Jesse)


**Table 2-3. Software required for running CMAQ**

| **Software** | **Description** | **Source** |
|-------------------|-----------------------------|--------------------------------|
|***CMAQ Programs***|||
|Bldmake|Executable builder for source code compilation|Contained in the standard CMAQ distribution available in the [CMAQv5.2.1 branch of the CMAQ repo](https://github.com/USEPA/CMAQ/tree/5.2.1). [Release notes](https://github.com/USEPA/CMAQ/tree/5.2.1/CCTM/docs/Release_Notes) and [documentation](https://github.com/USEPA/CMAQ/blob/5.2.1/DOCS/User_Manual/CMAQ_OGD_ch07_programs_libraries.md) are available.|
|ICON|Initial conditions preprocessor|" |
|BCON|Boundary conditions preprocessor|"|
|MCIP|Meteorology-Chemistry Interface Processor| "|
|CCTM|CMAQ Chemistry-Transport Model| "|
|***Compilers***| | |
|IFORT|Intel Fortran 90 compiler|[<http://www.intel.com>](http://www.intel.com/)|
|PGF90|Portland Group Fortran 90 compiler|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|GFORT|Gnu Fortran compiler|[<http://gcc.gnu.org/fortran/>](http://gcc.gnu.org/fortran/)|
|GCC|Gnu C compiler|[<http://gcc.gnu.org/>](http://gcc.gnu.org/)|
|***Code Libraries***| | |
|OpenMPI|Library for the message passing interface; used for multiprocessor CMAQ simulations|[<https://www.open-mpi.org>](https://www.open-mpi.org)|
|MPICH|Library for the message passing interface; used for multiprocessor CMAQ simulations|[<http://www.mcs.anl.gov/research/projects/mpich2/>](http://www.mcs.anl.gov/research/projects/mpich2/)|
|netCDF|Network Common Data Form library for controlling CMAQ file formats\*|[<http://www.unidata.ucar.edu/software/netcdf/>](http://www.unidata.ucar.edu/software/netcdf/)|
|I/O API|Input/Output Application Programming Interface for controlling internal and external communications|[<https://www.cmascenter.org/ioapi/>](https://www.cmascenter.org/ioapi/)|
|LAPACK|Linear algebra packages for use with the bidirectional mercury module|[<http://www.netlib.org/lapack/>](http://www.netlib.org/lapack/)|
|BLAS|Basic Linear Algebra Subprograms|[<http://netlib.org/blas/>](http://netlib.org/blas/)|

> *Note: CMAQ Output File Format*
> CMAQ uses a modified version of the netCDF file format.
> Although CMAQ output is described as being in the netCDF format,
> it is actually a [hybrid format of the I/O API and the netCDF](https://www.cmascenter.org/ioapi/).


## 2.3 Running CMAQ

**>>COMMENT<<**   netCDF /= M3 I/O API

CCTM integrates the output from the preprocessing programs described above (BCON, ICON, and MCIP), as well as CMAQ-ready emissions inputs (e.g., output from SMOKE), to simulate continuous atmospheric chemical conditions. The modeled concentrations of relevant species can be captured for output at a user-defined time frequency (typically hourly). The CCTM output files are all binary netCDF files of gridded and temporally resolved air pollutant information, such as gas- and aerosol-phase species mixing ratios, wet and dry deposition values, visibility metrics, and time-averaged concentrations.

The maximum spatial and temporal coverages of CCTM are dictated by the input meteorology information. The science configuration is specific to each application of the model and can be adjusted to optimize model performance both computationally and in the numerical reproduction of observed air quality trends. Configuration options for CCTM include the temporal coverage of the simulation, the chemical mechanism to use in the modeling, the physics scheme to use for modeling pollutant transport, heterogeneous and aqueous chemistry options, inline processing options, and diagnostic options (such as process analysis, discussed in the next paragraph).


**>>COMMENT<<** This section seems premature at this point in the document.

**>>COMMENT<<** This section and the next section: Jump from compiler options to conceptual formulation of the model is really awkward here.  Seems like all of this section needs to be reworked.

**>>COMMENT<<** Need to add a description of which CMAQ options must be selected at compilation versus at execution.  This was done in Chapter 7 (Programs and Libraries) of the old User’s Document but it needs to be updated and streamlined/condensed.

**>>COMMENT>>** Update or remove reference to chapter 7 from old User's document

The user must create new CMAQ executables for each suite of science configuration options for all programs except MCIP. There are too many combinations of the various chemical mechanisms, horizontal and vertical transport schemes, cloud routines, and chemistry solvers in the CMAQ science configuration to include efficiently in a single executable. In addition to compile-time configuration options with CMAQ, there are also execution-time configuration options (options that are chosen when the model is run versus when it is compiled). The horizontal domain configuration and the vertical coordinate system are dynamic features in CMAQ that are independent of the executable. In other words, a user can employ a single executable for a simulation that uses any of the supported map projections or grid definitions, without having to recompile the source code into a new executable. A description of which CMAQ options must be selected at compilation versus at execution are is included in [Appendix A: Model options](CMAQ_UG_appendix.md#Options).

**>>COMMENT<<** Insert "Explanation of Runscript structure"

## 2.4 Benchmarking Model Run Times
After successfully compiling the various CMAQ programs, use the distributed run scripts to generate the CCTM input files and then to run CCTM for the CMAQ benchmark case. CCTM must be run last in the simulation sequence; MCIP must be run first. Note however that CMAQ-ready meteorology data are distributed with the CMAQ test case, which means that MCIP does not actually need to be run to test the model installation. With the exception of MCIP, there are no dependencies among the other CMAQ programs, so they can be run in any order to create input data for CCTM.

To run the test simulation for the various CMAQ programs, change directories to the location of each program and execute the run script.

Run ICON to produce initial conditions:

```
cd $CMAQ_HOME/PREP/icon/scripts
./run_icon.csh |& tee run_icon.log
```

Run BCON to produce boundary conditions:

```
cd $CMAQ_HOME/PREP/bcon/scripts
./run_bcon.csh |& tee run_bcon.log
```

Check the ICON and BCON log file to ensure that the programs completed successfully.

The CCTM is configured by default to run in multiprocessor mode. This mode requires run time settings that specify the number of processors to allocate to the simulation and the location of the MPI initialization command (mpirun) on your system. Set the number of processors to use for the simulation by setting the number of rows (NPROW) and columns (NPCOL) to use for the MPI domain decomposition. The product of NPCOLS and NPROWS is the number of processors to use for the CCTM run. For example, if you have a system with six processors available to run CMAQ, set NPCOL to 3 and NPROW to 2; the total number of processors (NPROCS) is automatically set by the script.

For an MPI configuration with 6 processors,

```
@ NPCOL = 3; @ NPROW = 2
```

Most clustered multiprocessor systems require a command to start the MPI run-time environment. The default CCTM run script uses the *mpirun* command. Consult your system administrator to find out how to invoke MPI when running multiprocessor applications. For single-processor computing, set NPROCS to 1, NPCOL to 1, and NPROW to 1.

For single-processor computing, set NPCOL_NPROW to "1 1":

```
setenv NPCOL_NPROW "1 1"
```

After configuring the MPI settings for your Linux system, using the following command to run the CCTM. Per the note above, different Linux systems have different requirements for submitting MPI jobs. The command below is an example of how to submit the CCTM run script and may differ depending on the MPI requirements of your Linux system.

```
./run_cctm.csh |& tee cctm.log
```

**>>COMMENT<<** This section is redundant.

Benchmarking is the process of confirming that the model source code compiles and executes correctly on a new computer system. CMAQ should be benchmarked on a computing system before the model is used for research or regulatory applications on that system. The purpose of benchmarking is to ensure that there are no inconsistencies introduced into the model solution from local system parameters, such as compilers, processors, or operating systems. While differences are expected in the CMAQ results produced by different operating systems, hardware, and compilers, these differences should be small and within the numerical error of the model. Input and output reference data are packaged with CMAQ to use for benchmarking the model on new systems. After running the test case packaged with the source code, compare the results against the reference data provided in the CMAQ distribution.

### 2.4.1 CMAQ benchmark parameters
The CMAQ benchmark test case is a single day simulation for July 1, 2011 on a 100 column x 80 row x 35 layer 12-km resolution domain over the southeast U.S. The CCTM configuration parameters for the benchmark test case include the following:

-   Multiprocessor simulation
-   Horizontal advection: Yamo
-   Vertical advection: WRF
-   Horizontal diffusion: Multiscale
-   Vertical diffusion: ACM2
-   Deposition: M3Dry
-   Chemistry solver: EBI
-   Aerosol module: AERO6
-   Cloud module: ACM_AE6
-   Mechanism: cb6r3_ae6_aq
-   Lightning NOx emissions calculated with hourly NLDN strike data
-   Dynamic vertical diffusivity
-   In-line deposition velocities
-   Surface HONO interaction
-   In-line biogenic emissions
-   In-line plume rise
-   In-line windblown dust emissions
-   Bi-directional ammonia flux
-   No stratosphere-troposphere ozone exchange

The system configuration parameters used to generate the benchmark reference data include the following:
- Red Hat Enterprise Linux Server release 5.11 (Tikanga)
- Linux Kernel 2.6.18-238.12.1.el5 x86_64
- Intel v15.0 compiler, 8 processors with OpenMPI

### 2.4.2 CMAQ Benchmark Results

  After completing the CMAQ benchmark case, the CCTM output files can be compared with the reference datasets provided in the CMAQ distribution. The reference data for CMAQ are available from the CMAS Center Software Clearinghouse. The reference data may be compared to the results from your simulation using tile plots of differences, grid-cell statistics (minimum/maximum differences), and domain-wide statistics. See [Chapter 12](CMAQ_OGD_ch12_analysis_tools.md) for a list of analysis tools that are available for comparing two model simulations.

  Domain-wide differences between the reference data and your simulation results for each model species and each simulation time step (hourly) of less than 1% indicate a successful benchmarking of the software on a Linux system. While larger differences may indicate a problem with the installation, they may also point to a configuration discrepancy in the benchmark simulation. Review the compiler optimization flags and the CCTM configuration that you used to resolve differences with the reference data.

  Support for CMAQ is available from the CMAS Center (see [Chapter 13](CMAQ_OGD_ch13_support.md)).

## 2.5 Pre-Processors

### 2.5.1 Initial Conditions Processor (ICON)
**>>COMMENT<<** ICON/BCON summaries need to be updated to reflect v5.3 updates

**>>COMMENT<<**  I don't know what "netCDF" is yet.

**>>COMMENT<<**  Technically ICON and BCON output are in M3 I/O API, not in netCDF.

ICON generates a gridded binary netCDF file of the chemical conditions in the modeling domain for the initial time of a simulation. It can generate these initial conditions from either an ASCII file of vertically resolved concentration profiles (distributed with CMAQ) or from an existing CCTM output file. If the profiles in an ASCII file do not have the same vertical structure as the CCTM configuration to be used, ICON will interpolate the data to a vertical structure consistent with CCTMs. Using an existing CCTM output file to generate initial conditions is applicable when extrapolating initial conditions from a coarse to a fine grid simulation, as may occur when setting up nested simulations (simulations with finer-resolution grids that cover part of coarser-resolution grids). The configuration options for ICON include selecting the chemical mechanism to model, defining the horizontal and vertical grids, and choosing whether the initial conditions are generated from an ASCII profile or from an existing CCTM output file.

Information on environment variables, input and output files, compiling and running ICON are provided in the [README.md file in the PREP/icon folder](../../PREP/icon/README.md).

### 2.5.2 Boundary Conditions Processor (BCON)

**>>COMMENT<<**  oes BCON really generate the GEOS-Chem-based dynamic BCs?  If not, I think this is misleading.  (Second paragraph clarifies this, but the first paragraph is misleading with regard to dynamic BCs.)

BCON generates a gridded binary netCDF file of the chemical conditions along the lateral boundaries of the modeling domain. These boundary conditions can be either static or time-varying, and (as with ICON) can be generated from either an ASCII file of vertically resolved concentration profiles or from an existing CCTM output file. Also as with ICON, BCON will interpolate the data in ASCII profiles to a vertical resolution that is consistent with the CCTM configuration. BCON differs from ICON, however, in that it can generate time-varying (i.e., dynamic) boundary conditions. Dynamic boundary conditions are typically extracted either from CCTM outputs from a coarse-grid simulation for nested simulations or from a CCTM simulation using a global-scale model. The file structure of the ASCII input profiles can also support the creation of dynamic boundary conditions, but generally these files are used only for creating static data. The configuration options for BCON include selecting the chemical mechanism to model, defining the horizontal and vertical grids, and choosing whether the boundary conditions are generated from an ASCII profile or from an existing CCTM output file.

BCON is only used to create boundary conditions inputs for the CCTM from an ASCII profile file or from an existing CCTM output file.  Users are responsible for preparing CCTM input boundary conditions from other sources, such as global chemistry transport models.

Information on environment variables, input and output files, compiling and running BCON are provided in the [README.md file in the PREP/bcon folder](../../PREP/bcon/README.md).  

### 2.5.3 Meteorology-Chemistry Interface Processor (MCIP)  

**>>COMMENT<<** Does MCIP still estimate dry deposition velocities?

**>>COMMENT<<** MCIP is technically in M3 I/O API, not netCDF.

**>>COMMENT<<** Need to define "window" for new users.

**>>COMMENT<<** Should SMOKE have its own section?

**>> Comment <<** ***Tanya will update this information.  Some of it is outdated.***

The Meteorology-Chemistry Interface Processor (MCIP) processes meteorological model output from the WRF‑ARW model into I/O API-formatted files that are compatible with CMAQ and SMOKE (the emissions processor that computes emissions inputs to CMAQ). MCIP automatically determines whether an input file is generated by WRF‑ARW by trying to open the file as a netCDF file. If the file can be read as netCDF, MCIP assumes the input is a WRF‑ARW dataset.  MCIP can be used to uniformly trim cells off the lateral boundary of the domain defined by the meteorological model, or to window in on a subset of that domain. Configuration options for MCIP include the time periods over which to extract data from the meteorological model output files, horizontal and vertical grid definitions, and selections for integrating satellite cloud observations into MCIP output.

Many of the fields that are simulated by the meteorological model are not modified by MCIP for the emissions model and CMAQ, and they are written to I/O API files. Fields that are required for the transformation to CMAQ’s generalized coordinate system are calculated within MCIP. The dry deposition velocities are no longer calculated by the current version of MCIP. CMAQv5 can now calculate all deposition velocities, MCIPv3.4 will be the last version of MCIP to calculate those velocities internally.

MCIP can extract both temporal and spatial subsets of the input meteorology files. The run script allows the user to specify the beginning and end dates/times of the MCIP simulation; these dates/times can fall anywhere within the range of the input meteorological time period, but must be consistent with the time granularity of the meteorological files. MCIP cannot perform temporal interpolations to artificially increase the temporal resolution of the meteorology fields. Two types of horizontal domain windowing are allowed with MCIP. The boundary trim option (“BTRIM”) uniformly trims grid cells off each of the four lateral boundaries of the input meteorology grid. The nonuniform trim option specifies an offset from the lower left corner of the input meteorology domain and the number of cells in the X and Y directions from the revised origin to extract from the input domain. More information about how to invoke these options is provided in the next section: [MCIP](#Execution Configuration Variables). MCIP also provides the capability to reconfigure the vertical layer structure in the input meteorology through interpolation from the input structure to an output structure defined through sigma coordinates in the run script. Commonly referred to as “layer collapsing,” this option should be exercised with caution as it can significantly impact the conservation of energy assumption inherent in the meteorology through its effects on the predicted wind fields.

Information on environment variables, input and output files, compiling and running MCIP are provided in the [README.md file in the PREP/mcip folder](../../PREP/mcip/README.md).  

### 2.5.4 Optional Pre-processors
Insert brief information on JPROC, CALMAP (agdust), and Windblown dust (wbdust).

**>>COMMENT<<** Has anyone run/tested the CALMAP processor recently?  Deposition changes in v5.3 have a large impact on dust estimates.  Need to think carefully about what we want to say about windblown dust in the updated guide.

**>>COMMENT<<** Are we recommending dust here?  How would a user know whether to use dust or not?

**>>COMMENT<<**  Can I run dust with the two-way model?  If so, where would I get GRIDCRO2D?  Is this done before I run CCTM, or can it be done on the fly?

**>>COMMENT<<** Figure 2-6: Where do I get these inputs?  Need to specify what comes with CCTM and what a user has to do separately.  Where does BELD01 come from?

**JPROC**

**Windblown Dust**

**CALMAP** CMAQ has the capability to estimate windblown dust emissions in-line in the CCTM. The CMAQ dust emissions module uses land cover/land use data to identify dust source regions. The dust module includes a feature to estimate dust blown off by the wind (as opposed to anthropogenic dust emissions) from agricultural areas and the impacts of planting and harvesting cycles on available erodible lands that affect dust emissions. Calmap is a preprocessor to the CCTM that uses crop calendar information to produce gridded crop planting and harvesting dates for input to the CMAQ dust module. CALMAP reads grid information from the GRIDCRO2D meteorology file (MCIP output), land cover/land use data from [BELD3](https://www.epa.gov/air-emissions-modeling/biogenic-emissions-landuse-database-version-3-beld3), and crop calendar data to produce files of planting start dates, planting end dates, and harvesting end dates for different crop types interpolated to the modeling grid. These files are input to the CCTM when it is configured to estimate windblown dust and simulate the impacts of agricultural activity on the windblown dust emissions.

### 2.5.5 External Input-Generation Models and Tools
**WRF**

**SMOKE**

**FEST-C**

**Spatial Allocator**

### 2.5.6 Utilities for developers
Insert breif description of chemmech, create_ebi, inline_phot_prepro, nml

**Chemical Mechanism Compiler (CHEMMECH)**

**>>COMMENT<<** How do "users choose which mechanism to use"?

**>>COMMENT<<** The last sentence of this section is confusing.

The release version of CMAQ includes all necessary chemical mechanism information for the preconfigured atmospheric chemistry reactions sets or mechanisms in a released version of CMAQ. Users choose which mechanism to use for compiling and running the CCTM executable. CCTM implements a chemical mechanism by using its namelist and FORTRAN modules. The files are in ASCII format and include the mechanism parameters required such as the species, reaction stoichiometry, and kinetics information. The module files are used to compile the CCTM executable while the namelists are read by CCTM at run time.

Advanced users who wish to generate a new chemical mechanism have to use the CHEMMECH utility to convert the mechanism into the files needed by the CCTM program. CHEMMECH uses a mechanism definition file, often named “mech.def”, and optionally the mechanism namelist files to generate FORTRAN modules. The “mech.def” is an ASCII file that uses a rigid syntax to define reactions and their rate constants.

This approach defining the CMAQ chemical mechanisms allows the chemical reactions and their species to be a fixed part of the executable code. Modifications to the namelists can change predictions saved to the output files, deposition processes of species, emissions inputs and other options for species without recompiling the executable. The namelists defining a chemical mechanism are used by CCTM as well as the ICON and BCON pre-processors. The FORTRAN modules are required to run utility programs such as create_ebi and inline_phot_preproc and JPROC.

**create_ebi**

**inline_phot_preproc**

**nml**

## 2.6 Post processing and visualizing model input and output
Many software programs are available for pre- and post-processing, evaluating and visualizing CMAQ output.

**Table 2-4. Optional support software for CMAQ**

|**Software**|**Description**|     **Source**    |
|------------|-------------------------------|---------------------------------------------|
|***Post-processing***|||
|CMAQ POST Tools|Programs released with CMAQ source code to prepare output data for model evaluation|[https://github.com/USEPA/CMAQ](../../POST)|
|I/O API Tools|Postprocessing tools for manipulating data in the I/O API/netCDF format|[https://www.cmascenter.org/ioapi](https://www.cmascenter.org/ioapi)|
|NCO|netCDF Operators: Postprocessing tools for manipulating data in the netCDF format|[http://nco.sourceforge.net](http://nco.sourceforge.net)
|***Evaluation/Visualization***| | |
|AMET|Atmospheric Model Evaluation Tool for analysis and evaluation of meteorlogical and air quality models|[https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool](https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool)|
|VERDI|Visualization Environment for Rich Data Interpretation for graphical analysis of netCDF gridded data|[http://www.verdi-tool.org](http://www.verdi-tool.org/)|
|PseudoNetCDF|Reading, plotting, and sometimes writing capabilities for atmospheric science data formats including CMAQ files|[https://github.com/barronh/pseudonetcdf/wiki](https://github.com/barronh/pseudonetcdf/wiki)|
|RSIG|2D and 3D visualization of satellite and modeled data|[https://www.epa.gov/hesc/remote-sensing-information-gateway](https://www.epa.gov/hesc/remote-sensing-information-gateway)|
|NCL|NCAR Command Language for scientific data processing and visualization|[http://www.ncl.ucar.edu](http://www.ncl.ucar.edu)|
|IDV|Integrated Data Viewer for 3-D graphical analysis of netCDF gridded data|[http://www.unidata.ucar.edu/software/idv/](http://www.unidata.ucar.edu/software/idv/)|





<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch03_input_and_output.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
