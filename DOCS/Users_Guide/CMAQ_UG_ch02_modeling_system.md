
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

Consistency of configuration variables is critical for building CMAQ itself, not just its libraries. Accordingly CMAQ includes the configuration script `config_cmaq.csh` to help enforce consistent environment settings for CMAQ and its associated libraries [Table 2-2](#Table5-4) lists the `config_cmaq.csh` variables defined for the build process and suggests values to which to set those variables.

Note that for multiprocessor applications it is recommended that the Fortran MPI wrapper script `mpif90` be specified for the Fortran compiler (myFC). Using this script, instead of a direct call to the Fortran compiler, will ensure that the full suite of MPI components (libraries and include files) for the compiler are included in the parallel build.


**Table 2-2. config_cmaq.csh configuration variables**

| **Variable Name** | **Suggested Value** |
|-----------------|---------------------------------------------------------------------|
|`CMAQ_HOME`|The central CMAQ installation directory. For example, if you installed the CMAQ source code in the directory `/home/user/CMAQ` set CMAQ_HOME with `export CMAQ_HOME=/home/user/CMAQ` for bash or `setenv CMAQ_HOME /home/user/CMAQ` for csh; note that this variable is M3HOME prior to CMAQv5.2|
|`CMAQ_DATA`|Automatically set by config_cmaq.csh; note that this variable is M3DATA prior to CMAQv5.2|
|`CMAQ_LIB`|Automatically set by config_cmaq.csh; note that this variable is M3LIB prior to CMAQv5.2|
|`M3MODEL`|Automatically set by config_cmaq.csh; deprecated in CMAQv5.2|
|`compiler`|Set the Fortran compiler type that you will use to compile CMAQ; choices are intel, pgi, or gcc|
|`compilerVrsn`|(Optional) Set the Fortran compiler version number that you will use to compile CMAQ; if you employ this variable, it will be appended to the compiler type when naming build directories and executables|
|`IOAPI_MOD_DIR`|Location of the I/O API modules installation on your Linux system|
|`IOAPI_INCL_DIR`|Location of the I/O API include file installation on your Linux system|
|`IOAPI_LIB_DIR`|Location of the I/O API library installation on your Linux system|
|`NETCDF_LIB_DIR`|Location of the netCDF installation on your Linux system|
|`MPI_LIB_DIR`|Location of the Message Passing Interface installation on your Linux system|
|`netcdf_lib`|Name of the netCDF library on your system;  set to "-lnetcdf" for versions < 4.2.0, "-lnetcdff -lnetcdf" for version 4.2.0 and later|
|`ioapi_lib`|Name of the I/O API libraryar on your system; set to "-lioapi"|
|`pnetcdf_lib`|Name of the parallel netCDF library on your system; set to "-lpnetcdf"|
|`mpi_lib`|Name of the MPI library on your system; set to "-lmpich" for MVAPICH, "-lmpi" for OpenMPI|
|`myFC`|Set to match the `FC` (Fortran compiler) you used to compile netCDF|
|`myCC`|Set to match the `CC` (C compiler) you used to compile netCDF|
|`myFFLAGS`|Fixed-format Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script|
|`myCFLAGS`|C compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script|
|`myFRFLAGS`|Free form-format Fortran compiler optimization flags for your Linux system; suggested values for CMAQ are in the distributed script|
|`MPI_INC`|Set to the path to your MPI library INCLUDE files, e.g. `$M3LIB/mpich/include`|
|`extra_lib`|Set to other libraries required for compiling on your Linux system; users will likely need to change this setting in the distributed script for portability to their system.|
|`EXEC_ID`|build tag, should be automatically set by config_cmaq.csh|

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

### 2.5.5 Utilities for developers
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

**>>COMMENT<<** move the AMET subsection up behind VERDI and emphasize tool much more than the others and provide more specifics on how to run AMET similar to the detail for ICON/BCON. Suggested order: VERDI, AMET, CMAQ tools (hr2day, etc.), then all the other stuff like NCO and IDL.

**>>COMMENT<<** Recommend shortening this section. Remove the summaries of the software programs that are a mix of technical and big picture descriptions.  For each tool just provide a brief summary (in list form) of the tasks that the software can be used to perform.


### 2.6.1 Visualization tools

**>>COMMENT<<** Add reference to RSIG?

#### 2.6.1.1 Visualization Environment for Rich Data Interpretation (VERDI)

**>>COMMENT<<**  Donna should review summary of VERDI (12.2.1).  Should mention that VERDI can be used to view MPAS data.

[http://www.verdi-tool.org](http://www.verdi-tool.org/)

The Visualization Environment for Rich Data Interpretation (VERDI) is a flexible and modular Java-based visualization software tool that allows users to visualize multivariate gridded environmental datasets created by environmental modeling systems such as SMOKE, CMAQ and WRF, namely gridded concentration and deposition fields that users need to visualize and compare with observational data both spatially and temporally. VERDI has been designed keeping most of the functionality of PAVE in mind, and hence can help users analyze and visualize model outputs in a very similar vein, using both command-line driven scripts as well as using a Graphical User Interface (GUI). Further, VERDI is under active development to enhance its features beyond PAVE.

#### 2.6.1.2 Integrated Data Viewer (IDV)

[http://www.unidata.ucar.edu/software/idv/](http://www.unidata.ucar.edu/software/idv/)

The Integrated Data Viewer (IDV) from Unidata is a Java™-based software framework for analyzing and visualizing geoscience data. The IDV release includes a software library and a reference application made from that software. It uses the VisAD library ([<http://www.ssec.wisc.edu/~billh/visad.html>](http://www.ssec.wisc.edu/~billh/visad.html)) and other Java-based utility packages.

The IDV is developed at the Unidata Program Center (UPC), part of the University Corporation for Atmospheric Research in Boulder, CO, which is funded by the National Science Foundation. The software is freely available under the terms of the GNU Lesser General Public License.

The IDV "reference application" is a geoscience display and analysis software system with many of the standard data displays that other Unidata software (e.g., GEMPAK and McIDAS) provides. It brings together the ability to display and work with satellite imagery, gridded data (for example, numerical weather prediction model output), surface observations, balloon soundings, NWS WSR-88D Level II and Level III RADAR data, and NOAA National Profiler Network data, all within a unified interface. It also provides 3-D views of the earth system and allows users to interactively slice, dice, and probe the data, creating cross-sections, profiles, animations and value read-outs of multidimensional data sets. The IDV can display any Earth-located data if they are provided in a supported format.

IDV includes the capability to read I/O API netCDF formatted files and a scripting interface to create and manipulate images and movies. The scripting is accomplished through an XML file: *IDV Scripting Language* (ISL). The ISL file can be opened from a running IDV, or one can be passed to the IDV as a command-line argument:

`runIDV capture.isl`

### 2.6.2 Post processing

#### 2.6.2.1 CMAQ utility Tools

[https://github.com/USEPA/CMAQ](https://github.com/USEPA/CMAQ)

Several Fortran-based post-processing tools are provided along with the CMAQ code/scripts distribution. These are located in the $CMAQ_HOME/POST directory in the CMAQ distribution (version 5.2 and later). These tools work directly with the CMAQ outputs and help in processing, formatting, and preparing datasets from various ambient monitoring networks for subsequent evaluation. These networks include the EPA Air Quality System (AQS)AIRS-AQS, Interagency Monitoring of Protected Visual Environments (IMPROVE), Clean Air Status Trends Network (CASTNET), Speciated Trends Network (STN), National Atmospheric Deposition Program (NADP), Mercury Deposition Network (MDN) and the Southeast Aerosol Research and Characterization Study (SEARCH). The formatted observation data files needed for running the sitecmp and sitecmp_dailyo3 utilities are available for 2000 through 2014 from the CMAS Center Data Clearinghouse under the heading "2000-2014 North American Air Quality Observation Data": https://www.cmascenter.org/download/data.cfm.

The various CMAQ utility tools are described below.  Documentation and sample run scripts are provided in the [$CMAQ_HOME/POST](https://github.com/USEPA/CMAQ/tree/5.2.1) directory for each utility.

<a id="post_tools"><a/>

-   **[appendwrf](https://github.com/USEPA/CMAQ/blob/5.2.1/POST/appendwrf/README.md)**: This program concatenates variables from multiple WRF input or output files into a single file along the Time (unlimited) dimension. This can be useful in cases where a user may have WRF input or output files that were generated for shorter time periods and wants to combine them into files with longer (e.g. monthly) duration.
-   **[bldoverlay](https://github.com/USEPA/CMAQ/tree/5.2.1/POST/bldoverlay/README.md)**: This program creates an observation overlay file that can be imported into either PAVE or VERDI.
-   **[block_extract](https://github.com/USEPA/CMAQ/tree/5.2.1/POST/block_extract/README.md)**: This  program extracts time series of 1 or more variables from 1 or more (up to 99) IOAPI files for a specified range of cells.
- 	**[combine](https://github.com/USEPA/CMAQ/tree/5.2.1/POST/combine/README.md)**:This program combines species from raw CMAQ output files or wrfout input files into a new IOAPI output file. Species can be aggregated or transformed into variables of interest (i.e. to match observed quantities from a specific monitoring network).
-   **[hr2day](https://github.com/USEPA/CMAQ/tree/5.2.1/POST/hr2day/README.md)**: This program creates gridded I/O API files with daily values (e.g. daily average, daily sum, maximum daily 8-hr average) from gridded I/O API files containing hourly values.
- 	**[sitecmp](https://github.com/USEPA/CMAQ/tree/5.2.1/POST/sitecmp/README.md)**: This program generates a csv (comma separated values) file that compares CMAQ generated concentrations with an observed dataset.
-   **[sitecmp_dailyo3](https://github.com/USEPA/CMAQ/tree/5.2.1/POST/sitecmp_dailyo3/README.md)**: This program generates a csv (comma separated values) file that compares various daily ozone metrics computed from hourly CMAQ generated and observed ozone concentrations. The metrics included in the output file are daily maximum 1-hr ozone concentrations, daily maximum 1-hr ozone concentrations in the nine cells surrounding a monitor, time of occurrence of daily maximum 1-hr ozone concentrations, daily maximum 8-hr ozone concentrations, daily maximum 8-hr ozone concentrations in the nine cells surrounding a monitor, time of occurrence of daily maximum 8-hr ozone concentrations, the daily W126 ozone value, and the daily SUM06 ozone value.
-   **[writesite](https://github.com/USEPA/CMAQ/tree/5.2.1/POST/writesite/README.md)**: This program generates a csv file from an IOAPI data file for a set of species at defined site locations.

#### 2.6.2.2 M3tools
**>>COMMENT<<** M3tools list needs to be checked to confirm it is still accurate/up to date.  Same for NCO operators.  Do we want to continue to keep this list up to date, or simply point to the websites for these tools?

[<https://www.cmascenter.org/ioapi/>](https://www.cmascenter.org/ioapi/)

An extensive set of utility programs called *m3tools* that use the I/O API library have been developed and made available for the modeling community. These utility routines assist in manipulating dates and times, performing coordinate conversions, storing and recalling grid definitions, sparse matrix arithmetic, etc., as well as in data manipulation and statistical analyses. All *m3tools* can be run at the command line, and the various options can be provided interactively, or all of them can be stored in a file and executed as scripts.

A list of these utility programs and brief descriptions is provided below.

-   **airs2m3**: Imports air quality monitor data from an AIRS AMP350-format ASCII file and puts them into an I/O API "observational data" file
-   **bcwndw**: Extracts data from a gridded file to the boundary of a subgrid window (see **m3wndw** later in this list for extracting to the window itself)
-   **datshift**: Takes calendar date (form YYYYMMDD) and a number of days D, and reports the date D days later.
-   **gregdate**: Computes calendar-style date "Month DD, YYYY", day-of-week (Sunday, Monday, ..., Saturday), and whether or not Daylight Saving Time is in effect from Julian date YYYYDDD, or from "yesterday", "today", or "tomorrow"
-   **juldate**: Computes Julian date YYYYDDD, day-of-week (Sunday, Monday, ..., Saturday), and whether or not Daylight Saving Time is in effect from calendar-style date "Month DD, YYYY", or from "yesterday", "today", or "tomorrow".
-   **m3combo**: Computes linear combinations of sets of variables from an I/O API input file, and writes the resulting variables to an I/O API output file
-   **m3cple**: Copies to the same grid, or interpolates to another grid, a time sequence of all variables from a source file to a target file, under the optional control of an I/O API coupling-mode "synch file"
-   **m3diff**: Computes statistics for pairs of variables and for the results of applying various comparison ("differencing") operations to those variables in a pair of files.
-   **m3edhdr**: Edits header attributes/file descriptive parameters
-   **m3fake**: Builds a file according to user specifications, filled either with dummy data or with data read in from a set of user-supplied files
-   **m3merge**: Merges selected variables from a set of input files for a specified time period, and writes them to a single output file, with optional variable renaming in the process
-   **m3pair**: Builds an ASCII file of paired values for two variables from two files, within a user-selected window into the grid, according to user specifications
-   **m3stat**: Computes statistics for variables in a file
-   **m3tproc**: Computes time period aggregates (e.g., 08:00-16:00 gridded daily maxima) and writes them to an output file. Can be used to create running averages, (e.g., 8-h O<sub>3</sub> data from 1-h O<sub>3</sub>), daily averages, daily maxima, etc.
-   **m3tshift**: Copies/time-shifts data from a file
-   **m3wndw**: Windows data from a gridded file to a subgrid (see **bcwndw** earlier in this list for extracting to the boundary of the subgrid window)
-   **m3xtract**: Extracts a subset of variables from a file for *\<time interval\>* .Can also be used to concatenate data from two or more files with different time periods into one file
-   **m4filter**: Converts first-edition Models-3 files to current version
-   **mtxblend**: Uses a sparse-matrix file to interpolate/transform data from an input file to the grid of a "base" file and to merge it with data from the "base" file
-   **mtxbuild**: Builds a sparse-matrix transform file from user-supplied ASCII coefficient inputs
-   **mtxcalc**: Builds a grid-to-grid sparse-matrix transform file using a subsampling algorithm
-   **mtxcple**: Uses a sparse-matrix file to interpolate a time sequence of all variables from a source file to a target file, under the optional control of an I/O API coupling-mode "synch file"
-   **presterp**: Interpolates from a 3-D sigma-coordinate file to a new 3-D pressure-coordinate file, using coefficients from PRES\_CRO\_3D
-   **selmrg2d**: Selects multiple 2-D layer/variable combinations from multiple gridded input files, and writes result to merged 2-D gridded output file
-   **utmtool**: Performs coordinate conversions and grid-related computations for lat-lon, Lambert, and UTM coordinate systems.
-   **vertot**: Computes vertical-column totals of variables in a file

#### 2.6.2.3 netCDF
[http://www.unidata.ucar.edu/software/netcdf/](http://www.unidata.ucar.edu/software/netcdf/)

Almost all of the CMAQ input and output files use the I/O API netCDF file format. If the user has already built the netCDF library for compiling CMAQ, the ncdump utility should also be available on the user’s machine. This utility generates an ASCII representation of the netCDF file using the CDF notation developed by NCAR.

The UNIX syntax for invoking ncdump is the following:

`ncdump [-h] [-c] [-n name] [inputfile]`

where:

-h produces only the "header" information in the output file; i.e., the declarations of dimensions, variables, and attribute, but no data values for the variables.

-c produces the "header" information in the output file and the data values for coordinate variables (variables that are also dimensions).

-n name is used to specify a different name for the network Common data form Description Language (CDL) description than the default.

[http://nco.sourceforge.net/](http://nco.sourceforge.net/)

The netCDF Operators (NCO) are a suite of programs known as operators. Each operator is a stand-alone, command-line program that is executed at the UNIX shell level, similar to the commands ls or mkdir. The operators take netCDF files as input, then perform a set of operations (e.g., deriving new data, averaging, hyperslabbing, or metadata manipulation) and produce a netCDF file as output. The operators are primarily designed to aid manipulation and analysis of gridded scientific data. The single command style of NCO allows users to manipulate and analyze files interactively and with simple scripts, avoiding the overhead (and some of the power) of a high-level programming environment.

NCO achieves flexibility by using *command-line options*. These options are implemented in all traditional UNIX commands as single-letter *switches*, e.g., \`ls -l'. NCO supports both short-format (single letter) and long-format (multiletter) options.

An overview of the various netCDF operators is given below.

-   **ncap (netCDF Arithmetic Processor)**: ncap and ncap2 arithmetically process netCDF files. The processing instructions are contained either in the NCO script file fl.nco or in a sequence of command-line arguments.
-   **ncatted (netCDF Attribute Editor)**: ncatted edits attributes in a netCDF file. ncatted can *append*, *create*, *delete*, *modify*, and *overwrite* attributes (all explained below). Furthermore, ncatted allows each editing operation to be applied to every variable in a file. This saves time when changing attribute conventions throughout a file.
-   **ncbo (netCDF Binary Operator)**: ncbo performs binary operations on variables in *file\_1* and the corresponding variables (those with the same name) in *file\_2* and stores the results in *file\_3*. The binary operation operates on the entire files.
-   **ncea (netCDF Ensemble Averager)**: ncea performs grid-point averages of variables across an arbitrary number (an *ensemble*) of *input-files*, with each file receiving an equal weight in the average. Each variable in the *output-file* will be the same size as the same variable in any one of the *input-files*, and all *input-files* must be the same size. ncea averages entire files, and weights each file evenly. This is distinct from ncra (discussed later in this list), which averages only over the record dimension (e.g., time), and weights each record in the record dimension evenly; ncea *always averages* coordinate variables, regardless of the arithmetic operation type performed on the noncoordinate variables. All dimensions, including the record dimension, are treated identically and preserved in the *output-file*.
-   **ncecat (netCDF Ensemble Concatenator**): ncecat concatenates an arbitrary number of input files into a single output file. A new record dimension acts as the glue to bind the input files data together. Each variable in each input file becomes one record in the same variable in the output file. All *input-files* must contain all extracted variables (or else there would be "gaps" in the output file). Each extracted variable must be constant in size and rank across all *input-files*. The *input-files* are stored consecutively as a single record in *output-file*. Thus, the *output-file* size is the sum of the sizes of the extracted variable in the input files.
-   **ncflint (netCDF File Interpolator)**: ncflint creates an output file that is a linear combi­nation of the input files. This linear combination is a weighted average, a normalized weighted average, or an interpolation of the input files. Coordinate variables are not acted upon in any case; they are simply copied from *file\_1*.
-   **ncks (netCDF Kitchen Sink)**: ncks combines selected features of ncdump, ncextr, and the nccut and ncpaste specifications into one versatile utility. ncks extracts a subset of the data from *input-file* and prints it as ASCII text to stdout, writes it in flat binary format to binary-file, and writes (or pastes) it in netCDF format to *output-file*.
-   **ncpdq (netCDF Permute Dimensions Quickly)**: ncpdq performs one of two distinct functions—packing or dimension permutation—but not both. ncpdq is optimized to perform these actions in a parallel fashion with a minimum of time and memory.
-   **ncra (netCDF Record Averager)**: ncra averages record variables across an arbitrary number of *input-files*. The record dimension is, by default, retained as a degenerate (size 1) dimension in the output variables.
-   **ncrcat (netCDF Record Concatenator)**: ncrcat concatenates record variables across an arbitrary number of *input-files*. The final record dimension is by default the sum of the lengths of the record dimensions in the input files.
-   **ncrename (netCDF Renamer)**: ncrename renames dimensions, variables, and attributes in a netCDF file. Each object that has a name in the list of old names is renamed using the corresponding name in the list of new names. All the new names must be unique.
-   **ncwa (netCDF Weighted Averager)**: ncwa averages variables in a single file over arbitrary dimensions, with options to specify weights, masks, and normalization.

### 2.6.3 Model performance

#### 2.6.3.1 Atmospheric Model Evaluation Tool (AMET)
**>>COMMENT<<** Ask Wyat/Rob to update this summary.

[http://www.cmascenter.org](http://www.cmascenter.org/)

The Atmospheric Model Evaluation Tool (AMET) is a suite of software designed to facilitate the analysis and evaluation of meteorological and air quality models. AMET matches the model output for particular locations to the corresponding observed values from one or more networks of monitors. These pairings of values (model and observation) are then used to statistically and graphically analyze the model’s performance. More specifically, AMET is currently designed to analyze outputs from MM5, WRF, CMAQ, and CAMx as well as MCIP-postprocessed meteorological data (surface only).

The basic structure of AMET consists of two ''fields ''and two *processes*. The two fields (scientific topics) are MET and AQ, corresponding to meteorology and air quality data. The two processes (actions) are database population and analysis. Database population refers to the underlying structure of AMET; after the observations and model data are paired in space and time, the pairs are inserted into a MySQL database. Analysis refers to the statistical evaluation of these pairings and their subsequent plotting. Practically, a user may be interested in using only one of the fields (either MET or AQ), or may be interested in using both fields. That decision is based on the scope of the study. The three main software components of AMET are MySQL (an open-source database software system), R (a free software environment for statistical computing and graphics), and perl (an open-source, cross-platform programming language).



**Table 2-4. Optional support software for CMAQ**

|**Software**|**Description**|     **Source**    |
|------------|-------------------------------|---------------------------------------------|
|***Evaluation and visualization tools***| | |
|VERDI|Visualization Environment for Rich Data Interpretation for graphical analysis of netCDF gridded data|[<http://www.verdi-tool.org>](http://www.verdi-tool.org/)|
|PAVE|Package for Analysis and Visualization of Environmental data for graphical analysis of netCDF gridded data|[<http://www.cmascenter.org>](http://www.cmascenter.org/)|
|IDV|Integrated Data Viewer for 3-D graphical analysis of netCDF gridded data|[<http://www.unidata.ucar.edu/software/idv/>](http://www.unidata.ucar.edu/software/idv/)|
|I/O API Tools|Postprocessing tools for manipulating data in the I/O API/netCDF format|[<https://www.cmascenter.org/ioapi/>](https://www.cmascenter.org/ioapi/)|
|netCDF Tools|Postprocessing tools for manipulating data in the netCDF format|[<http://my.unidata.ucar.edu/content/software/netcdf/index.html>](http://my.unidata.ucar.edu/content/software/netcdf/index.html)|
| ***Source code diagnostics*** |
|GDB|Gnu Fortran debugger|[<https://www.sourceware.org/gdb/>](https://www.sourceware.org/gdb/)|
|PGDBG|Portland Group Fortran debugger|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|PGPROF|Portland Group Fortran code profiler|[<http://www.pgroup.com/>](http://www.pgroup.com/)|
|IDB|Intel Fortran debugger|[<https://software.intel.com/en-us/articles/idb-linux>](https://software.intel.com/en-us/articles/idb-linux)|

### 2.6.4 External Input-Generation Models and Tools


<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch03_input_and_output.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
