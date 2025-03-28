
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch04_model_inputs.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch06_model_configuration_options.md)

<!-- END COMMENT -->

# 5. Running a CMAQ Simulation

## 5.1 Introduction

During this chapter the user will learn about how to obtain the CMAQ source codes and how to set-up their CMAQ environment to complete a CMAQ simulation. It should be noted that before you can configure your CMAQ Environment, consult the chapter "Preparing to run" to see you have the minimum requirement of hardware and software on your system.

## 5.2 Getting the CMAQ Source Code

CMAQ source code can be installed either using git or from tarballs downloaded from the git repository hosted by GitHub. Both options are described here.

### 5.2.1 Git Installation

In the directory where you would like to install CMAQ, issue the following command to clone the official EPA GitHub repository for CMAQv5.5:

`git clone -b main https://github.com/USEPA/CMAQ CMAQ_REPO`

Using the git clone option, CMAQ will install into the following directories:

```
CMAQ_REPO/CCTM
CMAQ_REPO/PREP
CMAQ_REPO/POST
CMAQ_REPO/UTIL
CMAQ_REPO/PYTOOLS
CMAQ_REPO/DOCS

```

### 5.2.2 Zip file Installation

Zip files of the CMAQ source code are available from the public GitHub repository. Click the button "Clone or download" from https://github.com/USEPA/CMAQ and select "Download ZIP" to download a Zip file of the CMAQ repository. Alternatively, you may download the Zip file from the [EPA CMAQ website](https://www.epa.gov/cmaq/access-cmaq-source-code).

Reference input/output data for testing the installation of the software are available from the CMAS Center; *data are not available through GitHub*. You must register/login to access the source codes and data from the CMAS Center.

In the directory where you would like to install CMAQ, unzip the model distribution file:

`unzip CMAQ-main.zip`

The following directories will be created:

```
CMAQ-main/CCTM
CMAQ-main/PREP
CMAQ-main/POST
CMAQ-main/UTIL
CMAQ-main/PYTOOLS
CMAQ-main/DOCS
```

The Git and Zip file installation options will produce slightly different subdirectories on your Linux system. The base installation directory using the git clone command will be `CMAQ_REPO`; the directory from the Zip file will be `CMAQ-main`. The subsequent instructions in this guide will be based on the git clone installation. For Zip file installations, replace `CMAQ_REPO` with `CMAQ-main` in the instructions that follow. The differences in the directory names highlights the difference in functionality between the two options. Cloning the repository gives the user access to the full repository and its history, while downloading the Zip file will only give access to version 5.4.

## 5.3 The CMAQ Repository Structure

After downloading the source codes the user is encouraged to look through the repository to familiarize themselves with the structure. A summarized image of the repository is shown below:

<a id=Figure5-1></a>

![image](https://user-images.githubusercontent.com/47453034/193078832-ca2369c8-f86a-4b81-8dbb-3bfcfc711e09.png)

**Figure 5‑1. CMAQ repository structure**

In this image it can be seen that there are six main sub folders within the CMAQ repository. The first folder, CCTM, houses all the source codes (i.e. Fortran/C programs) and scripts that drive the CMAQ Chemistry Transport Model (CCTM). 

The second folder, DOCS, contains the CMAQ User's Guide and a Developers Guide for a general description of CMAQ's open-source collaboration workflow and step-by-step instructions for how to make code contributions through GitHub.

The third folder in the repository is the POST folder which contains several very useful tools for post-processing of the input/output data files. Each tool within the folder comes wth the source code, scripts and a README used to run the tool. A technical description of the tools within this folder can be found in [Chapter 8](CMAQ_UG_ch08_analysis_tools.md).

The fourth folder in the repository is the PREP folder which contains several pre-processing programs that can be run before the CCTM to prepare meteorology, initial conditions and boundary conditions inputs. Similar to the POST tools, documentation on compiling and running the programs is provided within each subfolder under PREP.

The fifth folder in the repository is the PYTOOLS folder. This folder holds python tools relating to OCEAN file augmentation and tools relating to PREP and POST processing of inputs for CMAQ. Similar to the PREP and POST tools, documentation on how to run these tools is provided within each subfolder under PYTOOLS.

The last folder within the repository is the UTIL folder which contains useful utilities relating to the CMAQ program suite. An example is the bldmake utility which is used to compile the source code into executables when you use any of the build scripts in the CMAQ repository. Also included in this repository is a top-level README file with an overview of the contents of the release and two additional C-Shell scripts, `bldit_project.csh` and `config_cmaq.csh`.  `bldit_project.csh` allows the user to extract the build and run scripts and compile the model outside of the repository, while `config_cmaq.csh` helps enforce consistent environment setting for the CMAQ project. Both these scripts will be discussed in the following sections.

## 5.4 Building CMAQ Outside of the Repository in a User-Specified Directory

When cloning the repository or unpacking the tar file of the CMAQ distribution, the top-level directory is recognized by the default build and run scripts as `CMAQ_HOME` (formerly M3HOME prior to CMAQv5.2). This directory is an arbitrary base location of the CMAQ installation on your Linux system for a specific application. If the user will build and run CMAQ within the repository folder structure, then `CMAQ_HOME` does not need to be set explicitly in the `bldit_project.csh` script. If, on the other hand, the user wishes to extract the build and run scripts and compile the model outside of the repository, then `CMAQ_HOME` will need to be specified in `bldit_project.csh`. Executing `bldit_project.csh` will automatically perform this extraction and create a CMAQ folder structure under the location now specified by `CMAQ_HOME`. To perform this operation, modify the variable `CMAQ_HOME` in the `bldit_project.csh ` script to identify the folder that you would like to install the CMAQ package under. For example:

```
set CMAQ_HOME = /home/username/CMAQ_v5.5
```

Now execute the script:

```
./bldit_project.csh
```

It should be noted that from now on, the other CMAQ directories are referenced relative to CMAQ_HOME and it is where your CMAQ project will be run from. While this directory structure is convenient for the benchmark case and most CMAQ applications, other configurations are possible.

## 5.5 Initialization of CMAQ Environment

Consistency of configuration variables is critical for building CMAQ itself, not just its libraries. Accordingly CMAQ includes the configuration script `config_cmaq.csh` to help enforce consistent environment settings for CMAQ and its associated libraries. [Appendix A](Appendix/CMAQ_UG_appendixA_model_options.md) lists the `config_cmaq.csh` variables defined for the build process and suggests values to which to set those variables.

Note that for multiprocessor applications it is recommended that the Fortran MPI wrapper script mpiifort (for Intel compiler; for GNU and PGI fortran compiler, use mpifort) be specified for the Fortran compiler (myFC). Using this script, instead of a direct call to the Fortran compiler, will ensure that the full suite of MPI components (libraries and include files) for the compiler are included in the parallel build without anything provided by the user explicitly.

Use the following steps to initialize your CMAQ environment:

```
source config_cmaq.csh [compiler]
```

After running the above command, it should be noticed now under CMAQ_HOME, the data file directory has been created and serves as a container for the input and output data for the model, and the lib directory contains links to the compiled binary library files required to build the CMAQ executables. The CMAQ scripts use the following environment variables to alias the locations of these directories:


`CMAQ_LIB   = $CMAQ_HOME/lib` (M3LIB before CMAQv5.2)<br>
`CMAQ_DATA  = $CMAQ_HOME/data` (M3DATA before CMAQv5.2)

If you encounter errors about libraries not being found, check the settings of the `config_cmaq.csh` script variables IOAPI, NETCDF, or MPI to ensure that they correctly point to the locations of these libraries on your Linux system.

Sourcing the `config_cmaq.csh` script only needs to be invoked during a new installation of CMAQ to make sure the links to these libraries are working correctly. For every successive session, links to these libraries will automatically be created when you run any of the build or run scripts.

## 5.6 Compiling CMAQ Chemistry-Transport Model (CCTM)

After all required CMAQ inputs are generated using the preprocessors mentioned above the user is now ready to compile CCTM. CMAQ’s current coding structure is based on a modular design principle that seperates CCTM’s main driver, science modules, data estimation modules, and control/utility subroutines. Also distinguished from each other are the science models (including submodels for meteorology, emissions, chemistry-transport modeling) and the analysis and visualization subsystems.

In CCTM, the process modules that affect the pollutant concentration fields are classified as listed below. Each bullet contains a description of the process followed by module name in parentheses. These modules are discussed further in [Chapter 6](CMAQ_UG_ch06_model_configuration_options.md).

Science Modules:

-  Horizontal advection (hadv)
-  Vertical advection (vadv)
-  Horizontal diffusion (hdiff)
-  Vertical diffusion (vdiff)
-  Emissions (offline and online emissions sources) (emis) 
-  Dry Deposition/Air Surface Exchange (depv)
-  Gas-phase chemical reaction solver (gas)
-  Aqueous-phase reactions and cloud mixing (cloud)
-  Aerosol dynamics and size distributions (aero)
-  Potential vorticity scaling for stratosphere/troposphere exchange (pv_o3)

The user has the ability to configure the model in a multitude of ways by selecting from different options for each scientific process. Model configuration is split into build time options and run time options. To modify any science options during build time, edit the `bldit_cctm.csh` script. The `bldit_cctm.csh` script also contains other information, such as the option to run in single or multiprocessor mode as well as debug mode. It should be noted default build time options are alrady set within the `bldit_cctm.csh`. To modify any run time options, such as turning on in-line biogenic emission calculation or using in-line windblown dust emission, edit the run script, `run_cctm.csh`, and set the corresponding environment variable. To read more about build and run time configurations for specific scientific processes, see the next chapter [(Chapter 6)](CMAQ_UG_ch06_model_configuration_options.md). To see a complete list configuration options reference [Appendix A](Appendix/CMAQ_UG_appendixA_model_options.md).  

Once the `bldit_cctm.csh` script is configured to the user's preference, the user is ready to run the script to build the CCTM executable. To do this run the following commands:

```
cd $CMAQ_HOME/CCTM/scripts
source bldit_cctm.csh [compiler] [version] |& tee build_cctm.log
```

The bldit script invokes the CMAQ utility program [bldmake][link_5_bldmake], which extracts source code from your CMAQ GIT repository, constructs a Makefile based on your selected options, and compiles the executable automatically.  Following normal termination of the script with the default configuration, the user will notice a BLD directory created. This is the location of the CCTM executable along with the relevant source codes and the Makefile needed to build the model. In this directory a few useful commands can be used to update the executable if any changes are made to the Fortran source codes via the MakeFile. For example, if the user wants to recompile the source codes in debug mode _instead_ of re-running the `bldit_cctm.csh` script the user can use the following commands:

```
cd BLD_CCTM_v54_[compiler][version]
make clean
make DEBUG=TRUE
```

In another example, if the user has made any changes to the source codes in the BLD directory and wanted to update the CCTM executable to reflect these changes the user can use the following commands:

```
cd BLD_CCTM_v54_[compiler][version]
make
```

The Make utility only compiles the modified files and all associated file which are defined by the dependency of each source file in the Makefile.

## 5.7 Running CCTM

After setting up the CCTM executable the model is ready to be run. Much like the `bldit_cctm.csh` script, to modify any run time options edit the `run_cctm.csh` script referencing [Appendix A](Appendix/CMAQ_UG_appendixA_model_options.md) for a complete list of optional settings. After these settings have been configured use the following commands to run the script:


```
cd $CMAQ_HOME/CCTM/scripts
run_cctm.csh |& tee run_cctm.log
```

### 5.7.1 CCTM Logfiles

The CCTM simulation will write two types of logfile, a main logfile (e.g. run_cctm.log) and processor-specific logfiles that have the name convention:  
```
CTM_LOG_[ProcessorID].v54_[compiler]_[data_name]/_[RUNDATE].log
```

The main logfile contains extensive metadata and useful information about the details of your simulation. The following examples describe some of this information:  
```
Start Model Run At Tue Sep 13 14:55:26 EDT 2022
Compiler is set to intel
No compiler version given. Atmos system Detected. Assume Intel 21.0

Working Directory is ...
Build Directory is ...
Output Directory is ...
Log Directory is ...
Executable Name is CCTM_v54.exe

---CMAQ EXECUTION ID: CMAQ_CCTMv54_sha=[git-SHA]_[userID]_YYYYMMDD_hhmmss_nanosecs ---

Set up input and output files for Day YYYY-MM-DD.

Existing Logs and Output Files for Day YYYY-MM-DD Will Be Deleted
/bin/rm: No match.

CMAQ Processing of Day 20170722 Began at Tue Sep 13 14:55:26 EDT 2022
```
This section documents the folder structure, username, and run date for the simulation, and is meant to aid in maintaining transparency of simulation results after runs have been completed. This section is followed by the CMAQ and I/O API headers, and a record of all environment variables and their values for this simulation.

Next, the program outputs a table describing the domain decomposition breakdown for the run.  
```
          -=-  MPP Processor-to-Subdomain Map  -=-
                 Number of Processors = 128
    ____________________________________________________
    |                                                  |
    |  PE    #Cols    Col_Range     #Rows    Row_Range |
    |__________________________________________________|
    |                                                  |
    |  0       12      1:  12         24      1:  24   |
    |  1       12     13:  24         24      1:  24   |
    |  2       12     25:  36         24      1:  24   |
    |  3       12     37:  48         24      1:  24   |
    |  4       12     49:  60         24      1:  24   |
    |  5       12     61:  72         24      1:  24   |
    |  6       12     73:  84         24      1:  24   |
    |  7       12     85:  96         24      1:  24   |
    |  8       12     97: 108         24      1:  24   |
    |  9       12    109: 120         24      1:  24   |
    | 10       12    121: 132         24      1:  24   |
    | 11       11    133: 143         24      1:  24   |
    | 12       11    144: 154         24      1:  24   |
    | 13       11    155: 165         24      1:  24   |
    | 14       11    166: 176         24      1:  24   |
    | 15       11    177: 187         24      1:  24   |
    | 16       12      1:  12         24     25:  48   |
    | 17       12     13:  24         24     25:  48   |
    | 18       12     25:  36         24     25:  48   |
    | 19       12     37:  48         24     25:  48   |
    | 20       12     49:  60         24     25:  48   |
    | 21       12     61:  72         24     25:  48   |
    | 22       12     73:  84         24     25:  48   |
    | 23       12     85:  96         24     25:  48   |
    | 24       12     97: 108         24     25:  48   |
    | 25       12    109: 120         24     25:  48   |
    | 26       12    121: 132         24     25:  48   |
    | 27       11    133: 143         24     25:  48   |
    | 28       11    144: 154         24     25:  48   |
    | 29       11    155: 165         24     25:  48   |
    | 30       11    166: 176         24     25:  48   |
    | 31       11    177: 187         24     25:  48   |
    | 32       12      1:  12         24     49:  72   |
    | 33       12     13:  24         24     49:  72   |
    | 34       12     25:  36         24     49:  72   |
    | 35       12     37:  48         24     49:  72   |
    | 36       12     49:  60         24     49:  72   |
    | 37       12     61:  72         24     49:  72   |
    | 38       12     73:  84         24     49:  72   |
    | 39       12     85:  96         24     49:  72   |
    | 40       12     97: 108         24     49:  72   |
    | 41       12    109: 120         24     49:  72   |
    | 42       12    121: 132         24     49:  72   |
    | 43       11    133: 143         24     49:  72   |
    | 44       11    144: 154         24     49:  72   |
    | 45       11    155: 165         24     49:  72   |
    | 46       11    166: 176         24     49:  72   |
    | 47       11    177: 187         24     49:  72   |
    | 48       12      1:  12         23     73:  95   |
    | 49       12     13:  24         23     73:  95   |
    | 50       12     25:  36         23     73:  95   |
    | 51       12     37:  48         23     73:  95   |
    | 52       12     49:  60         23     73:  95   |
    | 53       12     61:  72         23     73:  95   |
    | 54       12     73:  84         23     73:  95   |
    | 55       12     85:  96         23     73:  95   |
    | 56       12     97: 108         23     73:  95   |
    | 57       12    109: 120         23     73:  95   |
    | 58       12    121: 132         23     73:  95   |
    | 59       11    133: 143         23     73:  95   |
    | 60       11    144: 154         23     73:  95   |
    | 61       11    155: 165         23     73:  95   |
    | 62       11    166: 176         23     73:  95   |
    | 63       11    177: 187         23     73:  95   |
    | 64       12      1:  12         23     96: 118   |
    | 65       12     13:  24         23     96: 118   |
    | 66       12     25:  36         23     96: 118   |
    | 67       12     37:  48         23     96: 118   |
    | 68       12     49:  60         23     96: 118   |
    | 69       12     61:  72         23     96: 118   |
    | 70       12     73:  84         23     96: 118   |
    | 71       12     85:  96         23     96: 118   |
    | 72       12     97: 108         23     96: 118   |
    | 73       12    109: 120         23     96: 118   |
    | 74       12    121: 132         23     96: 118   |
    | 75       11    133: 143         23     96: 118   |
    | 76       11    144: 154         23     96: 118   |
    | 77       11    155: 165         23     96: 118   |
    | 78       11    166: 176         23     96: 118   |
    | 79       11    177: 187         23     96: 118   |
    | 80       12      1:  12         23    119: 141   |
    | 81       12     13:  24         23    119: 141   |
    | 82       12     25:  36         23    119: 141   |
    | 83       12     37:  48         23    119: 141   |
    | 84       12     49:  60         23    119: 141   |
    | 85       12     61:  72         23    119: 141   |
    | 86       12     73:  84         23    119: 141   |
    | 87       12     85:  96         23    119: 141   |
    | 88       12     97: 108         23    119: 141   |
    | 89       12    109: 120         23    119: 141   |
    | 90       12    121: 132         23    119: 141   |
    | 91       11    133: 143         23    119: 141   |
    | 92       11    144: 154         23    119: 141   |
    | 93       11    155: 165         23    119: 141   |
    | 94       11    166: 176         23    119: 141   |
    | 95       11    177: 187         23    119: 141   |
    | 96       12      1:  12         23    142: 164   |
    | 97       12     13:  24         23    142: 164   |
    | 98       12     25:  36         23    142: 164   |
    | 99       12     37:  48         23    142: 164   |
    |100       12     49:  60         23    142: 164   |
    |101       12     61:  72         23    142: 164   |
    |102       12     73:  84         23    142: 164   |
    |103       12     85:  96         23    142: 164   |
    |104       12     97: 108         23    142: 164   |
    |105       12    109: 120         23    142: 164   |
    |106       12    121: 132         23    142: 164   |
    |107       11    133: 143         23    142: 164   |
    |108       11    144: 154         23    142: 164   |
    |109       11    155: 165         23    142: 164   |
    |110       11    166: 176         23    142: 164   |
    |111       11    177: 187         23    142: 164   |
    |112       12      1:  12         23    165: 187   |
    |113       12     13:  24         23    165: 187   |
    |114       12     25:  36         23    165: 187   |
    |115       12     37:  48         23    165: 187   |
    |116       12     49:  60         23    165: 187   |
    |117       12     61:  72         23    165: 187   |
    |118       12     73:  84         23    165: 187   |
    |119       12     85:  96         23    165: 187   |
    |120       12     97: 108         23    165: 187   |
    |121       12    109: 120         23    165: 187   |
    |122       12    121: 132         23    165: 187   |
    |123       11    133: 143         23    165: 187   |
    |124       11    144: 154         23    165: 187   |
    |125       11    155: 165         23    165: 187   |
    |126       11    166: 176         23    165: 187   |
    |127       11    177: 187         23    165: 187   |
    |__________________________________________________|
```
With this output, users will be able to trace issues that occur on specific processors to geographic regions of the model domain.

Then, as the time-dependent portion of the model begins, output is provided for every timestep with the following form:
```
     Processing Day/Time [YYYYDDD:HHMMSS]: 2017356:000000
       Which is Equivalent to (UTC): 0:00:00  Friday,  Dec. 22, 2017
       Time-Step Length (HHMMSS): 000500
                 VDIFF completed...    3.7 seconds
                COUPLE completed...    0.1 seconds
                  HADV completed...    8.4 seconds
                  ZADV completed...    0.3 seconds
                 HDIFF completed...    0.3 seconds
              DECOUPLE completed...    0.0 seconds
                  PHOT completed...    1.4 seconds
               CLDPROC completed...    0.3 seconds
                  CHEM completed...    1.5 seconds
                  AERO completed...    2.5 seconds
            Master Time Step
            Processing completed...    18.7 seconds
```
This section documents the date and time the model is currently processing along with the time spent calculating every major sub-process. At the end of each simulation hour, the calculation time is also printed for the output process.
```
    Processing Day/Time [YYYYDDD:HHMMSS]: 2017356:005500
       Which is Equivalent to (UTC): 0:55:00  Thursday,  Dec. 22, 2017
       Time-Step Length (HHMMSS): 000500
                 VDIFF completed...    31.7 seconds
                COUPLE completed...    0.2 seconds
                  HADV completed...    2.3 seconds
                  ZADV completed...    0.4 seconds
                 HDIFF completed...    0.5 seconds
              DECOUPLE completed...    0.0 seconds
                  PHOT completed...    0.6 seconds
               CLDPROC completed...    19.1 seconds
                  CHEM completed...    1.5 seconds
                  AERO completed...    2.5 seconds
            Master Time Step
            Processing completed...    58.9 seconds
 
      =--> Data Output completed...    52.2 seconds
```
This procedure repeats for every hour of the output day until completion of that day.
```
     ==============================================
     |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|
     ==============================================
     Date and time 0:00:00   Dec. 23, 2017   (2017357:000000)
 
     The elapsed time for this simulation was     6390.3 seconds.
 
real 6394.83
user 2000938.03
sys 827.71

CMAQ Processing of Day 20171222 Finished at Tue Sep 13 16:42:02 EDT 2022

\\\\\=====\\\\\=====\\\\\=====\\\\\=====/////=====/////=====/////=====/////
```

After the final day has been completed, summary information is printed for the computation time of every executed day.
```
==================================
  ***** CMAQ TIMING REPORT *****
==================================
Start Day: 2017-12-22
End Day:   2018-01-01
Number of Simulation Days: 8
Domain Name:               12US1
Number of Grid Cells:      1538636  (ROW x COL x LAY)
Number of Layers:          44
Number of Processes:       128
   All times are in seconds.

Num  Day        Wall Time
01   2017-12-22   6394.83
02   2017-12-23   6137.89
03   2017-12-24   6039.40
04   2017-12-25   6201.84
05   2017-12-26   6403.34
06   2017-12-27   6108.96
07   2017-12-28   6308.07
08   2017-12-29   6207.25
09   2017-12-30   6306.42
10   2017-12-31   6303.56
11   2018-01-01   6107.77
     Total Time = 68519.33
      Avg. Time = 6229.03 
``` 

The processor-specific logfiles provide detailed information on the operation of hundreds of model tasks from mapping variables to opening and reading input files. Warnings that may be important for users to be aware of are printed to these files. To confirm that the model ran to completion view the run.[data].log file. For MPI runs, you may check any of the CTM_LOG_[ProcessorID]*.log files. A successful run will contain the following line at the bottom of the log(s):

```
>>----> Program completed successfully <----<<
```

Note: The log file for each processor is also moved from the $CMAQ_HOME/CCTM/scripts directory to the data output directory:

```
$CMAQ_DATA/output_CCTM_v54_[compiler]/[data_name]
```

### 5.7.2 CCTM Output files

The output results will have been placed in the directory:

```
$CMAQ_DATA/output_CCTM_v54_[compiler]_[data_name]
```

and can include the following netCDF-type files: ACONC, AELMO, B3GTS_S, CGRID, CONC, DEPV, DRYDEP, DUSTEMIS, LTNGDIAG1, LTNGDIAG2, MEDIA_CONC, ELMO, RJ_1, RJ_2, RJ_3, SOILOUT, SSEMIS, VDIFF, VSED, WETDEP1, WETDEP2 and VEXT_1. The in-depth description about each of these files is described in [Chapter 7](CMAQ_UG_ch07_model_outputs.md).


### 5.7.3 Common errors causing the CCTM simulation to crash

Common errors in a CCTM simulation include the following:

-  Incorrect paths to input files. Look in the CCTM screen output (captured in your log file) for an Error message about an input file   not being found.
-  Incorrect MPI implementation. A series of MPI errors at the end of the log file often indicate that the MPI job was not submitted correctly.

Check the last few lines of the CCTM output log for messages to help diagnose why the simulation did not complete.

## 5.8 CMAQ Benchmark Test Case
See the [CMAQ Installation and Benchmarking Tutorial](Tutorials/CMAQ_UG_tutorial_benchmark.md) for step-by-step instructions for running the 2 day benchmark case and links to reference input and output data. 


<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch04_model_inputs.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch06_model_configuration_options.md)<br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->

<!-- START_OF_COMMENT -->

[link_5_bldmake]: ../../UTIL/bldmake/README.md

<!-- END_OF_COMMENT -->

[link_5_bldmake]:  https://github.com/USEPA/CMAQ/blob/main/UTIL/bldmake/README.md