# WRF-CMAQ Benchmarking Tutorial # 

**Purpose**: This guide describes how to install and run the WRF-CMAQ test case, which serves two different purposes. The first being to familiarize the user with the WRF-CMAQ suite of programs and how they work together, and secondly to verify the installation of the software on your system via benchmarking. 

Users are highly encouraged to work through the [CMAQ Benchmark Tutorial](CMAQ_UG_tutorial_benchmark.md) and [WRF Installation Guide](https://www2.mmm.ucar.edu/wrf/users/) to familiarize themselves with the individuals program components.

## System Checks 

CMAQ requires a specific hardware and software configuration. To learn about these requirements, please refer to the tutorial on [preparing your compute environment for CMAQ simulations](CMAQ_UG_tutorial_configure_linux_environment.md).

## Installing WRF-CMAQ ##

If you followed the [CMAQ_UG_tutorial_build_library_gcc_support_nc4.md](CMAQ_UG_tutorial_build_library_gcc_support_nc4.md) tutorial then you have installed the required netCDF-C and netCDF-Fortran libraries in to a combined folder, and you can skip to the section Configuring the WRF-CMAQ environment.

In the directory where you would like to install WRF-CMAQ, create the directory issuing the following command to clone the EPA GitHub repository for CMAQv5.5:

```
git clone -b main https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

## Check Out a new Branch in the CMAQ Repository 

Checking out a new branch is a good idea even if you are not doing code development, per se. It is likely that you will want to retrieve new updates in the future, and an easy way to do this is through the main branch in the git repo. Thus, it is beneficial to leave it unperturbed if possible.
```
cd CMAQ_REPO
git checkout -b my_branch
```

## Building and running in a user-specified directory outside of the repository

In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

In bldit_project.csh, modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:
```
set CMAQ_HOME = [your_install_path]/CMAQv5.5
```
Now execute the script.
```
./bldit_project.csh
```

CMAQ_HOME will be the location of your newly created WRF_CMAQ project directory (where you will compile and run WRF-CMAQ).

## Combining the netCDF-C and netCDF-Fortran libraries

Compiling WRF-CMAQ requires several libraries and include files to be made avialable. Because these libraries and include files are not expected to be in standard locations, users need to explicitly provide the path to this via environmental variables. 

The environmental variables that need to be set are located in the config_cmaq.csh script, located at the root of the WRF-CMAQ project directory. 

Note: WRF source code expects that you have already collated the netCDF-C and netCDF-Fortran libraires into one directory. If you have not done so, please follow the example instructions: 

```
   cd /[your_install_path]/LIBRARIES_gcc
   mkdir netcdf_combined
   cp -rp ./netcdf-fortran-4.4.5-gcc9.1.0/* ./netcdf_combined/
   cp -rp ./netcdf-c-4.7.0-gcc9.1.0/* ./netcdf_combined/
```

### Configuring the WRF-CMAQ Environment

Navigate to the compiler of your choice to compile WRF-CMAQ using distributed memory parallel (dmpar). 
For example, for GNU based compilers go to line 148 of the config_cmaq.csh. Then set the paths for NETCDF and IOAPI and the WRF_ARCH to 34. 

```
#> I/O API and netCDF for WRF-CMAQ 
setenv NETCDF [your_install_path]/LIBRARIES_gcc
setenv IOAPI  [your_install_path]/LIBRARIES_gcc/ioapi-3.2  
setenv WRF_ARCH 34              # [1-75]  WRF 64 Bit Linux_x86 Compiler/Architecture options
```

*Note: WRF_ARCH environment variable is used to configure WRF and is based on the following options:*

```Please select from among the following Linux x86_64 options:

  1. (serial)    2. (smpar)   3. (dmpar)      4. (dm+sm)   PGI (pgf90/gcc)
  5. (serial)    6. (smpar)   7. (dmpar)      8. (dm+sm)   PGI (pgf90/pgcc): SGI MPT
  9. (serial)   10. (smpar)  11. (dmpar)     12. (dm+sm)   PGI (pgf90/gcc): PGI accelerator
 13. (serial)   14. (smpar)  15. (dmpar)     16. (dm+sm)   INTEL (ifort/icc)
                                             17. (dm+sm)   INTEL (ifort/icc): Xeon Phi (MIC architecture)
 18. (serial)  19. (smpar)  20. (dmpar)      21. (dm+sm)   INTEL (ifort/icc): Xeon (SNB with AVX mods)
 22. (serial)  23. (smpar)  24. (dmpar)      25. (dm+sm)   INTEL (ifort/icc): SGI MPT
 26. (serial)  27. (smpar)  28. (dmpar)      29. (dm+sm)   INTEL (ifort/icc): IBM POE
 30. (serial)               31. (dmpar)                    PATHSCALE (pathf90/pathcc)
 32. (serial)  33. (smpar)  34. (dmpar)      35. (dm+sm)   GNU (gfortran/gcc)
 36. (serial)  37. (smpar)  38. (dmpar)      39. (dm+sm)   IBM (xlf90_r/cc_r)
 40. (serial)  41. (smpar)  42. (dmpar)      43. (dm+sm)   PGI (ftn/gcc): Cray XC CLE
 44. (serial)  45. (smpar)  46. (dmpar)      47. (dm+sm)   CRAY CCE (ftn $(NOOMP)/cc): Cray XE and XC
 48. (serial)  49. (smpar)  50. (dmpar)      51. (dm+sm)   INTEL (ftn/icc): Cray XC
 52. (serial)  53. (smpar)  54. (dmpar)      55. (dm+sm)   PGI (pgf90/pgcc)
 56. (serial)  57. (smpar)  58. (dmpar)      59. (dm+sm)   PGI (pgf90/gcc): -f90=pgf90
 60. (serial)  61. (smpar)  62. (dmpar)      63. (dm+sm)   PGI (pgf90/pgcc): -f90=pgf90
 64. (serial)  65. (smpar)  66. (dmpar)      67. (dm+sm)   INTEL (ifort/icc): HSW/BDW
 68. (serial)  69. (smpar)  70. (dmpar)      71. (dm+sm)   INTEL (ifort/icc): KNL MIC
 72. (serial)  73. (smpar)  74. (dmpar)      75. (dm+sm)   FUJITSU (frtpx/fccpx): FX10/FX100 SPARC64 IXfx/Xlfx

Enter selection [1-75] : ------------------------------------------------------------------------
```

If you have never configured WRF before, here are some guidelines:

   - Choose the appropriate compiler platform and hardware architecture if applicable
   - Choose the dmpar (Distributed Memory Parallelization -- MPI) option only. The serial, Single Memory Parallelization (smpar), and (dm+sm) are out of scope of this tutorial, configuration with those options require [additional support](https://forum.cmascenter.org/). 
   - For more information refer to the [WRF User Guide](https://www2.mmm.ucar.edu/wrf/users/docs/user_guide_v4/v4.0/contents.html)

## Compiling WRF-CMAQ
*Before proceeding, it should be noted that building the ICON and BCON executables are optional steps when working specifically with the benchmark data. This is because the initial condition and boundary condition files have been provided for you within the benchmark data set. For further information on these preprocessors please reference [Chapter 4](../CMAQ_UG_ch04_model_inputs.md).*  

### Modify the bldit_cctm.csh 

Navigate to the WRF-CMAQ project directory and from there navigate to the CCTM/scripts directory.

```
cd CCTM/scripts
cp bldit_cctm.csh bldit_wrf4.5.1_cctmv55.csh
```

Edit bldit_wrf4.5.1_cctmv55.csh and uncomment the option to build WRF-CMAQ twoway:   
      
```
#> Two-way WRF-CMAQ 
set build_twoway                      #> uncomment to build WRF-CMAQ twoway; 
```

Verify that WRF_CMAQ environment variable is set to 1:

```
grep WRF_CMAQ bldit_wrf4.5.1_cctmv55.csh
```

Output:

```
    setenv WRF_CMAQ 1
```

Set the WRF version to release-v4.5.1

```
 if ( $?build_twoway ) then            # WRF Version used for WRF-CMAQ Model (must be v4.4+)
    set WRF_VRSN = release-v4.5.1
 endif
```

Set the BLD directory name to add wrf to it, so that when the script copies the BLD directory, that it does not copy an existing direectory.

```
#> Set and create the "BLD" directory for checking out and compiling 
#> source code. Move current directory to that build directory.
 if ( $?Debug_CCTM ) then
     set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_${Mechanism}_${DepMod}_wrfcmaq_debug
 else
     set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_${Mechanism}_${DepMod}_wrfcmaq
 endif
```


Configure CMAQ benchmark Science Modules:

The build directory parameters for the benchmark test case include the following:

-   Multiprocessor simulation 
-   3-D Advection Scheme: wrf_cons
-   Horizontal diffusion: Multiscale
-   Vertical diffusion: ACM2_M3Dry
-   Deposition: M3Dry
-   Chemistry solver: EBI
-   Aerosol module: AERO7
-   Cloud module: ACM_AE7
-   Mechanism: cb6r3_ae7_aq
-   Online biogenic emissions
-   Inline plume rise

To configure these parameters, the CCTM Science Modules within the bldit_cctm.csh need to be modified from set defaults. The comments within the script itself should help guide the user on the options for each variable and how to set them. Further information on variable names can be found in 
[Appendix A](../Appendix/CMAQ_UG_appendixA_model_options.md).

### Run the build script

If you have set up environment modules for your libraries, then load them

```
module load netcdf-4.5.3-for_nc4/gcc-11.2  ioapi-3.2/gcc-11.2  openmpi_5.0.5/gcc
```

Run the bldit script to compile wrf-cmaq

```
./bldit_wrf4.5.1_cctmv55.csh gcc |& tee bldit_wrf4.5.1_cctmv55.log
```
Users should look for the following message at the end of their bldit_wrf4.5.1_cctmv55.log: 

```
--->                  Executables successfully built                  <---

-rwxr-xr-x. 1 user home 51139232 Jun  7 19:03 main/ndown.exe
-rwxr-xr-x. 1 user home 51187088 Jun  7 19:03 main/real.exe
-rwxr-xr-x. 1 user home 50445512 Jun  7 19:03 main/tc.exe
-rwxr-xr-x. 1 user home 81349320 Jun  7 19:02 main/wrf.exe

==========================================================================

```

If the User sees this, the WRF-CMAQ model has been successfully compiled and built. If not, the User should double check the library paths above and try again. 

If you get this error:

```
Error NoahMP submodule files not populating WRF directories
make: *** [wrf] Error 31
```

This is because of a conflict between git clone and the environment modules.

Try the following commands:

```
cd ./BLD_WRF_release-v4.5.1_CCTM_v55_gcc/phys
git clone ssh://github.com/NCAR/noahmp/
cd ..
./compile em_real |& tee compile.again.log
```

If the git clone ssh command doesn't work, try 

```
git clone https://github.com/NCAR/noahmp/
```

If you get this error:

gfortran: error: libcmaqlib.a: No such file or directory

Then try the following commands to recompile the cmaq executable, and then retry building WRF-CMAQ

```
cd cmaq
make -f Makefile.twoway
cd ..
./compile em_real |& compile.again.log
```

If you get this error 

```
name2fid.f:(.text+0x17): undefined reference to `GOMP_critical_name_start'
```

Edit the configure.wrf to remove the commented out -fopenmp flags.

Change

```
OMP             =       # -fopenmp
OMPCC           =       # -fopenmp
```

to

```
OMP             =        -fopenmp
OMPCC           =        -fopenmp
```

Recompile

```
./compile em_real | & tee ./compile.again.3rd.log
```

An additional error can occur if time is not found on your system.
If this is the case, edit the configure.wrf to remove the time command from the F90 definition.

```
cd BLD_WRF_release-v4.5.1_CCTM_v55_gcc
vi configure.wrf
```

edit line 138

change

```
FC              =       time $(DM_FC)
```

to

```
FC              =       $(DM_FC)
```

If the build fails due to not finding the mpi.h include file, then edit the configure.wrf file to add the following:

Add the following to the list of the INCLUDE_MODULES = 

```
-I$(MPIPATH)/include \
```

Then define MPIPATH (add under the NETCDFPATH settings) to your local path
Example: 

```
MPIPATH         =    /nas/sycamore/apps/openmpi/5.0.5/
```

Example:

```
INCLUDE_MODULES =    $(MODULE_SRCH_FLAG) \
                     $(ESMF_MOD_INC) $(ESMF_LIB_FLAGS) \
                      -I$(WRF_SRC_ROOT_DIR)/main \
                      -I$(WRF_SRC_ROOT_DIR)/external/io_netcdf \
                      -I$(WRF_SRC_ROOT_DIR)/external/io_int \
                      -I$(WRF_SRC_ROOT_DIR)/frame \
                      -I$(WRF_SRC_ROOT_DIR)/share \
                      -I$(WRF_SRC_ROOT_DIR)/phys \
                      -I$(WRF_SRC_ROOT_DIR)/wrftladj \
                      -I$(WRF_SRC_ROOT_DIR)/chem -I$(WRF_SRC_ROOT_DIR)/inc \
                      -I$(NETCDFPATH)/include \
                      -I$(MPIPATH)/include \

REGISTRY        =    Registry
CC_TOOLS_CFLAGS = -DNMM_CORE=$(WRF_NMM_CORE)

LIB             =    $(LIB_BUNDLED) $(LIB_EXTERNAL) $(LIB_LOCAL) $(LIB_WRF_HYDRO)  $(NETCDF4_DEP_LIB)
LDFLAGS         =    $(OMP) $(FCFLAGS) $(LDFLAGS_LOCAL)
ENVCOMPDEFS     =     -DWRF_CMAQ
WRF_CHEM        =       0
CPPFLAGS        =    $(ARCHFLAGS) $(ENVCOMPDEFS) -I$(LIBINCLUDE) $(TRADFLAG)
NETCDFPATH      =    /proj/ie/proj/CMAS/CMAQ/WRF-CMAQv5.5/build/LIBRARIES_gcc11.2
MPIPATH         =    /nas/sycamore/apps/openmpi/5.0.5/
HDF5PATH        =
WRFPLUSPATH     =
RTTOVPATH       =
PNETCDFPATH     =
ADIOS2PATH      =
```

Also edit the configure.wrf to add the -fallow-argument-mismatch option 

```
FCOPTIM         =       -O2 -ftree-vectorize -funroll-loops -fallow-argument-mismatch
FCCOMPAT        =        -fallow-argument-mismatch -fallow-invalid-boz -fallow-argument-mismatch
```

Rerun the bldit script

```
./bldit_wrf4.5.1_cctmv55.csh gcc | & tee ./bldit_wrf4.5.1_cctmv55.log
```



If it still fails, please reach post on the [CMAS Forum](https://forum.cmascenter.org/c/wrf-cmaq).



### Install the WRF-CMAQ reference input benchmark data


Download the CMAQ two day reference input data from the [CMAS Center Data Warehouse Google Drive](https://drive.google.com/drive/folders/1AFUB-4kzIXXoZr4hOHNBqRvy9JQ9_MDp?usp=sharing) or from the [CMAS Center Data Warehouse Amazon Web Services S3 Bucket](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html#v5_5/
), file CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz. The CMAQ benchmark test case is a two day simulation for July 1-2 2018 on a 100 column x 105 row x 35 layer 12-km resolution domain over the northeast U.S.  

  - Use the gdrive command if you are downloading the dataset from Google Drive.
  - If this is the first time that you are using gdrive, or if you have an issue with your token, please read the following instructions
  - [Tips to download data from CMAS Data Warehouse](https://docs.google.com/document/d/1e7B94zFkbKygVWfrhGwEZL51jF4fGXGXZbvi6KzXYQ4)
  - Text files are included that provide a list of the files in the benchmark input datasets.
  - wget can be used to download data from the CMAS Open Data Folder on AWS
  - Reference output files for WRF-CMAQv5.5 will be provided at a later date.


Copy the data to `$CMAQ_DATA`. Navigate to the `$CMAQ_DATA` directory, unzip and untar the two day benchmark input files:

```
cd $CMAQ_DATA
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz
tar xvzf CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz
```

The input files for the WRF-CMAQ benchmark case are provided in the base model benchmark inputs .tar file. Output WRF-CMAQ files associated with the sample run script for the coupled WRF-CMAQ model in this release package are also available.  
        
## Running the WRF-CMAQ model

Note, in this new coupled model design, the namelist is used to modify settings for WRF.
Environment variables such as WRF_CMAQ_FREQ are no longer used.  
The following commonly modified namelist options for WRF-CMAQ are specified in the run script.

    1. wrf_cmaq_option     (dictates how the coupled model execute)

       0 = run WRF only
       1 = run WRF only               w   producing MCIP like GRID and MET files
       2 = run WRF-CMAQ coupled model w/o producing MCIP like GRID and MET files
       3 = run WRF-CMAQ coupled model w   producing MCIP like GRID and MET files

    2. wrf_cmaq_freq       (indicate how often WRF and CMAQ interact)

    3. met_file_tstep      (time step size of MCIP like intermediate output files)

    4. direct_sw_feedback  (indicate to turn on aerosol short wave direct effect)

    5. feedback_restart    (indicate aerosol short wave direct effect information is
                            available in the WRF restart file or not)
                            
* One sample run scripts is provided (run_cctm_Bench_2018_12NE3.WRFCMAQ.csh) for running the coupled
  model with short wave radiation feedback turned on.  Reference benchmark output will be provided for a test simulations with short wave radiation feedback turned on ("sf") and turned off ("nf"). 


  - Start with the run_cctm_Bench_2018_12NE3.WRFCMAQ.csh that specifies direct_sw_feedback = .true.
  - and the CMAQv5.4 input benchmark dataset to run CMAQ-WRF with feedback
  - It is configured to run on 32 processors and for 1 day of model simulation
  - Edit the script to specify the paths, modify the number of processors and batch queue commands
  - Fix VEGPARM.TBL, since Benchmark runs with PX LSM with NLCD40 Data
  
**\* Users using PX Land Surface Model option with NLCD40 dataset are required to fix their VEGPARM.TBL before running WRF-CMAQ, due to a bug in the WRF released VEGPARM.TBL. Please see the [WRF Repository](https://github.com/wrf-model/WRF/pull/1733) for more information on the bug.***

   - For example, one option may include manually editing the existing VEGPARM.TBL (found in WRF-CMAQ Build Directory) with the fix seen on the [WRF Repository](https://github.com/wrf-model/WRF/pull/1733/files). 


  Edit the run script

  ```
    vi run_cctm_Bench_2018_12NE3.WRFCMAQ.csh
  ```

  Change end date to run for two days


  ```
     set START_DATE = "2018-07-01"     # beginning date (July 1, 2016)
     set END_DATE   = "2018-07-02"     # ending date    (July 14, 2016)
  ```

   Set the WRF version number

  ```
     set wrfv    = release_v4.5.1
  ```

   Now, modify the following section to specify your local paths: 

   ```
     set WORKDIR     = ${PWD}
     set WRF_DIR     = $WORKDIR/BLD_WRF_release-v4.5.1_CCTM_v55_gcc  # WRF source code directory
     set INPDIR      = ${CMAQ_DATA}/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input/2018_12NE3
     set OUTPUT_ROOT = $WORKDIR  # output root directory
     set output_direct_name = WRFCMAQ-output-${version}        # Output Directory Name
     setenv OUTDIR $OUTPUT_ROOT/$output_direct_name   # output files and directories
     set NMLpath     = $WRF_DIR/cmaq                           # path with *.nml file mechanism dependent

  ```

   - Set the number of processors to run WRF-CMAQ 

    ```
    set NPROCS =    32
    ```

  - Load the environment modules 

    ```
    module load netcdf-4.5.3-for_nc4/gcc-11.2  ioapi-3.2/gcc-11.2  openmpi_5.0.5/gcc
    ```
    
  - Run the job (if you have a batch queuing system such as SLURM use sbatch after adding the sbatch commands to the top of the run script): 

  ```
  ./run_cctm_Bench_2018_12NE3.WRFCMAQ.csh
  ```
  
### Verify that the run was successful
   - look for the output directory
   
   ```
   cd WRFCMAQ-output-sf_feedback
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./2018182/rsl.out.0000
   ```

       |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|

### Compare the output to what is available on the S3 bucket

    ```
     cd $CMAQ_DATA
     wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/WRF-CMAQ/WRFv4.5.1-CMAQv55-output-sw_feedback_gcc.tar.gz
     tar xvzf WRFv4.5.1-CMAQv55-output-sw_feedback_gcc.tar.gz
     ```

