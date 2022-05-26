# WRF-CMAQ Benchmarking Tutorial # 

**Purpose**: This guide describes how to install and run the WRF-CMAQ test case, which serves two different purposes. The first being to familiarize the user with the WRF-CMAQ suite of programs and how they work together, and secondly to verify the installation of the software on your system via benchmarking. 

Users are highly encouraged to work through the [CMAQ Benchmark Tutorial](../CMAQ_UG_tutorial_benchmark.md) and [WRF Installation Guide](https://www2.mmm.ucar.edu/wrf/users/) to familiarize themselves with the individuals program components.

The following support software are required for compiling and running WRF-CMAQ.  

1. Fortran and C compilers, e.g., [Intel](https://software.intel.com/en-us/fortran-compilers), [Portland Group](http://www.pgroup.com), [Gnu](https://gcc.gnu.org/wiki/GFortran)
2. [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
2. Message Passing Interface (MPI), e.g., [OpenMPI](https://www.open-mpi.org) or [MVAPICH2](http://www.mcs.anl.gov/research/projects/mpich2).
3. Latest release of [netCDF-C](https://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html) and [netCDF-Fortran](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html)
4. [I/O API](https://www.cmascenter.org/download/software/ioapi/ioapi_3-2.cfm?DB=TRUE) version 3.2 **tagged 20200828 without OpenMP support**
5. [C-Shell](https://github.com/tcsh-org/tcsh) 

**Note: if you have not installed the above libraries, please see the CMAQ_UG_tutorial_build_[gcc/intel/pgi].md tutorials available here: 
https://github.com/USEPA/CMAQ/tree/main/DOCS/Users_Guide/Tutorials**

The suggested hardware requirements for running the CMAQ Southeast Benchmark case on a Linux workstation are:

1. Linux environment with a 16 processors
2. 16 GB RAM
3. 400 GB hard drive storage


## Installing WRF-CMAQ ##

In the directory where you would like to install WRF-CMAQ, create the directory issue the following command to clone the EPA GitHub repository for CMAQv5.3.3:

```
git clone https://github.com/USEPA/CMAQ.git CMAQ_REPO
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
set CMAQ_HOME = [your_install_path]/WRF_CMAQv533
```
Now execute the script.
```
./bldit_project.csh
```

CMAQ_HOME will be the location of your newly created WRF_CMAQ project directory (where you will compile and run WRF-CMAQ).

## Configuring the WRF-CMAQ Environment

Compiling WRF-CMAQ requires several libraries and include files to be made avialable. Because these libraries and include files are not expected to be in standard locations, users need to explicitly provide the path to this via environmental variables. 

The environmental variables that need to be set are located in the config_cmaq.csh script, located at the root of the WRF-CMAQ project directory. 

Note: WRF source code expects that you have already collated the netCDF-C and netCDF-Fortran libraires into one directory. If you have done so, please follow the example instructions: 

```
   cd /[your_install_path]/LIBRARIES
   mkdir netcdf_combined
   cp -rp ./netcdf-fortran-4.4.5-gcc9.1.0/* ./netcdf_combined/
   cp -rp ./netcdf-c-4.7.0-gcc9.1.0/* ./netcdf_combined/
```

Once netCDF collation is completed, navigate back to your WRF-CMAQ project directory and edit the config_cmaq.csh script like the following example: 

```
Navigate to the compiler of your choice for. For example, for GNU based compilers go to section 148 of the script. Then set the 

#> I/O API and netCDF for WRF-CMAQ 
setenv NETCDF [your_install_path]/LIBRARIES/netcdf_combined 
setenv IOAPI  [your_install_path]/LIBRARIES/ioapi-3.2  
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
```

Edit bldit_cctm.csh and uncomment the option to build WRF-CMAQ twoway:   
      
```
#> Two-way WRF-CMAQ 
set build_twoway                      #> uncomment to build WRF-CMAQ twoway; 
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

### Run the bldit_cctm.csh script

```
./bldit_cctm.csh gcc |& tee bldit_cctm_twoway.log
```
Users should look for the following message at the end of their bldit_cctm_twoway.log: 

```
--->                  Executables successfully built                  <---

-rwxr-xr-x. 1 user home 51139232 Jun  7 19:03 main/ndown.exe
-rwxr-xr-x. 1 user home 51187088 Jun  7 19:03 main/real.exe
-rwxr-xr-x. 1 user home 50445512 Jun  7 19:03 main/tc.exe
-rwxr-xr-x. 1 user home 81349320 Jun  7 19:02 main/wrf.exe

==========================================================================

```

If the User sees this, the WRF-CMAQ model has been successfully compiled and built. If not, the User should double check the library paths above and try again. If it still fails, please reach post on the [CMAS Forum](https://forum.cmascenter.org/c/wrf-cmaq).

## Install the CMAQ input reference/benchmark data

[Link to CMAQv5.3.2_Benchmark_2Day_Input.tar.gz input data on Google Drive](https://drive.google.com/file/d/1ex6Wr4dX6a0fgaDfhO0VEJNaCKqOflI5/view?usp=sharing)

  - Use the gdrive command to download the dataset.
  - If this is the first time that you are using gdrive, or if you have an issue with your token, please read the following instructions
  - [Tips to download data from CMAS Data Warehouse](https://docs.google.com/document/d/1e7B94zFkbKygVWfrhGwEZL51jF4fGXGXZbvi6KzXYQ4)
  
  
  ```
  gdrive download 1ex6Wr4dX6a0fgaDfhO0VEJNaCKqOflI5
  ```
  
    
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

    5. feedback_restart    (indicate aerosol SW direct effect information is
                            available in the WRF restart file or not)
                            
* One sample run scripts is provided; run_cctm_Bench_2016_12SE1.WRFCMAQ.csh (for coupled
  model with SW feedback on).


  - Start with the run_cctm_Bench_2016_12SE1.WRFCMAQ.csh that specifies direct_sw_feedback = .true.
  - and the CMAQv5.3.3 input benchmark dataset to run CMAQ-WRF with feedback
  - It is configured to run on 16 processors and for 2 days of model simulation
  - Edit the script to specify the paths, modify the number of processors and batch queue commands
  - Verify that the OMIfile definition matches the latest release of CMAQv5.3.3.
  
  Now, modify the following section to specify your local paths:
  
  ```
     set WORKDIR     = /proj/ie/proj/CMAS/WRF-CMAQ/CMAQ_v5.3.3/CCTM/scripts
     set WRF_DIR     = $WORKDIR/BLD_WRFv4.3_CCTM_v533_gcc  # WRF source code directory
     set INPDIR      = /proj/ie/proj/CMAS/WRF-CMAQ/from_EPA/from_gdrive/CMAQv5.3.2_Benchmark_2Day_Input/2016_12SE1
     set OMIpath     = $WRF_DIR/cmaq                              # path optics related data files
     set OUTPUT_ROOT = $WORKDIR  # output root directory
     set NMLpath     = $WRF_DIR/cmaq                              # path with *.nml file mechanism dependent
     set NMLpath2    = $WRF_DIR/cmaq                              # path with Species_Table_TR_0.nml file
     set EMISSCTRL   = $WRF_DIR/cmaq                              # path of Emissions Control Fil
  
  ```  
  
   - Verify the following settings
    ```
    set NPROCS =    16
    set OMIfile    = OMI_1979_to_2019.dat
    ```
  - Run the job (if you have a batch queuing system such as SLURM use sbatch): 
  ```
  ./run_cctm_Bench_2016_12SE1.WRFCMAQ.csh
  ```
  
### Verify that the run was successful
   - look for the output directory
   
   ```
   cd WRFCMAQ-output-sw_feedback
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./2016183/rsl.out.0000
   ```
   | Note, the CMAQv5.3.3 output results will not directly compare to the no shortwave feedback (nosw) WRF-CMAQ output, as different meterology and timesteps were used.  To do a comparison between CMAQv5.3.3 and WRF-CMAQ, use WRF-CMAQ to output the MCIP meteorology files, and then use those MCIP inputs with the CMAQv5.3.3 ICON and BCON inputs.
