## WRF-CMAQ Tutorial ## 

### Procedure to build and run the WRF-CMAQ model using gnu compiler: ###

### Step 1: choose your compiler, and load it using the module command if it is available on your system

```
module avail
```

```
module load openmpi_4.0.1/gcc_9.1.0 
```

### Step 2: Download and install netCDF Fortran and C libraries

   **Skip to Step 3, if you have a module for netCDF avialable on your system and you have loaded it**

   Follow the tutorial for building libraries to build netCDF C and Fortran Libraries
   https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_build_library_gcc.md
   
   - Follow these instructions to combine the libraries into a single combined directory
   
   ```
   cd /[your_install_path]/openmpi_4.0.1_gcc_9.1.0/LIBRARIES
   mkdir netcdf_combined
   cp -rp ./netcdf-fortran-4.4.5-gcc9.1.0/* ./netcdf_combined/
   cp -rp ./netcdf-c-4.7.0-gcc9.1.0/* ./netcdf_combined/
   ```
   
   Now you should have a copy of both the netcdf C and netcdf Fortran libraries under 
   netcdf_combined/lib

   - set the following environment variables including the path to your combined netcdf libraries, include files
   
   ```
   setenv NETCDF [your_install_path]/openmpi_4.0.1_gcc_9.1.0/Build_WRF/LIBRARIES/netcdf_combined
   setenv CC gcc
   setenv CXX g++
   setenv FC gfortran
   setenv FCFLAGS -m64
   setenv F77 gfortran
   setenv FFLAGS -m64
   ```
   
 - check to see that the path to each compiler is defined using
 
    ```
    which gcc
    which g++
    which gfortran
    ```
    
  - If they are not found, ask for assistance from your system administrator, 
    or if you know the path then specify it using the environment variable
    
    ```
    setenv CC /nas/longleaf/apps/gcc/9.1.0/bin/gcc
    ```

### Edit your .cshrc to add the path to the library by setting the LD_LIBRARY_PATH environment variable

```
#for gcc WRF-CMAQ build
setenv NCF_COMBO /[your_install_path]/openmpi_4.0.1_gcc_9.1.0/LIBRARIES/netcdf_combined/
setenv LD_LIBRARY_PATH ${NCF_COMBO}/lib:${LD_LIBRARY_PATH}
```

### Make sure that there is no other definition or setting of LD_LIBRARY_PATH further down in your .cshrc file that may be overwriting your setting.

### Make sure you log out and log back, or run csh in to activate the LD_LIBRARY_PATH setting.

### Step 3: Download IOAPI_3.2 (a specific tagged version, see below) and install it.

Note The complete I/O API installation guide can be found at either of the following:

https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html

or

https://cjcoats.github.io/ioapi/AVAIL.html

#### Follow the instructions on how to install I/O API available

#### Method 1. Download the tar.gz file from the github site.
     
     wget http://github.com/cjcoats/ioapi-3.2/archive/20200828.tar.gz
     tar -xzvf 20200828.tar.gz
     cd ioapi-3.2-20200828
     

#### Method 2. Use Git clone to obtain the code
    
     
     git clone https://github.com/cjcoats/ioapi-3.2
     cd ioapi-3.2         ! change directory to ioapi-3.2
     git checkout -b 20200828   ! change branch to 20200828 for code updates
     ln -s ioapi-3.2-20200828 ./ioapi-3.2  ! create a symbolic link to specify the tagged version
     

#### Change directories to the ioapi directory
     
     
     cd ioapi
     
     
#### Copy the Makefile.nocpl to Makefile 
     
     
     cp Makefile.nocpl Makefile

#### Change the BASEDIR definition from HOME to INSTALL

```
BASEDIR = ${HOME}/ioapi-3.2
````
change to
```
BASEDIR = ${INSTALL}/ioapi-3.2-20200828
```
     
     
 #### set the INSTALL and BIN environment variables:
     
     
     setenv INSTALL [your_install_path]/LIBRARIES/openmpi_4.0.1_gcc_9.1.0
     setenv BIN  Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
     

 #### Make the installation directory

    
     mkdir $INSTALL/$BIN
      
 ### Edit the Makefile to add a path to the combined netCDF library directory
 
 change
 
 ```
 NCFLIBS = -lnetcdff -lnetcdf
 ```
 
 to
 
   ```
   NCFLIBS    = -L /[your_install_path]/LIBRARIES/netcdf_combined/lib/ -lnetcdff -lnetcdf
   ```
 
 #### change into the ioapi directory and copy the existing Makeinclude.Linux2_x86_64gfort to have an extension that is the same as the BIN environment variable
 
 ```
 cd ioapi
 cp Makeinclude.Linux2_x86_64gfort Makeinclude.Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
 ```
 ### Edit the Makeinclude.Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0 to comment out the OMPFLAG and OMPLIB
 
 ```
 gedit Makeinclude.Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
 ```
 
 - comment out the following lines by adding a # before the setting
 
 ```
 #OMPFLAGS  = -fopenmp
 #OMPLIBS   = -fopenmp
 ```
 
 
 ### Build ioapi using the following command
 
 
 ```
 make |& tee make.log
 ```
 
 ### Verify that the libioapi.a and the m3tools have been successfully built
 
 ```
 ls -lrt /[your_install_path]/LIBRARIES/ioapi-3.2-20200828/Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0/libioapi.a
 ```
 
 ### Note: If you get a shared object problem when trying to run m3tools such as the following:
 ```
./juldate
./juldate: error while loading shared libraries: libimf.so: cannot open shared object file: No such file or directory
```

### Step 4: Install CMAQ
     - follow these instructions to download the code, then use the modifications in Step 5:  [CMAQ Benchmark Tutorial](CMAQ_UG_tutorial_benchmark.md)
In the directory where you would like to install CMAQ, create the directory issue the following command to clone the EPA GitHub repository for CMAQv5.3.2:

```
git clone -b master https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

Build and run in a user-specified directory outside of the repository

In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

Edit bldit_project.csh, to modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:

```
set CMAQ_HOME = /home/username/WRF-CMAQ/CMAQ_v5.3.2
```

Now execute the script.

./bldit_project.csh

Change directories to the CMAQ_HOME directory

### Step 5. Edit and source the config_cmaq.csh to specify the paths of the ioapi and netCDF libraries 
   
```
#> I/O API and netCDF for WRF-CMAQ 
setenv NETCDF netcdf_root_gcc # Note please combine netCDF-C & Fortran Libraries (e.g. /usr/local/netcdf-4.7.0)
setenv IOAPI  ioapi_root_gcc  (e.g. /usr/local/ioapi-3.2)
setenv WRF_ARCH 34              # [1-75]  64 Bit Linux_x86 Compiler/Architecture options
```

*Note: WRF_ARCH environment variable is based on the following options:*

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

####  If you have never configured WRF before, here are some guidelines

   - choose the dmpar option with the appropriate compiler platform (34 for gcc case)
   - For more information refer to the [WRF User Guide](https://www2.mmm.ucar.edu/wrf/users/docs/user_guide_v4/v4.0/contents.html)

```
source config_cmaq.csh
```

### Step 6: Modify the bldit_cctm.csh 

Uncomment the option to build WRF-CMAQ twoway:      
      
```
#> Two-way WRF-CMAQ 
set build_twoway                      #> uncomment to build WRF-CMAQ twoway; 
```


### Run the bldit_cctm.csh script
```
./bldit_cctm.csh gcc |& tee bldit_cctm_twoway.log
```

### At this point, Users who have [Git](https://git-scm.com/) installed on their system should look for the following message at the end of their bldit_cctm_twoway.log: 

```
--->                  Executables successfully built                  <---

-rwxr-xr-x. 1 fsidi mod3dev 51139232 Jun  7 19:03 main/ndown.exe
-rwxr-xr-x. 1 fsidi mod3dev 51187088 Jun  7 19:03 main/real.exe
-rwxr-xr-x. 1 fsidi mod3dev 50445512 Jun  7 19:03 main/tc.exe
-rwxr-xr-x. 1 fsidi mod3dev 81349320 Jun  7 19:02 main/wrf.exe

==========================================================================

```

  If the User sees this, the WRF-CMAQ model has been successfully compiled and built and they may skip to Step 12. If not, the User should double check the library paths above and try again. If it still fails, please reach post on the [CMAS Forum](https://forum.cmascenter.org/c/wrf-cmaq).


### Step 7: Download WRF 4.3 and install it
   - Please register at the WRF User site https://www2.mmm.ucar.edu/wrf/users/download/get_source.html
   - obtain the WRF-Modeling System source code
   - download version 4.3 from https://github.com/wrf-model/WRF/releases/tag/v4.3
   - extract the tar.gz file
   
   ```
   cd /home/username/WRF-CMAQ/CMAQ_v5.3.2/scripts
   tar -xzvf WRF-4.3.tar.gz ./BLD_WRFv4.3_CCTM_v532_gcc
   ```
   
### Step 8: Move wrfcmaq_twoway_coupler and BLD_CCTM_v532_gcc into BLD_WRFv4.1.1_CCTM_v532_gcc

```
source ../../config_cmaq.csh
mv BLD_CCTM_v532_gcc BLD_WRFv4.3_CCTM_v532_gcc/cmaq
cd BLD_WRFv4.3_CCTM_v532_gcc
cp -rp $CMAQ_REPO/UTIL/wrfcmaq_twoway_coupler .
```

### Step 9: Set environmnetal variable and run the coupler script

   ```
    setenv wrf_path $CMAQ_HOME/CCTM/scripts/BLD_WRFv4.3_CCTM_v532_gcc
   ./wrfcmaq_twoway_coupler/assemble
   ```
   
  - This command will update all necessary files in WRF and CMAQ to create the WRF-CMAQ model. 
  - Verify that the path for the I/O API library is set correctly in the configure.wrf file and modify if needed.
    
 ```
    #### BEGIN for WRF-CMAQ twoway model
IOAPI   = /proj/ie/proj/CMAS/WRFv4.1.1-CMAQv5.3.2_rel_debug/LIBRARIES/openmpi_4.0.1_gcc_9.1.0/ioapi-3.2-20200820
LIOAPI  = Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
    #### END for WRF-CMAQ twoway model
 ```

### Step 10: Configure and Compile the WRF-CMAQ model

```
setenv WRF_CMAQ 1
./configure 
./compile em_real >& mylog
```

####  If you have never configured WRF before, here are some guidelines

   - choose the dmpar option with the appropriate compiler platform (34 for gcc case)
   - For more information refer to the [WRF User Guide](https://www2.mmm.ucar.edu/wrf/users/docs/user_guide_v4/v4.0/contents.html)


  - If compilation is done successfully, you can find main/wrf.exe file.

```
--->                  Executables successfully built                  <---

-rwxr-xr-x. 1 fsidi mod3dev 51139232 Jun  7 19:03 main/ndown.exe
-rwxr-xr-x. 1 fsidi mod3dev 51187088 Jun  7 19:03 main/real.exe
-rwxr-xr-x. 1 fsidi mod3dev 50445512 Jun  7 19:03 main/tc.exe
-rwxr-xr-x. 1 fsidi mod3dev 81349320 Jun  7 19:02 main/wrf.exe

==========================================================================

```
  - If not found, use vi or gedit to view the mylog file, and look for errors near the compilation step for wrf.exe

### Step 11: If you have to rebuild the model, but want to keep the configure.wrf file use:

```
./clean -a
```

### Step 12: Download the input data

[Link to CMAQv5.3.2_Benchmark_2Day_Input.tar.gz input data on Google Drive](https://drive.google.com/file/d/1fp--3dVvQHUyB_BodpU2aHBv5LjlC6E4/view?usp=sharing)

  - Use the gdrive command to download the dataset.
  - If this is the first time that you are using gdrive, or if you have an issue with your token, please read the following instructions
  - [Tips to download data from CMAS Data Warehouse](https://docs.google.com/document/d/1e7B94zFkbKygVWfrhGwEZL51jF4fGXGXZbvi6KzXYQ4)
  
  
  ```
  gdrive download 1fp--3dVvQHUyB_BodpU2aHBv5LjlC6E4
  ```
  
    
### Step 13: Run the WRF-CMAQ model

  - Use the run.twoway_model_411_532_nf_run_script.16pe.csh script and the CMAQv5.3.2 input benchmark dataset to run CMAQ-WRF with no feedback
  - It is configured to run on 16 processors and for 2 days of model simulation
  - Edit the script to specify the paths, modify the number of processors and batch queue commands
  - Verify that the OMIfile definition matches the latest release of CMAQv5.3.2
  
  Modify the following section to specify your local paths
  ```
set ROOT_PATH   = /proj/ie/proj/CMAS/WRF-CMAQ/openmpi_4.0.1_gcc_9.1.0_debug/
set WRF_DIR     = $ROOT_PATH/WRF-4.1.1  # WRF source code directory
set INPDIR      = /proj/ie/proj/CMAS/WRF-CMAQ/from_EPA/from_gdrive/CMAQv5.3.2_Benchmark_2Day_Input/2016_12SE1
set OMIpath     = $WRF_DIR/cmaq                              # path optics related data files
set OUTPUT_ROOT = $ROOT_PATH/WRF-4.1.1  # output root directory
set NMLpath     = $WRF_DIR/cmaq                              # path with *.nml file mechanism dependent
set NMLpath2    = $WRF_DIR/cmaq                              # path with Species_Table_TR_0.nml file
set EMISSCTRL   = $WRF_DIR/cmaq                              # path of Emissions Control File
 ```
    
  - Verify the following settings
    ```
    set NPROCS =    16
    set OMIfile    = OMI_1979_to_2019.dat
    ```
    
  - Submit the job using the batch queueing system
    ```
    sbatch run.twoway_model_411_532_nf_run_script.16pe.csh
    ```

### Step 14: Verify that the run was successful
   - look for the output directory
   
   ```
   cd output_12km_nf_rrtmg_20_5_1_v411532_debug
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./2016183/rsl.out.0000
   ```
   |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|

### Step 15: Compare results to the WRF-CMAQ 2 day benchmark results
 
   - Download WRF-CMAQ bencmark output data from the google drive folder

     https://drive.google.com/drive/u/1/folders/1poigGFlABCfepaIjDw-6JOyznJ6xz1ck

   - Compare CCTM_ACONC_v411532_20160702.nc files to your benchmark results

   - Both debug and optimized benchmark outputs are provided for your comparisons.

   - Note, the CMAQv5.3.2 output results will not directly compare to the no feedback (nf) WRF-CMAQ output, as different meterology and timesteps were used.  To do a comparison between CMAQv5.3.2 and WRF-CMAQ, use WRF-CMAQ to output the MCIP meteorology files, and then use those MCIP inputs with the CMAQv5.3.2 ICON and BCON inputs.
