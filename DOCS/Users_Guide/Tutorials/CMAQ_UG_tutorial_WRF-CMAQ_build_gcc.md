## WRF-CMAQ Tutorial ## 

### Procedure to build and run the WRF-CMAQ model using gnu compiler: ###

### Step 1: choose your compiler, and load it using the module command if it is available on your system

```
module avail
```

```
module load openmpi_4.0.1/gcc_9.1.0 
```

### Step 2a:  Download WRF 4.1.1 and install it
   - Please register at the WRF User site https://www2.mmm.ucar.edu/wrf/users/download/get_source.html
   - obtain the WRF-Modeling System source code
   
   - Method 1: clone from github
   
  ```
  git clone --branch v4.1.1 https://github.com/wrf-model/WRF.git
  ```
   
   - Method 2: Downloading an archived version from github
   - download version 4.1.1 from https://github.com/wrf-model/WRF/releases/tag/v4.1.1
   - extract the tar.gz file
   
   ```
   tar -xzvf WRF-4.1.1.tar.gz
   ```

### Step 2b: Download and install netCDF Fortran and C libraries

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
   
   For debug option, set 
   ```
   setenv FCFLAGS '-m64 -g3 -O0 -fno-inline'
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

   -  Configure WRF by typing ./configure (this creates a configure.wrf file)
   
      ```
      ./configure |& tee ./configure.log
      ```
   
   -  Note: to configure WRF for debug mode add the '-d' option
   
      ```
      ./configure -d |& tee ./configure.log
      ```

####  If you have never done WRF configure before, here are some guidelines

   - choose the dmpar option with the appropriate compiler platform
   - in the compile for nesting section, choose the default value
      

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
BASEDIR = ${INSTALL}/ioapi-3.2
```
     
     
 #### set the BASEDIR and BIN environment variables:
     
     
     setenv BASEDIR /proj/ie/proj/CMAS/WRFv4.1.1-CMAQv5.3.2_rel_debug/LIBRARIES/openmpi_4.0.1_gcc_9.1.0
     setenv BIN  Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0
     

 #### Make the installation directory

    
     mkdir $BASEDIR/$BIN
      
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
 ls -lrt /[your_install_path]/LIBRARIES/ioapi-3.2/Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0/libioapi.a
 ```
 
 ### Note: If you get a shared object problem when trying to run m3tools such as the following:
 ```
 [lizadams@dogwood-login1 Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0]$ ./juldate
./juldate: error while loading shared libraries: libimf.so: cannot open shared object file: No such file or directory
```

### Edit your .cshrc to add the path to the library by setting the LD_LIBRARY_PATH environment variable

```
#for gcc WRF-CMAQ build
setenv NCF_COMBO /[your_install_path]/openmpi_4.0.1_gcc_9.1.0/LIBRARIES/netcdf_combined/
setenv LD_LIBRARY_PATH ${NCF_COMBO}/lib:${LD_LIBRARY_PATH}
```

### Make sure that there is no other definition or setting of LD_LIBRARY_PATH further down in your .cshrc file that may be overwriting your setting.

### Make sure you log out and log back, or run csh in to activate the LD_LIBRARY_PATH setting.
      
#### Set the IOAPI environment variable to the path where it has been installed

```
setenv IOAPI /[your_install_path]/openmpi_4.0.1_gcc_9.1.0/LIBRARIES/ioapi-3.2
```
    

### Step 4: Install CMAQ
     - follow these instructions to download the code, then use the modifications in Step 5
     
https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md

### Step 5: Modify the bldit_cctm.csh to uncomment the following options:

     
      ```
      set MakeFileOnly                      #> uncomment to build a Makefile, but do not compile;
      ```
      
      
      
```
      #> Two-way WRF-CMAQ 
set build_twoway                      #> uncomment to build WRF-CMAQ twoway; 
```

### Run the bldit_cctm.csh script
```
./bldit_cctm.csh gcc |& tee bldit_cctm_twoway.log
```
      

#### After running the blidit script, copy BLD_CCTM_V531_gcc into WRFV411/cmaq directory.

example {depends on the location of your WRF-4.1.1 directory}:

```
cp -rp BLD_CCTM_v531_gcc ../../../WRF-4.1.1/cmaq
```

### Step 6: Download WRF4.1.1_CMAQ5.3.2_Coupled_Model_20191220.tar.gz and unzip it. 
A twoway directory is formed and move it inside WRFV411 as well.

- The WRFv4.1.1-CMAQv5.3.2 model is released as a tarball 

[Link to WRFv4.1.1-CMAQv5.3.2 Model on Google Drive](https://drive.google.com/open?id=10wFNch1MkI49ZjD2XD6wK2xzDWOav2zY)

The WRF-CMAQ model is also available as a tarball (twoway.tar.gz) from the the US EPA annoymous ftp server:

[ftp://newftp.epa.gov/exposure/CMAQ/V5_3/WRF-CMAQ_Coupled_Mode](https://bit.ly/3cuoDyi)

The following commands must be adjusted for the paths on your system.
```
cd WRF4.1.1
tar -xzvf ../../WRF4.1.1_CMAQ5.3.2_Coupled_Model_20191220.tar.gz
```

### Step 7: Go into directory WRFV411

   ```
   cd /proj/ie/proj/CMAS/WRF-CMAQ/openmpi_4.0.1_gcc_9.1.0_debug/WRF-4.1.1
   ```
### Step 8: run the following command
   ```
   ./twoway/assemble
   ```
   
  - This command will update all necessary files in WRF and CMAQ to create the WRF-CMAQ model. 
  - You can find the original files inside twoway/misc/orig directory.
  - Verify that the path for the I/O API library is set correctly in the configure.wrf file and modify if needed.
    
 ```
    #### BEGIN for WRF-CMAQ twoway model
IOAPI   = /proj/ie/proj/CMAS/WRF-CMAQ/openmpi_4.0.1_gcc_9.1.0/ioapi-3.2
LIOAPI  = Linux2_x86_64gfort
    #### END for WRF-CMAQ twoway model
 ```

 - I modified LIOAPI to Linux2_x86_64gfort_openmpi_4.0.1_gcc_9.1.0


### Step 9: Edit the configure.wrf to link with the openmp library

add -fopenmp to the the definition for LIB_EXTERNAL
```
LIB_EXTERNAL    = -L$(WRF_SRC_ROOT_DIR)/external/io_netcdf -lwrfio_nf -L/proj/ie/proj/CMAS/WRFv4.1.1-CMAQv5.3.2_debug/openmpi_4.0.1_gcc_9.1.0/LIBRARIES/netcdf_combined/lib -lnetcdff -lnetcdf -fopenmp
```


### Step 10: Compile the WRF-CMAQ model

```
./compile em_real >& mylog
```

  - If compilation is done successfully, you can find main/wrf.exe file.
  
### Step 10: Download the input data

[Link to CMAQv5.3.2_Benchmark_2Day_Input.tar.gz input data on Google Drive](https://drive.google.com/file/d/1fp--3dVvQHUyB_BodpU2aHBv5LjlC6E4/view?usp=sharing)

  - Use the gdrive command to download the dataset.
  - If this is the first time that you are using gdrive, or if you have an issue with your token, please read the following instructions
  - [Tips to download data from CMAS Data Warehouse](https://docs.google.com/document/d/1e7B94zFkbKygVWfrhGwEZL51jF4fGXGXZbvi6KzXYQ4)
  
  
  ```
  gdrive download 1fp--3dVvQHUyB_BodpU2aHBv5LjlC6E4
  ```
  
    
### Step 11: Run the WRF-CMAQ model

  - Use the twoway_model_411_531_run_script_nf script and the CMAQv5.3.2 input benchmark dataset to run CMAQ-WRF with no feedback
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
    sbatch twoway_model_411_531_run_script_nf
    ```

### Step 12: Verify that the run was successful
   - look for the output directory
   
   ```
   cd output_12km_nf_rrtmg_20_5_1_v411531_debug
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./2016183/rsl.out.0000
   ```
   |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|
