## CMAQ-ISAM Benchmark Tutorial ## 

### Procedure to build and run the CMAQ-ISAM model using gnu compiler: ###

### Step 1: Read the User Guide Chapter on Integrated Source Apportionment Method.
https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/CMAQ_UG_ch11_ISAM.md

Note: This benchmark is intended to demonstrate how to build and run CMAQ-ISAM with the provided input files:
```
isam_control.txt
GRIDMASK_STATES_12SE1.nc
```

The following instructions will also require the user to edit the emissions control namelist file in the BLD directory.

```
EmissCtrl_cb6r3_ae7_aq.nml
```


### Step 2: choose your compiler, and load it using the module command if it is available on your system

```
module avail
```

```
module load openmpi_4.0.1/gcc_9.1.0 
```

### Step 3: Install I/O API (note, this assumes you have already installed netCDF C and Fortran Libraries)

I/O APIv3.2 supports up to MXFILE3=64 open files, each with up to MXVARS3=2048. ISAM applications configured to calculate source attribution of a large number of sources may exceed this upper limit of model variables, leading to a model crash. To avoid this issue, users may use I/O API version 3.2 "large" that increases MXFILE3 to 512 and MXVARS3 to 16384. Instructions to build this version are found in Chapter 3. Note, using this ioapi-large version is <b>not required</b> for the CMAQ-ISAM Benchmark Case. If a user needs to use larger setting for MXFILE3 and MXVAR3 to support their application, note that the memory requirements will be increased. This version is available as a zip file from the following address:

https://www.cmascenter.org/ioapi/download/ioapi-3.2-large-2020220.tar.gz

### Step 4: Install CMAQ

```
git clone -b master https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

Build and run in a user-specified directory outside of the repository

In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

Edit bldit_project.csh, to modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:

```
set CMAQ_HOME = /home/username/CMAQ_v5.3.2
```

Now execute the script.

./bldit_project.csh

Change directories to the CMAQ_HOME directory

### Step 5. Edit the config_cmaq.csh to specify the paths of the ioapi and netCDF libraries

### Step 6: Modify the bldit_cctm.csh 


Comment out the following option to compile CCTM with ISAM:
```
#> Integrated Source Apportionment Method (ISAM)
set ISAM_CCTM                         #> uncomment to compile CCTM with ISAM activated

When this is set, the executable that is built will be tagged with v532_ISAM
#> Working directory and Version IDs
```
 if ( $?ISAM_CCTM ) then
     set VRSN  = v532_ISAM                      #> model configuration ID
    else if ( $?DDM3D_CCTM ) then
     set VRSN = v532_DDM3D
    else
     set VRSN = v532               #> model configuration ID
 endif
```

### Step 5: Run the bldit_cctm.csh script
```
./bldit_cctm.csh gcc |& tee bldit_cctm_isam.log
```

### Step 6: Edit the Emission Control Namelist to recognize the CMAQ_REGIONS file 
Uncomment the line with ISAM_REGIONS as the File_Label
```
&RegionsRegistry
 RGN_NML  =
 !          | Region Label   | File_Label  | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'        ,'N/A',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
               'ALL'         ,'ISAM_REGIONS','ALL',
```
      
### Step 6: Download the benchmark input data

[Link to CMAQv5.3.2_Benchmark_2Day_Input.tar.gz input data on the following Google Drive Folder](https://drive.google.com/drive/u/1/folders/1jAKw1EeEzxLSsmalMplNwYtUv08pwUYk)

  - You can see a list of the files in the Benchmark dataset: by clicking on CMAQv5.3.2_Benchmark_2Day_Input.tar.gz.list
  - Use the gdrive command to download the dataset.
  - If this is the first time that you are using gdrive, or if you have an issue with your token, please read the following instructions
  - [Tips to download data from CMAS Data Warehouse](https://docs.google.com/document/d/1e7B94zFkbKygVWfrhGwEZL51jF4fGXGXZbvi6KzXYQ4)
  
  
  ```
  gdrive download 1ex6Wr4dX6a0fgaDfhO0VEJNaCKqOflI5
  ```
  
    
### Step 6: Run the CMAQ-ISAM model

    
  - Verify the following settings
    ```
    set NPROCS =    16
    set OMIfile    = OMI_1979_to_2019.dat
    ```

  - Change the code version to use the tag v532_ISAM
```
#> Set General Parameters for Configuring the Simulation
 set VRSN      = v532_ISAM         #> Code Version
```
    
  - Submit the job using the batch queueing system
    ```
    sbatch run_cctm_Bench_2016_12SE1.csh
    ```

### Step 7: Verify that the run was successful
   - look for the output directory
   
   ```
   cd ../data/output_CCTM_v532_ISAM_gcc_Bench_2016_12SE1
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./LOGS/CTM_LOG_000.v532_ISAM_gcc_Bench_2016_12SE1_20160701
   ```
   |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|

### Step 8: Compare output with the 2 day benchmark output provided
    Note, the following ISAM output files are generated in addition to the standard CMAQ output files.
    CCTM_SA_CONC_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
    CCTM_SA_WETDEP_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
    CCTM_SA_DRYDEP_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
    CCTM_SA_ACONC_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
    CCTM_SA_CGRID_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc

