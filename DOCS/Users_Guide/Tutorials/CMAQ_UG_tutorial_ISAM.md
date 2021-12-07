## CMAQ-ISAM Benchmark Tutorial ## 

### Procedure to build and run the CMAQ-ISAM model using gnu compiler: ###

### Step 1: Download and run the CMAQv5.3.3 benchmark case (without ISAM) to confirm that your model run is consistent with the provided benchmark output.
- [CMAQ Benchmark Tutorial](CMAQ_UG_tutorial_benchmark.md)

If you encounter any errors, try running the model in debug mode and refer to the CMAS User Forum to determine if any issues have been reported.

https://forum.cmascenter.org/

### Step 2: Read the User Guide Chapter on Integrated Source Apportionment Method.
- [CMAQ User's Guide Chapter on ISAM](../CMAQ_UG_ch11_ISAM.md)

Note: This benchmark is intended to demonstrate how to build and run CMAQ-ISAM with the provided input files:

The following isam control file is provided in the CCTM/scripts directory when you obtain the CMAQv5.3.3 code from github (step 5 below):

```
isam_control.txt
```

The following gridmask file is provided with the benchmark inputs in the CMAQv5.3.2_Benchmark_2Day_Input/2016_12SE1 directory (see step 10 below)

```
GRIDMASK_STATES_12SE1.nc
```

The instructions require the user to edit the emissions control namelist file in the BLD directory (see step 9 below).

```
EmissCtrl_cb6r3_ae7_aq.nml
```


### Step 3 (optional): choose your compiler, and load it using the module command if it is available on your system

```
module avail
```

```
module load openmpi_4.0.1/gcc_9.1.0 
```

### Step 4 (optional): Install I/O API (note, this assumes you have already installed netCDF C and Fortran Libraries)

I/O APIv3.2 supports up to MXFILE3=256 open files, each with up to MXVARS3=2048. ISAM applications configured to calculate source attribution of a large number of sources may exceed this upper limit of model variables, leading to a model crash. To avoid this issue, users may use I/O API version 3.2 "large" that increases MXFILE3 to 512 and MXVARS3 to 16384. Instructions to build this version are found in Chapter 3. Note, using this ioapi-large version is <b>NOT REQUIRED</b> for the CMAQ-ISAM Benchmark Case. If a user needs to use these larger setting for MXFILE3 and MXVAR3 to support their application, the memory requirements will be increased. If needed, this version is available as a zip file from the following address:

https://www.cmascenter.org/ioapi/download/ioapi-3.2-large-20200828.tar.gz

Otherwise, use the I/O API version available here:
https://www.cmascenter.org/ioapi/download/ioapi-3.2-20200828.tar.gz

### Step 5: Install CMAQ with ISAM

```
git clone -b main https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

Build and run in a user-specified directory outside of the repository

In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

Edit bldit_project.csh, to modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:

```
set CMAQ_HOME = [your_install_path]/CMAQ_v5.3.3
```

Now execute the script.

```
./bldit_project.csh
```

Change directories to the CMAQ_HOME directory

```
cd [your_install_path]/CMAQ_v5.3.3
```


### Step 6. Edit the config_cmaq.csh to specify the paths of the ioapi and netCDF libraries

### Step 7: Modify the bldit_cctm.csh 

Change directory to CCTM/scripts

```
cd CCTM/scripts
```

Comment out the following option to compile CCTM with ISAM:

```
#> Integrated Source Apportionment Method (ISAM)
set ISAM_CCTM                         #> uncomment to compile CCTM with ISAM activated
```

### Step 8: Run the bldit_cctm.csh script
```
./bldit_cctm.csh gcc |& tee bldit_cctm_isam.log
```

### Step 9: Edit the Emission Control Namelist to recognize the CMAQ_REGIONS file 

Change directories to the build directory
```
cd BLD_CCTM_v532_ISAM_gcc
```

edit the emissions namelist file

```
gedit EmissCtrl_cb6r3_ae7_aq.nml 
```

Uncomment the line that contains ISAM_REGIONS as the File Label

```
&RegionsRegistry
 RGN_NML  =
 !          | Region Label   | File_Label  | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'        ,'N/A',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
               'ALL'         ,'ISAM_REGIONS','ALL',
```
  
### Step 10: Download the benchmark input data

[Link to CMAQv5.3.2_Benchmark_2Day_Input.tar.gz input data on the following Google Drive Folder](https://drive.google.com/drive/u/1/folders/1jAKw1EeEzxLSsmalMplNwYtUv08pwUYk)

  - You can see a list of the files in the Benchmark dataset: by clicking on CMAQv5.3.2_Benchmark_2Day_Input.tar.gz.list
  - Use the gdrive command to download the dataset.
  - If this is the first time that you are using gdrive, or if you have an issue with your token, please read the following instructions
  - [Tips to download data from CMAS Data Warehouse](https://docs.google.com/document/d/1e7B94zFkbKygVWfrhGwEZL51jF4fGXGXZbvi6KzXYQ4)
  
  
  ```
  gdrive download 1ex6Wr4dX6a0fgaDfhO0VEJNaCKqOflI5
  ```
  
    
### Step 11: Edit the CMAQ-ISAM runscript

```
gedit run_cctm_Bench_2016_12SE1.csh
```

Set General Parameters for Configuring the Simulation

```
set VRSN = v532_ISAM
```


Turn on ISAM and uncomment ISAM regions file

```
setenv CTM_ISAM Y
setenv ISAM_REGIONS $INPDIR/GRIDMASK_STATES_12SE1.nc
```
   
Run or Submit the script to the batch queueing system

```
./run_cctm_Bench_2016_12SE1.csh
```

OR (If using SLRUM)

```
sbatch run_cctm_Bench_2016_12SE1.csh
```

### Step 12: Verify that the run was successful
   - look for the output directory
   
   ```
   cd ../../data/output_CCTM_v532_ISAM_gcc_Bench_2016_12SE1
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./LOGS/CTM_LOG_000.v532_ISAM_gcc_Bench_2016_12SE1_20160701
   ```
   |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|

### Step 13: Compare output with the 2 day benchmark outputs provided on the google drive


    https://drive.google.com/drive/u/1/folders/1jAKw1EeEzxLSsmalMplNwYtUv08pwUYk

    Note, the following ISAM output files are generated in addition to the standard CMAQ output files.

```
    CCTM_SA_CONC_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
    CCTM_SA_WETDEP_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
    CCTM_SA_DRYDEP_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
    CCTM_SA_ACONC_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
    CCTM_SA_CGRID_v532_ISAM_gcc_Bench_2016_12SE1_20160701.nc
```

