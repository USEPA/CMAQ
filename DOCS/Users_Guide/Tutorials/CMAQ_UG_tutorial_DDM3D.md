## CMAQ-DDM3D Benchmark Tutorial ## 

### Procedure to build and run the CMAQ-DDM3D model using cb6r5 mechanism and m3dry dry deposition scheme using gnu compiler: ###

### Step 1: Download and run the CMAQv5.5 cb6r5 benchmark case (without DDM3D) to confirm that your model run is consistent with the provided benchmark output.
- [CMAQ Benchmark Tutorial](CMAQ_UG_tutorial_benchmark.md)

If you encounter any errors, try running the model in debug mode and refer to the CMAS User Forum to determine if any issues have been reported.

https://forum.cmascenter.org/

### Step 2: Read the User Guide Chapter on the Decoupled Direct Method in Three Dimensions
- [CMAQ User's Guide Chapter on DDM-3D](../CMAQ_UG_ch10_HDDM-3D.md)

Note: This benchmark is intended to demonstrate how to build and run CMAQ-DDM-3D with the provided input files:

The following  control file is provided in the CCTM/scripts directory when you obtain the CMAQv5.5 code from github (step 5 below):

```
sensinput.2018_12NE3.dat
```

The above file contains the following sensitivity definition block:

```
PPA
 EMIS
  PT_EGU
 SPECIES
  SO2
 REGION
  PA

GNJ
 EMIS
  GRIDDED_EMIS
 SPECIES
  NO2, NO
 REGION
  NJ

END
```

The following gridmask file is provided with the [5.5 benchmark inputs](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html#v5_5/) tar file CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz. (See step 10 below.)

```
GRIDMASK_STATES_12NE3.nc
```

The instructions require the user to edit the DESID emissions control namelist file and the DESID chemical control namelist file in the BLD directory. If you want to use emission scaling (independently from ISAM or DDM3D) you will also need to edit these files.

```
CMAQ_Control_DESID.nml
CMAQ_Control_DESID_${MECH}.nml
```


### Step 3 (optional): choose your compiler, and load it using the module command if it is available on your system

```
module avail
```

```
module load openmpi_4.0.1/gcc_9.1.0 
```

### Step 4 (optional): Install I/O API (note, this assumes you have already installed netCDF C and Fortran Libraries)

I/O APIv3.2 supports up to MXFILE3=256 open files, each with up to MXVARS3=2048. DDM-3D applications configured to calculate sensitivities to a large number of parameters may exceed this upper limit of model variables, leading to a model crash. To avoid this issue, users may use I/O API version 3.2 "large" that increases MXFILE3 to 512 and MXVARS3 to 16384. Instructions to build this version are found in Chapter 3. Note, using this ioapi-large version is <b>NOT REQUIRED</b> for the CMAQ-DDM Benchmark Case. If a user needs to use these larger setting for MXFILE3 and MXVAR3 to support their application, the memory requirements will be increased. If needed, this version is available as a zip file from the following address:

https://www.cmascenter.org/ioapi/download/ioapi-3.2-large-20200828.tar.gz

Otherwise, use the I/O API version available here:
https://www.cmascenter.org/ioapi/download/ioapi-3.2-20200828.tar.gz

### Step 5: Install CMAQ with DDM-3D

```
git clone -b main https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

Build and run in a user-specified directory outside of the repository

In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

Edit bldit_project.csh, to modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:

```
set CMAQ_HOME = [your_install_path]/CMAQ_v5.5
```

Now execute the script.

```
./bldit_project.csh
```

Change directories to the CMAQ_HOME directory

```
cd [your_install_path]/CMAQ_v5.5
```


### Step 6. Edit the config_cmaq.csh to specify the paths of the ioapi and netCDF libraries

### Step 7: Copy the bldit_cctm.csh script to a new bldit_cctm_cb6r5_m3dry_ddm.csh bldit script, and make the following edits:

Change directory to CCTM/scripts

```
cd CCTM/scripts
cp bldit_cctm.csh bldit_cctm_cb6r5_m3dry_ddm.csh
```

Modify the following option to compile CCTM with DDM3D:

```
set DDM3D_CCTM                        #> uncomment to compile CCTM with DD3D activated
```

### Step 8: Run the bldit_cctm.csh script
```
./bldit_cctm_cb6r5_m3dry_ddm.csh gcc |& tee bldit_cctm_cb6r5_m3dry_ddm.log
```

### Step 9: Edit the Emission Control Namelist to recognize the CMAQ_REGIONS file 

Change directories to the build directory
```
cd BLD_CCTM_v55_DDM3D_gcc_cb6r5_ae7_aq_m3dry
```

edit the Control_DESID namelist file

```
gedit CMAQ_Control_DESID.nml
```

Add the Regions to the &Desid_RegionDef section of the control DESID namelist.

```
&Desid_RegionDef
 Desid_Reg_nml  =
 !            Region Label   | File_Label  | Variable on File
               'EVERYWHERE'  ,'N/A'        ,'N/A',
                'PA'        ,'CMAQ_MASKS'        , 'PA',
                'NJ'        ,'CMAQ_MASKS'        , 'NJ',
/
```

### Step 10: Example of emissions scaling (Reduce the PT_EGU emissions in PA by 25%) (Optional step)

edit the DESID chemical control namelist file, note please specify the mechanism or define the MECH environment variable.


```
#gedit CMAQ_Control_DESID_${MECH}.nml
gedit CMAQ_Control_DESID_cb6r5_ae7_aq.nml
```

Add the following line at the bottom of the the namelist file (before the /)

```
   ! PT_EGU Emissions Scaling reduce PT_EGU emissions in Pennsylvania by 25%. Note, to reduce the emissions by 25% we use DESID to multiply what had been 100% emissions by .75, so that the resulting emissions is reduced by 25%.
   'PA'  , 'PT_EGU'      ,'All'    ,'All'         ,'All' ,.75    ,'UNIT','o',

```


### Step 11: Install the CMAQ-DDM-3D reference input and output benchmark data

Download the CMAQ two day reference input and output data from the [CMAS Center Data Warehouse Amazon Web Services S3 Bucket](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html#v5_5/): CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz	and output_CCTM_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz. The CMAQ benchmark test case is a two day simulation for July 1-2 2018 on a 100 column x 105 row x 35 layer 12-km resolution domain over the northeast U.S that uses the cb6r5_ae7_aq mechanism and the m3dry dry deposition scheme.  

Download and copy the data to `$CMAQ_DATA`. Navigate to the `$CMAQ_DATA` directory, unzip and untar the two day benchmark input and output files:

```
cd $CMAQ_DATA
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz
tar xvzf CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz
mkdir ref_output
cd ref_output
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz
tar xvzf output_CCTM_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz
```

    
### Step 12: Review the CMAQ-DDM3D runscript

```
gedit run_cctm_Bench_2018_12NE3_cb6r5_m3dry_ddm.csh
```

Verify General Parameters for Configuring the Simulation

```
 set VRSN      = v55_DDM3D
```


Verify that script turns on DDM3D, uncomments SEN_INPUT file and sets the number of sensitivity parameter to match the SEN_INPUT

```
 setenv CTM_DDM3D Y  
 setenv SEN_INPUT ${WORKDIR}/sensinput.2018_12NE3.dat
 set NPMAX    = 2      # Number of sensitivity parameters defined in SEN_INPUT
```
   
Run or Submit the script to the batch queueing system

```
./run_cctm_Bench_2018_12NE3_cb6r5_m3dry_ddm.csh
```

OR (If using SLRUM)

```
sbatch run_cctm_Bench_2018_12NE3_cb6r5_m3dry_ddm.csh
```

### Step 13: Verify that the run was successful
   - look for the output directory
   
   ```
   cd ../../data/2018_12NE3_BENCH/output_CCTM_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./LOGS/CTM_LOG_000.v55_DDM3D_gcc_Bench_2018_12NE3_cracmm2_20180702
   ```
   |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|

### Step 14: Compare output with the 2 day benchmark outputs provided on the CMAS Center AWS Open Data Program

Note, the following DDM-3D output files are generated in addition to the standard CMAQ output files.

```
 CCTM_SENWDEP_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180701.nc
 CCTM_SENDDEP_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180701.nc
 CCTM_SENGRID_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180701.nc
```

### Step 15: Compare sensitivities

First order sensitivities should not be larger than bulk, second order should not be larger than first order.
