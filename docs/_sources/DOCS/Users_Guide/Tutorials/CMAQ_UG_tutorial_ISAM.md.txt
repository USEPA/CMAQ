## CMAQ-ISAM Benchmark Tutorial ## 

Procedure to build and run the CMAQ-ISAM model using gnu compiler for the cb6r5_ae7_aq mechanism with the m3dry dry deposition scheme:

### Step 1: Download and run the CMAQv5.5 benchmark case (without ISAM) to confirm that your model run is consistent with the provided benchmark output.
- [CMAQ Benchmark Tutorial](CMAQ_UG_tutorial_benchmark.md)

If you encounter any errors, try running the model in debug mode and refer to the CMAS User Forum to determine if any issues have been reported.

https://forum.cmascenter.org/

### Step 2: Read the User Guide Chapter on Integrated Source Apportionment Method.
- [CMAQ User Guide Chapter on ISAM](../CMAQ_UG_ch11_ISAM.md)

Note: This benchmark is intended to demonstrate how to build and run CMAQ-ISAM with the provided input files

The following isam control file is provided in the CCTM/scripts directory when you obtain the CMAQv5.5 code from github (step 5 below):

```
cat isam_control.2018_12NE3.txt
```

This file contains the following tag classes

```
TAG NAME        |EGU
REGION(S)       |EVERYWHERE
EMIS STREAM(S)  |PT_EGU

TAG NAME        |BIO
REGION(S)       |NY
EMIS STREAM(S)  |BIOG
```

The following gridmask file is provided with the benchmark inputs in the 2018_12NE3_BENCH/2018_12NE3 directory (see step 11 below)

```
GRIDMASK_STATES_12NE3.nc
```

Note, all states are listed in the variable list in the header of the file, but the data only contains valid entries for the states in the 12NE3 domain. 

The instructions require the user to edit the DESID emissions control namelist file and the DESID chemical control namelist file in the BLD directory. If you want to use emission scaling (independently from ISAM or DDM3D) you will also need to edit these files. (see step 10 below).

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

### Step 7: Modify the bldit_cctm.csh to activate ISAM

Change directory to CCTM/scripts

```
cd CCTM/scripts
cp bldit_cctm.csh bldit_cctm_isam.csh
```

Uncomment the following option to compile CCTM with ISAM (remove the # before set ISAM_CCTM):

```
#> Integrated Source Apportionment Method (ISAM)
set ISAM_CCTM                         #> uncomment to compile CCTM with ISAM activated
```
### Step 8: Modify the bldit_cctm.csh to specify the cb6r5_ae7_aq mechanism and the m3dry dry deposition scheme and update the BLD directory name.

```
set DepMod    = m3dry                 #> dry deposition scheme (m3dry or stage)
setenv Mechanism cb6r5_ae7_aq              #> chemical mechanism (see $CMAQ_MODEL/CCTM/src/MECHS) 
```

Verify that the bldit_cctm_isam.csh script contains the following lines: (the mechanism and the dry deposition scheme have been added to the BLD directory name):

#> Set and create the "BLD" directory for checking out and compiling 
#> source code. Move current directory to that build directory.

```
 if ( $?Debug_CCTM ) then
    set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_${Mechanism}_${DepMod}_debug
 else
    set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_${Mechanism}_${DepMod}
 endif
```

### Step 9: Run the bldit_cctm_isam.csh script

```
./bldit_cctm_isam.csh gcc |& tee bldit_cctm_isam.log
```

### Step 10: Edit the Emission Control Namelist to recognize the CMAQ_REGIONS file 

Change directories to the build directory
```
cd BLD_CCTM_v55_ISAM_gcc_cb6r5_ae7_aq_m3dry
```

edit the DESID emissions namelist file

```
gedit CMAQ_Control_DESID.nml
```

Uncomment the line that contains ISAM_REGIONS as the File Label

```
&Desid_RegionDef
 Desid_Reg_nml  =
 !            Region Label   | File_Label  | Variable on File
               'EVERYWHERE'  ,'N/A'        ,'N/A',
 !              'NY'          ,'CMAQ_MASKS', 'NY',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
               'ALL'         ,'ISAM_REGIONS','ALL',
/
```


### Step 11: Example of emissions scaling (Reduce the PT_EGU emissions in NY by 25%) (Optional step, described here, but not used)

edit the DESID chemical control namelist file, note please specify the mechanism or define the MECH environment variable.

```
gedit CMAQ_Control_DESID_${MECH}.nml
```

Add the following line at the bottom of the the namelist file (before the /)

```
   ! PT_EGU Emissions Scaling reduce PT_EGU emissions in NY by 25%. Note, to reduce the emissions by 25% we use DESID to multiply what had been 100% emissions by .75, so that the resulting emissions is reduced by 25%.
   'NY'  , 'PT_EGU'      ,'All'    ,'All'         ,'All' ,.75    ,'UNIT','o',

```

### Step 12: Install the CMAQ-ISAM reference input and output benchmark data

Download the CMAQ two day reference input and output data from the  [CMAS Center Data Warehouse Amazon Web Services S3 Bucket](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html#v5_5/): CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz and output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cracmm2_stage.tar.gz.

Download and copy the data to `$CMAQ_DATA`. Navigate to the `$CMAQ_DATA` directory, unzip and untar the two day benchmark input and output files:

```
cd $CMAQ_DATA
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz
tar xvzf CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz
mkdir ref_output
cd ref_output
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz
tar xzvf output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz
```

The input files for the CMAQv5.4 ISAM benchmark case are the same as the benchmark inputs for the base model. Output source apportionment files associated with the sample isam_control.txt provided in this release package are included in the benchmark outputs for the base model.
    
### Step 13: Edit the CMAQ-ISAM runscript

Note: there is an example of the run script on the AWS S3 bucket.

```
cd CMAQ_v5.5/CCTM/scripts
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/CCTM/scripts/run_cctm_Bench_2018_12NE3_cb6r5_m3dry_ISAM.csh
cat run_cctm_Bench_2018_12NE3_cb6r5_m3dry_ISAM.csh
```

Verify the following settings in the run script for this ISAM benchmark.

Verify the General Parameters for Configuring the Simulation

```
 set VRSN = v55_ISAM
 set PROC      = mpi               #> serial or mpi
 set MECH      = cb6r5_ae7_aq      #> Mechanism ID
 set APPL      = Bench_2018_12NE3_${MECH}_m3dry  #> Application Name (e.g. Gridname)
```

Verify the Build directory to include the dry deposition mechanism in the name

```
#> Set the build directory (this is where the CMAQ executable
#> is located by default).
 set BLD       = ${CMAQ_HOME}/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_${MECH}_m3dry
```

Verify the input data directory

```
#> Set Working, Input, and Output Directories
 setenv WORKDIR ${CMAQ_HOME}/CCTM/scripts          #> Working Directory. Where the runscript is.
 setenv OUTDIR  ${CMAQ_DATA}/output_CCTM_${RUNID}  #> Output Directory
 setenv INPDIR  ${CMAQ_DATA}/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input/2018_12NE3            #> Input Directory
```

Verify the start and end dates to match the input data for this benchmark.

```
#> Set Start and End Days for looping
 setenv NEW_START TRUE             #> Set to FALSE for model restart
 set START_DATE = "2018-07-01"     #> beginning date (July 1, 2016)
 set END_DATE   = "2018-07-02"     #> ending date    (July 2, 2016)
```


Verify that ISAM is turned on and that the SA_IOLIST file and ISAM regions file definitions are uncommented.

```
setenv CTM_ISAM Y
setenv SA_IOLIST ${WORKDIR}/isam_control.2018_12NE3.txt
setenv ISAM_REGIONS $INPDIR/GRIDMASK_STATES_12NE3.nc
```

   
Run or Submit the script to the batch queueing system

```
./run_cctm_Bench_2018_12NE3_cb6r5_m3dry_ISAM.csh
```

OR (If using SLRUM) edit the #SBATCH commands at the top of the script for your machine, then run using

```
sbatch run_cctm_Bench_2018_12NE3_cb6r5_m3dry_ISAM.csh
```

### Step 14: Verify that the run was successful
   - look for the output directory
   
   ```
   cd ../../data/output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./LOGS/CTM_LOG_000.v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180702
   ```
   |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|

### Step 15: Compare output with the 2 day benchmark outputs provided on the google drive

The following ISAM output files are generated in addition to the standard CMAQ output files. Note, the ACONC files created for the  benchmark case without ISAM and this run will not be comparible if emission scaling is used (Step 11 - optional), but if emission scaling was not used, the files should be identical.

```
CCTM_SA_CONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180702.nc
CCTM_SA_WETDEP_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180702.nc
CCTM_SA_DRYDEP_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180702.nc
CCTM_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180702.nc
CCTM_SA_CGRID_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180702.nc
```

### Step 16: Compare the tagged species in `CCTM_SA_ACONC` output file to the species in `CCTM_ACONC` output file

```
ncdump -h CCTM_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_20180701.nc | grep SO2_
```


The following tagged species should add up to the total SO2 in the CONC file.

```
	float SO2_EGU(TSTEP, LAY, ROW, COL) ;
		SO2_EGU:long_name = "SO2_EGU         " ;
		SO2_EGU:units = "ppmV            " ;
		SO2_EGU:var_desc = "tracer conc.                                                                    " ;
	float SO2_BIO(TSTEP, LAY, ROW, COL) ;
		SO2_BIO:long_name = "SO2_BIO         " ;
		SO2_BIO:units = "ppmV            " ;
		SO2_BIO:var_desc = "tracer conc.                                                                    " ;
	float SO2_BCO(TSTEP, LAY, ROW, COL) ;
		SO2_BCO:long_name = "SO2_BCO         " ;
		SO2_BCO:units = "ppmV            " ;
		SO2_BCO:var_desc = "tracer conc.                                                                    " ;
	float SO2_OTH(TSTEP, LAY, ROW, COL) ;
		SO2_OTH:long_name = "SO2_OTH         " ;
		SO2_OTH:units = "ppmV            " ;
		SO2_OTH:var_desc = "tracer conc.                                                                    " ;
	float SO2_ICO(TSTEP, LAY, ROW, COL) ;
		SO2_ICO:long_name = "SO2_ICO         " ;
		SO2_ICO:units = "ppmV            " ;
		SO2_ICO:var_desc = "tracer conc.                    
```

The sum of the tagged species in the SA_ACONC file is equal to the species in the ACONC file.

```
SO2_EGU[1] + SO2_BIO[1] + SO2_BCO[1] + SO2_OTH[1] + SO2_ICO[1] = SO2[2]

[1] = SA_ACONC
[2] = ACONC
```

Both tagged species EGU and BIO contribute to the bulk concentration, therefore the sum of all tagged species including boundary conditions (BCO) and initial conditions (ICO) and other (all untagged emissions) (OTH)

### Step 17: Obtain scripts and species definition files to post process CMAQ-ISAM 

Note: we will be running each post processing routine twice, once for the tagged species found in the SA_ACONC, SA_DRYDEP, and SA_WETDEP output files, and again for the untagged species found in ACONC and the DRYDEP, WETDEP files. This will allow us to confirm that the sum of the tagged species is equal to the untagged species.

Example species definition file and combine run script are provided to help users post-process the CMAQ-ISAM output to aggregate output from the SA_ACONC, SA_DRYDEP, and SA_WETDEP files.

Download the run script and species definition files for this case from the AWS S3 Bucket.

```
cd CMAQ_v5.5/POST/combine/scripts
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/POST/combine/scripts/run_combine_ISAM_aconc+dep_example_cb6r5_ae7_aq_12ne3_benchmark.csh
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/POST/combine/scripts/run_combine_ISAM_sa_aconc+sa_dep_example_cb6r5_ae7_aq_12ne3_benchmark.csh
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/POST/combine/scripts/SpecDef_ISAM_Conc_benchmark_cb6r5_ae7_aq.txt
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/POST/combine/scripts/SpecDef_ISAM_Dep_benchmark_cb6r5_ae7_aq.txt
```

List the files after they have been downloaded

```
ls -lrt
```

Output

```
SpecDef_ISAM_Conc_benchmark_cb6r5_ae7_aq.txt
SpecDef_ISAM_Dep_benchmark_cb6r5_ae7_aq.txt
run_combine_ISAM_sa_aconc+sa_dep_example_cb6r5_ae7_aq_12ne3_benchmark.csh
run_combine_ISAM_aconc+dep_example_cb6r5_ae7_aq_12ne3_benchmark.csh
```


### Step 18: Build and run combine

Build the combine executable

```
cd CMAQ_v5.5/POST/combine/scripts
./bldit_combine.csh gcc |& tee ./bldit_combine.log
```

Run combine to create a file with all hours for the time period of your ISAM simulation for each tagged aggregate species in the SA_ACONC output file and for another file with all hours of the time period in your ISAM simulation for the SA_DRYDEP and SA_WETDEP output files.

```
./run_combine_ISAM_sa_aconc+sa_dep_example_cb6r5_ae7_aq_12ne3_benchmark.csh gcc |& tee ./run_combine_ISAM_sa_aconc+sa_dep_example_cb6r5_ae7_aq_12ne3_benchmark.log
```

Run combine to create a file with all hours for the time period of your ISAM simulation for each aggregate species in the ACONC output file and for another file with all hours of the time period in your ISAM simulation for the DRYDEP and WETDEP output files.

```
./run_combine_ISAM_aconc+dep_example_cb6r5_ae7_aq_12ne3_benchmark.csh |& tee ./run_combine_ISAM_aconc+dep_example_cb6r5_ae7_aq_12ne3_benchmark.log
```

Examine the output files

```
ls -lrt ../../../data/output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry/POST
```

You should see that four output files were created:

```
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 223856976 Sep 26 15:32 COMBINE_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 223857340 Sep 26 15:33 COMBINE_DEP_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 393254448 Sep 26 15:33 COMBINE_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 526353656 Sep 26 15:34 COMBINE_SA_DEP_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
```

### Step 19: Review the species definition files for the ISAM run.

The species definition file calculates each of the tagged aggregate species. To see each tagged species definition for NOX, where NOX = NO + NO2, use the following grep command:.

```
grep  NOX_ SpecDef_ISAM_Conc_benchmark_cb6r5_ae7_aq.txt
```

Output:

```
NOX_EGU             ,ppbV      ,1000.0*(NO_EGU[1] + NO2_EGU[1])
NOX_BIO             ,ppbV      ,1000.0*(NO_BIO[1] + NO2_BIO[1])
NOX_BCO             ,ppbV      ,1000.0*(NO_BCO[1] + NO2_BCO[1])
NOX_ICO             ,ppbV      ,1000.0*(NO_ICO[1] + NO2_ICO[1])
NOX_OTH             ,ppbV      ,1000.0*(NO_OTH[1] + NO2_OTH[1])
```
 

### Step 20: Build and run calc_tmetric to calculate the average of all tagged species, and the average of all species for your ISAM run.


Build the calc_tmetric executable

```
cd CMAQ_v5.5/POST/calc_tmetric/scripts
./bldit_calc_tmetric.csh gcc |& tee ./bldit_calc_tmetric.log
```

Download the run scripts for calc_tmetric for the ISAM run and copy them to the calc_tmetric/scripts directory..

```
cd CMAQ_v5.5/POST/calc_tmetric/scripts
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/POST/calc_tmetric/scripts/run_calc_tmetric_ISAM_aconc.csh
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/POST/calc_tmetric/scripts/run_calc_tmetric_ISAM_sa_aconc.csh
```

Run the calc_tmetric scripts

```
./run_calc_tmetric_ISAM_sa_aconc.csh gcc |& tee ./run_calc_tmetric_ISAM_sa_aconc.log
./run_calc_tmetric_ISAM_aconc.csh gcc |& tee ./run_calc_tmetric_ISAM_aconc.log
``` 

### Step 21: Build and run hr2day to calculate the daily average concentration for each tagged and aggregated species.

Build the hr2day executable

```
cd CMAQ_v5.5/POST/hr2day/scripts
./bldit_hr2day.csh gcc |& tee ./bldit_hr2day.log
```

Download the run scripts for hr2day for the ISAM run

```
cd  CMAQ_v5.5/POST/hr2day/scripts
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/POST/hr2day/scripts/run_hr2day_ISAM_aconc.csh
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/ISAM_Benchmark/POST/hr2day/scripts/run_hr2day_ISAM_sa_aconc.csh
```


Run hr2day for both the SA_ACONC and ACONC file

```
./run_hr2day_ISAM_sa_aconc.csh gcc |& tee ./run_hr2day_ISAM_sa_aconc.log
./run_hr2day_ISAM_aconc.csh gcc |& tee ./run_hr2day_ISAM_aconc.log
```

Note, there are HR2DAY configuration options that were modified from the default settings, as this ISAM benchmark contains only two days of output, so it does not make sense to use the option to change from GMT time to local time, which is typically done to compare to observational data.

The output data is set to be saved under the ISAM output directory.

```
cd CMAQ_v5.5/data/output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry/POST
ls -lrt
```

List of POST Output files:

```
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 223856976 Sep 26 15:32 COMBINE_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 223857340 Sep 26 15:33 COMBINE_DEP_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 393254448 Sep 26 15:33 COMBINE_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 526353656 Sep 26 15:34 COMBINE_SA_DEP_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx   8251888 Sep 27 14:07 AVG_COMBINE_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx    432268 Sep 27 14:13 dailyavg_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx     95208 Sep 27 14:14 dailyavg_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.nc
```

VERDI can be used to compare the aggregated species in ACONC to the sum of the tagged aggregated species in the SA_ACONC file.

```
cd CMAQ_v5.5/data/output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry/POST
verdi -f $cwd/COMBINE_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc -f $cwd/COMBINE_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc -s "NOX[1]" -g tile -s "NOX_EGU[2]+NOX_BIO[2]+NOX_BCO[2]+NOX_ICO[2]+NOX_OTH[2]" -g tile 
```
Note, the min and max of the two tile plots should be identical. The difference can also be calculated to verify that they are only different by numerical roundoff.

```
verdi -f $cwd/COMBINE_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc -f $cwd/COMBINE_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc -s "NOX[1] - (NOX_EGU[2]+NOX_BIO[2]+NOX_BCO[2]+NOX_ICO[2]+NOX_OTH[2])" -g tile
```

VERDI can also be used to confirm that the average concentration of the aggregated species is equal to the sum of the tagged aggregated species, please note that this average is taken over two days, as the ISAM benchmark ran for two days, and two days were available in the combine output file.

```
verdi -f $cwd/AVG_COMBINE_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.nc -f $cwd/AVG_COMBINE_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.nc -s "NOX[1]" -g tile -s "NOX_EGU_AVG[2]+NOX_BIO_AVG[2]+NOX_BCO_AVG[2]+NOX_ICO_AVG[2]+NOX_OTH_AVG[2]" -g tile
```

VERDI can also be used to confirm that the daily average concentration of the aggregated species is equal to the sum of the tagged aggregated species. Note, that there are two timesteps in each daily average file, one containing the average for day 1 and one containing the average for day 2

```
verdi -f $cwd/dailyavg_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc -f $cwd/dailyavg_SA_ACONC_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry_201807.nc -s "NOX[1]" -g tile -s "NOX_EGU[2]+NOX_BIO[2]+NOX_BCO[2]+NOX_ICO[2]+NOX_OTH[2]" -g tile  
```







