## CMAQ-ISAM Benchmark Tutorial ## 

Procedure to build and run the CMAQ-ISAM model using intel compiler for the cracmm3 mechanism with the STAGE dry deposition scheme:

### Step 1: Download and run the CMAQv6 benchmark case (without ISAM) to confirm that your model run is consistent with the provided benchmark output.
- [CMAQ Benchmark Tutorial](CMAQ_UG_tutorial_benchmark.md)

If you encounter any errors, try running the model in debug mode and refer to the CMAS User Forum to determine if any issues have been reported.

https://forum.cmascenter.org/

### Step 2: Read the User Guide Chapter on Integrated Source Apportionment Method.
- [CMAQ User Guide Chapter on ISAM](../CMAQ_UG_ch11_ISAM.md)

Note: This benchmark is intended to demonstrate how to build and run CMAQ-ISAM with the provided input files

The following isam control file is provided in the CCTM/scripts directory when you obtain the CMAQv6 code from github (step 5 below):

```
cat isam_control.2022_12SE1.txt
```

This file contains the following tag classes and tag names.

```
TAG CLASSES     |SULFATE, OZONE

TAG NAME        |NCE
REGION(S)       |NC
EMIS STREAM(S)  |PT_EGU

TAG NAME        |NCF
REGION(S)       |NC
EMIS STREAM(S)  |PT_FIRES
```

The following gridmask file is provided with the benchmark inputs in the CMAQv6.0_2022_12SE1_Benchmark_2day/2022_12SE1/surface/ directory (see step 11 below)

```
GRIDMASK_STATES_12SE1.nc
```

Note, all states are listed in the variable list in the header of the file, but the data only contains valid entries for the states in the 12SE1 domain. 

The instructions require the user to edit the emissions control namelist file and the chemical control namelist file in the BLD directory. If you want to use emission scaling (independently from ISAM or DDM3D) you will also need to edit these files. (see step 10 below).

```
CMAQ_Control.nml
CMAQ_Control_${MECH}.nml
```


### Step 3 (optional): choose your compiler, and load it using the module command if it is available on your system

```
module avail
```

```
module load openmpi/5.0.10/gcc_15.2.0
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
set CMAQ_HOME = [your_install_path]/CMAQ_v6
```

Now execute the script.

```
./bldit_project.csh
```

Change directories to the CMAQ_HOME directory

```
cd [your_install_path]/CMAQ_v6
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
### Step 8: Modify the bldit_cctm.csh to specify the cracmm3 mechanism

```
setenv Mechanism cracmm3              #> chemical mechanism (see $CMAQ_MODEL/CCTM/src/MECHS) 
```

Verify that the bldit_cctm_isam.csh script contains the following lines: (the mechanism and the dry deposition scheme have been added to the BLD directory name):

#> Set and create the "BLD" directory for checking out and compiling 
#> source code. Move current directory to that build directory.

```
 if ( $?Debug_CCTM ) then
     set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_debug_${Mechanism}
 else
     set Bld = $CMAQ_HOME/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_${Mechanism}
 endif
```

### Step 9: Run the bldit_cctm_isam.csh script

```
./bldit_cctm_isam.csh gcc |& tee bldit_cctm_isam.log
```

### Step 10: Edit the Emission Control Namelist to recognize the CMAQ_REGIONS file 

Change directories to the build directory
```
cd BLD_CCTM_v6_ISAM_gcc_cracmm3 
```

edit the emissions namelist file

```
gedit CMAQ_Control.nml
```

Uncomment the line that contains ISAM_REGIONS as the File Label

```
               'EVERYWHERE'  ,'N/A'        ,'N/A',
 !              'NY'          ,'CMAQ_MASKS', 'NY',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
               'ALL'         ,'ISAM_REGIONS','ALL',
/
```


### Step 11: Example of emissions scaling (Reduce the PT_EGU emissions in NC by 25%) (Optional step, described here, but not used)

edit the chemical control namelist file, note please specify the mechanism or define the MECH environment variable.

```
gedit CMAQ_Control_${MECH}.nml
```

Add the following line at the bottom of the the namelist file (before the /)

```
   ! PT_EGU Emissions Scaling reduce PT_EGU emissions in NC by 25%. Note, to reduce the emissions by 25% we use DESID to multiply what had been 100% emissions by .75, so that the resulting emissions is reduced by 25%.
   'NC'  , 'PT_EGU'      ,'All'    ,'All'         ,'All' ,.75    ,'UNIT','o',

```

### Step 12: Install the CMAQ-ISAM reference input and output benchmark data

Download the CMAQ two day reference input and output data from the  [CMAS Center Data Warehouse Amazon Web Services S3 Bucket](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/index.html#v6/): CMAQv6.0_2022_12SE1_Benchmark_2day_Input.tar.gz and CMAQv6.0_ISAM_2022_12SE1_Benchmark_2day_Output.tar.gz.

Download and copy the data to `$CMAQ_DATA`. Navigate to the `$CMAQ_DATA` directory, unzip and untar the two day benchmark input and output files:

```
cd $CMAQ_DATA
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v6/CMAQv6.0_2022_12SE1_Benchmark_2day_Input.tar.gz
tar xvzf CMAQv6.0_2022_12SE1_Benchmark_2day_Input.tar.gz
mkdir ref_output
cd ref_output
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v6/ISAM_Benchmark/CMAQv6.0_ISAM_2022_12SE1_Benchmark_2day_Output.tar.gz
tar xzvf CMAQv6.0_ISAM_2022_12SE1_Benchmark_2day_Output.tar.gz
```

The input files for the CMAQv6 ISAM benchmark case are the same as the benchmark inputs for the base model. Output source apportionment files associated with the sample isam_control.txt provided in this release package are included in the benchmark outputs for the base model.
    
### Step 13: Edit the CMAQ-ISAM runscript

Note: there is an example of the run script on the AWS S3 bucket.

```
cd CMAQ_v6/CCTM/scripts
wget https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v6/ISAM_Benchmark/CCTM/scripts/run_cctm_Bench_2022_12SE1_cracmm3_ISAM.csh
cat run_cctm_Bench_2022_12SE1_cracmm3_ISAM.csh 
```

Verify the following settings in the run script for this ISAM benchmark.

Verify the General Parameters for Configuring the Simulation

```
 set VRSN      = v6_ISAM
 set PROC      = mpi               #> serial or mpi
 set MECH      = cracmm3      #> Mechanism ID
 set APPL      = Bench_2022_12SE1_${MECH}  #> Application Name (e.g. Gridname)
```


Verify the input data directory

```
#> Set Working, Input, and Output Directories
 setenv WORKDIR ${CMAQ_HOME}/CCTM/scripts          #> Working Directory. Where the runscript is.
 setenv OUTDIR  ${CMAQ_DATA}/output_CCTM_${RUNID}  #> Output Directory
 setenv INPDIR  ${CMAQ_DATA}/CMAQv6.0/CMAQv6.0_2022_12SE1_Benchmark_2day/2022_12SE1            #> Input Directory
```

Verify the start and end dates to match the input data for this benchmark.

```
#> Set Start and End Days for looping
 setenv NEW_START TRUE             #> Set to FALSE for model restart
 set START_DATE = "2022-07-01"     #> beginning date (July 1, 2022)
 set END_DATE   = "2022-07-02"     #> ending date    (July 2, 2022)
```


Verify that ISAM is turned on and that the SA_IOLIST file and ISAM regions file definitions are uncommented.

```
setenv CTM_ISAM Y
setenv SA_IOLIST ${WORKDIR}/isam_control.2022_12SE1.txt
setenv ISAM_REGIONS $INPDIR/GRIDMASK_STATES_12SE1.nc
```

   
Run or Submit the script to the batch queueing system

```
./run_cctm_Bench_2022_12SE1_cracmm3_ISAM.csh
```

OR (If using SLRUM) edit the #SBATCH commands at the top of the script for your machine, then run using

```
sbatch run_cctm_Bench_2022_12SE1_cracmm3_ISAM.csh
```

### Step 14: Verify that the run was successful
   - look for the output directory
   
   ```
   cd ../../data/output_CCTM_v6_ISAM_gcc_Bench_2022_12SE1
   ```
   If the run was successful you will see the following output
   
   ```
   tail ./LOGS/CTM_LOG_016.v6_ISAM_gcc_Bench_2022_12SE1_20220702
   ```
   |>---   PROGRAM COMPLETED SUCCESSFULLY   ---<|

### Step 15: Compare output with the 2 day benchmark outputs that were downloaded from the s3 bucket above. 

The following ISAM output files are generated in addition to the standard CMAQ output files. Note, the ACONC files created for the  benchmark case without ISAM and this run will not be comparible if emission scaling is used (Step 11 - optional), but if emission scaling was not used, the files should be identical.

```
CCTM_SA_CONC_v6_ISAM_gcc_Bench_2022_12SE1_20220701.nc
CCTM_SA_WETDEP_v6_ISAM_gcc_Bench_2022_12SE1_20220701.nc
CCTM_SA_DRYDEP_v6_ISAM_gcc_Bench_2022_12SE1_20220701.nc
CCTM_SA_CGRID_v6_ISAM_gcc_Bench_2022_12SE1_20220701.nc
CCTM_SA_ACONC_v6_ISAM_gcc_Bench_2022_12SE1_20220701.nc
```

### Step 16: Compare the tagged species in `CCTM_SA_ACONC` output file to the species in `CCTM_ACONC` output file

```
ncdump -h CCTM_SA_CONC_v6_ISAM_gcc_Bench_2022_12SE1_20220701.nc | grep SO2_
```


The following tagged species should add up to the total SO2 in the CONC file.

```
	float SO2_NCE(TSTEP, LAY, ROW, COL) ;
		SO2_NCE:long_name = "SO2_NCE         " ;
		SO2_NCE:units = "ppmV            " ;
		SO2_NCE:var_desc = "tracer conc.                                                                    " ;
	float SO2_NCF(TSTEP, LAY, ROW, COL) ;
		SO2_NCF:long_name = "SO2_NCF         " ;
		SO2_NCF:units = "ppmV            " ;
		SO2_NCF:var_desc = "tracer conc.                                                                    " ;
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
		SO2_ICO:var_desc = "tracer conc.                                                                    " ;
```

The sum of the tagged species in the SA_ACONC file is equal to the species in the ACONC file.

```
SO2_NCE[1] + SO2_NCF[1] + SO2_BCO[1] + SO2_OTH[1] + SO2_ICO[1] = SO2[2]

[1] = SA_ACONC
[2] = ACONC
```

Both tagged species NCE and NCF contribute to the bulk concentration, therefore the sum of all tagged species including boundary conditions (BCO) and initial conditions (ICO) and other (all untagged emissions) (OTH)

### Step 17: Obtain scripts and species definition files to post process CMAQ-ISAM 


Note: we will be running each post processing routine twice, once for the tagged species found in the SA_ACONC, SA_DRYDEP, and SA_WETDEP output files, and again for the untagged species found in ACONC and the DRYDEP, WETDEP files. This will allow us to confirm that the sum of the tagged species is equal to the untagged species.

Example species definition file and combine run script are provided to help users post-process the CMAQ-ISAM output to aggregate output from the SA_ACONC, SA_DRYDEP, and SA_WETDEP files.

Download the run script and species definition files for this case from the AWS S3 Bucket.
Install the AWS CLI on your local computer using the following instructions: <a href="https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html#cliv2-linux-install">Install AWS CLI</a>

```
cd CMAQ_v6/POST/combine/scripts
aws s3 cp --recursive --no-sign-request --recursive s3://cmaq-release-benchmark-data-for-easy-download/v6/ISAM_Benchmark/POST/combine/scripts/ .
```

List the files after they have been downloaded

```
ls -lrt
```

Output

```
run_combine_ISAM_aconc+dep_example_cracmm3_12se1_benchmark.csh
run_combine_ISAM_sa_aconc+sa_dep_example_cracmm3_12se1_benchmark.csh
SpecDef_ISAM_Dep_cracmm3.txt
SpecDef_ISAM_Conc_cracmm3.txt
```


### Step 18: Build and run combine

Build the combine executable

```
cd CMAQ_v6/POST/combine/scripts
./bldit_combine.csh gcc |& tee ./bldit_combine.log
```

Run combine to create a file with all hours for the time period of your ISAM simulation for each tagged aggregate species in the SA_ACONC output file and for another file with all hours of the time period in your ISAM simulation for the SA_DRYDEP and SA_WETDEP output files.

```
./run_combine_ISAM_sa_aconc+sa_dep_example_cracmm3_12se1_benchmark.csh |& tee ./run_combine_ISAM_sa_aconc+sa_dep_example_cracmm3_12se1_benchmark.log
```

Run combine to create a file with all hours for the time period of your ISAM simulation for each aggregate species in the ACONC output file and for another file with all hours of the time period in your ISAM simulation for the DRYDEP and WETDEP output files.

```
./run_combine_ISAM_aconc+dep_example_cracmm3_12se1_benchmark.csh |& tee ./run_combine_ISAM_aconc+dep_example_cracmm3_12se1_benchmark.log
```

Examine the output files

```
ls -lrt ../../../CMAQv6.0/CMAQv6.0_2022_12SE1_Benchmark_2day/POST/
```

You should see that four output files were created:

```
COMBINE_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc
COMBINE_DEP_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc
COMBINE_SA_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc
COMBINE_SA_DEP_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc
```

### Step 19: Review the species definition files for the ISAM run.

The species definition file calculates each of the tagged aggregate species. To see each tagged species definition for NOX, where NOX = NO + NO2, use the following grep command:.

```
grep  NOX_  SpecDef_ISAM_Conc_cracmm3.txt 
```

Output:

```
NOX_NCE             ,ppbV      ,1000.0*(NO_NCE[1] + NO2_NCE[1])
NOX_NCF             ,ppbV      ,1000.0*(NO_NCF[1] + NO2_NCF[1])
NOX_BCO             ,ppbV      ,1000.0*(NO_BCO[1] + NO2_BCO[1])
NOX_ICO             ,ppbV      ,1000.0*(NO_ICO[1] + NO2_ICO[1])
NOX_OTH             ,ppbV      ,1000.0*(NO_OTH[1] + NO2_OTH[1])
```
 

### Step 20: Build and run calc_tmetric to calculate the average of all tagged species, and the average of all species for your ISAM run.


Build the calc_tmetric executable

```
edit script to use version = v6_ISAM
cd CMAQ_v6/POST/calc_tmetric/scripts
./bldit_calc_tmetric_ISAM.csh gcc |& tee ./bldit_calc_tmetric_ISAM.log
```

Download the run scripts for calc_tmetric for the ISAM run and copy them to the calc_tmetric/scripts directory..

```
cd CMAQ_v6/POST/calc_tmetric/scripts
aws s3 cp --no-sign-request --recursive s3://cmaq-release-benchmark-data-for-easy-download/v6/ISAM_Benchmark/POST/calc_tmetric/scripts/ .
```

Run the calc_tmetric scripts

```
./run_calc_tmetric_ISAM_aelmo.csh |& tee ./run_calc_tmetric_ISAM_aelmo.log
./run_calc_tmetric_ISAM_sa_aelmo.csh gcc |& tee ./run_calc_tmetric_ISAM_sa_aelmo.log
``` 

### Step 21: Build and run hr2day to calculate the daily average concentration for each tagged and aggregated species.

Build the hr2day executable

```
cd CMAQ_v6/POST/hr2day/scripts
./bldit_hr2day_ISAM.csh gcc |& tee ./bldit_hr2day_ISAM.log
```

Download the run scripts for hr2day for the ISAM run

```
cd  CMAQ_v6/POST/hr2day/scripts
aws s3 cp --no-sign-request --recursive s3://cmaq-release-benchmark-data-for-easy-download/v6/ISAM_Benchmark/POST/hr2day/scripts/ .
```


Run hr2day for both the SA_ACONC and ACONC file

```
./run_hr2day_ISAM_sa_aelmo.csh gcc |& tee ./run_hr2day_ISAM_sa_aelmo.log
./run_hr2day_ISAM_aelmo.csh gcc |& tee ./run_hr2day_ISAM_aelmo.log
```

Note, there are HR2DAY configuration options that were modified from the default settings, as this ISAM benchmark contains only two days of output, so it does not make sense to use the option to change from GMT time to local time, which is typically done to compare to observational data.


```
cd CMAQ_v6/CMAQv6.0_2022_12SE1_Benchmark_2day/POST
ls -lrt
```

List of POST Output files:

```
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 253555116 Oct  4 17:43 COMBINE_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 162894164 Oct  4 17:43 COMBINE_DEP_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx  63427596 Oct  5 10:12 average_concentrations_ISAM_AELMO_v6_ISAM_gcc_Bench_2022_12SE1.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 327314176 Oct  5 10:44 COMBINE_SA_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx 301192136 Oct  5 10:44 COMBINE_SA_DEP_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx  81876832 Oct  5 10:46 average_concentrations_ISAM_SA_AELMO_v6_ISAM_gcc_Bench_2022_12SE1.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx     43200 Oct  5 10:48 dailymaxozone_ISAM_AELMO_v6_ISAM_gcc_Bench_2022_12SE1.nc
-rw-rw-r-- 1 lizadams rc_cep-emc_psx     11192 Oct  5 10:48 dailymaxozone_ISAM_sa_aelmo_v6_ISAM_gcc_Bench_2022_12SE1.nc
```

VERDI can be used to compare the aggregated species in ACONC to the sum of the tagged aggregated species in the SA_ACONC file.

```
cd CMAQ_v6/CMAQv6.0_2022_12SE1_Benchmark_2day/POST/
verdi -f $cwd/COMBINE_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc -f $cwd/COMBINE_SA_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc -s "NOX[1]" -g tile -s "NOX_NCE[2]+NOX_NCF[2]+NOX_BCO[2]+NOX_ICO[2]+NOX_OTH[2]" -g tile 
```
Note, the min and max of the two tile plots should be identical. The difference can also be calculated to verify that they are only different by numerical roundoff.

```
verdi -f $cwd/COMBINE_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc -f $cwd/COMBINE_SA_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc -s "NOX[1] - (NOX_NCE[2]+NOX_NCF[2]+NOX_BCO[2]+NOX_ICO[2]+NOX_OTH[2])" -g tile
```

VERDI can also be used to confirm that the average concentration of the aggregated species is equal to the sum of the tagged aggregated species, please note that this average is taken over two days, as the ISAM benchmark ran for two days, and two days were available in the combine output file.

```
verdi -f $cwd/average_concentrations_ISAM_AELMO_v6_ISAM_gcc_Bench_2022_12SE1.nc -f $cwd/COMBINE_SA_AELMO_v6_ISAM_gcc_Bench_2022_12SE1_202207.nc -s "NOX[1]" -g tile -s "NOX_NCE_AVG[2]+NOX_NCF_AVG[2]+NOX_BCO_AVG[2]+NOX_ICO_AVG[2]+NOX_OTH_AVG[2]" -g tile
```

VERDI can also be used to confirm that the daily average concentration of the aggregated species is equal to the sum of the tagged aggregated species. Note, that there are two timesteps in each daily average file, one containing the average for day 1 and one containing the average for day 2

```
verdi -f $cwd/dailyavg_ISAM_AELMO_v6_ISAM_gcc_Bench_2022_12SE1.nc -f $cwd/dailyavg_ISAM_SA_AELMO_v6_ISAM_gcc_Bench_2022_12SE1.nc -s "NOX[1]" -g tile -s "NOX_NCE[2]+NOX_NCF[2]+NOX_BCO[2]+NOX_ICO[2]+NOX_OTH[2]" -g tile  
```
