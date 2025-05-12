# CMAQ Debugging Tips

Purpose: This guide describes how to examine log files and debug issues encountered when installing and running the CMAQ test case.
This guide helps you to find and report errors to the CMAS Center Forum and follows the [best practices for posting new issues to the forum](https://forum.cmascenter.org/t/please-read-before-posting/1321).

## Building CMAQ
### Prerequisite: Build Libraries and CMAQ using gcc or intel compilers
Follow the CMAQ Build Tutorials for the appropriate compiler: 

### Uncompressed netCDF library builds:<br>

* [Building CMAQ classic libraries for GNU](CMAQ_UG_tutorial_build_library_gcc.md)
* [Building CMAQ classic libraries for Intel](CMAQ_UG_tutorial_build_library_intel.md)

### Compressed netCDF-4 Library builds:<br>

* [Building CMAQ libraries netCDF-4 compression for GNU](CMAQ_UG_tutorial_build_library_gcc_support_nc4.md)
* [Building CMAQ using netCDF-4 compression for Intel](CMAQ_UG_tutorial_build_library_intel_support_nc4.md)

### Verify that an executable was created
```
cd $CMAQ_HOME/CCTM/scripts
ls */*.exe
```

### Inspecting CMAQ Build log files
```
grep -i error  bldit_cctm.log
tail bldit_cctm.log
```

### If you encounter an error building CMAQ
* [Search the CMAS Forum](https://forum.cmascenter.org/search?expanded=true) for an error similar to the one that you are seeing in your bldit_cctm.log file.
* [Review the CMAQ FAQ](https://www.epa.gov/cmaq/frequent-cmaq-questions)

If you don’t find an answer that solves the issue that you are having, create a new topic on the CMAS Center Forum.
Please create a new topic in describing your issue, even if your issue is similar to that of another user.

**See the instructions at the bottom of this tutorial for creating a new topic on the CMAS User Forum.**

## Running CMAQ
### Prerequisite: Run the CMAQ Benchmark case
[Follow Benchmark Tutorial instructions](CMAQ_UG_tutorial_benchmark.md)
(does not require running ICON/BCON as inputs are provided in the Benchmark Input Data).


### Inspect the CMAQ run log files

Check the output log file in the run directory to see if it has completed successfully. 
```
cd $CMAQ_HOME/CCTM/scripts
```

**The type of output log file that is created depends on how you submit the job.** If you use slurm, with the sbatch command to submit the job, the standard error and output is logged to a slurm-\*.out file.
 
Use grep to verify how many processors were used to run cmaq.  
```
grep -i ‘Number of Processors’ slurm-*.out
```

Use grep to determine if CMAQ completed successfully.  

```
grep -i 'PROGRAM COMPLETED SUCCESSFULLY' slurm-*.out
```

Use grep to check for any errors in the slurm log files.  
```
grep -i 'error' slurm-*.out
```

### If your run error contains the following message:  

```
error while loading shared libraries  …  cannot open shared object file …
```

This error indicates that the `LD_LIBRARY_PATH` environment variable is not properly set to include the path to all the required shared library object files. If this occurs, one solution is to edit the CCTM run script to add the path to the netCDF C (libnetcdf.so) and Fortran (libnetcdff.so) libraries to the `LD_LIBRARY_PATH` variable.  For example:
```
setenv NCDIR ${CMAQ_HOME}/lib/x86_64/intel/netcdf
setenv NCFDIR ${CMAQ_HOME}/lib/x86_64/intel/netcdff
setenv LD_LIBRARY_PATH ${NCDIR}/lib:${NCFDIR}/lib:${LD_LIBRARY_PATH}
```
Note this path is dependent on what compiler you used, replace intel with gcc if you used gnu rather than the intel compiler.  
If this does not work, then cd to the build directory and use the command:
```
ldd CCTM*.exe
```
This will show the absolute path to the libraries used in compiling the model. Edit the `LD_LIBRARY_PATH` environment variable to include the *directories* where the libnetcdf.so and libnetcdff.so files are located.

### If the program did not complete successfully for another reason
If the program did not complete successfully for another reason, you will need to check the per processor log files which begin with the name: CTM_LOG_\*.
* These files may either be located in the run directory, if the run script was aborted.
* Or they may have been moved by the run script to a LOGS directory under the output directory. 

Look in the following locations for the CTM_LOG* log files:  
```
cd $CMAQ_HOME/CCTM/scripts
```
or
```
cd $CMAQ_HOME/data/LOGS
```

Determine the number of log files that exist using the ls command and word count command.  
```
ls CTM_LOG* | wc
```

There should be 1 log command for each processor used to run CMAQ for each day.  

Use the grep command to determine if the message “PROGRAM COMPLETED SUCCESSFULLY” is at the bottom of all of the log files.  
```
grep -i 'PROGRAM COMPLETED SUCCESSFULLY' CTM_LOG* | wc
```

* If you ran the program on 16 processors, you should see a word count of 16 files that contain this message for each day that you run the model.
* If there were fewer findings of the successful run command message than the number of processors that were used to run CMAQ:
Use the grep command to find an error in any of the files
```
grep -i error CTM_LOG*
```

## If you encounter an error running CMAQ
* [Search the CMAS Forum](https://forum.cmascenter.org/search?expanded=true) for an error similar to the one that you are seeing in your CTM_LOG file.
* [Review the CMAQ FAQ](https://www.epa.gov/cmaq/frequent-cmaq-questions)

* If you don’t see a similar error reported in an issue or in a FAQ that provides enough information for you to troubleshoot and solve the issue then submit a new topic.

## If the program crashed
* If the program crashed (as opposed to aborting with an error message), you may get a stack trace similar to the following:
```
forrtl: severe (174): SIGSEGV, segmentation fault occurred
Image              PC                Routine            Line        Source
CCTM_s07tic_noche  00000000009AF90D  Unknown               Unknown  Unknown
libpthread-2.18.s  00002AF0F5B4B6D0  Unknown               Unknown  Unknown
CCTM_s07tic_noche  00000000006F3A8A  Unknown               Unknown  Unknown
CCTM_s07tic_noche  0000000000605EF2  Unknown               Unknown  Unknown
CCTM_s07tic_noche  00000000005FEC8C  Unknown               Unknown  Unknown
CCTM_s07tic_noche  00000000005FD619  Unknown               Unknown  Unknown
CCTM_s07tic_noche  0000000000406D9E  Unknown               Unknown  Unknown
libc-2.18.so       00002AF0F6464D65  __libc_start_main     Unknown  Unknown
CCTM_s07tic_noche  0000000000406CA9  Unknown               Unknown  Unknown
```
Please do not post an unreadable stack trace to the user forum! Instead, recompile the model in debug mode (uncomment "set Debug_CCTM" in bldit_cctm.csh) and rerun. The model will run much more slowly, but when a crash occurs, the stack trace will provide information which should help with debugging.  For example:
```
forrtl: severe (174): SIGSEGV, segmentation fault occurred
Image              PC                Routine            Line        Source
CCTM_s07tic_noche  0000000001A61C1D  Unknown               Unknown  Unknown
libpthread-2.18.s  00002B8D9E0FC6D0  Unknown               Unknown  Unknown
CCTM_s07tic_noche  0000000001551229  aero_                     503  aero_driver.F
CCTM_s07tic_noche  0000000000E617C1  sciproc_                  298  sciproc.F
CCTM_s07tic_noche  0000000000E48385  cmaq_driver_              679  driver.F
CCTM_s07tic_noche  0000000000E40B84  MAIN__                     96  cmaq_main.F
CCTM_s07tic_noche  0000000000406D9E  Unknown               Unknown  Unknown
libc-2.18.so       00002B8D9EA15D65  __libc_start_main     Unknown  Unknown
CCTM_s07tic_noche  0000000000406CA9  Unknown               Unknown  Unknown
```
This stack trace indicates that the error occurred on line 503 of the file aero_driver.F.

To rebuild a debug version

```
cp bldit_cctm.csh bldit_cctmv55_debug.csh
vi bldit_cctmv55_debug.csh
```

uncomment the following line 

```
#set Debug_CCTM 
```

change to 

```
set Debug_CCTM 
```

Rerun the build script

```
./bldit_cctmv55_debug.csh gcc |& tee ./bldit_cctmv55_debug.log
```

Edit your run script to use the newly compiled debug version that is in a BLD directory with the following extension `_debug`

Re-run using the debug version


## Submit a new topic issue on the CMAS User Forum


* [Visit the category](https://forum.cmascenter.org/categories) that best describes your issue.

* For example, if you are having an issue running CMAQ [choose the category cmaq runtime issues](https://forum.cmascenter.org/c/cmaq/run-time-errors-and-issues/14)

* Or choose the [parent CMAQ category](https://forum.cmascenter.org/c/cmaq/7) 

* Click on + New Topic in the upper right corner.
The Category will be pre-selected if you start a new topic request from within a category. If the category is “Uncategorized”, then use the pull-down menu to select the category for your topic.


### Selecting a category for your issue
Selecting a category is important, as the CMAS Center and EPA staff are only monitoring topics submitted within a category that matches their expertise.

 
* Type in a title for your topic that describes your CMAQ compiler environment
Example Title: 
```
CMAQv5.4 segmentation fault using gcc and openmpi
```

### Template for what to include in your new issue.
**Please include the following information when creating a new issue.**  This will make it much faster and easier for others to understand your issue and respond with an appropriate suggestion.

1. Report the **compiler and version used to run CMAQ**
```
mpif90 --version
```
2. Report the **version of CMAQ** that you are using.
```
ls */*.exe
```
3. Report the name of the run script if it is a benchmark case, or **report the Domain and resolution**
4. Report a limited amount of the **error message contents** in the body of the issue with output obtained by using the following commands:
```
cd $CMAQ_HOME/data/{YOUR_OUTPUT_DIR}/LOGS/
```
* The grep argument -B NUM prints NUM lines before the matching text is found.  For example, this command will print out the 10 lines prior to each occurrence of the word "error" (case insensitive):
```
grep -B 10 -i error CTM_LOG_000*
```

#### Example of information to include in a new issue post:

| | |
|:--------:|:----------------:|
| Compiler Version | ifort version 18.0.1 |
|CMAQ Version | BLD_CCTM_v54_intel/CCTM_v54.exe |
| Run Script | run_cctm_Bench_2018_12NE3.csh |

Error message encountered: 
```
error while loading shared libraries  …  cannot open shared object file …

```

### Upload additional files 

Click on the up arrow icon in the menu underneath the Create New Topic Title including:
* your run script
* standard out log file
* per-processor log file that contains the error message.
* Note You will need to rename any files to match one of the following extensions (jpg, jpeg, png, gif, csh, txt, csv), for instance, copy cmaq.log to cmaq.log.txt

```
CTM_LOG_000.v54_gcc_Bench_2018_12NE3_2day_20180701  CTM_LOG_000.v54_gcc_Bench_2018_12NE3_2day_20180702  
```

* When someone replies to your topic, you will receive an e-mail notification. 
* Please click on the “VISIT TOPIC” button in your e-mail to return to your CMAS Center Forum Issue and reply to any follow-up questions or suggestions. 
