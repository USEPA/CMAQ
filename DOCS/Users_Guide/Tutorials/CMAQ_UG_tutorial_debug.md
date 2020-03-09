# CMAQ Debugging Tips

Purpose: This guide describes how to examine log files and debug issues encountered when installing and running the CMAQ test case.
This guide helps you to find and report errors to the CMAS Center Forum and follows the [best practices for posting new issues to the forum](https://forum.cmascenter.org/t/please-read-before-posting/1321).

## Prerequisite: Build Libraries and CMAQ using gcc and intel compilers: 
Follow the CMAQ Build Tutorials for the appropriate compiler: https://github.com/USEPA/CMAQ/tree/master/DOCS/Users_Guide/Tutorials

### Verify that an executable was created
```
cd $CMAQ_HOME/CCTM/scripts
ls */*.exe
```

### Inspecting CMAQ Build log files:
```
grep -i error  bldit_cctm.log
tail bldit_cctm.log
```

### If you encounter an error building CMAQ
* [Search the CMAS Forum](https://forum.cmascenter.org/search?expanded=true) for an error similar to the one that you are seeing in your bldit_cctm.log file.
* [Review the CMAQ FAQ](https://www.epa.gov/cmaq/frequent-cmaq-questions)

If you don’t find an answer that solves the issue that you are having, create a new topic on the CMAS Center Forum.
Submit a new topic issue, even if you are having an issue similar to another user.

See the [following instructions for creating a new topic](https://github.com/lizadams/CMAQ/blob/master/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_debug.md#submit-a-new-topic-issue) in the CMAS Center Forum.


### Running CMAQ:
[Follow Benchmark Tutorial instructions](https://github.com/USEPA/CMAQ/blob/master/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md) 
(does not require running ICON/BCON as inputs are provided in the Benchmark Input Data):


### Inspect the CMAQ  run log files:

#### Check the output log file in the run directory to see if it has completed successfully
```
cd $CMAQ_HOME/CCTM/scripts
```

#### The type of output log file that is created depends on how you submit the job.
IF you use slurm, with the sbatch command to submit the job, the standard error and output is logged to a slurm-*.out file

#### Use grep to verify how many processors were used to run cmaq

```
grep -i ‘Number of Processors’ slurm-*.out
```

#### Use grep to determine if CMAQ completed successfully.

```
grep -i 'PROGRAM COMPLETED SUCCESSFULLY' slurm-*.out
```

#### Use grep to check for any errors in the slurm log files.
```
grep -i 'error' slurm-*.out
```

### If your run error contains the following message:

```
error while loading shared libraries  …  cannot open shared object file …
```

* Set the $LD_LIBRARY_PATH in your .cshrc to include the location of your netCDF and netCDFF library shared object files. 
* Note: your .cshrc file should be located in your home directory

* Change directories to your home directory
```
cd ~
```
* View the contents of your .cshrc
```
more .cshrc
```

* Edit your .cshrc to set the LD_LIBRARY_PATH to include the location of the netcdf libraries
* Note this path is dependent on what compiler you used, replace intel with gcc if you used gnu rather than the intel compiler.
```
setenv NCDIR ${CMAQ_HOME}/lib/x86_64/intel/netcdf
setenv NCFDIR ${CMAQ_HOME}/lib/x86_64/intel/netcdff
setenv LD_LIBRARY_PATH ${NCDIR}/lib:$NCFDIR/lib:${LD_LIBRARY_PATH}
```

#### If the program did not completed successfully for another reason, you will need to check the per processor log files which begin with the name: CTM_LOG_*.
* These files may either be located in the run directory, if the run script was aborted
* Or they may have been moved by the run script to a LOGS directory under the output directory. 

#### Look in the following locations for the CTM_LOG* log files:
```
cd $CMAQ_HOME/CCTM/scripts
```
or
```
cd $CMAQ_HOME/data/LOGS
```

#### Determine the number of log files that exist using the ls command and word count command
```
ls CTM_LOG* | wc
```

* There should be 1 log command for each processor used to run CMAQ for each day.

#### Use the grep command to determine if the message “PROGRAM COMPLETED SUCCESSFULLY” is at the bottom of all of the log files.
```
grep -i 'PROGRAM COMPLETED SUCCESSFULLY' CTM_LOG* | wc
```

* If you ran the program on 16 processors, you should see a word count of 16 files that contain this message for each day that you run the model.
* If there were fewer findings of the successful run command message than the number of processors that were used to run CMAQ:
Use the grep command to find an error in any of the files
```
grep -i error CTM_LOG*
```

### If you encounter an error running CMAQ.
* [Search the CMAS Forum](https://forum.cmascenter.org/search?expanded=true) for an error similar to the one that you are seeing in your CTM_LOG file.
* [Review the CMAQ FAQ](https://www.epa.gov/cmaq/frequent-cmaq-questions)

* If you don’t see a similar error reported in an issue or in a FAQ that provides enough information for you to troubleshoot and solve the issue then submit a new topic.

### Submit a new topic issue

* [Visit the category](https://forum.cmascenter.org/categories) that best describes your issue.

#### For example, if you are having an issue running CMAQ [choose the category](https://forum.cmascenter.org/c/cmaq/run-time-errors-and-issues/14)

Or choose the [parent CMAQ category](https://forum.cmascenter.org/c/cmaq/7) 

#### Click on + New Topic in the upper right corner
The Category will be pre-selected if you start a new topic request from within a category, if the category is “Uncategorized”, then use the pull-down menu to select the category for your topic.

#### Selecting a category is important, as the CMAS Center and EPA staff are only monitoring topics submitted within a category that matches their expertise.
 
#### Type in a title for your topic that describes your CMAQ compiler environment
Example Title: 
```
CMAQv5.3.1 segmentation fault using gcc and openmpi
```

#### In the body of the new issue please provide the output from the following commands
* Report the compiler and version used to run CMAQ
```
mpif90 --version
```

* Example Compiler Version:
```
GNU Fortran (GCC) 9.1.0
```

* Report the version of CMAQ that you are using.
```
ls */*.exe
```

* Example CMAQ version:
```
BLD_CCTM_v531_intel/CCTM_v531.exe
```

* Report the name of the run script if it is a benchmark case, or report the Domain and resolution
Example CMAQ run script:
```
run_cctm_Bench_2016_12SE1.csh
```

* Report a limited amount of the error message contents in the body of the issue with output obtained by using the following commands:
```
cd $CMAQ_HOME/data/{YOUR_OUTPUT_DIR}/LOGS/
```
* The following grep command -B NUM, prints NUM lines before the error statement is found.
```
grep -B 5 -i error CTM_LOG*
```

### Upload additional files by clicking on the up arrow icon in the menu underneath the Create New Topic Title including:
* your run script
* standard out log file
* per-processor log file that contains the error message.
* Note You will need to rename any files to match one of the following extensions (jpg, jpeg, png, gif, csh, txt, csv), for instance, copy cmaq.log to cmaq.log.txt

```
cp CTM_LOG_000.v531_intel_Bench_2016_12SE1_20160701 CTM_LOG_000.v531_intel_Bench_2016_12SE1_20160701.txt
```

* When someone replies to your topic, you will receive an e-mail notification. 
* Please click on the “VISIT TOPIC” button in your e-mail to return to your CMAS Center Forum Issue and reply to any follow-up questions or suggestions. 
