calc_tmetric
========

This Fortran program creates gridded IOAPI files with temporally averaged or summed values that were calculated from one or more gridded time-dependent IOAPI files.

## Run Time Environment variables used:

```
 OPERATION     operation to perform - SUM for summation and AVG for averaging (default SUM)
 SPECIES_#     list of species to output (e.g. setenv SPECIES_1 O3).  
               To extract all species use: setenv SPECIES_1 ALL
 M3_FILE_#     List of input IOAPI file names with time-dependent values.
               The program will concatenate time steps from all input files to construct the
	       longest possible time record which can be processed. Duplicate time steps are
	       eliminated. The program will then sum or average variable values across these 
	       non-duplicate time steps.
	       The maximum number of IOAPI files is set to be one less than the global IOAPI parameter MXFILE3.
	       Since this parameter is currently set to 64 (https://www.cmascenter.org/ioapi/documentation/all_versions/html/TUTORIAL.html),
	       the maximum number of IOAPI input files is 63.
 OUTFILE       output IOAPI file name with gridded summed or averaged values
```

## Compile calc_tmetric source code:

Execute the build script to compile calc_tmetric:

```
cd $CMAQ_HOME/POST/calc_tmetric/scripts
./bldit_calc_tmetric.csh [compiler] [version] |& tee build_calc_tmetric.log
```

## Run calc_tmetric:
Edit the sample run script (run.calc_tmetric.csh), then run:
```
 ./run.calc_tmetric.csh |& tee calc_tmetric.log
```
Check the log file to ensure complete and correct execution without errors.

