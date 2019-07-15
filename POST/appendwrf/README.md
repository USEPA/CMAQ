appendwrf
========

This Fortran program concatenates variables from multiple WRF input or output files into a single file along the “Time” (unlimited) dimension. This can be useful in cases where a user may have WRF input or output files that were generated for shorter time periods and wants to combine them into files with longer (e.g. monthly) duration.

## Run Time Environment variables used:

```
 INFILE_1      input file number 1, (max of 15).
 INFILE_2      input file number 2, (max of 15).
 OUTFILE       output file name
```

## Compile appendwrf source code

Execute the build script to compile appendwrf:

```
cd $CMAQ_HOME/POST/appendwrf/scripts
./bldit_appendwrf.csh [compiler] [version] |& tee build_appendwrf.log
```

## Run appendwrf:
Edit the sample run script (run.appenwrf), then run: 
```
 ./run.appendwrf |& tee appendwrf.log
```

Check the log file to ensure complete and correct execution without errors.

