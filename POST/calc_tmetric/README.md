calc_tmetric
========

This Fortran program creates gridded IOAPI files with temporally averaged or summed values that were calculated from one or more gridded time-dependent IOAPI files.

##Environment variables used:

```
 OPERATION     operation to perform - SUM for summation and AVG for averaging (default SUM)
 SPECIES_#     list of species to output (e.g. setenv SPECIES_1 O3).  
               To extract all species use: setenv SPECIES_1 ALL
 M3_FILE_#     List of up to 366 input IOAPI file name with time-dependent values.
               The program will concatenate time steps from all input files to construct the
	       longest possible time record which can be processed. Duplicate time steps are
	       eliminated. The program will then sum or average variable values across these 
	       non-duplicate time steps
 OUTFILE       output IOAPI file name with gridded summed or averaged values
```

##To run:
Edit the sample run script (run.calc_tmetric.csh), then run:
```
 run.calc_tmetric.csh |& tee calc_tmetric.log
```
Check the log file to ensure complete and correct execution without errors.
