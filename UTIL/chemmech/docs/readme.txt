This tarfile contains the runscript called runit_chemmech, input data, and code 
directory to generates the RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 files 
for CMAQ version 5.1.

To use this tool:
1) Compile the tool by setting the COMPILER variable and setting compile = T in the bldrun.chemmech script.
The changes set the fortran compiler, cc compiler, their compilation
and link flags based on your system.

2) Modify the runscript by selecting the mechanism name that you would like to
compile into CMAQ input files. Use either an existing mechanism or create a
new mechanism under the $M3HOME/tools/chemmech/input directory.
