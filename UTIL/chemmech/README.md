# CMAQ.mech_processor
FORTRAN code that creates the RXNS modules for the CMAQ model version 5.1

This repository  contains the runscript called runit_chemmech and code directory to generatesthe RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 files
for CMAQ version 5.1.

To use this tool:
1) Compile the tool by modifying the Makefile in the BLD-kpp_hetero directory. The changes set fortran compiler, cc compiler, their compilation
and link flags based on your system.

2) Next, modify the runscript for your own application input data or create test inputs the mechanism definitions files in sub directories under MECHS in the CMAQ.git repository. You may want to make a back up copy of the test data if the former
case is being conducted. Also, define the output direct in the runscript. 

3) Execute the runscript and inspect the results under the output directory.
