# CMAQ.mech_processor
FORTRAN and c code that creates the RXNS modules for the CMAQ model version 5.1

This repository  contains the template bldrun script under script and code directory to generates the RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 files
for CMAQ version 5.2.

To use this tool:

1) Compile the tool by modifying the Makefile in the src directory. The changes set fortran compiler(s), cc compiler(s), their compilation and link flags based on your system, IF NEEDED.

2) Next, copy modify the bldrun template into its parent direcory.

3) Modify the template for your own application.

3) Execute the runscript and inspect the results under the output directory.
