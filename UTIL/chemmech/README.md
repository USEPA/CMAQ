# CMAQ.mech_processor
FORTRAN and c code that creates the RXNS modules for the CMAQ model version 5.2

This repository  contains the template bldrun script under script and code directory to generates the RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 files
for CMAQ version 5.2.

To use this tool:

1) Compile the tool by modifying the bldit script in the scripts directory. Set Fortran compiler based on your system, save and run the script.

2) Modify the run script by setting the Mechanism that you are building.

3) Execute the run script and inspect the results under the output directory.
