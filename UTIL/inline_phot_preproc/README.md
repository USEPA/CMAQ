# CMAQ.inline phot preprocessor
FORTRAN and c code that create a new CSQY_DATA table for a photochemical mechanism

1) Edit scripts/bldit.inline_phot_preproc for your compiler and Mechanism. Save and run to build the software.

2) IF NECESSARY, modify src/dumb.makefile based on the compilers and their flags on your computer platform.

3) Edit scripts/run.inline_phot_preproc for your application. Mechanism needs to match what was selected in the build script.

4) If application uses photolysis rates whose cross-section and quantum yields are not listed under photolysis_CSQY_data, create the data files and add them to the directory.
 
5) Execute the script. Inputs and reference outputs are provided under input and output subdirectories based on mechanisms in CMAQ version 5.2.

To report potential program errors or EBI solver failures, 
contact Bill Hutzell/USEPA at hutzell.bill@epa.gov
