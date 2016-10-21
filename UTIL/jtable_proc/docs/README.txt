# CMAQ.jtable_processor
FORTRAN and c code that create a new CSQY_DATA table for a phtochemical mechanism

1) Copy the build/run script template under scripts into its parent directory.

2) IF NECESSARY, modify src/dumb.makefile based on the compilers and their flags 
on your computer platform.

3) Change the build/run script for your application. 
If application uses photolysis rates whose cross-section and quantum yields are 
not listed under photolysis_CSQY_data, create the data files and add them to the 
directory.
 
4) Execute the script. Inputs and reference outputs are provided under input and 
output subdirectories based on mechanisms in CMAQ version 5.1.

To report program errors or failures, contact Bill Hutzell/USEPA at 
hutzell.bill@epa.gov.
