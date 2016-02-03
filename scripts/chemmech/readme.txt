The directory contains the runscript and code subdirectory to generate the
RXDT and RXCM.EXT files for CMAQ version 5.0.

To use this tools:
1) Compile the tool by modifying the Makefile in the src 
subdirectory. The changes set fortran compiler, cc compiler, their compilation
and link flags on your system.

2) Next, modify the runscript for your own application or use the test data
defined in the runscript. Make a back up copy of the test data if the latter
case is being conducted.

3) Execute the runscript and inspect the results.

