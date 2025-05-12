# CMAQ.inline phot preprocessor

##  General Information

The utility creates two input files used by the in-line method for calculating
photolysis rates. The CSQY_DATA_"mechanism" file contains the cross-section 
and quantum yields for the photolysis rates used by the specified photochemical
"mechanism". The "mechanism" is determined the RXNS_DATA_MODULE.F90 for 
building and running the utility. The PHOT_OPTICS.dat file gives the optical
properties for cloud water and ice plus the refractive indice for aerosol 
species. The file does not change between photochemical "mechanisms". When 
using the files for CCTM executions, the number of wavebands defined in the 
CSQY_DATA_"mechanism" and PHOT_OPTICS.dat files need to be the same. The 
buildrun script sets this number.


##  Using the Utility.

The utility uses FORTRAN. It is built and executed for each application 
because the RXNS_DATA_MODULE.F90 file can change between applications. 

To use the utility follow the below instructions.

1) Copy and edit scripts/bldrun.inline_phot_preproc.csh for your compiler 
and Mechanism. Save and run to build the software.

2) IF NECESSARY, modify src/inline_phot_preproc.makefile based on the 
compilers and their flags on your computer platform.

3) If application uses photolysis rates whose cross-section and quantum yields
are not listed under photolysis_CSQY_data, create the data files and add them 
to the directory.
 
4) Execute the script. Check the bldrun.log file if the executable does not 
produce CSQY_DATA table in the output directory.  

To report potential program errors or failures, contact Bill Hutzell/USEPA at
hutzell.bill@epa.gov
