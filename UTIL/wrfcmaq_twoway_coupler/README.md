WRF-CMAQ Coupler: 

   The new coupled WRF-CMAQ model is based on WRF 4.3 and 
CMAQ 5.3.3 (users cannot use prevoius versions CMAQv5.3 & CMAQv5.3.1).It supports 
only RRTMG radiation scheme for short wave aerosol direct effect. 
It uses core-shell model to perform aerosol optics calculation rather 
than volume mixing technique as in the previous version of the twoway model. 
It is required to use version IOAPI 3.2 tagged as of 20200828 which also 
supports true parallel I/O within CMAQ (make sure you have the appropriate 
option build_parallel_io turn on if you want to utilize this feature).

   WRF model does not depend on a particular version of netCDF library.
However, if you have built a version of netCDF library that splits C
and Fortran portion of the netCDF library into two distinctive paths, 
you need to create a brand new netCDF library with combining all the
contents in each of the following four subdirectories: bin, include, 
lib, and share from C and Fortran portion, respectively. 

To build the WRF-CMAQ coupled model please see:

https://github.com/USEPA/CMAQ/tree/master/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_WRF-CMAQ_build_gcc.md
