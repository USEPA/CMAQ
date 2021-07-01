# WRF-CMAQ Coupler: 

The new WRF-CMAQ model is based on WRF 4.3 and CMAQ 5.3.3 and supports the RRTMG radiation scheme for short wave aerosol direct effect. The new version uses a core-shell representation to perform aerosol optics calculations rather than the volume mixing technique applied in the previous versions of WRF-CMAQ. )

**Dependencies**
* Users must upgrade to CMAQv5.3.3 to use the new WRF-CMAQ model
* Users must use [IOAPI version 3.2 tagged as of 20200828](https://github.com/cjcoats/ioapi-3.2/releases/tag/20200828) which supports true parallel I/O within CMAQ.  Make sure you have the appropriate option build_parallel_io turn on if you want to utilize this feature.
* The WRF model does not depend on a particular version of the netCDF library. However, if you have built a version of the netCDF library that splits the C and Fortran portion of the netCDF library into two distinctive paths, you need to create a new netCDF library that combines all the contents in each of the following four subdirectories: bin, include, lib, and share from the C and Fortran portion, respectively. 

To build the WRF-CMAQ coupled model please see the [WRF-CMAQ Tutorial for GCC](../../DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_WRF-CMAQ_build_gcc.md). For further documentation see [Chapter 13 of the CMAQ User's Guide](../../DOCS/Users_Guide/CMAQ_UG_ch13_WRF-CMAQ.md).
