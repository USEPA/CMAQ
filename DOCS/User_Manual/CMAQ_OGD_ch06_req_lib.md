
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch05_sys_req.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch07_programs_libraries.md)

<!-- END COMMENT -->

** >> Comment <<**  Throughout… need to de-emphasize I/O API language and dependencies. I think we can reference Carlie's page rather than trying to repeat it all here.  I/O API may *not* be required and may become optional VERY soon.

# Required Libraries #

The CMAQ programs require a set of third-party libraries that must be installed on the user's system before CMAQ can be compiled and run. These libraries control the data flow through CMAQ, define the binary file formats used by the CMAQ input and output files, and control how CMAQ functions in a multiple-processor computing environment. The [Input/Output Applications Programming Interface (I/O API)](#IOAPI) and the [Network Common Data Form (netCDF)](#NCF) are required for all applications of CMAQ. The [Message Passing Interface (MPI)](#MPI) is only required for multiple-processor applications of CCTM. Brief descriptions of these three libraries are provided in this section. For additional information, including how to compile and configure these libraries, refer to the documentation associated with each library.

<a name=IOAPI></a>

[Input/Output Applications Programming Interface (I/O API)](https://www.cmascenter.org/ioapi)
---------------------------------------------------------

<a id=NCF></a>

[Network Common Data Form (netCDF)](http://www.unidata.ucar.edu/software/netcdf)
---------------------------------



<a id=MPI></a>

Message Passing Interface Library (MPI)
-----------------------------------------


------------------------------------------
Coats, C., 2005: The EDSS/Models-3 I/O API. Available online at the [I/O API website](https://www.cmascenter.org/ioapi)
Unidata, 2009: NetCDF. Available online at [NetCDF website](http://www.unidata.ucar.edu/software/netcdf)

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch05_sys_req.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch07_programs_libraries.md)
CMAQ Operational Guidance Document (c) 2016

<!-- END COMMENT -->
