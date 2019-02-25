
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch04_science.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch06_req_lib.md)

<!-- END COMMENT -->

# CMAQ Installation and System Requirements #

This section describes how to set up and install CMAQ on a Linux system.  The installation instructions in this section guide the user through obtaining the CMAQ source code and installing it on his or her system. Brief instructions for running the CMAQ benchmark case and benchmarking the model are also addressed. Here, the term “benchmarking” refers to the process of verifying that a model has installed correctly on a new computer. CMAQ is distributed with a reference dataset that can be used to benchmark the CMAQ installation. This distribution includes input data (e.g. emissions, land use, meteorology, etc.) and reference output data from a CMAQ run so that results may be compared.

After benchmarking has been successfully completed, the CMAQ system can be configured for other simulations. The same steps that are required to build the model for the benchmark case apply to building it for new simulations. Configuring CMAQ for new applications is covered in [Chapter 10](CMAQ_OGD_ch10_new_simulation.md).

## System Recommendations

### Hardware

### Software



Installing and Compiling CMAQ Source Code
-----------------------------------------

Several steps are required to prepare your Linux system for compiling and running CMAQ. The general sequence for installing CMAQ, including the required support software and libraries, is listed here.  

1. Check for Fortran and C compilers on your Linux system. Install if they are not available.  
2. [Install Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) (or CVS for older versions of CMAQ).
3. Download the [I/O API](http://www.cmascenter.org/ioapi) and [netCDF](http://www.unidata.ucar.edu/software/netcdf/) source and install those libraries. Follow the instructions in the documentation for each library on how to build them for your Linux system. Note: It is highly recommended that you use the same compiler for these libraries as you will use to build the CMAQ programs.
4. Install a Message Passing Interface (MPI) library on your Linux system.
5. Download the CMAQ source code and scripts from either the [EPA GitHub Repository](https://github.com/USEPA/CMAQ) or the CMAS Center ([<http://www.cmascenter.org>](http://www.cmascenter.org/)). After registering to download CMAQ on the CMAS Center Software Clearinghouse, users are redirected to a page that contains links to download Linux tar files of the CMAQ code, scripts, and benchmark data along with various documents describing the installation and execution processes. *Note that GitHub only provides access to source codes and scripts.  Benchmark input and output data may only be downloaded from EPA or the CMAS Center.*

### Distribution contents

Starting with CMAQv5.2, the structure of the CMAQ repository includes:
- CCTM - Chemistry Transport Model source code, scripts, and release notes
- PREP - Input pre-processing software (e.g., ICON, BCON, MCIP) source code and scripts
- UTIL - Utility software (e.g., BLDMAKE, CHEMMECH, NML) source code and scripts
- POST - Post-processing and analysis software (e.g., COMBINE, HR2DAY, BLDOVERLAY) source code and scripts
- DOCS - This User's Manual, tutorials, and developers guidance

### Notes on the CMAQ directory structure

### Configuring your system for compiling CMAQ

### Installing CMAQ on your system

### Running the CMAQ Installation Test Simulation


### Benchmarking



<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch04_science.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch06_req_lib.md)<br>
CMAQ Operational Guidance Document (c) 2016<br>

<!-- END COMMENT -->
