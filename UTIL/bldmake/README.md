The bldmake utility is used to create a Makefile suitable for compiling the components of CMAQ (e.g. CCTM, post-processing tools, ICON, BCON, etc.). The Makefiles produced will contain the proper rules, compilation flags, and library paths to produce each executable.   

The utility relies on a text-based configuration file (e.g. CCTM_v53.cfg) to determine the contents of the Makefile. Each configuration file includes information about the build-time options for the target executable, the compiler brand and version, the paths to required libraries (i.e. NetCDF and IOAPI), and compilation flags. With this input, bldmake harvests source code from the CMAQ repository consistent with the selected build-time options and auto-populates the Makefile with the correct dependencies for each process module.  

Note that each Makefile created by bldmake will only work for a specific compiler/version (set in the input to bldmake). Users should ensure that their compute environment is consistent when compiling with that Makefile and running the generated executable.  

The CMAQ repo contains shell script drivers (e.g. bldit_cctm.csh, bldit_combine.csh, etc.) for choosing build options, populating the bldmake configuration file, running bldmake to generate a Makefile, and running the Makefile to create an executable. The CMAQ team recommends users refrain from modifying bldmake or executing it stand-alone. Instead, please use these shell script drivers.  


