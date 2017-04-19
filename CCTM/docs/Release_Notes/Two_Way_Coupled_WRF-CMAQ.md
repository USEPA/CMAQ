# Two-way Coupled Meteorology Chemistry WRF-CMAQ    

**Author/P.O.C.:**, [David Wong](mailto:wong.david-c@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The CMAQv5.2 Two-Way Model is an online meteorology-chemistry model that simulates the two-way feedback between meteorology and chemistry in a single simulation. Coupled with the Weather Research Forecast Model version 3.8 (WRFv3.8), the CMAQv5.2 Two-Way Model, or WRF-CMAQ, simulates the interactions of estimated aerosol mass on incoming shortwave radiation.

The current release of the WRF-CMAQ model supports only the RRTMG radiation scheme for short wave aerosol direct effect. It does not simulate the effects of aerosols on long wave radiation and cannot be used with the CAM radiation scheme. This release also uses a core-shell model to perform the aerosol optics calculation rather than the volume mixing technique used in the previous version of WRF-CMAQ. This version of the model also only supports the CB05 chemical mechanism and AE6 aerosol mechanism (cb05tucl_ae6_aq).

## Build Instructions

### Download and install NetCDF and I/O API 
If not already available on your system, install the netCDFv4.x and I/O APIv3.1 libraries

### Download and configure WRFv3.8
Download WRF version 3.8 from NCAR and unzip/untar (tar xvzf) the tar file. Rename the directory WRFV3 to WRFV38.

#### Configure WRF
* Set the variable NETCDF to the point the netCDF installation directory on your system
* Type configure at the command line (this creates a configure.wrf file) 
* Choose the dmpar + compiler platform that matches your system - WRF-CMAQ does not support the serial, smpar or dm_sm options.
* In the compile for nesting section, choose the default value

### Download and install CMAQv5.2

Download CMAQ version 5.2 model from GitHub and unzip/untar
* Follow the standard build procedures for bldit
* Configure the CCTM build to output a Makefile: 
* uncomment the "CopySrc" line
* uncomment the "set MakeFileOnly" line
* uncomment the "build_twoway" line
* Select the cb05tucl_ae6_aq Mechanism and associated solver for the CCTM. **NOTE: WRF-CMAQ only supports the cb05tucl_ae6_aq mechanism at this time**

Run the bldit.cctm script and rename the resulting BLD directory to "cmaq"

Move or copy this directory into the WRFV38 directory

### Download and install the WRF-CMAQ Two-Way Package 

Set the following environment variables before proceeding
'''
FC (compiler you will use: pgi, ifort, or gfort)
IOAPI (path of the ioapi 3.1 library, e.g. /home/wdx/lib/x86_64/ifc/ioapi_3.1)
MPI_INC (path of the mpif.h, e.g. /usr/local/intel/impi/3.2.2.006/include64)
'''

Download twoway.tar.gz from GitHub and unzip/untar. 
* Move the resulting "twoway" directory into the WRFV38 directory 
Go into the directory WRFV38 and execute the command: "twoway/assemble" from the command prompt
* This command will update all necessary files in WRF and CMAQ to create the WRF-CMAQ model. The original WRF and CCTM files are saved to the twoway/misc/orig directory 
Compile the twoway model by typing "compile em_real >& mylog"
* Look for the executable main/wrf.exe to confirm that the compilation completed successfully

## Run Instructions
A sample run script (run.wrf38_cctm52) can be found in the twoway/script directory. The run script requires outck_wrfcmaq.q, which should be located in the same directory as the run script. Table 1 lists the required and optional run-time environment variables for WRF-CMAQ. Download and use the WRF-CMAQ benchmark dataset to test the installation of the codes and for examples of the input data needed to run the model.  The benchmark data include sample WRF (including outputs from REAL) and CMAQ input files for running a one day test case. 

To run WRF-CMAQ test case:
* Point the run script to the config.cmaq configured for your system
* Set the BASE environment variable in the run script to the location of the WRF38 directory that you created during the WRF-CMAQ installation
* Set the IOAPIDIR environment variable to the location of the [http://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools I/O API m3tools] binaries on your system
* The default script is configured for the WRF-CMAQ test case. Set the number of processors to run the case using the NPROCS environment variable. Check how the MPI wrapper is configured for the executable call toward the bottom of the script. Users will need to configure this call to be consistent for running MPI jobs on their system. Run the script at the command line once it is configured correctly for your system.

To check the results of the test case: 
* Look in the $M3DATA/wrfcmaq/output/20080620 for the CCTM output files and the run logs.  
* Check the rsl.error.# and rsl.out.# files for any errors or warnings
* Compare the results of the 1-day test case to the results from the WRF-CMAQ reference dataset

## WRF-CMAQ Input/Output Data

The WRF-CMAQ benchmark data provide examples of the files needed to run the model. The general list of inputs required for WRF-CMAQ include,

* REAL outputs
required: wrfbdy, wrfinput, wrflowinp
optional: wrffdda, wrfsfdda, wrfrstrt
* CMAQ inputs
required: emissions (CB05ae6 speciation), IC, BC, OMI, ocean file
optional: lightning NOx, gridded landuse for inline biogenics and windblown dust

WRF-CMAQ outputs standard WRF (wrfout) and CMAQ output files.

## References:

Wong, D. C., Pleim, J., Mathur, R., Binkowski, F., Otte, T., Gilliam, R., Pouliot, G., Xiu, A., Young, J. O., and Kang, D.: WRF-CMAQ two-way coupled system with aerosol feedback: software development and preliminary results, Geosci. Model Dev., 5, 299-312, doi:10.5194/gmd-5-299-2012 , 2012.

For an overview of the 2-way Coupled WRF-CMAQ see: http://www.cmascenter.org/conference/2011/slides/mathur_overview_two-way_2011.pptx

and for more details on the 2-way Coupled WRF-CMAQ system see: http://www.cmascenter.org/conference/2011/slides/wong_wrf-cmaq_two-way_2011.pptx

-----
