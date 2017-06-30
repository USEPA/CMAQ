# Two-way Coupled Meteorology Chemistry WRF-CMAQ    

**Author/P.O.C.:**, [David Wong](mailto:wong.david-c@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The new two-way coupled WRF-CMAQ model which is based on WRF 3.8 and CMAQ 5.2, is an online meteorology-chemistry model that simulates the two-way feedback between meteorology and chemistry. The feeback focuses on the interactions of estimated aerosol mass on incoming shortwave radiation.


## Build Instructions

- Download WRF 3.8 and unzip it (recommend command: tar xfz the_zip_file) 
   At the end of this step, you will see a new directory WRFV3 and rename it 
   to WRFV38.
- Configure WRF by typing configure (this creates a configure.wrf file)
- If you have never done WRF configure before, here are some guidelines:
* if the configure script does not find the NETCDF path, follow the prompt to enter the explicit NETCDF include path and library path
* option selection determines by choosing an approriate compiler vendor and hardware description on right most column of the displayed option table and intercept with with the third column (dmpar)
       example: for INTEL (ifort/icc), selection is 15
* in the compile for nesting section, choose the default value

- Download IOAPI_3.1 (or IOAPI 3.2) and install it.

- Go through regular building CMAQ model process. Make sure bldit.cctm
   script  have:

    "set MakeFileOnly" line uncomment out
    "set build_twoway" line uncomment out

   After running the blidit.cctm script, rename BLD_* as cmaq and move it 
   into WRFV38 directory.

 - Download twoway.tar.gz from the CMAS Center Software Clearinghouse and unzip it. A twoway directory is formed and move it inside WRFV38 as well.. From http://www.cmascenter.org, select Download -> Software -> CMAQ and choose version 5.2 to download the coupled model tarball, twoway.tar.gz.
- Go into directory WRFV38 and execute the following command: 

   twoway/assemble 

   This command will update all necessary files in WRF and CMAQ to create 
   the twoway model. You can find the original files inside twoway/misc/orig 
   directory.

   Note: 
   
   1. You might need to edit the IOAPI and MPI paths in configure.wrf base on 
      the location of those two libraries on your system.

   2. You can also replace the netCDF link with explicit netCDF path under
      LIB_EXTERNAL and NETCDFPATH. Copy configure.wrf to configure.wrf.saved.
      Once you have this done and in the future if you type "clean -a", you
      can restore the configure file by "cp configure.wrf.saved configure.wrf"
      without going through configure step again.

   3. In the future, when you bring in a newer version of cmaq, you just
      need to type

      twoway/assemble m

      and a Makefile.twoway will be created inside that new cmaq directory

- Compile the twoway model by typing "compile em_real >& mylog". This might
   take some time for completion and you can monitor size changes of file,
   mylog. If compilation is done successfully, you can find main/wrf.exe file.

Note: 

 1. A one day test dataset is also available for download from your browser at:

    ftp://newftp.epa.gov/exposure/CMAQ/V5_2/WRF-CMAQ_Coupled_Model/

    Once you unpack the file, you can store in anywhere you want.

 2. a run script, twoway_model_run_script is in twoway/script inside twoway.tar.gz

    In order to use this script to run the WRF-CMAQ two-way model, you need to
    modify two variables: WRF_DIR and INPDIR.

    In general, the area in bwtween

       '# ##### begin user define area #####'

    and

       '# ##### end user define area #####'

    can be modified to suit a particular simulation.

If you have any question, please contact David Wong at wong.david-c@epa.gov


## Run Instructions
A test dataset is also available from the CMAS Center Software Clearinghouse (wrf_38_cmaq_52_input.tar.gz, wrf_38_cmaq_52_output.tar.gz). A sample run script, twoway_model_run_script, is in the twoway.tar.gz under script subdirectory.

## WRF-CMAQ Input/Output Data

The WRF-CMAQ benchmark data provide examples of the files needed to run the model. The general list of inputs required for WRF-CMAQ include,

* REAL outputs :: wrfbdy, wrflowinp, wrffdda, wrfsfdda, wrfrstrt
* CMAQ inputs  :: emissions, IC, BC, OMI, ocean file

WRF-CMAQ outputs standard WRF (wrfout) and CMAQ output files.

## References:

Wong, D. C., Pleim, J., Mathur, R., Binkowski, F., Otte, T., Gilliam, R., Pouliot, G., Xiu, A., Young, J. O., and Kang, D.: WRF-CMAQ two-way coupled system with aerosol feedback: software development and preliminary results, Geosci. Model Dev., 5, 299-312, doi:10.5194/gmd-5-299-2012 , 2012.

For an overview of the 2-way Coupled WRF-CMAQ see: http://www.cmascenter.org/conference/2011/slides/mathur_overview_two-way_2011.pptx

and for more details on the 2-way Coupled WRF-CMAQ system see: http://www.cmascenter.org/conference/2011/slides/wong_wrf-cmaq_two-way_2011.pptx

-----
