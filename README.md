# create_CMAQ_OMI_file Utility

The utility creates the OMI_DAT input file describing how total ozone column density varies over the globe and time.
The file support CMAQ model's in-line calculation of photolysis rates. Creating the OMI_DAT file involves processing 
observations from satellites, ASCII files for the latitude/longitude distribution of the ozone column for a calendar 
day. The utility also creates IOAPI files for visualing the observations and data in the OMI_DAT file. They can 
differ because the utility interpolates observations to horizontal resolution of the OMI_DAT file. The resolution is 
an option specified by the utility's run-script.

### Building the Utility

Compiling the utility requires a **FORTRAN** compiler, **netcdf** and **IOAPI**. 
If the requirements are met, a user has the below options.  

   1. Option One:  
      - Copy src directory.
      - Go into the new directory and modify the Makefile to define the library and include paths for netcdf and
        IOAPI.
      - Set the environment variable _compiler_ to intel, pgi or gcc based on the user's 
   preference.  
      - Type "make clean" then type "make".  
      
   2. Option Two: 
       - Copy to a work directory and modify the _bldit_ script under the _scripts_ subdirectory.   
       - Type bldit_create_CMAQ_OMI_file.csh _compiler_. 
       -  Go into the created build directory, type "make clean" and type "make".
