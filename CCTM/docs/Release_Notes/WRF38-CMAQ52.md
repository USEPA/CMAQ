# WRF-CMAQ Two-way Coupled Meteorlogy-Chemistry Model

**Author/P.O.C.:**, [David Wong](mailto:wong.david@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The new twoway coupled WRF-CMAQ model is based on WRF 3.8 and 
CMAQ 5.2. It supports only RRTMG radiation scheme for short wave 
aerosol direct effect. It uses core-shell model to perform 
aerosol optics calculation rather than volume mixing technique 
as in the previous version of the twoway model. In order to use
parallel I/O implemented with pnetCDF, IOAPI 3.2 is required.
Otherwise, IOAPI 3.1 is sufficient.


## Significance and Impact

## Additional Notes/Build Notes

Procdedure to build the WRF-CMAQ model:

1. Download WRF 3.8 and unzip it 
(recommend command: tar xfz the_zip_file)
At the end of this step, you will see a new directory WRFV3 and
rename it to WRFV38.

Configure WRF by typing configure (this creates a configure.wrf file)

If you have never done WRF configure before, here are some 
guidelines:

- if the configure script does not find the NETCDF path, follow
the prompt to enter the explicit NETCDF include path and library
path

- choose the appropriate supported platform
  Since CMAQ does not support OpenMP, DO NOT choose smpar or dm_sm
  option.

- in the compile for nesting section, choose the default value

2. Download IOAPI_3.1 (or IOAPI 3.2) and install it.

3. Go through regular building CMAQ model process. Make sure bldit have: 

- "set MakeFileOnly" line uncomment out
- "set build_twoway" line uncomment out

After running the blidit script, rename BLD_* as cmaq and move it 
into WRFV38 directory.  

4. Download twoway.tar.gz and unzip it. A twoway directory is formed 
and move it inside WRFV38 as well.

set FC = ifort, pgi, or gcc

5. Go into directory WRFV38 and execute the following command:
twoway/assemble
This command will update all necessary files in WRF and CMAQ to 
create the twoway model. You can find the original files inside 
    twoway/misc/orig directory.

6. Compile the twoway model by typing "compile em_real >& mylog"
If compilation is done successfully, you can find main/wrf.exe file.

Running the model:

In this package, a run script sample can be found under script 
directory. The run script requires in_outb.q which is located in the 
same directory. You can download a test dataset and conduct a test run. 
Feel free to change any options, such as wrf options, in the script but 
the main area is bounded by:

```
# ##### begin user define area #####
# ##### end user define area #####
```

If you have any question, please contact David Wong at wong.david-c@epa.gov
