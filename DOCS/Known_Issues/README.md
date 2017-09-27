CMAQv5.2 Known Issues 
=====================

This directory contains descriptions and solutions for Known Issues in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system.
The following issues have been recognized for CMAQv5.2

## *CMAQv5.2-i1:* Allow longer strings for CTM_LOGs
Date: 2017-8-10  
Contact: Ben Murphy (murphy.benjamin@epa.gov)

### Description  
The CCTM outputs log files for each processor. The names of these files are concatenated with the following format: CTM_LOG_[Processor #]_[User String] where the "User String" is defined by the variable CTM_APPL in the CCTM runscript. Currently, the CCTM source code truncates CTM_APPL at 48 characters, limiting the usefulness of this name for long strings. Follow the steps below to allow for longer names.

### Scope and Impact
This issue only effects the names of Log files, not results or runtimes for CMAQ.

### Solution
In CCTM/src/util/util/setup_logdev.F, edit line 53:
```
CHARACTER( 48 ) :: EQNAME
```
Increase the length of EQNAME to any desired number (e.g. 200). Note that the legnth of IOLOGEQ on line 56 should be at least 11 characters greater than the length of EQNAME. Don't exceed the Linux maximum of 255 total characters for IOLOGEQ.

## *CMAQv5.2-i2:* Resolve Determination of LASTTIC in Lighting NOx Emission Module
Date: 2017-8-30  
Contact: Daiwen Kang (kang.daiwen@epa.gov)

### Description  
The original method to determine LASTTIC was based on TSTEP3D which is an IOAPI_3 variable and it can be changed anywhere in the code when DESC3 is called and it might change to an undesired value.

### Scope and Impact
This will only impact when you want to output lightning diagnotics files.

### Solution
Replace CCTM/src/emis/emis/LTNG_DEFN.F file in repository with the version located in the folder CMAQv5.2-i2
 
## *CMAQv5.2-i3:* Turn on Biogenic Emissions Seasonality By Default  
Date: 2017-8-31  
Contact: Ben Murphy (murphy.benjamin@epa.gov)  

### Description  
The release version of the CCTM runscript (CCTM/scripts/run_cttm.csh) instructed the model to ignore switching between summer and winter biogenic emission factors as a function of space and day. Because the default emission factor was set to winter, very little biogenic emissions from decidus plants were introduced by the model. 

### Scope and Impact
This problem must be fixed by anyone running the model for spring, summer or fall conditions. It will affect VOC, oxidant, ozone and PM predictions substantially in forrested areas.

### Solution
The runscript is currently up to date in the repository. If you are starting from scratch after Sep 1, 2017, you will not need to do anything. If you can replace your run script with the new one, it is encouraged. Otherwise simply set the following environment variables:  
    BIOSW_YN   Y   #Will invoke the seasonality as read from the bioseason file
    SUMMER_YN  Y   #Will default to summertime emission factors if BIOSW_YN is set to no ("N") or false ("F").  

We have also updated the bioseason file for the benchmark case (Southeast US for July 1-14, 2011). This must be updated or the model will crash when attempting to read it. Please simply redownload the tarball from the CMAS center.  


