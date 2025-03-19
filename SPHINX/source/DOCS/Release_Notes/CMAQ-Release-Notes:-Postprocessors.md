# Post-processors

## [calc_tmetric](../../POST/calc_tmetric/README.md) 
 No changes were made to this tool in CMAQv5.5.


## [combine](../../POST/combine/README.md)

### Improve Checks on Formulas Used by COMBINE
[William T. Hutzell](mailto:Hutzell.Bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: The update to COMBINE detects and reports syntax errors in a species definitions files listed below. Error messages list the syntax error so the user can more easily correct the definitions file. Note that COMBINE stops at the first detected error so correcting a species definitions file is an iterative process if the file contains several errors.   
````
O3_ERROR1 , ppmV, O3[1]/       
O3_ERROR2 , ppmV, O3[1]\*    
O3_ERROR3 , ppmV, O3[1]+
O3_ERROR4 , ppmV, O3[1].1000.   
O3_ERROR5 , ppmV, O3[1]y1000.
O3_ERROR5 , ppmV, O3[1]O3[1]    
````     

**Significance and Impact**: Prevents Errors in Results from Postprocessing by COMBINE  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1091](https://github.com/USEPA/CMAQ/commit/b4ff5c9631caa1361593656c38b70715211e11f7) | [PR#1091](https://github.com/USEPA/CMAQ_Dev/pull/1091)  |  

### Corrected Deposition Species Definition (SpecDef_Dep) files for missing nitrogen species
[Jesse Bash](mailto:Bash.Jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: There are a couple of issues with the SpecDef_Dep files resulting in the underestimation of total nitrogen deposition. The following changes were made to be more consistent with new NADP wet deposition and field scale dry deposition measurements: 
1. HNO4 (PNA) was missing from oxidized dry deposition 
2. NO3 radicle was omitted from oxidized dry and wet deposition
3. CLNO2 and CLNO3 were omitted from oxidized dry and wet deposition   
4. cb6 was missing MTNO3J in the organic N deposition
5. PANT was not added to the organic N deposition 

**Significance and Impact**: This does not change model results and only modifies post processing. The impact on post processed wet deposition results are minimal. The omissions from the dry deposition variables can amount to about a 1% increase in the post processed results. However, the largest increases are in remote vegetated and coastal areas which tend to be more sensitive to nitrogen loading.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1063](https://github.com/USEPA/CMAQ/commit/24c0840315978f94541d8b6288163e7a54c8694d) | [PR#1063](https://github.com/USEPA/CMAQ_Dev/pull/1063)  | 

### CRACMM SpecDef Deposition Updates for HNO4 and CLNO2
[Havala Pye](mailto:Pye.Havala@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: Removes any formulas that include "CLNO2", CRACMM does not have "CLNO2". Fixes any formulas that contain "PNA", which is named "HNO4" in CRACMM.   
**Significance and Impact**: Enables automated post processing of deposition via combine. Does not affect model results.    

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#966](https://github.com/USEPA/CMAQ/commit/1433cdb44cdc64b4f8209cf388c034510b18856e) | [PR#966](https://github.com/USEPA/CMAQ_Dev/pull/966)  |  

## [hr2day](../../POST/hr2day/README.md)
### Updating tz.csv to Natural Earth  
**Primary Contact**: [Barron Henderson](mailto:henderson.barron@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Change in input file  
**Release Version/Date**: CMAQv5.5  

**Description**: The tz.csv file is used to assign a time zone offset to each CMAQ grid cell.  This is then used by hr2day to calculate daily metrics in Local Standard Time (LST).  The previous tz.csv seems to have unrealistic boundaries and what must be typos in other places. In addition, the tz.csv file has origins that are lost to history. We do not know on what database it was founded on or how it was converted.

The previous tz.csv file was compared to two other time zone databases: tz_world.geojson [1] and Natural Earth [2]. The other databases were found to be more consistent in terms of the time zone boundaries. In addition, there were several locations in the western US in the previous tz.csv file with an offset of -5 UTC in otherwise Mountain time zones, which were likely typos.  

A new tz.csv file was created from the Natural Earth 10m time zone shapefile (v4.1.0)[2]. The new tz.csv file is 2.5 MB, compared to the 4 MB original. The new file is smaller because it does not attempt to hug coastal boundaries, but instead allows for time zones that extend into the water to do so.  

**Significance and Impact**: This will not affect model concentrations, but it will impact h2day calculations because small areas have updated time zones. This tends to matter most along the edges of time zones.  The original, new, and difference in "TZ hours behind UTC" are shown below.  

<img width="523" alt="image" src="https://github.com/user-attachments/assets/cf3ab1fb-fa09-48c9-ba7e-c21a18434121">

**References**:  
[1] http://efele.net/maps/tz/world/  
[2] https://www.naturalearthdata.com/downloads/10m-cultural-vectors/timezones/  
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1017](https://github.com/USEPA/CMAQ/commit/d7d24f8f2ca536353fc9983fd4301319da8fbead) | [PR#1017](https://github.com/USEPA/CMAQ_Dev/pull/1017)  |   

### Clarification of W126 Daily Index Computation and Minor Code Corrections
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: documentation, minor bug fix  
**Release Version/Date**: CMAQv5.4  
**Description**: Updated the README and inline code documentation to clarify that the W126 option computes the W126 daily index value as a weighted average of ozone concentrations between 8am & 7pm and that these daily index values are only an intermediate step in computing the W126 metric for secondary ozone standard analyses. These daily index values can then be used to calculate annual W126 values for the secondary ozone standard by using different tools to first compute 3-month sums of daily W126 index values and then determine the consecutive three month period with the largest 3-month sum of daily W126 values. There were also minor code corrections that fixed two issues that may have caused problems for certain compilers in debug mode but did not affect the results of the computations in the tests conducted.   
**Significance and Impact**: No impact on results for the cases tested.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#859](https://github.com/USEPA/CMAQ/commit/3fbf66df2a123de5d4cc4c75352c08016a1123d2) | [PR#859](https://github.com/USEPA/CMAQ_Dev/pull/859)  |    

## [sitecmp](../../POST/sitecmp/README.md)
 No changes were made to this tool in CMAQv5.5.

## [sitecmp_dailyo3](../../POST/sitecmp_dailyo3/README.md)
### Increase of station ID character limit  
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: expand code functionality    
**Release Version/Date**: CMAQv5.5  
**Description**: The previous version of sitecmp_dailyo3 code imposed a maximum station ID character limit of 9. While sufficient for all observational networks previously processed through sitecmp_dailyo3, this constraint caused problems when processing emerging networks with hourly data which had station IDs exceeding that limit. The increase of the maximum station ID length to 20 remedies this problem and makes the limit consistent with the one being used in sitecmp.    
**Significance and Impact**:
There is no impact on results for stations with IDs not exceeding 9 characters. When the previous code encountered a station ID with more than 9 characters, each such site was processed and written out 24 times per day (rather than just once), and the observed daily metrics were missing for each of these 24 output records per day. The modeled daily metrics were correct and repeated for each of the 24 output records written for each day. Due to this 24-fold repetition of matched records for each day in such cases, the output files created with the previous code were substantially larger than they should have been, and they also contained no valid observed metrics at such stations. The updated code corrects this behavior.  
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#987](https://github.com/USEPA/CMAQ/commit/8d67a79d9cfd45bf421fcc864f3c0c70960df17b) | [PR#987](https://github.com/USEPA/CMAQ_Dev/pull/987)  |    

## Removal of appendwrf, bldoverlay and blockextract  
**Type of update**: Model Clean-up  
**Release Version/Date**: CMAQv5.4    
**Description**: 
The POST tools appendwrf, bldoverlay, and blockextract are no longer maintained and have been removed from the CMAQ code repository beginning with version 5.4. These tools can still be accessed through previous CMAQ versions.
