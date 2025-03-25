# PYTOOLS

## [shp2cmaq: Create CMAQ-Ready File from Shapefile](../../PYTOOLS/shp2cmaq/README.md)
[Barron Henderson](mailto:henderson.barron@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: New Pre-Processing Tool  
**Release Version/Date**: CMAQv5.5  

**Description**: This Python Notebook converts GIS shapefiles into gridded netCDF mask files that represent spatial features such as political boundaries. This tool works on both CONUS and Northern Hemisphere domains and has the capability to represent quantitative variables such as population.  The CMAQ-ready mask files can be used for [defining regions and region families with DESID](../Users_Guide/Appendix/CMAQ_UG_appendixB_emissions_control.md#b34-defining-and-using-regions-and-region-families) and using [geographic source regions when running CMAQ-ISAM](../Users_Guide/CMAQ_UG_ch11_ISAM.md#1131-isam-control-file-sa_iolist).  The mask files can also be used to spatially subset CMAQ input and output files for analysis, for example calculating emissions totals by state or averaging modeled ambient concentrations over census tracts.  The code relies on [geopandas](https://geopandas.org/en/stable/) and [cmaqsatproc](https://github.com/barronh/cmaqsatproc) to perform spatial operations and create IOAPI-like files for CMAQ. The overall process requires a shapefile with attributes and a GRIDDESC file. It produces variables that specify grid cell fractional coverage by each unique value of a specified attribute. It also produces total and dominant attribute variables. The total specifies the fraction coverage of any attribute. The dominant variable specifies which attribute has the largest area coverage.  
  
**Example of a gridded mask file for state boundaries in the contiguous US.**
<img width="85%" src=https://github.com/user-attachments/assets/9e12c366-8985-46c7-8482-4d5edf373113>

**Significance and Impact**: No impact on core model results. The significance of this tool is to provide a simple shapefile processor to use the DESID masking feature. Spatial mask files can also be used for ISAM simulations or as part of post-processing model output. 

**References**:  
[DESID Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md)  
[ISAM Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_ISAM.md)  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#904](https://github.com/USEPA/CMAQ/commit/cff875c350f99422bc56d43367762516aa15aa14) | [PR#904](https://github.com/USEPA/CMAQ_Dev/pull/904)  | 

## [DMSCHLO: Augment the Standard Ocean File for Marine Chemistry](https://github.com/USEPA/CMAQ/blob/main/PYTOOLS/dmschlo/README.md)
[Barron Henderson](mailto:henderson.barron@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: New Pre-Processing Tool  
**Release Version/Date**: CMAQv5.4 
 
**Description**: Depending on the chemical mechanism selected, the traditional "Ocean" file maybe required to contain temporal ocean dimethyl sulfide (DMS) and chlorophyll-a (CHLO) concentrations for atmospheric DMS and halogen chemistry, respectively. To create these fields for input, a new Python tool called "DMSCHLO" is released to augment standard CMAQ "Ocean" files with this data.  

**Significance and Impact**: No impact on core model results. The significance of this tool is to provide an easy to use tool in which to augment the standard "Ocean" file for use with CCTM.  See the [Ocean File Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_oceanfile.md) for further information.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#903](https://github.com/USEPA/CMAQ/commit/7e91d013bf75699783bb03773d5e9da2fb66b99f) | [PR#903](https://github.com/USEPA/CMAQ_Dev/pull/903)  | 





