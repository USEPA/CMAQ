## CMAQ Tutorial ##
### Creating an OCEAN file for input to CMAQ ###
Purpose: This tutorial describes how to create an ocean mask file that defines the percentage of each grid cell covered by open ocean or surf zone in the CMAQ modeling domain.


------------

The CMAQ in-line sea salt emissions module requires the input of an ocean mask file (OCEAN). OCEAN is a time-independent I/O API netCDF file that identifies the fractional coverage in each model grid cell allocated to open ocean (OPEN) or surf zone (SURF). The CCTM uses this coverage information to estimate sea salt emission fluxes from the model grid cells in-line during a CCTM run.

If your domain includes sources of sea salt emissions, follow OPTION 1. If you would rather bypass the CMAQ sea salt module with a fake OCEAN file, follow OPTION 2.

## OPTION 1: Create OCEAN file from shapefile of domain

### STEP 1: Download the Spatial Allocator</strong>

The Spatial Allocator (SA) tool can be downloaded from the CMAS Center at the following link: https://www.cmascenter.org/sa-tools/. Login and follow the download and installation instructions.

### STEP 2: Create the OCEAN file

If your domain is in the U.S., there is a shapefile included with the SA tool in the data directory (surfzone_poly_st.shp). If your domain is outside the U.S., you will need a shapefile of your domain. See the surfzone_poly_st.shp for a template of the attibutes requried by the Spatial Allocator for generating an OCEAN file.

Using the sample script `alloc_srf_zone_to_oceanfile.csh` (located in the **scripts** directory of the SA tool) as a guide, customize a script to run the SA executable on your machine.

The default alloc_srf_zone_to_oceanfile.csh script is shown below. To customize this script for a new domain, set the `GRIDDESC` variable to point to an I/O API grid description file that includes the new domain definition. Set `OUTPUT_GRID_NAME` to the name of the new grid as defined in the GRIDDESC file. If needed, change the `OUTPUT_FILE_MAP_PRJN` variable to the projection definition for the new domain.

```
#! /bin/csh -f
#******************* Allocate Shapefiles Run Script **************************
# Allocates a polygon shapefile's data to an I/O API gridded file
#*****************************************************************************

setenv DEBUG_OUTPUT Y

# Set executable
setenv EXE "$SA_HOME/bin/32bits/allocator.exe"

# Set Input Directory
setenv DATADIR $SA_HOME/data
setenv OUTPUT $SA_HOME/output

# Select method of spatial analysis

setenv MIMS_PROCESSING ALLOCATE

setenv TIME time

#set "data" shapefile parameters
setenv GRIDDESC $DATADIR/GRIDDESC.txt

#set parameters for file being allocated
setenv INPUT_FILE_NAME $DATADIR/surfzone/surfzone_NC_SC
setenv INPUT_FILE_TYPE ShapeFile
setenv INPUT_FILE_MAP_PRJN "+proj=lcc,+lat_1=33,+lat_2=45,+lat_0=40,+lon_0=-97"
setenv INPUT_FILE_ELLIPSOID "+a=6370000.0,+b=6370000.0"
setenv ALLOCATE_ATTRS TYPE
setenv ALLOC_MODE_FILE ALL_AREAPERCENT

#Set this to SURF_ZONE to create the variables needed for CMAQ OCEANfile
setenv ALLOC_ATTR_TYPE  SURF_ZONE

# Set name and path of resulting shapefile
setenv OUTPUT_FILE_TYPE IoapiFile
setenv OUTPUT_GRID_NAME NC4KM
setenv OUTPUT_FILE_MAP_PRJN "+proj=lcc,+lat_1=33,+lat_2=45,+lat_0=40,+lon_0=-97"
setenv OUTPUT_FILE_ELLIPSOID "+a=6370000.0,+b=6370000.0"
setenv OUTPUT_FILE_NAME $OUTPUT/ocean_file_${OUTPUT_GRID_NAME}.ncf

#echo "Allocating surf zone data to CMAQ OCEANfile"
$TIME $EXE
```

Run the script and check the output directory designated in the run script for the new OCEAN file.

## OPTION 2: Zero Out Sea Spray Emissions

To turn off the Sea-Spray Emissions, set the RunScript option "CTM_SS_AERO" to "N" or "F". You should then not need an OCEAN input file.

