
# Regional 12km Grid Mask Files

[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Two GRIDMASK files (I/O API formatted netcdf files) are now available on the CMAS Data Warehouse.  These GRIDMASK files can be used with the 12US1 modeling grid domain (grid origin x = -2556000 m, y = -1728000 m; N columns = 459, N rows = 299).

1. **GRIDMASK_STATES_12US1.nc** - This file containes 49 variables for the 48 states in the conterminous U.S. plus DC. Each state variable (e.g., AL, AZ, AR, etc.) is a 2D array (299 x 459) providing the fractional area of each grid cell that falls within that state. 
2. **GRIDMASK_CLIMATE_REGIONS_12US1.nc** This file containes 9 variables for 9 NOAA climate regions based on the [Karl and Koss (1984) definition of climate regions](https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions.php). Each climate region variable (e.g., CLIMATE_REGION_1, CLIMATE_REGION_2, etc.) is a 2D array (299 x 459) providing the fractional area of each grid cell that falls within that climate region.  

 NOAA Climate regions:
 * CLIMATE_REGION_1: Northwest (OR, WA, ID)
 * CLIMATE_REGION_2: West (CA, NV)
 * CLIMATE_REGION_3: West North Central (MT, WY, ND, SD, NE)
 * CLIMATE_REGION_4: Southwest (UT, AZ, NM, CO)
 * CLIMATE_REGION_5: South (KS, OK, TX, LA, AR, MS)
 * CLIMATE_REGION_6: Central (MO, IL, IN, KY, TN, OH, WV)
 * CLIMATE_REGION_7: East North Central (MN, IA, WI, MI)
 * CLIMATE_REGION_8: Northeast (MD, DE, NJ, PA, NY, CT, RI, MA, VT, NH, ME) + Washington, D.C.*
 * CLIMATE_REGION_9: Southeast (VA, NC, SC, GA, AL, GA)

*Note that Washington, D.C. is not included in any of the climate regions on the website but was included with the “Northeast” region for the generation of this GRIDMASK file.

**[Link to GRIDMASK files on CMAS Data Warehouse Google Drive.](https://drive.google.com/drive/folders/1x9mJUbKjJaMDFawgy2PUbETwEUopAQDl)**

## Significance and Impact
These files can be used with CMAQ-ISAMv5.3 to track state- or region-specific emissions. See [Chapter 11 of the CMAQ User's Guide](../Users_Guide/CMAQ_UG_ch11_ISAM.md) for further information on how to use the ISAM control file with GRIDMASK files.  

The files can also be used for state or region-specific scaling of emissions using the CMAQv5.3 DESID module.  See the [DESID Tutorial in the CMAQ User's Guide](../Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md) for further information on how to use the Emission Control File to scale emissions in predetermined geographical areas.

Information on how these files were created is provided in the [README_GRIDMASK_Processing.txt](https://drive.google.com/drive/folders/1x9mJUbKjJaMDFawgy2PUbETwEUopAQDl) file that accompanies the .ncf GRIDMASK files.  Users can follow a similar set of steps for creating their own custom GRIDMASK files.

Note, these grid mask files are provided as examples to give the user community a starting point for exploring CMAQ-ISAM and DESID region capabilities. They are not intended as official or sanctioned maps for any specific analysis.  Users are encouraged to review how the maps were created to judge whether or not it satisfies the needs of a given application.

## Affected Files
None
