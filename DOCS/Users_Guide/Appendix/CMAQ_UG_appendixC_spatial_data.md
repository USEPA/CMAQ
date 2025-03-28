<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixB_emissions_control.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixD_parallel_implementation.md)

<!-- END COMMENT -->

# Appendix C: Processing Spatial Data for CMAQ Inputs

## C.1 Geospatial Data

Air quality modeling requires many spatial data to generate anthropogenic,
biogenic, fire, sea salt, dust, and NH<sub>3</sub> emissions. In addition, land surface characteristics such as
land cover types with vegetation leaf area index (LAI) and fraction, albedo, and soil types are required in
modeling the exchanges of heat, moisture, and momentum between the land and atmosphere and dry deposition
of trace chemicals (e.g. O<sub>3</sub> and NH<sub>3</sub>). It is important to use a consistent coordinate system for all the geospatial data used in emission, meteorology, and air quality modeling. Most of the geospatial data
required for the Sparse Matrix Operator Kernel Emissions (SMOKE), Weather Research and Forecasting
(WRF), and CMAQ modeling can be generated using the [Spatial Allocator (SA)](https://www.cmascenter.org/sa-tools/) or the [Surrogate Tool](https://github.com/CEMPD/SurrogateToolsDB).

In using the spatial data, it is important to know the datum, which is a spheroidal surface that represents the surface of the earth, and the projection, which is a mathematic transformation that converts a location on the datum to the location on a flat plane. The following sections briefly describe the appropriate datum and projections to use with the CMAQ system and the methods for generating the needed spatial data in the correct form.

## C.2 Geodetic datum

 A geodetic datum is a coordinate system used to define a location on the Earth.
 There are many datums used in spatial datasets depending on what geographic regions they are and
 how the Earth’s surface is approximated as a spheroid.  Most of U.S. geospatial data are defined in
 North American Datum of 1983 (NAD83) and the global data sets are often defined in World Geodetic System
1984 (WGS84).

WRF datasets are in WGS84.  All latitude-longitude geographic data sets used in a CMAQ simulation,
such as emissions shapefiles, land use or biogenic data files, and the ocean file, should be in WGS84
so that they are spatially aligned with the WRF files.  For simulations over North America, NAD83 is only
slightly different from the WGS84 datum.  As a result, NAD83 can be used for North America domains without
introducing spatial misalignment issues in the model datasets.

## C.3 Spatial Data Projection

CCTM can use any of the [four map projections defined for WRF.](https://www2.mmm.ucar.edu/wrf/users/docs/user_guide_v4/v4.4/users_guide_chap3.html)
The four map projection coordinate systems are regular latitude-longitude geographic, Lambert conformal conic, Mercator, and Polar
stereographic. However, users should note that several of the PREP and POST tools that are part of the CMAQ system do not currently support the Mercator projection.  These include ICON, BCON, sitecmp, sitecmp_dailyo3, bldoverlay, hr2day and writesite.

It is important to know that in projecting spatial data that is in WGS84 to the CMAQ projection or projecting CMAQ data to another map projection, users SHOULD NOT do any datum transformation. This is consistent with the WRF preprocessing system (WPS). Datum transformation will result in  geographic location shifting.

The CMAQ domain projection is defined through the [PROJ](https://proj.org) coordinate transformation software library using a spherical surface with an earth radius of 6370000 m to match the WRF domain projection definition.  Once an input dataset is in WGS84 the following examples can be used to define the projection transformation needed to match the WRF data:

Lambert Conformal Conic:  "+proj=lcc +a=6370000.0 +b=6370000.0 +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97"

Polar stereographic:  "+proj=stere +a=6370000.0 +b=6370000.0 +lat_ts=33 +lat_0=90 +lon_0=-97 +k_0=1.0"

Mercator:  "+proj=merc +a=6370000.0 +b=6370000.0 +lat_ts=33 +lon_0=0"

Geographic:  "+proj=latlong +a=6370000.0 +b=6370000.0"

## C.4 Spatial Data Generation

Emission spatial allocation surrogates are required for generating anthropogenic emissions by SMOKE to
spatially allocate county-based emission inventories to model grid cells. Emission surrogates can be based
on population, roads, airports, railroads, and land use spatial data sets. The [Surrogate Tool](https://github.com/CEMPD/SurrogateToolsDB) can be used for to generate spatial surrogates for SMOKE.

**Regional masks** are used to specify regions such as states, counties, or countries within a gridded spatial domain. These regions are applied to regionally scale emissions as specified in section B.3.4 of the [DESID Appendix](CMAQ_UG_appendixB_emissions_control.md) and to track emissions by region in [ISAM](../Users_Guide/CMAQ_UG_ch11_ISAM.md) Masks can be created from a geospatial file of regions, such as county shapefile, with the [shp2cmaq][link_C.4] tool.

**Biogenic emissions** requires land use input including coverage of different tree species. The Biogenic Emissions Landcover Dataset version 5 (BELD5) consists of 257 different landuse types at 1km horizontal resolution that covers all the contiguous United States, Mexico, most of Canada, parts of southern Alaska, and other Caribbean and
Central American countries. BELDv5 data is available from the 2017 emissions modeling platform ftp site: [https://gaftp.epa.gov/Air/emismod/2017/biogenics/](https://gaftp.epa.gov/Air/emismod/2017/biogenics/). For more information on BELD5 landuse types see:
[https://www.cmascenter.org/smoke/documentation/4.8/html/ch08s09.html#sect_input_source_beld5](https://www.cmascenter.org/smoke/documentation/4.8/html/ch08s09.html#sect_input_source_beld5)
and see the EPA 2017 NEI Technical Support Documentation (section 4.6): [https://www.epa.gov/sites/production/files/2020-04/documents/nei2017_tsd_full_30apr2020.pdf](https://www.epa.gov/sites/production/files/2020-04/documents/nei2017_tsd_full_30apr2020.pdf). To aggregate the BELD5 to a coarser modeling domain with the same map projection use the SMOKE Utility Tool called AGGWNDW. The SMOKE User’s Manual has information on this tool here:
[https://www.cmascenter.org/smoke/documentation/4.8/html/ch05s03s02.html](https://www.cmascenter.org/smoke/documentation/4.8/html/ch05s03s02.html).

**Sea spray emissions** require open ocean and surf zone (50m) buffer fractions for the modeling grid
 cells in an I/O API file. For most of North American domain, a SA Vector allocation tool can be used
 to generate the surf zone and open ocean file from a polygon shapefile with land, surf zone buffer,
 and open ocean in SA data directory. For areas outside U.S., users have to generate a surf zone polygon
 shapefile with has the same attribute as the file in the SA to use the tool.  See the [CMAQ Tutorial on creating an ocean file](../Tutorials/CMAQ_UG_tutorial_oceanfile.md) for step by step instructions on creating this CMAQ input file. [Chapter 6](../CMAQ_UG_ch06_model_configuration_options.md#sea-spray) has additional information on sea spray module in CMAQ.
 
**DMS and halocarbon emissions** are calculated in-line and require the presence of DMS and CHLO in the ocean file. 
 A Python note book can be used to add DMS and CHLO to an existing ocean file. See the [CMAQ Tutorial on creating an ocean file](Appendix/CMAQ_UG_tutorial_oceanfile.md) for step by step instructions on creating an ocean file and for adding DMS and CHLO to the ocean file. 
 
**NH3 emissions** from agricultural lands can be estimated using the CMAQ bi-directional NH3 model. The
input for the CMAQ bi-directional NH<sub>3</sub> model is generated by the Fertilizer Emission Scenario Tool for
CMAQ (FEST-C) system. FEST-C contains three main components: Java interface, Environmental Policy
Integrated Climate (EPIC) model, and SA Raster Tools. The interface guides users through generating
required land user and crop data and EPIC input files and simulating EPIC, and extracting EPIC output
for CMAQ. The generated BELD4 land use data by FEST-C needs to be converted into an I/O API format
using a utility program in FEST-C for CMAQ input. Note that the BELD4 data used for FEST-C is generated by the 2nd approach described above in Biogenic emission generation approaches.  

- [Fertilizer Emission Scenario Tool for CMAQ (FEST-C)](https://www.cmascenter.org/fest-c/) 
- FEST-C reference: Ran, L., Yuan, Y., Cooter, E., Benson, V., Yang, D., Pleim, J., Wang, R. and Williams, J. (2019). An integrated agriculture, atmosphere, and hydrology modeling system for ecosystem assessments. Journal of Advances in Modeling Earth Systems, 11(12), 4645-4668. DOI: [https://doi.org/10.1029/2019MS001708](https://doi.org/10.1029/2019MS001708)

**Land use and land cover data for surface flux modeling** in meteorology and air quality can be
generated using WPS or the SA Raster Tools. It is important to use consistent land use data in both
meteorology and air quality modeling. For the U.S., WPS contains re-gridded 9-arc
second (around 250 m resolution) 2011 NLCD land cover, imperviousness, and canopy data while 2011 MODIS
land cover is used for areas outside the U.S. In addition, users can use the land use re-gridding tool in the
SA Raster Tools system to generate land cover data for any domain directly using NLCD (at 30 m resolution)
or/and MODIS land cover data (at 1 km or 500 m resolution). Users can use a provided R utility in SA to
update their geogrid land cover data using the more accurate land cover data generating using SA.

<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixB_emissions_control.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixD_parallel_implementation.md)<br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->

<!-- START_OF_COMMENT -->

[link_C.4]: ../../../PYTOOLS/shp2cmaq/

<!-- END_OF_COMMENT -->

[link_C.4]: https://github.com/USEPA/CMAQ/blob/main/PYTOOLS/shp2cmaq/ 
