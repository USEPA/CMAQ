# Implemented a new windblown dust emission parameterization

**Author/P.O.C.:**, [Hosein Foroutan](mailto:foroutan.hosein@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

This update implements a new physics-based windblown dust emission scheme. A new dynamic relation for the surface roughness length is developed [Foroutan et al., 2017] in this scheme, and the friction velocity at the surface is calculated accordingly. The MODIS FPAR satellite observation is used to represent more realistic time-varying vegetation coverage in this model that affects both the friction velocity and the drag partitioning coefficient. Additionally, the threshold friction velocity for dust emission is updated to reflect the dependency on soil grain size following Shao and Lu [2000]. Finally, a physics-based sandblasting efficiency formulation [Lu and Shao, 1999] is used in the present model that includes the effects of soil plasticity, texture, and density.

## Significance and Impact

Evaluations [Foroutan et al., 2017] showed significant improvements over the previous scheme in CMAQ.

## Affected Files:
emis/emis/DUST_EMIS.F  

## Generating the MODIS FPAR Input File

The windblown dust module in CMAQv5.2 uses Fraction of Photosynthetically Active Radiation (FPAR) from MODIS as a surrogate to vegetation fraction. These data should be provided by the user as an input to the CCTM. The 1-km resolution gridded data are updated every 8 days and can be downloaded from the USGS [Land Processes Distributed Active Archive Center (LPDAAC)](https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod15a2).

These data should be smoothed and gap-filled data before use with the CCTM. Monthly global MODIS FPAR and leaf area index (LAI) data averaged over 10 years (2001-2010) are available as part of WRF Preprocessing System (WPS). A procedure to obtain and process the LPDAAC daily gridded FPAR and LAI data for CMAQ is described here.

#### 1. Regrid MODIS FPAR and LAI Data

Use the WRF program geogrid.exe to re-grid the global MODIS FPAR and LAI data from WPS over a specific domain.

Modify the "rel_path" of "GREENFRAC" in GEOGRID.TBL (not needed for WPSv3.8 and later), so that MODIS FPAR data will be used.

```
name=GREENFRAC
rel_path=default:greenfrac_fpar_modis/
```

Run geogrid.exe to create a “geo_em.d01.nc” file containing re-gridded FPAR and LAI over the specified domain. A sample namelist.wps for running geogrid.exe is provided below (in this example a 12-km 460 X 300 horizontal grid over the CONUS):

```
&share
 wrf_core = 'ARW',
 max_dom = 1,
 start_date = '2007-06-30_00:00:00',
 end_date   = '2007-06-31_00:00:00',
 interval_seconds = 21600
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = 'path/to/your/working/directory ',
 debug_level = 0
/

&geogrid
 parent_id         =   1,
 parent_grid_ratio =   1,
 i_parent_start    =   1,
 j_parent_start    =   1,
 e_we              = 460,
 e_sn              = 300,
 geog_data_res     = 'modis_30s+5m',
 dx = 12000,
 dy = 12000,
 map_proj = 'lambert',
 ref_lat   =  40.57356,
 ref_lon   = -94.64295,
 truelat1  =  33.0,
 truelat2  =  45.0,
 stand_lon = -97.0,
 opt_geogrid_tbl_path = '/path/to/geogrid/table'
 geog_data_path = '/path/to/geogrid/data'
/
```

#### 2. Create daily MODIS FPAR and LAI data

Next create daily data from a monthly “geo_em.d01.nc” file. A sample ncl code (monthly_to_daily.ncl) is included in the CMAQ_v52/PREP/wbdust directory.

Input: geo_em.d01.nc
Output: MODIS_FPAR_LAI_daily.nc


#### 3. Create I/O API netCDF Formatted Files

As the CMAQ input data need to be in the I/O API netCDF format, A conversion program (dpp) is provided (see CMAQ_v52/PREP/wbdust directory) to convert WRF NetCDF file to the I/O API netCDF format.

```
./dpp -cw "40" -s "MODIS_FPAR_T MODIS_LAI_T" MODIS_FPAR_LAI_daily.nc MODIS_FPAR_LAI_daily.nc.ioapi
```

#### 4. Use the MODIS FPAR and LAI data in the CCTM

The output file (MODIS_FPAR_LAI_daily.nc.ioapi) is ready to be used by CMAQ. Include the path to this file in the runscript:

```
setenv MODIS_FPAR /path/to/MODIS_FPAR_LAI_daily.nc.ioapi
```

## References:

1. Foroutan et al. (2017) Development and evaluation of a physics-based windblown dust emission scheme implemented in the CMAQ modeling system, J. Adv. Mod. Earth Sys. 9: 585-608.  
2. Lu H and Shao Y (1999) A new model for dust emission by saltation bombardment, J Geophys Res 104: 16827-16842.
3. Shao Y and Lu H (2000) A simple expression for wind erosion threshold friction velocity, J Geophys Res 105: 22437-22443.


-----
<!---
### Relevant Pull Requests:

  [PR #26](https://github.com/USEPA/CMAQ_Dev/pull/26)  
  [PR #135](https://github.com/USEPA/CMAQ_Dev/pull/135)  

### Commit IDs:

a8216accbe0830eb90b7d5af8c22612c2c964785  
8705282abed3f5f6e870d99ac97ba6c554668672  
5a4cb1103fb09c429cb93010a864e9f1431e6acf  
918ad000e648b4b3b6db091ab514fcb91b636190  
--->
