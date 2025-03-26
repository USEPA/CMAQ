Create CMAQ-Ready File from Shapefile
=====================================

---
    author: Barron H. Henderson
    date: 2020-04-25
    updated: 2024-04-24
---

This Notebook uses geopandas and cmaqsatproc to create IOAPI-like files for
CMAQ. The overall process requires a shapefile with attributes and a GRIDDESC
file. It produces variables that specify grid cell fractional coverage by each
unique value of a specified attribute. It also produces total and dominant
attribute variables. The total specifies the fraction coverage of any
attribute. The dominant variable specifies which attribute has the largest
area coverage.

Two special notes:

1. It is possible to output an overlap weighted variable. For example, the
   population weighted by the fraction of a polygon's area in a grid cell.
2. In python, you can pass in a geopandas.GeoDataFrame instead of the path
   to a shapefile. This is useful when you want to add custom attributes
   before processing.


Examples
--------

### Census State Boundaries for 12US1 Domain

The US Census state boundary shapefile has Polygons for each state with the
STUSPS attribute. With attrkey set to STUSPS, the notebook will create a
variable for each unique value of STUSPS (e.g, AL, AK, AR, ... WV, WY) with
the fractional area covered by that state. It will also create a
variable STUSPS_DOM that uses numeric codes (in alphabetic order) to
identify which STUSPS has the largest coverage of that cell. And, it will
include a variable STUSPS_TOT with the total coverage of any STUSPS.

1. Download
    * https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_state_500k.zip
    * If url doesn't work, download from census.gov
2. Run tool:

From SHELL:
```bash
python shp2cmaq.py cb_2022_us_state_500k.zip STUSPS 12US1
```

In python:
```python
from shp2cmaq import shp2cmaq
shp2cmaq('cb_2022_us_state_500k.zip', 'STUSPS', '12US1')
```

### Natural Earth Countries for Hemispheric Domain

The Natural Earth countries shapefile (administrative-level 0) has Polygons
with the country identifier attribute ADM0_A3. The notebook will create a
variable for each unique value of ADM0_A3 (e.g, USA, MEX, CAN). It will also
create a variable (ADM0_A3_DOM) that uses numeric codes (in alphabetic order)
to identify which ADM0_A3 has the largest coverage of that cell. And, it will
include a variable ADM0_A3_TOT with the total coverage of any ADM0_A3.

1. Download
    * https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_0_countries.zip
    * If url doesn't work, download from naturalearth.org
2. Run tool:

From SHELL:
```bash
python shp2cmaq.py ne_10m_admin_0_countries.zip ADM0_A3 108NHEMI2
```

In python:
```python
from shp2cmaq import shp2cmaq
shp2cmaq('ne_110m_admin_0_countries.zip', 'ADM0_A3', '108NHEMI2')
```

### Natural Earth State/Province Level for 36US3

The Natural Earth countries shapefile (administrative-level 1) has Polygons
with the state/provice identifier attribute iso_3166_2. The notebook will
create a variable for each unique value of iso_3166_2. It will also
create a variable (iso_3166_2) that uses numeric codes (in alphabetic order)
to identify which iso_3166_2_DOM has the largest coverage of that cell. And,
it will include a variable iso_3166_2_TOT with the total coverage of any
iso_3166_2.

1. Download
    * https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_1_states_provinces.zip
    * If url doesn't work, download from naturalearth.org
2. Run tool:

From SHELL:
```bash
python shp2cmaq.py ne_10m_admin_1_states_provinces.zip iso_3166_2 36US3
```

In python:
```python
from shp2cmaq import shp2cmaq
shp2cmaq('ne_10m_admin_1_states_provinces.zip', 'iso_3166_2', '36US3')
```


### Overlap Weighted Variable

shp2cmaq can also output a weighted area. For example, if you have population
by state, each cell will be assigned a fraction of each state's population.
The fraction will be equal to the fraction of the state's area that is in that
grid cell. Obviously, populations are not uniformaly distributed across states
by area. The finer the geography used, the better assumption that is.

Steps:

1. Go to censusreporter.org.
2. Create a map of total population (B01003001) by state.
3. Download the map as a zipped shapefile.
4. Unzip the shapefile.
5. Run the tool:

From SHELL:
```bash
python shp2cmaq.py --srckey=B01003001 acs2022_5yr_B01003_04000US21/acs2022_5yr_B01003_04000US21.shp geoid 36US3
```

In python:
```python
from shp2cmaq import shp2cmaq
shppath = 'acs2022_5yr_B01003_04000US21/acs2022_5yr_B01003_04000US21.shp'
shp2cmaq(shppath, 'geoid', '36US3', srckey='B01003001')
```

### Custom GeoDataFrame

This example of a custom dataframe builds on the "Census State Boundaries for
12US1 Domain" example. It starts by opening the shapefile, but then adds a
custom attribute. In this example, the custom attribute is the climate region.
Climate region is derived from the postal abbreviation. The area overlap of
each region should be the same as adding the variables from states in the
example from which this is derived (e.g., CLIMREG_W = STUSPS_CA + STUSPS_NV).

This is a trivial example, but illustrates a general capability.

1. Download
    * https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_state_500k.zip
    * If url doesn't work, download from census.gov
2. Run tool:

```python
from shp2cmaq import shp2cmaq
import geopandas as gpd


regions = {
    'NW': ['OR', 'WA', 'ID'],
    'W': ['CA', 'NV'],
    'NRP': ['MT', 'WY', 'ND', 'SD', 'NE'],
    'SW': ['UT', 'AZ', 'CO', 'NM'],
    'S': ['KS', 'OK', 'TX', 'AR', 'LA', 'MS'],
    'UMW': ['MN', 'IA', 'WI', 'MI'],
    'OV': ['MO', 'IL', 'IN', 'OH', 'KY', 'TN', 'WV'],
    'SE': ['VA', 'NC', 'SC', 'GA', 'AL', 'FL'],
    'NE': ['PA', 'NY', 'VT', 'NH', 'ME', 'MA', 'RI', 'CT', 'NJ', 'DE', 'MD', 'DC'],
    'OTHER': ['AK', 'AS', 'HI', 'PR', 'VI', 'GU', 'MP']
}
st2reg = {}
for reg, sts in regions.items():
    for st in sts:
        st2reg[st] = reg


shpf = gpd.read_file('cb_2022_us_state_500k.zip')
shpf['CLIMREG'] = shpf['STUSPS'].apply(lambda x: st2reg.get(x, "UNKNOWN"))
shp2cmaq(shpf, 'CLIMREG', '12US1', outpath='CLIMREG.12US1.nc')
```